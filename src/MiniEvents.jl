using MacroTools


include("Events.jl")
using .Events

include("Scheduler.jl")
using .Scheduler

"Generic `refresh!` for iterables of agents."
function refresh!(agents, alist)
	for agent in agents
		change_rates!(agent, alist, calc_rates(agent))
	end
end


function gen_event_count_fn(decl, n)
    ag_type = decl.args[2]
    quote
        function $(esc(:event_count))(::Type{$ag_type}) 
            $n
        end
    end
end


function gen_refresh_fn(decl)
    ag_type = decl.args[2]
    quote
        function $(esc(:refresh!))(agent::$ag_type, alist)
            change_rates!(agent, alist, calc_rates(agent))
        end
    end
end


function gen_calc_rate_fn(decl, conds, rates)
    n = length(conds)
    VT = :(SVector{$n, Float64})

    con_call = :($VT())
    for (c, r) in zip(conds, rates)
        arg = :($(esc(c)) ? $(esc(r)) : 0.0)
        push!(con_call.args, arg)
    end

    quote
        function $(esc(:calc_rates))($(esc(decl))) 
            $con_call
        end
    end
end


function filter_refreshs(actions, r_arg)
	MacroTools.postwalk(actions) do x
		if @capture(x, @r(args__))
			ret = quote end
			for arg in args
				ex = :(refresh!($arg, $r_arg))
				push!(ret.args, ex)
			end
			ret
		else
			x
		end
	end
end


function gen_step_fn(decl, actions)
    check_actions = quote end

    for (i, a) in enumerate(actions)
		act = filter_refreshs(a, :alist)
        check = :(if (r -= rates[$i]) < 0
                      $(esc(act))
                      return
                  end)
        push!(check_actions.args, check)
    end

    l_type = :(SVector{$(length(actions)), Float64})
    ag_name = decl.args[1]
    ag_type = decl.args[2]

    quote
        function $(esc(:step))($(esc(:alist)) :: $(esc(:EventList)){$ag_type, $l_type}, rnum)
            i, r = lookup(alist.sums, rnum)

            ag_actions = alist.events[i]
            rates = ag_actions.rates
            $(esc(ag_name)) = ag_actions.agent

            $check_actions

            error("somthing went wrong!")
        end
    end
end


function events(decl_agent, block, decl_world=nothing)
	block = rmlines(block)
	rates = []
	conds = []
	actions = []
	for line in block.args
		if @capture(line, @rate(expr_rate_) ~ expr_cond_ => expr_act_)
			push!(rates, expr_rate)
			push!(conds, expr_cond)
			push!(actions, expr_act)
		elseif @capture(line, @repeat(expr_interv_, expr_start_) => expr_act_)
		elseif @capture(line, @at(expr_time_) => expr_act_)
		else
			error("Event declarations expected: @event(<RATE>) ~ <COND> => <ACTION>")	
		end
	end
	
	generate(decl_agent, conds, rates, actions)
end


function generate(decl, conds, rates, actions)
    res = quote end

    fd = gen_event_count_fn(decl, length(conds))
    push!(res.args, fd)

    fd = gen_refresh_fn(decl)
    push!(res.args, fd)

    fd = gen_calc_rate_fn(decl, conds, rates)
    push!(res.args, fd)

    fd = gen_step_fn(decl, actions)
    push!(res.args, fd)

    res
end


function gen_next_in_dtime!(dtime, sim, schedulers...)
	t_expr = quote end
	t_args = t_expr.args	
	
	min_expr = :(min())
	min_args = min_expr.args

	step_min = quote end	
	step_args = step_min.args

	for i in eachindex(schedulers)
		tname = Symbol("t_$i")

		push!(t_args, :($tname = time_next(schedulers[$i])))

		push!(min_args, tname)

		push!(step_min.args, 
			quote
				sched = schedulers[$i]
				# if this is the scheduler with the soonest event, run it
				if min_next == $tname
					next!(sched)
				else
				# otherwise just change the time
					advance!(sched, min_next)
				end
			end)
	end

	quote
		# current time
		now = time_now(schedulers[1])
		# time of next rate event	
		check_next = now + dtime
		
		# get scheduled times of all schedulers
		$t_expr
		
		# soonest scheduled event
		min_next = $min_expr
		# next schedule event is after next rate event, nothing more to do
		if min_next > check_next
			return dtime
		end

		# handle next scheduled event
		$step_min
		return min_next - now
	end 
end

@generated function next_in_dtime!(dtime, sim, schedulers...)
	gen_next_in_dtime!(dtime, sim, schedulers...)
end

function gen_time_or_rates_fn(n_alists, n_schedulers)
# *** sum of rates and waiting time
	cs_args = []
	
	for i in 1:n_alists
		sname = Symbol("sum_$i")
		al_name = Symbol("alist_$i")
		push!(cs_args, :($sname = Events.sum_rates(sim.$al_name)))
	end

	sum_exp = Expr(:call, :+)
	for i in 1:n_alists
		sname = Symbol("sum_$i")
		push!(sum_exp.args, sname)
	end
	
	push!(cs_args, :(sum = $sum_exp))

	# draw waiting time
	push!(cs_args, :(sim.dtime = rand(Exponential(1.0/sum))))

	calc_sums_and_draw_wtime = Expr(:block, cs_args...)

# *** check scheduled events
	next_dt_call = :(next_in_dtime!(dtime, sim))
	ndc_args = next_dt_call.args
	for i in 1:n_schedulers
		sname = Symbol("sched_$i")
		push!(ndc_args, :(sim.$sname))
	end
	
	check_scheduled_events = quote
			# check if next scheduled event is earlier than waiting time
			# if so execute it
			elapsed = $next_dt_call
			sim.dtime = dtime - elapsed
			# we are still not at waiting time, so do nothing
			if sim.dtime > 0
				return
			end
		end

# *** select alist
	# this part happens only if we have reached waiting time
	sal_args = []
	
	# draw random variable [0, sum(rates)]
	push!(sal_args, :(r = rand() * sum))

	# select alist accordingly and execute 
	for i in 1:n_alists
		sname = Symbol("sum_$i")
		al_name = Symbol("alist_$i")
		push!(sal_args, 
			quote
				if r <= $sname
					return step(sim.$al_name, r)
				end
				r -= $sname
			end)
	end
	push!(sal_args, :(error("ran out of options")))

	select_alist = Expr(:block, sal_args...)

# *** put everything together
	quote 
		function $(esc(:dispatch_time_or_rates))(sim) 		
			if sim.dtime <= 0
				$calc_sums_and_draw_wtime
			end
			$check_scheduled_events
			$select_alist
		end
	end |> MacroTools.flatten
end


macro simulation(name, types...)
	# struct declaration and members
    decl = :(struct $name end)
    members = decl.args[3].args
	
	# constructor and arguments
    constr = :($(esc(name))())
	args = constr.args

	# add_agent and arguments
    add_a_block = quote end
    add_a_args = add_a_block.args

	# generate alist members for rate-based scheduling
    for (i, t) in enumerate(types)
        al_name = Symbol("alist_$i")
        al_type = :(EventList{$t, SVector{event_count($t), Float64}})
        al_e = :($al_name :: $al_type)
        al_aa = :(Events.add_agent!(a :: $t, s :: $name) = 
                 Events.add_agent!(a, s.$al_name, calc_rates(a)))

        push!(members, al_e)
        push!(args, :($al_type()))
        push!(add_a_args, al_aa)
    end

	# generate scheduler members for time-based scheduling
	for (i, t) in enumerate(types)
		sched_name = Symbol("sched_$i")
		sched_type = :(PQScheduler{Float64, $t})
		
		sched_e = :($sched_name :: $sched_type)
		# TODO add_agent

		push!(members, sched_e)
		push!(args, :($sched_type()))
		# TODO add_a_args
	end

	time_rates_fn = gen_time_or_rates_fn(length(types), length(types))

    quote
        $decl
        $(esc(name))() = $constr

		$(time_rates_fn)

        $(esc(:step))(sim :: $name) = dispatch_time_or_rates(sim)

        $add_a_block
    end
end


macro events(decl_agent, block)
	events(decl_agent, block)
end


macro events(decl_agent, decl_world, block)
	events(decl_agent, block, decl_world)
end


