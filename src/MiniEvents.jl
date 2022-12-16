using MacroTools
using StaticArrays 


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

"Forward schedule to the scheduler belonging to `obj`'s type."
function Scheduler.schedule!(fun, obj, at, sim)
	schedule!(fun, obj, at, get_scheduler(sim, typeof(obj)))
end


sched_mem_name(i) = Symbol("sched_$i")
alist_mem_name(i) = Symbol("alist_$i")
sum_name(i) = Symbol("sum_$i")
const VecType = SVector


# *** generators for agent type specific helper functions

"Generates `event_count(type)` for given type."
function gen_event_count_fn(decl, n)
    ag_type = decl.args[2]
    quote
        function $(esc(:event_count))(::Type{$ag_type}) 
            $n
        end
    end
end

"Generates `refresh!(agent, alist)` for given type."
function gen_refresh_fn(decl)
    ag_type = decl.args[2]
    quote
        function $(esc(:refresh!))(agent::$ag_type, alist)
            change_rates!(agent, alist, calc_rates(agent))
        end
    end
end

"Generates `get_scheduler(sim, type)` for given type."
function gen_get_scheduler_fn(ag_type, n)
	sched_name = sched_mem_name(n)
	:($(esc(:get_scheduler))(sim, ::Type{$ag_type}) = sim.$sched_name)
end


function gen_calc_rate_fn(decl, conds, rates)
    n = length(conds)
    VT = :(VecType{$n, Float64})

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


"Generate the initial scheduling to be done by `add_agent`."
function gen_scheduled_action_fn(decl, interval, start, action)
    ag_name = decl.args[1]
    ag_type = decl.args[2]

	fun_name = :($(esc(:scheduled_action!)))

	repeat = if interval != nothing
			:($fun_name($(esc(ag_name)), sim, $interval))
		else
			:()
		end

	fn_body = isempty(action.args) ?
		:() : # return noop function if no actions are provided
		quote 
			schedule_in!($ag_name, dt, get_scheduler(sim, $ag_type)) do $(esc(decl)) 
				$(esc(action))
				$repeat
			end
		end	

	quote
		# first interation starts at $start
		function $fun_name($decl, sim, dt=$start)
			$fn_body
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

"Generate `step(alist)` for a single type, including all actions."
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

    l_type = :(VecType{$(length(actions)), Float64})
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


function parse_events(decl_agent, block, decl_world=nothing)
	block = rmlines(block)
	rates = []
	conds = []
	actions = []

	# default values that will do nothing
	expr_start = :(0)
	expr_interval = nothing
	expr_action = :()
	sched_exprs = expr_interval, expr_start, expr_action
	has_sched = false

	for line in block.args
		if @capture(line, @rate(expr_rate_) ~ expr_cond_ => expr_act_)
			push!(rates, expr_rate)
			push!(conds, expr_cond)
			push!(actions, expr_act)
		# these are for convenience only, more sophisticated scenarios have to be done
		# manually using schedule!
		elseif @capture(line, @repeat(expr_interval_, expr_start_) => expr_action_)
			if has_sched
				error("only one schedule per type allowed")
			end
			sched_exprs = expr_interval, expr_start, expr_action
			has_sched = true
		elseif @capture(line, @at(expr_start_) => expr_action_)
			if has_sched
				error("only one schedule per type allowed")
			end
			sched_exprs = expr_interval, expr_start, expr_action
			has_sched = true
		else
			error("Event declarations expected: @event(<RATE>) ~ <COND> => <ACTION>")	
		end
	end
	
	generate_events_code(decl_agent, conds, rates, actions, sched_exprs)
end


function generate_events_code(decl, conds, rates, actions, sched_exprs)
    res = quote end

    fd = gen_event_count_fn(decl, length(conds))
    push!(res.args, fd)

    fd = gen_refresh_fn(decl)
    push!(res.args, fd)

    fd = gen_calc_rate_fn(decl, conds, rates)
    push!(res.args, fd)

    fd = gen_step_fn(decl, actions)
    push!(res.args, fd)

	fd = gen_scheduled_action_fn(decl, sched_exprs...)
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
		sname = sum_name(i)
		al_name = alist_mem_name(i)
		push!(cs_args, :($sname = Events.sum_rates(sim.$al_name)))
	end

	sum_exp = Expr(:call, :+)
	for i in 1:n_alists
		sname = sum_name(i)
		push!(sum_exp.args, sname)
	end
	
	push!(cs_args, :(sum = $sum_exp))

	# draw waiting time
	push!(cs_args, :(sim.dtime = rand(Exponential(1.0/sum))))

	calc_sums_and_draw_wtime = Expr(:block, cs_args...)

# *** check scheduled events
	next_dt_call = :(next_in_dtime!(sim.dtime, sim))
	ndc_args = next_dt_call.args
	for i in 1:n_schedulers
		sname = sched_mem_name(i)
		push!(ndc_args, :(sim.$sname))
	end
	
	check_scheduled_events = quote
			# check if next scheduled event is earlier than waiting time
			# if so execute it
			elapsed = $next_dt_call
			sim.dtime -= elapsed
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
		sname = sum_name(i)
		al_name = alist_mem_name(i)
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
    decl = :(mutable struct $name 
		dtime :: Float64
		end)
    members = decl.args[3].args
	
	# constructor and arguments
    constr = :($(esc(name))(0.0))
	args = constr.args

	# add_agent and arguments
    add_a_block = quote end
    add_a_args = add_a_block.args

	# generate alist members for rate-based scheduling
    for (i, t) in enumerate(types)
        al_name = alist_mem_name(i)
        al_type = :(EventList{$t, VecType{event_count($t), Float64}})
        al_e = :($al_name :: $al_type)
        al_aa = quote 
				function $(esc(:spawn!))(a :: $t, s :: $name) 
					scheduled_action!(a, s)
	                Events.add_agent!(a, s.$al_name, calc_rates(a))
				end
			end

        push!(members, al_e)
        push!(args, :($al_type()))
        push!(add_a_args, al_aa)
    end

	getsch_fns = []

	# generate scheduler members for time-based scheduling
	for (i, t) in enumerate(types)
		sched_name = sched_mem_name(i)
		sched_type = :(PQScheduler{Float64, $t})
		
		sched_e = :($sched_name :: $sched_type)

		push!(members, sched_e)
		push!(args, :($sched_type()))

		getsch_fn = gen_get_scheduler_fn(t, i)
		push!(getsch_fns, getsch_fn) 
	end

	time_rates_fn = gen_time_or_rates_fn(length(types), length(types))


    quote
        $decl
        $(esc(name))() = $constr

		$(getsch_fns...)

		$(time_rates_fn)

        $(esc(:step))(sim :: $name) = dispatch_time_or_rates(sim)

        $add_a_block
    end
end


macro events(decl_agent, block)
	parse_events(decl_agent, block)
end


macro events(decl_agent, decl_world, block)
	parse_events(decl_agent, block, decl_world)
end


