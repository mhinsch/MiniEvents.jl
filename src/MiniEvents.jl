module MiniEvents

using MacroTools
using StaticArrays 
using Distributions
using Reexport

export @events, @simulation, refresh!, schedule!, spawn!


include("EventLists.jl")
@reexport using .EventLists

include("Scheduler.jl")
@reexport using .Scheduler


"Generic `refresh!` for iterables of agents."
function refresh!(agents, alist)
	for agent in agents
		change_rates!(agent, alist, calc_rates(agent))
	end
end


sched_mem_name(i) = Symbol("sched_$i")
alist_mem_name(i) = Symbol("alist_$i")
sum_name(i) = Symbol("sum_$i")
const VecType = SVector


# *** generators for agent type specific helper functions

function event_count end
"Generates `event_count(type)` for given type."
function gen_event_count_fn(decl, n)
    ag_type = decl.args[2]
	quote $(esc(:(
        MiniEvents.event_count(::Type{$ag_type}) = $n
	))) end 
end

"Generates `refresh!(agent, alist)` for given type."
function gen_refresh_fn(decl)
    ag_type = decl.args[2]
	quote $(esc(:(
        MiniEvents.refresh!(agent::$ag_type, alist) = 
			MiniEvents.change_rates!(agent, alist, MiniEvents.calc_rates(agent))
	))) end 
end

function get_alist end
"Generates `get_alist(sim, type)` for given type."
function gen_get_alist_fn(ag_type, n)
	alist_name = alist_mem_name(n)
	quote $(esc(:(
			MiniEvents.get_alist(sim, ::Type{$ag_type}) = sim.$alist_name
	))) end 
end


function get_scheduler end
"Generates `get_scheduler(sim, type)` for given type."
function gen_get_scheduler_fn(ag_type, n)
	sched_name = sched_mem_name(n)
	quote $(esc(:(
			MiniEvents.get_scheduler(sim, ::Type{$ag_type}) = sim.$sched_name
	))) end
end

function calc_rates end

function gen_calc_rate_fn(decl, conds, rates)
    n = length(conds)
    VT = :(VecType{$n, $(esc(:Float64))})

    con_call = :($VT())
    for (c, r) in zip(conds, rates)
        arg = :($(esc(c)) ? $(esc(r)) : 0.0)
        push!(con_call.args, arg)
    end

    quote
        function $(esc(:(MiniEvents.calc_rates)))($(esc(decl))) 
            $con_call
        end
    end
end


function scheduled_action! end
"Generate the initial scheduling to be done by `add_agent`."
function gen_scheduled_action_fn(decl, interval, start, action)
    ag_name = decl.args[1]
    ag_type = decl.args[2]

	fun_name = :(MiniEvents.scheduled_action!)
	sim_name = gensym("sim")

	repeat = if interval != nothing
			:($fun_name($ag_name, $sim_name, $interval))
		else
			:()
		end

	fn_body = isempty(action.args) ?
		:() : # return noop function if no actions are provided
		quote 
			Scheduler.schedule_in!(agent, dt, MiniEvents.get_scheduler($sim_name, $ag_type)) do $decl 
				$action
				$repeat
			end
		end	

	quote
		$(esc(:(
			# first interation starts at $start
			function $fun_name(agent::$ag_type, $sim_name, dt=$start)
				$fn_body
			end
		)))
	end |> MacroTools.flatten
end


"Replace @r with calls to refresh."
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

function next_rate_event! end

"Generate `next_rate_event!(alist)` for a single type, including all actions."
function gen_rate_event_fn(decl, actions)
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
        function $(esc(:(MiniEvents.next_rate_event!)))($(esc(:alist)) :: $(esc(:EventList)){$(esc(ag_type)), $l_type}, rnum)
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

    fd = gen_rate_event_fn(decl, actions)
    push!(res.args, fd)

	fd = gen_scheduled_action_fn(decl, sched_exprs...)
    push!(res.args, fd)
    
	res |> MacroTools.flatten
end


function gen_next_in_dtime!(t_next_evt , sim, schedulers...)
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

	# if check_next is sooner than the next scheduled event
	# we simply advance all schedulers
	push!(min_args, :(t_next_evt))

	quote
		# current time
		now = time_now(schedulers[1])
		
		# get scheduled times of all schedulers
		$t_expr
		
		# soonest scheduled event
		min_next = $min_expr

		# handle next scheduled event
		$step_min
		return min_next
	end |> MacroTools.flatten
end

@generated function next_in_dtime!(t_next_evt , sim, schedulers...)
	gen_next_in_dtime!(t_next_evt , sim, schedulers...)
end


function gen_calc_sum_rates(n_alists)
	cs_args = []
	
	for i in 1:n_alists
		sname = sum_name(i)
		al_name = alist_mem_name(i)
		push!(cs_args, :($sname = EventLists.sum_rates(sim.$al_name)))
	end

	if n_alists > 1
		sum_exp = Expr(:call, :+)
		for i in 1:n_alists
			sname = sum_name(i)
			push!(sum_exp.args, sname)
		end
	else
		sum_exp = sum_name(1)
	end

	push!(cs_args, :(sum = $sum_exp))
	Expr(:block, cs_args...)
end

function dispatch_time_or_rates end
	
function gen_time_or_rates_fn(n_alists, n_schedulers)
# *** sum of rates and waiting time
	
	# draw waiting time
	draw_wait_time =  quote
			sim.t_next_evt  = now(sim) + rand(Exponential(1.0/sum))
			#println(sim.t_next_evt)
		end 

# *** check scheduled events
	next_dt_call = :(next_in_dtime!(sim.t_next_evt , sim))
	ndc_args = next_dt_call.args
	for i in 1:n_schedulers
		sname = sched_mem_name(i)
		push!(ndc_args, :(sim.$sname))
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
					return next_rate_event!(sim.$al_name, r)
				end
				r -= $sname
			end)
	end
	push!(sal_args, :(error("ran out of options")))

	select_alist = Expr(:block, sal_args...)

# *** put everything together
	quote 
		function $(esc(:(MiniEvents.dispatch_time_or_rates)))(sim) 		
			# calc sum of rates
			$(gen_calc_sum_rates(n_alists))
			# rate event has been triggered or rates been changes
			if sim.t_next_evt <= now(sim)
				$draw_wait_time
			end
			# check if next scheduled event is earlier than waiting time
			# if so execute it
			$next_dt_call
			# we are still not at waiting time, so 
			# a scheduled event must have fired, do nothing
			if sim.t_next_evt > now(sim)
				print("$(sim.t_next_evt) ")
				return nothing
			end
			# execute rate event
			$select_alist
			
			nothing
		end
	end |> MacroTools.flatten
end


macro simulation(name, types...)
	# struct declaration and members
    decl = :(mutable struct $name 
		t_next_evt  :: $(esc(:Float64))
		end)
    members = decl.args[3].args
	
	# constructor and arguments
    constr = :($(esc(name))(0.0))
	args = constr.args

	get_alist_fns = []

	# generate alist members for rate-based scheduling
    for (i, t) in enumerate(types)
        al_name = alist_mem_name(i)
        al_type = :(EventList{$(esc(t)), VecType{event_count($(esc(t))), $(esc(Float64))}})
        al_e = :($al_name :: $al_type)
        push!(members, al_e)
        push!(args, :($al_type()))

		get_alist_fn = gen_get_alist_fn(t, i)
		push!(get_alist_fns, get_alist_fn) 
    end

	get_sched_fns = []

	# generate scheduler members for time-based scheduling
	for (i, t) in enumerate(types)
		sched_name = sched_mem_name(i)
		sched_type = :(PQScheduler{$(esc(Float64)), $(esc(t))})
		
		sched_e = :($sched_name :: $sched_type)

		push!(members, sched_e)
		push!(args, :($sched_type()))

		get_sched_fn = gen_get_scheduler_fn(t, i)
		push!(get_sched_fns, get_sched_fn) 
	end

	time_rates_fn = gen_time_or_rates_fn(length(types), length(types))

	sched_fn = :(Scheduler.schedule!(fun, obj, at, sim::$(esc(name))) =
		schedule!(fun, obj, at, get_scheduler(sim, typeof(obj))))

    quote
        $decl
        $(esc(name))() = $constr

#		$(esc(:(
#			MiniEvents.num_agent_types(::Type{$name}) = $(length(types))
#		)))

		$(get_alist_fns...)
		$(get_sched_fns...)

		$sched_fn

		$time_rates_fn

        $(esc(:step!))(sim :: $(esc(name))) = dispatch_time_or_rates(sim)
    end |> MacroTools.flatten
end

@generated now(sim) = :(time_now(sim.$(sched_mem_name(1))))

"Reset waiting time."
function refresh_simulation!(sim)
	sim.t_next_evt  = now(sim)
end


function spawn!(a, s) 
	scheduled_action!(a, s)
    EventLists.add_agent!(a, get_alist(s, typeof(a)), calc_rates(a))
	# rates have changed => redraw waiting time
	refresh_simulation!(s)
end

function spawn_pop!(agents, s) 
	for a in agents
		scheduled_action!(a, s)
	    EventLists.add_agent!(a, get_alist(s, typeof(a)), calc_rates(a))
	end
	# rates have changed => redraw waiting time
	refresh_simulation!(s)
end

macro events(decl_agent, block)
	parse_events(decl_agent, block)
end


macro events(decl_agent, decl_world, block)
	parse_events(decl_agent, block, decl_world)
end

end # MiniEvents.jl
