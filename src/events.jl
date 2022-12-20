
"Return number of events defined for a given agent type."
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

"Calculate transition rates for all events for a given agent types."
function calc_rates end
"Generates `calc_rates(type_decl, conditions, rates)` for a given agent type."
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

"Trigger the next rate event for a type."
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


macro events(decl_agent, block)
	parse_events(decl_agent, block)
end


macro events(decl_agent, decl_world, block)
	parse_events(decl_agent, block, decl_world)
end
