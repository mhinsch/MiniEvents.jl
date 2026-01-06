
"""
Return number of events defined for a given agent type.
"""
function event_count end
"Generates `event_count(type)` for given type."
function gen_event_count_fn(decl, n)
    ag_type = decl.args[2]
	quote $(esc(:(
        MiniEvents.event_count(::Type{$ag_type}) = $n
	))) end 
end

"""
Generates `refresh!(agent, alist)` for given type.
"""
function gen_refresh_fn(decl)
    ag_type = decl.args[2]
	quote $(esc(:(
        MiniEvents.refresh!(agent::$ag_type, sim) = 
			MiniEvents.change_rates!(agent, MiniEvents.get_alist(sim, $ag_type), MiniEvents.calc_rates(agent, sim))
	))) end 
end

"""
Generates `kill!(agent, alist)` for given type.
"""
function gen_kill_fn(decl)
    ag_type = decl.args[2]
	quote $(esc(:(
        function MiniEvents.kill!(agent::$ag_type, sim) 
			al = MiniEvents.get_alist(sim, $ag_type)
			if haskey(al, agent)
				MiniEvents.remove_agent!(agent, al)
			end
			sched = MiniEvents.get_scheduler(sim, $ag_type)
			if haskey(sched, agent)
				MiniEvents.unschedule!(sched, agent)
			end
		end
	))) end 
end


function filter_sim(code, sim_name)
	MacroTools.postwalk(code) do x
		@capture(x, @sim()) ? sim_name : x
	end
end	
	
"Calculate transition rates for all events for a given agent types."
function calc_rates end
"Generates `calc_rates(type_decl, conditions, rates)` for a given agent type."
function gen_calc_rate_fn(decl, conds, rates)
    n = length(conds)
    VT = :(MiniEvents.VecType{$n, Float64})

	calc = quote end
    con_call = :($VT())
	prev = :()

    for (i, (c, r)) in enumerate(zip(conds, rates))
		cur = Symbol("tmp_sum_$i")
        sum_expr = i == 1 ? 
			:($cur = $c ? $r : 0.0) :
			:($cur = $prev + ($c ? $r : 0.0))
		push!(calc.args, sum_expr)
        push!(con_call.args, cur)
		prev = cur
    end

	sim_name = gensym("sim")

    quote $(esc(:( 
        function MiniEvents.calc_rates($decl, $sim_name) 
            $(filter_sim(calc, sim_name))
            $(filter_sim(con_call, sim_name))
        end
		)))
    end
end

"Replace @spawn with calls to spawn!."
function filter_spawns(actions)
	MacroTools.postwalk(actions) do x
		if @capture(x, @spawn(args__))
			ret = quote end
			for arg in args
				ex = :(spawn!($arg, @sim()))
				push!(ret.args, ex)
			end
			ret
		else
			x
		end
	end
end


"Replace @kill with calls to kill!."
function filter_kills(actions)
	MacroTools.postwalk(actions) do x
		if @capture(x, @kill(args__))
			ret = quote end
			for arg in args
				ex = :(kill!($arg, @sim()))
				push!(ret.args, ex)
			end
			ret
		else
			x
		end
	end
end

"Replace @r with calls to refresh."
function filter_refreshs(actions)
	MacroTools.postwalk(actions) do x
		if @capture(x, @r(args__))
			ret = quote end
			for arg in args
				ex = :(refresh!($arg, @sim()))
				push!(ret.args, ex)
			end
			ret
		else
			x
		end
	end
end

"Replace @selected with selected item. Only valid within @rates_for."
function filter_selnum(code, selnum_name)
	MacroTools.postwalk(code) do x
		@capture(x, @selnum()) ? selnum_name : x
	end
end	

"Replace @selected with selected item. Only valid within @rates_for."
function filter_sel(code, sel_name)
	MacroTools.postwalk(code) do x
		@capture(x, @selected()) ? sel_name : x
	end
end	

"Trigger the next rate event for a type."
function next_rate_event! end
"Generate `next_rate_event!(alist)` for a single type, including all actions."
function gen_rate_event_fn(decl, actions, debug = false)
    check_actions = quote end

	sim_name = gensym("sim")
	selnum_name = gensym("selnum")

    for (i, a) in enumerate(actions)
    	act = a |>
    		filter_refreshs |>
    		filter_kills |>
    		filter_spawns |>
    		x->filter_sim(x, sim_name) |>
    		x->filter_selnum(x, selnum_name)
		#act = filter_selnum(filter_sim(filter_kills(filter_refreshs(a)), sim_name), selnum_name)
        check = :(if r < rates[$i]
				      # TODO only add if @selnum is actually used
				      $(esc(selnum_name)) = r - $(i>1 ? :(rates[$(i-1)]) : 0)
                      $(esc(act))
                      return
                  end)
        push!(check_actions.args, check)
    end

    l_type = :(MiniEvents.VecType{$(length(actions)), Float64})
    ag_name = decl.args[1]
    ag_type = decl.args[2]

	debug_code = if debug 
		quote 
			crates = calc_rates(ag_actions.agent, $sim_name)
			if rates != crates				
				println(stderr, "rate mismatch detected: stored ($rates) != calculated ($crates)")
				exit(1)
			end
		end
	else
		:()
	end

    quote
        function $(esc(:(MiniEvents.next_rate_event!)))(
			$(esc(:( alist :: MiniEvents.EventList{$ag_type, $l_type}))), rnum, $(esc(sim_name)))

            i, r = lookup($(esc(:alist)).sums, rnum)

            ag_actions = $(esc(:alist)).events[i]
            rates = ag_actions.rates
			$debug_code 
            $(esc(ag_name)) = ag_actions.agent

            $check_actions

            error("something went wrong ($r, $rates)!")
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
	delta_t = gensym("delta_t")

	interval = filter_sim(interval, sim_name)
	start = filter_sim(start, sim_name)

	repeat = if interval != nothing
			:($fun_name($ag_name, $sim_name, $interval))
		else
			:()
		end

	fn_body = isempty(action.args) ?
		:() : # return noop function if no actions are provided
		quote 
			MiniEvents.schedule_in!(agent, $delta_t, MiniEvents.get_scheduler($sim_name, $ag_type)) do $decl 
				$(filter_sim(filter_kills(filter_refreshs(action)), sim_name))
				$repeat
			end
		end	

	quote
		$(esc( quote
			function $fun_name(agent::$ag_type, $sim_name)
				$delta_t = $start
				$fn_body
			end
			# first interation starts at $start
			function $fun_name(agent::$ag_type, $sim_name, $delta_t)
				$fn_body
			end
		end ))
	end |> MacroTools.flatten
end

function generate_events_code(decl, conds, rates, actions, sched_exprs, debug = false)
    res = quote end

    fd = gen_event_count_fn(decl, length(conds))
    push!(res.args, fd)

    fd = gen_refresh_fn(decl)
    push!(res.args, fd)

    fd = gen_kill_fn(decl)
    push!(res.args, fd)

    fd = gen_calc_rate_fn(decl, conds, rates)
    push!(res.args, fd)

    fd = gen_rate_event_fn(decl, actions, debug)
    push!(res.args, fd)

	fd = gen_scheduled_action_fn(decl, sched_exprs...)
    push!(res.args, fd)
    
	res |> MacroTools.flatten
end

"Preprocess multi-rates."
function process_rates_for(rate_f, iter, expr_act)
	rate = :(sum($rate_f, $iter))
	
	sel = gensym("selected")
	act = quote
		s = @selnum()
		$sel = nothing
		for i in $iter
			r = $rate_f(i)
			if s < r
				$sel = i
				break
			end
			s -= r 
		end
		
		$(filter_sel(expr_act, sel))
	end
			
	rate, act
end


function parse_events(decl_agent, block)
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
	debug = false

	for line in block.args
        if @capture(line, @debug) || @capture(line, @debugevents)
			debug = true
		elseif @capture(line, @rate(expr_rate_) ~ expr_cond_ => expr_act_)
			push!(rates, expr_rate)
			push!(conds, expr_cond)
			push!(actions, expr_act)
		elseif @capture(line, @ratesfor(rate_fn_, iter_) ~ expr_cond_ => expr_act_)
			r, a = process_rates_for(rate_fn, iter, expr_act)
			push!(rates, r)
			push!(conds, expr_cond)
			push!(actions, a)
		# these are for convenience only, more sophisticated scenarios have to be done
		# manually using schedule!
		elseif @capture(line, @repeat(expr_interval_) => expr_action_)
			if has_sched
				error("only one schedule per type allowed")
			end
			sched_exprs = expr_interval, expr_interval, expr_action
			has_sched = true
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
	
	generate_events_code(decl_agent, conds, rates, actions, sched_exprs, debug)
end

"""
```Julia
@events(decl_agent, block)
```

Parse `block` to generate event code using agent declaration `decl_agent` (where `decl_agent` has the format `<name> :: <type>`).

`block` has to consist of a list of event declaration and directives (in no particular order). 

# Directives

## `@debug`

Emits code that compares the current value of an object's rates with the stored values. This is especially useful to catch cases where a required refresh (see below) was emitted.

# Event declarations

MiniEvents provides basic support for deterministic scheduling with `@repeat` and `@at`. Note that only *one* of these event types can occur per event declaration. In addition any number of stochastic events can be declared using `@rate` or `@ratesfor`.

## `@repeat`

```
@repeat(interval) => action
```
Repeat `action` every `interval` time units.
```@repeat(interval, start) => action```
Repeat `action` every `interval` time units, starting at `start`.

## `@at`

```
@at(start) => action
```
Run `action` exactly once at `start`. Note that future actions have to be scheduled manually. 

## `@rate`

```
@rate(rate) ~ condition => action
```
If `condition` is met perform `action` at rate `rate`. Note that rates and conditions are evaluated at the point of scheduling and need to be refreshed manually if the agent object's state changes (see below).

## `@ratesfor`

```
@ratesfor(function, iterator) ~ condition => action
```
This allows for the efficient scheduling of an event whose rate is the sum of a number of individual rates when it is important to know which particular individual rate was triggered. For example infection rate of an individual could depend on the number of its contacts, but in some cases we want to know *which* contact was the source of infection.

# Scheduling

The following pseudo macros are used to control scheduling. They generally take one or several arguments (each of which can be an iterable). They can only be used in action blocks.

## `@r`

Refresh an object. This updates the rate and condition. Objects without a rate event can't be refreshed.

## `@kill`

Remove an object from scheduling.

## `@spawn`

Add a new object to scheduling.

# Information

These macros provide access to some internal state of the scheduler and take no arguments. They can only be used in action blocks.

## `@sim`

Access the sim object.

## `@selected`

The selected item in `@ratesfor`. 

"""
macro events(decl_agent, block)
	parse_events(decl_agent, block)
end

