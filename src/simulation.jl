sched_mem_name(i) = Symbol("sched_$i")
alist_mem_name(i) = Symbol("alist_$i")
sum_name(i) = Symbol("sum_$i")
const VecType = SVector


function get_alist end
"Generates `get_alist(sim, type)` for given type."
function gen_get_alist_fn(ag_type, n)
	alist_name = alist_mem_name(n)
	quote $(esc(:(
			MiniEvents.get_alist(sim, ::Type{$ag_type}) = sim.$alist_name
	))) end 
end

"Obtain the scheduler belonging to a specific agent type."
function get_scheduler end
"Generates `get_scheduler(sim, type)` for given type."
function gen_get_scheduler_fn(ag_type, n)
	sched_name = sched_mem_name(n)
	quote $(esc(:(
			MiniEvents.get_scheduler(sim, ::Type{$ag_type}) = sim.$sched_name
	))) end
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


"Current time."
@generated now(sim) = :(time_now(sim.$(sched_mem_name(1))))


function gen_next_in_dtime_sched_fn(t_next_evt, sim, schedulers...)
	# *** for each scheduler get the time of next event,
	# *** find time of soonest event, then trigger that one and
	# *** advance all others
	
	# time of next event per scheduler
	t_expr = quote end
	t_args = t_expr.args	
	
	# soonest of all events 
	min_expr = :(min())
	min_args = min_expr.args
	
	# trigger soonest event or advance time
	step_min = quote end	
	step_args = step_min.args
	
	for i in eachindex(schedulers)
		tname = Symbol("t_$i")

		push!(t_args, :($tname = time_next(schedulers[$i])))

		push!(min_args, tname)

		push!(step_min.args, 
			quote
				sched = schedulers[$i]
				# if this is (one of) the scheduler(s) with the soonest event, run it
				if min_next == $tname
					next!(sched)
					triggered = true
				else
				# otherwise just change the time
					advance!(sched, min_next)
				end
			end)
	end

	# if t_next_evt is sooner than the next scheduled event
	# we simply advance all schedulers
	push!(min_args, :(t_next_evt))

	quote
		triggered = false
		# current time
		now = time_now(schedulers[1])
		
		# get scheduled times of all schedulers
		$t_expr
		
		# soonest scheduled event
		min_next = $min_expr

		# handle next scheduled event
		$step_min
		return triggered
	end |> MacroTools.flatten
end

"""Trigger next event with t<=t_next_evt. All schedulers are advanced to the time of that event or to t_next_evt otherwise."""
@generated function next_in_dtime_sched!(t_next_evt, sim, schedulers...)
	gen_next_in_dtime_sched_fn(t_next_evt, sim, schedulers...)
end


function next_in_dtime! end
	
"Generates overload to next_in_dtime! that only takes the sim object as argument."
function gen_next_in_dtime_fn(n_schedulers)
	next_dt_call = :(next_in_dtime_sched!(dt, sim))
	ndc_args = next_dt_call.args
	# add schedulers to argument list
	for i in 1:n_schedulers
		sname = sched_mem_name(i)
		push!(ndc_args, :(sim.$sname))
	end
	quote
		function $(esc(:(MiniEvents.next_in_dtime!)))(dt, sim)
			# call next_in_dtime_sched! with full list of schedulers
			$next_dt_call
		end
	end
end



function next_event! end
	
function gen_next_event_fn(n_alists, n_schedulers)
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
					next_rate_event!(sim.$al_name, r, sim)
					return true
				end
				r -= $sname
			end)
	end
	push!(sal_args, :(error("ran out of options")))

	select_alist = Expr(:block, sal_args...)

# *** put everything together
	quote 
		function $(esc(:(MiniEvents.next_event!)))(sim, max_t = Inf) 		
			# calc sum of rates
			$(gen_calc_sum_rates(n_alists))
			# rate event has been triggered or rates been changes
			if sim.t_next_evt <= now(sim) || sum != sim.sum_rates
				sim.t_next_evt  = now(sim) + rand(Exponential(1.0/sum))
			end
			sim.sum_rates = sum
			# check if next scheduled event is earlier than waiting time
			# if so execute it
			# advance now to waiting time or earliest event
			triggered = next_in_dtime!(min(max_t, sim.t_next_evt), sim)
			# we are still not at waiting time, so 
			# a scheduled event must have fired, do nothing
			if sim.t_next_evt > now(sim)
				#print("sim:$(sim.t_next_evt) ")
				return triggered
			end
			# now() == t_next_evt, so execute rate event
			$select_alist
			
			true
		end
	end |> MacroTools.flatten
end

const sim_syntax_error =
	"expected: @simulation <name> <type> [ <type>...] [ <declarations> ]"

macro simulation(name, args...)

	@assert length(args)>0 sim_syntax_error
	
	# check for user elements for sim type
	l_arg = args[end]
	if typeof(l_arg) == Expr &&	l_arg.head == :block 
		@assert length(args)>1 sim_syntax_error
		# just declarations
		add_decls = [esc(x) for x in rmlines(l_arg).args]
		types = args[1:end-1]
	else
		add_decls = []
		types = args
	end

	# struct declaration and members
    decl = :(mutable struct $name 
		$(add_decls...)
		t_next_evt  :: $(esc(:Float64))
		sum_rates :: $(esc(:Float64))
		end)
    members = decl.args[3].args
	
	# constructor and arguments
	constr_decl_args = [gensym("arg") for x in rmlines(add_decls)]
    constr_call_args = vcat(constr_decl_args, :(0.0), :(0.0))

	get_alist_fns = []

	# generate alist members for rate-based scheduling
    for (i, t) in enumerate(types)
        al_name = alist_mem_name(i)
        al_type = :(EventList{$(esc(t)), VecType{event_count($(esc(t))), $(esc(Float64))}})
        al_e = :($al_name :: $al_type)
        push!(members, al_e)
        push!(constr_call_args, :($al_type()))

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
		push!(constr_call_args, :($sched_type()))

		get_sched_fn = gen_get_scheduler_fn(t, i)
		push!(get_sched_fns, get_sched_fn) 
	end

	next_in_dtime_fn = gen_next_in_dtime_fn(length(types))

	next_event_fn = gen_next_event_fn(length(types), length(types))

	#sched_fn = :(Scheduler.schedule!(fun, obj, at, sim::$(esc(name))) =
	#	schedule_at!(fun, obj, at, get_scheduler(sim, typeof(obj))))

    quote
        $decl
        $(esc(name))($(constr_decl_args...)) = $(esc(name))($(constr_call_args...))

#		$(esc(:(
#			MiniEvents.num_agent_types(::Type{$name}) = $(length(types))
#		)))

		$(get_alist_fns...)
		$(get_sched_fns...)

		$next_in_dtime_fn

		#$sched_fn

		$next_event_fn
    end |> MacroTools.flatten
end
