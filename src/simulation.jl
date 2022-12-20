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


@generated now(sim) = :(time_now(sim.$(sched_mem_name(1))))

"Reset waiting time."
function refresh_simulation!(sim)
	sim.t_next_evt  = now(sim)
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


function next_event! end
	
function gen_next_event_fn(n_alists, n_schedulers)
# *** waiting time
	
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
					return next_rate_event!(sim.$al_name, r, sim)
				end
				r -= $sname
			end)
	end
	push!(sal_args, :(error("ran out of options")))

	select_alist = Expr(:block, sal_args...)

# *** put everything together
	quote 
		function $(esc(:(MiniEvents.next_event!)))(sim) 		
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

	next_event_fn = gen_next_event_fn(length(types), length(types))

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

		$next_event_fn
    end |> MacroTools.flatten
end
