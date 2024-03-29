module MiniEvents

using MacroTools
using StaticArrays 
using Distributions
using Reexport

export @events, @simulation 
export refresh!, kill!, schedule!, spawn!, spawn_pop!, next_event!, step_dt!, step_until!, now
export schedule_dt!


include("EventLists.jl")
using .EventLists

include("Scheduler.jl")
using .Scheduler


include("events.jl")
include("simulation.jl")


"Recalculate event rates for agents. Generic version for iterables of agents."
function refresh!(agents, sim)
	#println("RA: $(length(agents))")
	# this is slightly less efficient than calling change_rates directly
	# but this way heterogeneous collections are supported as well
	for agent in agents
		refresh!(agent, sim)
	end
end

"Remove all agent events (rates as well as scheduled). Generic version for iterables of agents."
function kill!(agents, sim)
	for agent in agents
		kill!(agent, sim)
	end
end

"Activate agent by adding its events to the scheduler."
function spawn!(a, s) 
	scheduled_action!(a, s)
    EventLists.add_agent!(a, get_alist(s, typeof(a)), calc_rates(a, s))
end

"Spawn an entire population of agents."
function spawn_pop!(agents, s) 
	for a in agents
		scheduled_action!(a, s)
	    EventLists.add_agent!(a, get_alist(s, typeof(a)), calc_rates(a, s))
	end
end

"Process events until `now(sim) >= t`."
function step_until!(sim, t)
	while next_event!(sim, t) end
end

"Process events until time has increased by `dt`."
function step_dt!(sim, dt)
	step_until!(sim, now(sim) + dt)
end

"Schedule `fun(obj)` to be triggered at time `at`."
@generated function schedule!(fun, obj, at, sim)
	:(schedule_at!(fun, obj, at, get_scheduler(sim, $obj)))
end

"Schedule `fun(obj)` to be triggered at `now(sim) + dt`."
schedule_dt!(fun, obj, dt, sim) = schedule!(fun, obj, now(sim)+dt, sim)

end # MiniEvents.jl
