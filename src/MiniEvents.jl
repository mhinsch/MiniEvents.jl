module MiniEvents

using MacroTools
using StaticArrays 
using Distributions
using Reexport

export @events, @simulation 
export refresh!, kill!, schedule!, spawn!, spawn_pop!, next_event!, step_time!, now


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


function spawn!(a, s) 
	scheduled_action!(a, s)
    EventLists.add_agent!(a, get_alist(s, typeof(a)), calc_rates(a, s))
end


function spawn_pop!(agents, s) 
	for a in agents
		scheduled_action!(a, s)
	    EventLists.add_agent!(a, get_alist(s, typeof(a)), calc_rates(a, s))
	end
end


function step_time!(sim, dt)
	t = now(sim) + dt
	while next_event!(sim, t) end
end

end # MiniEvents.jl
