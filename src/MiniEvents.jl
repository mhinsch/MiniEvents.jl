module MiniEvents

using MacroTools
using StaticArrays 
using Distributions
using Reexport

export @events, @simulation, refresh!, schedule!, spawn!, now


include("EventLists.jl")
using .EventLists

include("Scheduler.jl")
using .Scheduler


include("events.jl")
include("simulation.jl")


"Generic `refresh!` for iterables of agents."
function refresh!(agents, alist)
	for agent in agents
		change_rates!(agent, alist, calc_rates(agent))
	end
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


end # MiniEvents.jl
