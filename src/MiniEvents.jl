module MiniEvents

using MacroTools
using StaticArrays 
using Distributions
using Reexport

export @events, @simulation, refresh!, schedule!, spawn!, spawn_pop!, next_event!, now


include("EventLists.jl")
using .EventLists

include("Scheduler.jl")
using .Scheduler


include("events.jl")
include("simulation.jl")


"Generic `refresh!` for iterables of agents."
function refresh!(agents, sim)
	# this is slightly less efficient than calling change_rates directly
	# but this way heterogeneous collections are supported as well
	for agent in agents
		refresh!(agent, sim)
	end
end


function spawn!(a, s) 
	scheduled_action!(a, s)
    EventLists.add_agent!(a, get_alist(s, typeof(a)), calc_rates(a, s))
	# rates have changed => redraw waiting time
	refresh_simulation!(s)
end


function spawn_pop!(agents, s) 
	for a in agents
		scheduled_action!(a, s)
	    EventLists.add_agent!(a, get_alist(s, typeof(a)), calc_rates(a, s))
	end
	# rates have changed => redraw waiting time
	refresh_simulation!(s)
end


end # MiniEvents.jl
