module EventLists

export AgentEvents, EventList, lookup, change_rates!, add_agent!, remove_agent!

"Single agent and the rates of all actions it can do."
struct AgentEvents{AT, V}
	agent :: AT
	rates :: V
end

"All actions currently available to all agents of type `AT`."
mutable struct EventList{AT, V}
	"Map agent references to indices in the action vector."	
	indices :: Dict{AT, Int}
	"Sums of rates of subtrees."
	sums :: Vector{Float64}
	"All agents and their respective events."
	events :: Vector{AgentEvents{AT, V}}
end

function EventList{AT, V}() where {AT, V}
	EventList{AT, V}(Dict{AT, Int}(), Float64[], [])
end

sum_rates(a::AgentEvents{AT, V}) where {AT, V} = a.rates[end]
sum_rates(al::EventList{AT,V}) where {AT, V} = isempty(al.sums) ? 0.0 : al.sums[1]


left(idx) = 2*idx
right(idx) = 2*idx + 1
parent(idx) = idx ÷ 2


Base.haskey(el::EventList, agent) = haskey(el.indices, agent)

# TODO function of alist
# TODO select action, return action index
function lookup(sums, prob)
	if isempty(sums) || prob > sums[1]
		return 0, prob
	end

	lookup(sums, prob, 1)
end

function lookup(sums, prob, idx)
	sum_all = sums[idx]
	@assert prob <= sum_all

	#println(idx, ", ", prob)

	l = left(idx)
	sum_left = 0.0

	# check left child if present
	if l <= length(sums)
		sum_left = sums[l]
		if prob <= sum_left
			return lookup(sums, prob, l)
		end
	end

	r = right(idx)
	sum_right = 0.0
	if r <= length(sums)
		sum_right = sums[r]
	end

	# current node is it
	if prob <= sum_all-sum_right
		return idx, prob-sum_left
	end
	
	@assert r <= length(sums)

	# p            |
	# all [..........]
	#     [l.][m][r..]
	# => p' = p - l - m
	# m = all - l - r
	# => p' = p - l - all + l + r
	# => p' = p - all + r

	lookup(sums, prob - sum_all + sum_right, r)
end

function change_rates!(agent::T, alist::EventList{T, V}, rates::V) where {T,V}
	idx = alist.indices[agent]
	old_sum = sum_rates(alist.events[idx])	
	alist.events[idx] = AgentEvents(agent, rates)
	if rates[end] == old_sum
		return
	end

	trickle_up!(alist, idx)
end

function add_delta!(sums, idx, delta)
	sums[idx] += delta

	if idx == 1
		return
	end

	add_delta!(sums, parent(idx), delta)
end

function add_agent!(agent::T, alist::EventList{T, V}, rates::V) where {T,V}
	if length(rates) < 1 return end
		
	new_sum = rates[end]
	push!(alist.events, AgentEvents(agent, rates))
	push!(alist.sums, new_sum)
	alist.indices[agent] = length(alist.events)

	if new_sum == 0 || length(alist.events) == 1
		return
	end

	trickle_up!(alist, parent(length(alist.sums)))
end


function remove_agent!(agent, alist)
	removed_idx = alist.indices[agent]

	removed_evts = alist.events[removed_idx]
	removed_sumrates = sum_rates(removed_evts)

	moved_evts = alist.events[end]
	moved_sumrates = sum_rates(moved_evts)

	alist.events[removed_idx] = moved_evts
	pop!(alist.events)

	alist.indices[moved_evts.agent] = removed_idx
	pop!(alist.indices, agent)

	idx2 = parent(length(alist.sums))
	pop!(alist.sums)

	# that's it, all agents are dead
	if isempty(alist.sums)
		return
	end
	
	# might have been the last agent that got removed
	if removed_idx <= length(alist.sums)
		trickle_up!(alist, removed_idx)
	end
	trickle_up!(alist, idx2)

	@assert sum_rates(alist) >= 0
	nothing
end	


function update!(alist, idx)
	l = left(idx)
	sum_left = l <= length(alist.sums) ? alist.sums[l] : 0.0

	r = right(idx)	
	sum_right = r <= length(alist.sums) ? alist.sums[r] : 0.0
	
	alist.sums[idx] = sum_left + sum_right + sum_rates(alist.events[idx])
end


function trickle_up!(alist, idx)
	update!(alist, idx)

	while idx > 1
		idx = parent(idx)
		update!(alist, idx)
	end
end


function recalculate!(alist)
	if isempty(alist.sums)
		return
	end

	recalculate!(alist, 1)
end

function recalculate!(alist, idx)
	sum_left = left(idx) <= length(alist.sums) ?
		recalculate!(alist, left(idx)) : 0.0
	
	sum_right = right(idx) <= length(alist.sums) ?
		recalculate!(alist, right(idx)) : 0.0

	
	alist.sums[idx] = sum_left + sum_right + sum_rates(alist.events[idx])
end

end # module EventLists
