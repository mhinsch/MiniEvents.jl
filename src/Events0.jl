
struct Agent{AT}
	agent :: AT
	probs :: Vector{Float64}
	self_sum :: Float64
end

struct TreeNode
	sum :: Float64
	ag :: Agent
end

mutable struct Simulator
	indices :: Dict{Agent, Int}
	tree :: Vector{TreeNode}
end


left(idx) = 2*idx
right(idx) = 2*idx + 1
parent(idx) = idx 

function lookup(agent, tree, idx, prob)
	@assert prob <= tree[idx].sum
	sum_left = tree[left(idx)].sum
	if prob <= sum_left
		return lookup(agent, tree, left(idx), prob)
	end

	prob -= sum_left

	if prob <= tree[idx.ag.self_sum]
		return idx.ag
	end

	prob -= idx.ag.self_sum

	lookup(agent, tree, right(idx), prob)
end

function add!(agent, tree)
end
