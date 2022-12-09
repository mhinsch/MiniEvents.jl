
include("Events.jl")
using .Events


"Dispatch over several action lists (of different type)."
@generated function dispatch_step(alists...)
	n = length(alists)

	expr = quote end
	
	for i in 1:n
		sname = Symbol("sum_$i")
		push!(expr.args, :($sname = Events.sum_rates(alists[$i])))
	end

	sum_exp = Expr(:call, :+)
	for i in 1:n
		sname = Symbol("sum_$i")
		push!(sum_exp.args, sname)
	end

	push!(expr.args, :(r = rand() * ($sum_exp)))

	for i in 1:n
		sname = Symbol("sum_$i")
		push!(expr.args, 
			quote
				if r <= $sname
					return step(alists[$i], r)
				end
				r -= $sname
			end)
	end
	push!(expr.args, :(error("ran out of options")))

	expr
end
	

function refresh!(agents, alist)
	for agent in agents
		change_rates!(agent, alist, calc_rates(agent))
	end
end

