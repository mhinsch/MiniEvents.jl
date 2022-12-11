using MacroTools


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


function gen_event_count_fn(decl, n)
    ag_type = decl.args[2]
    quote
        function $(esc(:event_count))(::Type{$ag_type}) 
            $n
        end
    end
end


function gen_refresh_fn(decl)
    ag_type = decl.args[2]
    quote
        function $(esc(:refresh!))(agent::$ag_type, alist)
            change_rates!(agent, alist, calc_rates(agent))
        end
    end
end


function gen_calc_rate_fn(decl, conds, rates)
    n = length(conds)
    VT = :(SVector{$n, Float64})

    con_call = :($VT())
    for (c, r) in zip(conds, rates)
        arg = :($(esc(c)) ? $(esc(r)) : 0.0)
        push!(con_call.args, arg)
    end

    quote
        function $(esc(:calc_rates))($(esc(decl))) 
            $con_call
        end
    end
end

function gen_step_fn(decl, actions)
    check_actions = quote end

    for (i, a) in enumerate(actions)
        check = :(if (r -= rates[$i]) < 0
                      $(esc(a))
                      return
                  end)
        push!(check_actions.args, check)
    end

    l_type = :(SVector{$(length(actions)), Float64})
    ag_name = decl.args[1]
    ag_type = decl.args[2]

    quote
        function $(esc(:step))(alist :: $(esc(:EventList)){$ag_type, $l_type}, rnum)
            i, r = lookup(alist.sums, rnum)

            ag_actions = alist.events[i]
            rates = ag_actions.rates
            $(esc(ag_name)) = ag_actions.agent

            $(esc(:refresh!))(p) = refresh!(p, alist)

            $check_actions

            error("somthing went wrong!")
        end
    end
end


function events(decl_agent, block, decl_world=nothing)
	block = rmlines(block)
	rates = []
	conds = []
	actions = []
	for line in block.args
		if !@capture(line, @rate(expr_rate_) ~ expr_cond_ => expr_act_)
			error("Event declarations expected: @event(<RATE>) ~ <COND> => <ACTION>")	
		end
		push!(rates, expr_rate)
		push!(conds, expr_cond)
		push!(actions, expr_act)
	end
	
	generate(decl_agent, conds, rates, actions)
end


function filter_refreshs!(actions)
	for (i, a) in enumerate(actions)
		actions[i] = MacroTools.postwalk(a) do x
			if @capture(x, @r(args__))
				ret = quote end
				ret.args = [:(refresh!($arg)) for arg in args]
				ret
			else
				x
			end
		end
	end
	nothing	
end


function generate(decl, conds, rates, actions)
    res = quote end

    fd = gen_event_count_fn(decl, length(conds))
    push!(res.args, fd)

    fd = gen_refresh_fn(decl)
    push!(res.args, fd)

    fd = gen_calc_rate_fn(decl, conds, rates)
    push!(res.args, fd)

	filter_refreshs!(actions)

    fd = gen_step_fn(decl, actions)
    push!(res.args, fd)

    res
end


macro simulation(name, types...)
    decl = :(struct $name end)
    members = decl.args[3].args

    constr = :($(esc(name))())
    args = constr.args

    step_call = :($(esc(:dispatch_step))())
    step_args = step_call.args

    aa_block = quote end
    aa_args = aa_block.args

    for (i, t) in enumerate(types)
        m_name = Symbol("m_$(i)")
        m_type = :(EventList{$t, SVector{event_count($t), Float64}})
        m_e = :($m_name :: $m_type)
        m_aa = :(Events.add_agent!(a :: $t, s :: $name) = 
                 Events.add_agent!(a, s.$m_name, calc_rates(a)))

        push!(members, m_e)
        push!(args, :($m_type()))
        push!(step_args, :(sim.$m_name))
        push!(aa_args, m_aa)
    end

    quote
        $decl
        $(esc(name))() = $constr

        $(esc(:step))(sim :: $name) = $step_call

        $aa_block
    end
end


macro events(decl_agent, block)
	events(decl_agent, block)
end


macro events(decl_agent, decl_world, block)
	events(decl_agent, block, decl_world)
end


