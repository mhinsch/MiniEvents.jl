using Random
using BenchmarkTools

include("Events.jl")


# all possible states a person can be in
@enum Status susceptible infected recovered

# this is our agent type
mutable struct Person
    # state
    status :: Status
    # other agents this one can infect or be infected by
    contacts :: Vector{Person}
end

mutable struct World
	x :: Float64
end

# how we construct a person object
Person() = Person(susceptible, [])
Person(state) = Person(state, [])

mutable struct Simulation
    # and this is our population of agents
    pop :: Vector{Person}
	alist :: Vector{ActionList}
end


function step(sim)
	s = 0.0
	for al in sim.alist
		s += al.sums[1]
	end

	r = rand() * s

	for al in sim.alist
		if r <= al.sums[1]
			return step(al, r)
		end
		r -= al.sums[1]
	end
end
		

function step(alist::ActionList{World}, r)
	w = alist.actions[1].agent
	w.x = r

	schedule!(w, alist)
end

function step(alist::ActionList{Person}, r)
	#println("lu: ", r)

	i, r = lookup(alist.sums, r)

	#println("-> ", i, " ", r)

	ag = alist.actions[i]
	rs = ag.probs
	person = ag.agent

	sched = if (r -= rs[1]) < 0
		person.status = recovered
		[person; person.contacts]
	elseif (r -= rs[2]) < 0
		person.status = infected
		[person; person.contacts]
	elseif (r -= rs[3]) < 0
		person.status = recovered
		[person; person.contacts]
	elseif (r -= rs[4]) < 0
		person.status = susceptible
		[person]
	elseif r <= rs[5]
		person.status = infected
		[person; person.contacts]
	else
		println(r, ": ", rs)
		error("something went wrong!")
	end	
	
	for p in sched
		schedule!(p, alist)
	end
end

function calc_rates(person::Person)
	rates = zeros(Float64, 5)

	if person.status == infected
		rates[1] = 1e-3
	end
	if person.status == susceptible
		rates[2] = count(p -> p.status == infected, person.contacts)
	end
	if person.status == infected
		rates[3] = 1e-2
	end
	if person.status == recovered
		rates[4] = 1e-4
	end
	if person.status == susceptible
		rates[5] = 1e-6
	end

	rates
end

function calc_rates(w::World)
	[0.00001]
end

function schedule!(person, alist)
	change_rates!(person, alist, calc_rates(person))
end


function setup_grid(constr, xs, ys)
	# construct the population (contactless for now)
	pop = [ constr(susceptible) for i in 1:xs*ys ]

	# make a matrix to simplify finding neighbours
	matrix = reshape(pop, ys, :)

	for x in 1:xs, y in 1:ys
		p = matrix[y, x]
		if x > 1
			push!(p.contacts, matrix[y, x-1])
		end
		if y > 1
			push!(p.contacts, matrix[y-1, x])
		end
		if x < xs
			push!(p.contacts, matrix[y, x+1])
		end
		if y < ys
			push!(p.contacts, matrix[y+1, x])
		end
	end

	pop
end

function spawn(sim)
    # spawn activates agents
    # this function was generated by the model declaration
    for person in sim.pop
		add_agent!(person, sim.alist[1], calc_rates(person))
    end

	w = World(0.0)
	add_agent!(w, sim.alist[2], [0.0001])
end

function create_sim(xs, ys)
    pop = setup_grid(Person, xs, ys)

    # create a simulation object with parameter values
	sim = Simulation(pop, [ActionList{Person}(Dict(), [], []), ActionList{World}(Dict(), [], [])])
    
	sim.pop[1, 1].status = infected

	sim
end

function setup_sim(;xs, ys, seed, warmup=3000000)
	sim = create_sim(xs, ys)

	spawn(sim)

    # for reproducibility
    Random.seed!(seed)

	for i in 1:warmup
		step(sim)
	end

    sim
end


function run_sim(sim, n_steps)

	for i in 1:n_steps
		step(sim)
    end
end


function run_bench()
	i = 1
	for grid_size in [2, 4, 8, 16, 32, 64, 128, 256, 512]
		println("grid size: ", grid_size)
		@btime run_sim(sim, 500000) setup=(sim = setup_sim(xs=$grid_size, ys=$grid_size, seed=$i))
		i += 1
	end
end


run_bench()

