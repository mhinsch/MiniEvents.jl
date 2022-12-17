using Random
using BenchmarkTools
using StaticArrays
using Distributions

include("MiniEvents.jl")

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


@events world::World begin
	@rate(0.00001) ~ true => (world.x = 1)
end

@events person::Person begin
    @rate(count(p -> p.status == infected, person.contacts) + 1e-6) ~
	    person.status == susceptible =>
 	    begin
			person.status = infected

			@r person person.contacts
		end

    @rate(1e-2) ~
	    person.status == infected =>
	    begin
			person.status = recovered

			@r person person.contacts
		end

    @rate(1e-4) ~
	    person.status == recovered =>
	    begin
			person.status = susceptible

			@r person
	    end
end

@simulation Sim Person World


mutable struct Simulation
    # and this is our population of agents
    pop :: Vector{Person}
	sim :: Sim
end

function step(sim)
	step!(sim.sim)
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

function spawn(sim::Simulation)
    # spawn activates agents
    # this function was generated by the model declaration
    for person in sim.pop
		spawn!(person, sim.sim)
    end

	w = World(0.0)
	spawn!(w, sim.sim)
end

function create_sim(xs, ys)
    pop = setup_grid(Person, xs, ys)

    # create a simulation object with pARAMEter values
    sim = Simulation(pop, Sim())
    
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

function check_sum(n)
	# check correctness
	c = 0
	for i in 1:n
		sim = setup_sim(xs=100, ys=100, seed=i+41)
		run_sim(sim, 500000)
		c += count(p->p.status==recovered, sim.pop)
		print(".")
	end
	println; println(c/n)
end

function run_bench()
	i = 1
	for grid_size in [2, 4, 8, 16, 32, 64, 128, 256, 512]
		println("grid size: ", grid_size)
		@btime run_sim(sim, 500000) setup=(sim = setup_sim(xs=$grid_size, ys=$grid_size, seed=$i))
		i += 1
	end
end

check_sum(1)
run_bench()


