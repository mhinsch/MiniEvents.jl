push!(LOAD_PATH, "./src")

using MiniEvents

@enum AState asleep awake

mutable struct A1
	id :: Int
	state :: AState
	food :: Int
end

mutable struct Model
	pop :: Vector{A1}
end

Model() = Model([])


A1(i) = A1(i, asleep, 0)


function wakeup(a::A1)
	println("$(a.id), $(a.food): wake up")
	a.state = awake
end

function sleep(a::A1)
	println("$(a.id), $(a.food): sleep")
	a.food -= 1
end

function walk(a::A1)
	println("$(a.id), $(a.food): walk")
	a.food -= 1
end

function forage(a::A1)
	println("$(a.id), $(a.food): forage")
	a.food += rand(1:5)
end

function fallasleep(a::A1)
	println("$(a.id), $(a.food): go to bed")
	a.state = asleep
end

function die(a::A1)
	println("$(a.id), $(a.food): dying")
end

@events agent::A1 begin
	@debug
	@rate(@sim().wake_rate)	~ agent.state == asleep			=> begin
		wakeup(agent); @r agent end
	@rate(1.0)	~ agent.state == asleep						=> begin
		sleep(agent); @r agent end
	@rate(0.5)	~ agent.state == awake && agent.food > 3	=> begin
		walk(agent); @r agent end
	@rate(1.0) 	~ agent.state == awake && agent.food <= 3	=> begin
		forage(agent); @r agent end
	@rate(3.0)	~ agent.state == awake && agent.food > 1	=> begin
		fallasleep(agent); @r agent end
	@rate(-agent.food * 2.0) ~ agent.food < 0				=> begin
		die(agent); @kill agent end
end

@events m::Model begin
	@debug
	@repeat((1.0/(length(@sim().model.pop)+1)), 0.1) 		=> begin
		if length(m.pop) < 10
			push!(m.pop, A1(length(m.pop)))
			spawn!(m.pop[end], @sim())
			t = now(@sim())
			println("$t: added agent")
			@r m.pop[end] # redundant, just to see if it works
 		end
	end
end

@simulation Sim A1 Model begin
	model :: Model
	wake_rate :: Float64
end

function setup(sim)
	spawn!(sim.model, sim)
end

function run(n, sim)
	for i in 1:n
		next_event!(sim)
	end
	println(now(sim))
end

const simulation = Sim(Model(), 1.5)

setup(simulation)

run(10, simulation)
