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


function A1(i)
	A1(i, asleep, 0)
end


function wakeup(a::A1)
	println("$(a.id): wake up")
	a.state = awake
end

function sleep(a::A1)
	println("$(a.id): sleep")
	a.food -= 1
end

function walk(a::A1)
	println("$(a.id): walk")
	a.food -= 1
end

function forage(a::A1)
	println("$(a.id): forage")
	a.food += rand(1:5)
end

function fallasleep(a::A1)
	println("$(a.id): go to bed")
	a.state = asleep
end

@events self::A1 begin
	@debug
	@rate(@sim().wake_rate)	~ self.state == asleep				=> begin
		wakeup(self); @r self end
	@rate(1.0)	~ self.state == asleep				=> begin
		sleep(self); @r self end
	@rate(0.5)	~ self.state == awake && self.food > 3	=> begin
		walk(self); @r self end
	@rate(1.0) 	~ self.state == awake && self.food <= 3	=> begin
		forage(self); @r self end
	@rate(3.0)	~ self.state == awake && self.food > 1	=> begin
		fallasleep(self); @r self end
end

@events m::Model begin
	@debug
	@repeat((1.0/(length(@sim().model.pop)+1)), 0.1) => begin
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
