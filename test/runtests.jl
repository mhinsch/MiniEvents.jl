include("../src/MiniEvents.jl")
using .MiniEvents

include("../src/EventLists.jl")
using .EventLists

using Test

mutable struct A1
	state :: Int
end


update1!(a) = (a.state += 1)

update2!(a, n) = (a.state = n) 

@events agent::A1 begin
	@debug
	@rate(@sim().r1)	~ agent.state < 10			=> begin
		update1!(agent); @r agent end
	@repeat(1.0, 1.0)								=> begin
		update2!(agent, 0); @r agent end
end

@simulation Sim A1 begin
	r1 :: Float64
	r2 :: Float64
end


@testset "simple" begin
sim = Sim(1000.0, 0.0)
agent = A1(0)

spawn!(agent, sim)
next_event!(sim)

@test now(sim) < 1.0
@test agent.state == 1

step_until!(sim, 0.9)

@test now(sim) == 0.9
@test agent.state == 10

step_until!(sim, 1.0)

@test now(sim) == 1.0
@test agent.state == 0
end

mutable struct A2
	state :: Int
end


function update3!(agent, sim)
	schedule_dt!(agent, 1.0, sim) do a
		a.state = 1
		# refresh manually as we don't have access to pseudo macros at this point
		refresh!(a, sim)
	end
end

@events agent::A2 begin
	@debug
	@rate(@sim().r1)	~ agent.state > 0			=> begin
		update2!(agent, 3); @r agent end
	@at(1.0)									=> begin
		update3!(agent, @sim()); @r agent end
end

@simulation Sim2 A2 begin
	r1 :: Float64
end

@testset "manual schedule" begin
sim = Sim2(1000.0)
agent = A2(0)

spawn!(agent, sim)
# triggers @at(1.0), calls update3
next_event!(sim)

@test now(sim) == 1.0
@test agent.state == 0

# triggers function scheduled in update3 at 2.0
next_event!(sim)

@test now(sim) == 2.0
@test agent.state == 1

# triggers @rate(@sim().r1) as agent.state is now 1
next_event!(sim)

@test now(sim) > 2.0
@test agent.state == 3
end


mutable struct A3
	state :: Int
	r :: Float64
end

@events agent::A3 begin
	@debug
	@rate(agent.r)			~ true			=> begin
		update1!(agent); @r agent end
	@rate(100.0)		~ agent.state > 2		=> begin
		agent.state = -1
		@kill agent end
end

@simulation Sim3 A3 begin
end

@testset "agent removal" begin
sim = Sim3()
pop = A3[]
for i in 1:10
	push!(pop, A3(0, 1.0 + rand()))
	spawn!(pop[end], sim)
end

# run until an agent gets killed
while true
	next_event!(sim)
	if any(a -> a.state == -1, pop)
		break
	end
end

@test length(sim.alist_1.sums) == 9
@test length(sim.alist_1.events) == 9
@test all(ae -> ae.agent.state>-1, sim.alist_1.events)
@test all(i -> i <= 9, values(sim.alist_1.indices))

sum_all_stored = sim.alist_1.sums[1]
sum_all_real = sum(ae->ae.rates[end], sim.alist_1.events)

@test sum_all_stored - sum_all_real < 0.00001

end
			

mutable struct A4
	state :: Int
	r :: Float64
end

@events agent::A4 begin
	@debug
	@rate(agent.r)			~ true			=> begin
		update1!(agent); @r agent end
	@rate(100.0)		~ agent.state > 2		=> begin
		child = A4(-1, 100.0)
		push!(@sim().pop, child)
		@spawn child
		@r agent end
end

@simulation Sim4 A4 begin
	pop :: Vector{A4}
end

@testset "agent addition" begin
sim = Sim4([])
for i in 1:10
	push!(sim.pop, A4(0, 1.0 + rand()))
	spawn!(sim.pop[end], sim)
end

# run until an agent gets born
while true
	next_event!(sim)
	if any(a -> a.state == -1, sim.pop)
		break
	end
end

@test length(sim.alist_1.sums) == 11
@test length(sim.alist_1.events) == 11
@test any(ae -> ae.agent.state==-1, sim.alist_1.events)
@test any(i -> i == 11, values(sim.alist_1.indices))

sum_all_stored = sim.alist_1.sums[1]
sum_all_real = sum(ae->ae.rates[end], sim.alist_1.events)

@test sum_all_stored - sum_all_real < 0.00001


end
