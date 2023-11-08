include("../src/MiniEvents.jl")
using .MiniEvents

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
