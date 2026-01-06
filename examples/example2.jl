using MiniEvents

@enum Status susceptible infected

mutable struct Person
    status :: Status
    contacts :: Vector{Person}
end

Person() = Person(susceptible, [])

infect!(person) = (person.status = infected)
heal!(person) = (person.status = susceptible)


struct World
    pop :: Vector{Person}
end


@kwdef struct Params
	r_inf :: Float64 = 1e-6
	r_rec :: Float64 = 1e-2
	t_inf :: Float64 = 1.0
end


@events person::Person begin
    # this is optional, it checks if activated objects have changed state since the last activation
    @debug

    @rate(count(p->p.status == infected, person.contacts) * @sim().pars.r_inf) ~
        # this is a boolean condition that determines whether the event can take place
        person.status == susceptible =>
        begin
            infect!(person)
            # all objects whose event rates are affected need to be refreshed
            @r person, person.contacts
        end

    @rate(@sim().pars.r_rec) ~
        person.status == infected =>
        begin
            heal!(person)
            @r person person.contacts
        end
end

@events world::World begin
	@debug

	@repeat(@sim().pars.t_inf) =>
	begin
		p = rand(world.pop)
		p.status = infected
		@r p
	end
end
 
# the simulation object has to be told about every type that has events associated with it
@simulation Model Person World begin
	world :: World
	pars :: Params
end


function step!(model)
    # find and execute the next event in line
    next_event!(model)
end

function MiniEvents.spawn!(model::Model)
    # before an object can receive events it needs to be activated
    # spawn_pop! activates a list of objects at once
    spawn_pop!(model.world.pop, model)
    spawn!(model.world, model)
end

function setup(;n = 100, ncontacts=500, pars = Params())
	model = Model(World([]), pars)
	for i in 1:n
		push!(model.world.pop, Person())
	end
	
	for i in 1:ncontacts
		a1 = rand(model.world.pop)
		while (a2 = rand(model.world.pop)) == a1
		end
		push!(a1.contacts, a2)
		push!(a2.contacts, a1)
	end

	model
end

# run the entire simulation time in one go
function run(model, t = 1000)
	spawn!(model)

	step_until!(model, t)

	model
end

# run one event at a time
function run_stepwise(model, t = 1000)
	spawn!(model)

	while now(model) < t
		step!(model)
	end

	model
end

