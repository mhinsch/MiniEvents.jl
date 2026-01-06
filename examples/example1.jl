using MiniEvents

@enum Status susceptible infected

mutable struct Person
    status :: Status
    contacts :: Vector{Person}
end

Person() = Person(susceptible, [])

infect!(person) = (person.status = infected)
heal!(person) = (person.status = susceptible)


@events person::Person begin
    # this is optional, it checks if activated objects have changed state since the last activation
    @debug

    @rate(count(p->p.status == infected, person.contacts) * 1e-6) ~
        # this is a boolean condition that determines whether the event can take place
        person.status == susceptible =>
        begin
            infect!(person)
            # all objects whose event rates are affected need to be refreshed
            @r person, person.contacts
        end

    @rate(1e-2) ~
        person.status == infected =>
        begin
            heal!(person)
            @r person person.contacts
        end
end
# 
# the simulation object has to be told about every type that has events associated with it
@simulation Sim Person

mutable struct Model
    pop :: Vector{Person}
    sim :: Sim
end

function step!(model)
    # find and execute the next event in line
    next_event!(model.sim)
end

function spawn!(model)
    # before an object can receive events it needs to be activated
    # spawn_pop! activates a list of objects at once
    spawn_pop!(model.pop, model.sim)
end

function setup(n = 100, ncontacts=500)
	model = Model([], Sim())
	for i in 1:n
		push!(model.pop, Person())
	end
	
	for i in 1:ncontacts
		a1 = rand(model.pop)
		while (a2 = rand(model.pop)) == a1
		end
		push!(a1.contacts, a2)
		push!(a2.contacts, a1)
	end

	model
end

# run the entire simulation time in one go
function run(model, t = 1000)
	spawn!(model)

	step_until!(model.sim, t)

	model
end

# run one event at a time
function run_stepwise(model, t = 1000)
	spawn!(model)
	while now(model.sim) < t
		step!(model)
	end
end

