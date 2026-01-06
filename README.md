# MiniEvents.jl

[![CI](https://github.com/mhinsch/MiniObserve.jl/actions/workflows/ci.yml/badge.svg)](https://github.com/mhinsch/MiniObserve.jl/actions/workflows/ci.yml)[![](https://img.shields.io/badge/docs-stable-blue.svg)](http://mhinsch.github.io/MiniEvents.jl/dev/README)

Minimalistic event based simulation package.

MiniEvents provides a simple macro interface to describe continuous time, discrete events simulations. It uses an efficient simulation algorithm ([Köster et al., 2024](https://www.jasss.org/27/1/10.html)), so that even agent-based models with tens of thousands of agents can easily be handled. It also supports the simultaneous use of different agent types as well as deterministic events and doesn't require any special considerations in the simulation code itself.

## Simple example

As an example let's implement a simple SI model.

We will start by defining the model without thinking about events or scheduling:

```Julia
@enum Status susceptible infected

mutable struct Person
    status :: Status
    contacts :: Vector{Person}
end

Person() = Person(susceptible, [])

infect!(person) = (person.status = infected)
heal!(person) = (person.status = susceptible)
```

At this point we have the basic workings of a model. Note that this is basically the same implementation we would use if we wanted to use step-wise updates.

To set up our event-based simulation we have to define events:

```Julia

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
```

Now we need to create the main simulation object that will contain the necessary state of the simulator.

```Julia
# the simulation object has to be told about every type that has events associated with it
@simulation Sim Person
```

This will provide all the machinery of the simulation, but to make this a usable simulation model we will have to add a bit more infrastructure:

```Julia
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
```

Everything else that would be needed for a working simulation (e.g. setup, output) can be implemented in exactly the same way as it would for step-wise updating.

## More advanced example

We can improve this example with a few convenience features. `@simulation` supports adding custom properties to the generated simulation type. This way we don't have to keep track of separate simulation and model objects and we can also easily add parameters. So, given these definitions:

```Julia
struct World
    pop :: Vector{Person}
end


@kwdef struct Params
	r_inf :: Float64 = 1e-6
	r_rec :: Float64 = 1e-2
	t_inf :: Float64 = 1.0
end
```

we can now define our simulation like this:

```Julia
@simulation Model Person World begin
	world :: World
	pars :: Params
end
```

Then we can replace our hard-coded rates in the event definition:

```Julia
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
```

Note that the pseudo macro `@sim` gives us access to the simulation object.

While we are at it we can now also define events for `World`:

```Julia
@events world::World begin
	@debug

	@repeat(@sim().pars.t_inf) =>
	begin
		p = rand(world.pop)
		p.status = infected
		@r p
	end
end
```

Just for the sake of demonstration we are using a different event type in this case - `@repeat` triggers events at exact time points instead of stochastically. This means we also don't have to refresh the world's rate with `@r` (we do have to refresh the agent we are changing, though).

With all this in place we can define our step and spawn methods. Since `Model` is now our simulation type there is no need any more to refer to `model.sim` separately.

```Julia
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
```
