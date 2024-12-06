# MiniEvents.jl
Minimalistic event based simulation package.

MiniEvents provides a simple macro interface to describe continuous time, discrete events simulations. It uses an efficient simulation algorithm ([Köster et al., 2024](https://www.jasss.org/27/1/10.html)), so that even agent-based models with tens of thousands of agents can easily be handled. It also supports the simultaneous use of different agent types as well as deterministic events and doesn't require any special considerations in the simulation code itself.

## Example

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
