module Scheduler

export PQScheduler, isempty, schedule_at!, time_now, time_next, schedule_in!, next!, upto!, unschedule!, reset!, advance!


using DataStructures

using DocStringExtensions

"A simple scheduler based on PriorityQueue."
mutable struct PQScheduler{TIME, OBJ}
	queue :: PriorityQueue{OBJ, TIME}
	actions :: Dict{OBJ, Function}
	now :: TIME
end

"Construct an empty PQScheduler with a give TIME type."
PQScheduler{TIME, OBJ}() where {TIME, OBJ} = PQScheduler{TIME, OBJ}(
	PriorityQueue{OBJ, TIME}(), Dict{OBJ, Function}(), TIME(0))

time_type(sched::PQScheduler{TIME, OBJ}) where {TIME, OBJ} = TIME

"""
$(SIGNATURES)

Returns true if the scheduler does not contain any actions.
"""
Base.isempty(scheduler::PQScheduler) = isempty(scheduler.queue)

Base.haskey(scheduler::PQScheduler, obj) = haskey(scheduler.actions, obj)

"""
$(SIGNATURES)

Add a single item to the scheduler. Adds function `fun` to be called on `obj` at time `at` to `scheduler`.
"""
function schedule_at!(fun, obj, at, scheduler)
	scheduler.queue[obj] = at
	scheduler.actions[obj] = fun
#	println("<- ", at)
end

"""
$(SIGNATURES)

Time stamp of the last action that was executed by `scheduler`.
"""
time_now(scheduler) = scheduler.now

"""
$(SIGNATURES)

Time stamp of the next action to be executed by `scheduler` or `time_now` if it is empty.
"""
time_next(scheduler) = isempty(scheduler) ? time_type(scheduler)(Inf) : first(scheduler.queue)[2]

"""
$(SIGNATURES)

Advance time to `t`. Caution, this does not check for consistency!
"""
function advance!(scheduler, t)
	scheduler.now = t
end

"""
$(SIGNATURES)

Add a single item (`fun` to be called on `obj`) at `wait` time from now to `scheduler`.
"""
function schedule_in!(fun, obj, wait, scheduler)
	t = time_now(scheduler) + wait
	schedule_at!(fun, obj, t, scheduler)
end


"""
$(SIGNATURES)

Run the next action in `scheduler` or do nothing if empty. Returns the action's return value.
"""
function next!(scheduler)
#	println("! ", scheduler.now)

	if isempty(scheduler)
		return
	end

	obj, time = first(scheduler.queue)

	scheduler.now = time
	popfirst!(scheduler.queue)
	fun = scheduler.actions[obj]
	delete!(scheduler.actions, obj)
	fun(obj)
end

# we could implement this using repeated calls to next but that
# would require redundant calls to first
"""
$(SIGNATURES)

Run actions in `scheduler` up to time `atime`. Returns the scheduler.
"""
function upto!(scheduler, atime)
#	println("! ", scheduler.now, " ... ", time)

	while !isempty(scheduler)
		obj, time = first(scheduler.queue)

		if time > atime
			scheduler.now = atime
			break
		end

		scheduler.now = time
		popfirst!(scheduler.queue)
		fun = scheduler.actions[obj]
		delete!(scheduler.actions, obj)
		fun(obj)
	end

	scheduler
end

"""
$(SIGNATURES)

Remove action for `obj` from `scheduler`.
"""
function unschedule!(scheduler, obj)
	delete!(scheduler.queue, obj)
	delete!(scheduler.actions, obj)
end

"""
$(SIGNATURES)

Remove all actions from `scheduler` and reset time to 0.
"""
function reset!(scheduler)
	empty!(scheduler.actions)
	empty!(scheduler.queue)
	scheduler.now = typeof(scheduler.now)(0)
end


# *** alternative, simpler implementation
# this is actually substantially slower with more memory allocation

#=
mutable struct PQScheduler2{TIME}
	queue :: PriorityQueue{Any, Tuple{TIME, Function}}
	now :: TIME
end

PQScheduler2{TIME}() where {TIME} = PQScheduler2{TIME}(
	PriorityQueue{Any, Tuple{TIME, Function}}(), TIME(0))


Base.isempty(scheduler::PQScheduler2{TIME}) where {TIME} = isempty(scheduler.queue)

"add a single item"
function schedule!(fun, obj, at, scheduler::PQScheduler2{T}) where{T}
	scheduler.queue[obj] = (at, fun)
#	println("<- ", at)
end


time_next(scheduler::PQScheduler2{T}) where{T} = isempty(scheduler) ? scheduler.now : first(scheduler.queue)[2][1]


"run the next action"
function next!(scheduler::PQScheduler2{T}) where{T}
#	println("! ", scheduler.now)

	if isempty(scheduler)
		return
	end

	obj, (time, fun) = first(scheduler.queue)

	scheduler.now = time
	popfirst!(scheduler.queue)
	fun(obj)
end

# we could implement this using repeated calls to next but that
# would require redundant calls to first
"run actions up to `time`"
function upto!(scheduler::PQScheduler2{T}, atime) where{T}
#	println("! ", scheduler.now, " ... ", time)

	while !isempty(scheduler)
		obj, (time, fun) = first(scheduler.queue)

		if time > atime
			scheduler.now = atime
			break
		end

		scheduler.now = time
		popfirst!(scheduler.queue)
		fun(obj)
	end

	scheduler
end

function unschedule!(scheduler::PQScheduler2{T}, obj::Any) where{T}
	delete!(scheduler.queue, obj)
end

function reset!(scheduler::PQScheduler2{T}) where{T}
	empty!(scheduler.queue)
	scheduler.time = typeof(scheduler.time)(0)
end
=#
end
