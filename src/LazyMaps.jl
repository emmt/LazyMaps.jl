#
# LazyMaps.jl -
#
# Implementation of lazy maps for Julia.
#
#------------------------------------------------------------------------------
#
# This file is part of the `LazyMaps.jl` package licensed under the MIT
# "Expat" License.
#
# Copyright (C) 2017, Éric Thiébaut.
#

isdefined(Base, :__precompile__) && __precompile__(true)

module LazyMaps

export
    lazymap

# Singleton type to indicate that the element type is unknown.
struct Unknown; end
const unknown = Unknown()

struct LazyMap{F <: Function, A, T}
    f::F # function
    a::A # input array/collection
    v::T # first value or `unknown`
end

"""
    lazymap(f, a [, guess=true])

yields a read-only collection whose items are given by the function `f`
applied *on-the-fly* to the items of `a`.  This is useful to provide an
elementwise filtered array or collection to some method without creating an
intermediate array or collection.

If `guess` is `true`, the method attempts to evaluate `f` for the first
item of `a` to determine the type of the elements.  Set `guess` to `false`
to avoid this at the cost of making some optimizations not possible.  Note
that if the element type can be determined, it is assumed (and asserted)
that all elements have the same type, *i.e.* the elements of result are
**type stable**.

See also [`map`](@ref).

"""
lazymap(f, a) = _lazymap(f, a, Base.iteratorsize(a))
lazymap(f, a, guess::Bool) =
    (guess ? lazymap(f, a) : LazyMap(f, a, unknown))

_lazymap(f, a, ::Union{Base.HasLength,Base.HasShape}) =
    LazyMap(f, a, length(a) ≥ 1 ? f(first(a)) : unknown)
_lazymap(f, a, ::Base.IsInfinite) =
    LazyMap(f, a, f(first(a)))
_lazymap(f, a, ::Base.SizeUnknown) =
    LazyMap(f, a, f(first(a)))

Base.getindex(m::LazyMap{F,A,T}, key...) where {F,A,T} = m.f(m.a[key...]) :: T
Base.getindex(m::LazyMap{F,A,Unknown}, key...) where {F,A} = m.f(m.a[key...])
Base.eltype(m::LazyMap{F,A,T}) where {F,A,T} = T
Base.eltype(m::LazyMap{F,A,Unknown}) where {F,A} = error("element type is unknown")
Base.length(m::LazyMap) = length(m.a)
Base.ndims(m::LazyMap) = ndims(m.a)
Base.size(m::LazyMap) = size(m.a)
Base.size(m::LazyMap, d) = size(m.a, d)
Base.stride(m::LazyMap, k) = stride(m.a, k)
Base.strides(m::LazyMap) = strides(m.a)
Base.indices(m::LazyMap) = indices(m.a)
Base.indices(m::LazyMap, d) = indices(m.a, d)
Base.first(m::LazyMap{F,A,Unknown}) where {F,A} = m.f(first(m.a))
Base.first(m::LazyMap) = m.v
Base.last(m::LazyMap) = m.f(last(m.a))
Base.endof(m::LazyMap) = endof(m.a)

# Make an instance of a LazyMap an iterable.
Base.start(m::LazyMap) = start(m.a)
Base.done(m::LazyMap, state) = done(m.a, state)
function Base.next(m::LazyMap, state)
    item, state = next(m.a, state)
    return m.f(item), state
end

# Provide traits for LazyMap instances.
Base.IndexStyle(::Type{LazyMap{F,A,T}}) where {F,A,T} =
    Base.IndexStyle(A)
Base.iteratorsize(::Type{LazyMap{F,A,T}}) where {F,A,T} =
    Base.iteratorsize(A)
Base.iteratoreltype(::Type{LazyMap{F,A,Unknown}}) where {F,A} =
    Base.EltypeUnknown()
Base.iteratoreltype(::Type{LazyMap{F,A,T}}) where {F,A,T} =
    Base.HasEltype()

end
