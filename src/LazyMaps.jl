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
    LazyMap,
    lazymap

# Singleton type to indicate that the element type is unknown.
struct Unknown; end
const unknown = Unknown()
using TypeUtils

struct LazyMapArray{F,A<:AbstractArray,T,N} <: AbstractArray{T,N}
    f::F # function or callable
    a::A # input array/collection
    v::T # first value or `unknown`
end

struct LazyMapOther{F,A,T,N}
    f::F # function or callable
    a::A # input array/collection
end

const LazyMap{F,A,T,N} = Union{LazyMapArray{F,A,T,N},
                               LazyMapOther{F,A,T,N}}

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
lazymap(f, a::AbstractArray) = _lazymap(f, a, f(first(a)))
lazymap(f, a::AbstractArray, guess::Bool) =
    (guess ? lazymap(f, a) : _lazymap(f, a, unknown))
lazymap(f::F, a::A) where {F,A} = LazyMapOther{F,A,Unknown,0}(f, a)
lazymap(f::F, a::A, ::Bool) where {F,A} = LazyMapOther{F,A,Unknown,0}(f, a)

_lazymap(f::F, a::A, v::T) where {F,A<:AbstractArray{E,N},T} where {E,N} =
    LazyMapArray{F,A,T,N}(f, a, v)

Base.getindex(m::LazyMapArray{F,A,T,N}, key...) where {F,A,T,N} =
    m.f(m.a[key...]) :: T
Base.getindex(m::LazyMap{F,A,Unknown,N}, key...) where {F,A,N} =
    m.f(m.a[key...])

Base.eltype(m::LazyMap{F,A,T,N}) where {F,A,T,N} = T
Base.eltype(m::LazyMap{F,A,Unknown,N}) where {F,A,N} =
    error("element type is unknown")
Base.length(m::LazyMap) = length(m.a)
Base.ndims(m::LazyMapArray{F,A,T,N}) where {F,A,T,N} = N
Base.size(m::LazyMapArray) = size(m.a)
Base.size(m::LazyMapArray, d) = size(m.a, d)
Base.stride(m::LazyMapArray, k) = stride(m.a, k)
Base.strides(m::LazyMapArray) = strides(m.a)
Base.indices(m::LazyMapArray) = indices(m.a)
Base.indices(m::LazyMapArray, d) = indices(m.a, d)
Base.first(m::LazyMap{F,A,Unknown,N}) where {F,A,N} = m.f(first(m.a))
Base.first(m::LazyMapArray) = m.v
Base.last(m::LazyMap) = m.f(last(m.a))
Base.endof(m::LazyMap) = endof(m.a)
#Base.eachindex(m::LazyMap) = eachindex(m.a)
#Base.eachindex(m::LazyMap, args...) = eachindex(m.a, args...)

# Make an instance of a LazyMap an iterable.
Base.start(m::LazyMap) = start(m.a)
Base.done(m::LazyMap, state) = done(m.a, state)
function Base.next(m::LazyMap, state)
    item, state = next(m.a, state)
    return m.f(item), state
end

# Provide traits for LazyMap instances.
Base.IndexStyle(::Type{LazyMapArray{F,A,T,N}}) where {F,A,T,N} =
    Base.IndexStyle(A)
Base.iteratorsize(::Type{LazyMap{F,A,T,N}}) where {F,A,T,N} =
    Base.iteratorsize(A)
Base.iteratoreltype(::Type{LazyMap{F,A,Unknown,N}}) where {F,A,N} =
    Base.EltypeUnknown()
Base.iteratoreltype(::Type{LazyMap{F,A,T,N}}) where {F,A,T,N} =
    Base.HasEltype()

end
