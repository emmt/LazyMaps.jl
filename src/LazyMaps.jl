"""

`LazyMaps` provides lazily mapped arrays or collections for Julia.

"""
module LazyMaps

export
    LazyMap,
    lazymap

using TypeUtils

struct LazyMapArray{T,N,F,A<:AbstractArray,L} <: AbstractArray{T,N}
    func::F # callable or type (unused if type)
    data::A # input array
    LazyMapArray{T}(func::F, data::A) where {T,N,F,A<:AbstractArray{<:Any,N}} =
        new{T,N,F,A,IndexStyle(A) isa IndexLinear}(func, data)
end

struct LazyMapOther{T,N,F,A}
    func::F # callable or type (unused if type)
    data::A # input collection
    LazyMapOther{T}(func::F, data::A) where {T,F,A} =
        new{T,infer_ndims(Base.IteratorSize(A)),F,A}(func, data)
end

const LazyMapArrayLinear{T,N,F,A} = LazyMapArray{T,N,F,A,true}
const LazyMapArrayCartesian{T,N,F,A} = LazyMapArray{T,N,F,A,false}
const LazyMap{T,N,F,A} = Union{LazyMapArray{T,N,F,A},
                               LazyMapOther{T,N,F,A}}

"""
    B = lazymap([T::Type,] f, A)

yields a read-only array or collection `B` whose elements are given by the function `f`
applied *on-the-fly* to those of `A`. This is useful to provide an elementwise array or
collection filtered by some function without creating an intermediate array or collection.

Optional argument `T` is to specify the element type of `B`. If unspecified, it is
inferred from `f` and from the element type of `A`. The returned object `B` has
type-stable element type in the sense that its element have guaranteed type `T`, even
though `T` may be abstract.

The call:

    B = lazymap(T, identity, A)

builds an object `B` that lazily **converts** the elements of `A` to type `T`. By
contrast:

    B = lazymap(T, A)

lazily **constructs** an object of type `T` for each element of `A`. In the former case,
an element `b` of `B` is given by `b = convert(T, a)::T` with `a` the corresponding
element of `A` ; while, in the latter case, it is given by `b = T(a)::T`. Note that, in
both cases, it is asserted that `b` is indeed of type `T`. The two cases are equivalent if
`T` is a number.

"""
lazymap(func::F, data::A) where {F,A} = lazymap(infer_eltype(func, data), func, data)
lazymap(::Type{T}, func, data::AbstractArray) where {T} = LazyMapArray{T}(func, data)
lazymap(::Type{T}, func, data::Any) where {T} = LazyMapOther{T}(func, data)
lazymap(::Type{T}, data) where {T} = lazymap(T, pass, data)

infer_eltype(func::F, data::A) where {F,A} =
    Base.IteratorEltype(A) isa Base.HasEltype ? Base.promote_op(func, eltype(A)) : Any

# For collections, the shape traits are inferred according to the rules for tuples: if
# `IteratorSize(A)` yields `HasShape{N}()`, then `A` has a length, a number of dimensions,
# a size, and axes. Otherwise, if `IteratorSize(A)` yields `HasLength()`, then `A` has a
# length but no number of dimensions, size, nor axes.
infer_ndims(trait::Base.HasShape{N}) where {N} = N
infer_ndims(trait::Base.IteratorSize) = -1

# Dummy function for lazy maps `B = lazymap(T, A)` computing their output as `T(x)::T`,
# not as `convert(T, B.func(x))::T`. This function behaves like `identity` but has its own
# type. Using it to implement the `T(x)::T` behavior results in a smaller size for `B`
# which only stores one reference (to the data) instead of 2 (to the data and to `T`).
pass(x) = x

# Traits for LazyMap instances.
Base.IteratorSize(::Type{<:LazyMapOther{T,N,F,A}}) where {T,N,F,A} = Base.IteratorSize(A)
Base.IteratorEltype(::Type{<:LazyMapOther{T,N,F,A}}) where {T,N,F,A} = Base.IteratorEltype(A)

# Abstract array API for instances of LazyMapArray.
Base.length(m::LazyMapArray) = length(m.data)
Base.size(m::LazyMapArray) = size(m.data)
Base.axes(m::LazyMapArray) = axes(m.data)
Base.strides(m::LazyMapArray) = strides(m.data)
Base.stride(m::LazyMapArray, k::Integer) = stride(m.data, k)

Base.similar(m::LazyMapArray, ::Type{T}) where {T} = similar(m.data, T)
Base.similar(m::LazyMapArray, ::Type{T}, shape::Union{Dims,ArrayAxes}) where {T} =
    similar(m.data, T, shape)

for (S, Idecl, Icall) in ((:Linear,    :(i::Int),           :(i)),
                          (:Cartesian, :(I::Vararg{Int,N}), :(I...)))
    type  = Symbol("LazyMapArray", S)
    style = Symbol("Index", S)
    @eval begin
        Base.IndexStyle(::Type{<:$type}) = $style()
        @inline function Base.getindex(m::$type{T,N}, $Idecl) where {T,N}
            @boundscheck checkbounds(m, $Icall)
            x = @inbounds getindex(m.data, $Icall)
            return result(m, x)
        end
        @inline function Base.setindex!(m::$type{T,N}, x, $Idecl) where {T,N}
            @boundscheck checkbounds(A, $Icall)
            throw_read_only()
        end
    end
end

# Compute the value of a LazyMap element.
result(m::LazyMap{T,N,F,A}, x) where {T,N,F,A} = as(T, m.func(x))
result(m::LazyMap{T,N,DataType,A}, x) where {T,N,A} = T(x)::T
result(m::LazyMap{T,N,typeof(pass),A}, x) where {T,N,A} = T(x)::T

# Iterator and (partial) abstract array API for instances of LazyMapOther.
Base.eltype(m::LazyMapOther{T,N}) where {T,N} = T
Base.eltype(::Type{<:LazyMapOther{T,N}}) where {T,N} = T

Base.ndims(m::LazyMapOther) = ndims(typeof(m))
Base.ndims(::Type{<:LazyMapOther{T,N}}) where {T,N} = N
Base.ndims(::Type{<:LazyMapOther{T,-1}}) where {T} = throw_unknown_ndims()

Base.length(m::LazyMapOther) = _length(Base.IteratorSize(m), m.data)
_length(trait::Base.HasLength, data) = length(data)
_length(trait::Base.HasShape, data) = prod(_size(trait, data))
_length(trait::Base.IteratorSize, data) = throw_unknown_length()

Base.size(m::LazyMapOther) = _size(Base.IteratorSize(m), m.data)
_size(trait::Base.HasShape, data) = map(length, axes(data))
_size(trait::Base.IteratorSize, data) = throw_unknown_shape()

Base.axes(m::LazyMapOther) = _axes(Base.IteratorSize(m), m.data)
_axes(trait::Base.HasShape, data) = axes(data)
_axes(trait::Base.IteratorSize, data) = throw_unknown_shape()

# Make an instance of LazyMapOther an iterable.
Base.iterate(m::LazyMapOther) = _iterate_result(m, iterate(m.data))
Base.iterate(m::LazyMapOther, s) = _iterate_result(m, iterate(m.data, s))
_iterate_result(m::LazyMapOther{T}, r::Nothing) where {T} = nothing
_iterate_result(m::LazyMapOther{T}, r::Tuple{Any,Any}) where {T} =
    (result(m, r[1]), r[2])

@noinline throw_read_only() =
    throw(ArgumentError("attempt to write read-only lazily mapped array"))
@noinline throw_unknown_ndims() =
    throw(ArgumentError("collection in lazy map has no defined number of dimensions"))
@noinline throw_unknown_length() =
    throw(ArgumentError("collection in lazy map has no defined length"))
@noinline throw_unknown_shape() =
    throw(ArgumentError("collection in lazy map has no defined shape"))

end
