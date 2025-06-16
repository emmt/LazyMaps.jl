"""

`LazyMaps` provides lazily mapped arrays or collections for Julia.

"""
module LazyMaps

export
    LazyMap,
    lazymap

using TypeUtils
using TypeUtils: @public
@public result

struct LazyMapArray{T,N,F,A<:AbstractArray,L,B} <: AbstractArray{T,N}
    func::F # forward function, (callable or type, unused if type)
    data::A # input array
    invf::B # backward function (callable of unknown)
    LazyMapArray{T}(func::F, data::A, invf::B) where {T,N,F,A<:AbstractArray{<:Any,N},B} =
        new{T,N,F,A,IndexStyle(A) isa IndexLinear,B}(func, data, invf)
end

struct LazyMapOther{T,N,F,A}
    func::F # callable or type (unused if type)
    data::A # input collection
    LazyMapOther{T}(func::F, data::A) where {T,F,A} =
        new{T,infer_ndims(Base.IteratorSize(A)),F,A}(func, data)
end

const LazyMap{T,N,F,A} = Union{LazyMapArray{T,N,F,A},
                               LazyMapOther{T,N,F,A}}

# Singleton type to indicate unknown parameter or type.
struct Unknown end

"""
    B = lazymap([T::Type,] f, A)

yields a read-only array or collection `B` whose elements are given by the function `f`
applied *on-the-fly* to the elements of `A`. This is useful to provide an array or
collection with elementwise transformation by some function wit no needs for intermediate
storage.

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
element of `A`; while, in the latter case, it is given by `b = T(a)::T`. Note that, in
both cases, it is asserted that `b` is indeed of type `T`. The two cases are equivalent if
`T` is a number.

If `A` is an array and `invf` is a function, the call:

    B = lazymap([T::Type,] f, A, invf)

yields a lazy mapped array `B` whose elements can be set with the syntax `B[i] = x` and
with the side effect of modifying the corresponding element of `A` as if `A[i] = invf(x)`
has been evaluated. If the objective is to convert in the two direction between the
type `T` and `eltype(A)`, it sufficient to build `B` as `B = lazymap(T, A)`.

""" function lazymap end

# 2 arguments constructors.
lazymap(f, data) = lazymap(infer_eltype(f, data), f, data)
lazymap(::Type{T}, data::AbstractArray) where {T} = LazyMapArray{T}(pass, data, pass)
lazymap(::Type{T}, data::Any) where {T} = lazymap(T, pass, data)

# 3 arguments constructors.
@inline lazymap(f, data, invf) =
    data isa AbstractArray ? lazymap(infer_eltype(f, data), f, data, invf) :
    throw(ArgumentError("in `lazymap(f, data, invf)`, `data` must be an array"))
lazymap(::Type{T}, f, data::AbstractArray) where {T} = LazyMapArray{T}(f, data, Unknown())
lazymap(::Type{T}, f, data::Any) where {T} =  LazyMapOther{T}(f, data)

# 4 arguments constructor.
lazymap(::Type{T}, f, data::AbstractArray, invf) where {T} = LazyMapArray{T}(f, data, invf)

infer_eltype(func::F, data::A) where {F,A} =
    Base.IteratorEltype(A) isa Base.HasEltype ? Base.promote_op(func, eltype(A)) : Unknown

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

for shape in (:Dims,
              :(Tuple{Integer,Vararg{Integer}}),
              :(Tuple{Union{Integer,UnitRange{<:Integer}},
                      Vararg{Union{Integer,UnitRange{<:Integer}}}}))
    @eval Base.similar(m::LazyMapArray, ::Type{T}, shape::$shape) where {T} =
        similar(m.data, T, shape)
end

for (style, Idecl, Icall) in ((:IndexLinear,    :(i::Int),           :(i)),
                              (:IndexCartesian, :(I::Vararg{Int,N}), :(I...)))
    linear = (style === :IndexLinear)
    @eval begin
        Base.IndexStyle(::Type{<:LazyMapArray{T,N,F,A,$linear}}) where {T,N,F,A} =
            $style()
        @inline function Base.getindex(m::LazyMapArray{T,N,F,A,$linear},
                                       $Idecl) where {T,N,F,A}
            @boundscheck checkbounds(m, $Icall)
            x = @inbounds getindex(m.data, $Icall)
            return result(m, x)
        end
        @inline function Base.setindex!(m::LazyMapArray{T,N,F,A,$linear}, x,
                                        $Idecl) where {T,N,F,A}
            @boundscheck checkbounds(m.data, $Icall)
            unsafe_setindex!(m, x, $Icall)
            return m
        end
        function unsafe_setindex!(m::LazyMapArray{T,N,F,A,$linear,Unknown}, x,
                                  $Idecl) where {T,N,F,A}
            throw_read_only()
        end
        function unsafe_setindex!(m::LazyMapArray{T,N,F,A,$linear}, x,
                                  $Idecl) where {T,N,F,A}
            @inbounds setindex!(m.data, m.invf(x), $Icall)
        end
    end
end

"""
    LazyMaps.result(B, x) -> x′

yields the value returned by lazy map `B = lazymap([T,] f, A)` where the value of the
associated array or collection `A` is `x`. This method may be specialized based on the
type of the callable `f` provided it is :

    LazyMaps.result(B::LazyMap{T,N,typeof(f), x) where {T,N} = ...

"""
result(m::LazyMap{T,N,F,A}, x) where {T,N,F,A} = as(T, m.func(x))
result(m::LazyMap{T,N,DataType,A}, x) where {T,N,A} = T(x)::T
result(m::LazyMap{T,N,typeof(pass),A}, x) where {T,N,A} = T(x)::T

# Iterator and (partial) abstract array API for instances of LazyMapOther.
Base.eltype(m::LazyMapOther) = eltype(typeof(m))
Base.eltype(::Type{<:LazyMapOther{T}}) where {T} = T
Base.eltype(::Type{<:LazyMapOther{Unknown}}) = throw_unknown_eltype()

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
@noinline throw_unknown_eltype() =
    throw(ArgumentError("collection in lazy map has no defined element type"))

end
