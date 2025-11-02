"""

`LazyMaps` provides lazily mapped arrays or collections for Julia. Typically:

```julia
B = lazymap([T::Type,] f, A[, f_inv])
```

yields a view of `A` (an iterator or an array) such that the `i`-th element of `B` is given
by `Bᵢ = f(Aᵢ)` with `Aᵢ` the `i`-th element of `A`. If `T` is specified, `Bᵢ = convert(T,
f(Aᵢ))`. If `A` is an array, then the inverse `f_inv` of `f` may be specified to have a
read-write view.

"""
module LazyMaps

export
    LazyMap,
    lazymap

using TypeUtils
using TypeUtils: @public
@public result

struct LazyMapArray{T,N,F,A<:AbstractArray,L,I} <: AbstractArray{T,N}
    f::F     # callable or type (unused if type)
    arg::A   # input array
    f_inv::I # inverse function (callable or unknown)
    LazyMapArray{T}(f::F, arg::A, f_inv::I) where {T,N,F,A<:AbstractArray{<:Any,N},I} =
        new{T,N,F,A,IndexStyle(A) isa IndexLinear,I}(f, arg, f_inv)
end

struct LazyMapOther{T,N,F,A}
    f::F   # callable or type (unused if type)
    arg::A # input collection
    LazyMapOther{T}(f::F, arg::A) where {T,F,A} =
        new{T,infer_ndims(Base.IteratorSize(A)),F,A}(f, arg)
end

const LazyMap{T,N,F,A} = Union{LazyMapArray{T,N,F,A},
                               LazyMapOther{T,N,F,A}}

# Singleton type to indicate unknown parameter or type.
struct Unknown end

"""
    B = lazymap([T::Type,] f, A)

Build a view of the array or iterator `A` such that the `i`-th element of `B` is given by
`Bᵢ = f(Aᵢ)` with `Aᵢ` the `i`-th element of `A`.

Optional argument `T` is to specify the element type of `B`. If unspecified, it is inferred
from `f` and from the element type of `A`. The returned object `B` has type-stable element
type in the sense that its element have guaranteed type `T`, even though `T` may be
abstract.

The call:

    B = lazymap(T::Type, identity, A)

builds an object `B` that lazily **converts** the elements of `A` to type `T`. The `i`-th
element of `B` is given by `Bᵢ = as(T, Aᵢ)`.

By contrast:

    B = lazymap(T::Type, A)

lazily **constructs** an object of type `T` for each element of `A`. The `i`-th element of
`B` is given by `Bᵢ = T(Aᵢ)::T`.

Note that, in both cases, it is asserted that `Bᵢ` is of type `T`. The two cases are
equivalent if `T` is a numeric type (a sub-type of `Number`).

If `A` is an array, then:

    B = lazymap([T::Type,] f, A, f_inv)

yields a lazy mapped array `B` whose elements can be set with the syntax `B[i] = x` and with
the side effect of modifying the corresponding element of `A` as if `A[i] = f_inv(x)` has
been evaluated. If the objective is to convert in the two directions between the type `T`
and `eltype(A)`, it sufficient to build `B` as `B = lazymap(T, A)`.

"""
lazymap(f, arg::Any) = lazymap(infer_eltype(f, arg), f, arg)
function lazymap(f, arg::Any, f_inv)
    arg isa AbstractArray || throw(ArgumentError(
        "in `lazymap(f, arg, f_inv)`, `arg` must be an array"))
    return lazymap(infer_eltype(f, arg), f, arg, f_inv)
end
lazymap(::Type{T}, arg::AbstractArray) where {T} = lazymap(T, pass, arg, pass)
lazymap(::Type{T}, arg::Any) where {T} = lazymap(T, pass, arg)
lazymap(::Type{T}, f, arg::AbstractArray) where {T} = lazymap(T, f, arg, Unknown())
lazymap(::Type{T}, f, arg::AbstractArray, f_inv) where {T} = LazyMapArray{T}(f, arg, f_inv)
lazymap(::Type{T}, f, arg::Any) where {T} = LazyMapOther{T}(f, arg)
lazymap(::Type{T}, f, arg::Any, f_inv) where {T} = throw(ArgumentError(
    "in `lazymap(T, f, arg, f_inv)`, `arg` must be an array"))

infer_eltype(f, arg::AbstractArray) = Base.promote_op(f, eltype(arg))
infer_eltype(f, arg::Any) =
    Base.IteratorEltype(arg) isa Base.HasEltype ? Base.promote_op(f, eltype(arg)) : Unknown

# For collections, the shape traits are inferred according to the rules for tuples: if
# `IteratorSize(A)` yields `HasShape{N}()`, then `A` has a length, a number of dimensions,
# a size, and axes. Otherwise, if `IteratorSize(A)` yields `HasLength()`, then `A` has a
# length but no number of dimensions, size, nor axes.
infer_ndims(trait::Base.HasShape{N}) where {N} = N
infer_ndims(trait::Base.IteratorSize) = -1

# Dummy function for lazy maps `B = lazymap(T, A)` computing their output as `T(x)::T`,
# not as `convert(T, B.f(x))::T`. This function behaves like `identity` but has its own
# type. Using it to implement the `T(x)::T` behavior results in a smaller size for `B`
# which only stores one reference (to the data) instead of 2 (to the data and to `T`).
pass(x) = x

# Abstract array API for instances of LazyMapArray.

Base.length(m::LazyMapArray) = length(m.arg)
Base.size(m::LazyMapArray) = size(m.arg)
Base.axes(m::LazyMapArray) = axes(m.arg)
Base.strides(m::LazyMapArray) = strides(m.arg)
Base.stride(m::LazyMapArray, k::Integer) = stride(m.arg, k)

for shape in (:Dims,
              :(Tuple{Integer,Vararg{Integer}}),
              :(Tuple{Union{Integer,UnitRange{<:Integer}},
                      Vararg{Union{Integer,UnitRange{<:Integer}}}}))
    @eval Base.similar(m::LazyMapArray, ::Type{T}, shape::$shape) where {T} =
        similar(m.arg, T, shape)
end

for (style, Idecl, Icall) in ((:IndexLinear,    :(i::Int),           :(i)),
                              (:IndexCartesian, :(I::Vararg{Int,N}), :(I...)))
    linear = (style === :IndexLinear)
    @eval begin
        Base.IndexStyle(::Type{<:LazyMapArray{T,N,F,A,$linear}}) where {T,N,F,A} = $style()
        @inline function Base.getindex(m::LazyMapArray{T,N,F,A,$linear},
                                       $Idecl) where {T,N,F,A}
            @boundscheck checkbounds(m, $Icall)
            x = @inbounds getindex(m.arg, $Icall)
            return result(m, x)
        end
        @inline function Base.setindex!(m::LazyMapArray{T,N,F,A,$linear}, x,
                                        $Idecl) where {T,N,F,A}
            @boundscheck checkbounds(m.arg, $Icall)
            unsafe_setindex!(m, x, $Icall)
            return m
        end
        function unsafe_setindex!(m::LazyMapArray{T,N,F,A,$linear,Unknown}, x,
                                  $Idecl) where {T,N,F,A}
            throw_read_only()
        end
        function unsafe_setindex!(m::LazyMapArray{T,N,F,A,$linear}, x,
                                  $Idecl) where {T,N,F,A}
            @inbounds setindex!(m.arg, m.f_inv(x), $Icall)
        end
    end
end

"""
    LazyMaps.result(B, x) -> x′

Return the value returned by lazy map `B = lazymap([T,] f, A)` where the value of the
associated array or collection `A` is `x`. This method may be specialized based on the
type of the callable `f` provided it is:

    LazyMaps.result(B::LazyMap{T,N,typeof(f), x) where {T,N} = ...

"""
result(m::LazyMap{T,N,F,A}, x) where {T,N,F,A} = as(T, m.f(x))
result(m::LazyMap{T,N,DataType,A}, x) where {T,N,A} = T(x)::T
result(m::LazyMap{T,N,typeof(pass),A}, x) where {T,N,A} = T(x)::T

# Iterator and (partial) abstract array API for instances of LazyMapOther.
Base.IteratorEltype(::Type{<:LazyMapOther{T,N,F,A}}) where {T,N,F,A} =
    Base.HasEltype()
Base.IteratorEltype(::Type{<:LazyMapOther{T,N,F,A}}) where {T<:Unknown,N,F,A} =
    Base.EltypeUnknown()

Base.IteratorSize(::Type{<:LazyMapOther{T,N,F,A}}) where {T,N,F,A} = Base.IteratorSize(A)

Base.eltype(m::LazyMapOther) = eltype(typeof(m))
Base.eltype(::Type{<:LazyMapOther{T}}) where {T} = T
Base.eltype(::Type{<:LazyMapOther{Unknown}}) = throw_unknown_eltype()

Base.ndims(m::LazyMapOther) = ndims(typeof(m))
Base.ndims(::Type{<:LazyMapOther{T,N}}) where {T,N} = N
Base.ndims(::Type{<:LazyMapOther{T,-1}}) where {T} = throw_unknown_ndims()

Base.length(m::LazyMapOther) = _length(Base.IteratorSize(m), m.arg)
_length(trait::Base.HasLength, arg) = length(arg)
_length(trait::Base.HasShape, arg) = prod(_size(trait, arg))
_length(trait::Base.IteratorSize, arg) = throw_unknown_length()

Base.size(m::LazyMapOther) = _size(Base.IteratorSize(m), m.arg)
_size(trait::Base.HasShape, arg) = map(length, axes(arg))
_size(trait::Base.IteratorSize, arg) = throw_unknown_shape()

Base.axes(m::LazyMapOther) = _axes(Base.IteratorSize(m), m.arg)
_axes(trait::Base.HasShape, arg) = axes(arg)
_axes(trait::Base.IteratorSize, arg) = throw_unknown_shape()

# Make an instance of LazyMapOther an iterable.
Base.iterate(m::LazyMapOther) = _iterate_result(m, iterate(m.arg))
Base.iterate(m::LazyMapOther, s) = _iterate_result(m, iterate(m.arg, s))
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
