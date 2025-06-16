# LazyMaps: mapped arrays and collections with deferred evaluation

[![Build Status](https://github.com/emmt/LazyMaps.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/emmt/LazyMaps.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![Build Status](https://ci.appveyor.com/api/projects/status/github/emmt/LazyMaps.jl?svg=true)](https://ci.appveyor.com/project/emmt/LazyMaps-jl)
[![Coverage](https://codecov.io/gh/emmt/LazyMaps.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/emmt/LazyMaps.jl)
[![Aqua QA](https://raw.githubusercontent.com/JuliaTesting/Aqua.jl/master/badge.svg)](https://github.com/JuliaTesting/Aqua.jl)

This package implements lazily mapped arrays or collections for
[Julia](http://julialang.org/). A *lazy map*, say `B = lazymap(f, A)`, associates a
function `f` and an array or a collection `A` in an object `B` whose elements are the
element-wise result of the function `f` applied to `A`. This is similar to calling `map(f,
A)` except that evaluation is performed *on the fly* and thus avoids the storage that
would be required by an intermediate array or collection. *Lazy maps* save storage and may
speedup computations for large arrays or collections.

## Basic usage

To build a *lazy map* combining the function `f` and the array or collection `A`, call:

```julia
B = lazymap(f, A)
```

If `A` is an array, the object `B` is a read-only abstract array (of same shape and
indexing style as `A`) such that `B[i]` yields `f(A[i])` for any valid index `i` of `A`.

Even though `A` is not an array, it is assumed to be iterable and so is the lazy map `B`.
Hence:

```julia
for b in B
    ...
end
```

is a shortcut for:

```julia
for a in A
    b = f(a)
    ...
end
```

Incidentally `collect(B)` is the same as `map(f, a)` or `f.(a)`.

If `A` is an array while `f` and `finv` are functions, a *writable lazy map* is built by:

```julia
B = lazymap(f, A, finv)
```

Then, the syntax `B[i] = x` is allowed and has the side effect of changing the
corresponding element of `A`, as if `A[i] = finv(x)` has been called. It may be of
interest to realize that it is not imposed that `finv` be the inverse of `f`.


## Element type stability

By default, `lazymap` attempts to infer the element type of lazy map `B` as that of `f(a)`
for any element `a` of `A` but without actually calling `f` (i.e. using
`Base.promote_op`). It is however possible to specify the element type `T` of the lazy map
explicitly by building `B` as:

```julia
B = lazymap(T, f, A[, finv])
```

where `T::Type` does not need to be `typeof(f(a))`, any needed conversion will be lazily
performed when extracting elements from `B`.

This latter rule is general: regardless of how `T = eltype(B)` is determined (explicitly
of implicitly), each element `b` of `B` corresponding to the element `a` of `A` is given
by converting `f(a)` to the type `T` so that `b::T` is guaranteed to hold. Hence , `B =
lazymap(T, identity, A)` can be used to lazily convert the elements of `A` or to stabilize
the element type of `A`.


## Related things

Compared to `Iterators.map(f, A)` which is always an iterator, the object returned by
`lazymap(f, A)` is an (abstract) array if `A` is an array, an iterator otherwise.

`lazymap(T, A)` is the analogue of `of_eltype(T, A)` provided by
[`MappedArrays`](https://github.com/JuliaArrays/MappedArrays.jl) or of `as_eltype(T, A)`
provided by [`TypeUtils`](https://github.com/emmt/TypeUtils.jl).

Compared to `mappedarray(f, A)` or `mappedarray(f, finv, A)` with the
[`MappedArrays`](https://github.com/JuliaArrays/MappedArrays.jl) package, the element type
of the result may be explicitly specified by `lazymap(T, f, A)` or by `lazymap(T, f, A,
finv)`. In any case, the result of a lazy map is type-stable.

`LazyMaps` does not implement lazily mapping multiple arrays, a possibility are offered by
`MappedArrays`. This may be done in the future.


## Installation

`LazyMaps` is not yet an [official Julia package](https://pkg.julialang.org/) so you have
to clone the repository to install it:

```julia
using Pkg
Pkg.clone("https://github.com/emmt/LazyMaps.jl.git")
```
