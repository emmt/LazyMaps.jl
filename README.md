# LazyMaps: mapped arrays and collections with deferred evaluation

[![Build Status](https://github.com/emmt/LazyMaps.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/emmt/LazyMaps.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![Build Status](https://ci.appveyor.com/api/projects/status/github/emmt/LazyMaps.jl?svg=true)](https://ci.appveyor.com/project/emmt/LazyMaps-jl)
[![Coverage](https://codecov.io/gh/emmt/LazyMaps.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/emmt/LazyMaps.jl)
[![Aqua QA](https://raw.githubusercontent.com/JuliaTesting/Aqua.jl/master/badge.svg)](https://github.com/JuliaTesting/Aqua.jl)


`LazyMaps` is a small [Julia](http://julialang.org/) package providing lazily mapped arrays or collections.

## Usage

`using LazyMaps` exports a single method:

```julia
B = lazymap([T::Type,] f, A)
B = lazymap([T::Type,] f, A::AbstractArray, f_inv = inverse(f))
```

yield a view of the array or iterator `A` such that the `i`-th element of `B` is given by
`Bᵢ = as(T, f(Aᵢ))` with `Aᵢ` the `i`-th element of `A` and where the `as` method is an
extension of `convert` provided by the [`TypeUtils`](https://github.com/emmt/TypeUtils.jl)
package.

Optional argument `T` is to explicitly specify the element type of `B`; otherwise, it is
inferred from `f` and from the element type of `A`. The lazy map `B` has type-stable element
type in the sense that its element have guaranteed type `T`, even though `T` may be
abstract.

If `A` is an array, `f_inv` is the assumed inverse of `f` such that `B[i] = x` has the side
effect of modifying `A` by `A[i] = f_inv(x)`. If unspecified, `f_inv` is inferred by the
`inverse` method of the
[`InverseFunctions`](https://github.com/JuliaMath/InverseFunctions.jl) package.

As a special case:

```julia
C = lazymap(T::Type, A)
```

builds an object `C` that lazily maps the **constructor** `T` to the elements of `A`. This
is not exactly the same as:

```julia
B = lazymap(T::Type, identity, A)
```

which builds an object `B` that lazily **converts** the elements of `A` to type `T`. In
other words, the `i`-th element of `C` is given by `Cᵢ = T(Aᵢ)::T`, while the `i`-th element
of `B` is given by `Bᵢ = as(T, Aᵢ)`. In both cases, it is asserted that `Cᵢ` and `Bᵢ` are of
type `T`. The two are equivalent if `T` is a numeric type (a sub-type of `Number`).


## Related things

There exist objects or packages with functionalities similar to lazy maps but with
differences that justify the existence of this package:

* Compared to `Iterators.map(f, A)` which is always an iterator, the object returned by
  `lazymap(f, A)` is an (abstract) array if `A` is an array, an iterator otherwise.

* `mappedarray(f, A)` and `mappedarray(f, inv_f, A)` using
  [`MappedArrays`](https://github.com/JuliaArrays/MappedArrays.jl) are similar to
  `lazymap(f, A)` and `lazymap(f, A, inv_f)` but:
  - `f_inv` is not automatically inferred by `mappedarray` if not specified;
  - `A` must be an array in `mappedarray`;
  - the element type of a mapped array is inferred and cannot be specified as for a lazy map
    which is type-stable with respect to its `eltype`.

* `lazymap(T, A)` is the analogue of `of_eltype(T, A)` and `as_eltype(T, A)` respectively
  using [`MappedArrays`](https://github.com/JuliaArrays/MappedArrays.jl) and
  [`TypeUtils`](https://github.com/emmt/TypeUtils.jl).

* `LazyMaps` does not implement lazily mapping multiple arrays, a possibility offered by
  `MappedArrays`, but this may be emulated by combining `LazyMaps` and
  [`ZippedArrays`](https://github.com/emmt/ZippedArrays.jl).

* `mapview(f, A)` using [`FlexiMaps`](https://github.com/JuliaAPlavin/FlexiMaps.jl) is very
  similar to `lazymap(f, A)` but, with `FlexiMaps`, there is no equivalent to `lazymap(T,
  f, A)` to specify an element type for the mapped view and directly build a
  `FlexiMaps.MappedArray` with a given element type yields an abstract array whose `eltype`
  is not the type of the result returned by `getindex` while `lazymap` takes care of
  converting this result correctly.

* `BroadcastArray(f, A)` and `BroadcastArray{T}(f, A)` using
  [`LazyArrays`](https://github.com/JuliaArrays/LazyArrays.jl) is similar to `lazymap(f, A)`
  and `lazymap(T, f, A)` but broadcast arrays are read-only and restricted to arrays while
  lazy maps are writable if inverse function is known or specified and can be used over
  other collections than arrays.

As shown by [benchmark tests](./test/benchmarks.jl) for `LazyMaps` and these different
packages, evaluating `B[i]` for an object `B` lazily representing `f.(A)` is as fast as
calling `f(A[i])`. Also any of these objects can be used with no allocations and, except
`BroadcastArray`, no construction overheads compared to `f(A[i])`. A `BroadcastArray` using
[`LazyArrays`](https://github.com/JuliaArrays/LazyArrays.jl) is as fast as `mapreduce` for
reductions (like a `sum`) of the broadcast array which provides some speedup for large
arrays.

Direct dependencies:

* [`InverseFunctions`](https://github.com/JuliaMath/InverseFunctions.jl) is used by
  `LazyMaps` to infer inverse functions.

* [`TypeUtils`](https://github.com/emmt/TypeUtils.jl) is by `LazyMaps` to convert the values
  returned by lazily mapped object.

## Installation

`LazyMaps` is not yet an [official Julia package](https://pkg.julialang.org/) so you have
to clone the repository to install it:

```julia
using Pkg
Pkg.clone("https://github.com/emmt/LazyMaps.jl.git")
```
