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
