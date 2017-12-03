# LazyMaps.jl

[![Build Status](https://travis-ci.org/emmt/LazyMaps.jl.svg?branch=master)](https://travis-ci.org/emmt/LazyMaps.jl)

This package implements lazy maps for [Julia](http://julialang.org/).  A
*lazy map* associates a function and an array or a collection in an object
whose elements are the result of the function applied to the elements of
the embedded array or collection.  This is similar to calling `map` except
that evaluation is performed *on the fly* and thus avoids creating an
intermediate array or collection.  *Lazy maps* are therefore useful to
provide an elementwise filtered array or collection to some method without
creating an intermediate array or collection.  This can save storage and
speedup computations for large arrays or collections.


## Usage

To create a *lazy map* whith function `f` and array or collection `a`, call:

```julia
m = lazymap(f, a)
```

The object `m` can be used as a read-only array:

```julia
m[i]
```
is the same as `f(a[i])`,  or as an iterable:

```julia
for item in m
    ...
end
```

to loop through all values produced by `f.(a)`.

Incidently:

```julia
collect(m)
```

is the same as `map(f, a)` or `f.(a)`.

By default, `lazymap` attempts to evaluate `f` for the first item of `a` to
determine the type of the elements.  To avoid this, at the cost of making
some optimizations not possible, call:

```julia
m = lazymap(f, a, false)
```

instead.  When the element type can be determined, it is assumed (and
asserted) that all elements have the same type, *i.e.* the elements of
result are **type stable**.


## Installation

`LazyMaps` is not yet an
[official Julia package](https://pkg.julialang.org/) so you have to clone the
repository to install it:

```julia
Pkg.clone("https://github.com/emmt/LazyMaps.jl.git")
```

The `LazyMaps` package is pure Julia code and there is nothing to build.
