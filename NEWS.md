# User visible changes in `Neutrals`

This page describes the most important changes in `Neutrals`. The format is based on [Keep
a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to [Semantic
Versioning](https://semver.org).

## Version 0.2.1 (2025-06-20)

This is the first official release.


## Version 0.2.0 (2025-06-16)

### Added

- `B = lazymap([T::Type,] f, A::AbstractArray, finv)` builds a **writable** lazy mapped
  array.

- Tests with [`Aqua.jl`](https://github.com/JuliaTesting/Aqua.jl).

### Fixed

- Some ambiguities in `similar`have been fixed.


## Version 0.1.0 (2025-06-16)

- Initial release.
