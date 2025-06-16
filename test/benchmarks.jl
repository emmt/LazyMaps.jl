module BenchmarkingLazyMaps

using BenchmarkTools
using ThreadPinning
using MappedArrays
using LazyMaps

pinthreads(:cores)
T = Float32
for dims in ((3,4), (10_000,),)
    println("\nTests with $(prod(dims)) elements:")
    A = rand(T, dims)
    M = mappedarray(abs2, A)
    L = lazymap(abs2, A)
    @assert M ≈ L ≈ abs2.(A)
    print(" sum(abs2.(A))       "); @btime sum(abs2.($A));
    print(" sum(M::MappedArray) "); @btime sum($M);
    print(" sum(L::LazyMapArray)"); @btime sum($L);
end

end # module
