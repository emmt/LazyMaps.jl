module BenchmarkingLazyMaps

using BenchmarkTools
using ThreadPinning
using MappedArrays
using FlexiMaps
using LazyMaps
using LazyArrays

function simd_mapreduce(f, op, A::AbstractArray)
    #=
    T = Base.promote_op(f, eltype(A))
    S = Base.promote_op(op, T, T)
    =#
    start = firstindex(A)
    stop = lastindex(A)
    stop - start > 1 || eror("not large enough!")
    r = @inbounds op(f(A[start]), f(A[start+1]))
    T = typeof(r)
    @inbounds @simd for i in start+2:stop
        r = convert(T, op(r, f(A[i])))::T
    end
    return r
end

f1(x) = sqrt(abs2(oneunit(x)) + abs2(x))
f2(x) = inv(f1(x))

pinthreads(:cores)
T = Float32
for dims in ((3,4), (10_000,),)
    A = rand(T, dims)
    for f in (abs2, f1, f2)
        println("\nTests with $(prod(dims)) elements and f=$(nameof(f)):")
        fA = f.(A)
        B = BroadcastArray(f, A)
        @assert B ≈ fA
        M = mappedarray(f, A)
        @assert M ≈ fA
        V = mapview(f, A)
        @assert V ≈ fA
        L = lazymap(f, A)
        @assert L ≈ fA
        print(" mapreduce(f, +, A)       "); @btime mapreduce($f, +, $A);
        print(" sum(B#=BroadcastArray=#) "); @btime sum($B);
        print(" sum(M#=mappedarray=#)    "); @btime sum($M);
        print(" sum(V#=mapview=#)        "); @btime sum($V);
        print(" sum(L#=lazymap=#)        "); @btime sum($L);
    end
end

end # module
