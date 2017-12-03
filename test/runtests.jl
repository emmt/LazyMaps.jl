if ! isdefined(:LazyMaps)
    include(joinpath("..", "src", "LazyMaps.jl"))
end

module LazyMapsTests

using LazyMaps, Base.Test

const DEBUG = false

function identicalvalues(A, B, ::Type{Val{:array}})
    for i in eachindex(A, B)
        A[i] !== B[i] && return false
    end
    return true
end

function identicalvalues(A::AbstractArray, B::LazyMap,
                         ::Type{Val{:collection}})
    i = 0
    for b in B
        i += 1
        A[i] !== b && return false
    end
    return true
end

a = randn(11)
b = randn(5,6,7)
c = view(b, :, 2:3, 3:6)
d = Dict("A" => 33, "B" => 100, "C" => pi)


@testset "LazyMaps" begin

    @testset "arrays" begin
        @test identicalvalues(cos.(a), lazymap(cos, a), Val{:array})
        @test identicalvalues(cos.(b), lazymap(cos, b), Val{:array})
        @test identicalvalues(cos.(c), lazymap(cos, c), Val{:array})
    end

    @testset "collections" begin
        @test identicalvalues(sin.(a), lazymap(sin, a), Val{:collection})
        @test identicalvalues(sin.(b), lazymap(sin, b), Val{:collection})
        @test identicalvalues(sin.(c), lazymap(sin, c), Val{:collection})
    end

    #@testset "dictionaries" begin
    #    @test identicalvalues([repr(pair) for pair in d],
    #                          lazymap(repr, d), Val{:collection})
    #end
end

end # module
