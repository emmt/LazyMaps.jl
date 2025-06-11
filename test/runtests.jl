module TestingLazyMaps

using LazyMaps, Test, TypeUtils

# The following structure "hides" an abstract array in an iterator which has a "shape".
struct IteratedArray{A<:AbstractArray}
    data::A
end
Base.IteratorSize(::Type{<:IteratedArray{A}}) where {A} = Base.IteratorSize(A)
Base.IteratorEltype(::Type{<:IteratedArray{A}}) where {A} = Base.IteratorEltype(A)
Base.eltype(A::IteratedArray) = eltype(typeof(A))
Base.eltype(::Type{<:IteratedArray{<:AbstractArray{T,N}}}) where {T,N} = T
Base.ndims(A::IteratedArray) = ndims(typeof(A))
Base.ndims(::Type{<:IteratedArray{<:AbstractArray{T,N}}}) where {T,N} = N
Base.axes(A::IteratedArray) = axes(A.data)
Base.size(A::IteratedArray) = map(length, axes(A))
Base.length(A::IteratedArray) = prod(size(A))
Base.iterate(A::IteratedArray) = iterate(A.data)
Base.iterate(A::IteratedArray, s) = iterate(A.data, s)

@testset "LazyMaps" begin

    @testset "arrays (f=$f, T=$T, dims=$(repr(dims)))" for (f,T,dims) in (
        (cos, Float32, (11,)),
        (abs2, Complex{Float32}, (2,3,4,)),)

        A = rand(T, dims)
        B = @inferred(lazymap(f, A))
        @test B isa LazyMaps.LazyMapArray
        @test eltype(B) == typeof(f(zero(eltype(A))))
        @test ndims(B) == ndims(A)
        @test length(B) == length(A)
        @test size(B) == size(A)
        @test axes(B) == axes(A)
        @test strides(B) == strides(A)
        @test IndexStyle(B) == IndexStyle(A)
        @test B == f.(A)

        B = @inferred(lazymap(Float64, f, A))
        @test B isa LazyMaps.LazyMapArray
        @test eltype(B) == Float64
        @test ndims(B) == ndims(A)
        @test length(B) == length(A)
        @test size(B) == size(A)
        @test axes(B) == axes(A)
        @test strides(B) == strides(A)
        @test IndexStyle(B) == IndexStyle(A)
        @test B == (Float64∘f).(A)

        if ndims(A) == 3
            C = view(A, :, 2:3, :) # this array has Cartesian indexing
            @test IndexStyle(C) isa IndexCartesian
            B = @inferred(lazymap(f, C))
            @test B isa LazyMaps.LazyMapArray
            @test eltype(B) == typeof(f(zero(eltype(C))))
            @test ndims(B) == ndims(C)
            @test length(B) == length(C)
            @test size(B) == size(C)
            @test axes(B) == axes(C)
            @test strides(B) == strides(C)
            @test IndexStyle(C) == IndexStyle(C)
            @test B == f.(C)
        end

        T′ = T <: Complex ? Complex{Float64} : Float64
        B = @inferred(lazymap(T′, A))
        @test B === lazymap(T′, identity, A)
        @test eltype(B) === T′
        @test B == T′.(A)
    end

    @testset "collections" begin

        A = Dict("A" => 33, "B" => 100, "C" => pi, "D" => 1f0)
        f((k,v)::Pair) = k => sin(v)

        B = @inferred(lazymap(f, A))
        @test B isa LazyMaps.LazyMapOther
        @test !isconcretetype(eltype(B))
        @test_throws Exception ndims(B)
        @test length(B) == length(A)
        @test_throws Exception size(B)
        @test_throws Exception axes(B)
        @test Base.IteratorSize(B) === Base.IteratorSize(A)
        @test Base.IteratorEltype(B) === Base.IteratorEltype(A)
        for (a,b) in zip(A,B)
            @test first(b) == first(a)
            @test last(b) === sin(last(a))
        end

        B = @inferred(lazymap(Pair{String,Float16}, f, A))
        @test eltype(B) <: Pair{String,Float16}
        for (a,b) in zip(A,B)
            @test first(b) == first(a)
            @test last(b) === Float16(sin(last(a)))
        end

        A = rand(Float32, 2,3,4)
        B = IteratedArray(A)
        f = sqrt
        C = @inferred(lazymap(f, B))
        @test C isa LazyMaps.LazyMapOther
        @test eltype(C) == typeof(f(zero(eltype(A))))
        @test ndims(C) == ndims(A)
        @test length(C) == length(A)
        @test size(C) == size(A)
        @test axes(C) == axes(A)
        @test Base.IteratorSize(C) === Base.IteratorSize(A)
        @test Base.IteratorEltype(C) === Base.IteratorEltype(A)
        for (a,c) in zip(A,C)
            @test c === f(a)
        end

        A = (1, 2f0, 3.0, 0x04)
        B = @inferred(lazymap(Int, A))
        for (a,b) in zip(A,B)
            @test b === Int(a)
        end

    end
end

end # module
