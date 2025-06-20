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

struct Forever{T<:Tuple}
    values::T
end
Base.IteratorSize(::Type{<:Forever{<:Tuple{Vararg{T}}}}) where {T} = Base.IsInfinite()
Base.IteratorSize(::Type{<:Forever{<:Tuple{}}}) = Base.HasLength()
Base.IteratorEltype(::Type{<:Forever{<:Tuple{Vararg{T}}}}) where {T} = Base.HasEltype()
Base.IteratorEltype(::Type{<:Forever{<:Tuple{}}}) = Base.EltypeUnknown()
Base.eltype(::Type{<:Forever{<:Tuple{Vararg{T}}}}) where {T} = T
Base.length(iter::Forever{<:Tuple{}}) = 0
function Base.iterate(A::Forever, s::Int = 0)
    i = (s % length(A.values)) + 1
    return A.values[i], i
end

@testset "LazyMaps" begin

    @testset "miscellaneous" begin
        @test LazyMaps.pass(π) === π
    end

    @testset "arrays (f=$f, T=$T, dims=$(repr(dims)))" for (f,T,dims) in (
        (cos, Float32, (11,)),
        (abs2, Complex{Float32}, (2,3,4,)),)

        A = rand(T, dims)
        B = @inferred lazymap(f, A)
        @test B isa LazyMaps.LazyMapArray
        @test eltype(B) == typeof(f(zero(eltype(A))))
        @test ndims(B) == ndims(A)
        @test length(B) == length(A)
        @test size(B) == size(A)
        @test axes(B) == axes(A)
        @test strides(B) == strides(A)
        for k in 1:ndims(B)
            @test stride(B, k) == stride(A, k)
        end
        @test IndexStyle(B) == IndexStyle(A)
        @test B == f.(A)
        @test_throws Exception B[firstindex(B)] = B[lastindex(B)] # read-only by default

        if f === cos
            # Build a writable lazy array.
            B = @inferred lazymap(cos, A, acos)
            @test B isa LazyMaps.LazyMapArray
            @test eltype(B) == typeof(f(zero(eltype(A))))
            @test ndims(B) == ndims(A)
            @test length(B) == length(A)
            @test size(B) == size(A)
            @test axes(B) == axes(A)
            @test strides(B) == strides(A)
            for k in 1:ndims(B)
                @test stride(B, k) == stride(A, k)
            end
            @test IndexStyle(B) == IndexStyle(A)
            @test B == f.(A)
            A[firstindex(A)] = 0.2
            B[firstindex(B)] = 1
            @test A[firstindex(A)] == 0
            B[firstindex(B)] = -1
            @test A[firstindex(A)] ≈ π
            B[firstindex(B)] = 1//2
            @test A[firstindex(A)] ≈ π/3

            # Idem with a different element type.
            C = @inferred lazymap(Float64, cos, A, acos)
            @test eltype(C) == Float64
            C[firstindex(C)] = 1
            @test A[firstindex(A)] == 0
            C[firstindex(C)] = -1
            @test A[firstindex(A)] ≈ π rtol=1e-6
            C[firstindex(C)] = 1//2
            @test A[firstindex(A)] ≈ π/3 rtol=1e-6

        end

        # similar
        C = @inferred similar(B)
        @test typeof(C) <: Array{eltype(B), ndims(B)}
        @test axes(C) == axes(B)
        T′ = T <: Complex ? Complex{Float64} : Float64
        C = @inferred similar(B, T′)
        @test typeof(C) <: Array{T′, ndims(B)}
        @test axes(C) == axes(B)
        rngs = (Base.OneTo(3),(Base.OneTo(5)))
        dims = map(length, rngs)
        C = @inferred similar(B, T′, dims)
        @test typeof(C) <: Array{T′, 2}
        @test size(C) == dims
        @test axes(C) == rngs
        C = @inferred similar(B, T′, rngs)
        @test typeof(C) <: Array{T′,2}
        @test size(C) == dims
        @test axes(C) == rngs

        B = @inferred lazymap(Float64, f, A)
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
            B = @inferred lazymap(f, C)
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
        B = @inferred lazymap(T′, A)
        @test typeof(B) === typeof(lazymap(T′, LazyMaps.pass, A, LazyMaps.pass))
        @test eltype(B) === T′
        @test B == T′.(A)
        C = @inferred lazymap(T′, identity, A)
        @test eltype(C) === T′
        @test C == B
        C = @inferred lazymap(T′, T′, A)
        @test eltype(C) === T′
        @test C == B
    end

    @testset "collections" begin

        A = Dict("A" => 33, "B" => 100, "C" => pi, "D" => 1f0)
        f((k,v)::Pair) = k => sin(v)

        B = @inferred lazymap(f, A)
        @test B isa LazyMaps.LazyMapOther
        @test eltype(B) === eltype(typeof(B))
        @test !isconcretetype(eltype(B))
        @test_throws Exception ndims(B)
        @test length(B) == length(A)
        @test_throws Exception size(B)
        @test_throws Exception axes(B)
        @test Base.IteratorSize(typeof(B)) === Base.IteratorSize(B) === Base.IteratorSize(A)
        @test Base.IteratorEltype(typeof(B)) === Base.IteratorEltype(B) === Base.IteratorEltype(A)
        for (a,b) in zip(A,B)
            @test first(b) == first(a)
            @test last(b) === sin(last(a))
        end

        B = @inferred lazymap(Pair{String,Float16}, f, A)
        @test eltype(B) <: Pair{String,Float16}
        for (a,b) in zip(A,B)
            @test first(b) == first(a)
            @test last(b) === Float16(sin(last(a)))
        end

        A = rand(Float32, 2,3,4)
        B = IteratedArray(A)
        f = sqrt
        C = @inferred lazymap(f, B)
        @test C isa LazyMaps.LazyMapOther
        @test eltype(C) == typeof(f(zero(eltype(A))))
        @test ndims(C) == ndims(A)
        @test length(C) == length(A)
        @test size(C) == size(A)
        @test axes(C) == axes(A)
        @test Base.IteratorSize(typeof(C)) === Base.IteratorSize(C) === Base.IteratorSize(A)
        @test Base.IteratorEltype(typeof(C)) === Base.IteratorEltype(C) === Base.IteratorEltype(A)
        for (a,c) in zip(A,C)
            @test c === f(a)
        end

        # Cannot build a writable lazy iterator.
        @test_throws Exception lazymap(f, B, x -> x^2)

        A = (1, 2f0, 3.0, 0x04)
        T = Int
        B = @inferred lazymap(T, A)
        @test B === lazymap(T, LazyMaps.pass, A)
        @test eltype(B) === T
        for (a,b) in zip(A,B)
            @test b === T(a)
        end
        C = @inferred lazymap(T, identity, A)
        @test eltype(C) === T
        for (b,c) in zip(B,C)
            @test b === c
        end

        A = Forever((42,))
        B = @inferred lazymap(x -> x + 2, A)
        @test B isa LazyMaps.LazyMapOther
        @test first(B) === 44
        @test eltype(B) == Int
        @test_throws Exception length(B)
        @test_throws Exception axes(B)
        @test_throws Exception size(B)

        A = Forever(())
        B = @inferred lazymap(x -> x + 2, A)
        @test B isa LazyMaps.LazyMapOther
        @test_throws Exception eltype(B)
        @test length(B) == 0
        @test_throws Exception axes(B)
        @test_throws Exception size(B)
    end

    if VERSION ≥ v"1.6"
        include("aqua.jl")
    end

end

end # module
