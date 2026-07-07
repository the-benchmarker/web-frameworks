"""
HTTP.jl Benchmark Dependencies

Production-grade dependency management for HTTP.jl benchmark server.
"""

using Pkg

# Activate the project environment
Pkg.activate(@__DIR__)

# Add the General registry if not already present
if !("General" in Pkg.registries())
    Pkg.Registry.add(RegistrySpec(url = "https://github.com/JuliaRegistries/General"))
end

# Resolve and instantiate dependencies
Pkg.resolve()
Pkg.instantiate()

# Precompile dependencies for better performance
Pkg.precompile()
