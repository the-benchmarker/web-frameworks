using Pkg

Pkg.activate(pwd())
Pkg.Registry.add(RegistrySpec(url = "https://github.com/JuliaRegistries/General"))
Pkg.update()
Pkg.resolve()
