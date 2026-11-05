# Spine WFB implementation

A minimal HTTP application using the public releases of
`github.com/NARUBROWN/spine v0.6.5` and
`github.com/NARUBROWN/spine-echo-platform v0.1.0`.
Every request passes through Spine routing, constructor injection, argument resolution,
controller execution, and response handling. The application does not register routes
directly with Echo or change the default HTTP protections.

| Request | Status | Body |
| --- | --- | --- |
| `GET /` | 200 | Empty body |
| `GET /user/:id` | 200 | The supplied `id` |
| `POST /user` | 200 | Empty body |

The default port is `3000`; set `SPINE_PORT` to override it.
The application uses no database, message broker, or external service.
It selects only an HTTP implementation and registers no WebSocket, event, or validator implementation.

The project follows Spine CLI's default `layered-v1` layout:

```text
main.go                                   # Validate configuration and run the server
internal/application/application.go       # Compose spine.New(echoplatform.Option()), constructors, and runtime options
src/controller/benchmark_controller.go    # Handle benchmark responses
src/route/route.go                        # Register the three HTTP routes
spine.toml                               # Configure layered-v1, Echo, and the v0.6.5 recipe
```

Spine CLI is not required to build or run the application or to run the shared container checks below.

`spine contract check --json` and `spine verify --json` are optional development checks
provided by the separately installed
[Spine CLI v0.4.2](https://github.com/NARUBROWN/spine-cli/releases/tag/v0.4.2).
Installing Go module dependencies does not install the `spine` command.

Run the shared container checks from the WFB repository root:

```sh
bundle exec rake config
make -f go/spine/.Makefile build
make -f go/spine/.Makefile test
make -f go/spine/.Makefile unbuild
```

The shared language templates generate `.Dockerfile.net-http` and `.Makefile`.
The shared Go configuration copies `.go` files and `go.mod`, then runs `go get`
inside the container to generate the required checksums before building.
Keep `go.sum` tracked for local development.
Response-contract checks are separate from performance measurements;
local measurements are not official benchmark rankings.
