package org.http4k.benchmark

import org.http4k.core.ContentType.Companion.TEXT_PLAIN
import org.http4k.core.Method.GET
import org.http4k.core.Method.POST
import org.http4k.core.Request
import org.http4k.core.Response
import org.http4k.core.Status.Companion.OK
import org.http4k.core.then
import org.http4k.filter.ServerFilters.SetContentType
import org.http4k.routing.bind
import org.http4k.routing.path
import org.http4k.routing.routes
import org.http4k.server.Undertow
import org.http4k.server.asServer

fun BenchmarkApp() =
    SetContentType(TEXT_PLAIN)
        .then(
            routes(
                "/user/{id}" bind GET to { req: Request -> Response(OK).body(req.path("id")!!) },
                "/user" bind POST to { Response(OK) },
                "/" bind GET to { Response(OK) },
            ),
        )

fun main() {
    BenchmarkApp().asServer(Undertow(3000)).start()
}
