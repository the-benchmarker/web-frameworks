package com.hexagonkt

import com.hexagonkt.core.require
import com.hexagonkt.core.media.TEXT_PLAIN
import com.hexagonkt.http.model.ContentType
import com.hexagonkt.http.server.HttpServerSettings
import com.hexagonkt.http.server.netty.epoll.NettyEpollServerAdapter
import com.hexagonkt.http.server.serve
import java.net.InetAddress

fun main() {
    val textPlain = ContentType(TEXT_PLAIN)
    val adapter = NettyEpollServerAdapter(
        executorThreads = 0,
        keepAliveHandler = false,
        httpAggregatorHandler = false,
        chunkedHandler = false,
        enableWebsockets = false,
    )
    val settings = HttpServerSettings(InetAddress.getByName("0.0.0.0"), 3000)

    serve(adapter, settings) {
        get {
            ok(contentType = textPlain)
        }

        path("/user") {
            post {
                ok(contentType = textPlain)
            }
            get("/{id}") {
                ok(pathParameters.require("id"), contentType = textPlain)
            }
        }
    }
}
