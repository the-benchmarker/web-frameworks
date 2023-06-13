package com.hexagonkt

import com.hexagonkt.core.require
import com.hexagonkt.core.media.TEXT_PLAIN
import com.hexagonkt.handlers.async.done
import com.hexagonkt.http.model.ContentType
import com.hexagonkt.http.server.async.HttpServerSettings
import com.hexagonkt.http.server.netty.async.NettyServerAdapter
import com.hexagonkt.http.server.async.serve
import java.net.InetAddress

fun main() {
    val textPlain = ContentType(TEXT_PLAIN)
    val adapter = NettyServerAdapter()
    val settings = HttpServerSettings(InetAddress.getByName("0.0.0.0"), 3000)

    serve(adapter, settings) {
        get {
            ok(contentType = textPlain).done()
        }

        path("/user") {
            post {
                ok(contentType = textPlain).done()
            }
            get("/{id}") {
                ok(pathParameters.require("id"), contentType = textPlain).done()
            }
        }
    }
}
