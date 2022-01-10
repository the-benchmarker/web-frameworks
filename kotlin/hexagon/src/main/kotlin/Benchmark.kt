package com.hexagonkt

import com.hexagonkt.core.helpers.require
import com.hexagonkt.core.media.TextMedia
import com.hexagonkt.http.model.ContentType
import com.hexagonkt.http.server.HttpServerSettings
import com.hexagonkt.http.server.jetty.JettyServletAdapter
import com.hexagonkt.http.server.serve
import java.net.InetAddress

fun main() {
    val textPlain = ContentType(TextMedia.PLAIN)
    val adapter = JettyServletAdapter()
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
