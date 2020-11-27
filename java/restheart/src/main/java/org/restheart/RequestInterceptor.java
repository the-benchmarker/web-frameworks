package org.restheart;

import org.restheart.exchange.ByteArrayRequest;
import org.restheart.exchange.ByteArrayResponse;
import org.restheart.plugins.ByteArrayInterceptor;
import org.restheart.plugins.RegisterPlugin;

import io.undertow.util.Headers;

@RegisterPlugin(name = "forceIdentityEcoding", description = "set Accept-Encoding=identity")
public class RequestInterceptor implements ByteArrayInterceptor {
    @Override
    public void handle(ByteArrayRequest request, ByteArrayResponse response) throws Exception {
        request.getHeaders().put(Headers.ACCEPT_ENCODING, "identity");
    }

    @Override
    public boolean resolve(ByteArrayRequest request, ByteArrayResponse response) {
        return true;
    }
}
