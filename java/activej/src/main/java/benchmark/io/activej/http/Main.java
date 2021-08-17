package benchmark.io.activej.http;

import io.activej.config.Config;
import io.activej.http.AsyncServlet;
import io.activej.http.HttpResponse;
import io.activej.http.RoutingServlet;
import io.activej.inject.annotation.Provides;
import io.activej.inject.module.AbstractModule;
import io.activej.inject.module.Module;
import io.activej.launchers.http.MultithreadedHttpServerLauncher;
import io.activej.worker.annotation.Worker;

import java.net.InetSocketAddress;

import static io.activej.bytebuf.ByteBufStrings.wrapAscii;
import static io.activej.config.Config.ofSystemProperties;
import static io.activej.config.converter.ConfigConverters.ofInetSocketAddress;
import static io.activej.http.HttpMethod.GET;
import static io.activej.http.HttpMethod.POST;

public final class Main extends MultithreadedHttpServerLauncher {
	private static final int PORT = 3000;

	@Provides
	@Worker
	AsyncServlet mainServlet() {
		return RoutingServlet.create()
				.map("/", request -> HttpResponse.ok200())
				.map("/user/:id", request -> HttpResponse.ok200()
						.withBody(wrapAscii(request.getPathParameter("id"))))
				.map("/user", request -> HttpResponse.ok200());
	}

	@Override
	protected Module getOverrideModule() {
		return new AbstractModule() {
			@Provides
			Config config() {
				return Config.create()
						.with("http.listenAddresses", Config.ofValue(ofInetSocketAddress(), new InetSocketAddress(PORT)))
						.with("workers", "" + Runtime.getRuntime().availableProcessors())
						.overrideWith(ofSystemProperties("config"));
			}
		};
	}

	public static void main(String[] args) throws Exception {
		new Main().launch(args);
	}
}
