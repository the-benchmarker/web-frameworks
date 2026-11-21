package io.github.suppierk.shoostr.benchmark;

import static com.fasterxml.jackson.databind.DeserializationFeature.FAIL_ON_TRAILING_TOKENS;
import static com.fasterxml.jackson.databind.DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.github.suppierk.shoostr.Codec;
import io.github.suppierk.shoostr.Options;
import io.github.suppierk.shoostr.Request;
import io.github.suppierk.shoostr.Shoostr;
import io.github.suppierk.shoostr.http.HttpHeaders;
import io.github.suppierk.shoostr.http.MediaType;
import io.github.suppierk.shoostr.http.exceptions.BadRequestException;
import io.github.suppierk.shoostr.http.exceptions.UnsupportedMediaTypeException;
import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HexFormat;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

public final class Bench {
  private static final int ITEM_COUNT = 100;
  private static final int MAX_ID = 1_000_000;
  private static final Pattern PATH_ID = Pattern.compile("\\d{8}");
  private static final Pattern SEED = Pattern.compile("[a-z]{8}");

  private Bench() {}

  public static void main(String[] args) throws Exception {
    var options = new Options();
    options.setPort(Integer.parseInt(System.getenv().getOrDefault("PORT", "3000")));

    new Shoostr(options)
        .codec(new JsonCodec())
        .routes(
            routes -> {
              routes.get("/", (request, response) -> response.text(""));

              routes.get(
                  "/user/{id}",
                  (request, response) -> {
                    var id = request.pathParam("id").orElseThrow();
                    if (!PATH_ID.matcher(id).matches()) {
                      throw new BadRequestException();
                    }

                    response.body(MediaType.APPLICATION_JSON, Map.of("id", id));
                  });

              routes.post("/user", (request, response) -> response.text(""));

              routes.get(
                  "/serialization",
                  (request, response) ->
                      response.body(MediaType.APPLICATION_JSON, repeatSeed(request)));

              routes.post(
                  "/deserialization",
                  (request, response) ->
                      response.body(MediaType.APPLICATION_JSON, sumItemIds(request)));

              routes.post(
                  "/upload",
                  (request, response) ->
                      response.body(MediaType.APPLICATION_JSON, uploadSummary(request)));
            })
        .start();
  }

  private static List<Map<String, String>> repeatSeed(Request request) {
    final var query = request.queryParamMap();
    final var seeds = query.getOrDefault("seed", List.of());
    if (query.size() != 1 || seeds.size() != 1 || !SEED.matcher(seeds.getFirst()).matches()) {
      throw new BadRequestException();
    }

    final var seed = seeds.getFirst();
    final var items = new ArrayList<Map<String, String>>(ITEM_COUNT);
    for (var index = 0; index < ITEM_COUNT; index++) {
      items.add(Map.of("seed", seed));
    }

    return items;
  }

  private static Map<String, Integer> sumItemIds(Request.Typed request) throws Exception {
    if (!isJson(mediaType(request))) {
      throw new UnsupportedMediaTypeException();
    }
    if (!request.queryParamMap().isEmpty()) {
      throw new BadRequestException();
    }

    final var items = request.body(ObjectNode.class).path("items");
    if (!items.isArray() || items.size() != ITEM_COUNT) {
      throw new BadRequestException();
    }

    var checksum = 0;
    for (final var item : items) {
      final var id = item.path("id");
      if (!id.canConvertToInt() || !id.canConvertToExactIntegral()) {
        throw new BadRequestException();
      }

      final var value = id.intValue();
      if (value < 0 || value > MAX_ID) {
        throw new BadRequestException();
      }
      checksum += value;
    }

    return Map.of("count", items.size(), "checksum", checksum);
  }

  private static boolean isJson(MediaType mediaType) {
    final var type = mediaType.value();
    return MediaType.APPLICATION_JSON.equals(mediaType)
        || type.equals("text/x-json")
        || type.equals("application/jsonrequest")
        || type.endsWith("+json");
  }

  private static Map<String, Object> uploadSummary(Request request)
      throws IOException, NoSuchAlgorithmException {
    if (!MediaType.MULTIPART_FORM_DATA.equals(mediaType(request))) {
      throw new UnsupportedMediaTypeException();
    }

    final var files =
        request.files("file").stream().filter(file -> !file.fileName().isEmpty()).toList();
    if (files.size() != 1) {
      throw new BadRequestException();
    }
    try {
      if (!request.formParams("file").isEmpty()) {
        throw new BadRequestException();
      }
    } catch (UnsupportedMediaTypeException failure) {
      // The request media type is valid. An unsupported field makes the multipart body invalid.
      throw new BadRequestException(failure);
    }

    byte[] bytes;
    try (final var content = files.getFirst().content()) {
      bytes = content.readAllBytes();
    }

    final var digest = MessageDigest.getInstance("SHA-256").digest(bytes);

    final var result = new LinkedHashMap<String, Object>();
    result.put("first_byte", bytes.length == 0 ? null : Byte.toUnsignedInt(bytes[0]));
    result.put("last_byte", bytes.length == 0 ? null : Byte.toUnsignedInt(bytes[bytes.length - 1]));
    result.put("checksum", HexFormat.of().formatHex(digest));

    return result;
  }

  private static MediaType mediaType(Request request) {
    final var header = request.header(HttpHeaders.CONTENT_TYPE.value()).orElse("");
    final var parameterStart = header.indexOf(';');
    final var type = parameterStart < 0 ? header : header.substring(0, parameterStart);

    try {
      return MediaType.of(type.trim());
    } catch (IllegalArgumentException failure) {
      throw new UnsupportedMediaTypeException(failure);
    }
  }

  /** Jackson codec for JSON handling. */
  private static final class JsonCodec implements Codec {
    private final JsonMapper mapper =
        JsonMapper.builder().enable(FAIL_ON_TRAILING_TOKENS, USE_BIG_DECIMAL_FOR_FLOATS).build();

    @Override
    public <T> T read(byte[] source, Class<T> type) throws IOException {
      try {
        final var value = mapper.readValue(source, type);
        if (value == null) {
          throw new BadRequestException();
        }
        return value;
      } catch (JsonProcessingException failure) {
        throw new BadRequestException(failure);
      }
    }

    @Override
    public byte[] write(Object value) throws JsonProcessingException {
      return mapper.writeValueAsBytes(value);
    }
  }
}
