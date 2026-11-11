package org.restheart;

import com.google.gson.Strictness;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import io.undertow.server.HttpServerExchange;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import org.restheart.exchange.BadRequestException;
import org.restheart.exchange.BufferedContent;
import org.restheart.exchange.ServiceRequest;

/**
 * Request type for {@code POST /deserialization}.
 *
 * <p>The body, with or without {@code Content-Length}, is read by RESTHeart
 * before the service runs ({@link BufferedContent}), and is parsed with a
 * strict streaming {@link JsonReader} that validates the schema while reading:
 * a JSON object with an {@code items} array of exactly 100 objects, each with a
 * numeric, integral {@code id} between 0 and 1000000.</p>
 *
 * @author Andrea Di Cesare <andrea@softinstigate.com>
 */
public class ItemsRequest extends ServiceRequest<ItemsRequest.Summary> {

  static final int ITEMS = 100;
  static final int MAX_ID = 1_000_000;
  private static final BigDecimal MAX_ID_DECIMAL = BigDecimal.valueOf(MAX_ID);

  /**
   * @param count the number of items
   * @param checksum the sum of the item ids
   */
  public record Summary(int count, long checksum) {}

  private ItemsRequest(HttpServerExchange exchange) {
    super(exchange);
  }

  public static ItemsRequest init(HttpServerExchange exchange) {
    return new ItemsRequest(exchange);
  }

  public static ItemsRequest of(HttpServerExchange exchange) {
    return of(exchange, ItemsRequest.class);
  }

  @Override
  public Summary parseContent() throws BadRequestException {
    var body = BufferedContent.get(getExchange());
    // no buffered content means no body: parse() rejects it as empty
    return parse(new ByteArrayInputStream(body == null ? new byte[0] : body));
  }

  /**
   * @param body the request body
   * @return the items summary
   * @throws BadRequestException if the body is not valid
   */
  static Summary parse(java.io.InputStream body) throws BadRequestException {
    var reader = new JsonReader(new InputStreamReader(body, StandardCharsets.UTF_8));
    reader.setStrictness(Strictness.STRICT);

    try {
      if (reader.peek() != JsonToken.BEGIN_OBJECT) {
        throw new BadRequestException("body must be a JSON object");
      }

      Summary summary = null;

      reader.beginObject();
      while (reader.hasNext()) {
        if ("items".equals(reader.nextName())) {
          if (summary != null) {
            throw new BadRequestException("repeated items");
          }
          summary = items(reader);
        } else {
          reader.skipValue();
        }
      }
      reader.endObject();

      // rejects trailing garbage and a second top-level value
      if (reader.peek() != JsonToken.END_DOCUMENT) {
        throw new BadRequestException("unexpected content after the JSON object");
      }

      if (summary == null) {
        throw new BadRequestException("missing items");
      }

      return summary;
    } catch (BadRequestException bre) {
      throw bre;
    } catch (IOException | IllegalStateException | NumberFormatException e) {
      // MalformedJsonException, EOFException (empty or truncated body) and wrong token types
      throw new BadRequestException("invalid JSON body", e);
    }
  }

  private static Summary items(JsonReader reader) throws IOException, BadRequestException {
    if (reader.peek() != JsonToken.BEGIN_ARRAY) {
      throw new BadRequestException("items must be an array");
    }

    int count = 0;
    long checksum = 0;

    reader.beginArray();
    while (reader.hasNext()) {
      if (count == ITEMS) {
        throw new BadRequestException("items must contain exactly " + ITEMS + " objects");
      }

      if (reader.peek() != JsonToken.BEGIN_OBJECT) {
        throw new BadRequestException("each item must be an object");
      }

      boolean hasId = false;
      reader.beginObject();
      while (reader.hasNext()) {
        if ("id".equals(reader.nextName())) {
          if (hasId) {
            throw new BadRequestException("repeated id");
          }
          if (reader.peek() != JsonToken.NUMBER) {
            throw new BadRequestException("id must be a number");
          }
          checksum += id(reader.nextString());
          hasId = true;
        } else {
          reader.skipValue();
        }
      }
      reader.endObject();

      if (!hasId) {
        throw new BadRequestException("missing id");
      }

      count++;
    }
    reader.endArray();

    if (count != ITEMS) {
      throw new BadRequestException("items must contain exactly " + ITEMS + " objects");
    }

    return new Summary(count, checksum);
  }

  /**
   * @param literal a JSON number literal, as returned by {@link JsonReader#nextString()}
   * @return its value, if integral and between 0 and {@link #MAX_ID}
   */
  static int id(String literal) throws BadRequestException {
    // fast path: plain non-negative integers, as sent by the load script
    int len = literal.length();
    if (len <= 7) {
      int value = 0;
      int i = 0;
      for (; i < len; i++) {
        char c = literal.charAt(i);
        if (c < '0' || c > '9') {
          break;
        }
        value = value * 10 + (c - '0');
      }
      if (i == len) {
        if (value > MAX_ID) {
          throw new BadRequestException("id out of range");
        }
        return value;
      }
    }

    // slow path: fractions and exponents, e.g. 2.0 and 2e0 are valid, 1.5 is not
    var value = new BigDecimal(literal);

    if (value.signum() < 0 || value.compareTo(MAX_ID_DECIMAL) > 0) {
      throw new BadRequestException("id out of range");
    }

    try {
      return value.intValueExact();
    } catch (ArithmeticException e) {
      throw new BadRequestException("id must be integral");
    }
  }
}
