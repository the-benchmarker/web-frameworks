package org.restheart;

import static org.restheart.plugins.InterceptPoint.ANY;

import com.google.gson.JsonNull;
import com.google.gson.JsonObject;
import io.undertow.server.handlers.form.FormData;
import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import org.restheart.exchange.BadRequestException;
import org.restheart.exchange.Exchange;
import org.restheart.exchange.JsonResponse;
import org.restheart.exchange.MultipartRequest;
import org.restheart.exchange.Request;
import org.restheart.plugins.MultipartService;
import org.restheart.plugins.RegisterPlugin;
import org.restheart.utils.HttpStatus;

/**
 * Serves {@code POST /upload}: reads the one uploaded file of the part named
 * {@code file} and returns its first and last bytes and its SHA-256 digest.
 *
 * <p>The media type is checked before the body is read, so an unsupported media
 * type returns 415 even when the body is malformed.</p>
 *
 * <p>The body is parsed by RESTHeart's {@link MultipartRequest} into Undertow's
 * {@link FormData}. Blocking: the 256 KiB load payload is above RESTHeart's
 * pre-buffering threshold, so the virtual thread reads it while it arrives.</p>
 *
 * @author Andrea Di Cesare <andrea@softinstigate.com>
 */
@RegisterPlugin(
  name = "benchmarkUploadService",
  description = "multipart upload benchmark",
  defaultURI = "/upload",
  uriMatchPolicy = RegisterPlugin.MATCH_POLICY.EXACT,
  blocking = true,
  dontIntercept = ANY
)
public class UploadService implements MultipartService {

  private static final String FILE_PART = "file";
  private static final int BUFFER_SIZE = 16 * 1024;
  private static final HexFormat HEX = HexFormat.of();

  @Override
  public void handle(MultipartRequest request, JsonResponse response) throws IOException {
    if (!request.isPost()) {
      response.setInError(HttpStatus.SC_METHOD_NOT_ALLOWED, "method not allowed");
      return;
    }

    var contentType = request.getHeader("Content-Type");

    if (!Exchange.isContentTypeMultipartFormData(request.getExchange())) {
      response.setInError(HttpStatus.SC_UNSUPPORTED_MEDIA_TYPE, "a multipart/form-data request body is required");
      return;
    }

    if (MultipartRequest.boundary(contentType) == null) {
      response.setInError(HttpStatus.SC_BAD_REQUEST, "missing or invalid multipart boundary");
      return;
    }

    FormData form;
    try {
      form = request.getContent();
    } catch (BadRequestException bre) {
      // getContent() already set the error response
      return;
    }

    var file = uploadedFile(form);

    if (file == null) {
      response.setInError(HttpStatus.SC_BAD_REQUEST, "exactly one uploaded file named file is required");
      return;
    }

    response.setContent(describe(file));
  }

  /**
   * Parts with {@code filename=""} are unselected file inputs and are ignored;
   * a text part named {@code file}, or more than one uploaded file, is rejected.
   *
   * @return the only uploaded file of the part named file, or null if there is not exactly one
   */
  static FormData.FileItem uploadedFile(FormData form) {
    var values = form.get(FILE_PART);

    if (values == null) {
      return null;
    }

    FormData.FileItem ret = null;

    for (var value : values) {
      var fileName = value.getFileName();

      if (fileName != null && fileName.isEmpty()) {
        continue; // unselected file input
      }

      if (!value.isFileItem() || fileName == null || ret != null) {
        return null; // a text field, or a repeated upload
      }

      ret = value.getFileItem();
    }

    return ret;
  }

  static JsonObject describe(FormData.FileItem file) throws IOException {
    var digest = sha256();
    int first = -1;
    int last = -1;

    try (var in = file.getInputStream()) {
      var buffer = new byte[BUFFER_SIZE];
      int read;
      while ((read = in.read(buffer)) != -1) {
        if (read == 0) {
          continue;
        }
        if (first < 0) {
          first = buffer[0] & 0xff;
        }
        last = buffer[read - 1] & 0xff;
        digest.update(buffer, 0, read);
      }
    }

    var ret = new JsonObject();
    if (first < 0) {
      // empty file
      ret.add("first_byte", JsonNull.INSTANCE);
      ret.add("last_byte", JsonNull.INSTANCE);
    } else {
      ret.addProperty("first_byte", first);
      ret.addProperty("last_byte", last);
    }
    ret.addProperty("checksum", HEX.formatHex(digest.digest()));

    return ret;
  }

  private static MessageDigest sha256() {
    try {
      return MessageDigest.getInstance("SHA-256");
    } catch (NoSuchAlgorithmException e) {
      throw new IllegalStateException("SHA-256 not available", e);
    }
  }

  @Override
  public boolean corsEnabled(Request<?> r) {
    // disable CORS headers
    return false;
  }
}
