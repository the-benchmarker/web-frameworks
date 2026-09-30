package benchmark.springboot;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import tools.jackson.databind.JsonNode;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.server.ResponseStatusException;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;

@RestController
public class RestWorkloadController {
    private static final int MAX_QUOTE_ITEMS = 100;
    private static final int BASIS_POINTS = 10_000;
    private static final java.util.regex.Pattern SAFE_FILENAME =
            java.util.regex.Pattern.compile("[A-Za-z0-9][A-Za-z0-9._-]{0,254}");
    private static final List<SerializedUser> SERIALIZATION_USERS = IntStream.rangeClosed(1, 100)
            .mapToObj(id -> new SerializedUser(id, "User " + id))
            .toList();

    @GetMapping("/")
    public void root() {
    }

    @GetMapping("/health")
    public Map<String, String> health() {
        return Map.of("status", "ok");
    }

    @GetMapping("/user/{id}")
    public String user(@PathVariable("id") @Min(0) int id) {
        return Integer.toString(id);
    }

    @PostMapping("/user")
    public String createUserLegacy() {
        return "";
    }

    @PostMapping(path = "/upload", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public UploadResult upload(@RequestPart("file") MultipartFile file) throws IOException {
        String filename = file.getOriginalFilename();
        if (filename == null || !SAFE_FILENAME.matcher(filename).matches()) {
            throw badRequest("invalid filename");
        }
        return new UploadResult(filename, file.getBytes().length);
    }

    @PostMapping(path = "/deserialization", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void deserialization(@RequestBody JsonNode body) {
        // Spring parses the body before invoking this method. The response is empty
        // so the route measures deserialization without response serialization.
    }

    @GetMapping("/serialization")
    public List<SerializedUser> serialization() {
        return SERIALIZATION_USERS;
    }

    @PostMapping(path = "/compute", consumes = MediaType.APPLICATION_JSON_VALUE)
    public QuoteResult compute(@Valid @RequestBody QuoteRequest request) {
        long subtotalCents = 0;
        long discountCents = 0;
        long taxCents = 0;
        for (QuoteItem item : request.items()) {
            long lineSubtotal = item.unitPriceCents() * item.quantity();
            long lineDiscount = lineSubtotal * item.discountBps() / BASIS_POINTS;
            long lineTax = (lineSubtotal - lineDiscount) * item.taxBps() / BASIS_POINTS;
            subtotalCents += lineSubtotal;
            discountCents += lineDiscount;
            taxCents += lineTax;
        }

        return new QuoteResult(subtotalCents, discountCents, taxCents,
                subtotalCents - discountCents + taxCents);
    }

    private static ResponseStatusException badRequest(String reason) {
        return new ResponseStatusException(HttpStatus.BAD_REQUEST, reason);
    }

    public record UploadResult(String filename, int size) {
    }

    public record SerializedUser(int id, String name) {
    }

    public record QuoteRequest(@NotEmpty @Size(max = MAX_QUOTE_ITEMS) List<@NotNull @Valid QuoteItem> items) {
    }

    public record QuoteItem(
            @NotNull @Min(0) @Max(1_000_000_000) Long unitPriceCents,
            @NotNull @Min(1) @Max(1_000) Integer quantity,
            @NotNull @Min(0) @Max(BASIS_POINTS) Integer discountBps,
            @NotNull @Min(0) @Max(BASIS_POINTS) Integer taxBps
    ) {
    }

    public record QuoteResult(long subtotalCents, long discountCents, long taxCents, long totalCents) {
    }
}
