package benchmark.springboot.serialization;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class SerializationResponse {
    List<SerializationRequest> response;
    String name;
    Double count;
    Boolean required;
    LocalDateTime startDate;
}
