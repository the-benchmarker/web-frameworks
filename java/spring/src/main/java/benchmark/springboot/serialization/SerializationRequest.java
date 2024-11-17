package benchmark.springboot.serialization;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class SerializationRequest {
    String name;
    Double count;
    Boolean required;
    LocalDateTime startDate;
}
