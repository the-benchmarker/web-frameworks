package benchmark.springboot.web;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@NoArgsConstructor
public class WebRequest {
    private LocalDateTime startDate;
    private LocalDateTime endDate;
    private int page;
    private int limit;
}
