package benchmark.springboot.web;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.util.List;

@Data
@NoArgsConstructor
@SuperBuilder
public class WebResponse {
    private double inflows;
    private double outflows;
    private int totalResponses;
    private List<TransactionDto> transactions;
}
