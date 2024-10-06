package benchmark.springboot.web;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@NoArgsConstructor
@SuperBuilder
public class TransactionDto {
    private UUID id;
    private String categoryName;
    private Double amountEUR;
    private Double amountUSD;
    private LocalDateTime transactionDateTime;
}
