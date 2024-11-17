package benchmark.springboot.web;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@ToString(callSuper = true)
@SuperBuilder(toBuilder = true)
@NoArgsConstructor
@Entity
@Table(name = "transaction")
public class Transaction {
    @Id
    @Column(name = "id", updatable = false, nullable = false)
    private UUID id;
    @Column(name = "category_id", nullable = false)
    private UUID categoryId;
    @Column(name = "amount", nullable = false)
    private Double amount;
    @Column(name = "currency", nullable = false)
    private String currency;
    @Column(name = "transaction_date_time", nullable = false)
    @Temporal(TemporalType.TIMESTAMP)
    private LocalDateTime transactionDateTime;
}
