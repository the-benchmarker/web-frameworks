package benchmark.springboot.web;

import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

public interface TransactionRepository extends JpaRepository<Transaction, UUID> {
    List<Transaction> findAllByTransactionDateTimeBetween(LocalDateTime startDate, LocalDateTime endDate, Pageable pageable);
}
