package benchmark.springboot;

import benchmark.springboot.serialization.SerializationRequest;
import benchmark.springboot.serialization.SerializationResponse;
import benchmark.springboot.web.Category;
import benchmark.springboot.web.CategoryRepository;
import benchmark.springboot.web.Transaction;
import benchmark.springboot.web.TransactionDto;
import benchmark.springboot.web.TransactionRepository;
import benchmark.springboot.web.WebRequest;
import benchmark.springboot.web.WebResponse;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.val;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestClient;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;

@SpringBootApplication
public class BenchmarkApplication {

    public static void main(String[] args) {
        SpringApplication.run(BenchmarkApplication.class, args);
    }

    @RestController
    @RequiredArgsConstructor
    public class BenchmarkController {

        @GetMapping("/")
        public void root() {
        }

        // --

        @GetMapping("/external-service")
        @SneakyThrows
        public void externalService() {
            Thread.sleep(500);
        }

        // --

        @PostMapping("/serialization")
        public SerializationResponse serialization(
                @RequestBody List<SerializationRequest> request,
                @RequestParam String name,
                @RequestParam Double count,
                @RequestParam Boolean required,
                @RequestParam LocalDateTime startDate
        ) {
            return new SerializationResponse(
                    request.stream().map(it ->
                            new SerializationRequest(
                                    it.getName() + "_2",
                                    it.getCount() + 2,
                                    !it.getRequired(),
                                    it.getStartDate().plusDays(2)
                            )
                    ).toList(),
                    name + "_2",
                    count + 2,
                    !required,
                    startDate.plusDays(1)
            );
        }

        // --

        @Value("${server.port}")
        String serverPort;
        RestClient webClient = RestClient.builder().build();
        String file;
        {
            try {
                file = new String(
                        Files.readAllBytes(
                                Path.of(
                                        new File("./../../../database_transactions.csv")
                                                .toURI()
                                )
                        )
                );
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        @SneakyThrows
        @GetMapping("/file")
        public String getFile() {
            return file;
        }

        @GetMapping("/http-client")
        public String httpClient() {
            return webClient.get()
                    .uri("http://localhost:" + serverPort + "/file")
                    .retrieve()
                    .body(String.class);
        }

        // --

        private final TransactionRepository transactionRepository;
        private final CategoryRepository categoryRepository;

        TransactionDto mapToTransactionDto(Transaction tx, List<Category> categories) {
            return TransactionDto.builder()
                    .id(tx.getId())
                    .categoryName(categories.stream().filter(c -> c.getId().equals(tx.getCategoryId())).findFirst().get().getName())
                    .amountEUR(tx.getAmount())
                    .amountUSD(convertToUsd(tx.getAmount()))
                    .transactionDateTime(tx.getTransactionDateTime())
                    .build();
        }

        @SneakyThrows
        double convertToUsd(double euro) {
            Thread.sleep(10);
            return euro * 1.09d;
        }

        @PostMapping("/web")
        public WebResponse web(
                @RequestBody WebRequest body
        ) {

            val transactions = transactionRepository
                    .findAllByTransactionDateTimeBetween(
                            body.getStartDate(),
                            body.getEndDate(),
                            PageRequest.of(body.getPage(), body.getLimit(), Sort.by("id")));

            val categoryIds = transactions.stream()
                    .map(Transaction::getCategoryId)
                    .collect(Collectors.toMap(p -> p, p -> p, (p, q) -> p))
                    .values();

            val categories = categoryRepository.findAllById(categoryIds);

            val transactionsDTO = transactions.stream()
                    .map(tx -> mapToTransactionDto(tx, categories))
                    .toList();

            val inflows = transactions.stream().mapToDouble(Transaction::getAmount).filter(amount -> amount > 0).sum();
            val outflows = transactions.stream().mapToDouble(Transaction::getAmount).filter(amount -> amount < 0).sum();

            val response = WebResponse.builder()
                    .inflows(inflows)
                    .outflows(outflows)
                    .totalResponses(transactions.size())
                    .transactions(transactionsDTO)
                    .build();

            return response;

        }

    }
}

