package benchmark.springboot.web;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import java.util.UUID;

@Data
@ToString(callSuper = true)
@SuperBuilder(toBuilder = true)
@NoArgsConstructor
@Entity
@Table(name = "category")
public class Category {
    @Id
    @Column(name = "id", updatable = false, nullable = false)
    private UUID id;
    @Column(name = "name", nullable = false)
    private String name;
}
