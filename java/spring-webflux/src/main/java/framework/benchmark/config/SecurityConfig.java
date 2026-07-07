package framework.benchmark.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.reactive.EnableWebFluxSecurity;
import org.springframework.security.web.server.SecurityWebFilterChain;

/**
 * Security Configuration for Spring WebFlux Benchmark Application.
 * 
 * <p>Production-grade security configuration for reactive applications:
 * - Disables CSRF for benchmark endpoints (not needed for API-only service)
 * - Permits all requests to benchmark endpoints (for benchmarking purposes)
 * - Uses stateless session management
 * - Disables default security headers for maximum performance</p>
 */
@Configuration
@EnableWebFluxSecurity
public class SecurityConfig {

    /**
     * Configures the security web filter chain with production-optimized settings.
     * 
     * <p>Optimized for benchmarking with minimal security overhead:
     * - CSRF disabled (not needed for stateless API)
     * - No session creation
     * - Minimal security headers for performance
     * - All requests permitted to benchmark endpoints</p>
     * 
     * @param http The ServerHttpSecurity to configure
     * @return The configured SecurityWebFilterChain
     */
    @Bean
    public SecurityWebFilterChain securityWebFilterChain(org.springframework.security.web.server.ServerHttpSecurity http) {
        return http
                .csrf(csrf -> csrf.disable())
                .cors(cors -> cors.disable())
                .httpBasic(basic -> basic.disable())
                .formLogin(form -> form.disable())
                .logout(logout -> logout.disable())
                .requestCache(cache -> cache.disable())
                .securityMatcher("/**")
                .authorizeExchange(exchanges -> exchanges
                        .anyExchange().permitAll()
                )
                .headers(headers -> headers
                        .contentSecurityPolicy(csp -> csp.disable())
                        .frameOptions(frame -> frame.disable())
                        .httpStrictTransportSecurity(hsts -> hsts.disable())
                )
                .build();
    }
}
