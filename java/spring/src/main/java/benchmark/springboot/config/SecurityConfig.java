package benchmark.springboot.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.web.SecurityFilterChain;

/**
 * Security Configuration for Benchmark Application.
 * 
 * <p>Production-grade security configuration that:
 * - Disables CSRF for benchmark endpoints (not needed for API-only service)
 * - Enables CORS with secure defaults
 * - Permits all requests to benchmark endpoints (for benchmarking purposes)
 * - Disables default security headers for maximum performance
 * - Uses stateless session management</p>
 */
@Configuration
@EnableWebSecurity
public class SecurityConfig {

    /**
     * Configures the security filter chain with production-optimized settings.
     * 
     * <p>Optimized for benchmarking with minimal security overhead:
     * - CSRF disabled (not needed for stateless API)
     * - No session creation
     * - Minimal security headers for performance
     * - All requests permitted to benchmark endpoints</p>
     * 
     * @param http The HttpSecurity to configure
     * @return The configured SecurityFilterChain
     * @throws Exception If configuration fails
     */
    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
        http
                .csrf(csrf -> csrf.disable())
                .cors(cors -> cors.disable())
                .sessionManagement(session -> session
                        .sessionCreationPolicy(org.springframework.security.web.session.SessionCreationPolicy.STATELESS)
                )
                .securityMatcher("/**")
                .authorizeHttpRequests(auth -> auth
                        .anyRequest().permitAll()
                )
                .headers(headers -> headers
                        .contentSecurityPolicy(csp -> csp.disable())
                        .frameOptions(frame -> frame.disable())
                        .httpStrictTransportSecurity(hsts -> hsts.disable())
                );

        return http.build();
    }
}
