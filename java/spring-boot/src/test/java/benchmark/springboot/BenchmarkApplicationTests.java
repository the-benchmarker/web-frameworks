package benchmark.springboot;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@SpringBootTest
@AutoConfigureMockMvc
public class BenchmarkApplicationTests {
  @Autowired
  private MockMvc mockMvc;

  @Test
  public void contextLoads() {
  }

  @Test
  public void testGetRoot() throws Exception {
    mockMvc.perform(get("/")).andExpect(content().string("")).andExpect(status().isOk());
  }

  @Test
  public void testGetUserIdNotInt() throws Exception {
    String userId = "the_id_of_a_user";
    mockMvc.perform(get("/user/{id}", userId)).andExpect(status().isBadRequest());
  }

  @Test
  public void testGetUserId() throws Exception {
    String userId = "1234";
    mockMvc.perform(get("/user/{id}", userId)).andExpect(status().isOk())
        .andExpect(content().string(userId));
  }

  @Test
  public void testPostUser() throws Exception {
    mockMvc.perform(post("/user")).andExpect(content().string("")).andExpect(status().isOk());
  }

}
