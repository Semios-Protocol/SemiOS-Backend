package semios.gateway.listener;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Configuration;

/**
 * @description: listener
 * @author: xiangbin
 * @create: 2022-04-22 16:10
 **/
@Slf4j
@Configuration
public class ApplicationStartListener implements CommandLineRunner {


    @Override
    public void run(String... args) throws Exception {
        try {
            log.info("[ApplicationStartListener] init...");
        } catch (Exception e) {
            log.error("[ApplicationStartListener] logs only for exception e:", e);
            throw new Exception("start error!");
        }
    }


}
