package semios.gateway.schedule;

import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;

/**
 * @description: gas
 * @author: xiangbin
 * @create: 2022-04-24 13:48
 **/
@Slf4j
@Configuration
@EnableScheduling
@EnableAsync
public class ProtoDaoSchedule {


//    @Async
//    @Scheduled(cron = "0/30 * * * * ? ")
//    public void helloWorld(){
//
//        try {
//            log.info("[HelloWorldSchedule] hello world");
//        } catch (Exception e){
//            log.error("[HelloWorldSchedule] exception e:", e);
//        }
//    }

}
