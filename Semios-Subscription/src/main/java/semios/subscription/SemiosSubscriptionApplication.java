package semios.subscription;

import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;
import semios.subscription.utils.SpringBeanUtil;

/**
 * @description: Application
 * @author: xiangbin
 * @create: 2022-04-07 16:47
 **/
@SpringBootApplication
@MapperScan("semios.subscription.mapper")
public class SemiosSubscriptionApplication {

    public static void main(String[] args) {
        ConfigurableApplicationContext configurableApplicationContext = SpringApplication.run(SemiosSubscriptionApplication.class, args);

        SpringBeanUtil.applicationContext = configurableApplicationContext;
    }
}
