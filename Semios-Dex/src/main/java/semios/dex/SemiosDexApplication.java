package semios.dex;

import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.ConfigurableApplicationContext;
import semios.dex.utils.SpringBeanUtil;

/**
 * @description: Application
 * @author: xiangbin
 * @create: 2022-04-07 16:47
 **/
@SpringBootApplication
@MapperScan("semios.dex.mapper")
@EnableFeignClients(basePackages = {"semios.dex.service.feign"})
public class SemiosDexApplication {

    public static void main(String[] args) {
        ConfigurableApplicationContext configurableApplicationContext = SpringApplication.run(SemiosDexApplication.class, args);

        SpringBeanUtil.applicationContext = configurableApplicationContext;
    }
}
