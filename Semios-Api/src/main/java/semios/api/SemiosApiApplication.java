package semios.api;

import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.web.servlet.ServletComponentScan;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.ConfigurableApplicationContext;
import semios.api.utils.SpringBeanUtil;

import javax.annotation.PostConstruct;
import java.util.TimeZone;

/**
 *
 * @description: Application
 * @author: xiangbin
 * @create: 2022-04-07 16:47
 **/
@ServletComponentScan //@ServletComponentScan扫描带@WebFilter、@WebServlet、@WebListener
@SpringBootApplication
@MapperScan("semios.api.mapper")
@EnableFeignClients(basePackages = {"semios.api.service.feign"})
public class SemiosApiApplication {

    public static void main(String[] args) {
        ConfigurableApplicationContext configurableApplicationContext = SpringApplication.run(SemiosApiApplication.class, args);

        SpringBeanUtil.applicationContext = configurableApplicationContext;
    }

    @PostConstruct
    void started() {
        TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
//        TimeZone.setDefault(TimeZone.getTimeZone("Asia/Shanghai"));
        //      TimeZone.setDefault(TimeZone.getTimeZone("GMT+8"));
    }
}
