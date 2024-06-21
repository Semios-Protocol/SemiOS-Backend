package semios.gateway;

import com.alibaba.druid.spring.boot.autoconfigure.DruidDataSourceAutoConfigure;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.autoconfigure.jdbc.DataSourceTransactionManagerAutoConfiguration;
import org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration;
import org.springframework.context.ConfigurableApplicationContext;
import semios.gateway.utils.SpringBeanUtil;

/**
 * @description: Application
 * @author: xiangbin
 * @create: 2022-04-07 16:47
 **/
@SpringBootApplication(exclude = {DataSourceAutoConfiguration.class, DataSourceTransactionManagerAutoConfiguration.class, DruidDataSourceAutoConfigure.class, HibernateJpaAutoConfiguration.class})
//@MapperScan("semios.gateway.boot.mapper")
//@EnableFeignClients(basePackages = {"semios.gateway.boot.service.feign"})
public class SemiosGatewayApplication {

    public static void main(String[] args) {
        ConfigurableApplicationContext configurableApplicationContext = SpringApplication.run(SemiosGatewayApplication.class, args);

        SpringBeanUtil.applicationContext = configurableApplicationContext;
    }
}
