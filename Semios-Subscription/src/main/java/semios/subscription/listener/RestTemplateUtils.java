package semios.subscription.listener;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;

/**
 * @description: utils
 * @author: xiangbin
 * @create: 2022-04-14 14:15
 **/
@Configuration
public class RestTemplateUtils {


    @Bean
    public RestTemplate restTemplate() {

        SimpleClientHttpRequestFactory clientHttpRequestFactory
                = new SimpleClientHttpRequestFactory();
        clientHttpRequestFactory.setConnectTimeout(5 * 1000);
        clientHttpRequestFactory.setReadTimeout(10 * 1000);
        return new RestTemplate(clientHttpRequestFactory);

    }

}
