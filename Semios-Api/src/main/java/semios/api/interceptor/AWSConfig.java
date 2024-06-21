package semios.api.interceptor;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.AWSStaticCredentialsProvider;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3ClientBuilder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.client.ClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;
import semios.api.service.common.SSL;

import javax.annotation.Resource;


/**
 * @description: AWSConfig
 * @author: xiangbin
 * @create: 2022-08-16 16:00
 **/
@Slf4j
@Configuration
public class AWSConfig {

    @Value("${aws_access_key}")
    private String accessKeyId;

    @Value("${aws_secret_key}")
    private String secretKey;

    //从配置文件获取
    @Bean
    public AWSCredentials credentials() {
        return new BasicAWSCredentials(accessKeyId, secretKey);
    }

    @Bean
    public AmazonS3 amazonS3() {
        AmazonS3 s3client = AmazonS3ClientBuilder
                .standard()
                .withCredentials(new AWSStaticCredentialsProvider(credentials()))
                .withRegion(Regions.AP_SOUTHEAST_1)
                .build();
        return s3client;
    }


    @Bean(name = "restTemplateSSl")
    @Resource(name = "simpleClientHttpRequestFactory") // 在多ClientHttpRequestFactory的时候指定用哪个
    public RestTemplate restTemplate(ClientHttpRequestFactory factory) {
        return new RestTemplate(factory);
    }

    @Bean
    public ClientHttpRequestFactory simpleClientHttpRequestFactory() {
        SSL factory = new SSL();
        factory.setReadTimeout(60000);
        factory.setConnectTimeout(30000);//单位为ms
        return factory;
    }

}
