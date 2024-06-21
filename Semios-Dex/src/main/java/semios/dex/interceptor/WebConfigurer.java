package semios.dex.interceptor;

import feign.RequestInterceptor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

/**
 * @author fjtan
 */
@Configuration
@ConfigurationProperties(prefix = "cors")
public class WebConfigurer implements WebMvcConfigurer {

    private static String allowedDomain1;
    private static String allowedDomain2;
    private static String allowedDomain3;
    private static String allowedDomain4;

    @Autowired
    private LogMdcInterceptor logMdcInterceptor;

    @Autowired
    private UserInterceptor userInterceptor;

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(logMdcInterceptor).addPathPatterns("/**");
        registry.addInterceptor(userInterceptor).addPathPatterns("/liquidity/erc20", "/liquidity/erc20_token");
    }

    /**
     * 解决跨域请求
     *
     * @return
     */
    @Override
    public void addCorsMappings(CorsRegistry registry) {
        // 设置允许跨域的路由
        registry.addMapping("/**")
                // 是否允许证书（cookies）
                .allowCredentials(true)
                // 设置允许跨域请求的域名
                .allowedOrigins(allowedDomain1, allowedDomain2, allowedDomain3, allowedDomain4)
                // .allowedOriginPatterns("*")
                // 设置允许的header
                .allowedHeaders("*")
                // 设置允许的方法
                .allowedMethods("*")
                // 跨域允许时间
                .maxAge(3600);

        WebMvcConfigurer.super.addCorsMappings(registry);
    }

    public void setAllowedDomain1(String allowedDomain1) {
        WebConfigurer.allowedDomain1 = allowedDomain1;
    }

    public void setAllowedDomain2(String allowedDomain2) {
        WebConfigurer.allowedDomain2 = allowedDomain2;
    }

    public void setAllowedDomain3(String allowedDomain3) {
        WebConfigurer.allowedDomain3 = allowedDomain3;
    }

    public void setAllowedDomain4(String allowedDomain4) {
        WebConfigurer.allowedDomain4 = allowedDomain4;
    }

    @Bean
    public FilterRegistrationBean<UserInfoFilter> addLogFilterBean() {
        FilterRegistrationBean<UserInfoFilter> registration = new FilterRegistrationBean<UserInfoFilter>();
        registration.setFilter(new UserInfoFilter());
        registration.setName("userInfoFilter");
        // 请求中过滤器执行的先后顺序，值越小越先执行  要保证这个最后执行
        registration.setOrder(2);
        registration.addUrlPatterns("/*");
        return registration;
    }


    @Bean
    public RequestInterceptor requestInterceptor() {
        return new TraceLogFeignInterceptor();
    }

}
