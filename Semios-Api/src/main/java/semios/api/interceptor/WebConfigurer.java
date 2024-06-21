package semios.api.interceptor;

import feign.RequestInterceptor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;
import semios.api.filter.UserInfoFilter;
import semios.api.filter.XssFilter;

import java.util.concurrent.Executor;
import java.util.concurrent.ThreadPoolExecutor;

/**
 * @author fjtan
 */
@Slf4j
@Configuration
@ConfigurationProperties(prefix = "cors")
public class WebConfigurer implements WebMvcConfigurer {

    public static String allowedDomain1;
    public static String allowedDomain2;
    public static String allowedDomain3;
    public static String allowedDomain4;
    public static String allowedDomain5;

    @Autowired
    private JWTInterceptor jwtInterceptor;

    @Autowired
    private UserInterceptor userInterceptor;

    @Autowired
    private CorsInterceptor corsInterceptor;

    @Autowired
    private CanvasPausedInterceptor canvasPausedInterceptor;

    @Autowired
    private DaoPausedInterceptor daoPausedInterceptor;

    @Autowired
    private D4APausedInterceptor d4APausedInterceptor;

    @Autowired
    private LogMdcInterceptor logMdcInterceptor;

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(logMdcInterceptor).addPathPatterns("/**");
        registry.addInterceptor(corsInterceptor).addPathPatterns("/**");
        registry.addInterceptor(d4APausedInterceptor).addPathPatterns("/**").excludePathPatterns(
                "/user/privacy/agreement", "/user/login", "/transaction/call", "/method/call", "/doc/**", "/canvas/info",
                "/canvas/edit/info", "/dao/user/authority", "/canvas/unmintedWorks", "/canvas/nfts", "/canvas/drbNfts",
                "/canvas/rewards", "/canvas/collections", "/canvas/rankings", "/dao/detail", "/dao/edit/detail",
                "/dao/user/authority", "/dao/gallery", "/dao/nfts", "/dao/nfts/v2", "/dao/unmintedWorks", "/dao/unmintedWorks/v2", "/dao/drbNfts", "/dao/drbNfts/v2", "/dao/rewards",
                "/dao/activity", "/dao/collections", "/dao/seedNodes", "/dao/rankings", "/dao/name/check", "/work/detail",
                "/work/favorite/user", "/work/explore/unmintedWorks", "/work/explore/unmintedWorks/v2", "/work/explore/nfts", "/work/explore/drbNfts",
                "/work/rankings/nfts", "/search/amount", "/search/works", "/search/works/v2", "/search/seedNodes", "/search/canvas", "/search/daos",
                "/user/cookie/info", "/dao/paused", "/dao/canvas", "/contract/abi", "/user/income/wallet/received",
                "/user/income/wallet/dao", "/user/income/wallet/unclaimed", "/user/logout", "/user/signature",
                "/user/profile/info", "/user/profile/save", "/canvas/create/uri", "/canvas/mycanvas",
                "/canvas/list/createWork", "/canvas/createWork/info", "/dao/canvas", "/dao/times", "/dao/list/createCanvas",
                "/dao/list/createWork", "/dao/mydao", "/favorite/dao", "/favorite/canvas", "/favorite/work", "/favorite/work/v2",
                "/token/activity", "/work/hold", "/work/hold/v2", "/work/minted", "/work/minted/v2", "/dao/available/number", "/dao/new/available",
                "/dao/analytics/**", "/dex/dao_erc20", "/dex/user", "/common/drb/info", "/event/**", "/sync/**");
        registry.addInterceptor(daoPausedInterceptor).addPathPatterns("/**").excludePathPatterns("/user/signature",
                "/user/privacy/agreement", "/user/login", "/transaction/call", "/method/call", "/doc/**", "/canvas/info",
                "/canvas/edit/info", "/canvas/unmintedWorks", "/canvas/nfts", "/canvas/drbNfts", "/canvas/rewards",
                "/canvas/collections", "/canvas/rankings", "/dao/detail", "/dao/edit/detail", "/dao/user/authority",
                "/dao/gallery", "/dao/nfts", "/dao/nfts/v2", "/dao/unmintedWorks", "/dao/unmintedWorks/v2", "/dao/drbNfts", "/dao/drbNfts/v2", "/dao/rewards", "/dao/activity",
                "/dao/collections", "/dao/seedNodes", "/dao/rankings", "/dao/name/check", "/work/detail", "/work/favorite/user",
                "/work/explore/unmintedWorks", "/work/explore/unmintedWorks/v2", "/work/explore/nfts", "/work/explore/nfts/v2", "/work/explore/drbNfts", "/work/rankings/nfts",
                "/search/amount", "/search/works", "/search/works/v2", "/search/seedNodes", "/search/canvas", "/search/daos", "/user/cookie/info", "/dao/paused",
                "/dao/canvas", "/dao/available/number", "/dao/new/available", "/dao/analytics/**", "/dex/dao_erc20",
                "/dex/user", "/common/drb/info", "/event/**", "/sync/**");
        registry.addInterceptor(canvasPausedInterceptor).addPathPatterns("/**").excludePathPatterns("/user/signature",
                "/user/privacy/agreement", "/user/login", "/transaction/call", "/method/call", "/doc/**", "/canvas/info",
                "/canvas/edit/info", "/canvas/unmintedWorks", "/canvas/nfts", "/canvas/drbNfts", "/canvas/rewards",
                "/canvas/collections", "/canvas/rankings", "/dao/detail", "/dao/edit/detail", "/dao/user/authority",
                "/dao/gallery", "/dao/nfts", "/dao/nfts/v2", "/dao/unmintedWorks", "/dao/unmintedWorks/v2", "/dao/drbNfts", "/dao/drbNfts/v2", "/dao/rewards", "/dao/activity",
                "/dao/collections", "/dao/seedNodes", "/dao/rankings", "/dao/name/check", "/work/detail", "/work/favorite/user",
                "/work/explore/unmintedWorks", "/work/explore/unmintedWorks/v2", "/work/explore/nfts", "/work/explore/nfts/v2", "/work/explore/drbNfts", "/work/rankings/nfts",
                "/search/amount", "/search/works", "/search/works/v2", "/search/seedNodes", "/search/canvas", "/search/daos", "/user/cookie/info", "/dao/paused",
                "/dao/canvas", "/dao/available/number", "/dao/new/available", "/dao/analytics/**", "/dex/dao_erc20",
                "/dex/user", "/common/drb/info", "/event/**", "/sync/**");
        registry.addInterceptor(userInterceptor).addPathPatterns("/**").excludePathPatterns("/user/signature",
                "/user/privacy/agreement", "/user/login", "/transaction/call", "/method/call", "/doc/**", "/canvas/info",
                "/canvas/unmintedWorks", "/canvas/nfts", "/canvas/drbNfts", "/canvas/rewards", "/canvas/collections",
                "/canvas/rankings", "/dao/detail", "/dao/gallery", "/dao/nfts", "/dao/nfts/v2", "/dao/unmintedWorks", "/dao/unmintedWorks/v2", "/dao/drbNfts", "/dao/drbNfts/v2",
                "/dao/rewards", "/dao/activity", "/dao/collections", "/dao/seedNodes", "/dao/rankings", "/dao/name/check", "/work/detail",
                "/work/favorite/user", "/work/explore/unmintedWorks", "/work/explore/unmintedWorks/v2", "/work/explore/nfts", "/work/explore/nfts/v2", "/work/explore/drbNfts",
                "/work/rankings/nfts", "/search/amount", "/search/works", "/search/works/v2", "/search/seedNodes", "/search/canvas", "/search/daos",
                "/user/cookie/info", "/dao/paused", "/dao/canvas", "/contract/abi", "/dao/new/available", "/user/logout",
                "/dao/user/authority", "/dao/analytics/**", "/dex/dao_erc20", "/dex/user", "/common/drb/info",
                "/dao/list/protodao", "/dao/protodao/member", "/dao/createWork/info", "/dao/transaction/hash", "/dao/protodao/authority",
                "/protodao/related", "/dao/times", "/dao/maincreator", "/dao/allcation/list", "/dao/allocation", "/event/**", "/sync/**", "/work/detail/nft", "/plan/together/tap", "/plan/together/list", "/plan/basic/info");
        registry.addInterceptor(jwtInterceptor).addPathPatterns("/dao/edit", "/canvas/edit", "/favorite/actions",
                "/favorite/cancel", "/user/profile/save", "/work/edit");
    }

    /**
     * 解决跨域请求
     */
    @Override
    public void addCorsMappings(CorsRegistry registry) {
        // 设置允许跨域的路由
        registry.addMapping("/**")
                // 是否允许证书（cookies）
                .allowCredentials(true)
                // 设置允许跨域请求的域名
                .allowedOrigins(allowedDomain1, allowedDomain2, allowedDomain3, allowedDomain4, allowedDomain5)
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
        log.info("allowedDomain1:{}", allowedDomain1);
        WebConfigurer.allowedDomain1 = allowedDomain1;
    }

    public void setAllowedDomain2(String allowedDomain2) {
        log.info("allowedDomain2:{}", allowedDomain2);
        WebConfigurer.allowedDomain2 = allowedDomain2;
    }

    public void setAllowedDomain3(String allowedDomain3) {
        log.info("allowedDomain3:{}", allowedDomain3);
        WebConfigurer.allowedDomain3 = allowedDomain3;
    }

    public void setAllowedDomain4(String allowedDomain4) {
        log.info("allowedDomain4:{}", allowedDomain4);
        WebConfigurer.allowedDomain4 = allowedDomain4;
    }

    public void setAllowedDomain5(String allowedDomain5) {
        log.info("allowedDomain4:{}", allowedDomain5);
        WebConfigurer.allowedDomain5 = allowedDomain5;
    }

    // add filter
    @Bean
    public FilterRegistrationBean<XssFilter> addTimeFilterBean() {
        FilterRegistrationBean<XssFilter> registration = new FilterRegistrationBean<XssFilter>();
        registration.setFilter(new XssFilter());
        registration.setName("xssFilter");
        // 请求中过滤器执行的先后顺序，值越小越先执行
        registration.setOrder(1);
        registration.addUrlPatterns("/*");
        return registration;
    }

    @Bean
    public FilterRegistrationBean<UserInfoFilter> addLogFilterBean() {
        FilterRegistrationBean<UserInfoFilter> registration = new FilterRegistrationBean<UserInfoFilter>();
        registration.setFilter(new UserInfoFilter());
        registration.setName("userInfoFilter");
        // 请求中过滤器执行的先后顺序，值越小越先执行
        registration.setOrder(2);
        registration.addUrlPatterns("/*");
        return registration;
    }

    @Bean
    public RequestInterceptor requestInterceptor() {
        return new TraceLogFeignInterceptor();
    }

    @Bean("taskExecutor")
    public Executor taskExecutor() {

        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        //获取到服务器的cpu内核
//        int i = Runtime.getRuntime().availableProcessors();
        //核心池大小
        executor.setCorePoolSize(4);
        //最大线程数
        executor.setMaxPoolSize(4);
        //队列长度
        executor.setQueueCapacity(50);
        //线程空闲时间
        executor.setKeepAliveSeconds(100);
        //线程前缀名称
        executor.setThreadNamePrefix("Async-Task");
        executor.setTaskDecorator(new TraceLogMdcTaskDecorator());
        //配置拒绝策略
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.AbortPolicy());
        return executor;
    }


    // cookie长时间写入失败的原因，必须去掉这个
    // @Bean
    // public WebServerFactoryCustomizer<TomcatServletWebServerFactory> cookieProcessorCustomizer() {
    // return (factory) -> factory.addContextCustomizers(
    // (context) -> context.setCookieProcessor(new LegacyCookieProcessor()));
    // }
}
