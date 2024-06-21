//package semios.gateway.interceptor;
//
//import org.springframework.boot.context.properties.ConfigurationProperties;
//import org.springframework.context.annotation.Configuration;
//import org.springframework.web.servlet.config.annotation.CorsRegistry;
//import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
//import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;
//
///**
// * @author fjtan
// */
//@Configuration
//@ConfigurationProperties(prefix = "cors")
//public class WebConfigurer implements WebMvcConfigurer {
//
//    private static String allowedDomain1;
//    private static String allowedDomain2;
//
//
//    @Override
//    public void addInterceptors(InterceptorRegistry registry) {
////        registry.addInterceptor(loginInterceptor).addPathPatterns("/system/**");
//    }
//
//    /**
//     * 解决跨域请求
//     *
//     * @return
//     */
//    @Override
//    public void addCorsMappings(CorsRegistry registry) {
//        // 设置允许跨域的路由
//        registry.addMapping("/**")
//            // 是否允许证书（cookies）
//            .allowCredentials(true)
//            // 设置允许跨域请求的域名
////            .allowedOrigins(allowedDomain1, allowedDomain2)
//            .allowedOriginPatterns("*")
//            // 设置允许的header
//            .allowedHeaders("*")
//            // 设置允许的方法
//            .allowedMethods("*")
//            // 跨域允许时间
//            .maxAge(3600);
//
//        WebMvcConfigurer.super.addCorsMappings(registry);
//    }
//
//    public void setAllowedDomain1(String allowedDomain1) {
//        WebConfigurer.allowedDomain1 = allowedDomain1;
//    }
//
//    public void setAllowedDomain2(String allowedDomain2) {
//        WebConfigurer.allowedDomain2 = allowedDomain2;
//    }
//
//}
