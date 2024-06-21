package semios.api.interceptor;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.ModelAndView;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

@Slf4j
@Component
public class CorsInterceptor implements HandlerInterceptor {

    @Override
    public void afterCompletion(HttpServletRequest request, HttpServletResponse response, Object obj, Exception e)
            throws Exception {

    }

    @Override
    public void postHandle(HttpServletRequest request, HttpServletResponse response, Object obj, ModelAndView mav)
            throws Exception {

    }

    // 拦截每个请求
    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object obj) {
        log.info("[CorsInterceptor]进入拦截器！");
        try {
            if ("OPTIONS".equals(request.getMethod().toUpperCase())) {
                return true;
            }
            String origin = request.getHeader("Origin");
            log.info("[CorsInterceptor]origin:{}", origin);
            // 后端响应基本配置项
            if (!StringUtils.isEmpty(origin)) {
                // response.setHeader("Access-Control-Allow-Origin", origin);
                response.setHeader("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
                response.setHeader("Access-Control-Allow-Methods", "POST, GET, OPTIONS");
                response.setHeader("Access-Control-Allow-Credentials", "true");
                // response.setHeader(HttpHeaders.SET_COOKIE, "SameSite=None;");
            }

            return true;
        } catch (Exception e) {
            log.error("[CorsInterceptor]error:", e);
        }
        return false;
    }

}
