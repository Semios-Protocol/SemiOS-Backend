package semios.api.interceptor;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.ModelAndView;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.exceptions.PausedException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * D4A停机拦截器
 *
 * @description:
 * @author: xiangbin
 * @create: 2022-08-09 14:35
 **/
@Slf4j
@Component
public class D4APausedInterceptor implements HandlerInterceptor {

    @Override
    public void afterCompletion(HttpServletRequest request, HttpServletResponse response, Object obj, Exception e)
            throws Exception {

    }

    @Override
    public void postHandle(HttpServletRequest request, HttpServletResponse response, Object obj, ModelAndView mav)
            throws Exception {

    }

    // 拦截每个写请求，直接返回异常
    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object obj) {
        String path = request.getRequestURI().substring(request.getContextPath().length()).replaceAll("[/]+$", "");
        log.info("[D4APausedInterceptor]进入拦截器！path:{}", path);

        if ("OPTIONS".equals(request.getMethod().toUpperCase())) {
            return true;
        }

        if (ProtoDaoConstant.D4APause) {
            throw new PausedException("D4A This function is suspended for security reasons.");
        }
        return true;
        // return false;
    }

}