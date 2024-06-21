package semios.dex.interceptor;

import org.slf4j.MDC;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;
import semios.dex.model.dto.common.Dao4ArtDexConstant;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.UUID;

/**
 * @description: 日志添加traceId
 * @author: xiangbin
 * @create: 2022-03-29 14:35
 **/
@Component
public class LogMdcInterceptor implements HandlerInterceptor {

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler)
            throws Exception {
        // 如果有上层调用就用上层的ID
        String traceId = request.getHeader(Dao4ArtDexConstant.TRACE_ID);
        if (traceId == null) {
            traceId = UUID.randomUUID().toString().replaceAll("-", "");
        }

        MDC.put(Dao4ArtDexConstant.TRACE_ID, traceId);
        return true;
    }

    @Override
    public void afterCompletion(HttpServletRequest request, HttpServletResponse response, Object handler, Exception ex)
            throws Exception {
        // 调用结束后删除
        MDC.remove(Dao4ArtDexConstant.TRACE_ID);
    }

}
