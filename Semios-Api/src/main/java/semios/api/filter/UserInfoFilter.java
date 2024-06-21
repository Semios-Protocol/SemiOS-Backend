package semios.api.filter;

import lombok.extern.slf4j.Slf4j;
import semios.api.model.dto.common.CustomHttpServletRequestWrapper;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.util.*;

/**
 * @description: filter
 * @author: xiangbin
 * @create: 2022-08-12 15:54
 **/
@Slf4j
public class UserInfoFilter implements Filter {
    private static final Set<String> ALLOWED_PATHS = Collections.unmodifiableSet(
            new HashSet<>(Arrays.asList("/user/login", "/transaction/call", "/method/call", "/user/profile/save")));// "/dao/create","/canvas/create/uri","/work/create"

    @Override
    public void doFilter(ServletRequest req, ServletResponse res, FilterChain chain)
            throws IOException, ServletException {
        CustomHttpServletRequestWrapper customHttpServletRequestWrapper = null;
        try {
            HttpServletRequest request = (HttpServletRequest) req;
            // HttpServletResponse response = (HttpServletResponse)res;
            String path = request.getRequestURI().substring(request.getContextPath().length()).replaceAll("[/]+$", "");
            boolean allowedPath = ALLOWED_PATHS.contains(path);

            if (allowedPath) {
                log.info("[userFilter]这里是不需要处理的url进入的方法path:{}", path);
                chain.doFilter(req, res);
                return;
            }

            log.info("[userFilter]filter path:{}", path);
            customHttpServletRequestWrapper = new CustomHttpServletRequestWrapper(request);
            log.info("[userFilter]customHttpServletRequestWrapper is null:{}",
                    Objects.isNull(customHttpServletRequestWrapper));
        } catch (Exception e) {
            log.error("customHttpServletRequestWrapper Error:", e);
        }
        chain.doFilter((Objects.isNull(customHttpServletRequestWrapper) ? req : customHttpServletRequestWrapper), res);
    }
}
