package semios.api.filter;

import lombok.extern.slf4j.Slf4j;
import semios.api.model.dto.XSSRequestWrapper;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * @description: xss filter
 * @author: xiangbin
 * @create: 2023-01-09 10:43
 **/
@Slf4j
public class XssFilter implements Filter {

    private static final Set<String> ALLOWED_PATHS =
            Collections.unmodifiableSet(new HashSet<>(Arrays.asList("/dao/whitelist/proof")));// "/dao/create","/canvas/create/uri","/work/create","/canvas/edit","/dao/edit","/user/profile/save"

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {

    }

    @Override
    public void doFilter(ServletRequest req, ServletResponse res, FilterChain filterChain)
            throws IOException, ServletException {
        HttpServletRequest request = (HttpServletRequest) req;
        String path = request.getRequestURI().substring(request.getContextPath().length()).replaceAll("[/]+$", "");
        boolean allowedPath = ALLOWED_PATHS.contains(path);

        if (allowedPath) {
            log.info("[XssFilter]这里是不需要处理的url进入的方法path:{}", path);
            filterChain.doFilter(req, res);
            return;
        }

        // 使用包装器
        log.info("[XssFilter]filter process path:{}", path);
        XSSRequestWrapper XSSRequestWrapper = new XSSRequestWrapper((HttpServletRequest) req);
        filterChain.doFilter(XSSRequestWrapper, res);
    }

    @Override
    public void destroy() {

    }
}
