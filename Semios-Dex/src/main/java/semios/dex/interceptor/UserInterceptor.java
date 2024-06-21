package semios.dex.interceptor;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.ModelAndView;
import semios.dex.model.dto.common.*;
import semios.dex.utils.CookieUtil;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * 用户登陆拦截器
 *
 * @description:
 * @author: xiangbin
 * @create: 2022-08-09 14:35
 **/
@Slf4j
@Component
public class UserInterceptor implements HandlerInterceptor {

    @Value("${domain.url}")
    private String domainUrl;

    @Value("${domain.url.semios}")
    private String domainUrlSemios;


    @Override
    public void afterCompletion(HttpServletRequest request, HttpServletResponse response, Object obj, Exception e)
            throws Exception {

    }

    @Override
    public void postHandle(HttpServletRequest request, HttpServletResponse response, Object obj, ModelAndView mav)
            throws Exception {

    }

    /**
     * 拦截每个请求
     */
    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object obj) {
        String path = request.getRequestURI().substring(request.getContextPath().length()).replaceAll("[/]+$", "");
        log.info("[UserInterceptor]进入拦截器！path:{}", path);

        if ("OPTIONS".equals(request.getMethod().toUpperCase())) {
            return true;
        }
        String origin = request.getHeader("Origin");
        // 后端响应基本配置项
        if (!StringUtils.isEmpty(origin)) {
            response.setHeader("Access-Control-Allow-Origin", origin);
            response.setHeader("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
            response.setHeader("Access-Control-Allow-Methods", "POST, GET, OPTIONS");
            response.setHeader("Access-Control-Allow-Credentials", "true");
        }
        String useraddress = CookieUtil.getUserAddressFromCookie(request, Dao4ArtDexConstant.COOKIE_ADDRESS);
        String usersession = CookieUtil.getUserAddressFromCookie(request, Dao4ArtDexConstant.SESSION_ADDRESS);
        log.info("[UserInterceptor]useraddress:{} usersession:{}", useraddress, usersession);

        try {
            if (StringUtils.isAllEmpty(useraddress, usersession)) {
                return true;
            }
            if (StringUtils.isAnyBlank(useraddress, usersession) || !useraddress.equalsIgnoreCase(usersession)) {
                log.info("[UserInterceptor]用户未登陆！");
                // 这里只要在错误处直接打印 返回前端 让前端根据错误码进行处理即可
                response.setContentType("application/json;charset=utf-8");
                Result<Object> result = new Result<>();
                result.setResultCode(ResultDesc.USER_ERROR.getResultCode());
                result.setResultDesc("please login.");
                response.getWriter().write(JSON.toJSONString(result));
                Cookie addresssession = new Cookie(Dao4ArtDexConstant.SESSION_ADDRESS, Dao4ArtDexConstant.SESSION_ADDRESS);
                if (StringUtils.isEmpty(origin) || !origin.contains(DexConstant.LOCAL_HOST)) {
                    log.info("[UserInterceptor]进入拦截器！origin:{}", origin);
                    if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                        addresssession.setDomain(domainUrl);
                    }
                    if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                        addresssession.setDomain(domainUrlSemios);
                    }
                }
                addresssession.setPath("/");
                // 去掉为session级别的cookie
                addresssession.setMaxAge(0);
                response.addCookie(addresssession);
                return false;
            }

            useraddress = useraddress.toLowerCase();
            if (request instanceof CustomHttpServletRequestWrapper) {
                log.info("[UserInterceptor]CustomHttpServletRequestWrapper path:{}", path);
                CustomHttpServletRequestWrapper requestWrapper = (CustomHttpServletRequestWrapper) request;
                String body = requestWrapper.getBody();
                if (!StringUtils.isBlank(body)) {
                    JSONObject param = JSONObject.parseObject(body);
                    param.put(Dao4ArtDexConstant.COOKIE_USER_ADDRESS, useraddress);
                    requestWrapper.setBody(JSON.toJSONString(param));
                } else {
                    JSONObject param = new JSONObject();
                    param.put(Dao4ArtDexConstant.COOKIE_USER_ADDRESS, useraddress);
                    requestWrapper.setBody(JSON.toJSONString(param));
                }

            } else {
                log.warn("[UserInterceptor]request instanceof CustomHttpServletRequestWrapper false path:{}", path);
            }
            return true;
        } catch (Exception e) {
            log.error("[UserInterceptor]error:", e);
            throw new RuntimeException("System exception, please try again later!");
        }
    }

}