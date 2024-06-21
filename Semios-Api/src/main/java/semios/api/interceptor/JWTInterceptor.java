package semios.api.interceptor;

import com.alibaba.fastjson.JSON;
import io.jsonwebtoken.Claims;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.ModelAndView;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.common.UserHash;
import semios.api.utils.CookieUtil;
import semios.api.utils.JacksonUtil;
import semios.api.utils.JjwtUtil;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * @description: jwt
 * @author: xiangbin
 * @create: 2022-08-09 14:35
 **/
@Slf4j
@Component
public class JWTInterceptor implements HandlerInterceptor {

    public static void main(String[] args) {
        String subject = "{\"userhash\": \"0x14611sdfsdfsd\"}";
        UserHash userHash = JacksonUtil.json2pojo(subject, UserHash.class);
        System.out.println(userHash.getUserhash());

    }

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
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object obj) throws Exception {
        String path = request.getRequestURI().substring(request.getContextPath().length()).replaceAll("[/]+$", "");
        log.info("[JWTInterceptor]开始！path:{}", path);
        if ("OPTIONS".equals(request.getMethod().toUpperCase())) {
            return true;
        }
        // 通过url得到token请求头是否包含Authorization
        String token = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_TOKEN);
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        log.info("token:{} userAddress:{}", token, userAddress);

        try {
            // 检测请求头是否为空
            if (token == null) {
                log.warn("用户未登录，验证失败");
                // return true;

                // 这里只要在错误处直接打印 返回前端 让前端根据错误码进行处理即可
                response.setContentType("application/json;charset=utf-8");
                Result result = new Result();
                result.setResultCode(ResultDesc.USER_ERROR.getResultCode());
                result.setResultDesc("please login.");
                response.getWriter().write(JSON.toJSONString(result));

                return false;
            }

            if (JjwtUtil.verifyToken(token)) {
                Claims c = JjwtUtil.parseJWT(token);
                String subject = c.getSubject();
                UserHash userHash = JacksonUtil.json2pojo(subject, UserHash.class);
                log.info("用户[ " + c.get("username") + ":" + userHash.getUserhash() + " ]已是登录状态");
                if (StringUtils.isBlank(userAddress) || StringUtils.isBlank(userHash.getUserhash())
                        || !userAddress.equalsIgnoreCase(userHash.getUserhash())) {
                    log.error("token解析错误，验证失败 userAddress:{} userHash:{}", userAddress, userHash.getUserhash());
                    throw new RuntimeException("Unsigned, please sign again before operation!");
                }
                log.info("[JWTInterceptor]结束！");
                return true;
            }

            log.error("token解析错误，验证失败");
            // response.getWriter().write("未签名，请重新签名后操作");

            throw new RuntimeException("Unsigned, please sign again before operation!");
        } catch (Exception e) {
            log.error("[JWTInterceptor]error:", e);
            throw new RuntimeException("System exception, please try again later!");
        }
        // return false;
    }

}