//package semios.gateway.interceptor;
//
//import lombok.extern.slf4j.Slf4j;
//import org.springframework.cloud.gateway.filter.GatewayFilterChain;
//import org.springframework.cloud.gateway.filter.GlobalFilter;
//import org.springframework.core.Ordered;
//import org.springframework.core.io.buffer.DataBuffer;
//import org.springframework.core.io.buffer.DataBufferUtils;
//import org.springframework.http.HttpCookie;
//import org.springframework.http.HttpRequest;
//import org.springframework.http.HttpStatus;
//import org.springframework.http.server.reactive.ServerHttpRequest;
//import org.springframework.http.server.reactive.ServerHttpResponse;
//import org.springframework.stereotype.Component;
//import org.springframework.util.MultiValueMap;
//import org.springframework.util.StringUtils;
//import org.springframework.web.server.ServerWebExchange;
//import org.springframework.web.server.WebSession;
//import org.springframework.web.util.UriComponentsBuilder;
//import reactor.core.publisher.Flux;
//import reactor.core.publisher.Mono;
//import semios.gateway.boot.model.dto.common.Dao4ArtConstant;
//import semios.gateway.boot.utils.JacksonUtil;
//
//import javax.servlet.http.HttpServletRequest;
//import javax.servlet.http.HttpSession;
//import java.net.URI;
//import java.nio.CharBuffer;
//import java.nio.charset.StandardCharsets;
//import java.util.Map;
//import java.util.concurrent.atomic.AtomicReference;
//
///**
// * @description: AuthenticationFilter
// * @author: xiangbin
// * @create: 2023-05-28 13:11
// **/
//@Slf4j
//@Component
//public class AuthenticationFilter implements GlobalFilter, Ordered {
//
//
//
//    @Override
//    public Mono<Void> filter(ServerWebExchange exchange, GatewayFilterChain chain) {
//        //获得请求和响应对象
//        ServerHttpRequest req = exchange.getRequest();
//        ServerHttpResponse response = exchange.getResponse();
//
//        try{
//            String path = req.getPath().toString();
//            log.info("[AuthenticationFilter] login path:{}", path);
//
//
//            if(Dao4ArtConstant.LOGIN_PATH.equalsIgnoreCase(path)) {
//                // 获取请求体
//                Flux<DataBuffer> body = req.getBody();
//                AtomicReference<String> bodyRef = new AtomicReference<>();
//                body.subscribe(buffer -> {
//                    CharBuffer charBuffer = StandardCharsets.UTF_8.decode(buffer.asByteBuffer());
//                    DataBufferUtils.release(buffer);
//                    bodyRef.set(charBuffer.toString());
//                });
//                String result = bodyRef.get();
//
//                Map<String, Object> paramMap = JacksonUtil.json2map(result);
//                String userAddress = (String) paramMap.get("userAddress");
//                log.info("[AuthenticationFilter] login userAddress:{}", userAddress);
//
//                MultiValueMap<String, HttpCookie> cookies = req.getCookies();
//                String user = String.valueOf(cookies.get("user"));
//                log.info("[AuthenticationFilter] login cookies:{}", JacksonUtil.obj2json(cookies));
//                log.info("[AuthenticationFilter] login user:{}", user);
//
//
//                //String userAddress = req.getQueryParams().getFirst("userAddress"); //get方法的参数
//                URI reuri = req.getURI();
//                log.info("[AuthenticationFilter] login reuri:{}", reuri);
//                exchange.getSession().flatMap(session -> {
//                    String userAdd = session.getAttribute("userAddress");
//                    System.out.println(userAdd);
////                    log.info("[AuthenticationFilter] login userAdd:{}", userAdd);
//                    if(StringUtils.isEmpty(userAdd)){
//                        session.getAttributes().put("userAddress",  "123456");
//                    }
//                    //继续执行
//                    return null;
//                });
//
//            }
//
//            Mono<Void> mono = chain.filter(exchange);
//            return mono;
//        }catch (Exception ex){
//            log.error("token解析失败",ex);
//            //返回验证失败的响应信息
//            response.setStatusCode(HttpStatus.UNAUTHORIZED);
//
//            DataBuffer wrap = response.bufferFactory().wrap("验证错误，需要登录".getBytes());
//
//            return response.writeWith(Mono.just(wrap));
//        }
//    }
//
//    @Override
//    public int getOrder() {
//        return 5;
//    }
//}
