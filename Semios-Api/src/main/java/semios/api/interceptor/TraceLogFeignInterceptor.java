package semios.api.interceptor;

import feign.RequestInterceptor;
import feign.RequestTemplate;
import semios.api.model.dto.common.ProtoDaoConstant;

import java.util.UUID;

/**
 * @description: TraceLogFeignInterceptor
 * @author: xiangbin
 * @create: 2023-05-25 17:40
 **/
public class TraceLogFeignInterceptor implements RequestInterceptor {

    @Override
    public void apply(RequestTemplate requestTemplate) {
        requestTemplate.header(ProtoDaoConstant.TRACE_ID, UUID.randomUUID().toString().replaceAll("-", ""));
    }

}
