package semios.dex.interceptor;

import feign.RequestInterceptor;
import feign.RequestTemplate;
import semios.dex.model.dto.common.Dao4ArtDexConstant;

import java.util.UUID;

/**
 * @description: TraceLogFeignInterceptor
 * @author: xiangbin
 * @create: 2023-05-25 17:40
 **/
public class TraceLogFeignInterceptor implements RequestInterceptor {

    @Override
    public void apply(RequestTemplate requestTemplate) {
        requestTemplate.header(Dao4ArtDexConstant.TRACE_ID, UUID.randomUUID().toString().replaceAll("-", ""));
    }

}
