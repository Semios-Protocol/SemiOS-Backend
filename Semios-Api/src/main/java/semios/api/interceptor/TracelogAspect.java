package semios.api.interceptor;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.slf4j.MDC;
import org.springframework.stereotype.Component;
import semios.api.model.dto.common.ProtoDaoConstant;

import java.util.UUID;

/**
 * @description: TracelogAspect
 * @author: xiangbin
 * @create: 2023-05-25 18:00
 **/
@Aspect
@Component
public class TracelogAspect {
    // https://blog.csdn.net/weixin_59383491/article/details/128673614

    @Pointcut("@annotation(org.springframework.scheduling.annotation.Scheduled) || @annotation(javax.annotation.PostConstruct)")
    public void tracelogPointCut() {
    }

    @Around("tracelogPointCut()")
    public Object around(ProceedingJoinPoint point) throws Throwable {
        MDC.put(ProtoDaoConstant.TRACE_ID, UUID.randomUUID().toString().replaceAll("-", ""));
        return point.proceed();
    }

}
