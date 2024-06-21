package semios.api.service.aspect;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.stereotype.Component;
import semios.api.model.annotation.RepeatSubmit;
import semios.api.model.exceptions.PausedException;
import semios.api.model.vo.RepeatVo;
import semios.api.utils.JacksonUtil;

import java.lang.reflect.Method;
import java.util.concurrent.TimeUnit;

/**
 * @description: RepeatSubmitAspect
 * @author: xiangbin
 * @create: 2023-10-27 11:20
 **/
@Slf4j
@Aspect
@Component
public class RepeatSubmitAspect {

    private static final Cache<String, Object> CACHES = CacheBuilder.newBuilder()
            // 最大缓存 设置为1000个
            .maximumSize(1000)
            // 设置写缓存后5s过期
            .expireAfterWrite(5, TimeUnit.SECONDS)
            .build();

    /**
     * key 的生成策略
     *
     * @param keyExpress 表达式
     * @param args       参数
     * @return 生成的key
     */
    public static String getKey(String keyExpress, Object[] args) {
        StringBuffer stringBuffer = new StringBuffer(keyExpress);

        for (int i = 0; i < args.length; i++) {
            if (args[i] instanceof RepeatVo) {
                stringBuffer.append("_");
                stringBuffer.append(JacksonUtil.obj2json(args[i]));
            }
        }

        log.info("[GetKey] keyExpress:{} stringBuffer:{}", keyExpress, stringBuffer);
        return stringBuffer.toString();
    }

    public static void main(String[] args) {
        System.out.println(RepeatSubmitAspect.getKey("hello", new Object[]{"a", "b", "c"}));
    }

    @Around("execution(public * *(..)) && @annotation(semios.api.model.annotation.RepeatSubmit))")
    public Object interceptor(ProceedingJoinPoint pjp) throws Throwable {
        MethodSignature signature = (MethodSignature) pjp.getSignature();
        Method method = signature.getMethod();
        RepeatSubmit repeatSubmit = method.getAnnotation(RepeatSubmit.class);
        String key = getKey(repeatSubmit.key(), pjp.getArgs());
        if (StringUtils.isNotBlank(key)) {
            if (CACHES.getIfPresent(key) != null) {
                throw new PausedException("Please do not repeat the request！");
            }
            // 如果是第一次请求，就将key 当前对象压入缓存中
            CACHES.put(key, key);
        }

        return pjp.proceed();

    }
}
