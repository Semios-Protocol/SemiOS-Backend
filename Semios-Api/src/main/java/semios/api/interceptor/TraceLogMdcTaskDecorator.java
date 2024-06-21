package semios.api.interceptor;

import org.slf4j.MDC;
import org.springframework.core.task.TaskDecorator;

import java.util.Map;

/**
 * @description: 异步线程获取主线程traceId
 * @author: xiangbin
 * @create: 2023-07-14 15:07
 **/
public class TraceLogMdcTaskDecorator implements TaskDecorator {

    @Override
    public Runnable decorate(Runnable runnable) {
        Map<String, String> map = MDC.getCopyOfContextMap();
        return () -> {
            try {
                if (map != null) {
                    MDC.setContextMap(map);
                }
                runnable.run();
            } finally {
                MDC.clear();
            }
        };
    }

}
