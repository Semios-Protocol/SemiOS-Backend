package semios.api.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author fjtan
 */
public class LogUtil {
    /**
     * 获得Logger
     *
     * @param clazz 日志发出的类
     * @return Logger
     */
    public static Logger getLogger(Class<?> clazz) {
        return LoggerFactory.getLogger(clazz);
    }

    /**
     * 获得Logger
     *
     * @param name 自定义的日志发出者名称
     * @return Logger
     */
    public static Logger getLogger(String name) {
        return LoggerFactory.getLogger(name);
    }


    /**
     * @return 获得日志发出者
     */
    private static Logger innerGet() {
        StackTraceElement[] stackTrace = Thread.currentThread().getStackTrace();
        return LoggerFactory.getLogger(stackTrace[3].getClassName());
    }
}
