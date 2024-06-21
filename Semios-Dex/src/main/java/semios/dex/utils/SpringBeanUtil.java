package semios.dex.utils;

import org.springframework.context.ConfigurableApplicationContext;

/**
 * @description: utils
 * @author: xiangbin
 * @create: 2022-04-14 13:41
 **/
public class SpringBeanUtil {

    //将管理上下文的applicationContext设置成静态变量，供全局调用
    public static ConfigurableApplicationContext applicationContext;

    //定义一个获取已经实例化bean的方法
    public static <T> T getBean(Class<T> c) {
        if (applicationContext == null) return null;
        return applicationContext.getBean(c);
    }

    public static <T> T getBeanByName(String beanName, Class<T> c) {
        if (applicationContext == null) return null;
        return applicationContext.getBean(beanName, c);
    }


}
