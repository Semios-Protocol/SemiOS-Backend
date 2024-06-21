package semios.api.utils;

import net.sf.cglib.beans.BeanCopier;
import net.sf.cglib.beans.BeanMap;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.BeanWrapper;
import org.springframework.beans.BeanWrapperImpl;

import java.beans.PropertyDescriptor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiConsumer;
import java.util.function.BiPredicate;
import java.util.function.Consumer;

/**
 * @author fjtan
 */
public class BeanUtil {
    // 使用多线程安全的Map来缓存BeanCopier，由于读操作远大于写，所以性能影响可以忽略
    public static ConcurrentHashMap<String, BeanCopier> beanCopierMap = new ConcurrentHashMap<String, BeanCopier>();

    /**
     * 通过cglib BeanCopier形式,使用cglib拷贝，同属性同类型拷贝
     *
     * @param source 源转换的对象
     * @param target 目标转换的对象
     */
    public static void copyProperties(Object source, Object target) {
        String beanKey = generateKey(source.getClass(), target.getClass());
        BeanCopier copier = null;
        if (beanCopierMap.containsKey(beanKey)) {
            copier = beanCopierMap.get(beanKey);
        } else {
            copier = BeanCopier.create(source.getClass(), target.getClass(), false);
            beanCopierMap.putIfAbsent(beanKey, copier);// putIfAbsent已经实现原子操作了。
        }
        copier.copy(source, target, null);
    }

    /**
     * 通过cglib BeanCopier形式,使用cglib拷贝，同属性同类型拷贝
     *
     * @param source 源转换的对象
     * @param target 目标转换的对象
     * @param action 支持Lambda函数给拷贝完的对象一些自定义操作
     * @return
     */
    public static <O, T> T copyProperties(O source, T target, Consumer<T> action) {
        String beanKey = generateKey(source.getClass(), target.getClass());
        BeanCopier copier = null;
        if (beanCopierMap.containsKey(beanKey)) {
            copier = beanCopierMap.get(beanKey);
        } else {
            copier = BeanCopier.create(source.getClass(), target.getClass(), false);
            beanCopierMap.putIfAbsent(beanKey, copier);// putIfAbsent已经实现原子操作了。
        }
        copier.copy(source, target, null);
        action.accept(target);
        return target;
    }

    private static String generateKey(Class<?> class1, Class<?> class2) {
        return class1.toString() + class2.toString();
    }

    /**
     * transalte常规反射拷贝，同名同类型拷贝,推荐使用 transTo(O o, Class<T> clazz) ×通过常规反射形式 DTO对象转换为实体对象。如命名不规范或其他原因导致失败。
     *
     * @param t 源转换的对象
     * @param e 目标转换的对象
     * @throws InvocationTargetException
     * @throws IllegalArgumentException
     * @throws IllegalAccessException
     */
    public static <T, O> void transalte(T t, O e)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        Method[] tms = t.getClass().getDeclaredMethods();
        Method[] tes = e.getClass().getDeclaredMethods();
        for (Method m1 : tms) {
            if (m1.getName().startsWith("get")) {
                String mNameSubfix = m1.getName().substring(3);
                String forName = "set" + mNameSubfix;
                for (Method m2 : tes) {
                    if (m2.getName().equals(forName)) {
                        // 如果类型一致，或者m2的参数类型是m1的返回类型的父类或接口
                        boolean canContinue = m2.getParameterTypes()[0].isAssignableFrom(m1.getReturnType());
                        if (canContinue) {
                            m2.invoke(e, m1.invoke(t));
                            break;
                        }
                    }
                }
            }
        }

    }

    /**
     * 对象属性拷贝，忽略源转换对象的 null值
     *
     * @param t 源转换的对象
     * @param e 目标转换的对象
     */
    public static <T, O> void copyPropertiesIgnoreNull(T t, O e) {
        final BeanWrapper bw = new BeanWrapperImpl(t);
        PropertyDescriptor[] pds = bw.getPropertyDescriptors();

        Set<String> emptyNames = new HashSet<String>();
        for (PropertyDescriptor pd : pds) {
            Object srcValue = bw.getPropertyValue(pd.getName());
            if (srcValue != null)
                emptyNames.add(pd.getName());
        }
        partialCopy(t, e, emptyNames.toArray());
    }

    /**
     * 对象属性拷贝，同属性同类型拷贝，忽略源转换对象不符合自定义规则的属性
     *
     * @param t      源转换的对象
     * @param e      目标转换的对象
     * @param action lambda传入的是t的属性名和属性值，返回true和e对象有该属性则拷贝该值
     */
    public static <T, O> void copyPropertiesIgnoreCustom(T t, O e, BiPredicate<String, Object> action) {
        final BeanWrapper bw = new BeanWrapperImpl(t);
        PropertyDescriptor[] pds = bw.getPropertyDescriptors();

        Set<String> emptyNames = new HashSet<String>();
        for (PropertyDescriptor pd : pds) {
            Object srcValue = bw.getPropertyValue(pd.getName());
            // 自定义条件的成立与否，返回true则拷贝，反之不拷贝，满足同属性同类型。
            if (action.test(pd.getName(), srcValue))
                emptyNames.add(pd.getName());
        }
        partialCopy(t, e, emptyNames.toArray());
    }

    /**
     * 同类型字段部分拷贝
     *
     * @param t   源数据对象
     * @param e   接收对象
     * @param key 要拷贝的字段名数组
     */
    @SuppressWarnings("unchecked")
    public static <T> void partialCopy(T t, T e, Object... key) {
        BeanMap t1 = BeanMap.create(t);
        BeanMap e1 = BeanMap.create(e);
        int i = key.length;
        while (i-- > 0) {
            e1.replace(key[i], t1.get(key[i]));
        }
    }

    /**
     * 对象集合转换，两个对象的属性名字需要一样
     */
    public static <T, O> List<T> transTo(List<O> fromList, Class<T> clazz)
            throws IllegalAccessException, InstantiationException, InvocationTargetException {
        List<T> toList = new ArrayList<>();
        for (O e : fromList) {
            T entity = clazz.newInstance();
            BeanUtils.copyProperties(e, entity);
            toList.add(entity);
        }
        return toList;
    }

    /**
     * 使用BeanUtils，对象集合转换，两个对象的属性名字需要一样，并可自定义设置一些参数
     *
     * @param fromList 源数据List
     * @param clazz    需要转换成的clazz
     * @param action   支持lambda表达式自定义设置一些参数
     * @return
     */
    public static <T, O> List<T> transToCustom(List<O> fromList, Class<T> clazz, BiConsumer<O, T> action)
            throws IllegalAccessException, InstantiationException, InvocationTargetException {
        List<T> toList = new ArrayList<>();
        for (O e : fromList) {
            T entity = clazz.newInstance();
            BeanUtils.copyProperties(e, entity);
            action.accept(e, entity);
            toList.add(entity);
        }
        return toList;
    }

    /**
     * 使用BeanUtils，对象转换，E转为t对象
     */
    public static <T, O> T transTo(O e, Class<T> clazz)
            throws IllegalAccessException, InstantiationException, InvocationTargetException {
        T t = clazz.newInstance();
        BeanUtils.copyProperties(e, t);
        return t;
    }

    /**
     * 接口常量转为指定类型的List
     */
    @SuppressWarnings("unchecked")
    public static <T> List<T> interfaceTransToVal(Class<?> clazz, Class<T> toClazz) throws IllegalAccessException {
        List<T> list = new ArrayList<>();
        Field[] fields = clazz.getDeclaredFields();
        for (Field field : fields) {
            T t = (T) field.get(clazz);
            if (t.getClass() == toClazz) {
                list.add(t);
            }
        }
        return list;
    }
}
