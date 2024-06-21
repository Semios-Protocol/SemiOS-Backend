package semios.api.model.annotation;

import java.lang.annotation.*;

/**
 * @author xiangb
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Inherited
public @interface RepeatSubmit {
    String key() default "";
}
