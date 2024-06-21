package semios.gateway.model.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;

import java.io.Serializable;

/**
 * <p>
 *
 * </p>
 *
 * @author xiangbin
 * @since
 */
@TableName("hello_world")
public class HelloWorld implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 这里一定要有type = IdType.AUTO 使用数据库自定义的自增
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    private String name;

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return "HelloWorld{" +
                "id=" + id +
                ", name=" + name +
                "}";
    }
}
