package semios.api.model.vo.req;

import lombok.Data;

@Data
public class WorkLockHours {
    /**
     * 用户输入锁定的小时数
     */
    private Integer hours;
}
