package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * work当前状态
 * 0-未铸造
 * 1-已铸造
 * 2-已过期
 */
@NoArgsConstructor
@AllArgsConstructor
public enum WorkLockStatusEnum {
    NOT_LOCK(0, "为锁定"),
    LOCK(1, "已锁定");


    @Getter
    private Integer status;

    @Getter
    private String name;
}
