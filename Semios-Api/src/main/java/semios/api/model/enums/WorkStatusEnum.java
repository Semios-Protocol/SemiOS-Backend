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
public enum WorkStatusEnum {
    NOT_CAST(0, "未铸造"),
    CASTED(1, "已铸造"),
    EXPIRED(2, "已过期");


    @Getter
    private Integer status;

    @Getter
    private String name;
}
