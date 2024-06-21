package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 0-未创建1-已创建 2-已停机
 *
 * @description: status
 * @author: xiangbin
 * @create: 2022-08-26 15:59
 **/
@NoArgsConstructor
@AllArgsConstructor
public enum CanvasStatusEnum {
    NOT_CREATED(0, "未创建"),
    CREATED(1, "已创建"),
    SHUT_DOWN(2, "已停机");

    @Getter
    private Integer status;

    @Getter
    private String desc;
}
