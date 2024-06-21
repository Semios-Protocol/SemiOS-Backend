package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 0-未创建1-已创建未开始2-已开始3-已结束 4-已停机  -1代表不展示
 *
 * @description: status
 * @author: xiangbin
 * @create: 2022-08-26 15:59
 **/
@NoArgsConstructor
@AllArgsConstructor
public enum DaoStatusEnum {
    NOT_CREATED(0, "未创建"),
    NOT_STARTED(1, "未开始"),
    STARTED(2, "已开始"),
    FINISHED(3, "已结束"),
    SHUT_DOWN(4, "已停机");

    @Getter
    private Integer status;

    @Getter
    private String desc;
}
