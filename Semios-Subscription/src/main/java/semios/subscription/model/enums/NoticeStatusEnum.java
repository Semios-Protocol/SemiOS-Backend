package semios.subscription.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 通知状态 0-未成功 1-已成功
 */
@NoArgsConstructor
@AllArgsConstructor
public enum NoticeStatusEnum {
    FAIL(0, "未成功"),
    SUCCESS(1, "已成功");

    @Getter
    private Integer status;
    @Getter
    private String desc;

}
