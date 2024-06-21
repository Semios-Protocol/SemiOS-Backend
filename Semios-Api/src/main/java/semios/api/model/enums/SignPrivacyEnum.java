package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 0-未签署1-已签署
 */
@NoArgsConstructor
@AllArgsConstructor
public enum SignPrivacyEnum {
    WQS(0, "未签署"),
    YQS(1, "已签署");

    @Getter
    private Integer code;

    @Getter
    private String desc;
}
