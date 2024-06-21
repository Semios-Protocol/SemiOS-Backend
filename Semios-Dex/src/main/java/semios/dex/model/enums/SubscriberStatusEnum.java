package semios.dex.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
public enum SubscriberStatusEnum {
    SUCCESS("SUCCESS", "成功"),
    FAIL("FAIL", "失败");
    @Getter
    private String status;

    @Getter
    private String desc;
}
