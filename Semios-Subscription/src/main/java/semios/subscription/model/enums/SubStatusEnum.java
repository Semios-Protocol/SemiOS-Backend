package semios.subscription.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
public enum SubStatusEnum {
    OPEN(1, "开启"),
    CLOSE(0, "关闭");

    @Getter
    private Integer status;
    @Getter
    private String desc;
}
