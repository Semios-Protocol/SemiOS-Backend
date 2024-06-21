package semios.dex.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
public enum SubscribeStatusEnum {

    CLOSE(0, "关闭"),
    OPEN(1, "开启");

    @Getter
    private Integer type;

    @Getter
    private String desc;

}
