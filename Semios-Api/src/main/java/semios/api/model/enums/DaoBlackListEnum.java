package semios.api.model.enums;


import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 黑名单 0-未开通 1-开通
 */
@NoArgsConstructor
@AllArgsConstructor
public enum DaoBlackListEnum {

    CLOSE(0, "未开通"),
    OPEN(1, "开通");

    @Getter
    private Integer status;

    @Getter
    private String desc;
}
