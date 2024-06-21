package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 0-D4A 1-DAO 2-Canvas
 *
 * @description: status
 * @author: xiangbin
 * @create: 2022-08-26 15:59
 **/
@NoArgsConstructor
@AllArgsConstructor
public enum ShutdownTypeEnum {
    D4A(0, "D4A"),
    DAO(1, "DAO"),
    CANVAS(2, "CANVAS");

    @Getter
    private Integer type;

    @Getter
    private String desc;
}
