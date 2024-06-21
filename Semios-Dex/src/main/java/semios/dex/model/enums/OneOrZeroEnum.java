package semios.dex.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;


/**
 * @author xiangbin
 */

@NoArgsConstructor
@AllArgsConstructor
public enum OneOrZeroEnum {

    /**
     * 0-否
     */
    ZERO(0, "否"),
    /**
     * 1-是
     */
    ONE(1, "是");

    @Getter
    private Integer status;

    @Getter
    private String desc;
}
