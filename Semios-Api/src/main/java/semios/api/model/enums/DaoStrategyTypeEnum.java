package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 0-create_canvas 1-mint_work
 */
@NoArgsConstructor
@AllArgsConstructor
public enum DaoStrategyTypeEnum {
    CREATE_CANVAS(0, "create_canvas"),
    MINT_WORK(1, "mint_work");

    @Getter
    private Integer type;

    @Getter
    private String desc;
}
