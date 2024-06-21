package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 价格类型 0-canvas_price 1-fixed_price
 */
@NoArgsConstructor
@AllArgsConstructor
public enum WorkPriceTypeEnum {
    CANVAS_PRICE(0, "canvas价格"), // 浮动价格
    FIXED_PRICE(1, "一口价"),   // work一口价

    DAO_GLOBAL_PRICE(2, "全局一口价");   // dao全局一口价,数据库不维护
    @Getter
    private Integer type;

    @Getter
    private String desc;
}
