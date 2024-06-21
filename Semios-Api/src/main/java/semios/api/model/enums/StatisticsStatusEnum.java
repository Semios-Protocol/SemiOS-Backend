package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 0-未计算
 * 1-计算中
 * 2-计算完成
 */
@NoArgsConstructor
@AllArgsConstructor
public enum StatisticsStatusEnum {
    WJS(0, "未计算"),
    JSZ(1, "计算中"),
    JSWC(2, "计算完成");

    @Getter
    private Integer status;

    @Getter
    private String desc;
}
