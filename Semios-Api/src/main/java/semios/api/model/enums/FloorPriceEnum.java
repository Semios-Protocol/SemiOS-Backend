package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * protocol里的setting协议
 */
@NoArgsConstructor
@AllArgsConstructor
public enum FloorPriceEnum {
    ZERO(0, 0.01, "10000000000000000"),
    ONE(1, 0.05, "50000000000000000"),
    TWO(2, 0.1, "100000000000000000"),
    THREE(3, 0.2, "200000000000000000"),
    FOUR(4, 0.5, "500000000000000000"),
    FIVE(5, 1.0, "1000000000000000000"),
    SIX(6, 2.0, "2000000000000000000"),
    SEVEN(7, 10.0, "10000000000000000000");


    @Getter
    private int index;
    @Getter
    private Double value;
    @Getter
    private String originValue;


    public static Double getValueByIndex(int index) {
        for (FloorPriceEnum floorPriceEnum : FloorPriceEnum.values()) {
            if (floorPriceEnum.getIndex() == index) {
                return floorPriceEnum.getValue();
            }
        }
        return null;
    }

}
