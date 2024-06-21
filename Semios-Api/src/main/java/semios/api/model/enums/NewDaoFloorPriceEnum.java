package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;


@NoArgsConstructor
@AllArgsConstructor
public enum NewDaoFloorPriceEnum {
    /**
     * [
     * { label: "0 ETH", value: 9999 },
     * { label: "0.01 ETH", value: 0 },
     * { label: "0.02 ETH", value: 1 },
     * { label: "0.03 ETH", value: 2 },
     * { label: "0.05 ETH", value: 3 },
     * { label: "0.1 ETH", value: 4 },
     * { label: "0.2 ETH", value: 5 },
     * { label: "0.3 ETH", value: 6 },
     * { label: "0.5 ETH", value: 7 },
     * { label: "1 ETH", value: 8 },
     * { label: "2 ETH", value: 9 },
     * { label: "3 ETH", value: 10 },
     * { label: "5 ETH", value: 11 },
     * { label: "10 ETH", value: 12 },
     * ]
     */
    ZERO_ZERO(0, 9999),
    ZERO(0.01, 0),
    ONE(0.02, 1),
    TWO(0.03, 2),
    THREE(0.05, 3),
    FOUR(0.1, 4),
    FIVE(0.2, 5),
    SIX(0.3, 6),
    SEVEN(0.5, 7),
    EIGHT(1, 8),
    NINE(2, 9),
    TEN(3, 10),
    ELEVEN(5, 11),
    TWELVE(10, 12);

    @Getter
    private double lable;
    @Getter
    private int value;


    public static int getValueByLable(double lable) {
        for (NewDaoFloorPriceEnum daoFloorPriceEnum : NewDaoFloorPriceEnum.values()) {
            if (daoFloorPriceEnum.getLable() == lable) {
                return daoFloorPriceEnum.getValue();
            }
        }
        return 0;
    }

}
