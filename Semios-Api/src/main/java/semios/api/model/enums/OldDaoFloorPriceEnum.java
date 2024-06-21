package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;


@NoArgsConstructor
@AllArgsConstructor
public enum OldDaoFloorPriceEnum {
    ZERO(0, 0.01f),
    ONE(1, 0.05f),
    TWO(2, 0.1f),
    THREE(3, 0.2f),
    FOUR(4, 0.5f),
    FIVE(5, 1.0f),
    SIX(6, 2.0f),
    SEVEN(7, 10.0f);

    @Getter
    private int index;
    @Getter
    private float value;

    public static float getValueByIndex(int index) {
        for (OldDaoFloorPriceEnum daoFloorPriceEnum : OldDaoFloorPriceEnum.values()) {
            if (daoFloorPriceEnum.getIndex() == index) {
                return daoFloorPriceEnum.getValue();
            }
        }
        return 0;
    }

}
