package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
public enum DaoCreateFeeEnum {
    ZERO(0, 5, 500),
    ONE(1, 6, 600),
    TWO(2, 7, 700),
    THREE(3, 8, 800),
    FOUR(4, 9, 900),
    Five(5, 10, 1000);

    @Getter
    private int index;
    @Getter
    private int showValue;
    @Getter
    private int actualValue;


    public static DaoCreateFeeEnum getDaoCreateFeeEnumByIndex(int index) {
        for (DaoCreateFeeEnum daoCreateFeeEnum : DaoCreateFeeEnum.values()) {
            if (daoCreateFeeEnum.getIndex() == index) {
                return daoCreateFeeEnum;
            }
        }
        return null;
    }

    public static DaoCreateFeeEnum getDaoCreateFeeEnumByActualValue(int actualValue) {
        for (DaoCreateFeeEnum daoCreateFeeEnum : DaoCreateFeeEnum.values()) {
            if (daoCreateFeeEnum.getActualValue() == actualValue) {
                return daoCreateFeeEnum;
            }
        }
        return null;
    }
}
