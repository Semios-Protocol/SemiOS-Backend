package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
public enum MintWindowEnum {
    ONE(0, 30),
    TWO(1, 60),
    THREE(2, 90),
    FOUR(3, 120),
    FIVE(4, 180),
    SIX(5, 270),
    SEVEN(6, 360);

    @Getter
    private int index;
    @Getter
    private Integer drb;


    public static Integer getValueByIndex(int index) {
        for (MintWindowEnum mintWindowEnum : MintWindowEnum.values()) {
            if (mintWindowEnum.getIndex() == index) {
                return mintWindowEnum.getDrb();
            }
        }
        return null;
    }

    public static int getIndexByValue(int drb) {
        for (MintWindowEnum mintWindowEnum : MintWindowEnum.values()) {
            if (mintWindowEnum.getDrb() == drb) {
                return mintWindowEnum.getIndex();
            }
        }
        return 0;
    }
}
