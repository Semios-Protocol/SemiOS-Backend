package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
public enum NftAmountEnum {
    ZERO(0, 1000),
    ONE(1, 5000),
    TWO(2, 10000),
    THREE(3, 50000),
    FOUR(4, 100000);

    @Getter
    private int index;
    @Getter
    private int value;


    public static Integer getValueByIndex(int index) {
        for (NftAmountEnum nftAmountEnum : NftAmountEnum.values()) {
            if (nftAmountEnum.getIndex() == index) {
                return nftAmountEnum.getValue();
            }
        }
        return null;
    }
}
