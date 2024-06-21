package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
public enum TotalNftMintCapEnum {
    /**
     * [
     * { label: "1k", value: 0 },
     * { label: "5k", value: 1 },
     * { label: "10k", value: 2 },
     * { label: "50k", value: 3 },
     * { label: "100k", value: 4 },
     * ],
     */
    ZERO(1000, 0),
    ONE(5000, 1),
    TWO(10000, 2),
    THREE(50000, 3),
    FOUR(100000, 4);

    @Getter
    private long lable;
    @Getter
    private int value;

    public static int getValueByLable(long lable) {
        for (TotalNftMintCapEnum totalNftMintCapEnum : TotalNftMintCapEnum.values()) {
            if (totalNftMintCapEnum.getLable() == lable) {
                return totalNftMintCapEnum.getValue();
            }
        }
        return 0;
    }
}
