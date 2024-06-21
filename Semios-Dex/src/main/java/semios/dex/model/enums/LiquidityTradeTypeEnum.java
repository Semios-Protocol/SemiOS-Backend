package semios.dex.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 1-swapErc20 2-swapEth 3-add 4-remove 5-burn
 *
 * @author: fjtan
 * @create: 2023-05-15 15:59
 **/
@NoArgsConstructor
@AllArgsConstructor
public enum LiquidityTradeTypeEnum {
    SWAPERC20(1, "Swaps"),
    SWAPETH(2, "Swaps"),
    ADD(3, "Adds"),
    REMOVE(4, "Removes"),
    BURN(5, "Burn");

    @Getter
    private Integer status;

    @Getter
    private String desc;

    /**
     * 根据status查找
     *
     * @param status
     * @return desc
     */
    public static String findEnumByCode(Integer status) {
        for (LiquidityTradeTypeEnum statusEnum : LiquidityTradeTypeEnum.values()) {
            if (statusEnum.getStatus() == status) {
                return statusEnum.getDesc();
            }
        }
        throw new IllegalArgumentException("status is invalid");
    }

}
