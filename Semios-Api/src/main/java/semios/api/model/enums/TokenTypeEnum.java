package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 交易类型0-collect 1-swap 2-transfer
 */
@NoArgsConstructor
@AllArgsConstructor
public enum TokenTypeEnum {
    COLLECT(0, "Collect"),//领取代币
    SWAP(1, "Swap"),//兑换eth
    TRANSFER(2, "Transfer"),//交易代币
    UNLOCK(3, "Unlock");  // 未clime,直接解锁erc20
    @Getter
    private Integer type;

    @Getter
    private String desc;

    public static TokenTypeEnum getEnumById(Integer type) {
        for (TokenTypeEnum typeEnum : TokenTypeEnum.values()) {
            if (typeEnum.getType().equals(type)) {
                return typeEnum;
            }
        }
        return TokenTypeEnum.COLLECT;
    }
}
