package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
public enum TokenReceiveTypeEnum {
    DAO(1, "dao领取"), CANVAS(2, "canvas领取"), TRANSFER(3, "交易领取"), MINTER(4, "Minter领取"), UNLOCK(5, "解锁erc20领取");

    @Getter
    private Integer type;

    @Getter
    private String desc;
}
