package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 是否为topup模式铸造
 * 0-非topup
 * 1-topup下铸造topup的nft
 * 2-topup下铸造非topup的nft
 */
@NoArgsConstructor
@AllArgsConstructor
public enum WorkTopupModeEnum {
    MINT_NO_TOPUP(0, "非topup铸造"),
    MINT_TOPUP_YES(1, "topup下铸造topup的nft"),
    MINT_TOPUP_NO(2, "topup下铸造非topup的nft");


    @Getter
    private Integer status;

    @Getter
    private String name;
}
