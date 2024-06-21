package semios.dex.model.dto.response;

import lombok.Data;

import java.math.BigDecimal;

/**
 * @description: search
 * @author: xiangbin
 * @create: 2023-05-12 10:35
 **/
@Data
public class Erc20BalanceResDto {

    /**
     * erc20地址
     *
     * @mock 0x32c3972a564262ad13fbe4bf38ca585f5cead4cb
     */
    private String erc20Address;

    /**
     * erc20名称
     *
     * @mock D4A NFT for No.121
     */
    private String erc20Name;

    /**
     * erc20 Symbol
     *
     * @mock D4A.T124
     */
    private String erc20Symbol;

    /**
     * 用户erc20余额
     *
     * @mock 456
     */
    private String erc20Balance = "0";

    public String getErc20Balance() {
        return new BigDecimal(erc20Balance).stripTrailingZeros().toPlainString();
    }

}
