package semios.dex.model.dto.response;

import lombok.Data;

import java.math.BigDecimal;

/**
 * @author: fjtan
 * @create: 2023-05-17 14:35
 **/
@Data
public class Erc20TradeResDto {

    /**
     * erc20地址
     *
     * @mock 0x32c3972a564262ad13fbe4bf38ca585f5cead4cb
     */
    private String erc20Address;

    /**
     * 交易总量
     *
     * @mock 123
     */
    private BigDecimal tradingVolume = BigDecimal.ZERO;

}
