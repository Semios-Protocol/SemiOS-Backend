package semios.dex.model.dto.response;

import lombok.Data;

import java.math.BigDecimal;

/**
 * @author: fjtan
 * @create: 2023-05-19 22:35
 **/
@Data
public class AssetPoolPriceResDto {

    /**
     * erc20地址
     *
     * @mock 0x32c3972a564262ad13fbe4bf38ca585f5cead4cb
     */
    private String erc20Address;

    /**
     * asset pool price
     *
     * @mock 0.0000012
     */
    private BigDecimal price = BigDecimal.ZERO;

    /**
     * 交易量
     *
     * @mock 0.102
     */
    private BigDecimal tradingVolume;

    /**
     * 记录时间戳
     *
     * @mock 1684213200
     */
    private Long recordTime;

}
