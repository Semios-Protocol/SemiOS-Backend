package semios.dex.model.dto.response;

import lombok.Data;
import semios.dex.model.dto.common.DexConstant;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * @author: fjtan
 * @create: 2023-05-17 10:35
 **/
@Data
public class Erc20PriceResDto {

    /**
     * erc20地址
     *
     * @mock 0x32c3972a564262ad13fbe4bf38ca585f5cead4cb
     */
    private String erc20Address;

    /**
     * erc20数量
     *
     * @mock 123
     */
    private BigDecimal erc20Amount = BigDecimal.ZERO;

    /**
     * eth数量
     *
     * @mock 456
     */
    private BigDecimal ethAmount = BigDecimal.ZERO;

    /**
     * 交易量
     *
     * @mock 0.102
     */
    private BigDecimal tradingVolume = BigDecimal.ZERO;

    /**
     * 记录时间戳
     *
     * @mock 1684213200
     */
    private Long recordTime;

    public BigDecimal getPrice() {
        if (erc20Amount.compareTo(BigDecimal.ZERO) > 0 && ethAmount.compareTo(BigDecimal.ZERO) > 0) {
            return ethAmount.divide(erc20Amount, DexConstant.PRICE_DECIMAL, RoundingMode.HALF_UP);
        }
        return BigDecimal.ZERO;
    }

}
