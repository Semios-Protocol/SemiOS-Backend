package semios.dex.model.dto.response;

import lombok.Data;
import lombok.EqualsAndHashCode;

import java.math.BigDecimal;

/**
 * @description: search
 * @author: xiangbin
 * @create: 2023-05-12 10:35
 **/
@Data
@EqualsAndHashCode(callSuper = true)
public class Erc20SwapResDto extends Erc20ResDto {

    /**
     * 交易总量
     *
     * @mock 123
     */
    private String totalVolume = "0";

    public String getTotalVolume() {
        return new BigDecimal(totalVolume).stripTrailingZeros().toPlainString();
    }

}
