package semios.dex.model.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;
import org.apache.commons.lang3.StringUtils;
import semios.dex.model.dto.common.DexConstant;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * @description: search
 * @author: xiangbin
 * @create: 2023-05-12 10:35
 **/
@Data
public class Erc20ResDto {

    /**
     * daoId
     *
     * @mock 122
     */
    private String daoId;

    /**
     * projectId
     *
     * @mock 0xca96a4e6645689fc92d2f7bc6250ec0682541a72c07e9b958574b07e5a4990f5
     */
    private String projectId;

    /**
     * erc20地址
     *
     * @mock 0x32c3972a564262ad13fbe4bf38ca585f5cead4cb
     */
    private String erc20Address;

    /**
     * erc721地址
     *
     * @mock 0x32c3972a564262ad13fbe4bf38ca585f5cead4cb
     */
    private String erc721Address;

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
     * eth地址
     *
     * @mock 0xb4fbf271143f4fbf7b91a5ded31805e42b2208d6
     */
    private String ethAddress;

    /**
     * 流动性资金池pair地址
     *
     * @mock 0x32c3972a564262ad13fbe4bf38ca585f5cead4cb
     */
    private String pairAddress;

    /**
     * pair token总余额
     *
     * @mock 123
     */
    @Deprecated
    @JsonIgnore
    private String pairBalance = "0";

    /**
     * erc20总余额
     *
     * @mock 123
     */
    private String erc20Balance = "0";

    /**
     * eth总余额
     *
     * @mock 456
     */
    private String ethBalance = "0";

    /**
     * erc20流动性资金池价格
     *
     * @mock 0.000123465
     */
    @SuppressWarnings("unused")
    private String price;

    /**
     * erc20流动性资金池是否开通 0-未开通 1-已开通未创建流通性 2-已开通已添加过流通性
     *
     * @mock 1
     */
    @SuppressWarnings("unused")
    private int liquidityStatus;

    /**
     * dao版本 1-1.8.5前的版本 2-1.8.5版本的 3-1.8.5之后的版本
     */
    private Integer daoVersion;

    public String getErc20Balance() {
        return new BigDecimal(erc20Balance).stripTrailingZeros().toPlainString();
    }

    public String getEthBalance() {
        return new BigDecimal(ethBalance).stripTrailingZeros().toPlainString();
    }

    public String getPrice() {
        if (!erc20Balance.equals("0") && !ethBalance.equals("0")) {
            return new BigDecimal(ethBalance)
                    .divide(new BigDecimal(erc20Balance), DexConstant.PRICE_DECIMAL, RoundingMode.HALF_UP)
                    .stripTrailingZeros().toPlainString();
        }
        return "0";
    }

    public int getLiquidityStatus() {
        return pairAddress == null ? 0
                : (StringUtils.isNotBlank(erc20Balance) && erc20Balance.compareTo("0") > 0) ? 2 : 1;
    }

}
