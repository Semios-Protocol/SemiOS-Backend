package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * 1.3 subdao asset pool转账
 *
 * @description:
 * @author: xiangbin
 * @create: 2022-08-04 14:45
 **/
@Slf4j
@Data
public class DaoAssetAllocationVo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * SubDAO Asset Pool地址 （subDao assetPool）
     */
    private String subDaoAssetPool;

    /**
     * subDao erc20地址 （subDao erc20地址）
     */
    private String subDaoErc20;

    /**
     * Current DAO Token on SubDAO Asset Pool（subDao erc20余额）
     */
    private BigDecimal currentDaoToken = BigDecimal.ZERO;

    /**
     * Current DAO ETH on SubDAO Asset Pool（subDao eth余额）
     */
    private BigDecimal currentDaoEth = BigDecimal.ZERO;


    /**
     * 1.7 支付货币类型
     */
    private String payCurrencyType;

    /**
     * 1.7 input token的logo地址
     */
    private String inputTokenLogo;

    /**
     * 1.7 input token的address
     */
    private String inputTokenAddress;

    /**
     * 1.7 input token的decimals
     */
    private Integer inputTokenDecimals;

    /**
     * dao symbol
     */
    private String daoSymbol;
}
