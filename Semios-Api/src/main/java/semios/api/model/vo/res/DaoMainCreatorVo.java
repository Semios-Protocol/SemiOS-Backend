package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * 1.3
 *
 * @description: 判断当前用户是否为creator
 * @author: xiangbin
 * @create: 2022-08-04 14:45
 **/
@Slf4j
@Data
public class DaoMainCreatorVo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * existDaoId
     */
    private String existDaoId;

    /**
     * erc20Address
     */
    private String erc20Address;

    /**
     * 是否为mainDao creator
     */
    private Boolean isCreator;

    /**
     * 剩余的待分配的erc20数量
     */
    private BigDecimal remainingDaoToken = BigDecimal.ZERO;

    /**
     * 是否为外部ERC20 false-否 true-是
     */
    private Boolean isThirdpartyToken;

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
