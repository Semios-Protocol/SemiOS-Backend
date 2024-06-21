package semios.api.model.vo.res;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

/**
 * @description: 这一系列DAO的Treasury的信息
 * @author: zhyyao
 * @create: 2024-02-22 14:45
 **/
@Slf4j
@Data
public class TogetherDaoTreasuryTransactionVo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * from地址
     */
    private String fromAddress;

    /**
     * to地址
     */
    private String toAddress;

    /**
     * 交易hash
     */
    private String transactionHash;

    /**
     * 交易金额
     */
    private BigDecimal amount;

    /**
     * 创建时间(时间戳)
     */
    private Long createTime;

    /**
     * 创建时间(时间戳)
     */
    @JsonIgnore
    private Timestamp createTimeStamp;

    /**
     * 是否通过国库打款
     */
    private Integer isUseTreasury;

    /**
     * 1.7 所属的支付货币类型
     */
    private String payCurrencyType;


    /**
     * 1.7 input token的address
     */
    private String inputTokenAddress;

    /**
     * 1.7 work所属的input token的decimals
     */
    private Integer inputTokenDecimals;

    /**
     * dao symbol
     */
    private String daoSymbol;

    /**
     * project对应的erc20 token地址
     */
    private String erc20Token;

}
