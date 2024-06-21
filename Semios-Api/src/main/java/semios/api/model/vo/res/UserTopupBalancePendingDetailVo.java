package semios.api.model.vo.res;

import lombok.Data;
import net.minidev.json.annotate.JsonIgnore;

import java.math.BigDecimal;
import java.sql.Timestamp;


@Data
public class UserTopupBalancePendingDetailVo {
    /**
     * minted work id
     */
    private Integer mintedWorkId;

    /**
     * minted dao id
     */
    private Integer mintedDaoId;

    /**
     * minted work编号
     */
    private Integer mintedWorkNumber;

    /**
     * minted dao名称
     */
    private String mintedDaoName;


    /**
     * voucher work id
     */
    private Integer voucherWorkId;

    /**
     * minted dao id
     */
    private Integer voucherDaoId;

    /**
     * minted work编号
     */
    private Integer voucherWorkNumber;

    /**
     * minted dao名称
     */
    private String voucherDaoName;


    /*
     * Operation Time操作时间
     */
    private Long operationTime;

    /*
     * Mint Fee,也就是铸造的时候的minted work的price
     */
    private BigDecimal mintFee;

    /*
     * Mint Fee,也就是铸造的时候的minted work的price
     */
    private String mintedDaoPayCurrencyType;

    /*
     * mintedDaoInputTokenAddress minted dao的input token address
     */
    private String mintedDaoInputTokenAddress;

    /*
     * endBlockTime 也就是mintedDaoId还有多久周期结束
     */
    private Long endBlockTime;

    /*
     * 创建时间
     * @ignore
     */
    @JsonIgnore
    private Timestamp createTimestamp;
}
