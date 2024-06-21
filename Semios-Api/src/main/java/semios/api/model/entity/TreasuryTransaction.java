package semios.api.model.entity;

import java.math.BigDecimal;

import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;
import lombok.Data;

import java.time.LocalDateTime;
import java.io.Serializable;

/**
 * <p>
 * 国库交易表
 * </p>
 *
 * @author zhyyao
 * @since 2024-02-22
 */
@TableName("treasury_transaction")
@Data
public class TreasuryTransaction implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * dao id
     */
    // private Integer daoId;

    /**
     * daoID
     */
    private String projectId;

    /**
     * 记录的交易hash
     */
    private String transactionHash;

    /**
     * from的地址
     */
    private String fromAddress;

    /**
     * to的地址
     */
    private String toAddress;

    /**
     * 上链时间区块高度
     */
    private String blockNumber;

    /**
     * 交易类型 0-打款 1-收款
     */
    private Integer transactionType;

    /**
     * 赠送的NFT的721地址
     */
    private String generateErc721Address;

    /**
     * 赠送的NFT的token id
     */
    private String generateTokenId;

    /**
     * 打款或收款的amount
     */
    private BigDecimal amount;

    /**
     * 是否通过国库打款
     */
    private Integer isUseTreasury;

    /**
     * sub dao project id
     */
    private String subDaoProjectId;


    /**
     * 创建时间
     */
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    @JsonSerialize(using = LocalDateTimeSerializer.class)
    private LocalDateTime createTime;
}
