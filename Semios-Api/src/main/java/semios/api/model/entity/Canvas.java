package semios.api.model.entity;

import java.io.Serializable;
import java.math.BigDecimal;

import org.apache.commons.lang3.StringUtils;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.fasterxml.jackson.annotation.JsonIgnore;

import lombok.Data;
import semios.api.model.dto.common.ProtoDaoConstant;

/**
 * <p>
 * canvas画布
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Data
public class Canvas implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * canvas名称
     */
    private String canvasName;

    /**
     * canvas描述信息
     */
    private String canvasDescription;

    /**
     * canvas的logo地址
     */
    private String canvasLogo;

    /**
     * cavans的编号
     */
    private Integer canvasNumber;

    /**
     * dao的id
     */
    private Integer daoId;

    /**
     * dao的projectid
     */
    private String projectId;

    /**
     * 所属dao的编号
     */
    private Integer daoNumber;

    /**
     * canvas创建者地址
     */
    private String ownerAddress;

    /**
     * canvas创建的交易hash
     */
    private String transactionHash;

    /**
     * opensea链接
     */
    private String openseaLink;

    /**
     * Twitter地址
     */
    private String twitterLink;

    /**
     * discord地址
     */
    private String discordLink;

    /**
     * Canvas创建的区块时间
     */
    private String blockTime;

    /**
     * 当前铸造价格
     */
    private BigDecimal currentPrice;

    /**
     * Canvas创建的区块号
     */
    private String blockNumber;

    /**
     * canvas创建时的canvasid
     */
    private String canvasId;

    /**
     * canvas创建时的drb号
     */
    private Integer drbNumber;

    /**
     * canvas创建剩余的drb总数
     */
    private Integer totalDrbNumber;

    /**
     * 所属dao的地板价
     */
    private BigDecimal daoFloorPrice;

    /**
     * canvas的uri地址
     */
    private String canvasUri;

    /**
     * 已领取token数量
     */
    private BigDecimal receivedToken;

    /**
     * 未领取token数量
     */
    private BigDecimal unclaimedToken;

    /**
     * transfer token
     */
    private BigDecimal transferToken;

    /**
     * 已兑换token数量
     */
    private BigDecimal swapToken;

    /**
     * 已兑换eht数量
     */
    private BigDecimal swapEth;

    /**
     * 当前计算的drb，每次drb变化了获取最新的铸造价格
     */
    private Integer restDrb;

    /**
     * 0-未创建1-已创建 2-已停机
     */
    private Integer canvasStatus;

    /**
     * 2-已开始3-已结束
     */
    private Integer daoStatus;

    /**
     * canvas收藏数
     */
    private Integer favoriteAmount = 0;

    /**
     * dao symbol
     */
    private String daoSymbol;
    /**
     * 社交链接 多个用逗号分隔
     */
    private String socialLinks;

    /**
     * income/wallet/dao接口用到，区分是canvas还是minter
     */
    @JsonIgnore
    @TableField(exist = false)
    private Boolean isMinter = false;

    /**
     * canvas设置的royalty token 比例 默认为空
     */
    private BigDecimal royaltyToken;

    /**
     * 已领取的eth数量
     */
    private BigDecimal receivedEth;

    /**
     * 未领取eth数量
     */
    private BigDecimal unclaimedEth;

    public String getCanvasDescription() {
        if (StringUtils.isBlank(canvasDescription)) {
            return "";
        }
        return canvasDescription;
    }

    public String getCanvasLogo() {
        if (StringUtils.isBlank(canvasLogo)) {
            return canvasLogo;
        }
        return canvasLogo.replace(ProtoDaoConstant.s3ImageUrl, ProtoDaoConstant.s3CdnImageUrl);
    }
}
