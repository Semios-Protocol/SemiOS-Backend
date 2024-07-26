package semios.api.model.entity;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;

import org.apache.commons.lang3.StringUtils;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;

import lombok.Data;
import semios.api.model.dto.common.ProtoDaoConstant;

/**
 * <p>
 * work作品
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Data
public class Work implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * work编号
     */
    private Integer workNumber;

    /**
     * 图片地址
     */
    private String imageUrl;

    /**
     * work描述
     */
    private String workDescription;

    /**
     * canvasid
     */
    private String canvasId;

    /**
     * 所属canvas表的id
     */
    private Integer canId;

    /**
     * canvas编号
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
     * dao的编号
     */
    private Integer daoNumber;

    /**
     * Work铸造时的交易hash，默认为空
     */
    private String transactionHash;

    /**
     * 创建人地址-canvas得拥有者地址
     */
    private String creatorAddress;

    /**
     * 铸造者地址
     */
    private String mintedAddress;

    /**
     * 铸造价格
     */
    private BigDecimal mintedPrice;

    /**
     * 铸造的区块时间
     */
    private String blockTime;

    /**
     * 铸造的区块号
     */
    private String blockNumber;

    /**
     * 铸造时所属drb区块
     */
    private Integer drbNumber;

    /**
     * 当前owner地址
     */
    private String ownerAddress;

    /**
     * work的uri
     */
    private String workUri;

    /**
     * Work状态0-已创建1-已铸造2-已失效
     */
    private Integer workStatus;

    /**
     * work的hash值
     */
    private String workHash;

    /**
     * work创建时间
     */
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    @JsonSerialize(using = LocalDateTimeSerializer.class)
    private LocalDateTime createTime;

    /**
     * 收藏数
     */
    private Integer favoriteAmount;

    /**
     * 0-未删除 1-已删除
     */
    private Integer isDel;

    /**
     * 图片背景色
     */
    private String bgColor;

    /**
     * 计算图片在260宽度时的高度
     */
    private Double height;

    /**
     * 创建时的签名hash
     */
    private String createSignHash;

    /**
     * 价格类型 0-canvas_price 1-fixed_price
     */
    private Integer priceType;

    /**
     * 一口价
     */
    private BigDecimal fixedPrice;

    /**
     * 是否自动生成 1-自动生成 2-上传的 3-合约生成的NFT
     */
    private Integer generate;

//    /**
//     * 社交链接 多个用逗号分隔
//     */
//    private String socialLinks;

    /**
     * 社交链接
     * <p>
     * 社交链接 List<DaoSocialLinks> 的json对象
     */
    private String socialLinks;

    /**
     * opensea链接地址
     */
    private String openseaLink;

    /**
     * Twitter链接地址
     */
    private String twitterLink;

    /**
     * Discord链接地址
     */
    private String discordLink;

    /**
     * 1.3 是否为topup模式铸造 0-非topup 1-topup下铸造topup的nft 2-topup下铸造非topup的nft
     */
    private Integer topupMode;

    /**
     * 1.5 work锁定的状态 锁定状态 0-未锁定 1-已锁定
     */
    private Integer lockStatus;

    /**
     * 1.5 work锁定的区块高度
     */
    private String lockStartBlock;

    /**
     * 1.5 work锁定的区块数量
     */
    private String lockDurationBlock;

    /**
     * 1.5 铸造 绑定的workID
     */
    private Integer mountWorkId;


//    /**
//     * canvas Royalty Token设置后的价格 不设置默认为空
//     */
//    private BigDecimal royaltyTokenPrice;

    @TableField(exist = false)
    private String daoLogoUrl;
    @TableField(exist = false)
    private String canvasLogo;

    @TableField(exist = false)
    private BigDecimal daoFloorPrice;

    @TableField(exist = false)
    private Integer daoStatus;

    /**
     * work所属的dao名称
     */
    @TableField(exist = false)
    private String daoName;

    @TableField(exist = false)
    private Integer workAmount;

    /**
     * 是否被当前用户收藏
     */
    @TableField(exist = false)
    private Boolean favorited = false;

    public String getWorkDescription() {
        if (StringUtils.isBlank(workDescription)) {
            return "";
        }
        return workDescription;
    }

    public String getImageUrl() {
        if (StringUtils.isBlank(imageUrl)) {
            return imageUrl;
        }
        return imageUrl.replace(ProtoDaoConstant.s3CdnImageUrl, ProtoDaoConstant.s3ImageUrl);
//        return imageUrl.replace(ProtoDaoConstant.s3ImageUrl, ProtoDaoConstant.s3CdnImageUrl);
    }
}
