package semios.api.model.entity;

import java.io.Serializable;
import java.math.BigDecimal;

import org.apache.commons.lang3.StringUtils;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;

import lombok.Data;
import semios.api.model.dto.common.ProtoDaoConstant;

/**
 * <p>
 * Dao在drb的统计信息
 * </p>
 *
 * @author xiangbin
 */
@Data
@TableName("dao_drb_statistics")
public class DaoDrbStatistics implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * dao id
     */
    private Integer daoId;

    /**
     * Drb区块号
     */
    private Integer drbNumber;

    /**
     * DAO在该区块结束时所有canvas的最低价
     */
    private BigDecimal floorPrice;

    /**
     * DAO在该区块和之前的六个区块（包括轮空的区块）铸造费用总和
     */
    private BigDecimal sevenDayDrbVol;

    /**
     * DAO在该区块结束时资金池里的总金额
     */
    private BigDecimal daoAssetPool;

    /**
     * DAO在该区块结束时所发放的所有ERC20数量
     */
    private BigDecimal daoReward;

    /**
     * DAO在该区块结束时canvas的数量
     */
    private Integer canvas;

    /**
     * DAO在该区块结束时所有NFT所在地址的私钥拥有者去重计数
     */
    private String owners;

    /**
     * DAO在该区块结束时所有NFT的数量
     */
    private String nft;

    /**
     * DAO在该区块结束时所有work的数量
     */
    private Integer works;

    /**
     * Mint Revenue+未来收益
     */
    private String dre;

    /**
     * 当前drb的铸造费用
     */
    private BigDecimal drbVol;

    /**
     * Dao在当前区块铸造费用总和 除税 Dao在当前区块铸造费用总和*0.3
     */
    private BigDecimal drbVolExTax;

    /**
     * 当前dao铸造总金额
     */
    private BigDecimal mintRevenue;

    /**
     * 当前dao铸造总金额 mintRevenue*30%或者35% Dao的铸造总收入*0.3
     */
    private BigDecimal mintRevenueExTax;

    /**
     * 0-未计算 1-计算中 2-计算完成
     */
    private Integer status;
    /**
     * 计算失败次数
     */
    private Integer times;

    /**
     * 归属时间 用于DaoDrbStatistics统计时用 如果统计时未完成的则更改时间到下一个自然日
     */
    private Long recordTime;

    /**
     * DAO在该区块结束时总出块token减去不出块部分
     */
    private BigDecimal assetPoolTokenCost;

    /**
     * DAO在该区块结束时总出块eth减去不出块部分
     */
    private BigDecimal assetPoolEthCost;

    /**
     * 当前drb的铸造nft的贡献度
     */
    private BigDecimal contribution;

    /**
     * 当前drb铸造出块的ERC20总量
     */
    private BigDecimal erc20Amount;

    /**
     * 当前drb铸造出块的ETH总量
     */
    private BigDecimal ethAmount;

    @TableField(exist = false)
    private String daoName;
    @TableField(exist = false)
    private String daoLogourl;
    @TableField(exist = false)
    private String daoDescription;
    @TableField(exist = false)
    private String daoNumber;
    @TableField(exist = false)
    private Integer daoStatus;
    @TableField(exist = false)
    private BigDecimal daoFloorPrice;
    @TableField(exist = false)
    private Integer favoriteAmount;

    @TableField(exist = false)
    private BigDecimal canvasFloorPrice;

    @TableField(exist = false)
    private Integer daoItemId;

    @TableField(exist = false)
    private String feePool;

    /**
     * 是否被当前用户收藏
     */
    @TableField(exist = false)
    private Boolean favorited = false;

    /**
     * 是否设置了黑白名单
     */
    @TableField(exist = false)
    private Boolean whiteList = false;

    /**
     * 是否开启了TopUp模式 false-否 true-是
     */
    @TableField(exist = false)
    private Integer topupMode;

    /**
     * 1.4 是否开启Erc20支付模式 false-否 true-是
     */
    @TableField(exist = false)
    private Boolean erc20PaymentMode = false;

    @TableField(exist = false)
    private Integer erc20Pay;

    public String getDaoDescription() {
        if (StringUtils.isBlank(daoDescription)) {
            return "";
        }
        return daoDescription;
    }

    public String getDaoLogourl() {
        if (StringUtils.isBlank(daoLogourl)) {
            return daoLogourl;
        }
        return daoLogourl.replace(ProtoDaoConstant.s3ImageUrl, ProtoDaoConstant.s3CdnImageUrl);
    }
}
