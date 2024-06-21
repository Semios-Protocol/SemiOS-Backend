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
 * canvas在drb的统计信息
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Data
@TableName("canvas_drb_statistics")
public class CanvasDrbStatistics implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * canvas 表id
     */
    private Integer canvasId;

    /**
     * dao id
     */
    private Integer daoId;

    /**
     * dao的projectId
     */
    private String projectId;

    /**
     * drb号
     */
    private Integer drbNumber;

    /**
     * 当前铸造价格
     */
    private BigDecimal mintPrice;

    /**
     * Canvas在当前区块铸造费用总和
     */
    private BigDecimal drbVol;

    /**
     * Canvas在当前区块铸造费用总和 除税 Canvas在当前区块铸造费用总和*0.675
     */
    private BigDecimal drbVolExTax;

    /**
     * Dao当前区块铸造费用总和 除税 Dao的当前canvas在当前区块铸造费用总和*0.3
     */
    private BigDecimal daoDrbVolExTax;

    /**
     * 7天drbvol总和
     */
    private BigDecimal sevenDayDrbVol;

    /**
     * Canvas在本区块和之前所有区块铸造总费用
     */
    private BigDecimal totalVol;

    /**
     * Canvas在本区块铸造总费用占DAO在本区块铸造总费用的占比
     */
    private BigDecimal ntvr = BigDecimal.ZERO;

    /**
     * Canvas在该区块结束后累计收到的ERC20数量
     */
    private BigDecimal daoReward;

    /**
     * Mint Revenue+未来收益
     */
    private BigDecimal dre;

    /**
     * Canvas在该区块结束时所有NFT所在地址的私钥拥有者去重计数
     */
    private Integer owners;

    /**
     * Canvas在该区块结束时所有NFT的数量
     */
    private Integer nft;

    /**
     * Canvas在该区块结束时所有work的数量
     */
    private Integer workAmount;

    /**
     * Canvas本区块加上六个区块的铸造总费用占DAO在本区块加上六个区块铸造总费用的占比
     */
    private BigDecimal sevenDayNtrv;

    /**
     * Canvas的铸造总收入
     */
    private BigDecimal mintRevenue;

    /**
     * Canvas的铸造总收入*0.675
     * <p>
     * MintRevenue * 1-0.325 或者 1-0.375
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

    @TableField(exist = false)
    private String canvasName;
    @TableField(exist = false)
    private String canvasLogo;
    @TableField(exist = false)
    private String canvasDescription;
    @TableField(exist = false)
    private String canvasNumber;
    @TableField(exist = false)
    private String daoNumber;
    @TableField(exist = false)
    private Integer daoStatus;
    @TableField(exist = false)
    private BigDecimal currentPrice;
    @TableField(exist = false)
    private Integer canId;
    @TableField(exist = false)
    private String canvasIdStr;
    @TableField(exist = false)
    private Integer favoriteAmount;

    /**
     * 是否被当前用户收藏
     */
    @TableField(exist = false)
    private Boolean favorited = false;

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
