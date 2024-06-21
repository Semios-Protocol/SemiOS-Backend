package semios.api.model.vo.res;

import lombok.Data;
import semios.api.model.dto.chain.DaoReserveRatio;
import semios.api.model.dto.chain.DaoRoyaltyToken;

import java.math.BigDecimal;

/**
 * dao的收益分配 + canvas 的Royalty Token
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-06-12 17:03
 **/
@Data
public class CanvasBenefitsDistributeResVo {

    /**
     * dao projectId
     */
    private String projectId;

    /**
     * canvasID
     */
    private String canvasId;

    /**
     * 代币分配策略
     */
    private DaoRoyaltyToken daoRoyaltyToken;

    /**
     * 一口价mint铸造收益分配策略
     */
    private DaoReserveRatio fixedReserveRatio;

    /**
     * 非一口价mint铸造收益分配策略
     */
    private DaoReserveRatio unFixedReserveRatio;

    /**
     * canvas Royalty Token
     */
    private BigDecimal extraRoyaltyToken;

    /**
     * dao版本 1-1.8.5前的版本 2-1.8.5版本的 3-1.8.5之后的版本
     */
    private Integer daoVersion;
}
