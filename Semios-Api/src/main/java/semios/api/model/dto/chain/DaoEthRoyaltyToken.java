package semios.api.model.dto.chain;

import lombok.Data;

import java.math.BigDecimal;

/**
 * 1.3
 *
 * @description: eth分配策略
 * @author: xiangbin
 * @create: 2023-11-13 13:50
 **/
@Data
public class DaoEthRoyaltyToken {

    /**
     * dao分配比例
     */
    private BigDecimal daoCreatorETHReward;
    /**
     * d4a分配比例
     */
    private BigDecimal d4aReward;
    /**
     * canvas分配比例
     */
    private BigDecimal canvasCreatorETHReward;
    /**
     * minter分配比例
     */
    private BigDecimal minterETHReward;
}
