package semios.api.model.dto.chain;

import lombok.Data;

import java.math.BigDecimal;

/**
 * @description: 代币分配策略
 * @author: xiangbin
 * @create: 2023-06-12 13:50
 **/
@Data
public class DaoRoyaltyToken {

    /**
     * dao分配比例  subDao Starter Reward
     */
    private BigDecimal daoReward = new BigDecimal("3");
    /**
     * d4a分配比例  PDAO Reward
     */
    private BigDecimal d4aReward = new BigDecimal("2");
    /**
     * canvas分配比例 6.19改为canvasReward+minterReward=100  builder Reward
     */
    private BigDecimal canvasReward = new BigDecimal("100");
    /**
     * minter分配比例  Minter Reward
     */
    private BigDecimal minterReward = BigDecimal.ZERO;
}
