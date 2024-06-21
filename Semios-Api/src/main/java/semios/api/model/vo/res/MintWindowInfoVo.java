package semios.api.model.vo.res;

import lombok.Data;

/**
 * @description: current mint window information
 * @author: xiangbin
 * @create: 2023-12-27 14:15
 **/
@Data
public class MintWindowInfoVo {

    /**
     * 1.4 铸造人数
     */
    private Integer minters = 0;

    /**
     * 1.4 铸造金额
     */
    private String mintFee = "0";

    /**
     * 1.4 铸造work的数量
     */
    private Integer mintedWorks = 0;

    /**
     * 1.4 该mint Window出块的资产 Token
     */
    private String blockRewardToken = "0";

    /**
     * 1.4 该mint Window出块的资产 eth
     */
    private String blockRewardEth = "0";

    /**
     * 1.4 这个DRB铸造会用于内部奖励的资产 Token
     */
    private String internalRewardToken = "0";

    /**
     * 1.4 这个DRB铸造会用于内部奖励的资产 eth
     */
    private String internalRewardEth = "0";
}
