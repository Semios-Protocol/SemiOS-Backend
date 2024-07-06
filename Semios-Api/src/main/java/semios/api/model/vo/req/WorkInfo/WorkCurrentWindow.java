package semios.api.model.vo.req.WorkInfo;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Data
public class WorkCurrentWindow {
    /**
     * 1.4.3 铸造人数
     */
    private Integer minters = 0;

    /**
     * 1.4.3 铸造金额
     */
    private String mintFee = "0";

    /**
     * 1.4.3 铸造work的数量
     */
    private Integer mintedWorks = 0;

    /**
     * 1.4.3 该mint Window出块的资产 Token
     */
    private String blockRewardToken = "0";

    /**
     * 1.4.3 该mint Window出块的资产 eth
     */
    private String blockRewardEth = "0";

    /**
     * 1.4.3 这个DRB铸造会用于内部奖励的资产 Token
     */
    private String internalRewardToken = "0";

    /**
     * 1.4.3 这个DRB铸造会用于内部奖励的资产 eth
     */
    private String internalRewardEth = "0";

    //如果已经铸造了不展示
    /**
     * 1.4.3 该用户铸造这个作品会获得的奖励 Token -如果已经铸造成NFT则不需要展示
     */
    private String mintersMaxRewardToken = "0";

    /**
     * 1.4.3 该用户铸造这个作品会获得的奖励 eth-如果已经铸造成NFT则不需要展示
     */
    private String mintersMaxRewardEth = "0";

}
