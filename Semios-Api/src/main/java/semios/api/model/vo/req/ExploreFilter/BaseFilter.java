package semios.api.model.vo.req.ExploreFilter;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class BaseFilter {

    /**
     * 排序条件
     * 0 recently listed
     * 1 most favorited
     * 2 price high to low
     * 3 price low to high
     */
    private String sortCondition = "0";

    /**
     * 最小价格
     * @ignore 忽略
     */
    private String minPrice;
    /**
     * 最大价格
     * @ignore 忽略
     */
    private String maxPrice;

    /**
     * 价格类型 0-canvas_price 1-fixed_price 2-all
     * @ignore 忽略
     */
    private Integer fixedPrice;

    /**
     * 1.12 筛选是否包含topup模式
     * @mock false
     */
    private Boolean topUpMode;

    /**
     * 1.12 筛选包含的inputType token 类型
     * @mock ["ETH","USDC"]
     */
    private List<String> inputTokenTypes = new ArrayList<>();

    /**
     * 价格类型 0-Folating Price 1-Fixed Price 2-Unified Price
     * @mock [0,1]
     */
    private List<Integer> priceType = new ArrayList<>();


    /**
     * 1.12 explore nft 筛选是否展示绑定过Permissions NFT的数据
     * @mock false
     */
    private Boolean isPermissionNft;


    /**
     * 1.12 explore nft 筛选是否展示被锁定的NFT的数据
     * @mock false
     */
    private Boolean lockedNft;


    /**
     * 1.12 筛选sub Node包含Mode Status
     */
    private NodeModes nodeModeStatus = new NodeModes();


    /**
     * 1.12 筛选是否展示那些Seed Nodes下有Incentive Plan的
     * @mock true
     */
    private Boolean withIncentivePlan;
}
