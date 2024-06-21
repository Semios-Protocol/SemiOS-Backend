package semios.dex.model.vo.res;

import lombok.Data;

/**
 * Assets Pool 展示信息
 *
 * @description: analytics
 * @author: xiangbin
 * @create: 2023-04-19 15:14
 **/
@Data
public class AssetPoolInfoResVo {

    /**
     * ETH In Pool (D4A.T123)
     *
     * @mock 1.002
     */
    private String ethInPool;

    /**
     * 当前ERC20 Burn了的数量
     *
     * @mock 0.002
     */
    private String burnVolume;

    /**
     * 资金池地址
     *
     * @mock 0xadec1c00e249de6ff9778e05a39e4ebdc9d65618
     */
    private String daoAssetPool;

}
