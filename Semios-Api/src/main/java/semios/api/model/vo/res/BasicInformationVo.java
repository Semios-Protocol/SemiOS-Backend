package semios.api.model.vo.res;

import lombok.Data;

/**
 * @description: Basic Information
 * @author: xiangbin
 * @create: 2023-12-27 14:15
 **/
@Data
public class BasicInformationVo {

    /**
     * 1.4 该DAO已经铸造的个数
     */
    private Integer mintCap = 0;

    /**
     * 1.4 DAO设置的总铸造上限
     */
    private Integer totalMintCap = 0;

    /**
     * 1.4 该DAO这个mint window已经铸造的数量
     */
    private Integer mintWindowCap = 0;

    /**
     * 1.4 该DAO设置的每个mint window铸造上限
     */
    private Integer totalMintWindowCap = 0;

    /**
     * 1.4 mintWindow Duration
     */
    private Integer mintWindowDuration = 0;

    /**
     * 1.4 剩余的mint Window
     */
    private Integer remainingMintWindow = 0;


    /**
     * 1.4 Asset Pool ETH
     */
    private String subDaoAssetPoolEth = "0";

    /**
     * 1.4 Asset Pool DAO Token
     */
    private String subDaoAssetPoolDaoToken = "0";
}
