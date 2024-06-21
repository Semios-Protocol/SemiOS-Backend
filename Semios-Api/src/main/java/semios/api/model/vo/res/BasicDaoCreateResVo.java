package semios.api.model.vo.res;

import lombok.Data;

/**
 * 创建basic dao时返回的数据
 *
 * @description: dao res
 * @author: xiangbin
 * @create: 2022-08-11 10:16
 **/
@Data
public class BasicDaoCreateResVo {

    /**
     * dao开始的drb
     */
    private Integer startDrb;

    /**
     * dao开始的block
     */
    private String startBlock = "0";

    /**
     * 每个mint window的持续区块数
     */
    private String duration;

    /**
     * 铸造周期(DRB)数量，例如3个月为90个DRB
     */
    private Integer mintableRounds;

//    /**
//     * 用户选择第几个地板价，从0开始编号，例如选择第一个则该值为0
//     */
//    private Integer floorPriceRank;

    /**
     * 用户选择的最大铸造数量，从0开始编号，例如选择第一个则该值为0
     */
    private Integer maxNftRank;

    /**
     * 二次售卖印花税设置，直接传值,取值范围为:[500,1000] 加上250的税
     */
    private Integer royaltyFee;

    /**
     * Project的uri
     */
    private String projectUri;


    /**
     * 初始代币分配比例，为 5%
     */
    private Integer initTokenSupplyRatio = 500;

    /**
     * canvasId
     */
    private String canvasId;

    /**
     * canvasUri
     */
    private String canvasUri;

    /**
     * daoName
     */
    private String daoName;

    /**
     * createProjectFee
     */
    private Double createProjectFee;
}
