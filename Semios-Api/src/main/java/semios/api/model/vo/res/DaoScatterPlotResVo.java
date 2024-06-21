package semios.api.model.vo.res;

import lombok.Data;

import java.util.List;

/**
 * mint 散点图
 *
 * @description: 散点图
 * @author: xiangbin
 * @create: 2023-04-19 15:14
 **/
@Data
public class DaoScatterPlotResVo {

    /**
     * 交易时间
     */
    private List<String> xtime;
    // /**
    // * 交易金额
    // */
    // private List<BigDecimal> value;
    /**
     * 交易金额
     */
    private List<String> price;

    /**
     * NFT编号
     */
    private List<String> name;

    /**
     * NFT图片
     */
    private List<String> image;

}
