package semios.api.model.vo.res;

import lombok.Data;

import java.math.BigDecimal;

/**
 * Proportion 占比
 *
 * @description: analytics
 * @author: xiangbin
 * @create: 2023-04-19 15:14
 **/
@Data
public class DaoProportionResVo {

    /**
     * canvas id
     *
     * @mock 123
     */
    private Integer canvasId;
    /**
     * canvas 名称
     *
     * @mock Canvas name
     */
    private String canvasName;

    /**
     * canvas 跳转地址
     *
     * @mock http://128.0.0.1/a.jpg
     */
    private String canvasUrl;

    /**
     * nft 数量
     *
     * @mock 5
     */
    private Integer nftAmount;

    /**
     * 占比
     *
     * @mock 0.25
     */
    private BigDecimal ratio;

    /**
     * 饼图展示颜色
     */
    private String color;

}
