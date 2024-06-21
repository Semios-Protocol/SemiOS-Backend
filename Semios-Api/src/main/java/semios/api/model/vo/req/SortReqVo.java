package semios.api.model.vo.req;

import lombok.Data;

/**
 * @description: 排序查询条件
 * @author: xiangbin
 * @create: 2022-08-05 16:42
 **/
@Data
public class SortReqVo {

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
     */
    private String minPrice;
    /**
     * 最大价格
     */
    private String maxPrice;

    /**
     * 价格类型 0-canvas_price 1-fixed_price 2-all
     */
    private Integer fixedPrice;

}
