package semios.api.model.vo.req;

import lombok.Data;

/**
 * @description: analytics
 * @author: xiangbin
 * @create: 2023-04-19 15:14
 **/
@Data
public class DaoPriceRangeReqVo {

    /**
     * dao id
     */
    private Integer daoId;

    /**
     * 是否包含一口价 0-不包含 1-包含
     *
     * @mock 1
     */
    private Integer fixedPrice;
}
