package semios.dex.model.vo;

import lombok.Data;

/**
 * @description: page分页参数
 * @author: xiangbin
 * @create: 2022-08-04 16:16
 **/
@Data
public class PageVo {

    /**
     * 每页显示多少条
     */
    private Long pageSize = 10L;

    /**
     * 当前页数
     */
    private Long pageNo = 1L;


}
