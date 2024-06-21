package semios.api.model.vo.req;

import lombok.Data;

/**
 * @description: search参数
 * @author: xiangbin
 * @create: 2022-08-05 10:17
 **/
@Data
public class SearchReqVo {

    /**
     * 搜索参数
     */
    private String searchWord;

    /**
     * 返回条数
     */
    private Integer number;
}
