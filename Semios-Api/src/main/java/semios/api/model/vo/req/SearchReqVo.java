package semios.api.model.vo.req;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;

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


    /**
     * 每页显示多少条
     */
    private Long pageSize = 10L;

    /**
     * 当前页数
     */
    private Long pageNo = 1L;

    /**
     * 搜索参数
     *
     * @ignore
     */
    private List<String> searchWordList = new ArrayList<>();
}
