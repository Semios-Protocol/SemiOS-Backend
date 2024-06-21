package semios.api.model.vo.res;

import lombok.Data;

/**
 * @description: 搜索数量返回vo
 * @author: xiangbin
 * @create: 2022-08-05 10:20
 **/
@Data
public class SearchNumberResVo {


    /**
     * work数量
     */
    private Integer workAmount = 0;
    /**
     * canvas数量
     */
    private Integer canvasAmount = 0;
    /**
     * dao数量
     */
    private Integer daoAmount = 0;

    /**
     * seedNodes数量
     */
    private Integer seedNodesAmount = 0;

}
