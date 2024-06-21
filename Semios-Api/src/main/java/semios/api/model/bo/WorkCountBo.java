package semios.api.model.bo;

import lombok.Data;

/**
 * @description: WorkCountDo
 * @author: xiangbin
 * @create: 2023-12-18 15:21
 **/
@Data
public class WorkCountBo {

    /**
     * 1.4 铸造人数
     */
    private Integer minters;

    /**
     * 1.4 铸造金额
     */
    private String mintFee;

    /**
     * 1.4 铸造work的数量
     */
    private Integer mintedWorks;

}
