package semios.api.model.vo.res;

import lombok.Data;

/**
 * @description: drb
 * @author: xiangbin
 * @create: 2023-07-17 16:24
 **/
@Data
public class DrbInfoResVo {

    /**
     * 当前drb剩余区块百分比
     */
    private Double proportion;

    /**
     * 下一个区块开始时间还有多少秒
     */
    private Integer nextPrbStartTime;
}
