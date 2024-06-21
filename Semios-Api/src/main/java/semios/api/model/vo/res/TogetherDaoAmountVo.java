package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.io.Serializable;

/**
 * @description: 这一系列DAO的数量
 * @author: xiangbin
 * @create: 2022-08-04 14:45
 **/
@Slf4j
@Data
public class TogetherDaoAmountVo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * DAO总数
     */
    private Integer totalAmount = 0;

    /**
     * 运行下的main dao ID
     */
    private Integer mainDaoId = 0;


    /**
     * 正在运行的dao数量
     */
    private Integer runningDaoAmount = 0;

    /**
     * 已经结束的dao数量
     */
    private Integer endedDaoAmount = 0;

    /**
     * 未开始的dao数量
     */
    private Integer notStartedDaoAmount = 0;


}
