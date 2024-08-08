package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.io.Serializable;

/**
 * @description: 这一系列DAO的Maker的信息
 * @author: xiangbin
 * @create: 2022-08-04 14:45
 **/
@Slf4j
@Data
public class TogetherDaoMakerVo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * maker的总数量
     */
    private Integer makerTotalAmount = 0;

    /**
     * 还没花费的ETH加和
     */
    private String noSpendEthAmount = "0";

    /**
     * 还没花费的Token加和
     */
    private String noSpendTokenAmount = "0";


}
