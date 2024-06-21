package semios.api.model.vo.res;

import lombok.Data;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * myDao信息，只返回dao名字
 *
 * @description: DAO列表信息
 * @author: xiangbin
 * @create: 2022-08-04 14:45
 **/
@Data
public class DaoFlowResVo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * dao projectId
     */
    private String projectId;

    /**
     * dao feePool地址
     */
    private String feePool;

    /**
     * 2eth-feePool当前交易流水
     */
    private BigDecimal daoFlow;

    /**
     * 是否为basic dao 1-proto dao 2- basic dao
     */
    private Integer basicDao;

}
