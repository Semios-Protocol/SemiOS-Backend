package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @description: 这一系列DAO的Treasury的信息
 * @author: zhyyao
 * @create: 2024-02-22 14:45
 **/
@Slf4j
@Data
public class TogetherDaoTreasuryVo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * treasury的总数量
     */
    private BigDecimal treasuryTotalAmount;

}
