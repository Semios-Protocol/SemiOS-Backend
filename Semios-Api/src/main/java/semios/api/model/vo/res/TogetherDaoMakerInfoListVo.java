package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @description: 这一系列DAO的Maker的页面信息
 * @author: zhyyao
 * @create: 2024-02-22 14:45
 **/
@Slf4j
@Data
public class TogetherDaoMakerInfoListVo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * dao ID
     *
     * @mock 123
     */
    private Integer daoId;

    /**
     * sub dao project ID
     *
     * @mock 123
     */
    private String projectId;

    /**
     * sub dao dao name
     *
     * @mock 123
     */
    private String daoName;

    /**
     * Wallet比例
     *
     * @mock 30
     */
    private BigDecimal walletRatio;

    /**
     * Redeem Asset Pool比例 或者 Treasury比例
     *
     * @mock 30
     */
    private BigDecimal treasuryOrPoolRatio;

}
