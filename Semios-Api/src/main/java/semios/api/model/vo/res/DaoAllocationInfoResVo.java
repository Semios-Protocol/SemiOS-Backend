package semios.api.model.vo.res;

import lombok.Data;
import semios.api.model.dto.chain.DaoRoyaltyToken;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * 1.4 dao的eth或者token分配比例查询
 *
 * @description: analytics
 * @author: xiangbin
 * @create: 2023-04-19 15:14
 **/
@Data
public class DaoAllocationInfoResVo {

    /**
     * 从其他dao分配的ETH比例
     */
    private List<DaoAllocationVo> receivedEthFromOther = new ArrayList<>();

    /**
     * 当前dao分配给其他dao的ETH比例
     */
    private List<DaoAllocationVo> allocationEthToOtherDao = new ArrayList<>();

    /**
     * 从其他dao分配的Token比例
     */
    private List<DaoAllocationVo> receivedTokenFromOther = new ArrayList<>();

    /**
     * 当前dao分配给其他dao的Token比例
     */
    private List<DaoAllocationVo> allocationTokenToOtherDao = new ArrayList<>();

    /**
     * eth分配比例
     */
    private DaoRoyaltyToken ethRoyaltyToken;

    /**
     * token分配比例
     */
    private DaoRoyaltyToken royaltyToken;


    /**
     * 当前区块铸造信息展示
     */
    private CurrentMintWindowInfoVo currentMintWindowInfoVo;

    /**
     * dao是否为topup模式
     */
    private Boolean topupMode;

    /**
     * dao分配比例
     */
    @Data
    static class AllocationVo {

        /**
         * daoId
         */
        private String daoId;

        /**
         * dao名字
         */
        private String daoName;

        /**
         * 分配比例
         */
        private BigDecimal allocationRatio;
    }


}


