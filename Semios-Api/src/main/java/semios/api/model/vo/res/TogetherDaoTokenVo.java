package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.io.Serializable;

/**
 * @description: DAO的ERC-20信息
 * @author: xiangbin
 * @create: 2022-08-04 14:45
 **/
@Slf4j
@Data
public class TogetherDaoTokenVo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * ERC-20 地址
     */
    private String erc20Address;

    /**
     * ERC-20  totalSupply()
     */
    private String totalSupply;

    /**
     * DAO Token的流通量
     * (系列 DAO 下所有 DaoAssetPool 的 ERC20 balance － DaoRedeemPool 的 ERC20 balance)
     */
    private String daoTokenBalance;

    /**
     * redeem池的ETH余额
     */
    private String redeemAssetPoolEth;

    /**
     * Redeem池中的erc-20数量（也就是已经redeem的数量）
     */
    private String redeemedErc20Amont;


}
