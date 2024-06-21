package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import semios.api.model.entity.Dao;
import semios.api.utils.CommonUtil;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * @description: 这一系列DAO的Maker的页面信息
 * @author: zhyyao
 * @create: 2024-02-22 14:45
 **/
@Slf4j
@Data
public class TogetherDaoMakerListVo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 聚合dao project ID
     *
     * @mock 123
     */
    private String projectId;

    /**
     * main dao owner address
     *
     * @mock 123
     */
    private String ownerAddress;


    /**
     * eth解锁token的wallet比例
     *
     * @mock 70
     */
    private BigDecimal ethTransTokenWalletRatioDefault;

    /**
     * eth解锁token的Treasury比例
     *
     * @mock 30
     */
    private BigDecimal ethTransTokenTreasuryRatioDefault;

    /**
     * eth解锁token的List
     */
    private List<TogetherDaoMakerInfoListVo> ethTransTokenList;


    /**
     * token解锁eth的wallet比例
     *
     * @mock 70
     */
    private BigDecimal tokenTransEthWalletRatioDefault;

    /**
     * token解锁eth的Redeem比例
     *
     * @mock 30
     */
    private BigDecimal tokenTransEthRedeemRatioDefault;

    /**
     * token解锁eth的List
     */
    private List<TogetherDaoMakerInfoListVo> tokenTransEthList;


    public static TogetherDaoMakerListVo transForTogetherDaoMakerLis(List<Dao> daoList) {
        TogetherDaoMakerListVo togetherDaoMakerListVo = new TogetherDaoMakerListVo();
        List<TogetherDaoMakerInfoListVo> ethToTokenList = new ArrayList<>();
        List<TogetherDaoMakerInfoListVo> tokenToEthList = new ArrayList<>();

        for (Dao dao : daoList) {
            // eth解锁token 比例
            // 列表不包含 开启dao token模式和top-up模式的dao
            if (dao.getErc20PaymentMode().equals(0) && dao.getTopupMode().equals(0)) {
                TogetherDaoMakerInfoListVo ethToToken = new TogetherDaoMakerInfoListVo();
                ethToToken.setDaoId(dao.getId());
                ethToToken.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
                ethToToken.setDaoName(dao.getDaoName());
                ethToToken.setTreasuryOrPoolRatio(dao.getEthTokenRoyalty().multiply(new BigDecimal("100")));
                ethToToken.setWalletRatio(new BigDecimal("100").subtract(ethToToken.getTreasuryOrPoolRatio()));
                ethToTokenList.add(ethToToken);
            }


            // token解锁eth 比例
            // 只包含开启dao token模式的dao
            if (dao.getErc20PaymentMode().equals(1)) {
                TogetherDaoMakerInfoListVo tokenToEth = new TogetherDaoMakerInfoListVo();
                tokenToEth.setDaoId(dao.getId());
                tokenToEth.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
                tokenToEth.setDaoName(dao.getDaoName());
                tokenToEth.setTreasuryOrPoolRatio(dao.getTokenEthRoyalty().multiply(new BigDecimal("100")));
                tokenToEth.setWalletRatio(new BigDecimal("100").subtract(tokenToEth.getTreasuryOrPoolRatio()));
                tokenToEthList.add(tokenToEth);
            }

        }
        togetherDaoMakerListVo.setEthTransTokenList(ethToTokenList);
        togetherDaoMakerListVo.setTokenTransEthList(tokenToEthList);


        return togetherDaoMakerListVo;
    }

}
