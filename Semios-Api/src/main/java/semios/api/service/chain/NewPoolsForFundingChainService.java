package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.enums.TrueOrFalseEnum;
import semios.api.service.IDaoService;
import semios.api.service.SubscriberChainService;
import semios.api.service.common.CommonService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.util.List;

/**
 * NewPoolsForFunding 改为了NewPools 事件
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-09-04 13:43
 **/
@Slf4j
@Service
public class NewPoolsForFundingChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    @Autowired
    private CommonService commonService;

    public static void main(String[] args) {
        String data = "0x58717bcca2312f1cb0dc5ad10138271a0fe13a57c5b157dff6eafc452ddd46d6000000000000000000000000b15e458cac809ee256339ddb8ad2af885c62fe3100000000000000000000000080e3ae9728b748d2faa045b47ce0e7091e0ed5650000000000000000000000000000000000000000000000000000000000000000";
        List<String> dataList = CommonUtil.splitBy32Bytes(data);


        // projectId
        String projectId = CommonUtil.removeHexPrefixIfExists(dataList.get(0));
        // 资产池
        String daoAssetPool = CommonUtil.removeHexPrefixIfExists(CommonUtil.formatBytes32Address(dataList.get(1)));
        // Redeem池
        String daoRedeemPool = CommonUtil.removeHexPrefixIfExists(CommonUtil.formatBytes32Address(dataList.get(2)));
        // 这个参数暂时不需要考虑，可能会被删除
//        String daoFundingPool = CommonUtil.removeHexPrefixIfExists(CommonUtil.formatBytes32Address(dataList.get(3)));
        // 是否为外部ERC20
        String isThirdPartyToken = CommonUtil.removeHexPrefixIfExists(dataList.get(3));
        System.out.println("project_id" + " is " + projectId);
        System.out.println("daoAssetPool" + " is " + daoAssetPool);
        System.out.println("daoRedeemPool" + " is " + daoRedeemPool);
        System.out.println("isThirdPartyToken" + " is " + isThirdPartyToken);
    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[NewPoolsForFundingChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        // projectId
        String projectId = CommonUtil.removeHexPrefixIfExists(dataList.get(0));
        // 资产池
        String daoAssetPool = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));
        // Redeem池
        String daoRedeemPool = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2)));
        // 这个参数暂时不需要考虑，可能会被删除  1.4已删除
//        String daoFundingPool = CommonUtil.removeHexPrefixIfExists(CommonUtil.formatBytes32Address(dataList.get(3)));

        // check 比例
        // 1.6 eth-->token比例
        String tokenTransEth = CommonUtil.hexToTenString(dataList.get(3));
        String ethTransToken = CommonUtil.hexToTenString(dataList.get(4));
        log.info("[NewPoolsForFundingChainService] ethTransToken:{},tokenTransEth:{}", ethTransToken, tokenTransEth);

        // 是否为外部ERC20
        String isThirdPartyToken = CommonUtil.removeHexPrefixIfExists(dataList.get(5));

        Dao dao = daoService.daoDetailByProjectId(projectId);
        if (dao == null) {
            throw new RuntimeException("NewPoolsForFundingChainService cannot find dao");
        }
        BigDecimal erc20TotalSupply = commonService.erc20BalanceOf(dao.getErc20Token(), daoAssetPool, dao.getErc20TokenDecimals(), dao.getInputTokenDecimals());

        Dao updateDao = new Dao();
        updateDao.setId(dao.getId());
        updateDao.setFeePool(daoAssetPool);
        updateDao.setDaoRedeemPool(daoRedeemPool);
        updateDao.setIsThirdpartyToken(Integer.parseInt(isThirdPartyToken));
        if (erc20TotalSupply != null) {
            updateDao.setErc20TotalSupply(erc20TotalSupply.stripTrailingZeros().toPlainString());
            updateDao.setSubdaoAssetPoolBalance(erc20TotalSupply.stripTrailingZeros().toPlainString());
        }
        //第三方erc20地址不需要同步dex，也不可以redeem
        if (updateDao.getIsThirdpartyToken() == 1) {
            updateDao.setSyncDex(0);
        }
        if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getIsAncestordao()) && dao.getTogetherDaoId() != null) {
            Dao togetherDao = daoService.getById(dao.getTogetherDaoId());
            if (togetherDao != null && StringUtils.isBlank(togetherDao.getFeePool())) {
                log.info("[NewPoolsForFundingChainService] update togetherDao id:{}  feePoll:{}", togetherDao.getId(), daoAssetPool);
                togetherDao.setFeePool(daoAssetPool);
                daoService.updateById(togetherDao);
            }
        }

        // 1.6 添加国库erc20地址
        BigDecimal tokenTransEthRatio = new BigDecimal(tokenTransEth).divide(new BigDecimal("10000"));
        updateDao.setTokenEthRoyalty(tokenTransEthRatio);
        BigDecimal ethTransTokenRatio = new BigDecimal(ethTransToken).divide(new BigDecimal("10000"));
        updateDao.setEthTokenRoyalty(ethTransTokenRatio);

        daoService.updateById(updateDao);
        log.info("[NewPoolsForFundingChainService] daoId:{} updateDao:{}", dao.getId(), JacksonUtil.obj2json(updateDao));

    }
}
