package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.chain.DaoReserveRatio;
import semios.api.model.dto.chain.DaoRoyaltyToken;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.service.IDaoService;
import semios.api.service.SubscriberChainService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;
import semios.api.utils.ProtoDaoCommonUtil;

import java.util.List;

/**
 * dao收益分配事件监听
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-06-12 13:43
 **/
@Slf4j
@Service
public class DaoRatioSetChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    // https://goerli.etherscan.io/tx/0x86c241fc8f702dfcf432dc3c8a9ffc9c5684667c814ca663de062c3031b3be2c
    // v2 https://goerli.etherscan.io/tx/0x001a4a459e1670e2988f1b73efe7cb40e35a7b84726c4a87bf19db9f5b5fd6ea#eventlog

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[DaoRatioSetChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String projectId = CommonUtil.addHexPrefixIfNotExist(dataList.get(0).toLowerCase());
        String daoCreatorErc20Ratio = CommonUtil.hexToTenString(dataList.get(1));
        String canvasCreatorRatio = CommonUtil.hexToTenString(dataList.get(2));
        String nftMinterRatio = CommonUtil.hexToTenString(dataList.get(3));
        String daoFeePoolEthRatio = CommonUtil.hexToTenString(dataList.get(4));
        String daoFeePoolEthRatioFlatPrice = CommonUtil.hexToTenString(dataList.get(5));

        Dao dao = daoService.daoDetailByProjectId(CommonUtil.removeHexPrefixIfExists(projectId));
        if (dao == null) {
            log.error("[DaoRatioSetChainService] dao not find projectId:{}", projectId);
            throw new RuntimeException("DaoRatioSetChainService cannot find dao");
        }
        if (dao.getDaoVersion().equals(0)) {
            log.error("[DaoRatioSetChainService] dao not Support modification daoId:{}", dao.getId());
            throw new RuntimeException("DaoRatioSetChainService dao not Support modification");
        }
        DaoRoyaltyToken daoRoyaltyToken = new DaoRoyaltyToken();
        if (StringUtils.isNotBlank(dao.getRoyaltyToken())) {
            daoRoyaltyToken = JacksonUtil.json2pojo(dao.getRoyaltyToken(), DaoRoyaltyToken.class);
        }
        daoRoyaltyToken.setDaoReward(ProtoDaoCommonUtil.strToBigDecimal(daoCreatorErc20Ratio));
        daoRoyaltyToken.setCanvasReward(ProtoDaoCommonUtil.strToBigDecimal(canvasCreatorRatio));
        daoRoyaltyToken.setMinterReward(ProtoDaoCommonUtil.strToBigDecimal(nftMinterRatio));
        // 非一口价
        DaoReserveRatio daoReserveRatio = new DaoReserveRatio(true);
        if (StringUtils.isNotBlank(dao.getFixedReserveRatio())) {
            daoReserveRatio = JacksonUtil.json2pojo(dao.getUnfixedReserveRatio(), DaoReserveRatio.class);
        }
        daoReserveRatio.setDaoMintFee(ProtoDaoCommonUtil.strToBigDecimal(daoFeePoolEthRatio));
        // 一口价
        DaoReserveRatio fixedDaoReserveRatio = new DaoReserveRatio(false);
        if (StringUtils.isNotBlank(dao.getRoyaltyToken())) {
            fixedDaoReserveRatio = JacksonUtil.json2pojo(dao.getFixedReserveRatio(), DaoReserveRatio.class);
        }
        fixedDaoReserveRatio.setDaoMintFee(ProtoDaoCommonUtil.strToBigDecimal(daoFeePoolEthRatioFlatPrice));

        Dao updateDao = new Dao();
        updateDao.setId(dao.getId());

        updateDao.setRoyaltyToken(JacksonUtil.obj2json(daoRoyaltyToken));
        updateDao.setUnfixedReserveRatio(JacksonUtil.obj2json(daoReserveRatio));
        updateDao.setFixedReserveRatio(JacksonUtil.obj2json(fixedDaoReserveRatio));

        log.info("[DaoRatioSetChainService] daoId:{} royaltyToken:{}", dao.getId(), updateDao.getRoyaltyToken());
        log.info("[DaoRatioSetChainService] daoId:{} unfixedReserveRatio:{}", dao.getId(),
                updateDao.getUnfixedReserveRatio());
        log.info("[DaoRatioSetChainService] daoId:{} fixedReserveRatio:{}", dao.getId(), updateDao.getFixedReserveRatio());
        daoService.updateById(updateDao);
    }


}
