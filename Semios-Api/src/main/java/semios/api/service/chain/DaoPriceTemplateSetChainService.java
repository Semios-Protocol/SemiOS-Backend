package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.service.IDaoService;
import semios.api.service.SubscriberChainService;
import semios.api.service.common.CommonService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.util.List;

/**
 * 设置 DAO 的变价规则
 *
 * @description: 设置 DAO 的变价规则
 * @author: xiangbin
 * @create: 2023-06-12 13:43
 **/
@Slf4j
@Service
public class DaoPriceTemplateSetChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    @Autowired
    private CommonService commonService;

    // https://goerli.etherscan.io/tx/0x38f62e4c840f1ffb2278922d165c84fbd621e776a467e94b0bebaef71d18525b#eventlog

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[DaoPriceTemplateSetChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());
        if (topics == null) {
            throw new RuntimeException("DaoPriceTemplateSetChainService topics is null");
        }
        String projectId = CommonUtil.addHexPrefixIfNotExist(topics.get(1));

        String priceTemplateType = CommonUtil.hexToTenString(dataList.get(0));
        String nftPriceFactor = CommonUtil.hexToTenString(dataList.get(1));
        if (priceTemplateType == null || nftPriceFactor == null) {
            throw new RuntimeException("DaoPriceTemplateSetChainService param is null");
        }

        Dao dao = daoService.daoDetailByProjectId(CommonUtil.removeHexPrefixIfExists(projectId));
        if (dao == null) {
            log.error("[DaoPriceTemplateSetChainService] dao not find projectId:{}", projectId);
            throw new RuntimeException("DaoPriceTemplateSetChainService cannot find dao");
        }
        if (dao.getDaoVersion().equals(1)) {
            log.error("[DaoPriceTemplateSetChainService] dao not Support modification daoId:{}", dao.getId());
            throw new RuntimeException("DaoPriceTemplateSetChainService dao not Support modification");
        }

        Dao updateDao = new Dao();
        updateDao.setId(dao.getId());
        updateDao.setCanvasPriceFluctuationMethod(Integer.valueOf(priceTemplateType));
        updateDao.setFluctuationMethodFactor(new BigDecimal(nftPriceFactor));

        if (!updateDao.getCanvasPriceFluctuationMethod().equals(dao.getCanvasPriceFluctuationMethod())
                || !updateDao.getFluctuationMethodFactor().equals(dao.getFluctuationMethodFactor())) {
            commonService.updateCanvasPrice(dao);
        }

        log.info("[DaoPriceTemplateSetChainService] daoId:{} priceTemplateType:{} nftPriceFactor:{}", dao.getId(), priceTemplateType, nftPriceFactor);

        daoService.updateById(updateDao);
    }


}
