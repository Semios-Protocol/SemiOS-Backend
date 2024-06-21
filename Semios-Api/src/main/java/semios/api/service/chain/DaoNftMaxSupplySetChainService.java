package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.service.IDaoService;
import semios.api.service.SubscriberChainService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.util.List;

/**
 * 设置 DAO NFT 发放总量
 *
 * @description: 设置 DAO NFT 发放总量
 * @author: xiangbin
 * @create: 2023-06-12 13:43
 **/
@Slf4j
@Service
public class DaoNftMaxSupplySetChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    // https://goerli.etherscan.io/tx/0x38f62e4c840f1ffb2278922d165c84fbd621e776a467e94b0bebaef71d18525b#eventlog

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[DaoNftMaxSupplySetChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());
        if (topics == null) {
            throw new RuntimeException("DaoNftMaxSupplySetChainService topics is null");
        }
        String projectId = CommonUtil.addHexPrefixIfNotExist(topics.get(1));

        String newMaxSupply = CommonUtil.hexToTenString(dataList.get(0));
        if (newMaxSupply == null) {
            throw new RuntimeException("DaoNftMaxSupplySetChainService newMaxSupply is null");
        }

        Dao dao = daoService.daoDetailByProjectId(CommonUtil.removeHexPrefixIfExists(projectId));
        if (dao == null) {
            log.error("[DaoNftMaxSupplySetChainService] dao not find projectId:{}", projectId);
            throw new RuntimeException("DaoNftMaxSupplySetChainService cannot find dao");
        }
        if (dao.getDaoVersion().equals(1)) {
            log.error("[DaoNftMaxSupplySetChainService] dao not Support modification daoId:{}", dao.getId());
            throw new RuntimeException("DaoNftMaxSupplySetChainService dao not Support modification");
        }

        Dao updateDao = new Dao();
        updateDao.setId(dao.getId());
        updateDao.setTotalNftCasting(Integer.valueOf(newMaxSupply));

        log.info("[DaoNftMaxSupplySetChainService] daoId:{} newMaxSupply:{} ", dao.getId(), newMaxSupply);

        daoService.updateById(updateDao);
    }


}
