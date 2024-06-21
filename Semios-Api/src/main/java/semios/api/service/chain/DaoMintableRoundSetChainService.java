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
 * 设置 DAO 总铸造周期
 *
 * @description: 设置 DAO 总铸造周期
 * @author: xiangbin
 * @create: 2023-06-12 13:43
 **/
@Slf4j
@Service
public class DaoMintableRoundSetChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    // https://goerli.etherscan.io/tx/0x38f62e4c840f1ffb2278922d165c84fbd621e776a467e94b0bebaef71d18525b#eventlog

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[DaoMintableRoundSetChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);


        String projectId = CommonUtil.addHexPrefixIfNotExist(dataList.get(0));

        String newMintableRounds = CommonUtil.hexToTenString(dataList.get(1));
        if (newMintableRounds == null) {
            throw new RuntimeException("DaoMintableRoundSetChainService newMintableRounds is null");
        }

        Dao dao = daoService.daoDetailByProjectId(CommonUtil.removeHexPrefixIfExists(projectId));
        if (dao == null) {
            log.error("[DaoMintableRoundSetChainService] dao not find projectId:{}", projectId);
            throw new RuntimeException("DaoMintableRoundSetChainService cannot find dao");
        }
        if (dao.getDaoVersion().equals(1)) {
            log.error("[DaoMintableRoundSetChainService] dao not Support modification daoId:{}", dao.getId());
            throw new RuntimeException("DaoMintableRoundSetChainService dao not Support modification");
        }

        Dao updateDao = new Dao();
        updateDao.setId(dao.getId());
        updateDao.setDaoMintWindow(Integer.valueOf(newMintableRounds));

        log.info("[DaoMintableRoundSetChainService] daoId:{} newMintableRounds:{} ", dao.getId(), newMintableRounds);

        daoService.updateById(updateDao);
    }


}
