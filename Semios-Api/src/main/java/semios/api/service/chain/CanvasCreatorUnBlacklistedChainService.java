package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoStrategy;
import semios.api.model.enums.DaoBlackListEnum;
import semios.api.model.enums.DaoStrategyStrategyTypeEnum;
import semios.api.model.enums.DaoStrategyTypeEnum;
import semios.api.service.IDaoService;
import semios.api.service.IDaoStrategyService;
import semios.api.service.SubscriberChainService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * 消除 canvas creator 黑名单
 * 1.1 消除 create work 黑名单
 *
 * @description: 消除 canvas creator 黑名单
 * @author: xiangbin
 * @create: 2022-08-25 13:43
 **/
@Slf4j
@Service
public class CanvasCreatorUnBlacklistedChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    @Autowired
    private IDaoStrategyService daoStrategyService;

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[CanvasCreatorUnBlacklistedChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        // String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getTopics());
        // List<String> dataList = CommonUtil.splitBy32Bytes(data);
        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());
        String daoId = CommonUtil.removeHexPrefixIfExists(topics.get(1));
        String account =
                CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(2)).toLowerCase());

        log.info("[CanvasCreatorUnBlacklistedChainService] daoId:{} account:{}", daoId, account);

        Dao dao = daoService.daoDetailByProjectId(daoId);
        if (dao == null) {
            throw new RuntimeException("CanvasCreatorUnBlacklistedChainService cannot find dao");
        }

        DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                DaoStrategyTypeEnum.CREATE_CANVAS.getType(), DaoStrategyStrategyTypeEnum.BLACK_LIST.getType());
        Dao dao1 = new Dao();
        if (daoStrategy == null) {
            throw new RuntimeException("CanvasCreatorUnBlacklistedChainService cannot find daoStrategy!");
        } else {
            String originAddress = daoStrategy.getOriginAddress();
            Set<String> addressSet = new HashSet<>(Arrays.asList(originAddress.split(",")));
            addressSet.remove(account);
            if (addressSet.size() > 0) {
                daoStrategy.setOriginAddress(String.join(",", addressSet));
            } else {
                daoStrategy.setOriginAddress("");
            }

            if (addressSet.size() == 0) {
                dao1.setId(dao.getId());
                dao1.setCanvasCreatedBlacklist(DaoBlackListEnum.CLOSE.getStatus());
                log.info("[CanvasCreatorUnBlacklistedChainService] dao setCanvasCreatedBlacklist CLOSE");
            } else {
                dao1 = null;
            }
        }

        daoStrategyService.saveDaoStrategyOrUpdateDao(daoStrategy, dao1);

    }

}
