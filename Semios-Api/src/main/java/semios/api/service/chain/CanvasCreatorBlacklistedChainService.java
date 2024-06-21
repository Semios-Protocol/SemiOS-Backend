package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
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
 * 进入canvas creator黑名单
 * 1.1 create work 黑名单
 *
 * @description: 进入canvas creator黑名单
 * @author: xiangbin
 * @create: 2022-08-25 13:43
 **/
@Slf4j
@Service
public class CanvasCreatorBlacklistedChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    @Autowired
    private IDaoStrategyService daoStrategyService;

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[CanvasCreatorBlacklistedChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        // String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getTopics());
        // List<String> dataList = CommonUtil.splitBy32Bytes(data);
        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());
        String daoId = CommonUtil.removeHexPrefixIfExists(topics.get(1));
        String account =
                CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(2)).toLowerCase());

        log.info("[CanvasCreatorBlacklistedChainService] daoId:{} account:{}", daoId, account);

        Dao dao = daoService.daoDetailByProjectId(daoId);
        if (dao == null) {
            throw new RuntimeException("CanvasCreatorBlacklistedChainService cannot find dao");
        }

        DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                DaoStrategyTypeEnum.CREATE_CANVAS.getType(), DaoStrategyStrategyTypeEnum.BLACK_LIST.getType());
        if (daoStrategy == null) {
            daoStrategy = new DaoStrategy();
            daoStrategy.setType(DaoStrategyTypeEnum.CREATE_CANVAS.getType());
            daoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.BLACK_LIST.getType());
            daoStrategy.setOriginAddress(account);
            daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
            daoStrategy.setBlockTime(transactionDto.getBlockTime());
            daoStrategy.setDaoUri(dao.getDaoUri());
            daoStrategy.setDaoId(dao.getId());
            daoStrategy.setProjectId(dao.getProjectId());
            daoStrategy.setDaoNumber(dao.getDaoNumber());

        } else {
            String originAddress = daoStrategy.getOriginAddress();
            if (StringUtils.isBlank(originAddress)) {
                daoStrategy.setOriginAddress(account);
            } else {
                Set<String> addressSet = new HashSet<>(Arrays.asList(originAddress.split(",")));
                addressSet.add(account);
                daoStrategy.setOriginAddress(String.join(",", addressSet));
            }

            daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
            daoStrategy.setBlockTime(transactionDto.getBlockTime());
        }
        Dao dao1 = new Dao();
        if (dao.getCanvasCreatedBlacklist().equals(DaoBlackListEnum.CLOSE.getStatus())) {
            dao1.setId(dao.getId());
            dao1.setCanvasCreatedBlacklist(DaoBlackListEnum.OPEN.getStatus());
            log.info("[CanvasCreatorBlacklistedChainService] dao setCanvasCreatedBlacklist open");
        } else {
            dao1 = null;
        }

        daoStrategyService.saveDaoStrategyOrUpdateDao(daoStrategy, dao1);

    }

}
