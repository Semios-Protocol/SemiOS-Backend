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
 * 进入minter黑名单
 *
 * @description: 进入minter黑名单
 * @author: xiangbin
 * @create: 2022-08-25 13:43
 **/
@Slf4j
@Service
public class MinterBlacklistedChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    @Autowired
    private IDaoStrategyService daoStrategyService;

    public static void main(String[] args) {
        // Set<String> addressSet = new HashSet<>(Arrays.asList("One".split(",")));
        // addressSet.add("one");
        // String address = String.join(",", addressSet);
        // System.out.println(String.join(",", addressSet));
        // List<String> strList = Arrays.asList(address.split(","));
        // for (String s : strList) {
        // System.out.println(s);
        // }
        DaoStrategy daoStrategy = new DaoStrategy();
        daoStrategy.setOriginAddress("");
        System.out.println(StringUtils.isBlank(daoStrategy.getOriginAddress()));
        String originAddress = daoStrategy.getOriginAddress();
        Set<String> addressSet = new HashSet<>(Arrays.asList(originAddress.split(",")));
        addressSet.add("0xf8baf7268f3daefe4135f7711473ae8b6c3b47d8");
        System.out.println(String.join(",", addressSet));
        System.out.println(StringUtils.join(addressSet, ","));
        System.out.println(StringUtils.join(addressSet, ','));

    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[MinterBlacklistedChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        // String topics = CommonUtil.removeHexPrefixIfExists(transactionDto.getTopics());
        // List<String> dataList = CommonUtil.splitBy32Bytes(data);
        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());
        String daoId = CommonUtil.removeHexPrefixIfExists(topics.get(1));
        String account =
                CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(2)).toLowerCase());

        log.info("[MinterBlacklistedChainService] daoId:{} account:{}", daoId, account);
        Dao dao = daoService.daoDetailByProjectId(daoId);
        if (dao == null) {
            throw new RuntimeException("MinterBlacklistedChainService cannot find dao");
        }

        DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.BLACK_LIST.getType());
        if (daoStrategy == null) {
            daoStrategy = new DaoStrategy();
            daoStrategy.setType(DaoStrategyTypeEnum.MINT_WORK.getType());
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
        if (dao.getMinterWorksBlacklist().equals(DaoBlackListEnum.CLOSE.getStatus())) {
            dao1.setId(dao.getId());
            dao1.setMinterWorksBlacklist(DaoBlackListEnum.OPEN.getStatus());
            log.info("[MinterBlacklistedChainService] dao setMinterWorksBlacklist OPEN");
        } else {
            dao1 = null;
        }

        daoStrategyService.saveDaoStrategyOrUpdateDao(daoStrategy, dao1);

    }

}
