package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.chain.DesignatedCap;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoStrategy;
import semios.api.model.enums.DaoStrategyStrategyTypeEnum;
import semios.api.model.enums.DaoStrategyTypeEnum;
import semios.api.service.IDaoService;
import semios.api.service.IDaoStrategyService;
import semios.api.service.SubscriberChainService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.util.ArrayList;
import java.util.List;

/**
 * 添加铸造上限
 *
 * @description: 添加铸造上限
 * @author: xiangbin
 * @create: 2023-03-10 13:43
 **/
@Slf4j
@Service
public class MintCapAddedChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    @Autowired
    private IDaoStrategyService daoStrategyService;

    public static void main(String[] args) {
        String data = CommonUtil.removeHexPrefixIfExists(
                "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000000300000000000000000000000075f7a0b820f7aaf559a0324192ef2f9fe85585d00000000000000000000000000000000000000000000000000000000000000001000000000000000000000000c537a223b7fe86483d31442248c5918177526bef0000000000000000000000000000000000000000000000000000000000000002000000000000000000000000f8baf7268f3daefe4135f7711473ae8b6c3b47d80000000000000000000000000000000000000000000000000000000000000003");
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String minterMerkleRoot = dataList.get(0);
        String minterNftHolderPass =
                CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1))).toLowerCase();
        String canvasCreatorMerkleRoot = dataList.get(2);
        String canvasCreatorNftHolderPass =
                CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(3))).toLowerCase();

        String amount = CommonUtil.hexToTenString(dataList.get(4));

        System.out.println(minterMerkleRoot);
        System.out.println(minterNftHolderPass);
        System.out.println(canvasCreatorMerkleRoot);
        System.out.println(canvasCreatorNftHolderPass);
        System.out.println(amount);

        // String origin =
        // "[\"0x75F7A0B820f7AaF559A0324192EF2F9fE85585D0\",\"0x1111111111111111111111111111111111110007\"]";
        // System.out.println(origin.substring(1, origin.length() - 1).replaceAll("\"", ""));
    }

    /**
     * event MintCapAdded(bytes32 indexed DAO_id, uint32 mintCap, DesignatedCap[] designatedMintCaps);
     * <p>
     * 其中 mintCap 为 DAO 下的全局铸造上限，designatedMintCaps 为高优先级指定铸造上限的名单的数组 结构体如下
     * <p>
     * struct DesignatedCap { address account; uint32 cap; } 对应指定的高优先级铸造上限的名单，每个 account 对应一个 cap 铸造上限
     * <p>
     * cap为零代表取消其对应地址的优先级
     *
     * @param transactionDto 参数
     * @throws Exception 异常
     */
    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[MintCapAddedChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());
        String daoId = null;
        if (topics != null && topics.size() > 1) {
            daoId = CommonUtil.removeHexPrefixIfExists(topics.get(1));
        }

        Dao dao = daoService.daoDetailByProjectId(daoId);
        if (dao == null) {
            throw new RuntimeException("MintCapAddedChainService cannot find dao");
        }

        Dao dao1 = new Dao();

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String mintCap = CommonUtil.hexToTenString(dataList.get(0));

        String capSize = CommonUtil.hexToTenString(dataList.get(2));
        List<DesignatedCap> designatedCaps = null;
        if (StringUtils.isNotBlank(capSize) && Integer.parseInt(capSize) > 0) {
            designatedCaps = new ArrayList<>(Integer.parseInt(capSize));
            for (int i = 0; i < Integer.parseInt(capSize); i++) {
                DesignatedCap designatedCap = new DesignatedCap();
                String account = CommonUtil
                        .addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2 * i + 3))).toLowerCase();
                designatedCap.setAccount(account);

                String cap = CommonUtil.hexToTenString(dataList.get(2 * i + 4));
                if (StringUtils.isNotBlank(cap)) {
                    designatedCap.setCap(Integer.parseInt(cap));
                    if (Integer.parseInt(cap) > 0) {
                        designatedCaps.add(designatedCap);
                    }
                }
            }
        }
        List<DaoStrategy> daoStrategyList = new ArrayList<>();
        DaoStrategy newDaoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.HIGH_PRIORITY.getType());
        if (newDaoStrategy == null) {
            newDaoStrategy = new DaoStrategy();
            newDaoStrategy.setType(DaoStrategyTypeEnum.MINT_WORK.getType());
            newDaoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.HIGH_PRIORITY.getType());
            newDaoStrategy.setProofId(dao.getId());
            newDaoStrategy.setDaoUri(dao.getDaoUri());
            newDaoStrategy.setDaoId(dao.getId());
            newDaoStrategy.setProjectId(dao.getProjectId());
            newDaoStrategy.setDaoNumber(dao.getDaoNumber());
        } else {
            log.info("[MintCapAddedChainService] daoId:{} origin daoStrategy:{}", dao.getId(),
                    JacksonUtil.obj2json(newDaoStrategy));
        }

        newDaoStrategy.setTransactionHash(transactionDto.getTransactionHash());
        newDaoStrategy.setBlockTime(transactionDto.getBlockTime());

        if (designatedCaps != null && !designatedCaps.isEmpty()) {
            newDaoStrategy.setOriginAddress(JacksonUtil.obj2json(designatedCaps));
            if (dao.getMintCap() == null || dao.getMintCap() == 0) {
                dao1.setId(dao.getId());
                dao1.setMintCap(1);
            }
        } else {
            newDaoStrategy.setOriginAddress(null);
            if (dao.getMintCap() != null && dao.getMintCap() == 1) {
                dao1.setId(dao.getId());
                dao1.setMintCap(0);
            }
        }

        daoStrategyList.add(newDaoStrategy);

        if (StringUtils.isNotBlank(mintCap)) {
            dao1.setId(dao.getId());
            dao1.setGlobalMintCap(Integer.parseInt(mintCap));
        }
        if (dao1.getId() == null) {
            dao1 = null;
        }
        log.info("[MintCapAddedChainService] dao:{} daoStrategyList:{}", JacksonUtil.obj2json(dao1),
                JacksonUtil.obj2json(daoStrategyList));
        int i = daoStrategyService.saveDaoStrategyListOrUpdateDao(daoStrategyList, dao1);
        log.info("[MintCapAddedChainService] daoId:{} return i:{}", dao.getId(), i);

    }

}
