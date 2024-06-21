package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.chain.DesignatedCap;
import semios.api.model.dto.chain.DesignatedNftCap;
import semios.api.model.dto.common.ProtoDaoConstant;
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
 * 修改黑白名单相关参数
 *
 * @description: 修改黑白名单相关参数
 * @author: xiangbin
 * @create: 2023-09-25 13:43
 **/
@Slf4j
@Service
public class MintCapSetChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    @Autowired
    private IDaoStrategyService daoStrategyService;

    public static void main(String[] args) {
        String data = CommonUtil.removeHexPrefixIfExists(
                "0x0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String mintCap = CommonUtil.hexToTenString(dataList.get(0));
        log.info("mintCap:" + mintCap);
        String userMintCapStr = CommonUtil.hexToTenString(dataList.get(1));

        String nftMinterCapStr = CommonUtil.hexToTenString(dataList.get(2));
        Integer nftMinterCapCount = Integer.parseInt(nftMinterCapStr) / 32;

        String nftMinterCapIdStr = CommonUtil.hexToTenString(dataList.get(3));
        Integer nftMinterCapIdCount = Integer.parseInt(nftMinterCapIdStr) / 32;


        String userMintCapSize = CommonUtil.hexToTenString(dataList.get(Integer.parseInt(userMintCapStr) / 32));
        System.out.println("userMintCapSize:" + userMintCapSize);
        String nftMinterCapSize = CommonUtil.hexToTenString(dataList.get(Integer.parseInt(nftMinterCapStr) / 32));
        System.out.println("nftMinterCapSize:" + nftMinterCapSize);
        String nftMinterCapIdSize = CommonUtil.hexToTenString(dataList.get(Integer.parseInt(nftMinterCapIdStr) / 32));
        System.out.println("nftMinterCapIdSize:" + nftMinterCapIdSize);

        List<DesignatedCap> designatedCaps = null;
        if (StringUtils.isNotBlank(userMintCapSize) && Integer.parseInt(userMintCapSize) > 0) {
            int userMintCap = Integer.parseInt(userMintCapSize);
            designatedCaps = new ArrayList<>();
            for (int i = 0; i < userMintCap; i++) {
                DesignatedCap designatedCap = new DesignatedCap();
                String account = CommonUtil
                        .addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2 * i + 5))).toLowerCase();
                System.out.println("userMintCapSize account:" + account);
                designatedCap.setAccount(account);

                String cap = CommonUtil.hexToTenString(dataList.get(2 * i + 6));
                System.out.println("userMintCapSize cap:" + cap);
                if (StringUtils.isNotBlank(cap)) {
                    designatedCap.setCap(Integer.parseInt(cap));
                    if (Integer.parseInt(cap) > 0) {
                        designatedCaps.add(designatedCap);
                    }
                }
            }
        }
        log.info("designatedCaps:" + JacksonUtil.obj2json(designatedCaps));


        List<DesignatedCap> nftDesignatedCaps = null;
        if (StringUtils.isNotBlank(nftMinterCapSize) && Integer.parseInt(nftMinterCapSize) > 0) {
            int nftMinterCap = Integer.parseInt(nftMinterCapSize);
            nftDesignatedCaps = new ArrayList<>();
            for (int i = 0; i < nftMinterCap; i++) {
                DesignatedCap designatedCap = new DesignatedCap();
                String account = CommonUtil
                        .addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2 * i + nftMinterCapCount + 1))).toLowerCase();
                System.out.println("nftMinterCapSize account:" + account);
                designatedCap.setAccount(account);

                String cap = CommonUtil.hexToTenString(dataList.get(2 * i + nftMinterCapCount + 2));
                System.out.println("nftMinterCapSize cap:" + cap);
                if (StringUtils.isNotBlank(cap)) {
                    designatedCap.setCap(Integer.parseInt(cap));
                    if (Integer.parseInt(cap) > 0) {
                        nftDesignatedCaps.add(designatedCap);
                    }
                }
            }
        }
        log.info("nftDesignatedCaps:" + JacksonUtil.obj2json(nftDesignatedCaps));

        List<DesignatedNftCap> nftDesignatedIdCaps = null;
        if (StringUtils.isNotBlank(nftMinterCapIdSize) && Integer.parseInt(nftMinterCapIdSize) > 0) {
            int nftMinterIdCap = Integer.parseInt(nftMinterCapIdSize);
            log.info("nftMinterIdCap:" + nftMinterIdCap);
            nftDesignatedIdCaps = new ArrayList<>();
            for (int i = 0; i < nftMinterIdCap; i++) {
                DesignatedNftCap designatedCap = new DesignatedNftCap();
                String account = CommonUtil
                        .addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(3 * i + nftMinterCapIdCount + 1))).toLowerCase();
                System.out.println("nftMinterCapIdSize account:" + account);
                designatedCap.setNftAddress(account);

                String tokenId = CommonUtil.hexToTenString(dataList.get(3 * i + nftMinterCapIdCount + 2));
                System.out.println("nftMinterCapIdSize tokenId:" + tokenId);
                if (StringUtils.isNotBlank(tokenId)) {
                    designatedCap.setTokenId(Integer.parseInt(tokenId));
                }

                String cap = CommonUtil.hexToTenString(dataList.get(3 * i + nftMinterCapIdCount + 3));
                System.out.println("nftMinterCapIdSize cap:" + cap);
                if (StringUtils.isNotBlank(cap)) {
                    designatedCap.setNftMintCap(Integer.parseInt(cap));
                    if (Integer.parseInt(cap) > 0) {
                        nftDesignatedIdCaps.add(designatedCap);
                    }
                }
            }
        }
        log.info("nftDesignatedIdCaps:" + JacksonUtil.obj2json(nftDesignatedIdCaps));

    }

    /**
     * event MintCapAdded(bytes32 indexed DAO_id, uint32 mintCap, DesignatedCap[] designatedMintCaps);
     * --
     * emit MintCapSet(daoId, daoMintCap, userMintCapParams, nftMinterCapInfo);
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

        log.info("[MintCapSetChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());
        String daoId = null;
        if (topics != null && topics.size() > 1) {
            daoId = CommonUtil.removeHexPrefixIfExists(topics.get(1));
        }

        Dao dao = daoService.daoDetailByProjectId(daoId);
        if (dao == null) {
            throw new RuntimeException("MintCapSetChainService cannot find dao");
        }

        Dao dao1 = new Dao();

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String mintCap = CommonUtil.hexToTenString(dataList.get(0));
        log.info("mintCap:" + mintCap);

        String userMintCapStr = CommonUtil.hexToTenString(dataList.get(1));

        String nftMinterCapStr = CommonUtil.hexToTenString(dataList.get(2));
        Integer nftMinterCapCount = Integer.parseInt(nftMinterCapStr) / 32;

        String nftMinterCapIdStr = CommonUtil.hexToTenString(dataList.get(3));
        Integer nftMinterCapIdCount = Integer.parseInt(nftMinterCapIdStr) / 32;


        String userMintCapSize = CommonUtil.hexToTenString(dataList.get(Integer.parseInt(userMintCapStr) / 32));
        System.out.println("userMintCapSize:" + userMintCapSize);
        String nftMinterCapSize = CommonUtil.hexToTenString(dataList.get(Integer.parseInt(nftMinterCapStr) / 32));
        System.out.println("nftMinterCapSize:" + nftMinterCapSize);
        String nftMinterCapIdSize = CommonUtil.hexToTenString(dataList.get(Integer.parseInt(nftMinterCapIdStr) / 32));
        System.out.println("nftMinterCapIdSize:" + nftMinterCapIdSize);

        List<DesignatedCap> designatedCaps = null;
        if (StringUtils.isNotBlank(userMintCapSize) && Integer.parseInt(userMintCapSize) > 0) {
            int userMintCap = Integer.parseInt(userMintCapSize);
            designatedCaps = new ArrayList<>();
            for (int i = 0; i < userMintCap; i++) {
                DesignatedCap designatedCap = new DesignatedCap();
                String account = CommonUtil
                        .addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2 * i + 5))).toLowerCase();
                designatedCap.setAccount(account);

                String cap = CommonUtil.hexToTenString(dataList.get(2 * i + 6));
                if (StringUtils.isNotBlank(cap)) {
                    designatedCap.setCap(Integer.parseInt(cap));
                    if (Integer.parseInt(cap) > 0) {
                        designatedCaps.add(designatedCap);
                    }
                }
            }
        }
        log.info("designatedCaps:" + JacksonUtil.obj2json(designatedCaps));

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
            log.info("[MintCapSetChainService] daoId:{} HIGH_PRIORITY origin daoStrategy:{} ", dao.getId(),
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


        List<DesignatedCap> nftDesignatedCaps = null;
        if (StringUtils.isNotBlank(nftMinterCapSize) && Integer.parseInt(nftMinterCapSize) > 0) {
            int nftMinterCap = Integer.parseInt(nftMinterCapSize);
            nftDesignatedCaps = new ArrayList<>();
            for (int i = 0; i < nftMinterCap; i++) {
                DesignatedCap designatedCap = new DesignatedCap();
                String account = CommonUtil
                        .addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2 * i + nftMinterCapCount + 1))).toLowerCase();
                designatedCap.setAccount(account);

                String cap = CommonUtil.hexToTenString(dataList.get(2 * i + nftMinterCapCount + 2));
                if (StringUtils.isNotBlank(cap)) {
                    designatedCap.setCap(Integer.parseInt(cap));
                    if (Integer.parseInt(cap) > 0) {
                        if (ProtoDaoConstant.ZERO_ADDRESS.equals(account)) {
                            designatedCap.setAccount(dao.getErc721Token());
                        }
                        nftDesignatedCaps.add(designatedCap);
                    }
                }
            }
        }

        DaoStrategy nftMinterStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.HIGH_PRIORITY_ERC721.getType());
        if (nftMinterStrategy == null) {
            nftMinterStrategy = new DaoStrategy();
            nftMinterStrategy.setType(DaoStrategyTypeEnum.MINT_WORK.getType());
            nftMinterStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.HIGH_PRIORITY_ERC721.getType());
            nftMinterStrategy.setProofId(dao.getId());
            nftMinterStrategy.setDaoUri(dao.getDaoUri());
            nftMinterStrategy.setDaoId(dao.getId());
            nftMinterStrategy.setProjectId(dao.getProjectId());
            nftMinterStrategy.setDaoNumber(dao.getDaoNumber());
        } else {
            log.info("[MintCapSetChainService] daoId:{} HIGH_PRIORITY_ERC721 origin daoStrategy:{}", dao.getId(),
                    JacksonUtil.obj2json(nftMinterStrategy));
        }

        nftMinterStrategy.setTransactionHash(transactionDto.getTransactionHash());
        nftMinterStrategy.setBlockTime(transactionDto.getBlockTime());

        if (nftDesignatedCaps != null && !nftDesignatedCaps.isEmpty()) {
            nftMinterStrategy.setOriginAddress(JacksonUtil.obj2json(nftDesignatedCaps));
            if (dao.getErc721MintCap() == null || dao.getErc721MintCap() == 0) {
                dao1.setId(dao.getId());
                dao1.setErc721MintCap(1);
            }
        } else {
            nftMinterStrategy.setOriginAddress(null);
            if (dao.getErc721MintCap() != null && dao.getErc721MintCap() == 1) {
                dao1.setId(dao.getId());
                dao1.setErc721MintCap(0);
            }
        }

        daoStrategyList.add(nftMinterStrategy);


        //  处理erc721下nft的白名单
        List<DesignatedNftCap> nftDesignatedIdCaps = null;
        if (StringUtils.isNotBlank(nftMinterCapIdSize) && Integer.parseInt(nftMinterCapIdSize) > 0) {
            int nftMinterIdCap = Integer.parseInt(nftMinterCapIdSize);
            nftDesignatedIdCaps = new ArrayList<>();
            for (int i = 0; i < nftMinterIdCap; i++) {
                DesignatedNftCap designatedCap = new DesignatedNftCap();
                String account = CommonUtil
                        .addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(3 * i + nftMinterCapIdCount + 1))).toLowerCase();
                System.out.println("nftMinterCapIdSize account:" + account);
                designatedCap.setNftAddress(account);

                String tokenId = CommonUtil.hexToTenString(dataList.get(3 * i + nftMinterCapIdCount + 2));
                System.out.println("nftMinterCapIdSize tokenId:" + tokenId);
                if (StringUtils.isNotBlank(tokenId)) {
                    designatedCap.setTokenId(Integer.parseInt(tokenId));
                }

                String cap = CommonUtil.hexToTenString(dataList.get(3 * i + nftMinterCapIdCount + 3));
                System.out.println("nftMinterCapIdSize cap:" + cap);
                if (StringUtils.isNotBlank(cap)) {
                    designatedCap.setNftMintCap(Integer.parseInt(cap));
                    if (Integer.parseInt(cap) > 0) {
                        nftDesignatedIdCaps.add(designatedCap);
                    }
                }
            }
        }
        log.info("nftDesignatedIdCaps:" + JacksonUtil.obj2json(nftDesignatedIdCaps));

        DaoStrategy nftMinterIdStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.HIGH_PRIORITY_ERC721_NFT.getType());
        if (nftMinterIdStrategy == null) {
            nftMinterIdStrategy = new DaoStrategy();
            nftMinterIdStrategy.setType(DaoStrategyTypeEnum.MINT_WORK.getType());
            nftMinterIdStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.HIGH_PRIORITY_ERC721_NFT.getType());
            nftMinterIdStrategy.setProofId(dao.getId());
            nftMinterIdStrategy.setDaoUri(dao.getDaoUri());
            nftMinterIdStrategy.setDaoId(dao.getId());
            nftMinterIdStrategy.setProjectId(dao.getProjectId());
            nftMinterIdStrategy.setDaoNumber(dao.getDaoNumber());
        } else {
            log.info("[MintCapSetChainService] daoId:{} HIGH_PRIORITY_ERC721 origin daoStrategy:{}", dao.getId(),
                    JacksonUtil.obj2json(nftMinterIdStrategy));
        }

        nftMinterIdStrategy.setTransactionHash(transactionDto.getTransactionHash());
        nftMinterIdStrategy.setBlockTime(transactionDto.getBlockTime());

        if (nftDesignatedIdCaps != null && !nftDesignatedIdCaps.isEmpty()) {
            nftMinterIdStrategy.setOriginAddress(JacksonUtil.obj2json(nftDesignatedIdCaps));
            if (dao.getErc721MintCapId() == null || dao.getErc721MintCapId() == 0) {
                dao1.setId(dao.getId());
                dao1.setErc721MintCapId(1);
            }
        } else {
            nftMinterIdStrategy.setOriginAddress(null);
            if (dao.getErc721MintCapId() != null && dao.getErc721MintCapId() == 1) {
                dao1.setId(dao.getId());
                dao1.setErc721MintCapId(0);
            }
        }

        daoStrategyList.add(nftMinterIdStrategy);


        if (StringUtils.isNotBlank(mintCap)) {
            dao1.setId(dao.getId());
            dao1.setGlobalMintCap(Integer.parseInt(mintCap));
        }
        if (dao1.getId() == null) {
            dao1 = null;
        }
        log.info("[MintCapSetChainService] dao:{} daoStrategyList:{}", JacksonUtil.obj2json(dao1),
                JacksonUtil.obj2json(daoStrategyList));
        int i = daoStrategyService.saveDaoStrategyListOrUpdateDao(daoStrategyList, dao1);
        log.info("[MintCapSetChainService] daoId:{} return i:{}", dao.getId(), i);

    }

}
