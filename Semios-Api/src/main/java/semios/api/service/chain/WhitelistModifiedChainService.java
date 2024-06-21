package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.chain.NftIdentifier;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoStrategy;
import semios.api.model.entity.WhiteList;
import semios.api.model.enums.DaoStrategyStrategyTypeEnum;
import semios.api.model.enums.DaoStrategyTypeEnum;
import semios.api.model.enums.DaoWhiteListEnum;
import semios.api.service.IDaoService;
import semios.api.service.IDaoStrategyService;
import semios.api.service.IWhiteListService;
import semios.api.service.SubscriberChainService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;
import semios.api.utils.merkle.MerkleTree;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * 白名单修改
 *
 * @description: 白名单修改
 * @author: xiangbin
 * @create: 2022-08-25 13:43
 **/
@Slf4j
@Service
public class WhitelistModifiedChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    @Autowired
    private IDaoStrategyService daoStrategyService;

    @Autowired
    private IWhiteListService whiteListService;

    public static void main(String[] args) {
//        String data = CommonUtil.removeHexPrefixIfExists(
//                "0x000000000000000000000000000000000000000000000000000000000000002007caf22504ccaa17d1850607ba4f04b45c08f51515036b11ee8691851f6b27eb00000000000000000000000000000000000000000000000000000000000000805e91d4991d7dbc5d8a6261b10bb674ee46a9e591a1ef30a33b096fb1be425f3e00000000000000000000000000000000000000000000000000000000000000e00000000000000000000000000000000000000000000000000000000000000002000000000000000000000000aa2ad3486c0c6d9089e6dde6ec10d2f56c8c8893000000000000000000000000917ef5060f560f02b8bb93173da8b36508d4c3030000000000000000000000000000000000000000000000000000000000000002000000000000000000000000f3d2154b09f35713786c00c031ab760caec7a1c40000000000000000000000004ba4dd89d69a7a2145fb8c8b0199cca2a6bedf4e");
//        List<String> dataList = CommonUtil.splitBy32Bytes(data);
//        String minterMerkleRoot = dataList.get(0);
//        String minterNFTHolderPass =
//                CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1))).toLowerCase();
//        String canvasCreatorMerkleRoot = dataList.get(2);
//        String canvasCreatorNFTHolderPass =
//                CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(3))).toLowerCase();
//
//        System.out.println(minterMerkleRoot);
//        System.out.println(minterNFTHolderPass);
//        System.out.println(canvasCreatorMerkleRoot);
//        System.out.println(canvasCreatorNFTHolderPass);
//
//        String origin =
//                "[\"0x75F7A0B820f7AaF559A0324192EF2F9fE85585D0\",\"0x1111111111111111111111111111111111110007\"]";
//        System.out.println(origin.substring(1, origin.length() - 1).replaceAll("\"", ""));

        String data = CommonUtil.removeHexPrefixIfExists("0x0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000000e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001400000000000000000000000000000000000000000000000000000000000000160000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000000000000000000000007af09a11cda4e696006a950309fe371d52b801d70000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000000000000000000000007af09a11cda4e696006a950309fe371d52b801d70000000000000000000000000000000000000000000000000000000000000003");
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String minterMerkleRoot = CommonUtil.addHexPrefixIfNotExist(dataList.get(1));
        log.info("minterMerkleRoot:" + minterMerkleRoot);
        String canvasCreatorMerkleRoot = CommonUtil.addHexPrefixIfNotExist(dataList.get(4));
        log.info("canvasCreatorMerkleRoot:" + canvasCreatorMerkleRoot);


        List<String> minterNFTHolderPass = new ArrayList<>();
        List<String> canvasCreatorNFTHolderPass = new ArrayList<>();
        List<NftIdentifier> minterNFTIdHolderPass = new ArrayList<>();
        List<NftIdentifier> canvasCreatorNFTIdHolderPass = new ArrayList<>();

        // 计算 minterNFTHolderPass
        int minterNFTIndex = 7; // 开始计算list数据
        String minterNFTSize = CommonUtil.hexToTenString(dataList.get(minterNFTIndex));
        int minterNFTSizeInt = Integer.parseInt(minterNFTSize);
        if (minterNFTSizeInt > 0) {
            for (int i = 0; i < minterNFTSizeInt; i++) {
                minterNFTHolderPass.add(CommonUtil
                        .addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(i + 8))).toLowerCase());
            }
        }
        log.info("minterNFTHolderPass:" + JacksonUtil.obj2json(minterNFTHolderPass));

        // 计算 minterNFTIdHolderPass
        int minterNFTIdIndex = minterNFTIndex + minterNFTSizeInt + 1;
        String minterNFTIdSize = CommonUtil.hexToTenString(dataList.get(minterNFTIdIndex));
        int minterNFTIdSizeInt = Integer.parseInt(minterNFTIdSize);
        if (minterNFTIdSizeInt > 0) {
            for (int i = 0; i < minterNFTIdSizeInt; i++) {
                NftIdentifier nftIdentifier = new NftIdentifier();
                nftIdentifier.setErc721Address(CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2 * i + 1 + minterNFTIdIndex))));
                nftIdentifier.setTokenId(Integer.parseInt(dataList.get(2 * i + 2 + minterNFTIdIndex), 16));
                minterNFTIdHolderPass.add(nftIdentifier);
            }
        }
        log.info("minterNFTIdHolderPass:" + JacksonUtil.obj2json(minterNFTIdHolderPass));


        // 计算 canvasCreatorNFTHolderPass
        int canvasCreatorNFTIndex = minterNFTIdIndex + minterNFTIdSizeInt * 2 + 1;
        String canvasCreatorNFTSize = CommonUtil.hexToTenString(dataList.get(canvasCreatorNFTIndex));
        int canvasCreatorNFTSizeInt = Integer.parseInt(canvasCreatorNFTSize);
        if (canvasCreatorNFTSizeInt > 0) {
            for (int i = 1; i <= canvasCreatorNFTSizeInt; i++) {
                canvasCreatorNFTHolderPass
                        .add(CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(i + canvasCreatorNFTIndex)))
                                .toLowerCase());
            }
        }
        log.info("canvasCreatorNFTHolderPass:" + JacksonUtil.obj2json(canvasCreatorNFTHolderPass));


        // 计算 canvasCreatorNFTIdHolderPass
        int canvasCreatorNFTIdIndex = canvasCreatorNFTIndex + canvasCreatorNFTSizeInt + 1;
        String canvasCreatorNFTIdSize = CommonUtil.hexToTenString(dataList.get(canvasCreatorNFTIdIndex));
        Integer canvasCreatorNFTIdSizeInt = Integer.parseInt(canvasCreatorNFTIdSize);
        log.info("canvasCreatorNFTIdSizeInt:" + canvasCreatorNFTIdSizeInt);
        if (canvasCreatorNFTIdSizeInt > 0) {
            for (int i = 0; i < canvasCreatorNFTIdSizeInt; i++) {
                NftIdentifier nftIdentifier = new NftIdentifier();
                nftIdentifier.setErc721Address(CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2 * i + 1 + canvasCreatorNFTIdIndex))));
                nftIdentifier.setTokenId(Integer.parseInt(dataList.get(2 * i + 2 + canvasCreatorNFTIdIndex), 16));
                canvasCreatorNFTIdHolderPass.add(nftIdentifier);
            }
        }
        log.info("canvasCreatorNFTIdHolderPass:" + JacksonUtil.obj2json(canvasCreatorNFTIdHolderPass));

//        0x0000000000000000000000000000000000000000000000000000000000000000,
//        0x0000000000000000000000000000000000000000000000000000000000000000,

//        [0x9a89c88F4a22D0DB9427B0ea553264044cfEE757],
//        [(0x9a89c88F4a22D0DB9427B0ea553264044cfEE757, 2323), (0x9a89c88F4a22D0DB9427B0ea553264044cfEE757, 2322)],
//        [0x9a89c88F4a22D0DB9427B0ea553264044cfEE757],
//        [(0x9a89c88F4a22D0DB9427B0ea553264044cfEE757, 1), (0x9a89c88F4a22D0DB9427B0ea553264044cfEE757, 2)]
    }

    /**
     * // 白名单，含 minter 和 canvas creator 对应的 merkle root 和 NFTHolderPass struct Whitelist { bytes32 minterMerkleRoot;
     * address minterNFTHolderPass; bytes32 canvasCreatorMerkleRoot; address canvasCreatorNFTHolderPass; }
     *
     * @param transactionDto
     * @throws Exception
     */
    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[WhitelistModifiedChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());
        String daoId = CommonUtil.removeHexPrefixIfExists(topics.get(1));

        Dao dao = daoService.daoDetailByProjectId(daoId);
        if (dao == null) {
            throw new RuntimeException("WhitelistModifiedChainService cannot find dao");
        }

        Dao dao1 = new Dao();
        dao1.setId(dao.getId());
        dao1.setCanvasCreatedWhitelist(dao.getCanvasCreatedWhitelist());
        dao1.setMinterWorksWhitelist(dao.getMinterWorksWhitelist());
        dao1.setCanvasCreatedWhitelistNft(dao1.getCanvasCreatedWhitelistNft());

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String minterMerkleRoot = CommonUtil.addHexPrefixIfNotExist(dataList.get(1));
        log.info("minterMerkleRoot:" + minterMerkleRoot);
        String canvasCreatorMerkleRoot = CommonUtil.addHexPrefixIfNotExist(dataList.get(4));
        log.info("canvasCreatorMerkleRoot:" + canvasCreatorMerkleRoot);


        List<String> minterNFTHolderPass = new ArrayList<>();
        List<String> canvasCreatorNFTHolderPass = new ArrayList<>();
        List<NftIdentifier> minterNFTIdHolderPass = new ArrayList<>();
        List<NftIdentifier> canvasCreatorNFTIdHolderPass = new ArrayList<>();

        // 计算 minterNFTHolderPass
        int minterNFTIndex = 7; // 开始计算list数据
        String minterNFTSize = CommonUtil.hexToTenString(dataList.get(minterNFTIndex));
        int minterNFTSizeInt = Integer.parseInt(minterNFTSize);
        if (minterNFTSizeInt > 0) {
            for (int i = 0; i < minterNFTSizeInt; i++) {
                minterNFTHolderPass.add(CommonUtil
                        .addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(i + 8))).toLowerCase());
            }
        }
        log.info("minterNFTIdHolderPass:" + JacksonUtil.obj2json(minterNFTIdHolderPass));

        //  计算 minterNFTIdHolderPass
        int minterNFTIdIndex = minterNFTIndex + minterNFTSizeInt + 1;
        String minterNFTIdSize = CommonUtil.hexToTenString(dataList.get(minterNFTIdIndex));
        int minterNFTIdSizeInt = Integer.parseInt(minterNFTIdSize);
        if (minterNFTIdSizeInt > 0) {
            for (int i = 0; i < minterNFTIdSizeInt; i++) {
                NftIdentifier nftIdentifier = new NftIdentifier();
                nftIdentifier.setErc721Address(CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2 * i + 1 + minterNFTIdIndex))));
                nftIdentifier.setTokenId(Integer.parseInt(dataList.get(2 * i + 2 + minterNFTIdIndex), 16));
                minterNFTIdHolderPass.add(nftIdentifier);
            }
        }
        log.info("minterNFTIdHolderPass:" + JacksonUtil.obj2json(minterNFTIdHolderPass));


        // 计算 canvasCreatorNFTHolderPass
        int canvasCreatorNFTIndex = minterNFTIdIndex + minterNFTIdSizeInt * 2 + 1;
        String canvasCreatorNFTSize = CommonUtil.hexToTenString(dataList.get(canvasCreatorNFTIndex));
        int canvasCreatorNFTSizeInt = Integer.parseInt(canvasCreatorNFTSize);
        if (canvasCreatorNFTSizeInt > 0) {
            for (int i = 1; i <= canvasCreatorNFTSizeInt; i++) {
                canvasCreatorNFTHolderPass
                        .add(CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(i + canvasCreatorNFTIndex)))
                                .toLowerCase());
            }
        }
        log.info("canvasCreatorNFTHolderPass:" + JacksonUtil.obj2json(canvasCreatorNFTHolderPass));


        //  添加 builder(canvas creator)针对nft的白名单 计算 canvasCreatorNFTIdHolderPass
        int canvasCreatorNFTIdIndex = canvasCreatorNFTIndex + canvasCreatorNFTSizeInt + 1;
        String canvasCreatorNFTIdSize = CommonUtil.hexToTenString(dataList.get(canvasCreatorNFTIdIndex));
        Integer canvasCreatorNFTIdSizeInt = Integer.parseInt(canvasCreatorNFTIdSize);
        if (canvasCreatorNFTIdSizeInt > 0) {
            for (int i = 0; i < canvasCreatorNFTIdSizeInt; i++) {
                NftIdentifier nftIdentifier = new NftIdentifier();
                nftIdentifier.setErc721Address(CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2 * i + 1 + canvasCreatorNFTIdIndex))));
                nftIdentifier.setTokenId(Integer.parseInt(dataList.get(2 * i + 2 + canvasCreatorNFTIdIndex), 16));
                canvasCreatorNFTIdHolderPass.add(nftIdentifier);
            }
        }
        log.info("canvasCreatorNFTIdHolderPass:" + JacksonUtil.obj2json(canvasCreatorNFTIdHolderPass));


        // String minterNFTHolderPass =
        // CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1))).toLowerCase();
        // String canvasCreatorNFTHolderPass =
        // CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(3))).toLowerCase();

        List<DaoStrategy> daoStrategyList = new ArrayList<>();

        log.info(
                "[WhitelistModifiedChainService] daoId:{} minterMerkleRoot:{} minterNFTHolderPass:{} canvasCreatorMerkleRoot:{} canvasCreatorNFTHolderPass:{}",
                dao.getId(), minterMerkleRoot, JacksonUtil.obj2json(minterNFTHolderPass), canvasCreatorMerkleRoot,
                JacksonUtil.obj2json(canvasCreatorNFTHolderPass));
        if (StringUtils.isNotBlank(minterMerkleRoot)) {
            if (minterMerkleRoot.equals(ProtoDaoConstant.ZERO_MERKLE_ROOT)) {
                if (DaoWhiteListEnum.ADDRESS.getStatus().equals(dao1.getMinterWorksWhitelist())
                        || DaoWhiteListEnum.ADDRESS_ERC721.getStatus().equals(dao1.getMinterWorksWhitelist())) {
                    dao1.setMinterWorksWhitelist(dao1.getMinterWorksWhitelist() - 1);
                    // daoStrategyService.saveDaoStrategyOrUpdateDao(null, dao);
                }
            } else {
                // 查询merkleRoot
                WhiteList whiteList = whiteListService.selectByAddressAndRoot(dao.getOwnerAddress(), minterMerkleRoot);
                if (whiteList == null) {
                    if (transactionDto.getTransactionHash().equals(dao.getTransactionHash())) {
                        // 查询merkleRoot
                        whiteList = new WhiteList();
                        whiteList.setUserAddress(dao.getOwnerAddress());
                        String originAddress = dao.getOwnerAddress();
                        List<String> list = Arrays.asList(originAddress);
                        MerkleTree mt = new MerkleTree(list);
                        mt.init();
                        String rootHash = mt.getRootHash();
                        if (!rootHash.equals(minterMerkleRoot.toLowerCase())) {
                            throw new RuntimeException(
                                    "WhitelistModifiedChainService diff root hash, front:" + minterMerkleRoot + ", end:" + rootHash);
                        }
                        // mintingWhiteList.setOriginAddress(daoWhiteListReqVo.getMintingOriginAddress());
                        whiteList.setOriginAddress(JacksonUtil.obj2json(list));
                        whiteList.setProof(JacksonUtil.obj2json(mt));
                        whiteList.setProofRootHash(minterMerkleRoot);
                        whiteListService.save(whiteList);
                    } else {
                        if (StringUtils.isNotBlank(dao.getExistDaoId())) {
                            Dao existedDao = daoService.daoDetailByProjectId(dao.getExistDaoId());
                            if (existedDao != null) {
                                whiteList = whiteListService.selectByAddressAndRoot(existedDao.getOwnerAddress(), canvasCreatorMerkleRoot);
                            }
                        }
                        if (whiteList == null) {
                            throw new RuntimeException("WhitelistModifiedChainService cannot find whiteList");
                        }
                    }

                }

                DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                        DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.WHITE_LIST.getType());
                if (daoStrategy == null) {
                    daoStrategy = new DaoStrategy();
                    daoStrategy.setProofId(whiteList.getId());
                    daoStrategy.setType(DaoStrategyTypeEnum.MINT_WORK.getType());
                    daoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.WHITE_LIST.getType());
                    daoStrategy.setOriginAddress(whiteList.getOriginAddress()
                            .substring(1, whiteList.getOriginAddress().length() - 1).replaceAll("\"", ""));
                    daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
                    daoStrategy.setBlockTime(transactionDto.getBlockTime());
                    daoStrategy.setDaoUri(dao.getDaoUri());
                    daoStrategy.setDaoId(dao.getId());
                    daoStrategy.setProjectId(dao.getProjectId());
                    daoStrategy.setDaoNumber(dao.getDaoNumber());

                } else {
                    daoStrategy.setOriginAddress(whiteList.getOriginAddress()
                            .substring(1, whiteList.getOriginAddress().length() - 1).replaceAll("\"", ""));
                    daoStrategy.setProofId(whiteList.getId());
                    daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
                    daoStrategy.setBlockTime(transactionDto.getBlockTime());
                }

                if (DaoWhiteListEnum.CLOSE.getStatus().equals(dao1.getMinterWorksWhitelist())) {
                    dao1.setMinterWorksWhitelist(DaoWhiteListEnum.ADDRESS.getStatus());
                }

                if (DaoWhiteListEnum.ERC721.getStatus().equals(dao1.getMinterWorksWhitelist())) {
                    dao1.setMinterWorksWhitelist(DaoWhiteListEnum.ADDRESS_ERC721.getStatus());
                }

                daoStrategyList.add(daoStrategy);
                // daoStrategyService.saveDaoStrategyOrUpdateDao(daoStrategy, dao);
            }
        }

        if (minterNFTHolderPass.isEmpty()) {
            // if (minterNFTHolderPass.get(0).equals(Dao4ArtConstant.ZERO_ADDRESS)) {
            if (DaoWhiteListEnum.ERC721.getStatus().equals(dao1.getMinterWorksWhitelist())
                    || DaoWhiteListEnum.ADDRESS_ERC721.getStatus().equals(dao1.getMinterWorksWhitelist())) {
                dao1.setMinterWorksWhitelist(dao1.getMinterWorksWhitelist() - 2);
                // daoStrategyService.saveDaoStrategyOrUpdateDao(null, dao);
            }
        } else {
            DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                    DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.ERC721.getType());
            if (daoStrategy == null) {
                daoStrategy = new DaoStrategy();
                daoStrategy.setType(DaoStrategyTypeEnum.MINT_WORK.getType());
                daoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.ERC721.getType());
                daoStrategy.setOriginAddress(JacksonUtil.obj2json(minterNFTHolderPass));
                daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
                daoStrategy.setBlockTime(transactionDto.getBlockTime());
                daoStrategy.setDaoUri(dao.getDaoUri());
                daoStrategy.setDaoId(dao.getId());
                daoStrategy.setProjectId(dao.getProjectId());
                daoStrategy.setDaoNumber(dao.getDaoNumber());

            } else {
                daoStrategy.setOriginAddress(JacksonUtil.obj2json(minterNFTHolderPass));
                daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
                daoStrategy.setBlockTime(transactionDto.getBlockTime());
            }

            if (DaoWhiteListEnum.CLOSE.getStatus().equals(dao1.getMinterWorksWhitelist())) {
                dao1.setMinterWorksWhitelist(DaoWhiteListEnum.ERC721.getStatus());
            }

            if (DaoWhiteListEnum.ADDRESS.getStatus().equals(dao1.getMinterWorksWhitelist())) {
                dao1.setMinterWorksWhitelist(DaoWhiteListEnum.ADDRESS_ERC721.getStatus());
            }

            daoStrategyList.add(daoStrategy);
            // daoStrategyService.saveDaoStrategyOrUpdateDao(daoStrategy, dao);
        }


        //  实现存储 minterNFTIdHolderPass
        if (minterNFTIdHolderPass.isEmpty()) {
            dao1.setMinterWorksWhitelistNft(0);
        } else {
            DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                    DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.ERC721_NFT.getType());
            if (daoStrategy == null) {
                daoStrategy = new DaoStrategy();
                daoStrategy.setType(DaoStrategyTypeEnum.MINT_WORK.getType());
                daoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.ERC721_NFT.getType());
                daoStrategy.setOriginAddress(JacksonUtil.obj2json(minterNFTIdHolderPass));
                daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
                daoStrategy.setBlockTime(transactionDto.getBlockTime());
                daoStrategy.setDaoUri(dao.getDaoUri());
                daoStrategy.setDaoId(dao.getId());
                daoStrategy.setProjectId(dao.getProjectId());
                daoStrategy.setDaoNumber(dao.getDaoNumber());
            } else {
                daoStrategy.setOriginAddress(JacksonUtil.obj2json(minterNFTIdHolderPass));
                daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
                daoStrategy.setBlockTime(transactionDto.getBlockTime());
            }
            dao1.setMinterWorksWhitelistNft(1);
            daoStrategyList.add(daoStrategy);
        }


        if (StringUtils.isNotBlank(canvasCreatorMerkleRoot)) {
            if (canvasCreatorMerkleRoot.equals(ProtoDaoConstant.ZERO_MERKLE_ROOT)) {
                if (DaoWhiteListEnum.ADDRESS.getStatus().equals(dao1.getCanvasCreatedWhitelist())
                        || DaoWhiteListEnum.ADDRESS_ERC721.getStatus().equals(dao1.getCanvasCreatedWhitelist())) {
                    dao1.setCanvasCreatedWhitelist(dao1.getCanvasCreatedWhitelist() - 1);
                    // daoStrategyService.saveDaoStrategyOrUpdateDao(null, dao);
                }
            } else {
                // 查询merkleRoot
                WhiteList whiteList =
                        whiteListService.selectByAddressAndRoot(dao.getOwnerAddress(), canvasCreatorMerkleRoot);
                if (whiteList == null) {
                    if (StringUtils.isNotBlank(dao.getExistDaoId())) {
                        Dao existedDao = daoService.daoDetailByProjectId(dao.getExistDaoId());
                        if (existedDao != null) {
                            whiteList = whiteListService.selectByAddressAndRoot(existedDao.getOwnerAddress(), canvasCreatorMerkleRoot);
                        }
                    }

                    if (whiteList == null) {
                        throw new RuntimeException("WhitelistModifiedChainService cannot find whiteList");
                    }
                }

                DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                        DaoStrategyTypeEnum.CREATE_CANVAS.getType(), DaoStrategyStrategyTypeEnum.WHITE_LIST.getType());
                if (daoStrategy == null) {
                    daoStrategy = new DaoStrategy();
                    daoStrategy.setProofId(whiteList.getId());
                    daoStrategy.setType(DaoStrategyTypeEnum.CREATE_CANVAS.getType());
                    daoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.WHITE_LIST.getType());
                    daoStrategy.setOriginAddress(whiteList.getOriginAddress()
                            .substring(1, whiteList.getOriginAddress().length() - 1).replaceAll("\"", ""));
                    daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
                    daoStrategy.setBlockTime(transactionDto.getBlockTime());
                    daoStrategy.setDaoUri(dao.getDaoUri());
                    daoStrategy.setDaoId(dao.getId());
                    daoStrategy.setProjectId(dao.getProjectId());
                    daoStrategy.setDaoNumber(dao.getDaoNumber());

                } else {
                    daoStrategy.setProofId(whiteList.getId());
                    daoStrategy.setOriginAddress(whiteList.getOriginAddress()
                            .substring(1, whiteList.getOriginAddress().length() - 1).replaceAll("\"", ""));
                    daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
                    daoStrategy.setBlockTime(transactionDto.getBlockTime());
                }

                if (DaoWhiteListEnum.CLOSE.getStatus().equals(dao1.getCanvasCreatedWhitelist())) {
                    dao1.setCanvasCreatedWhitelist(DaoWhiteListEnum.ADDRESS.getStatus());
                }

                if (DaoWhiteListEnum.ERC721.getStatus().equals(dao1.getCanvasCreatedWhitelist())) {
                    dao1.setCanvasCreatedWhitelist(DaoWhiteListEnum.ADDRESS_ERC721.getStatus());
                }

                daoStrategyList.add(daoStrategy);
                // daoStrategyService.saveDaoStrategyOrUpdateDao(daoStrategy, dao);
            }
        }

        if (canvasCreatorNFTHolderPass.isEmpty()) {
            // if (canvasCreatorNFTHolderPass.equals(Dao4ArtConstant.ZERO_ADDRESS)) {
            if (DaoWhiteListEnum.ERC721.getStatus().equals(dao1.getCanvasCreatedWhitelist())
                    || DaoWhiteListEnum.ADDRESS_ERC721.getStatus().equals(dao1.getCanvasCreatedWhitelist())) {
                dao1.setCanvasCreatedWhitelist(dao1.getCanvasCreatedWhitelist() - 2);
                // daoStrategyService.saveDaoStrategyOrUpdateDao(null, dao);
            }
        } else {
            DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                    DaoStrategyTypeEnum.CREATE_CANVAS.getType(), DaoStrategyStrategyTypeEnum.ERC721.getType());
            if (daoStrategy == null) {
                daoStrategy = new DaoStrategy();
                daoStrategy.setType(DaoStrategyTypeEnum.CREATE_CANVAS.getType());
                daoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.ERC721.getType());
                daoStrategy.setOriginAddress(JacksonUtil.obj2json(canvasCreatorNFTHolderPass));
                daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
                daoStrategy.setBlockTime(transactionDto.getBlockTime());
                daoStrategy.setDaoUri(dao.getDaoUri());
                daoStrategy.setDaoId(dao.getId());
                daoStrategy.setProjectId(dao.getProjectId());
                daoStrategy.setDaoNumber(dao.getDaoNumber());

            } else {
                daoStrategy.setOriginAddress(JacksonUtil.obj2json(canvasCreatorNFTHolderPass));
                daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
                daoStrategy.setBlockTime(transactionDto.getBlockTime());
            }

            if (DaoWhiteListEnum.CLOSE.getStatus().equals(dao1.getCanvasCreatedWhitelist())) {
                dao1.setCanvasCreatedWhitelist(DaoWhiteListEnum.ERC721.getStatus());
            }

            if (DaoWhiteListEnum.ADDRESS.getStatus().equals(dao1.getCanvasCreatedWhitelist())) {
                dao1.setCanvasCreatedWhitelist(DaoWhiteListEnum.ADDRESS_ERC721.getStatus());
            }

            daoStrategyList.add(daoStrategy);
            // daoStrategyService.saveDaoStrategyOrUpdateDao(daoStrategy, dao);
        }


        //  实现存储 canvasCreatorNFTIdHolderPass
        if (canvasCreatorNFTIdHolderPass.isEmpty()) {
            dao1.setCanvasCreatedWhitelistNft(0);
        } else {
            DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                    DaoStrategyTypeEnum.CREATE_CANVAS.getType(), DaoStrategyStrategyTypeEnum.ERC721_NFT.getType());
            if (daoStrategy == null) {
                daoStrategy = new DaoStrategy();
                daoStrategy.setType(DaoStrategyTypeEnum.CREATE_CANVAS.getType());
                daoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.ERC721_NFT.getType());
                daoStrategy.setOriginAddress(JacksonUtil.obj2json(canvasCreatorNFTIdHolderPass));
                daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
                daoStrategy.setBlockTime(transactionDto.getBlockTime());
                daoStrategy.setDaoUri(dao.getDaoUri());
                daoStrategy.setDaoId(dao.getId());
                daoStrategy.setProjectId(dao.getProjectId());
                daoStrategy.setDaoNumber(dao.getDaoNumber());
            } else {
                daoStrategy.setOriginAddress(JacksonUtil.obj2json(canvasCreatorNFTIdHolderPass));
                daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
                daoStrategy.setBlockTime(transactionDto.getBlockTime());
            }
            dao1.setCanvasCreatedWhitelistNft(1);

            daoStrategyList.add(daoStrategy);
        }


        log.info("[WhitelistModifiedChainService] dao:{} daoStrategyList:{}", JacksonUtil.obj2json(dao1),
                JacksonUtil.obj2json(daoStrategyList));
        int i = daoStrategyService.saveDaoStrategyListOrUpdateDao(daoStrategyList, dao1);
        log.info("[WhitelistModifiedChainService] daoId:{} return i:{}", dao.getId(), i);
    }

}
