package semios.api.model.vo.res.DaoExportInfo;

import lombok.Data;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import semios.api.model.dto.chain.DesignatedCap;
import semios.api.model.dto.chain.DesignatedNftCap;
import semios.api.model.dto.chain.NftIdentifier;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoStrategy;
import semios.api.model.enums.DaoStrategyStrategyTypeEnum;
import semios.api.model.enums.DaoStrategyTypeEnum;
import semios.api.service.IDaoStrategyService;
import semios.api.service.IWhiteListService;
import semios.api.utils.JacksonUtil;
import semios.api.utils.SpringBeanUtil;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

@Data
public class NodesStrategies {

    private static final Logger log = LoggerFactory.getLogger(NodesStrategies.class);
    /**
     * 创建canvas策略
     */
    private BuilderInfo builderInfo;

    /**
     * mint nft 策略
     */
    private MinterInfo minterInfo;

    public static NodesStrategies transferNodesStrategies(Dao dao) {
        NodesStrategies nodesStrategies = new NodesStrategies();

        IWhiteListService whiteListService = SpringBeanUtil.getBean(IWhiteListService.class);
        IDaoStrategyService daoStrategyService = SpringBeanUtil.getBean(IDaoStrategyService.class);
        if (whiteListService == null || daoStrategyService == null) {
            return nodesStrategies;
        }

        List<DaoStrategy> daoStrategyList = daoStrategyService.selectDaoStrategyByDaoId(dao.getId());
        if (daoStrategyList == null || daoStrategyList.isEmpty()) {
            return nodesStrategies;
        }

        List<DaoStrategy> builderStrategyList = daoStrategyList.stream().filter(daoStrategy ->
                daoStrategy.getType().equals(DaoStrategyTypeEnum.CREATE_CANVAS.getType())).collect(Collectors.toList());
        nodesStrategies.setBuilderInfo(transferBuilderInfo(builderStrategyList, dao));

        List<DaoStrategy> minterStrategyList = daoStrategyList.stream().filter(daoStrategy ->
                daoStrategy.getType().equals(DaoStrategyTypeEnum.MINT_WORK.getType())).collect(Collectors.toList());
        nodesStrategies.setMinterInfo(transferMinterInfo(minterStrategyList, dao));

        return nodesStrategies;
    }


    private static BuilderInfo transferBuilderInfo(List<DaoStrategy> builderStrategyList, Dao dao) {
        // BuilderInfo builderInfo
        BuilderInfo building = new BuilderInfo();

        // build white list
        if (dao.getCanvasCreatedWhitelist() == 1 || dao.getCanvasCreatedWhitelist() == 3) {
            builderStrategyList.stream().filter(daoStrategy ->
                    daoStrategy.getStrategyType().equals(DaoStrategyStrategyTypeEnum.WHITE_LIST.getType())).findFirst().ifPresent(daoStrategy ->
                    building.setWhiteList(Arrays.asList(daoStrategy.getOriginAddress().split(",")))
            );
        }


        // build black list
        if (dao.getCanvasCreatedBlacklist() == 1) {
            builderStrategyList.stream().filter(daoStrategy ->
                    daoStrategy.getStrategyType().equals(DaoStrategyStrategyTypeEnum.BLACK_LIST.getType())).findFirst().ifPresent(daoStrategy ->
                    building.setBlackList(Arrays.asList(daoStrategy.getOriginAddress().split(",")))
            );
        }

        //  build nft
        if (dao.getCanvasCreatedWhitelistNft() == 1) {
            builderStrategyList.stream().filter(daoStrategy ->
                    daoStrategy.getStrategyType().equals(DaoStrategyStrategyTypeEnum.ERC721_NFT.getType())).findFirst().ifPresent(daoStrategy ->
                    {
                        try {
                            List<NftIdentifier> erc721IdList;
                            erc721IdList = JacksonUtil.json2list(daoStrategy.getOriginAddress(), NftIdentifier.class);
                            building.setWhiteListedERC721Id(erc721IdList);
                        } catch (Exception e) {
                            log.info("transferNodesStrategies error:{}", e.getMessage());
                        }
                    }
            );
        }

        // build erc721 address
        if (dao.getCanvasCreatedWhitelist() == 2 || dao.getCanvasCreatedWhitelist() == 3) {
            builderStrategyList.stream().filter(daoStrategy ->
                    daoStrategy.getStrategyType().equals(DaoStrategyStrategyTypeEnum.ERC721.getType())).findFirst().ifPresent(daoStrategy ->
                    {
                        List<String> erc721List;
                        if (daoStrategy.getOriginAddress().contains("[")) {
                            erc721List = JacksonUtil.json2StringList(daoStrategy.getOriginAddress());
                        } else {
                            erc721List = Collections.singletonList(daoStrategy.getOriginAddress());
                        }
                        if (erc721List != null) {
                            building.setWhiteListedERC721(erc721List);
                        }
                    }
            );
        }


        return building;
    }


    private static MinterInfo transferMinterInfo(List<DaoStrategy> minterStrategyList, Dao dao) {
        MinterInfo minting = new MinterInfo();

        // mint black list
        if (dao.getMinterWorksBlacklist() == 1) {
            minterStrategyList.stream().filter(daoStrategy ->
                    daoStrategy.getStrategyType().equals(DaoStrategyStrategyTypeEnum.BLACK_LIST.getType())).findFirst().ifPresent(daoStrategy ->
                    minting.setBlackList(Arrays.asList(daoStrategy.getOriginAddress().split(",")))
            );
        }

        // mint white list
        if (dao.getMinterWorksWhitelist() == 1 || dao.getMinterWorksWhitelist() == 3) {
            minterStrategyList.stream().filter(daoStrategy ->
                    daoStrategy.getStrategyType().equals(DaoStrategyStrategyTypeEnum.WHITE_LIST.getType())).findFirst().ifPresent(daoStrategy ->
                    minting.setWhiteListAddress(Arrays.asList(daoStrategy.getOriginAddress().split(",")))
            );
        }

        // mint erc721 address
        if (dao.getMinterWorksWhitelist() == 2 || dao.getMinterWorksWhitelist() == 3) {
            minterStrategyList.stream().filter(daoStrategy ->
                    daoStrategy.getStrategyType().equals(DaoStrategyStrategyTypeEnum.ERC721.getType())).findFirst().ifPresent(daoStrategy ->
                    {
                        List<String> erc721List;
                        if (daoStrategy.getOriginAddress().contains("[")) {
                            erc721List = JacksonUtil.json2StringList(daoStrategy.getOriginAddress());
                        } else {
                            erc721List = Collections.singletonList(daoStrategy.getOriginAddress());
                        }
                        if (erc721List != null) {
                            minting.setWhiteListedERC721(erc721List);
                        }
                    }
            );
        }

        // minter nft
        if (dao.getMinterWorksWhitelistNft() == 1) {
            minterStrategyList.stream().filter(daoStrategy ->
                    daoStrategy.getStrategyType().equals(DaoStrategyStrategyTypeEnum.ERC721_NFT.getType())).findFirst().ifPresent(daoStrategy ->
                    {
                        try {
                            List<NftIdentifier> erc721IdList;
                            erc721IdList = JacksonUtil.json2list(daoStrategy.getOriginAddress(), NftIdentifier.class);
                            minting.setWhiteListedERC721Id(erc721IdList);
                        } catch (Exception e) {
                            log.info("transferMinterInfo nft error:{}", e.getMessage());
                        }

                    }
            );
        }

        // Whitelist Address With Max Minting
        // 高优白名单信息 用户地址
        if (dao.getMintCap() != null && dao.getMintCap() == 1) {
            minterStrategyList.stream().filter(daoStrategy ->
                    daoStrategy.getStrategyType().equals(DaoStrategyStrategyTypeEnum.HIGH_PRIORITY.getType())).findFirst().ifPresent(daoStrategy ->
                    {
                        try {
                            minting.setDesignatedMintCaps(
                                    JacksonUtil.json2list(daoStrategy.getOriginAddress(), DesignatedCap.class));
                        } catch (Exception e) {
                            log.info("transferMinterInfo address mint cap error:{}", e.getMessage());
                        }
                    }
            );
        }

        // 高优白名单信息 erc721地址
        if (dao.getErc721MintCap() != null && dao.getErc721MintCap() == 1) {
            minterStrategyList.stream().filter(daoStrategy ->
                    daoStrategy.getStrategyType().equals(DaoStrategyStrategyTypeEnum.HIGH_PRIORITY_ERC721.getType())).findFirst().ifPresent(daoStrategy ->
                    {
                        try {
                            minting.setErc721MintCaps(
                                    JacksonUtil.json2list(daoStrategy.getOriginAddress(), DesignatedCap.class));
                        } catch (Exception e) {
                            log.info("transferMinterInfo erc721 mint cap error:{}", e.getMessage());
                        }
                    }
            );
        }

        // 高优白名单信息 erc721下的nft地址
        if (dao.getErc721MintCapId() != null && dao.getErc721MintCapId() == 1) {
            minterStrategyList.stream().filter(daoStrategy ->
                    daoStrategy.getStrategyType().equals(DaoStrategyStrategyTypeEnum.HIGH_PRIORITY_ERC721_NFT.getType())).findFirst().ifPresent(daoStrategy ->
                    {
                        try {
                            minting.setErc721MintIdCaps(
                                    JacksonUtil.json2list(daoStrategy.getOriginAddress(), DesignatedNftCap.class));
                        } catch (Exception e) {
                            log.info("transferMinterInfo erc721 mint id cap error:{}", e.getMessage());
                        }
                    }
            );
        }

        if (dao.getGlobalMintCap() != null) {
            minting.setMaxMintingAmount(dao.getGlobalMintCap());
        }

        return minting;
    }

    @Data
    public static class BuilderInfo {

        /**
         * 白名单列表-Whitelist Address
         */
        private List<String> whiteList;

        /**
         * 黑名单列表-Blacklist Address
         */
        private List<String> blackList;

        /**
         * Whitelisted NFT
         */
        private List<NftIdentifier> whiteListedERC721Id = new ArrayList<>();

        /**
         * erc721地址
         */
        private List<String> whiteListedERC721 = new ArrayList<>();
    }

    @Data
    public static class MinterInfo {

        /**
         * 黑名单列表-Blacklist Address
         */
        private List<String> blackList;


        /**
         * Whitelist Address With Max Minting
         * 高优白名单信息 用户地址
         */
        private List<DesignatedCap> designatedMintCaps = new ArrayList<>();

        /**
         * 无限铸造白名单-Whitelist Address With Unlimited Minting
         */
        private List<String> whiteListAddress;

        /**
         * 高优白名单信息 erc721下的nft地址
         */
        private List<DesignatedNftCap> erc721MintIdCaps = new ArrayList<>();

        /**
         * erc721 下的nft白名单 地址
         */
        private List<NftIdentifier> whiteListedERC721Id = new ArrayList<>();

        /**
         * 高优白名单信息 erc721地址
         */
        private List<DesignatedCap> erc721MintCaps = new ArrayList<>();

        /**
         * erc721地址
         */
        private List<String> whiteListedERC721 = new ArrayList<>();

        /**
         * 全局铸造上限 仅mint nft 策略有
         */
        private Integer maxMintingAmount = 0;

    }

}
