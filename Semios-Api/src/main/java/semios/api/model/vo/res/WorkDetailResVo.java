package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.dto.chain.DaoReserveRatio;
import semios.api.model.dto.chain.DaoRoyaltyToken;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.request.InfuraCallRequestDto;
import semios.api.model.entity.*;
import semios.api.model.enums.*;
import semios.api.service.IDaoDrbStatisticsService;
import semios.api.service.IUserService;
import semios.api.service.IWorkService;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;
import semios.api.utils.ProtoDaoCommonUtil;
import semios.api.utils.SpringBeanUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

/**
 * @description: wor详情
 * @author: xiangbin
 * @create: 2022-08-05 15:43
 **/
@Slf4j
@Data
public class WorkDetailResVo {

    /**
     * dao id
     */
    private Integer daoId;

    /**
     * dao 32位id
     */
    private String projectId;

    /**
     * dao状态 0-未创建1-已创建未开始2-已开始3-已结束
     */
    private Integer daoStatus;

    /**
     * work所属dao编号
     */
    private Integer daoNumber;

    /**
     * canvas 的 canvasId
     */
    private String canvasId;

    /**
     * work所属canvas在合约上的ID
     */
    private String canId;

    /**
     * work所属canvas编号
     */
    private Integer canvasNumber;

    /**
     * work所属work编号
     */
    private Integer workNumber;

    /**
     * work当前状态 0-未铸造 1-已铸造 2-已过期
     */
    private Integer workStatus;

    /**
     * work的uri地址，只有未铸造状态会返回
     */
    private String workUri;

    /**
     * work所属dao名称
     */
    private String daoName;

    /**
     * work所属canvas名称
     */
    private String canvasName;

    /**
     * work的hash值
     */
    private String workHash;

    /**
     * work被收藏数量
     */
    private Integer favoritesAmount;

    /**
     * work是否被当前用户收藏
     */
    private Boolean favorited = false;

    /**
     * work创建者地址
     */
    private String creatorAddress;

    /**
     * work创建者名字
     */
    private String creatorName;

    /**
     * work铸造者地址
     */
    private String ownerAddress;

    /**
     * work铸造者名字
     */
    private String ownerName;

    /**
     * 铸造花费，未铸造时展示根据Canvas的价格或者一口价，铸造后价格固定
     */
    private String mintedPrice;

    /**
     * DAO的地板价
     */
    private String daoFloorPrice;

    /**
     * canvas current price
     */
    private String canvasCurrentPrice;

    /**
     * Minted by后的添加此Work的铸造者，未铸造出来时用Unminted占位 如果用户未设置用户名则展示钱包地址格式为0X...1234
     */
    private String mintedBy;
    /**
     * Minted Name
     */
    private String mintedName;

    /**
     * 此处显示铸造时的区块数，如果未铸造用Unminted占位
     */
    private String mintedBlock;

    /**
     * 此处显示被铸造的时间具体到日 July 7, 2021
     */
    private String mintedDate;

    /**
     * 此处显示的金额为canvas当前售价
     */
    private String price;

    /**
     * 是否为一口价 version_1.5
     *
     * @mock false
     */
    private Boolean fixedPrice = false;

    /**
     * work描述信息
     */
    private String workDescription;

    /**
     * work图片地址
     */
    private String imageUrl;

    /**
     * work原图片地址
     */
    private String originalImageUrl;

    /**
     * work id
     */
    private Integer workId;

    /**
     * tradeNft 地址
     */
    private String tradeNft;

    /**
     * 图片背景色
     */
    private String bgColor;

    /**
     * 计算图片在260宽度时的高度
     */
    private Double height;

    /**
     * 是否可修改 version_1.5
     *
     * @mock false
     */
    private Boolean modifiable = false;

    /**
     * 创建时的签名hash version_1.5
     */
    private String createSignHash;

    /**
     * canvas设置royaltyToken后的价格 1.9
     */
    private BigDecimal royaltyTokenPrice;

    /**
     * 最大收益
     */
    private BigDecimal maximumProfit = BigDecimal.ZERO;

    /**
     * 最大收益代币单位 daoSymbol
     */
    private String daoSymbol;

    /**
     * dao版本 1-1.8.5前的版本 2-1.8.5版本的 3-1.8.5之后的版本
     */
    private Integer daoVersion;

    /**
     * canvasUri
     */
    private String canvasUri;

    /**
     * opensea链接
     */
    private String openseaLink;

    /**
     * twitter链接
     */
    private String twitterLink;

    /**
     * discord链接
     */
    private String discordLink;

    /**
     * 是否自动生成 1-自动生成 2-上传的
     */
    private Integer generate;

//    /**
//     * 社交链接 多个用逗号分隔
//     *
//     * @mock "http://123.com,http://456.com"
//     */
//    private List<String> socialLinks;

    /**
     * 社交链接
     * <p>
     * 社交链接 List<DaoSocialLinks> 的json对象
     */
    private List<DaoSocialLinksVo> socialLinks;

    /**
     * 是否为basic dao 1-proto dao 2- basic dao
     *
     * @see BasicDaoEnum
     */
    private Integer basicDao;

    /**
     * 下一个区块开始时间还有多少秒
     */
    private Long nextDrbStartTime = 0L;
    /**
     * 设置的自动生成的数量
     */
    private Integer passesTotalQuantity;
    /**
     * 已经自动生成的被铸造的数量
     */
    private Integer havePassesQuantity;

    /**
     * 是否展示已铸造比例
     */
    private Boolean unifiedPriceModeOff = false;

    /**
     * 是否为全局一口价
     */
    private Boolean unifiedPriceSet = false;

    /**
     * 是否开启了TopUp模式 false-否 true-是
     */
    private Boolean topupMode = false;

    /**
     * 1.4 是否开启Erc20支付模式 false-否 true-是
     */
    private Boolean erc20PaymentMode = false;

    /**
     * 1.4 mint铸造收益分配策略
     */
    private DaoReserveRatio reserveRatio;


    /**
     * 1.4 铸造人数
     */
    private Integer minters = 0;

    /**
     * 1.4 铸造金额
     */
    private String mintFee = "0";

    /**
     * 1.4 铸造work的数量
     */
    private Integer mintedWorks = 0;

    /**
     * 1.4 该mint Window出块的资产 Token
     */
    private String blockRewardToken = "0";

    /**
     * 1.4 该mint Window出块的资产 eth
     */
    private String blockRewardEth = "0";

    /**
     * 1.4 这个DRB铸造会用于内部奖励的资产 Token
     */
    private String internalRewardToken = "0";

    /**
     * 1.4 这个DRB铸造会用于内部奖励的资产 eth
     */
    private String internalRewardEth = "0";

    //如果已经铸造了不展示
    /**
     * 1.4 该用户铸造这个作品会获得的奖励 Token -如果已经铸造成NFT则不需要展示
     */
    private String mintersMaxRewardToken = "0";

    /**
     * 1.4 该用户铸造这个作品会获得的奖励 eth-如果已经铸造成NFT则不需要展示
     */
    private String mintersMaxRewardEth = "0";


    /**
     * approve 方法的spender地址 为protocol合约地址
     */
    private String spenderAddress;

    /**
     * approve 方法的spender地址 为protocol合约地址
     */
    private String daoErc20Address;

    /**
     * 是否为外部ERC20 0-否 1-是
     */
    private Boolean isThirdpartyToken;

    /**
     * 1.5 Work锁定状态 0-未锁定 1-已锁定
     */
    private Integer workLockStatus;

    /**
     * 1.5 Work锁定剩余时间---单位：秒
     */
    private Long lockTime;

    /**
     * 1.5 当前work的721地址
     */
    private String erc721Address;

    /**
     * 1.6  Wallet比例
     */
    private BigDecimal walletRatio;

    /**
     * 1.6
     * 开启erc20支付：Redeem Asset Pool比例
     * 未开启erc20支付：Treasury比例
     */
    private BigDecimal treasuryOrPoolRatio;


    /**
     * 1.6.1 价格类型(0-Price)(1-Fixed Price)(2-Unified Price)
     * 0-浮动价格
     * 1-work的一口价
     * 2-dao的全局一口价
     */
    private Integer priceType;


    /**
     * 1.7 支付货币类型
     */
    private String payCurrencyType;

    /**
     * 1.7 input token的logo地址
     */
    private String inputTokenLogo;

    /**
     * 1.7 input token的address
     */
    private String inputTokenAddress;

    /**
     * 1.7 input token的decimals
     */
    private Integer inputTokenDecimals;


    public static WorkDetailResVo transfer(Work work, Dao dao, Canvas canvas) {
        WorkDetailResVo workDetailResVo = new WorkDetailResVo();
        workDetailResVo.setDaoId(dao.getId());
        workDetailResVo.setDaoStatus(dao.getDaoStatus());
        workDetailResVo.setDaoNumber(work.getDaoNumber());
        workDetailResVo.setCanvasNumber(work.getCanvasNumber());
        if (work.getWorkNumber() != null) {
            workDetailResVo.setWorkNumber(work.getWorkNumber());
        }
        workDetailResVo.setWorkStatus(work.getWorkStatus());
        workDetailResVo.setWorkUri(work.getWorkUri());
        workDetailResVo.setDaoName(dao.getDaoName());
        workDetailResVo.setCanvasName(canvas.getCanvasName());
        workDetailResVo.setWorkHash(work.getWorkHash());
        workDetailResVo.setFavoritesAmount(work.getFavoriteAmount());
        workDetailResVo.setFavorited(false);
        workDetailResVo.setBgColor(work.getBgColor());
        workDetailResVo.setHeight(work.getHeight());
        workDetailResVo.setCreateSignHash(work.getCreateSignHash());
        workDetailResVo.setDaoVersion(dao.getDaoVersion());
        workDetailResVo.setDaoSymbol(dao.getDaoSymbol());
        workDetailResVo.setCanvasUri(canvas.getCanvasUri());
        workDetailResVo.setTwitterLink(work.getTwitterLink());
        workDetailResVo.setDiscordLink(work.getDiscordLink());
        workDetailResVo.setOpenseaLink(work.getOpenseaLink());
        workDetailResVo.setGenerate(work.getGenerate());
        workDetailResVo.setSpenderAddress(ProtoDaoConstant.protocolContract);
        workDetailResVo.setDaoErc20Address(dao.getErc20Token());
        workDetailResVo.setIsThirdpartyToken(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getIsThirdpartyToken()));

        workDetailResVo.setPayCurrencyType(dao.getPayCurrencyType());
        workDetailResVo.setInputTokenLogo(dao.getInputTokenLogo());
        workDetailResVo.setInputTokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getInputTokenAddress()));
        workDetailResVo.setInputTokenDecimals(dao.getInputTokenDecimals());

        if (StringUtils.isNotBlank(work.getSocialLinks())) {
            try {
                List<DaoSocialLinksVo> daoSocialLinksVos = JacksonUtil.json2list(work.getSocialLinks(), DaoSocialLinksVo.class);
                workDetailResVo.setSocialLinks(daoSocialLinksVos);
            } catch (Exception e) {
                log.error("[workDetailResVo]get socialLinks error workId:{} socialLinks:{} ", work.getId(), work.getSocialLinks());
            }
        }
//        if (StringUtils.isNotBlank(work.getSocialLinks())) {
//            workDetailResVo.setSocialLinks(Arrays.asList(work.getSocialLinks().split(",")));
//        } else {
//            workDetailResVo.setSocialLinks(Arrays.asList("", "", ""));
//        }
//        for (int i = 0; i < 3; i++) {
//            if (workDetailResVo.getSocialLinks().size() < 3) {
//                workDetailResVo.getSocialLinks().add("");
//            }
//        }
        workDetailResVo.setBasicDao(dao.getBasicDao());

        IUserService userService = SpringBeanUtil.getBean(IUserService.class);
        if (StringUtils.isNotBlank(work.getCreatorAddress())) {
            workDetailResVo.setCreatorAddress(work.getCreatorAddress().toLowerCase());
            workDetailResVo.setOwnerAddress(work.getCreatorAddress().toLowerCase());
            if (userService != null) {
                User user = userService.findUserByAddressHash(work.getCreatorAddress().toLowerCase());
                if (user != null && StringUtils.isNotBlank(user.getUserName())) {
                    workDetailResVo.setCreatorName(user.getUserName());
                }
            }
        }

        if (StringUtils.isNotBlank(work.getOwnerAddress())) {
            String address = CommonUtil.addHexPrefixIfNotExist(work.getOwnerAddress().toLowerCase());
            workDetailResVo.setOwnerAddress(address);
            if (userService != null) {
                User user = userService.findUserByAddressHash(address);
                if (user != null && StringUtils.isNotBlank(user.getUserName())) {
                    workDetailResVo.setOwnerName(user.getUserName());
                }
            }
        }
        workDetailResVo.setDaoFloorPrice(dao.getDaoFloorPrice().stripTrailingZeros().toPlainString());
        if (StringUtils.isNotBlank(work.getMintedAddress())) {
            String mintedAddress = CommonUtil.addHexPrefixIfNotExist(work.getMintedAddress().toLowerCase());
            workDetailResVo.setMintedBy(mintedAddress);
            if (userService != null) {
                User user = userService.findUserByAddressHash(mintedAddress);
                if (user != null && StringUtils.isNotBlank(user.getUserName())) {
                    workDetailResVo.setMintedName(user.getUserName());
                }
            }
        }
        if (StringUtils.isNotBlank(work.getBlockNumber())) {
            workDetailResVo.setMintedBlock(CommonUtil.hexToTenString(work.getBlockNumber()));
        }
        workDetailResVo.setMintedDate(work.getBlockTime());
        if (work.getWorkStatus() != null && WorkStatusEnum.CASTED.getStatus().equals(work.getWorkStatus())) {
            if (work.getFixedPrice() != null) {
                workDetailResVo.setFixedPrice(true);
            }
            workDetailResVo.setPrice(ProtoDaoCommonUtil.bigdecimalToString(work.getMintedPrice(),6));
            workDetailResVo.setMintedPrice(ProtoDaoCommonUtil.bigdecimalToString(work.getMintedPrice(),6));

            boolean fixedPrice = (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0);
            if (!fixedPrice) {
                fixedPrice = (work.getFixedPrice() != null);
            }
            String reserveRatio = fixedPrice ? dao.getFixedReserveRatio() : dao.getUnfixedReserveRatio();
            DaoReserveRatio daoReserveRatio = new DaoReserveRatio(fixedPrice);
            if (StringUtils.isNotBlank(reserveRatio)) {
                daoReserveRatio = JacksonUtil.json2pojo(reserveRatio, DaoReserveRatio.class);
            }
            workDetailResVo.setReserveRatio(daoReserveRatio);
        } else {
            boolean fixedPrice = false;
            BigDecimal price;
            if (work.getFixedPrice() != null) {
                workDetailResVo.setPrice(ProtoDaoCommonUtil.bigdecimalToString(work.getFixedPrice(), 6));
                price = work.getFixedPrice();
                workDetailResVo.setMintedPrice(ProtoDaoCommonUtil.bigdecimalToString(work.getFixedPrice(), 6));
                workDetailResVo.setFixedPrice(true);
                fixedPrice = true;
            } else {
                price = canvas.getCurrentPrice();
                workDetailResVo.setPrice(ProtoDaoCommonUtil.bigdecimalToString(canvas.getCurrentPrice(), 6));
                workDetailResVo.setMintedPrice(ProtoDaoCommonUtil.bigdecimalToString(canvas.getCurrentPrice(), 6));
            }
            //1.9版本新增
            // 根据当前dao是否开启全局一口价来显示
            // 如果是非一口价作品（那么node肯定没开全局一口价，且fixed_price的值是空的），此时显示的是非一口价
            // 如果是一口价作品（那么node肯定没开全局一口价，但fixed_price不是空的）， 此时显示的是应该是一口价的
            // 如果是全局一口价的（node开了全局一口价，global_dao_price >=0的），那么应该显示的是一口价的百分比
            fixedPrice = (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0);
            if (!fixedPrice) {
                fixedPrice = (work.getFixedPrice() != null);
            }
            DaoRoyaltyToken daoRoyaltyToken = JacksonUtil.json2pojo(dao.getRoyaltyToken(), DaoRoyaltyToken.class);
            String reserveRatio = fixedPrice ? dao.getFixedReserveRatio() : dao.getUnfixedReserveRatio();
            DaoReserveRatio daoReserveRatio = new DaoReserveRatio(fixedPrice);
            if (StringUtils.isNotBlank(reserveRatio)) {
                daoReserveRatio = JacksonUtil.json2pojo(reserveRatio, DaoReserveRatio.class);
            }
            workDetailResVo.setReserveRatio(daoReserveRatio);
            if (canvas.getRoyaltyToken() != null) {
                if (daoRoyaltyToken != null && daoRoyaltyToken.getMinterReward() != null && daoRoyaltyToken.getMinterReward().compareTo(BigDecimal.ZERO) != 0) {
                    BigDecimal canvasMintFee = daoReserveRatio.getCanvasMintFee().divide(new BigDecimal("100"));
                    workDetailResVo.setRoyaltyTokenPrice(price.subtract(price.multiply(canvasMintFee).multiply(canvas.getRoyaltyToken().divide(new BigDecimal("100")))));
                    //都是零的情况，或者canvasMintFee为零，或者royaltyToken为零
                    if (price.compareTo(workDetailResVo.getRoyaltyTokenPrice()) == 0) {
                        workDetailResVo.setRoyaltyTokenPrice(null);
                    }
                }
            }
            //预估最大收益 公式 DRB结束时发放的ERC20总量✖️Mint Reward的比例✖️（这幅作品价格✖️DAO Mint Fee的比例➗当前DRB所有mint DAO收入的ETH）✖️（1-Royalty Token比例）
            BigDecimal drbErc20Amount = BigDecimal.ZERO;
            //String royaltyToken = dao.getRoyaltyToken();
            //DaoRoyaltyToken daoRoyaltyToken = JacksonUtil.json2pojo(royaltyToken, DaoRoyaltyToken.class);
            BigDecimal mintReward = BigDecimal.ZERO;
            if (daoRoyaltyToken != null && daoRoyaltyToken.getMinterReward() != null) {
                if (dao.getDaoVersion() <= 2) {
                    mintReward = ProtoDaoCommonUtil.bigdecimalPercentageToString(daoRoyaltyToken.getMinterReward(), false).divide(new BigDecimal("100"));
                } else {
                    mintReward = daoRoyaltyToken.getMinterReward().divide(new BigDecimal("100"));
                }
            }

            BigDecimal ratio = BigDecimal.ONE;
            if (mintReward.compareTo(BigDecimal.ZERO) > 0) {
                IDaoDrbStatisticsService daoDrbStatisticsService = SpringBeanUtil.getBean(IDaoDrbStatisticsService.class);
                DaoDrbStatistics daoDrbStatistics = daoDrbStatisticsService.selectByDaoIdAndDrbNumber(dao.getId(), Integer.valueOf(dao.getCurrentRound()));
                if (daoDrbStatistics != null && daoDrbStatistics.getDrbVolExTax() != null && daoDrbStatistics.getDrbVolExTax().compareTo(BigDecimal.ZERO) > 0) {
                    BigDecimal drbVolExtax = daoDrbStatistics.getDrbVolExTax();
                    BigDecimal ratioPrice = price.multiply(daoReserveRatio.getDaoMintFee().divide(new BigDecimal("100")));
                    ratio = ratioPrice.divide(drbVolExtax.add(ratioPrice), 4, RoundingMode.HALF_UP);
                } else {
                    IWorkService workService = SpringBeanUtil.getBean(IWorkService.class);
                    if (workService != null) {
                        //铸造价格为零的情况，查询canvas铸造数量/dao铸造数量
//                        int canvasCount = workService.selectDrbNftCountByCanvasId(canvas.getId() + "", Integer.valueOf(Dao4ArtConstant.CURRENT_ROUND));
                        int daoCount = workService.selectDrbNftCountByDaoId(canvas.getDaoId() + "", Integer.valueOf(dao.getCurrentRound()));
                        int canvasCount = 1;
                        daoCount = daoCount + 1;
                        ratio = new BigDecimal(String.valueOf(canvasCount)).divide(new BigDecimal(String.valueOf(daoCount)), 4, RoundingMode.HALF_UP);
                    }

                }
                try {
                    if (dao.getDaoVersion() == 3) {
                        // 此处逻辑需要删除
                        // 获取指定 DAO 和 round 的 ERC20 奖励
                        // function getRoundReward(bytes32 daoId, uint256 round) public view returns (uint256);
                        InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
                        infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
                        infuraCallRequestDto.setTo(ContractMethodEnum.GET_ROUND_REWARD.getContractAddress());
                        infuraCallRequestDto.setData(ContractMethodEnum.GET_ROUND_REWARD.getMethodAddress() + dao.getProjectId() + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.tenToHex(Integer.parseInt(dao.getCurrentRound()))));
                        log.info("[workDetailResVo]infura getRoundRewardResult contractAddress:{} request data:{}", infuraCallRequestDto.getTo(), infuraCallRequestDto.getData());
                        ISubscriptionService iSubscriptionService = SpringBeanUtil.getBean(ISubscriptionService.class);
                        // 调用查询使用数据集的user
//                        Result<String> getRoundRewardResult = iSubscriptionService.infuraCall(infuraCallRequestDto);
                        Result<String> getRoundRewardResult = new Result<>();
                        getRoundRewardResult.setData("0");
                        getRoundRewardResult.setResultCode(ResultDesc.SUCCESS.getResultCode());
                        if (getRoundRewardResult.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
                            //log.info("[workDetailResVo]infura getRoundRewardResult return data:{}", getRoundRewardResult.getData());
                            //String claim = getRoundRewardResult.getData();
                            //String claimPrice = CommonUtil.hexToTenString(claim);
                            //drbErc20Amount = new BigDecimal(claimPrice).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()));
                            drbErc20Amount = BigDecimal.ZERO;
                        } else {
                            log.error("[workDetailResVo]infura getRoundRewardResult return data:{} desc:{} daoId:{}",
                                    getRoundRewardResult.getData(), getRoundRewardResult.getResultDesc(), dao.getId());
                        }
                    } else {
                        // 计算方法 总的发放量/总的drb
                        Integer mintWindow = dao.getDaoMintWindow();
                        String totalSupply = dao.getErc20TotalSupply();
                        drbErc20Amount = new BigDecimal(String.valueOf(totalSupply)).divide(new BigDecimal(String.valueOf(mintWindow)), 18, RoundingMode.FLOOR);
                    }

                    if (drbErc20Amount.compareTo(BigDecimal.ZERO) > 0) {
                        log.info("[workDetailResVo] daoVersion:{} drbErc20Amount:{} mintReward:{} ratio:{} canvas.getRoyaltyToken():{} ", dao.getDaoVersion(), drbErc20Amount, mintReward, ratio,
                                canvas.getRoyaltyToken());
                        BigDecimal canvasRoyaltyToken = canvas.getRoyaltyToken() == null ? BigDecimal.ZERO : canvas.getRoyaltyToken();
                        BigDecimal maximumProfit = drbErc20Amount.multiply(mintReward).multiply(ratio).multiply(BigDecimal.ONE.subtract(canvasRoyaltyToken.divide(new BigDecimal("100"))));
                        log.info("[workDetailResVo] workId:{} maximumProfit:{} ", work.getId(), maximumProfit);
                        workDetailResVo.setMaximumProfit(maximumProfit);
                    }

                } catch (Exception e) {
                    log.error("[workDetailResVo]infura getRoundRewardResult error daoId:{} e:{} ", dao.getId(), e);
                    //如果失败了先置为0，之后修改数据库
                }

            }
        }
        workDetailResVo.setCanvasCurrentPrice(ProtoDaoCommonUtil.bigdecimalToString(canvas.getCurrentPrice(), 6));
        workDetailResVo.setWorkDescription(work.getWorkDescription());
        String imageUrl = work.getImageUrl();
        workDetailResVo
                .setImageUrl(imageUrl.replaceAll(ProtoDaoConstant.bucketName, ProtoDaoConstant.compressBucketName));
        workDetailResVo.setOriginalImageUrl(imageUrl);
        workDetailResVo.setWorkId(work.getId());
//        workDetailResVo.setCanvasId(canvas.getId());
        if (StringUtils.isNotBlank(canvas.getCanvasId())) {
            workDetailResVo.setCanvasId(CommonUtil.addHexPrefixIfNotExist(canvas.getCanvasId()));
        }

        workDetailResVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
        if (dao.getDaoNumber() != null) {
            workDetailResVo.setTradeNft(
                    ProtoDaoConstant.daoOpenseaLink + ("ProtoDao " + dao.getDaoName()).toLowerCase().replaceAll(" ", "-"));
        }
        if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
            if (!WorkStatusEnum.CASTED.getStatus().equals(work.getWorkStatus())) {
                workDetailResVo.setPrice(ProtoDaoCommonUtil.bigdecimalToString(dao.getGlobalDaoPrice(), 6));
                workDetailResVo.setMintedPrice(ProtoDaoCommonUtil.bigdecimalToString(dao.getGlobalDaoPrice(), 6));
            }
            workDetailResVo.setUnifiedPriceSet(true);
        }
        if (work.getGenerate() == 1 && dao.getNeedMintableWork() != null && dao.getNeedMintableWork() == 0) {
            workDetailResVo.setPassesTotalQuantity(dao.getGenerateWorkSet());
            workDetailResVo.setUnifiedPriceModeOff(true);
        }

        workDetailResVo.setTopupMode(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getTopupMode()));
        workDetailResVo.setErc20PaymentMode(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getErc20PaymentMode()));


        if (workDetailResVo.getUnifiedPriceSet()) {
            workDetailResVo.setPriceType(WorkPriceTypeEnum.DAO_GLOBAL_PRICE.getType());  // 全局一口价类型
        } else if (workDetailResVo.getFixedPrice()) {
            workDetailResVo.setPriceType(WorkPriceTypeEnum.FIXED_PRICE.getType());  // Work一口价类型
        } else {
            workDetailResVo.setPriceType(WorkPriceTypeEnum.CANVAS_PRICE.getType());  // 浮动价格类型
        }

        //mint分配比例
//        DaoReserveRatio reserveRatio;
//        if (WorkPriceTypeEnum.FIXED_PRICE.getType().equals(work.getPriceType())) {
//            reserveRatio = JacksonUtil.json2pojo(dao.getFixedReserveRatio(), DaoReserveRatio.class);
//        } else {
//            reserveRatio = JacksonUtil.json2pojo(dao.getUnfixedReserveRatio(), DaoReserveRatio.class);
//        }


        return workDetailResVo;
    }
}
