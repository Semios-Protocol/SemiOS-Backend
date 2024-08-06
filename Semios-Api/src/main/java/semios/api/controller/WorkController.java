package semios.api.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.multipart.MultipartFile;
import org.web3j.crypto.Hash;
import org.web3j.utils.Numeric;
import semios.api.interceptor.S3Service;
import semios.api.model.annotation.RepeatSubmit;
import semios.api.model.bo.WorkCountBo;
import semios.api.model.dto.chain.DaoEthRoyaltyToken;
import semios.api.model.dto.chain.DaoReserveRatio;
import semios.api.model.dto.chain.DaoRoyaltyToken;
import semios.api.model.dto.common.*;
import semios.api.model.dto.response.MintWorkUriDto;
import semios.api.model.dto.response.NewCanvasUriDto;
import semios.api.model.entity.*;
import semios.api.model.enums.*;
import semios.api.model.exceptions.PausedException;
import semios.api.model.vo.PageVo;
import semios.api.model.vo.req.*;
import semios.api.model.vo.res.BaseWorkVo.WorkNftDetailsVo;
import semios.api.model.vo.res.*;
import semios.api.model.vo.res.NodePermission.CreateNodeId;
import semios.api.service.*;
import semios.api.service.common.CommonService;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.*;
import semios.api.utils.eip712.EIP712Domain;
import semios.api.utils.eip712.EIP712Message;
import semios.api.utils.eip712.Entry;

import javax.servlet.http.HttpServletRequest;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * work作品相关接口
 *
 * @author xiangbin
 * @order 3
 */
@Slf4j
@RestController
@RequestMapping("/work")
public class WorkController {

    private static final RestTemplate restTemplate = new RestTemplate();
    @Autowired
    private IWorkService workService;
    @Autowired
    private IFavoritesService favoritesService;
    @Autowired
    private IUserService userService;
    @Autowired
    private IDaoService daoService;
    @Autowired
    private ICanvasService canvasService;
    @Autowired
    private S3Service s3Service;
    @Value("${file.dir}")
    private String fileDir;
    @Autowired
    private EIP712Message eip712Message;
    @Value("${network.id}")
    private Integer networkId;
    @Autowired
    private CommonService commonService;
    @Autowired
    private IDaoDrbStatisticsService daoDrbStatisticsService;
    @Autowired
    private IDaoAllocationStrategyService daoAllocationStrategyService;
    @Autowired(required = false)
    private ISubscriptionService iSubscriptionService;

    public static void main(String[] args) {
        WorkEditResVo workEditResVo = new WorkEditResVo();
        Work work = new Work();
        work.setSocialLinks("");

    }

    /**
     * 1.6 work详情接口 添加比例
     *
     * @apiNote 添加当前work的锁定信息
     */
    @PostMapping(value = "/detail")
    public Result<WorkDetailResVo> workDetail(@RequestBody(required = false) WorkReqVo workReqVo,
                                              HttpServletRequest request) {
        Result<WorkDetailResVo> result = new Result<>();
        log.info("[workDetail] workReqVo:{}", JacksonUtil.obj2json(workReqVo));
        Work work = workService.selectWorkById(workReqVo.getWorkId());
        if (work == null || WorkStatusEnum.EXPIRED.getStatus().equals(work.getWorkStatus())) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("work not exist!");
            return result;
        }

        Dao dao = daoService.getById(work.getDaoId());
        if (dao == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("dao not exist!");
            return result;
        }

        Canvas canvas = canvasService.selectCanvasDetailByCanvasId(work.getCanvasId());

        if (canvas == null || StringUtils.isBlank(canvas.getCanvasId())) {
            canvas = new Canvas();
            if (StringUtils.isNotBlank(work.getCanvasId())) {
                canvas.setCurrentPrice(commonService.getCanvasNextPrice(dao.getProjectId(), work.getCanvasId(), commonService.getErc20DecimalIfErc20PayModel(dao), dao.getInputTokenDecimals()));
            } else {
                canvas.setCurrentPrice(dao.getCanvasFloorPrice());
            }
            if (StringUtils.isNotBlank(work.getCanvasId())) {
                CanvasCreateReqVo canvasCreateReqVo = new CanvasCreateReqVo();
                try {
                    String canvasId = work.getCanvasId();
                    canvasCreateReqVo.setUserAddress(work.getCreatorAddress());
                    NewCanvasUriDto newCanvasUriDto = NewCanvasUriDto.transfer(canvasCreateReqVo);
                    String fileName = canvasId + ".json";
                    fileName = CommonUtil.replaceOperator(fileName);
                    String urlPrefix = String.format(ProtoDaoConstant.urlPrefix, ProtoDaoConstant.bucketName);

                    BucketObjectRepresentaion representaion = new BucketObjectRepresentaion();
                    representaion.setObjectName(fileName);
                    representaion.setText(JacksonUtil.obj2json(newCanvasUriDto));
                    s3Service.putObject(
                            ProtoDaoConstant.bucketName + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.canvasBucketName,
                            representaion);

                    canvas.setCanvasUri(
                            urlPrefix + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.canvasBucketName + "/" + fileName);
                    canvas.setCanvasId(canvasId);
                    log.info("[workDetail] canvas uri:{}", urlPrefix + fileName);

                } catch (Exception e) {
                    log.error("[workDetail] s3 canvas uri error e", e);
                    result.setResultCode(ResultDesc.FAIL.getResultCode());
                    result.setResultDesc("please try again later!");
                    return result;
                }
            }
        }
        WorkDetailResVo workDetailResVo = WorkDetailResVo.transfer(work, dao, canvas);

        if (workDetailResVo.getPassesTotalQuantity() != null) {
            int workCount = workService.selectCountNftGenerateWork(dao.getId());
            workDetailResVo.setHavePassesQuantity(workCount);
        }

//        int drbNftNums = workService.selectDrbNftCountByDaoId(String.valueOf(dao.getId()), Integer.valueOf(dao.getCurrentRound()));
        WorkCountBo workCountBo = workService.selectDrbNftOwnerCountByDaoId(String.valueOf(dao.getId()), Integer.valueOf(dao.getCurrentRound()));
        workDetailResVo.setMintedWorks(workCountBo.getMintedWorks());
        workDetailResVo.setMinters(workCountBo.getMinters());
        workDetailResVo.setMintFee(StringUtils.isBlank(workCountBo.getMintFee()) ? "0" : workCountBo.getMintFee());

        if (WorkStatusEnum.NOT_CAST.getStatus().equals(work.getWorkStatus()) && BasicDaoEnum.PROTO_DAO.getBasicType().equals(dao.getBasicDao())) {
            // 查询到下一个drb周期还需要多久...
            Result<String> resultBlockNum = iSubscriptionService.ethGetBlockNumber(ProtoDaoConstant.netWork);
            if (resultBlockNum.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[drbInfo] ethGetBlockNumber error:{}", result.getResultDesc());
                result.setResultCode(ResultDesc.ERROR.getResultCode());
                result.setResultDesc("network anomaly！ please try again later!");
                return result;
            }
            if (workCountBo.getMintedWorks() >= dao.getDailyMintCap()) {

                BigDecimal denominator = new BigDecimal(dao.getDuration()).divide(new BigDecimal("1e18"));  // 分母=每周期出块数量
                log.info("分母=每周期出块数量:" + denominator);
                BigDecimal blockNumber = new BigDecimal(CommonUtil.hexToTenString(resultBlockNum.getData())); // 当前区块数
                log.info("当前区块数:" + blockNumber);
                BigDecimal startBlockNumber = new BigDecimal(dao.getDaoStartBlock()); // 开始区块
                log.info("开始区块:" + startBlockNumber);
                BigDecimal currentRound = new BigDecimal(Integer.parseInt(dao.getCurrentRound()) - 1);   // 已经完成周期数
                log.info("当前周期数:" + currentRound);
                BigDecimal numThisCurrentRound = blockNumber.subtract(startBlockNumber).subtract(currentRound.multiply(denominator));

                if (dao.getDaoRestartBlock() != null) {
                    log.info("重新开始的区块高度为:" + dao.getDaoRestartBlock());
                    // 如果重新开始，当前周期内出的块应该加上余数..
                    BigDecimal restartBlock = new BigDecimal(dao.getDaoRestartBlock());
                    BigDecimal roundSub = restartBlock.subtract(startBlockNumber);
                    log.info("重新开始的时间-开始的时间的差值:" + currentRound);
                    BigDecimal[] resultDiv = roundSub.divideAndRemainder(denominator);
                    log.info("时间差对每个周期的区块数相除的值:" + JacksonUtil.obj2json(resultDiv));
                    BigDecimal blockRemainder = resultDiv[1];
                    numThisCurrentRound = numThisCurrentRound.add(blockRemainder);
                }
                log.info("当前周期内已经出了多少块:" + numThisCurrentRound);

                BigDecimal numerator = denominator.subtract(numThisCurrentRound);
                log.info("分子：还有多少块到下个周期:" + numerator);
                Long countdown = numerator.multiply(new BigDecimal(ProtoDaoConstant.BLOCK_SECONDS)).longValue();
                workDetailResVo.setNextDrbStartTime(countdown);

//                Double nextDrbStartTime = commonService.nextDrbStartTime();
//                if (nextDrbStartTime != null) {
//                    workDetailResVo.setNextDrbStartTime(nextDrbStartTime.intValue());
//                }

            }

        }
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isNotBlank(userAddress)) {
            if (work.getCreatorAddress().equals(userAddress)
                    && work.getWorkStatus().equals(WorkStatusEnum.NOT_CAST.getStatus())) {
                workDetailResVo.setModifiable(true);
            }
            if (userAddress.equals(work.getOwnerAddress())
                    && work.getWorkStatus().equals(WorkStatusEnum.CASTED.getStatus())) {
                workDetailResVo.setModifiable(true);
            }
            Favorites favorites = favoritesService.findByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(),
                    work.getId() + "", userAddress);
            if (favorites != null) {
                workDetailResVo.setFavorited(true);
            }
        }
        //1.4 查询数据看板信息

//        BigDecimal remainderErc20 = new BigDecimal(dao.getErc20TotalSupply());
//        BigDecimal remainderEth = dao.getDaoAssetPool() == null ? BigDecimal.ZERO : dao.getDaoAssetPool();
//        DaoDrbStatistics daoDrbStatistics = daoDrbStatisticsService.selectByDaoIdAndDrbNumberForAnalytics(dao.getId(), Integer.valueOf(dao.getCurrentRound()));
//        if (daoDrbStatistics != null && daoDrbStatistics.getDaoReward() != null) {
//            remainderErc20 = remainderErc20.subtract(daoDrbStatistics.getDaoReward());
//        }
        //实时查询最新的token和eth
        DaoDrbStatistics daoDrbStatistics = daoDrbStatisticsService.selectByDaoIdAndDrbNumber(dao.getId(), Integer.valueOf(dao.getCurrentRound()));
        if (daoDrbStatistics != null && daoDrbStatistics.getErc20Amount() != null) {
            workDetailResVo.setBlockRewardToken(ProtoDaoCommonUtil.bigdecimalToString(daoDrbStatistics.getErc20Amount()));
            workDetailResVo.setBlockRewardEth(ProtoDaoCommonUtil.bigdecimalToString(daoDrbStatistics.getEthAmount()));
        } else {
            BigDecimal remainderErc20 = commonService.erc20BalanceOf(dao.getErc20Token(), dao.getFeePool(), dao.getErc20TokenDecimals(), dao.getInputTokenDecimals());
            //BigDecimal remainderEth = commonService.ethBalanceOf(dao.getFeePool(),dao.getInputTokenDecimals());
            BigDecimal remainderEth = commonService.getInputToken(dao);
            //无限模式全部发放
            if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getInfiniteMode())) {
                //不用计算，全部发放
            } else {
                //乐透模式
                if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getRoyaltyTokenLotteryMode()) && dao.getLastActiveRound() != null && StringUtils.isNotBlank(dao.getCurrentRound())) {
                    BigDecimal lotteryRount = new BigDecimal(dao.getCurrentRound()).subtract(new BigDecimal(String.valueOf(dao.getLastActiveRound())));
                    BigDecimal mintWind = new BigDecimal(String.valueOf(dao.getRemainingMintWindow())).add(lotteryRount).subtract(BigDecimal.ONE);
                    if (mintWind.compareTo(BigDecimal.ZERO) > 0) {
                        remainderErc20 = remainderErc20.multiply(lotteryRount).divide(mintWind, 18, RoundingMode.HALF_UP);
                        remainderEth = remainderEth.multiply(lotteryRount).divide(mintWind, 18, RoundingMode.HALF_UP);
                    }

                } else {
                    if (dao.getRemainingMintWindow() != null && dao.getRemainingMintWindow() > 0) {
                        remainderErc20 = remainderErc20.divide(new BigDecimal(String.valueOf(dao.getRemainingMintWindow())), 18, RoundingMode.HALF_UP);
                        remainderEth = remainderEth.divide(new BigDecimal(String.valueOf(dao.getRemainingMintWindow())), 18, RoundingMode.HALF_UP);
                    }
                }
            }


            workDetailResVo.setBlockRewardToken(ProtoDaoCommonUtil.bigdecimalToString(remainderErc20));
            workDetailResVo.setBlockRewardEth(ProtoDaoCommonUtil.bigdecimalToString(remainderEth));
        }


        if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getTopupMode())) {
            workDetailResVo.setInternalRewardToken(workDetailResVo.getBlockRewardToken());
            workDetailResVo.setInternalRewardEth(workDetailResVo.getBlockRewardEth());
        } else {
            List<DaoAllocationStrategy> daoAllocationStrategies = daoAllocationStrategyService.selectByOriginProjectIdAndType(dao.getProjectId(), null);
            DaoAllocationStrategy daoAllocationStrategy = new DaoAllocationStrategy();
            daoAllocationStrategy.setRoyaltyProportion(BigDecimal.ONE);
            DaoAllocationStrategy daoAllocationStrategyToken = daoAllocationStrategies.stream().filter(v -> TrueOrFalseEnum.FALSE.getStatus().equals(v.getType()) && DaoRoyaltyTypeEnum.TWO.getType().equals(v.getRoyaltyType())).findFirst().orElseGet(() -> daoAllocationStrategy);
            DaoAllocationStrategy daoAllocationStrategyEth = daoAllocationStrategies.stream().filter(v -> TrueOrFalseEnum.TRUE.getStatus().equals(v.getType()) && DaoRoyaltyTypeEnum.TWO.getType().equals(v.getRoyaltyType())).findFirst().orElseGet(() -> daoAllocationStrategy);

            workDetailResVo.setInternalRewardToken(ProtoDaoCommonUtil.bigdecimalToString(new BigDecimal(workDetailResVo.getBlockRewardToken()).multiply(ProtoDaoCommonUtil.bigdecimalPercentage(daoAllocationStrategyToken.getRoyaltyProportion()))));
            workDetailResVo.setInternalRewardEth(ProtoDaoCommonUtil.bigdecimalToString(new BigDecimal(workDetailResVo.getBlockRewardEth()).multiply(ProtoDaoCommonUtil.bigdecimalPercentage(daoAllocationStrategyEth.getRoyaltyProportion()))));
        }

        //interreqward * mint reward * 贡献度
        //贡献度 = 当前价格 * minterReward / 总的价格* mintereward + 当前价格 * minterReward
        log.info("workDetailResVo:{}", JacksonUtil.obj2json(workDetailResVo));
        if (WorkStatusEnum.NOT_CAST.getStatus().equals(work.getWorkStatus())) {
            log.info("当前work未铸造:{}", JacksonUtil.obj2json(work));
            DaoReserveRatio daoReserveRatio = workDetailResVo.getReserveRatio();
            log.info("mint铸造收益分配策略:{}", JacksonUtil.obj2json(daoReserveRatio));
            BigDecimal maxPrice = new BigDecimal(workDetailResVo.getPrice()).compareTo(BigDecimal.ZERO) == 0 ? BigDecimal.ONE : new BigDecimal(workDetailResVo.getPrice());
            log.info("maxPrice:{}", maxPrice.toPlainString());
            BigDecimal mintFee = daoReserveRatio.getDaoMintFee() == null || daoReserveRatio.getDaoMintFee().compareTo(BigDecimal.ZERO) == 0 ? BigDecimal.TEN.multiply(BigDecimal.TEN) : daoReserveRatio.getDaoMintFee();
            log.info("mintFee:{}", mintFee.toPlainString());
            BigDecimal mintersMaxRewardEth = maxPrice.multiply(ProtoDaoCommonUtil.bigdecimalPercentage(mintFee));
            log.info("mintersMaxRewardEth:{}", mintersMaxRewardEth.toPlainString());
            //区分登录和不登录两种情况

            //1查询minter分配比例  查询分配比例

            DaoRoyaltyToken daoRoyaltyToken = new DaoRoyaltyToken();
            if (StringUtils.isNotBlank(dao.getRoyaltyToken())) {
                daoRoyaltyToken = JacksonUtil.json2pojo(dao.getRoyaltyToken(), DaoRoyaltyToken.class);
                log.info("token代币分配策略:{}", JacksonUtil.obj2json(daoRoyaltyToken));
            }
            //ETH
            DaoEthRoyaltyToken daoEthRoyaltyToken = new DaoEthRoyaltyToken();
            if (StringUtils.isNotBlank(dao.getEthRoyaltyToken())) {
                daoEthRoyaltyToken = JacksonUtil.json2pojo(dao.getEthRoyaltyToken(), DaoEthRoyaltyToken.class);
                log.info("eth分配策略:{}", JacksonUtil.obj2json(daoEthRoyaltyToken));
            }
            BigDecimal royaltyToken = daoRoyaltyToken.getMinterReward();
            royaltyToken = royaltyToken == null ? BigDecimal.ZERO : royaltyToken;
            log.info("token的minter分配比例:{}", royaltyToken);
            BigDecimal royaltyEth = daoEthRoyaltyToken.getMinterETHReward();
            royaltyEth = royaltyEth == null ? BigDecimal.ZERO : royaltyEth;
            log.info("eth的minter分配比例:{}", royaltyEth);

            if (daoDrbStatistics != null && daoDrbStatistics.getContribution() != null && daoDrbStatistics.getContribution().compareTo(BigDecimal.ZERO) > 0) {
                log.info("daoDrbStatistics不为空:{}", JacksonUtil.obj2json(daoDrbStatistics));
                workDetailResVo.setMintersMaxRewardToken(ProtoDaoCommonUtil.bigdecimalToString(mintersMaxRewardEth.divide(daoDrbStatistics.getContribution().add(mintersMaxRewardEth), 6, RoundingMode.HALF_UP).multiply(new BigDecimal(workDetailResVo.getInternalRewardToken())).multiply(ProtoDaoCommonUtil.bigdecimalPercentage(royaltyToken))));
                workDetailResVo.setMintersMaxRewardEth(ProtoDaoCommonUtil.bigdecimalToString(mintersMaxRewardEth.divide(daoDrbStatistics.getContribution().add(mintersMaxRewardEth), 6, RoundingMode.HALF_UP).multiply(new BigDecimal(workDetailResVo.getInternalRewardEth())).multiply(ProtoDaoCommonUtil.bigdecimalPercentage(royaltyEth))));
            } else {
                log.info("daoDrbStatistics为空,workDetailResVo.getInternalRewardToken()={},workDetailResVo.getInternalRewardEth()={}", workDetailResVo.getInternalRewardToken(), workDetailResVo.getInternalRewardEth());
                workDetailResVo.setMintersMaxRewardToken(new BigDecimal(workDetailResVo.getInternalRewardToken()).multiply(ProtoDaoCommonUtil.bigdecimalPercentage(royaltyToken)).stripTrailingZeros().toPlainString());
                workDetailResVo.setMintersMaxRewardEth(new BigDecimal(workDetailResVo.getInternalRewardEth()).multiply(ProtoDaoCommonUtil.bigdecimalPercentage(royaltyEth)).stripTrailingZeros().toPlainString());
            }

        } else {
            workDetailResVo.setMintersMaxRewardToken(null);
            workDetailResVo.setMintersMaxRewardEth(null);
        }

        // 1.5 添加锁定状态信息
        workDetailResVo.setWorkLockStatus(work.getLockStatus());
        if (WorkLockStatusEnum.LOCK.getStatus().equals(work.getLockStatus())) {
            //如果是锁定状态，需要计算出剩余的锁定时间
            //开始区块高度+锁定区块高度-当前区块高度=剩余区块高度
            //剩余区块高度*13 = 还有多少秒
            Result<String> resultBlockNum = iSubscriptionService.ethGetBlockNumber(ProtoDaoConstant.netWork);
            if (resultBlockNum.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[drbInfo] ethGetBlockNumber error:{}", result.getResultDesc());
                result.setResultCode(ResultDesc.ERROR.getResultCode());
                result.setResultDesc("network anomaly！ please try again later!");
                return result;
            }
            BigDecimal remainBlock = new BigDecimal(work.getLockStartBlock())
                    .add(new BigDecimal(work.getLockDurationBlock()))
                    .subtract(new BigDecimal(CommonUtil.hexToTenString(resultBlockNum.getData())));
            workDetailResVo.setLockTime(remainBlock.multiply(new BigDecimal(ProtoDaoConstant.BLOCK_SECONDS)).longValue());
        }

        // 1.5 加入dao的721Address
        workDetailResVo.setErc721Address(dao.getErc721Token());


        // 1.6 对未开启top up的work添加比例字段

        if (!workDetailResVo.getTopupMode()) {
            if (workDetailResVo.getErc20PaymentMode()) {
                // 如果开启erc20支付，，展示token to eth 比例
                workDetailResVo.setTreasuryOrPoolRatio(dao.getTokenEthRoyalty().multiply(new BigDecimal("100")));
            } else {
                // 未开启erc20支付，，展示 eth to token 比例
                workDetailResVo.setTreasuryOrPoolRatio(dao.getEthTokenRoyalty().multiply(new BigDecimal("100")));
            }
            workDetailResVo.setWalletRatio(new BigDecimal("100").subtract(workDetailResVo.getTreasuryOrPoolRatio()));
        }

        result.setData(workDetailResVo);

        return result;
    }

    /**
     * 收藏者列表
     */
    @PostMapping(value = "/favorite/user")
    public Result<WorkFavoriteResVo> workFavoriteUser(@RequestBody(required = false) WorkReqVo workReqVo) {
        Result<WorkFavoriteResVo> result = new Result<>();

        Page<Favorites> iPage = new Page<>(workReqVo.getPageNo(), workReqVo.getPageSize());
        Page<Favorites> favoritesPage =
                favoritesService.findFavoritesById(iPage, workReqVo.getWorkId(), FavoriteTypeEnum.WORK_FAVORITE.getType());
        List<Favorites> favoritesList = favoritesPage.getRecords();
        List<String> userAddressList =
                favoritesList.stream().map(Favorites::getUserAddress).collect(Collectors.toList());
        if (!userAddressList.isEmpty()) {
            List<User> userList = userService.findUsersByUserAddress(userAddressList);
            Map<String, User> userMap = userList.stream().collect(Collectors.toMap(User::getUserAddress, v -> v));
            List<WorkFavoriteResVo> workFavoriteResVoList = userAddressList.stream().map(userMap::get)
                    .map(WorkFavoriteResVo::transfor).collect(Collectors.toList());
            result.setDataList(workFavoriteResVoList);
        } else {
            result.setDataList(new ArrayList<>());
        }

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(workReqVo.getPageNo());
        page.setPageSize(workReqVo.getPageSize());
        page.setCount(favoritesPage.getTotal());
        result.setPage(page);
        return result;
    }

    /**
     * explore下未铸造的work列表 1.6
     */
    @PostMapping(value = "/explore/unmintedWorks")
    public Result<WorkListVo> exploreUnmintedWorks(@RequestBody(required = false) DaoSortedReqVo daoSortedReqVo,
                                                   HttpServletRequest request) {

        Result<WorkListVo> result = new Result<>();

        Page<Work> iPage = new Page<>(daoSortedReqVo.getPageNo(), daoSortedReqVo.getPageSize());
        Page<Work> workPage = workService.unmintedWorks(iPage, daoSortedReqVo);
        List<Work> works = workPage.getRecords();
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && favoritesIds.size() > 0) {
            List<Integer> favoritesIds2 = favoritesIds;
            works.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getId())));
        }
        // 1.4.3 修改返回值
        List<WorkListVo> workListVos =
                works.stream().map(v -> WorkListVo.transfor(v, null)).collect(Collectors.toList());
        result.setDataList(workListVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoSortedReqVo.getPageNo());
        page.setPageSize(daoSortedReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);

        return result;

    }

    /**
     * 1.6.1 explore下未铸造的work列表(图片形式) P0
     */
    @PostMapping(value = "/explore/unmintedWorks/v2")
    public Result<WorkListVoV2> exploreUnmintedWorksV2(@RequestBody(required = false) DaoSortedReqVo daoSortedReqVo,
                                                       HttpServletRequest request) {
        Result<WorkListVoV2> result = new Result<>();

        Page<Work> iPage = new Page<>(daoSortedReqVo.getPageNo(), daoSortedReqVo.getPageSize());
        Page<Work> workPage = workService.unmintedWorks(iPage, daoSortedReqVo);
        List<Work> works = workPage.getRecords();

        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && !favoritesIds.isEmpty()) {
            List<Integer> favoritesIds2 = favoritesIds;
            works.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getId())));
        }
        log.info("获取到的work info:" + JacksonUtil.obj2json(works));

        // 1.4.3 修改返回值
        List<WorkListVoV2> workListVos =
                works.stream().map(v -> WorkListVoV2.transfor(v, null)).collect(Collectors.toList());
        result.setDataList(workListVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoSortedReqVo.getPageNo());
        page.setPageSize(daoSortedReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);

        return result;

    }

    /**
     * explore下nft列表
     */
    @PostMapping(value = "/explore/nfts")
    public Result<WorkListVo> exploreNfts(@RequestBody(required = false) DaoSortedReqVo daoSortedReqVo,
                                          HttpServletRequest request) {
        log.info("explore/nfts 接口开始时间:" + DateUtil.getCurrentTimestamp());
        Result<WorkListVo> result = new Result<>();
        Page<Work> iPage = new Page<>(daoSortedReqVo.getPageNo(), daoSortedReqVo.getPageSize());

        log.info("获取所有的nft数据 开始时间:" + DateUtil.getCurrentTimestamp());
        Page<Work> workPage = workService.selectNfts(iPage, daoSortedReqVo);
        log.info("获取所有的nft数据 结束时间:" + DateUtil.getCurrentTimestamp());
        List<Work> works = workPage.getRecords();
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && favoritesIds.size() > 0) {
            List<Integer> favoritesIds2 = favoritesIds;
            works.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getId())));
        }
        log.info("开始转换返回dto时间:" + DateUtil.getCurrentTimestamp());
        List<WorkListVo> workListVos =
                works.stream().map(v -> WorkListVo.transfor(v, null)).collect(Collectors.toList());
        log.info("结束转换返回dto时间:" + DateUtil.getCurrentTimestamp());
        result.setDataList(workListVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoSortedReqVo.getPageNo());
        page.setPageSize(daoSortedReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);

        log.info("explore/nfts 接口结束时间:" + DateUtil.getCurrentTimestamp());
        return result;

    }

    /**
     * 1.6.1 explore下nft列表(图片形式) P0
     */
    @PostMapping(value = "/explore/nfts/v2")
    public Result<WorkListVoV2> exploreNftsV2(@RequestBody(required = false) DaoSortedReqVo daoSortedReqVo,
                                              HttpServletRequest request) {
        Result<WorkListVoV2> result = new Result<>();
        Page<Work> iPage = new Page<>(daoSortedReqVo.getPageNo(), daoSortedReqVo.getPageSize());
        Page<Work> workPage = workService.selectNfts(iPage, daoSortedReqVo);
        List<Work> works = workPage.getRecords();
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && !favoritesIds.isEmpty()) {
            List<Integer> favoritesIds2 = favoritesIds;
            works.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getId())));
        }

        List<WorkListVoV2> workListVos =
                works.stream().map(v -> WorkListVoV2.transfor(v, null)).collect(Collectors.toList());

        result.setDataList(workListVos);
        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoSortedReqVo.getPageNo());
        page.setPageSize(daoSortedReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);

        return result;

    }

    /**
     * explore当前drb下铸造的nft
     */
    @PostMapping(value = "/explore/drbNfts")
    public Result<WorkListVo> exploreDrbNfts(@RequestBody(required = false) DaoSortedReqVo daoSortedReqVo,
                                             HttpServletRequest request) {

        Result<WorkListVo> result = new Result<>();
        Page<Work> iPage = new Page<>(daoSortedReqVo.getPageNo(), daoSortedReqVo.getPageSize());
        daoSortedReqVo.setCurrentDrb(Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
        Page<Work> workPage = workService.selectDrbNfts(iPage, daoSortedReqVo);
        List<Work> works = workPage.getRecords();
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && favoritesIds.size() > 0) {
            List<Integer> favoritesIds2 = favoritesIds;
            works.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getId())));
        }
        List<WorkListVo> workListVos =
                works.stream().map(v -> WorkListVo.transfor(v, null)).collect(Collectors.toList());
        result.setDataList(workListVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoSortedReqVo.getPageNo());
        page.setPageSize(daoSortedReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);
        return result;
    }

    /**
     * Rankings NFTS
     */
    @PostMapping(value = "/rankings/nfts")
    public Result<WorkListForRankingsVo> rankingsNfts(@RequestBody(required = false) PageVo pageVo,
                                                      HttpServletRequest request) {

        Result<WorkListForRankingsVo> result = new Result<>();
        Page<Work> iPage = new Page<>(pageVo.getPageNo(), pageVo.getPageSize());
        Page<Work> workPage = workService.rankingNfts(iPage);
        List<Work> works = workPage.getRecords();
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && favoritesIds.size() > 0) {
            List<Integer> favoritesIds2 = favoritesIds;
            works.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getId())));
        }
        List<WorkListForRankingsVo> workListForRankingsVos =
                works.stream().map(WorkListForRankingsVo::transfor).collect(Collectors.toList());
        result.setDataList(workListForRankingsVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(pageVo.getPageNo());
        page.setPageSize(pageVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);
        return result;

    }

    /**
     * nft-hold藏品
     */
    @PostMapping(value = "/hold")
    public Result<WorkListVo> workHole(@RequestBody(required = false) UserProfilePageReqVo userProfilePageReqVo) {

        Result<WorkListVo> result = new Result<>();
        Page<Work> iPage = new Page<>(userProfilePageReqVo.getPageNo(), userProfilePageReqVo.getPageSize());
        String userAddress = userProfilePageReqVo.getUserAddress();
        Page<Work> workPage = workService.findHoldByUserAddress(iPage, userAddress);
        List<Work> works = workPage.getRecords();
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && favoritesIds.size() > 0) {
            List<Integer> favoritesIds2 = favoritesIds;
            works.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getId())));
        }
        List<WorkListVo> workListVoList =
                works.stream().map(v -> WorkListVo.transfor(v, null)).collect(Collectors.toList());
        result.setDataList(workListVoList);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(userProfilePageReqVo.getPageNo());
        page.setPageSize(userProfilePageReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);

        return result;

    }

    /**
     * 1.6.1 个人中心页面 nft-hold藏品(图片形式) P1
     */
    @PostMapping(value = "/hold/v2")
    public Result<WorkListVoV2> workHoleV2(@RequestBody(required = false) UserProfilePageReqVo userProfilePageReqVo) {
        Result<WorkListVoV2> result = new Result<>();
        Page<Work> iPage = new Page<>(userProfilePageReqVo.getPageNo(), userProfilePageReqVo.getPageSize());
        String userAddress = userProfilePageReqVo.getUserAddress();

        Page<Work> workPage = workService.findHoldByUserAddress(iPage, userAddress);
        List<Work> works = workPage.getRecords();
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && !favoritesIds.isEmpty()) {
            List<Integer> favoritesIds2 = favoritesIds;
            works.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getId())));
        }
        List<WorkListVoV2> workListVoList =
                works.stream().map(v -> WorkListVoV2.transfor(v, null)).collect(Collectors.toList());
        result.setDataList(workListVoList);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(userProfilePageReqVo.getPageNo());
        page.setPageSize(userProfilePageReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);

        return result;

    }

    /**
     * minted藏品 铸造的藏品 1.6
     */
    @PostMapping(value = "/minted")
    public Result<WorkListVo> workMinted(@RequestBody(required = false) UserProfilePageReqVo userProfilePageReqVo) {

        Result<WorkListVo> result = new Result<>();
        Page<Work> iPage = new Page<>(userProfilePageReqVo.getPageNo(), userProfilePageReqVo.getPageSize());
        String userAddress = userProfilePageReqVo.getUserAddress();
        Page<Work> workPage = workService.findMintedByUserAddress(iPage, userAddress);
        List<Work> works = workPage.getRecords();
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && favoritesIds.size() > 0) {
            List<Integer> favoritesIds2 = favoritesIds;
            works.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getId())));
        }
        List<WorkListVo> workListVoList =
                works.stream().map(v -> WorkListVo.transfor(v, null)).collect(Collectors.toList());
        result.setDataList(workListVoList);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(userProfilePageReqVo.getPageNo());
        page.setPageSize(userProfilePageReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);
        return result;

    }

    /**
     * 1.6.1 个人中心页面 minted藏品 铸造的藏品(图片形式) P1
     */
    @PostMapping(value = "/minted/v2")
    public Result<WorkListVoV2> workMintedV2(@RequestBody(required = false) UserProfilePageReqVo userProfilePageReqVo) {
        Result<WorkListVoV2> result = new Result<>();
        Page<Work> iPage = new Page<>(userProfilePageReqVo.getPageNo(), userProfilePageReqVo.getPageSize());
        String userAddress = userProfilePageReqVo.getUserAddress();

        Page<Work> workPage = workService.findMintedByUserAddress(iPage, userAddress);
        List<Work> works = workPage.getRecords();
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && favoritesIds.size() > 0) {
            List<Integer> favoritesIds2 = favoritesIds;
            works.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getId())));
        }
        List<WorkListVoV2> workListVoList =
                works.stream().map(v -> WorkListVoV2.transfor(v, null)).collect(Collectors.toList());
        result.setDataList(workListVoList);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(userProfilePageReqVo.getPageNo());
        page.setPageSize(userProfilePageReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);
        return result;
    }

    /**
     * 保存work信息 返回data信息为work的hash值
     */
    @PostMapping(value = "/create")
    @RepeatSubmit(key = "work_create")
    public Result<WorkCreateResVo> createWork(HttpServletRequest request, WorkCreateReqVo workCreateReqVo) {

        /*
         * 1。根据图片生成hash值
         * 2。上传图片到aws返回图片地址
         * 数据落盘
         */
        Result<WorkCreateResVo> result = new Result<>();
        log.info("[createWork] workCreateReqVo:{}", JacksonUtil.obj2json(workCreateReqVo));

        String useraddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isNotBlank(useraddress)) {
            workCreateReqVo.setUserAddress(useraddress.toLowerCase());
        } else {

            result.setResultDesc("please login.");
            result.setResultCode(ResultDesc.USER_ERROR.getResultCode());
            return result;
        }

        // 判断一口价是否传值
        if (workCreateReqVo.getPriceType() != null
                && workCreateReqVo.getPriceType().equals(WorkPriceTypeEnum.FIXED_PRICE.getType())) {
            if (workCreateReqVo.getFixedPrice() == null) {
                result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
                result.setResultDesc("fixed price is null");
                return result;
            }
        } else {
            workCreateReqVo.setFixedPrice(null);
        }

        MultipartFile workImage = workCreateReqVo.getImage();
        if (workImage == null || workImage.isEmpty()) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc("image is null");
            return result;
        }

        if (!ImageUtil.isImage(workImage)) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc("not an image");
            return result;
        }

        try {
            Dao dao = daoService.getById(workCreateReqVo.getDaoId());
//            Canvas canvas = canvasService.getById(workCreateReqVo.getCanvasId());

            if (dao == null) {
                result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
                result.setResultDesc("DAO or canvas is not exists!");
                return result;
            }

//            if (!canvas.getOwnerAddress().equals(workCreateReqVo.getUserAddress())) {
//                result.setResultCode(ResultDesc.FAIL.getResultCode());
//                result.setResultDesc("Operation failed, parameters are incorrect.");
//                return result;
//            }

//            if (!canvas.getDaoId().equals(Integer.valueOf(workCreateReqVo.getDaoId()))) {
//                result.setResultCode(ResultDesc.FAIL.getResultCode());
//                result.setResultDesc("Operation failed, parameters are incorrect.");
//                return result;
//            }

            if (DaoStatusEnum.FINISHED.getStatus().equals(dao.getDaoStatus())) {
                result.setResultCode(ResultDesc.FAIL.getResultCode());
                result.setResultDesc("You can no longer edit as the DAO Mint Window has ended.");
                return result;
            }

            if (dao.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())) {
                String msg = dao.getDaoName() + " (D4A@%s) This function is suspended for security reasons.";
                msg = String.format(msg, dao.getDaoNumber());
                throw new PausedException(msg);
            }

//            if (canvas.getCanvasStatus().equals(CanvasStatusEnum.SHUT_DOWN.getStatus())) {
//                String msg =
//                        canvas.getCanvasName() + " (D4A@%s/Canvas*%s) This function is suspended for security reasons.";
//                msg = String.format(msg, canvas.getDaoNumber(), canvas.getCanvasNumber());
//                throw new PausedException(msg);
//            }

            // check eip712 sig
//            Map<String, List<Entry>> types = JacksonUtil.json2maplist(eip712Message.getTypesJson(), Entry.class);
//            EIP712Domain domain1 = JacksonUtil.json2pojo(eip712Message.getDomainJson1(), EIP712Domain.class);
//            EIP712Domain domain2 = JacksonUtil.json2pojo(eip712Message.getDomainJson2(), EIP712Domain.class);
//            eip712Message.setTypes(types);
//            if (dao.getDaoVersion() == 3) {
//                eip712Message.setDomain(domain2);
//            } else {
//                eip712Message.setDomain(domain1);
//            }
//            Map<String, String> message = new HashMap<String, String>();
//            message.put("canvasID", Numeric.prependHexPrefix(workCreateReqVo.getCanvasId()));
            String urlPrefix = String.format(ProtoDaoConstant.urlPrefix, ProtoDaoConstant.bucketName);
            String imageName = workCreateReqVo.getWorkUriRandom();
            if (StringUtils.isBlank(imageName)) {
                imageName = CodeUtil.generateCode('W');
            }
            String fileName = imageName + ".json";
            String s3FileName =
                    urlPrefix + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.workBucketName + "/" + fileName;
//            message.put("tokenURIHash", Hash.sha3String(s3FileName));
//            if (workCreateReqVo.getFixedPrice() == null) {
//                message.put("flatPrice", "0");
//            } else {
//                message.put("flatPrice", Convert.toWei(workCreateReqVo.getFixedPrice(), Convert.Unit.ETHER)
//                        .stripTrailingZeros().toPlainString());
//            }
//            eip712Message.setMessage(message);
//            log.info("[createWork] eip712Message:{}", JacksonUtil.obj2json(eip712Message));

//            String address =
//                    EthersUtils.recoverEip712Address(eip712Message, networkId, workCreateReqVo.getCreateSignHash());
//            log.info("[createWork] canvas owner address:{}, recover eip712 address:{}", useraddress,
//                    address);
//            if (!canvas.getOwnerAddress().equals(address)) {
//                result.setResultCode(ResultDesc.FAIL.getResultCode());
//                result.setResultDesc("Operation failed, parameters are incorrect.");
//                return result;
//            }

            String originalFilename = workImage.getOriginalFilename();
            String ext = "";
            if (originalFilename.lastIndexOf(".") > 0) {
                ext = originalFilename.substring(originalFilename.lastIndexOf("."));
            }
            String imageFileName = CodeUtil.generateCode('F') + ext;
            String dirStr = fileDir;
            File dirPath = new File(dirStr);
            if (!dirPath.exists()) {
                dirPath.mkdirs();
            }
            String filePath = dirStr + File.separator + imageFileName;
            File imageFile = new File(filePath);
            workImage.transferTo(imageFile);

            String workHash = ImageUtil.getMD5(imageFile);

//            Work hashWork = workService.selectWorkByHash(workHash);
//            if (hashWork != null) {
//                result.setResultCode(ResultDesc.FAIL.getResultCode());
//                result.setResultDesc("Invalid work！The works is duplicate");
//                return result;
//            }
            // canvas最多500个未铸造的work
//            Page<Work> iPage = new Page<>(1, 10);
//            CanvasSortedReqVo canvasSortedReqVo = new CanvasSortedReqVo();
//            canvasSortedReqVo.setCanvasId(canvas.getCanvasId());
//            Page<Work> workPage = workService.selectWorkByCanvasId(iPage, canvasSortedReqVo);
//            if (workPage.getTotal() >= 500) {
//                result.setResultCode(ResultDesc.FAIL.getResultCode());
//                result.setResultDesc("The creation limit has been reached");
//                return result;
//            }
            Work work = new Work();
            work.setGenerate(2);
            ImageUtil.HeightAndBgColor heightAndBgColor = ImageUtil.getImageRgb(imageFile);
            if (heightAndBgColor != null) {
                log.info("workHash:{} height:{} bgColor:{}", workHash, heightAndBgColor.getHeight(),
                        heightAndBgColor.getBgColor());
                work.setHeight(heightAndBgColor.getHeight());
                work.setBgColor(heightAndBgColor.getBgColor());
            } else {
                log.error("image not height error");
                // work.setHeight(260.0);
                // work.setBgColor("#FFFFFF");
                result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
                result.setResultDesc("Works creation failed, please try again later");
                return result;
            }

            String imageUrl = "";

            try {
                // 文件上传前的名称 处理图片用
                String image = imageName + ext;
                s3Service.putImage(ProtoDaoConstant.bucketName + ProtoDaoConstant.workBucketName, imageFile, image, true);

                imageUrl = urlPrefix + ProtoDaoConstant.workBucketName + "/" + image;
                workCreateReqVo.setImageUrl(imageUrl);
                log.info("[work-create] s3DaoLogoUrl:{}", imageUrl);
            } catch (Exception e) {
                log.error("[work-create]upload image error", e);
            }

            // 处理uri用
            MintWorkUriDto mintWorkUriDto = MintWorkUriDto.transfer(workCreateReqVo);
            // String sourceString = JacksonUtil.obj2json(mintWorkUriDto);
            // byte[] sourceByte = sourceString.getBytes(StandardCharsets.UTF_8);
            //
            // long second = LocalDateTime.now().toInstant(ZoneOffset.of("+8")).getEpochSecond();
            // String fileName = Md5Utils.md5AsBase64(sourceByte) + String.valueOf(second).substring(5) + ".json";
            // fileName = CommonUtil.replaceOperator(fileName);
            BucketObjectRepresentaion representaion = new BucketObjectRepresentaion();
            representaion.setObjectName(fileName);
            representaion.setText(JacksonUtil.obj2json(mintWorkUriDto));
            s3Service.putObject(
                    ProtoDaoConstant.bucketName + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.workBucketName,
                    representaion);
            log.info("[work-create] s3FileName:{}", s3FileName);

            work.setWorkDescription(workCreateReqVo.getWorkDescription());
            work.setCreatorAddress(workCreateReqVo.getUserAddress());
            work.setOwnerAddress(workCreateReqVo.getUserAddress());
            work.setWorkStatus(WorkStatusEnum.NOT_CAST.getStatus());
            work.setCreateTime(LocalDateTime.now());
            work.setCanvasId("");
            work.setCanId(null);
            work.setDaoId(Integer.valueOf(workCreateReqVo.getDaoId()));
            work.setImageUrl(imageUrl);
            work.setCanvasNumber(null);
            work.setDaoNumber(dao.getDaoNumber());
            work.setProjectId(dao.getProjectId());
            work.setWorkHash(workHash);
            work.setWorkUri(s3FileName);
            work.setFavoriteAmount(0);
            work.setCreateSignHash(workCreateReqVo.getCreateSignHash());
            work.setPriceType(workCreateReqVo.getPriceType());// 一口价
            if (StringUtils.isNotBlank(workCreateReqVo.getFixedPrice())
                    && !workCreateReqVo.getFixedPrice().equals("null")) {
                work.setFixedPrice(new BigDecimal(workCreateReqVo.getFixedPrice()));
            }

            workService.save(work);

            WorkCreateResVo workCreateResVo = new WorkCreateResVo();
            workCreateResVo.setWorkId(work.getId());
            workCreateResVo.setWorkHash(workHash);
            workCreateResVo.setImageUrl(imageUrl);
            workCreateResVo.setDaoName(dao.getDaoName());
            workCreateResVo.setCanvasName("");
//            workCreateResVo.setCanvasName(canvas.getCanvasName());
//            workCreateResVo.setCanvasCurrentPrice(canvas.getCurrentPrice());
            workCreateResVo.setCanvasCurrentPrice(dao.getCanvasFloorPrice());
            if (workCreateReqVo.getPriceType() != null
                    && workCreateReqVo.getPriceType().equals(WorkPriceTypeEnum.FIXED_PRICE.getType())) {
                workCreateResVo.setCanvasCurrentPrice(new BigDecimal(workCreateReqVo.getFixedPrice()));
            }
            result.setData(workCreateResVo);

            return result;
        } catch (FileNotFoundException e) {
            log.error("[work-create]FileNotFoundException ", e);
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("Operation failed, please try again later.");
            return result;
        } catch (IOException e) {
            log.error("[work-create]IOException", e);
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("Operation failed, please try again later.");
            return result;
        } catch (Exception e) {
            log.error("[work-create]Exception", e);
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("Operation failed, please try again later.");
            return result;
        }

    }

    /**
     * 批量删除works 根据返回dada中的数字展示本次删除的数量
     */
    @PostMapping(value = "/delete")
    @RepeatSubmit(key = "work_delete")
    public Result<Integer> deleteWork(@RequestBody(required = false) WorkDeleteReqVo workDeleteReqVo,
                                      HttpServletRequest request) {

        Result<Integer> result = new Result<>();
        log.info("[deleteWork] workDeleteReqVo:{}", JacksonUtil.obj2json(workDeleteReqVo));

        int size = 0;

//        if (StringUtils.isBlank(workDeleteReqVo.getCanvasId())) {
//            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
//            result.setResultDesc("Operation failed, parameters are incorrect.");
//            return result;
//        }

        if (workDeleteReqVo.getWorkIds() == null || workDeleteReqVo.getWorkIds().size() == 0) {
            result.setData(size);
            return result;
        }

        String useraddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
//        Canvas canvas = canvasService.getById(workDeleteReqVo.getCanvasId());
//
//        if (canvas == null) {
//            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
//            result.setResultDesc("Operation failed, parameters are incorrect.");
//            return result;
//        }
//
//        if (!canvas.getOwnerAddress().equals(useraddress)) {
//            result.setResultCode(ResultDesc.FAIL.getResultCode());
//            result.setResultDesc("Operation failed, parameters are incorrect.");
//            return result;
//        }

//        if (canvas.getCanvasStatus().equals(CanvasStatusEnum.SHUT_DOWN.getStatus())) {
//            String msg =
//                    canvas.getCanvasName() + " (D4A@%s/Canvas*%s) This function is suspended for security reasons.";
//            msg = String.format(msg, canvas.getDaoNumber(), canvas.getCanvasNumber());
//            throw new PausedException(msg);
//        }

//        Dao dao = daoService.getById(canvas.getDaoId());
//
//        if (dao != null && dao.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())) {
//            String msg = dao.getDaoName() + " (D4A@%s) This function is suspended for security reasons.";
//            msg = String.format(msg, dao.getDaoNumber());
//            throw new PausedException(msg);
//        }

        List<String> idList = new ArrayList<String>();
        for (String workId : workDeleteReqVo.getWorkIds()) {
            Work work = workService.getById(workId);
            if (work != null && work.getWorkStatus().equals(WorkStatusEnum.NOT_CAST.getStatus())
                    && work.getCreatorAddress().equals(useraddress)) {
                idList.add(workId);
            }
        }

        if (idList.size() > 0) {
            size = workService.deleteWorkByIds(idList);
        }
        result.setData(size);

        return result;

    }

    /**
     * work修改 version_1.1
     */
    @PostMapping(value = "/edit")
    @RepeatSubmit(key = "work_edit")
    public Result<String> workEdit(@RequestBody(required = false) WorkReqVo workReqVo, HttpServletRequest request) {

        Work work = workService.selectWorkById(workReqVo.getWorkId());
        Result<String> result = new Result<>();
        if (work == null || WorkStatusEnum.EXPIRED.getStatus().equals(work.getWorkStatus())) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("The status of the work has changed. Please refresh the page and try again.");
            return result;
        }

        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isBlank(userAddress) || !userAddress.equals(work.getOwnerAddress())) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("You are not the owner, please check the connected wallet account.");
            return result;
        }


        //只有未铸造的可以修改workDescription
        if (!WorkStatusEnum.CASTED.getStatus().equals(work.getWorkStatus()) && workReqVo.getWorkDescription() != null && !workReqVo.getWorkDescription().equals(work.getWorkDescription())) {
            try {
                MintWorkUriDto mintWorkUriDto = restTemplate.getForObject(work.getWorkUri(), MintWorkUriDto.class);
                mintWorkUriDto.setDescription(workReqVo.getWorkDescription());

                String fileName = work.getWorkUri().substring(work.getWorkUri().lastIndexOf("/") + 1);

                s3Service.deleteObject(
                        ProtoDaoConstant.bucketName + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.workBucketName,
                        fileName);

                BucketObjectRepresentaion representaion = new BucketObjectRepresentaion();
                representaion.setObjectName(fileName);
                representaion.setText(JacksonUtil.obj2json(mintWorkUriDto));
                s3Service.putObject(
                        ProtoDaoConstant.bucketName + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.workBucketName,
                        representaion);

            } catch (Exception e) {
                log.error("[work-edit] error param:{} e:{}", JacksonUtil.obj2json(workReqVo), e);
                result.setResultDesc("network error please try again later.");
                result.setResultCode(ResultDesc.ERROR.getResultCode());
                return result;
            }

            work.setWorkDescription(workReqVo.getWorkDescription());
        }

        work.setTwitterLink(workReqVo.getTwitterLink());
        work.setDiscordLink(workReqVo.getDiscordLink());
        work.setOpenseaLink(workReqVo.getOpenseaLink());
        if (workReqVo.getSocialLinks() != null) {
            work.setSocialLinks(JacksonUtil.obj2json(workReqVo.getSocialLinks()));
        }

        workService.updateById(work);

        return result;

    }

    /**
     * work编辑时查询详情页 version_1.1
     */
    @PostMapping(value = "/edit/detail")
    public Result<WorkEditResVo> workEditDetail(@RequestBody(required = false) WorkReqVo workReqVo,
                                                HttpServletRequest request) {

        Result<WorkEditResVo> result = new Result<>();

        if (workReqVo == null || StringUtils.isBlank(workReqVo.getWorkId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isBlank(userAddress)) {
            result.setResultDesc("please login.");
            result.setResultCode(ResultDesc.USER_ERROR.getResultCode());
            return result;
        }

        Work work = workService.getById(Integer.valueOf(workReqVo.getWorkId()));
        if (work == null) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("work is not exist!");
            return result;
        }

        if (!work.getOwnerAddress().equals(userAddress)) {
            log.info("[workEditDetail] ownerAddress:{} userAddress:{}", work.getOwnerAddress(), userAddress);
            result.setResultDesc("You are not the owner, please check the connected wallet account.");
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            return result;
        }

        WorkEditResVo workEditResVo = new WorkEditResVo();
        workEditResVo.setWorkId(String.valueOf(work.getId()));
        workEditResVo.setWorkDescription(work.getWorkDescription());
        if (StringUtils.isNotBlank(work.getSocialLinks())) {
            try {
                List<DaoSocialLinksVo> daoSocialLinksVos = JacksonUtil.json2list(work.getSocialLinks(), DaoSocialLinksVo.class);
                workEditResVo.setSocialLinks(daoSocialLinksVos);
            } catch (Exception e) {
                log.error("[workEditDetail]get socialLinks error workId:{} socialLinks:{} ", work.getId(), work.getSocialLinks());
            }
        }
        if (workEditResVo.getSocialLinks() == null || workEditResVo.getSocialLinks().isEmpty()) {
            DaoSocialLinksVo daoSocialLinksVo = new DaoSocialLinksVo();
            List<DaoSocialLinksVo> daoSocialLinksList = new ArrayList<>();
            daoSocialLinksList.add(daoSocialLinksVo);
            daoSocialLinksList.add(daoSocialLinksVo);
            daoSocialLinksList.add(daoSocialLinksVo);
            workEditResVo.setSocialLinks(daoSocialLinksList);
        }
//        if (StringUtils.isNotBlank(work.getSocialLinks())) {
//            workEditResVo.setSocialLinks(new ArrayList<>(Arrays.asList(work.getSocialLinks().split(","))));
//        } else {
//            workEditResVo.setSocialLinks(Arrays.asList("", "", ""));
//        }
//        for (int i = 0; i < 3; i++) {
//            if (workEditResVo.getSocialLinks().size() < 3) {
//                workEditResVo.getSocialLinks().add("");
//            }
//        }
//
        workEditResVo.setOpenseaLink(work.getOpenseaLink());
        workEditResVo.setTwitterLink(work.getTwitterLink());
        workEditResVo.setDiscordLink(work.getDiscordLink());

        workEditResVo.setWorkStatus(work.getWorkStatus());
        workEditResVo.setOwnerAddress(work.getOwnerAddress());

        result.setData(workEditResVo);
        return result;
    }

    /**
     * 查询是否有铸造work的权限
     */
    @PostMapping(value = "/permission/cast")
    public Result<WorkPermissionResVo> permissionToCast(@RequestBody(required = false) DaoReqVo daoReqVo, HttpServletRequest request) {

        Result<WorkPermissionResVo> result = new Result<>();
        WorkPermissionResVo workPermissionResVo = new WorkPermissionResVo();
        String useraddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isBlank(useraddress)) {
            result.setResultDesc("please login.");
            result.setResultCode(ResultDesc.USER_ERROR.getResultCode());
            return result;
        }
        if (daoReqVo == null || StringUtils.isBlank(daoReqVo.getDaoId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        Work work = workService.findHoldByAddressAndDaoId(useraddress, Integer.valueOf(daoReqVo.getDaoId()));
        workPermissionResVo.setPermission(work == null);
        result.setData(workPermissionResVo);

        return result;
    }

    /**
     * 保存work信息 返回data信息为work的hash值
     */
    @PostMapping(value = "/protoDao/create")
    @RepeatSubmit(key = "protodao_work_create")
    public Result<WorkCreateResVo> protoDaoCreateWork(HttpServletRequest request, WorkCreateReqVo workCreateReqVo) {

        /*
         * 1。根据图片生成hash值
         * 2。上传图片到aws返回图片地址
         * 数据落盘
         */
        Result<WorkCreateResVo> result = new Result<>();
        log.info("[protoDaoCreateWork] workCreateReqVo:{}", JacksonUtil.obj2json(workCreateReqVo));

        String useraddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isNotBlank(useraddress)) {
            workCreateReqVo.setUserAddress(useraddress.toLowerCase());
        } else {

            result.setResultDesc("please login.");
            result.setResultCode(ResultDesc.USER_ERROR.getResultCode());
            return result;
        }

        //image可以为空
        MultipartFile workImage = workCreateReqVo.getImage();

        if (workImage != null && !workImage.isEmpty() && !ImageUtil.isImage(workImage)) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc("not an image");
            return result;
        }


        try {
            Dao dao = daoService.getById(workCreateReqVo.getDaoId());
            Canvas canvas = canvasService.selectCanvasDetailByCanvasId(workCreateReqVo.getCanvasId());

            if (dao == null) {
                result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
                result.setResultDesc("DAO or canvas is not exists!");
                return result;
            }

            if (canvas != null && !canvas.getOwnerAddress().equals(workCreateReqVo.getUserAddress())) {
                log.error("[protoDaoCreateWork] canvas ownerAddress not equals userAddress workCreateReqVo:{}", JacksonUtil.obj2json(workCreateReqVo));
                result.setResultCode(ResultDesc.FAIL.getResultCode());
                result.setResultDesc("Operation failed, parameters are incorrect.");
                return result;
            }

            if (canvas != null && !canvas.getDaoId().equals(Integer.valueOf(workCreateReqVo.getDaoId()))) {
                log.error("[protoDaoCreateWork] daoId is not equals workCreateReqVo:{}", JacksonUtil.obj2json(workCreateReqVo));
                result.setResultCode(ResultDesc.FAIL.getResultCode());
                result.setResultDesc("Operation failed, parameters are incorrect.");
                return result;
            }

            if (DaoStatusEnum.FINISHED.getStatus().equals(dao.getDaoStatus())) {
                result.setResultCode(ResultDesc.FAIL.getResultCode());
                result.setResultDesc("You can no longer edit as the DAO Mint Window has ended.");
                return result;
            }

            if (dao.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())) {
                String msg = dao.getDaoName() + " (D4A@%s) This function is suspended for security reasons.";
                msg = String.format(msg, dao.getDaoNumber());
                throw new PausedException(msg);
            }

            if (canvas != null && canvas.getCanvasStatus().equals(CanvasStatusEnum.SHUT_DOWN.getStatus())) {
                String msg =
                        canvas.getCanvasName() + " (D4A@%s/Canvas*%s) This function is suspended for security reasons.";
                msg = String.format(msg, canvas.getDaoNumber(), canvas.getCanvasNumber());
                throw new PausedException(msg);
            }


            // check eip712 sig
            Map<String, List<Entry>> types = JacksonUtil.json2maplist(eip712Message.getTypesJson(), Entry.class);
            EIP712Domain domain1 = JacksonUtil.json2pojo(eip712Message.getDomainJson1(), EIP712Domain.class);
//            EIP712Domain domain2 = JacksonUtil.json2pojo(eip712Message.getDomainJson2(), EIP712Domain.class);
            eip712Message.setTypes(types);
            eip712Message.setDomain(domain1);
            Map<String, String> message = new HashMap<String, String>();
            message.put("canvasID", Numeric.prependHexPrefix(workCreateReqVo.getCanvasId()));
            String urlPrefix = String.format(ProtoDaoConstant.urlPrefix, ProtoDaoConstant.bucketName);
            String imageName = workCreateReqVo.getWorkUriRandom();
            if (StringUtils.isBlank(imageName)) {
                imageName = CodeUtil.generateCode('W');
            }
            String fileName = imageName + ".json";
            String s3FileName =
                    urlPrefix + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.workBucketName + "/" + fileName;
            log.info("[protoDaoCreateWork] s3FileName:{}", s3FileName);
            message.put("tokenURIHash", Hash.sha3String(s3FileName));
            if (StringUtils.isBlank(workCreateReqVo.getFixedPrice()) || "null".equals(workCreateReqVo.getFixedPrice())) {
                message.put("flatPrice", "0");
            } else {

                if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getErc20PaymentMode())) {
                    message.put("flatPrice", new BigDecimal(workCreateReqVo.getFixedPrice())
                            .multiply(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals())).stripTrailingZeros().toPlainString());
                } else {
                    //message.put("flatPrice", Convert.toWei(workCreateReqVo.getFixedPrice(), Convert.Unit.ETHER).stripTrailingZeros().toPlainString());
                    message.put("flatPrice", new BigDecimal(workCreateReqVo.getFixedPrice())
                            .multiply(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals())).stripTrailingZeros().toPlainString());
                }

            }
            eip712Message.setMessage(message);
            log.info("[protoDaoCreateWork] eip712Message:{}", JacksonUtil.obj2json(eip712Message));

            String address =
                    EthersUtils.recoverEip712Address(eip712Message, networkId, workCreateReqVo.getCreateSignHash());
            log.info("[protoDaoCreateWork] canvas owner address:{}, recover eip712 address:{}", workCreateReqVo.getUserAddress(),
                    address);

            if (canvas != null) {
                log.info("[protoDaoCreateWork] canvas:" + JacksonUtil.obj2json(canvas));
            }
            log.info("[protoDaoCreateWork] workCreateReqVo:" + JacksonUtil.obj2json(workCreateReqVo));
            log.info("[protoDaoCreateWork] eip712 address:" + address);

            if (canvas != null && !canvas.getOwnerAddress().equals(address)) {
                log.error("[protoDaoCreateWork] canvas ownerAddress createSignHash workCreateReqVo:{}", JacksonUtil.obj2json(workCreateReqVo));
                result.setResultCode(ResultDesc.FAIL.getResultCode());
                result.setResultDesc("Operation failed, parameters are incorrect.");
                return result;
            }
            // 校验签名信息时，需要计算decimal
            if (!workCreateReqVo.getUserAddress().equals(address)) {
                log.error("[protoDaoCreateWork]  useAddress not equals address workCreateReqVo:{}", JacksonUtil.obj2json(workCreateReqVo));
                result.setResultCode(ResultDesc.FAIL.getResultCode());
                result.setResultDesc("Operation failed, parameters are incorrect.");
                return result;
            }

            String imageUrl = "";
            String workHash = "";
            ImageUtil.HeightAndBgColor heightAndBgColor = null;

            if (workImage != null && !workImage.isEmpty()) {
                String originalFilename = workImage.getOriginalFilename();
                String ext = "";
                if (originalFilename.lastIndexOf(".") > 0) {
                    ext = originalFilename.substring(originalFilename.lastIndexOf("."));
                }
                String imageFileName = CodeUtil.generateCode('F') + ext;
                String dirStr = fileDir;
                File dirPath = new File(dirStr);
                if (!dirPath.exists()) {
                    dirPath.mkdirs();
                }
                String filePath = dirStr + File.separator + imageFileName;
                File imageFile = new File(filePath);
                workImage.transferTo(imageFile);

                workHash = ImageUtil.getMD5(imageFile);

                Work hashWork = workService.selectWorkByHash(workHash);
                if (hashWork != null) {
                    result.setResultCode(ResultDesc.FAIL.getResultCode());
                    result.setResultDesc("Invalid work！The work is duplicate");
                    return result;
                }
                try {
                    heightAndBgColor = ImageUtil.getImageRgb(imageFile);
                    // 文件上传前的名称 处理图片用
                    String image = imageName + ext;
                    s3Service.putImage(ProtoDaoConstant.bucketName + ProtoDaoConstant.workBucketName, imageFile, image, true);

                    imageUrl = urlPrefix + ProtoDaoConstant.workBucketName + "/" + image;
                    workCreateReqVo.setImageUrl(imageUrl);
                    log.info("[[protoDaoCreateWork]] s3DaoLogoUrl:{}", imageUrl);
                } catch (Exception e) {
                    log.error("[[protoDaoCreateWork]]upload image error", e);
                }

            } else {
                //图片为空的情况用默认图片或者dao的logo图片  在铸造work和上传logo那里生成默认的图片和尺寸，看保存到哪里
                workCreateReqVo.setImageUrl(dao.getDaoWorkUrl());
            }


            // 处理uri用
            MintWorkUriDto mintWorkUriDto = MintWorkUriDto.transfer(workCreateReqVo);
            // String sourceString = JacksonUtil.obj2json(mintWorkUriDto);
            // byte[] sourceByte = sourceString.getBytes(StandardCharsets.UTF_8);
            //
            // long second = LocalDateTime.now().toInstant(ZoneOffset.of("+8")).getEpochSecond();
            // String fileName = Md5Utils.md5AsBase64(sourceByte) + String.valueOf(second).substring(5) + ".json";
            // fileName = CommonUtil.replaceOperator(fileName);
            BucketObjectRepresentaion representaion = new BucketObjectRepresentaion();
            representaion.setObjectName(fileName);
            representaion.setText(JacksonUtil.obj2json(mintWorkUriDto));
            s3Service.putObject(
                    ProtoDaoConstant.bucketName + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.workBucketName,
                    representaion);
            log.info("[[protoDaoCreateWork]] s3FileName:{}", s3FileName);

            Work work = new Work();

            work.setWorkDescription(workCreateReqVo.getWorkDescription());
            work.setCreatorAddress(workCreateReqVo.getUserAddress());
            work.setWorkStatus(WorkStatusEnum.NOT_CAST.getStatus());
            work.setCreateTime(LocalDateTime.now());
            work.setCanvasId(CommonUtil.removeHexPrefixIfExists(workCreateReqVo.getCanvasId()));
            work.setCanvasNumber(null);
            work.setCanId(null);
            work.setDaoId(Integer.valueOf(workCreateReqVo.getDaoId()));
            work.setDaoNumber(dao.getDaoNumber());
            work.setProjectId(dao.getProjectId());
            work.setFavoriteAmount(0);
            work.setCreateSignHash(workCreateReqVo.getCreateSignHash());
            work.setWorkUri(s3FileName);
            work.setOwnerAddress(workCreateReqVo.getUserAddress());
            work.setPriceType(workCreateReqVo.getPriceType());// 一口价

            if (workCreateReqVo.getPriceType() != null
                    && workCreateReqVo.getPriceType().equals(WorkPriceTypeEnum.FIXED_PRICE.getType())) {
                if (StringUtils.isNotBlank(workCreateReqVo.getFixedPrice())
                        && !"null".equals(workCreateReqVo.getFixedPrice())) {
                    work.setFixedPrice(new BigDecimal(workCreateReqVo.getFixedPrice()));
                } else {
                    work.setFixedPrice(dao.getDaoFloorPrice());
                }
            }
//            if (StringUtils.isNotBlank(workCreateReqVo.getFixedPrice())
//                    && !"null".equals(workCreateReqVo.getFixedPrice())) {
//                work.setFixedPrice(new BigDecimal(workCreateReqVo.getFixedPrice()));
//            }
            if (workImage != null && !workImage.isEmpty()) {
                if (heightAndBgColor != null) {
                    log.info("[protoDaoCreateWork] workHash:{} height:{} bgColor:{}", workHash, heightAndBgColor.getHeight(),
                            heightAndBgColor.getBgColor());
                    work.setHeight(heightAndBgColor.getHeight());
                    work.setBgColor(heightAndBgColor.getBgColor());
                } else {
                    log.error("image not height error");
                    // work.setHeight(260.0);
                    // work.setBgColor("#FFFFFF");
                    result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
                    result.setResultDesc("Works creation failed, please try again later");
                    return result;
                }
                work.setImageUrl(imageUrl);

                work.setWorkHash(workHash);

            } else {
                if (StringUtils.isBlank(dao.getDaoWorkUrl())) {
                    log.error("daoId:{} not workUrl error", dao.getId());
                    result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
                    result.setResultDesc("Works creation failed, please try again later");
                    return result;
                }
                work.setImageUrl(dao.getDaoWorkUrl());
                work.setWorkHash(dao.getWorkHash());
                work.setHeight(dao.getHeight());
                work.setBgColor(dao.getColor());
            }


            workService.save(work);

            WorkCreateResVo workCreateResVo = new WorkCreateResVo();
            workCreateResVo.setWorkId(work.getId());
            workCreateResVo.setWorkHash(workHash);
            workCreateResVo.setImageUrl(imageUrl);
            workCreateResVo.setDaoName(dao.getDaoName());
            workCreateResVo.setCanvasName(null);
            workCreateResVo.setCanvasCurrentPrice(null);
            if (workCreateReqVo.getPriceType() != null
                    && workCreateReqVo.getPriceType().equals(WorkPriceTypeEnum.FIXED_PRICE.getType())) {
                if (StringUtils.isNotBlank(workCreateReqVo.getFixedPrice())
                        && !"null".equals(workCreateReqVo.getFixedPrice())) {
                    workCreateResVo.setCanvasCurrentPrice(new BigDecimal(workCreateReqVo.getFixedPrice()));
                } else {
                    workCreateResVo.setCanvasCurrentPrice(dao.getDaoFloorPrice());
                }
            }
            result.setData(workCreateResVo);

            return result;
        } catch (FileNotFoundException e) {
            log.error("[[protoDaoCreateWork]]FileNotFoundException ", e);
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("Operation failed, please try again later.");
            return result;
        } catch (IOException e) {
            log.error("[[protoDaoCreateWork]]IOException", e);
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("Operation failed, please try again later.");
            return result;
        } catch (Exception e) {
            log.error("[[protoDaoCreateWork]]Exception", e);
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("Operation failed, please try again later.");
            return result;
        }

    }

    /**
     * 我上传的
     */
    @PostMapping(value = "/creator")
    public Result<WorkListVo> workCreator(@RequestBody(required = false) UserProfilePageReqVo userProfilePageReqVo) {

        Result<WorkListVo> result = new Result<>();
        Page<Work> iPage = new Page<>(userProfilePageReqVo.getPageNo(), userProfilePageReqVo.getPageSize());
        String userAddress = userProfilePageReqVo.getUserAddress();
        Page<Work> workPage = workService.findCreatorByUserAddress(iPage, userAddress);
        List<Work> works = workPage.getRecords();
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && favoritesIds.size() > 0) {
            List<Integer> favoritesIds2 = favoritesIds;
            works.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getId())));
        }
        List<WorkListVo> workListVoList =
                works.stream().map(v -> WorkListVo.transfor(v, null)).collect(Collectors.toList());
        result.setDataList(workListVoList);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(userProfilePageReqVo.getPageNo());
        page.setPageSize(userProfilePageReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);
        return result;

    }

    /**
     * 1.6.1 个人中心界面 我上传的work(图片形式) P1
     */
    @PostMapping(value = "/creator/v2")
    public Result<WorkListVoV2> workCreatorV2(@RequestBody(required = false) UserProfilePageReqVo userProfilePageReqVo) {
        Result<WorkListVoV2> result = new Result<>();

        Page<Work> iPage = new Page<>(userProfilePageReqVo.getPageNo(), userProfilePageReqVo.getPageSize());
        String userAddress = userProfilePageReqVo.getUserAddress();
        Page<Work> workPage = workService.findCreatorByUserAddress(iPage, userAddress);
        List<Work> works = workPage.getRecords();
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && favoritesIds.size() > 0) {
            List<Integer> favoritesIds2 = favoritesIds;
            works.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getId())));
        }
        List<WorkListVoV2> workListVoList =
                works.stream().map(v -> WorkListVoV2.transfor(v, null)).collect(Collectors.toList());
        result.setDataList(workListVoList);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(userProfilePageReqVo.getPageNo());
        page.setPageSize(userProfilePageReqVo.getPageSize());
        page.setCount(0);
        result.setPage(page);
        return result;

    }

    /**
     * 1.5 用户铸造top-up nodes下作品
     *
     * @return MineNftVo
     * @apiNote 获取所有属于我的nft信息
     */
    @PostMapping(value = "/mint/topup")
    public ResultList<MineNftVo> workMintTopUp(HttpServletRequest request, @RequestBody(required = false) DaoIdParam daoIdParam) {
        //获取当前登陆用户，所拥有的nft信息.   注意：nft所属的dao可能不属于该用户. 用户持有的NFT来选择绑定到哪个NFT上
        //先查work表中owner是自己的，然后与amount-workID联查对应的信息。
        ResultList<MineNftVo> result = new ResultList<>();

        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isBlank(userAddress)) {
            result.setResultDesc("please login.");
            result.setResultCode(ResultDesc.USER_ERROR.getResultCode());
            return result;
        }

        Dao dao = daoService.getById(daoIdParam.getDaoId());
        if (dao == null) {
            result.setResultCode(ResultDesc.AUTH_ERROR.getResultCode());
            result.setResultDesc("param is error.");
            return result;
        }

        String existDaoId = StringUtils.isBlank(dao.getExistDaoId()) ? dao.getProjectId() : dao.getExistDaoId();
        dao = daoService.daoDetailByProjectId(existDaoId);

        Page<Work> iPage = new Page<>(daoIdParam.getPageNo(), daoIdParam.getPageSize());
        daoIdParam.setUserAddress(userAddress);

        Page<MineNftVo> workPage = workService.workMintTopUp(iPage, userAddress, existDaoId);
        List<MineNftVo> mineNftVos = workPage.getRecords();

        // 添加聚合dao name
        for (MineNftVo mineNftVo : mineNftVos) {
            mineNftVo.setPayCurrencyType(dao.getPayCurrencyType());
            mineNftVo.setDaoSymbol(dao.getDaoSymbol());
            mineNftVo.setDaoErc20Address(dao.getErc20Token());
            mineNftVo.setInputTokenAddress(dao.getInputTokenAddress());

            if (mineNftVo.getTogetherDaoId() == null) {
                continue;
            }
            Dao daoTogether = daoService.getById(mineNftVo.getTogetherDaoId());
            mineNftVo.setTogetherDaoName(daoTogether.getDaoName());
        }

        result.setDataList(mineNftVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoIdParam.getPageNo());
        page.setPageSize(daoIdParam.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);
        return result;
    }

    /**
     * 1.5 铸造非top-up nodes下作品
     * return MineNftVo
     *
     * @apiNote 获取当前dao下用户可以消费的nft资产，需要传入当前daoID
     */
    @PostMapping(value = "/mint/unTopup")
    public ResultList<MineNftVo> workMintNotTopUp(HttpServletRequest request, @RequestBody(required = false) DaoIdParam daoIdParam) {
        // 我要去消费，查处这个dao下有谁绑定了我的work的信息
        // 先查topup-work
        // 获取当前登陆用户，这个dao下有被mount，且数量>0，且amount_workId所对应的owner是当前登陆用户
        // 是否需要根据dao id 查询这一系列dao下的...

        ResultList<MineNftVo> result = new ResultList<>();
        if (StringUtils.isBlank(daoIdParam.getDaoId())) {
            result.setResultDesc("dao id is null.");
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }

        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isBlank(userAddress)) {
            result.setResultDesc("please login.");
            result.setResultCode(ResultDesc.USER_ERROR.getResultCode());
            return result;
        }

        Dao dao = daoService.getById(daoIdParam.getDaoId());
        if (dao == null) {
            result.setResultCode(ResultDesc.AUTH_ERROR.getResultCode());
            result.setResultDesc("param is error.");
            return result;
        }

        Page<Work> iPage = new Page<>(daoIdParam.getPageNo(), daoIdParam.getPageSize());
        daoIdParam.setUserAddress(userAddress);

        Page<MineNftVo> workPage = workService.workMintNotTopUp(iPage, daoIdParam);
        List<MineNftVo> mineNftVos = workPage.getRecords();

        // 添加聚合dao name
        for (MineNftVo mineNftVo : mineNftVos) {
            mineNftVo.setPayCurrencyType(dao.getPayCurrencyType());
            mineNftVo.setDaoSymbol(dao.getDaoSymbol());
            mineNftVo.setDaoErc20Address(dao.getErc20Token());
            mineNftVo.setInputTokenAddress(dao.getInputTokenAddress());

            if (mineNftVo.getTogetherDaoId() == null) {
                continue;
            }
            Dao daoTogether = daoService.getById(mineNftVo.getTogetherDaoId());
            mineNftVo.setTogetherDaoName(daoTogether.getDaoName());
        }

        result.setDataList(mineNftVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoIdParam.getPageNo());
        page.setPageSize(daoIdParam.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);
        return result;
    }

    /**
     * 1.5 work详情中的top-up balance信息
     *
     * @return WorkNftDetailsVo
     * @apiNote work详情页展示该NFT的top-up balance列表，需传入workID
     */
    @PostMapping(value = "/detail/nft")
    public ResultList<WorkNftDetailsVo> workDetailNft(HttpServletRequest request, @RequestBody(required = false) WorkId workId) {
        ResultList<WorkNftDetailsVo> result = new ResultList<>();
        // 查询谁绑定了我这个work的信息
        // 这个workID=transfer_workId，查询work表与其绑定的workID信息。再查出dao信息...
        if (StringUtils.isBlank(workId.getWorkId())) {
            result.setResultDesc("work id is null.");
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }

        Page<Work> iPage = new Page<>(workId.getPageNo(), workId.getPageSize());

        Page<WorkNftDetailsVo> workPage = workService.workDetailNft(iPage, workId);
        List<WorkNftDetailsVo> workNftDetailsVos = workPage.getRecords();
        result.setDataList(workNftDetailsVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(workId.getPageNo());
        page.setPageSize(workId.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);

        return result;
    }

    /**
     * 1.5 将锁定小时转换为区块高度
     *
     * @return WorkLockDuration
     * @apiNote 将锁定的小时数转换为区块高度(合约参数)
     */
    @PostMapping(value = "/lock/duration")
    public Result<WorkLockDuration> workLockDuration(@RequestBody WorkLockHours workLockHours) {
        Result<WorkLockDuration> result = new Result<>();
        log.info("workLockHours:" + JacksonUtil.obj2json(workLockHours));
        if (workLockHours == null || workLockHours.getHours() == null) {
            result.setResultDesc("lock time is null.");
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }

        if (workLockHours.getHours() < 1 || workLockHours.getHours() > 720) {
            result.setResultDesc("this number is illegal.");
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }

        Result<String> resultBlockNum = iSubscriptionService.ethGetBlockNumber(ProtoDaoConstant.netWork);
        if (resultBlockNum.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[drbInfo] ethGetBlockNumber error:{}", result.getResultDesc());
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            result.setResultDesc("network anomaly！ please try again later!");
            return result;
        }
        WorkLockDuration workLockDuration = new WorkLockDuration();
        workLockDuration.setStartBlock(CommonUtil.hexToTenString(resultBlockNum.getData()));

        // 每小时的出块数量
        BigDecimal blockHours = new BigDecimal(ProtoDaoConstant.etherscanBlockNumber).divide(new BigDecimal("1e18"), 0, RoundingMode.HALF_UP);
        // blockHours*小时= 指定小时出块的数量
        BigDecimal durationBlock = blockHours.multiply(new BigDecimal(workLockHours.getHours()));
        workLockDuration.setDurationBlock(durationBlock.toString());

        result.setData(workLockDuration);
        return result;
    }

    /**
     * 1.10 通过交易hash查询0号NFT的id和node id
     */
    @PostMapping(value = "/transaction/hash")
    public Result<CreateNodeId> transactionHashForWork(HttpServletRequest request, @RequestBody(required = false) DaoTransactionHashReqVo daoTransactionHashReqVo) {

        Result<CreateNodeId> result = new Result<>();
        CreateNodeId createNodeId = new CreateNodeId();

        log.info("[transactionHashForDao] transactionHash:{}", daoTransactionHashReqVo.getTransactionHash());

        //通过交易hash查询dao信息，用于发完交易跳转dao详情
        Dao dao = daoService.selectDaoByTransactionHash(daoTransactionHashReqVo.getTransactionHash());
        if (dao != null) {
            createNodeId.setDaoId(dao.getId());
        }

        //通过交易hash查询dao信息，用于发完交易跳转dao详情
        Work work = workService.selectWorkByTransactionHash(daoTransactionHashReqVo.getTransactionHash());
        if (work != null) {
            createNodeId.setWorkId(work.getId());
            createNodeId.setImgUrl(work.getImageUrl());
            createNodeId.setHeight(work.getHeight());
            createNodeId.setBgColor(work.getBgColor());
        }
        result.setData(createNodeId);
        return result;

    }
}
