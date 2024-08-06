package semios.api.model.vo.res.BaseWorkVo;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import semios.api.interceptor.S3Service;
import semios.api.model.bo.WorkCountBo;
import semios.api.model.dto.chain.DaoEthRoyaltyToken;
import semios.api.model.dto.chain.DaoReserveRatio;
import semios.api.model.dto.chain.DaoRoyaltyToken;
import semios.api.model.dto.common.BucketObjectRepresentaion;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.response.NewCanvasUriDto;
import semios.api.model.entity.*;
import semios.api.model.enums.DaoRoyaltyTypeEnum;
import semios.api.model.enums.TrueOrFalseEnum;
import semios.api.model.enums.WorkStatusEnum;
import semios.api.model.vo.req.CanvasCreateReqVo;
import semios.api.model.vo.res.WorkDetailResVo;
import semios.api.service.ICanvasService;
import semios.api.service.IDaoAllocationStrategyService;
import semios.api.service.IDaoDrbStatisticsService;
import semios.api.service.IWorkService;
import semios.api.service.common.CommonService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;
import semios.api.utils.ProtoDaoCommonUtil;
import semios.api.utils.SpringBeanUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;


@Data
@Slf4j
public class BaseWorkListVo {

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


    public static BaseWorkListVo transfer(Work work, Dao dao) {
        IWorkService workService = SpringBeanUtil.getBean(IWorkService.class);
        IDaoDrbStatisticsService daoDrbStatisticsService = SpringBeanUtil.getBean(IDaoDrbStatisticsService.class);
        CommonService commonService = SpringBeanUtil.getBean(CommonService.class);
        IDaoAllocationStrategyService daoAllocationStrategyService = SpringBeanUtil.getBean(IDaoAllocationStrategyService.class);
        ICanvasService canvasService = SpringBeanUtil.getBean(ICanvasService.class);
        if (workService == null || daoAllocationStrategyService == null || daoDrbStatisticsService == null || commonService == null || canvasService == null) {
            return new BaseWorkListVo();
        }

        Canvas canvas = generateCanvas(work, dao);
        if (canvas == null) {
            return new BaseWorkListVo();
        }

        WorkDetailResVo workDetailRes = WorkDetailResVo.transfer(work, dao, canvas);

        BaseWorkListVo workDetailResVo = new BaseWorkListVo();
        WorkCountBo workCountBo = workService.selectDrbNftOwnerCountByDaoId(String.valueOf(dao.getId()), Integer.valueOf(dao.getCurrentRound()));
        workDetailResVo.setMintedWorks(workCountBo.getMintedWorks());
        workDetailResVo.setMinters(workCountBo.getMinters());
        workDetailResVo.setMintFee(StringUtils.isBlank(workCountBo.getMintFee()) ? "0" : workCountBo.getMintFee());

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
            log.info("开启了top up:projectId:{}", dao.getProjectId());
            log.info("setInternalRewardToken:{}", workDetailResVo.getBlockRewardToken());
            workDetailResVo.setInternalRewardToken(workDetailResVo.getBlockRewardToken());
            log.info("setInternalRewardEth:{}", workDetailResVo.getBlockRewardEth());
            workDetailResVo.setInternalRewardEth(workDetailResVo.getBlockRewardEth());
        } else {
            log.info("没有开启top up:projectId:{}", dao.getProjectId());
            List<DaoAllocationStrategy> daoAllocationStrategies = daoAllocationStrategyService.selectByOriginProjectIdAndType(dao.getProjectId(), null);
            log.info("从数据库中娶到的daoAllocationStrategies:" + JacksonUtil.obj2json(daoAllocationStrategies));
            DaoAllocationStrategy daoAllocationStrategy = new DaoAllocationStrategy();
            daoAllocationStrategy.setRoyaltyProportion(BigDecimal.ONE);
            DaoAllocationStrategy daoAllocationStrategyToken = daoAllocationStrategies.stream().filter(v -> TrueOrFalseEnum.FALSE.getStatus().equals(v.getType()) && DaoRoyaltyTypeEnum.TWO.getType().equals(v.getRoyaltyType())).findFirst().orElseGet(() -> daoAllocationStrategy);
            log.info("daoAllocationStrategyToken:{}", JacksonUtil.obj2json(daoAllocationStrategyToken));
            DaoAllocationStrategy daoAllocationStrategyEth = daoAllocationStrategies.stream().filter(v -> TrueOrFalseEnum.TRUE.getStatus().equals(v.getType()) && DaoRoyaltyTypeEnum.TWO.getType().equals(v.getRoyaltyType())).findFirst().orElseGet(() -> daoAllocationStrategy);
            log.info("daoAllocationStrategyEth:{}", JacksonUtil.obj2json(daoAllocationStrategyEth));

            log.info("setInternalRewardToken:workDetailResVo.getBlockRewardToken():{}---daoAllocationStrategyToken.getRoyaltyProportion():{}", workDetailResVo.getBlockRewardToken(), daoAllocationStrategyToken.getRoyaltyProportion());
            workDetailResVo.setInternalRewardToken(ProtoDaoCommonUtil.bigdecimalToString(new BigDecimal(workDetailResVo.getBlockRewardToken()).multiply(ProtoDaoCommonUtil.bigdecimalPercentage(daoAllocationStrategyToken.getRoyaltyProportion()))));

            log.info("setInternalRewardEth:workDetailResVo.getBlockRewardEth():{}---daoAllocationStrategyEth.getRoyaltyProportion():{}", workDetailResVo.getBlockRewardEth(), daoAllocationStrategyEth.getRoyaltyProportion());
            workDetailResVo.setInternalRewardEth(ProtoDaoCommonUtil.bigdecimalToString(new BigDecimal(workDetailResVo.getBlockRewardEth()).multiply(ProtoDaoCommonUtil.bigdecimalPercentage(daoAllocationStrategyEth.getRoyaltyProportion()))));
        }

        //interreqward * mint reward * 贡献度
        //贡献度 = 当前价格 * minterReward / 总的价格* mintereward + 当前价格 * minterReward

        if (WorkStatusEnum.NOT_CAST.getStatus().equals(work.getWorkStatus())) {
            log.info("work未铸造:{}", work.getId());
            // 修改(未知正确) WorkDetailResVo.java
//            boolean fixedPrice = work.getFixedPrice() != null;
//            DaoReserveRatio daoReserveRatio = new DaoReserveRatio(fixedPrice);
//            String reserveRatio = fixedPrice ? dao.getFixedReserveRatio() : dao.getUnfixedReserveRatio();
//            if (StringUtils.isNotBlank(reserveRatio)) {
//                daoReserveRatio = JacksonUtil.json2pojo(reserveRatio, DaoReserveRatio.class);
//            }

            DaoReserveRatio daoReserveRatio = workDetailRes.getReserveRatio();
            log.info("maxPrice计算方法:workDetailRes.getPrice():{}", workDetailRes.getPrice());
            BigDecimal maxPrice = new BigDecimal(workDetailRes.getPrice()).compareTo(BigDecimal.ZERO) == 0 ? BigDecimal.ONE : new BigDecimal(workDetailRes.getPrice());
            log.info("maxPrice:{}", maxPrice);

            log.info("mintFee计算方法:daoReserveRatio.getDaoMintFee():{}", daoReserveRatio.getDaoMintFee());
            BigDecimal mintFee = daoReserveRatio.getDaoMintFee() == null || daoReserveRatio.getDaoMintFee().compareTo(BigDecimal.ZERO) == 0 ? BigDecimal.TEN.multiply(BigDecimal.TEN) : daoReserveRatio.getDaoMintFee();
            log.info("mintFee:{}", mintFee);

            log.info("mintersMaxRewardEth计算方法:maxPrice:{}---mintFee:{}", maxPrice, mintFee);
            BigDecimal mintersMaxRewardEth = maxPrice.multiply(ProtoDaoCommonUtil.bigdecimalPercentage(mintFee));
            log.info("mintersMaxRewardEth:{}", mintersMaxRewardEth);
            //BigDecimal maxPrice = new BigDecimal(ProtoDaoCommonUtil.bigdecimalToString(dao.getGlobalDaoPrice())).compareTo(BigDecimal.ZERO) == 0 ? BigDecimal.ONE : new BigDecimal(ProtoDaoCommonUtil.bigdecimalToString(dao.getGlobalDaoPrice()));
            //BigDecimal mintFee = daoReserveRatio.getDaoMintFee() == null || daoReserveRatio.getDaoMintFee().compareTo(BigDecimal.ZERO) == 0 ? BigDecimal.TEN.multiply(BigDecimal.TEN) : daoReserveRatio.getDaoMintFee();
            //BigDecimal mintersMaxRewardEth = maxPrice.multiply(ProtoDaoCommonUtil.bigdecimalPercentage(mintFee));
            //区分登录和不登录两种情况

            //1查询minter分配比例  查询分配比例

            DaoRoyaltyToken daoRoyaltyToken = new DaoRoyaltyToken();
            if (StringUtils.isNotBlank(dao.getRoyaltyToken())) {
                daoRoyaltyToken = JacksonUtil.json2pojo(dao.getRoyaltyToken(), DaoRoyaltyToken.class);
            }
            //ETH
            DaoEthRoyaltyToken daoEthRoyaltyToken = new DaoEthRoyaltyToken();
            if (StringUtils.isNotBlank(dao.getEthRoyaltyToken())) {
                daoEthRoyaltyToken = JacksonUtil.json2pojo(dao.getEthRoyaltyToken(), DaoEthRoyaltyToken.class);
            }
            BigDecimal royaltyToken = daoRoyaltyToken.getMinterReward();
            royaltyToken = royaltyToken == null ? BigDecimal.ZERO : royaltyToken;
            log.info("royaltyToken:{}", royaltyToken);
            BigDecimal royaltyEth = daoEthRoyaltyToken.getMinterETHReward();
            royaltyEth = royaltyEth == null ? BigDecimal.ZERO : royaltyEth;
            log.info("royaltyEth:{}", royaltyEth);

            if (daoDrbStatistics != null && daoDrbStatistics.getContribution() != null && daoDrbStatistics.getContribution().compareTo(BigDecimal.ZERO) > 0) {
                log.info("BaseWorkListVo transfer setMintersMaxRewardToken: daoDrbStatistics != null,mintersMaxRewardEth:{}--daoDrbStatistics.getContribution():{}:workDetailResVo.getInternalRewardToken():{}---royaltyToken:{}", mintersMaxRewardEth, daoDrbStatistics.getContribution(), workDetailResVo.getInternalRewardToken(), royaltyToken);
                workDetailResVo.setMintersMaxRewardToken(ProtoDaoCommonUtil.bigdecimalToString(mintersMaxRewardEth.divide(daoDrbStatistics.getContribution().add(mintersMaxRewardEth), 6, RoundingMode.HALF_UP).multiply(new BigDecimal(workDetailResVo.getInternalRewardToken())).multiply(ProtoDaoCommonUtil.bigdecimalPercentage(royaltyToken))));
                log.info("BaseWorkListVo transfer setMintersMaxRewardEth: daoDrbStatistics != null,mintersMaxRewardEth:{}--daoDrbStatistics.getContribution():{}:workDetailResVo.getInternalRewardToken():{}---royaltyToken:{}", mintersMaxRewardEth, daoDrbStatistics.getContribution(), workDetailResVo.getInternalRewardEth(), royaltyEth);
                workDetailResVo.setMintersMaxRewardEth(ProtoDaoCommonUtil.bigdecimalToString(mintersMaxRewardEth.divide(daoDrbStatistics.getContribution().add(mintersMaxRewardEth), 6, RoundingMode.HALF_UP).multiply(new BigDecimal(workDetailResVo.getInternalRewardEth())).multiply(ProtoDaoCommonUtil.bigdecimalPercentage(royaltyEth))));
            } else {
                log.info("BaseWorkListVo transfer setMintersMaxRewardToken: workDetailResVo.getInternalRewardToken():{}--royaltyToken:{}", workDetailResVo.getInternalRewardToken(), royaltyToken);
                workDetailResVo.setMintersMaxRewardToken(new BigDecimal(workDetailResVo.getInternalRewardToken()).multiply(ProtoDaoCommonUtil.bigdecimalPercentage(royaltyToken)).stripTrailingZeros().toPlainString());
                log.info("BaseWorkListVo transfer setMintersMaxRewardEth: workDetailResVo.getInternalRewardEth():{}--royaltyEth:{}", workDetailResVo.getInternalRewardEth(), royaltyEth);
                workDetailResVo.setMintersMaxRewardEth(new BigDecimal(workDetailResVo.getInternalRewardEth()).multiply(ProtoDaoCommonUtil.bigdecimalPercentage(royaltyEth)).stripTrailingZeros().toPlainString());
            }
        } else {
            workDetailResVo.setMintersMaxRewardToken(null);
            workDetailResVo.setMintersMaxRewardEth(null);
        }

        return workDetailResVo;
    }


    public static Canvas generateCanvas(Work work, Dao dao) {
        ICanvasService canvasService = SpringBeanUtil.getBean(ICanvasService.class);
        CommonService commonService = SpringBeanUtil.getBean(CommonService.class);
        S3Service s3Service = SpringBeanUtil.getBean(S3Service.class);
        if (canvasService == null || commonService == null || s3Service == null) {
            return null;
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


                } catch (Exception e) {
                    return null;
                }
            }
        }
        return canvas;
    }
}
