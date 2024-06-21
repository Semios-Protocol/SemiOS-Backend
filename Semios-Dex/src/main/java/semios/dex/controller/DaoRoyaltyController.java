package semios.dex.controller;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import semios.dex.model.dto.common.DexConstant;
import semios.dex.model.dto.common.Result;
import semios.dex.model.dto.common.ResultDesc;
import semios.dex.model.dto.common.ResultList;
import semios.dex.model.dto.feign.UserInfoDto;
import semios.dex.model.dto.response.AssetPoolPriceResDto;
import semios.dex.model.entity.UserLiquidityStatistics;
import semios.dex.model.vo.req.Erc20QueryReqVo;
import semios.dex.model.vo.res.DaoErc20OwnersResVo;
import semios.dex.model.vo.res.PriceFoldLineResVo;
import semios.dex.service.IErc20LiquidityService;
import semios.dex.service.ILiquidityPriceRecordService;
import semios.dex.service.ILiquidityTransactionService;
import semios.dex.service.IUserLiquidityStatisticsService;
import semios.dex.service.feign.IDao4ArtService;
import semios.dex.service.feign.IProtoDaoService;
import semios.dex.utils.DateUtil;
import semios.dex.utils.JacksonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * dao Royalty页面展示接口
 *
 * @author xiangbin
 */
@Slf4j
@RestController
@RequestMapping("/royalty")
public class DaoRoyaltyController extends BaseController {

    @Autowired
    private ILiquidityPriceRecordService liquidityPriceRecordService;

    @Autowired
    private ILiquidityTransactionService liquidityTransactionService;

    @Autowired
    private IUserLiquidityStatisticsService userLiquidityStatisticsService;

    @Autowired
    private IErc20LiquidityService erc20LiquidityService;

    @Autowired
    private IDao4ArtService dao4ArtService;

    @Autowired
    private IProtoDaoService protoDaoService;


    /**
     * erc20 Owner图表数据查询接口（返回数据集合dataList）
     *
     * @return
     * @apiNote erc20 Owner图表数据查询
     */
    @PostMapping("/erc20/owners")
    public ResultList<DaoErc20OwnersResVo> erc20Owners(@RequestBody(required = true) Erc20QueryReqVo erc20QueryReqVo)
            throws Exception {
        ResultList<DaoErc20OwnersResVo> result = new ResultList<>();
        if (!checkAddress(erc20QueryReqVo.getErc20Address())) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc() + "wrong erc20 address.");
            return result;
        }
        String erc20Address = formatAddress(erc20QueryReqVo.getErc20Address());
        List<UserLiquidityStatistics> liquidityStatisticsList =
                userLiquidityStatisticsService.getLiquidityStatisticsByErc20Address(erc20Address);

        if (liquidityStatisticsList.size() == 0) {
            result.setDataList(new ArrayList<>());
            return result;
        }

        BigDecimal distributedSupply = userLiquidityStatisticsService.getSumBalanceByErc20Address(erc20Address);

        List<String> userAddressList = new ArrayList<>();
        List<DaoErc20OwnersResVo> daoErc20OwnersList = new ArrayList<>();
        for (UserLiquidityStatistics liquidityStatistics : liquidityStatisticsList) {
            DaoErc20OwnersResVo erc20OwnersResVo = new DaoErc20OwnersResVo();
            BigDecimal erc20Balance = liquidityStatistics.getErc20Balance();
            userAddressList.add(liquidityStatistics.getUserAddress());
            erc20OwnersResVo.setUserAddress(liquidityStatistics.getUserAddress());
            erc20OwnersResVo.setIsContract(liquidityStatistics.getIsContract());
            erc20OwnersResVo.setErc20Balance(erc20Balance.stripTrailingZeros().toPlainString());
            erc20OwnersResVo
                    .setPercentage(erc20Balance.divide(distributedSupply, DexConstant.PERCENT_DECIMAL, RoundingMode.HALF_UP)
                            .multiply(new BigDecimal("100")).stripTrailingZeros().toPlainString());
            daoErc20OwnersList.add(erc20OwnersResVo);
        }
        ResultList<UserInfoDto> userInfoResult;
        log.info("owners erc20Address:" + erc20Address);
//        Erc20Liquidity erc20Liquidity = erc20LiquidityService.selectErc20LiquidityByErc20Address(erc20Address);
//        log.info("get erc20Address:{},erc20Liquidity:{}",erc20Address,JacksonUtil.obj2json(erc20Liquidity));
//
//        if (erc20Liquidity.getAppSource() == 1) {
//            log.info("Dao4Art接口调用-获取user信息-请求参数：{}", userAddressList);
//            userInfoResult = dao4ArtService.getUserInfo(userAddressList);
//            log.info("Dao4Art接口调用-获取user信息-返回结果userInfoResult：{}", JacksonUtil.obj2json(userInfoResult));
//            if (ResultDesc.SUCCESS.getResultCode() != userInfoResult.getResultCode()) {
//                throw new Exception("error get user info from dao4art");
//            }
//        } else {
//            log.info("ProtoDao接口调用-获取user信息-请求参数：{}", userAddressList);
//            userInfoResult = protoDaoService.getUserInfo(userAddressList);
//            log.info("ProtoDao接口调用-获取user信息-返回结果userInfoResult：{}", JacksonUtil.obj2json(userInfoResult));
//            if (ResultDesc.SUCCESS.getResultCode() != userInfoResult.getResultCode()) {
//                throw new Exception("error get user info from dao4art");
//            }
//        }
        // 不用调用dao4a服务
        log.info("ProtoDao接口调用-获取user信息-请求参数：{}", userAddressList);
        userInfoResult = protoDaoService.getUserInfo(userAddressList);
        log.info("ProtoDao接口调用-获取user信息-返回结果userInfoResult：{}", JacksonUtil.obj2json(userInfoResult));
        if (ResultDesc.SUCCESS.getResultCode() != userInfoResult.getResultCode()) {
            throw new Exception("error get user info from dao4art");
        }

        // user info map
        Map<String, UserInfoDto> userInfoHash =
                userInfoResult.getDataList().stream().collect(Collectors.toMap(UserInfoDto::getUserAddress, e -> e));

        for (DaoErc20OwnersResVo erc20OwnersResVo : daoErc20OwnersList) {
            String userAddress = erc20OwnersResVo.getUserAddress();
            if (userInfoHash.containsKey(userAddress)) {
                erc20OwnersResVo.setAvatarAddress(userInfoHash.get(userAddress).getAvatarAddress());
                erc20OwnersResVo.setUserName(userInfoHash.get(userAddress).getUserName());
            }
        }
        result.setDataList(daoErc20OwnersList);


        return result;
    }

    /**
     * Asset Pool的price图表数据查询接口
     *
     * @return
     * @apiNote Asset Pool的price图表数据查询
     */
    @PostMapping("/price/asset_pool")
    public Result<PriceFoldLineResVo> priceOnAssetPool(@RequestBody(required = true) Erc20QueryReqVo erc20QueryReqVo) {
        Result<PriceFoldLineResVo> result = new Result<PriceFoldLineResVo>();
        if (!checkAddress(erc20QueryReqVo.getErc20Address())) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc() + "wrong erc20 address.");
            return result;
        }
        String erc20Address = formatAddress(erc20QueryReqVo.getErc20Address());
        Date today = DateUtil.getBeginOfToday();
        long startDate = DateUtil.getIntervalTime(today, (erc20QueryReqVo.getDayTime() - 1) * -1);
        long endDate = DateUtil.getBeginOfNextHour().getTime() / 1000;
        List<AssetPoolPriceResDto> priceRecordList =
                liquidityPriceRecordService.getHoursAssetPoolPrice(erc20Address, startDate, endDate);

        long currentHour = DateUtil.getBeginOfCurrentHour().getTime() / 1000;
        BigDecimal latestTradingVolume = liquidityTransactionService.getBurnVolume(erc20Address, currentHour, endDate);

        List<Long> time = new ArrayList<Long>();
        List<String> volume = new ArrayList<String>();
        List<String> price = new ArrayList<String>();

        BigDecimal maxVolume = latestTradingVolume == null ? BigDecimal.ZERO : latestTradingVolume;
        BigDecimal maxPrice = BigDecimal.ZERO;

        if (priceRecordList.size() > 0) {
            Map<Long, AssetPoolPriceResDto> priceMap =
                    priceRecordList.stream().collect(Collectors.toMap(AssetPoolPriceResDto::getRecordTime, e -> e));

            AssetPoolPriceResDto assetPoolPriceResDto = priceRecordList.get(0);
            BigDecimal currentPrice = assetPoolPriceResDto.getPrice();

            maxPrice = currentPrice;
            if (assetPoolPriceResDto.getRecordTime() <= startDate) {
                time = DateUtil.getHoursList(startDate, endDate);
            } else {
                time = DateUtil.getHoursList(assetPoolPriceResDto.getRecordTime(), endDate);
            }

            for (int i = 0; i < time.size(); i++) {
                if (i == time.size() - 1) {
                    volume.add(
                            latestTradingVolume == null ? "0" : latestTradingVolume.stripTrailingZeros().toPlainString());
                    price.add(currentPrice.stripTrailingZeros().toPlainString());
                } else {
                    AssetPoolPriceResDto priceRecord = priceMap.get(time.get(i));
                    if (priceRecord != null) {
                        BigDecimal tradingVolume = priceRecord.getTradingVolume();
                        BigDecimal recordPrice = priceRecord.getPrice();
                        volume.add(tradingVolume.stripTrailingZeros().toPlainString());
                        price.add(recordPrice.stripTrailingZeros().toPlainString());
                        if (tradingVolume.compareTo(maxVolume) > 0) {
                            maxVolume = tradingVolume;
                        }
                        if (recordPrice.compareTo(maxPrice) > 0) {
                            maxPrice = recordPrice;
                        }
                        currentPrice = recordPrice;
                    } else {
                        volume.add("0");
                        price.add(currentPrice.stripTrailingZeros().toPlainString());
                    }
                }
            }
        }

        PriceFoldLineResVo priceFoldLineResVo = new PriceFoldLineResVo();
        priceFoldLineResVo.setTime(time);
        priceFoldLineResVo.setVolume(volume);
        priceFoldLineResVo.setPrice(price);
        priceFoldLineResVo.setMaxVolume(maxVolume.stripTrailingZeros().toPlainString());
        priceFoldLineResVo.setMaxPrice(maxPrice.stripTrailingZeros().toPlainString());
        result.setData(priceFoldLineResVo);

        return result;
    }

}
