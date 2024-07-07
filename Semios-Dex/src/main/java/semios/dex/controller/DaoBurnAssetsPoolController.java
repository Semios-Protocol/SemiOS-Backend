package semios.dex.controller;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import semios.dex.model.dto.common.Result;
import semios.dex.model.dto.common.ResultDesc;
import semios.dex.model.dto.common.ResultList;
import semios.dex.model.dto.feign.DaoErc20Dto;
import semios.dex.model.entity.Erc20Liquidity;
import semios.dex.model.entity.LiquidityDailyStatistics;
import semios.dex.model.entity.LiquidityPriceRecord;
import semios.dex.model.vo.req.Erc20QueryReqVo;
import semios.dex.model.vo.req.Erc20ReqVo;
import semios.dex.model.vo.res.AssetPoolEthResVo;
import semios.dex.model.vo.res.AssetPoolInfoResVo;
import semios.dex.model.vo.res.AssetPoolPriceResVo;
import semios.dex.service.IErc20LiquidityService;
import semios.dex.service.ILiquidityDailyStatisticsService;
import semios.dex.service.ILiquidityPriceRecordService;
import semios.dex.service.feign.IDao4ArtService;
import semios.dex.service.feign.IProtoDaoService;
import semios.dex.utils.DateUtil;
import semios.dex.utils.JacksonUtil;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * dex(burn) 页面AssetsPool
 *
 * @author xiangbin
 */
@Slf4j
@RestController
@RequestMapping("/burn")
public class DaoBurnAssetsPoolController extends BaseController {

    @Autowired
    private ILiquidityDailyStatisticsService liquidityDailyStatisticsService;

    @Autowired
    private ILiquidityPriceRecordService liquidityPriceRecordService;

    @Autowired
    private IDao4ArtService dao4ArtService;

    @Autowired
    private IProtoDaoService protoDaoService;

    @Autowired
    private IErc20LiquidityService erc20LiquidityService;

    /**
     * Asset Pool price信息查询接口
     *
     * @return
     * @apiNote Asset Pool price信息查询
     */
    @PostMapping("/asset_pool/price")
    public Result<AssetPoolPriceResVo> assetPoolPrice(@RequestBody(required = true) Erc20ReqVo erc20ReqVo)
            throws Exception {
        Result<AssetPoolPriceResVo> result = new Result<>();
        if (!checkAddress(erc20ReqVo.getErc20Address())) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc() + "wrong erc20 address.");
            return result;
        }
        String erc20Address = formatAddress(erc20ReqVo.getErc20Address());

        List<String> erc20AddressList = new ArrayList<String>();
        erc20AddressList.add(erc20Address);
        ResultList<DaoErc20Dto> daoErc20Result = protoDaoService.getDaoErcInfo(erc20AddressList);
        log.info("ProtoDao接口调用-获取dao信息-返回结果daoErc20Result：{}", JacksonUtil.obj2json(daoErc20Result));
        if (ResultDesc.SUCCESS.getResultCode() != daoErc20Result.getResultCode()) {
            throw new Exception("error get dao info from dao4art");
        }
        DaoErc20Dto daoErc20Dto = null;
        if (!daoErc20Result.getDataList().isEmpty()){
            daoErc20Dto = daoErc20Result.getDataList().get(0);
        }


        AssetPoolPriceResVo assetPoolPriceResVo = new AssetPoolPriceResVo();
        assetPoolPriceResVo.setErc20Address(erc20Address);


        if (daoErc20Dto !=null && "ETH".equals(daoErc20Dto.getPayCurrencyType())){
            LiquidityPriceRecord priceRecord = liquidityPriceRecordService.getLatestAssetPoolPrice(erc20Address);

            if (priceRecord != null && priceRecord.getPrice() != null) {
                assetPoolPriceResVo.setAssetPoolPrice(priceRecord.getPrice().stripTrailingZeros().toPlainString());
            }
        }else {
            assetPoolPriceResVo.setAssetPoolPrice("0");
        }

        result.setData(assetPoolPriceResVo);

        return result;
    }

    /**
     * Asset Pool info信息查询接口
     *
     * @return
     * @apiNote Asset Pool info信息查询
     */
    @PostMapping("/asset_pool/info")
    public Result<AssetPoolInfoResVo> assetPoolInfo(@RequestBody(required = true) Erc20QueryReqVo erc20QueryReqVo)
            throws Exception {
        Result<AssetPoolInfoResVo> result = new Result<>();
        if (!checkAddress(erc20QueryReqVo.getErc20Address())) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc() + "wrong erc20 address.");
            return result;
        }
        String erc20Address = formatAddress(erc20QueryReqVo.getErc20Address());

        List<String> erc20AddressList = new ArrayList<String>();
        erc20AddressList.add(erc20Address);

        ResultList<DaoErc20Dto> daoErc20Result;
        Erc20Liquidity erc20Liquidity = erc20LiquidityService.selectErc20LiquidityByErc20Address(erc20Address);
        if (erc20Liquidity.getAppSource() == 1) {
            log.info("[asset_pool_info]Dao4Art接口调用-获取dao信息-请求参数：{}", erc20AddressList);
            daoErc20Result = dao4ArtService.getDaoErcInfo(erc20AddressList);
            log.info("[asset_pool_info]Dao4Art接口调用-获取dao信息-返回结果daoErc20Result：{}", JacksonUtil.obj2json(daoErc20Result));
            if (ResultDesc.SUCCESS.getResultCode() != daoErc20Result.getResultCode()) {
                throw new Exception("error get dao info from dao4art");
            }
        } else {
            log.info("[asset_pool_info]ProtoDao接口调用-获取dao信息-请求参数：{}", erc20AddressList);
            daoErc20Result = protoDaoService.getDaoErcInfo(erc20AddressList);
            log.info("[asset_pool_info]ProtoDao接口调用-获取dao信息-返回结果daoErc20Result：{}", JacksonUtil.obj2json(daoErc20Result));
            if (ResultDesc.SUCCESS.getResultCode() != daoErc20Result.getResultCode()) {
                throw new Exception("error get dao info from dao4art");
            }
        }

        if (daoErc20Result.getDataList() != null && daoErc20Result.getDataList().size() > 0) {
            DaoErc20Dto daoErc20Dto = daoErc20Result.getDataList().get(0);

            AssetPoolInfoResVo assetPoolInfoResVo = new AssetPoolInfoResVo();
            assetPoolInfoResVo.setEthInPool(decimal2String(daoErc20Dto.getEthInPool()));
            assetPoolInfoResVo.setBurnVolume(decimal2String(daoErc20Dto.getBurnVolume()));
            assetPoolInfoResVo.setDaoAssetPool(daoErc20Dto.getDaoAssetPool());
            result.setData(assetPoolInfoResVo);
        }


        return result;
    }

    /**
     * Asset Pool ETH图表数据查询接口
     *
     * @return
     * @apiNote Asset Pool ETH图表数据查询
     */
    @PostMapping("/asset_pool/eth")
    public Result<AssetPoolEthResVo> assetPoolEth(@RequestBody(required = true) Erc20QueryReqVo erc20QueryReqVo) {
        Result<AssetPoolEthResVo> result = new Result<AssetPoolEthResVo>();
        if (!checkAddress(erc20QueryReqVo.getErc20Address())) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc() + "wrong erc20 address.");
            return result;
        }
        String erc20Address = formatAddress(erc20QueryReqVo.getErc20Address());
        Date today = DateUtil.getBeginOfToday();
        long startDate = DateUtil.getIntervalTime(today, (erc20QueryReqVo.getDayTime() - 1) * -1);
        long endDate = DateUtil.getBeginOfNextHour().getTime() / 1000;
        List<LiquidityDailyStatistics> dailyStatisticslist =
                liquidityDailyStatisticsService.getErc20DailyStatistics(erc20Address, startDate, endDate);

        List<Long> time = new ArrayList<Long>();
        List<String> totalAmount = new ArrayList<String>();
        List<String> incomes = new ArrayList<String>();
        List<String> costs = new ArrayList<String>();
        List<String> changes = new ArrayList<String>();

        BigDecimal maxTotalAmount = BigDecimal.ZERO;
        BigDecimal maxIncomes = BigDecimal.ZERO;
        BigDecimal maxCosts = BigDecimal.ZERO;
        BigDecimal maxChanges = BigDecimal.ZERO;

        for (LiquidityDailyStatistics dailyStatistics : dailyStatisticslist) {
            BigDecimal assetPoolTotalAmount = dailyStatistics.getAssetPoolTotalAmount();
            BigDecimal assetPoolIncome = dailyStatistics.getAssetPoolIncome();
            BigDecimal assetPoolCost = dailyStatistics.getAssetPoolCost();
            BigDecimal assetPoolVariation = dailyStatistics.getAssetPoolVariation();
            time.add(dailyStatistics.getRecordTime());
            totalAmount.add(assetPoolTotalAmount.stripTrailingZeros().toPlainString());
            incomes.add(assetPoolIncome.stripTrailingZeros().toPlainString());
            costs.add(assetPoolCost.stripTrailingZeros().toPlainString());
            changes.add(assetPoolVariation.stripTrailingZeros().toPlainString());
            if (assetPoolTotalAmount.compareTo(maxTotalAmount) > 0) {
                maxTotalAmount = assetPoolTotalAmount;
            }
            if (assetPoolIncome.compareTo(maxIncomes) > 0) {
                maxIncomes = assetPoolIncome;
            }
            if (assetPoolCost.compareTo(maxCosts) > 0) {
                maxCosts = assetPoolCost;
            }
            if (assetPoolVariation.compareTo(maxChanges) > 0) {
                maxChanges = assetPoolVariation;
            }
        }

        AssetPoolEthResVo assetPoolEthResVo = new AssetPoolEthResVo();
        assetPoolEthResVo.setTime(time);
        assetPoolEthResVo.setTotalAmount(totalAmount);
        assetPoolEthResVo.setIncomes(incomes);
        assetPoolEthResVo.setCosts(costs);
        assetPoolEthResVo.setChanges(changes);
        assetPoolEthResVo.setMaxTotalAmount(maxTotalAmount.stripTrailingZeros().toPlainString());
        assetPoolEthResVo.setMaxIncomes(maxIncomes.stripTrailingZeros().toPlainString());
        assetPoolEthResVo.setMaxCosts(maxCosts.stripTrailingZeros().toPlainString());
        assetPoolEthResVo.setMaxChanges(maxChanges.stripTrailingZeros().toPlainString());
        result.setData(assetPoolEthResVo);

        return result;
    }

}
