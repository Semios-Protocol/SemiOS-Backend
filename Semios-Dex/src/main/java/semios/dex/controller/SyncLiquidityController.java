package semios.dex.controller;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import semios.dex.model.dto.common.Result;
import semios.dex.model.entity.Erc20Liquidity;
import semios.dex.model.entity.LiquidityTransaction;
import semios.dex.model.entity.UserLiquidityStatistics;
import semios.dex.model.enums.OneOrZeroEnum;
import semios.dex.model.vo.req.Erc20LiquidityReqVo;
import semios.dex.model.vo.req.LiquidityTransactionReqVo;
import semios.dex.model.vo.req.UserLiquidityStatisticsVo;
import semios.dex.service.IErc20LiquidityService;
import semios.dex.service.ILiquidityTransactionService;
import semios.dex.service.IUserLiquidityStatisticsService;
import semios.dex.utils.JacksonUtil;

import java.math.BigDecimal;

/**
 * @description: sync
 * @author: xiangbin
 * @create: 2023-05-19 16:49
 **/
@Slf4j
@RestController
@RequestMapping("/sync")
public class SyncLiquidityController {

    @Autowired
    private IErc20LiquidityService erc20LiquidityService;

    @Autowired
    private ILiquidityTransactionService liquidityTransactionService;

    @Autowired
    private IUserLiquidityStatisticsService userLiquidityStatisticsService;

    public static void main(String[] args) {

        LiquidityTransactionReqVo liquidityTransactionReqVo = new LiquidityTransactionReqVo();
        LiquidityTransaction liquidityTransaction = new LiquidityTransaction();
        liquidityTransactionReqVo.setInTokenAmount(BigDecimal.ONE);
        liquidityTransactionReqVo.setOutTokenAmount(BigDecimal.ONE);

        BeanUtils.copyProperties(liquidityTransactionReqVo, liquidityTransaction);

        System.out.println(JacksonUtil.obj2json(liquidityTransaction));
    }

    /**
     * dao开始时同步erc20信息
     *
     * @return Result  Integer
     */
    @PostMapping("/erc20")
    public Result<Integer> syncErc20(@RequestBody(required = false) Erc20LiquidityReqVo erc20LiquidityReqVo) {
        Result<Integer> result = new Result<>();

        log.info("[syncErc20] erc20Address:{} erc20LiquidityReqVo:{}", erc20LiquidityReqVo.getErc20Address(), JacksonUtil.obj2json(erc20LiquidityReqVo));
        Erc20Liquidity erc20Liquidity = erc20LiquidityService.selectErc20LiquidityByErc20Address(erc20LiquidityReqVo.getErc20Address());
        if (erc20Liquidity != null) {
            log.info("[syncErc20] existed erc20Address:{}", erc20LiquidityReqVo.getErc20Address());
            erc20Liquidity.setDaoId(erc20LiquidityReqVo.getDaoId());
            erc20Liquidity.setErc20Address(erc20LiquidityReqVo.getErc20Address());
            erc20Liquidity.setErc20Name(erc20LiquidityReqVo.getErc20Name());
            erc20Liquidity.setErc20Symbol(erc20LiquidityReqVo.getErc20Symbol());
            erc20Liquidity.setProjectId(erc20LiquidityReqVo.getProjectId());
            erc20Liquidity.setDaoStatus(erc20LiquidityReqVo.getDaoStatus());
            erc20Liquidity.setErc721Address(erc20LiquidityReqVo.getErc721Address());
            erc20Liquidity.setDaoVersion(erc20LiquidityReqVo.getDaoVersion());
            erc20Liquidity.setAppSource(2);
            if (StringUtils.isNotBlank(erc20LiquidityReqVo.getErc20BlockTime())) {
                erc20Liquidity.setErc20BlockTime(Long.valueOf(erc20LiquidityReqVo.getErc20BlockTime()));
            }
            boolean updateResult = erc20LiquidityService.updateById(erc20Liquidity);
            log.info("[syncErc20] erc20Address:{} updateResult:{}", erc20LiquidityReqVo.getErc20Address(), updateResult);
            result.setData(updateResult ? 1 : 0);
            result.setData(1);
            return result;
        }
        erc20Liquidity = new Erc20Liquidity();
        BeanUtils.copyProperties(erc20LiquidityReqVo, erc20Liquidity);
        erc20Liquidity.setAppSource(2);
        erc20Liquidity.setDaoStatus(erc20LiquidityReqVo.getDaoStatus());
        if (StringUtils.isNotBlank(erc20LiquidityReqVo.getErc20BlockTime())) {
            erc20Liquidity.setErc20BlockTime(Long.valueOf(erc20LiquidityReqVo.getErc20BlockTime()));
        }
        boolean saveResult = erc20LiquidityService.save(erc20Liquidity);
        log.info("[syncErc20] erc20Address:{} saveResult:{}", erc20LiquidityReqVo.getErc20Address(), saveResult);
        result.setData(saveResult ? 1 : 0);
        return result;

    }

    /**
     * 同步burn交易
     *
     * @return Result  Integer
     */
    @PostMapping("/transaction")
    public Result<Integer> syncTransaction(@RequestBody(required = false) LiquidityTransactionReqVo liquidityTransactionReqVo) {
        Result<Integer> result = new Result<>();

        log.info("[syncTransaction] transactionHash:{} tradeType:{} liquidityTransactionReqVo:{}", liquidityTransactionReqVo.getTransactionHash(), liquidityTransactionReqVo.getTradeType(), JacksonUtil.obj2json(liquidityTransactionReqVo));
        LiquidityTransaction liquidityTransaction = liquidityTransactionService.selectByTransactionHashAndType(liquidityTransactionReqVo.getErc20Address(), liquidityTransactionReqVo.getTransactionHash(), liquidityTransactionReqVo.getTradeType());
        if (liquidityTransaction != null) {
            log.info("[syncTransaction] existed transactionHash:{} tradeType:{}", liquidityTransactionReqVo.getTransactionHash(), liquidityTransactionReqVo.getTradeType());
            result.setData(1);
            return result;
        }
        liquidityTransaction = new LiquidityTransaction();
        BeanUtils.copyProperties(liquidityTransactionReqVo, liquidityTransaction);
        liquidityTransaction.setInTokenAmount(liquidityTransactionReqVo.getInTokenAmount());
        liquidityTransaction.setOutTokenAmount(liquidityTransactionReqVo.getOutTokenAmount());
        liquidityTransaction.setEthAmount(liquidityTransactionReqVo.getEthAmount());
        liquidityTransaction.setBlockTime(Long.valueOf(liquidityTransactionReqVo.getBlockTime()));
        boolean saveResult = liquidityTransactionService.save(liquidityTransaction);
        log.info("[syncTransaction] transactionHash:{} tradeType:{} saveResult:{}", liquidityTransactionReqVo.getTransactionHash(), liquidityTransactionReqVo.getTradeType(), saveResult);
        result.setData(saveResult ? 1 : 0);
        return result;

    }

    /**
     * 同步用户erc20的数量
     *
     * @return Result  Integer
     */
    @PostMapping("/erc20Balance")
    public Result<Integer> syncErc20Balance(@RequestBody(required = false) UserLiquidityStatisticsVo userLiquidityStatisticsVo) {
        Result<Integer> result = new Result<>();
        log.info("[syncErc20Balance] erc20Address:{} userAddress:{}", userLiquidityStatisticsVo.getErc20Address(), userLiquidityStatisticsVo.getUserAddress());

        UserLiquidityStatistics userLiquidityStatistics = userLiquidityStatisticsService.selectByUserAddress(userLiquidityStatisticsVo.getUserAddress(), userLiquidityStatisticsVo.getErc20Address());
        if (userLiquidityStatistics == null) {
            log.warn("[syncErc20Balance] userLiquidityStatistics not exist erc20Address:{} userAddress:{}", userLiquidityStatisticsVo.getErc20Address(), userLiquidityStatisticsVo.getUserAddress());
            userLiquidityStatistics = new UserLiquidityStatistics();
            userLiquidityStatistics.setErc20Address(userLiquidityStatisticsVo.getErc20Address());
            userLiquidityStatistics.setUserAddress(userLiquidityStatisticsVo.getUserAddress());
            userLiquidityStatistics.setSyncErc20Balance(OneOrZeroEnum.ONE.getStatus());
            boolean saveResult = userLiquidityStatisticsService.save(userLiquidityStatistics);
            log.info("[syncErc20Balance] erc20Address:{} userAddress:{} saveResult:{}", userLiquidityStatisticsVo.getErc20Address(), userLiquidityStatisticsVo.getUserAddress(), saveResult);
            return result;
        }
        userLiquidityStatistics.setSyncErc20Balance(OneOrZeroEnum.ONE.getStatus());
        boolean updateResult = userLiquidityStatisticsService.updateById(userLiquidityStatistics);
        log.info("[syncErc20Balance] erc20Address:{} userAddress:{} saveResult:{}", userLiquidityStatisticsVo.getErc20Address(), userLiquidityStatisticsVo.getUserAddress(), updateResult);
        return result;

    }

    /**
     * 查询erc20Owner数量
     *
     * @return Result  Integer
     */
    @PostMapping("/erc20Owner")
    public Result<Integer> erc20Owner(@RequestBody(required = false) UserLiquidityStatisticsVo userLiquidityStatisticsVo) {
        Result<Integer> result = new Result<>();
        log.info("[erc20Owner] erc20Address:{} ", userLiquidityStatisticsVo.getErc20Address());

        Integer amount = userLiquidityStatisticsService.erc20OwnerAmount(userLiquidityStatisticsVo.getErc20Address());
        log.info("[erc20Owner] erc20Address:{}  amount:{}", userLiquidityStatisticsVo.getErc20Address(), amount);
        result.setData(amount == null ? 0 : amount);
        return result;

    }
}
