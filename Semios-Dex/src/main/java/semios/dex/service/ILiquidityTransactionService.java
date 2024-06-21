package semios.dex.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import semios.dex.model.dto.response.Erc20BalanceResDto;
import semios.dex.model.dto.response.Erc20TradeResDto;
import semios.dex.model.entity.Erc20Liquidity;
import semios.dex.model.entity.LiquidityTransaction;
import semios.dex.model.vo.req.TransactionReqVo;

import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * 流通性交易表 服务类
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface ILiquidityTransactionService extends IService<LiquidityTransaction> {
    Erc20BalanceResDto getSwapVolumeMax(long startDate, long endDate);

    BigDecimal getSwapVolume(String erc20Address, Long startDate, Long endDate);

    BigDecimal getBurnVolume(String erc20Address, Long startDate, Long endDate);

    int saveLiquidityTransactionAndUpdateErc20Liquidity(LiquidityTransaction liquidityTransaction, Erc20Liquidity erc20Liquidity);

    List<Erc20TradeResDto> getSwapVolumeList(List<String> erc20AddressList, Long startDate, Long endDate);

    List<Erc20TradeResDto> getBurnVolumeList(List<String> erc20AddressList, Long startDate, Long endDate);

    IPage<LiquidityTransaction> getTradeList(TransactionReqVo transactionReqVo);

    LiquidityTransaction selectByTransactionHashAndType(String erc20Address, String transactionHash, Integer tradeType);
}
