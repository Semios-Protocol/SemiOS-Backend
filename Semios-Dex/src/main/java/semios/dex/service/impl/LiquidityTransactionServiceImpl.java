package semios.dex.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import semios.dex.mapper.Erc20LiquidityMapper;
import semios.dex.mapper.LiquidityTransactionMapper;
import semios.dex.model.dto.response.Erc20BalanceResDto;
import semios.dex.model.dto.response.Erc20TradeResDto;
import semios.dex.model.entity.Erc20Liquidity;
import semios.dex.model.entity.LiquidityTransaction;
import semios.dex.model.vo.req.TransactionReqVo;
import semios.dex.service.ILiquidityTransactionService;

import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * 流通性交易表 服务实现类
 * </p>
 *
 * @author xiangbin
 */
@Service
public class LiquidityTransactionServiceImpl extends ServiceImpl<LiquidityTransactionMapper, LiquidityTransaction>
        implements ILiquidityTransactionService {

    @Autowired
    LiquidityTransactionMapper liquidityTransactionMapper;

    @Autowired
    Erc20LiquidityMapper erc20LiquidityMapper;

    @Override
    public Erc20BalanceResDto getSwapVolumeMax(long startDate, long endDate) {
        return liquidityTransactionMapper.getTradingVolumeMax(startDate, endDate);
    }

    @Override
    public BigDecimal getSwapVolume(String erc20Address, Long startDate, Long endDate) {
        return liquidityTransactionMapper.getSwapVolume(erc20Address, startDate, endDate);
    }

    @Override
    public BigDecimal getBurnVolume(String erc20Address, Long startDate, Long endDate) {
        return liquidityTransactionMapper.getBurnVolume(erc20Address, startDate, endDate);
    }

    @Override
    public List<Erc20TradeResDto> getSwapVolumeList(List<String> erc20AddressList, Long startDate, Long endDate) {
        return liquidityTransactionMapper.getSwapVolumeList(erc20AddressList, startDate, endDate);
    }

    @Override
    public List<Erc20TradeResDto> getBurnVolumeList(List<String> erc20AddressList, Long startDate, Long endDate) {
        return liquidityTransactionMapper.getBurnVolumeList(erc20AddressList, startDate, endDate);
    }

    @Override
    public IPage<LiquidityTransaction> getTradeList(TransactionReqVo transactionReqVo) {
        long pageNo = transactionReqVo.getPageNo();
        long pageSize = transactionReqVo.getPageSize();
        Page<LiquidityTransaction> page;
        if (pageNo > 1) {
            page = new Page<>(pageNo, pageSize, false);
        } else {
            page = new Page<>(pageNo, pageSize);
        }
        return liquidityTransactionMapper.getTradeList(page, transactionReqVo.getErc20Address(),
                transactionReqVo.getTradeType(), transactionReqVo.getStartDate(), transactionReqVo.getEndDate());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public int saveLiquidityTransactionAndUpdateErc20Liquidity(LiquidityTransaction liquidityTransaction, Erc20Liquidity erc20Liquidity) {
        int i = 0;
        if (liquidityTransaction != null) {
            i += liquidityTransactionMapper.insert(liquidityTransaction);
        }
        if (erc20Liquidity != null) {
            i += erc20LiquidityMapper.updateById(erc20Liquidity);
        }
        return i;
    }

    @Override
    public LiquidityTransaction selectByTransactionHashAndType(String erc20Address, String transactionHash, Integer tradeType) {
        return liquidityTransactionMapper.selectByTransactionHashAndType(erc20Address, transactionHash, tradeType);
    }
}
