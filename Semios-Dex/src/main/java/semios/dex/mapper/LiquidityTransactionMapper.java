package semios.dex.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.ibatis.annotations.Select;
import semios.dex.model.dto.response.Erc20BalanceResDto;
import semios.dex.model.dto.response.Erc20TradeResDto;
import semios.dex.model.entity.LiquidityTransaction;

import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * 流通性交易表 Mapper 接口
 * </p>
 *
 * @author xiangbin
 */
public interface LiquidityTransactionMapper extends BaseMapper<LiquidityTransaction> {

    @Select("select\n"
            + "    erc20_address,\n"
            + "    erc20_name,\n"
            + "    erc20_symbol\n"
            + "from\n"
            + "    liquidity_transaction lt\n"
            + "where\n"
            + "    block_time >= #{startDate}\n"
            + "    and block_time < #{endDate}\n"
            + "    group by erc20_address \n"
            + "order by\n"
            + "        sum(eth_amount) desc")
    Erc20BalanceResDto getTradingVolumeMax(long startDate, long endDate);

    BigDecimal getSwapVolume(String erc20Address, Long startDate, Long endDate);

    BigDecimal getBurnVolume(String erc20Address, Long startDate, Long endDate);

    List<Erc20TradeResDto> getSwapVolumeList(List<String> erc20AddressList, Long startDate, Long endDate);

    List<Erc20TradeResDto> getBurnVolumeList(List<String> erc20AddressList, Long startDate, Long endDate);

    Page<LiquidityTransaction> getTradeList(IPage<LiquidityTransaction> page, String erc20Address,
                                            List<Integer> tradeTypeList, Long startDate, Long endDate);


    @Select("select * from liquidity_transaction where erc20_address = #{erc20Address} and transaction_hash = #{transactionHash} and trade_type = #{tradeType}")
    LiquidityTransaction selectByTransactionHashAndType(String erc20Address, String transactionHash, Integer tradeType);

}
