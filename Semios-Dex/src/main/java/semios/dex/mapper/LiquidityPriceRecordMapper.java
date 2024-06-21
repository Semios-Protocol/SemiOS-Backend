package semios.dex.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;
import semios.dex.model.dto.response.AssetPoolPriceResDto;
import semios.dex.model.dto.response.Erc20PriceResDto;
import semios.dex.model.entity.LiquidityPriceRecord;

import java.util.List;

/**
 * <p>
 * erc20流通性价格变更记录 Mapper 接口
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface LiquidityPriceRecordMapper extends BaseMapper<LiquidityPriceRecord> {
    List<Erc20PriceResDto> getOneHourErc20Price(List<String> erc20AddressList, Long searchTime);

    List<Erc20PriceResDto> getHoursErc20Price(String erc20Address, Long startDate, Long endDate);

    List<AssetPoolPriceResDto> getHoursAssetPoolPrice(String erc20Address, Long startDate, Long endDate);

    @Select("select * from liquidity_price_record where erc20_address = #{erc20Address} and type = 1 order by record_time desc limit 1")
    LiquidityPriceRecord getLatestAssetPoolPrice(String erc20Address);

    @Select("select * from liquidity_price_record where erc20_address = #{erc20Address} and type = 0 order by record_time desc limit 1")
    LiquidityPriceRecord getLatestSwapPrice(String erc20Address);

    @Select("select * from liquidity_price_record where erc20_address = #{erc20Address} and type = #{type} and record_time = #{recordTime}")
    LiquidityPriceRecord getLastHoursPriceRecord(String erc20Address, Integer type, Long recordTime);
}
