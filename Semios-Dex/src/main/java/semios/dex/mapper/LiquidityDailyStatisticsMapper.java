package semios.dex.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;
import semios.dex.model.entity.LiquidityDailyStatistics;

import java.util.List;

/**
 * <p>
 * erc20流通性每日零点统计数据 Mapper 接口
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface LiquidityDailyStatisticsMapper extends BaseMapper<LiquidityDailyStatistics> {
    List<LiquidityDailyStatistics> getErc20DailyStatistics(String erc20Address, Long startDate, Long endDate);

    @Select("select * from liquidity_daily_statistics where erc20_address = #{erc20Address} and record_time = #{lastDayTime}")
    LiquidityDailyStatistics getErc20DailyStatisticsLastDay(String erc20Address, Long lastDayTime);
}
