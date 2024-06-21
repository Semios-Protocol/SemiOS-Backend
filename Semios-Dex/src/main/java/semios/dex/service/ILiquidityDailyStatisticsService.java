package semios.dex.service;

import com.baomidou.mybatisplus.extension.service.IService;
import semios.dex.model.entity.LiquidityDailyStatistics;

import java.util.List;

/**
 * <p>
 * erc20流通性每日零点统计数据 服务类
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface ILiquidityDailyStatisticsService extends IService<LiquidityDailyStatistics> {
    List<LiquidityDailyStatistics> getErc20DailyStatistics(String erc20Address, Long startDate, Long endDate);

    LiquidityDailyStatistics getErc20DailyStatisticsLastDay(String erc20Address, Long lastDayTime);
}
