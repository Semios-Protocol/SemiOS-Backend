package semios.dex.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.dex.mapper.LiquidityDailyStatisticsMapper;
import semios.dex.model.entity.LiquidityDailyStatistics;
import semios.dex.service.ILiquidityDailyStatisticsService;

import java.util.List;

/**
 * <p>
 * erc20流通性每日零点统计数据 服务实现类
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Service
public class LiquidityDailyStatisticsServiceImpl extends
        ServiceImpl<LiquidityDailyStatisticsMapper, LiquidityDailyStatistics> implements ILiquidityDailyStatisticsService {

    @Autowired
    LiquidityDailyStatisticsMapper liquidityDailyStatisticsMapper;

    @Override
    public List<LiquidityDailyStatistics> getErc20DailyStatistics(String erc20Address, Long startDate, Long endDate) {
        return liquidityDailyStatisticsMapper.getErc20DailyStatistics(erc20Address, startDate, endDate);
    }

    @Override
    public LiquidityDailyStatistics getErc20DailyStatisticsLastDay(String erc20Address, Long lastDayTime) {
        return liquidityDailyStatisticsMapper.getErc20DailyStatisticsLastDay(erc20Address, lastDayTime);
    }
}
