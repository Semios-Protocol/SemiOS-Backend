package semios.api.service;


import com.baomidou.mybatisplus.extension.service.IService;
import semios.api.model.dto.common.Result;
import semios.api.model.entity.MakerInfoStatistics;
import semios.api.model.vo.req.Maker.AnalyticsTokenParam;
import semios.api.model.vo.res.Maker.AnalyticsBalanceVo;
import semios.api.model.vo.res.Maker.AnalyticsMakerVo;
import semios.api.model.vo.res.Maker.MakerOwnerInfoVo;

/**
 * <p>
 * seed node maker统计信息 服务类
 * </p>
 *
 * @author zhyyao
 * @since 2024-09-09
 */
public interface IMakerInfoStatisticsService extends IService<MakerInfoStatistics> {

    Result<AnalyticsBalanceVo> analyticsToken(AnalyticsTokenParam analyticsTokenParam);

    Result<AnalyticsMakerVo> analyticsMaker(AnalyticsTokenParam analyticsTokenParam);

    Result<MakerOwnerInfoVo> analyticsMakerList(AnalyticsTokenParam analyticsTokenParam);
}
