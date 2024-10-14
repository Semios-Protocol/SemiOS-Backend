package semios.api.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;
import semios.api.model.entity.MakerInfoStatistics;
import semios.api.model.vo.req.Maker.AnalyticsTokenParam;
import semios.api.model.vo.res.Maker.AnalyticsMakerDataVo;
import semios.api.model.vo.res.Maker.AnalyticsTokenDataVo;
import semios.api.model.vo.res.Maker.MakerOwnerListVo;

import java.util.List;

/**
 * <p>
 * seed node maker统计信息 Mapper 接口
 * </p>
 *
 * @author zhyyao
 * @since 2024-09-09
 */
public interface MakerInfoStatisticsMapper extends BaseMapper<MakerInfoStatistics> {


    List<AnalyticsTokenDataVo> analyticsTokenData(@Param("analyticsTokenParam") AnalyticsTokenParam analyticsTokenParam);


    List<AnalyticsMakerDataVo> getMakerInfoStatisticsList(@Param("analyticsTokenParam") AnalyticsTokenParam analyticsTokenParam);


    List<MakerOwnerListVo> getMakerOwnerList(String projectId);
}
