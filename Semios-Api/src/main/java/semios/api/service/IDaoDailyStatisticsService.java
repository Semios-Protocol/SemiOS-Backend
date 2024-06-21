package semios.api.service;

import com.baomidou.mybatisplus.extension.service.IService;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoDailyStatistics;
import semios.api.model.entity.DaoDrbStatistics;

import java.util.List;

/**
 * <p>
 * Dao资金池每日零点统计数据 服务类
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface IDaoDailyStatisticsService extends IService<DaoDailyStatistics> {


    int updateDaoAndSaveDaoDailyStatistics(Dao dao, DaoDailyStatistics daoDailyStatistics);


    List<DaoDailyStatistics> selectDaoDailyIncompleteStatus(Long recordTime);

    List<DaoDailyStatistics> selectDaoDailyCompleteStatus(Long recordTime);

    DaoDailyStatistics selectDaoDailyCompleteByDaoId(Integer daoId, Long recordTime);


    int insertDaoDailyStatisticsAndUpdateDaoDrbStatistics(List<DaoDrbStatistics> daoDrbStatisticsUpdateList, List<DaoDailyStatistics> daoDailyStatisticsList);


    List<DaoDailyStatistics> selectDaoDailyCompleteByDaoIdAndRecordTime(Integer daoId, Long recordTime);

    DaoDailyStatistics selectDaoDailyByProjectId(String projectId, Long recordTime);
}
