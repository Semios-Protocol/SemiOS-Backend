package semios.api.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import semios.api.mapper.DaoDailyStatisticsMapper;
import semios.api.mapper.DaoDrbStatisticsMapper;
import semios.api.mapper.DaoMapper;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoDailyStatistics;
import semios.api.model.entity.DaoDrbStatistics;
import semios.api.service.IDaoDailyStatisticsService;

import java.util.List;

/**
 * <p>
 * Dao资金池每日零点统计数据 服务实现类
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Service
public class DaoDailyStatisticsServiceImpl extends ServiceImpl<DaoDailyStatisticsMapper, DaoDailyStatistics> implements IDaoDailyStatisticsService {

    @Autowired
    private DaoMapper daoMapper;

    @Autowired
    private DaoDailyStatisticsMapper daoDailyStatisticsMapper;

    @Autowired
    private DaoDrbStatisticsMapper daoDrbStatisticsMapper;

    @Transactional(rollbackFor = Exception.class)
    @Override
    public int updateDaoAndSaveDaoDailyStatistics(Dao dao, DaoDailyStatistics daoDailyStatistics) {
        int i = 0;
        if (dao != null) {
            i += daoMapper.updateById(dao);
        }
        if (daoDailyStatistics != null) {
            i += daoDailyStatisticsMapper.insert(daoDailyStatistics);
        }
        return i;
    }

    @Override
    public List<DaoDailyStatistics> selectDaoDailyIncompleteStatus(Long recordTime) {
        return daoDailyStatisticsMapper.selectDaoDailyIncompleteStatus(recordTime);
    }


    @Override
    public List<DaoDailyStatistics> selectDaoDailyCompleteStatus(Long recordTime) {
        return daoDailyStatisticsMapper.selectDaoDailyCompleteStatus(recordTime);
    }

    @Override
    public DaoDailyStatistics selectDaoDailyCompleteByDaoId(Integer daoId, Long recordTime) {
        return daoDailyStatisticsMapper.selectDaoDailyCompleteByDaoId(daoId, recordTime);
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public int insertDaoDailyStatisticsAndUpdateDaoDrbStatistics(List<DaoDrbStatistics> daoDrbStatisticsUpdateList, List<DaoDailyStatistics> daoDailyStatisticsList) {
        int i = 0;
        if (daoDrbStatisticsUpdateList != null && !daoDrbStatisticsUpdateList.isEmpty()) {
            for (DaoDrbStatistics daoDrbStatistics : daoDrbStatisticsUpdateList) {
                i += daoDrbStatisticsMapper.updateById(daoDrbStatistics);
            }
        }
        if (daoDailyStatisticsList != null && !daoDailyStatisticsList.isEmpty()) {
            for (DaoDailyStatistics daoDailyStatistics : daoDailyStatisticsList) {
                if (daoDailyStatistics.getId() == null) {
                    i += daoDailyStatisticsMapper.insert(daoDailyStatistics);
                } else {
                    i += daoDailyStatisticsMapper.updateById(daoDailyStatistics);
                }
            }
        }

        return i;
    }

    @Override
    public List<DaoDailyStatistics> selectDaoDailyCompleteByDaoIdAndRecordTime(Integer daoId, Long recordTime) {
        return daoDailyStatisticsMapper.selectDaoDailyCompleteByDaoIdAndRecordTime(daoId, recordTime);
    }

    @Override
    public DaoDailyStatistics selectDaoDailyByProjectId(String projectId, Long recordTime) {
        return daoDailyStatisticsMapper.selectDaoDailyByProjectId(projectId, recordTime);
    }
}
