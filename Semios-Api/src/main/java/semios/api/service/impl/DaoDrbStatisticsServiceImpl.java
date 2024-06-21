package semios.api.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import semios.api.mapper.DaoDrbStatisticsMapper;
import semios.api.mapper.DaoMapper;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoDrbStatistics;
import semios.api.service.IDaoDrbStatisticsService;

import java.util.List;

/**
 * <p>
 * Dao在drb的统计信息 服务实现类
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Service
public class DaoDrbStatisticsServiceImpl extends ServiceImpl<DaoDrbStatisticsMapper, DaoDrbStatistics>
        implements IDaoDrbStatisticsService {

    @Autowired
    private DaoDrbStatisticsMapper daoDrbStatisticsMapper;

    @Autowired
    private DaoMapper daoMapper;

    @Override
    public List<DaoDrbStatistics> selectGalleryDao(Integer startDrb, Integer endDrb) {
        return daoDrbStatisticsMapper.selectGalleryDao(startDrb, endDrb);
    }

    @Override
    public DaoDrbStatistics selectLastedDrbByDaoId(Integer daoId) {
        return daoDrbStatisticsMapper.selectLastedDrbByDaoId(daoId);
    }

    @Override
    public Page<DaoDrbStatistics> selectByDaoId(IPage<DaoDrbStatistics> page, Integer daoId) {
        return daoDrbStatisticsMapper.selectByDaoId(page, daoId);
    }

    @Override
    public Page<DaoDrbStatistics> daosRanking(IPage<DaoDrbStatistics> page) {
        return daoDrbStatisticsMapper.daosRanking(page);
    }

    @Override
    public List<DaoDrbStatistics> selectByDrbNumber(Integer drbNumber) {
        return daoDrbStatisticsMapper.selectByDrbNumber(drbNumber);
    }

    @Override
    public DaoDrbStatistics selectByDaoIdAndDrbNumber(Integer daoId, Integer drbNumber) {
        return daoDrbStatisticsMapper.selectByDaoIdAndDrbNumber(daoId, drbNumber);
    }

    @Override
    public List<DaoDrbStatistics> selectFailStatus(Integer drbNumber) {
        return daoDrbStatisticsMapper.selectFailStatus(drbNumber);
    }

    @Override
    public DaoDrbStatistics selectByDaoIdAndDrbNumberForAnalytics(Integer daoId, Integer drbNumber) {
        return daoDrbStatisticsMapper.selectByDaoIdAndDrbNumberForAnalytics(daoId, drbNumber);
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public int updateDaoAndDaoDrbStatistics(DaoDrbStatistics daoDrbStatistics, Dao dao) {
        int i = 0;
        if (dao != null) {
            i += daoMapper.updateById(dao);
        }
        if (daoDrbStatistics != null) {
            if (daoDrbStatistics.getId() != null) {
                i += daoDrbStatisticsMapper.updateById(daoDrbStatistics);
            } else {
                i += daoDrbStatisticsMapper.insert(daoDrbStatistics);
            }
        }
        return i;
    }

    @Override
    public List<DaoDrbStatistics> selectByDaoIdAndRecordTime(Integer daoId, Long recordTime) {
        return daoDrbStatisticsMapper.selectByDaoIdAndRecordTime(daoId, recordTime);
    }
}
