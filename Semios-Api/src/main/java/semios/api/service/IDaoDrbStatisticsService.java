package semios.api.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoDrbStatistics;

import java.util.List;

/**
 * <p>
 * Dao在drb的统计信息 服务类
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface IDaoDrbStatisticsService extends IService<DaoDrbStatistics> {

    /**
     * 仅返回daoId
     *
     * @param startDrb
     * @param endDrb
     * @return
     */
    List<DaoDrbStatistics> selectGalleryDao(Integer startDrb, Integer endDrb);

    DaoDrbStatistics selectLastedDrbByDaoId(Integer daoId);

    Page<DaoDrbStatistics> selectByDaoId(IPage<DaoDrbStatistics> page, Integer daoId);

    Page<DaoDrbStatistics> daosRanking(IPage<DaoDrbStatistics> page);

    List<DaoDrbStatistics> selectByDrbNumber(Integer drbNumber);

    DaoDrbStatistics selectByDaoIdAndDrbNumber(Integer daoId, Integer drbNumber);

    List<DaoDrbStatistics> selectFailStatus(Integer drbNumber);

    DaoDrbStatistics selectByDaoIdAndDrbNumberForAnalytics(Integer daoId, Integer drbNumber);

    int updateDaoAndDaoDrbStatistics(DaoDrbStatistics daoDrbStatistics, Dao dao);

    List<DaoDrbStatistics> selectByDaoIdAndRecordTime(Integer daoId, Long recordTime);
}
