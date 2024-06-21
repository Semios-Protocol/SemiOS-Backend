package semios.api.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import semios.api.mapper.CanvasDrbStatisticsMapper;
import semios.api.mapper.CanvasMapper;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.CanvasDrbStatistics;
import semios.api.service.ICanvasDrbStatisticsService;

import java.util.List;

/**
 * <p>
 * canvas在drb的统计信息 服务实现类
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Service
public class CanvasDrbStatisticsServiceImpl extends ServiceImpl<CanvasDrbStatisticsMapper, CanvasDrbStatistics> implements ICanvasDrbStatisticsService {

    @Autowired
    private CanvasDrbStatisticsMapper canvasDrbStatisticsMapper;

    @Autowired
    private CanvasMapper canvasMapper;

    @Override
    public Page<CanvasDrbStatistics> selectByProjectId(IPage<CanvasDrbStatistics> page, String projectId) {
        return canvasDrbStatisticsMapper.selectByProjectId(page, projectId);
    }

    @Override
    public CanvasDrbStatistics selectLastedByCanvasId(Integer canvasId) {
        return canvasDrbStatisticsMapper.selectLastedByCanvasId(canvasId);
    }

    @Override
    public Page<CanvasDrbStatistics> selectByCanvasId(IPage<CanvasDrbStatistics> page, String canvasId) {
        return canvasDrbStatisticsMapper.selectByCanvasId(page, canvasId);
    }

    @Override
    public Page<CanvasDrbStatistics> canvasRanking(IPage<CanvasDrbStatistics> page) {
        return canvasDrbStatisticsMapper.canvasRanking(page);
    }

    @Override
    public List<CanvasDrbStatistics> selectByDrbNumber(Integer drbNumber) {
        return canvasDrbStatisticsMapper.selectByDrbNumber(drbNumber);
    }

    @Override
    public CanvasDrbStatistics selectByCanvasIdAndDrbNumber(Integer canvasId, Integer drbNumber) {
        return canvasDrbStatisticsMapper.selectByCanvasIdAndDrbNumber(canvasId, drbNumber);
    }

    @Override
    public CanvasDrbStatistics selectByRangeDrb(String canvasId, Integer startDrb, Integer endDrb) {
        return canvasDrbStatisticsMapper.selectByRangeDrb(canvasId, startDrb, endDrb);
    }

    @Override
    public CanvasDrbStatistics selectMintRevenueByDaoId(Integer daoId, Integer drbNumber) {
        return canvasDrbStatisticsMapper.selectMintRevenueByDaoId(daoId, drbNumber);
    }

    @Override
    public List<CanvasDrbStatistics> selectFailStatus(Integer drbNumber) {
        return canvasDrbStatisticsMapper.selectFailStatus(drbNumber);
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public int updateCanvasAndCanvasDrbStatistics(CanvasDrbStatistics canvasDrbStatistics, Canvas canvas) {
        int i = 0;
        if (canvas != null) {
            i += canvasMapper.updateById(canvas);
        }
        if (canvasDrbStatistics != null) {
            i += canvasDrbStatisticsMapper.updateById(canvasDrbStatistics);
        }
        return i;
    }

    @Override
    public List<CanvasDrbStatistics> selectByDrbNumberAndDaoId(Integer drbNumber, Integer daoId) {
        return canvasDrbStatisticsMapper.selectByDrbNumberAndDaoId(drbNumber, daoId);
    }
}
