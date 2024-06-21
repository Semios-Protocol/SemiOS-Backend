package semios.api.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.CanvasDrbStatistics;

import java.util.List;

/**
 * <p>
 * canvas在drb的统计信息 服务类
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface ICanvasDrbStatisticsService extends IService<CanvasDrbStatistics> {

    Page<CanvasDrbStatistics> selectByProjectId(IPage<CanvasDrbStatistics> page, String projectId);

    CanvasDrbStatistics selectLastedByCanvasId(Integer canvasId);


    Page<CanvasDrbStatistics> selectByCanvasId(IPage<CanvasDrbStatistics> page, String canvasId);


    Page<CanvasDrbStatistics> canvasRanking(IPage<CanvasDrbStatistics> page);

    List<CanvasDrbStatistics> selectByDrbNumber(Integer drbNumber);

    CanvasDrbStatistics selectByCanvasIdAndDrbNumber(Integer canvasId, Integer drbNumber);


    CanvasDrbStatistics selectByRangeDrb(String canvasId, Integer startDrb, Integer endDrb);

    CanvasDrbStatistics selectMintRevenueByDaoId(Integer daoId, Integer drbNumber);

    List<CanvasDrbStatistics> selectFailStatus(Integer drbNumber);

    int updateCanvasAndCanvasDrbStatistics(CanvasDrbStatistics canvasDrbStatistics, Canvas canvas);

    List<CanvasDrbStatistics> selectByDrbNumberAndDaoId(Integer drbNumber, Integer daoId);


}
