package semios.api.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.CanvasDrbStatistics;
import semios.api.model.entity.ShutdownRecord;
import semios.api.model.vo.req.DaoSortedReqVo;

import java.util.List;

/**
 * <p>
 * canvas画布 服务类
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface ICanvasService extends IService<Canvas> {


    Integer searchAmount(String searchId);


    List<Canvas> searchCanvas(String searchId);


    Page<Canvas> myCanvas(IPage<Canvas> page, String ownerAddress);

    List<Canvas> listCanvasByDaoId(String daoId);

    Page<Canvas> findFavoritesByUserAddress(IPage<Canvas> page, String userAddress);

    Page<CanvasDrbStatistics> collectionsCanvas(IPage<CanvasDrbStatistics> page, DaoSortedReqVo daoSortedReqVo);

    Canvas selectCanvasDetailByCanvasId(String canvasId);

    List<Canvas> selectCanvasDetailByCanvasIdList(List<String> canvasIdList);

    Canvas selectCanvasByUri(String canvasUri);

    Canvas listCanvasFloorPriceByDaoId(String daoId);

    Integer listCanvasAmountByDaoId(String daoId);

    Canvas selectCanvasByName(String canvasName, String daoId);


    List<Canvas> listCanvasLessThanDrb(Integer drb);

    int updateCanvasPaused(Canvas canvas, ShutdownRecord shutdownRecord);

    List<Canvas> myCanvasForAll(String ownerAddress);

    Canvas selectByNumber(Long daoNumber, Long canvasNumber);

    List<Canvas> listCanvasByDaoIds(List<Integer> daoIds);
}
