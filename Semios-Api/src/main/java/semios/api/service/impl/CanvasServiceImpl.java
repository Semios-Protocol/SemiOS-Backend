package semios.api.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import semios.api.mapper.CanvasMapper;
import semios.api.mapper.ShutdownRecordMapper;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.CanvasDrbStatistics;
import semios.api.model.entity.ShutdownRecord;
import semios.api.model.vo.req.DaoSortedReqVo;
import semios.api.service.ICanvasService;

import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * canvas画布 服务实现类
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Service
public class CanvasServiceImpl extends ServiceImpl<CanvasMapper, Canvas> implements ICanvasService {

    @Autowired
    private CanvasMapper canvasMapper;

    @Autowired
    private ShutdownRecordMapper shutdownRecordMapper;

    @Override
    public Integer searchAmount(String searchId) {
        return canvasMapper.searchAmount(searchId);
    }

    @Override
    public List<Canvas> searchCanvas(String searchId) {
        return canvasMapper.searchCanvas(searchId);
    }

    @Override
    public Page<Canvas> myCanvas(IPage<Canvas> page, String ownerAddress) {
        return canvasMapper.myCanvas(page, ownerAddress);
    }

    @Override
    public List<Canvas> listCanvasByDaoId(String daoId) {
        return canvasMapper.listCanvasByDaoId(daoId);
    }

    @Override
    public Page<Canvas> findFavoritesByUserAddress(IPage<Canvas> page, String userAddress) {
        return canvasMapper.findFavoritesByUserAddress(page, userAddress);
    }

    @Override
    public Page<CanvasDrbStatistics> collectionsCanvas(IPage<CanvasDrbStatistics> page, DaoSortedReqVo daoSortedReqVo) {
        return canvasMapper.collectionsCanvas(page, daoSortedReqVo);
    }

    @Override
    public Canvas selectCanvasDetailByCanvasId(String canvasId) {
        return canvasMapper.selectCanvasDetailByCanvasId(canvasId);
    }

    @Override
    public List<Canvas> selectCanvasDetailByCanvasIdList(List<String> canvasIdList) {
        if (canvasIdList == null || canvasIdList.size() == 0) {
            return new ArrayList<>();
        }
        return canvasMapper.selectCanvasDetailByCanvasIdList(canvasIdList);
    }

    @Override
    public Canvas selectCanvasByUri(String canvasUri) {
        return canvasMapper.selectCanvasByUri(canvasUri);
    }

    @Override
    public Canvas listCanvasFloorPriceByDaoId(String daoId) {
        return canvasMapper.listCanvasFloorPriceByDaoId(daoId);
    }

    @Override
    public Integer listCanvasAmountByDaoId(String daoId) {
        return canvasMapper.listCanvasAmountByDaoId(daoId);
    }

    @Override
    public Canvas selectCanvasByName(String canvasName, String daoId) {
        return canvasMapper.selectCanvasByNameAndDaoId(canvasName, daoId);
    }

    @Override
    public List<Canvas> listCanvasLessThanDrb(Integer drb) {
        return canvasMapper.listCanvasLessThanDrb(drb);
    }

    @Transactional
    @Override
    public int updateCanvasPaused(Canvas canvas, ShutdownRecord shutdownRecord) {
        int i = canvasMapper.updateById(canvas);
        i += shutdownRecordMapper.insert(shutdownRecord);
        return i;
    }

    @Override
    public List<Canvas> myCanvasForAll(String ownerAddress) {
        return canvasMapper.myCanvasForAll(ownerAddress);
    }

    @Override
    public Canvas selectByNumber(Long daoNumber, Long canvasNumber) {
        return canvasMapper.selectByNumber(daoNumber, canvasNumber);
    }

    @Override
    public List<Canvas> listCanvasByDaoIds(List<Integer> daoIds) {
        if (daoIds == null || daoIds.isEmpty()) {
            return new ArrayList<>();
        }
        return canvasMapper.listCanvasByDaoIds(daoIds);
    }
}
