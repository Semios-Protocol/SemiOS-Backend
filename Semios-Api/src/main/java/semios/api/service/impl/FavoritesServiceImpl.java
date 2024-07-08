package semios.api.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import semios.api.mapper.FavoritesMapper;
import semios.api.model.entity.*;
import semios.api.model.enums.FavoriteTypeEnum;
import semios.api.service.*;
import semios.api.utils.JacksonUtil;

import java.util.List;

/**
 * <p>
 * 收藏 服务实现类
 * </p>
 *
 * @author xiangbin
 * @since 
 */
@Slf4j
@Service
public class FavoritesServiceImpl extends ServiceImpl<FavoritesMapper, Favorites> implements IFavoritesService {

    @Autowired
    private FavoritesMapper favoritesMapper;

    @Autowired
    private IDaoService daoService;

    @Autowired
    private ICanvasService canvasService;

    @Autowired
    private IWorkService workService;


    @Override
    public Page<Favorites> findFavoritesById(IPage<Favorites> page, String favoriteId,Integer type) {
        return favoritesMapper.findFavoritesById(page, favoriteId,type);
    }


    @Override
    @Transactional
    public boolean saveFavoriteByType(Favorites favorites) {
        if(FavoriteTypeEnum.DAO_FAVORITE.getType().equals(favorites.getType())){
            Dao dao = daoService.getById(favorites.getFavoriteId());
            if(dao == null){
                throw new RuntimeException("dao not exist");
            }
            Integer favoriteAmount = dao.getFavoriteAmount();
            if(favoriteAmount == null){
                favoriteAmount = 0;
            }
            dao.setFavoriteAmount(favoriteAmount + 1);
            daoService.updateById(dao);
        } else if (FavoriteTypeEnum.CANVAS_FAVORITE.getType().equals(favorites.getType())){
            Canvas canvas = canvasService.getById(favorites.getFavoriteId());
            if (canvas == null) {
                throw new RuntimeException("canvas not exist");
            }
            Integer favoriteAmount = canvas.getFavoriteAmount();
            if(favoriteAmount == null){
                favoriteAmount = 0;
            }
            canvas.setFavoriteAmount(favoriteAmount + 1);
            canvasService.updateById(canvas);

        } else if (FavoriteTypeEnum.WORK_FAVORITE.getType().equals(favorites.getType())){

            Work work = workService.selectWorkById(favorites.getFavoriteId());
            if(work == null){
                throw new RuntimeException("work not exists");
            }
            Integer favoriteAmount = work.getFavoriteAmount();
            if(favoriteAmount == null){
                favoriteAmount = 0;
            }
            work.setFavoriteAmount(favoriteAmount + 1);
            workService.updateById(work);
        }

        favoritesMapper.insert(favorites);
        return true;
    }

    @Override
    public Favorites findByUserAddress(Integer type, String favoriteId, String userAddress) {
        return favoritesMapper.findByUserAddress(type, favoriteId, userAddress);
    }

    @Override
    public List<Favorites> findListByUserAddress(Integer type, String userAddress) {
        return favoritesMapper.findListByUserAddress(type, userAddress);
    }

    @Override
    public List<Integer> findIdListByUserAddress(Integer type, String userAddress) {
        return favoritesMapper.findIdListByUserAddress(type, userAddress);
    }

    @Override
    @Transactional
    public boolean removeFavoriteByType(Favorites favorites) {
        Favorites favorites1 = favoritesMapper.findByUserAddress(favorites.getType(), favorites.getFavoriteId(), favorites.getUserAddress().toLowerCase());
        if(favorites1 == null){
            log.info("[removeFavoriteByType] is null favorites:{}", JacksonUtil.obj2json(favorites));
            return true;
        }
        favoritesMapper.deleteById(favorites1.getId());
        if(FavoriteTypeEnum.DAO_FAVORITE.getType().equals(favorites.getType())){
            Dao dao = daoService.getById(favorites.getFavoriteId());
            if(dao == null){
                throw new RuntimeException("dao not exist");
            }
            dao.setFavoriteAmount(dao.getFavoriteAmount() - 1);
            if(dao.getFavoriteAmount() >= 0){
                daoService.updateById(dao);
            }
        } else if (FavoriteTypeEnum.CANVAS_FAVORITE.getType().equals(favorites.getType())){
            Canvas canvas = canvasService.getById(favorites.getFavoriteId());
            if (canvas == null) {
                throw new RuntimeException("canvas not exist");
            }
            canvas.setFavoriteAmount(canvas.getFavoriteAmount() - 1);
            if(canvas.getFavoriteAmount() >= 0){
                canvasService.updateById(canvas);
            }

        } else if (FavoriteTypeEnum.WORK_FAVORITE.getType().equals(favorites.getType())){

            Work work = workService.selectWorkById(favorites.getFavoriteId());
            if(work == null){
                throw new RuntimeException("work not exists");
            }
            Integer favoriteAmount = work.getFavoriteAmount();
            work.setFavoriteAmount(favoriteAmount - 1);
            if(work.getFavoriteAmount() >= 0) {
                workService.updateById(work);
            }
        }

        return true;
    }
}
