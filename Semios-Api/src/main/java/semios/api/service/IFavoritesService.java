package semios.api.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import semios.api.model.entity.Favorites;

import java.util.List;

/**
 * <p>
 * 收藏 服务类
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface IFavoritesService extends IService<Favorites> {

    Page<Favorites> findFavoritesById(IPage<Favorites> page, String favoriteId, Integer type);


    boolean saveFavoriteByType(Favorites favorites);

    boolean removeFavoriteByType(Favorites favorites);

    Favorites findByUserAddress(Integer type, String favoriteId, String userAddress);

    List<Favorites> findListByUserAddress(Integer type, String userAddress);

    List<Integer> findIdListByUserAddress(Integer type, String userAddress);
}
