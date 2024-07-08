package semios.api.mapper;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.ibatis.annotations.Select;
import semios.api.model.entity.Favorites;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;

import java.util.List;

/**
 * <p>
 * 收藏 Mapper 接口
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface FavoritesMapper extends BaseMapper<Favorites> {


    @Select("select * from favorites where favorite_id = #{favoriteId} and type = #{type} order by create_time desc")
    Page<Favorites> findFavoritesById(IPage<Favorites> page, String favoriteId, Integer type);

    @Select("select * from favorites where type = #{type} and favorite_id = #{favoriteId} and user_address = #{userAddress} limit 1")
    Favorites findByUserAddress(Integer type, String favoriteId, String userAddress);


    List<Favorites> findListByUserAddress(Integer type, String userAddress);

    @Select("select favorite_id from favorites where type = #{type} and user_address = #{userAddress} ")
    List<Integer> findIdListByUserAddress(Integer type, String userAddress);

}
