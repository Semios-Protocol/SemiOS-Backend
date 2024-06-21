package semios.api.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;
import semios.api.model.entity.UserTopupHarvest;

import java.util.List;

/**
 * <p>
 * topup模式用户拥有eth和token的数量 Mapper 接口
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface UserTopupHarvestMapper extends BaseMapper<UserTopupHarvest> {


    @Select("select * from user_topup_harvest where is_del = 0 and project_id = #{projectId} and user_address = #{userAddress}")
    UserTopupHarvest selectByProjectIdAndUserAddress(String projectId, String userAddress);


    @Select("select * from user_topup_harvest where is_del = 0 and  user_address = #{userAddress} order by eth_amount desc ")
    List<UserTopupHarvest> selectByUserAddress(String userAddress);


    List<UserTopupHarvest> selectUserTopupHarvestByDaoIds(List<Integer> daoIds);
}
