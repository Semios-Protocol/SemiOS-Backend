package semios.api.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;
import semios.api.model.entity.UserHarvestToken;

import java.util.List;

/**
 * <p>
 * 用户mint获得的代币 Mapper 接口
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface UserHarvestTokenMapper extends BaseMapper<UserHarvestToken> {

    @Select("select * from user_harvest_token where user_address = #{userAddress} and received_token > 0")
    List<UserHarvestToken> selectReceivedTokenByUserAddress(String userAddress);

    @Select("select * from user_harvest_token where user_address = #{userAddress} and unclaimed_token >= 0")
    List<UserHarvestToken> selectUnclaimedTokenByUserAddress(String userAddress);

    @Select("select * from user_harvest_token where user_address = #{userAddress}")
    List<UserHarvestToken> selectAllByUserAddress(String userAddress);

    @Select("select * from user_harvest_token where dao_id = #{daoId} and user_address = #{userAddress}")
    UserHarvestToken selectByDaoIdAndUserAddress(Integer daoId, String userAddress);
}
