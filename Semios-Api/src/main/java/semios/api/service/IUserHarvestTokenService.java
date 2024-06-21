package semios.api.service;

import com.baomidou.mybatisplus.extension.service.IService;
import semios.api.model.entity.UserHarvestToken;

import java.util.List;

/**
 * <p>
 * 用户mint获得的代币 服务类
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface IUserHarvestTokenService extends IService<UserHarvestToken> {

    /**
     * 查询minter已领取代币大于零的
     *
     * @param userAddress
     * @return
     */
    List<UserHarvestToken> selectReceivedTokenByUserAddress(String userAddress);

    /**
     * 查询minter未领取代币大于零的
     *
     * @param userAddress
     * @return
     */
    List<UserHarvestToken> selectUnclaimedTokenByUserAddress(String userAddress);

    List<UserHarvestToken> selectAllByUserAddress(String userAddress);

    UserHarvestToken selectByDaoIdAndUserAddress(Integer daoId, String userAddress);
}
