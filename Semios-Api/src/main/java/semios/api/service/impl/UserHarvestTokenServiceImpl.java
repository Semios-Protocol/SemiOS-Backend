package semios.api.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.mapper.UserHarvestTokenMapper;
import semios.api.model.entity.UserHarvestToken;
import semios.api.service.IUserHarvestTokenService;

import java.util.List;

/**
 * <p>
 * 用户mint获得的代币 服务实现类
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Service
public class UserHarvestTokenServiceImpl extends ServiceImpl<UserHarvestTokenMapper, UserHarvestToken>
        implements IUserHarvestTokenService {

    @Autowired
    private UserHarvestTokenMapper userHarvestTokenMapper;

    @Override
    public List<UserHarvestToken> selectReceivedTokenByUserAddress(String userAddress) {
        return userHarvestTokenMapper.selectReceivedTokenByUserAddress(userAddress);
    }

    @Override
    public List<UserHarvestToken> selectUnclaimedTokenByUserAddress(String userAddress) {
        return userHarvestTokenMapper.selectUnclaimedTokenByUserAddress(userAddress);
    }

    @Override
    public List<UserHarvestToken> selectAllByUserAddress(String userAddress) {
        return userHarvestTokenMapper.selectAllByUserAddress(userAddress);
    }

    @Override
    public UserHarvestToken selectByDaoIdAndUserAddress(Integer daoId, String userAddress) {
        return userHarvestTokenMapper.selectByDaoIdAndUserAddress(daoId, userAddress);
    }
}
