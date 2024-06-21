package semios.api.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.mapper.UserTopupHarvestMapper;
import semios.api.model.entity.UserTopupHarvest;
import semios.api.service.IUserTopupHarvestService;

import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * topup模式用户拥有eth和token的数量 服务实现类
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Service
public class UserTopupHarvestServiceImpl extends ServiceImpl<UserTopupHarvestMapper, UserTopupHarvest> implements IUserTopupHarvestService {

    @Autowired
    private UserTopupHarvestMapper userTopupHarvestMapper;

    @Override
    public UserTopupHarvest selectByProjectIdAndUserAddress(String projectId, String userAddress) {
        return userTopupHarvestMapper.selectByProjectIdAndUserAddress(projectId, userAddress);
    }

    @Override
    public List<UserTopupHarvest> selectByUserAddress(String userAddress) {
        return userTopupHarvestMapper.selectByUserAddress(userAddress);
    }

    @Override
    public List<UserTopupHarvest> selectUserTopupHarvestByDaoIds(List<Integer> daoIds) {
        if (daoIds == null || daoIds.isEmpty()) {
            return new ArrayList<>();
        }
        return userTopupHarvestMapper.selectUserTopupHarvestByDaoIds(daoIds);
    }
}
