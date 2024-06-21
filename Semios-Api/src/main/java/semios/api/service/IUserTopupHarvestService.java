package semios.api.service;

import com.baomidou.mybatisplus.extension.service.IService;
import semios.api.model.entity.UserTopupHarvest;

import java.util.List;

/**
 * <p>
 * topup模式用户拥有eth和token的数量 服务类
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface IUserTopupHarvestService extends IService<UserTopupHarvest> {


    UserTopupHarvest selectByProjectIdAndUserAddress(String projectId, String userAddress);

    List<UserTopupHarvest> selectByUserAddress(String userAddress);

    List<UserTopupHarvest> selectUserTopupHarvestByDaoIds(List<Integer> daoIds);
}
