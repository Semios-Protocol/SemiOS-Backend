package semios.api.service;

import com.baomidou.mybatisplus.extension.service.IService;
import semios.api.model.entity.User;

import java.util.List;

/**
 * <p>
 * 用户信息表 服务类
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface IUserService extends IService<User> {

    User findUserByAddressHash(String userAddress);

    List<User> findUsersByUserAddress(List<String> userAddressList);

    User findUserByName(String userName);

    List<User> findUsersIsContractNull();
}
