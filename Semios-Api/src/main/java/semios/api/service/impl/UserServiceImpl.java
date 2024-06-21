package semios.api.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.mapper.UserMapper;
import semios.api.model.entity.User;
import semios.api.service.IUserService;

import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * 用户信息表 服务实现类
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Service
public class UserServiceImpl extends ServiceImpl<UserMapper, User> implements IUserService {

    @Autowired
    private UserMapper userMapper;

    @Override
    public User findUserByAddressHash(String userAddress) {
        return userMapper.findUserByAddressHash(userAddress);
    }

    @Override
    public List<User> findUsersByUserAddress(List<String> userAddressList) {
        if (userAddressList == null || userAddressList.size() == 0) {
            return new ArrayList<>();
        }
        return userMapper.findUsersByUserAddress(userAddressList);
    }

    @Override
    public User findUserByName(String userName) {
        return userMapper.findUserByName(userName);
    }

    @Override
    public List<User> findUsersIsContractNull() {
        return userMapper.findUsersIsContractNull();
    }
}
