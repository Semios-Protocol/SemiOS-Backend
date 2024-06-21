package semios.api.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;
import semios.api.model.entity.User;

import java.util.List;

/**
 * <p>
 * 用户信息表 Mapper 接口
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface UserMapper extends BaseMapper<User> {

    @Select("select * from user where user_address = #{userAddress}")
    User findUserByAddressHash(String userAddress);

    List<User> findUsersByUserAddress(List<String> userAddressList);

    @Select("select * from user where LOWER(Replace(user_name,' ','')) = LOWER(Replace(#{userName},' ','')) limit 1")
    User findUserByName(String userName);

    @Select("select * from user where is_contract is null")
    List<User> findUsersIsContractNull();
}
