package semios.api.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;
import semios.api.model.entity.WhiteList;

/**
 * <p>
 * 黑白名单地址表 Mapper 接口
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface WhiteListMapper extends BaseMapper<WhiteList> {


    @Select("select id,user_address,origin_address from white_list where user_address = #{userAddress} and proof_root_hash = #{proofRootHash} and is_del = 0 and is_valid = 1")
    WhiteList selectByAddressAndRoot(String userAddress, String proofRootHash);

}
