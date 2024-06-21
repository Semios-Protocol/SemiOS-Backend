package semios.subscription.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;
import semios.subscription.model.entity.SubNumValue;

/**
 * <p>
 * 数值类型订阅最新值 Mapper 接口
 * </p>
 *
 * @author xiangbin
 */
public interface SubNumValueMapper extends BaseMapper<SubNumValue> {

    @Select("select * from sub_num_value where net_work =#{netWork} and address = #{address} and topic = #{topic}")
    SubNumValue selectAddressAndTopic(String netWork, String address, String topic);

    @Select("select * from sub_num_value where filter_id =#{filterId}")
    SubNumValue selectByFilterId(Integer filterId);


}
