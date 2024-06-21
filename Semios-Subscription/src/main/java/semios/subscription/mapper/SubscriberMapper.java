package semios.subscription.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;
import semios.subscription.model.entity.Subscriber;

import java.util.List;

/**
 * <p>
 * 订阅记录表 Mapper 接口
 * </p>
 *
 * @author xiangbin
 */
public interface SubscriberMapper extends BaseMapper<Subscriber> {


    @Select("SELECT * FROM subscriber where sub_status=1 and is_del = 0 and (interval_time = 10 or interval_time is null) order by id asc;")
    List<Subscriber> findAllOpenTenSub();

    @Select("SELECT * FROM subscriber where sub_status=1 and is_del = 0 and interval_time = 60 order by id asc;")
    List<Subscriber> findAllOpenSixtySub();

    @Select("SELECT * FROM subscriber where sub_status=1 and is_del = 0 and network = #{network} and address = #{address} and topics = #{topics} and notice_url = #{noticeUrl} and app_name = #{appName}")
    List<Subscriber> findSubByAppNameAnd(Subscriber subscriber);
}
