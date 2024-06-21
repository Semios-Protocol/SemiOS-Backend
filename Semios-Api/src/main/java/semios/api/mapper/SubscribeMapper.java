package semios.api.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;
import semios.api.model.entity.Subscribe;

import java.util.List;

/**
 * <p>
 * 订阅记录 Mapper 接口
 * </p>
 *
 * @author xiangbin
 */
public interface SubscribeMapper extends BaseMapper<Subscribe> {

    @Select("select * from subscribe where filter_id = #{filterId}")
    Subscribe selectByFilterId(String filterId);

    @Select("select * from subscribe where is_del = 0 order by `order_init` asc")
    List<Subscribe> selectAll();

    @Select("SELECT * FROM subscribe where is_del = 0 and status = 0")
    List<Subscribe> selectByStatusStoped();

    @Select("select * from subscribe where trade_type = #{tradeType}")
    List<Subscribe> selectByTradeType(String tradeType);

    @Select("select * from subscribe where trade_type = #{tradeType} and contract_address = #{contractAddress} and topics = #{topics} ")
    Subscribe selectByTradeTypeAndContractTopics(String tradeType, String contractAddress, String topics);
}
