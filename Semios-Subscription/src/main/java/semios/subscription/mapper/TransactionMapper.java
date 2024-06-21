package semios.subscription.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;
import semios.subscription.model.entity.Transaction;

import java.util.List;

/**
 * <p>
 * transaction记录表 Mapper 接口
 * </p>
 *
 * @author xiangbin
 */
public interface TransactionMapper extends BaseMapper<Transaction> {


    @Select("SELECT * from transaction where notice_status = 0 and notice_times < 6 and TIMESTAMPDIFF(SECOND, create_time,NOW()) > 10")
    List<Transaction> selectNoticeTransaction();

    @Select("SELECT * from transaction where notice_status = 0 and notice_times < 6 and sub_id = #{subId} and TIMESTAMPDIFF(SECOND, create_time,NOW()) > 10")
    List<Transaction> selectNoticeTransactionBySubId(Integer subId);


    @Select("SELECT * from transaction where transaction_hash = #{hash} and  app_name = #{appName}  and notice_status = 1 and sub_id = #{subId} limit 1")
    Transaction selectNoticeTransactionByHash(String appName, String hash, Integer subId);

    @Select("SELECT * from transaction where notice_status = 0 and notice_times < 6  and sub_id = #{subId} and TIMESTAMPDIFF(SECOND, create_time,NOW()) > 15")
    List<Transaction> selectNoticeFailedTransactionBySubId(Integer subId);
}
