package semios.api.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;
import semios.api.model.entity.DaoAppendTokenLog;

/**
 * <p>
 * dao代币追加记录 Mapper 接口
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface DaoAppendTokenLogMapper extends BaseMapper<DaoAppendTokenLog> {


    @Select("select * from dao_append_token_log where transaction_hash = #{transactionHash}")
    DaoAppendTokenLog selectByTransactionHash(String transactionHash);

}
