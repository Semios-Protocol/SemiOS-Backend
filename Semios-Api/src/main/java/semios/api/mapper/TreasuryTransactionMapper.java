package semios.api.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.ibatis.annotations.Select;
import semios.api.model.entity.TreasuryTransaction;
import semios.api.model.vo.res.TogetherDaoTreasuryTransactionVo;

/**
 * <p>
 * 国库交易表 Mapper 接口
 * </p>
 *
 * @author zhyyao
 * @since 2024-02-22
 */
public interface TreasuryTransactionMapper extends BaseMapper<TreasuryTransaction> {
    @Select("select from_address,to_address,transaction_hash,amount,create_time as createTimeStamp,IFNULL(is_use_treasury,0) as is_use_treasury from treasury_transaction where project_id=#{projectId} order by create_time desc ")
    Page<TogetherDaoTreasuryTransactionVo> selectTogetherDaoTreasuryTransactionList(IPage<TreasuryTransaction> page, String projectId);
}
