package semios.api.mapper;


import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;
import semios.api.model.entity.CollectRecord;

import java.math.BigDecimal;

/**
 * <p>
 * nft plan奖励分配记录 Mapper 接口
 * </p>
 *
 * @author zhyyao
 * @since 2024-04-29
 */
public interface CollectRecordMapper extends BaseMapper<CollectRecord> {

    @Select("select IFNULL(sum(c.collect_amount),0) from collect_record c " +
            "  left join work w on c.work_id = w.id " +
            "  where w.owner_address=#{address} and c.dao_id=#{daoId} group by c.dao_id")
    BigDecimal getTotalCollectedByDaoId(Integer daoId, String address);


    @Select("select IFNULL(sum(c.collect_amount),0) from collect_record c " +
            "  left join work w on c.work_id = w.id " +
            "  where w.owner_address=#{address} and c.plan_code=#{planCode} group by c.dao_id")
    BigDecimal getPlanTotalCollectedByDaoId(String address, String planCode);
}
