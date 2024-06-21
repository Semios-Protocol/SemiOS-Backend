package semios.api.service;


import com.baomidou.mybatisplus.extension.service.IService;
import semios.api.model.entity.CollectRecord;

import java.math.BigDecimal;

/**
 * <p>
 * nft plan奖励分配记录 服务类
 * </p>
 *
 * @author zhyyao
 * @since 2024-04-29
 */
public interface ICollectRecordService extends IService<CollectRecord> {

    BigDecimal getTotalCollectedByDaoId(Integer daoId, String address);

    BigDecimal getPlanTotalCollectedByDaoId(String address, String planCode);
}
