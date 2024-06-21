package semios.api.service.impl;


import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.mapper.CollectRecordMapper;
import semios.api.model.entity.CollectRecord;
import semios.api.service.ICollectRecordService;

import java.math.BigDecimal;

/**
 * <p>
 * nft plan奖励分配记录 服务实现类
 * </p>
 *
 * @author zhyyao
 * @since 2024-04-29
 */
@Service
public class CollectRecordServiceImpl extends ServiceImpl<CollectRecordMapper, CollectRecord> implements ICollectRecordService {

    @Autowired
    private CollectRecordMapper collectRecordMapper;

    @Override
    public BigDecimal getTotalCollectedByDaoId(Integer daoId, String address) {
        return collectRecordMapper.getTotalCollectedByDaoId(daoId, address);
    }

    @Override
    public BigDecimal getPlanTotalCollectedByDaoId(String address, String planCode) {
        return collectRecordMapper.getPlanTotalCollectedByDaoId(address, planCode);
    }
}
