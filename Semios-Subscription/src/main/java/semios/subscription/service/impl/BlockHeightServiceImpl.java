package semios.subscription.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.subscription.mapper.BlockHeightMapper;
import semios.subscription.model.entity.BlockHeight;
import semios.subscription.service.IBlockHeightService;

/**
 * <p>
 * 订阅区块高度表 服务实现类
 * </p>
 *
 * @author xiangbin
 */
@Service
public class BlockHeightServiceImpl extends ServiceImpl<BlockHeightMapper, BlockHeight> implements IBlockHeightService {

    @Autowired
    private BlockHeightMapper blockHeightMapper;

    @Override
    public BlockHeight getBySubId(String subId) {
        return blockHeightMapper.selectBySubId(subId);
    }

    @Override
    public int updateFilterIdAndToBlockToNull(Integer id) {
        return blockHeightMapper.updateFilterIdAndToBlockToNull(id);
    }
}
