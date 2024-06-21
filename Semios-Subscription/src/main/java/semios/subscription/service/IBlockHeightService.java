package semios.subscription.service;

import com.baomidou.mybatisplus.extension.service.IService;
import semios.subscription.model.entity.BlockHeight;

/**
 * <p>
 * 订阅区块高度表 服务类
 * </p>
 *
 * @author xiangbin
 */
public interface IBlockHeightService extends IService<BlockHeight> {

    BlockHeight getBySubId(String subId);

    int updateFilterIdAndToBlockToNull(Integer id);
}
