package semios.api.service;

import com.baomidou.mybatisplus.extension.service.IService;
import semios.api.model.entity.Subscribe;

import java.util.List;

/**
 * <p>
 * 订阅记录 服务类
 * </p>
 *
 * @author xiangbin
 */
public interface ISubscribeService extends IService<Subscribe> {

    Subscribe selectByFilterId(String filterId);

    List<Subscribe> selectAll();

    List<Subscribe> selectByStatusStoped();

    List<Subscribe> selectByTradeType(String tradeType);

    Subscribe selectByTradeTypeAndContractTopics(String tradeType, String contractAddress, String topics);

}
