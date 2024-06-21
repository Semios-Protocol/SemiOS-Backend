package semios.subscription.service;

import com.baomidou.mybatisplus.extension.service.IService;
import semios.subscription.model.entity.SubNumValue;

/**
 * <p>
 * 数值类型订阅最新值 服务类
 * </p>
 *
 * @author xiangbin
 */
public interface ISubNumValueService extends IService<SubNumValue> {

    SubNumValue selectAddressAndTopic(String netWork, String address, String topic);

    SubNumValue selectByFilterId(Integer filterId);

}
