package semios.subscription.service;

import com.baomidou.mybatisplus.extension.service.IService;
import semios.subscription.model.entity.Subscriber;

import java.util.List;

/**
 * <p>
 * 订阅记录表 服务类
 * </p>
 *
 * @author xiangbin
 */
public interface ISubscriberService extends IService<Subscriber> {

    int saveSub(Subscriber subscriber);

    List<Subscriber> findAllOpenTenSub();

    List<Subscriber> findAllOpenSixtySub();

    List<Subscriber> findSubByAppNameAnd(Subscriber subscriber);

}
