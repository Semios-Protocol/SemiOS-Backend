import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import semios.subscription.SemiosSubscriptionApplication;
import semios.subscription.model.entity.Subscriber;
import semios.subscription.service.ISubscriberService;

/**
 * @description: test
 * @author: xiangbin
 * @create: 2022-04-14 15:11
 **/
@RunWith(SpringRunner.class)
@SpringBootTest(classes = SemiosSubscriptionApplication.class)
public class SubscriberServiceTest {

    @Autowired
    private ISubscriberService subscriberService;

    @Test
    public void findAllOpenSub() {
        System.out.println(subscriberService.findAllOpenTenSub());
    }

    @Test
    public void findSubByAppNameAnd() {
        Subscriber subscriber = new Subscriber();
        subscriber.setNetwork("ropsten");
        subscriber.setAddress("0x47dC47921e285af62Db608f3DA7b59bdD6a74d19");
        subscriber.setTopics("[\"e25aa5fa\"]");
        subscriber.setNoticeUrl("http://127.0.0.1:8581/method/call");
        subscriber.setAppName("USDC-1");

        System.out.println(subscriberService.findSubByAppNameAnd(subscriber));
    }
}
