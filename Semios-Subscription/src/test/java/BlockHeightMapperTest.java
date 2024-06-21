import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import semios.subscription.SemiosSubscriptionApplication;
import semios.subscription.mapper.BlockHeightMapper;
import semios.subscription.model.entity.BlockHeight;

/**
 * @description: test
 * @author: xiangbin
 * @create: 2022-04-14 15:11
 **/
@RunWith(SpringRunner.class)
@SpringBootTest(classes = SemiosSubscriptionApplication.class)
public class BlockHeightMapperTest {

    @Autowired
    private BlockHeightMapper blockHeightMapper;

    @Test
    public void update() {
        BlockHeight blockHeight = new BlockHeight();
        blockHeight.setOriginBlock(null);
        System.out.println(blockHeightMapper.updateByBlock(2, blockHeight.getOriginBlock(), "1"));
    }
}
