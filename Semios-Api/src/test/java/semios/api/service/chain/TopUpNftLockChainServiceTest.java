package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;
import semios.api.SemiosApiApplication;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.Work;
import semios.api.model.enums.WorkLockStatusEnum;
import semios.api.service.IDaoService;
import semios.api.service.IWorkService;
import semios.api.utils.CommonUtil;

import javax.annotation.Resource;

import static org.junit.jupiter.api.Assertions.*;

@RunWith(SpringRunner.class)
@SpringBootTest(classes = SemiosApiApplication.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@Transactional
@Slf4j
class TopUpNftLockChainServiceTest {

    @Resource
    TopUpNftLockChainService topUpNftLockChainService;

    @Resource
    IWorkService workService;

    @Resource
    IDaoService daoService;

    @Test
    void handleTrade() {
        try {
            // 测试监听事件
            TransactionDto transactionDto = new TransactionDto();
            transactionDto.setData("0x00000000000000000000000004ac45d77f01407f53f02a7ece2b25fe771f620c000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000004f49620000000000000000000000000000000000000000000000000000000000000106");

            // info
            /*
            * erc721Address:0x00000000000000000000000004ac45d77f01407f53f02a7ece2b25fe771f620c
            * tokenId:0x0000000000000000000000000000000000000000000000000000000000000002
            * lockStartBlock:0x00000000000000000000000000000000000000000000000000000000004f4962
            * duration:0x0000000000000000000000000000000000000000000000000000000000000106
            * */

            topUpNftLockChainService.handleTrade(transactionDto);

            Dao dao = daoService.selectDaoByErc721Token("0x4ac45d77f01407f53f02a7ece2b25fe771f620c");
            assert dao!=null;

            Work work=workService.selectWorkByNumber(dao.getId(),"2");
            assert work!=null;

            assertEquals(work.getLockStatus(), WorkLockStatusEnum.LOCK.getStatus(),"锁定状态");
            assertEquals(work.getLockStartBlock(),CommonUtil.hexToTenString("0x00000000000000000000000000000000000000000000000000000000004f4962") ,"锁定开始区块");
            assertEquals(work.getLockDurationBlock(), CommonUtil.hexToTenString("0x0000000000000000000000000000000000000000000000000000000000000106"),"锁定区块数");

        }catch (Exception e){
            log.info("异常",e);
        }
    }
}