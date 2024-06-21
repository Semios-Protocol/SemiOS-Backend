package semios.api.service.common;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import semios.api.interceptor.S3Service;
import semios.api.model.entity.Dao;
import semios.api.model.entity.Work;
import semios.api.service.ICanvasDrbStatisticsService;
import semios.api.service.ICanvasService;
import semios.api.service.IDaoAllocationStrategyService;
import semios.api.service.IDaoDrbStatisticsService;
import semios.api.service.IDaoService;
import semios.api.service.ISubscribeService;
import semios.api.service.IUserHarvestTokenService;
import semios.api.service.IUserTopupHarvestService;
import semios.api.service.IWorkService;
import semios.api.service.common.CommonService;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.JacksonUtil;

@ContextConfiguration(classes = {CommonService.class})
@ExtendWith(SpringExtension.class)
class CommonServiceTest {
    @Autowired
    private CommonService commonService;

    @MockBean
    private ICanvasDrbStatisticsService iCanvasDrbStatisticsService;

    @MockBean
    private ICanvasService iCanvasService;

    @MockBean
    private IDaoAllocationStrategyService iDaoAllocationStrategyService;

    @MockBean
    private IDaoDrbStatisticsService iDaoDrbStatisticsService;

    @MockBean
    private IDaoService iDaoService;

    @MockBean
    private ISubscribeService iSubscribeService;

    @MockBean
    private ISubscriptionService iSubscriptionService;

    @MockBean
    private IUserHarvestTokenService iUserHarvestTokenService;

    @MockBean
    private IUserTopupHarvestService iUserTopupHarvestService;

    @MockBean
    private IWorkService iWorkService;

    @MockBean
    private S3Service s3Service;

    /**
     * Method under test: {@link CommonService#searchTokenIncome(String, String, String, String)}
     */
    @Test
    void testSearchTokenIncome() {
        System.out.println(commonService.searchTokenIncome("2024-01-08", "2024-01-11", "0x311025bbca13feb83c779a5127b9c77c9d26f829", "0x12b4745293c78282ebe8da3984cc0e325257f6c1"));
    }

    /**
     * Method under test
     */
    @Test
    void testSearchTokeCost() {
        System.out.println(commonService.searchTokeCost("2024-01-08", "2024-01-11", "0x311025bbca13feb83c779a5127b9c77c9d26f829", "0x12b4745293c78282ebe8da3984cc0e325257f6c1"));
    }

    /**
     * Method under test
     */
    @Test
    void searchEthIncome() {
        System.out.println(commonService.searchEthIncome("2024-01-07", "2024-01-11", "0xbD9C96244F08e92717d9Ad614cA98dB58BC4bE5A"));
    }

    /**
     * Method under test
     */
    @Test
    void searchEthCost() {
        System.out.println(commonService.searchEthCost("2024-01-08", "2024-01-11", "0xbD9C96244F08e92717d9Ad614cA98dB58BC4bE5A"));
    }


    @Test
    void updateMinterWorkTopupHarvest(){
        // 监听到mint nft的存入和余额更新
        String projectId = "9a3391f24b6b70e39c32708ccda493e4fe7a6f6b56eb539646525c8435205417";

        Dao mountDao = iDaoService.daoDetailByProjectId(projectId);
        System.out.println("获取到的mount dao info:" + JacksonUtil.obj2json(mountDao));

        String mount_work_number = "2";
        Work mountWork = iWorkService.selectWorkByNumber(mountDao.getId(),mount_work_number);
        System.out.println("获取到的mount work info:" + JacksonUtil.obj2json(mountWork));

        // 数据库多出一条记录
        commonService.updateMinterWorkTopupHarvest(projectId,0,mountDao,mountWork);

        // drb 周期结束，余额更新
        commonService.updateMinterWorkTopupHarvest(projectId,Integer.valueOf(mount_work_number));
    }
}

