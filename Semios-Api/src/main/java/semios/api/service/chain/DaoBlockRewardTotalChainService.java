package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoDrbStatistics;
import semios.api.service.*;
import semios.api.service.common.CommonService;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.util.List;

/**
 * 每个drb第一次铸造时抛出事件记录erc20和eth的总量
 *
 * @description:
 * @author: xiangbin
 * @create: 2022-08-25 09:37
 **/
@Slf4j
@Service
public class DaoBlockRewardTotalChainService implements SubscriberChainService {

    private static final RestTemplate restTemplate = new RestTemplate();
    @Autowired
    private IWorkService workService;
    @Autowired
    private ICanvasService canvasService;
    @Autowired
    private IDaoService daoService;
    @Autowired
    private IDaoDrbStatisticsService daoDrbStatisticsService;
    @Autowired
    private ICanvasDrbStatisticsService canvasDrbStatisticsService;
    @Autowired(required = false)
    private ISubscriptionService iSubscriptionService;

    @Autowired
    private CommonService commonService;

    public static void main(String[] args) {


        String data =
                "c293cfd4fcc160f52fe2fa18878b6a3b257be25492e458847b9f095ea687a64ac7ee4e1f6e72fdeb050714c04f5db8629e250485d12b1414c6c748bce7a48997000000000000000000000000000000000000000000000000000000000000000b00000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000002386f26fc10000000000000000000000000000000000000000000000000000000000000000005768747470733a2f2f746573742d70726f746f64616f2e73332e61702d736f757468656173742d312e616d617a6f6e6177732e636f6d2f6d6574612f776f726b2f5731373031333135333537333635353238302e6a736f6e000000000000000000";
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String projectId = dataList.get(0);
        String canvasId = dataList.get(1);
        String token_id = CommonUtil.hexToTenString(dataList.get(2));
        String workUri = CommonUtil.dynamicArgumentDecoding(data, dataList.get(3), true);
        String price = CommonUtil.hexToTenString(dataList.get(4));

    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {
        log.info("[DaoBlockRewardTotalChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String projectId = dataList.get(0);
        String token = dataList.get(1);
        String erc20Amount = CommonUtil.hexToTenString(dataList.get(2));
        String ethAmount = CommonUtil.hexToTenString(dataList.get(3));
        String round = CommonUtil.hexToTenString(dataList.get(4));


        Dao dao = daoService.daoDetailByProjectId(projectId);
        if (dao == null) {
            log.error("[DaoBlockRewardTotalChainService]未查询到dao记录projectId:{}", projectId);
            throw new RuntimeException("未查询到对应的dao信息");
        }


        // 添加两个drb static记录
        DaoDrbStatistics daoDrbStatistics = daoDrbStatisticsService.selectByDaoIdAndDrbNumber(dao.getId(), Integer.valueOf(round));
        if (daoDrbStatistics == null) {
            log.error("[DaoBlockRewardTotalChainService]未查询到daoDrbStatistics记录projectId:{}", projectId);
            throw new RuntimeException("未查询到对应的daoDrbStatistics信息");
        }
        // 外部20，并且decimal不等于18的时候
//        if (dao.getIsThirdpartyToken().equals(1)  && dao.getErc20TokenDecimals()!=null){
//            daoDrbStatistics.setErc20Amount(new BigDecimal(erc20Amount).divide(new BigDecimal("10").pow(dao.getErc20TokenDecimals())));
//        }else {
//            // 如果为空，默认为18
//            daoDrbStatistics.setErc20Amount(new BigDecimal(erc20Amount).divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals()), 18, BigDecimal.ROUND_UP));
//        }
        daoDrbStatistics.setErc20Amount(new BigDecimal(erc20Amount).divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals()), 18, BigDecimal.ROUND_UP));
        daoDrbStatistics.setEthAmount(new BigDecimal(ethAmount).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()), 18, BigDecimal.ROUND_UP));

        log.info("[DaoBlockRewardTotalChainService]记录projectId:{} daoDrbStatistics:{}", projectId, JacksonUtil.obj2json(daoDrbStatistics));

        Dao updateDao = new Dao();
        updateDao.setId(dao.getId());
        updateDao.setSubdaoAssetPoolBalance(commonService.erc20BalanceOf(dao.getErc20Token(), dao.getFeePool(), dao.getErc20TokenDecimals(), dao.getInputTokenDecimals()).toPlainString());
        daoDrbStatisticsService.updateDaoAndDaoDrbStatistics(daoDrbStatistics, updateDao);


    }
}
