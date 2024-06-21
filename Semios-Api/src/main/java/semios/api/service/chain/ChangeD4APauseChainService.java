package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.ShutdownRecord;
import semios.api.model.enums.ShutdownTypeEnum;
import semios.api.service.IShutdownRecordService;
import semios.api.service.SubscriberChainService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * D4A停机事件
 *
 * @description:
 * @author: xiangbin
 * @create: 2022-08-25 13:43
 **/
@Slf4j
@Service
public class ChangeD4APauseChainService implements SubscriberChainService {

    @Autowired
    private IShutdownRecordService shutdownRecordService;

    public static void main(String[] args) {
        String data =
                "0x524f7178000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000c00000000000000000000000000000000000000000000000000000000000000003b6956f19e7ad14b0d14c853f57973b9e4f154e62fc1424f56998c91390ebcf3ed9cd034648a5c6894512cbf4ae674d9ece7094637b990d6cd4e70a64ba46c6a9f3b4ec8296f0f8be7907b8f1e6bb2fb671ac8d7135a27be8c4681439c1860cf60000000000000000000000000000000000000000000000000000000000000004b3f8d6781c4fa9497486c54aad18d529051240c5b2646cbfa85eff88e642ef96b3f8d6781c4fa9497486c54aad18d529051240c5b2646cbfa85eff88e642ef96b3f8d6781c4fa9497486c54aad18d529051240c5b2646cbfa85eff88e642ef96b3f8d6781c4fa9497486c54aad18d529051240c5b2646cbfa85eff88e642ef96";
        data = data.substring(10);
        System.out.println(data);

        data = data.replace("0x592a7d3e", "0x");
        data = data.replace("0x524f7178", "0x");
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String a = CommonUtil.hexToTenString(dataList.get(2));
        List<String> canvasIds = new ArrayList<>();
        List<String> projectIds = new ArrayList<>();
        int x = 2;
        for (int i = 0; i < Integer.valueOf(a); i++) {
            x = x + 1;
            canvasIds.add(dataList.get(x));
        }
        x = x + 1;
        String b = CommonUtil.hexToTenString(dataList.get(x));
        for (int i = 0; i < Integer.valueOf(b); i++) {
            x = x + 1;
            projectIds.add(dataList.get(x));
        }

        for (String canvasId : canvasIds) {
            System.out.println("canvasId:" + canvasId);

        }

        for (String projectId : projectIds) {
            System.out.println("projectId:" + projectId);

        }

        //
        // String project_id = CommonUtil.dynamicArgumentDecoding(data, dataList.get(0) + dataList.get(1), false);
        // String canvas_id = CommonUtil.dynamicArgumentDecoding(data, dataList.get(1), false);
        // String erc20_token = CommonUtil.dynamicArgumentDecoding(data, dataList.get(2), false);
        //// String amount = CommonUtil.hexToTenString(dataList.get(3));
        // System.out.println("project_id" + " is " + project_id);
        // System.out.println("canvas_id" + " is " + canvas_id);
        // System.out.println("erc20_token" + " is " + erc20_token);
        // System.out.println("amount" + " is " + amount);
    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[ChangeD4APauseChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String is_paused = CommonUtil.hexToTenString(dataList.get(0));

        if ("1".equals(is_paused)) {
            log.info("[ChangeD4APauseChainService] D4A true pause:{}", is_paused);
            ProtoDaoConstant.D4APause = true;
        } else {
            log.info("[ChangeD4APauseChainService] D4A false pause:{}", is_paused);
            ProtoDaoConstant.D4APause = false;
        }
        ShutdownRecord shutdownRecord = new ShutdownRecord();
        shutdownRecord.setType(ShutdownTypeEnum.D4A.getType());
        shutdownRecord.setIsPaused("1".equals(is_paused) ? 0 : 1);// 是否停机,true停机，false不停机
        shutdownRecord.setBlockNumber(transactionDto.getBlockNumber());
        shutdownRecord.setBlockTime(transactionDto.getBlockTime());
        shutdownRecord.setDrbNumber(Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
        shutdownRecord.setTransactionHash(transactionDto.getTransactionHash());
        shutdownRecord.setCreateTime(LocalDateTime.now());

        shutdownRecordService.save(shutdownRecord);
    }
}
