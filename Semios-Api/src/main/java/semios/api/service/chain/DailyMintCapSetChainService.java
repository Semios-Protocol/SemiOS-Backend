package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.service.IDaoService;
import semios.api.service.SubscriberChainService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.util.List;

/**
 * 追加DaoToken的发放额度
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-09-04 13:43
 **/
@Slf4j
@Service
public class DailyMintCapSetChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    public static void main(String[] args) {
        String data = "58d2d3c7a28441df4656fab3eb0e51d94a45b38100a7e74dba50afae6124d6d72ae972c0263f681f4f35a5fbffcb8860ddb2ceb32ab4e3a4d8f6912888dca8c8000000000000000000000000ffd23ddffa1d6181c8a711a8b4939eedf9cc00bd0000000000000000000000000000000000000000000000000000000000000000";
        List<String> dataList = CommonUtil.splitBy32Bytes(data);


        String projectId = dataList.get(0);
        String canvasId = dataList.get(1);
        String erc20Token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2)));
        String amount = CommonUtil.hexToTenString(dataList.get(3));
        System.out.println("project_id" + " is " + projectId);
        System.out.println("canvas_id" + " is " + canvasId);
        System.out.println("erc20_token" + " is " + erc20Token);
        System.out.println("amount" + " is " + amount);
    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[DailyMintCapSetChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));

        List<String> topic = JacksonUtil.json2StringList(transactionDto.getTopics());

        String projectId = CommonUtil.removeHexPrefixIfExists(topic.get(1));
//        List<String> data = JacksonUtil.json2StringList(transactionDto.getData());
        String dailyMintCap = CommonUtil.hexToTenString(transactionDto.getData());

        Dao dao = daoService.daoDetailByProjectId(projectId);
        if (dao == null) {
            throw new RuntimeException("DailyMintCapSetChainService cannot find dao");
        }

        Dao updateDao = new Dao();
        updateDao.setId(dao.getId());
        log.info("[DailyMintCapSetChainService] updateDao:{} origin dailyMintCap is:{}", dao.getId(), dao.getDailyMintCap());
        updateDao.setDailyMintCap(Integer.parseInt(dailyMintCap));

        daoService.updateById(updateDao);
        log.info("[DailyMintCapSetChainService] updateDao:{} dailyMintCap to:{}", dao.getId(), updateDao.getDailyMintCap());

    }
}
