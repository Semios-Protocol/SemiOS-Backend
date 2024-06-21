package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.enums.BasicDaoEnum;
import semios.api.service.IDaoService;
import semios.api.service.SubscriberChainService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.util.List;

/**
 * basic DAO 解锁事件
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-09-04 13:43
 **/
@Slf4j
@Service
public class BasicDaoUnlockedChainService implements SubscriberChainService {

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

        log.info("[BasicDaoUnlockedChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));

        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());

        String projectId = CommonUtil.removeHexPrefixIfExists(topics.get(1));

        Dao dao = daoService.daoDetailByProjectId(projectId);
        if (dao == null) {
            throw new RuntimeException("BasicDaoUnlockedChainService cannot find dao");
        }


        Dao updateDao = new Dao();
        updateDao.setId(dao.getId());
        updateDao.setBasicDao(BasicDaoEnum.PROTO_DAO.getBasicType());
        daoService.updateById(updateDao);
        log.info("[BasicDaoUnlockedChainService] updateDao:{}", dao.getId() + "basic dao to proto dao");

    }
}
