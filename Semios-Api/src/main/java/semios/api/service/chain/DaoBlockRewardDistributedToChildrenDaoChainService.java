package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoAllocationAmount;
import semios.api.model.enums.DaoAllocationTypeEnum;
import semios.api.service.IDaoAllocationAmountService;
import semios.api.service.IDaoService;
import semios.api.service.SubscriberChainService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

/**
 * 1.3 奖励分配事件
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-11-17 13:43
 **/
@Slf4j
@Service
public class DaoBlockRewardDistributedToChildrenDaoChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    @Autowired
    private IDaoAllocationAmountService daoAllocationAmountService;

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

        log.info("[DaoBlockRewardDistributedToChildrenDaoChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);


        String fromDaoId = CommonUtil.addHexPrefixIfNotExist(dataList.get(0));
        String toDaoId = CommonUtil.addHexPrefixIfNotExist(dataList.get(1));
        String token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2)));

        String amount = CommonUtil.hexToTenString(dataList.get(3));
        String round = CommonUtil.hexToTenString(dataList.get(4));

        Dao dao = daoService.daoDetailByProjectId(toDaoId);
        if (dao == null) {
            throw new RuntimeException("DaoBlockRewardDistributedToChildrenDaoChainService cannot find dao");
        }

        DaoAllocationAmount daoAllocationAmount = new DaoAllocationAmount();
        daoAllocationAmount.setType(ProtoDaoConstant.ZERO_ADDRESS.equalsIgnoreCase(token) ? 1 : 0);
        daoAllocationAmount.setAllocationType(DaoAllocationTypeEnum.OTHER_DAO.getType());
        daoAllocationAmount.setFromDaoId(fromDaoId);
        daoAllocationAmount.setToDaoId(toDaoId);
        daoAllocationAmount.setToken(token);
        daoAllocationAmount.setToDid(dao.getId());
        daoAllocationAmount.setToDaoName(dao.getDaoName());
        daoAllocationAmount.setToDaoNumber(dao.getDaoNumber());
        daoAllocationAmount.setAmount(new BigDecimal(amount).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()), 18, RoundingMode.FLOOR));
        daoAllocationAmount.setRoundDrb(Integer.valueOf(round));
        daoAllocationAmount.setTransactionHash(transactionDto.getTransactionHash());
        daoAllocationAmount.setBlockTime(transactionDto.getBlockTime());

        daoAllocationAmountService.save(daoAllocationAmount);

        log.info("[DaoBlockRewardDistributedToChildrenDaoChainService] daoAllocationAmount:{}", JacksonUtil.obj2json(daoAllocationAmount));

    }
}
