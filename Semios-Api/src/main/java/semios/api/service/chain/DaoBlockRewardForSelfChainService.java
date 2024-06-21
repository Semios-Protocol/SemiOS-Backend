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
public class DaoBlockRewardForSelfChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    @Autowired
    private IDaoAllocationAmountService daoAllocationAmountService;

    public static void main(String[] args) {
        String dataStr = "0xd736d03d7d5aff19789c24a642cd1933868a5206491113084ca6302caa1d232a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000070f00ed4b18fcd555550000000000000000000000000000000000000000000000000000000000002480";
        String data = CommonUtil.removeHexPrefixIfExists(dataStr);
        List<String> dataList = CommonUtil.splitBy32Bytes(data);


        String fromDaoId = CommonUtil.addHexPrefixIfNotExist(dataList.get(0));
        String token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));

        String amount = CommonUtil.hexToTenString(dataList.get(2));
        String round = CommonUtil.hexToTenString(dataList.get(3));

        Dao dao = new Dao();

        DaoAllocationAmount daoAllocationAmount = new DaoAllocationAmount();
        daoAllocationAmount.setType(ProtoDaoConstant.ZERO_ADDRESS.equalsIgnoreCase(token) ? 1 : 0);
        daoAllocationAmount.setAllocationType(DaoAllocationTypeEnum.CURRENT_DAO.getType());
        daoAllocationAmount.setFromDaoId(fromDaoId);
        daoAllocationAmount.setToDaoId(fromDaoId);
        daoAllocationAmount.setToken(token);
        daoAllocationAmount.setToDid(dao.getId());
        daoAllocationAmount.setToDaoName(dao.getDaoName());
        daoAllocationAmount.setToDaoNumber(dao.getDaoNumber());
        daoAllocationAmount.setAmount(new BigDecimal(amount).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO), 18, RoundingMode.FLOOR));
        daoAllocationAmount.setRoundDrb(Integer.valueOf(round));
    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[DaoBlockRewardForSelfChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);


        String fromDaoId = CommonUtil.addHexPrefixIfNotExist(dataList.get(0));
        String token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));

        String amount = CommonUtil.hexToTenString(dataList.get(2));
        String round = CommonUtil.hexToTenString(dataList.get(3));

        Dao dao = daoService.daoDetailByProjectId(fromDaoId);
        if (dao == null) {
            throw new RuntimeException("DaoBlockRewardForSelfChainService cannot find dao");
        }

        DaoAllocationAmount daoAllocationAmount = new DaoAllocationAmount();
        daoAllocationAmount.setType(ProtoDaoConstant.ZERO_ADDRESS.equalsIgnoreCase(token) ? 1 : 0);
        daoAllocationAmount.setAllocationType(DaoAllocationTypeEnum.CURRENT_DAO.getType());
        daoAllocationAmount.setFromDaoId(fromDaoId);
        daoAllocationAmount.setToDaoId(fromDaoId);
        daoAllocationAmount.setToken(token);
        daoAllocationAmount.setToDid(dao.getId());
        daoAllocationAmount.setToDaoName(dao.getDaoName());
        daoAllocationAmount.setToDaoNumber(dao.getDaoNumber());
        daoAllocationAmount.setAmount(new BigDecimal(amount).divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals()), 18, RoundingMode.FLOOR));
        daoAllocationAmount.setRoundDrb(Integer.valueOf(round));
        daoAllocationAmount.setTransactionHash(transactionDto.getTransactionHash());
        daoAllocationAmount.setBlockTime(transactionDto.getBlockTime());

        daoAllocationAmountService.save(daoAllocationAmount);

        log.info("[DaoBlockRewardForSelfChainService] daoAllocationAmount:{}", JacksonUtil.obj2json(daoAllocationAmount));

    }
}
