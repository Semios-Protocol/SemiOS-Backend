package semios.api.service.chain;


import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.TokenReceivedRecord;
import semios.api.model.enums.TokenReceiveTypeEnum;
import semios.api.model.enums.TokenTypeEnum;
import semios.api.model.vo.req.UserLiquidityStatisticsVo;
import semios.api.service.IDaoService;
import semios.api.service.ITokenReceivedRecordService;
import semios.api.service.IUserHarvestTokenService;
import semios.api.service.SubscriberChainService;
import semios.api.service.feign.IProtoDaoDexService;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;
import java.util.Map;

/**
 * 铸造非topup的work，用户获取到的收益
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-06-14 11:40
 **/
@Slf4j
@Service
public class TopUpErc20SplittedChainService implements SubscriberChainService {

    @Autowired
    private ITokenReceivedRecordService tokenReceivedRecordService;

    @Autowired
    private IDaoService daoService;

    @Autowired(required = false)
    private IProtoDaoDexService protodaoDexService;

    @Autowired(required = false)
    private ISubscriptionService subscriptionService;

    @Autowired
    private IUserHarvestTokenService userHarvestTokenService;

    public static void main(String[] args) {
        String data =
                "b2ec93de13a0b26ae10bcb168f6cc63cdc58d9413e79059e1ff564ab440d833e0000000000000000000000008405fee3e2a3b9a96c794a9237a33a6cbbc03221000000000000000000000000000000000000000000034f086f3b33b684000000";
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String projectId = dataList.get(0);
        String erc20Token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));
        String amount = CommonUtil.hexToTenString(dataList.get(2));
        System.out.println("projectId" + " is " + projectId);
        System.out.println("erc20Token" + " is " + erc20Token);
        System.out.println("amount" + " is " + amount);
    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {
        log.info("[TopUpErc20SplittedChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String projectId = dataList.get(0);
        String receiver = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));
        String treasuryAddress = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2)));
        String topUpErc20AmountToSender = CommonUtil.hexToTenString(dataList.get(3)); //流向用户地址的erc20数量  //合约名称修改 topUpOutputAmountToSender
        String topUpErc20AmountToTreasury = CommonUtil.hexToTenString(dataList.get(4)); //流向国库的erc20数量  // 合约名称修改 topUpOutputAmountToTreasury

        Dao dao = daoService.daoDetailByProjectId(projectId);
        if (dao == null) {
            log.error("[TopUpErc20SplittedChainService] dao is null transactionDao:{}",
                    JacksonUtil.obj2json(transactionDto));
            throw new RuntimeException("dao is null");
        }

        BigDecimal tokenNum =
                new BigDecimal(topUpErc20AmountToSender).divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals()), 18, RoundingMode.FLOOR);

        Result<String> result = subscriptionService.ethGetTransactionByHash(ProtoDaoConstant.netWork,
                CommonUtil.addHexPrefixIfNotExist(transactionDto.getTransactionHash()));
        log.info("[TopUpErc20SplittedChainService] result:{}", result.getData());
        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[TopUpErc20SplittedChainService] error result:{}", result.getResultDesc());
            throw new RuntimeException("GetTransactionByHash error");
        }
        Map<String, Object> objectMap = JacksonUtil.json2map(result.getData());
        if (objectMap == null) {
            log.error("[TopUpErc20SplittedChainService] objectMap is null result:{}", result.getData());
            throw new RuntimeException("GetTransactionByHash objectMap is null");
        }
        String fromAddress = (String) objectMap.get("from");
        fromAddress = CommonUtil.addHexPrefixIfNotExist(fromAddress.toLowerCase());

        TokenReceivedRecord tokenReceivedRecord = new TokenReceivedRecord();
        tokenReceivedRecord.setTokenNum(tokenNum);
        tokenReceivedRecord.setEthNum(BigDecimal.ZERO);
        tokenReceivedRecord.setTokenNumBalance(tokenNum);
        tokenReceivedRecord.setReceiveType(TokenReceiveTypeEnum.UNLOCK.getType());
        tokenReceivedRecord.setTokenType(TokenTypeEnum.UNLOCK.getType());
        tokenReceivedRecord.setDaoNumber(dao.getDaoNumber());
        tokenReceivedRecord.setReceiveId(dao.getId());

        tokenReceivedRecord.setProjectId(projectId);
        tokenReceivedRecord.setCanvasId(null);
        tokenReceivedRecord.setBlockNumber(transactionDto.getBlockNumber());
        tokenReceivedRecord.setBlockTime(transactionDto.getBlockTime());
        tokenReceivedRecord.setTransactionHash(transactionDto.getTransactionHash());
        tokenReceivedRecord.setDrbNumber(Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));

        tokenReceivedRecord.setFromAddress(ProtoDaoConstant.protocolContract);
        tokenReceivedRecord.setToAddress(fromAddress);
        tokenReceivedRecord.setReceiveAddress(receiver);

        UserLiquidityStatisticsVo userLiquidityStatisticsVo = new UserLiquidityStatisticsVo();
        userLiquidityStatisticsVo.setErc20Address(dao.getErc20Token());
        userLiquidityStatisticsVo.setUserAddress(fromAddress);
        Result<Integer> resultInt = protodaoDexService.syncErc20Balance(userLiquidityStatisticsVo);
        if (resultInt.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[TopUpErc20SplittedChainService] daoId:{} syncErc20Balance error result:{}", dao.getId(),
                    resultInt.getResultDesc());
            throw new RuntimeException("dao sync erc20Balance error");
        }

        tokenReceivedRecordService.save(tokenReceivedRecord);
    }

}
