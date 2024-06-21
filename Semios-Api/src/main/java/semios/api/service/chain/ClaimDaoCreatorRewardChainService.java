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
import semios.api.service.SubscriberChainService;
import semios.api.service.feign.IProtoDaoDexService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

/**
 * @description: 监听dao领取代币事件
 * @author: xiangbin
 * @create: 2022-08-25 11:40
 **/
@Slf4j
@Service
public class ClaimDaoCreatorRewardChainService implements SubscriberChainService {

    @Autowired
    private ITokenReceivedRecordService tokenReceivedRecordService;

    @Autowired
    private IDaoService daoService;

    @Autowired(required = false)
    private IProtoDaoDexService protodaoDexService;

    public static void main(String[] args) {
        String data =
                "b2ec93de13a0b26ae10bcb168f6cc63cdc58d9413e79059e1ff564ab440d833e0000000000000000000000008405fee3e2a3b9a96c794a9237a33a6cbbc03221000000000000000000000000000000000000000000034f086f3b33b684000000";
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String project_id = dataList.get(0);
        String erc20_token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));
        String amount = CommonUtil.hexToTenString(dataList.get(2));
        System.out.println("project_id" + " is " + project_id);
        System.out.println("erc20_token" + " is " + erc20_token);
        System.out.println("amount" + " is " + amount);
    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {
        log.info("[ClaimDaoCreatorRewardChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String project_id = dataList.get(0);
        String erc20_token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));
        String erc20Amount = CommonUtil.hexToTenString(dataList.get(2));
        String ethAmount = CommonUtil.hexToTenString(dataList.get(3));
        String receiver = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(4)));

        Dao dao = daoService.daoDetailByProjectId(project_id);
        if (dao == null) {
            log.error("[ClaimDaoCreatorRewardChainService] dao is null transactionDao:{}",
                    JacksonUtil.obj2json(transactionDto));
            throw new RuntimeException("dao is null");
        }
        BigDecimal tokenNum =
                new BigDecimal(erc20Amount).divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals()), 18, RoundingMode.FLOOR);
        BigDecimal ethNum =
                new BigDecimal(ethAmount).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()), 18, RoundingMode.FLOOR);
        if (tokenNum.compareTo(BigDecimal.ZERO) == 0 && ethNum.compareTo(BigDecimal.ZERO) == 0) {
            log.info("[ClaimDaoCreatorRewardChainService] tokenNum & ethNum is zero transactionDao:{}",
                    JacksonUtil.obj2json(transactionDto));
            return;
        }

        TokenReceivedRecord tokenReceivedRecord = new TokenReceivedRecord();
        tokenReceivedRecord.setTokenNum(tokenNum);
        tokenReceivedRecord.setEthNum(ethNum);
        tokenReceivedRecord.setReceiveType(TokenReceiveTypeEnum.DAO.getType());
        tokenReceivedRecord.setTokenType(TokenTypeEnum.COLLECT.getType());
        tokenReceivedRecord.setDaoNumber(dao.getDaoNumber());
        tokenReceivedRecord.setReceiveId(dao.getId());

        // tokenReceivedRecord.setReceiveAddress(dao.getOwnerAddress());
        tokenReceivedRecord.setProjectId(project_id);
        tokenReceivedRecord.setCanvasId(null);
        tokenReceivedRecord.setBlockNumber(transactionDto.getBlockNumber());
        tokenReceivedRecord.setBlockTime(transactionDto.getBlockTime());
        tokenReceivedRecord.setTransactionHash(transactionDto.getTransactionHash());
        tokenReceivedRecord.setDrbNumber(Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));

        tokenReceivedRecord.setFromAddress(ProtoDaoConstant.protocolContract);
        tokenReceivedRecord.setToAddress(dao.getOwnerAddress());

        // 修改受益人
        tokenReceivedRecord.setReceiveAddress(receiver);

        //tokenNum
        if (dao.getReceivedToken() != null && dao.getReceivedToken().compareTo(BigDecimal.ZERO) > 0) {
            dao.setReceivedToken(dao.getReceivedToken().add(tokenNum));
        } else {
            dao.setReceivedToken(tokenNum);
        }
        dao.setUnclaimedToken(BigDecimal.ZERO);

        //ethNum
        if (dao.getReceivedEth() != null && dao.getReceivedEth().compareTo(BigDecimal.ZERO) > 0) {
            dao.setReceivedEth(dao.getReceivedEth().add(ethNum));
        } else {
            dao.setReceivedEth(ethNum);
        }
        dao.setUnclaimedEth(BigDecimal.ZERO);

        UserLiquidityStatisticsVo userLiquidityStatisticsVo = new UserLiquidityStatisticsVo();
        userLiquidityStatisticsVo.setErc20Address(dao.getErc20Token());
        userLiquidityStatisticsVo.setUserAddress(dao.getOwnerAddress());
        Result<Integer> result = protodaoDexService.syncErc20Balance(userLiquidityStatisticsVo);
        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            throw new RuntimeException("dao sync erc20Balance error");
        }
        tokenReceivedRecordService.saveTokenReceivedAndUpdateDao(tokenReceivedRecord, dao);

    }
}
