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
import semios.api.model.entity.UserHarvestToken;
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
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 监听minter领取代币事件
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-06-14 11:40
 **/
@Slf4j
@Service
public class ClaimNftMinterRewardChainService implements SubscriberChainService {

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

    // https://goerli.etherscan.io/tx/0x8b6b71e9aec98a340bb0051ec5f4fc6cffd8d215b01d721bb584f0f7f54b5bec#eventlog

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
        log.info("[PDClaimNftMinterRewardChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String projectId = dataList.get(0);
        String erc20Token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));
        String erc20Amount = CommonUtil.hexToTenString(dataList.get(2));
        String ethAmount = CommonUtil.hexToTenString(dataList.get(3));
        String receiver = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(4)));

        Dao dao = daoService.daoDetailByProjectId(projectId);
        if (dao == null) {
            log.error("[PDClaimNftMinterRewardChainService] dao is null transactionDao:{}",
                    JacksonUtil.obj2json(transactionDto));
            throw new RuntimeException("dao is null");
        }

        BigDecimal tokenNum =
                new BigDecimal(erc20Amount).divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals()), 18, RoundingMode.FLOOR);
        BigDecimal ethNum =
                new BigDecimal(ethAmount).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()), 18, RoundingMode.FLOOR);
        if (tokenNum.compareTo(BigDecimal.ZERO) == 0 && ethNum.compareTo(BigDecimal.ZERO) == 0) {
            log.info("[PDClaimNftMinterRewardChainService] tokenNum & ethNum is zero transactionDao:{}",
                    JacksonUtil.obj2json(transactionDto));
            return;
        }
//        List<Dao> daoList = daoService.selectDaoListByErc20Token(dao.getErc20Token());
//        List<Integer> daoIdList = daoList.stream().map(Dao::getId).collect(Collectors.toList());
        List<Integer> daoIdList = Collections.singletonList(dao.getId());

        Result<String> result = subscriptionService.ethGetTransactionByHash(ProtoDaoConstant.netWork,
                CommonUtil.addHexPrefixIfNotExist(transactionDto.getTransactionHash()));
        log.info("[PDClaimNftMinterRewardChainService] result:{}", result.getData());
        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[PDClaimNftMinterRewardChainService] error result:{}", result.getResultDesc());
            throw new RuntimeException("GetTransactionByHash error");
        }
        Map<String, Object> objectMap = JacksonUtil.json2map(result.getData());
        if (objectMap == null) {
            log.error("[PDClaimNftMinterRewardChainService] objectMap is null result:{}", result.getData());
            throw new RuntimeException("GetTransactionByHash objectMap is null");
        }
        String fromAddress = (String) objectMap.get("from");
        fromAddress = CommonUtil.addHexPrefixIfNotExist(fromAddress.toLowerCase());

        List<UserHarvestToken> userHarvestTokenList = userHarvestTokenService.selectUnclaimedTokenByUserAddress(fromAddress);
        userHarvestTokenList = userHarvestTokenList.stream().filter(v -> daoIdList.contains(v.getDaoId())).collect(Collectors.toList());
        BigDecimal unclaimedToken = userHarvestTokenList.stream().map(UserHarvestToken::getUnclaimedToken).reduce(BigDecimal.ZERO, BigDecimal::add);
        if (tokenNum.compareTo(unclaimedToken) > 0) {
            log.error("[PDClaimNftMinterRewardChainService] daoId:{} tokenNum:{} unclaimedToken:{}", dao.getId(), tokenNum, unclaimedToken);
        }
        //实际只有一个UserHarvestToken，每个dao单独抛出事件
        for (UserHarvestToken userHarvestToken : userHarvestTokenList) {
            //token
            BigDecimal receivedToken =
                    userHarvestToken.getReceivedToken() == null ? BigDecimal.ZERO : userHarvestToken.getReceivedToken();
            userHarvestToken.setReceivedToken(receivedToken.add(tokenNum));
            userHarvestToken.setUnclaimedToken(BigDecimal.ZERO);

            //eth
            BigDecimal receivedEth =
                    userHarvestToken.getReceivedEth() == null ? BigDecimal.ZERO : userHarvestToken.getReceivedEth();
            userHarvestToken.setReceivedEth(receivedEth.add(ethNum));
            userHarvestToken.setUnclaimedEth(BigDecimal.ZERO);

            userHarvestToken.setLastTransactionHash(transactionDto.getTransactionHash());
        }

        TokenReceivedRecord tokenReceivedRecord = new TokenReceivedRecord();
        tokenReceivedRecord.setTokenNum(tokenNum);
        tokenReceivedRecord.setEthNum(ethNum);
        tokenReceivedRecord.setTokenNumBalance(tokenNum);
        tokenReceivedRecord.setReceiveType(TokenReceiveTypeEnum.MINTER.getType());
        tokenReceivedRecord.setTokenType(TokenTypeEnum.COLLECT.getType());
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

        //tokenReceivedRecord.setReceiveAddress(fromAddress);
        // 修改受益人
        tokenReceivedRecord.setReceiveAddress(receiver);

        UserLiquidityStatisticsVo userLiquidityStatisticsVo = new UserLiquidityStatisticsVo();
        userLiquidityStatisticsVo.setErc20Address(dao.getErc20Token());
        userLiquidityStatisticsVo.setUserAddress(fromAddress);
        Result<Integer> resultInt = protodaoDexService.syncErc20Balance(userLiquidityStatisticsVo);
        if (resultInt.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[PDClaimNftMinterRewardChainService] daoId:{} syncErc20Balance error result:{}", dao.getId(),
                    resultInt.getResultDesc());
            throw new RuntimeException("dao sync erc20Balance error");
        }

        log.info("[PDClaimNftMinterRewardChainService] daoId:{} tokenReceivedRecord:{} userHarvestTokenList:{}", dao.getId(), JacksonUtil.obj2json(tokenReceivedRecord), JacksonUtil.obj2json(userHarvestTokenList));
        int i = tokenReceivedRecordService.saveTokenReceivedAndUpdateHarvestTokenList(tokenReceivedRecord, userHarvestTokenList);
        log.info("[PDClaimNftMinterRewardChainService] daoId:{} update i:{}", dao.getId(), i);

    }
}
