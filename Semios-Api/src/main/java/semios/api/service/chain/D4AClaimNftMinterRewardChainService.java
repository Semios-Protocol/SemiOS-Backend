//package semios.api.service.chain;
//
//import lombok.extern.slf4j.Slf4j;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.stereotype.Service;
//import semios.api.model.dto.common.ProtoDaoConstant;
//import semios.api.model.dto.common.Result;
//import semios.api.model.dto.common.ResultDesc;
//import semios.api.model.dto.response.TransactionDto;
//import semios.api.model.entity.Dao;
//import semios.api.model.entity.TokenReceivedRecord;
//import semios.api.model.entity.UserHarvestToken;
//import semios.api.model.enums.TokenReceiveTypeEnum;
//import semios.api.model.enums.TokenTypeEnum;
//import semios.api.model.vo.req.UserLiquidityStatisticsVo;
//import semios.api.service.IDaoService;
//import semios.api.service.ITokenReceivedRecordService;
//import semios.api.service.IUserHarvestTokenService;
//import semios.api.service.SubscriberChainService;
//import semios.api.service.feign.IProtoDaoDexService;
//import semios.api.service.feign.ISubscriptionService;
//import semios.api.utils.CommonUtil;
//import semios.api.utils.JacksonUtil;
//
//import java.math.BigDecimal;
//import java.math.RoundingMode;
//import java.util.Arrays;
//import java.util.Collections;
//import java.util.List;
//import java.util.Map;
//import java.util.function.Supplier;
//import java.util.stream.Collectors;
//
///**
// * 监听minter领取代币事件
// *
// * @description:
// * @author: xiangbin
// * @create: 2023-06-14 11:40
// *
// * @update: zhyyao 已废弃
// *
// **/
//@Deprecated
//@Slf4j
//@Service
//public class D4AClaimNftMinterRewardChainService implements SubscriberChainService {
//
//    @Autowired
//    private ITokenReceivedRecordService tokenReceivedRecordService;
//
//    @Autowired
//    private IDaoService daoService;
//
//    @Autowired(required = false)
//    private IProtoDaoDexService protodaoDexService;
//
//    @Autowired(required = false)
//    private ISubscriptionService subscriptionService;
//
//    @Autowired
//    private IUserHarvestTokenService userHarvestTokenService;
//
//    // https://goerli.etherscan.io/tx/0x8b6b71e9aec98a340bb0051ec5f4fc6cffd8d215b01d721bb584f0f7f54b5bec#eventlog
//
//    @Override
//    public void handleTrade(TransactionDto transactionDto) throws Exception {
//        log.info("[D4AClaimNftMinterRewardChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
//        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
//        List<String> dataList = CommonUtil.splitBy32Bytes(data);
//        String projectId = dataList.get(0);
//        String erc20Token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));
//        String amount = CommonUtil.hexToTenString(dataList.get(2));
//
//        Dao dao = daoService.daoDetailByProjectId(projectId);
//        if (dao == null) {
//            log.error("[D4AClaimNftMinterRewardChainService] dao is null transactionDao:{}",
//                    JacksonUtil.obj2json(transactionDto));
//            throw new RuntimeException("dao is null");
//        }
//        if (amount == null) {
//            log.info("[D4AClaimNftMinterRewardChainService] amount is null transactionDao:{}",
//                    JacksonUtil.obj2json(transactionDto));
//            return;
//        }
//        BigDecimal tokenNum =
//                new BigDecimal(amount).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO), 18, RoundingMode.FLOOR);
//        if (tokenNum.compareTo(BigDecimal.ZERO) == 0) {
//            log.info("[D4AClaimNftMinterRewardChainService] tokenNum is zero transactionDao:{}",
//                    JacksonUtil.obj2json(transactionDto));
//            return;
//        }
////        List<Dao> daoList = daoService.selectDaoListByErc20Token(dao.getErc20Token());
////        List<Integer> daoIdList = daoList.stream().map(Dao::getId).collect(Collectors.toList());
//        List<Integer> daoIdList = Collections.singletonList(dao.getId());
//
//        Result<String> result = subscriptionService.ethGetTransactionByHash(ProtoDaoConstant.netWork,
//                CommonUtil.addHexPrefixIfNotExist(transactionDto.getTransactionHash()));
//        log.info("[D4AClaimNftMinterRewardChainService] result:{}", result.getData());
//        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//            log.error("[D4AClaimNftMinterRewardChainService] error result:{}", result.getResultDesc());
//            throw new RuntimeException("GetTransactionByHash error");
//        }
//        Map<String, Object> objectMap = JacksonUtil.json2map(result.getData());
//        if (objectMap == null) {
//            log.error("[D4AClaimNftMinterRewardChainService] objectMap is null result:{}", result.getData());
//            throw new RuntimeException("GetTransactionByHash objectMap is null");
//        }
//        String fromAddress = (String) objectMap.get("from");
//        fromAddress = CommonUtil.addHexPrefixIfNotExist(fromAddress.toLowerCase());
//
//        List<UserHarvestToken> userHarvestTokenList = userHarvestTokenService.selectUnclaimedTokenByUserAddress(fromAddress);
//        userHarvestTokenList = userHarvestTokenList.stream().filter(v -> daoIdList.contains(v.getDaoId())).collect(Collectors.toList());
//        BigDecimal unclaimedToken = userHarvestTokenList.stream().map(UserHarvestToken::getUnclaimedToken).reduce(BigDecimal.ZERO, BigDecimal::add);
//        if (tokenNum.compareTo(unclaimedToken) > 0) {
//            log.error("[D4AClaimNftMinterRewardChainService] daoId:{} tokenNum:{} unclaimedToken:{}", dao.getId(), tokenNum, unclaimedToken);
//        }
//        //实际只有一个UserHarvestToken，每个dao单独抛出事件
//        for (UserHarvestToken userHarvestToken : userHarvestTokenList) {
//            BigDecimal receivedToken =
//                    userHarvestToken.getReceivedToken() == null ? BigDecimal.ZERO : userHarvestToken.getReceivedToken();
//            userHarvestToken.setReceivedToken(receivedToken.add(tokenNum));
//            userHarvestToken.setUnclaimedToken(BigDecimal.ZERO);
//            userHarvestToken.setLastTransactionHash(transactionDto.getTransactionHash());
//        }
//
//        TokenReceivedRecord tokenReceivedRecord = new TokenReceivedRecord();
//        tokenReceivedRecord.setTokenNum(tokenNum);
//        tokenReceivedRecord.setTokenNumBalance(tokenNum);
//        tokenReceivedRecord.setReceiveType(TokenReceiveTypeEnum.MINTER.getType());
//        tokenReceivedRecord.setTokenType(TokenTypeEnum.COLLECT.getType());
//        tokenReceivedRecord.setDaoNumber(dao.getDaoNumber());
//        tokenReceivedRecord.setReceiveId(dao.getId());
//
//        tokenReceivedRecord.setProjectId(projectId);
//        tokenReceivedRecord.setCanvasId(null);
//        tokenReceivedRecord.setBlockNumber(transactionDto.getBlockNumber());
//        tokenReceivedRecord.setBlockTime(transactionDto.getBlockTime());
//        tokenReceivedRecord.setTransactionHash(transactionDto.getTransactionHash());
//        tokenReceivedRecord.setDrbNumber(Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
//
//        tokenReceivedRecord.setFromAddress(ProtoDaoConstant.protocolContract);
//        tokenReceivedRecord.setToAddress(fromAddress);
//        tokenReceivedRecord.setReceiveAddress(fromAddress);
//
//        UserLiquidityStatisticsVo userLiquidityStatisticsVo = new UserLiquidityStatisticsVo();
//        userLiquidityStatisticsVo.setErc20Address(dao.getErc20Token());
//        userLiquidityStatisticsVo.setUserAddress(fromAddress);
//        Result<Integer> resultInt = protodaoDexService.syncErc20Balance(userLiquidityStatisticsVo);
//        if (resultInt.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//            log.error("[D4AClaimNftMinterRewardChainService] daoId:{} syncErc20Balance error result:{}", dao.getId(),
//                    resultInt.getResultDesc());
//            throw new RuntimeException("dao sync erc20Balance error");
//        }
//
//        log.info("[D4AClaimNftMinterRewardChainService] daoId:{} tokenReceivedRecord:{} userHarvestTokenList:{}", dao.getId(), JacksonUtil.obj2json(tokenReceivedRecord), JacksonUtil.obj2json(userHarvestTokenList));
//        int i = tokenReceivedRecordService.saveTokenReceivedAndUpdateHarvestTokenList(tokenReceivedRecord, userHarvestTokenList);
//        log.info("[D4AClaimNftMinterRewardChainService] daoId:{} update i:{}", dao.getId(), i);
//
//    }
//
//    public static void main(String[] args) {
//        String data =
//                "b2ec93de13a0b26ae10bcb168f6cc63cdc58d9413e79059e1ff564ab440d833e0000000000000000000000008405fee3e2a3b9a96c794a9237a33a6cbbc03221000000000000000000000000000000000000000000034f086f3b33b684000000";
//        List<String> dataList = CommonUtil.splitBy32Bytes(data);
//
//        String projectId = dataList.get(0);
//        String erc20Token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));
//        String amount = CommonUtil.hexToTenString(dataList.get(2));
//        System.out.println("projectId" + " is " + projectId);
//        System.out.println("erc20Token" + " is " + erc20Token);
//        System.out.println("amount" + " is " + amount);
//    }
//}
