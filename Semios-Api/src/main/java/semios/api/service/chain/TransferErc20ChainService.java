package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.chain.TransferErc20Dto;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.*;
import semios.api.model.enums.TokenReceiveTypeEnum;
import semios.api.model.enums.TokenTypeEnum;
import semios.api.model.enums.TrueOrFalseEnum;
import semios.api.model.vo.req.UserLiquidityStatisticsVo;
import semios.api.service.*;
import semios.api.service.common.CommonService;
import semios.api.service.feign.IProtoDaoDexService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 代币转移监听 转移扣减顺序 1transfer 2canvas 3dao
 *
 * @description:
 * @author: xiangbin
 * @create: 2022-08-25 10:22
 **/
@Slf4j
@Service
public class TransferErc20ChainService implements SubscriberChainService {

    @Autowired
    private ICanvasService canvasService;

    @Autowired
    private IDaoService daoService;

    @Autowired(required = false)
    private IProtoDaoDexService protoDaoDexService;

    @Autowired
    private ITokenReceivedRecordService tokenReceivedRecordService;

    @Autowired
    private IUserHarvestTokenService userHarvestTokenService;

    @Autowired
    private IDaoAppendTokenLogService daoAppendTokenLogService;

    @Autowired
    private CommonService commonService;

    public static void main(String[] args) throws Exception {
        // String data =
        // "{\"address\":\"0x958774e80b35c6273b3cd8c5c455586cd04627d1\",\"blockHash\":\"0x3fbaf8ebacfc1702dd9d9e6290a9b430129858f9f698bd2cc2ff508d9f9d155e\",\"blockNumber\":\"0x7b5685\",\"blockIntNum\":8083077,\"data\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"logIndex\":\"0x17\",\"removed\":\"false\",\"topics\":\"[\\\"0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef\\\",\\\"0x0000000000000000000000000000000000000000000000000000000000000000\\\",\\\"0x000000000000000000000000d11a8d3a72d3ef5da1c531386438c29d1a212664\\\"]\",\"transactionHash\":\"0x1d6eb6abf3c0df8e95275523fb71208912eb4dd85fb3c8b4fc429883ff087c9d\",\"transactionIndex\":\"0xb\",\"subId\":30,\"blockTime\":\"1670297640\",\"contractAddress\":\"0x958774e80b35c6273b3cd8c5c455586cd04627d1\"}";
        // TransactionDto transactionDto = JacksonUtil.json2pojo(data, TransactionDto.class);
        // List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());
        //
        // String fromAddress =
        // CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(0)).toLowerCase());
        // String toAdress =
        // CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(1)).toLowerCase());
        // String amount = CommonUtil.hexToTenString(topics.get(2));
        // System.out.println(fromAddress);
        // System.out.println(toAdress);
        // System.out.println(amount);

        Map<String, Integer> fromMap = new HashMap<>();
        Map<String, Integer> toMap = new HashMap<>();
        fromMap.put("1", 1);
        fromMap.put("2", 2);
        fromMap.put("3", 3);
        toMap.put("1", 1);
        toMap.put("3", 3);
        Iterator<String> it = fromMap.keySet().iterator();
        while (it.hasNext()) {
            String address = it.next();
            Integer toDecimal = toMap.get(address);
            Integer fromDecimal = fromMap.get(address);
            if (fromDecimal.equals(toDecimal)) {
                it.remove();
                toMap.remove(address);
            }
        }
        System.out.println(JacksonUtil.obj2json(fromMap));
        System.out.println(JacksonUtil.obj2json(toMap));
    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[TransferErc20ChainService] transactionDto:{}", JacksonUtil.obj2json(transactionDto));

        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());

        String fromAddress =
                CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(1)).toLowerCase());
        String toAdress =
                CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(2)).toLowerCase());
        String amount = CommonUtil.hexToTenString(transactionDto.getData());

        // 根据erc20获取decimal transactionDto.getContractAddress()为dao erc20的值
        String erc20Decimal = commonService.erc20Decimals(transactionDto.getContractAddress());
        BigDecimal tokenNum =
                new BigDecimal(amount).divide(CommonUtil.getPowBigDecimal(Integer.valueOf(erc20Decimal)), 18, RoundingMode.FLOOR);
        BigDecimal tokenNumBalance = tokenNum;

        List<Dao> daoList = daoService.selectDaoListByErc20TokenNoPage(transactionDto.getContractAddress());

        if (daoList == null || daoList.size() == 0) {
            log.error("[TransferErc20ChainService] dao selectDaoByErc20Token is null transactionDto:{}",
                    JacksonUtil.obj2json(transactionDto));
            throw new RuntimeException("dao is null!");
        }

        List<Integer> daoNumberList = daoList.stream().map(Dao::getDaoNumber).collect(Collectors.toList());
        List<Integer> daoIdList = daoList.stream().map(Dao::getId).collect(Collectors.toList());

        UserLiquidityStatisticsVo userLiquidityStatisticsVo = new UserLiquidityStatisticsVo();
        userLiquidityStatisticsVo.setErc20Address(daoList.get(0).getErc20Token());
        if (!toAdress.equals(ProtoDaoConstant.ZERO_ADDRESS)) {
            userLiquidityStatisticsVo.setUserAddress(toAdress);
            Result<Integer> result = protoDaoDexService.syncErc20Balance(userLiquidityStatisticsVo);
            if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[TransferErc20ChainService]  sync erc20Address:{} toAddress:{}", daoList.get(0).getErc20Token(),
                        toAdress);
            }
        }
        if (!fromAddress.equals(ProtoDaoConstant.ZERO_ADDRESS)) {
            userLiquidityStatisticsVo.setUserAddress(fromAddress);
            Result<Integer> result = protoDaoDexService.syncErc20Balance(userLiquidityStatisticsVo);
            if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[TransferErc20ChainService]  sync erc20Address:{} fromAddress:{}", daoList.get(0).getErc20Token(),
                        fromAddress);
            }
        }
        if (fromAddress.equals(ProtoDaoConstant.ZERO_ADDRESS)
                || fromAddress.equalsIgnoreCase(ProtoDaoConstant.protocolContract)
                || fromAddress.equalsIgnoreCase(ProtoDaoConstant.protocolContract)
                || toAdress.equals(ProtoDaoConstant.ZERO_ADDRESS)
                || toAdress.equalsIgnoreCase(ProtoDaoConstant.protocolContract)
                || toAdress.equalsIgnoreCase(ProtoDaoConstant.protocolContract)) {
            log.info("[TransferErc20ChainService] from or to is zero address");
            // f2d 平台所有者地址
            if ((fromAddress.equalsIgnoreCase(ProtoDaoConstant.protocolContract) || fromAddress.equalsIgnoreCase(ProtoDaoConstant.protocolContract))
                    && toAdress.equalsIgnoreCase(ProtoDaoConstant.protocol_fee_pool)) {
                log.info("[TransferErc20ChainService] fromAddress:{}", fromAddress);
                log.info("[TransferErc20ChainService] toAddress:{}", toAdress);
                // main net fork -protocol_fee_pool = 0xc8fc1a79f17f11c1cdcb261b759d9886b5fb868d
                // 不返回，需要记录一下
            } else {
                return;
            }
        }

        //1.3 如果to地址是feePool,且当前dao为三方erc20,则这个事件为三方erc20注入事件,
        //如果from地址是当前feePool地址,则可能是当前dao分配erc20给其他dao分配,不用监听处理.
        //其余,不涉及feePool的为token的转移.需要后续处理
        Optional<Dao> optionalDao = daoList.stream().filter(v -> CommonUtil.addHexPrefixIfNotExist(v.getFeePool()).equalsIgnoreCase(toAdress)).findFirst();
        if (optionalDao.isPresent()) {
            if (TrueOrFalseEnum.TRUE.getStatus().equals(optionalDao.get().getIsThirdpartyToken())) {
                DaoAppendTokenLog daoAppendToken = daoAppendTokenLogService.selectByTransactionHash(transactionDto.getTransactionHash());
                if (daoAppendToken != null) {
                    log.info("[TransferErc20ChainService] append token erc20Address:{} transactionHash:{} had log", daoList.get(0).getErc20Token(), transactionDto.getTransactionHash());
                    return;
                }
                Dao appendDao = optionalDao.get();
                //如果to地址是feePool,且当前dao为三方erc20,则这个事件为三方erc20注入事件
                DaoAppendTokenLog daoAppendTokenLog = new DaoAppendTokenLog();
                daoAppendTokenLog.setDaoId(appendDao.getId());
                daoAppendTokenLog.setProjectId(appendDao.getProjectId());
                BigDecimal totalErc20 = StringUtils.isBlank(appendDao.getErc20TotalSupply()) ? BigDecimal.ZERO : new BigDecimal(appendDao.getErc20TotalSupply());
                daoAppendTokenLog.setOriginTotalSupply(totalErc20);
                daoAppendTokenLog.setNewTotalSupply(totalErc20.add(tokenNum));
                daoAppendTokenLog.setUserAddress(fromAddress);
                daoAppendTokenLog.setIsThirdparty(TrueOrFalseEnum.TRUE.getStatus());
                daoAppendTokenLog.setTransactionHash(transactionDto.getTransactionHash());
                daoAppendTokenLog.setBlockTime(transactionDto.getBlockTime());

                appendDao.setErc20TotalSupply(daoAppendTokenLog.getNewTotalSupply().toPlainString());

                int i = daoAppendTokenLogService.insertDaoAppendTokenLogAndUpdateDao(daoAppendTokenLog, appendDao);
                log.info("[TransferErc20ChainService]  append token erc20Address:{} fromAddress:{} newToken:{}", daoList.get(0).getErc20Token(), fromAddress, appendDao.getErc20TotalSupply());
                return;

            } else {
                log.info("[TransferErc20ChainService]  append token not thirdparty erc20Address:{} tokenNum:{}", daoList.get(0).getErc20Token(), tokenNum);
                return;
            }
        }
        Optional<Dao> fromDao = daoList.stream().filter(v -> CommonUtil.addHexPrefixIfNotExist(v.getFeePool()).equalsIgnoreCase(fromAddress)).findFirst();
        if (fromDao.isPresent()) {
            log.info("[TransferErc20ChainService]  from dao to address erc20Address:{} tokenNum:{} toAddress:{}", daoList.get(0).getErc20Token(), tokenNum, toAdress);
            return;
        }

        List<TokenReceivedRecord> tokenReceivedRecordListArray = new ArrayList<>();
        List<Canvas> canvasListArray = new ArrayList<>();
        List<Dao> daoListArray = new ArrayList<>();
        List<UserHarvestToken> userHarvestTokenArray = new ArrayList<>();

        // first TokenReceivedRecord

        List<TokenReceivedRecord> tokenReceivedRecordList = tokenReceivedRecordService.recordList(fromAddress);
        tokenReceivedRecordList =
                tokenReceivedRecordList.stream().filter(v -> daoNumberList.contains(v.getDaoNumber()))
                        .filter(v -> v.getTokenType().equals(TokenTypeEnum.TRANSFER.getType()))
                        .filter(
                                v -> fromAddress.equals(v.getToAddress()) && v.getTokenNumBalance().compareTo(BigDecimal.ZERO) > 0)
                        .collect(Collectors.toList());
        log.info("tokenNumBalance的值为:{}, tokenReceivedRecordList的size值为:{}", tokenNumBalance, tokenReceivedRecordList.size());
        if (tokenNumBalance.compareTo(BigDecimal.ZERO) > 0 && tokenReceivedRecordList.size() > 0) {
            for (TokenReceivedRecord receivedRecord : tokenReceivedRecordList) {
                log.info("receivedRecord id:{},receivedRecord TokenNumBalance:{}", receivedRecord.getId(), receivedRecord.getTokenNumBalance());
                TokenReceivedRecord tokenReceivedRecord1 = new TokenReceivedRecord();

                tokenReceivedRecord1.setBlockNumber(transactionDto.getBlockNumber());
                tokenReceivedRecord1.setBlockTime(transactionDto.getBlockTime());
                tokenReceivedRecord1.setTransactionHash(transactionDto.getTransactionHash());
                tokenReceivedRecord1.setDrbNumber(Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
                tokenReceivedRecord1.setFromAddress(fromAddress);
                tokenReceivedRecord1.setToAddress(toAdress);
                tokenReceivedRecord1.setTokenType(TokenTypeEnum.TRANSFER.getType());
                //transfer扣除
                tokenReceivedRecord1.setReceiveType(TokenReceiveTypeEnum.TRANSFER.getType());

                tokenReceivedRecord1.setDaoNumber(daoList.get(0).getDaoNumber());
                tokenReceivedRecord1.setReceiveId(daoList.get(0).getId());
                tokenReceivedRecord1.setReceiveAddress(toAdress);
                tokenReceivedRecord1.setProjectId(daoList.get(0).getProjectId());

                if (receivedRecord.getTokenNumBalance().compareTo(tokenNumBalance) >= 0) {
                    receivedRecord.setTokenNumBalance(receivedRecord.getTokenNumBalance().subtract(tokenNumBalance));

                    tokenReceivedRecord1.setTokenNum(tokenNumBalance);
                    tokenReceivedRecord1.setTokenNumBalance(tokenNumBalance);

                    tokenReceivedRecordListArray.add(receivedRecord);
                    tokenReceivedRecordListArray.add(tokenReceivedRecord1);

                    tokenNumBalance = BigDecimal.ZERO;
                    break;
                } else {
                    tokenReceivedRecord1.setTokenNum(receivedRecord.getTokenNumBalance());
                    tokenReceivedRecord1.setTokenNumBalance(receivedRecord.getTokenNumBalance());

                    tokenNumBalance = tokenNumBalance.subtract(receivedRecord.getTokenNumBalance());

                    receivedRecord.setTokenNumBalance(BigDecimal.ZERO);

                    tokenReceivedRecordListArray.add(receivedRecord);
                    tokenReceivedRecordListArray.add(tokenReceivedRecord1);
                }
            }
        }

        // second nft minter received
        List<UserHarvestToken> userHarvestTokenList =
                userHarvestTokenService.selectReceivedTokenByUserAddress(fromAddress);
        userHarvestTokenList = userHarvestTokenList.stream().filter(v -> daoIdList.contains(v.getDaoId())).collect(Collectors.toList());
        log.info("tokenNumBalance的值为:{}, userHarvestTokenList的size值为:{}", tokenNumBalance, userHarvestTokenList.size());
        for (UserHarvestToken userHarvestToken : userHarvestTokenList) {
            log.info("userHarvestToken id:{},userHarvestToken ReceivedToken:{}", userHarvestToken.getId(), userHarvestToken.getReceivedToken());
            if (tokenNumBalance.compareTo(BigDecimal.ZERO) > 0 && userHarvestToken != null
                    && userHarvestToken.getReceivedToken() != null
                    && userHarvestToken.getReceivedToken().compareTo(BigDecimal.ZERO) > 0) {

                TokenReceivedRecord tokenReceivedRecord1 = new TokenReceivedRecord();

                tokenReceivedRecord1.setBlockNumber(transactionDto.getBlockNumber());
                tokenReceivedRecord1.setBlockTime(transactionDto.getBlockTime());
                tokenReceivedRecord1.setTransactionHash(transactionDto.getTransactionHash());
                tokenReceivedRecord1.setDrbNumber(Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
                tokenReceivedRecord1.setFromAddress(fromAddress);
                tokenReceivedRecord1.setToAddress(toAdress);
                tokenReceivedRecord1.setTokenType(TokenTypeEnum.TRANSFER.getType());
                //minter扣除
                tokenReceivedRecord1.setReceiveType(TokenReceiveTypeEnum.MINTER.getType());

                tokenReceivedRecord1.setDaoNumber(daoList.get(0).getDaoNumber());
                tokenReceivedRecord1.setReceiveId(userHarvestToken.getId());
                tokenReceivedRecord1.setReceiveAddress(toAdress);
                tokenReceivedRecord1.setProjectId(daoList.get(0).getProjectId());
                tokenReceivedRecord1.setCanvasId(null);

                if (userHarvestToken.getReceivedToken().compareTo(tokenNumBalance) >= 0) {
                    userHarvestToken.setReceivedToken(userHarvestToken.getReceivedToken().subtract(tokenNumBalance));
                    userHarvestToken.setTransferToken(userHarvestToken.getTransferToken() == null ? tokenNumBalance
                            : userHarvestToken.getTransferToken().add(tokenNumBalance));
                    userHarvestToken.setLastTransactionHash(transactionDto.getTransactionHash());
                    tokenReceivedRecord1.setTokenNum(tokenNumBalance);
                    tokenReceivedRecord1.setTokenNumBalance(tokenNumBalance);

                    userHarvestTokenArray.add(userHarvestToken);
                    tokenReceivedRecordListArray.add(tokenReceivedRecord1);

                    tokenNumBalance = BigDecimal.ZERO;
                } else {
                    tokenReceivedRecord1.setTokenNum(userHarvestToken.getReceivedToken());
                    tokenReceivedRecord1.setTokenNumBalance(userHarvestToken.getReceivedToken());
                    userHarvestToken
                            .setTransferToken(userHarvestToken.getTransferToken() == null ? userHarvestToken.getReceivedToken()
                                    : userHarvestToken.getTransferToken().add(userHarvestToken.getReceivedToken()));
                    tokenNumBalance = tokenNumBalance.subtract(userHarvestToken.getReceivedToken());
                    userHarvestToken.setLastTransactionHash(transactionDto.getTransactionHash());
                    userHarvestToken.setReceivedToken(BigDecimal.ZERO);

                    userHarvestTokenArray.add(userHarvestToken);
                    tokenReceivedRecordListArray.add(tokenReceivedRecord1);
                }

            }
        }

        // third canvas
        List<Canvas> canvasList = canvasService.myCanvasForAll(fromAddress);
        canvasList = canvasList.stream()
                .filter(v -> daoIdList.contains(v.getDaoId()) && v.getReceivedToken().compareTo(BigDecimal.ZERO) > 0 && v.getOwnerAddress().equalsIgnoreCase(fromAddress))
                .collect(Collectors.toList());
        log.info("tokenNumBalance的值为:{}, canvasList的size值为:{}", tokenNumBalance, canvasList.size());
        if (tokenNumBalance.compareTo(BigDecimal.ZERO) > 0 && canvasList.size() > 0) {
            for (Canvas canvas : canvasList) {
                log.info("canvas id:{},canvas ReceivedToken:{}", canvas.getId(), canvas.getReceivedToken());
                TokenReceivedRecord tokenReceivedRecord1 = new TokenReceivedRecord();

                tokenReceivedRecord1.setBlockNumber(transactionDto.getBlockNumber());
                tokenReceivedRecord1.setBlockTime(transactionDto.getBlockTime());
                tokenReceivedRecord1.setTransactionHash(transactionDto.getTransactionHash());
                tokenReceivedRecord1.setDrbNumber(Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
                tokenReceivedRecord1.setFromAddress(fromAddress);
                tokenReceivedRecord1.setToAddress(toAdress);
                tokenReceivedRecord1.setTokenType(TokenTypeEnum.TRANSFER.getType());
                //canvas扣除
                tokenReceivedRecord1.setReceiveType(TokenReceiveTypeEnum.CANVAS.getType());

                tokenReceivedRecord1.setDaoNumber(daoList.get(0).getDaoNumber());
                tokenReceivedRecord1.setReceiveId(canvas.getId());
                tokenReceivedRecord1.setReceiveAddress(toAdress);
                tokenReceivedRecord1.setProjectId(daoList.get(0).getProjectId());
                tokenReceivedRecord1.setCanvasId(canvas.getCanvasId());

                if (canvas.getReceivedToken().compareTo(tokenNumBalance) >= 0) {
                    canvas.setReceivedToken(canvas.getReceivedToken().subtract(tokenNumBalance));
                    canvas.setTransferToken(canvas.getTransferToken() == null ? tokenNumBalance
                            : canvas.getTransferToken().add(tokenNumBalance));
                    tokenReceivedRecord1.setTokenNum(tokenNumBalance);
                    tokenReceivedRecord1.setTokenNumBalance(tokenNumBalance);

                    canvasListArray.add(canvas);
                    tokenReceivedRecordListArray.add(tokenReceivedRecord1);

                    tokenNumBalance = BigDecimal.ZERO;
                    break;
                } else {
                    tokenReceivedRecord1.setTokenNum(canvas.getReceivedToken());
                    tokenReceivedRecord1.setTokenNumBalance(canvas.getReceivedToken());
                    canvas.setTransferToken(canvas.getTransferToken() == null ? canvas.getReceivedToken()
                            : canvas.getTransferToken().add(canvas.getReceivedToken()));
                    tokenNumBalance = tokenNumBalance.subtract(canvas.getReceivedToken());

                    canvas.setReceivedToken(BigDecimal.ZERO);

                    canvasListArray.add(canvas);
                    tokenReceivedRecordListArray.add(tokenReceivedRecord1);
                }
            }
        }

        // fourth dao
        daoList = daoList.stream().filter(v -> fromAddress.equalsIgnoreCase(v.getOwnerAddress())).collect(Collectors.toList());
        log.info("tokenNumBalance的值为:{}, daoList的size值为:{}", tokenNumBalance, daoList.size());
        for (Dao dao : daoList) {
            log.info("dao id:{},dao ReceivedToken:{}", dao.getId(), dao.getReceivedToken());
            if (tokenNumBalance.compareTo(BigDecimal.ZERO) > 0) {
                TokenReceivedRecord tokenReceivedRecord = new TokenReceivedRecord();
                tokenReceivedRecord.setTokenNum(tokenNum);
                tokenReceivedRecord.setBlockNumber(transactionDto.getBlockNumber());
                tokenReceivedRecord.setBlockTime(transactionDto.getBlockTime());
                tokenReceivedRecord.setTransactionHash(transactionDto.getTransactionHash());
                tokenReceivedRecord.setDrbNumber(Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
                tokenReceivedRecord.setFromAddress(fromAddress);
                tokenReceivedRecord.setToAddress(toAdress);
                tokenReceivedRecord.setTokenType(TokenTypeEnum.TRANSFER.getType());
                //dao扣除
                tokenReceivedRecord.setReceiveType(TokenReceiveTypeEnum.DAO.getType());

                tokenReceivedRecord.setDaoNumber(dao.getDaoNumber());
                tokenReceivedRecord.setReceiveId(dao.getId());
                tokenReceivedRecord.setReceiveAddress(toAdress);
                tokenReceivedRecord.setProjectId(dao.getProjectId());


                BigDecimal receivedToken = dao.getReceivedToken() == null ? BigDecimal.ZERO : dao.getReceivedToken();
                if (!((fromAddress.equalsIgnoreCase(ProtoDaoConstant.protocolContract) || fromAddress.equalsIgnoreCase(ProtoDaoConstant.protocolContract))
                        && toAdress.equalsIgnoreCase(ProtoDaoConstant.protocol_fee_pool))) {
                    if (receivedToken.compareTo(tokenNumBalance) >= 0) {
                        tokenReceivedRecord.setTokenNum(tokenNumBalance);
                        tokenReceivedRecord.setTokenNumBalance(tokenNumBalance);
                        tokenReceivedRecordListArray.add(tokenReceivedRecord);

                        dao.setReceivedToken(receivedToken.subtract(tokenNumBalance));
                        dao.setTransferToken(
                                dao.getTransferToken() == null ? tokenNumBalance : dao.getTransferToken().add(tokenNumBalance));
                        daoListArray.add(dao);

                        tokenNumBalance = BigDecimal.ZERO;
                        break;
                    } else {
                        tokenReceivedRecord.setTokenNum(dao.getReceivedToken());
                        tokenReceivedRecord.setTokenNumBalance(dao.getReceivedToken());

                        dao.setTransferToken(dao.getTransferToken() == null ? dao.getReceivedToken()
                                : dao.getTransferToken().add(dao.getReceivedToken()));

                        tokenNumBalance = tokenNumBalance.subtract(dao.getReceivedToken());

                        dao.setReceivedToken(BigDecimal.ZERO);

                        daoListArray.add(dao);
                        tokenReceivedRecordListArray.add(tokenReceivedRecord);
                    }
                }
            }
        }
        //由于之间保留的小数位数只有两位，所以这里小于1的不做处理
        if (tokenNumBalance.compareTo(BigDecimal.ONE) > 0) {
            if ((fromAddress.equalsIgnoreCase(ProtoDaoConstant.protocolContract) || fromAddress.equalsIgnoreCase(ProtoDaoConstant.protocolContract))
                    && toAdress.equalsIgnoreCase(ProtoDaoConstant.protocol_fee_pool)) {
                // 不返回，需要记录一下
            } else {
                // throw exception
                log.error("[TransferErc20ChainService]tokenNumBalance:{} is not zero transactionDto:{}", tokenNumBalance, JacksonUtil.obj2json(transactionDto));
                throw new RuntimeException("dao not have enouth balance");
            }
        }


        log.info("[TransferErc20ChainService] tokenReceivedRecordListArra:{} canvasListArray:{} daoListArray:{} userHarvestTokenArray:{}",
                JacksonUtil.obj2json(tokenReceivedRecordListArray), JacksonUtil.obj2json(canvasListArray),
                JacksonUtil.obj2json(daoListArray), JacksonUtil.obj2json(userHarvestTokenArray));
        int i = tokenReceivedRecordService.saveOrUpdateTokenReceivedForTransfer(tokenReceivedRecordListArray,
                canvasListArray, daoListArray, userHarvestTokenArray);
        log.info(
                "[TransferErc20ChainService] tokenReceived size:{} canvas size:{} dao size:{} userHarvestTokenArray:{} return size:{}",
                tokenReceivedRecordListArray.size(), canvasListArray.size(), daoListArray.size(),
                userHarvestTokenArray.size(), i);

    }

    @Override
    public void handleTradeList(List<TransactionDto> transactionDtoList) throws Exception {
        log.info("[TransferErc20ChainService-handleTradeList] transactionDtoList:{}",
                JacksonUtil.obj2json(transactionDtoList));
        if (transactionDtoList == null || transactionDtoList.size() == 0) {
            log.error("[TransferErc20ChainService-handleTradeList] transactionDtoList is null");
            return;
        }
        if (transactionDtoList.size() == 1) {
            this.handleTrade(transactionDtoList.get(0));
            return;
        }

        // 处理transactionDtoList ，去掉from和to地址对冲掉的
        List<TransactionDto> transactionDtoArray = handleTransactionDto(transactionDtoList);
        for (TransactionDto transactionDto : transactionDtoArray) {
            log.info("[TransferErc20ChainService-handleTradeList] transactionDto:{}",
                    JacksonUtil.obj2json(transactionDto));
            this.handleTrade(transactionDto);
        }
    }

    /**
     * 处理transactionDtoList ，去掉from和to地址对冲掉的 然后把最后两个的合并为一个处理
     *
     * @param transactionDtoList transactionDtoList
     * @return List<TransactionDto>
     */
    private List<TransactionDto> handleTransactionDto(List<TransactionDto> transactionDtoList) {
        List<TransactionDto> transactionDtos = new ArrayList<>();

        List<TransferErc20Dto> transferErc20DtoList =
                transactionDtoList.stream().map(TransferErc20Dto::transfer).collect(Collectors.toList());
        Map<String, BigDecimal> fromMap = transferErc20DtoList.stream().collect(
                Collectors.toMap(TransferErc20Dto::getFromAddress, TransferErc20Dto::getTokenNum, BigDecimal::add));
        Map<String, BigDecimal> toMap = transferErc20DtoList.stream()
                .collect(Collectors.toMap(TransferErc20Dto::getToAddress, TransferErc20Dto::getTokenNum, BigDecimal::add));
        Iterator<String> it = fromMap.keySet().iterator();
        while (it.hasNext()) {
            String address = it.next();
            if (!address.equals(ProtoDaoConstant.ZERO_ADDRESS) && toMap != null) {
                BigDecimal toDecimal = toMap.get(address);
                BigDecimal fromDecimal = fromMap.get(address);
                if (fromDecimal.equals(toDecimal)) {
                    it.remove();
                    toMap.remove(address);
                }
            }
        }
        // for (String address : fromMap.keySet()) {
        // // toDecimal可能为空
        // BigDecimal toDecimal = toMap.get(address);
        // BigDecimal fromDecimal = fromMap.get(address);
        // if (fromDecimal.equals(toDecimal)) {
        // fromMap.remove(address);
        // toMap.remove(address);
        // }
        // }
        if (fromMap.keySet().size() == 1 && toMap.keySet().size() == 1) {
            String fromAddress = new ArrayList<>(fromMap.keySet()).get(0);
            Optional<TransferErc20Dto> fromOptional =
                    transferErc20DtoList.stream().filter(v -> fromAddress.equalsIgnoreCase(v.getFromAddress())).findFirst();
            String toAddress = new ArrayList<>(toMap.keySet()).get(0);
            Optional<TransferErc20Dto> toOptional =
                    transferErc20DtoList.stream().filter(v -> toAddress.equalsIgnoreCase(v.getToAddress())).findFirst();
            if (fromOptional.isPresent() && toOptional.isPresent()) {
                TransferErc20Dto fromTransferErc20Dto = fromOptional.get();
                TransferErc20Dto toTransferErc20Dto = toOptional.get();
                if (fromTransferErc20Dto.getTokenNum().equals(toTransferErc20Dto.getTokenNum())) {
                    TransactionDto fromTransactionDto = fromTransferErc20Dto.getTransactionDto();
                    TransactionDto toTransactionDto = toTransferErc20Dto.getTransactionDto();
                    List<String> fromTopics = JacksonUtil.json2StringList(fromTransactionDto.getTopics());
                    List<String> toTopics = JacksonUtil.json2StringList(toTransactionDto.getTopics());
                    String toAdd = toTopics.get(2);
                    fromTopics.set(2, toAdd);
                    fromTransactionDto.setTopics(JacksonUtil.obj2json(fromTopics));
                    transactionDtos.add(fromTransactionDto);
                    return transactionDtos;
                }
            }
        }

        // 处理不了的都返回原来的list
        return transactionDtoList;

    }
}
