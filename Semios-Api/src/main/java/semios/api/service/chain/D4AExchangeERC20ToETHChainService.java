package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.Dao;
import semios.api.model.entity.TokenReceivedRecord;
import semios.api.model.entity.UserHarvestToken;
import semios.api.model.enums.TokenReceiveTypeEnum;
import semios.api.model.enums.TokenTypeEnum;
import semios.api.service.*;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 监听erc20兑换eth 跟进transactionHash查询交易信息 这里的erc20总量可能是dao的，可能是canvas的，也可能是别人转给他的。 兑换扣减顺序 1transfer 2canvas 3dao
 *
 * @description:
 * @author: xiangbin
 * @create: 2022-08-25 11:40
 **/
@Slf4j
@Service
public class D4AExchangeERC20ToETHChainService implements SubscriberChainService {

    @Autowired
    private ITokenReceivedRecordService tokenReceivedRecordService;

    @Autowired
    private IDaoService daoService;

    @Autowired
    private ICanvasService canvasService;

    @Autowired(required = false)
    private ISubscriptionService subscriptionService;

    @Autowired
    private IUserHarvestTokenService userHarvestTokenService;

    public static void main(String[] args) {
        // String data =
        // "6264311abbbd7401bff80e76f8017e5f9a83ccefdb6cced27dbfa9296ce011af000000000000000000000000f8baf7268f3daefe4135f7711473ae8b6c3b47d8000000000000000000000000f8baf7268f3daefe4135f7711473ae8b6c3b47d800000000000000000000000000000000000000000008bb39d0318f936a8e38e3000000000000000000000000000000000000000000000000003b105973640fff";
        // List<String> dataList = CommonUtil.splitBy32Bytes(data);
        //
        //
        // String project_id = dataList.get(0);
        // String erc20_token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));
        // String amount = CommonUtil.hexToTenString(dataList.get(2));
        // System.out.println("project_id" + " is " + project_id);
        // System.out.println("erc20_token" + " is " + erc20_token);
        // System.out.println("amount" + " is " + amount);

        // String input =
        // "0xf538965fd0154a827437170383842c48fc2b8d71aea1e65cec54c5e6fa02ed63078994a90000000000000000000000000000000000000000002bbfa8692cd98a15805000000000000000000000000000f8baf7268f3daefe4135f7711473ae8b6c3b47d8";
        // input = input.substring(10);
        //// input = input.replace("0x592a7d3e", "0x");
        //// input = input.replace("0x524f7178", "0x");
        //// input = input.replace("0xf538965f", "0x");
        // List<String> dataList1 = CommonUtil.splitBy32Bytes(input);
        // System.out.println(dataList1.size());

        // String data =
        // "0xd0154a827437170383842c48fc2b8d71aea1e65cec54c5e6fa02ed63078994a9000000000000000000000000f8baf7268f3daefe4135f7711473ae8b6c3b47d8000000000000000000000000f8baf7268f3daefe4135f7711473ae8b6c3b47d80000000000000000000000000000000000000000002bbfa8692cd98a1580500000000000000000000000000000000000000000000000000000927e2984ea16bb";
        // List<String> dataList = CommonUtil.splitBy32Bytes(data);
        // String projectId = CommonUtil.addHexPrefixIfNotExist(dataList.get(0));
        // String ownerAddress =
        // CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1))).toLowerCase();
        // String toAddress =
        // CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2))).toLowerCase();
        // String erc20_amount = CommonUtil.hexToTenString(dataList.get(3));
        // String eth_amount = CommonUtil.hexToTenString(dataList.get(4));
        // System.out.println(projectId);

        // String input =
        // "0xf538965f365c870f18c3cc1eb3fd9492bff0ea67e760d8b1734034099ed0b6a921286c1300000000000000000000000000000000000000000018d0bf423c03d8de0000000000000000000000000000001431e5ebc057136253db692300cc21775578063d";
        // input = input.substring(10);
        //// input = input.replace("0x592a7d3e", "0x");
        //// input = input.replace("0x524f7178", "0x");
        //// input = input.replace("0xf538965f", "0x");
        // List<String> dataList1 = CommonUtil.splitBy32Bytes(input);
        // List<String> canvasIds = new ArrayList<>();
        // List<String> projectIds = new ArrayList<>();
        //
        // if(dataList1.size() > 3){
        // String a = CommonUtil.hexToTenString(dataList1.get(2));
        //
        // int x = 2;
        // for (int i = 0; i < Integer.parseInt(a); i++) {
        // x = x + 1;
        // canvasIds.add(dataList1.get(x));
        // }
        // x = x+1;
        // String b = CommonUtil.hexToTenString(dataList1.get(x));
        // for (int i = 0; i < Integer.parseInt(b); i++) {
        // x = x+1;
        // projectIds.add(dataList1.get(x));
        // }
        //
        // if(canvasIds.size() == 0 && projectIds.size() == 0){
        // return;
        // }
        // System.out.println(canvasIds.size() + projectIds.size());
        // }
        // else {
        // System.out.println("1234567");
        // }

        String data = CommonUtil.removeHexPrefixIfExists(
                "0x6264311abbbd7401bff80e76f8017e5f9a83ccefdb6cced27dbfa9296ce011af000000000000000000000000f8baf7268f3daefe4135f7711473ae8b6c3b47d8000000000000000000000000f8baf7268f3daefe4135f7711473ae8b6c3b47d80000000000000000000000000000000000000000000046960944eef9e05555550000000000000000000000000000000000000000000000000001dd7c1681d000");
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String projectId = CommonUtil.addHexPrefixIfNotExist(dataList.get(0));
        String ownerAddress =
                CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1))).toLowerCase();
        String toAddress =
                CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2))).toLowerCase();
        String erc20_amount = CommonUtil.hexToTenString(dataList.get(3));
        String eth_amount = CommonUtil.hexToTenString(dataList.get(4));

        BigDecimal erc20Aamount = new BigDecimal(erc20_amount).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO));
        BigDecimal ethAmount = new BigDecimal(eth_amount).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO));

        System.out.println("projectId" + projectId);
        System.out.println("ownerAddress" + ownerAddress);
        System.out.println("toAddress" + toAddress);
        System.out.println("erc20_amount" + erc20_amount);
        System.out.println("eth_amount" + eth_amount);
        System.out.println("erc20Aamount" + erc20Aamount);
        System.out.println("ethAmount" + ethAmount);

    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {
        log.info("[D4AExchangeERC20ToETHChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String projectId = CommonUtil.addHexPrefixIfNotExist(dataList.get(0));
        String ownerAddress =
                CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1))).toLowerCase();
        String toAddress =
                CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2))).toLowerCase();
        String erc20_amount = CommonUtil.hexToTenString(dataList.get(3));   // tokenAmount
        String eth_amount = CommonUtil.hexToTenString(dataList.get(4)); // inputAmount


        Dao daoDecimal = daoService.daoDetailByProjectId(projectId);
        if (daoDecimal == null) {
            throw new RuntimeException("[D4AExchangeERC20ToETHChainService] can not find dao:" + projectId);
        }

        BigDecimal erc20Aamount =
                new BigDecimal(erc20_amount).divide(CommonUtil.getPowBigDecimal(daoDecimal.getErc20TokenDecimals()), 18, RoundingMode.FLOOR);
        BigDecimal ethAmount =
                new BigDecimal(eth_amount).divide(CommonUtil.getPowBigDecimal(daoDecimal.getInputTokenDecimals()), 18, RoundingMode.FLOOR);

        BigDecimal erc20AamountBlance = erc20Aamount;
        BigDecimal ethAmountBalance = ethAmount;
        log.info("[D4AExchangeERC20ToETHChainService] erc20Aamount:{},ethAmount:{}", erc20AamountBlance, ethAmountBalance);

        // 获取transactionHash的参数input，从中获取调用方法的参数canvasId和projectId
        Result<String> result = subscriptionService.ethGetTransactionByHash(ProtoDaoConstant.netWork,
                CommonUtil.addHexPrefixIfNotExist(transactionDto.getTransactionHash()));
        log.info("[D4AExchangeERC20ToETHChainService] result:{}", result.getData());
        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[D4AExchangeERC20ToETHChainService] error result:{}", result.getResultDesc());
            return;
        }
        Map<String, Object> objectMap = JacksonUtil.json2map(result.getData());
        if (objectMap == null) {
            log.error("[D4AExchangeERC20ToETHChainService] objectMap is null result:{}", result.getData());
            return;
        }
        String input = (String) objectMap.get("input");
        input = input.substring(10);
        // input = input.replace("0x592a7d3e", "0x");
        // input = input.replace("0x524f7178", "0x");
        // input = input.replace("0xf538965f", "0x");
        List<String> dataList1 = CommonUtil.splitBy32Bytes(input);
        List<String> canvasIds = new ArrayList<>();
        List<String> projectIds = new ArrayList<>();

        if (dataList1.size() > 3) {
            String a = CommonUtil.hexToTenString(dataList1.get(2));

            int x = 2;
            for (int i = 0; i < Integer.parseInt(a); i++) {
                x = x + 1;
                canvasIds.add(dataList1.get(x));
            }
            x = x + 1;
            String b = CommonUtil.hexToTenString(dataList1.get(x));
            for (int i = 0; i < Integer.parseInt(b); i++) {
                x = x + 1;
                projectIds.add(dataList1.get(x));
            }

            if (canvasIds.size() == 0 && projectIds.size() == 0) {
                return;
            }
        } else {
            projectIds.add(CommonUtil.removeHexPrefixIfExists(projectId));
        }

        // 查询canvas，dao ，TokenReceivedRecord
//        List<Canvas> canvasList = canvasService.selectCanvasDetailByCanvasIdList(canvasIds);

        projectIds = projectIds.stream().filter(v -> v.equalsIgnoreCase(CommonUtil.removeHexPrefixIfExists(projectId)))
                .collect(Collectors.toList());
        List<Dao> daoList = daoService.daoDetailByProjectIdList(projectIds);
//        if (dataList1.size() == 3 && daoList.size() == 1) {
//            canvasList = canvasService.listCanvasByDaoId(daoList.get(0).getId() + "");
//            canvasList = canvasList.stream().filter(v -> v.getOwnerAddress().equalsIgnoreCase(ownerAddress))
//                    .collect(Collectors.toList());
//        }

//        canvasList = canvasList.stream()
//                .filter(v -> v.getProjectId().equalsIgnoreCase(CommonUtil.removeHexPrefixIfExists(projectId)))
//                .collect(Collectors.toList());

        daoList = daoService.selectDaoByErc20TokenList(daoList.stream().map(Dao::getErc20Token).collect(Collectors.toList()));
//        daoList = daoList.stream().filter(v -> v.getOwnerAddress().equalsIgnoreCase(ownerAddress)).collect(Collectors.toList());

        List<Integer> daoNumberList = daoList.stream().map(Dao::getDaoNumber).collect(Collectors.toList());
        Map<Integer, Dao> daoIdMap = daoList.stream().collect(Collectors.toMap(Dao::getId, v -> v));

        List<Canvas> canvasList = canvasService.myCanvasForAll(ownerAddress);
        canvasList = canvasList.stream()
                .filter(v -> daoIdMap.get(v.getDaoId()) != null && v.getReceivedToken().compareTo(BigDecimal.ZERO) > 0)
                .collect(Collectors.toList());

        List<TokenReceivedRecord> tokenReceivedRecordList = tokenReceivedRecordService.recordList(ownerAddress);
//        tokenReceivedRecordList = tokenReceivedRecordList.stream()
//                .filter(v -> daoNumberList.contains(v.getDaoNumber()))
//                .filter(v -> (v.getTokenType().equals(TokenTypeEnum.TRANSFER.getType()) || v.getTokenType().equals(TokenTypeEnum.UNLOCK.getType()) ) )
//                .filter(v -> ownerAddress.equals(v.getToAddress()) && v.getTokenNumBalance().compareTo(BigDecimal.ZERO) > 0)
//                .collect(Collectors.toList());

        tokenReceivedRecordList = tokenReceivedRecordList.stream()
                .filter(v -> daoNumberList.contains(v.getDaoNumber()))
                .filter(v -> (v.getTokenType().equals(TokenTypeEnum.TRANSFER.getType()) || v.getTokenType().equals(TokenTypeEnum.UNLOCK.getType())))
                .filter(v -> ownerAddress.equals(v.getToAddress()) && v.getTokenNumBalance().compareTo(BigDecimal.ZERO) > 0)
                .sorted(Comparator.comparing(v -> {
                    if (v.getTokenType().equals(TokenTypeEnum.UNLOCK.getType())) {
                        return 0;
                    } else {
                        return 1;
                    }
                }))
                .collect(Collectors.toList());


        List<UserHarvestToken> userHarvestTokenList =
                userHarvestTokenService.selectReceivedTokenByUserAddress(ownerAddress);
        userHarvestTokenList =
                userHarvestTokenList.stream().filter(v -> daoIdMap.containsKey(v.getDaoId())).collect(Collectors.toList());

        List<TokenReceivedRecord> tokenReceivedRecordListArray = new ArrayList<>();
        List<Canvas> canvasListArray = new ArrayList<>();
        List<Dao> daoListArray = new ArrayList<>();
        List<UserHarvestToken> userHarvestTokenArray = new ArrayList<>();

        // first TokenReceivedRecord
        log.info("erc20AamountBlance的值为:{}, tokenReceivedRecordList的size值为:{}", erc20AamountBlance, tokenReceivedRecordList.size());
        if (erc20AamountBlance.compareTo(BigDecimal.ZERO) > 0 && tokenReceivedRecordList.size() > 0) {
            for (TokenReceivedRecord receivedRecord : tokenReceivedRecordList) {
                log.info("receivedRecord id:{},receivedRecord TokenNumBalance:{}", receivedRecord.getId(), receivedRecord.getTokenNumBalance());
                TokenReceivedRecord tokenReceivedRecord1 = new TokenReceivedRecord();

                tokenReceivedRecord1.setBlockNumber(transactionDto.getBlockNumber());
                tokenReceivedRecord1.setBlockTime(transactionDto.getBlockTime());
                tokenReceivedRecord1.setTransactionHash(transactionDto.getTransactionHash());
                tokenReceivedRecord1.setDrbNumber(Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
                tokenReceivedRecord1.setFromAddress(ownerAddress);
                tokenReceivedRecord1.setToAddress(ProtoDaoConstant.protocolContract);
                tokenReceivedRecord1.setTokenType(TokenTypeEnum.SWAP.getType());
                tokenReceivedRecord1.setReceiveType(receivedRecord.getReceiveType());

                tokenReceivedRecord1.setDaoNumber(receivedRecord.getDaoNumber());
                tokenReceivedRecord1.setReceiveId(receivedRecord.getId());
                tokenReceivedRecord1.setReceiveAddress(toAddress);
                tokenReceivedRecord1.setProjectId(receivedRecord.getProjectId());
                tokenReceivedRecord1.setSyncDex(0);

                if (receivedRecord.getTokenNumBalance().compareTo(erc20AamountBlance) >= 0) {
                    receivedRecord.setTokenNumBalance(receivedRecord.getTokenNumBalance().subtract(erc20AamountBlance));

                    tokenReceivedRecord1.setTokenNum(erc20AamountBlance);
                    tokenReceivedRecord1.setEthAmount(ethAmountBalance);

                    tokenReceivedRecordListArray.add(receivedRecord);
                    tokenReceivedRecordListArray.add(tokenReceivedRecord1);

                    erc20AamountBlance = BigDecimal.ZERO;
                    ethAmountBalance = BigDecimal.ZERO;
                    break;
                } else {
                    tokenReceivedRecord1.setTokenNum(receivedRecord.getTokenNumBalance());
                    BigDecimal ethAmount1 = receivedRecord.getTokenNumBalance()
                            .divide(erc20AamountBlance, 18, RoundingMode.FLOOR).multiply(ethAmountBalance);
                    tokenReceivedRecord1.setEthAmount(ethAmount1);

                    ethAmountBalance = ethAmountBalance.subtract(ethAmount1);
                    erc20AamountBlance = erc20AamountBlance.subtract(receivedRecord.getTokenNumBalance());

                    receivedRecord.setTokenNumBalance(BigDecimal.ZERO);

                    tokenReceivedRecordListArray.add(receivedRecord);
                    tokenReceivedRecordListArray.add(tokenReceivedRecord1);
                }
            }
        }
        // second nft minter claimed
        userHarvestTokenList = userHarvestTokenList.stream()
                .filter(v -> v.getReceivedToken() != null && v.getReceivedToken().compareTo(BigDecimal.ZERO) > 0)
                .collect(Collectors.toList());

        log.info("erc20AamountBlance的值为:{}, userHarvestTokenList的size值为:{}", erc20AamountBlance, userHarvestTokenList.size());
        if (erc20AamountBlance.compareTo(BigDecimal.ZERO) > 0 && userHarvestTokenList.size() > 0) {
            for (UserHarvestToken userHarvestToken : userHarvestTokenList) {
                log.info("userHarvestToken id:{},userHarvestToken ReceivedToken:{}", userHarvestToken.getId(), userHarvestToken.getReceivedToken());
                Dao harvestDao = daoIdMap.get(userHarvestToken.getDaoId());
                TokenReceivedRecord tokenReceivedRecord1 = new TokenReceivedRecord();

                tokenReceivedRecord1.setBlockNumber(transactionDto.getBlockNumber());
                tokenReceivedRecord1.setBlockTime(transactionDto.getBlockTime());
                tokenReceivedRecord1.setTransactionHash(transactionDto.getTransactionHash());
                tokenReceivedRecord1.setDrbNumber(Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
                tokenReceivedRecord1.setFromAddress(ownerAddress);
                tokenReceivedRecord1.setToAddress(ProtoDaoConstant.protocolContract);
                tokenReceivedRecord1.setTokenType(TokenTypeEnum.SWAP.getType());
                tokenReceivedRecord1.setReceiveType(TokenReceiveTypeEnum.MINTER.getType());

                tokenReceivedRecord1.setDaoNumber(harvestDao.getDaoNumber());
                tokenReceivedRecord1.setReceiveId(userHarvestToken.getId());
                tokenReceivedRecord1.setReceiveAddress(toAddress);
                tokenReceivedRecord1.setProjectId(harvestDao.getProjectId());
                tokenReceivedRecord1.setCanvasId(null);
                tokenReceivedRecord1.setSyncDex(0);

                // if (userHarvestToken.getReceivedToken() == null
                // || userHarvestToken.getReceivedToken().compareTo(BigDecimal.ZERO) == 0) {
                // continue;
                // }

                if (userHarvestToken.getReceivedToken().compareTo(erc20AamountBlance) >= 0) {
                    userHarvestToken.setReceivedToken(userHarvestToken.getReceivedToken().subtract(erc20AamountBlance));
                    BigDecimal swapEth =
                            userHarvestToken.getSwapEth() == null ? BigDecimal.ZERO : userHarvestToken.getSwapEth();
                    BigDecimal swapToken =
                            userHarvestToken.getSwapToken() == null ? BigDecimal.ZERO : userHarvestToken.getSwapToken();
                    userHarvestToken.setSwapEth(swapEth.add(ethAmountBalance));
                    userHarvestToken.setSwapToken(swapToken.add(erc20AamountBlance));
                    userHarvestToken.setLastTransactionHash(transactionDto.getTransactionHash());
                    tokenReceivedRecord1.setTokenNum(erc20AamountBlance);
                    tokenReceivedRecord1.setEthAmount(ethAmountBalance);

                    userHarvestTokenArray.add(userHarvestToken);
                    tokenReceivedRecordListArray.add(tokenReceivedRecord1);

                    erc20AamountBlance = BigDecimal.ZERO;
                    ethAmountBalance = BigDecimal.ZERO;
                    break;
                } else {
                    tokenReceivedRecord1.setTokenNum(userHarvestToken.getReceivedToken());
                    BigDecimal ethAmount1 = userHarvestToken.getReceivedToken()
                            .divide(erc20AamountBlance, 18, RoundingMode.FLOOR).multiply(ethAmountBalance);
                    tokenReceivedRecord1.setEthAmount(ethAmount1);

                    erc20AamountBlance = erc20AamountBlance.subtract(userHarvestToken.getReceivedToken());
                    ethAmountBalance = ethAmountBalance.subtract(ethAmount1);

                    BigDecimal swapEth =
                            userHarvestToken.getSwapEth() == null ? BigDecimal.ZERO : userHarvestToken.getSwapEth();
                    BigDecimal swapToken =
                            userHarvestToken.getSwapToken() == null ? BigDecimal.ZERO : userHarvestToken.getSwapToken();
                    userHarvestToken.setSwapToken(swapToken.add(userHarvestToken.getReceivedToken()));
                    userHarvestToken.setSwapEth(swapEth.add(ethAmount1));
                    userHarvestToken.setReceivedToken(BigDecimal.ZERO);
                    userHarvestToken.setLastTransactionHash(transactionDto.getTransactionHash());
                    userHarvestTokenArray.add(userHarvestToken);
                    tokenReceivedRecordListArray.add(tokenReceivedRecord1);
                }
            }
        }

        // third canvas
        log.info("erc20AamountBlance的值为:{}, canvasList的size值为:{}", erc20AamountBlance, canvasList.size());
        if (erc20AamountBlance.compareTo(BigDecimal.ZERO) > 0 && canvasList.size() > 0) {
            for (Canvas canvas : canvasList) {
                log.info("canvas id:{},canvas ReceivedToken:{}", canvas.getId(), canvas.getReceivedToken());
                TokenReceivedRecord tokenReceivedRecord1 = new TokenReceivedRecord();

                tokenReceivedRecord1.setBlockNumber(transactionDto.getBlockNumber());
                tokenReceivedRecord1.setBlockTime(transactionDto.getBlockTime());
                tokenReceivedRecord1.setTransactionHash(transactionDto.getTransactionHash());
                tokenReceivedRecord1.setDrbNumber(Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
                tokenReceivedRecord1.setFromAddress(ownerAddress);
                tokenReceivedRecord1.setToAddress(ProtoDaoConstant.protocolContract);
                tokenReceivedRecord1.setTokenType(TokenTypeEnum.SWAP.getType());
                tokenReceivedRecord1.setReceiveType(TokenReceiveTypeEnum.CANVAS.getType());

                tokenReceivedRecord1.setDaoNumber(canvas.getDaoNumber());
                tokenReceivedRecord1.setReceiveId(canvas.getId());
                tokenReceivedRecord1.setReceiveAddress(toAddress);
                tokenReceivedRecord1.setProjectId(canvas.getProjectId());
                tokenReceivedRecord1.setCanvasId(canvas.getCanvasId());
                tokenReceivedRecord1.setSyncDex(0);

                if (canvas.getReceivedToken() == null || canvas.getReceivedToken().compareTo(BigDecimal.ZERO) == 0) {
                    continue;
                }

                if (canvas.getReceivedToken().compareTo(erc20AamountBlance) >= 0) {
                    canvas.setReceivedToken(canvas.getReceivedToken().subtract(erc20AamountBlance));
                    BigDecimal swapEth = canvas.getSwapEth() == null ? BigDecimal.ZERO : canvas.getSwapEth();
                    BigDecimal swapToken = canvas.getSwapToken() == null ? BigDecimal.ZERO : canvas.getSwapToken();
                    canvas.setSwapEth(swapEth.add(ethAmountBalance));
                    canvas.setSwapToken(swapToken.add(erc20AamountBlance));

                    tokenReceivedRecord1.setTokenNum(erc20AamountBlance);
                    tokenReceivedRecord1.setEthAmount(ethAmountBalance);

                    canvasListArray.add(canvas);
                    tokenReceivedRecordListArray.add(tokenReceivedRecord1);

                    erc20AamountBlance = BigDecimal.ZERO;
                    ethAmountBalance = BigDecimal.ZERO;
                    break;
                } else {
                    tokenReceivedRecord1.setTokenNum(canvas.getReceivedToken());
                    BigDecimal ethAmount1 = canvas.getReceivedToken().divide(erc20AamountBlance, 18, RoundingMode.FLOOR)
                            .multiply(ethAmountBalance);
                    tokenReceivedRecord1.setEthAmount(ethAmount1);

                    erc20AamountBlance = erc20AamountBlance.subtract(canvas.getReceivedToken());
                    ethAmountBalance = ethAmountBalance.subtract(ethAmount1);

                    BigDecimal swapEth = canvas.getSwapEth() == null ? BigDecimal.ZERO : canvas.getSwapEth();
                    BigDecimal swapToken = canvas.getSwapToken() == null ? BigDecimal.ZERO : canvas.getSwapToken();
                    canvas.setSwapToken(swapToken.add(canvas.getReceivedToken()));
                    canvas.setSwapEth(swapEth.add(ethAmount1));
                    canvas.setReceivedToken(BigDecimal.ZERO);

                    canvasListArray.add(canvas);
                    tokenReceivedRecordListArray.add(tokenReceivedRecord1);
                }
            }
        }

        // fourth dao
        log.info("erc20AamountBlance的值为:{}, daoList的size值为:{}", erc20AamountBlance, daoList.size());
        if (erc20AamountBlance.compareTo(BigDecimal.ZERO) > 0 && daoList.size() > 0) {
            daoList = daoList.stream().filter(v -> v.getOwnerAddress().equalsIgnoreCase(ownerAddress))
                    .collect(Collectors.toList());
            for (Dao dao : daoList) {
                log.info("dao id:{},dao ReceivedToken:{}", dao.getId(), dao.getReceivedToken());
                TokenReceivedRecord tokenReceivedRecord1 = new TokenReceivedRecord();
                tokenReceivedRecord1.setBlockNumber(transactionDto.getBlockNumber());
                tokenReceivedRecord1.setBlockTime(transactionDto.getBlockTime());
                tokenReceivedRecord1.setTransactionHash(transactionDto.getTransactionHash());
                tokenReceivedRecord1.setDrbNumber(Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
                tokenReceivedRecord1.setFromAddress(ownerAddress);
                tokenReceivedRecord1.setToAddress(ProtoDaoConstant.protocolContract);
                tokenReceivedRecord1.setTokenType(TokenTypeEnum.SWAP.getType());
                tokenReceivedRecord1.setReceiveType(TokenReceiveTypeEnum.DAO.getType());

                tokenReceivedRecord1.setDaoNumber(dao.getDaoNumber());
                tokenReceivedRecord1.setReceiveId(dao.getId());
                tokenReceivedRecord1.setReceiveAddress(toAddress);
                tokenReceivedRecord1.setProjectId(dao.getProjectId());

                tokenReceivedRecord1.setSyncDex(0);
                BigDecimal receivedToken = dao.getReceivedToken() == null ? BigDecimal.ZERO : dao.getReceivedToken();
                if (receivedToken == null || receivedToken.compareTo(BigDecimal.ZERO) == 0) {
                    continue;
                }

                if (dao.getReceivedToken().compareTo(erc20AamountBlance) >= 0) {
                    dao.setReceivedToken(dao.getReceivedToken().subtract(erc20AamountBlance));
                    BigDecimal swapEth = dao.getSwapEth() == null ? BigDecimal.ZERO : dao.getSwapEth();
                    BigDecimal swapToken = dao.getSwapToken() == null ? BigDecimal.ZERO : dao.getSwapToken();
                    dao.setSwapEth(swapEth.add(ethAmountBalance));
                    dao.setSwapToken(swapToken.add(erc20AamountBlance));

                    tokenReceivedRecord1.setTokenNum(erc20AamountBlance);
                    tokenReceivedRecord1.setEthAmount(ethAmountBalance);

                    daoListArray.add(dao);
                    tokenReceivedRecordListArray.add(tokenReceivedRecord1);

                    erc20AamountBlance = BigDecimal.ZERO;
                    ethAmountBalance = BigDecimal.ZERO;
                    break;
                } else {
                    tokenReceivedRecord1.setTokenNum(dao.getReceivedToken());
                    BigDecimal ethAmount1 = dao.getReceivedToken().divide(erc20AamountBlance, 18, RoundingMode.FLOOR)
                            .multiply(ethAmountBalance);
                    tokenReceivedRecord1.setEthAmount(ethAmount1);

                    erc20AamountBlance = erc20AamountBlance.subtract(dao.getReceivedToken());
                    ethAmountBalance = ethAmountBalance.subtract(ethAmount1);

                    BigDecimal swapEth = dao.getSwapEth() == null ? BigDecimal.ZERO : dao.getSwapEth();
                    BigDecimal swapToken = dao.getSwapToken() == null ? BigDecimal.ZERO : dao.getSwapToken();
                    dao.setSwapToken(swapToken.add(dao.getReceivedToken()));
                    dao.setSwapEth(swapEth.add(ethAmount1));
                    dao.setReceivedToken(BigDecimal.ZERO);

                    daoListArray.add(dao);
                    tokenReceivedRecordListArray.add(tokenReceivedRecord1);
                }
            }
        }

        if (erc20AamountBlance.compareTo(BigDecimal.ONE) > 0) {
            log.error("[D4AExchangeERC20ToETHChainService] erc20AamountBlance:{} greater than zero",
                    erc20AamountBlance);
            throw new RuntimeException("erc20AamountBlance greater than zero");
        }

        int i = tokenReceivedRecordService.saveOrUpdateTokenReceivedForTransfer(tokenReceivedRecordListArray,
                canvasListArray, daoListArray, userHarvestTokenArray);
        log.info(
                "[D4AExchangeERC20ToETHChainService] tokenReceived size:{} canvas size:{} dao size:{} userHarvestTokenArray:{} return size:{}",
                tokenReceivedRecordListArray.size(), canvasListArray.size(), daoListArray.size(),
                userHarvestTokenArray.size(), i);

    }
}
