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
import semios.api.model.enums.TokenReceiveTypeEnum;
import semios.api.model.enums.TokenTypeEnum;
import semios.api.model.vo.req.UserLiquidityStatisticsVo;
import semios.api.service.ICanvasService;
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
 * 1.3 监听canvas领取事件
 *
 * @description: 监听canvas领取代币和eth
 * @author: xiangbin
 * @create: 2022-08-25 13:43
 **/
@Slf4j
@Service
public class ClaimCanvasRewardChainService implements SubscriberChainService {

    @Autowired
    private ITokenReceivedRecordService tokenReceivedRecordService;

    @Autowired
    private IDaoService daoService;

    @Autowired
    private ICanvasService canvasService;

    @Autowired(required = false)
    private IProtoDaoDexService protoDaoDexService;

    public static void main(String[] args) {
        String data =
                "58d2d3c7a28441df4656fab3eb0e51d94a45b38100a7e74dba50afae6124d6d72ae972c0263f681f4f35a5fbffcb8860ddb2ceb32ab4e3a4d8f6912888dca8c8000000000000000000000000ffd23ddffa1d6181c8a711a8b4939eedf9cc00bd0000000000000000000000000000000000000000000000000000000000000000";
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String project_id = dataList.get(0);
        String canvas_id = dataList.get(1);
        String erc20_token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2)));
        String amount = CommonUtil.hexToTenString(dataList.get(3));
        System.out.println("project_id" + " is " + project_id);
        System.out.println("canvas_id" + " is " + canvas_id);
        System.out.println("erc20_token" + " is " + erc20_token);
        System.out.println("amount" + " is " + amount);
    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[PDClaimCanvasRewardChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String project_id = dataList.get(0);
        String canvas_id = dataList.get(1);
        String erc20_token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2)));
        String erc20AmountTotal = CommonUtil.hexToTenString(dataList.get(3));
        String ethAmountTotal = CommonUtil.hexToTenString(dataList.get(4));
        String receiver = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(5)));

        Dao dao = daoService.daoDetailByProjectId(project_id);
        if (dao == null) {
            log.error("[PDClaimCanvasRewardChainService] dao is null transactionDao:{}",
                    JacksonUtil.obj2json(transactionDto));
            throw new RuntimeException("dao is null");
        }
        Canvas canvas = canvasService.selectCanvasDetailByCanvasId(canvas_id);
        if (canvas == null) {
            log.error("[PDClaimCanvasRewardChainService] canvas is null transactionDao:{}",
                    JacksonUtil.obj2json(transactionDto));
            throw new RuntimeException("canvas is null");
        }
        BigDecimal tokenNum =
                new BigDecimal(erc20AmountTotal).divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals()), 18, RoundingMode.FLOOR);
        BigDecimal ethNum =
                new BigDecimal(ethAmountTotal).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()), 18, RoundingMode.FLOOR);
        if (tokenNum.compareTo(BigDecimal.ZERO) == 0 && ethNum.compareTo(BigDecimal.ZERO) == 0) {
            log.info("[PDClaimCanvasRewardChainService] tokenNum & ethNum is zero transactionDao:{}",
                    JacksonUtil.obj2json(transactionDto));
            return;
        }

        TokenReceivedRecord tokenReceivedRecord = new TokenReceivedRecord();
        tokenReceivedRecord.setTokenNum(tokenNum);
        tokenReceivedRecord.setEthNum(ethNum);
        tokenReceivedRecord.setReceiveType(TokenReceiveTypeEnum.CANVAS.getType());
        tokenReceivedRecord.setTokenType(TokenTypeEnum.COLLECT.getType());
        tokenReceivedRecord.setReceiveId(canvas.getId());
        // tokenReceivedRecord.setReceiveAddress(canvas.getOwnerAddress());
        tokenReceivedRecord.setProjectId(project_id);
        tokenReceivedRecord.setCanvasId(canvas_id);
        tokenReceivedRecord.setBlockNumber(transactionDto.getBlockNumber());
        tokenReceivedRecord.setBlockTime(transactionDto.getBlockTime());
        tokenReceivedRecord.setTransactionHash(transactionDto.getTransactionHash());
        tokenReceivedRecord.setDrbNumber(Integer.valueOf(dao.getCurrentRound()));
        tokenReceivedRecord.setDaoNumber(canvas.getDaoNumber());

        tokenReceivedRecord.setFromAddress(ProtoDaoConstant.protocolContract);
        tokenReceivedRecord.setToAddress(canvas.getOwnerAddress());

        // 修改受益人
        tokenReceivedRecord.setReceiveAddress(receiver);

        if (canvas.getReceivedToken() != null && canvas.getReceivedToken().compareTo(BigDecimal.ZERO) > 0) {
            canvas.setReceivedToken(canvas.getReceivedToken().add(tokenNum));
        } else {
            canvas.setReceivedToken(tokenNum);
        }
        // 每次都是全部领取
        canvas.setUnclaimedToken(BigDecimal.ZERO);

        if (canvas.getReceivedEth() != null && canvas.getReceivedEth().compareTo(BigDecimal.ZERO) > 0) {
            canvas.setReceivedEth(canvas.getReceivedEth().add(ethNum));
        } else {
            canvas.setReceivedEth(ethNum);
        }
        // 每次都是全部领取
        canvas.setUnclaimedEth(BigDecimal.ZERO);

        UserLiquidityStatisticsVo userLiquidityStatisticsVo = new UserLiquidityStatisticsVo();
        userLiquidityStatisticsVo.setErc20Address(dao.getErc20Token());
        userLiquidityStatisticsVo.setUserAddress(canvas.getOwnerAddress());
        Result<Integer> result = protoDaoDexService.syncErc20Balance(userLiquidityStatisticsVo);
        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            throw new RuntimeException("canvas sync erc20Balance error");
        }

        tokenReceivedRecordService.saveTokenReceivedAndUpdateCanvas(tokenReceivedRecord, canvas);
    }
}
