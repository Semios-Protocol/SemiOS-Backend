package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Canvas;
import semios.api.service.ICanvasService;
import semios.api.service.SubscriberChainService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;
import semios.api.utils.ProtoDaoCommonUtil;

import java.util.List;

/**
 * 设置 Canvas 下 Mint NFT 时 ETH 折扣比例
 *
 * @description: 设置 Canvas 下 Mint NFT 时 ETH 折扣比例
 * @author: xiangbin
 * @create: 2023-06-12 13:43
 **/
@Slf4j
@Service
public class CanvasRebateRatioInBpsSetChainService implements SubscriberChainService {

    //https://goerli.etherscan.io/tx/0x19f3f8532445b579391bda85eeb4606a3fd9a41ffa4503cf37d462f36e78f417#eventlog

    @Autowired
    private ICanvasService canvasService;

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[CanvasRebateRatioInBpsSetChainService] transactionDto:{}", JacksonUtil.obj2json(transactionDto));
        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());
        String canvasId = CommonUtil.addHexPrefixIfNotExist(topics.get(1));

        String newCanvasRebateRatioInBps = CommonUtil.hexToTenString(dataList.get(0));
        if (newCanvasRebateRatioInBps == null) {
            throw new RuntimeException("CanvasRebateRatioInBpsSetChainService newCanvasRebateRatioInBps is null");
        }

        Canvas canvas = canvasService.selectCanvasDetailByCanvasId(CommonUtil.removeHexPrefixIfExists(canvasId));
        if (canvas == null) {
            log.error("[CanvasRebateRatioInBpsSetChainService] canvas not find canvasId:{}", canvasId);
            throw new RuntimeException("CanvasRebateRatioInBpsSetChainService cannot find canvas");
        }

        canvas.setRoyaltyToken(ProtoDaoCommonUtil.strToBigDecimal(newCanvasRebateRatioInBps));

        log.info("[CanvasRebateRatioInBpsSetChainService] canvasId:{} newCanvasRebateRatioInBps:{} ", canvas.getId(), newCanvasRebateRatioInBps);

        canvasService.updateById(canvas);
    }


}
