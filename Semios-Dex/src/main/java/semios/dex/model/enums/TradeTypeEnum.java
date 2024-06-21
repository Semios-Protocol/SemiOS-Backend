package semios.dex.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import semios.dex.model.dto.common.Dao4ArtDexConstant;
import semios.dex.utils.CommonUtil;

@Slf4j
@NoArgsConstructor
@AllArgsConstructor
public enum TradeTypeEnum {
    //    MINT_D4A_FEE_RATIO("mintD4aFeeRatio", "nft铸造时d4a协议收费的比例", true, SubscriberTypeEnum.VALUE, ""),
    D4APairCreated("D4APairCreated", "创建交易对", false, SubscriberTypeEnum.EVENT, "d4aPairCreatedChainServiceImpl"),
    D4ATransfer("D4ATransfer", "交易对的流动", false, SubscriberTypeEnum.EVENT, "d4aTransferChainServiceImpl"),
    D4AMint("D4AMint", "添加流动性", false, SubscriberTypeEnum.EVENT, "d4aMintChainServiceImpl"),
    D4ASync("D4ASync", "最新储备量", false, SubscriberTypeEnum.EVENT, "d4aSyncChainServiceImpl"),
    D4ABurn("D4ABurn", "移除流动性", false, SubscriberTypeEnum.EVENT, "d4aBurnChainServiceImpl"),
    D4ASwap("D4ASwap", "兑换交易", false, SubscriberTypeEnum.EVENT, "d4aSwapChainServiceImpl"),
    EthTransfered("EthTransfered", "二次交易", false, SubscriberTypeEnum.EVENT, "ethTransferedChainServiceImpl"),
    WETH_ADDRESS("wethAddress", "查询router合约weth合约地址", true, SubscriberTypeEnum.VALUE, "");

    @Getter
    private String type;

    @Getter
    private String name;

    /**
     * 仅启动时初始化的变量
     */
    @Getter
    private Boolean local;

    /**
     * 事件订阅还是值订阅
     */
    @Getter
    private SubscriberTypeEnum subscriberTypeEnum;

    @Getter
    private String tradeServiceName;

    public static TradeTypeEnum queryByType(String type) {
        for (TradeTypeEnum tradeTypeEnum : TradeTypeEnum.values()) {
            if (tradeTypeEnum.getType().equals(type)) {
                return tradeTypeEnum;
            }
        }
        return null;
    }

    public static void setDefaultValue(TradeTypeEnum tradeTypeEnum, String value) {

        if (TradeTypeEnum.WETH_ADDRESS.equals(tradeTypeEnum)) {
            //地址类型的处理
            value = CommonUtil.formatBytes32Address(value);
        } else {
            // 十六进制转十进制
            value = CommonUtil.hexToTenString(value);
        }
        if (tradeTypeEnum == null || StringUtils.isBlank(value)) {
            return;
        }
        if (TradeTypeEnum.WETH_ADDRESS.equals(tradeTypeEnum)) {
            log.info("[TradeTypeEnum]WETH_ADDRESS:{}", value);
            Dao4ArtDexConstant.WETH_ADDRESS = CommonUtil.addHexPrefixIfNotExist(value.toLowerCase());
        }
    }

    public static void main(String[] args) {
        String value = "0x000000000000000000000000f4267391072b27d76ed8f2a9655bcf5246013f2d";
        // 十六进制转十进制
        value = CommonUtil.formatBytes32Address(value);
        System.out.println(value);
    }

}
