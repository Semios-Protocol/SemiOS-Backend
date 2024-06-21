package semios.api.model.vo.res.Plan;

import lombok.Data;

@Data
public class CreatePlanParamVo {
    /**
     * dao开始的block
     */
    private String startBlock = "0";

    /**
     * 每个mint window的持续区块数
     */
    private String durationBlock;

    /**
     * plan的uri
     */
    private String planUri;

    /**
     * rewardToken == erc20 Address
     */
    // private String rewardToken;

    /**
     * input token的decimals
     */
    private Integer inputTokenDecimals;

    /**
     * decimals小数位数
     */
    private Integer erc20TokenDecimals;

    /**
     * erc20 address
     */
    private String erc20TokenAddress;

    /**
     * input token address,如果是ETH为空
     */
    private String inputTokenAddress;

    /**
     * 支付类型 ETH,USDC,USDT
     */
    private String payCurrencyType;

    /**
     * dao的owner address
     */
    private String ownerAddress;


    /**
     * 1.8.1 用户自己输入的外部三方erc20地址
     *
     * @mock 1
     */
    private String customTokenAddress;


    /**
     * 1.8.1 用户自己输入的外部三方20地址的decimal
     *
     * @mock 1
     */
    private Integer customTokenDecimal;
}
