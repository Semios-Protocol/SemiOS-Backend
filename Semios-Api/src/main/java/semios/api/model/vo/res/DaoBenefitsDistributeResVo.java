package semios.api.model.vo.res;

import lombok.Data;
import semios.api.model.dto.chain.DaoEthRoyaltyToken;
import semios.api.model.dto.chain.DaoReserveRatio;
import semios.api.model.dto.chain.DaoRoyaltyToken;
import semios.api.model.enums.BasicDaoEnum;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * dao的收益分配
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-06-12 17:03
 **/
@Data
public class DaoBenefitsDistributeResVo {

    /**
     * dao projectId
     */
    private String projectId;

    /**
     * 代币分配策略
     */
    private DaoRoyaltyToken daoRoyaltyToken;

    /**
     * ETH分配策略
     */
    private DaoEthRoyaltyToken daoEthRoyaltyToken;

    /**
     * 一口价mint铸造收益分配策略
     */
    private DaoReserveRatio fixedReserveRatio;

    /**
     * 非一口价mint铸造收益分配策略
     */
    private DaoReserveRatio unFixedReserveRatio;

    /**
     * Canvas Reward DRB结束时Canvas拥有者分ERC20的比例
     */
    private String canvasReward = "95";

    /**
     * Minter Reward DRB结束时Minter拥有者分ERC20的比例
     */
    private String minterReward = "0";

    /**
     * Canvas minting fee Canvas Mint Fee 非一口价作品铸造时canvas拥有者分ETH的比例
     */
    private String canvasMintFee = "67.5";

    /**
     * Canvas minting fee DAO Mint Fee 非一口价作品铸造时DAO分ETH的比例
     */
    private String daoMintFee = "30";

    /**
     * 已铸造nft数量
     */
    private Integer nftNumber = 0;

    /**
     * dao选的nft铸造总量
     */
    private Integer totalNftMintCap;

    /**
     * 已经过的mint Window
     */
    private Long mintWindow;

    /**
     * dao选择的mintWindow
     */
    private int daoMintWindow;

    /**
     * dao 地板价
     */
    private BigDecimal daoFloorPrice;

    /**
     * canvas价格变化规律 0-指数增加 1-线性增长
     */
    private Integer canvasPriceFluctuationMethod;

    /**
     * canvas价格增长系数 依赖于canvasPriceFluctuationMethod字段
     */
    private BigDecimal fluctuationMethodFactor;
    /**
     * dao版本 目前都是3
     */
    private Integer daoVersion;

    /**
     * dao每天可以铸造的上限  最大值为10000 v1.1
     */
    private int dailyMintCap = 10000;


    /**
     * Dao Token allocation 剩余dao token数量 为erc20没有给其他dao分配的token数量 v1.1
     */
    private BigDecimal daoTokenAllocation;

    /**
     * 当前dao剩余多少token v1.1
     */
    private BigDecimal daoTokenLeave = BigDecimal.ZERO;

    /**
     * 是否为basic dao 1-proto dao 2- basic dao
     *
     * @see BasicDaoEnum
     */
    private Integer basicDao;

    /**
     * 1-已创建未开始2-已开始3-已结束
     * 等于3的时候提示 会给出提示：You can no longer edit as the DAO has ended
     */
    private Integer daoStatus;

    /**
     * 设置Unified Price 为空时没设置
     */
    private BigDecimal unifiedPrice;

    /**
     * 是否设置Unified Price true-设置 false-未设置
     */
    private Boolean unifiedPriceSet = false;
    /**
     * DAO token allocation
     */
    private List<DaoAllocationVo> daoTokenAllocationVos = new ArrayList<>();
    /**
     * DAO eth allocation
     */
    private List<DaoAllocationVo> daoEthAllocationVos = new ArrayList<>();

    /**
     * eth分配比例
     */
    private List<BigDecimal> ethAllocation;
    /**
     * daoToken分配比例
     */
    private List<BigDecimal> daoAllocation;

    /**
     * 线性DRB释放ERC20 0-关闭 1-开启 对应isProgressiveJackpot
     */
    private Boolean royaltyTokenLotteryMode;

    /**
     * 是否为外部ERC20 0-否 1-是
     */
    private Boolean isThirdpartyToken;

    /**
     * 是否开启了TopUp模式 0-否 1-是
     */
    private Boolean topupMode;

    /**
     * 是否为MainDAO false-否 true-是
     */
    private Boolean isMainDaoCreator = false;

    /**
     * 是否开启无限模式，开启时返回1 true，关闭时返回0 false。
     */
    private Boolean infiniteMode = false;

    /**
     * 剩余mintWindow
     */
    private Integer remainingMintWindow;

    /**
     * erc20地址
     */
    private String erc20Address;

    /**
     * 是否开启token 支付模式
     */
    private Boolean daoTokenMode = false;


    /**
     * 1.7 支付货币类型
     */
    private String payCurrencyType;


    /**
     * 1.7 input token的address
     */
    private String inputTokenAddress;

    /**
     * 1.7 input token的decimals
     */
    private Integer inputTokenDecimals;

    /**
     * erc20支付模式下，decimals小数位数
     */
    private Integer erc20TokenDecimals;
}
