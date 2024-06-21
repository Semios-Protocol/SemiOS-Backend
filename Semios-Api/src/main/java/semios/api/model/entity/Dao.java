package semios.api.model.entity;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Objects;

import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateSerializer;
import org.apache.commons.lang3.StringUtils;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;

import lombok.Data;
import semios.api.model.dto.chain.DaoEthRoyaltyToken;
import semios.api.model.dto.chain.DaoReserveRatio;
import semios.api.model.dto.chain.DaoRoyaltyToken;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.enums.BasicDaoEnum;
import semios.api.model.enums.DaoBlackListEnum;
import semios.api.model.enums.DaoWhiteListEnum;

/**
 * <p>
 * dao
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Data
public class Dao implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * DAO名称
     */
    private String daoName;

    /**
     * DAO宣言
     */
    private String daoManitesto;

    /**
     * DAO介绍
     */
    private String daoDescription;

    /**
     * DAOlogo地址
     */
    private String daoLogoUrl;

    /**
     * DAO背景图片地址
     */
    private String daoBgBanner;

    /**
     * DAO开始日期
     */
    @JsonDeserialize(using = LocalDateDeserializer.class)
    @JsonSerialize(using = LocalDateSerializer.class)
    private LocalDate daoStartDate;

    /**
     * 1.4 dao开始drb  去掉 开始使用daoStartBlock
     */
    @Deprecated
    private Integer daoStartDrb;

    /**
     * 最多可铸造的nft数量
     */
    private Integer totalNftCasting;

    /**
     * DAO周期
     */
    private Integer daoMintWindow;

    /**
     * DAO地板价 DAO Floor Price增加0ETH的选项
     */
    private BigDecimal daoFloorPrice;

    /**
     * Dao创建手续费
     */
    private Float daoCreateFee;

    /**
     * opensea链接地址
     */
    private String openseaLink;

    /**
     * Twitter链接地址
     */
    private String twitterLink;

    /**
     * Discord链接地址
     */
    private String discordLink;

    /**
     * dao编号
     */
    private Integer daoNumber;

    /**
     * Dao状态0-未创建1-已创建未开始2-已开始3-已结束 4-已停机
     */
    private Integer daoStatus;

    /**
     * 上链时间
     */
    private String blockTime;

    /**
     * dao erc20 symbol
     */
    private String daoSymbol;

    /**
     * dao创建的交易hash
     */
    private String transactionHash;

    /**
     * Dao拥有者地址
     */
    private String ownerAddress;

    /**
     * dao的uri地址
     */
    private String daoUri;

    /**
     * Dao创建时的drb区块
     */
    private Integer drbNumber;

    /**
     * Dao创建的链上区块号
     */
    private String blockNumber;

    /**
     * dao的projectid
     */
    private String projectId;

    /**
     * 已领取token数量
     */
    private BigDecimal receivedToken;

    /**
     * 未领取token数量
     */
    private BigDecimal unclaimedToken;

    /**
     * 已兑换token数量
     */
    private BigDecimal swapToken;

    /**
     * transfer token
     */
    private BigDecimal transferToken;

    /**
     * 已兑换eht数量
     */
    private BigDecimal swapEth;

    /**
     * project对应的asset pool 地址
     * 1.3
     * daoassetpool指的是自己的subdao asset pool
     */
    private String feePool;

    /**
     * project对应的erc20 token地址
     */
    private String erc20Token;

    /**
     * project对应的erc721 token地址
     */
    private String erc721Token;

    /**
     * 二次售卖印花税设置
     */
    private String royaltyFee;

    /**
     * 发放erc20总数
     */
    private String erc20TotalSupply;

    /**
     * dao收藏数
     */
    private Integer favoriteAmount;
    //数据实体不要写这种赋值操作，默认值在数据库中设置
//    private Integer favoriteAmount = 0;

    /**
     * dao当前canvas最低价格
     */
    private BigDecimal canvasFloorPrice;

//    /**
//     * 社交链接 List<DaoSocialLinks> 的json对象
//     *
//     * @see DaoSocialLinksVo
//     */
    /**
     * 社交链接 多个用逗号分隔
     */
    private String socialLinks;

    /**
     * DAO在上一个区块结束时资金池里的总金额
     */
    private BigDecimal daoAssetPool;

    /**
     * DAO在上一个区块结束时所有未兑换的代币数量
     */
    private BigDecimal unchangedTokenAmount;

    // add work
    /**
     * 是否开通创建canvas白名单 0-未开通 1-开通address 2- 开通ERC721 3-address和ERC721都开通
     *
     * @see DaoWhiteListEnum
     */
    private Integer canvasCreatedWhitelist;

    /**
     * 是否开通创建canvas下某nft白名单  0关闭 1-开启
     */
    private Integer canvasCreatedWhitelistNft;

    /**
     * 是否开通创建canvas黑名单 0-未开通 1-开通
     *
     * @see DaoBlackListEnum
     */
    private Integer canvasCreatedBlacklist;

    // minter
    /**
     * 是否开通铸造nft白名单 0-未开通 1-开通address 2- 开通ERC721 3-address和ERC721都开通
     *
     * @see DaoWhiteListEnum
     */
    private Integer minterWorksWhitelist;

    /**
     * 是否开通铸造erc721下某个nft白名单 0-未开通 1-开通
     */
    private Integer minterWorksWhitelistNft;

    /**
     * 是否开通铸造nft黑名单 0-未开通 1-开通
     *
     * @see DaoBlackListEnum
     */
    private Integer minterWorksBlacklist;

    /**
     * 是否需要刷新opensea 0-不需要 1-需要
     */
    private Integer freshOpensea;

    /**
     * 是否开启 address 高优白名单 0-未开启 1-已开启
     */
    private Integer mintCap;

    /**
     * 是否开启 Erc721 高优白名单 0-未开启 1-已开启
     */
    private Integer erc721MintCap;

    /**
     * 是否开启 Erc721下的 nft 高优白名单 0-未开启 1-已开启
     */
    private Integer erc721MintCapId;

    /**
     * dao全局铸造上限
     */
    private Integer globalMintCap;

    /**
     * 是否开启流动性 0-未开启 1-已开启
     */
    private Integer liquidityPool;

    /**
     * erc20 name 例：D4A Token for D4A@1
     */
    private String erc20Name;

    /**
     * 是否需要同步dex 0-不需要 1-需要
     */
    private Integer syncDex;

    /**
     * DAO发放的所有ERC20数量
     */
    private BigDecimal daoReward;

    /**
     * DAOburn的erc20数量
     */
    private BigDecimal burnAmount;

    /**
     * splitter合约地址
     */
    private String splitterAddress;

    /**
     * 版税二次交易收益
     */
    private BigDecimal royaltyFeeIncome;


    /**
     * 一口价铸造收益分配策略
     *
     * @see DaoReserveRatio
     */
    private String fixedReserveRatio;

    /**
     * 非一口价铸造收益分配策略
     *
     * @see DaoReserveRatio
     */
    private String unfixedReserveRatio;

    /**
     * dao版本 1-1.8.5前的版本 2-1.8.5版本的 3-1.8.5之后的版本
     */
    private Integer daoVersion;

    /**
     * 代币发放逻辑 0-线性生成 1-衰减曲线  对应rewardTemplateType
     */
    private Integer royaltyTokenGenerationMethod;

    /**
     * 奖励发放衰减的系数  对应rewardDecayFactor
     */
    private BigDecimal royaltyTokenGenerationFactory;

    /**
     * 乐透模式 线性DRB释放ERC20 0-关闭 1-开启 对应isProgressiveJackpot
     */
    private Integer royaltyTokenLotteryMode;

    /**
     * canvas价格变化规律 0-指数增加 1-线性增长  对应priceTemplateType
     */
    private Integer canvasPriceFluctuationMethod;

    /**
     * canvas价格增长系数 依赖于canvasPriceFluctuationMethod字段  对应priceFactor
     */
    private BigDecimal fluctuationMethodFactor;


    /**
     * 是否为basic dao 1-proto dao 2- basic dao
     * 1.3 后默认都是proto dao
     *
     * @see BasicDaoEnum
     */
    private Integer basicDao;

    /**
     * dao 交易流水
     */
    private BigDecimal daoFlow;

    /**
     * 程序判断是否生成work的字段 添加work 0-需要添加 1-不需要添加
     */
    private Integer addWork;

    /**
     * PASS模式 合约设置的字段 是否需要创建1000张work 0-需要 1-不需要
     */
    private Integer needMintableWork;

    /**
     * 自动生成图片地址后缀
     */
    private String workUrlSuffix;

    /**
     * 自动生成图片的地址
     */
    private String daoWorkUrl;

    /**
     * 自动生成图片的高度
     */
    private Double height;

    /**
     * 自动生成图片颜色
     */
    private String color;

    /**
     * 自动生成图片的workHash
     */
    private String workHash;


    /**
     * 用户选择关联的旧 Basic DAO v1.1
     */
    private String existDaoId;

    /**
     * dao每天可以铸造的上限  最大值为10000 v1.1
     */
    private Integer dailyMintCap;

    /**
     * 设置自动生成work的总数量 0-为未设置
     */
    private Integer generateWorkSet;

    /**
     * dao全局铸造价格 Unified Price 可以为零
     */
    private BigDecimal globalDaoPrice;

    //1.3加
    /**
     * 是否开启了TopUp模式 0-否 1-是
     */
    private Integer topupMode;

    /**
     * DAO Redeem池
     */
    private String daoRedeemPool;

    /**
     * 是否为外部ERC20 0-否 1-是
     */
    private Integer isThirdpartyToken;

    /**
     * 代币分配策略
     *
     * @see DaoRoyaltyToken
     */
    private String royaltyToken;

    /**
     * ETH分配策略
     *
     * @see DaoEthRoyaltyToken
     */
    private String ethRoyaltyToken;


    /**
     * 是否为MainDAO 0-否 1-是
     */
    private Integer isAncestordao;

    /**
     * 已领取的eth数量
     */
    private BigDecimal receivedEth;

    /**
     * 未领取eth数量
     */
    private BigDecimal unclaimedEth;

    //1.4 新增

    /**
     * 是否开启无限模式，开启时返回1，关闭时返回0。
     */
    private Integer infiniteMode;
    /**
     * 是否开启Erc20支付模式，开启时为1，关闭时为0。
     */
    private Integer erc20PaymentMode;

    /**
     * dao开始block
     */
    private String daoStartBlock;

    /**
     * dao的每个mintableRound的持续时间 若为2小时，传给合约的值应该是 2 * 239055188367481833171 每个小时的区块数
     */
    private String duration;

    //需要增加一个记录当前window的值
    //需要记录下一次开始的区块高度

    /**
     * dao 当前周期数
     * 如果dao停止后重新开始，则这个数从零开始计
     * 如果改为无限模式，从零开始计
     * 从无限模式改为非无限模式 从开始计
     */
    private String currentRound;

    /**
     * 剩余mintWindow  查询getDaoRemainingRound方法
     */
    private Integer remainingMintWindow;

    /**
     * 最后一个活跃周期的编号 查询getDaoLastActiveRound方法
     */
    private Integer lastActiveRound;

    /**
     * erc20支付模式下，decimals小数位数
     */
    private Integer erc20TokenDecimals;

    /**
     * 是否为聚合dao，0-否，1-是
     */
    private Integer isTogetherDao;

    /**
     * 聚合dao的daoID
     */
    private Integer togetherDaoId;

    /**
     * token的holder数量
     */
    private Integer tokenHolders;

    /**
     * 重新开始的mintWindow 默认0
     */
    private Integer lastModifyRound;

    /**
     * subDao AssetPool Balance 展示最新的balance余额
     */
    private String subdaoAssetPoolBalance;

    /**
     * 1.5 记录dao重启之后的区块高度
     */
    private String daoRestartBlock;

    /**
     * 1.6 国库 对应的erc20 token地址
     */
    private String treasuryErc20;

    /**
     * 1.6 eth解锁dao token的wallet比例(未开启wec20)
     */
    private BigDecimal ethTokenRoyalty;

    /**
     * 1.6 dao token解锁eth的wallet比例(开启erc20)
     */
    private BigDecimal tokenEthRoyalty;

    /**
     * 1.6 创建dao 生成的nft凭证721,,暂时与原先的721相同
     */
    private String daoNftErc721;

    /**
     * 1.6 创建dao，给sub dao 打款生成的erc721地址
     */
    private String grantDaoNftErc721;

    /**
     * 1.6 创建Main dao用于给国库打款赠送的erc721的地址
     */
    private String grantTreasuryNftErc721;

    /**
     * 1.7 支付货币类型
     */
    private String payCurrencyType;

    /**
     * 1.7 input token的logo地址
     */
    private String inputTokenLogo;

    /**
     * 1.7 input token的address
     */
    private String inputTokenAddress;

    /**
     * 1.7 input token的decimals
     */
    private Integer inputTokenDecimals;


    // *******************************************************//

    /**
     * DAO在该区块结束时canvas的数量
     */
    @TableField(exist = false)
    private Integer canvas;

    /**
     * DAO在该区块结束时所有work的数量
     */
    @TableField(exist = false)
    private Integer works;

    /**
     * DAO在该区块结束时所有canvas的最低价
     */
    @TableField(exist = false)
    private BigDecimal floorPrice;

    /**
     * 是否被当前用户收藏
     */
    @TableField(exist = false)
    private Boolean favorited = false;

//    //去掉 以后不要重写这种类型的了，只在用的地方重新判断一下
//    public String getDaoManitesto() {
//        if (StringUtils.isBlank(daoManitesto)) {
//            return "";
//        }
//        return daoManitesto;
//    }
//
//    public String getDaoDescription() {
//        if (StringUtils.isBlank(daoDescription)) {
//            return "";
//        }
//        return daoDescription;
//    }

    public String getDaoLogoUrl() {
        if (StringUtils.isBlank(daoLogoUrl)) {
            return daoLogoUrl;
        }
        return daoLogoUrl.replace(ProtoDaoConstant.s3ImageUrl, ProtoDaoConstant.s3CdnImageUrl);
    }

    public String getDaoBgBanner() {
        if (StringUtils.isBlank(daoBgBanner)) {
            return daoBgBanner;
        }
        return daoBgBanner.replace(ProtoDaoConstant.s3ImageUrl, ProtoDaoConstant.s3CdnImageUrl);
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Dao dao = (Dao) o;
        return id.equals(dao.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
}
