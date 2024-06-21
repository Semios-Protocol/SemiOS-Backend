package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import semios.api.interceptor.WebConfigurer;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoDrbStatistics;
import semios.api.model.entity.TokenReceivedRecord;
import semios.api.model.entity.Work;
import semios.api.model.enums.TokenTypeEnum;
import semios.api.utils.BeanUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @description: analytics
 * @author: xiangbin
 * @create: 2023-04-19 15:14
 **/
@Data
@Slf4j
public class DaoAnalyticsResVo {

    /**
     * 名称
     *
     * @mock Volume
     */
    private String name;

    /**
     * 当前值
     *
     * @mock 45
     */
    private BigDecimal value = BigDecimal.ZERO;

    /**
     * 增长比例 可为正数 负数 0 或者空 四种情况
     *
     * @mock 0.45
     */
    private BigDecimal increase;

    /**
     * 是否展示图标 0- 不展示 1- 展示
     *
     * @mock 0
     */
    private Integer icon = 0;

    /**
     * 跳转地址 为空时不展示 View details
     *
     * @mock https://testnets.opensea.io/collection/dao4art-yyy
     */
    private String url;

    /**
     * 1.4 是否开启Erc20支付模式 false-否 true-是
     */
    private Boolean erc20PaymentMode = false;

    /**
     * 1.7 支付货币类型
     */
    private String payCurrencyType;

    /**
     * 1.7 input token的address
     */
    private String inputTokenAddress;

    /**
     * dao symbol
     */
    private String daoSymbol;

    /**
     * dao symbol
     */
    private String daoErc20Address;

    /**
     * 构造交易总额 Total Vol
     *
     * @param firstWorkList  最近7天交易数据
     * @param secondWorkList 7-14天交易数据
     * @return DaoAnalyticsResVo
     */
    public static DaoAnalyticsResVo buildTotalVol(List<Work> firstWorkList, List<Work> secondWorkList) {

        DaoAnalyticsResVo daoAnalyticsResVo = new DaoAnalyticsResVo();
        daoAnalyticsResVo.setName("Total Vol");
        daoAnalyticsResVo.setIcon(1);

        BigDecimal firstValue = BigDecimal.ZERO;
        BigDecimal secondValue;
        if (firstWorkList != null) {
            firstValue = firstWorkList.stream().map(Work::getMintedPrice).reduce(BigDecimal.ZERO, BigDecimal::add);
            daoAnalyticsResVo.setValue(firstValue);
        }
        if (secondWorkList != null && secondWorkList.size() > 0) {
            secondValue = secondWorkList.stream().map(Work::getMintedPrice).reduce(BigDecimal.ZERO, BigDecimal::add);
            if (!secondValue.equals(BigDecimal.ZERO)) {
                daoAnalyticsResVo.setIncrease(buildIncrease(firstValue, secondValue));
            }
        }

        return daoAnalyticsResVo;
    }

    /**
     * top bid dao七天内的最大铸造价格
     *
     * @param firstWorkList  最近7天交易数据
     * @param secondWorkList 7-14天交易数据
     * @return DaoAnalyticsResVo
     */
    public static DaoAnalyticsResVo buildTopBid(List<Work> firstWorkList, List<Work> secondWorkList) {

        DaoAnalyticsResVo daoAnalyticsResVo = new DaoAnalyticsResVo();
        daoAnalyticsResVo.setName("Top Bid");
        daoAnalyticsResVo.setIcon(0);

        BigDecimal firstValue = BigDecimal.ZERO;
        if (firstWorkList != null) {
            Optional<Work> firstOptional = firstWorkList.stream()
                    .max(Comparator.comparing(Work::getMintedPrice).thenComparing(Work::getBlockTime));
            if (firstOptional.isPresent()) {
                daoAnalyticsResVo
                        .setUrl(WebConfigurer.allowedDomain1 + "/workDetails?id=" + firstOptional.get().getId());
                firstValue = firstOptional.get().getMintedPrice();
                daoAnalyticsResVo.setValue(firstValue);
            }
        }

        if (secondWorkList != null) {
            Optional<Work> secondOptional = secondWorkList.stream().max(Comparator.comparing(Work::getMintedPrice));
            if (secondOptional.isPresent()) {
                BigDecimal secondValue = secondOptional.get().getMintedPrice();
                if (!secondValue.equals(BigDecimal.ZERO)) {
                    daoAnalyticsResVo.setIncrease(buildIncrease(firstValue, secondValue));
                }
            }
        }

        return daoAnalyticsResVo;
    }

    /**
     * Mint Vol Dao七天内的铸造数量 Mint Quantity
     *
     * @param firstWorkList  最近7天交易数据
     * @param secondWorkList 7-14天交易数据
     * @return DaoAnalyticsResVo
     */
    public static DaoAnalyticsResVo buildMintQuantity(List<Work> firstWorkList, List<Work> secondWorkList) {

        DaoAnalyticsResVo daoAnalyticsResVo = new DaoAnalyticsResVo();
        daoAnalyticsResVo.setName("Mint Quantity");
        daoAnalyticsResVo.setIcon(0);

        BigDecimal firstValue = BigDecimal.ZERO;
        if (firstWorkList != null) {
            firstValue = new BigDecimal(firstWorkList.size());
            daoAnalyticsResVo.setValue(firstValue);
        }

        if (secondWorkList != null) {
            BigDecimal secondValue = new BigDecimal(secondWorkList.size());
            if (!secondValue.equals(BigDecimal.ZERO)) {
                daoAnalyticsResVo.setIncrease(buildIncrease(firstValue, secondValue));
            }
        }

        return daoAnalyticsResVo;
    }

    /**
     * Nft Owners 当前Dao这一刻NFT拥有者的数量
     *
     * @param nftOwners      当前nft owner数量
     * @param endDateOwners  7天前owner数量
     * @param secondWorkList 7-14天交易数据
     * @return DaoAnalyticsResVo
     */
    public static DaoAnalyticsResVo buildNftOwners(Integer nftOwners, Integer endDateOwners, List<Work> secondWorkList,
                                                   Dao dao) {

        DaoAnalyticsResVo daoAnalyticsResVo = new DaoAnalyticsResVo();
        daoAnalyticsResVo.setName("NFTs Owners");
        daoAnalyticsResVo.setIcon(0);
        daoAnalyticsResVo.setValue(new BigDecimal(String.valueOf(nftOwners)));
        daoAnalyticsResVo.setUrl(
                ProtoDaoConstant.daoOpenseaLink + ("protodao " + dao.getDaoName()).toLowerCase().replaceAll(" ", "-"));
        BigDecimal firstValue;
        // 如果不足7天secondWorkList为空则不展示涨幅
        if (secondWorkList != null && secondWorkList.size() > 0) {

            firstValue = new BigDecimal(String.valueOf(endDateOwners));

            daoAnalyticsResVo.setIncrease(
                    buildIncrease(daoAnalyticsResVo.getValue(), daoAnalyticsResVo.getValue().subtract(firstValue)));
        }

        // if (secondWorkList != null) {
        // BigDecimal secondValue = new BigDecimal(
        // new HashSet<>(secondWorkList.stream().map(Work::getOwnerAddress).collect(Collectors.toList())).size());
        // if (!secondValue.equals(BigDecimal.ZERO)) {
        // daoAnalyticsResVo.setIncrease(buildIncrease(firstValue, secondValue));
        // }
        // }

        return daoAnalyticsResVo;
    }

    /**
     * DAO Floor Price 当前DAO这一刻的Floor Price
     *
     * @param dao              dao
     * @param daoDrbStatistics daoDrbStatistics
     * @return DaoAnalyticsResVo
     */
    public static DaoAnalyticsResVo buildDaoFloorPrice(Dao dao, DaoDrbStatistics daoDrbStatistics) {

        DaoAnalyticsResVo daoAnalyticsResVo = new DaoAnalyticsResVo();
        daoAnalyticsResVo.setName("DAO Floor Price");
        daoAnalyticsResVo.setIcon(1);
        daoAnalyticsResVo.setValue(dao.getCanvasFloorPrice().setScale(4, RoundingMode.FLOOR).stripTrailingZeros());
        if (daoDrbStatistics != null && daoDrbStatistics.getFloorPrice() != null) {
            daoAnalyticsResVo.setIncrease(buildIncrease(dao.getCanvasFloorPrice(), daoDrbStatistics.getFloorPrice()));
        }

        if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
            daoAnalyticsResVo.setValue(dao.getGlobalDaoPrice().setScale(4, RoundingMode.FLOOR).stripTrailingZeros());
            daoAnalyticsResVo.setIncrease(BigDecimal.ZERO);
        }

        return daoAnalyticsResVo;
    }

    /**
     * D4A.T123 Owners 当前DAO这一刻ERC20拥有者的数量
     *
     * @param amount
     * @param firstTokenReceivedList  当前代币领取记录
     * @param secondTokenReceivedList 七天前代币领取记录
     * @param secondWorkList
     * @return DaoAnalyticsResVo
     */
    public static DaoAnalyticsResVo buildErc20Owners(int amount, List<TokenReceivedRecord> firstTokenReceivedList,
                                                     List<TokenReceivedRecord> secondTokenReceivedList, List<Work> secondWorkList, Dao dao) {

        DaoAnalyticsResVo daoAnalyticsResVo = new DaoAnalyticsResVo();
        daoAnalyticsResVo.setName(dao.getDaoSymbol() + " Owners");
        daoAnalyticsResVo.setIcon(0);
        daoAnalyticsResVo.setUrl(ProtoDaoConstant.etherscanUrl + ProtoDaoConstant.tokenUrl + dao.getErc20Token());
        BigDecimal owners = BigDecimal.ZERO;
        if (firstTokenReceivedList != null) {
            List<TokenReceivedRecord> firstTransferList = firstTokenReceivedList.stream()
                    .filter(v -> v.getTokenType().equals(TokenTypeEnum.TRANSFER.getType())).collect(Collectors.toList());
            if (firstTransferList.size() > 0) {
                try {
                    List<TokenReceivedRecord> transferList =
                            BeanUtil.transTo(firstTransferList, TokenReceivedRecord.class);
                    // 增加临时类型变量 99
                    transferList.forEach(v -> v.setTokenType(99));
                    // 额外增加一遍，算作fromAddress的出账
                    firstTokenReceivedList.addAll(transferList);
                } catch (Exception e) {
                    log.error("daoId:{} BeanUtil transTo exception firstTransferList:{}", dao.getId(),
                            JacksonUtil.obj2json(firstTransferList));
                }
            }
            // log.info("daoId:{} firstTokenReceivedList:{}", dao.getId(),
            // JacksonUtil.obj2json(firstTokenReceivedList));
            Map<String, BigDecimal> decimalMap = firstTokenReceivedList.stream().collect(Collectors.toMap(v -> {
                if (v.getTokenType().equals(99) || v.getTokenType().equals(TokenTypeEnum.SWAP.getType())) {
                    return v.getFromAddress();
                } else {
                    return v.getToAddress();
                }
            }, w -> {
                if (w.getTokenType().equals(99) || w.getTokenType().equals(TokenTypeEnum.SWAP.getType())) {
                    return w.getTokenNum().negate();
                } else {
                    return w.getTokenNum();
                }
            }, BigDecimal::add));

            log.info("daoId:{} decimalMap:{}", dao.getId(), JacksonUtil.obj2json(decimalMap));

            owners =
                    new BigDecimal((int) decimalMap.values().stream().filter(v -> v.compareTo(BigDecimal.ZERO) > 0).count());
            daoAnalyticsResVo.setValue(new BigDecimal(String.valueOf(amount)));
        }
        if (secondTokenReceivedList != null && secondWorkList != null && secondWorkList.size() > 0) {
            List<TokenReceivedRecord> secondTransferList = secondTokenReceivedList.stream()
                    .filter(v -> v.getTokenType().equals(TokenTypeEnum.TRANSFER.getType())).collect(Collectors.toList());
            if (secondTransferList.size() > 0) {
                try {
                    List<TokenReceivedRecord> transferList =
                            BeanUtil.transTo(secondTransferList, TokenReceivedRecord.class);
                    // 增加临时类型变量 99
                    transferList.forEach(v -> v.setTokenType(99));
                    // 额外增加一遍，算作fromAddress的出账
                    firstTokenReceivedList.addAll(transferList);
                } catch (Exception e) {
                    log.error("daoId:{} BeanUtil transTo exception secondTransferList:{}", dao.getId(),
                            JacksonUtil.obj2json(secondTransferList));
                }
            }
            Map<String, BigDecimal> decimalMap = secondTokenReceivedList.stream().collect(Collectors.toMap(v -> {
                if (v.getTokenType().equals(99) || v.getTokenType().equals(TokenTypeEnum.SWAP.getType())) {
                    return v.getFromAddress();
                } else {
                    return v.getToAddress();
                }
            }, w -> {
                if (w.getTokenType().equals(99) || w.getTokenType().equals(TokenTypeEnum.SWAP.getType())) {
                    return w.getTokenNum().negate();
                } else {
                    return w.getTokenNum();
                }
            }, BigDecimal::add));

            BigDecimal secondOwners =
                    new BigDecimal((int) decimalMap.values().stream().filter(v -> v.compareTo(BigDecimal.ZERO) > 0).count());
            if (secondOwners.compareTo(BigDecimal.ZERO) > 0) {
                daoAnalyticsResVo.setIncrease(buildIncrease(owners, secondOwners));
            }
        }

        return daoAnalyticsResVo;
    }

    // =============private separate==========================================//

    private static BigDecimal buildIncrease(BigDecimal firstValue, BigDecimal secondValue) {
        if (secondValue.compareTo(BigDecimal.ZERO) == 0) {
            return BigDecimal.ZERO;
        }
        return secondValue.subtract(firstValue).divide(secondValue, 4, RoundingMode.HALF_UP)
                .multiply(BigDecimal.ONE.negate()).multiply(new BigDecimal("100")).stripTrailingZeros();
    }

    public static void main(String[] args) throws Exception {
        // List<TokenReceivedRecord> tokenReceivedRecordList = new ArrayList<>();
        // TokenReceivedRecord tokenReceivedRecord = new TokenReceivedRecord();
        // tokenReceivedRecord.setFromAddress("1");
        // tokenReceivedRecord.setToAddress("1");
        // tokenReceivedRecord.setTokenType(0);
        // tokenReceivedRecord.setTokenNum(new BigDecimal("5"));
        //
        // TokenReceivedRecord tokenReceivedRecord1 = new TokenReceivedRecord();
        // tokenReceivedRecord1.setFromAddress("2");
        // tokenReceivedRecord1.setToAddress("2");
        // tokenReceivedRecord1.setTokenType(0);
        // tokenReceivedRecord1.setTokenNum(new BigDecimal("5"));
        //
        // TokenReceivedRecord tokenReceivedRecord2 = new TokenReceivedRecord();
        // tokenReceivedRecord2.setFromAddress("3");
        // tokenReceivedRecord2.setToAddress("3");
        // tokenReceivedRecord2.setTokenType(0);
        // tokenReceivedRecord2.setTokenNum(new BigDecimal("5"));
        //
        // TokenReceivedRecord tokenReceivedRecord3 = new TokenReceivedRecord();
        // tokenReceivedRecord3.setFromAddress("1");
        // tokenReceivedRecord3.setToAddress("1");
        // tokenReceivedRecord3.setTokenType(99);
        // tokenReceivedRecord3.setTokenNum(new BigDecimal("5"));
        // tokenReceivedRecordList.add(tokenReceivedRecord);
        // tokenReceivedRecordList.add(tokenReceivedRecord1);
        // tokenReceivedRecordList.add(tokenReceivedRecord2);
        // tokenReceivedRecordList.add(tokenReceivedRecord3);
        //
        // tokenReceivedRecordList = JacksonUtil.json2list(
        // "[{\"id\":392,\"tokenNum\":666666.660000,\"receiveType\":3,\"receiveId\":216,\"receiveAddress\":\"0xe6046371b729f23206a94ddcace89fecebbd565c\",\"projectId\":\"e541d48c5cd414ab9b937eae9842755d34d40200069527bcbcf4f57057737afa\",\"canvasId\":null,\"blockNumber\":\"0x890705\",\"blockTime\":\"1683791340\",\"transactionHash\":\"0xa0f91b4de0da3d0eed92fc7655f6752ff2d6b20a6bef3b43aeae8f6de7460b0b\",\"drbNumber\":4994,\"tokenType\":99,\"fromAddress\":\"0xd11a8d3a72d3ef5da1c531386438c29d1a212664\",\"toAddress\":\"0xe6046371b729f23206a94ddcace89fecebbd565c\",\"ethAmount\":null,\"daoNumber\":189,\"tokenNumBalance\":666666.660000},{\"id\":393,\"tokenNum\":1000000.000000,\"receiveType\":1,\"receiveId\":216,\"receiveAddress\":\"0xb90b90225e628188a16c1ab2ffbd8372e49b39df\",\"projectId\":\"e541d48c5cd414ab9b937eae9842755d34d40200069527bcbcf4f57057737afa\",\"canvasId\":null,\"blockNumber\":\"0x890705\",\"blockTime\":\"1683791340\",\"transactionHash\":\"0xa0f91b4de0da3d0eed92fc7655f6752ff2d6b20a6bef3b43aeae8f6de7460b0b\",\"drbNumber\":4994,\"tokenType\":0,\"fromAddress\":\"0xd11a8d3A72d3eF5DA1c531386438C29d1A212664\",\"toAddress\":\"0xb90b90225e628188a16c1ab2ffbd8372e49b39df\",\"ethAmount\":null,\"daoNumber\":189,\"tokenNumBalance\":0.000000},{\"id\":394,\"tokenNum\":31666666.660000,\"receiveType\":2,\"receiveId\":118,\"receiveAddress\":\"0xb90b90225e628188a16c1ab2ffbd8372e49b39df\",\"projectId\":\"e541d48c5cd414ab9b937eae9842755d34d40200069527bcbcf4f57057737afa\",\"canvasId\":\"7c104583502680fd03e507211f4934b97714d7b18e1d504367f6380b373c2ad2\",\"blockNumber\":\"0x890705\",\"blockTime\":\"1683791340\",\"transactionHash\":\"0xa0f91b4de0da3d0eed92fc7655f6752ff2d6b20a6bef3b43aeae8f6de7460b0b\",\"drbNumber\":4994,\"tokenType\":0,\"fromAddress\":\"0xd11a8d3A72d3eF5DA1c531386438C29d1A212664\",\"toAddress\":\"0xb90b90225e628188a16c1ab2ffbd8372e49b39df\",\"ethAmount\":null,\"daoNumber\":189,\"tokenNumBalance\":0.000000},{\"id\":395,\"tokenNum\":1000000.000000,\"receiveType\":3,\"receiveId\":118,\"receiveAddress\":\"0xf8baf7268f3daefe4135f7711473ae8b6c3b47d8\",\"projectId\":\"e541d48c5cd414ab9b937eae9842755d34d40200069527bcbcf4f57057737afa\",\"canvasId\":\"7c104583502680fd03e507211f4934b97714d7b18e1d504367f6380b373c2ad2\",\"blockNumber\":\"0x891b52\",\"blockTime\":\"1683869856\",\"transactionHash\":\"0x2e523d840bb6af4b55cf0ff297a4eec9d9cc82bc5bf84303ddd45e4e2e136498\",\"drbNumber\":5015,\"tokenType\":99,\"fromAddress\":\"0xb90b90225e628188a16c1ab2ffbd8372e49b39df\",\"toAddress\":\"0xf8baf7268f3daefe4135f7711473ae8b6c3b47d8\",\"ethAmount\":null,\"daoNumber\":189,\"tokenNumBalance\":1000000.000000},{\"id\":392,\"tokenNum\":666666.660000,\"receiveType\":3,\"receiveId\":216,\"receiveAddress\":\"0xe6046371b729f23206a94ddcace89fecebbd565c\",\"projectId\":\"e541d48c5cd414ab9b937eae9842755d34d40200069527bcbcf4f57057737afa\",\"canvasId\":null,\"blockNumber\":\"0x890705\",\"blockTime\":\"1683791340\",\"transactionHash\":\"0xa0f91b4de0da3d0eed92fc7655f6752ff2d6b20a6bef3b43aeae8f6de7460b0b\",\"drbNumber\":4994,\"tokenType\":99,\"fromAddress\":\"0xd11a8d3a72d3ef5da1c531386438c29d1a212664\",\"toAddress\":\"0xe6046371b729f23206a94ddcace89fecebbd565c\",\"ethAmount\":null,\"daoNumber\":189,\"tokenNumBalance\":666666.660000},{\"id\":395,\"tokenNum\":1000000.000000,\"receiveType\":3,\"receiveId\":118,\"receiveAddress\":\"0xf8baf7268f3daefe4135f7711473ae8b6c3b47d8\",\"projectId\":\"e541d48c5cd414ab9b937eae9842755d34d40200069527bcbcf4f57057737afa\",\"canvasId\":\"7c104583502680fd03e507211f4934b97714d7b18e1d504367f6380b373c2ad2\",\"blockNumber\":\"0x891b52\",\"blockTime\":\"1683869856\",\"transactionHash\":\"0x2e523d840bb6af4b55cf0ff297a4eec9d9cc82bc5bf84303ddd45e4e2e136498\",\"drbNumber\":5015,\"tokenType\":99,\"fromAddress\":\"0xb90b90225e628188a16c1ab2ffbd8372e49b39df\",\"toAddress\":\"0xf8baf7268f3daefe4135f7711473ae8b6c3b47d8\",\"ethAmount\":null,\"daoNumber\":189,\"tokenNumBalance\":1000000.000000}]",
        // TokenReceivedRecord.class);
        // Map<String, List<TokenReceivedRecord>> stringListMap =
        // tokenReceivedRecordList.stream().collect(Collectors.groupingBy(v -> {
        // if (v.getTokenType().equals(99) || v.getTokenType().equals(TokenTypeEnum.SWAP.getType())) {
        // return v.getFromAddress();
        // } else {
        // return v.getToAddress();
        // }
        // }));

        // Map<String, BigDecimal> decimalMap = tokenReceivedRecordList.stream().collect(Collectors.toMap(v -> {
        // if (v.getTokenType().equals(99) || v.getTokenType().equals(TokenTypeEnum.SWAP.getType())) {
        // return v.getFromAddress();
        // } else {
        // return v.getToAddress();
        // }
        // }, w -> {
        // if (w.getTokenType().equals(99) || w.getTokenType().equals(TokenTypeEnum.SWAP.getType())) {
        // return w.getTokenNum().negate();
        // } else {
        // return w.getTokenNum();
        // }
        // }, BigDecimal::add));
        // BigDecimal owners =
        // new BigDecimal((int)decimalMap.values().stream().filter(v -> v.compareTo(BigDecimal.ZERO) > 0).count());
        // System.out.println(owners);
        // System.out.println("1");
        String str = "{\n" + "    \"0x686b7ce5ad6cdc491cd3b171f95adf99fe12eed6\": 794.200000,\n"
                + "    \"0x1a495a32eb384fdc37a9a96436721f4f3a9947f0\": 680.950000,\n"
                + "    \"0x0b10bfab2e6a4e899cd1ef842c12fe2036ceebee\": 806.080000,\n"
                + "    \"0x9f73aa3ee037781cac954edce251d4f56ae5858b\": 193.970000,\n"
                + "    \"0x07af7a1fae95537c38194358d5a53f8970893c66\": 915.630000,\n"
                + "    \"0x06ee93725effa8a89c35467d378bd9f97b3d0679\": 955.320000,\n"
                + "    \"0xaad9603067cc3a93ef900dab0152b95212b30af7\": 688.330000,\n"
                + "    \"0x5e4eb5023cd85131d148740d495f4ba641dd50d9\": 944.040000,\n"
                + "    \"0x34c5e6a43659ee43dec852fe8ad2b90afce3d2a2\": 913.080000,\n"
                + "    \"0xf5eb5456505b1649b09f75637df50c255285c54c\": 658.360000,\n"
                + "    \"0xecc2ba7c8a1c024c06dea99cb0b353fa80326663\": 477.780000,\n"
                + "    \"0xd4bdd0a429ef7035e648d35b02f4d0faecf5efc8\": 2505.060000,\n"
                + "    \"0x4c94d71c54396f122665989005bee772d959ccbe\": 975.720000,\n"
                + "    \"0xa4fc1322e57332f59db787a59308240197c437b9\": 527.590000,\n"
                + "    \"0x7a349ed7a528b1d3b55b1de3216928d92682a574\": 310.600000,\n"
                + "    \"0x8881878bab5d6693e6d200b00305597262be379b\": 3840.170000,\n"
                + "    \"0xf078a18a904b3349939f0d6c640f5b08e06bb5e7\": 1031979.500000,\n"
                + "    \"0xa21cbc6ce2135f56d7c432081260eeec2afcd81d\": 667.130000,\n"
                + "    \"0xbbd09b19c83fde6c13601a2ca70f79c464233d95\": 814.560000,\n"
                + "    \"0xb048ed71007ade3b3172bc6d95781833ffa3aac5\": 0.000000,\n"
                + "    \"0x2474a2d82de408e3ae9adf329f8b86c56483ac4e\": 143.030000,\n"
                + "    \"0x3a7f3d5ce23ed409274391673e43299ecbd21183\": 483.990000,\n"
                + "    \"0x30335bdf635747017b2e1d3506705ffc3ce659d8\": 128.440000,\n"
                + "    \"0xc2571305aa2b04fc0760a5d1dc1dba524e89c0e1\": 2117.510000,\n"
                + "    \"0xb5df11a77bd680dc17d3bbc55dfb2317be6ef2c2\": 234.720000,\n"
                + "    \"0xca2c3db51e8fee194275dbb23fc9e6d2ea9b9107\": 297.660000,\n"
                + "    \"0x6c2b470b014534e60dad221e0a5b32e4383c637c\": 733.400000,\n"
                + "    \"0x87bb666740e9ac6ea637b691341e8fa0b66516e9\": 277.840000,\n"
                + "    \"0xca8d7d34047ad5d0252f3f6aec3fcabcb3b8f043\": 208.430000,\n"
                + "    \"0xb9fea3529d4b7a88ecff109a5628d025b748efc4\": 391.990000,\n"
                + "    \"0x4494de67ddd111a3722d13d504094f02f7c6fdc1\": 650.210000,\n"
                + "    \"0xaf3c7003ada5c49c9aed00d70ec77eec01c88fd0\": 386.220000,\n"
                + "    \"0x556579dc18d6827ade60ffc3946919981ceeac55\": 768.920000,\n"
                + "    \"0xe6cbbb7b6382e6c40326533222f21152d120dccf\": 415.650000,\n"
                + "    \"0x7933c3ccb9ed198d5cfff1338e24c924f36fbba9\": 533.930000,\n"
                + "    \"0xd0c242bced08d7edb8244b71a8e036845939d924\": 1004.260000,\n"
                + "    \"0x36b425d137ea6a074a1b788f0944f5a0ef629e1b\": 777.320000,\n"
                + "    \"0x507f6c5b2a2f6b532d6c39ba614be7fae5525780\": 364.020000,\n"
                + "    \"0x4b306bf88147eb972460f0e582818967c925341f\": 926.010000,\n"
                + "    \"0x2a4b29664a59da82fccfbc3f67a7f0a9332638d9\": 222.590000,\n"
                + "    \"0xf54e7b37b55dd0c398a0b9238ebd3a2322d49c75\": 647.020000,\n"
                + "    \"0x74d3c5822129e804730c2abcf93c03f70d7593e1\": 256.340000,\n"
                + "    \"0xc9373261f261622a5aadd1cb80b0dbb9ffdf1780\": 651.850000,\n"
                + "    \"0xcfc0e6fc6443058922003cfd7182a1088bf5a0f8\": 1034.040000,\n"
                + "    \"0x3f42d66efcbb4cfd991decf370014c713cb33b2c\": 0.000000,\n"
                + "    \"0xae418f08b908cf4fd50c9407d134e093bbb22004\": 4151.100000,\n"
                + "    \"0xfd4b092d4c49834cda0a421984c1f65656a05314\": 0.830000,\n"
                + "    \"0x125bf0fea5c0a78c737bc467390122858dac11b0\": 871.260000,\n"
                + "    \"0x47b025cfe00aa6919b501fde02b58f128bf1a064\": 354.380000,\n"
                + "    \"0x68ef7bed246dd382da52b7da21eeb6d5df586f7f\": 980.910000,\n"
                + "    \"0xea971c4c01cf829556596483d5861e7b066eee86\": 1770.520000,\n"
                + "    \"0x68831995780c7a3cda54b3bbbff705cd8ab2d60c\": 2128.430000,\n"
                + "    \"0x84eae39383b177caa883493024220725ed4d0dcc\": 204.650000,\n"
                + "    \"0xec693e3e2e69c4c68620eca4b6124c76b37f1599\": 178.630000,\n"
                + "    \"0xf92b69a402fcd10653d6ac48f639748b87e7be68\": 539.880000,\n"
                + "    \"0x6b75d8af000000e20b7a7ddf000ba900b4009a80\": 0.000000,\n"
                + "    \"0xd5ddfa332e00a028a6748103d15500db090f4d1a\": 606.540000,\n"
                + "    \"0x4d22515b23e6541a26d71b23eb163da851ff96bf\": 471.280000,\n"
                + "    \"0x68c410b5b43561f7033cf205e24dfb80240fd404\": 177.870000,\n"
                + "    \"0x51c93670c3956412e99caa4844bac0f1e5bd4860\": 616.990000,\n"
                + "    \"0x3c3498412dda4e16f0dede252d5e70551d3ab8fb\": 147.630000,\n"
                + "    \"0xcccd912352e33e6aec6930859309d2fd06c5ba1d\": 271.130000,\n"
                + "    \"0x10d656fb81cbb580874eead373df3fd403d3286f\": 351.210000,\n"
                + "    \"0x243c272de70380c058638c377884fe606021d7e5\": 839.310000,\n"
                + "    \"0x6aeed35781a8fb5b8949c2edc70891db2116f41b\": 344.350000,\n"
                + "    \"0x89ef9b58f1b16a5909b29bb3243080bb3fdb16fe\": 937.130000,\n"
                + "    \"0xa6bc64f0ba3638be1eadcea4b1aee4a925428113\": 563.770000,\n"
                + "    \"0xe1fefab1f2772ee2fe05dc19639eb3642fb4cf77\": 355.250000,\n"
                + "    \"0x4946e68566f32c7d32bc966c2600fc5f25e4ee6d\": 629.650000,\n"
                + "    \"0xf065a5d157b10ba3cb695f2d076c11741f0a8d8f\": 143.870000,\n"
                + "    \"0xb2ec190be89fe26a0b37b8d2ba260002b4dd387d\": 3101.120000,\n"
                + "    \"0xd3c06e8bb15b7692d1b0c079bed94f82790f5059\": 126.320000,\n"
                + "    \"0xa2194fe9cdac3a17758f523dac7cd3bc51489bdd\": 145.740000,\n"
                + "    \"0x29bce06a05f5943c20b2e81411afb0e667938e0f\": 764.570000,\n"
                + "    \"0x5cf7d73e90c3a213c806356990d0e196226c42b4\": 479.930000,\n"
                + "    \"0x1931a62b7664b860da64d43a6bd93b1b7055381b\": 2063.210000,\n"
                + "    \"0xe4f53a88dc1fbe42d4e417a5c50fee0a1430ab43\": 357.170000,\n"
                + "    \"0x2e3d5d655800621b91f0acc02bb90ec359501893\": 252.080000,\n"
                + "    \"0xe5a50ed31a31b413dd3607159f36505fe2273039\": 256.080000,\n"
                + "    \"0xd45b56ab58465874d8f5bfa309d14350177b4c8c\": 500.090000,\n"
                + "    \"0xbc481238be741eee8dd33431b770ad0fb0d45322\": 186.600000,\n"
                + "    \"0x8441bf30f70c311e29f24fd106e2d75ef15405ef\": 825.460000,\n"
                + "    \"0x22a65ac90e9ddbe3725713f7649c6be68df0c6d3\": 985.250000,\n"
                + "    \"0x512c7bf2a30fae3f6e30efe228dfa70ed240cc4f\": 301.860000,\n"
                + "    \"0x4d2876571ab49a86487021aace23a6f3d16cc128\": 1866.300000,\n"
                + "    \"0xeba1d2a51dcc74be22ee65f86b474ee1c2134bf1\": 970.690000,\n"
                + "    \"0x087663b75770687128219b0bf1957e224188365a\": 285.840000,\n"
                + "    \"0xdc75da24e2269027e191796912ae024c0699f879\": 496.350000,\n"
                + "    \"0xfed3c3cff7400535efa6f55db3b3ef6d2eb59973\": 853.160000,\n"
                + "    \"0x200be1adf7326ccc56a0745a2b74107801f40bd6\": 0.000000,\n"
                + "    \"0x5ade50593b444e56f66d8fa64fffccc8bebc89c1\": 147.750000,\n"
                + "    \"0xecdd9c52348f30a95d57107c909622fd74d906bc\": 494.370000,\n"
                + "    \"0xa5e151a8e733a880fd1b79553c85dc99cfe0cfc5\": 419.590000,\n"
                + "    \"0x161f3ecb1fa5e30909a7443aa39ae2f143d59b7b\": 279.260000,\n"
                + "    \"0xd0a790a37e3e05e418bf1c23672de0da774ffcc0\": 537.310000,\n"
                + "    \"0x20e4389be38d29db12bc9cd878b3d145f4d8cffa\": 478.330000,\n"
                + "    \"0xcaf9a2dc93306ec6f7d604f4c58410fde27f8af6\": 1020.700000,\n"
                + "    \"0x5bbacfdf642c93808c8d6549ab8d5e52101eca4c\": 679.210000,\n"
                + "    \"0x0bbc94813e89f775bb27171991d83a263d8b8982\": 754.190000,\n"
                + "    \"0xd5b1805865b189bcea5eb701f0d0d1ed101db9bd\": 442.910000,\n"
                + "    \"0xdf3ae88da44d118389ae5009512b99a3ac08ae33\": 604.720000,\n"
                + "    \"0x0011a4becd7d3e1fa61e733405f4ec004d8eaf76\": 624.030000,\n"
                + "    \"0x72d0c35bb7ab571ca4cae3be9b65dd35afa7e5d1\": 378.590000,\n"
                + "    \"0x224d8e6e2308babd926e32d9667f5835928bda07\": 918.020000,\n"
                + "    \"0x970346c916698dc36ef2ff7a8b0a10f66d60afd9\": 13996.340000,\n"
                + "    \"0xc7e3ee089795c165ca8a1270f6744f91a309355b\": 419.180000,\n"
                + "    \"0x8448c4ef394212df2e438bfcde18223c9e86463a\": 588.660000,\n"
                + "    \"0xd59a66c579d4861810a05da88f2bc48613f92083\": 130.320000,\n"
                + "    \"0xdb57ba0145c999aed2de82b04eceed107874c33e\": 847.320000,\n"
                + "    \"0x00aa1aaa36dcb6a6b468f807ffce36c3dda25437\": 154.590000,\n"
                + "    \"0x8c05958d8efef4eef729acc22f312aadbd1e8142\": 885.990000,\n"
                + "    \"0x86e4344924057feb70f8366b0cda796ebc67f697\": 130.240000,\n"
                + "    \"0xec0c487bcad7c1e4c3205e15473159afae5430d5\": 484.000000,\n"
                + "    \"0x92b0975cb0aa5fd8bcc9cea478639ddce5acb31f\": 965.720000,\n"
                + "    \"0x87b1f9633788c116858b17dad3b1941d489b28bd\": 2695.170000,\n"
                + "    \"0x983f56ae49feaf0e4c15b476a0b0897e472b0d32\": 948.640000,\n"
                + "    \"0x4310fd34ed5a5ca2a2c9159638477dfadef2934e\": 207.980000,\n"
                + "    \"0xe5f6d4bd1ff233668feb9fb65d15052167293312\": 347.730000,\n"
                + "    \"0x9eac33b7895ab43b6fad6281d77fff556cf334fc\": 207.220000,\n"
                + "    \"0xd23c08b7712a26e4339fdea6aa44afb2eca3cb28\": 338.370000,\n"
                + "    \"0xd385e2b76695fd84ef58398e7dd3236c02fca300\": 779.260000,\n"
                + "    \"0x7fb10c0734d5cbc3b008a578d3c643ff5ca5221d\": 377.780000,\n"
                + "    \"0x44f859f164e48ebe11ac4af7391eecda33392844\": 348.720000,\n"
                + "    \"0xdf9cbd7375e2d2b5133a228a8a72580497f3a2f4\": 2178.510000,\n"
                + "    \"0xe62cc8b369f637a27900450d52c7d9570e363172\": 537.060000,\n"
                + "    \"0x3136a79b1f024a148671094228846b84bb03a64e\": 150058.760000,\n"
                + "    \"0x8485971ae7984419edc62400d940bc46eee66ce2\": 0.060000,\n"
                + "    \"0xc8056ead7eb242926e4e1c156dc579541a590a0d\": 471.480000,\n"
                + "    \"0xdf9f166b3d3b0240bee5b27ca07271d36b4d31f4\": 983.540000,\n"
                + "    \"0x2b20c42a188a06658c3d29ddbcfa1d6c27dcd173\": 3846.570000,\n"
                + "    \"0x46a1b93ddfc0e0831a6d8f08559fa2497b08e847\": 945.110000,\n"
                + "    \"0x49cfa11703d3a83e67a8509fa1f9c8a84f6a7698\": 894.250000,\n"
                + "    \"0xbd66198351f29561471229a580d5e6551c420107\": 1987.770000,\n"
                + "    \"0xa14c899284f5eb31cf45c0d4ffcc2a7e2df212e6\": 604.370000,\n"
                + "    \"0xeadd66370f35dcd7c4fd169a577a8c2d17ab9b64\": 523.700000,\n"
                + "    \"0xc011bab2d4a36962ccca45dfbed50885d0da6544\": 684.370000,\n"
                + "    \"0x30d5e41982f0f29724f3bf7a8ab12c143d28da75\": 1563.960000,\n"
                + "    \"0x37d904166298b2378507474452ad727cdf2fbe99\": 794.120000,\n"
                + "    \"0x7363b8120d1feb09ce34edd2de61e44746766389\": 976.130000,\n"
                + "    \"0x1e1049b402dc45d7c70a8582e1f77a811784cf55\": 328.290000,\n"
                + "    \"0xb82d21950fabec756766ad37d271e0b7e037f800\": 0.000000,\n"
                + "    \"0x06fdbf2c33e67f92fba57039cadfafc3f7d597e5\": 379.470000,\n"
                + "    \"0x68b78921629e901ed3f0eb7fed1568472832126f\": 171.500000,\n"
                + "    \"0xc7d03da7ea7673181fd1a1b79e3e70b279002ac6\": 132.300000,\n"
                + "    \"0x193ef28a8fefa1ba19c548d32782a64a45b281e5\": 141.620000,\n"
                + "    \"0x67b4dadd275274759bb2bc24f22262f67330d561\": 4416.420000,\n"
                + "    \"0xa13451d4a5b3b6002c22ac0feb9ee2ab0e5600ef\": 809.620000,\n"
                + "    \"0xb9615c0803a6f5461c86387031c87d72a0ba64ca\": 302.830000,\n"
                + "    \"0xc703309753fdfba7f4c268ed4dc0147fe14cd456\": 679.390000,\n"
                + "    \"0x92f789c64f0cf064a3ea1e99737c1ca0015dac65\": 245.060000,\n"
                + "    \"0xe721907006f5675db82a283198267451fbe78bb1\": 2116.370000,\n"
                + "    \"0x80bb729d22b8690c9080601a7174158aa17238f2\": 208.950000,\n"
                + "    \"0xe09085bc0c77da087990d9a281bc9e4831738a5b\": 461.640000,\n"
                + "    \"0xbddbaa818fd813c563890213aa0f08c5a7473fde\": 338.520000,\n"
                + "    \"0x4349549a702d8574cecfe16fa317fed424cc8efd\": 1816.540000,\n"
                + "    \"0xf69bb59c2e0d841388c770a80de9359e78a5e2d3\": 601.790000,\n"
                + "    \"0x98f674b378342d211e27fd0bd19dcd3d55481ba3\": 710.460000,\n"
                + "    \"0x4e551c4e3f69a3f2dd342f6719939edafe385efd\": 699.320000,\n"
                + "    \"0x010622794fed49c7967aa45f4b6f27cc1cd82eba\": 0.000000,\n"
                + "    \"0x0f8a70ae727844c77e790ffb04de2da91001287c\": 819.960000,\n"
                + "    \"0x15c4c9d7d1be21c34f00389bb174525969121545\": 1581.510000,\n"
                + "    \"0x64dedb5fc4a343f1f265fb2cc882e46740e0e034\": 255.780000,\n"
                + "    \"0x3595edb48025374d84d8b8e6caefeaf6304f2f05\": 243.440000,\n"
                + "    \"0x60ff08f14033a9e2b82efd03fcefc7d973096550\": 1615.220000,\n"
                + "    \"0x19ddfce666ae5b92a124497ba76e04bd3756b002\": 440.030000,\n"
                + "    \"0xab8101f232a79ea9a7a303c6b2c10b5f07e58e44\": 838.950000,\n"
                + "    \"0x39a9e859f6e85a5f12a52e127db5142aeb0c7928\": 285.040000,\n"
                + "    \"0x4b56553909237fa006e7a7854942055c55cd2a5c\": 588.890000,\n"
                + "    \"0x2d8cd51ba29c32b6a99d6247a6c33ced3e5d8af6\": 252.330000,\n"
                + "    \"0xb80008329779d94354e0b1b1390aa7e8ed1d80be\": 702.020000,\n"
                + "    \"0xef5c9809017f88f67600a69402daae67a8b352c3\": 1359.050000,\n"
                + "    \"0x8a62be35c4d0282721edee75dd55f2bbe18c6d13\": 867.610000,\n"
                + "    \"0x3843c98ab5a666442ecc79eb69321769c454efc7\": 337.730000,\n"
                + "    \"0x46225e993b14a598aa4d4f9222e9a59cedbe493a\": 985.370000,\n"
                + "    \"0xc3c806c447081f1fb6e73fdf763d0cc49689d834\": 3002.210000,\n"
                + "    \"0xeaf553bb7b35915390cc795947a5258470780117\": 375.230000,\n"
                + "    \"0xa7176da8106344f4ba1c42668d8936cf41d0ed18\": 274.640000,\n"
                + "    \"0x69a0879f61181254877188c2139ce0dfff60b831\": 226.120000,\n"
                + "    \"0x8a4caefb0788e24563dd2aa4d2097afd988fac17\": 215.380000,\n"
                + "    \"0xe982338efa263188f23c33ae0724d478a72495e2\": 238.180000,\n"
                + "    \"0x698d21472bff7a23a676cdc3da185ae9b3e94772\": 1791.610000,\n"
                + "    \"0x1373b1fd9b555db63c518f7de568cf7581792f65\": 2083.110000,\n"
                + "    \"0x40d3e3709391625bd6b3a5e54ef5c24846a0b96c\": 648.440000,\n"
                + "    \"0xd282c691fac5c136729aef61229f7aa2bbdfc734\": 329.940000,\n"
                + "    \"0x18903144e413875b15f72647208ee80b481bcb18\": 1521.000000,\n"
                + "    \"0x4b4ba000ddaf39ace4caa1ab000b34d97a72d656\": 317.300000,\n"
                + "    \"0xb354297dbb26c0c67495f955b3831f157edb8b47\": 1400.000000,\n"
                + "    \"0x6c747af3e284d5afded08ea2db2a6ac1e40123db\": 565.840000,\n"
                + "    \"0x509d7f1f849fa5f750611d2dd7f31fa3b6150df8\": 954.090000,\n"
                + "    \"0x1224b9b742acb702dae2b264fe7bce1d1b04dfe5\": 297.650000,\n"
                + "    \"0x3c0aec3e16e3f689f5d6cb8b8a56137b9857338d\": 647.160000,\n"
                + "    \"0x79b0fccaf1a15604e05e47bc7e9f720ff90df352\": 269.030000,\n"
                + "    \"0x3b361fbaaf0f540516d1ef053c6952df3bb3eb38\": 612.970000,\n"
                + "    \"0x80ac2c455a2f4d7e34c99c13e496ff649fd5e601\": 653.260000,\n"
                + "    \"0x5260bbae392d531097b63e12754c0a5e45b4266a\": 471.350000,\n"
                + "    \"0x0c75abe0103a5a10c3603dcf0ab1a4625be5cf6d\": 663.370000,\n"
                + "    \"0xbdd138d7915303e99e001cfc09ea908522ef091b\": 997.760000,\n"
                + "    \"0x4df04ce5ff984d7745a97be8f1440cf49354ea90\": 340.530000,\n"
                + "    \"0xcf02f6260adee33008a8cdf424e0504ad10379b8\": 164.140000,\n"
                + "    \"0x58ad6c64003a3e0394e38dba0e5b67bf0f714fcc\": 476.430000,\n"
                + "    \"0x591737de7d53a03602cbd40eef786379f15bc15b\": 1199.810000,\n"
                + "    \"0x0e52c3c531f32c96f29bc901f6824fd045228082\": 531.720000,\n"
                + "    \"0xa218b97d2b4b0d26dadc3bc27396833185b78446\": 528.860000,\n"
                + "    \"0xf781a153c57770c318a3de23d37a43b0d655c273\": 693.260000,\n"
                + "    \"0xacb17321a248abb2a19203b195aa2c8600a3f3c8\": 1120.280000,\n"
                + "    \"0x907e6fb5a5b4731a4d17a1236161921cd655cbac\": 0.000000,\n"
                + "    \"0x827287fee7ff114659127ee08ef7fd752d98146d\": 696.790000,\n"
                + "    \"0xce7803af87dfa4588a21c6cdcf69ff2125c9f546\": 675.510000,\n"
                + "    \"0x1e22043fcc02f1fee965b51734c5cdeca270d522\": 727.700000,\n"
                + "    \"0x228a0979dde49501bdb2def6a8fc707f2968896a\": 30579.540000,\n"
                + "    \"0xd43fcb1711f7a54d8b8bc096866f20c7905376ff\": 4542.360000,\n"
                + "    \"0x1a1a6a84e64e9895f8e55f6dadae8c2b05cf30f9\": 1020.390000,\n"
                + "    \"0x7fb9b873f19c5ed62e5c1819478b09f1b09495c4\": 0.000000,\n"
                + "    \"0x9c113f1f05930fdd661fe8a639249c9eb57b1e20\": 840.980000,\n"
                + "    \"0xaca7d2781d12300151ee3fe27e0e98e8e11b71d1\": 978.610000,\n"
                + "    \"0xc453408eed99011199e10ac15ab342be277535b2\": 849.300000,\n"
                + "    \"0x8368b76ca0585e6a045ad99d57e887415de147c8\": 829.940000,\n"
                + "    \"0x16a5e238ddc04df3e8ff502ce1ba7525dfa43965\": 471.200000,\n"
                + "    \"0xc96e5cf07f01a39be478d79896d7fdb9e8c9359f\": 253.730000,\n"
                + "    \"0xb6ec23aba2f15914e8fdcbede6aefd15e6cc857d\": 650.020000,\n"
                + "    \"0xd6a68abbce6687bcec8e4b0b6bb65bc51ce3427c\": 1063.960000,\n"
                + "    \"0x9a58aa530bfd54c4878bcf730362629cebcde804\": 245.660000,\n"
                + "    \"0x563db01790bbf3f2284b08c417c194b0548a8f85\": 725.780000,\n"
                + "    \"0xb200ab22d05c8bf78649a7a1e669276c5ee77da2\": 221.630000,\n"
                + "    \"0x16fe7ddad6238a188d9dfdc48dc4f54a889b29a4\": 914.290000,\n"
                + "    \"0xf7768527bc2054e624d0e13cda7ec96567efb7de\": 2547121.560000,\n"
                + "    \"0xe8726a2afa398593b8a37a2ff5e4038eb114a0cb\": 383.510000,\n"
                + "    \"0xf176d57a8634a06c05cc2655f1b04cea71247e20\": 563.990000,\n"
                + "    \"0x97c78ed7b75b72015748187f23789f2156d0d701\": 44060.380000,\n"
                + "    \"0x387903c7deaff7c233edc9fff9f6776fc7863054\": 1043.160000,\n"
                + "    \"0x8b5f441f97fbca80144a8fd58a91c312416b8ed7\": 990.230000,\n"
                + "    \"0x7757839aea95a0cf2c10ab87bbac56af87c33d21\": 505.400000,\n"
                + "    \"0xf46deec4cb987e733eaff20e4b42983e6468db40\": 468.380000,\n"
                + "    \"0x2e31ac3a38979c371459871ff180361553f29090\": 236.320000,\n"
                + "    \"0xd8293f3890848da5d61adffe023dff099cf0f6b9\": 0.000000,\n"
                + "    \"0xbb8b10b360f94732880a55da3fd612dadfeefdfa\": 1635.310000,\n"
                + "    \"0x94a63f16a3892328242940d8ec8ac2a08ff2ee55\": 633.720000,\n"
                + "    \"0x10fff14430a502c56746c6221c394a712500d324\": 683.550000,\n"
                + "    \"0x3d956679d146a998c05f317898f9cedf58220075\": 412.750000,\n"
                + "    \"0x46d36670b85b432b9d068d15cdec2fae5526c121\": 0.010000,\n"
                + "    \"0x814ad4b3c833d1cec42a259ddcd9804acddbfa3c\": 411.960000,\n"
                + "    \"0xf0c42d0e7fcb8b26c87b85bcbd40e3450be772cd\": 873.120000,\n"
                + "    \"0x167053efe514408f61e44aa3807a806f611aac68\": 772.670000,\n"
                + "    \"0xdabf80a2477bb847fb3b9b5e0ebaa07570d33cfc\": 463.320000,\n"
                + "    \"0x2c2b8ff342ebfad3b88acb831c89aec0e217e183\": 204548737.840000,\n"
                + "    \"0x5b2953f859b6eac15b5d571a38bf03b266ccedfb\": 279.800000,\n"
                + "    \"0x5a21266770f6871af7a47c8b7ce421fc8e2c55ca\": 173.880000,\n"
                + "    \"0xcc56f7a8020745b5d720466e4ad4b56f75e01796\": 828.690000,\n"
                + "    \"0xa44e4f77642e7d69c4dd68e67f4c08611f92c3ee\": 516.070000,\n"
                + "    \"0x4b3df436b9c29f8abb7e603c34fdadae0bdfb39b\": 480.170000,\n"
                + "    \"0x3206faf1574ccce411c8049b2ff64355a7b92fc7\": 1599.230000,\n"
                + "    \"0x108f534a7b97e90eedc1e309a5e0df0b69457ebc\": 636.670000,\n"
                + "    \"0x1f05fa35956f913a8bff34a1667582596500a540\": 1987.930000,\n"
                + "    \"0xb1e93da3dc3f1590cb97f0bf9ec5e533f7ba841d\": 143.080000,\n"
                + "    \"0xad6ef7977e11ea6fbc55acb9015ff8b4d57fad64\": 691.940000,\n"
                + "    \"0x9d6e079e4997004ef384dd10282b52acdb0ac969\": 994.790000,\n"
                + "    \"0xfbaf859735c5579a15e4146d5a5ac0839458bdba\": 1310.950000,\n"
                + "    \"0xe6bbbae942460ac32eace4279bdd9c34237cced5\": 885.100000,\n"
                + "    \"0xfca65d76a7f36bbbae253acf755fe5d916810aa0\": 1938.230000,\n"
                + "    \"0x0dd947ff1f786ca8e29e594540cdcb5fcc0b5523\": 361.290000,\n"
                + "    \"0xe0a90e2b6fb64d171a0cd4b3d4fae074c7cff294\": 482.580000,\n"
                + "    \"0xf009d454995d0652fadb2372f6cf461d10d290ea\": 194.290000,\n"
                + "    \"0x74161ce61c42d28530839eeb32cdd1915ca06805\": 0.000000,\n"
                + "    \"0x08ebc41ebf0da467795d6267b8d3b7a549e060c8\": 303.250000,\n"
                + "    \"0x4940fe6368a625ccddee86fcd627c0f07b5866f3\": 1957.710000,\n"
                + "    \"0x2dc5577e7bbda0c52cfe0fea5ab11541482acf6b\": 745.800000,\n"
                + "    \"0x633fcf3904a12c4623a777c9a3668ce78ba9ca9d\": 170.030000,\n"
                + "    \"0x7917eed8040cb2988d6598d92c2ab6a31676b2d5\": 393.410000,\n"
                + "    \"0x6bf8e830f7c9c2242d7621f2bfd5cf3025ba716c\": 500.120000,\n"
                + "    \"0x198842732b8aacf8b953e740f05a9ef7c7ddbf92\": 450.590000,\n"
                + "    \"0xbce1519a0e9c73d2b7b4028880a6bed63bf7c1a4\": 555.500000,\n"
                + "    \"0x019aa0d14253aeec3e4af4437aa4a85d6d342289\": 913.470000,\n"
                + "    \"0xff387551845752e52df809e624462139e54a2587\": 1387.870000,\n"
                + "    \"0x862e52e628fea0b05505defa3031fbd44d7cdd2f\": 343.710000,\n"
                + "    \"0x1f1313383e02993f5bbaa238426190ffebe54b0a\": 389.590000,\n"
                + "    \"0x5de7750788a0dfb104ab3d3e0637335ec9ea77b6\": 298.910000,\n"
                + "    \"0xceceb7ea5a650bb3ed753ca6bdd5d7cb0fa18bdd\": 0.220000,\n"
                + "    \"0x42187ef2b14a0bd460f9d52a5ab0a41fb3e84769\": 262.890000,\n"
                + "    \"0xbe001b7ebeb48176db5d56dcd96d419fb847a0f6\": 349.680000,\n"
                + "    \"0xae339b31585810370942037e8ee939d09bca790d\": 172.230000,\n"
                + "    \"0x49fc69d7b648889af79cfe340a18eeab96ecfe87\": 706.280000,\n"
                + "    \"0x06a078787f71c34515a2c71a41ad6f749dca2dec\": 740.470000,\n"
                + "    \"0x200de2c295ddfb7eadc163f6089a8f3e15136df1\": 351.080000,\n"
                + "    \"0x27bb108d47c4a1ac6ef1e86a0ce2fa4b8f69a0b1\": 673.090000,\n"
                + "    \"0x5c457d03323a0eb2a56d6772ae4da2388aca7afe\": 1936.900000,\n"
                + "    \"0x74de5d4fcbf63e00296fd95d33236b9794016631\": 0.000000,\n"
                + "    \"0xf854ca6dda32c9911029b8efec89e3524a50f04f\": 1504.310000,\n"
                + "    \"0x121b2f4a6ebf9e7e2c8e3b3551b104b7d3940ec1\": 525.370000,\n"
                + "    \"0x66e74b437faa63947f88d40c3d1d3b9b919b9320\": 380.510000,\n"
                + "    \"0x6455e0504fb4688cba07a4c3498989b138c5ccde\": 5238.050000,\n"
                + "    \"0xfb2833701a275336f0ca7ec42966eb22e63f84b7\": 1986.190000,\n"
                + "    \"0x7c1f9d671d3046d0e6bda09b352a3175d368d90f\": 504.190000,\n"
                + "    \"0x5acac955aaba6e5546ee99763f0195c9eb79292a\": 521.860000,\n"
                + "    \"0xc22d06cb7b2a0b9a72069a9190e224cd4ddd654f\": 318.640000,\n"
                + "    \"0x5b409cc9757d9fafaa926b9a320ef5e5988f1cb9\": 679.160000,\n"
                + "    \"0x2e2754f4ba87115337143685eebefe85eada984a\": 809.490000,\n"
                + "    \"0x04970bbf848e90402fc7db10ec25dde9ad4b35d1\": 559.010000,\n"
                + "    \"0x390db783122bd6db2ee82bdc8fc3986c6f300908\": 358.680000,\n"
                + "    \"0x8e100f9a031bab1ea7f7499870a9ea6901737596\": 2250.550000,\n"
                + "    \"0x02b7b9b20352a351c82313e0d806f637b00f10e3\": 499.840000,\n"
                + "    \"0xdb6082be803bdd9756211a0f94c10ca3fa72c398\": 208.960000,\n"
                + "    \"0x55d4266773d008b537a3ce0fefd589a460437a71\": 344.220000,\n"
                + "    \"0xf72893b4fe8d5121ae10cf524e9b8a7eec649f14\": 4348.870000,\n"
                + "    \"0xac58ddb1a2f756cd259d67144f46873cd7644c03\": 290.260000,\n"
                + "    \"0xa2433a9de3cfc770a347119007a0e55296ecd792\": 0.000000,\n"
                + "    \"0x9bc257a015b563252cd6021bf8d7998832b0ce5f\": 461.840000,\n"
                + "    \"0x24b46de8f8919f10a1dd677dcb3937d7a45d64ea\": 197.660000,\n"
                + "    \"0x01baaf076eece9ad12b1e7577a5276ebe1247780\": 4500.490000,\n"
                + "    \"0xe5ef79b256984655169284f0fcd834fbad8aaf25\": 0.000000,\n"
                + "    \"0x833aa15eb7f4b60724b01af98fd6eb27273ca571\": 155.700000,\n"
                + "    \"0x6b673e3fd3838ca294a163ab3fc9ff867ce31a49\": 810.640000,\n"
                + "    \"0x04dff4a52db2fd17ee5481920e0c39f1110018cf\": 591.420000,\n"
                + "    \"0xc75fe85e98587ecdd8c5f815ef6a4832155af10d\": 410.230000,\n"
                + "    \"0x4fbadf790ea00a8d39ac0b88407cea8c9e844058\": 634.840000,\n"
                + "    \"0xd1a65ffa2bada7a3e2ca33b5c7affa510bdf6552\": 778.730000,\n"
                + "    \"0x07c26e0e9f3b7df22aee6d7482b3b277ded0fb6a\": 1536.100000,\n"
                + "    \"0xb92b568c49ef7a5d2d70833e2abaf92394367e90\": 870.180000,\n"
                + "    \"0x1de05813d793dd8a99da11c55a33a72c3472953c\": 959.010000,\n"
                + "    \"0xdc686d8beb7c71c119791d71e2e6dcc9d26d42eb\": 605.400000,\n"
                + "    \"0xa20abcbc888b35ec2321050adb0b8261e8e72f9a\": 472.780000,\n"
                + "    \"0x1ba11542111f5ec26224d8267886c24049fc47a4\": 979.310000,\n"
                + "    \"0x2fb583ac17cc349e878882e14502b7213df99e64\": 782.410000,\n"
                + "    \"0xd445af41b55fd9eaf2941a6e4a011d5dc524453b\": 0.040000,\n"
                + "    \"0xb57d27c1ce69e2c7d3b1c206cfe3824024b37b37\": 3163.330000,\n"
                + "    \"0x0cfe036d4d496912b1d07ca91119363e714e4b9a\": -0.010000,\n"
                + "    \"0x55008f861642734ec42ab551a91ddeff1d8aa3af\": 0.110000,\n"
                + "    \"0x9ba68e70924a2fe3a7039301a470a17d4e302628\": 672.530000,\n"
                + "    \"0x37eba7a462cef9ac4fbb78e900cafed87b07871d\": 551.060000,\n"
                + "    \"0x2844477b40d1437ff2e8520298bd51337892e395\": 768.610000,\n"
                + "    \"0xaa3d7981948d2d2df41435e5229da4b5fc0d6246\": 912.370000,\n"
                + "    \"0x3ac8030d1e3a533d8640c3964e554d2844fdee93\": 322.810000,\n"
                + "    \"0xec39f26bf3ffd3454f776401364980fe2bd754b0\": 387.390000,\n"
                + "    \"0xdf62b17d361a6c0f50aafc96b971ba977197fc01\": 420.590000,\n"
                + "    \"0xbbf346beec2950de3c7f9ef8d5791569414fdddd\": 336.000000,\n"
                + "    \"0x2de7e849659ded31ba2793fedd34ee0c9e5c37bf\": 2198.850000,\n"
                + "    \"0x20b0debdc54690454dda0e9a06bfb5e1b629c22f\": 2818.980000,\n"
                + "    \"0xfd9f3900f9d18e428313945ad6b73b0dc5723503\": 134.220000,\n"
                + "    \"0x0370b490ec7441030d22bd210ff4c82ef0b474f5\": 631.100000,\n"
                + "    \"0xaf99ad30a23f45a53b65a12249167b01e260477d\": 569.330000,\n"
                + "    \"0xd6cb6e9267bac7c9e12de64bf91fb96e77baef72\": 1125.530000,\n"
                + "    \"0xf31b47e3dca0636493f93ce16eee02d68a73f1ff\": 662.580000,\n"
                + "    \"0xa0c08333368b63a05446c3da4ec062b5a8373756\": 583.560000,\n"
                + "    \"0x7310a76e9a6580443500648d320fb1b8fecf8930\": 691.130000,\n"
                + "    \"0x13aa872dd9ed9a8f0fdcc8e885288edd897cef53\": 1359.630000,\n"
                + "    \"0x33e8b8754545a50606c7241bde6f529f54a60a6b\": 744.450000,\n"
                + "    \"0x6cdd8488d2a119fa23c71acb043ab0a91e797502\": 855.420000,\n"
                + "    \"0xf3edb4b9afc583bd4795582009b24a9d8d2f7f45\": 570.250000,\n"
                + "    \"0xf9657a2e1b27aa307cd139af054fa25bbd8f9fbf\": 1087.580000,\n"
                + "    \"0x259c346b54ab23b4683ab6b0cae6224475b24415\": 0.000000,\n"
                + "    \"0x18992d5240163651138407cdee96ff1bbfacb249\": 198.200000,\n"
                + "    \"0xfe2f52f91c11fadb31ab0fe0c36d5c440c79bb5b\": 586.690000,\n"
                + "    \"0xc8a6fe79a6153703b57154798b37671c5e3ee0fd\": 917.480000,\n"
                + "    \"0x08cae4a5c73468fc7943b704461319cf83989777\": 807.090000,\n"
                + "    \"0xcfe8d4e4b6af63539ceb19dcf47cc728191005c3\": 361.460000,\n"
                + "    \"0x8fa146deb5152ad8955a9bb3570723f65f4e9191\": 216.000000,\n"
                + "    \"0xa00dc513fc16ae6fc1cb4c6707b1429b4f6a9c7e\": 2139.480000,\n"
                + "    \"0x7a920fe9f639d538610b8ebc05a1ecbdaeb775af\": 648.900000,\n"
                + "    \"0xcfedab451c83d874d0accb52f5ca2ced391bd776\": 1308.500000,\n"
                + "    \"0xf175d17859014571bd6ba9e5ef74810b46e78b2c\": 424.820000,\n"
                + "    \"0x4383ce5381f74945af62439ecdfe5a7cd12744d7\": 464.180000,\n"
                + "    \"0x1d944464ca548d77083130a0f5bb33c7b1214d37\": 807.050000,\n"
                + "    \"0xe427bd77308f7780c6c98a5a9b2d7ff3b17de0ed\": 127.020000,\n"
                + "    \"0x9eabe4bd7e524033662d3ce8c652ae02da00090e\": 331.730000,\n"
                + "    \"0xcd8a418e603ce53f6b6335488e9a65aeec7c901b\": 443.500000,\n"
                + "    \"0x6097a1e28e2c9eb7223d67991b09f242812fc391\": 303.300000,\n"
                + "    \"0x46a1d619e403a089ba1cd756a3445a6f5749b37f\": 254.940000,\n"
                + "    \"0xc5f515a6b6afd9d98788f81be5d0957884f53086\": 159.950000,\n"
                + "    \"0xd77f95831e156cf26fcbe0325f80626f8d955316\": 188.040000,\n"
                + "    \"0xdd005d01103689c4e4c3a900a8c4240f94aeb015\": 285.650000,\n"
                + "    \"0x63f8f0ef573b8206e1ae971030a9672fc674ec86\": 772.790000,\n"
                + "    \"0x7a85b56c57a6662f5cdc35bdc65a522b2a169cb6\": 2095.260000,\n"
                + "    \"0xd172726a1b7a0634c17e1ea78462c6ddba263e93\": 395.070000,\n"
                + "    \"0x1d1155e11145ac286e2ef4a565142d7936f85182\": 425.590000,\n"
                + "    \"0x8ee78a86b0c9f6740ea63b43f2f1ffa9c30541ea\": 3787.380000,\n"
                + "    \"0xaf2358e98683265cbd3a48509123d390ddf54534\": 355834.550000,\n"
                + "    \"0x83901b0c167cb384c6ec78a556b58369f59d153b\": 670.810000,\n"
                + "    \"0x3d790c61f32aa920b036ffa73fbdaaa5bd08852c\": 154.400000,\n"
                + "    \"0x360c38c503e2ca869e8a6883bcedf5774105794f\": 936.040000,\n"
                + "    \"0x9ca69ee3d8f97373e4864cc6374bfe444198167e\": 380.750000,\n"
                + "    \"0x4ceef5b104e11394791463907636948d44f04214\": 411.070000,\n"
                + "    \"0x17ab96682aaff595e27eb3c233103616de9d5ab5\": 783.540000,\n"
                + "    \"0x1ff61e8f75ad7b040a1f6e67314ac399940757d0\": 392.150000,\n"
                + "    \"0x867d4205a3724a29402609327f4a1e9c98cf7e36\": 288.190000,\n"
                + "    \"0xba4b3df89e6d03eba0542d08eea3948b81525ba2\": 703.520000,\n"
                + "    \"0x6e4ea0d83e7f59db116967f7e0115d9153da7131\": 448.680000,\n"
                + "    \"0x21f9d8c4b06059adf50d68a4b5e9fde852eff767\": 823.140000,\n"
                + "    \"0x81c5f84a85cc0649df0dd2e95080bbf1790b9499\": 772.460000,\n"
                + "    \"0x0077269639771d19372b9f4ade5230e72ccd01e7\": 0.000000,\n"
                + "    \"0x40e0cd6f849ceda200bd1bb6d4ef575fd108bf90\": 978.040000,\n"
                + "    \"0xaac3d1094dd33b908224e2cf6a773a6ebbe22311\": 1419.530000,\n"
                + "    \"0xe589c2a9f50172523555f2ff5fc7729108703bd1\": 383.210000,\n"
                + "    \"0x5fbe86be0ee2fc458eefb823da18da51fc03e873\": 236.750000,\n"
                + "    \"0x77987543c252675919e1046815d2d0f182966bb3\": 1822.990000,\n"
                + "    \"0x4c93153ce4dd844850ab9305d08d583f580a407e\": 664.870000,\n"
                + "    \"0x8cf27303a0496e45b6dd7c5bdc9e2ea3aa1dee1c\": 309.600000,\n"
                + "    \"0xd4650b297aea8761c22db09da3c98e95daa0f4be\": 771.410000,\n"
                + "    \"0x8ead6c0fc4f974ce2105ab10eef317d309e8d681\": 752.000000,\n"
                + "    \"0x4c5dabc1fbf13699078255eed93931fe4307a7d1\": 1256.430000,\n"
                + "    \"0x0d9d086e61484ef2ed6bf78fc3ecb4595351b86b\": 520.600000,\n"
                + "    \"0xf76bd5c40833ec2277d23f7333c40a8efc09e625\": 347.180000,\n"
                + "    \"0xde1e9fa72f2c07f5a7c928e90a1c902cfdd1d996\": 628.330000,\n"
                + "    \"0x6e21fbe3ba70340ff6d8960a96c9ba8022771fcd\": 397.220000,\n"
                + "    \"0x9101a3091254fcf5a54297ff6b9b9ac9b35c86df\": 406.970000,\n"
                + "    \"0xac2eeeb17e10c50805dfe3d7b3ecdb1fb147c8a7\": 827.630000,\n"
                + "    \"0x357b9de1c1720b048007dffe1e6d848c4860e063\": 696.100000,\n"
                + "    \"0xe03c11b6bddce8feaffc6aca7e208a2fec1627cb\": 705.630000,\n"
                + "    \"0xd0a120f8166969415871f908f1ce5350ac5f0794\": 620.320000,\n"
                + "    \"0xedf0eb2e078c70806feb569b409b28ff01c5b808\": 1246.380000,\n"
                + "    \"0xfe0408f49f558bb5e4e436dbe347ff1d9abbf667\": 407.980000,\n"
                + "    \"0x409ff062320be889f388ebabb1138ebcc1c7ce7e\": 275.490000,\n"
                + "    \"0xfedc105fcfa841ba90a0b2d399d19cbd02a52049\": 325.740000,\n"
                + "    \"0x9d58b48f4b46b1ce2aa1415cd6e02d82aedccaec\": 976.130000,\n"
                + "    \"0xcc928c060ae33667b5e797be91e8d91edfce9979\": 521.750000,\n"
                + "    \"0xd4315668aa1d88b4c581ec6fa902e131286dd0ab\": 0.000000,\n"
                + "    \"0xe0bc9bdd54dcc7b30b197737e82333fdd2d00c93\": 795.720000,\n"
                + "    \"0x4ce6e2ae4d656f09a628c0c3717ec8ab1193b771\": 990.700000,\n"
                + "    \"0x115da411c0692cb026a3c7b42e50cd215684e53d\": 191.950000,\n"
                + "    \"0x0bc627f748a0a94a50606ca4b6a5301d0055d058\": 44659.700000,\n"
                + "    \"0xe8e13f2d19bcd166ce707f96c16d493cb52b4431\": 139.140000,\n"
                + "    \"0x6a066b220322daf7e6ce97bfdaa2ef05b06d2b8e\": 2399.750000,\n"
                + "    \"0x84c6331351663d08717207af1e009d04707938d2\": 392.840000,\n"
                + "    \"0x2ff3f7aae29253b89cff2df54ece83409280825f\": 802.850000,\n"
                + "    \"0x6533eda0ecdd5ef1739f83883d67449545e5aaeb\": 857.800000,\n"
                + "    \"0x07a2fcff659653226828eb1645a43af1f6dda8f2\": 582.340000,\n"
                + "    \"0xc79eae438560cf9e4e546c3e528b7f17308e55f8\": 451.000000,\n"
                + "    \"0x74f8a467c6435ee8955964a988dfbb859a1914c7\": 0.000000,\n"
                + "    \"0x907c17bbccd57878b26a93cb394ded6927a1f076\": 671.380000,\n"
                + "    \"0x471ae5daf866b767c57357b71b943edd89de3026\": 175.790000,\n"
                + "    \"0x0823cd2dc5c8180a6147a0b60cd94d79520cdedc\": 3634.900000,\n"
                + "    \"0xaebdbace8a5f170a1c4bbfd6d66a4163d954c5af\": 176.870000,\n"
                + "    \"0xabea4c0b56469327f750cd805dac1825d6fce9d8\": 163.760000,\n"
                + "    \"0x683a786c1ab1f6063d01f87c59b0ae9e5d886188\": 507.390000,\n"
                + "    \"0x7b66073e8f25c7c8fbabeea5632d072920483e35\": 110557.190000,\n"
                + "    \"0x8121d734cc263bd445fae02793e100fad4d4d4dd\": 1507959.020000,\n"
                + "    \"0x3f3765920c1c12c8f5a683e3abc3603f8eeb0e23\": 686.170000,\n"
                + "    \"0x37023bb5196c400ca0b9ce9f8a0f728c086a1af8\": 573.060000,\n"
                + "    \"0xe59469ee36d9e2cd085f18271d7f495029ed3b88\": 778.060000,\n"
                + "    \"0xbbd9a2444589b111c8647accd48e0d1812277402\": 73199.440000,\n"
                + "    \"0x1c0a5ad5759c4edd4c9117048b6192b5d1b7ad81\": 582.260000,\n"
                + "    \"0x69bd88cc10be3b2996368dbc5966448052b38050\": 374.070000,\n"
                + "    \"0x44a3652fc2295c0111c57d4f2b8760f97a754481\": 896.460000,\n"
                + "    \"0x3f1cbb647566e7709371420505411a0c71173de1\": 901.410000,\n"
                + "    \"0xb12acbe14ea5e15c125ef179ad5fc01d18eed5bd\": 833.820000,\n"
                + "    \"0x2e09e3b96f76b16eee62ffd56e4be87476046527\": 2236.640000,\n"
                + "    \"0x64139e8fd9f3e15aac302ea6ac414d8aeff8a9a5\": 1062.250000,\n"
                + "    \"0xd7c04f39369f727030a34bf31b74beb1f2f9017a\": 192.480000,\n"
                + "    \"0x7be1083d2afcf9cc442ef3f4e0ef67fae6979ac6\": 503.460000,\n"
                + "    \"0x70829c0872ef14bab0a7d18a212719d9c5170917\": 982.540000,\n"
                + "    \"0x840220b2c7c629d9d57628aab9830e42fc6fe802\": 480.520000,\n"
                + "    \"0xff7534174745fa8dcaec62ebafb0e5e8e465ac8e\": 922.880000,\n"
                + "    \"0x03788faeccd507a56548222ac032862e19e03381\": 286.270000,\n"
                + "    \"0x540b7cea59ffde62ecfe01bfda652ee11195bcc2\": 420.170000,\n"
                + "    \"0xef98998c4a0fb91683487f8b55c5f65733b28e79\": 1575.380000,\n"
                + "    \"0x0b9f1a36b7ad8532b6d9f171cf8fe47ae9d0d875\": 450.880000,\n"
                + "    \"0x7fd98a34ffff942ce8a0c4fb88bd8618e2eddd03\": 566.530000,\n"
                + "    \"0xb266d7bd015d7c7fbadd372d4e0eb83150ea17c9\": 357.130000,\n"
                + "    \"0xa4c1e9b46cbd65d7a395a8705859e385ffdc1553\": 1712.770000,\n"
                + "    \"0xbe7d0c946bb8a0d1061c7828ea60d0a086ed20b1\": 311.780000,\n"
                + "    \"0xfbefffd5a2568032eec33f28e9202901571291f1\": 474.940000,\n"
                + "    \"0x41fac31c6c17ebcd0d4aacedf832de0775403408\": 389.690000,\n"
                + "    \"0xf3c7c6727fd5f9af0054f6e5d75d58ac66699364\": 13439.400000,\n"
                + "    \"0xcb2bd0994bf9adc31fa68464821b40b042c6ab2c\": 586.210000,\n"
                + "    \"0x47d116421f01e3fd9912447357964430c0de1223\": 1446.240000,\n"
                + "    \"0x9cd7bbdf83ac8d05df62851d5053640217ba2c3f\": 954.250000,\n"
                + "    \"0x011f8c4467e218617bf9146fdf8319276e4f10a4\": 1411.600000,\n"
                + "    \"0x8dea8de96285457e535014e82ed6d3779f08b62e\": 639.530000,\n"
                + "    \"0xbd311b5a8542e3058df7ec391abaaf46e1469158\": 1590.280000,\n"
                + "    \"0x3faf07bd15ce7fb393eeb3b6922ea705051afa10\": 591.810000,\n"
                + "    \"0x89f06d83214667d84b989b0ea07ee2e41f546380\": 935.360000,\n"
                + "    \"0xdb0f1f9e23ed2dc87f652cc68f6f937f79badde1\": 725.950000,\n"
                + "    \"0x69a89d26605187cfc7fd32c153f253a78c1b97bd\": 643.100000,\n"
                + "    \"0x94250c62e1386fa8739e4fef2284d4909bc9051c\": 71692.920000,\n"
                + "    \"0xc4272e1c21a94ea0b5c5cb7880c285f501e53d77\": 348.790000,\n"
                + "    \"0xbafde1836a175c5e14ee44ca8af3bdfb6373a757\": 673.550000,\n"
                + "    \"0x7da45172b4cf905fe39a007b32ab8d5f957bb09b\": 405.260000,\n"
                + "    \"0x77ed839e2ab8767ca5be58e67402617e7418e809\": 605.810000,\n"
                + "    \"0x8a0a08efb30760a6541e5ae1f4da037026bbb1a3\": 630.510000,\n"
                + "    \"0xf7ca1c843f381b122bd775c89ef853fe089efc3e\": 149.300000,\n"
                + "    \"0x5d103c2f0f5e33a63e7ecf56b976ecec5a336e4b\": 380.940000,\n"
                + "    \"0x20013a451908781afda93d020213d9c3d25c2ba5\": 802.440000,\n"
                + "    \"0x46cc0f843e544bf21a3c48a0c5736ae6ca8a89b5\": 197.200000,\n"
                + "    \"0xda2e076b8f7b9841e4d7e9019c55dc8b38cdf454\": 4300.270000,\n"
                + "    \"0x4c2cf6492c63119f01de1b315f660abbd8cbd89a\": 762.920000,\n"
                + "    \"0xad513be5ad68c31407cddae95a138c1e377b2f06\": 751.390000,\n"
                + "    \"0xf365f33e18926785be5900849649d7e1fff7ca88\": 189.710000,\n"
                + "    \"0x9533654b23762031a35bb1efdd920086852fc498\": 892.720000,\n"
                + "    \"0xbec8ac0527ddc3a435df020c12358b72194c22f8\": 769.240000,\n"
                + "    \"0x4a8acd5d8e2fd7384f1d76c6e082b0f7f3937701\": 944.960000,\n"
                + "    \"0x67466c9345cd136cde104d29e530da58950b68cf\": 69204.180000,\n"
                + "    \"0xedb0a2cdee429ec1a3ca7bbf6dc16dcb774791bb\": 191.330000,\n"
                + "    \"0x4cede48861df47ca199ff6ef6c593a6635c9bea4\": 231.780000,\n"
                + "    \"0x35d029e0c54f3ca88095758b867d2917d44c36ed\": 98724.680000,\n"
                + "    \"0xda4a488301f8add0ce8fc1d16b4d4ff4b3cf7e6f\": 133.870000,\n"
                + "    \"0x79aeb03352cf8bc3f925d48a88b07330ee13575e\": 645.200000,\n"
                + "    \"0xba9456383ccb44deb175de4a51ae37e1f5774975\": 855.860000,\n"
                + "    \"0x6a13aaa47bf9a12e454d7f7ed5b7866746d1c933\": 523.410000,\n"
                + "    \"0xc789cdb6979a878dbf34b76ca2f26a9de015dd3e\": 813.040000,\n"
                + "    \"0x4344cbc921728f9179105420fbbed25ecb4e55b1\": 707.600000,\n"
                + "    \"0x0514a7c7d47b1e098aa14e2d941851cf3a0c11c4\": 403.630000,\n"
                + "    \"0x40308228572ec6b0a563ef8d7c040fc002422e6d\": 649.530000,\n"
                + "    \"0x2c7c7465827b28acdfd1895d30f212b1f3c785a5\": 879.530000,\n"
                + "    \"0xb9646771188ab66907fdc54727bd650d1fcc2976\": 325.100000,\n"
                + "    \"0x6c1f5bba29742ffd95040bba2594136f3f14c66a\": 3531.210000,\n"
                + "    \"0x981b40c1b84837b3ba15a8e1ef921570bb096a76\": 227.580000,\n"
                + "    \"0xb6b0788a2672907fcc1e6e896fe3c598ff2dd022\": 551.820000,\n"
                + "    \"0x485557c35886fa37ee6eeb579a9e944e874d5076\": 692.010000,\n"
                + "    \"0xc429ed1cdd703f532c22173e7ba9e249dcd6f793\": 150.840000,\n"
                + "    \"0xf73ccbbf3cc7e2624d2022a6b559aa8bde6e84a6\": 637.850000,\n"
                + "    \"0xfd02f95340f46d554efabc077494392f7fb0ba65\": 1368.910000,\n"
                + "    \"0xa4ed3a554ae8463323e59a91684e0e33c6de21d2\": 784.150000,\n"
                + "    \"0x52beb02b779d619f39c69a688030d2bf964961e4\": 957.170000,\n"
                + "    \"0xa4edad598f37964c913fa79f18b9ac056f48622e\": 1531.550000,\n"
                + "    \"0xc445c2a7aee2613d865cd53ab545f6bcc54a1fc9\": 0.230000,\n"
                + "    \"0xe8bcabb989e9d6929c22da5d6c9c8d93ee585363\": 223.030000,\n"
                + "    \"0x9eeed310930c1884205902ddb750337675698208\": 658.840000,\n"
                + "    \"0x6094e53f3ee66aa66916833c19b912341063c8b1\": 779.220000,\n"
                + "    \"0x93bfbcec793e22583ec343a15482a762e6d41e07\": 372.430000,\n"
                + "    \"0x0989d6e8698da34070cd3211fe2e12dd7187ee00\": 185.860000,\n"
                + "    \"0x518bb8b609074e1597c1a47706b8d8bad905480b\": 431.390000,\n"
                + "    \"0xff7d2eaa8a9e4e887dacadfdf063de1c95b6cc3c\": 4897.670000,\n"
                + "    \"0x2877c5f8b0c608b6e64488a7458d81ada2f5ff13\": 0.030000,\n"
                + "    \"0x0dddfd4674aed9fa5b8a013b9eae60b448e67829\": 986.630000,\n"
                + "    \"0x178124d325ce351d56a6d29c557f63498ca61d53\": 111694.660000,\n"
                + "    \"0x97cb6b12eb9a0cff042bf11b4d8f9b0d521b9bb8\": 800.760000,\n"
                + "    \"0x3a7fb7cfda20664f944a874b9edbdcd8024f3587\": 713.900000,\n"
                + "    \"0x106ed1cad7a0511afd7196ea639a675b93c08fee\": 505.790000,\n"
                + "    \"0x260d59c39309530902dffc0e2e867dd5baf5fab2\": 775.310000,\n"
                + "    \"0x8003258c0bc262025048505000d73186e16e7c8a\": 298.400000,\n"
                + "    \"0x04095df9a5731e1de6d9f36ca54558a05ae52d2d\": 859.330000,\n"
                + "    \"0xc8900a27bf643918c8fe7a3321effbc0f636aba3\": 319.330000,\n"
                + "    \"0x703b659a967a5c606ba365ed1ae751676de5c816\": 2538.280000,\n"
                + "    \"0xb50fe86024106c97d1741af0021fd9c324010474\": 1891.580000,\n"
                + "    \"0xb0b31571794d63f741c5f0f7c9343c041fd0b28f\": 415.770000,\n"
                + "    \"0xe5a83a300648391f464b61d1eb4735720f28d846\": 297.480000,\n"
                + "    \"0xf75f67ab1a2dc60ffbf8e3c6b0b67dcd116672ce\": 1348.420000\n" + "}";
        Map<String, Double> map = JacksonUtil.json2pojo(str, Map.class);
        Map<String, Double> mapString = sortMapByValue(map);
        System.out.println(JacksonUtil.obj2json(mapString));

    }

    public static Map<String, Double> sortMapByValue(Map<String, Double> oriMap) {
        if (oriMap == null || oriMap.isEmpty()) {
            return null;
        }
        Map<String, Double> sortedMap = new LinkedHashMap<String, Double>();
        List<Map.Entry<String, Double>> entryList = new ArrayList<Map.Entry<String, Double>>(oriMap.entrySet());
        Collections.sort(entryList, new MapValueComparator());

        Iterator<Map.Entry<String, Double>> iter = entryList.iterator();
        Map.Entry<String, Double> tmpEntry = null;
        while (iter.hasNext()) {
            tmpEntry = iter.next();
            sortedMap.put(tmpEntry.getKey(), tmpEntry.getValue());
        }
        return sortedMap;
    }

    static class MapValueComparator implements Comparator<Map.Entry<String, Double>> {

        @Override
        public int compare(Map.Entry<String, Double> me1, Map.Entry<String, Double> me2) {

            return me2.getValue().compareTo(me1.getValue());
        }
    }

}
