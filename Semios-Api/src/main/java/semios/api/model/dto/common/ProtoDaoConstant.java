package semios.api.model.dto.common;

import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import lombok.extern.slf4j.Slf4j;

/**
 * @description: 常量
 * @author: xiangbin
 * @create: 2022-08-12 14:27
 **/
@Slf4j
@Component
public class ProtoDaoConstant {
    // traceId
    public static final String TRACE_ID = "traceId";
    public static final String COOKIE_ADDRESS = "address";
    public static final String SESSION_ADDRESS = "user";
    // token存放的是签名信息
    public static final String COOKIE_TOKEN = "token";
    public static final String COOKIE_TOKEN_TIME = "time";
    public static final String COOKIE_ROLE = "role";
    public static final String COOKIE_USER_ADDRESS = "userAddress";
    public static final String COOKIE_NAME = "name";
    public static final String COOKIE_AVATAR = "avatar";
    public static final String LOCAL_HOST = "localhost";
    public static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    public static final Integer SEVEN_DAY = 6;// gallery查询7日内交易总金额最高的三个DAO
    public static final String tokenUrl = "/token/";
    // 每块多少秒
    public static String BLOCK_SECONDS;
    public static String activity = "";
    public static String netWork = "goerli";// ""rinkeby";
    public static String appName = "protodao";
    public static Integer MINT_D4A_FEE_RATIO = 250;
    public static Integer RATIO_BASE = 10000;
    public static Integer MINT_PROJECT_FEE_RATIO = 3000;
    public static Integer MINT_PROJECT_FEE_RATIO_FLAT_PRICE = 3500;
    public static Integer AUTO_GENERATE_WORK_NUMBER = 1000;
    public static Long CREATE_PROJECT_FEE = 0L;
    public static Long CREATE_CANVAS_FEE = 0L;
    public static Integer DAO_RESERVE_NUMBER = 109;// dao 预留编号
    public static String BASIC_RATIO = "1000000000000000000";// 10的18次方
    public static String BASIC_RATIO_DECIMAL = "18";// 18
    public static String DEFAULT_PAY_CURRENCY_TYPE = "ETH";// 18
    public static String DEFAULT_PAY_CURRENCY_LOGO = "https://sepolia.etherscan.io/images/main/empty-token.png";// 18
    public static String ERC20_TOTAL = "1000000000";// erc20发放总量10亿个
    public static Boolean D4APause = false;// true停机 false 未停机
    public static String CURRENT_ROUND;// 当前DRB 通过值订阅和定期查询，如果变化了就计算前一天的static
    //下一个drb的开始区块高度
    public static Integer NEXT_DRB_START_BLOCK;
    //每个drb包含的区块数量
    public static Long PERIOD_BLOCK;
    public static String ZERO_ADDRESS = "0x0000000000000000000000000000000000000000";
    public static String ZERO_MERKLE_ROOT = "0x0000000000000000000000000000000000000000000000000000000000000000";
    // start value 0xf4267391072B27D76Ed8f2A9655BCf5246013F2d
    public static String protocol_fee_pool = "0xe6046371b729f23206a94ddcace89fecebbd565c";
    public static String exchangeERC20ToETH = "0x6dfa6d72"; // 0xf538965f
    public static String getTokenToETH = "0xf4b7440d";
    public static String claimProjectERC20RewardWithETH = "0xedd11881";
    public static String claimCanvasRewardWithETH = "0xdf6803f0";
    // for ERC721 contract
    public static String balanceOf = "0x70a08231";
    // for ERC721 contract select nft owner
    public static String nftOwnerOf = "0x6352211e";
    public static String daoBucketName = "/dao";
    public static String canvasBucketName = "/canvas";
    public static String workBucketName = "/work";
    public static String planBucketName = "/plan";
    public static String metaBucketName = "/meta";
    public static String userBucketName = "/user";
    public static String prbContract;
    public static String protocolContract;
    public static String protocolContractAbi;
    public static String projectProxyContract;
    public static String projectProxyContractAbi;
    public static String claimerContract;
    public static String claimerContractAbi;
    public static String permissionControl;
    public static String permissionControlAbi;
    public static String erc721ContractAbi;
    public static String erc20ContractAbi;
    public static String bucketName;
    public static String compressBucketName;
    public static String urlPrefix;
    public static String nameCheckList;
    public static String daoOpenseaLink;
    public static String openseaApiNftLink;
    public static String openseaApiKey;
    public static String openseaApiDaoLink;
    public static String openseaApiSuccess = "Successfully";
    public static String s3ImageUrl;
    public static String s3CdnImageUrl;
    public static String etherscanUrl;

    // public static String routerContract;

    // public static String routerContractAbi;
    public static String protocolSetterAbi;

    // public static String createFundingAbi;

    public static String pdCreateAbi;

    public static String pdRoundAbi;

    public static String d4aErc20Abi;


    // proto-dao-setting-writable-abi
    //public static String protoDaoSettingWritableAbi;

    public static String protocolReadableAbi;

    // public static String protoDaoSettingWritableAbiV2;


    public static String uniswapV2PairAbi;


    public static String galleryDao;
    public static String royaltyFeeDao;


    public static String workImageDefaultUrl;

    public static String workImageDaoLogoUrl;


    public static String imitateLoginAddress;


    public static String etherscanBlockHeight;

    public static String etherscanBlockDate;

    public static String etherscanBlockNumber;


    public static String etherscanBlockNumberUrl;


    // work lock address
    public static String workLockAddress;

    // work lock abi
    public static String workLockAbi;

    public static String pdGrantAbi;

    public static String pdPlanAbi;
    /**
     * 可用的daoNumber
     */
    public static List<Integer> daoNumberList = new ArrayList<>();

    public static String blockTimeUrl;


    @Value("${block_time}")
    public void setBlockSeconds(String blockSeconds) {
        log.info("galleryDao:{}", blockSeconds);
        ProtoDaoConstant.BLOCK_SECONDS = blockSeconds;
    }


    @Value("${block_time_url}")
    public void setBlockTimeUrl(String blockTimeUrl) {
        log.info("blockTimeUrl:{}", blockTimeUrl);
        ProtoDaoConstant.blockTimeUrl = blockTimeUrl;
    }

    @Value("${dao-reserve-number}")
    public void setDaoReserveNumber(Integer daoReserveNumber) {
        ProtoDaoConstant.DAO_RESERVE_NUMBER = daoReserveNumber;
        log.info("DAO_RESERVE_NUMBER:{}", daoReserveNumber);
        for (Integer i = 0; i <= daoReserveNumber; i++) {
            ProtoDaoConstant.daoNumberList.add(i);
        }
    }

    @Value("${protocol-contract}")
    public void setProtocolContract(String protocolContract) {
        log.info("protocolContract:{}", protocolContract);
        ProtoDaoConstant.protocolContract = protocolContract;
    }

    @Value("${protocol-contract-abi}")
    public void setProtocolContractAbi(String protocolContractAbi) {
        // log.info("protocolContractAbi:{}", protocolContractAbi);
        ProtoDaoConstant.protocolContractAbi = protocolContractAbi;
    }

//    @Value("${protocol-proxy-contract}")
//    public void setProjectProxyContract(String projectProxyContract) {
//        log.info("projectProxyContract:{}", projectProxyContract);
//        ProtoDaoConstant.projectProxyContract = projectProxyContract;
//    }

//    @Value("${protocol-proxy-contract-abi}")
//    public void setProjectProxyContractAbi(String projectProxyContractAbi) {
//        ProtoDaoConstant.projectProxyContractAbi = projectProxyContractAbi;
//    }

    @Value("${bucket_name}")
    public void setBucketName(String bucketName) {
        log.info("bucketName:{}", bucketName);
        ProtoDaoConstant.bucketName = bucketName;
    }

    @Value("${compress_bucket_name}")
    public void setCompressBucketName(String compressBucketName) {
        log.info("compressBucketName:{}", compressBucketName);
        ProtoDaoConstant.compressBucketName = compressBucketName;
    }

    @Value("${url_prefix}")
    public void setUrlPrefix(String urlPrefix) {
        log.info("urlPrefix:{}", urlPrefix);
        ProtoDaoConstant.urlPrefix = urlPrefix;
    }

    @Value("${name_check_list}")
    public void setNameCheckList(String nameCheckList) {
        log.info("nameCheckList:{}", nameCheckList);
        ProtoDaoConstant.nameCheckList = nameCheckList;
    }

    @Value("${dao_opensea_link}")
    public void setDaoOpenseaLink(String daoOpenseaLink) {
        log.info("dao_opensea_link:{}", daoOpenseaLink);
        ProtoDaoConstant.daoOpenseaLink = daoOpenseaLink;
    }

    @Value("${opensea_api_nft_link}")
    public void setOpenseaApiNftLink(String openseaApiNftLink) {
        log.info("opensea_api_nft_link:{}", openseaApiNftLink);
        ProtoDaoConstant.openseaApiNftLink = openseaApiNftLink;
    }

    @Value("${opensea_api_key}")
    public void setOpenseaApiKey(String openseaApiKey) {
        log.info("opensea_api_key:{}", openseaApiKey);
        ProtoDaoConstant.openseaApiKey = openseaApiKey;
    }

    @Value("${opensea_api_dao_link}")
    public void setOpenseaApiDaoLink(String daoOpenseaLink) {
        log.info("opensea_api_dao_link:{}", daoOpenseaLink);
        ProtoDaoConstant.openseaApiDaoLink = daoOpenseaLink;
    }

    @Value("${claimer-contract}")
    public void setClaimerContract(String claimerContract) {
        log.info("claimerContract:{}", claimerContract);
        ProtoDaoConstant.claimerContract = claimerContract;
    }

    @Value("${claimer-contract-abi}")
    public void setClaimerContractAbi(String claimerContractAbi) {
        ProtoDaoConstant.claimerContractAbi = claimerContractAbi;
    }

    @Value("${net_work}")
    public void setNetWork(String netWork) {
        log.info("netWork:{}", netWork);
        ProtoDaoConstant.netWork = netWork;
    }

    @Value("${prb-contract}")
    public void setPrbContract(String prbContract) {
        log.info("prbContract:{}", prbContract);
        ProtoDaoConstant.prbContract = prbContract;
    }

    @Value("${s3_image_url}")
    public void setS3ImageUrl(String s3ImageUrl) {
        log.info("s3_image_url:{}", s3ImageUrl);
        ProtoDaoConstant.s3ImageUrl = s3ImageUrl;
    }

    @Value("${s3_cdn_image_url}")
    public void setS3CdnImageUrl(String s3CdnImageUrl) {
        log.info("s3_cdn_image_url:{}", s3CdnImageUrl);
        ProtoDaoConstant.s3CdnImageUrl = s3CdnImageUrl;
    }

    @Value("${permission-control-contract}")
    public void setPermissionControl(String permissionControl) {
        log.info("permissionControl:{}", permissionControl);
        ProtoDaoConstant.permissionControl = permissionControl;
    }

    @Value("${permission-control-contract-abi}")
    public void setPermissionControlAbi(String permissionControlAbi) {
        ProtoDaoConstant.permissionControlAbi = permissionControlAbi;
    }

    @Value("${erc721-contract-abi}")
    public void setErc721ContractAbi(String erc721ContractAbi) {
        ProtoDaoConstant.erc721ContractAbi = erc721ContractAbi;
    }

    @Value("${etherscan.url}")
    public void setEtherscanUrl(String etherscanUrl) {
        log.info("etherscanUrl:{}", etherscanUrl);
        ProtoDaoConstant.etherscanUrl = etherscanUrl;
    }

//    @Value("${router-contract}")
//    public void setRouterContract(String routerContract) {
//        ProtoDaoConstant.routerContract = routerContract;
//    }

//    @Value("${router-contract-abi}")
//    public void setRouterContractAbi(String routerContractAbi) {
//        ProtoDaoConstant.routerContractAbi = routerContractAbi;
//    }

    @Value("${erc20-contract-abi}")
    public void setErc20ContractAbi(String erc20ContractAbi) {
        ProtoDaoConstant.erc20ContractAbi = erc20ContractAbi;
    }

//    @Value("${proto-dao-setting-writable-abi}")
//    public void setProtoDaoSettingWritableAbi(String protoDaoSettingWritableAbi) {
//        ProtoDaoConstant.protoDaoSettingWritableAbi = protoDaoSettingWritableAbi;
//    }

    @Value("${gallery-dao}")
    public void setGalleryDao(String galleryDao) {
        log.info("galleryDao:{}", galleryDao);
        ProtoDaoConstant.galleryDao = galleryDao;
    }

    @Value("${royalty-fee-dao}")
    public void setRoyaltyFeeDao(String royaltyFeeDao) {
        log.info("royaltyFeeDao:{}", royaltyFeeDao);
        ProtoDaoConstant.royaltyFeeDao = royaltyFeeDao;
    }


    @Value("${uniswap-v2-pair-abi}")
    public void setUniswapV2PairAbi(String uniswapV2PairAbi) {
        ProtoDaoConstant.uniswapV2PairAbi = uniswapV2PairAbi;
    }

    @Value("${work_image_default_url}")
    public void setWorkImageDefaultUrl(String workImageDefaultUrl) {
        log.info("work_image_default_url:{}", workImageDefaultUrl);
        ProtoDaoConstant.workImageDefaultUrl = workImageDefaultUrl;
    }

    @Value("${work_image_dao_logo_url}")
    public void setWorkImageDaoLogoUrl(String workImageDaoLogoUrl) {
        log.info("workImageDaoLogoUrl:{}", workImageDaoLogoUrl);
        ProtoDaoConstant.workImageDaoLogoUrl = workImageDaoLogoUrl;
    }

    @Value("${spring.profiles.active}")
    public void setActivity(String activity) {
        log.info("activity:{}", activity);
        ProtoDaoConstant.activity = activity;
    }

    @Value("${protocol-readable-abi}")
    public void setProtocolReadableAbi(String protocolReadableAbi) {
        ProtoDaoConstant.protocolReadableAbi = protocolReadableAbi;
    }

    @Value("${protocol-setter-abi}")
    public void setProtocolSetterAbi(String protocolSetterAbi) {
        ProtoDaoConstant.protocolSetterAbi = protocolSetterAbi;
    }

    @Value("${imitate.login.address}")
    public void setImitateLoginAddress(String imitateLoginAddress) {
        log.info("imitateLoginAddress:{}", imitateLoginAddress);
        ProtoDaoConstant.imitateLoginAddress = imitateLoginAddress;
    }

//    @Value("${create_funding_abi}")
//    public void setCreateFundingAbi(String createFundingAbi) {
//        ProtoDaoConstant.createFundingAbi = createFundingAbi;
//    }

    @Value("${pd_round_abi}")
    public void setPdRoundAbi(String pdRoundAbi) {
        ProtoDaoConstant.pdRoundAbi = pdRoundAbi;
    }

    @Value("${pd_create_abi}")
    public void setPdCreateAbi(String pdCreateAbi) {
        ProtoDaoConstant.pdCreateAbi = pdCreateAbi;
    }

    @Value("${etherscan_block_height}")
    public void setEtherscanBlockHeight(String etherscanBlockHeight) {
        log.info("etherscanBlockHeight:{}", etherscanBlockHeight);
        ProtoDaoConstant.etherscanBlockHeight = etherscanBlockHeight;
    }

    @Value("${etherscan_block_number}")
    public void setEtherscanBlockNumber(String etherscanBlockNumber) {
        log.info("etherscanBlockNumber:{}", etherscanBlockNumber);
        ProtoDaoConstant.etherscanBlockNumber = etherscanBlockNumber;
    }

    @Value("${d4a_erc20_abi}")
    public void setD4aErc20Abi(String d4aErc20Abi) {
        ProtoDaoConstant.d4aErc20Abi = d4aErc20Abi;
    }

    @Value("${etherscan_block_date}")
    public void setEtherscanBlockDate(String etherscanBlockDate) {
        log.info("etherscanBlockDate:{}", etherscanBlockDate);
        ProtoDaoConstant.etherscanBlockDate = etherscanBlockDate;
    }

    @Value("${etherscan_block_number_url}")
    public void setEtherscanBlockNumberUrl(String etherscanBlockNumberUrl) {
        ProtoDaoConstant.etherscanBlockNumberUrl = etherscanBlockNumberUrl;
    }

    @Value("${work_lock_address}")
    public void setWorkLockAddress(String workLockAddress) {
        ProtoDaoConstant.workLockAddress = workLockAddress;
    }

    @Value("${work_lock_abi}")
    public void setWorkLockAbi(String workLockAbi) {
        ProtoDaoConstant.workLockAbi = workLockAbi;
    }

    @Value("${pd-grant-abi}")
    public void setPdGrantAbi(String pdGrantAbi) {
        ProtoDaoConstant.pdGrantAbi = pdGrantAbi;
    }

    @Value("${pd-plan-abi}")
    public void setPdPlanAbi(String pdPlanAbi) {
        ProtoDaoConstant.pdPlanAbi = pdPlanAbi;
    }

}
