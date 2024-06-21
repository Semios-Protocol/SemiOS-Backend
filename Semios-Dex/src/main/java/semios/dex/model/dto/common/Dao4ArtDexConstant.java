package semios.dex.model.dto.common;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * @description: 常量
 * @author: xiangbin
 * @create: 2022-08-12 14:27
 **/
@Slf4j
@Component
public class Dao4ArtDexConstant {

    /**
     * traceId
     */
    public static final String TRACE_ID = "traceId";
    /**
     * 10的18次方
     */
    public static final String BASIC_RATIO = "1000000000000000000";
    /**
     * 零地址
     */
    public static final String ZERO_ADDRESS = "0x0000000000000000000000000000000000000000";
    public static final String COOKIE_ADDRESS = "address";
    public static final String SESSION_ADDRESS = "user";
    // public static String protocolContractV2;
    public static final String COOKIE_USER_ADDRESS = "userAddress";
    public static String exchangeERC20ToETH = "0x6dfa6d72"; // 0xf538965f
    public static String getTokenToETH = "0xf4b7440d";
    public static String protocolContract;
    /**
     * WETH地址
     */
    public static String WETH_ADDRESS = "";

    public static String netWork = "sepolia";

    public static String balanceOf = "0x70a08231";


    @Value("${net_work}")
    public void setNetWork(String netWork) {
        log.info("netWork:{}", netWork);
        Dao4ArtDexConstant.netWork = netWork;
    }

    @Value("${protocol-contract}")
    public void setProtocolContract(String protocolContract) {
        log.info("protocolContract:{}", protocolContract);
        Dao4ArtDexConstant.protocolContract = protocolContract;
    }

//    @Value("${protocol-contract-v2}")
//    public void setProtocolContractV2(String protocolContractV2) {
//        Dao4ArtDexConstant.protocolContractV2 = protocolContractV2;
//    }
}
