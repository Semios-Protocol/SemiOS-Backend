package semios.dex.model.dto.common;

import org.springframework.stereotype.Component;

/**
 * @author: fjtan
 * @create: 2023-05-18 09:27
 **/
@Component
public class DexConstant {

    // traceId
    public static final String TRACE_ID = "traceId";
    public static final String COOKIE_ADDRESS = "address";
    public static final String SESSION_ADDRESS = "user";
    public static final String COOKIE_TOKEN = "token";
    public static final String COOKIE_TOKEN_TIME = "time";
    public static final String COOKIE_ROLE = "role";
    public static final String COOKIE_USER_ADDRESS = "userAddress";
    public static final String COOKIE_NAME = "name";
    public static final String COOKIE_AVATAR = "avatar";
    public static final String LOCAL_HOST = "localhost";
    public static final Integer INTERVAL_DAY = -7;// 查询7天内交易量以及7天前价格等
    public static String netWork = "goerli";// ""rinkeby";
    public static String BASIC_RATIO = "1000000000000000000";// 10的18次方
    public static String CURRENT_ROUND;// 当前DRB 通过值订阅和定期查询，如果变化了就计算前一天的static

    public static String ZERO_ADDRESS = "0x0000000000000000000000000000000000000000";

    // start value
    public static String protocol_fee_pool = "0xf4267391072B27D76Ed8f2A9655BCf5246013F2d";

    public static String exchangeERC20ToETH = "0xf538965f";
    public static String claimProjectERC20RewardWithETH = "0xedd11881";
    public static String claimCanvasRewardWithETH = "0xdf6803f0";

    // for ERC721 contract
    public static String balanceOf = "0x70a08231";

    public static String ETH_UNIT = "ETH";

    public static Integer PRICE_DECIMAL = 12;

    public static Integer PERCENT_DECIMAL = 4;

}
