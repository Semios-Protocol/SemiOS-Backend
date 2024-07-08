package semios.api.utils;

import lombok.extern.slf4j.Slf4j;
import net.sf.json.JSONObject;
import org.apache.commons.lang3.StringUtils;
import org.springframework.web.client.RestTemplate;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.request.InfuraCallRequestDto;
import semios.api.service.feign.ISubscriptionService;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * @description: util
 * @author: xiangbin
 * @create: 2022-06-14 13:45
 **/
@Slf4j
public class ProtoDaoCommonUtil {

    /**
     * 查询合约方法
     *
     * @param netWork netWork
     * @param address address
     * @return String
     */
    public static String ethCall(ISubscriptionService subscriptionService, String netWork, String address,
                                 String method) {
        if (subscriptionService == null) {
            log.error("[ethCall] subscriptionService is null");
            return null;
        }
        InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
        infuraCallRequestDto.setNetWork(netWork);
        infuraCallRequestDto.setTo(address);
        infuraCallRequestDto.setData(method);
        Result<String> result = subscriptionService.infuraCall(infuraCallRequestDto);
        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[ethCall] error result:{}", result.getResultDesc());
            return null;
        }
        return result.getData();
    }

    public static Float bigdecimalToFloat(BigDecimal bigDecimal) {
        if (bigDecimal == null) {
            return 0.0f;
        }
        return bigDecimal.setScale(4, RoundingMode.FLOOR).stripTrailingZeros().floatValue();
    }

    /**
     * BigDecimal 转百分数 保留两位小数
     *
     * @param bigDecimal
     * @return
     */
    public static String bigdecimalPercentageToString(BigDecimal bigDecimal) {
        if (bigDecimal == null) {
            return "0";
        }
        return bigDecimal.setScale(2, RoundingMode.FLOOR).stripTrailingZeros().toPlainString();
    }

    public static String bigdecimalToString(BigDecimal bigDecimal) {
        if (bigDecimal == null) {
            return "0";
        }
        return bigDecimal.setScale(4, RoundingMode.UP).stripTrailingZeros().toPlainString();
    }

    public static String bigdecimalToString(BigDecimal bigDecimal, int scale) {
        if (bigDecimal == null) {
            return "0";
        }
        return bigDecimal.setScale(scale, RoundingMode.UP).stripTrailingZeros().toPlainString();
    }

    public static Integer stringToInteger(String str) {
        if (StringUtils.isBlank(str)) {
            return 0;
        }
        try {
            return Integer.valueOf(str);
        } catch (Exception e) {
            log.info("[ProtoDaoCommonUtil]stringToInteger str:{} e:{}", str, e);
        }
        return 0;
    }

    /**
     * 将参数除以100后返回 保留四位小数
     *
     * @param bigDecimal
     * @return
     */
    public static BigDecimal bigdecimalPercentage(BigDecimal bigDecimal) {
        if (bigDecimal == null) {
            return BigDecimal.ZERO;
        }
        return bigDecimal.divide(new BigDecimal("100"), 4, RoundingMode.FLOOR).stripTrailingZeros();
    }

    /**
     * BigDecimal 转百分数 保留一小数
     *
     * @param bigDecimal
     * @param round      true 向上取整 false 向下取整
     * @return
     */
    public static BigDecimal bigdecimalPercentageToString(BigDecimal bigDecimal, boolean round) {
        if (bigDecimal == null) {
            return BigDecimal.ZERO;
        }
        if (round) {
            bigDecimal = bigDecimal.multiply(new BigDecimal("0.95")).setScale(1, RoundingMode.UP);
        } else {
            bigDecimal = bigDecimal.multiply(new BigDecimal("0.95")).setScale(1, RoundingMode.FLOOR);
        }
        return bigDecimal;
    }

    /**
     * 除以10000，再乘以100变成百分数
     *
     * @param value
     * @return
     */
    public static BigDecimal strToBigDecimal(String value) {
        if (StringUtils.isBlank(value)) {
            return BigDecimal.ZERO;
        }
        return new BigDecimal(value).divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE), 4, RoundingMode.FLOOR)
                .multiply(new BigDecimal("100"));

    }


    public static JSONObject blockTime(String blockNo) {
        RestTemplate restTemplate = new RestTemplate();
        String requestUrl = String.format(ProtoDaoConstant.blockTimeUrl, blockNo);
        try {

            String object = restTemplate.postForObject(requestUrl, null, String.class);
            JSONObject jsonObject = JacksonUtil.json2pojo(object, JSONObject.class);
            if (jsonObject == null) {
                log.error("[blockTime] jsonObject is null, url:{}", requestUrl);
                return null;
            }
            if (jsonObject.getString("status").equals("0")) {
                log.info("[blockTime] request status is zero");
                return null;
            }
            JSONObject result = jsonObject.getJSONObject("result");
            return result;
//            return result.getDouble("EstimateTimeInSec");

        } catch (Exception e) {
            log.error("[blockTime] exception url:{} e:", requestUrl, e);
        }
        return null;
    }

    /**
     * {
     * "status": "1",
     * "message": "OK",
     * "result": "10302295"
     * }
     *
     * @param timestamp
     * @return
     */
    public static String timestampBlockNo(String timestamp) {
        if (StringUtils.isBlank(timestamp)) {
            return null;
        }
        RestTemplate restTemplate = new RestTemplate();
        String requestUrl = String.format(ProtoDaoConstant.etherscanBlockNumberUrl, timestamp);
        try {

            String object = restTemplate.postForObject(requestUrl, null, String.class);
            JSONObject jsonObject = JacksonUtil.json2pojo(object, JSONObject.class);
            if (jsonObject == null) {
                log.error("[timestampBlockNo] jsonObject is null, url:{}", requestUrl);
                return null;
            }
            if (jsonObject.getString("status").equals("0")) {
                log.info("[timestampBlockNo] request status is zero");
                return null;
            }
            String blockNo = jsonObject.getString("result");
            return blockNo;

        } catch (Exception e) {
            log.error("[timestampBlockNo] exception url:{} e:", requestUrl, e);
        }
        return null;
    }

    public static void main(String[] args) {
        BigDecimal bigDecimal = new BigDecimal("0.00301");
        System.out.println(bigDecimal.setScale(4, RoundingMode.UP).stripTrailingZeros().floatValue());
    }

}
