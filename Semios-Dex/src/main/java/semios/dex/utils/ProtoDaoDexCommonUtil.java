package semios.dex.utils;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import semios.dex.model.dto.common.Result;
import semios.dex.model.dto.common.ResultDesc;
import semios.dex.model.dto.request.InfuraCallRequestDto;
import semios.dex.service.feign.ISubscriptionService;

import java.math.BigDecimal;
import java.math.RoundingMode;


/**
 * @description: util
 * @author: xiangbin
 * @create: 2022-06-14 13:45
 **/
@Slf4j
public class ProtoDaoDexCommonUtil {


    /**
     * 查询合约方法
     *
     * @param netWork netWork
     * @param address address
     * @return String
     */
    public static String ethCall(ISubscriptionService subscriptionService, String netWork, String address, String method) {
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


    public static String bigdecimalToString(BigDecimal bigDecimal) {
        if (bigDecimal == null) {
            return "0";
        }
        return bigDecimal.setScale(4, RoundingMode.FLOOR).stripTrailingZeros().toPlainString();
    }


    public static Integer stringToInteger(String str) {
        if (StringUtils.isBlank(str)) {
            return 0;
        }
        try {

            return Integer.valueOf(str);
        } catch (Exception e) {
            log.info("[Dao4ArtCommonUtil]stringToInteger str:{} e:{}", str, e);
        }
        return 0;
    }


    public static void main(String[] args) {
        BigDecimal bigDecimal = new BigDecimal("0.00305");
        System.out.println(bigDecimal.setScale(4, RoundingMode.FLOOR).stripTrailingZeros().floatValue());
    }


}
