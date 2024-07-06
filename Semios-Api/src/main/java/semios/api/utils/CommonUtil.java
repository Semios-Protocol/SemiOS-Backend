package semios.api.utils;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.web3j.crypto.Keys;
import org.web3j.utils.Numeric;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.response.SearchNameParamDto;

import javax.xml.bind.DatatypeConverter;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Slf4j
public class CommonUtil {
    public static final int BYTES32_LENGTH_IN_HEX = 64;
    public static final int ADDRESS_LENGTH_IN_HEX = 40;
    public static final Pattern patternAddress = Pattern.compile("([A-F].*[a-f])|([a-f].*[A-F])");
    public static final Pattern pattern = Pattern.compile("[\u4E00-\u9FA5|\\！|\\，|\\。|\\（|\\）|\\《|\\》|\\“|\\”|\\？|\\：|\\；|\\【|\\】]");

    public static String removeHexPrefixIfExists(String hexString) {
        if(StringUtils.isBlank(hexString)){
            return "";
        }
        hexString = hexString.toLowerCase();
        if (hexString.startsWith("0x")) {
            return hexString.substring(2);
        }
        return hexString;
    }

    public static List<String> readAllLines(String fileFullPath) throws Exception {
        return Files.readAllLines(Paths.get(fileFullPath));
    }

    public static String addHexPrefixIfNotExist(String hexString) {
        if(StringUtils.isBlank(hexString)){
            return "";
        }
        if (hexString.startsWith("0x")) {
            return hexString;
        }
        return "0x" + hexString;
    }

    public static byte[] hexStringToByteArray(String s) {
        s = CommonUtil.removeHexPrefixIfExists(s);
        return DatatypeConverter.parseHexBinary(s);
    }

    public static String byteArrayToHexString(byte[] b) {
        return DatatypeConverter.printHexBinary(b);
    }

    public static List<String> splitBy32Bytes(String hexString) {
        hexString = CommonUtil.removeHexPrefixIfExists(hexString);
        List<String> items = new ArrayList<>();
        for (int i = 0; i < hexString.length(); i += 64) {
            items.add(hexString.substring(i, i + 64));
        }
        return items;
    }

    public static String formatBytes32Address(String address) {
        address = CommonUtil.removeHexPrefixIfExists(address);
        return address.substring(CommonUtil.BYTES32_LENGTH_IN_HEX - CommonUtil.ADDRESS_LENGTH_IN_HEX);
    }

    public static String fillLeadingZerosInBytes32(String hexString) throws Exception {
        hexString = CommonUtil.removeHexPrefixIfExists(hexString);
        if (hexString.length() > CommonUtil.BYTES32_LENGTH_IN_HEX) {
            throw new Exception("Hex string length too long!");
        }
        int hexStringLength = hexString.length();
        int zeros = CommonUtil.BYTES32_LENGTH_IN_HEX - hexStringLength;
        String result = hexString;
        for (int i = zeros; i > 0; i--) {
            result = '0' + result;
        }
        return result;
    }

    public static String hexToAscii(String hexStr) {
        StringBuilder output = new StringBuilder("");
        for (int i = 0; i < hexStr.length(); i += 2) {
            String str = hexStr.substring(i, i + 2);
            output.append((char) Integer.parseInt(str, 16));
        }
        return output.toString();
    }

    public static String dynamicArgumentDecoding(String data, String argumentOffsetInHex, boolean toAscii) {
        BigInteger argumentOffset = new BigInteger(argumentOffsetInHex, 16);
        int argumentOffsetInHexString = argumentOffset.intValue() << 1;
        BigInteger argumentLength = new BigInteger(data.substring(argumentOffsetInHexString, argumentOffsetInHexString + CommonUtil.BYTES32_LENGTH_IN_HEX), 16);
        int argumentLengthInHexString = argumentLength.intValue() << 1;
        String argumentHex = data.substring(argumentOffsetInHexString + CommonUtil.BYTES32_LENGTH_IN_HEX, argumentOffsetInHexString + CommonUtil.BYTES32_LENGTH_IN_HEX + argumentLengthInHexString);
        if (!toAscii) {
            return argumentHex;
        }
        return CommonUtil.hexToAscii(argumentHex);
    }

    public static String toChecksumAddress(String address) {
        return Keys.toChecksumAddress(address);
    }

    public static long tsToMillSeconds(BigInteger ts) {
        return 1000L * ts.longValue();
    }


    /**
     * 判断字符ch是否16进制字符，是返回true，否返回false
     */
    public static boolean sixteen(String str) {
        try {
            new BigInteger(str, 16);
        } catch (Exception e) {
            log.warn("str:{} is not hex", str);
            return false;
        }
        return true;
    }

    /**
     * 10进制转16进制 如果非数字返回空
     */
    public static String tenToHex(int number) {
        try {
            return Integer.toHexString(number);
        } catch (Exception e) {
            log.warn("number:{} is not hex", number);
            return null;
        }
    }

    public static String hexToTenString(String str) {
        try {
            if (StringUtils.isBlank(str)) {
                return null;
            }
            BigInteger bigInteger = new BigInteger(removeHexPrefixIfExists(str), 16);
            return bigInteger.toString();
        } catch (Exception e) {
            log.warn("str:{} is not hex", str);
            return null;
        }
    }


    public static String replaceOperator(String param) {
        if (StringUtils.isBlank(param)) {
            return param;
        }
        param = param.replaceAll("\\+", "");
        param = param.replaceAll("-", "");
        param = param.replaceAll("\\*", "");
        param = param.replaceAll("/", "");
        param = param.replaceAll("=", "");
        System.out.println(param);
        return param;
    }


    public static String nameCheck(String name) {
//        if(name.startsWith(" ") || name.endsWith(" ") || name.contains("  ")){
//            return "This name is not available.";
//        }
        if (StringUtils.isBlank(name)) {
            return "Name must contain one letter or digit.";
        }
        if (name.length() > 45) {
            return "Ensure this value has at most 45 characters (it has " + name.length() + ").";
        }
        String regex = "^[a-zA-Z0-9]+$";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(name.replaceAll(" ", ""));
        if (!matcher.matches()) {
            return "This name is not available.";
        }
        List<String> stringList = Arrays.asList(ProtoDaoConstant.nameCheckList.split(","));
        if (stringList.contains(name)) {
            return "This name is not available.";
        }
        return null;
    }

    /**
     * 判断是否为数字和字母的组合
     *
     * @param str
     * @return
     */
    public static boolean isLetterOrDigit(String str) {
        if (StringUtils.isBlank(str)) {
            return false;
        }
        String regex = "[a-zA-Z0-9@*#/]*";
        return str.matches(regex);
    }

    /**
     * 字符串是否包含中文
     *
     * @param str 待校验字符串
     * @return true 包含中文字符 false 不包含中文字符
     * @throws
     */
    public static boolean isContainChinese(String str) {

        if (StringUtils.isEmpty(str)) {
            return false;
        }
        Matcher m = pattern.matcher(str);
        return m.find();
    }

    /**
     * 校验地址字符串有效性和唯一性
     *
     * @param addressStr
     * @return
     * @throws Exception
     * @mock ["0x91b8650f6e27d7855a23772cab9daa46a3fd4f1f","0xf8baf7268f3daefe4135f7711473ae8b6c3b47d8"]
     */
    @SuppressWarnings("unchecked")
    public static List<String> checkAddress(String addressStr) throws Exception {
        List<String> result = new ArrayList<String>();
        List<String> addressList = JacksonUtil.json2pojo(addressStr, List.class);
        if (addressList == null) {
            throw new RuntimeException("address to array error:" + addressStr);
        }
        Set<String> addressSet = new HashSet<String>();
        for (String address : addressList) {
            if (!address.matches("^(0x)?[0-9a-fA-F]{40}$")) {
                throw new RuntimeException(address + " Wrong address please modify and resubmit");
            }
            address = Numeric.prependHexPrefix(address);

            Matcher matcher = patternAddress.matcher(address);
            if (matcher.find()) {
                String checkAddress = Keys.toChecksumAddress(address);
                if (!address.equals(checkAddress)) {
                    throw new RuntimeException(address + " Wrong address please modify and resubmit");
                }
            }
            address = address.toLowerCase();
            if (!addressSet.contains(address)) {
                addressSet.add(address);
                result.add(address);
            }
        }
        return result;
    }


    /**
     * 计算N小时之后的区块高度
     *
     *
     * @param days 距离23年12月11日的差多少天
     * @return
     */

    public static BigInteger calculateStartBlockHeight(Long hours) {
        if (ProtoDaoConstant.etherscanBlockNumber == null) {
            return BigInteger.ZERO;
        }
        return new BigInteger(String.valueOf(hours)).multiply(new BigInteger(ProtoDaoConstant.etherscanBlockNumber)).divide(new BigInteger(ProtoDaoConstant.BASIC_RATIO));
    }

    /**
     * 已知12月12号距离公式为
     * 10190445 * 10^18 + 24 * 239055188367481833171 = 10196182324520819563996104
     *
     * @param days 距离23年12月11日的差多少天
     * @return
     */
    public static BigInteger calculateStartBlockHeight(Integer days) {
        if (ProtoDaoConstant.etherscanBlockNumber == null || ProtoDaoConstant.etherscanBlockHeight == null) {
            return BigInteger.ZERO;
        }

        return new BigInteger(ProtoDaoConstant.etherscanBlockHeight).add(
                new BigInteger(ProtoDaoConstant.etherscanBlockNumber).multiply(new BigInteger(String.valueOf(days)).multiply(new BigInteger(String.valueOf(24))))).divide(new BigInteger(ProtoDaoConstant.BASIC_RATIO));
    }

    /**
     * 计算前一天零点，到开始日期的blockNo
     * 10190445 * 10^18 + 24 * 239055188367481833171 = 10196182324520819563996104
     *
     * @param days 距离昨天零点相差的天数
     * @return
     */
    public static BigInteger calculateStartBlockNo(String blockNo, Integer days) {
        if (ProtoDaoConstant.etherscanBlockNumber == null) {
            return BigInteger.ZERO;
        }

        return new BigInteger(blockNo).multiply(new BigInteger(ProtoDaoConstant.BASIC_RATIO)).add(
                new BigInteger(ProtoDaoConstant.etherscanBlockNumber).multiply(new BigInteger(String.valueOf(days)).multiply(new BigInteger(String.valueOf(24)))));
    }

    public static BigDecimal getPowBigDecimal(Integer decimal){
        if (decimal==null){
            return new BigDecimal(ProtoDaoConstant.BASIC_RATIO);
        }
        return new BigDecimal("10").pow(decimal);
    }


    /**
     * 是否匹配 D4A@123
     *
     * @param name
     * @return
     */
    public static boolean patternDao(String name) {
        String dao = "^PDAO.T[1-9][0-9]*$";
        Pattern pattern = Pattern.compile(dao);
        Matcher matcher = pattern.matcher(name);
        return matcher.matches();
    }

    /**
     * 是否匹配 D4A@123/Canvas*234/NFT#123
     *
     * @param name
     * @return
     */
    public static boolean patternWork(String name) {
        String work = "^PDAO.T[1-9][0-9]*.[1-9][0-9]*$";
        Pattern pattern = Pattern.compile(work);
        Matcher matcher = pattern.matcher(name);
        return matcher.matches();
    }


    /**
     * 获取daoNumber canvasNumber workNumber
     *
     * @param name
     * @return
     */
    public static SearchNameParamDto patternName(String name) {
        SearchNameParamDto searchNameParamDto = new SearchNameParamDto();
        if (StringUtils.isBlank(name)) {
            return searchNameParamDto;
        }
        name = name.replaceAll(" ", "");
        if (!name.startsWith("D4A@")) {
            return searchNameParamDto;
        }

        if (patternWork(name)) {
            searchNameParamDto.setDaoNumber(Long.valueOf(name.substring(4, name.indexOf("/Canvas*"))));
            searchNameParamDto
                    .setCnavasNumber(Long.valueOf(name.substring(name.indexOf("/Canvas*") + 8, name.indexOf("/NFT#"))));
            searchNameParamDto.setWorkNumber(Long.valueOf(name.substring(name.indexOf("/NFT#") + 5)));
            return searchNameParamDto;
        }

        if (patternDao(name)) {
            searchNameParamDto.setDaoNumber(Long.valueOf(name.substring(4)));
            return searchNameParamDto;
        }

        return searchNameParamDto;
    }




    public static void main(String[] args) throws Exception {

//        int a = 6397;
//        String b = tenToHex(a);
//        System.out.println(b);

//        String param = "LuBvI/EBbL*sS-WY+WK0RC+paQ==.json";
//        param = replaceOperator(param);
//        System.out.println(param);
//        String data = "0x0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000c454e465f76555344432076320000000000000000000000000000000000000000";
//        List<String> items = splitBy32Bytes(data);
//        for (String item : items) {
//            System.out.println(new String(hexStringToByteArray(item)));
//        }

//        for (int i = 0; i < items.size(); i++) {
//            System.out.println(new BigInteger(items.get(i), 16));
//        }

//        String str = "1@1 9*Ab# .。cD ";
//        System.out.println(CommonUtil.isContainChinese(str));
//
//        String res = CommonUtil.fillLeadingZerosInBytes32("60")+CommonUtil.fillLeadingZerosInBytes32("a36a9e04edd5ce11e65bfa942be8386f3b49aea8")
//                + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.tenToHex(2)) + CommonUtil.fillLeadingZerosInBytes32("1") + CommonUtil.removeHexPrefixIfExists("0240bc4a121498363365a81e88497d3b376f3f5b119e57785a4e6ad4d8372d1f");
//        System.out.println(res);


        DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate dateParam2 = LocalDate.parse("2024-06-14", df);
        LocalDateTime midnight = LocalDateTime.of(dateParam2, LocalTime.MIDNIGHT);
        ZonedDateTime targetMidnight = midnight.atZone(ZoneId.systemDefault());

        ZonedDateTime now = ZonedDateTime.now(ZoneId.systemDefault());
        Duration duration = Duration.between(now, targetMidnight);

        System.out.println(duration.toHours());

        System.out.println(calculateStartBlockHeight(duration.toHours()));
    }
}

