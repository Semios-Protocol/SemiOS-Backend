package semios.dex.utils;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.web3j.crypto.Keys;
import semios.dex.model.dto.common.Dao4ArtDexConstant;

import javax.xml.bind.DatatypeConverter;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

@Slf4j
public class CommonUtil {
    public static final int BYTES32_LENGTH_IN_HEX = 64;
    public static final int ADDRESS_LENGTH_IN_HEX = 40;

    public static String removeHexPrefixIfExists(String hexString) {
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
        String regex = "[a-zA-Z0-9]*";
        return str.matches(regex);
    }

    public static BigDecimal getPowBigDecimal(Integer decimal) {
        if (decimal == null) {
            return new BigDecimal(Dao4ArtDexConstant.BASIC_RATIO);
        }
        return new BigDecimal("10").pow(decimal);
    }


    public static void main(String[] args) throws Exception {
        String data = "0x0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000c454e465f76555344432076320000000000000000000000000000000000000000";
        List<String> items = splitBy32Bytes(data);
        for (String item : items) {
            System.out.println(new String(hexStringToByteArray(item)));
        }

//        for (int i = 0; i < items.size(); i++) {
//            System.out.println(new BigInteger(items.get(i), 16));
//        }


    }
}

