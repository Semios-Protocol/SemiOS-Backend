package semios.api.utils;

import org.apache.commons.lang3.RandomStringUtils;

/**
 * @author fjtan
 */
public class CodeUtil {

    /**
     * 生成随机18位唯一编码
     *
     * @param prefix
     * @return prefix + 13位系统毫秒 + 4位随机字符
     */
    public static String generateCode(char prefix) {
        return String.valueOf(prefix).toUpperCase() + System.currentTimeMillis() + RandomStringUtils.randomNumeric(4);
    }


    public static void main(String[] args) {
        String objectName = "1234.jpg";
        objectName = "456789" + objectName.substring(objectName.lastIndexOf("."));
        System.out.println(objectName);
    }

}
