package semios.api.model.dto;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;

import org.apache.commons.lang.StringUtils;

import lombok.extern.slf4j.Slf4j;
import semios.api.utils.XssFilterUtil;

/**
 * @description: XSSRequestWrapper
 * @author: xiangbin
 * @create: 2023-01-09 11:36
 **/
@Slf4j
public class XSSRequestWrapper extends HttpServletRequestWrapper {

    public XSSRequestWrapper(HttpServletRequest request) {
        super(request);
    }

    /**
     * 将容易引起xss漏洞的半角字符直接替换成全角字符 在保证不删除数据的情况下保存
     *
     * @param value
     * @return 过滤后的值
     */
    private static String xssEncode(String value) {
        if (value == null || value.isEmpty()) {
            return value;
        }
        value = value.replaceAll("eval\\((.*)\\)", "");
        value = value.replaceAll("<", "&lt;");
        value = value.replaceAll(">", "&gt;");
        value = value.replaceAll("'", "&apos;");
        value = value.replaceAll("[\\\"\\\'][\\s]*javascript:(.*)[\\\"\\\']", "\"\"");
        value = value.replaceAll("(?i)<script.*?>.*?<script.*?>", "");
        value = value.replaceAll("(?i)<script.*?>.*?</script.*?>", "");
        value = value.replaceAll("(?i)<.*?javascript:.*?>.*?</.*?>", "");
        value = value.replaceAll("(?i)<.*?\\s+on.*?>.*?</.*?>", "");
        // value = value.replaceAll("[<>{}\\[\\];\\&]","");
        return value;
    }

    /**
     * @param name
     * @return java.lang.String[]
     * @Title: 对数组参数进行特殊字符过滤
     * @methodName: getParameterValues
     * @Description:
     */
    @Override
    public String[] getParameterValues(String name) {
        log.info("【XSSRequestWrapper】getParameterValues name：{}", name);
        String[] values = super.getParameterValues(name);
        if (values == null) {
            return null;
        }
        int count = values.length;
        String[] encodedValues = new String[count];
        for (int i = 0; i < count; i++) {
            encodedValues[i] = clearXss(values[i]);
        }
        return encodedValues;
    }

    /**
     * @param name
     * @return java.lang.String
     * @Title: 对参数中特殊字符进行过滤
     * @methodName: getParameter
     * @Description:
     */
    @Override
    public String getParameter(String name) {
        log.info("【XSSRequestWrapper】getParameter name：{}", name);
        String value = super.getParameter(name);
        if (value == null) {
            return null;
        }
        return clearXss(value);
    }

    /**
     * @param
     * @return java.util.Map
     * @Title: 覆盖getParameterMap方法，将参数名和参数值都做xss & sql过滤
     * @methodName: getParameterMap
     * @Description: 覆盖getParameterMap方法，将参数名和参数值都做xss & sql过滤 【一般post表单请求，或者前台接收为实体需要这样处理】
     */
    @Override
    public Map getParameterMap() {
        log.info("【XSSRequestWrapper】getParameterMap");

        Map<String, Object> paramMap = new HashMap<String, Object>();
        Map<String, String[]> requestMap = super.getParameterMap();
        Iterator<Map.Entry<String, String[]>> it = requestMap.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry<String, String[]> entry = it.next();
            if (entry.getValue().length == 1) {
                paramMap.put(xssEncode(entry.getKey()), xssEncode(entry.getValue()[0]));
            } else {
                String[] values = entry.getValue();
                String value = "";
                for (int i = 0; i < values.length; i++) {
                    value = values[i] + ",";
                }
                value = value.substring(0, value.length() - 1);
                paramMap.put(xssEncode(entry.getKey()), xssEncode(entry.getValue()[0]));
            }
        }
        return paramMap;
    }

    /**
     * @Title: 对请求头部进行特殊字符过滤
     * @methodName: getHeader
     * @param name
     * @return java.lang.String
     * @Description:
     *
     */
    /*@Override
    public String getHeader(String name) {
        String value = super.getHeader(name);
        if (value == null) {
            return null;
        }
        return clearXss(value);
    }*/

    /**
     * @param name
     * @return java.lang.Object
     * @Title: 获取attribute, 特殊字符过滤
     * @methodName: getAttribute
     * @Description:
     */
    @Override
    public Object getAttribute(String name) {
        // log.info("【XSSRequestWrapper】getAttribute name：{}", name);
        Object value = super.getAttribute(name);
        if (value != null && value instanceof String) {
            clearXss((String) value);
        }
        return value;
    }

    /**
     * @param value
     * @return java.lang.String
     * @Title: 特殊字符处理（转义或删除）
     * @methodName: clearXss
     * @Description:
     */
    private String clearXss(String value) {

        if (StringUtils.isEmpty(value)) {
            return value;
        }

        // 字符编码不一致，需要转换。我们系统是UTF-8编码，这里不需要
        /*try {
            value = new String(value.getBytes("ISO8859-1"), "UTF-8");
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
            return null;
        }*/

        return XssFilterUtil.stripXss(value);
    }
}
