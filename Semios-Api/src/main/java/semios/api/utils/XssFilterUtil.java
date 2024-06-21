package semios.api.utils;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @description: XssFilterUtil
 * @author: xiangbin
 * @create: 2023-01-09 11:38
 **/
public class XssFilterUtil {


    private static final Logger log = LoggerFactory.getLogger(XssFilterUtil.class);

    private static List<Pattern> patterns = null;

    /**
     * @param
     * @return java.util.List<java.lang.Object [ ]>
     * @Title: XSS常见攻击
     * @methodName: getXssPatternList
     * @Description: Pattern.MULTILINE(? m)：在这种模式下，'^'和'$'分别匹配一行的开始和结束。此外，'^'仍然匹配字符串的开始，'$'也匹配字符串的结束。
     * 默认情况下，这两个表达式仅仅匹配字符串的开始和结束。
     * <p>
     * Pattern.DOTALL(?s) ：在这种模式下，表达式'.'可以匹配任意字符，包括表示一行的结束符。
     * 默认情况下，表达式'.'不匹配行的结束符。
     */
    private static List<Object[]> getXssPatternList() {

        List<Object[]> ret = new ArrayList<Object[]>();

        ret.add(new Object[]{"<(no)?script[^>]*>.*?</(no)?script>", Pattern.CASE_INSENSITIVE});
        ret.add(new Object[]{"</script>", Pattern.CASE_INSENSITIVE});
        ret.add(new Object[]{"<script(.*?)>", Pattern.CASE_INSENSITIVE | Pattern.MULTILINE | Pattern.DOTALL});
        ret.add(new Object[]{"eval\\((.*?)\\)", Pattern.CASE_INSENSITIVE | Pattern.MULTILINE | Pattern.DOTALL});
        ret.add(new Object[]{"expression\\((.*?)\\)", Pattern.CASE_INSENSITIVE | Pattern.MULTILINE | Pattern.DOTALL});
        ret.add(new Object[]{"(javascript:|vbscript:|view-source:)*", Pattern.CASE_INSENSITIVE});
        ret.add(new Object[]{"<(\"[^\"]*\"|\'[^\']*\'|[^\'\">])*>", Pattern.CASE_INSENSITIVE | Pattern.MULTILINE | Pattern.DOTALL});
        ret.add(new Object[]{"(window\\.location|window\\.|\\.location|document\\.cookie|document\\.|alert\\(.*?\\)|window\\.open\\()*", Pattern.CASE_INSENSITIVE | Pattern.MULTILINE | Pattern.DOTALL});
        ret.add(new Object[]{"<+\\s*\\w*\\s*(oncontrolselect|oncopy|oncut|ondataavailable|ondatasetchanged|ondatasetcomplete|ondblclick|ondeactivate|ondrag|ondragend|ondragenter|ondragleave|ondragover|ondragstart|ondrop|onerror=|onerroupdate|onfilterchange|onfinish|onfocus|onfocusin|onfocusout|onhelp|onkeydown|onkeypress|onkeyup|onlayoutcomplete|onload|onlosecapture|onmousedown|onmouseenter|onmouseleave|onmousemove|onmousout|onmouseover|onmouseup|onmousewheel|onmove|onmoveend|onmovestart|onabort|onactivate|onafterprint|onafterupdate|onbefore|onbeforeactivate|onbeforecopy|onbeforecut|onbeforedeactivate|onbeforeeditocus|onbeforepaste|onbeforeprint|onbeforeunload|onbeforeupdate|onblur|onbounce|oncellchange|onchange|onclick|oncontextmenu|onpaste|onpropertychange|onreadystatechange|onreset|onresize|onresizend|onresizestart|onrowenter|onrowexit|onrowsdelete|onrowsinserted|onscroll|onselect|onselectionchange|onselectstart|onstart|onstop|onsubmit|onunload)+\\s*=+", Pattern.CASE_INSENSITIVE | Pattern.MULTILINE | Pattern.DOTALL});
        return ret;
    }

    /**
     * @param
     * @return java.util.List<java.util.regex.Pattern>
     * @Title: XSS常见攻击-正则表达式
     * @methodName: getPatterns
     * @Description:
     */
    private static List<Pattern> getPatterns() {


        if (patterns == null) {

            List<Pattern> list = new ArrayList<Pattern>();

            String regex = null;
            Integer flag = null;
            int arrLength = 0;

            for (Object[] arr : getXssPatternList()) {
                arrLength = arr.length;
                for (int i = 0; i < arrLength; i++) {
                    regex = (String) arr[0];
                    flag = (Integer) arr[1];
                    list.add(Pattern.compile(regex, flag));
                }
            }

            patterns = list;
        }

        return patterns;
    }

    /**
     * @param value
     * @return java.lang.String
     * @Title: 处理特殊字符
     * @methodName: stripXss
     * @Description: 如果是特殊字符，策略有两种：转义或删除
     */
    public static String stripXss(String value) {

        if (StringUtils.isNotBlank(value) && !"null".equals(value)) {

//            log.info("【XSS攻击防御】，接收字符是：{}", value);

            //
            Matcher matcher = null;

            for (Pattern pattern : getPatterns()) {
                matcher = pattern.matcher(value);
                // 匹配
                if (matcher.find()) {
                    // 删除相关字符串
//                    log.info("【XSS攻击防御】匹配正则 接收字符是：{}", value);
                    value = matcher.replaceAll("");
//                    log.info("【XSS攻击防御】匹配正则 处理后是：{}", value);
                }
            }

//            log.info("【XSS攻击防御】，匹配正则是：{}，处理后是：{}", matcher, value);


            /*
             * 替换为转移字符，类似HtmlUtils.htmlEscape
             */
            //value = value.replaceAll("<", "&lt;").replaceAll(">", "&gt;");
            //删除特殊符号
            //String specialStr = "%20|=|!=|-|--|;|'|\"|%|#|+|//|/| |\\|<|>|(|)|{|}";

//            if (StringUtils.isNotBlank(value) && !"null".equals(value)) {
//                String specialStr = "%20|=|!=|-|--|;|'|\"|%|#|[+]|//|/| |\\|<|>|(|)|{|}";
//                for (String str : specialStr.split("\\|")) {
//                    if (value.indexOf(str) > -1) {
//                        log.info("【XSS攻击防御】，特殊符号处理前是：{}", value);
//                        value = value.replaceAll(str, "");
//                        log.info("【XSS攻击防御】，特殊符号处理后是：{}", value);
//
//                    }
//                }
//            }


        }


        return value;
    }


}