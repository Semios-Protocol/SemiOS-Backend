package semios.api.utils;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class ScriptUtil {
    public static void main(String[] args) {
        String url = "https://etherscan.io/token/0xdac17f958d2ee523a2206206994597c13d831ec7"; // 替换为要爬取的网页URL

        try {
            Map<String, String> cookies = new HashMap<>();
            cookies.put("OAID", "700efa2c44178bf316f4f18d8bc15953");
            cookies.put("OAGEO", "2%7CUS%7CNA%7C%7CLos%20Angeles%7C90001%7C33.972%7C-118.242%7C20%7CAmerica%2FLos_Angeles%7C803%7CCA%7C%7C%7C%7C%7C%7C%7C%7C%7C%7C%7C%7C%7C%7C%7C");
            // 使用Jsoup连接到指定URL，并获取页面内容
            Document doc = Jsoup.connect(url)
                    //.userAgent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36 Edg/123.0.0.0")
                    //.cookies(cookies)
                    .header("Origin", "https://etherscan.io")
                    .header("Accept-Encoding", "gzip, deflate, br, zstd")
                    .header("Connection", "keep-alive")
                    .header("Accept-Language", "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6")
                    .header("Cookie", "OAID=700efa2c44178bf316f4f18d8bc15953; OAGEO=2%7CUS%7CNA%7C%7CLos%20Angeles%7C90001%7C33.972%7C-118.242%7C20%7CAmerica%2FLos_Angeles%7C803%7CCA%7C%7C%7C%7C%7C%7C%7C%7C%7C%7C%7C%7C%7C%7C%7C")
                    .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36 Edg/123.0.0.0")
                    .header("Accept", "text/javascript, application/javascript, application/ecmascript, application/x-ecmascript, */*; q=0.01")
                    .header("Referer", "https://etherscan.io/")
                    .get();

            // 使用CSS选择器找到所有class为 "d-flex gap-2" 的元素
            Elements elements = doc.select("div.d-flex.gap-2");

            // 遍历找到的元素
            for (Element element : elements) {
                // 找到包含img标签的子元素
                Elements imgElements = element.select("img");

                // 遍历img标签，获取src属性的值
                for (Element imgElement : imgElements) {
                    String src = imgElement.attr("src");
                    // 输出src属性值
                    System.out.println("Image Source: " + src);
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

}
