package semios.api.utils;

import com.amazonaws.services.s3.AmazonS3;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.*;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import java.io.StringWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

/**
 * @description: s3 工具类
 * @author: xiangbin
 * @create: 2022-08-16 10:26
 **/
@Slf4j
public class AmazonS3Util {

    private static RestTemplate restTemplate = new RestTemplate();


    @Autowired
    private AmazonS3 amazonS3Client;

    public static String uploadImg() {
        MultiValueMap<String, String> paramMap = new LinkedMultiValueMap<>();
        paramMap.add("orgNo", "orgNo");
        paramMap.add("terminalNo", "terminalNo");
        paramMap.add("signData", "signData");
        paramMap.add("requestDate", "requestDate");
        paramMap.add("email", "email");
        paramMap.add("notifyUrl", "notifyUrl");
        paramMap.add("loginNo", "loginNo");

        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Type", "application/x-www-form-urlencoded");
        HttpEntity<MultiValueMap<String, String>> r = new HttpEntity<>(paramMap, headers);
        ResponseEntity<String> responseEntity = restTemplate.postForEntity("", r, String.class);

        return null;

    }


    public static <T> T putReqXml(String url, String params, Class<T> c) {
//        log.info("请求外部接口---" + baseUrl + url + ",参数:" + JSONObject.toJSONString(params));
        String date = getGMTTime();
        String acl = "public-read-write";
        HttpHeaders headers = new HttpHeaders();
//        headers.setContentType(MediaType.APPLICATION_XML);
        headers.add("Content-Type", MediaType.APPLICATION_XML_VALUE);
        headers.add("Host", "xiangbin910716.s3.us-east-1.amazonaws.com");
        headers.add("Date", date);
        headers.add("x-amz-acl", acl);

        String CanonicalizedResource = "/";

        String CanonicalizedAmzHeaders = "x-amz-acl:" + acl;

        String StringToSign = "HTTP-PUT" + "\n" +
                MediaType.APPLICATION_XML_VALUE + "\n" +
                date + "\n" +
                CanonicalizedAmzHeaders + "\n" +
                CanonicalizedResource;

        String Signature = "";//Base64( SecurityUtil.sha1( new String("YourSecretAccessKey".getBytes(StandardCharsets.UTF_8), "UTF-8"), new String("YourSecretAccessKey".getBytes(StandardCharsets.UTF_8), "UTF-8") );

        String Authorization = "AWS" + " " + "AWSAccessKeyId" + ":" + Signature;
        HttpEntity<String> httpEntity = new HttpEntity<>(params, headers);
        ResponseEntity<T> responseEntity = new ResponseEntity<>(HttpStatus.OK);
        try {
            responseEntity = restTemplate.exchange(url, HttpMethod.PUT, httpEntity, c);
        } catch (Exception e) {
//            log.error("远程接口--" + baseUrl + url + "--调用失败,参数:" + JSONObject.toJSONString(params), e);
//            throw new BusinessException(ResultEnum.API_ERROR.getCode(), "远程接口调用失败");
        }
        if (responseEntity.getStatusCode() == HttpStatus.OK) {
            return responseEntity.getBody();
        } else {
//            throw new BusinessException(ResultEnum.API_ERROR, responseEntity.getStatusCode());
        }
        return null;
    }


    /**
     * 将对象直接转换成String类型的 XML输出
     *
     * @param obj
     * @return
     */
    public static String convertToXml(Object obj) {
        // 创建输出流
        StringWriter sw = new StringWriter();
        try {
            // 利用jdk中自带的转换类实现
            JAXBContext context = JAXBContext.newInstance(obj.getClass());
            Marshaller marshaller = context.createMarshaller();
            // 格式化xml输出的格式
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
            // 将对象转换成输出流形式的xml
            marshaller.marshal(obj, sw);
        } catch (JAXBException e) {
            e.printStackTrace();
        }
        return sw.toString();
    }

    public static String getGMTTime() {
        final Date currentTime = new Date();

        final DateFormat sdf = new SimpleDateFormat("EEE, d MMM, yyyy hh:mm:ss z", Locale.ENGLISH);

        // Give it to me in GMT time.
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        log.info("GMT time:{}", sdf.format(currentTime));
        return sdf.format(currentTime);
    }


    public static void main(String[] args) {

//        CreateBucketConfiguration createBucketConfiguration = new CreateBucketConfiguration();
//        createBucketConfiguration.setLocationConstraint("us-east-1");
//        String str = AmazonS3Util.convertToXml(createBucketConfiguration);
//        System.out.println(str);
//        String result = AmazonS3Util.putReqXml("xiangbin910716.s3.us-east-1.amazonaws.com", str, String.class);
//        System.out.println(result);

    }
}
