package semios.api.utils;

import java.sql.Timestamp;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * @description: 日期操作类
 * @author: xiangbin
 * @create: 2022-03-02 13:40
 **/
public class DateUtil {

    public static Timestamp getCurrentTimestamp() {
        return new Timestamp(System.currentTimeMillis());
    }

    public static Date addMinute(Date date, int minite) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        calendar.add(Calendar.MINUTE, minite);
        return calendar.getTime();
    }

    public static Date addDay(Date date, int day) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        calendar.add(Calendar.DAY_OF_MONTH, day);
        return calendar.getTime();
    }

    public static String getThisDayBeginTime(LocalDate localDate) {
        LocalDateTime localDateTime = LocalDateTime.of(localDate, LocalTime.MIN);
        long localDateStr = localDateTime.toInstant(ZoneOffset.UTC).toEpochMilli();
        return String.valueOf(localDateStr);
    }

    public static Timestamp getBeginOfToday() {
        Calendar cal = Calendar.getInstance();
        SimpleDateFormat form = new SimpleDateFormat("yyyy-MM-dd");
        String dateStr = form.format(cal.getTime()) + " 00:00:00";
        Date date = null;
        try {
            date = form.parse(dateStr);
        } catch (ParseException e) {
            return null;
        }
        return new Timestamp(date.getTime());
    }

    /**
     * 获取指定Timestamp， n天之后的时间
     *
     * @param date
     * @param day
     * @return
     */
    public static Timestamp getTimestampAfterDay(Timestamp date, int day) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        calendar.set(Calendar.DATE, calendar.get(Calendar.DATE) + day);
        SimpleDateFormat form = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        String dateStr = form.format(calendar.getTime());
        Date BeginOfCurrentDate = null;
        try {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            BeginOfCurrentDate = sdf.parse(dateStr);
        } catch (ParseException e) {
            return null;
        }
        return new Timestamp(BeginOfCurrentDate.getTime());
    }


    public static void main(String[] args) throws Exception {

//        BigDecimal bigDecimal = new BigDecimal("527777777.777778");
//        System.out.println(bigDecimal.intValue());
//        System.out.println(bigDecimal.setScale(4, RoundingMode.FLOOR).stripTrailingZeros().longValue());
//        String json = "{\"userAddress\":\"0x56dba60a326c8a1e1ed148486a2695884aa34e3b\",\"signatureHash\":\"0xe64f6c50469299dcc1133603208275b8cbf76c924a79e733a768beb23fdcc42176813a92c3d12d7f9c48872dec7911b96c4732e6450a2f66dbdf21e1ca50d5c41c\",\"originalText\":\"Welcome to DAO4Art！Click to sign in and accept the DAO4Art Terms of Service: https://opensea.io/tos This request will not trigger a blockchain transaction or cost any gas fees. Your authentication status will reset after 24 hours.\"}";
//        Map<String,Object> map =  JacksonUtil.json2map(json);
//        System.out.println(map.get("userAddress"));
//        System.out.println(map.get("signatureHash"));
//        System.out.println(map.get("originalText"));
//        String address = EthersUtils.verifyMessage(map.get("originalText").toString(), map.get("signatureHash").toString());
//        System.out.println(address);


//        System.out.println(String.format("https://test-dao4art.s3.ap-southeast-1.amazonaws.com/user/%s.png", 5));
//        Random random = new Random();
//        System.out.println(random.nextInt(33));
//        System.out.println(random.nextInt(33));
//        System.out.println(random.nextInt(33));
//        System.out.println(random.nextInt(33));
//        System.out.println(random.nextInt(33));
//        long localTime1 = LocalDateTime.now().toInstant(ZoneOffset.of("+8")).getEpochSecond();
//        System.out.println(String.valueOf(localTime1).substring(5));

//        String date = "2022-02-28 10:00:00";
//        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
//        Calendar calendar = Calendar.getInstance();
//        calendar.setTime(sdf.parse(date));
//        calendar.add(Calendar.DAY_OF_MONTH, 1);
//        System.out.println(sdf.format(calendar.getTime()));
//        System.out.println(DateUtil.getThisDayBeginTime(LocalDate.now(ZoneOffset.of("+8"))));
//        LocalDate localDate = LocalDate.now();
//        LocalDateTime localDateTime = LocalDateTime.of(localDate, LocalTime.MIN);
//        System.out.println(LocalDate.now(ZoneOffset.UTC));
//        System.out.println(LocalDate.now(ZoneId.systemDefault()));
//        LocalDateTime localDateTime = LocalDateTime.now(ZoneOffset.UTC);
//        System.out.println(String.valueOf(localDateTime.toString()));
//        LocalDateTime localDateStr2 = LocalDateTime.now(ZoneId.systemDefault());
//        System.out.println(String.valueOf(localDateStr2.toString()));

        Map map = new HashMap();
        map.remove("1");
        System.out.println("24234234");
    }

}
