package semios.gateway.utils;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.util.Calendar;
import java.util.Date;

/**
 * @description: 日期操作类
 * @author: xiangbin
 * @create: 2022-03-02 13:40
 **/
public class DateUtil {

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
        long localDateStr = localDateTime.toInstant(ZoneOffset.of("+8")).toEpochMilli();
        return String.valueOf(localDateStr);
    }


    public static void main(String[] args) throws Exception {
//        String date = "2022-02-28 10:00:00";
//        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
//        Calendar calendar = Calendar.getInstance();
//        calendar.setTime(sdf.parse(date));
//        calendar.add(Calendar.DAY_OF_MONTH, 1);
//        System.out.println(sdf.format(calendar.getTime()));
        System.out.println(DateUtil.getThisDayBeginTime(LocalDate.now(ZoneOffset.of("+8"))));
//        LocalDate localDate = LocalDate.now();
//        LocalDateTime localDateTime = LocalDateTime.of(localDate, LocalTime.MIN);
//        System.out.println(LocalDate.now(ZoneOffset.UTC));
//        System.out.println(LocalDate.now(ZoneId.systemDefault()));
//        LocalDateTime localDateTime = LocalDateTime.now(ZoneOffset.UTC);
//        System.out.println(String.valueOf(localDateTime.toString()));
//        LocalDateTime localDateStr2 = LocalDateTime.now(ZoneId.systemDefault());
//        System.out.println(String.valueOf(localDateStr2.toString()));
    }

}
