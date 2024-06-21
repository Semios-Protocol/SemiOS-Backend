package semios.dex.utils;

import java.sql.Timestamp;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

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

    public static String formatTime(Date target) {
        if (target == null) {
            return "";
        }
        return new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(target);
    }

    /**
     * 获得当前时间的n天前或后
     *
     * @param origin
     * @param intervals
     * @return
     */
    public static Date getIntervalDate(Date origin, long intervals) {
        return new Date(origin.getTime() + intervals * 86400000);
    }

    /**
     * 获得当前时间的n天前或后时间戳（10位）
     *
     * @param origin
     * @param intervals
     * @return
     */
    public static long getIntervalTime(Date origin, long intervals) {
        return new Date(origin.getTime() + intervals * 86400000).getTime() / 1000;
    }

    /**
     * 获取当前时间上一个小时的开始时间
     *
     * @return
     */
    public static Timestamp getLastHourBeginTime() {
        Calendar cal = Calendar.getInstance();
        cal.set(Calendar.HOUR_OF_DAY, cal.get(Calendar.HOUR_OF_DAY) - 1);
        SimpleDateFormat form = new SimpleDateFormat("yyyy-MM-dd HH");
        String dateStr = form.format(cal.getTime()) + ":00:00";
        Date date = null;
        try {
            date = form.parse(dateStr);
        } catch (ParseException e) {
            return null;
        }
        return new Timestamp(date.getTime());
    }

    public static Timestamp getBeginOfCurrentHour() {
        Calendar cal = Calendar.getInstance();
        SimpleDateFormat form = new SimpleDateFormat("yyyy-MM-dd HH");
        String dateStr = form.format(cal.getTime()) + ":00:00";
        Date date = null;
        try {
            date = form.parse(dateStr);
        } catch (ParseException e) {
            return null;
        }
        return new Timestamp(date.getTime());
    }

    public static Timestamp getBeginOfNextHour() {
        Calendar cal = Calendar.getInstance();
        SimpleDateFormat form = new SimpleDateFormat("yyyy-MM-dd HH");
        String dateStr = form.format(cal.getTime()) + ":00:00";
        Date date = null;
        try {
            date = form.parse(dateStr);
        } catch (ParseException e) {
            return null;
        }
        return new Timestamp(date.getTime() + 3600 * 1000);
    }

    public static Timestamp getBeginOfLastHour() {
        Calendar cal = Calendar.getInstance();
        SimpleDateFormat form = new SimpleDateFormat("yyyy-MM-dd ");
        String dateStr = form.format(cal.getTime()) + "23:00:00";
        Date date = null;
        try {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            date = sdf.parse(dateStr);
        } catch (ParseException e) {
            return null;
        }
        return new Timestamp(date.getTime());
    }

    public static List<Long> getHoursList(long start, long end) {
        List<Long> result = new ArrayList<Long>();
        for (long i = start; i < end; i += 3600) {
            result.add(i);
        }
        return result;
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

    public static Timestamp getEndOfToday() {
        Calendar cal = Calendar.getInstance();
        SimpleDateFormat form = new SimpleDateFormat("yyyy-MM-dd");
        String dateStr = form.format(cal.getTime()) + " 23:59:59";
        Date date = null;
        try {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            date = sdf.parse(dateStr);
        } catch (ParseException e) {
            return null;
        }
        return new Timestamp(date.getTime());
    }

    /**
     * Description: 获得给定时间戳的当天的开始时间戳(凌晨)
     *
     * @param date
     */
    public static Timestamp getBeginOfCurrentDate(Timestamp date) {
        Calendar cal = Calendar.getInstance();
        cal.setTime(date);
        SimpleDateFormat form = new SimpleDateFormat("yyyy-MM-dd");
        String dateStr = form.format(cal.getTime()) + " 00:00:00";
        Date BeginOfCurrentDate = null;
        try {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            BeginOfCurrentDate = sdf.parse(dateStr);
        } catch (ParseException e) {
            return null;
        }
        return new Timestamp(BeginOfCurrentDate.getTime());
    }

    /**
     * Description: 获得给定时间戳的当天的结束时间戳
     *
     * @param date
     */
    public static Timestamp getEndOfCurrentDate(Timestamp date) {
        Calendar cal = Calendar.getInstance();
        cal.setTime(date);
        SimpleDateFormat form = new SimpleDateFormat("yyyy-MM-dd");
        String dateStr = form.format(cal.getTime()) + " 23:59:59";
        Date endOfCurrentDate = null;
        try {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            endOfCurrentDate = sdf.parse(dateStr);
        } catch (ParseException e) {
            return null;
        }
        return new Timestamp(endOfCurrentDate.getTime());

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
        Date today = DateUtil.getBeginOfToday();
        System.out.println(today.getTime());
        System.out.println(formatTime(today));
        Date startDate = DateUtil.getIntervalDate(today, -7);
        System.out.println(formatTime(startDate));
        Date endDate = DateUtil.getIntervalDate(today, 1);
        System.out.println(formatTime(endDate));
        // String date = "2022-02-28 10:00:00";
        // SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        // Calendar calendar = Calendar.getInstance();
        // calendar.setTime(sdf.parse(date));
        // calendar.add(Calendar.DAY_OF_MONTH, 1);
        // System.out.println(sdf.format(calendar.getTime()));
        System.out.println(DateUtil.getThisDayBeginTime(LocalDate.now(ZoneOffset.of("+8"))));
        System.out.println(DateUtil.getBeginOfCurrentHour().getTime() / 1000);
        // LocalDate localDate = LocalDate.now();
        // LocalDateTime localDateTime = LocalDateTime.of(localDate, LocalTime.MIN);
        // System.out.println(LocalDate.now(ZoneOffset.UTC));
        // System.out.println(LocalDate.now(ZoneId.systemDefault()));
        // LocalDateTime localDateTime = LocalDateTime.now(ZoneOffset.UTC);
        // System.out.println(String.valueOf(localDateTime.toString()));
        // LocalDateTime localDateStr2 = LocalDateTime.now(ZoneId.systemDefault());
        // System.out.println(String.valueOf(localDateStr2.toString()));
    }

}
