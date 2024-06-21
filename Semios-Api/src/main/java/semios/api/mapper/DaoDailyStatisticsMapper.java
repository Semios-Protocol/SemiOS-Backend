package semios.api.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;
import semios.api.model.entity.DaoDailyStatistics;

import java.util.List;

/**
 * <p>
 * Dao资金池每日零点统计数据 Mapper 接口
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface DaoDailyStatisticsMapper extends BaseMapper<DaoDailyStatistics> {


    /**
     * 查询计算完成的统计信息
     *
     * @param recordTime 日期时间戳，当日零点
     * @return DaoDailyStatistics
     */
    @Select("select * from dao_daily_statistics where status = 2 and record_time = #{recordTime} order by id desc ")
    List<DaoDailyStatistics> selectDaoDailyCompleteStatus(Long recordTime);

    /**
     * 查询未计算完成的统计信息
     *
     * @param recordTime 日期时间戳，当日零点
     * @return DaoDailyStatistics
     */
    @Select("select * from dao_daily_statistics where status != 2 and record_time = #{recordTime} order by id desc ")
    List<DaoDailyStatistics> selectDaoDailyIncompleteStatus(Long recordTime);


    /**
     * 查询dao某个时间已经完成的记录信息
     *
     * @param daoId      dao的ID
     * @param recordTime 记录时间
     * @return DaoDailyStatistics 这一天已经计算完的记录
     */
    @Select("select * from dao_daily_statistics where status = 2 and dao_id = #{daoId} and record_time = #{recordTime} order by id desc limit 1")
    DaoDailyStatistics selectDaoDailyCompleteByDaoId(Integer daoId, Long recordTime);

    /**
     * 查询dao某个时间段已经完成的记录信息
     *
     * @param daoId      dao的ID
     * @param recordTime 记录时间
     * @return List<DaoDailyStatistics>
     */
    @Select("select * from dao_daily_statistics where status = 2 and dao_id = #{daoId} and record_time >= #{recordTime} order by id asc")
    List<DaoDailyStatistics> selectDaoDailyCompleteByDaoIdAndRecordTime(Integer daoId, Long recordTime);


    /**
     * 查询dao某个时间的记录信息
     *
     * @param projectId  dao的projectId
     * @param recordTime 记录时间
     * @return DaoDailyStatistics 这一天已经计算完的记录
     */
    @Select("select * from dao_daily_statistics where project_id = #{projectId} and record_time = #{recordTime} order by id desc limit 1")
    DaoDailyStatistics selectDaoDailyByProjectId(String projectId, Long recordTime);


}
