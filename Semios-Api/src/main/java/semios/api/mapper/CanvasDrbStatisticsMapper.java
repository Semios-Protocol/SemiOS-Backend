package semios.api.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.ibatis.annotations.Select;
import semios.api.model.entity.CanvasDrbStatistics;

import java.util.List;

/**
 * <p>
 * canvas在drb的统计信息 Mapper 接口
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface CanvasDrbStatisticsMapper extends BaseMapper<CanvasDrbStatistics> {

    @Select("SELECT \n" +
            "    can.canvas_name, can.canvas_logo,cds.*,can.id as can_id,can.current_price\n" +
            "FROM\n" +
            "    canvas AS can \n" +
            "    left join ( SELECT MAX( id ) AS id,canvas_id FROM canvas_drb_statistics GROUP BY canvas_id ) as cda on can.id = cda.canvas_id\n" +
            "LEFT JOIN\n" +
            "    canvas_drb_statistics AS cds ON cda.id = cds.id\n" +
            "WHERE\n" +
            "    can.project_id = #{projectId} and can.canvas_status != 0\n" +
            "ORDER BY can.current_price DESC,can.id desc")
    Page<CanvasDrbStatistics> selectByProjectId(IPage<CanvasDrbStatistics> page, String projectId);

    @Select("select * from canvas_drb_statistics where canvas_id = #{canvasId} order by drb_number desc limit 1")
    CanvasDrbStatistics selectLastedByCanvasId(Integer canvasId);


    @Select("select * from canvas_drb_statistics where canvas_id = #{canvasId} and status = 2 order by id desc")
    Page<CanvasDrbStatistics> selectByCanvasId(IPage<CanvasDrbStatistics> page, String canvasId);

    Page<CanvasDrbStatistics> canvasRanking(IPage<CanvasDrbStatistics> page);


    @Select("select * from canvas_drb_statistics where drb_number = #{drbNumber}")
    List<CanvasDrbStatistics> selectByDrbNumber(Integer drbNumber);

    @Select("select * from canvas_drb_statistics where canvas_id = #{canvasId} and drb_number = #{drbNumber}")
    CanvasDrbStatistics selectByCanvasIdAndDrbNumber(Integer canvasId, Integer drbNumber);


    @Select("select canvas_id,sum(drb_vol) as sevenDayDrbVol,sum(drb_vol_ex_tax) as mintRevenueExTax from canvas_drb_statistics where canvas_id = #{canvasId} and drb_number between #{startDrb} and #{endDrb} group by canvas_id")
    CanvasDrbStatistics selectByRangeDrb(String canvasId, Integer startDrb, Integer endDrb);


    @Select("select sum(drb_vol) as sevenDayDrbVol,sum(dao_drb_vol_ex_tax) as mintRevenueExTax from canvas_drb_statistics where dao_id = #{daoId} and drb_number <= #{drbNumber}")
    CanvasDrbStatistics selectMintRevenueByDaoId(Integer daoId, Integer drbNumber);


    @Select("select * from canvas_drb_statistics where status in (0,1) and times < 3 and drb_number = #{drbNumber}")
    List<CanvasDrbStatistics> selectFailStatus(Integer drbNumber);

    @Select("select * from canvas_drb_statistics where drb_number = #{drbNumber} and dao_id = #{daoId}")
    List<CanvasDrbStatistics> selectByDrbNumberAndDaoId(Integer drbNumber, Integer daoId);

}
