package semios.api.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.ibatis.annotations.Select;
import semios.api.model.entity.DaoDrbStatistics;

import java.util.List;

/**
 * <p>
 * Dao在drb的统计信息 Mapper 接口
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface DaoDrbStatisticsMapper extends BaseMapper<DaoDrbStatistics> {

    @Select("select dds.dao_id,sum(dds.drb_vol) as drb_vol from dao_drb_statistics as dds left join dao as dao on dds.dao_id = dao.id  where dds.drb_number between #{startDrb} and #{endDrb} and dao.dao_status > 0 group by dds.dao_id order by drb_vol desc")
    List<DaoDrbStatistics> selectGalleryDao(Integer startDrb, Integer endDrb);

    @Select("select drb.*,dao.id as daoId,dao.dao_name,dao.dao_logo_url,dao.dao_description,dao.dao_number,dao.dao_status,dao.dao_floor_price,dao.favorite_amount,dao.topup_mode, "
            + "case "
            + "        when (dao.canvas_created_whitelist + dao.canvas_created_blacklist + dao.minter_works_whitelist + dao.minter_works_blacklist) > 0 then 1 "
            + "        when (dao.canvas_created_whitelist + dao.canvas_created_blacklist + dao.minter_works_whitelist + dao.minter_works_blacklist) = 0 then 0 "
            + "        end as whiteList " + "from dao_drb_statistics drb " + "left join dao as dao on drb.dao_id = dao.id "
            + "where drb.dao_id = #{daoId} and dao.dao_status > 1 order by drb.drb_number desc limit 1")
    DaoDrbStatistics selectLastedDrbByDaoId(Integer daoId);

    @Select("select * from dao_drb_statistics where dao_id = #{daoId} and status = 2 order by drb_number desc")
    Page<DaoDrbStatistics> selectByDaoId(IPage<DaoDrbStatistics> page, Integer daoId);

    Page<DaoDrbStatistics> daosRanking(IPage<DaoDrbStatistics> page);

    @Select("select * from dao_drb_statistics where drb_number = #{drbNumber}")
    List<DaoDrbStatistics> selectByDrbNumber(Integer drbNumber);

    @Select("select * from dao_drb_statistics where dao_id = #{daoId} and drb_number = #{drbNumber}")
    DaoDrbStatistics selectByDaoIdAndDrbNumber(Integer daoId, Integer drbNumber);

    @Select("select * from dao_drb_statistics where status in (0,1) and times < 3 and drb_number = #{drbNumber}")
    List<DaoDrbStatistics> selectFailStatus(Integer drbNumber);

    @Select("select * from dao_drb_statistics where dao_id = #{daoId} and drb_number <= #{drbNumber} order by id desc limit 1")
    DaoDrbStatistics selectByDaoIdAndDrbNumberForAnalytics(Integer daoId, Integer drbNumber);


    @Select("select * from dao_drb_statistics where dao_id = #{daoId} and record_time = #{recordTime} order by id desc")
    List<DaoDrbStatistics> selectByDaoIdAndRecordTime(Integer daoId, Long recordTime);
}
