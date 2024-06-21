package semios.api.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.ibatis.annotations.Select;
import semios.api.model.entity.IncentivePlan;
import semios.api.model.vo.res.Plan.TogetherPlanVo;

import java.util.List;

/**
 * <p>
 * 激励计划表 Mapper 接口
 * </p>
 *
 * @author zhyyao
 * @since 2024-04-29
 */
public interface IncentivePlanMapper extends BaseMapper<IncentivePlan> {

    @Select("select * from incentive_plan where plan_code = #{planCode} limit 1")
    IncentivePlan getPlanByPlanCode(String planCode);

    @Select("select * from incentive_plan where plan_uri = #{planUri} limit 1")
    IncentivePlan planDetailByUri(String planUri);

    @Select("select IFNULL(max(plan_number),0) from incentive_plan where project_id = #{projectId} for update")
    Integer getPlanNumberByProjectId(String projectId);

    @Select("select id,incentive_status from incentive_plan where project_id = #{projectId}   ")
    List<IncentivePlan> selectAllPlanByProjectId(String projectId);

    @Select("select * from incentive_plan where project_id = #{projectId} order by plan_number desc ")
    Page<IncentivePlan> selectAllPlanByProjectIdPage(Page<IncentivePlan> page, String projectId);

    @Select("SELECT SUM(incentive_status=1) AS planNotStarted, SUM(incentive_status=2) AS planOngoing, SUM(incentive_status=3) AS planEnd, COUNT(id) AS planTotal FROM incentive_plan where project_id = #{projectId} ;")
    TogetherPlanVo selectAllPlanByProjectIdGroup(String projectId);

}
