package semios.api.service;


import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import semios.api.model.entity.IncentivePlan;
import semios.api.model.vo.res.Plan.TogetherPlanVo;

import java.util.List;

/**
 * <p>
 * 激励计划表 服务类
 * </p>
 *
 * @author zhyyao
 * @since 2024-04-29
 */
public interface IIncentivePlanService extends IService<IncentivePlan> {

    IncentivePlan getPlanByPlanCode(String planCode);

    IncentivePlan planDetailByUri(String daoUri);

    Integer getPlanNumberByProjectId(String projectId);

    List<IncentivePlan> selectAllPlanByProjectId(String projectId);

    TogetherPlanVo selectAllPlanByProjectIdGroup(String projectId);

    Page<IncentivePlan> selectAllPlanByProjectIdPage(Page<IncentivePlan> page, String projectId);
}
