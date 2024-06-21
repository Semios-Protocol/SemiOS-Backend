package semios.api.service.impl;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.mapper.IncentivePlanMapper;
import semios.api.model.entity.IncentivePlan;
import semios.api.model.vo.res.Plan.TogetherPlanVo;
import semios.api.service.IIncentivePlanService;

import java.util.List;

/**
 * <p>
 * 激励计划表 服务实现类
 * </p>
 *
 * @author zhyyao
 * @since 2024-04-29
 */
@Service
public class IncentivePlanServiceImpl extends ServiceImpl<IncentivePlanMapper, IncentivePlan> implements IIncentivePlanService {

    @Autowired
    private IncentivePlanMapper incentivePlanMapper;

    @Override
    public IncentivePlan getPlanByPlanCode(String planCode) {
        return incentivePlanMapper.getPlanByPlanCode(planCode);
    }

    @Override
    public IncentivePlan planDetailByUri(String planUri) {
        return incentivePlanMapper.planDetailByUri(planUri);
    }

    @Override
    public Integer getPlanNumberByProjectId(String projectId) {
        return incentivePlanMapper.getPlanNumberByProjectId(projectId);
    }

    @Override
    public List<IncentivePlan> selectAllPlanByProjectId(String projectId) {
        return incentivePlanMapper.selectAllPlanByProjectId(projectId);
    }

    @Override
    public TogetherPlanVo selectAllPlanByProjectIdGroup(String projectId) {
        return incentivePlanMapper.selectAllPlanByProjectIdGroup(projectId);
    }

    @Override
    public Page<IncentivePlan> selectAllPlanByProjectIdPage(Page<IncentivePlan> page, String projectId) {
        return incentivePlanMapper.selectAllPlanByProjectIdPage(page, projectId);
    }
}
