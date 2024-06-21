package semios.api.service;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import semios.api.model.entity.TreasuryTransaction;
import semios.api.model.vo.res.TogetherDaoTreasuryTransactionVo;

/**
 * <p>
 * 国库交易表 服务类
 * </p>
 *
 * @author zhyyao
 * @since 2024-02-22
 */
public interface ITreasuryTransactionService extends IService<TreasuryTransaction> {
    // 根据dao id 找出对应dao下所有的信息
    Page<TogetherDaoTreasuryTransactionVo> getTreasuryTransactionList(IPage<TreasuryTransaction> page, String projectId);
}
