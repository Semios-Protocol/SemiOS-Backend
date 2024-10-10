package semios.api.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;
import semios.api.model.entity.WorkTopupHarvest;
import semios.api.model.vo.req.DaoProjectVo;
import semios.api.model.vo.res.*;
import semios.api.model.vo.res.TopUpReward.TopupNftListVo;

import java.util.List;

/**
 * <p>
 * topup模式work下的eth和token的数量 Mapper 接口
 * </p>
 *
 * @author zhyyao
 * @since 2024-01-30
 */
public interface WorkTopupHarvestMapper extends BaseMapper<WorkTopupHarvest> {


    @Select("select * from work_topup_harvest  where is_del = 0 and project_id=#{project} and mount_work_id = #{mountWorkId}")
    WorkTopupHarvest selectByProjectIdAndMountWorkId(String project, Integer mountWorkId);


    @Select("select * from work_topup_harvest where is_del = 0 and  mount_work_id = #{mountWorkId} and mount_erc721_address = #{mount721Address}")
    List<WorkTopupHarvest> selectListByMountWorkIdAndMount721Address(Integer mountWorkId, String mount721Address);

    @Select("select t.dao_id,sum(t.erc20_amount) as erc20_amount,sum(t.eth_amount) as eth_amount,sum(input_token_amount) as onChainEthBalance,sum(output_token_amount) as onChainTokenBalance  from work_topup_harvest t " +
            "    inner join work w on t.mount_work_id = w.id " +
            "    where w.owner_address = #{address} and w.work_status=1 and w.is_del=0 and (erc20_amount>0 or eth_amount>0)" +
            "    group by t.dao_id order by eth_amount desc,erc20_amount desc ")
    List<WorkTopupDaoBalanceVo> selectTopUpBalanceByAddress(String address);

    @Select("select w.id as workId,d.id as daoId,w.work_number as workNumber,d.dao_name as daoName,IFNULL(w.lock_status,0) as workLockStatus,t.eth_amount as ethBalance,t.erc20_amount as tokenBalance,d.together_dao_id as togetherDaoId ,d.pay_currency_type,d.input_token_address ,d.dao_symbol,d.erc20_token as daoErc20Address from " +
            " (select * from work_topup_harvest where dao_id=#{daoId} and (erc20_amount>0 or eth_amount>0)) t " +
            " inner join work w on t.mount_work_id=w.id " +
            " inner join dao d on w.dao_id=d.id " +
            "where w.owner_address = #{address} and w.work_status=1 and w.is_del=0 " +
            "order by t.eth_amount desc,t.erc20_amount desc,w.block_time desc")
    List<UserTopupBalanceDetailsVo> selectTopUpSeeMore(String address, String daoId);

    @Select("select count(distinct w.owner_address) as makerTotalAmount,sum(t.erc20_amount) as noSpendTokenAmount,sum(t.eth_amount) as noSpendEthAmount from " +
            " (select * from work_topup_harvest where project_id=#{projectId} and (erc20_amount>0 or eth_amount>0) ) t " +
            " inner join work w on t.mount_work_id = w.id ")
    TogetherDaoMakerVo getTogetherDaoMakerVo(String projectId);

    @Select("select sum(t.eth_amount) as eth_amount,sum(t.erc20_amount) as erc20_amount from " +
            "(select * from work_topup_harvest where project_id=#{projectId} and (erc20_amount>0 or eth_amount>0) ) t " +
            "inner join work w on t.mount_work_id=w.id " +
            "where w.owner_address=#{userAddress} and  w.work_status=1 and w.is_del=0 and w.topup_mode=1")
    WorkTopupHarvest selectByProjectIdAndUserAddress(String projectId, String userAddress);


    // 获取所有seed nodes下有过绑定的nft信息
    @Select("select  d.id as dao_id, wth.project_id as project_id , wth.mount_work_id as workId, wth.mount_erc721_address as erc721_token,wth.mount_work_number as work_number from work_topup_harvest wth " +
            "  left join dao d on wth.project_id = d.project_id " +
            "  where wth.project_id =#{projectId} and d.is_together_dao=1;")
    List<TopupNftListVo> getTopupRewardNftList(String projectId);

    // 获取一条记录
    @Select("select * from work_topup_harvest  where is_del = 0 and project_id=#{projectId} and mount_erc721_address = #{erc721Address} and mount_work_number=#{workNumber} ;")
    WorkTopupHarvest selectByProjectIdAndNft(String projectId, String erc721Address, String workNumber);


    @Select("select ifnull(sum(input_token_amount),0) as onChainEthBalance ,ifnull(sum(output_token_amount),0) as onChainTokenBalance from work_topup_harvest where project_id=#{projectId} ")
    UserTopupBalanceVo selectSumOnChainTokenByProjectId(String projectId);


    @Select("select count(distinct w.owner_address) from " +
            " (select * from work_topup_harvest where project_id=#{projectId} and (output_token_amount>0 or input_token_amount>0) ) t " +
            " inner join work w on t.mount_work_id = w.id ")
    Integer getTopupHoldersByProjectId(String projectId);


//    @Select("select any_value(t.project_id) as projectId,w.owner_address as userAddress from work_topup_harvest t " +
//            "  inner join work w on t.work_id = w.id " +
//            "  inner join dao d on w.dao_id=d.id " +
//            "  where w.owner_address = #{address} and w.drb_number=d.current_round and w.work_status=1 and w.is_del=0 " + // and (t.erc20_amount=0 and t.eth_amount=0)
//            "  group by t.dao_id order by t.dao_id desc;")
//    @Select("select any_value(COALESCE(d.exist_dao_id, d.project_id)) as projectId,w.owner_address as userAddress from work  w " +
//            "  inner join dao d on w.dao_id = d.id " +
//            "  where w.owner_address=#{address} and d.topup_mode=1  and w.drb_number=d.current_round and w.work_status=1  and w.is_del=0 and w.mount_work_id is not null" +
//            "  group by d.id order by d.id desc; ")

//    @Select("select any_value(COALESCE(d.exist_dao_id, d.project_id)) as projectId,any_value(w2.owner_address) as userAddress,d.id " +
//        "from work_topup_harvest wth " +
//        "inner join work w1 on wth.work_id=w1.id " +
//        "inner join work w2 on wth.mount_work_id=w2.id " +
//        "inner join dao d on w1.dao_id = d.id " +
//        "where w2.owner_address=#{address} and d.topup_mode=1 and w1.drb_number=d.current_round and w2.work_status=1 and w2.is_del=0 " +
//        "group by d.id order by d.id desc;")

    @Select("select any_value(COALESCE(d1.exist_dao_id, d1.project_id)) as projectId ,any_value(w2.owner_address) as userAddress,d1.id " +
            "from " +
            "work w1 " +
            "inner join (select * from dao where topup_mode=1 and is_together_dao=0) d1 on w1.dao_id = d1.id " +
            "inner join (select * from work where owner_address=#{address}) w2 on w1.mount_work_id = w2.id " +
            "inner join dao d2 on d2.id = w2.dao_id " +
            "where w1.drb_number=d1.current_round group by d1.id order by d1.id desc; ")
    List<DaoProjectVo> selectPendingBalanceByAddress(String address);


    //    @Select("select t.work_id as mintedWorkId,w1.dao_id as mintedDaoId,w1.work_number as mintedWorkNumber,d1.dao_name as mintedDaoName,w1.minted_price as mintFee, d1.pay_currency_type as mintedDaoPayCurrencyType, d1.input_token_address as mintedDaoInputTokenAddress," +
//            "       t.mount_work_id as voucherWorkId,w2.dao_id as voucherDaoId,w2.work_number as voucherWorkNumber,d2.dao_name as voucherDaoName,t.create_time as createTimestamp " +
//            "    from " +
//            "    (select * from work_topup_harvest where project_id=#{projectId} ) t " +    // and (erc20_amount=0 and eth_amount=0)
//            "    inner join work w1 on t.work_id=w1.id " +
//            "    inner join dao d1 on w1.dao_id = d1.id " +
//            "    inner join work w2 on t.mount_work_id=w2.id " +
//            "    inner join dao d2 on w2.dao_id = d2.id " +
//            "    where w2.owner_address=#{address} and w1.drb_number=d1.current_round order by t.create_time desc ")
//    @Select("select w1.id as mintedWorkId,w1.dao_id as mintedDaoId,w1.work_number as mintedWorkNumber,d1.dao_name as mintedDaoName,w1.minted_price as mintFee, d1.pay_currency_type as mintedDaoPayCurrencyType, d1.input_token_address as mintedDaoInputTokenAddress, " +
//            "     w2.id as voucherWorkId,w2.dao_id as voucherDaoId,w2.work_number as voucherWorkNumber,d2.dao_name as voucherDaoName,w1.create_time as createTimestamp " +
//            "    from  " +
//            "    (select * from work where owner_address=#{address}) w1 " +
//            "    inner join (select * from dao where project_id=#{projectId} || exist_dao_id=#{projectId} and topup_mode=1 and is_together_dao=0) d1 on w1.dao_id = d1.id " +
//            "    inner join work w2 on w1.mount_work_id = w2.id " +
//            "    inner join dao d2 on d2.id = w2.dao_id " +
//            "    where w1.drb_number=d1.current_round order by w1.create_time desc;")
    @Select("select w1.id as mintedWorkId,w1.dao_id as mintedDaoId,w1.work_number as mintedWorkNumber,d1.dao_name as mintedDaoName,w1.minted_price as mintFee, d1.pay_currency_type as mintedDaoPayCurrencyType, d1.input_token_address as mintedDaoInputTokenAddress, " +
            "    w2.id as voucherWorkId,w2.dao_id as voucherDaoId,w2.work_number as voucherWorkNumber,d2.dao_name as voucherDaoName,w1.create_time as createTimestamp " +
            "    from  " +
            "    work w1 " +
            "    inner join (select * from dao where project_id=#{projectId} || exist_dao_id=#{projectId} and topup_mode=1 and is_together_dao=0) d1 on w1.dao_id = d1.id " +
            "    inner join (select * from work where owner_address=#{address}) w2 on w1.mount_work_id = w2.id " +
            "    inner join dao d2 on d2.id = w2.dao_id " +
            "    where w1.drb_number=d1.current_round order by w1.create_time desc;")
    List<UserTopupBalancePendingDetailVo> selectTopUpPendingSeeMore(String address, String projectId);


    @Select(" select IF(LEFT(d.project_id, 2) = '0x', d.project_id, CONCAT('0x', d.project_id)) AS projectId, d.erc721_token as erc721Token ,w.work_number as workNumber from work_topup_harvest wth " +
            "  left join work w on wth.mount_work_id = w.id " +
            "  left join (select * from dao where is_together_dao=0) d on w.project_id = d.project_id " +
            " where w.owner_address=#{address} and  wth.project_id = #{projectId} and w.work_status=1 and w.is_del=0; ")
    List<TopupNftListVo> getTopupNftListVoByProjectAndAddress(String projectId, String address);


    @Select("select * from work_topup_harvest where mount_erc721_address=#{erc721Address} and mount_work_number=#{workNumber} limit 1;")
    WorkTopupHarvest selectOneByNft(String erc721Address, String workNumber);

}
