package semios.api.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;
import org.apache.ibatis.annotations.Update;
import semios.api.model.bo.DaoAnalyticsBo;
import semios.api.model.bo.WorkCountBo;
import semios.api.model.entity.Work;
import semios.api.model.vo.req.CanvasSortedReqVo;
import semios.api.model.vo.req.DaoSortedReqVo;
import semios.api.model.vo.res.BaseWorkVo.WorkNftDetailsVo;
import semios.api.model.vo.res.MineNftVo;

import java.util.List;

/**
 * <p>
 * work作品 Mapper 接口
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface WorkMapper extends BaseMapper<Work> {

    @Select("select count(id) from `work` where is_del = 0 and work_status !=2 and (creator_address like concat('%', #{searchId}, '%') or work_description like concat('%', #{searchId}, '%') or work_hash = #{searchId})")
    Integer searchAmount(String searchId);

    @Select("select *,\n" + "case\n" + "when work_hash = #{searchId} then 3\n"
            + "when creator_address like concat('%', #{searchId}, '%') then 2\n"
            + "when work_description like concat('%', #{searchId}, '%') then 1\n" + "end as ord\n"
            + "from `work` where is_del = 0 and work_status !=2 and (creator_address like concat('%', #{searchId}, '%') or work_description like concat('%', #{searchId}, '%') or work_hash = #{searchId}) order by ord desc,block_time desc")
    List<Work> searchWork(String searchId);

    Page<Work> selectNftByProjectId(@Param("page") IPage<Work> page,
                                    @Param("daoSortedReqVo") DaoSortedReqVo daoSortedReqVo);

    Page<Work> selectUnmintedWorkByProjectId(@Param("page") IPage<Work> page,
                                             @Param("daoSortedReqVo") DaoSortedReqVo daoSortedReqVo);

    Page<Work> selectDrbNftByProjectId(IPage<Work> page, DaoSortedReqVo daoSortedReqVo);

    // @Select("select * from `work` where is_del = 0 and canvas_id = #{canvasId} and work_status = 1 order by
    // block_time desc")
    // Page<Work> selectNftByCanvasId(IPage<Work> page, @Param("canvasSortedReqVo") CanvasSortedReqVo
    // canvasSortedReqVo);

    // Page<Work> selectUnmintedWorkByCanvasId(IPage<Work> page,
    // @Param("canvasSortedReqVo") CanvasSortedReqVo canvasSortedReqVo);

    // @Select("select * from `work` where is_del = 0 and canvas_id = #{canvasId} and work_status = 1 and drb_number =
    // #{drb} order by block_time desc")
    // Page<Work> selectDrbNftByCanvasId(IPage<Work> page, String canvasId, String drb);

    Page<Work> selectWorkByCanvasId(IPage<Work> page, @Param("canvasSortedReqVo") CanvasSortedReqVo canvasSortedReqVo);

    Page<Work> findFavoritesByUserAddress(IPage<Work> page, String userAddress);

    @Select("select * from `work` where is_del = 0 and id = #{workId}")
    Work selectWorkById(String workId);

    @Select("select * from `work` where is_del = 0 and work_number = #{workNumber} and dao_id = #{daoId}")
    Work selectWorkByNumber(Integer daoId, String workNumber);

    Page<Work> unmintedWorks(@Param("page") IPage<Work> page, @Param("daoSortedReqVo") DaoSortedReqVo daoSortedReqVo);

    Page<Work> selectNfts(@Param("page") IPage<Work> page, @Param("daoSortedReqVo") DaoSortedReqVo daoSortedReqVo);

    Page<Work> selectDrbNfts(@Param("page") IPage<Work> page, @Param("daoSortedReqVo") DaoSortedReqVo daoSortedReqVo);

    /**
     * 按Minted Price降序排列，价格相同的按创建时间降序排列
     */
    Page<Work> rankingNfts(IPage<Work> page);

    Page<Work> findHoldByUserAddress(IPage<Work> page, String userAddress);

    Page<Work> findMintedByUserAddress(IPage<Work> page, String userAddress);

    Page<Work> findCreatorByUserAddress(IPage<Work> page, String userAddress);

    int deleteWorkByIds(List<String> workIds);

    @Select("select * from `work` where is_del = 0 and work_uri = #{workUri} and is_del = 0 limit 1")
    Work selectWorkByUri(String workUri);

    @Select("select * from `work` where is_del = 0 and work_hash = #{workHash} and is_del = 0 limit 1")
    Work selectWorkByHash(String workHash);

    @Select("select count(distinct owner_address) from `work` where is_del = 0 and dao_id =  #{daoId} and work_status = 1;")
    Integer selectNftOwners(String daoId);

    @Select("select count(id) from `work` where is_del = 0 and dao_id = #{daoId} and work_status = 1")
    Integer selectNftAmounts(String daoId);

    @Select("select count(id) from `work` where is_del = 0 and dao_id = #{daoId}")
    Integer selectWorkAmounts(String daoId);

    @Select("select count(distinct owner_address) from `work` where is_del = 0 and canvas_id = #{canvasId} and work_status = 1")
    Integer selectNftOwnersByCanvasId(String canvasId);

    @Select("select count(id) from `work` where is_del = 0 and canvas_id = #{canvasId} and work_status = 1")
    Integer selectNftAmountsByCanvasId(String canvasId);

    @Select("select count(id) from `work` where is_del = 0 and canvas_id = #{canvasId} and work_status in (0,1)")
    Integer selectWorkAmountsByCanvasId(String canvasId);

    @Select("select sum(minted_price) from `work` where is_del = 0 and can_id = #{canvasId} and work_status = 1 and drb_number = #{drb}")
    Double selectNftMintedPriceByDrbAndCanId(String canvasId, String drb);

    @Select("select * from `work` where is_del = 0 and work_status = #{status} and dao_id = #{daoId}")
    List<Work> selectWorksByDaoIdAndStatus(String daoId, Integer status);

    @Select("select * from `work` where is_del = 0 and work_status = 1 and block_time >= #{nearlyTime}")
    List<Work> selectNftNearlyTwoHours(Long nearlyTime);

    @Select("select * from `work` where is_del = 0 and work_status <= 1 and dao_number = #{daoNumber} and canvas_number = #{canvasNumber} and work_number = #{workNumber}")
    Work selectByNumber(Long daoNumber, Long canvasNumber, Long workNumber);

    @Select("select * from `work` where is_del = 0 and work_status = 1 and owner_address = #{userAddress} and dao_id = #{daoId} order by block_time desc limit 1")
    Work findHoldByAddressAndDaoId(String userAddress, Integer daoId);

    @Select("select * from `work` where is_del = 0 and work_status = #{daoAnalyticsBo.workStatus} and dao_id = #{daoAnalyticsBo.daoId} and DATE_FORMAT(FROM_UNIXTIME(block_time),'%Y-%m-%d') >= #{daoAnalyticsBo.startDate} and DATE_FORMAT(FROM_UNIXTIME(block_time),'%Y-%m-%d') <= #{daoAnalyticsBo.endDate}")
    List<Work> selecWorkForAnalytics(@Param("daoAnalyticsBo") DaoAnalyticsBo daoAnalyticsBo);

    @Select("select owner_address,count(id) as workAmount from `work` where is_del = 0 and work_status = #{daoAnalyticsBo.workStatus} and dao_id = #{daoAnalyticsBo.daoId} group by owner_address order by count(id) desc,sum(block_time) desc limit 100")
    List<Work> selecWorkForTopOwnersAmount(@Param("daoAnalyticsBo") DaoAnalyticsBo daoAnalyticsBo);

    @Select("select * from `work` where is_del = 0 and work_status <= 1 and can_id = #{canvasId} order by work_status asc, block_time desc, id desc limit 3")
    List<Work> selectWorksForCanvasPic(String canvasId);

    @Select("select count(distinct owner_address) from `work` where is_del = 0 and dao_id =  #{daoId} and work_status = 1 and DATE_FORMAT(FROM_UNIXTIME(block_time),'%Y-%m-%d') <= #{endDate}")
    Integer selectNftOwnersByEndDate(String daoId, String endDate);

    @Select("select owner_address,count(id) as workAmount from `work` where is_del = 0 and work_status = #{daoAnalyticsBo.workStatus} and dao_id = #{daoAnalyticsBo.daoId} group by owner_address order by count(id) desc,sum(block_time) desc")
    List<Work> selecWorkForOwnersAmount(@Param("daoAnalyticsBo") DaoAnalyticsBo daoAnalyticsBo);

    @Select("select * from `work` where is_del = 0 and can_id = #{canvasId} and work_status = 1 and drb_number = #{drb}")
    List<Work> selectDrbNftByCanvasId(String canvasId, String drb);

    @Select("select count(*) from `work` where is_del = 0 and work_status = 1 and minted_address = #{userAddress} and dao_id = #{daoId} order by block_time desc")
    int selectCountHoldByAddressAndDaoId(String userAddress, Integer daoId);

    @Select("select count(*) from `work` where is_del = 0 and can_id = #{canvasId} and work_status = 1 and drb_number = #{drb}")
    int selectDrbNftCountByCanvasId(String canvasId, Integer drb);

    @Select("select count(*) from `work` where is_del = 0 and dao_id = #{daoId} and work_status = 1 and drb_number = #{drb}")
    int selectDrbNftCountByDaoId(String daoId, Integer drb);

    @Select("select count(*) from `work` where is_del = 0 and can_id = #{canvasId} and work_status = 1 and drb_number >= #{fromDrb} and drb_number <= #{endDrb}")
    int selectRangeNftCountByCanvasId(String canvasId, Integer fromDrb, Integer endDrb);

    @Select("select count(*) from `work` where is_del = 0 and dao_id = #{daoId} and work_status = 1 and drb_number >= #{fromDrb} and drb_number <= #{endDrb}")
    int selectRangeNftCountByDaoId(String daoId, Integer fromDrb, Integer endDrb);

    @Select("select dao_id from `work` where is_del = 0 and work_status = 1 and owner_address = #{userAddress} group by dao_id")
    List<Integer> selectDaoMemberWork(String userAddress);


    @Select("select * from `work` where is_del = 0 and generate = 1 and  work_status < 2 and dao_id = #{daoId}  order by id desc limit 1")
    Work selectLastGenerateWork(Integer daoId);

    @Select("select count(*) from `work` where is_del = 0 and work_status = 1 and minted_address = #{userAddress} and dao_id = #{daoId} order by block_time desc")
    int selectCountMintByAddressAndDaoId(String userAddress, Integer daoId);

    @Select("select count(1) from `work` where is_del = 0 and generate = 1 and  work_status =  1 and dao_id = #{daoId} ")
    int selectCountNftGenerateWork(Integer daoId);


    @Select("select * from `work` where is_del = 0 and work_status = 1 and drb_number = #{drb} and topup_mode = 1 order by block_time desc")
    List<Work> selectTopupWorkForCal(Integer drb);


    @Select("select count(distinct minted_address) as minters,count(id) as mintedWorks,sum(minted_price) as mintFee from `work` where is_del = 0 and dao_id = #{daoId} and work_status = 1 and drb_number = #{drb} ")
    WorkCountBo selectDrbNftOwnerCountByDaoId(String daoId, Integer drb);

    @Select("select * from `work` where is_del = 0 and dao_id = #{daoId} and work_status = 1 and drb_number = #{drb}")
    List<Work> selectDrbNftByDaoId(Integer daoId, Integer drb);

    List<Work> selectWorksByDaoIds(List<Integer> daoIds);


    @Select("select * from `work` where is_del = 0 and lock_status = 1 and work_status = 1 and CONVERT(lock_start_block, UNSIGNED) + CONVERT(lock_duration_block, UNSIGNED) <= CAST(#{blockNum} AS UNSIGNED)")
    List<Work> selectWorksByLockStatus(String blockNum);


    // 账户mint topup work筛选的 查询我拥有的nft
    // t.eth_amount,t.erc20_amount可能是个null,,加入IFNull
//    @Select("select w.id as workId,w.work_number,w.dao_id,d.dao_name,w.dao_number,w.canvas_number,d.erc721_token as erc721TokenAddress, " +
//            " w.image_url as imgUrl,w.bg_color,w.height,IFNULL(w.lock_status,0) as workLockStatus,IFNULL(sum(t.eth_amount) ,0) as ethAmount,IFNULL(sum(t.erc20_amount) ,0) as erc20Amount,d.together_dao_id ,d.pay_currency_type,d.input_token_address ,d.dao_symbol,d.erc20_token as daoErc20Address,d.erc20_payment_mode as erc20PaymentMode FROM work as w " +
//            " left join work_topup_harvest t on w.id = t.mount_work_id " +
//            " left join dao d on w.dao_id=d.id " +
//            " where t.project_id=#{projectId} and w.owner_address=#{address} and w.work_status = 1 group by w.id order by ethAmount desc,erc20Amount desc,w.block_time desc ")
//
    @Select("select w.id as workId,w.work_number,w.dao_id,d.dao_name,w.dao_number,w.canvas_number,d.erc721_token as erc721TokenAddress, " +
            "  w.image_url as imgUrl,w.bg_color,w.height,IFNULL(w.lock_status,0) as workLockStatus, " +
            "  d.together_dao_id ,d.pay_currency_type,d.input_token_address ,d.dao_symbol,d.erc20_token as daoErc20Address,d.erc20_payment_mode as erc20PaymentMode, " +
            "  IFNULL(wth.eth_amount ,0) as ethAmount,IFNULL(wth.erc20_amount ,0) as erc20Amount " +
            "  from (select * from work where owner_address=#{address} and work_status = 1) w " +
            "  left join (select * from  work_topup_harvest where project_id=#{projectId}) wth on wth.mount_work_id = w.id " +
            "   left join dao d on d.id=w.dao_id " +
            " order by ethAmount desc,erc20Amount desc,w.block_time desc")
    Page<MineNftVo> selectWorkMintTopUp(IPage<Work> page, String address, String projectId);

    // 账户mint not topup work筛选的，查询在当前dao下有mount_work是我的
    @Select("select w.id as workID,w.work_number,w.dao_id,d.dao_name,w.dao_number,w.canvas_number,d.erc721_token as erc721TokenAddress, " +
            " w.image_url as imgUrl,w.bg_color,w.height,IFNULL(w.lock_status,0) as workLockStatus,IFNULL(sum(t.eth_amount) ,0) as ethAmount,IFNULL(sum(t.erc20_amount) ,0) as erc20Amount,d.together_dao_id ,d.pay_currency_type,d.input_token_address ,d.dao_symbol,d.erc20_token as daoErc20Address ,d.erc20_payment_mode as erc20PaymentMode " +
            " from (select * from work_topup_harvest where project_id=#{projectId} and (erc20_amount>0 or eth_amount>0) ) t " +
            " left join work w on t.mount_work_id=w.id " +
            " left join dao d on w.dao_id = d.id " +
            " where w.work_status=1 and w.owner_address=#{address} group by w.id order by ethAmount desc,erc20Amount desc,w.block_time desc ")
    Page<MineNftVo> selectWorkMintNotTopUp(IPage<Work> page, String address, String projectId);

    // work详情下..查询谁绑定了我，按照dao分组并求和
    @Select("select d.id as daoId,d.dao_name,t.eth_amount as ethBalance,t.erc20_amount as tokenBalance,d.together_dao_id as togetherDaoId,d.pay_currency_type,d.input_token_address, d.input_token_decimals ,d.dao_symbol,d.erc20_token as daoErc20Address from " +
            " (select dao_id, sum(erc20_amount) as erc20_amount,sum(eth_amount) as eth_amount from work_topup_harvest where mount_work_id=#{workId} group by dao_id) t " +
            "left join dao d on t.dao_id=d.id " +
            " where (t.erc20_amount >0 or t.eth_amount>0) " +
            " order by t.eth_amount desc,t.erc20_amount desc ")
    Page<WorkNftDetailsVo> workDetailNft(IPage<Work> page, String workId);


    // 修改work未铸造且是默认图片的url
    @Update("update work set image_url=#{newImageUrl} where dao_id=#{daoId} and work_status=0 and image_url=#{oldImageUrl}")
    void updateWorkUrl(Integer daoId, String oldImageUrl, String newImageUrl);

    @Update("update work set image_url=#{newImageUrl} where dao_id=#{daoId} and work_status=0 and generate=1")
    void updatePassCardUrl(Integer daoId, String newImageUrl);
}
