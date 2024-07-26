package semios.api.service.chain;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import semios.api.model.dto.request.NoticeSubValueDto;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.*;
import semios.api.model.enums.*;
import semios.api.service.*;
import semios.api.service.common.CommonService;
import semios.api.utils.CommonUtil;
import semios.api.utils.DateUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * dao round 变更事件
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-09-04 13:43
 **/
@Slf4j
@Service
public class DaoCurrentRoundChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    @Autowired
    private CommonService commonService;

    @Autowired
    private IDaoDrbStatisticsService daoDrbStatisticsService;

    @Autowired
    private ICanvasDrbStatisticsService canvasDrbStatisticsService;

    @Autowired
    private ICanvasService canvasService;

    @Autowired
    private IDaoDailyStatisticsService daoDailyStatisticsService;

    @Autowired
    private IWorkService workService;

    public static void main(String[] args) {

    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {
        throw new RuntimeException("DaoCurrentRoundChainService not exist handleTrade!");
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void handleTradeValue(NoticeSubValueDto noticeSubValueDto) throws Exception {

        log.info("[DaoCurrentRoundChainService] noticeSubValueDto:{}", JacksonUtil.obj2json(noticeSubValueDto));

        String topics = noticeSubValueDto.getTopics();
        topics = topics.replace(ContractMethodEnum.GET_DAO_CURRENT_ROUND.getMethodAddress(), "");
        String projectId = CommonUtil.removeHexPrefixIfExists(topics);

        Dao dao = daoService.daoDetailByProjectId(projectId);
        if (dao == null) {
            throw new RuntimeException("DaoCurrentRoundChainService cannot find dao");
        }

        //1如果dao已经结束后又重新开始，监听dao restart 事件
        // 2如果从有限模式变到无限模式，则从零开始
        Dao updateDao = new Dao();
        DaoDailyStatistics daoDailyStatistics = null;
        updateDao.setId(dao.getId());
        updateDao.setSubdaoAssetPoolBalance(commonService.erc20BalanceOf(dao.getErc20Token(), dao.getFeePool(), dao.getErc20TokenDecimals(), dao.getInputTokenDecimals()).toPlainString());
        String callValue = CommonUtil.hexToTenString(noticeSubValueDto.getValue());
        if (StringUtils.isNotBlank(callValue) && !callValue.equals(dao.getCurrentRound())) {
            String currendRound = dao.getCurrentRound();
            updateDao.setCurrentRound(callValue);
            //如果变化的callValue周期为1，则把dao置为开始
            if (!DaoStatusEnum.STARTED.getStatus().equals(dao.getDaoStatus())) {
                updateDao.setDaoStatus(DaoStatusEnum.STARTED.getStatus());
                log.info("[DaoCurrentRoundChainService] daoId:{} STARTED", dao.getId());
            }


            if (StringUtils.isNotBlank(currendRound)) {
                Integer currentRoundInt = Integer.valueOf(currendRound);
                DaoDrbStatistics daoDrbStatistics = daoDrbStatisticsService.selectByDaoIdAndDrbNumber(dao.getId(), currentRoundInt);
                if (daoDrbStatistics != null) {
                    daoDrbStatistics.setStatus(StatisticsStatusEnum.JSZ.getStatus());
                    daoDrbStatisticsService.updateById(daoDrbStatistics);

                    commonService.handleDaoDrbStatistics(Collections.singletonList(daoDrbStatistics), currentRoundInt);


                    List<CanvasDrbStatistics> canvasDrbStatisticsList = canvasDrbStatisticsService.selectByDrbNumberAndDaoId(currentRoundInt, dao.getId());
                    if (!canvasDrbStatisticsList.isEmpty()) {
                        canvasDrbStatisticsList.forEach(v -> v.setStatus(StatisticsStatusEnum.JSZ.getStatus()));
                        canvasDrbStatisticsService.updateBatchById(canvasDrbStatisticsList);

                        commonService.handleCanvasDrbStatistics(canvasDrbStatisticsList, currentRoundInt);
                        // 计算当前drb铸造收益
                        commonService.handleCurrentDrbMinterProfit(canvasDrbStatisticsList, currentRoundInt);
                    }
                }

                // 更新所有canvas的最近铸造价格
                commonService.updateCanvasPrice(currentRoundInt, dao.getId());

                if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
                    updateDao.setCanvasFloorPrice(dao.getGlobalDaoPrice());
                } else {
                    Canvas canvas = canvasService.listCanvasFloorPriceByDaoId(dao.getId() + "");
                    if (canvas != null && canvas.getCurrentPrice() != null) {
                        log.info("[DaoCurrentRoundChainService] daoId:{} price:{}", dao.getId(), canvas.getCurrentPrice());
                        if (canvas.getCurrentPrice().compareTo(dao.getCanvasFloorPrice()) != 0) {
                            log.info("[DaoCurrentRoundChainService] price is changed daoId:{} originPrice:{} price:{}", dao.getId(),
                                    dao.getCanvasFloorPrice(), canvas.getCurrentPrice());
                            updateDao.setCanvasFloorPrice(canvas.getCurrentPrice());

                        }
                    }
                }

                List<Work> workList = workService.selectTopupWorkForCal(currentRoundInt);
                workList = workList.stream().filter(v -> v.getDaoId().equals(dao.getId())).collect(Collectors.toList());
                //Map<String, String> map = new HashMap<>();
                log.info("[DaoCurrentRoundChainService] filter workList by dao:" + JacksonUtil.obj2json(workList));
                for (Work work : workList) {
//                    commonService.updateMinterTopupHarvest(dao.getProjectId(), work.getMintedAddress());
                    //if (StringUtils.isBlank(map.get(work.getMintedAddress()))) {
                    //map.put(work.getMintedAddress(), dao.getProjectId());
                    String existDaoId = StringUtils.isBlank(dao.getExistDaoId()) ? dao.getProjectId() : dao.getExistDaoId();
                    //commonService.updateMinterTopupHarvest(existDaoId, work.getMintedAddress());

                    // 1.5 获取到workA下绑定的workB的信息
                    // 在workA下查询workB的721...
                    // 此处的这个work的收益--对应的mountWork（谁绑定的这个work）
                    // 更新所有这个work和dao的收益信息
                    // 合约抛出的是dao下的work，而不是mount work.
                    // 查询主dao信息和绑定的mountWorkId
                    commonService.updateMinterWorkTopupHarvest(existDaoId, work.getMountWorkId());
                    //}
                }
                //map.clear();
            }

            updateDao.setRemainingMintWindow(commonService.getDaoRemainingRound(dao).intValue());
            //非无限模式，剩余mintWindow等于0 则dao结束
            if (TrueOrFalseEnum.FALSE.getStatus().equals(updateDao.getRemainingMintWindow())) {
                updateDao.setDaoStatus(DaoStatusEnum.FINISHED.getStatus());
                log.info("[DaoCurrentRoundChainService] daoId:{} FINISHED", dao.getId());
                //如果关闭的话插入一条统计记录
                Timestamp today = DateUtil.getBeginOfToday();
                long lastDayBeginHour = DateUtil.getTimestampAfterDay(today, 0).getTime() / 1000;
                DaoDailyStatistics daoStatistics = daoDailyStatisticsService.selectDaoDailyByProjectId(dao.getProjectId(), lastDayBeginHour);
                if (daoStatistics == null) {
                    daoDailyStatistics = new DaoDailyStatistics();
                    daoDailyStatistics.setDaoId(dao.getId());
                    daoDailyStatistics.setProjectId(dao.getProjectId());
                    daoDailyStatistics.setStatus(StatisticsStatusEnum.WJS.getStatus());
                    daoDailyStatistics.setRecordTime(lastDayBeginHour);
                }
            }
            //乐透模式记录上一个出块的drb
            if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getRoyaltyTokenLotteryMode())) {
                updateDao.setLastActiveRound(commonService.getDaoLastActiveRound(dao).intValue());
            }


            Integer drbNumber = Integer.valueOf(updateDao.getCurrentRound());

            //乐透模式生成drbStatics
            if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getRoyaltyTokenLotteryMode()) && !DaoStatusEnum.FINISHED.getStatus().equals(updateDao.getDaoStatus())) {
                try {
                    DaoDrbStatistics daoDrbStatistics1 = new DaoDrbStatistics();
                    daoDrbStatistics1.setFloorPrice(dao.getDaoFloorPrice());
                    if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
                        daoDrbStatistics1.setFloorPrice(dao.getGlobalDaoPrice());
                    }
                    daoDrbStatistics1.setDaoId(dao.getId());
                    daoDrbStatistics1.setStatus(StatisticsStatusEnum.WJS.getStatus());
                    daoDrbStatistics1.setDrbVol(BigDecimal.ZERO);
                    daoDrbStatistics1.setDrbVolExTax(BigDecimal.ZERO);

                    DaoDrbStatistics daoDrbStatistics = daoDrbStatisticsService.selectLastedDrbByDaoId(dao.getId());
                    if (daoDrbStatistics != null) {
                        daoDrbStatistics1.setFloorPrice(daoDrbStatistics.getFloorPrice());
                        daoDrbStatistics1.setCanvas(daoDrbStatistics.getCanvas());
                        daoDrbStatistics1.setOwners(daoDrbStatistics.getOwners());
                        daoDrbStatistics1.setNft(daoDrbStatistics.getNft());
                        daoDrbStatistics1.setWorks(daoDrbStatistics.getWorks());
                        daoDrbStatistics1.setMintRevenue(daoDrbStatistics.getMintRevenue());
                        daoDrbStatistics1.setMintRevenueExTax(daoDrbStatistics.getMintRevenueExTax());
                        if (daoDrbStatistics.getDaoReward() != null) {
                            daoDrbStatistics1.setDaoReward(daoDrbStatistics.getDaoReward());
                        }
                        if (daoDrbStatistics.getDaoAssetPool() != null) {
                            daoDrbStatistics1.setDaoAssetPool(daoDrbStatistics.getDaoAssetPool());
                        }
                        if (daoDrbStatistics.getMintRevenue() != null) {
                            daoDrbStatistics1.setMintRevenue(daoDrbStatistics.getMintRevenue());
                        }
                        if (daoDrbStatistics.getMintRevenueExTax() != null) {
                            daoDrbStatistics1.setMintRevenueExTax(daoDrbStatistics.getMintRevenueExTax());
                        }
                        //计算dre
                        if (daoDrbStatistics.getMintRevenue() != null) {
                            Page<DaoDrbStatistics> iPage = new Page<>(1, 10);
                            Page<DaoDrbStatistics> daoDrbStatisticsPage = daoDrbStatisticsService.selectByDaoId(iPage, dao.getId());
                            long blockNumber = daoDrbStatisticsPage.getTotal() + 1;
                            BigDecimal dre = new BigDecimal(dao.getDaoMintWindow())
                                    .divide(new BigDecimal(blockNumber), 4, RoundingMode.FLOOR).multiply(daoDrbStatistics.getMintRevenue());
                            daoDrbStatistics1.setDre(dre.setScale(4, RoundingMode.FLOOR).stripTrailingZeros().toPlainString());
                        }


                        //计算SevenDayDrbVol
                        Integer startDrb = drbNumber >= 6 ? drbNumber - 6 : 0;
                        List<DaoDrbStatistics> daoDrbStatisticsList1 = daoDrbStatisticsService.selectGalleryDao(startDrb, drbNumber);
                        BigDecimal sevenDayDrbVol = daoDrbStatisticsList1.stream().filter(v -> v.getDaoId().equals(dao.getId()))
                                .map(DaoDrbStatistics::getDrbVol).reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
                        daoDrbStatistics1.setSevenDayDrbVol(sevenDayDrbVol);
                    }

                    daoDrbStatistics1.setDrbNumber(drbNumber);
                    log.info("[DaoCurrentRoundChainService] createDrbStatistics daoId:{} drbNumber:{}", dao.getId(), drbNumber);
                    daoDrbStatisticsService.save(daoDrbStatistics1);
                } catch (Exception e) {
                    log.error("[DaoCurrentRoundChainService] exception createDrbStatistics daoId:{} drbNumber:{} e:", dao.getId(), drbNumber, e);
                }
            }

        }


        daoDailyStatisticsService.updateDaoAndSaveDaoDailyStatistics(updateDao, daoDailyStatistics);

        log.info("[DaoCurrentRoundChainService] updateDao:{}", JacksonUtil.obj2json(updateDao));
        /**
         * 1。 如果历史value为0，新值为1，则开始dao
         * 2。 如果值变更了，需要计算dao static 和canvas static 统计上一个window的值
         * 3。如果到达dao window总数了则关闭dao  function getDaoRemainingRound(bytes32 daoId) public view returns (uint256)
         * 4。
         */


//        if (StringUtils.isNotBlank(callValue) && StringUtils.isNotBlank(ProtoDaoConstant.CURRENT_ROUND) && !callValue.equals(ProtoDaoConstant.CURRENT_ROUND)) {
//            Integer currentRount = Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND);
//            ProtoDaoConstant.CURRENT_ROUND = callValue;
//            // 做drb结算，变更drb
//            //                SearchController.searchExecutor.execute(() -> commonService.currentDrbDaoStatistics(currentRount));
//            taskExecutor.execute(() -> commonService.currentDrbDaoStatistics(currentRount));
//            //这个需要查询当前dao的开始事件或者开始区块。
//            commonService.handleNextDrbStartBlock(Integer.valueOf(callValue));
//            //这个保留
//            commonService.updateTopupModeMinterHarvest(currentRount);
//        }


        // 如果当前dao的周期已经结束，则将该node下所有 未铸造 的work置未失效状态
        if (updateDao.getDaoStatus() != null && updateDao.getDaoStatus().equals(DaoStatusEnum.FINISHED.getStatus())) {
            log.info("[DaoCurrentRoundChainService] dao id=:{} is stop,update work not cast.", dao.getId());
            List<Work> workList =
                    workService.selectWorksByDaoIdAndStatus(dao.getId() + "", WorkStatusEnum.NOT_CAST.getStatus());
            if (!workList.isEmpty()) {
                workList.forEach(v -> {
                    v.setWorkStatus(WorkStatusEnum.EXPIRED.getStatus());
                });
                workService.updateBatchById(workList);
            }
        }


    }
}
