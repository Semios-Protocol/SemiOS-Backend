package semios.api.service.chain;


import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.Work;
import semios.api.model.enums.WorkLockStatusEnum;
import semios.api.service.IDaoService;
import semios.api.service.IWorkService;
import semios.api.service.SubscriberChainService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import javax.annotation.Resource;
import java.util.List;

/*
 * 当work锁定时，会抛出此事件
 * event TopUpNftLock((address erc721Address,uint256 tokenId), uint256 lockStartBlock, uint256 duration);
 * erc721Address :dao的rec721地址
 * tokenId :workNumber
 * lockStartBlock :锁定时的区块高度
 * duration:锁定多少个区块
 * */
@Slf4j
@Service
public class TopUpNftLockChainService implements SubscriberChainService {


    @Resource
    private IWorkService workService;
    @Resource
    private IDaoService daoService;

    public static void main(String[] args) {

    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {
        log.info("[TopUpNftLockChainService.java]transactionDto:{}", JacksonUtil.obj2json(transactionDto));

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String erc721Address = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(0)));
        String tokenId = CommonUtil.hexToTenString(dataList.get(1));
        String lockStartBlock = CommonUtil.hexToTenString(dataList.get(2));
        String duration = CommonUtil.hexToTenString(dataList.get(3));

        Dao dao = daoService.selectDaoByErc721Token(erc721Address);
        if (dao == null) {
            log.info("[TopUpNftLockChainService.java]Get dao info is null:" + erc721Address);
            return;
        }

        Work work = workService.selectWorkByNumber(dao.getId(), tokenId);
        if (work == null) {
            log.info("[TopUpNftLockChainService.java]Get work info is null:daoID" + dao.getId() + " tokenId:" + tokenId);
            return;
        }

        work.setLockStatus(WorkLockStatusEnum.LOCK.getStatus());
        work.setLockStartBlock(lockStartBlock);
        work.setLockDurationBlock(duration);

        workService.updateById(work);
    }
}
