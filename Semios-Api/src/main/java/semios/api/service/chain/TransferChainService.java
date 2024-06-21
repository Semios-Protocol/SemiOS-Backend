package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.Work;
import semios.api.service.IDaoService;
import semios.api.service.IWorkService;
import semios.api.service.SubscriberChainService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.util.List;

/**
 * @description: 二次交易监听
 * @author: xiangbin
 * @create: 2022-08-25 10:22
 **/
@Slf4j
@Service
public class TransferChainService implements SubscriberChainService {

    @Autowired
    private IWorkService workService;

    @Autowired
    private IDaoService daoService;

    public static void main(String[] args) throws Exception {
        String data =
                "{\"address\":\"0x4ba4dd89d69a7a2145fb8c8b0199cca2a6bedf4e\",\"blockHash\":\"0x8fb39c7bd6695e3b85ebfef74a0f808e8f631e8cde6455cd226683f49e213d1e\",\"blockNumber\":\"0x7612fe\",\"blockIntNum\":7738110,\"data\":\"0x\",\"logIndex\":\"0x51\",\"removed\":\"false\",\"topics\":\"[\\\"0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef\\\",\\\"0x0000000000000000000000000000000000000000000000000000000000000000\\\",\\\"0x00000000000000000000000056dba60a326c8a1e1ed148486a2695884aa34e3b\\\",\\\"0x0000000000000000000000000000000000000000000000000000000000000001\\\"]\",\"transactionHash\":\"0x79051e79ada0cdbdb141b64c9462afc6905a6eed81b47ce4c0f3b6baf1e65124\",\"transactionIndex\":\"0xc\",\"subId\":8,\"blockTime\":\"1665300684\",\"contractAddress\":\"0x4ba4dd89d69a7a2145fb8c8b0199cca2a6bedf4e\"}";
        TransactionDto transactionDto = JacksonUtil.json2pojo(data, TransactionDto.class);
        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());

        String from = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(1)).toLowerCase());
        String to = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(2)).toLowerCase());
        String tokenId = CommonUtil.formatBytes32Address(topics.get(3));
        System.out.println(from);
        System.out.println(to);
        System.out.println(CommonUtil.hexToTenString(tokenId));
    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[TransferChainService] transactionDto:{}", JacksonUtil.obj2json(transactionDto));

        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());

        String from = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(1)).toLowerCase());
        String to = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(2)).toLowerCase());
        String tokenId = CommonUtil.formatBytes32Address(topics.get(3));

        String contractAddress = transactionDto.getContractAddress();

        Dao dao = daoService.selectDaoByErc721Token(contractAddress);
        if (dao == null) {
            log.error("[TransferChainService] dao is null transactionDto:{}", JacksonUtil.obj2json(transactionDto));
            throw new RuntimeException("dao is null");
        }
//        if (ProtoDaoConstant.ZERO_ADDRESS.equals(from)) {
//            log.info("[TransferChainService] from is zero transactionDto:{}", JacksonUtil.obj2json(transactionDto));
//            return;
//        }

        if (CommonUtil.addHexPrefixIfNotExist(dao.getTransactionHash()).equals(CommonUtil.addHexPrefixIfNotExist(transactionDto.getTransactionHash()))) {
            log.info("[TransferChainService] dao create now,have not work,return:{}", dao.getProjectId());
            return;
        }

        Work work = workService.selectWorkByNumber(dao.getId(), CommonUtil.hexToTenString(tokenId));
        if (work == null) {
            log.error("[TransferChainService] work is null transactionDto:{}", JacksonUtil.obj2json(transactionDto));
            throw new RuntimeException("work is null");
        }

        if (StringUtils.isBlank(work.getMintedAddress())) {
            log.error("[TransferChainService] work is not mint transactionDto:{}", JacksonUtil.obj2json(transactionDto));
            throw new RuntimeException("work is not mint");
        }

        //1.3 mintAndTransfer的情况
        if (ProtoDaoConstant.ZERO_ADDRESS.equals(from) && !work.getOwnerAddress().equals(to)) {
            log.info("[TransferChainService] work owner transfer from:{} to:{}", work.getOwnerAddress(), to);
            work.setOwnerAddress(to);
            workService.updateById(work);
        }

        if (!ProtoDaoConstant.ZERO_ADDRESS.equals(from) && !work.getOwnerAddress().equals(from)) {
            if (work.getOwnerAddress().equals(to)) {
                return;
            }
            log.error("[TransferChainService] workId:{} owner is different transactionDto:{}", work.getId(),
                    JacksonUtil.obj2json(transactionDto));
            throw new RuntimeException("work owner is different");
        }
        log.info("[TransferChainService] workId:{} owner transfer from:{} to:{}", work.getId(), work.getOwnerAddress(), to);
        work.setOwnerAddress(to);
        workService.updateById(work);

    }
}
