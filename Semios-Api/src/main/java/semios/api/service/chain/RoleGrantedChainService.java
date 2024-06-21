package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.User;
import semios.api.model.enums.RoleIndexEnum;
import semios.api.service.IUserService;
import semios.api.service.SubscriberChainService;
import semios.api.service.common.CommonService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.util.List;

/**
 * 授权事件
 *
 * @description:
 * @author: xiangbin
 * @create: 2022-08-25 13:43
 **/
@Slf4j
@Service
public class RoleGrantedChainService implements SubscriberChainService {

    @Autowired
    private IUserService userService;

    @Autowired
    private CommonService commonService;

    public static void main(String[] args) throws Exception {

        String topicstr = "[\"0x2f8788117e7eff1d82e926ec794901d17c78024a50270940304540a733656f0d\",\"0x20296b01d0b6bd176f0c1e29644934c0047abf080dae43609a1bbc09e39bafdb\",\"0x000000000000000000000000f8baf7268f3daefe4135f7711473ae8b6c3b47d8\",\"0x000000000000000000000000f39fd6e51aad88f6f4ce6ab8827279cfffb92266\"]";
        List<String> topics = JacksonUtil.json2StringList(topicstr);
        String role = CommonUtil.addHexPrefixIfNotExist(topics.get(1).toLowerCase());
        String account = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(2)).toLowerCase());
        String sender = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(3)).toLowerCase());

        System.out.println("role:" + role);
        System.out.println("account:" + account);
        System.out.println("sender:" + sender);

    }

    //https://goerli.etherscan.io/tx/0xef747c96d0ca7f078fb3b69634db16cfe3b9f6e86519755b57d22d21556a9afa
    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[RoleGrantedChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());

        String role = CommonUtil.addHexPrefixIfNotExist(topics.get(1).toLowerCase());
        String account = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(2)).toLowerCase());
        String sender = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(3)).toLowerCase());

        log.info("[RoleGrantedChainService] role:{} account:{} sender:{}", role, account, sender);

        User user = userService.findUserByAddressHash(account);
        if (user == null) {
            user = commonService.newUser(account);
        }
        Integer roleIndex = RoleIndexEnum.getIndexByHash(role);
        if (roleIndex.equals(RoleIndexEnum.OPERATION_ROLE.getIndex())) {
            user.setRole(roleIndex);
            user.setTransactionHash(transactionDto.getTransactionHash());

            userService.saveOrUpdate(user);
        }

    }

}
