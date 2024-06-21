package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.User;
import semios.api.model.enums.RoleIndexEnum;
import semios.api.service.IUserService;
import semios.api.service.SubscriberChainService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.util.List;

/**
 * 取消授权事件
 *
 * @description:
 * @author: xiangbin
 * @create: 2022-08-25 13:43
 **/
@Slf4j
@Service
public class RevokeRoleChainService implements SubscriberChainService {

    @Autowired
    private IUserService userService;


    //https://goerli.etherscan.io/tx/0x329fd3a39c103e238c26a99ea7c07485c310ddda90613f868e0011fa7b389a60#eventlog


    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[RevokeRoleChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());

        String role = CommonUtil.addHexPrefixIfNotExist(topics.get(1).toLowerCase());
        String account = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(2)).toLowerCase());
        String sender = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(3)).toLowerCase());

        log.info("[RevokeRoleChainService] role:{} account:{} sender:{}", role, account, sender);

        User user = userService.findUserByAddressHash(account);
        if (user == null) {
            log.error("[RevokeRoleChainService] user not exist account:{}", account);
            throw new RuntimeException("user not exist");
        }

        Integer roleIndex = RoleIndexEnum.getIndexByHash(role);

        if (roleIndex.equals(RoleIndexEnum.OPERATION_ROLE.getIndex())) {
            user.setRole(RoleIndexEnum.NO_HAVE_ROLE.getIndex());
            user.setTransactionHash(transactionDto.getTransactionHash());

            userService.updateById(user);
        }
    }


}
