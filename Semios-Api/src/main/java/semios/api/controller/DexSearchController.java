package semios.api.controller;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import semios.api.model.dto.common.ResultList;
import semios.api.model.dto.feign.DaoErc20Dto;
import semios.api.model.dto.feign.UserInfoDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.User;
import semios.api.service.IDaoService;
import semios.api.service.IUserService;
import semios.api.service.feign.ISubscriptionService;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * dex查询相关信息
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-05-19 15:53
 **/
@Slf4j
@RestController
@RequestMapping("/dex")
public class DexSearchController {

    @Autowired
    private IDaoService daoService;

    @Autowired
    private IUserService userService;

    @Autowired(required = false)
    private ISubscriptionService subscriptionService;

    @Value("${user_profile_image}")
    private String headImage;

    /**
     *
     * @PostMapping("/dex/user") ResultList<UserInfoDto> getUserInfo(@RequestBody(required = true) List<String>
     * userAddressList);
     */
    /**
     * 通过erc20地址查询dao相关信息
     */
    @PostMapping(value = "/dao_erc20")
    public ResultList<DaoErc20Dto> searchErc20Info(@RequestBody(required = true) List<String> erc20AddressList) {

        ResultList<DaoErc20Dto> result = new ResultList<>();
        if (erc20AddressList == null || erc20AddressList.size() == 0) {
            return result;
        }

        List<Dao> daoList = daoService.selectDaoByErc20TokenList(erc20AddressList);

        List<DaoErc20Dto> erc20SearchResVoList =
                daoList.stream().map(DaoErc20Dto::transfer).collect(Collectors.toList());

        result.setDataList(erc20SearchResVoList);
        return result;

    }

    /**
     * 通过userAddress查询用户相关信息
     */
    @PostMapping(value = "/user")
    public ResultList<UserInfoDto> searchUserInfo(@RequestBody(required = true) List<String> userAddressList) {

        ResultList<UserInfoDto> result = new ResultList<>();

        List<User> userList = userService.findUsersByUserAddress(userAddressList);
        Map<String, User> userMap = userList.stream().collect(Collectors.toMap(User::getUserAddress, v -> v));
        List<UserInfoDto> userListResVos = userAddressList.stream().map(v -> {
            User user = userMap.get(v);
            if (user == null) {
                user = new User();
                user.setUserAddress(v);
            }
            return UserInfoDto.transfer(user, headImage, subscriptionService);
        }).collect(Collectors.toList());

        result.setDataList(userListResVos);
        return result;

    }
}
