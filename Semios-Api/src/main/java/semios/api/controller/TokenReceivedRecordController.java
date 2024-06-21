package semios.api.controller;


import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.entity.Dao;
import semios.api.model.entity.TokenReceivedRecord;
import semios.api.model.entity.User;
import semios.api.model.vo.req.UserProfilePageReqVo;
import semios.api.model.vo.res.TokenRecordListVo;
import semios.api.service.IDaoService;
import semios.api.service.ITokenReceivedRecordService;
import semios.api.service.IUserService;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 代币领取记录相关接口
 *
 * @author xiangbin
 * @order 8
 */
@RestController
@RequestMapping("/token")
public class TokenReceivedRecordController {


    @Autowired
    private ITokenReceivedRecordService tokenReceivedRecordService;

    @Autowired
    private IDaoService daoService;

    @Autowired
    private IUserService userService;

    /**
     * Activity 列表
     *
     * @param userProfilePageReqVo
     * @return
     */
    @PostMapping(value = "/activity")
    public Result<TokenRecordListVo> activity(@RequestBody(required = false) UserProfilePageReqVo userProfilePageReqVo) {

        Result<TokenRecordListVo> result = new Result<>();
        Page<TokenReceivedRecord> iPage = new Page<>(userProfilePageReqVo.getPageNo(), userProfilePageReqVo.getPageSize());
        Page<TokenReceivedRecord> tokenReceivedRecordPage = tokenReceivedRecordService.pageRecordList(iPage, userProfilePageReqVo.getUserAddress());
        List<TokenReceivedRecord> tokenReceivedRecordList = tokenReceivedRecordPage.getRecords();
        if (tokenReceivedRecordList.size() > 0) {
            Set<Integer> daoNumbers = tokenReceivedRecordList.stream().map(TokenReceivedRecord::getDaoNumber).collect(Collectors.toSet());
            List<Dao> daoList = daoService.selectDaoByDaoNumberList(new ArrayList<>(daoNumbers));
            Map<Integer, Dao> daoMap = daoList.stream().collect(Collectors.toMap(Dao::getDaoNumber, v -> v, (entity1, entity2) -> entity1));

            Set<String> userAddressList = new HashSet<>();
            Set<String> fromAddress = tokenReceivedRecordList.stream().filter(v -> StringUtils.isNotBlank(v.getFromAddress())).filter(v -> !(v.getFromAddress().equals(ProtoDaoConstant.ZERO_ADDRESS)) && !(v.getFromAddress().equals(ProtoDaoConstant.protocolContract))).map(TokenReceivedRecord::getFromAddress).collect(Collectors.toSet());
            Set<String> toAddress = tokenReceivedRecordList.stream().filter(v -> StringUtils.isNotBlank(v.getToAddress())).filter(v -> !(v.getToAddress().equals(ProtoDaoConstant.ZERO_ADDRESS)) && !(v.getToAddress().equals(ProtoDaoConstant.protocolContract))).map(TokenReceivedRecord::getToAddress).collect(Collectors.toSet());
            if (fromAddress.size() > 0) userAddressList.addAll(fromAddress);
            if (toAddress.size() > 0) userAddressList.addAll(toAddress);
            Map<String, String> userMap = new HashMap<>();
            if (userAddressList.size() > 0) {
                List<User> userList = userService.findUsersByUserAddress(new ArrayList<>(userAddressList));
                userMap = userList.stream().filter(v -> StringUtils.isNotBlank(v.getUserName())).collect(Collectors.toMap(User::getUserAddress, User::getUserName));
            }

            if (StringUtils.isNotBlank(userProfilePageReqVo.getUserAddress())) {
                userMap.put(userProfilePageReqVo.getUserAddress(), "you");
            }
            Map<String, String> userMap1 = userMap;
            List<TokenRecordListVo> tokenRecordListVos = tokenReceivedRecordList.stream().map(v -> TokenRecordListVo.transfer(v, daoMap, userMap1)).collect(Collectors.toList());
            result.setDataList(tokenRecordListVos);
        } else {
            result.setDataList(new ArrayList<>());
        }

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(userProfilePageReqVo.getPageNo());
        page.setPageSize(userProfilePageReqVo.getPageSize());
        page.setCount(tokenReceivedRecordPage.getTotal());
        result.setPage(page);

        return result;
    }


}
