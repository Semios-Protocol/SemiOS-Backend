package semios.api.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.common.ResultList;
import semios.api.model.vo.req.NodePermission.NodePermission;
import semios.api.model.vo.req.NodePermission.SelectNftPermission;
import semios.api.model.vo.res.NodePermission.NftDetailPermission;
import semios.api.model.vo.res.NodePermission.NodeDetailPermission;
import semios.api.model.vo.res.NodePermission.NodePermissionInfo;
import semios.api.service.INodePermissionNftService;
import semios.api.utils.CookieUtil;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * 1.10 node 权限管理
 *
 * @author zhyyao
 * @order 100
 */
@Slf4j
@RestController
@RequestMapping("/permission")
public class NodePermissionNftController {


    @Autowired
    private INodePermissionNftService nodePermissionNftService;


    /**
     * 1.10 查询node操作权限
     * permissionType 不可以为空
     */
    @PostMapping(value = "/node/info")
    public Result<NodePermissionInfo> nodePermissionNft(@RequestBody(required = false) NodePermission nodePermission, HttpServletRequest request) {
        Result<NodePermissionInfo> result = new Result<>();

        if (nodePermission.getDaoId() == null || nodePermission.getPermissionType() == null) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            return result;
        }

        NodePermissionInfo nodePermissionInfo =
                nodePermissionNftService.selectNodePermissionNft(nodePermission.getDaoId(), nodePermission.getPermissionType());

        if (nodePermissionInfo == null){
            nodePermissionInfo = new NodePermissionInfo();
            nodePermissionInfo.setIsPermission(false);
            result.setData(nodePermissionInfo);
            return result;
        }

        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (userAddress != null) {
            nodePermissionInfo.setIsPermission(userAddress.equalsIgnoreCase(nodePermissionInfo.getOwnerAddress()));
        }

        result.setData(nodePermissionInfo);
        return result;
    }


    /**
     * 1.10 nft详情页面，展示这个nft绑定的所有node信息
     */
    @PostMapping(value = "/detail/work")
    public ResultList<NftDetailPermission> nftPermissionList(@RequestBody(required = false) SelectNftPermission selectNftPermission) {
        ResultList<NftDetailPermission> result = new ResultList<>();
        if (selectNftPermission.getWorkId() == null) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            return result;
        }

        Page<NftDetailPermission> iPage = new Page<>(selectNftPermission.getPageNo(), selectNftPermission.getPageSize());
        Page<NftDetailPermission> nftDetailPermissionPage = nodePermissionNftService.getNftPermissionList(iPage, selectNftPermission.getWorkId());

        result.setDataList(nftDetailPermissionPage.getRecords());

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(selectNftPermission.getPageNo());
        page.setPageSize(selectNftPermission.getPageSize());
        page.setCount(nftDetailPermissionPage.getTotal());
        result.setPage(page);
        return result;
    }


    /**
     * 1.10 node 详情页面，展示这个node的权限信息
     * permissionType不用传
     */
    @PostMapping(value = "/detail/node")
    public ResultList<NodeDetailPermission> nodePermissionList(@RequestBody(required = false) NodePermission nodePermission) {
        ResultList<NodeDetailPermission> result = new ResultList<>();

        if (nodePermission.getDaoId() == null) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            return result;
        }

        List<NodeDetailPermission> nodeDetailPermissionList = nodePermissionNftService.selectNodeDetailPermission(nodePermission.getDaoId());
        result.setDataList(nodeDetailPermissionList);

        return result;
    }


}
