package semios.api.model.vo.req.NodePermission;

import lombok.Data;
import semios.api.model.vo.PageVo;

/**
 * @description: 更改权限，下拉展示所有Nft参数
 * @author: xiangbin
 * @create: 2022-08-04 17:03
 **/
@Data
public class SelectNftPermission extends PageVo {

    /**
     * 当前的work id
     *
     * @required
     */
    private Integer workId;

    /**
     * 当前登陆用户地址
     *
     * @ignore
     */
    private String userAddress;

}
