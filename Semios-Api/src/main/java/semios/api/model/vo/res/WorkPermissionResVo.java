package semios.api.model.vo.res;

import lombok.Data;

/**
 * @description: 查询是否有铸造protoDao的权限
 * @author: xiangbin
 * @create: 2022-08-11 14:48
 **/
@Data
public class WorkPermissionResVo {

    /**
     * 是否有权限添加work
     */
    private Boolean permission;


}
