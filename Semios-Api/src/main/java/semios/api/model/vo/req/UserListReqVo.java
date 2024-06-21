package semios.api.model.vo.req;

import lombok.Data;

import java.util.List;

/**
 * @description: user
 * @author: xiangbin
 * @create: 2023-05-21 10:29
 **/
@Data
public class UserListReqVo {

    /**
     * 用户地址
     */
    private List<String> userAddress;
}
