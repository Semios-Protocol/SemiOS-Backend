package semios.api.model.dto.response;

import lombok.Data;

/**
 * 是否有创建canvas或者work的权限
 *
 * @description: authority
 * @author: xiangbin
 * @create: 2023-03-01 17:26
 **/
@Data
public class DaoAuthorityResDto {

    /**
     * 是否有创建canvas权限 true-有 false-没有
     */
    private Boolean createCanvas = true;
    /**
     * 是否有mint work权限 true-有 false-没有
     */
    private Boolean mintWork = true;

}
