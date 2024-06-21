package semios.dex.model.vo.req;

import lombok.Data;

import java.util.List;

/**
 * 查询条件
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-04-19 15:14
 **/
@Data
public class Erc20SearchReqVo {

    /**
     * erc20Address
     */
    private List<String> erc20Address;

}
