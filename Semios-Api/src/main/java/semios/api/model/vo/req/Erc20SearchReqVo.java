package semios.api.model.vo.req;

import lombok.Data;

import java.util.List;

/**
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
