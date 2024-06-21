package semios.dex.model.vo.req;

import lombok.Data;

/**
 * @description: search
 * @author: xiangbin
 * @create: 2023-05-12 10:35
 **/
@Data
public class Erc20ReqVo {

    /**
     * erc20地址
     *
     * @required true
     * @mock 0x68533e9519f0997b06968dda7f7c9e58b5ba029a
     */
    private String erc20Address;
}
