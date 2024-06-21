package semios.dex.model.vo.req;

import lombok.Data;

/**
 * 按照7天，30天，90天查询信息参数
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-04-19 15:14
 **/
@Data
public class Erc20QueryReqVo {

    /**
     * erc20地址
     *
     * @required true
     * @mock 0x68533e9519f0997b06968dda7f7c9e58b5ba029a
     */
    private String erc20Address;

    /**
     * 查询条件 7天-30天-90天
     *
     * @mock 7
     */
    private Integer dayTime = 7;

}
