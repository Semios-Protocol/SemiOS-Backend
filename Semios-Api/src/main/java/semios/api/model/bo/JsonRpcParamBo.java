package semios.api.model.bo;

import lombok.Data;

/**
 * 请求multichain获取token的holder总数的参数
 *
 * @description:
 * @author: xiangbin
 * @create: 2024-01-09 15:51
 **/
@Data
public class JsonRpcParamBo {

    /**
     * blockchain
     */
    private String blockchain = "eth";

    /**
     * contractAddress
     */
    private String contractAddress = "";


    /**
     * pageSize
     */
    private Integer pageSize = 100;

    /**
     * syncCheck
     */
    private Boolean syncCheck = false;


}
