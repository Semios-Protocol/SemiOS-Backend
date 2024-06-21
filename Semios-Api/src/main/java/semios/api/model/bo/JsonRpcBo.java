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
public class JsonRpcBo {

    /**
     * jsonrpc
     */
    private String jsonrpc = "2.0";

    /**
     * method ankr_getTokenHolders或者ankr_getNFTHolders
     */
    private String method = "ankr_getTokenHolders";


    /**
     * params
     */
    private JsonRpcParamBo params;

    /**
     * id
     */
    private String id = "1";


}
