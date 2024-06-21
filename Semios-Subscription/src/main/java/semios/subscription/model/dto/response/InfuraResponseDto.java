package semios.subscription.model.dto.response;

import lombok.Data;


/**
 * @description: response
 * @author: xiangbin
 * @create: 2022-04-13 16:03
 **/
@Data
public class InfuraResponseDto {

    private String jsonrpc = "2.0";

    private String result;

    private Integer id = 1;

    private InfuraErrorInfoDto error;


}
