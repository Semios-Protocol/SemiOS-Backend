package semios.subscription.model.dto.response;

import lombok.Data;


/**
 * @description: response
 * @author: xiangbin
 * @create: 2022-04-13 16:03
 **/
@Data
public class InfuraErrorInfoDto {

    private Integer code;
    private String message;
    private String data;


}
