package semios.subscription.model.dto.response;

import lombok.Data;

import java.util.List;


/**
 * @description: response
 * @author: xiangbin
 * @create: 2022-04-13 16:03
 **/
@Data
public class InfuraResponseTransactionListDto {

    private String jsonrpc = "2.0";

    private List<Object> result;

    private Integer id = 1;

    private InfuraErrorInfoDto error;


}
