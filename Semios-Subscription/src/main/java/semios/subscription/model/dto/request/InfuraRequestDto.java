package semios.subscription.model.dto.request;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;

/**
 * @description: param
 * @author: xiangbin
 * @create: 2022-04-13 15:55
 **/
@Data
public class InfuraRequestDto {

    private String jsonrpc = "2.0";

    private String method;

    private List<Object> params = new ArrayList<>();

    private Integer id = 1;
}
