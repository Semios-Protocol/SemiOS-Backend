package semios.subscription.model.dto.request;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

import java.util.List;


/**
 * @description: params
 * @author: xiangbin
 * @create: 2022-04-13 18:48
 **/
@Data
public class InfuraParamsForFilterDto {


    private String address;
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    private String fromBlock;//an integer block number, or the string "latest", "earliest" or "pending"
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    private String toBlock;//an integer block number, or the string "latest", "earliest" or "pending"
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    private List<String> topics;


}
