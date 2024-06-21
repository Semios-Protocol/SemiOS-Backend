package semios.subscription.model.dto.response;

import lombok.Data;

import java.util.List;


/**
 * @description: response
 * @author: xiangbin
 * @create: 2022-04-13 16:03
 **/
@Data
public class InfuraTransactionDto {

    private String address;

    private String blockHash;

    private String blockNumber;

    private String data;

    private String logIndex;

    private String removed;

    private List<String> topics;

    private String transactionHash;

    private String transactionIndex;
}
