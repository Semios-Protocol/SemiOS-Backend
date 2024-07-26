package semios.api.model.dto.request;

import lombok.Data;

import java.util.List;

/**
 * @description: response
 * @author: xiangbin
 * @create: 2022-04-20 19:09
 **/
@Data
public class TransactionDataDto {

    private List<String> transactionHash;

    private String topicType;   // NewSemiDaoErc721Address
}
