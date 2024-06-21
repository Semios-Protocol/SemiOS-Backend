package semios.api.model.dto.request;

import lombok.Data;
import semios.api.model.dto.response.TransactionDto;

import java.util.List;

/**
 * @description: response
 * @author: xiangbin
 * @create: 2022-04-20 19:09
 **/
@Data
public class TransactionCallRequestDto {

    private List<TransactionDto> result;

    private String timestamp;
}
