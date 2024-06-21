package semios.dex.model.dto.response;

import lombok.Data;

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
