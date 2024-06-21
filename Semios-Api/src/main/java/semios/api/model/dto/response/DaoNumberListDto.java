package semios.api.model.dto.response;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;

/**
 * 返回0-109可用的daoNumber
 *
 * @description: dao number
 * @author: xiangbin
 * @create: 2023-01-12 16:35
 **/
@Data
public class DaoNumberListDto {

    /**
     * 可用的daoNumber
     */
    private List<Integer> daoNumberList = new ArrayList<>();


}
