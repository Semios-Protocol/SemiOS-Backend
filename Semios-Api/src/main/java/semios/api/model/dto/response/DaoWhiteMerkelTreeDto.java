package semios.api.model.dto.response;

import lombok.Data;

import java.util.List;


/**
 * dao的白名单merkel信息
 *
 * @description:
 * @author: xiangbin
 * @create: 2022-08-04 17:03
 **/
@Data
public class DaoWhiteMerkelTreeDto {


    /**
     * address
     */
    private String address;

    /**
     * proof
     */
    private List<String> proof;

}
