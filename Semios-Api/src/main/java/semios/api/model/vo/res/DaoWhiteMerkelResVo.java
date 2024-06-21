package semios.api.model.vo.res;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;


/**
 * dao的白名单merkel信息
 *
 * @description:
 * @author: xiangbin
 * @create: 2022-08-04 17:03
 **/
@Data
public class DaoWhiteMerkelResVo {


    /**
     * proof信息
     *
     * @mock ["0x6907129bbb5d3b218bf7830f0d38894795272123a76fdf9e3549e19ee9d65f59"]
     */
    private List<String> proof = new ArrayList<>();

    /**
     * proof_root_hash
     */
    private String rootHash;

}
