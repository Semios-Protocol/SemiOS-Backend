package semios.api.model.vo.req;

import jdk.nashorn.internal.ir.annotations.Ignore;
import lombok.Data;
import semios.api.model.vo.RepeatVo;


/**
 * 保存basic dao页面换取uri参数
 *
 * @description: dao create
 * @author: xiangbin
 * @create: 2022-08-10 14:31
 **/
@Data
public class BasicDaoCreateReqVo extends RepeatVo {


    /**
     * dao名称
     */
    private String daoName;

    /**
     * dao开始DRB
     */
    private String daoStartDate;

    /**
     * dao nft总量
     */
    private String totalNftCasting;

    /**
     * dao 总的drb数量
     */
    private String daoMintWindow;

    /**
     * dao地板价
     */
    private String daoFloorPrice;

    /**
     * 1.4 duration 每一个mintableRound的持续时间
     */
    private Integer duration;

    /**
     * @ignore
     */
    @Ignore
    private String userAddress;


}
