package semios.api.model.vo.res;

import lombok.Data;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoAllocationStrategy;
import semios.api.model.enums.DaoRoyaltyTypeEnum;
import semios.api.service.IDaoService;
import semios.api.utils.CommonUtil;
import semios.api.utils.SpringBeanUtil;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @description: DAO 的daoToken和ETH的分配
 * @author: xiangbin
 * @create: 2023-11-16 14:45
 **/
@Data
public class DaoAllocationVo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * dao id
     */
    private Integer daoId;

    /**
     * dao projectId
     */
    private String projectId;

    /**
     * dao名称
     */
    private String daoName;

    /**
     * DAO编号
     */
    private Integer daoNumber;

    /**
     * 分配比例
     */
    private BigDecimal royaltyProportion;

    /**
     * 分配类型 0-非当前dao 1-redeem Asset Pool 2-selfReward 3-不出块比例
     */
    private Integer royaltyType;

    public DaoAllocationVo() {
    }

    public DaoAllocationVo(String projectId) {
        this.projectId = projectId;
        this.royaltyType = DaoRoyaltyTypeEnum.THREE.getType();

    }

    public static DaoAllocationVo transfer(DaoAllocationStrategy daoAllocationStrategy) {

        DaoAllocationVo daoAllocationVo = new DaoAllocationVo();
        daoAllocationVo.setDaoId(daoAllocationStrategy.getDaoId());
        daoAllocationVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(daoAllocationStrategy.getProjectId()));
        if (DaoRoyaltyTypeEnum.ZERO.getType().equals(daoAllocationStrategy.getRoyaltyType())) {
            daoAllocationVo.setDaoName(daoAllocationStrategy.getDaoName());
        } else {
            DaoRoyaltyTypeEnum daoRoyaltyTypeEnum = DaoRoyaltyTypeEnum.getEnumByType(daoAllocationStrategy.getRoyaltyType());
            if (daoRoyaltyTypeEnum != null) {
                daoAllocationVo.setDaoName(daoRoyaltyTypeEnum.getDesc());
            } else {
                daoAllocationVo.setDaoName(daoAllocationStrategy.getDaoName());
            }
        }
        daoAllocationVo.setDaoNumber(daoAllocationStrategy.getDaoNumber());
        daoAllocationVo.setRoyaltyProportion(daoAllocationStrategy.getRoyaltyProportion());
        daoAllocationVo.setRoyaltyType(daoAllocationStrategy.getRoyaltyType());
        return daoAllocationVo;

    }

    public static DaoAllocationVo transferOriginProject(DaoAllocationStrategy daoAllocationStrategy) {

        IDaoService daoService = SpringBeanUtil.getBean(IDaoService.class);
        Dao dao = null;
        if (daoService != null) {
            dao = daoService.daoDetailByProjectId(daoAllocationStrategy.getOriginProjectId());
        }
        DaoAllocationVo daoAllocationVo = new DaoAllocationVo();
        if (dao != null) {
            daoAllocationVo.setDaoId(dao.getId());
            daoAllocationVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
            daoAllocationVo.setDaoName(dao.getDaoName());
            daoAllocationVo.setDaoNumber(dao.getDaoNumber());
            daoAllocationVo.setRoyaltyProportion(daoAllocationStrategy.getRoyaltyProportion());
            daoAllocationVo.setRoyaltyType(daoAllocationStrategy.getRoyaltyType());
        }


        return daoAllocationVo;

    }

}
