package semios.api.model.vo;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.entity.Dao;
import semios.api.utils.CommonUtil;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;

/**
 * @description: DAO详情
 * @author: xiangbin
 * @create: 2022-08-04 14:45
 **/
@Slf4j
@Data
public class TreasuryTogetherDaoListVo implements Serializable {

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
     * dao的logo地址
     */
    private String daoLogoUrl;


    /**
     * dao描述
     */
    private String daoDescription;

    /**
     * dao宣言
     */
    private String daoManitesto;

    /**
     * DAO编号
     */
    private Integer daoNumber;


    /**
     * erc20 name 例：D4A Token for D4A@1
     */
    private String erc20Name;

    /**
     * sub dao的owner address
     */
    private String ownerAddress;


    public static TreasuryTogetherDaoListVo transfer(Dao dao) {
        TreasuryTogetherDaoListVo daoDetailVo = new TreasuryTogetherDaoListVo();
        daoDetailVo.setDaoId(dao.getId());
        daoDetailVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
        if (StringUtils.isBlank(dao.getDaoDescription())) {
            daoDetailVo.setDaoDescription("");
        } else {
            daoDetailVo.setDaoDescription(dao.getDaoDescription());
        }
        if (StringUtils.isBlank(dao.getDaoManitesto())) {
            daoDetailVo.setDaoManitesto("");
        } else {
            daoDetailVo.setDaoManitesto(dao.getDaoManitesto());
        }
        daoDetailVo.setDaoName(dao.getDaoName());
        daoDetailVo.setDaoLogoUrl(dao.getDaoLogoUrl());
        daoDetailVo.setErc20Name(dao.getErc20Name());
        daoDetailVo.setDaoNumber(dao.getDaoNumber());
        daoDetailVo.setOwnerAddress(dao.getOwnerAddress());
        return daoDetailVo;
    }

    public static void main(String[] args) {

        LocalDate startDate = LocalDate.of(2023, 4, 18);
        long localTime = LocalDateTime.now().toInstant(ZoneOffset.of("+8")).getEpochSecond();
        long startDateTime = startDate.atStartOfDay().plusHours(15).toInstant(ZoneOffset.of("+8")).getEpochSecond();
        System.out.println(startDateTime);
        System.out.println(localTime);
        System.out.println(startDateTime - localTime);
    }
}
