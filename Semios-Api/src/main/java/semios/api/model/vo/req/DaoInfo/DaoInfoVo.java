package semios.api.model.vo.req.DaoInfo;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.entity.Dao;
import semios.api.service.IDaoService;
import semios.api.utils.CommonUtil;
import semios.api.utils.SpringBeanUtil;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.List;

/**
 * @description: DAO详情
 * @author: xiangbin
 * @create: 2022-08-04 14:45
 **/
@Slf4j
@Data
public class DaoInfoVo implements Serializable {

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
     * 需要调整：展示为聚合dao名称
     */
    private String erc20Name;


    /**
     * 收藏数量
     */
    private Integer favoriteAmount = 0;

    /**
     * 是否被当前用户收藏
     */
    private Boolean favorited = false;

    /**
     * 1.7 支付货币类型
     */
    private String payCurrencyType;

    /**
     * 1.7 input token的logo地址
     */
    private String inputTokenLogo;

    /**
     * 1.7 input token的address
     */
    private String inputTokenAddress;

    /**
     * 1.7 input token的decimals
     */
    private Integer inputTokenDecimals;


    /**
     * dao symbol
     */
    private String daoSymbol;

    /**
     * dao symbol
     */
    private String daoErc20Address;

    /**
     * Dao状态 0-未创建1-已创建未开始2-已开始3-已结束 4-已停机
     */
    private Integer daoStatus;


    public static DaoInfoVo transfer(Dao dao,List<Integer> favoritesIds) {
        IDaoService daoService = SpringBeanUtil.getBean(IDaoService.class);
        if (daoService==null){
            log.info("dao service is null");
            return new DaoInfoVo();
        }

        DaoInfoVo daoDetailVo = new DaoInfoVo();
        daoDetailVo.setDaoId(dao.getId());
        daoDetailVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
        daoDetailVo.setDaoName(dao.getDaoName());
        daoDetailVo.setDaoLogoUrl(dao.getDaoLogoUrl());
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
        daoDetailVo.setDaoNumber(dao.getDaoNumber());

        // 优化...
        Dao togetherDao = daoService.getById(dao.getTogetherDaoId());
        daoDetailVo.setErc20Name(togetherDao.getDaoName());   // 需求调整，改为聚合dao名称
        daoDetailVo.setFavoriteAmount(dao.getFavoriteAmount());
        daoDetailVo.setFavorited(favoritesIds.contains(dao.getId()));
        daoDetailVo.setPayCurrencyType(dao.getPayCurrencyType());
        daoDetailVo.setInputTokenLogo(dao.getInputTokenLogo());
        daoDetailVo.setInputTokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getInputTokenAddress()));
        daoDetailVo.setInputTokenDecimals(dao.getInputTokenDecimals());
        daoDetailVo.setDaoSymbol(dao.getDaoSymbol());
        daoDetailVo.setDaoErc20Address(CommonUtil.addHexPrefixIfNotExist(dao.getErc20Token()));
        daoDetailVo.setDaoStatus(dao.getDaoStatus());
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
