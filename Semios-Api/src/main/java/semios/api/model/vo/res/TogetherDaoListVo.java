package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoAllocationStrategy;
import semios.api.model.entity.Favorites;
import semios.api.model.enums.FavoriteTypeEnum;
import semios.api.service.IDaoAllocationStrategyService;
import semios.api.service.IDaoService;
import semios.api.service.IFavoritesService;
import semios.api.service.common.CommonService;
import semios.api.utils.CommonUtil;
import semios.api.utils.DateUtil;
import semios.api.utils.SpringBeanUtil;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @description: DAO详情
 * @author: xiangbin
 * @create: 2022-08-04 14:45
 **/
@Slf4j
@Data
public class TogetherDaoListVo implements Serializable {

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
     * Current mint window information
     */
    private MintWindowInfoVo mintWindowInfoVo;

    /**
     * 基础信息
     */
    private BasicInformationVo basicInformationVo;

    /**
     * ModeStatus
     */
    private ModeStatusVo modeStatusVo;

    /**
     * 从其他dao分配的ETH比例
     */
    private List<DaoAllocationVo> receivedEthFromOther = new ArrayList<>();

    /**
     * 当前dao分配给其他dao的ETH比例
     */
    private List<DaoAllocationVo> allocationEthToOtherDao = new ArrayList<>();

    /**
     * 从其他dao分配的Token比例
     */
    private List<DaoAllocationVo> receivedTokenFromOther = new ArrayList<>();

    /**
     * 当前dao分配给其他dao的Token比例
     */
    private List<DaoAllocationVo> allocationTokenToOtherDao = new ArrayList<>();


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


    public static TogetherDaoListVo transfer(Dao dao) {
        IDaoService daoService = SpringBeanUtil.getBean(IDaoService.class);
        if (daoService == null) {
            return new TogetherDaoListVo();
        }

        TogetherDaoListVo daoDetailVo = new TogetherDaoListVo();
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
        daoDetailVo.setPayCurrencyType(dao.getPayCurrencyType());
        daoDetailVo.setInputTokenLogo(dao.getInputTokenLogo());
        daoDetailVo.setInputTokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getInputTokenAddress()));
        daoDetailVo.setInputTokenDecimals(dao.getInputTokenDecimals());
        daoDetailVo.setDaoSymbol(dao.getDaoSymbol());
        daoDetailVo.setDaoErc20Address(CommonUtil.addHexPrefixIfNotExist(dao.getErc20Token()));

        Dao togetherDao = daoService.getById(dao.getTogetherDaoId());
        daoDetailVo.setErc20Name(togetherDao.getDaoName());   // 需求调整，改为聚合dao名称

        daoDetailVo.setDaoNumber(dao.getDaoNumber());
        return daoDetailVo;
    }


    // 转换成 TogetherDaoListVo
    // address 当前登陆用户,null-未登录
    public static TogetherDaoListVo transferTogetherDaoListVo(Dao dao, String address) {
        log.info("开始转换daoId={},的信息---{}", dao.getId(), DateUtil.getCurrentTimestamp());
        CommonService commonService = SpringBeanUtil.getBean(CommonService.class);
        IDaoAllocationStrategyService daoAllocationStrategyService = SpringBeanUtil.getBean(IDaoAllocationStrategyService.class);
        if (commonService == null || daoAllocationStrategyService == null) {
            return null;
        }
        TogetherDaoListVo togetherDaoMakerVo = TogetherDaoListVo.transfer(dao);

        // 实时查询erc20和eth数量
        log.info("查询daoId={},的BasicInformationVo信息---{}", dao.getId(), DateUtil.getCurrentTimestamp());
        togetherDaoMakerVo.setBasicInformationVo(commonService.getBasicInformationVo(dao));
        log.info("结束查询daoId={},的BasicInformationVo信息---{}", dao.getId(), DateUtil.getCurrentTimestamp());
        togetherDaoMakerVo.setMintWindowInfoVo(commonService.getMintWindowInfoVo(dao));
        log.info("结束查询daoId={},的MintWindowInfoVo信息---{}", dao.getId(), DateUtil.getCurrentTimestamp());
        togetherDaoMakerVo.setModeStatusVo(ModeStatusVo.transfer(dao));
        log.info("结束查询daoId={},的ModeStatusVo信息---{}", dao.getId(), DateUtil.getCurrentTimestamp());

        //查询当前dao分配给其他dao的分配信息
        log.info("开始-查询daoId={},查询当前dao分配给其他dao的分配信息---{}", dao.getId(), DateUtil.getCurrentTimestamp());
        List<DaoAllocationStrategy> daoAllocationStrategyList = daoAllocationStrategyService.selectByOriginProjectIdAndType(dao.getProjectId(), null);
        List<DaoAllocationVo> daoTokenAllocationVos = daoAllocationStrategyList.stream().filter(v -> v.getType() == 0 && v.getRoyaltyType() == 0).map(DaoAllocationVo::transfer).collect(Collectors.toList());
        log.info("结束查询daoId={},分配给其他dao的分配的Token信息---{}", dao.getId(), DateUtil.getCurrentTimestamp());
        List<DaoAllocationVo> daoEthAllocationVos = daoAllocationStrategyList.stream().filter(v -> v.getType() == 1 && v.getRoyaltyType() == 0).map(DaoAllocationVo::transfer).collect(Collectors.toList());
        log.info("结束查询daoId={},分配给其他dao的分配的Eth信息---{}", dao.getId(), DateUtil.getCurrentTimestamp());
        togetherDaoMakerVo.setAllocationTokenToOtherDao(daoTokenAllocationVos);
        togetherDaoMakerVo.setAllocationEthToOtherDao(daoEthAllocationVos);

        //查询其他dao分配给当前dao的分配信息
        log.info("开始-查询daoId={},查询其他dao分配给当前dao的分配信息---{}", dao.getId(), DateUtil.getCurrentTimestamp());
        List<DaoAllocationStrategy> daoAllocationStrategyToProject = daoAllocationStrategyService.selectByProjectIdAndType(dao.getProjectId(), null);
        List<DaoAllocationVo> receivedTokenFromOther = daoAllocationStrategyToProject.stream().filter(v -> v.getType() == 0 && v.getRoyaltyType() == 0).map(DaoAllocationVo::transferOriginProject).collect(Collectors.toList());
        log.info("结束查询daoId={},其他dao的分配的Token信息---{}", dao.getId(), DateUtil.getCurrentTimestamp());
        List<DaoAllocationVo> receivedEthFromOther = daoAllocationStrategyToProject.stream().filter(v -> v.getType() == 1 && v.getRoyaltyType() == 0).map(DaoAllocationVo::transferOriginProject).collect(Collectors.toList());
        log.info("结束查询daoId={},其他dao的分配的Eth信息---{}", dao.getId(), DateUtil.getCurrentTimestamp());

        togetherDaoMakerVo.setReceivedTokenFromOther(receivedTokenFromOther);
        togetherDaoMakerVo.setReceivedEthFromOther(receivedEthFromOther);

        // dao 收藏数
        togetherDaoMakerVo.setFavoriteAmount(dao.getFavoriteAmount());
        // 当前用户是否收藏
        if (StringUtils.isNotBlank(address)) {
            IFavoritesService favoritesService = SpringBeanUtil.getBean(IFavoritesService.class);
            if (favoritesService != null) {
                Favorites favorites = favoritesService.findByUserAddress(FavoriteTypeEnum.DAO_FAVORITE.getType(), dao.getId() + "", address);
                if (favorites != null) {
                    togetherDaoMakerVo.setFavorited(true);
                }
            }
        }
        log.info("转换结束daoId={},的信息", dao.getId());
        return togetherDaoMakerVo;
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
