package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.Dao;
import semios.api.model.entity.Work;
import semios.api.model.enums.WorkStatusEnum;
import semios.api.service.ICanvasService;
import semios.api.service.IDaoService;
import semios.api.service.IWorkService;
import semios.api.utils.CommonUtil;
import semios.api.utils.SpringBeanUtil;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @description: explore下的SeedNodes列表
 * @author: zhyyao
 * @create: 2024-03-14 14:45
 **/
@Slf4j
@Data
public class SeedNodesListVo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 聚合daoID
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
     * erc20 name 例：D4A Token for D4A@1
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
     * DAO member的人数信息
     */
    private TogetherDaoMemberVo togetherDaoMemberVo;


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

    public static SeedNodesListVo transfer(Dao dao) {
        SeedNodesListVo daoDetailVo = new SeedNodesListVo();
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

        daoDetailVo.setFavoriteAmount(dao.getFavoriteAmount());
        daoDetailVo.setFavorited(dao.getFavorited());

        daoDetailVo.setPayCurrencyType(dao.getPayCurrencyType());
        daoDetailVo.setInputTokenLogo(dao.getInputTokenLogo());
        daoDetailVo.setInputTokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getInputTokenAddress()));
        daoDetailVo.setInputTokenDecimals(dao.getInputTokenDecimals());

        daoDetailVo.setTogetherDaoMemberVo(getTogetherDaoMemberVo(dao.getId()));
        return daoDetailVo;
    }

    private static TogetherDaoMemberVo getTogetherDaoMemberVo(Integer daoId) {
        IDaoService daoService = SpringBeanUtil.getBean(IDaoService.class);
        ICanvasService canvasService = SpringBeanUtil.getBean(ICanvasService.class);
        IWorkService workService = SpringBeanUtil.getBean(IWorkService.class);
        if (daoService == null || canvasService == null || workService == null) {
            log.info("获取到的service 为空!!");
            return new TogetherDaoMemberVo();
        }
        List<Dao> daoList = daoService.selectByTogetherDaoId(daoId + "");

        List<String> daoOwners = daoList.stream().map(Dao::getOwnerAddress).collect(Collectors.toList());
        List<Integer> daoIds = daoList.stream().map(Dao::getId).collect(Collectors.toList());

        List<Canvas> canvasList = canvasService.listCanvasByDaoIds(daoIds);
        Set<String> canvasOwners = canvasList.stream().map(Canvas::getOwnerAddress).collect(Collectors.toSet());

        List<Work> workList = workService.selectWorksByDaoIds(daoIds);
        Set<String> minters = workList.stream().filter(v -> WorkStatusEnum.CASTED.getStatus().equals(v.getWorkStatus())).map(Work::getMintedAddress).collect(Collectors.toSet());
        Set<String> holders = workList.stream().filter(v -> WorkStatusEnum.CASTED.getStatus().equals(v.getWorkStatus())).map(Work::getOwnerAddress).collect(Collectors.toSet());

        TogetherDaoMemberVo togetherDaoMemberVo = new TogetherDaoMemberVo();
        togetherDaoMemberVo.setStarter(new HashSet<>(daoOwners).size());
        togetherDaoMemberVo.setBuilder(canvasOwners.size());
        togetherDaoMemberVo.setMintter(minters.size());
        togetherDaoMemberVo.setNftHolders(holders.size());
        togetherDaoMemberVo.setErc20Holders(daoList.isEmpty() ? 0 : daoList.get(0).getTokenHolders());

        return togetherDaoMemberVo;
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
