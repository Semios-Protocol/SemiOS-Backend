package semios.api.model.dto.response;

import lombok.Data;
import org.apache.commons.lang.time.DateFormatUtils;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.vo.req.Plan.CreatePlanParam;

import java.util.Date;

/**
 * @description: plan的uri字段
 * @author: zhyyao
 * @create: 2024-05-01 15:53
 **/
@Data
public class NewPlanUriDto {
    /**
     * project名称
     */
    private String planName;

    /**
     * logo
     */
    private String planLogo;

    /*
     *  duration
     * */
    private Integer duration;

    /**
     * 用户地址
     *
     * @Ignore
     */
    private String userAddress;

    /**
     * 开始日期
     */
    private String planStartDate;


    public static NewPlanUriDto transfer(CreatePlanParam createPlanParam) {
        NewPlanUriDto newPlanUriDto = new NewPlanUriDto();

        newPlanUriDto.setPlanName(createPlanParam.getPlanName());
        newPlanUriDto.setPlanLogo(createPlanParam.getPlanLogoUrl());
        newPlanUriDto.setDuration(createPlanParam.getDuration());

        if (StringUtils.isNotBlank(createPlanParam.getPlanStartDate())) {
            newPlanUriDto.setPlanStartDate(createPlanParam.getPlanStartDate());
        } else {
            newPlanUriDto.setPlanStartDate(DateFormatUtils.format(new Date(), "yyyy-MM-dd"));
        }
        newPlanUriDto.setUserAddress(createPlanParam.getUserAddress().toLowerCase());

        return newPlanUriDto;
    }
}
