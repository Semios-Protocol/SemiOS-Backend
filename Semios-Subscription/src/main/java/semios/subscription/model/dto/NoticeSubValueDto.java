package semios.subscription.model.dto;

import lombok.Data;

/**
 * @description: notice
 * @author: xiangbin
 * @create: 2022-04-19 17:36
 **/
@Data
public class NoticeSubValueDto {

    private String type;

    private String value;

    private String subId;

    private String blockNo;
}
