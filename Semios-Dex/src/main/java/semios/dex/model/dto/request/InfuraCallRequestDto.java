package semios.dex.model.dto.request;

import lombok.Data;

import java.io.Serializable;

/**
 * @description: call
 * @author: xiangbin
 * @create: 2022-04-22 18:21
 **/
@Data
public class InfuraCallRequestDto implements Serializable {

    private static final long serialVersionUID = 1L;

    private String netWork;
    private String blockNumber;
    private String from;
    private String to;
    private String gas;
    private String gasPrice;
    private String value;
    private String data;


}
