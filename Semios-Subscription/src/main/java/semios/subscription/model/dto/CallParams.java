package semios.subscription.model.dto;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @description: params
 * @author: xiangbin
 * @create: 2022-04-19 10:17
 **/
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
public class CallParams {

    private String from;

    private String to;

    private String gas;

    private String gasPrice;

    private String value;

    private String data;

}
