package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 订阅类型目前包括值类型订阅和事件订阅
 */
@NoArgsConstructor
@AllArgsConstructor
public enum SubscriberTypeEnum {
    EVENT(0, "事件订阅"), VALUE(1, "值订阅"), BATCH_TRAN(2, " batch_transaction");

    @Getter
    private Integer type;

    @Getter
    private String desc;
}
