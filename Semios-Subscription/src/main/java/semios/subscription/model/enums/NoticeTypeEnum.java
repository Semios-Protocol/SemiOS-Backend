package semios.subscription.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
public enum NoticeTypeEnum {
    TRAN(0, " transaction"),
    NUM(1, "number"),
    BATCH_TRAN(2, " batch_transaction");

    @Getter
    private Integer type;
    @Getter
    private String name;
}
