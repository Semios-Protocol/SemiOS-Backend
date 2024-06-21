package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
@AllArgsConstructor
public enum TreasuryTransactionTypeEnum {
    TO_SUB_DAO(0, "给sub dao打款"),
    TO_TREASURY(1, "给国库打款");

    private Integer status;

    private String name;
}
