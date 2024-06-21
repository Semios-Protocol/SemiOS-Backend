package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

/**
 * recently listed
 * most favorited
 * price high to low
 * price low to high
 */
@NoArgsConstructor
@AllArgsConstructor
public enum SortConditionEnum {
    RECENTLY(0, "recently listed", "时间最近的"),
    FAVORITED(1, "most favorited", "收藏数最多的"),
    HIGH_TO_LOW(2, "price high to low", "价格从高到低排序"),
    LOW_TO_HIGH(3, "price low to high", "价格从低到高排序");


    private Integer sortValue;

    private String sortName;

    private String sortDesc;
}
