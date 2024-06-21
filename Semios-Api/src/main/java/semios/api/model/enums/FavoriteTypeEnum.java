package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 0-收藏dao
 * 1-收藏canvas
 * 2-收藏work
 */
@NoArgsConstructor
@AllArgsConstructor
public enum FavoriteTypeEnum {
    DAO_FAVORITE(0, "DAO"),
    CANVAS_FAVORITE(1, "CANVAS"),
    WORK_FAVORITE(2, "WORK"),

    SEED_NODES_FAVORITE(3, "SEED_NODES");

    @Getter
    private Integer type;

    @Getter
    private String desc;


}
