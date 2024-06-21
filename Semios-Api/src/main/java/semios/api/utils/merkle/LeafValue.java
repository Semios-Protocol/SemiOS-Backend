package semios.api.utils.merkle;

import lombok.Data;

/**
 * @author fjtan
 */
@Data
public class LeafValue {
    String value;
    Integer treeIndex;

    public LeafValue() {
    }

    public LeafValue(String value, Integer treeIndex) {
        this.value = value;
        this.treeIndex = treeIndex;
    }
}
