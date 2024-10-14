package semios.api.model.vo.res.ExploreFilter;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class TokenType {

    /**
     * 系统中所有的token类型
     */
    private List<String> tokenTypeList = new ArrayList<>();


}
