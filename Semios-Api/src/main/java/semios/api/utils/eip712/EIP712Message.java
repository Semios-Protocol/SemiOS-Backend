package semios.api.utils.eip712;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

/**
 * @author fjtan
 */
@Data
@Component
@ConfigurationProperties(prefix = "mint.nft.eip712")
public class EIP712Message {
    @JsonIgnore
    private String typesJson;
    private Map<String, List<Entry>> types;
    private String primaryType;
    private Object message;
    @JsonIgnore
    private String domainJson1;
    @JsonIgnore
    private String domainJson2;
    private EIP712Domain domain;
}
