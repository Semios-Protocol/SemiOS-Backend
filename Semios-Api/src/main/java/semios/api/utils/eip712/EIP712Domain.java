package semios.api.utils.eip712;

import lombok.Data;

/**
 * @author fjtan
 */
@Data
public class EIP712Domain {
    private String name;
    private String version;
    private Integer chainId;
    private String verifyingContract;
    private String salt;
}
