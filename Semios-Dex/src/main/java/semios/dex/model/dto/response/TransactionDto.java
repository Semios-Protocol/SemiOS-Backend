package semios.dex.model.dto.response;

import lombok.Data;

import java.io.Serializable;

/**
 * <p>
 * transaction记录表
 * </p>
 *
 * @author xiangbin
 * @create: 2022-04-14 13:41
 */
@Data
public class TransactionDto implements Serializable {

    private static final long serialVersionUID = 1L;

    private String address;

    private String blockHash;

    private String blockNumber;

    private Integer blockIntNum;

    private String data;

    private String logIndex;

    private String removed;

    private String topics;

    private String transactionHash;

    private String transactionIndex;

    private Integer subId;


    private String blockTime;

    private String contractAddress;
}
