package semios.api.model.vo.res;

import lombok.Data;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.entity.Dao;
import semios.api.model.entity.TokenReceivedRecord;
import semios.api.model.enums.TokenTypeEnum;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Map;

/**
 * token record
 *
 * @description: DAO列表信息
 * @author: xiangbin
 * @create: 2022-08-04 14:45
 **/
@Data
public class TokenRecordListVo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * record id
     */
    private Integer id;

    /**
     * dao名称
     */
    private String DaoReward;

    /**
     * dao erc20 address
     */
    private String daoErc20Address;

    /**
     * txnHash
     */
    private String transactionHash;

    /**
     * from address
     */
    private String fromAddress;

    /**
     * to address
     */
    private String toAddress;


    /**
     * from name
     */
    private String fromName;

    /**
     * to name
     */
    private String toName;

    /**
     * txn value
     */
    private BigDecimal value;

    /**
     * txn time  秒的时间戳
     */
    private String time;

    /**
     * txn type
     */
    private Integer type;

    /**
     * txn type name
     */
    private String typeName;

    public static TokenRecordListVo transfer(TokenReceivedRecord receivedRecord, Map<Integer, Dao> daoMap, Map<String, String> userMap) {
        TokenRecordListVo tokenRecordListVo = new TokenRecordListVo();
        tokenRecordListVo.setId(receivedRecord.getId());
        tokenRecordListVo.setDaoReward(daoMap.get(receivedRecord.getDaoNumber()).getDaoSymbol());
        tokenRecordListVo.setDaoErc20Address(daoMap.get(receivedRecord.getDaoNumber()).getErc20Token());
        if (StringUtils.isNotBlank(receivedRecord.getFromAddress())) {
            tokenRecordListVo.setFromName(userMap.get(receivedRecord.getFromAddress()));
        }
        if (StringUtils.isNotBlank(receivedRecord.getToAddress())) {
            tokenRecordListVo.setToName(userMap.get(receivedRecord.getToAddress()));
        }
        tokenRecordListVo.setFromAddress(receivedRecord.getFromAddress());
        tokenRecordListVo.setToAddress(receivedRecord.getToAddress());
        tokenRecordListVo.setTime(receivedRecord.getBlockTime());
        TokenTypeEnum tokenTypeEnum = TokenTypeEnum.getEnumById(receivedRecord.getTokenType());
        tokenRecordListVo.setType(tokenTypeEnum.getType());
        tokenRecordListVo.setTypeName(tokenTypeEnum.getDesc());
        tokenRecordListVo.setValue(receivedRecord.getTokenNum());

        tokenRecordListVo.setTransactionHash(receivedRecord.getTransactionHash());

        return tokenRecordListVo;
    }

}
