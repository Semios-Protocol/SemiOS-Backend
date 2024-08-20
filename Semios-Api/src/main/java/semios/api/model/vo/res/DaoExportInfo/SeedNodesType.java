package semios.api.model.vo.res.DaoExportInfo;


import lombok.Data;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.entity.Dao;
import semios.api.model.enums.TrueOrFalseEnum;

@Data
public class SeedNodesType {
    /**
     * 支付货币类型
     */
    private String payCurrencyType;

    /**
     * input token的address
     * 如果是payCurrencyType = eth，字段为空
     */
    private String inputTokenAddress = "";


    /**
     * 是否为外部ERC20 0-否 1-是
     */
    private Integer isThirdpartyToken;

    /**
     * project对应的erc20 token地址
     * isThirdpartyToken = 0,字段为空
     */
    private String erc20Token = "";


    public static SeedNodesType tranferSeedNodesType(Dao dao)
    {
        SeedNodesType seedNodesType = new SeedNodesType();
        seedNodesType.setPayCurrencyType(dao.getPayCurrencyType());

        if (!ProtoDaoConstant.DEFAULT_PAY_CURRENCY_TYPE.equals(seedNodesType.getPayCurrencyType())){
            seedNodesType.setInputTokenAddress(dao.getInputTokenAddress());
        }

        seedNodesType.setIsThirdpartyToken(dao.getIsThirdpartyToken());
        if (TrueOrFalseEnum.TRUE.getStatus().equals(seedNodesType.getIsThirdpartyToken())){
            seedNodesType.setErc20Token(dao.getErc20Token());
        }


        return seedNodesType;
    }
}
