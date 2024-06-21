package semios.dex.controller;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Controller;
import org.web3j.utils.Numeric;
import semios.dex.model.vo.req.Erc20ReqVo;

import java.math.BigDecimal;

/**
 * Controller基类
 *
 * @author fjtan
 * @ignore
 */
@Controller
public class BaseController {

    /**
     * 校验地址
     */
    protected boolean checkAddress(String address) {
        if (StringUtils.isBlank(address)) {
            return false;
        }
        return address.matches("^(0x)?[0-9a-fA-F]{40}$");
    }

    /**
     * 格式化地址
     */
    protected String formatAddress(String address) {
        return Numeric.prependHexPrefix(address).toLowerCase();
    }

    /**
     * 格式化地址
     */
    protected boolean checkErc20ReqVo(Erc20ReqVo erc20ReqVo) {
        if (erc20ReqVo == null || StringUtils.isBlank(erc20ReqVo.getErc20Address())
                || !checkAddress(erc20ReqVo.getErc20Address())) {
            return false;
        }
        return true;
    }

    /**
     * BigDecimal to String
     */
    protected String decimal2String(BigDecimal decimal) {
        return decimal == null ? null : decimal.stripTrailingZeros().toPlainString();
    }

}