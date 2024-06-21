package semios.api.controller;

import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;

import java.util.HashMap;
import java.util.Map;

/**
 * 提供合约abi接口
 *
 * @description: abi
 * @author: xiangbin
 * @create: 2022_09_01 10:35
 * @order 4
 **/
@RestController
@RequestMapping("/contract")
public class ContractController {

    /**
     * 查询使用的合约信息 version_1.5
     *
     * @return
     */
    @PostMapping(value = "/abi")
    public Result<Map<String, String>> dappContractAbi() {

        Result<Map<String, String>> result = new Result<>();

        Map<String, String> usdcMap = new HashMap<>();
        usdcMap.put("protocol_contract", ProtoDaoConstant.protocolContract);
        usdcMap.put("protocol_contract_abi", ProtoDaoConstant.protocolContractAbi);
        usdcMap.put("protocol_contract_proxy_abi", ProtoDaoConstant.protocolContractAbi);
        // usdcMap.put("project_proxy_contract", ProtoDaoConstant.projectProxyContract);
        // usdcMap.put("project_proxy_contract_abi", ProtoDaoConstant.projectProxyContractAbi);
        usdcMap.put("claimer_contract", ProtoDaoConstant.claimerContract);
        usdcMap.put("claimer_contract_abi", ProtoDaoConstant.claimerContractAbi);

        usdcMap.put("project_proxy_impl_contract", ProtoDaoConstant.protocolContractAbi);
        usdcMap.put("project_proxy_impl_contract_abi", ProtoDaoConstant.protocolContractAbi);
        usdcMap.put("permission_control_contract", ProtoDaoConstant.permissionControl);
        usdcMap.put("permission_control_contract_abi", ProtoDaoConstant.permissionControlAbi);
        usdcMap.put("erc721_contract_abi", ProtoDaoConstant.erc721ContractAbi);
        usdcMap.put("erc20_contract_abi", ProtoDaoConstant.erc20ContractAbi);

        // usdcMap.put("router_contract", ProtoDaoConstant.routerContract);
        // usdcMap.put("router_contract_abi", ProtoDaoConstant.routerContractAbi);

        // usdcMap.put("proto_dao_setting_writable_abi", ProtoDaoConstant.protoDaoSettingWritableAbi);
        usdcMap.put("protocol_readable_abi", ProtoDaoConstant.protocolReadableAbi);

        usdcMap.put("uniswap_v2_pair_abi", ProtoDaoConstant.uniswapV2PairAbi);

        usdcMap.put("protocol_setter_abi", ProtoDaoConstant.protocolSetterAbi);

        // usdcMap.put("create_funding_abi", ProtoDaoConstant.createFundingAbi);
        //1.4新增
        usdcMap.put("pd_create_abi", ProtoDaoConstant.pdCreateAbi);
        usdcMap.put("pd_round_abi", ProtoDaoConstant.pdRoundAbi);
        usdcMap.put("d4a_erc20_abi", ProtoDaoConstant.d4aErc20Abi);


        // 1.5新增lock合约
        usdcMap.put("work_lock_address", ProtoDaoConstant.workLockAddress);
        usdcMap.put("work_lock_abi", ProtoDaoConstant.workLockAbi);

        // 1.6新增合约abi
        usdcMap.put("pd_grant_abi", ProtoDaoConstant.pdGrantAbi);

        // 1.8 新增add plan abi
        usdcMap.put("pd_plan_abi", ProtoDaoConstant.pdPlanAbi);


        result.setData(usdcMap);
        return result;
    }
}
