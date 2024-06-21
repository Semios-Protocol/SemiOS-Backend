package semios.dex.service.feign;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import semios.dex.model.dto.common.ResultList;
import semios.dex.model.dto.feign.DaoErc20Dto;
import semios.dex.model.dto.feign.UserInfoDto;

import java.util.List;

@FeignClient(name = "protodao-client", url = "${protodao.service.url}")
public interface IProtoDaoService {

    @PostMapping("/dex/dao_erc20")
    ResultList<DaoErc20Dto> getDaoErcInfo(@RequestBody(required = true) List<String> erc20AddressList);

    //erc20/owners 用到这个了
    @PostMapping("/dex/user")
    ResultList<UserInfoDto> getUserInfo(@RequestBody(required = true) List<String> userAddressList);

}
