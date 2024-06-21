package semios.api.model.dto.response;

import lombok.Data;
import semios.api.model.vo.req.WorkCreateReqVo;

/**
 * @description: mint work
 * @author: xiangbin
 * @create: 2022-08-25 09:59
 **/
@Data
public class MintWorkUriDto {

    private String image;
    private String name;
    private String description;


    private String mintedAddress;

    public static MintWorkUriDto transfer(WorkCreateReqVo workCreateReqVo) {
        MintWorkUriDto mintWorkUriDto = new MintWorkUriDto();
        mintWorkUriDto.setImage(workCreateReqVo.getImageUrl());
        mintWorkUriDto.setName(null);
        mintWorkUriDto.setDescription(null);
        mintWorkUriDto.setMintedAddress(null);

        return mintWorkUriDto;

    }
}
