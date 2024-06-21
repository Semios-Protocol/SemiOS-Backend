package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;

/**
 * @description: DAO member的人数信息
 * @author: xiangbin
 * @create: 2022-08-04 14:45
 **/
@Slf4j
@Data
public class TogetherDaoMemberVo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * DAO的创建者人数  基于用户地址的去重
     */
    private Integer starter;

    /**
     * 作品上传者  基于用户地址的去重
     */
    private Integer builder;

    /**
     * 作品的铸造者  基于用户地址的去重
     */
    private Integer mintter;

    /**
     * erc-721拥有者地址的加和去重
     */
    private Integer nftHolders;

    /**
     * DAO ERC-20 holder的数量
     */
    private Integer erc20Holders = 0;


    public static void main(String[] args) {

        LocalDate startDate = LocalDate.of(2023, 4, 18);
        long localTime = LocalDateTime.now().toInstant(ZoneOffset.of("+8")).getEpochSecond();
        long startDateTime = startDate.atStartOfDay().plusHours(15).toInstant(ZoneOffset.of("+8")).getEpochSecond();
        System.out.println(startDateTime);
        System.out.println(localTime);
        System.out.println(startDateTime - localTime);
    }
}
