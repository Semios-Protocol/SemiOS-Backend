package semios.api.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import semios.api.interceptor.S3Service;
import semios.api.model.annotation.RepeatSubmit;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.common.ResultList;
import semios.api.model.entity.*;
import semios.api.model.enums.CanvasStatusEnum;
import semios.api.model.enums.DaoStatusEnum;
import semios.api.model.enums.SignPrivacyEnum;
import semios.api.model.enums.TrueOrFalseEnum;
import semios.api.model.vo.req.*;
import semios.api.model.vo.res.*;
import semios.api.model.vo.res.TopUpReward.TopupNftListVo;
import semios.api.model.vo.res.TopUpReward.UserTopupRewardDetailVo;
import semios.api.model.vo.res.TopUpReward.UserTopupRewardVo;
import semios.api.service.*;
import semios.api.service.common.CommonService;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.*;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 用户信息相关接口
 *
 * @author xiangbin
 * @order 6
 */
@Slf4j
@RestController
@RequestMapping("/user")
public class UserController {

    @Autowired
    private IUserService userService;

    @Autowired
    private IDaoService daoService;

    @Autowired
    private ICanvasService canvasService;

    @Autowired
    private ITokenReceivedRecordService tokenReceivedRecordService;

    @Autowired
    private IUserHarvestTokenService userHarvestTokenService;

    @Value("${domain.url}")
    private String domainUrl;

    @Value("${domain.url.semios}")
    private String domainUrlSemios;

    @Autowired
    private S3Service s3Service;

    @Autowired
    private CommonService commonService;

    @Autowired
    private IWorkTopupHarvestService workTopupHarvestService;

    @Autowired
    private ISubscriptionService iSubscriptionService;

    @Autowired
    private INftRewardAllocationService nftRewardAllocationService;

    @Autowired
    private ICollectRecordService collectRecordService;

    /**
     * 用户登陆接口 返回data信息为true代表已签署隐私协议，为false代表未签署隐私协议
     */
    @PostMapping(value = "/login")
    @RepeatSubmit(key = "login")
    public Result<Boolean> userLogin(@RequestBody(required = false) UserProfileReqVo userProfileReqVo,
                                     HttpServletRequest request, HttpServletResponse response) {
        // 1.查询user，看是否存在，不存在时插入一条记录
        // 2。cookie中添加用户登陆信息，
        // 3返回是否签署隐私协议
        Result<Boolean> result = new Result<>();
        if (userProfileReqVo == null || StringUtils.isBlank(userProfileReqVo.getUserAddress())) {
            result.setResultDesc("user address is null");
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            return result;
        }
        String origin = request.getHeader("Origin");

        String userAddress = userProfileReqVo.getUserAddress().toLowerCase();
        User user = userService.findUserByAddressHash(userAddress);
        boolean signPrivacy;
        // 将address写入

        if (user == null) {

            user = commonService.newUser(userAddress);

            boolean save = userService.save(user);
            signPrivacy = false;
            if (!save) {
                log.error("[userLogin]保存用户信息失败");
                result.setResultDesc(ResultDesc.FAIL.getResultDesc());
                result.setResultCode(ResultDesc.FAIL.getResultCode());
                return result;
            }
        } else {
            signPrivacy = SignPrivacyEnum.YQS.getCode().equals(user.getSignPrivacyAgreement());
        }

        if (!signPrivacy) {
            result.setData(false);
            return result;
        }
        log.info("[userLogin]origin:{}", origin);

        if (StringUtils.isNotBlank(user.getUserName())) {
            Cookie namecookie = new Cookie(ProtoDaoConstant.COOKIE_NAME, user.getUserName().replaceAll(" ", "_"));
            if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
                if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                    log.info("[userLogin]origin:{} is domainUrl", origin);
                    namecookie.setDomain(domainUrl);
                }
                if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                    log.info("[userLogin]origin:{} is domainUrlSemios", origin);
                    namecookie.setDomain(domainUrlSemios);
                }
            }
            namecookie.setPath("/");
            namecookie.setMaxAge(60 * 60 * 24);
            response.addCookie(namecookie);
        } else {
            Cookie namecookie = new Cookie(ProtoDaoConstant.COOKIE_NAME, "");
            if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
                if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                    log.info("[userLogin]origin:{} is domainUrl", origin);
                    namecookie.setDomain(domainUrl);
                }
                if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                    log.info("[userLogin]origin:{} is domainUrlSemios", origin);
                    namecookie.setDomain(domainUrlSemios);
                }
            }
            namecookie.setPath("/");
            namecookie.setMaxAge(0);
            response.addCookie(namecookie);
        }

        if (StringUtils.isNotBlank(user.getAvatarAddress())) {
            Cookie avatarcookie = new Cookie(ProtoDaoConstant.COOKIE_AVATAR, user.getAvatarAddress());
            if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
                if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                    avatarcookie.setDomain(domainUrl);
                }
                if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                    avatarcookie.setDomain(domainUrlSemios);
                }
            }
            avatarcookie.setPath("/");
            avatarcookie.setMaxAge(60 * 60 * 24);
            response.addCookie(avatarcookie);
        } else {
            Cookie avatarcookie = new Cookie(ProtoDaoConstant.COOKIE_AVATAR, "");
            if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
                if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                    avatarcookie.setDomain(domainUrl);
                }
                if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                    avatarcookie.setDomain(domainUrlSemios);
                }
            }
            avatarcookie.setPath("/");
            avatarcookie.setMaxAge(0);
            response.addCookie(avatarcookie);
        }

        Cookie tokenCookie = new Cookie(ProtoDaoConstant.COOKIE_TOKEN, "");
        tokenCookie.setMaxAge(0);
        tokenCookie.setPath("/");
        if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                tokenCookie.setDomain(domainUrl);
            }
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                tokenCookie.setDomain(domainUrlSemios);
            }
        }
//        tokenCookie.setDomain(domainUrl);
        response.addCookie(tokenCookie);

        Cookie timeCookie = new Cookie(ProtoDaoConstant.COOKIE_TOKEN_TIME, "0");
        timeCookie.setMaxAge(0);
        timeCookie.setPath("/");
        if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                timeCookie.setDomain(domainUrl);
            }
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                timeCookie.setDomain(domainUrlSemios);
            }
        }
        response.addCookie(timeCookie);

        Cookie roleCookie = new Cookie(ProtoDaoConstant.COOKIE_ROLE, user.getRole() + "");
        roleCookie.setMaxAge(60 * 60 * 24);
        roleCookie.setPath("/");
        if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                roleCookie.setDomain(domainUrl);
            }
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                roleCookie.setDomain(domainUrlSemios);
            }
        }
        response.addCookie(roleCookie);

        // Cookie addresscookie = new Cookie(Dao4ArtConstant.COOKIE_ADDRESS, userProfileReqVo.getUserAddress());
        // addresscookie.setMaxAge(60*60*24);
        // addresscookie.setPath("/");
        // addresscookie.setHttpOnly(true);
        // addresscookie.setValue("SameSite=None");
        //// addresscookie.setDomain(domainUrl);
        // response.addCookie(addresscookie);

        // ResponseCookie cookie = ResponseCookie.from(Dao4ArtConstant.COOKIE_ADDRESS, userAddress) // key & value
        //// .httpOnly(true) // 禁止js读取
        //// .secure(true) // 在http下也传输
        // .domain(domainUrl)// 域名 不能是127。0。0。1也不能是localhost，必须是ip10。10。10。49
        // .path("/") // path
        // .maxAge(60*60*24) // 有效期
        //// .sameSite("None") // 大多数情况也是不发送第三方 Cookie，但是导航到目标网址的 Get 请求除外
        // .build()
        // ;
        // // 设置Cookie Header
        // response.setHeader(HttpHeaders.SET_COOKIE, cookie.toString());

        Cookie addresscookie = new Cookie(ProtoDaoConstant.COOKIE_ADDRESS, userAddress);
        if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                addresscookie.setDomain(domainUrl);
            }
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                addresscookie.setDomain(domainUrlSemios);
            }
        }
        addresscookie.setPath("/");
        addresscookie.setMaxAge(60 * 60 * 24);
        response.addCookie(addresscookie);

        Cookie addresssession = new Cookie(ProtoDaoConstant.SESSION_ADDRESS, userAddress);
        if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                addresssession.setDomain(domainUrl);
            }
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                addresssession.setDomain(domainUrlSemios);
            }
        }
        addresssession.setPath("/");
        // addresssession.setMaxAge(0);//去掉为session级别的cookie
        response.addCookie(addresssession);

        HttpSession session = request.getSession();

        session.setAttribute(ProtoDaoConstant.SESSION_ADDRESS, userAddress);

        result.setData(true);
        return result;
    }

    /**
     * 用户签署隐私协议接口 -已废弃 userProfileReqVo 不需要前端传参数
     *
     * @Ignore
     */
    @Deprecated
    @PostMapping(value = "/privacy/agreement")
    public Result<String> userSignPrivacyAgreement(@RequestBody(required = false) UserProfileReqVo userProfileReqVo,
                                                   HttpServletResponse response) {
        Result<String> result = new Result<>();
        log.info("[privacy-agreement]userProfileReqVo:{}", JacksonUtil.obj2json(userProfileReqVo));
        User user = userService.findUserByAddressHash(userProfileReqVo.getUserAddress());
        if (user == null) {
            result.setResultDesc("用户不存在！");
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            return result;
        }
        user.setSignPrivacyAgreement(SignPrivacyEnum.YQS.getCode());
        boolean update = userService.updateById(user);
        if (!update) {
            result.setResultDesc("签署失败，请稍后再试");
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            return result;
        }

        if (StringUtils.isNotBlank(user.getUserName())) {
            Cookie namecookie = new Cookie(ProtoDaoConstant.COOKIE_NAME, user.getUserName().replaceAll(" ", "_"));
            namecookie.setDomain(domainUrl);
            namecookie.setPath("/");
            namecookie.setMaxAge(60 * 60 * 24);
            response.addCookie(namecookie);
        } else {
            Cookie namecookie = new Cookie(ProtoDaoConstant.COOKIE_NAME, "");
            namecookie.setDomain(domainUrl);
            namecookie.setPath("/");
            namecookie.setMaxAge(0);
            response.addCookie(namecookie);
        }

        if (StringUtils.isNotBlank(user.getAvatarAddress())) {
            Cookie avatarcookie = new Cookie(ProtoDaoConstant.COOKIE_AVATAR, user.getAvatarAddress());
            avatarcookie.setDomain(domainUrl);
            avatarcookie.setPath("/");
            avatarcookie.setMaxAge(60 * 60 * 24);
            response.addCookie(avatarcookie);
        } else {
            Cookie avatarcookie = new Cookie(ProtoDaoConstant.COOKIE_AVATAR, "");
            avatarcookie.setDomain(domainUrl);
            avatarcookie.setPath("/");
            avatarcookie.setMaxAge(0);
            response.addCookie(avatarcookie);
        }

        Cookie tokenCookie = new Cookie(ProtoDaoConstant.COOKIE_TOKEN, "");
        tokenCookie.setMaxAge(0);
        tokenCookie.setPath("/");
        tokenCookie.setDomain(domainUrl);
        response.addCookie(tokenCookie);

        Cookie timeCookie = new Cookie(ProtoDaoConstant.COOKIE_TOKEN_TIME, "0");
        timeCookie.setMaxAge(0);
        timeCookie.setPath("/");
        timeCookie.setDomain(domainUrl);
        response.addCookie(timeCookie);

        Cookie roleCookie = new Cookie(ProtoDaoConstant.COOKIE_ROLE, user.getRole() + "");
        roleCookie.setMaxAge(60 * 60 * 24);
        roleCookie.setPath("/");
        roleCookie.setDomain(domainUrl);
        response.addCookie(roleCookie);

        Cookie addresscookie = new Cookie(ProtoDaoConstant.COOKIE_ADDRESS, userProfileReqVo.getUserAddress());
        addresscookie.setDomain(domainUrl);
        addresscookie.setPath("/");
        addresscookie.setMaxAge(60 * 60 * 24);
        response.addCookie(addresscookie);

        return result;
    }

    /**
     * 获取用户profile信息
     */
    @PostMapping(value = "/profile/info")
    public Result<UserProfileResVo> userProfileInfo(@RequestBody(required = false) UserProfileReqVo userProfileReqVo) {
        Result<UserProfileResVo> result = new Result<>();

        User user = userService.findUserByAddressHash(userProfileReqVo.getUserAddress());
        if (user == null) {
            result.setResultDesc("用户不存在！");
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            return result;
        }

        UserProfileResVo userProfileResVo = UserProfileResVo.transfer(user);

        result.setData(userProfileResVo);
        return result;
    }

    /**
     * 保存用户profile信息 是否需要返回头像地址和昵称信息
     */
    @PostMapping(value = "/profile/save")
    @RepeatSubmit(key = "profile_save")
    public Result<Boolean> userProfileSave(UserProfileVo userProfileVo, HttpServletRequest request,
                                           @RequestParam(value = "avatar", required = false) MultipartFile avatar, HttpServletResponse response) {
        Result<Boolean> result = new Result<>();
        String useraddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isNotBlank(useraddress)) {
            userProfileVo.setUserAddress(useraddress);
        } else {
            result.setResultDesc("please login.");
            result.setResultCode(ResultDesc.USER_ERROR.getResultCode());
            return result;
        }

        if (StringUtils.isNotBlank(userProfileVo.getUserName())) {
            String checkString = CommonUtil.nameCheck(userProfileVo.getUserName());
            if (StringUtils.isNotBlank(checkString)) {
                result.setResultDesc(checkString);
                result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
                return result;
            }
            User user = userService.findUserByName(userProfileVo.getUserName());
            if (user != null && !user.getUserAddress().equals(userProfileVo.getUserAddress())) {
                result.setResultDesc("Invalid name . The name is already taken.");
                result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
                return result;
            }

            Cookie namecookie =
                    new Cookie(ProtoDaoConstant.COOKIE_NAME, userProfileVo.getUserName().replaceAll(" ", "_"));
            namecookie.setDomain(domainUrlSemios);
            namecookie.setPath("/");
            namecookie.setMaxAge(60 * 60 * 24);
            response.addCookie(namecookie);
        } else {
            Cookie namecookie = new Cookie(ProtoDaoConstant.COOKIE_NAME, "");
            namecookie.setDomain(domainUrlSemios);
            namecookie.setPath("/");
            namecookie.setMaxAge(0);
            response.addCookie(namecookie);
        }

        log.info("[user-profile-save] userProfileVo:{}", JacksonUtil.obj2json(userProfileVo));
        User user = userService.findUserByAddressHash(userProfileVo.getUserAddress());
        if (user == null) {
            result.setResultDesc("用户不存在！");
            result.setResultCode(ResultDesc.AUTH_ERROR.getResultCode());
            return result;
        }

        if (avatar != null && StringUtils.isNotBlank(avatar.getOriginalFilename())) {
            try {
                // 文件上传前的名称 处理图片用
                String avatarName = avatar.getOriginalFilename();
                String imageName = CodeUtil.generateCode('A');
                avatarName = imageName + avatarName.substring(avatarName.lastIndexOf("."));
                s3Service.putImage(ProtoDaoConstant.bucketName + "/user", avatar, imageName);

                String urlPrefix = String.format(ProtoDaoConstant.urlPrefix, ProtoDaoConstant.bucketName);
                String s3Url = urlPrefix + ProtoDaoConstant.userBucketName + "/" + avatarName;
                userProfileVo.setAvatarLink(s3Url);
                log.info("[user-profile-save] s3AvatarUrl:{}", s3Url);

                Cookie avatarcookie = new Cookie(ProtoDaoConstant.COOKIE_AVATAR, s3Url);
                // avatarcookie.setDomain(domainUrl);
                avatarcookie.setDomain(domainUrlSemios);
                avatarcookie.setPath("/");
                avatarcookie.setMaxAge(60 * 60 * 24);
                response.addCookie(avatarcookie);
            } catch (Exception e) {
                log.info("[user-profile-save] avator s3Service error ", e);
            }

        }

        UserProfileVo.transferToUser(userProfileVo, user);

        boolean update = userService.updateById(user);

        result.setData(update);
        return result;
    }

    /**
     * 用户dao收益
     */
    @PostMapping(value = "/income/dao")
    public Result<UserDaoIncomeVo>
    userIncomeForDao(@RequestBody(required = false) UserProfilePageReqVo userProfilePageReqVo) {
        Result<UserDaoIncomeVo> result = new Result<>();
        Page<Dao> iPage = new Page<>(userProfilePageReqVo.getPageNo(), userProfilePageReqVo.getPageSize());
        Page<Dao> daoPage = daoService.myDaoList(iPage, userProfilePageReqVo.getUserAddress());
        List<Dao> daoList = daoPage.getRecords();
        if (daoList.size() > 0) {
            List<UserDaoIncomeVo> userDaoIncomeVos =
                    daoList.stream().map(UserDaoIncomeVo::transfer).collect(Collectors.toList());
            result.setDataList(userDaoIncomeVos);
        } else {
            result.setDataList(new ArrayList<>());
        }
        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(userProfilePageReqVo.getPageNo());
        page.setPageSize(userProfilePageReqVo.getPageSize());
        page.setCount(daoPage.getTotal());
        result.setPage(page);

        return result;
    }

    /**
     * 用户canvas收益
     */
    @PostMapping(value = "/income/canvas")
    public Result<UserCanvasIncomeVo>
    userIncomeForCanvas(@RequestBody(required = false) UserProfilePageReqVo userProfilePageReqVo) {
        Result<UserCanvasIncomeVo> result = new Result<>();

        Page<Canvas> iPage = new Page<>(userProfilePageReqVo.getPageNo(), userProfilePageReqVo.getPageSize());
        Page<Canvas> canvasPage = canvasService.myCanvas(iPage, userProfilePageReqVo.getUserAddress());
        List<Canvas> canvasList = canvasPage.getRecords();
        if (canvasList.size() > 0) {
            List<UserCanvasIncomeVo> canvasIncomeVoList =
                    canvasList.stream().map(UserCanvasIncomeVo::transfer).collect(Collectors.toList());
            result.setDataList(canvasIncomeVoList);
        } else {
            result.setDataList(new ArrayList<>());
        }

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(userProfilePageReqVo.getPageNo());
        page.setPageSize(userProfilePageReqVo.getPageSize());
        page.setCount(canvasPage.getTotal());
        result.setPage(page);

        return result;
    }

    /**
     * 签名的时候返回token，token有效期24小时 对某些页面拦截，如果没有token或者token验证失效则返回重新登陆
     *
     * @return Result
     */
    @PostMapping(value = "/signature")
    public Result<String> signature(@RequestBody(required = false) UserSignatureReqVo userSignatureReqVo,
                                    HttpServletRequest request, HttpServletResponse response) {
        Result<String> result = new Result<>();
        try {
            log.info("[signature]param:{}", JacksonUtil.obj2json(userSignatureReqVo));
            if (StringUtils.isAnyBlank(userSignatureReqVo.getUserAddress(), userSignatureReqVo.getSignatureHash(),
                    userSignatureReqVo.getOriginalText())) {
                result.setResultCode(ResultDesc.ERROR.getResultCode());
                result.setResultDesc("Parameter exception");
                return result;
            }

            String origin = request.getHeader("Origin");

            User user = userService.findUserByAddressHash(userSignatureReqVo.getUserAddress());
            if (user == null) {
                result.setResultCode(ResultDesc.ERROR.getResultCode());
                result.setResultDesc("user does not exist");
                return result;
            }

            // 获取密文，明文及address验签
            String address = EthersUtils.verifyMessage(userSignatureReqVo.getOriginalText().replaceAll("\\\\n", "\\\n"),
                    userSignatureReqVo.getSignatureHash());
            if (!userSignatureReqVo.getUserAddress().equalsIgnoreCase(address)) {
                log.error("[signature] verify error userSignatureReqVo:{} address:{}",
                        JacksonUtil.obj2json(userSignatureReqVo), address);
                result.setResultCode(ResultDesc.FAIL.getResultCode());
                result.setResultDesc("You are not the owner");
                return result;
            }
            String useraddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);

            if (StringUtils.isNotBlank(useraddress) && !useraddress.equalsIgnoreCase(address)) {
                log.error("[signature] verify error userSignatureReqVo:{} address:{} useraddress:{}",
                        JacksonUtil.obj2json(userSignatureReqVo), address, useraddress);
                result.setResultDesc("please login.");
                result.setResultCode(ResultDesc.USER_ERROR.getResultCode());
                return result;
            }

            if (SignPrivacyEnum.WQS.getCode().equals(user.getSignPrivacyAgreement())) {
                user.setSignPrivacyAgreement(SignPrivacyEnum.YQS.getCode());
                userService.updateById(user);

                HttpSession session = request.getSession();

                session.setAttribute(ProtoDaoConstant.SESSION_ADDRESS, userSignatureReqVo.getUserAddress());

                if (StringUtils.isNotBlank(user.getUserName())) {
                    Cookie namecookie =
                            new Cookie(ProtoDaoConstant.COOKIE_NAME, user.getUserName().replaceAll(" ", "_"));
                    if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
                        if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                            namecookie.setDomain(domainUrl);
                        }
                        if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                            namecookie.setDomain(domainUrlSemios);
                        }
                    }
                    namecookie.setPath("/");
                    namecookie.setMaxAge(60 * 60 * 24);
                    response.addCookie(namecookie);
                } else {
                    Cookie namecookie = new Cookie(ProtoDaoConstant.COOKIE_NAME, "");
                    if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
                        if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                            namecookie.setDomain(domainUrl);
                        }
                        if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                            namecookie.setDomain(domainUrlSemios);
                        }
                    }
                    namecookie.setPath("/");
                    namecookie.setMaxAge(0);
                    response.addCookie(namecookie);
                }

                if (StringUtils.isNotBlank(user.getAvatarAddress())) {
                    Cookie avatarcookie = new Cookie(ProtoDaoConstant.COOKIE_AVATAR, user.getAvatarAddress());
                    if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
                        if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                            avatarcookie.setDomain(domainUrl);
                        }
                        if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                            avatarcookie.setDomain(domainUrlSemios);
                        }
                    }
                    avatarcookie.setPath("/");
                    avatarcookie.setMaxAge(60 * 60 * 24);
                    response.addCookie(avatarcookie);
                } else {
                    Cookie avatarcookie = new Cookie(ProtoDaoConstant.COOKIE_AVATAR, "");
                    if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
                        if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                            avatarcookie.setDomain(domainUrl);
                        }
                        if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                            avatarcookie.setDomain(domainUrlSemios);
                        }
                    }
                    avatarcookie.setPath("/");
                    avatarcookie.setMaxAge(0);
                    response.addCookie(avatarcookie);
                }

                Cookie roleCookie = new Cookie(ProtoDaoConstant.COOKIE_ROLE, user.getRole() + "");
                roleCookie.setMaxAge(60 * 60 * 24);
                roleCookie.setPath("/");
                if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
                    if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                        roleCookie.setDomain(domainUrl);
                    }
                    if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                        roleCookie.setDomain(domainUrlSemios);
                    }
                }
                response.addCookie(roleCookie);

                Cookie addresscookie = new Cookie(ProtoDaoConstant.COOKIE_ADDRESS, userSignatureReqVo.getUserAddress());
                if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
                    if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                        addresscookie.setDomain(domainUrl);
                    }
                    if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                        addresscookie.setDomain(domainUrlSemios);
                    }
                }
                addresscookie.setPath("/");
                addresscookie.setMaxAge(60 * 60 * 24);
                response.addCookie(addresscookie);

                Cookie addresssession =
                        new Cookie(ProtoDaoConstant.SESSION_ADDRESS, userSignatureReqVo.getUserAddress());
                if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
                    if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                        addresssession.setDomain(domainUrl);
                    }
                    if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                        addresssession.setDomain(domainUrlSemios);
                    }
                }
                addresssession.setPath("/");
                // addresssession.setMaxAge(0);
                response.addCookie(addresssession);

            }

            String audience = StringUtils.isBlank(user.getUserName()) ? "yeez" : user.getUserName();
            String subject = String.format("{\"userhash\": \"%s\"}", user.getUserAddress());
            String token = JjwtUtil.createJWT("yeez", audience, subject);

            Cookie tokenCookie = new Cookie(ProtoDaoConstant.COOKIE_TOKEN, token);
            tokenCookie.setMaxAge(60 * 60 * 24);
            tokenCookie.setPath("/");
            if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
                if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                    tokenCookie.setDomain(domainUrl);
                }
                if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                    tokenCookie.setDomain(domainUrlSemios);
                }
            }
            response.addCookie(tokenCookie);

            long localTime = LocalDateTime.now().toInstant(ZoneOffset.of("+8")).getEpochSecond();
            long localTime1 = LocalDateTime.now().toEpochSecond(ZoneOffset.UTC);
            log.info("localTime:{} localTime1:{}", localTime, localTime1);
            Cookie timeCookie = new Cookie(ProtoDaoConstant.COOKIE_TOKEN_TIME, String.valueOf(localTime));
            timeCookie.setMaxAge(60 * 60 * 24);
            timeCookie.setPath("/");
            if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
                if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                    timeCookie.setDomain(domainUrl);
                }
                if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                    timeCookie.setDomain(domainUrlSemios);
                }
            }
            response.addCookie(timeCookie);

            result.setData(token);

        } catch (Exception e) {
            log.error("[user-signature]error:", e);
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            result.setResultDesc("Server exception, please try again later");
        }
        return result;
    }

    /**
     * 用户退出登陆接口
     */
    @PostMapping(value = "/logout")
    public Result<String> userLogout(@RequestBody(required = false) UserProfileReqVo userProfileReqVo,
                                     HttpServletRequest request, HttpServletResponse response) {
        // 1.查询user，看是否存在，不存在返回错误提示
        // 2。cookie中删除用户登陆信息，
        Result<String> result = new Result<>();
        String origin = request.getHeader("Origin");
        Cookie addresssession = new Cookie(ProtoDaoConstant.SESSION_ADDRESS, "");
        if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                addresssession.setDomain(domainUrl);
            }
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                addresssession.setDomain(domainUrlSemios);
            }
        }
        addresssession.setPath("/");
        addresssession.setMaxAge(0);// 去掉为session级别的cookie
        response.addCookie(addresssession);

        Cookie namecookie = new Cookie(ProtoDaoConstant.COOKIE_NAME, "");
        namecookie.setMaxAge(0);
        namecookie.setPath("/");
        if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                namecookie.setDomain(domainUrl);
            }
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                namecookie.setDomain(domainUrlSemios);
            }
        }
        response.addCookie(namecookie);

        Cookie avatarcookie = new Cookie(ProtoDaoConstant.COOKIE_AVATAR, "");
        avatarcookie.setMaxAge(0);
        avatarcookie.setPath("/");
        if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                avatarcookie.setDomain(domainUrl);
            }
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                avatarcookie.setDomain(domainUrlSemios);
            }
        }
        response.addCookie(avatarcookie);

        Cookie timeCookie = new Cookie(ProtoDaoConstant.COOKIE_TOKEN_TIME, "");
        timeCookie.setMaxAge(0);
        timeCookie.setPath("/");
        if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                timeCookie.setDomain(domainUrl);
            }
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                timeCookie.setDomain(domainUrlSemios);
            }
        }
        response.addCookie(timeCookie);

        Cookie tokenCookie = new Cookie(ProtoDaoConstant.COOKIE_TOKEN, "");
        tokenCookie.setMaxAge(0);
        tokenCookie.setPath("/");
        if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                tokenCookie.setDomain(domainUrl);
            }
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                tokenCookie.setDomain(domainUrlSemios);
            }
        }
        response.addCookie(tokenCookie);

        Cookie addresscookie = new Cookie(ProtoDaoConstant.COOKIE_ADDRESS, "");
        addresscookie.setMaxAge(0);
        addresscookie.setPath("/");
        if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                addresscookie.setDomain(domainUrl);
            }
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                addresscookie.setDomain(domainUrlSemios);
            }
        }
        response.addCookie(addresscookie);

        Cookie rolecookie = new Cookie(ProtoDaoConstant.COOKIE_ROLE, "");
        rolecookie.setMaxAge(0);
        rolecookie.setPath("/");
        if (StringUtils.isEmpty(origin) || !origin.contains(ProtoDaoConstant.LOCAL_HOST)) {
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrl)) {
                rolecookie.setDomain(domainUrl);
            }
            if (StringUtils.isNotBlank(origin) && origin.contains(domainUrlSemios)) {
                rolecookie.setDomain(domainUrlSemios);
            }
        }
        response.addCookie(rolecookie);

        HttpSession session = request.getSession();
        String address = (String) session.getAttribute(ProtoDaoConstant.SESSION_ADDRESS);
        if (address != null) {
            request.getSession().removeAttribute(ProtoDaoConstant.SESSION_ADDRESS);
        }
        return result;

        // log.info("[user-logout]userProfileReqVo:{}", JacksonUtil.obj2json(userProfileReqVo));
        // User user = userService.findUserByAddressHash(userProfileReqVo.getUserAddress());
        // //将address写入
        // if (user == null) {
        // log.error("[userLogout]用户不存在userProfileReqVo:{}", JacksonUtil.obj2json(userProfileReqVo));
        // }

        // String address = CookieUtil.getUserAddressFromCookie(request, Dao4ArtConstant.COOKIE_ADDRESS);
        // String name = CookieUtil.getUserAddressFromCookie(request, Dao4ArtConstant.COOKIE_NAME);
        // String avatar = CookieUtil.getUserAddressFromCookie(request, Dao4ArtConstant.COOKIE_AVATAR);
        //
        // if(StringUtils.isNotBlank(name)){
        // Cookie namecookie = new Cookie(Dao4ArtConstant.COOKIE_NAME, name);
        // namecookie.setMaxAge(0);
        // namecookie.setPath("/");
        // namecookie.setDomain(domainUrl);
        // response.addCookie(namecookie);
        // }
        // if(StringUtils.isNotBlank(avatar)){
        // Cookie avatarcookie = new Cookie(Dao4ArtConstant.COOKIE_AVATAR, avatar);
        // avatarcookie.setMaxAge(0);
        // avatarcookie.setPath("/");
        // avatarcookie.setDomain(domainUrl);
        // response.addCookie(avatarcookie);
        // }
        // String time = CookieUtil.getUserAddressFromCookie(request, Dao4ArtConstant.COOKIE_TOKEN_TIME);
        // String token = CookieUtil.getUserAddressFromCookie(request, Dao4ArtConstant.COOKIE_TOKEN);
        // if(time != null){
        // Cookie timeCookie = new Cookie(Dao4ArtConstant.COOKIE_TOKEN_TIME, time);
        // timeCookie.setMaxAge(0);
        // timeCookie.setPath("/");
        // timeCookie.setDomain(domainUrl);
        // response.addCookie(timeCookie);
        // }
        // if(token != null){
        // Cookie tokenCookie = new Cookie(Dao4ArtConstant.COOKIE_TOKEN, token);
        // tokenCookie.setMaxAge(0);
        // tokenCookie.setPath("/");
        // tokenCookie.setDomain(domainUrl);
        // response.addCookie(tokenCookie);
        // }
        //
        // if(StringUtils.isNotBlank(address)) {
        // Cookie addresscookie = new Cookie(Dao4ArtConstant.COOKIE_ADDRESS, address);
        // addresscookie.setMaxAge(0);
        // addresscookie.setPath("/");
        // addresscookie.setDomain(domainUrl);
        // response.addCookie(addresscookie);
        //
        // Cookie addresssession = new Cookie(Dao4ArtConstant.SESSION_ADDRESS, address);
        // addresssession.setDomain(domainUrl);
        // addresssession.setPath("/");
        // addresssession.setMaxAge(0);//去掉为session级别的cookie
        // response.addCookie(addresssession);
        //
        // }

        // return result;
    }

    /**
     * 获取用户profile信息
     */
    @PostMapping(value = "/cookie/info")
    public Result<UserCookieResVo> userCookieInfo(HttpServletRequest request,
                                                  @RequestBody(required = false) UserProfileReqVo userProfileReqVo) {
        Result<UserCookieResVo> result = new Result<>();

        UserCookieResVo userCookieResVo = new UserCookieResVo();
        Cookie[] cookies = request.getCookies();
        if (cookies != null) {
            for (Cookie cookie : cookies) {
                if ("name".equals(cookie.getName())) {
                    userCookieResVo.setName(cookie.getValue());
                } else if ("avatar".equals(cookie.getName())) {
                    userCookieResVo.setAvatar(cookie.getValue());
                } else if ("address".equals(cookie.getName())) {
                    userCookieResVo.setAddress(cookie.getValue());
                } else if ("token".equals(cookie.getName())) {
                    userCookieResVo.setToken(cookie.getValue());
                } else if ("time".equals(cookie.getName())) {
                    userCookieResVo.setTime(Long.valueOf(cookie.getValue()));
                } else if ("role".equals(cookie.getName())) {
                    if (StringUtils.isNotBlank(cookie.getValue()) && !"null".equals(cookie.getValue())) {
                        userCookieResVo.setRole(Integer.valueOf(cookie.getValue()));
                    }
                } else {
                    log.info("cookie contains other name:{} value:{}", cookie.getName(), cookie.getValue());
                }
            }
        }

        result.setData(userCookieResVo);
        return result;
    }

    /**
     * 以ERC20为单位展示所有已领取erc20大于0的的信息 返回结果在dataList中
     */
    @PostMapping(value = "/income/wallet/received")
    public Result<UserWalletReceivedIncomeVo>
    userIncomeForWalletReceived(@RequestBody(required = false) UserProfileReqVo userProfileReqVo) {
        Result<UserWalletReceivedIncomeVo> result = new Result<>();
        // 此接口功能上已经不用，返回空
        if (StringUtils.isNotBlank(userProfileReqVo.getUserAddress())){
            result.setData(new UserWalletReceivedIncomeVo());
            return result;
        }

        List<Dao> daoList = daoService.myDaoListAll(userProfileReqVo.getUserAddress());
        List<Canvas> canvasList = canvasService.myCanvasForAll(userProfileReqVo.getUserAddress());
        //这里只能使用receiveType为transfer类型的数据，在下面UserWalletReceivedIncomeVo.transfer中有过滤。
        List<TokenReceivedRecord> tokenReceivedRecordList =
                tokenReceivedRecordService.recordList(userProfileReqVo.getUserAddress());
        List<UserHarvestToken> userHarvestTokenList =
                userHarvestTokenService.selectReceivedTokenByUserAddress(userProfileReqVo.getUserAddress());
        List<UserWalletReceivedIncomeVo> userWalletIncomeVos = UserWalletReceivedIncomeVo.transfer(daoList, canvasList,
                tokenReceivedRecordList, userHarvestTokenList, userProfileReqVo.getUserAddress());

        result.setDataList(userWalletIncomeVos);

        return result;
    }

    /**
     * 以ERC20为单位展示所有待领取的信息 且大于0的 返回信息在Result的data信息里
     */
    @PostMapping(value = "/income/wallet/unclaimed")
    public Result<UserWalletUnclaimedIncomeVo>
    userIncomeForWalletUnclaimed(@RequestBody(required = false) UserProfileReqVo userProfileReqVo) {
        Result<UserWalletUnclaimedIncomeVo> result = new Result<>();

        List<Dao> daoList = daoService.myDaoListAll(userProfileReqVo.getUserAddress());
        List<Canvas> canvasList = canvasService.myCanvasForAll(userProfileReqVo.getUserAddress());
        List<UserHarvestToken> userHarvestTokenList =
                userHarvestTokenService.selectUnclaimedTokenByUserAddress(userProfileReqVo.getUserAddress());
        userHarvestTokenList = userHarvestTokenList.stream().filter(v -> v.getUnclaimedToken() != null && v.getUnclaimedToken().compareTo(BigDecimal.ZERO) > 0).collect(Collectors.toList());

        UserWalletUnclaimedIncomeVo userWalletUnclaimedIncomeVo =
                UserWalletUnclaimedIncomeVo.transfer(daoList, canvasList, userHarvestTokenList);

        result.setData(userWalletUnclaimedIncomeVo);

        return result;
    }

    /**
     * 钱包中以dao为纬度的收益信息 返回信息在Result的data信息里
     */
    @PostMapping(value = "/income/wallet/dao")
    public Result<UserWalletDaoIncomeVo>
    userIncomeForWallet(@RequestBody(required = false) UserProfileReqVo userProfileReqVo) {
        Result<UserWalletDaoIncomeVo> result = new Result<>();

        UserWalletDaoIncomeVo userWalletUnclaimedIncomeVo = new UserWalletDaoIncomeVo();
        List<String> canvasIdList = new ArrayList<>();
        List<String> projectIdList = new ArrayList<>();
        List<String> canvasId2List = new ArrayList<>();
        List<String> projectId2List = new ArrayList<>();

        List<Dao> daoList = daoService.myDaoListAll(userProfileReqVo.getUserAddress());
        List<Canvas> canvasList = canvasService.myCanvasForAll(userProfileReqVo.getUserAddress());
        List<UserHarvestToken> userHarvestTokenList =
                userHarvestTokenService.selectAllByUserAddress(userProfileReqVo.getUserAddress());
        Map<Integer, List<Object>> map = new HashMap<>();

        for (Dao dao : daoList) {
            //钱包这里topup模式的不展示
            if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getTopupMode())) {
                continue;
            }
            if (!ProtoDaoConstant.D4APause && !dao.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())
                    && dao.getUnclaimedToken().compareTo(BigDecimal.ZERO) > 0) {
                if (dao.getDaoVersion() == 3) {
                    projectId2List.add(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
                } else {
                    projectIdList.add(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
                }
            }
            List<Object> objectList = new ArrayList<>();
            objectList.add(dao);
            map.put(dao.getDaoNumber(), objectList);
        }
        for (Canvas canvas : canvasList) {
            if (!ProtoDaoConstant.D4APause && !canvas.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())
                    && !canvas.getCanvasStatus().equals(CanvasStatusEnum.SHUT_DOWN.getStatus())
                    && canvas.getUnclaimedToken().compareTo(BigDecimal.ZERO) > 0) {
                Dao canvasDao = daoService.getById(canvas.getDaoId());
                //钱包这里topup模式的不展示
                if (canvasDao != null && TrueOrFalseEnum.TRUE.getStatus().equals(canvasDao.getTopupMode())) {
                    continue;
                }
                if (canvasDao != null && canvasDao.getDaoVersion() == 3) {
                    canvasId2List.add(CommonUtil.addHexPrefixIfNotExist(canvas.getCanvasId()));
                } else {
                    canvasIdList.add(CommonUtil.addHexPrefixIfNotExist(canvas.getCanvasId()));
                }
            }
            if (map.get(canvas.getDaoNumber()) != null) {
                List<Object> objectList = map.get(canvas.getDaoNumber());
                objectList.add(canvas);
                map.put(canvas.getDaoNumber(), objectList);
            } else {
                List<Object> objectList = new ArrayList<>();
                objectList.add(canvas);
                map.put(canvas.getDaoNumber(), objectList);
            }
        }
        if (userHarvestTokenList != null && userHarvestTokenList.size() > 0) {
            // 这里每个dao只展示一个minter reward，如果以后改为一个canvsa展示一个，则以canvasId分组
            Map<Integer, UserHarvestToken> userHarvestTokenmap =
                    userHarvestTokenList.stream().collect(Collectors.toMap(UserHarvestToken::getDaoId, v -> v));
            List<Dao> daoListArray = daoService.listByIds(userHarvestTokenmap.keySet());
            Map<Integer, Dao> daoMap = daoListArray.stream().collect(Collectors.toMap(Dao::getId, v -> v));
            for (Integer daoId : userHarvestTokenmap.keySet()) {
                UserHarvestToken userHarvestToken = userHarvestTokenmap.get(daoId);
                Canvas canvas = canvasService.getById(userHarvestToken.getCanvasId());
                BigDecimal swapEth =
                        userHarvestToken.getSwapEth() == null ? BigDecimal.ZERO : userHarvestToken.getSwapEth();
                BigDecimal swapToken =
                        userHarvestToken.getSwapToken() == null ? BigDecimal.ZERO : userHarvestToken.getSwapToken();
                BigDecimal transferToken =
                        userHarvestToken.getTransferToken() == null ? BigDecimal.ZERO : userHarvestToken.getTransferToken();
                BigDecimal unclaimedToken = userHarvestToken.getUnclaimedToken() == null ? BigDecimal.ZERO
                        : userHarvestToken.getUnclaimedToken();
                BigDecimal receivedToken =
                        userHarvestToken.getReceivedToken() == null ? BigDecimal.ZERO : userHarvestToken.getReceivedToken();

                BigDecimal unclaimedEth = userHarvestToken.getUnclaimedEth() == null ? BigDecimal.ZERO
                        : userHarvestToken.getUnclaimedEth();
                BigDecimal receivedEth =
                        userHarvestToken.getReceivedEth() == null ? BigDecimal.ZERO : userHarvestToken.getReceivedEth();

                Dao daoHarvest = daoMap.get(daoId);
                if (daoHarvest == null) {
                    continue;
                }
                //钱包这里topup模式的不展示
                if (TrueOrFalseEnum.TRUE.getStatus().equals(daoHarvest.getTopupMode())) {
                    continue;
                }
                if (!ProtoDaoConstant.D4APause && !daoHarvest.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())
                        && unclaimedToken.compareTo(BigDecimal.ZERO) > 0) {
                    if (daoHarvest.getDaoVersion() == 3) {
                        projectId2List.add(CommonUtil.addHexPrefixIfNotExist(daoHarvest.getProjectId()));
                    } else {
                        projectIdList.add(CommonUtil.addHexPrefixIfNotExist(daoHarvest.getProjectId()));
                    }
                }
                canvas.setSwapEth(swapEth);
                canvas.setSwapToken(swapToken);
                canvas.setTransferToken(transferToken);
                canvas.setUnclaimedToken(unclaimedToken);
                canvas.setReceivedToken(receivedToken);
                canvas.setReceivedEth(receivedEth);
                canvas.setUnclaimedEth(unclaimedEth);
                canvas.setIsMinter(true);
                // minter 当canvsa停机的时候不影响领取代币
                canvas.setCanvasStatus(CanvasStatusEnum.CREATED.getStatus());
                if (map.get(canvas.getDaoNumber()) != null) {
                    List<Object> objectList = map.get(canvas.getDaoNumber());
                    objectList.add(canvas);
                    map.put(canvas.getDaoNumber(), objectList);
                } else {
                    List<Object> objectList = new ArrayList<>();
                    objectList.add(canvas);
                    map.put(canvas.getDaoNumber(), objectList);
                }
            }
        }

        if (map.keySet().size() > 0) {
            userWalletUnclaimedIncomeVo.setCanvasIdList(canvasIdList);
            userWalletUnclaimedIncomeVo.setProjectIdList(projectIdList);
            userWalletUnclaimedIncomeVo.setCanvasId2List(canvasId2List);
            userWalletUnclaimedIncomeVo.setProjectId2List(projectId2List);
            List<UserWalletDaoIncomeVo.DaoIncomeVo> daoIncomeVos = new ArrayList<>();
            for (Integer daoNumber : map.keySet()) {
                List<Object> objectList = map.get(daoNumber);
                UserWalletDaoIncomeVo.DaoIncomeVo daoIncomeVo =
                        UserWalletDaoIncomeVo.transfer(objectList, userProfileReqVo.getUserAddress());
                daoIncomeVos.add(daoIncomeVo);
            }

            daoIncomeVos =
                    daoIncomeVos.stream().filter(v -> v != null && v.getDaoNumber() != null).sorted(Comparator.comparing(UserWalletDaoIncomeVo.DaoIncomeVo::getDaoNumber))
                            .collect(Collectors.toList());
            daoIncomeVos.forEach(v -> {
                if (v.getErc20PaymentMode()) {
                    v.setMintRewardToken(v.getMintReward());
                    v.setMintReward(BigDecimal.ZERO);
                }
            });
            userWalletUnclaimedIncomeVo.setDaoIncomeVos(daoIncomeVos);
        }

        result.setData(userWalletUnclaimedIncomeVo);

        return result;
    }


    /**
     * 1.8 钱包中以dao为维度返回的topup余额信息(修改接口)
     */
    @PostMapping(value = "/topup/balance")
    public ResultList<UserTopupBalanceVo> topupBalance(@RequestBody(required = false) UserProfileReqVo userProfileReqVo) {
        // 待修改,user_topup表废弃，修改余额为 用户持有的NFT的top-up balance余额汇总，精确到小数点后四位。（如果两个余额都为0，则不展示）
        // 用amount_work_id联查work表，onwerAddress=当前用户的。group by dao id,sum(work_topup.amount)..
        ResultList<UserTopupBalanceVo> result = new ResultList<>();
        List<UserTopupBalanceVo> userTopupBalanceVos = new ArrayList<>();
        result.setDataList(userTopupBalanceVos);

        String userAddress = userProfileReqVo.getUserAddress();
        if (StringUtils.isBlank(userAddress)) {
            result.setResultCode(ResultDesc.USER_ERROR.getResultCode());
            result.setResultDesc("Please login.");
            return result;
        }

        List<WorkTopupDaoBalanceVo> workTopupHarvests = workTopupHarvestService.selectTopUpBalanceByAddress(userAddress);
        if (workTopupHarvests.isEmpty()){
            return result;
        }

        for (WorkTopupDaoBalanceVo workTopupHarvest:workTopupHarvests){
            Dao dao = daoService.getById(workTopupHarvest.getDaoId());
            if (dao == null) {
                continue;
            }
            DaoListVo daoListVo = UserTopupBalanceVo.transfer(dao, userAddress, null);

            UserTopupBalanceVo userTopupBalanceVo = new UserTopupBalanceVo();
            BeanUtils.copyProperties(daoListVo, userTopupBalanceVo);

            userTopupBalanceVo.setTokenBalance(workTopupHarvest.getErc20Amount());
            userTopupBalanceVo.setEthBalance(workTopupHarvest.getEthAmount());

            userTopupBalanceVo.setOnChainEthBalance(workTopupHarvest.getOnChainEthBalance());
            userTopupBalanceVo.setOnChainTokenBalance(workTopupHarvest.getOnChainTokenBalance());

            BigDecimal offChainBalance = workTopupHarvest.getErc20Amount().add(workTopupHarvest.getEthAmount());
            BigDecimal onChainBalance = workTopupHarvest.getOnChainEthBalance().add(workTopupHarvest.getOnChainTokenBalance());

            // 如果不想等，就可以update，为true
            // 如果想等，不可以update，为false
            userTopupBalanceVo.setIsUpdateBalance(!offChainBalance.equals(onChainBalance));

            userTopupBalanceVos.add(userTopupBalanceVo);
        }
        result.setDataList(userTopupBalanceVos);

        return result;
    }


    /**
     * 1.5 返回dao下用户持有的nft资产详情列表
     * @return WorkLockDuration
     * @apiNote 用户在top-up balance页面点击see more后调用，传入当前的dao id
     */
    @PostMapping(value = "/topup/balance/details")
    public ResultList<UserTopupBalanceDetailsVo> topupBalanceMore(@RequestBody DaoIdReqVo daoIdReqVo, HttpServletRequest request) {
        ResultList<UserTopupBalanceDetailsVo> result = new ResultList<>();
        if (StringUtils.isBlank(daoIdReqVo.getDaoId())){
            result.setResultDesc("dao id is null.");
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        // 这个dao 下谁绑定了属于我的work
        // 查询top-up-work下，这个daoId下有被amount，且数量>0，且amount_workId所对应的owner是当前登陆用户
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isBlank(userAddress)) {
            result.setResultDesc("please login.");
            result.setResultCode(ResultDesc.USER_ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.getById(daoIdReqVo.getDaoId());
        if (dao == null) {
            result.setResultDesc("dao is not exist.");
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }

        List<UserTopupBalanceDetailsVo> userTopupBalanceDetailsVos = workTopupHarvestService.selectTopUpSeeMore(userAddress,daoIdReqVo.getDaoId());

        userTopupBalanceDetailsVos.forEach(vo -> {
            vo.setPayCurrencyType(dao.getPayCurrencyType());
            vo.setInputTokenAddress(dao.getInputTokenAddress());
            vo.setDaoSymbol(dao.getDaoSymbol());
            vo.setDaoErc20Address(CommonUtil.addHexPrefixIfNotExist(dao.getErc20Token()));
        });



        result.setDataList(userTopupBalanceDetailsVos);
        return result;
    }


    /**
     * 1.8 钱包中以dao为维度返回的topup pending信息
     */
    @PostMapping(value = "/topup/balance/pending")
    public ResultList<UserTopupBalancePendingVo> topupBalancePending(@RequestBody(required = false) UserProfileReqVo userProfileReqVo) {

        ResultList<UserTopupBalancePendingVo> result = new ResultList<>();

        String userAddress = userProfileReqVo.getUserAddress();
        if (StringUtils.isBlank(userAddress)) {
            result.setResultCode(ResultDesc.USER_ERROR.getResultCode());
            result.setResultDesc("Please login.");
            return result;
        }
        List<DaoProjectVo> workTopupDaoBalanceVos = workTopupHarvestService.selectPendingBalanceByAddress(userAddress);
        if (workTopupDaoBalanceVos.isEmpty()){
            result.setDataList(new ArrayList<>());
            return result;
        }

//        Result<String> resultBlockNum = iSubscriptionService.ethGetBlockNumber(ProtoDaoConstant.netWork);
//        if (resultBlockNum.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//            log.error("[drbInfo] ethGetBlockNumber error:{}", result.getResultDesc());
//            result.setResultCode(ResultDesc.ERROR.getResultCode());
//            result.setResultDesc("network anomaly！ please try again later!");
//            return result;
//        }

//        BigDecimal blockNumber =  new BigDecimal(CommonUtil.hexToTenString(resultBlockNum.getData())); // 当前区块数

        List<UserTopupBalancePendingVo> userTopupBalancePendingVos = new ArrayList<>();
        for (DaoProjectVo daoProjectVo:workTopupDaoBalanceVos){
            UserTopupBalancePendingVo userTopupBalancePendingVo = UserTopupBalancePendingVo.transfer(daoProjectVo);
            userTopupBalancePendingVos.add(userTopupBalancePendingVo);
        }
        result.setDataList(userTopupBalancePendingVos);

        return result;
    }


    /**
     * 1.8 钱包中以dao为维度返回的topup pending信息 detail信息
     */
    @PostMapping(value = "/topup/balance/pending/detail")
    public ResultList<UserTopupBalancePendingDetailVo> topupBalancePendingDetail(@RequestBody(required = false) DaoProjectVo daoProjectVo) {

        ResultList<UserTopupBalancePendingDetailVo> result = new ResultList<>();

        String userAddress = daoProjectVo.getUserAddress();
        if (StringUtils.isBlank(userAddress)) {
            result.setResultCode(ResultDesc.USER_ERROR.getResultCode());
            result.setResultDesc("Please login.");
            return result;
        }

        Result<String> resultBlockNum = iSubscriptionService.ethGetBlockNumber(ProtoDaoConstant.netWork);
        if (resultBlockNum.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[drbInfo] ethGetBlockNumber error:{}", result.getResultDesc());
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            result.setResultDesc("network anomaly！ please try again later!");
            return result;
        }

        BigDecimal blockNumber =  new BigDecimal(CommonUtil.hexToTenString(resultBlockNum.getData())); // 当前区块数

        List<UserTopupBalancePendingDetailVo> userTopupBalancePendingDetailVos = workTopupHarvestService.selectTopUpPendingSeeMore(userAddress, CommonUtil.removeHexPrefixIfExists(daoProjectVo.getProjectId()));
        for (UserTopupBalancePendingDetailVo userTopupBalancePendingDetailVo : userTopupBalancePendingDetailVos){
            userTopupBalancePendingDetailVo.setOperationTime(userTopupBalancePendingDetailVo.getCreateTimestamp().getTime());

            // minted dao 可能是sub dao
            Dao mintDao = daoService.getById(userTopupBalancePendingDetailVo.getMintedDaoId());
            userTopupBalancePendingDetailVo.setEndBlockTime(computedEndBlockTime(mintDao,blockNumber));
        }
        result.setDataList(userTopupBalancePendingDetailVos);
        return result;
    }



    /**
     * 1.8 返回topup balance reward信息
     */
    @PostMapping(value = "/topup/balance/reward")
    public ResultList<UserTopupRewardVo> topupBalanceReward(@RequestBody(required = false) UserProfileReqVo userProfileReqVo) {
        // TODO 修改接口
        // 获取当前登陆用户下可以领取的钱和未领取的钱有不为0的seed nodes list
        ResultList<UserTopupRewardVo> result = new ResultList<>();

        log.info("[topupBalanceReward] userAddress:{}",userProfileReqVo.getUserAddress());
        List<UserTopupRewardVo> userTopupRewardVos = nftRewardAllocationService.selectUserTopupRewardVo(userProfileReqVo.getUserAddress());
        if (userTopupRewardVos.isEmpty()){
            result.setDataList(new ArrayList<>());
            return result;
        }
        for (UserTopupRewardVo userTopupRewardVo : userTopupRewardVos){
            userTopupRewardVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(userTopupRewardVo.getProjectId()));
        }

        // userTopupRewardVos = UserTopupRewardVo.transfer(userTopupRewardVos);

        result.setDataList(userTopupRewardVos);
        return result;
    }


    /**
     * 1.8 返回topup balance reward信息
     */
    @PostMapping(value = "/topup/balance/reward/detail")
    public ResultList<UserTopupRewardDetailVo> topupBalanceRewardDetail(@RequestBody(required = false) DaoProjectVo daoProjectVo) {

        // TODO 新增接口
        // 获取指定seed nodes下所有plan的明细
        ResultList<UserTopupRewardDetailVo> result = new ResultList<>();

        List<UserTopupRewardDetailVo> rewardDetailList = nftRewardAllocationService.selectUserTopupRewardDetailVo(CommonUtil.removeHexPrefixIfExists(daoProjectVo.getProjectId()),daoProjectVo.getUserAddress());
        if (rewardDetailList.isEmpty()){
            result.setDataList(new ArrayList<>());
            return result;
        }

        for (UserTopupRewardDetailVo userTopupRewardDetailVo : rewardDetailList){
            BigDecimal collectableAmount =  collectRecordService.getPlanTotalCollectedByDaoId(daoProjectVo.getUserAddress(),userTopupRewardDetailVo.getPlanCode());
            userTopupRewardDetailVo.setCollectedAmount(collectableAmount!=null?collectableAmount:BigDecimal.ZERO);
        }

        result.setDataList(rewardDetailList);
        return result;
    }

    /**
     * 1.8 根据dao id返回 用户在这一系列dao中开启了topup下的nodes，所有nft信息
     */
    @PostMapping(value = "/topup/nft")
    public ResultList<TopupNftListVo> topupNftList(@RequestBody(required = false) DaoProjectVo daoProjectVo) {

        // 需要查询当前登陆账户的
        ResultList<TopupNftListVo> result = new ResultList<>();
        List<TopupNftListVo> topupNftListVos = workTopupHarvestService.getTopupNftListVoByProjectAndAddress(CommonUtil.removeHexPrefixIfExists(daoProjectVo.getProjectId()),daoProjectVo.getUserAddress());
        result.setDataList(topupNftListVos);
        return result;
    }


    // -------------  private ------------------
    private static Long computedEndBlockTime(Dao dao,BigDecimal blockNumber){
        if (dao==null){
            return null;
        }
        BigDecimal denominator = new BigDecimal(dao.getDuration()).divide(new BigDecimal("1e18"));  // 分母=每周期出块数量
        log.info("分母=每周期出块数量:"+denominator);
        log.info("当前区块数:"+blockNumber);
        BigDecimal startBlockNumber = new BigDecimal(dao.getDaoStartBlock()); // 开始区块
        log.info("开始区块:"+startBlockNumber);
        BigDecimal currentRound = new BigDecimal(Integer.parseInt(dao.getCurrentRound())-1);   // 已经完成周期数
        log.info("当前周期数:"+currentRound);
        BigDecimal numThisCurrentRound = blockNumber.subtract(startBlockNumber).subtract(currentRound.multiply(denominator));

        if (dao.getDaoRestartBlock()!=null){
            log.info("重新开始的区块高度为:"+dao.getDaoRestartBlock());
            BigDecimal restartBlock = new BigDecimal(dao.getDaoRestartBlock());
            BigDecimal roundSub = restartBlock.subtract(startBlockNumber);
            log.info("重新开始的时间-开始的时间的差值:"+roundSub);
            BigDecimal[] resultDiv = roundSub.divideAndRemainder(denominator);
            log.info("时间差对每个周期的区块数相除的值:"+ JacksonUtil.obj2json(resultDiv));
            BigDecimal blockRemainder = resultDiv[1];
            numThisCurrentRound = numThisCurrentRound.subtract(blockRemainder);
        }
        log.info("当前周期内已经出了多少块:"+numThisCurrentRound);

        BigDecimal numerator = denominator.subtract(numThisCurrentRound);
        log.info("分子：还有多少块到下个周期:"+numerator);
        // 如果小于0，返回0
        if (numerator.compareTo(BigDecimal.ZERO)<0){
            // 代表即将结束
            return 0L;
        }
        return numerator.multiply(new BigDecimal(ProtoDaoConstant.BLOCK_SECONDS)).multiply(new BigDecimal("1000")).longValue();
    }

}
