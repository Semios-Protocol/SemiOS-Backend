package semios.api.interceptor;

import com.alibaba.fastjson.JSONObject;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.ModelAndView;
import semios.api.model.dto.common.CustomHttpServletRequestWrapper;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.Dao;
import semios.api.model.entity.Work;
import semios.api.model.enums.CanvasStatusEnum;
import semios.api.model.enums.DaoStatusEnum;
import semios.api.model.exceptions.PausedException;
import semios.api.service.ICanvasService;
import semios.api.service.IDaoService;
import semios.api.service.IWorkService;
import semios.api.utils.SpringBeanUtil;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Arrays;
import java.util.List;

/**
 * D4A停机拦截器
 *
 * @description:
 * @author: xiangbin
 * @create: 2022-08-09 14:35
 **/
@Slf4j
@Component
public class CanvasPausedInterceptor implements HandlerInterceptor {

    @Override
    public void afterCompletion(HttpServletRequest request, HttpServletResponse response, Object obj, Exception e)
            throws Exception {

    }

    @Override
    public void postHandle(HttpServletRequest request, HttpServletResponse response, Object obj, ModelAndView mav)
            throws Exception {

    }

    // 拦截每个写请求，直接返回异常
    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object obj) {
        String path = request.getRequestURI().substring(request.getContextPath().length()).replaceAll("[/]+$", "");
        log.info("[CanvasPausedInterceptor]进入拦截器！path:{}", path);

        if ("OPTIONS".equals(request.getMethod().toUpperCase())) {
            return true;
        }

        if (request instanceof CustomHttpServletRequestWrapper) {
            log.info("[CanvasPausedInterceptor]CustomHttpServletRequestWrapper");
            CustomHttpServletRequestWrapper requestWrapper = (CustomHttpServletRequestWrapper) request;
            String body = requestWrapper.getBody();
            // log.info("[CanvasPausedInterceptor]body:{}", body);
            if (!StringUtils.isBlank(body)) {
                JSONObject param = JSONObject.parseObject(body);
                String canvasId = param.getString("canvasId");
                String favoriteId = param.getString("favoriteId");
                String type = param.getString("type");
                String workIds = param.getString("workIds");
                ICanvasService canvasService = SpringBeanUtil.getBean(ICanvasService.class);
                if (canvasService != null) {
                    Canvas canvas = null;
                    if (StringUtils.isNotBlank(canvasId)) {
                        canvas = canvasService.getById(canvasId);
                    }
                    if (StringUtils.isNoneBlank(favoriteId, type)) {
                        if (type.equals("1")) {
                            canvas = canvasService.getById(favoriteId);
                        }
                        if (type.equals("2")) {
                            IWorkService workService = SpringBeanUtil.getBean(IWorkService.class);
                            if (workService != null) {
                                Work work = workService.getById(favoriteId);
                                if (work != null) {
                                    canvas = canvasService.getById(work.getCanId());
                                }
                            }
                        }
                    }

                    if (StringUtils.isNotBlank(workIds)) {
                        List<String> workIdList = Arrays.asList(workIds.split(","));
                        String workId = workIdList.get(0);
                        IWorkService workService = SpringBeanUtil.getBean(IWorkService.class);
                        if (workService != null) {
                            Work work = workService.getById(workId);
                            if (work != null) {
                                canvas = canvasService.getById(work.getCanId());
                            }
                        }
                    }
                    if (canvas != null) {
                        if (canvas.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())) {
                            IDaoService daoService = SpringBeanUtil.getBean(IDaoService.class);
                            Dao dao = null;
                            if (daoService != null) {
                                dao = daoService.getById(canvas.getDaoId());
                                String msg =
                                        dao.getDaoName() + " (D4A@%s) This function is suspended for security reasons.";
                                msg = String.format(msg, canvas.getDaoNumber());
                                throw new PausedException(msg);
                            }

                        }
                        if (canvas.getCanvasStatus().equals(CanvasStatusEnum.SHUT_DOWN.getStatus())) {
                            String msg = canvas.getCanvasName()
                                    + " (D4A@%s/Canvas*%s) This function is suspended for security reasons.";
                            msg = String.format(msg, canvas.getDaoNumber(), canvas.getCanvasNumber());
                            throw new PausedException(msg);
                        }
                    }
                }
            }

        }
        return true;
        // return false;
    }

}