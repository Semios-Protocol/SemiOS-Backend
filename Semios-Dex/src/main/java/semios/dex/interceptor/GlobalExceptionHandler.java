package semios.dex.interceptor;

import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import semios.dex.model.dto.common.Result;
import semios.dex.model.dto.common.ResultDesc;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * @description: 全局异常处理
 * @author: xiangbin
 * @create: 2022-03-17 10:43
 **/
@Slf4j
@ControllerAdvice
public class GlobalExceptionHandler {

    @ExceptionHandler(value = Exception.class)
    @ResponseBody
    public Result<Object> exceptionHandler(HttpServletRequest request, HttpServletResponse response, Exception e) {
        log.error("[GlobalExceptionHandler]url:{}e:", request.getRequestURL(), e);
        Result<Object> result = new Result<Object>();
        result.setResultCode(ResultDesc.ERROR.getResultCode());
        result.setResultDesc(ResultDesc.ERROR.getResultDesc());
        return result;
    }
}
