package semios.api.interceptor;

import lombok.extern.slf4j.Slf4j;
import org.apache.catalina.connector.ClientAbortException;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.exceptions.LoginException;
import semios.api.model.exceptions.PausedException;

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
    public Result<Void> exceptionHandler(HttpServletRequest request, HttpServletResponse response, Exception e) {
        log.error("[GlobalExceptionHandler]url:{}e:", request.getRequestURL(), e);
        Result<Void> result = new Result<>();
        result.setResultCode(ResultDesc.ERROR.getResultCode());
        result.setResultDesc("system exception, please try again later!");
        return result;
    }

    @ExceptionHandler(value = {HttpRequestMethodNotSupportedException.class, ClientAbortException.class})
    @ResponseBody
    public Result<Void> httpRequestMethodNotSupportedExceptionHandler(HttpServletRequest request,
                                                                      HttpServletResponse response, Exception e) {
        log.warn("[GlobalExceptionHandler-httpRequestMethodNotSupportedExceptionHandler]url:{}e:",
                request.getRequestURL(), e);
        Result<Void> result = new Result<>();
        result.setResultCode(ResultDesc.ERROR.getResultCode());
        result.setResultDesc("system exception, Request method not supported!");
        return result;
    }

    @ExceptionHandler(value = RuntimeException.class)
    @ResponseBody
    public Result<Void> runtimeExceptionHandler(HttpServletRequest request, HttpServletResponse response, Exception e) {
        log.error("[GlobalExceptionHandler-runtimeExceptionHandler]url:{}e:", request.getRequestURL(), e);
        Result<Void> result = new Result<>();
        result.setResultCode(ResultDesc.ERROR.getResultCode());
        result.setResultDesc(e.getMessage());
        return result;
    }

    @ExceptionHandler(value = PausedException.class)
    @ResponseBody
    public Result<Void> pausedExceptionHandler(HttpServletRequest request, HttpServletResponse response, Exception e) {
        log.info("[GlobalExceptionHandler-pausedExceptionHandler]url:{} e:{}", request.getRequestURL(), e.getMessage());
        Result<Void> result = new Result<>();
        result.setResultCode(ResultDesc.FAIL.getResultCode());
        result.setResultDesc(e.getMessage());
        return result;
    }

    @ExceptionHandler(value = LoginException.class)
    @ResponseBody
    public Result<Void> loginExceptionHandler(HttpServletRequest request, HttpServletResponse response, Exception e) {
        log.info("[GlobalExceptionHandler-loginExceptionHandler]url:{}e:", request.getRequestURL(), e);
        Result<Void> result = new Result<>();
        result.setResultCode(ResultDesc.USER_ERROR.getResultCode());
        result.setResultDesc(e.getMessage());
        return result;
    }
}
