package semios.gateway.model.dto.common;

/**
 * @author fjtan
 */
public enum ResultDesc {
    SUCCESS(100, "成功"), ERROR(500, "失败,"), PARAM_ERROR(201, "参数有误,"), SESSION_ERROR(400, "失败,"), AUTH_ERROR(401, "失败,"),
    USER_ERROR(402, "失败,"), NOT_FOUND_ERROR(404, "失败,"), FAIL(300, "失败,");

    private int resultCode;
    private String resultDesc;
    private ResultDesc(int code, String desc) {
        this.resultCode = code;
        this.resultDesc = desc;
    }

    public int getResultCode() {
        return resultCode;
    }

    public String getResultDesc() {
        return resultDesc;
    }
}
