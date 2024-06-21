package semios.dex.model.dto.common;

/**
 * @author fjtan
 */
public enum ResultDesc {
    SUCCESS(100, "success"), ERROR(500, "fail, "), PARAM_ERROR(201, "Parameter error, "), SESSION_ERROR(400, "fail, "),
    AUTH_ERROR(401, "fail, "), USER_ERROR(402, "fail, "), NOT_FOUND_ERROR(404, "fail, "), FAIL(300, "fail, ");

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
