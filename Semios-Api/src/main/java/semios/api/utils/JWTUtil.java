package semios.api.utils;

import com.auth0.jwt.JWT;
import com.auth0.jwt.algorithms.Algorithm;
import com.auth0.jwt.exceptions.JWTDecodeException;
import com.auth0.jwt.interfaces.DecodedJWT;
import com.auth0.jwt.interfaces.JWTVerifier;
import com.google.common.io.BaseEncoding;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import org.springframework.stereotype.Component;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import java.util.Date;

/**
 * @ClassName: JWTUtil
 * @Description: 实现对用户名和密码的加密处理，校验token是否正确，获取用户名等操作
 * Algorithm algorithm = Algorithm.HMAC256(password) 是对密码进行加密后再与用户名混淆在一起
 * 在签名时可以通过 .withExpiresAt(date) 指定token的过期时间
 * @param:
 */
@Component
public class JWTUtil {

    // 过期时间，单位毫秒
    private static final long EXPIRE_TIME = 60 * 1000; // 1分钟
//	private static final long EXPIRE_TIME = 15 * 60 * 1000; // 15分钟

    // 加密密文，私钥
    private static final String TOKEN_SECRET = "jiamimiwen";

    // 由字符串生成加密key
    public static SecretKey generalKey() {
        System.out.println("进入由字符串生成加密key方法！");
        // 本地的密码解码
        byte[] encodedKey = BaseEncoding.base64().decode(TOKEN_SECRET);
        // 根据给定的字节数组使用AES加密算法构造一个密钥
        SecretKey key = new SecretKeySpec(encodedKey, 0, encodedKey.length, "AES");
        return key;
    }

    // 生成签名
    public static String sign(int id, String username, String password) {
        System.out.println("生成签名方法开始执行！");
        try {
            // 设置过期时间,单位毫秒
            Date expTime = new Date(System.currentTimeMillis() + EXPIRE_TIME);
            // 私钥和加密算法
            Algorithm algorithm = Algorithm.HMAC256(password); //使用用户输入的密码
//			Algorithm algorithm = Algorithm.HMAC256(TOKEN_SECRET);
            // 设置头部信息,也可以不用设置头部信息jwt会自动生成
//			Map<String, Object> header = new HashMap<String, Object>();
//			header.put("typ", "JWT");
//			header.put("alg", "HS256");
            // 或
            // header.put("Type", "JWT");
            //	header.put("alg", "HS256");
            // 生成JWT的时间
            Date issuedAt = new Date(System.currentTimeMillis());
            // 返回token字符串
            System.out.println("生成签名方法结束执行！");
            return JWT.create() // 表示new一个Jwt，设置jwt的body
//					.withHeader(header) // 设置头部信息
                    .withClaim("id", id) // 数据库中用户的id
                    .withClaim("username", username) // 前端输入的用户名
                    .withIssuedAt(issuedAt) // jwt的签发时间
                    .withExpiresAt(expTime) // jwt过期时间
                    .sign(algorithm);
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * @throws
     * @Title: verify
     * @Description: 检验token是否正确
     * @param: @param token 密钥
     * @param: @param username 登录名
     * @param: @param password 密码
     * @param: @return
     * @return: boolean
     */
    public static boolean verify(String token, String username, String password) {
        System.out.println("进入检验token是否正确方法！");
        try {
            Algorithm algorithm = Algorithm.HMAC256(password); //使用用户输入的密码
//			Algorithm algorithm = Algorithm.HMAC256(TOKEN_SECRET);
            JWTVerifier verifier = JWT.require(algorithm).withClaim("username", username).build();
//			JWTVerifier verifier = JWT.require(algorithm).build();
            verifier.verify(token);
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    // 获取登录名
    public static String getUsername(String token) {
        System.out.println("进入获取登录名方法！");
        try {
            DecodedJWT jwt = JWT.decode(token);
            return jwt.getClaim("username").asString();
        } catch (JWTDecodeException e) {
            return null;
        }
    }

    // 解密jwt
    public static Claims parseJWT(String jwt) throws Exception {
        System.out.println("进入解密jwt方法！");
        SecretKey key = generalKey(); // 签名秘钥，和生成的签名的秘钥一模一样
        Claims claims = Jwts.parser() // 得到DefaultJwtParser
                .setSigningKey(key) // 设置签名的秘钥
                .parseClaimsJws(jwt).getBody(); // 设置需要解析的jwt
        return claims;
    }

}
