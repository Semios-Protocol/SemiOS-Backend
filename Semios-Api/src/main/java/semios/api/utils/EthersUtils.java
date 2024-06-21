package semios.api.utils;

import org.web3j.crypto.*;
import org.web3j.crypto.Sign.SignatureData;
import org.web3j.utils.Numeric;
import semios.api.utils.eip712.EIP712Message;

import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

public class EthersUtils {
    private static final String MESSAGE_PREFIX = "\u0019Ethereum Signed Message:\n";

    public static String verifyMessage(String message, String signature) {
        return EthersUtils.recoverAddress(EthersUtils.hashMessage(message), signature);
    }

    public static String hashMessage(String message) {
        return Hash.sha3(Numeric.toHexStringNoPrefix(
                (EthersUtils.MESSAGE_PREFIX + message.getBytes().length + message).getBytes(StandardCharsets.UTF_8)));
    }

    public static String recoverAddress(String digest, String signature) {
        System.out.println(digest);
        SignatureData signatureData = EthersUtils.getSignatureData(signature);
        int header = 0;
        for (byte b : signatureData.getV()) {
            header = (header << 8) + (b & 0xFF);
        }
        if (header < 27 || header > 34) {
            return null;
        }
        int recId = header - 27;
        BigInteger key = Sign.recoverFromSignature(recId,
                new ECDSASignature(new BigInteger(1, signatureData.getR()), new BigInteger(1, signatureData.getS())),
                Numeric.hexStringToByteArray(digest));
        if (key == null) {
            return null;
        }
        return ("0x" + Keys.getAddress(key)).trim();
    }

    public static String recoverEip712Address(EIP712Message message, Integer chainId, String signature)
            throws Exception {
        SignatureData signatureData = EthersUtils.getSignatureData(signature);
        int recId = Sign.getRecId(signatureData, chainId);
        StructuredDataEncoder dataEncoder = new StructuredDataEncoder(JacksonUtil.obj2json(message));
        byte[] hashStructuredData = dataEncoder.hashStructuredData();
        BigInteger key = Sign.recoverFromSignature(recId,
                new ECDSASignature(new BigInteger(1, signatureData.getR()), new BigInteger(1, signatureData.getS())),
                hashStructuredData);
        if (key == null) {
            return null;
        }
        return ("0x" + Keys.getAddress(key)).trim();
    }

    private static SignatureData getSignatureData(String signature) {
        byte[] signatureBytes = Numeric.hexStringToByteArray(signature);
        byte v = signatureBytes[64];
        if (v < 27) {
            v += 27;
        }
        byte[] r = (byte[]) Arrays.copyOfRange(signatureBytes, 0, 32);
        byte[] s = (byte[]) Arrays.copyOfRange(signatureBytes, 32, 64);
        return new SignatureData(v, r, s);
    }

    public static void main(String[] args) {
        // String param =
        // "{\"userAddress\":\"0xf8baf7268f3daefe4135f7711473ae8b6c3b47d8\",\"signatureHash\":\"0xe4940b217c98781d4be392ce6a02b83a0de07f7630f563563f7fdb35521853f11e93732f6a482662e0946534f27335b288c1b43ac6b6a06c0f7291f67b8575b11b\",\"originalText\":\"Welcome
        // to DAO4Art！\\nClick to sign in and accept the DAO4Art Terms of Service: https://opensea.io/tos\\nThis request
        // will not trigger a blockchain transaction or cost any gas fees.\\nYour authentication status will reset after
        // 24 hours. \"}";
        // UserSignatureReqVo userSignatureReqVo = JacksonUtil.json2pojo(param, UserSignatureReqVo.class);
        // 签名后的数据
        String signature =
                "0x55f359335c7be11661eb398232b73b70f66cce38c77fa1a331c884cfb2b75af62ffe11e7b6ecaddb9e09176bb3056278801e7b84612fe4d9f858dc1ea67b3d6a1c";
        // 签名原文
        String message =
                "Welcome to DAO4Art！\\n\\nThis request will not trigger a blockchain transaction or cost any gas fees.\\n\\nYour authentication status will reset after 24 hours. \\n\\nWallet address:\\n0x91b8650F6E27d7855A23772CaB9daA46a3Fd4f1f \\n\\nNonce:\\nfzuubkj6-pt34-htsx-9hcy-2dgncpe38z4s";
        // String message = "Welcome to DAO4Art！\\n\\nThis request will not trigger a blockchain transaction or cost any
        // gas fees.\\n\\nYour authentication status will reset after 24 hours. \\n\\nWallet
        // address:\\n0x56dba60a326c8a1e1ed148486a2695884aa34e3b \\n\\nNonce:\\n97at14hl-oydt-f5w0-m8zx-5a7l252jfdgx";
        System.out.println(message);
        message = message.replaceAll("\\\\n", "\\\n");
        System.out.println(message);
        // 签名的钱包地址
        String address = "0x91b8650F6E27d7855A23772CaB9daA46a3Fd4f1f";

        String add = verifyMessage(message, signature);
        System.out.println(add);
        System.out.println(add.equals(address));
    }
}