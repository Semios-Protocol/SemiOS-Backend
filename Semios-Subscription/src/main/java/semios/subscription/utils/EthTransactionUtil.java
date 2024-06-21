package semios.subscription.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.web3j.abi.FunctionEncoder;
import org.web3j.abi.TypeReference;
import org.web3j.abi.datatypes.Function;
import org.web3j.abi.datatypes.Type;
import org.web3j.crypto.*;
import org.web3j.tx.ChainIdLong;
import org.web3j.utils.Numeric;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class EthTransactionUtil {
    private static final Logger logger = LoggerFactory.getLogger(EthTransactionUtil.class);

    private static ECKeyPair ecKeyPair;
    private static WalletFile walletFile;

    public static String decryptKeystore(String keystore, String password) {
        String privateKey = null;
        try {
            EthTransactionUtil.walletFile = JacksonUtil.json2pojo(keystore, WalletFile.class);
            EthTransactionUtil.ecKeyPair = Wallet.decrypt(password, EthTransactionUtil.walletFile);
            privateKey = EthTransactionUtil.ecKeyPair.getPrivateKey().toString(16);
        } catch (CipherException e) {
            e.printStackTrace();
        }
        return privateKey;
    }

    public static String keccak256Hash(String utf8String) {
        return CommonUtil.removeHexPrefixIfExists(Hash.sha3String(utf8String));
    }

    // encode function
    public static Function constructFunction(String functionName, Type... functionArguments) {
        Type[] args = functionArguments;
        List<Type> argumentList = new ArrayList<>();
        for (Type arg : args) {
            argumentList.add(arg);
        }
        return new Function(functionName, argumentList, Collections.<TypeReference<?>>emptyList());
    }

    public static String encodeFunction(String functionName, Type... functionArguments) {
        Function functionCall = EthTransactionUtil.constructFunction(functionName, functionArguments);
        return FunctionEncoder.encode(functionCall);
    }

    // call contract transaction
    public static RawTransaction constructCallContractTransaction(BigInteger nonce, BigInteger gasPrice, BigInteger gasLimit, String contractAddress, BigInteger value, String functionName, Type... functionArguments) {
        String functionEncoder = EthTransactionUtil.encodeFunction(functionName, functionArguments);
        return RawTransaction.createTransaction(nonce, gasPrice, gasLimit, contractAddress, value, functionEncoder);
    }

    // eth transfer transaction
    public static RawTransaction constructTransferTransaction(BigInteger nonce, BigInteger gasPrice, BigInteger gasLimit, String to, BigInteger value) {
        return RawTransaction.createEtherTransaction(nonce, gasPrice, gasLimit, to, value);
    }

    public static String signRawTransaction(RawTransaction rawTransaction, long chainId, String privateKey) {
        privateKey = CommonUtil.removeHexPrefixIfExists(privateKey);
        ECKeyPair ecKeyPair = ECKeyPair.create(new BigInteger(privateKey, 16));
        Credentials credentials = Credentials.create(ecKeyPair);
        byte[] signedMessage;
        if (chainId > ChainIdLong.NONE) {
            signedMessage = TransactionEncoder.signMessage(rawTransaction, chainId, credentials);
        } else {
            signedMessage = TransactionEncoder.signMessage(rawTransaction, credentials);
        }
        return Numeric.toHexString(signedMessage);
    }

    public static String signRawTransaction(RawTransaction rawTransaction, long chainId, String keystore, String password) {
        String privateKey = EthTransactionUtil.decryptKeystore(keystore, password);
        return signRawTransaction(rawTransaction, chainId, privateKey);
    }

    public static void main(String[] args) {

        System.out.println(keccak256Hash("totalSupply()"));
    }
}
