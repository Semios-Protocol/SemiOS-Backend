package semios.api.utils.merkle;

import org.bouncycastle.util.Arrays;
import org.web3j.crypto.Hash;
import org.web3j.utils.Numeric;

public final class Hashing {

    public static String combineHash(String left, String right) {
        byte[] leftByte = Numeric.hexStringToByteArray(left);
        byte[] rightByte = Numeric.hexStringToByteArray(right);
        int n = Arrays.compareUnsigned(leftByte, rightByte);
        if (n > 0) {
            return sha3_node(Bytes.concat(rightByte, leftByte));
        }
        return sha3_node(Bytes.concat(leftByte, rightByte));
    }

    public static String sha3_leaf(String address) {
        return Hash.sha3(Hash.sha3(address));
    }

    public static String sha3_node(byte[] buffer) {
        return Numeric.toHexString(Hash.sha3(buffer));
    }

}
