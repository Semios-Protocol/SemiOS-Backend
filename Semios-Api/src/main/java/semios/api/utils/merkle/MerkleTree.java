package semios.api.utils.merkle;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;
import org.bouncycastle.util.Arrays;
import org.web3j.abi.TypeEncoder;
import org.web3j.abi.datatypes.Address;
import org.web3j.utils.Numeric;

import java.util.*;

@Data
public class MerkleTree {

    private String[] tree;

    private List<LeafValue> values;

    @JsonIgnore
    private List<String> addressList;

    public MerkleTree() {
    }

    public MerkleTree(List<String> addressList) {
        this.addressList = addressList;
    }

    private static int leftChildIndex(int index) {
        return 2 * index + 1;
    }

    private static int rightChildIndex(int index) {
        return 2 * index + 2;
    }

    private static int parentIndex(int index) {
        return (int) Math.floor((index - 1) / 2);
    }

    private static int siblingIndex(int index) {
        return index - (int) Math.pow(-1, (index % 2));
    }

    @JsonIgnore
    public static List<String> getProof(String[] tree, int index) {
        List<String> proof = new ArrayList<String>();
        while (index > 0) {
            proof.add(tree[siblingIndex(index)]);
            index = parentIndex(index);
        }
        return proof;
    }

    public void init() throws Exception {
        Map<String, String> addressHashMap = new HashMap<String, String>();
        List<String> leafHashList = new ArrayList<String>();
        for (String address : this.addressList) {
            String leafHash = Hashing.sha3_leaf(TypeEncoder.encode(new Address(address)));
            addressHashMap.put(address, leafHash);
            leafHashList.add(leafHash);
        }

        Collections.sort(leafHashList, new Comparator<String>() {
            @Override
            public int compare(String b1, String b2) {
                return Arrays.compareUnsigned(Numeric.hexStringToByteArray(b1), Numeric.hexStringToByteArray(b2));
            }
        });

        String[] mtree = new String[2 * leafHashList.size() - 1];
        Map<String, Integer> hashIndexMap = new HashMap<String, Integer>();
        for (int i = 0; i < leafHashList.size(); i++) {
            int index = mtree.length - 1 - i;
            mtree[index] = leafHashList.get(i);
            hashIndexMap.put(leafHashList.get(i), index);
        }

        for (int i = mtree.length - 1 - leafHashList.size(); i >= 0; i--) {
            mtree[i] = Hashing.combineHash(mtree[leftChildIndex(i)], mtree[rightChildIndex(i)]);
        }
        this.tree = mtree;

        List<LeafValue> list = new ArrayList<LeafValue>();
        for (int i = 0; i < addressList.size(); i++) {
            String address = addressList.get(i);
            LeafValue value = new LeafValue(address, hashIndexMap.get(addressHashMap.get(address)));
            list.add(value);
        }
        this.values = list;
    }

    @JsonIgnore
    public String getRootHash() {
        return tree == null ? null : tree[0];
    }

}
