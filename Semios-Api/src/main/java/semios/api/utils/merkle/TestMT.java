package semios.api.utils.merkle;

import org.web3j.crypto.Keys;
import org.web3j.utils.Numeric;
import semios.api.utils.JacksonUtil;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author fjtan
 */
public class TestMT {

    @SuppressWarnings("unchecked")
    public static List<String> checkAddress(String addressStr) throws Exception {
        List<String> result = new ArrayList<String>();
        // String[] array = JacksonUtil.json2pojo(addressStr, String[].class);
        // if (array == null) {
        // throw new Exception("address to array error:" + addressStr);
        // }
        List<String> addressList = JacksonUtil.json2pojo(addressStr, List.class);
        Set<String> addressSet = new HashSet<String>();
        for (String address : addressList) {
            if (!address.matches("^(0x)?[0-9a-fA-F]{40}$")) {
                throw new RuntimeException(address + " Wrong address please modify and resubmit");
            }
            address = Numeric.prependHexPrefix(address);
            Pattern pattern = Pattern.compile("([A-F].*[a-f])|([a-f].*[A-F])");
            Matcher matcher = pattern.matcher(address);
            if (matcher.find()) {
                String checkAddress = Keys.toChecksumAddress(address);
                if (!address.equals(checkAddress)) {
                    throw new RuntimeException(address + " Wrong address please modify and resubmit");
                }
            }
            address = address.toLowerCase();
            if (!addressSet.contains(address)) {
                addressSet.add(address);
                result.add(address);
            }
        }
        return result;
    }

    public static void main(String[] args) throws Exception {
        System.out.println(Keys.toChecksumAddress("0x000000000000000000000000000000000000dead"));
        String str = "[\"0x1111111111111111111111111111111111110001\",\"0x1111111111111111111111111111111111110002\",\"0x1111111111111111111111111111111111110005\",\"0x1111111111111111111111111111111111111000\"]";
        long startTime = System.currentTimeMillis();
        List<String> list = checkAddress(str);
        long endTime = System.currentTimeMillis();
        System.out.println("校验耗时：" + (endTime - startTime));
        System.out.println(list.get(0));
        // System.out.println(JacksonUtil.obj2json(list));
        // String[] array = JacksonUtil.json2pojo(str, String[].class);
        // System.out.println(Arrays.toString(array));
        // List<String> list1 = Arrays.asList(array);
        // list1.stream().forEach(System.out::println);
        // String uri = "https://test-dao4art.s3.ap-southeast-1.amazonaws.com/W16653005415300444.json";
        // System.out.println(Hashing.sha3_node(uri.getBytes("UTF-8")));
        // List<String> list = new ArrayList<String>();
        // list.add("0x1111111111111111111111111111111111110001");
        // list.add("0x1111111111111111111111111111111111110002");
        // list.add("0x1111111111111111111111111111111111110005");
        // list.add("0x1111111111111111111111111111111111111000");
        startTime = System.currentTimeMillis();
        MerkleTree mt = new MerkleTree(list);
        mt.init();
        endTime = System.currentTimeMillis();
        System.out.println("生成耗时：" + (endTime - startTime));

        startTime = System.currentTimeMillis();
        String dump = JacksonUtil.obj2json(mt);
        System.out.println(dump);
        endTime = System.currentTimeMillis();
        System.out.println("打印耗时：" + (endTime - startTime));

        startTime = System.currentTimeMillis();
        System.out.println(mt.getRootHash());
        endTime = System.currentTimeMillis();
        System.out.println("hash耗时：" + (endTime - startTime));

        MerkleTree mt1 = JacksonUtil.json2pojo(dump, MerkleTree.class);
        startTime = System.currentTimeMillis();
        System.out.println(JacksonUtil.obj2json(MerkleTree.getProof(mt1.getTree(), 5)));
        endTime = System.currentTimeMillis();
        System.out.println("生成proof耗时：" + (endTime - startTime));
    }

}
