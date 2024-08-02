package semios.api.utils;

import net.jodah.expiringmap.ExpiringMap;

import java.util.concurrent.TimeUnit;

public class NodeNameMapUtil {

    private static final ExpiringMap<String, String> privateMap =
            ExpiringMap.builder().expiration(180, TimeUnit.SECONDS).maxSize(10000).build();


    public static synchronized void put(String nodeName, String address) {
        privateMap.put(nodeName, address);
    }

    public static synchronized String get(String nodeName) {
        return privateMap.get(nodeName);
    }
}
