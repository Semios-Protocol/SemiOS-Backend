package semios.subscription.listener;

import com.google.common.util.concurrent.ThreadFactoryBuilder;

import java.util.concurrent.*;

/**
 * @description: utils
 * @author: xiangbin
 * @create: 2022-04-14 11:44
 **/
public class ThreadPoolUtils {

    public static final ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(2,
            new ThreadFactoryBuilder()
                    .setNameFormat("EventListener-%d")
                    .setDaemon(true)
                    .build());


    public static final ThreadPoolExecutor filterExecutor = new ThreadPoolExecutor(
            4, 4, 3, TimeUnit.SECONDS,
            new ArrayBlockingQueue<>(20, true),
            new ThreadFactoryBuilder()
                    .setNameFormat("EventListener-filterExecutor-%d")
                    .setDaemon(true)
                    .build()
    );

    public static final ThreadPoolExecutor callbackExecutor = new ThreadPoolExecutor(
            4, 4, 3, TimeUnit.SECONDS,
            new ArrayBlockingQueue<>(20, true),
            new ThreadFactoryBuilder()
                    .setNameFormat("EventListener-callbackExecutor-%d")
                    .setDaemon(true)
                    .build()
    );
}
