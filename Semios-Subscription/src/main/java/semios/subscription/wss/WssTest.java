package semios.subscription.wss;

import javax.websocket.ContainerProvider;
import javax.websocket.WebSocketContainer;
import java.net.URI;

/**
 * @description: test
 * @author: xiangbin
 * @create: 2022-04-07 18:45
 **/
public class WssTest {

    public static void main(String[] args) throws Exception {
        try {
            WebSocketContainer container = ContainerProvider.getWebSocketContainer();
            WebClient client = new WebClient();
            container.connectToServer(client, new URI("ws://127.0.0.1:8080/myHandler"));
// container.setDefaultMaxSessionIdleTimeout(5000L);
            int turn = 0;
            while (turn++ < 10) {
                client.send("client send: 客户端消息 " + turn);
                Thread.sleep(1000);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        Thread.sleep(1000);

    }
}
