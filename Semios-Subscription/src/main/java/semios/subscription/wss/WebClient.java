package semios.subscription.wss;

import javax.websocket.*;

/**
 * @description: client
 * @author: xiangbin
 * @create: 2022-04-07 18:42
 **/
@ClientEndpoint
public class WebClient {

    private Session session;

    @OnOpen
    public void open(Session session) {
        System.out.println("open.....");
        this.session = session;
    }

    @OnMessage
    public void onMessage(String message) {
        System.out.println("onMessage..." + message);
    }

    @OnClose
    public void onClose() {
        System.out.println("onClose....");
    }


    /**
     * 发送客户端消息到服务端
     *
     * @param message 消息内容
     */
    public void send(String message) {
        this.session.getAsyncRemote().sendText(message);
    }
}
