package semios.subscription.wss;

import org.springframework.stereotype.Component;
import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.TextWebSocketHandler;

import java.util.concurrent.TimeUnit;

/**
 * @description: handler
 * @author: xiangbin
 * @create: 2022-04-07 18:32
 **/
@Component
public class WebSocHandler extends TextWebSocketHandler {

    @Override
    protected void handleTextMessage(WebSocketSession session, TextMessage message) throws Exception {
        String msg = message.getPayload();
        session.sendMessage(new TextMessage("server return:" + msg));
        session.sendMessage(new TextMessage("I while close"));
        TimeUnit.SECONDS.sleep(1);
        session.close();
    }

    @Override
    public void afterConnectionEstablished(WebSocketSession session) throws Exception {
        System.out.println("afterconnection.....");
//        TimeUnit.SECONDS.sleep(3);
        super.afterConnectionEstablished(session);
    }

    @Override
    public void afterConnectionClosed(WebSocketSession session, CloseStatus status) throws Exception {
        System.out.println("afterclosed.....");
        TimeUnit.SECONDS.sleep(3);
        super.afterConnectionClosed(session, status);
    }
}
