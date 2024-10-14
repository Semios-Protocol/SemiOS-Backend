
# Semios Backend Project

[![CI](https://github.com/Semios-Protocol/SemiOS-Backend/actions/workflows/maven.yml/badge.svg)][gh-ci]
[![License](https://img.shields.io/badge/License-MIT-orange.svg)][mit-license]
[![Chat][tg-badge]][tg-url]

This repository contains the backend services for the **Semios** project.

## Prerequisites

Before running any service, ensure you have the following installed:
- **Java Development Kit (JDK)**: 1.8
- **Maven**: 3.8
- **MySQL**: 5.7

Getting Started
```bash
git clone https://github.com/Semios-Protocol/SemiOS-Backend.git
cd SemiOS-Backend
```
---

## Services Overview

The Semios Backend project consists of the following services:

### 1. Semios Gateway

The gateway service handles the routing and distribution of frontend requests.

#### How to Run
1. If necessary, you can change the URL in semios_api and semios_dex to your service address.
   ```
   Modify the configuration in the Semios-Gateway/src/resources/application-test.yml file
   Default:
   API:  http://localhost:9480 
   DEX:  http://localhost:9483
   ```
2. Build and run the service:

   Path_To_GC_Log_File:The path to GC log files, such as:/home/ubuntu/semios/gateway/logs/gc.log

   Path_To_Log:The path to store the log file, such as:/home/ubuntu/semios/gateway/logs/gateway.log

   Service_IP: Service IP address, such as: 127.0.0.1
   ```bash
   cd Semios-Gateway
   mvn clean install && mvn package
   java -jar -Xms128m -Xmx128m -XX:MetaspaceSize=64M -XX:MaxMetaspaceSize=128M -XX:+UseG1GC -Xloggc:{Path_To_GC_Log_File} -XX:+PrintGCDetails -XX:+PrintGCTimeStamps -Dspring.profiles.active=test -Dserver.port=9482 -Dlogging.file.path={Path_To_Log} -Dserver.address={Service_IP} target/protodao-gateway-0.0.1-SNAPSHOT.jar
   ```

---

### 2. Semios Subscription

The subscription service is responsible for listening to events from the blockchain and smart contracts.The API service manages user requests and interactions.[Subscription File](Semios-Subscription/README.md)


#### How to Run
1. Import the database script:
   ```sql
   source Create-Sql/Subscription.sql
   ```
2. Build and run the service:

   Path_To_GC_Log_File:The path to GC log files, such as:/home/ubuntu/semios/subscription/logs/gc.log

   Path_To_Log:The path to store the log file, such as:/home/ubuntu/semios/subscription/logs/subscription.log

   Service_IP: Service IP address, such as: 127.0.0.1

   Database_User_Name: The database username, such as: root

   Database_Connection_Password: The database connection password, such as: 123456

   Ankr_Key: The Ankr API key，used to connect to ankr services, such as: 5b6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c
   ```bash
   cd Semios-Subscription
   mvn clean install && mvn package
   java -jar -Xms256m -Xmx256m -XX:MetaspaceSize=64M -XX:MaxMetaspaceSize=128M -XX:+UseG1GC -Xloggc:{Path_To_GC_Log_File} -XX:+PrintGCDetails -XX:+PrintGCTimeStamps -Dspring.profiles.active=test -Dserver.port=9380 -Dspring.datasource.username={Database_User_Name} -Dspring.datasource.password={Database_Connection_Password} -Dankr.secret.key={Ankr_Key} -Dlogging.file.path={Path_To_Log} -Dserver.address={Service_IP} target/subscribe-0.0.1-SNAPSHOT.jar
   ```
---

### 3. Semios API

The API service manages user requests and interactions.[Api File](Semios-Api/README.md)
#### How to Run
1. Import the database script:
   ```sql
   source Create-Sql/Api.sql
   ```
2. Build and run the service:
   Path_To_GC_Log_File:The path to GC log files, such as:/home/ubuntu/semios/subscription/logs/gc.log

   Path_To_Log:The path to store the log file, such as:/home/ubuntu/semios/subscription/logs/subscription.log

   Service_IP: Service IP address, such as: 127.0.0.1

   Database_User_Name: The database username, such as: root

   Database_Connection_Password: The database connection password, such as: 123456

   Subscription_Service: The subscription service address, such as: http://127.0.0.1:9380

   OpenSea_API_Key: The OpenSea API key, such as: 5b6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c
   ```bash
   cd Semios-Api
   mvn clean install && mvn package
   java -jar -Xms512m -Xmx512m -XX:MetaspaceSize=64M -XX:MaxMetaspaceSize=128M -XX:+UseG1GC -Xloggc:{Path_To_GC_Log_File} -XX:+PrintGCDetails -XX:+PrintGCTimeStamps -Dspring.profiles.active=test -Dserver.port=9480 -Dsubscription.service.url={Subscription_Service} -Dspring.datasource.username={Database_User_Name} -Dspring.datasource.password={Database_Connection_Password} -Dlogging.file.path={Path_To_Log} -Dopensea_api_key={OpenSea_API_Key} -Dserver.address={Service_IP} target/protodao-api-0.0.1-SNAPSHOT.jar
   ```

---

### 4. Semios DEX

The DEX (Decentralized Exchange) service handles all DEX-related business operations.[Dex File](Semios-Dex/README.md)

#### How to Run
1. Import the database script:
   ```sql
   source Create-Sql/Dex.sql
   ```
2. Build and run the service:
   Path_To_GC_Log_File:The path to GC log files, such as:/home/ubuntu/semios/subscription/logs/gc.log

   Path_To_Log:The path to store the log file, such as:/home/ubuntu/semios/subscription/logs/subscription.log

   Service_IP: Service IP address, such as: 127.0.0.1

   Database_User_Name: The database username, such as: root

   Database_Connection_Password: The database connection password, such as: 123456

   Subscription_Service: The subscription service address, such as: http://127.0.0.1:9380
   ```bash
   cd Semios-Dex
   mvn clean && mvn package
   java -jar -Xms128m -Xmx128m -XX:MetaspaceSize=64M -XX:MaxMetaspaceSize=128M -XX:+UseG1GC -Xloggc:{Path_To_GC_Log_File} -XX:+PrintGCDetails -XX:+PrintGCTimeStamps -Dspring.profiles.active=test -Dserver.port=9483 -Dsubscription.service.url={Subscription_Service} -Dspring.datasource.username={Database_User_Name} -Dspring.datasource.password={Database_Connection_Password} -Dlogging.file.path={Path_To_Log} -Dserver.address={Service_IP} target/protodao-dex-0.0.1-SNAPSHOT.jar
   ```

---

## License

This project is licensed under the [MIT License](https://opensource.org/license/mit/).

[gh-ci]: https://github.com/Semios-Protocol/SemiOS-Backend/actions/workflows/maven.yml
[mit-license]: https://opensource.org/license/mit/
[tg-url]: https://t.me/c/2070866902/1
[tg-badge]: https://img.shields.io/badge/chat-telegram-blue
