# API Service Project Structure

[![CI](https://github.com/Semios-Protocol/SemiOS-Backend/actions/workflows/maven.yml/badge.svg)][gh-ci]
[![License](https://img.shields.io/badge/License-MIT-orange.svg)][mit-license]
[![Chat][tg-badge]][tg-url]


This project provides the API services and follows a modular structure to ensure maintainability and scalability. Below is the directory structure:

```
/sql
```
Contains the records for database changes across different versions.

```
/src/main/java
```
This directory includes all the Java files related to the API services.

### `/src/main/java/com/semios/api`

This folder contains all the files related to the API services:

- **/controller**: Manages incoming requests from the frontend.
- **/filter**: Contains filters that process incoming HTTP requests.
- **/interceptor**: Handles request interceptors, such as validating user login status.
- **/listener**: Includes listeners for processing all events triggered during service runtime.
- **/mapper**: Responsible for executing database operations.
- **/model**: Defines all the object models used in the application.
- **/schedule**: Houses scheduled tasks.
- **/service**: Contains business logic implementations.
- **/util**: Utility classes used throughout the application.

```
/src/main/test
```
Includes all test files for unit and integration testing.

```
/src/main/resources
```
Contains all configuration files, such as application properties.


## License

This project is licensed under the [MIT License](https://opensource.org/license/mit/).

[gh-ci]: https://github.com/Semios-Protocol/SemiOS-Backend/actions/workflows/maven.yml
[mit-license]: https://opensource.org/license/mit/
[tg-url]: https://t.me/c/2070866902/1
[tg-badge]: https://img.shields.io/badge/chat-telegram-blue
