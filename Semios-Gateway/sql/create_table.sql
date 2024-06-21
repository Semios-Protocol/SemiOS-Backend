CREATE SCHEMA `hello` DEFAULT CHARACTER SET utf8mb4 ;


CREATE TABLE `hello`.`hello_world` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `name` VARCHAR(45) NULL,
  PRIMARY KEY (`id`));

INSERT INTO `hello`.`hello_world` (`id`, `name`) VALUES ('1', 'hello');
