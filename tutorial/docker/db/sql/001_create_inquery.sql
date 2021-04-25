CREATE TABLE IF NOT EXISTS `status`
(
    `status_id` VARCHAR(20) PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS `inquery`
(
    `id`     INT AUTO_INCREMENT PRIMARY KEY,
    `title`  VARCHAR(100) NOT NULL,
    `status` VARCHAR(20) NOT NULL,
    FOREIGN KEY (`status`)
      REFERENCES status(`status_id`)
);
