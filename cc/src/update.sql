ALTER TABLE  `rank` ADD  `power_all` BIGINT( 20 ) UNSIGNED NOT NULL DEFAULT  '0';

CREATE TABLE IF NOT EXISTS `log_gold` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT COMMENT '自增ID',
  `rid` int(11) unsigned NOT NULL COMMENT '角色ID',
  `type` int(11) unsigned NOT NULL COMMENT '策划定义的类型',
  `num` int(11) NOT NULL COMMENT '数量(负数为消耗)',
  `rest` int(11) unsigned NOT NULL COMMENT '剩余金币',
  PRIMARY KEY (`id`),
  KEY `rid` (`rid`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COMMENT='金币流水日志';

CREATE TABLE IF NOT EXISTS `log_card` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT COMMENT '自增ID',
  `rid` int(11) unsigned NOT NULL COMMENT '角色ID',
  `type` int(11) unsigned NOT NULL COMMENT '策划定义的类型',
  `num` int(11) NOT NULL COMMENT '数量(负数为消耗)',
  `rest` int(11) unsigned NOT NULL COMMENT '剩余点券',
  PRIMARY KEY (`id`),
  KEY `rid` (`rid`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COMMENT='点券流水日志';
