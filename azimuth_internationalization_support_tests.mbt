// Azimuth 国际化支持测试用例
// 专注于遥测系统的多语言、多地区和本地化功能

// 测试1: 多语言错误消息和日志
test "多语言错误消息和日志测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "i18n.test.logger")
  
  // 创建国际化管理器
  let i18n_manager = I18nManager::new()
  
  // 添加不同语言的错误消息
  I18nManager::add_translation(i18n_manager, "en", "error.connection.timeout", "Connection timeout occurred")
  I18nManager::add_translation(i18n_manager, "zh", "error.connection.timeout", "连接超时")
  I18nManager::add_translation(i18n_manager, "ja", "error.connection.timeout", "接続タイムアウト")
  I18nManager::add_translation(i18n_manager, "es", "error.connection.timeout", "Tiempo de conexión agotado")
  
  I18nManager::add_translation(i18n_manager, "en", "error.auth.failed", "Authentication failed")
  I18nManager::add_translation(i18n_manager, "zh", "error.auth.failed", "认证失败")
  I18nManager::add_translation(i18n_manager, "ja", "error.auth.failed", "認証失敗")
  I18nManager::add_translation(i18n_manager, "es", "error.auth.failed", "Autenticación fallida")
  
  // 设置当前语言环境
  I18nManager::set_locale(i18n_manager, "zh")
  
  // 创建本地化日志记录
  let log_record = Logger::create_log_record(logger)
  LogRecord::set_severity(log_record, Error)
  
  // 使用本地化的错误消息
  let localized_message = I18nManager::get_translation(i18n_manager, "error.connection.timeout")
  LogRecord::set_body(log_record, localized_message)
  LogRecord::set_attribute(log_record, "error.code", StringValue("CONN_TIMEOUT"))
  LogRecord::set_attribute(log_record, "locale", StringValue("zh"))
  
  Logger::emit(log_record)
  
  // 验证本地化消息
  assert_eq(localized_message, "连接超时")
  
  // 切换到英语环境
  I18nManager::set_locale(i18n_manager, "en")
  let english_message = I18nManager::get_translation(i18n_manager, "error.connection.timeout")
  assert_eq(english_message, "Connection timeout occurred")
  
  // 切换到日语环境
  I18nManager::set_locale(i18n_manager, "ja")
  let japanese_message = I18nManager::get_translation(i18n_manager, "error.auth.failed")
  assert_eq(japanese_message, "認証失敗")
}
