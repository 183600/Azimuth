// Azimuth Internationalization Support Tests
// 国际化支持测试用例 - 测试系统的国际化功能，包括多语言支持、本地化格式、字符编码处理等

// Test 1: 多语言错误消息支持
test "multilingual error message support" {
  let i18n_manager = I18nManager::new()
  
  // 加载不同语言的错误消息
  I18nManager::load_language_pack(i18n_manager, "en", "error_messages_en.json")
  I18nManager::load_language_pack(i18n_manager, "zh", "error_messages_zh.json")
  I18nManager::load_language_pack(i18n_manager, "ja", "error_messages_ja.json")
  I18nManager::load_language_pack(i18n_manager, "es", "error_messages_es.json")
  I18nManager::load_language_pack(i18n_manager, "fr", "error_messages_fr.json")
  
  // 测试英文错误消息
  I18nManager::set_current_language(i18n_manager, "en")
  let en_error = I18nManager::get_error_message(i18n_manager, "NETWORK_TIMEOUT")
  assert_eq(en_error, "Network connection timed out")
  
  let en_error_with_params = I18nManager::get_error_message_with_params(i18n_manager, "INVALID_PARAMETER", ["param_name", "user_id"])
  assert_eq(en_error_with_params, "Parameter 'param_name' is invalid. Expected: user_id")
  
  // 测试中文错误消息
  I18nManager::set_current_language(i18n_manager, "zh")
  let zh_error = I18nManager::get_error_message(i18n_manager, "NETWORK_TIMEOUT")
  assert_eq(zh_error, "网络连接超时")
  
  let zh_error_with_params = I18nManager::get_error_message_with_params(i18n_manager, "INVALID_PARAMETER", ["参数名", "用户ID"])
  assert_eq(zh_error_with_params, "参数'参数名'无效。期望：用户ID")
  
  // 测试日文错误消息
  I18nManager::set_current_language(i18n_manager, "ja")
  let ja_error = I18nManager::get_error_message(i18n_manager, "NETWORK_TIMEOUT")
  assert_eq(ja_error, "ネットワーク接続がタイムアウトしました")
  
  let ja_error_with_params = I18nManager::get_error_message_with_params(i18n_manager, "INVALID_PARAMETER", ["パラメータ名", "ユーザーID"])
  assert_eq(ja_error_with_params, "パラメータ'パラメータ名'が無効です。期待値：ユーザーID")
  
  // 测试西班牙文错误消息
  I18nManager::set_current_language(i18n_manager, "es")
  let es_error = I18nManager::get_error_message(i18n_manager, "NETWORK_TIMEOUT")
  assert_eq(es_error, "La conexión de red se agotó")
  
  let es_error_with_params = I18nManager::get_error_message_with_params(i18n_manager, "INVALID_PARAMETER", ["nombre_parámetro", "ID_usuario"])
  assert_eq(es_error_with_params, "El parámetro 'nombre_parámetro' es inválido. Esperado: ID_usuario")
  
  // 测试法文错误消息
  I18nManager::set_current_language(i18n_manager, "fr")
  let fr_error = I18nManager::get_error_message(i18n_manager, "NETWORK_TIMEOUT")
  assert_eq(fr_error, "La connexion réseau a expiré")
  
  let fr_error_with_params = I18nManager::get_error_message_with_params(i18n_manager, "INVALID_PARAMETER", ["nom_paramètre", "ID_utilisateur"])
  assert_eq(fr_error_with_params, "Le paramètre 'nom_paramètre' est invalide. Attendu : ID_utilisateur")
  
  // 测试回退到默认语言
  I18nManager::set_current_language(i18n_manager, "de") // 德文未加载
  let fallback_error = I18nManager::get_error_message(i18n_manager, "NETWORK_TIMEOUT")
  assert_eq(fallback_error, "Network connection timed out") // 回退到英文
  
  // 测试不存在的错误键
  let nonexistent_error = I18nManager::get_error_message(i18n_manager, "NONEXISTENT_ERROR")
  assert_eq(nonexistent_error, "NONEXISTENT_ERROR") // 返回键名作为回退
}

// Test 2: 日期和时间本地化
test "date and time localization" {
  let localizer = DateTimeLocalizer::new()
  
  // 测试时间戳
  let timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  // 美国英语格式
  let en_us_format = DateTimeLocalizer::format(localizer, timestamp, "en-US", "date_time")
  assert_eq(en_us_format, "1/1/2021, 12:00:00 AM")
  
  // 英国英语格式
  let en_gb_format = DateTimeLocalizer::format(localizer, timestamp, "en-GB", "date_time")
  assert_eq(en_gb_format, "01/01/2021, 00:00:00")
  
  // 中文格式
  let zh_format = DateTimeLocalizer::format(localizer, timestamp, "zh-CN", "date_time")
  assert_eq(zh_format, "2021/1/1 00:00:00")
  
  // 日文格式
  let ja_format = DateTimeLocalizer::format(localizer, timestamp, "ja-JP", "date_time")
  assert_eq(ja_format, "2021/01/01 0:00:00")
  
  // 德文格式
  let de_format = DateTimeLocalizer::format(localizer, timestamp, "de-DE", "date_time")
  assert_eq(de_format, "1.1.2021, 00:00:00")
  
  // 法文格式
  let fr_format = DateTimeLocalizer::format(localizer, timestamp, "fr-FR", "date_time")
  assert_eq(fr_format, "01/01/2021 00:00:00")
  
  // 西班牙文格式
  let es_format = DateTimeLocalizer::format(localizer, timestamp, "es-ES", "date_time")
  assert_eq(es_format, "1/1/2021 0:00:00")
  
  // 测试仅日期格式
  let en_us_date = DateTimeLocalizer::format(localizer, timestamp, "en-US", "date")
  assert_eq(en_us_date, "1/1/2021")
  
  let zh_date = DateTimeLocalizer::format(localizer, timestamp, "zh-CN", "date")
  assert_eq(zh_date, "2021/1/1")
  
  // 测试仅时间格式
  let en_us_time = DateTimeLocalizer::format(localizer, timestamp, "en-US", "time")
  assert_eq(en_us_time, "12:00:00 AM")
  
  let zh_time = DateTimeLocalizer::format(localizer, timestamp, "zh-CN", "time")
  assert_eq(zh_time, "00:00:00")
  
  // 测试相对时间格式
  let now = Time::now()
  let one_hour_ago = now - 3600000L // 1小时前
  let one_day_ago = now - 86400000L  // 1天前
  
  let en_us_relative_1h = DateTimeLocalizer::format_relative(localizer, one_hour_ago, "en-US")
  assert_eq(en_us_relative_1h, "1 hour ago")
  
  let zh_relative_1h = DateTimeLocalizer::format_relative(localizer, one_hour_ago, "zh-CN")
  assert_eq(zh_relative_1h, "1小时前")
  
  let en_us_relative_1d = DateTimeLocalizer::format_relative(localizer, one_day_ago, "en-US")
  assert_eq(en_us_relative_1d, "1 day ago")
  
  let zh_relative_1d = DateTimeLocalizer::format_relative(localizer, one_day_ago, "zh-CN")
  assert_eq(zh_relative_1d, "1天前")
}

// Test 3: 数字和货币本地化
test "number and currency localization" {
  let localizer = NumberLocalizer::new()
  
  // 测试数字格式化
  let number = 1234567.89
  
  // 美国英语数字格式
  let en_us_number = NumberLocalizer::format_number(localizer, number, "en-US")
  assert_eq(en_us_number, "1,234,567.89")
  
  // 德国数字格式
  let de_number = NumberLocalizer::format_number(localizer, number, "de-DE")
  assert_eq(de_number, "1.234.567,89")
  
  // 法国数字格式
  let fr_number = NumberLocalizer::format_number(localizer, number, "fr-FR")
  assert_eq(fr_number, "1 234 567,89")
  
  // 中文数字格式
  let zh_number = NumberLocalizer::format_number(localizer, number, "zh-CN")
  assert_eq(zh_number, "1,234,567.89")
  
  // 印度数字格式
  let hi_number = NumberLocalizer::format_number(localizer, number, "hi-IN")
  assert_eq(hi_number, "12,34,567.89")
  
  // 测试货币格式化
  let currency = 1234.56
  
  // 美元格式
  let usd_currency = NumberLocalizer::format_currency(localizer, currency, "en-US", "USD")
  assert_eq(usd_currency, "$1,234.56")
  
  // 欧元格式（德国）
  let eur_currency_de = NumberLocalizer::format_currency(localizer, currency, "de-DE", "EUR")
  assert_eq(eur_currency_de, "1.234,56 €")
  
  // 欧元格式（法国）
  let eur_currency_fr = NumberLocalizer::format_currency(localizer, currency, "fr-FR", "EUR")
  assert_eq(eur_currency_fr, "1 234,56 €")
  
  // 人民币格式
  let cny_currency = NumberLocalizer::format_currency(localizer, currency, "zh-CN", "CNY")
  assert_eq(cny_currency, "¥1,234.56")
  
  // 日元格式
  let jpy_currency = NumberLocalizer::format_currency(localizer, currency, "ja-JP", "JPY")
  assert_eq(jpy_currency, "￥1,235") // 日元通常不显示小数
  
  // 英镑格式
  let gbp_currency = NumberLocalizer::format_currency(localizer, currency, "en-GB", "GBP")
  assert_eq(gbp_currency, "£1,234.56")
  
  // 测试百分比格式化
  let percentage = 0.7542
  
  let en_us_percentage = NumberLocalizer::format_percentage(localizer, percentage, "en-US")
  assert_eq(en_us_percentage, "75%")
  
  let zh_percentage = NumberLocalizer::format_percentage(localizer, percentage, "zh-CN")
  assert_eq(zh_percentage, "75%")
  
  // 测试科学计数法
  let scientific_number = 1234567.89
  
  let en_us_scientific = NumberLocalizer::format_scientific(localizer, scientific_number, "en-US")
  assert_eq(en_us_scientific, "1.23E6")
  
  let de_scientific = NumberLocalizer::format_scientific(localizer, scientific_number, "de-DE")
  assert_eq(de_scientific, "1,23E6")
}

// Test 4: 多语言日志消息支持
test "multilingual log message support" {
  let i18n_logger = I18nLogger::new()
  
  // 加载不同语言的日志消息模板
  I18nLogger::load_message_templates(i18n_logger, "en", "log_templates_en.json")
  I18nLogger::load_message_templates(i18n_logger, "zh", "log_templates_zh.json")
  I18nLogger::load_message_templates(i18n_logger, "ja", "log_templates_ja.json")
  
  // 测试英文日志消息
  I18nLogger::set_language(i18n_logger, "en")
  let en_log = I18nLogger::format_message(i18n_logger, "USER_LOGIN", ["john_doe", "192.168.1.100"])
  assert_eq(en_log, "User 'john_doe' logged in from IP 192.168.1.100")
  
  let en_error_log = I18nLogger::format_message(i18n_logger, "DATABASE_ERROR", ["connection_timeout", "30"])
  assert_eq(en_error_log, "Database error occurred: connection_timeout after 30 seconds")
  
  // 测试中文日志消息
  I18nLogger::set_language(i18n_logger, "zh")
  let zh_log = I18nLogger::format_message(i18n_logger, "USER_LOGIN", ["张三", "192.168.1.100"])
  assert_eq(zh_log, "用户'张三'从IP 192.168.1.100登录")
  
  let zh_error_log = I18nLogger::format_message(i18n_logger, "DATABASE_ERROR", ["连接超时", "30"])
  assert_eq(zh_error_log, "数据库错误发生：连接超时，30秒后")
  
  // 测试日文日志消息
  I18nLogger::set_language(i18n_logger, "ja")
  let ja_log = I18nLogger::format_message(i18n_logger, "USER_LOGIN", ["田中太郎", "192.168.1.100"])
  assert_eq(ja_log, "ユーザー'田中太郎'がIP 192.168.1.100からログインしました")
  
  let ja_error_log = I18nLogger::format_message(i18n_logger, "DATABASE_ERROR", ["接続タイムアウト", "30"])
  assert_eq(ja_error_log, "データベースエラーが発生しました：接続タイムアウト、30秒後")
  
  // 测试多语言日志记录
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "i18n_test_logger")
  
  I18nLogger::set_language(i18n_logger, "en")
  let en_log_record = I18nLogger::create_log_record(i18n_logger, Info, "USER_LOGIN", ["alice", "10.0.0.1"])
  Logger::emit(logger, en_log_record)
  
  I18nLogger::set_language(i18n_logger, "zh")
  let zh_log_record = I18nLogger::create_log_record(i18n_logger, Info, "USER_LOGIN", ["李四", "10.0.0.2"])
  Logger::emit(logger, zh_log_record)
  
  // 验证多语言日志记录
  let log_records = Logger::get_emitted_logs(logger)
  assert_eq(log_records.length(), 2)
  
  match LogRecord::body(log_records[0]) {
    Some(body) => assert_eq(body, "User 'alice' logged in from IP 10.0.0.1")
    None => assert_true(false)
  }
  
  match LogRecord::body(log_records[1]) {
    Some(body) => assert_eq(body, "用户'李四'从IP 10.0.0.2登录")
    None => assert_true(false)
  }
}

// Test 5: 多语言UI元素支持
test "multilingual UI elements support" {
  let ui_localizer = UILocalizer::new()
  
  // 加载不同语言的UI文本
  UILocalizer::load_language_pack(ui_localizer, "en", "ui_texts_en.json")
  UILocalizer::load_language_pack(ui_localizer, "zh", "ui_texts_zh.json")
  UILocalizer::load_language_pack(ui_localizer, "ja", "ui_texts_ja.json")
  UILocalizer::load_language_pack(ui_localizer, "es", "ui_texts_es.json")
  UILocalizer::load_language_pack(ui_localizer, "fr", "ui_texts_fr.json")
  
  // 测试英文UI元素
  UILocalizer::set_language(ui_localizer, "en")
  let en_button = UILocalizer::get_text(ui_localizer, "button_submit")
  assert_eq(en_button, "Submit")
  
  let en_title = UILocalizer::get_text(ui_localizer, "title_dashboard")
  assert_eq(en_title, "Dashboard")
  
  let en_menu = UILocalizer::get_text(ui_localizer, "menu_settings")
  assert_eq(en_menu, "Settings")
  
  // 测试中文UI元素
  UILocalizer::set_language(ui_localizer, "zh")
  let zh_button = UILocalizer::get_text(ui_localizer, "button_submit")
  assert_eq(zh_button, "提交")
  
  let zh_title = UILocalizer::get_text(ui_localizer, "title_dashboard")
  assert_eq(zh_title, "仪表板")
  
  let zh_menu = UILocalizer::get_text(ui_localizer, "menu_settings")
  assert_eq(zh_menu, "设置")
  
  // 测试日文UI元素
  UILocalizer::set_language(ui_localizer, "ja")
  let ja_button = UILocalizer::get_text(ui_localizer, "button_submit")
  assert_eq(ja_button, "送信")
  
  let ja_title = UILocalizer::get_text(ui_localizer, "title_dashboard")
  assert_eq(ja_title, "ダッシュボード")
  
  let ja_menu = UILocalizer::get_text(ui_localizer, "menu_settings")
  assert_eq(ja_menu, "設定")
  
  // 测试西班牙文UI元素
  UILocalizer::set_language(ui_localizer, "es")
  let es_button = UILocalizer::get_text(ui_localizer, "button_submit")
  assert_eq(es_button, "Enviar")
  
  let es_title = UILocalizer::get_text(ui_localizer, "title_dashboard")
  assert_eq(es_title, "Panel de control")
  
  let es_menu = UILocalizer::get_text(ui_localizer, "menu_settings")
  assert_eq(es_menu, "Configuración")
  
  // 测试法文UI元素
  UILocalizer::set_language(ui_localizer, "fr")
  let fr_button = UILocalizer::get_text(ui_localizer, "button_submit")
  assert_eq(fr_button, "Soumettre")
  
  let fr_title = UILocalizer::get_text(ui_localizer, "title_dashboard")
  assert_eq(fr_title, "Tableau de bord")
  
  let fr_menu = UILocalizer::get_text(ui_localizer, "menu_settings")
  assert_eq(fr_menu, "Paramètres")
  
  // 测试复数形式
  UILocalizer::set_language(ui_localizer, "en")
  let en_single = UILocalizer::get_plural_text(ui_localizer, "item_count", 1)
  assert_eq(en_single, "1 item")
  
  let en_plural = UILocalizer::get_plural_text(ui_localizer, "item_count", 5)
  assert_eq(en_plural, "5 items")
  
  UILocalizer::set_language(ui_localizer, "zh")
  let zh_single = UILocalizer::get_plural_text(ui_localizer, "item_count", 1)
  assert_eq(zh_single, "1个项目")
  
  let zh_plural = UILocalizer::get_plural_text(ui_localizer, "item_count", 5)
  assert_eq(zh_plural, "5个项目")
}

// Test 6: 字符编码处理
test "character encoding handling" {
  let encoding_handler = EncodingHandler::new()
  
  // 测试UTF-8编码
  let utf8_text = "Hello 世界 こんにちは 안녕하세요"
  let utf8_encoded = EncodingHandler::encode_utf8(encoding_handler, utf8_text)
  let utf8_decoded = EncodingHandler::decode_utf8(encoding_handler, utf8_encoded)
  assert_eq(utf8_decoded, utf8_text)
  
  // 测试UTF-16编码
  let utf16_encoded = EncodingHandler::encode_utf16(encoding_handler, utf8_text)
  let utf16_decoded = EncodingHandler::decode_utf16(encoding_handler, utf16_encoded)
  assert_eq(utf16_decoded, utf8_text)
  
  // 测试ASCII编码（仅ASCII字符）
  let ascii_text = "Hello World"
  let ascii_encoded = EncodingHandler::encode_ascii(encoding_handler, ascii_text)
  let ascii_decoded = EncodingHandler::decode_ascii(encoding_handler, ascii_encoded)
  assert_eq(ascii_decoded, ascii_text)
  
  // 测试ISO-8859-1编码
  let latin1_text = "Café résumé"
  let latin1_encoded = EncodingHandler::encode_latin1(encoding_handler, latin1_text)
  let latin1_decoded = EncodingHandler::decode_latin1(encoding_handler, latin1_encoded)
  assert_eq(latin1_decoded, latin1_text)
  
  // 测试编码检测
  let mixed_text = "Hello 世界"
  let detected_encoding = EncodingHandler::detect_encoding(encoding_handler, mixed_text)
  assert_eq(detected_encoding, "UTF-8")
  
  // 测试编码转换
  let utf8_to_utf16 = EncodingHandler::convert_encoding(encoding_handler, utf8_text, "UTF-8", "UTF-16")
  let utf16_back_to_utf8 = EncodingHandler::convert_encoding(encoding_handler, utf8_to_utf16, "UTF-16", "UTF-8")
  assert_eq(utf16_back_to_utf8, utf8_text)
  
  // 测试无效编码处理
  let invalid_utf8 = [0xFF, 0xFE, 0xFD] // 无效的UTF-8序列
  let handled_invalid = EncodingHandler::handle_invalid_encoding(encoding_handler, invalid_utf8, "UTF-8")
  assert_true(handled_invalid.contains("�")) // 应该包含替换字符
  
  // 测试BOM处理
  let utf8_with_bom = [0xEF, 0xBB, 0xBF] + utf8_text.to_bytes()
  let bom_removed = EncodingHandler::remove_bom(encoding_handler, utf8_with_bom, "UTF-8")
  let bom_removed_text = String::from_bytes(bom_removed)
  assert_eq(bom_removed_text, utf8_text)
  
  // 测试BOM添加
  let utf8_with_bom_added = EncodingHandler::add_bom(encoding_handler, utf8_text, "UTF-8")
  assert_true(utf8_with_bom_added.length() > utf8_text.length())
  assert_true(utf8_with_bom_added.starts_with([0xEF, 0xBB, 0xBF]))
}

// Test 7: 时区和夏令时处理
test "timezone and daylight saving time handling" {
  let timezone_handler = TimezoneHandler::new()
  
  // 测试时区转换
  let utc_timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  // 转换为美国东部时间（EST）
  let est_time = TimezoneHandler::convert_from_utc(timezone_handler, utc_timestamp, "America/New_York")
  assert_eq(est_time, "2020-12-31 19:00:00 EST") // UTC-5
  
  // 转换为美国太平洋时间（PST）
  let pst_time = TimezoneHandler::convert_from_utc(timezone_handler, utc_timestamp, "America/Los_Angeles")
  assert_eq(pst_time, "2020-12-31 16:00:00 PST") // UTC-8
  
  // 转换为中国时间（CST）
  let cst_time = TimezoneHandler::convert_from_utc(timezone_handler, utc_timestamp, "Asia/Shanghai")
  assert_eq(cst_time, "2021-01-01 08:00:00 CST") // UTC+8
  
  // 转换为日本时间（JST）
  let jst_time = TimezoneHandler::convert_from_utc(timezone_handler, utc_timestamp, "Asia/Tokyo")
  assert_eq(jst_time, "2021-01-01 09:00:00 JST") // UTC+9
  
  // 转换为德国时间（CET）
  let cet_time = TimezoneHandler::convert_from_utc(timezone_handler, utc_timestamp, "Europe/Berlin")
  assert_eq(cet_time, "2021-01-01 01:00:00 CET") // UTC+1
  
  // 测试夏令时转换
  let dst_timestamp = 1622471100000L // 2021-05-31 23:45:00 UTC（美国夏令时期间）
  
  // 美国东部夏令时（EDT）
  let edt_time = TimezoneHandler::convert_from_utc(timezone_handler, dst_timestamp, "America/New_York")
  assert_eq(edt_time, "2021-05-31 19:45:00 EDT") // UTC-4（夏令时）
  
  // 测试夏令时转换边界
  let dst_start_timestamp = 1617228000000L // 2021-03-31 06:00:00 UTC（美国夏令时开始）
  let dst_start_time = TimezoneHandler::convert_from_utc(timezone_handler, dst_start_timestamp, "America/New_York")
  assert_eq(dst_start_time, "2021-03-31 02:00:00 EDT") // 夏令时开始
  
  let dst_end_timestamp = 1635675600000L // 2021-10-31 06:00:00 UTC（美国夏令时结束）
  let dst_end_time = TimezoneHandler::convert_from_utc(timezone_handler, dst_end_timestamp, "America/New_York")
  assert_eq(dst_end_time, "2021-10-31 01:00:00 EST") // 夏令时结束
  
  // 测试本地时间转换为UTC
  let local_time = "2021-01-01 12:00:00"
  let utc_from_local = TimezoneHandler::convert_to_utc(timezone_handler, local_time, "Asia/Shanghai", "YYYY-MM-DD HH:mm:ss")
  assert_eq(utc_from_local, "2021-01-01 04:00:00 UTC") // 东八区减8小时
  
  // 测试时区信息获取
  let timezone_info = TimezoneHandler::get_timezone_info(timezone_handler, "America/New_York")
  assert_eq(timezone_info.name, "America/New_York")
  assert_eq(timezone_info.standard_offset, "-05:00")
  assert_eq(timezone_info.daylight_offset, "-04:00")
  assert_true(timezone_info.uses_dst)
}

// Test 8: 文化特定格式处理
test "culture-specific formatting" {
  let culture_formatter = CultureFormatter::new()
  
  // 测试姓名格式
  let first_name = "John"
  let last_name = "Doe"
  
  // 西方格式：名 姓
  let western_name = CultureFormatter::format_name(culture_formatter, first_name, last_name, "en-US")
  assert_eq(western_name, "John Doe")
  
  // 东方格式：姓 名
  let eastern_name = CultureFormatter::format_name(culture_formatter, first_name, last_name, "zh-CN")
  assert_eq(eastern_name, "Doe John")
  
  // 匈牙利格式：姓 名
  let hungarian_name = CultureFormatter::format_name(culture_formatter, first_name, last_name, "hu-HU")
  assert_eq(hungarian_name, "Doe John")
  
  // 测试地址格式
  let address_components = {
    "street": "123 Main St",
    "city": "New York",
    "state": "NY",
    "postal_code": "10001",
    "country": "USA"
  }
  
  // 美国地址格式
  let us_address = CultureFormatter::format_address(culture_formatter, address_components, "en-US")
  assert_eq(us_address, "123 Main St\nNew York, NY 10001\nUSA")
  
  // 日本地址格式
  let jp_address_components = {
    "postal_code": "100-0001",
    "prefecture": "東京都",
    "city": "千代田区",
    "street": "千代田1-1",
    "country": "日本"
  }
  
  let jp_address = CultureFormatter::format_address(culture_formatter, jp_address_components, "ja-JP")
  assert_eq(jp_address, "〒100-0001\n東京都千代田区千代田1-1\n日本")
  
  // 测试电话号码格式
  let phone_number = "+12025551234"
  
  // 美国格式
  let us_phone = CultureFormatter::format_phone_number(culture_formatter, phone_number, "en-US")
  assert_eq(us_phone, "(202) 555-1234")
  
  // 德国格式
  let de_phone = CultureFormatter::format_phone_number(culture_formatter, phone_number, "de-DE")
  assert_eq(de_phone, "+1 202-555-1234")
  
  // 法国格式
  let fr_phone = CultureFormatter::format_phone_number(culture_formatter, phone_number, "fr-FR")
  assert_eq(fr_phone, "+1 202 555 12 34")
  
  // 测试度量衡单位
  let distance = 1000.0 // 米
  
  // 公制单位（大多数国家）
  let metric_distance = CultureFormatter::format_distance(culture_formatter, distance, "zh-CN")
  assert_eq(metric_distance, "1,000 米")
  
  // 英制单位（美国）
  let imperial_distance = CultureFormatter::format_distance(culture_formatter, distance, "en-US")
  assert_eq(imperial_distance, "0.62 英里")
  
  // 测试纸张尺寸
  let a4_size = CultureFormatter::format_paper_size(culture_formatter, "A4", "en-US")
  assert_eq(a4_size, "8.27 × 11.69 inches")
  
  let a4_size_metric = CultureFormatter::format_paper_size(culture_formatter, "A4", "de-DE")
  assert_eq(a4_size_metric, "21.0 × 29.7 cm")
  
  // 测试列表排序
  let names = ["张三", "李四", "王五", "赵六"]
  
  // 按拼音排序（中文）
  let zh_sorted = CultureFormatter::sort_strings(culture_formatter, names, "zh-CN")
  assert_eq(zh_sorted, ["李四", "王五", "张三", "赵六"])
  
  // 按字母排序（英文）
  let en_names = ["Zoe", "Alice", "Bob", "Charlie"]
  let en_sorted = CultureFormatter::sort_strings(culture_formatter, en_names, "en-US")
  assert_eq(en_sorted, ["Alice", "Bob", "Charlie", "Zoe"])
}