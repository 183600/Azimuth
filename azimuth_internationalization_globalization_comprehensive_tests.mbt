// Azimuth Internationalization and Globalization Comprehensive Test Suite
// 测试国际化和全球化功能，确保系统在不同语言和地区环境下的正确运行

test "多语言消息本地化测试" {
  // 创建本地化管理器
  let localization_manager = LocalizationManager::new()
  
  // 添加不同语言的消息
  LocalizationManager::add_message(localization_manager, "en", "operation.success", "Operation completed successfully")
  LocalizationManager::add_message(localization_manager, "zh", "operation.success", "操作成功完成")
  LocalizationManager::add_message(localization_manager, "es", "operation.success", "Operación completada con éxito")
  LocalizationManager::add_message(localization_manager, "fr", "operation.success", "Opération terminée avec succès")
  LocalizationManager::add_message(localization_manager, "de", "operation.success", "Operation erfolgreich abgeschlossen")
  LocalizationManager::add_message(localization_manager, "ja", "operation.success", "操作が正常に完了しました")
  
  LocalizationManager::add_message(localization_manager, "en", "error.connection.failed", "Connection failed")
  LocalizationManager::add_message(localization_manager, "zh", "error.connection.failed", "连接失败")
  LocalizationManager::add_message(localization_manager, "es", "error.connection.failed", "Conexión fallida")
  LocalizationManager::add_message(localization_manager, "fr", "error.connection.failed", "Échec de connexion")
  LocalizationManager::add_message(localization_manager, "de", "error.connection.failed", "Verbindung fehlgeschlagen")
  LocalizationManager::add_message(localization_manager, "ja", "error.connection.failed", "接続に失敗しました")
  
  // 测试英语本地化
  let en_success = LocalizationManager::get_message(localization_manager, "en", "operation.success")
  assert_eq(en_success, "Operation completed successfully")
  
  let en_error = LocalizationManager::get_message(localization_manager, "en", "error.connection.failed")
  assert_eq(en_error, "Connection failed")
  
  // 测试中文本地化
  let zh_success = LocalizationManager::get_message(localization_manager, "zh", "operation.success")
  assert_eq(zh_success, "操作成功完成")
  
  let zh_error = LocalizationManager::get_message(localization_manager, "zh", "error.connection.failed")
  assert_eq(zh_error, "连接失败")
  
  // 测试西班牙语本地化
  let es_success = LocalizationManager::get_message(localization_manager, "es", "operation.success")
  assert_eq(es_success, "Operación completada con éxito")
  
  let es_error = LocalizationManager::get_message(localization_manager, "es", "error.connection.failed")
  assert_eq(es_error, "Conexión fallida")
  
  // 测试法语本地化
  let fr_success = LocalizationManager::get_message(localization_manager, "fr", "operation.success")
  assert_eq(fr_success, "Opération terminée avec succès")
  
  let fr_error = LocalizationManager::get_message(localization_manager, "fr", "error.connection.failed")
  assert_eq(fr_error, "Échec de connexion")
  
  // 测试德语本地化
  let de_success = LocalizationManager::get_message(localization_manager, "de", "operation.success")
  assert_eq(de_success, "Operation erfolgreich abgeschlossen")
  
  let de_error = LocalizationManager::get_message(localization_manager, "de", "error.connection.failed")
  assert_eq(de_error, "Verbindung fehlgeschlagen")
  
  // 测试日语本地化
  let ja_success = LocalizationManager::get_message(localization_manager, "ja", "operation.success")
  assert_eq(ja_success, "操作が正常に完了しました")
  
  let ja_error = LocalizationManager::get_message(localization_manager, "ja", "error.connection.failed")
  assert_eq(ja_error, "接続に失敗しました")
}

test "日期和时间格式本地化测试" {
  // 创建日期时间格式化器
  let date_formatter = DateFormatter::new()
  
  // 创建测试日期时间
  let test_datetime = DateTime::new(2023, 11, 15, 14, 30, 45)
  
  // 测试英语日期格式
  let en_date = DateFormatter::format(date_formatter, test_datetime, "en", "date")
  assert_eq(en_date, "11/15/2023")
  
  let en_time = DateFormatter::format(date_formatter, test_datetime, "en", "time")
  assert_eq(en_time, "2:30:45 PM")
  
  let en_datetime = DateFormatter::format(date_formatter, test_datetime, "en", "datetime")
  assert_eq(en_datetime, "11/15/2023 2:30:45 PM")
  
  // 测试中文日期格式
  let zh_date = DateFormatter::format(date_formatter, test_datetime, "zh", "date")
  assert_eq(zh_date, "2023年11月15日")
  
  let zh_time = DateFormatter::format(date_formatter, test_datetime, "zh", "time")
  assert_eq(zh_time, "下午2:30:45")
  
  let zh_datetime = DateFormatter::format(date_formatter, test_datetime, "zh", "datetime")
  assert_eq(zh_datetime, "2023年11月15日 下午2:30:45")
  
  // 测试德语日期格式
  let de_date = DateFormatter::format(date_formatter, test_datetime, "de", "date")
  assert_eq(de_date, "15.11.2023")
  
  let de_time = DateFormatter::format(date_formatter, test_datetime, "de", "time")
  assert_eq(de_time, "14:30:45")
  
  let de_datetime = DateFormatter::format(date_formatter, test_datetime, "de", "datetime")
  assert_eq(de_datetime, "15.11.2023 14:30:45")
  
  // 测试日语日期格式
  let ja_date = DateFormatter::format(date_formatter, test_datetime, "ja", "date")
  assert_eq(ja_date, "2023/11/15")
  
  let ja_time = DateFormatter::format(date_formatter, test_datetime, "ja", "time")
  assert_eq(ja_time, "14:30:45")
  
  let ja_datetime = DateFormatter::format(date_formatter, test_datetime, "ja", "datetime")
  assert_eq(ja_datetime, "2023/11/15 14:30:45")
}

test "数字格式本地化测试" {
  // 创建数字格式化器
  let number_formatter = NumberFormatter::new()
  
  // 测试整数格式化
  let int_value = 1234567
  
  let en_int = NumberFormatter::format_integer(number_formatter, int_value, "en")
  assert_eq(en_int, "1,234,567")
  
  let zh_int = NumberFormatter::format_integer(number_formatter, int_value, "zh")
  assert_eq(zh_int, "1,234,567")
  
  let de_int = NumberFormatter::format_integer(number_formatter, int_value, "de")
  assert_eq(de_int, "1.234.567")
  
  let fr_int = NumberFormatter::format_integer(number_formatter, int_value, "fr")
  assert_eq(fr_int, "1 234 567")
  
  // 测试小数格式化
  let float_value = 1234567.89
  
  let en_float = NumberFormatter::format_float(number_formatter, float_value, "en", 2)
  assert_eq(en_float, "1,234,567.89")
  
  let zh_float = NumberFormatter::format_float(number_formatter, float_value, "zh", 2)
  assert_eq(zh_float, "1,234,567.89")
  
  let de_float = NumberFormatter::format_float(number_formatter, float_value, "de", 2)
  assert_eq(de_float, "1.234.567,89")
  
  let fr_float = NumberFormatter::format_float(number_formatter, float_value, "fr", 2)
  assert_eq(fr_float, "1 234 567,89")
  
  // 测试货币格式化
  let currency_value = 1234.56
  
  let en_currency = NumberFormatter::format_currency(number_formatter, currency_value, "en", "USD")
  assert_eq(en_currency, "$1,234.56")
  
  let zh_currency = NumberFormatter::format_currency(number_formatter, currency_value, "zh", "CNY")
  assert_eq(zh_currency, "¥1,234.56")
  
  let de_currency = NumberFormatter::format_currency(number_formatter, currency_value, "de", "EUR")
  assert_eq(de_currency, "1.234,56 €")
  
  let fr_currency = NumberFormatter::format_currency(number_formatter, currency_value, "fr", "EUR")
  assert_eq(fr_currency, "1 234,56 €")
  
  let ja_currency = NumberFormatter::format_currency(number_formatter, currency_value, "ja", "JPY")
  assert_eq(ja_currency, "¥1,235")
}

test "文本方向和书写系统测试" {
  // 创建文本方向处理器
  let text_direction_handler = TextDirectionHandler::new()
  
  // 测试从左到右的文本（英语、中文、日语等）
  let ltr_text = "Hello World"
  let ltr_direction = TextDirectionHandler::get_direction(text_direction_handler, ltr_text)
  assert_eq(ltr_direction, TextDirection::LTR)
  
  let zh_ltr_text = "你好世界"
  let zh_ltr_direction = TextDirectionHandler::get_direction(text_direction_handler, zh_ltr_text)
  assert_eq(zh_ltr_direction, TextDirection::LTR)
  
  let ja_ltr_text = "こんにちは世界"
  let ja_ltr_direction = TextDirectionHandler::get_direction(text_direction_handler, ja_ltr_text)
  assert_eq(ja_ltr_direction, TextDirection::LTR)
  
  // 测试从右到左的文本（阿拉伯语、希伯来语等）
  let rtl_text = "مرحبا بالعالم"
  let rtl_direction = TextDirectionHandler::get_direction(text_direction_handler, rtl_text)
  assert_eq(rtl_direction, TextDirection::RTL)
  
  let he_text = "שלום עולם"
  let he_direction = TextDirectionHandler::get_direction(text_direction_handler, he_text)
  assert_eq(he_direction, TextDirection::RTL)
  
  // 测试混合方向文本
  let mixed_text = "Hello مرحبا World"
  let mixed_direction = TextDirectionHandler::get_direction(text_direction_handler, mixed_text)
  assert_eq(mixed_direction, TextDirection::Mixed)
  
  // 测试文本对齐
  let ltr_aligned = TextDirectionHandler::align_text(text_direction_handler, ltr_text, TextDirection::LTR)
  assert_eq(ltr_aligned, "Hello World")
  
  let rtl_aligned = TextDirectionHandler::align_text(text_direction_handler, rtl_text, TextDirection::RTL)
  assert_eq(rtl_aligned, "مرحبا بالعالم")
}

test "时区处理测试" {
  // 创建时区处理器
  let timezone_handler = TimezoneHandler::new()
  
  // 创建UTC时间
  let utc_time = DateTime::new(2023, 11, 15, 14, 30, 45)
  
  // 测试不同时区转换
  let pst_time = TimezoneHandler::convert(timezone_handler, utc_time, "America/Los_Angeles")
  assert_eq(pst_time.hour, 6) // UTC-8 (标准时间)
  assert_eq(pst_time.minute, 30)
  
  let est_time = TimezoneHandler::convert(timezone_handler, utc_time, "America/New_York")
  assert_eq(est_time.hour, 9) // UTC-5 (标准时间)
  assert_eq(est_time.minute, 30)
  
  let cet_time = TimezoneHandler::convert(timezone_handler, utc_time, "Europe/Paris")
  assert_eq(cet_time.hour, 15) // UTC+1 (标准时间)
  assert_eq(cet_time.minute, 30)
  
  let jst_time = TimezoneHandler::convert(timezone_handler, utc_time, "Asia/Tokyo")
  assert_eq(jst_time.hour, 23) // UTC+9
  assert_eq(jst_time.minute, 30)
  
  let cst_time = TimezoneHandler::convert(timezone_handler, utc_time, "Asia/Shanghai")
  assert_eq(cst_time.hour, 22) // UTC+8
  assert_eq(cst_time.minute, 30)
  
  // 测试时区名称本地化
  let pst_name_en = TimezoneHandler::get_timezone_name(timezone_handler, "America/Los_Angeles", "en")
  assert_eq(pst_name_en, "Pacific Standard Time")
  
  let pst_name_zh = TimezoneHandler::get_timezone_name(timezone_handler, "America/Los_Angeles", "zh")
  assert_eq(pst_name_zh, "太平洋标准时间")
  
  let cet_name_en = TimezoneHandler::get_timezone_name(timezone_handler, "Europe/Paris", "en")
  assert_eq(cet_name_en, "Central European Time")
  
  let cet_name_zh = TimezoneHandler::get_timezone_name(timezone_handler, "Europe/Paris", "zh")
  assert_eq(cet_name_zh, "中欧标准时间")
}

test "字符编码和Unicode处理测试" {
  // 创建字符编码处理器
  let encoding_handler = EncodingHandler::new()
  
  // 测试多语言文本
  let multilingual_text = "English: Hello, 中文: 你好, 日本語: こんにちは, العربية: مرحبا, Русский: Привет"
  
  // 测试UTF-8编码
  let utf8_encoded = EncodingHandler::encode_utf8(encoding_handler, multilingual_text)
  let utf8_decoded = EncodingHandler::decode_utf8(encoding_handler, utf8_encoded)
  assert_eq(utf8_decoded, multilingual_text)
  
  // 测试字符计数
  let char_count = EncodingHandler::count_characters(encoding_handler, multilingual_text)
  assert_eq(char_count, 47) // 包括空格和标点符号
  
  // 测试字节计数
  let byte_count = EncodingHandler::count_bytes(encoding_handler, multilingual_text)
  assert_true(byte_count > 47) // 多字节字符会增加字节数
  
  // 测试字符截断
  let truncated = EncodingHandler::truncate_characters(encoding_handler, multilingual_text, 20)
  assert_eq(truncated.length(), 20)
  
  // 测试字节截断
  let byte_truncated = EncodingHandler::truncate_bytes(encoding_handler, multilingual_text, 20)
  assert_true(byte_truncated.to_bytes().length() <= 20)
  
  // 测试大小写转换
  let english_text = "Hello World"
  let upper_case = EncodingHandler::to_upper_case(encoding_handler, english_text, "en")
  assert_eq(upper_case, "HELLO WORLD")
  
  let lower_case = EncodingHandler::to_lower_case(encoding_handler, english_text, "en")
  assert_eq(lower_case, "hello world")
  
  // 测试中文大小写转换（中文没有大小写概念，应返回原文本）
  let chinese_text = "你好世界"
  let chinese_upper = EncodingHandler::to_upper_case(encoding_handler, chinese_text, "zh")
  assert_eq(chinese_upper, "你好世界")
  
  let chinese_lower = EncodingHandler::to_lower_case(encoding_handler, chinese_text, "zh")
  assert_eq(chinese_lower, "你好世界")
}

test "文化特定格式测试" {
  // 创建文化特定格式化器
  let cultural_formatter = CulturalFormatter::new()
  
  // 测试姓名格式
  let western_name = CulturalFormatter::format_name(cultural_formatter, "John", "Doe", "en")
  assert_eq(western_name, "John Doe")
  
  let eastern_name = CulturalFormatter::format_name(cultural_formatter, "张", "三", "zh")
  assert_eq(eastern_name, "张三")
  
  let japanese_name = CulturalFormatter::format_name(cultural_formatter, "田中", "太郎", "ja")
  assert_eq(japanese_name, "田中太郎")
  
  // 测试地址格式
  let us_address = CulturalFormatter::format_address(cultural_formatter, "123 Main St", "New York", "NY", "10001", "en")
  assert_eq(us_address, "123 Main St\nNew York, NY 10001")
  
  let chinese_address = CulturalFormatter::format_address(cultural_formatter, "建国路123号", "北京市", "朝阳区", "100000", "zh")
  assert_eq(chinese_address, "中国北京市朝阳区建国路123号\n100000")
  
  let japanese_address = CulturalFormatter::format_address(cultural_formatter, "丸の内1-2-3", "東京都", "千代田区", "100-0001", "ja")
  assert_eq(japanese_address, "〒100-0001\n東京都千代田区丸の内1-2-3")
  
  // 测试电话号码格式
  let us_phone = CulturalFormatter::format_phone(cultural_formatter, "1234567890", "en")
  assert_eq(us_phone, "(123) 456-7890")
  
  let chinese_phone = CulturalFormatter::format_phone(cultural_formatter, "13812345678", "zh")
  assert_eq(chinese_phone, "138-1234-5678")
  
  let japanese_phone = CulturalFormatter::format_phone(cultural_formatter, "09012345678", "ja")
  assert_eq(japanese_phone, "090-1234-5678")
  
  // 测试度量单位格式
  let metric_height = CulturalFormatter::format_height(cultural_formatter, 175, "metric", "en")
  assert_eq(metric_height, "175 cm")
  
  let imperial_height = CulturalFormatter::format_height(cultural_formatter, 175, "imperial", "en")
  assert_eq(imperial_height, "5'9\"")
  
  let metric_weight = CulturalFormatter::format_weight(cultural_formatter, 70, "metric", "en")
  assert_eq(metric_weight, "70 kg")
  
  let imperial_weight = CulturalFormatter::format_weight(cultural_formatter, 70, "imperial", "en")
  assert_eq(imperial_weight, "154 lbs")
}

test "语言回退机制测试" {
  // 创建语言回退处理器
  let fallback_handler = FallbackHandler::new()
  
  // 设置语言回退链
  FallbackHandler::set_fallback_chain(fallback_handler, "zh-CN", ["zh", "en"])
  FallbackHandler::set_fallback_chain(fallback_handler, "ja-JP", ["ja", "en"])
  FallbackHandler::set_fallback_chain(fallback_handler, "de-DE", ["de", "en"])
  
  // 添加消息
  FallbackHandler::add_message(fallback_handler, "en", "welcome.message", "Welcome!")
  FallbackHandler::add_message(fallback_handler, "zh", "welcome.message", "欢迎！")
  FallbackHandler::add_message(fallback_handler, "ja", "welcome.message", "ようこそ！")
  // 没有添加德语消息
  
  // 测试中文回退
  let zh_cn_message = FallbackHandler::get_message(fallback_handler, "zh-CN", "welcome.message")
  assert_eq(zh_cn_message, "欢迎！") // 直接找到zh-CN -> zh -> zh
  
  // 测试日语回退
  let ja_jp_message = FallbackHandler::get_message(fallback_handler, "ja-JP", "welcome.message")
  assert_eq(ja_jp_message, "ようこそ！") // 直接找到ja-JP -> ja -> ja
  
  // 测试德语回退
  let de_de_message = FallbackHandler::get_message(fallback_handler, "de-DE", "welcome.message")
  assert_eq(de_de_message, "Welcome!") // de-DE -> de -> 未找到 -> en
  
  // 测试不存在的语言回退
  let fr_message = FallbackHandler::get_message(fallback_handler, "fr", "welcome.message")
  assert_eq(fr_message, "welcome.message") // 未找到任何回退，返回键名
  
  // 测试不存在的消息
  let unknown_message = FallbackHandler::get_message(fallback_handler, "zh-CN", "unknown.message")
  assert_eq(unknown_message, "unknown.message") // 未找到消息，返回键名
}

test "国际化配置管理测试" {
  // 创建国际化配置管理器
  let i18n_config = I18nConfig::new()
  
  // 设置默认语言
  I18nConfig::set_default_language(i18n_config, "en")
  assert_eq(I18nConfig::get_default_language(i18n_config), "en")
  
  // 设置支持的语言列表
  I18nConfig::set_supported_languages(i18n_config, ["en", "zh", "ja", "es", "fr", "de"])
  let supported_languages = I18nConfig::get_supported_languages(i18n_config)
  assert_eq(supported_languages.length(), 6)
  assert_true(supported_languages.contains("en"))
  assert_true(supported_languages.contains("zh"))
  assert_true(supported_languages.contains("ja"))
  assert_true(supported_languages.contains("es"))
  assert_true(supported_languages.contains("fr"))
  assert_true(supported_languages.contains("de"))
  
  // 设置时区
  I18nConfig::set_default_timezone(i18n_config, "UTC")
  assert_eq(I18nConfig::get_default_timezone(i18n_config), "UTC")
  
  // 设置日期格式
  I18nConfig::set_date_format(i18n_config, "en", "MM/DD/YYYY")
  I18nConfig::set_date_format(i18n_config, "zh", "YYYY年MM月DD日")
  I18nConfig::set_date_format(i18n_config, "ja", "YYYY/MM/DD")
  
  assert_eq(I18nConfig::get_date_format(i18n_config, "en"), "MM/DD/YYYY")
  assert_eq(I18nConfig::get_date_format(i18n_config, "zh"), "YYYY年MM月DD日")
  assert_eq(I18nConfig::get_date_format(i18n_config, "ja"), "YYYY/MM/DD")
  
  // 设置时间格式
  I18nConfig::set_time_format(i18n_config, "en", "h:mm:ss A")
  I18nConfig::set_time_format(i18n_config, "zh", "A h:mm:ss")
  I18nConfig::set_time_format(i18n_config, "ja", "H:mm:ss")
  
  assert_eq(I18nConfig::get_time_format(i18n_config, "en"), "h:mm:ss A")
  assert_eq(I18nConfig::get_time_format(i18n_config, "zh"), "A h:mm:ss")
  assert_eq(I18nConfig::get_time_format(i18n_config, "ja"), "H:mm:ss")
  
  // 测试配置验证
  let is_valid = I18nConfig::validate(i18n_config)
  assert_true(is_valid)
  
  // 测试无效配置
  I18nConfig::set_default_language(i18n_config, "invalid_lang") // 不在支持的语言列表中
  let is_invalid = I18nConfig::validate(i18n_config)
  assert_false(is_invalid)
  
  // 修复配置
  I18nConfig::set_default_language(i18n_config, "en")
  let is_fixed = I18nConfig::validate(i18n_config)
  assert_true(is_fixed)
}