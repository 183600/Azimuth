// 国际化支持的高级测试用例
// 测试多语言、多地区环境下的遥测系统功能

test "多语言错误消息本地化" {
  // 测试错误消息的多语言本地化
  let localizer = ErrorLocalizer::new()
  
  // 添加多语言错误消息
  ErrorLocalizer::add_message(localizer, "span.already.ended", "en", "Span has already been ended")
  ErrorLocalizer::add_message(localizer, "span.already.ended", "zh", "Span已经结束")
  ErrorLocalizer::add_message(localizer, "span.already.ended", "ja", "Spanは既に終了しています")
  ErrorLocalizer::add_message(localizer, "span.already.ended", "es", "Span ya ha finalizado")
  ErrorLocalizer::add_message(localizer, "span.already.ended", "fr", "Span est déjà terminé")
  
  ErrorLocalizer::add_message(localizer, "invalid.trace.id", "en", "Invalid trace ID format")
  ErrorLocalizer::add_message(localizer, "invalid.trace.id", "zh", "无效的跟踪ID格式")
  ErrorLocalizer::add_message(localizer, "invalid.trace.id", "ja", "無効なトレースID形式")
  ErrorLocalizer::add_message(localizer, "invalid.trace.id", "es", "Formato de ID de traza inválido")
  ErrorLocalizer::add_message(localizer, "invalid.trace.id", "fr", "Format d'ID de trace invalide")
  
  // 测试各种语言的错误消息
  let languages = ["en", "zh", "ja", "es", "fr"]
  let error_codes = ["span.already.ended", "invalid.trace.id"]
  
  for lang in languages {
    for error_code in error_codes {
      let localized_message = ErrorLocalizer::get_message(localizer, error_code, lang)
      assert_true(localized_message.length > 0, 
        "Should have localized message for " + error_code + " in " + lang)
      
      // 验证消息不是错误代码本身
      assert_not_eq(localized_message, error_code, 
        "Localized message should not be the error code itself")
    }
  }
  
  // 测试回退到默认语言
  let fallback_message = ErrorLocalizer::get_message(localizer, "span.already.ended", "de")
  assert_eq(fallback_message, "Span has already been ended", 
    "Should fallback to English for unsupported language")
}

test "多地区时间格式处理" {
  // 测试不同地区的时间格式处理
  let timestamp = Clock::now_unix_nanos(Clock::system())
  let formatter = TimeFormatter::new()
  
  // 配置不同地区的时间格式
  TimeFormatter::set_locale(formatter, "en-US", "MM/dd/yyyy HH:mm:ss.SSS")
  TimeFormatter::set_locale(formatter, "zh-CN", "yyyy年MM月dd日 HH:mm:ss.SSS")
  TimeFormatter::set_locale(formatter, "ja-JP", "yyyy/MM/dd HH:mm:ss.SSS")
  TimeFormatter::set_locale(formatter, "es-ES", "dd/MM/yyyy HH:mm:ss.SSS")
  TimeFormatter::set_locale(formatter, "fr-FR", "dd/MM/yyyy HH:mm:ss.SSS")
  TimeFormatter::set_locale(formatter, "de-DE", "dd.MM.yyyy HH:mm:ss.SSS")
  
  // 测试不同地区的时间格式
  let locales = ["en-US", "zh-CN", "ja-JP", "es-ES", "fr-FR", "de-DE"]
  let formatted_times = []
  
  for locale in locales {
    let formatted_time = TimeFormatter::format(formatter, timestamp, locale)
    formatted_times.push((locale, formatted_time))
    
    assert_true(formatted_time.length > 0, "Should have formatted time for " + locale)
    
    // 验证时间戳解析回原始值
    let parsed_timestamp = TimeFormatter::parse(formatter, formatted_time, locale)
    assert_true(parsed_timestamp.is_some, "Should be able to parse back formatted time")
    
    // 允许毫秒级精度差异
    let time_diff = Math::abs(parsed_timestamp.unwrap - timestamp)
    assert_true(time_diff < 1000000L, "Parsed time should be within 1ms of original")
  }
  
  // 验证不同地区的格式差异
  let us_time = formatted_times.filter(fn(t) { t.0 == "en-US" })[0].1
  let cn_time = formatted_times.filter(fn(t) { t.0 == "zh-CN" })[0].1
  let jp_time = formatted_times.filter(fn(t) { t.0 == "ja-JP" })[0].1
  
  assert_true(us_time.contains("/"), "US format should contain slashes")
  assert_true(cn_time.contains("年"), "Chinese format should contain year character")
  assert_true(jp_time.contains("/"), "Japanese format should contain slashes")
}

test "多地区数字格式化" {
  // 测试不同地区的数字格式化
  let formatter = NumberFormatter::new()
  
  // 配置不同地区的数字格式
  NumberFormatter::set_locale(formatter, "en-US", ".", ",") // 小数点，千位分隔符
  NumberFormatter::set_locale(formatter, "de-DE", ",", ".") // 逗号小数点，点千位分隔符
  NumberFormatter::set_locale(formatter, "fr-FR", ",", " ") // 逗号小数点，空格千位分隔符
  NumberFormatter::set_locale(formatter, "zh-CN", ".", ",") // 与英文相同
  NumberFormatter::set_locale(formatter, "ja-JP", ".", ",") // 与英文相同
  
  // 测试各种数字的格式化
  let test_numbers = [
    1234.567,
    1234567.89,
    0.12345,
    1000000.0,
    987654321.987654321
  ]
  
  let locales = ["en-US", "de-DE", "fr-FR", "zh-CN", "ja-JP"]
  
  for number in test_numbers {
    for locale in locales {
      let formatted_number = NumberFormatter::format(formatter, number, locale)
      assert_true(formatted_number.length > 0, 
        "Should have formatted number for " + locale)
      
      // 验证可以解析回原始数字
      let parsed_number = NumberFormatter::parse(formatter, formatted_number, locale)
      assert_true(parsed_number.is_some, "Should be able to parse back formatted number")
      
      // 允许浮点精度差异
      let number_diff = Math::abs(parsed_number.unwrap - number)
      assert_true(number_diff < 0.000001, "Parsed number should be very close to original")
    }
  }
  
  // 验证特定地区的格式特征
  let us_formatted = NumberFormatter::format(formatter, 1234567.89, "en-US")
  let de_formatted = NumberFormatter::format(formatter, 1234567.89, "de-DE")
  let fr_formatted = NumberFormatter::format(formatter, 1234567.89, "fr-FR")
  
  assert_true(us_formatted.contains("1,234,567.89"), "US format should use comma as thousands separator")
  assert_true(de_formatted.contains("1.234.567,89"), "German format should use dot as thousands separator")
  assert_true(fr_formatted.contains("1 234 567,89"), "French format should use space as thousands separator")
}

test "多语言日志消息国际化" {
  // 测试多语言日志消息的国际化
  let provider = LoggerProvider::i18n()
  let logger = LoggerProvider::get_logger(provider, "i18n-logger")
  
  // 配置多语言日志消息模板
  let message_templates = [
    ("user.login.success", "en", "User {username} successfully logged in from {ip}"),
    ("user.login.success", "zh", "用户 {username} 从 {ip} 成功登录"),
    ("user.login.success", "ja", "ユーザー {username} が {ip} から正常にログインしました"),
    ("user.login.success", "es", "Usuario {username} inició sesión exitosamente desde {ip}"),
    
    ("service.started", "en", "Service {service_name} started on port {port}"),
    ("service.started", "zh", "服务 {service_name} 在端口 {port} 上启动"),
    ("service.started", "ja", "サービス {service_name} がポート {port} で開始されました"),
    ("service.started", "es", "Servicio {service_name} iniciado en puerto {port}"),
    
    ("error.database.connection", "en", "Failed to connect to database: {error_message}"),
    ("error.database.connection", "zh", "连接数据库失败：{error_message}"),
    ("error.database.connection", "ja", "データベースへの接続に失敗しました：{error_message}"),
    ("error.database.connection", "es", "Error al conectar a la base de datos: {error_message}")
  ]
  
  for (template_id, lang, template) in message_templates {
    Logger::add_message_template(logger, template_id, lang, template)
  }
  
  // 测试多语言日志记录
  let test_scenarios = [
    ("user.login.success", "en", [("username", StringValue("john.doe")), ("ip", StringValue("192.168.1.100"))]),
    ("user.login.success", "zh", [("username", StringValue("张三")), ("ip", StringValue("192.168.1.101"))]),
    ("service.started", "ja", [("service_name", StringValue("api-service")), ("port", IntValue(8080))]),
    ("error.database.connection", "es", [("error_message", StringValue("Connection timeout"))])
  ]
  
  for (template_id, lang, params) in test_scenarios {
    let log_record = Logger::create_i18n_log_record(logger, template_id, lang, params)
    
    // 验证本地化消息生成
    let localized_message = LogRecord::body(log_record).unwrap
    assert_true(localized_message.length > 0, 
      "Should generate localized message for " + template_id + " in " + lang)
    
    // 验证参数替换
    for (param_name, param_value) in params {
      let string_value = match param_value {
        StringValue(s) => s
        IntValue(i) => i.to_string()
        _ => ""
      }
      assert_true(localized_message.contains(string_value), 
        "Localized message should contain parameter value for " + param_name)
    }
    
    // 记录日志
    Logger::emit(logger, log_record)
  }
  
  // 收集并验证日志
  let log_data = Logger::collect_i18n(logger)
  
  for scenario in test_scenarios {
    let lang_logs = log_data.by_language.get(scenario.1)
    assert_true(lang_logs.unwrap > 0, "Should have logs in " + scenario.1)
  }
}

test "多地区单位转换和显示" {
  // 测试不同地区的单位转换和显示
  let converter = UnitConverter::new()
  
  // 配置地区特定的单位偏好
  UnitConverter::set_locale_preferences(converter, "en-US", [
    ("length", "imperial"),    // 英制单位
    ("temperature", "fahrenheit"), // 华氏度
    ("weight", "pounds"),      // 磅
    ("volume", "gallons")      // 加仑
  ])
  
  UnitConverter::set_locale_preferences(converter, "zh-CN", [
    ("length", "metric"),      // 公制单位
    ("temperature", "celsius"), // 摄氏度
    ("weight", "kilograms"),   // 千克
    ("volume", "liters")       // 升
  ])
  
  UnitConverter::set_locale_preferences(converter, "ja-JP", [
    ("length", "metric"),
    ("temperature", "celsius"),
    ("weight", "kilograms"),
    ("volume", "liters")
  ])
  
  // 测试单位转换
  let test_measurements = [
    ("length", 100.0, "m"),    // 100米
    ("temperature", 25.0, "C"), // 25摄氏度
    ("weight", 70.0, "kg"),     // 70千克
    ("volume", 5.0, "l")        // 5升
  ]
  
  let locales = ["en-US", "zh-CN", "ja-JP"]
  
  for (unit_type, value, unit) in test_measurements {
    for locale in locales {
      let converted = UnitConverter::convert_for_locale(converter, value, unit, unit_type, locale)
      
      assert_true(converted.value > 0, "Converted value should be positive")
      assert_true(converted.unit.length > 0, "Should have unit string")
      assert_true(converted.display_string.length > 0, "Should have display string")
      
      // 验证转换合理性
      match unit_type {
        "length" => {
          if locale == "en-US" {
            assert_true(converted.unit.contains("ft") || converted.unit.contains("in"), 
              "US should use imperial length units")
          } else {
            assert_true(converted.unit.contains("m") || converted.unit.contains("cm"), 
              "Asian locales should use metric length units")
          }
        }
        "temperature" => {
          if locale == "en-US" {
            assert_true(converted.unit.contains("F"), "US should use Fahrenheit")
            assert_true(converted.value > 77.0, "25°C should be about 77°F")
          } else {
            assert_true(converted.unit.contains("C"), "Other locales should use Celsius")
            assert_true(Math::abs(converted.value - 25.0) < 0.1, "Should stay close to 25°C")
          }
        }
        _ => () // 其他类型的基本验证
      }
    }
  }
}

test "多地区时区处理" {
  // 测试多地区时区处理
  let timezone_handler = TimezoneHandler::new()
  
  // 配置测试时区
  let timezones = [
    ("UTC", 0),
    ("America/New_York", -5),
    ("America/Los_Angeles", -8),
    ("Europe/London", 0),
    ("Europe/Paris", 1),
    ("Asia/Shanghai", 8),
    ("Asia/Tokyo", 9),
    ("Australia/Sydney", 11)
  ]
  
  let base_timestamp = Clock::now_unix_nanos(Clock::system())
  
  // 测试时区转换
  for (tz_name, offset) in timezones {
    let localized_time = TimezoneHandler::convert(timezone_handler, base_timestamp, tz_name)
    
    assert_true(localized_time.timezone_name == tz_name, "Should have correct timezone name")
    assert_true(localized_time.offset_hours == offset, "Should have correct offset")
    assert_true(localized_time.formatted_time.length > 0, "Should have formatted time")
    
    // 验证时区偏移计算正确
    let utc_time = TimezoneHandler::to_utc(timezone_handler, localized_time)
    let time_diff = Math::abs(utc_time - base_timestamp)
    assert_true(time_diff < 1000000L, "UTC conversion should be accurate")
  }
  
  // 测试夏令时处理（使用固定日期进行测试）
  let summer_timestamp = 1686681600000000000L // 2023年6月13日 UTC
  let winter_timestamp = 1703856000000000000L // 2023年12月29日 UTC
  
  let ny_summer = TimezoneHandler::convert(timezone_handler, summer_timestamp, "America/New_York")
  let ny_winter = TimezoneHandler::convert(timezone_handler, winter_timestamp, "America/New_York")
  
  // 纽约夏季应该使用EDT (UTC-4)，冬季使用EST (UTC-5)
  assert_true(ny_summer.offset_hours == -4, "New York should use EDT in summer")
  assert_true(ny_winter.offset_hours == -5, "New York should use EST in winter")
}

test "多语言用户界面元素" {
  // 测试多语言用户界面元素
  let ui_localizer = UILocalizer::new()
  
  // 添加多语言UI元素
  let ui_elements = [
    ("button.start", "en", "Start"),
    ("button.start", "zh", "开始"),
    ("button.start", "ja", "開始"),
    ("button.start", "es", "Iniciar"),
    ("button.start", "fr", "Démarrer"),
    
    ("button.stop", "en", "Stop"),
    ("button.stop", "zh", "停止"),
    ("button.stop", "ja", "停止"),
    ("button.stop", "es", "Detener"),
    ("button.stop", "fr", "Arrêter"),
    
    ("menu.dashboard", "en", "Dashboard"),
    ("menu.dashboard", "zh", "仪表板"),
    ("menu.dashboard", "ja", "ダッシュボード"),
    ("menu.dashboard", "es", "Panel"),
    ("menu.dashboard", "fr", "Tableau de bord"),
    
    ("label.trace.id", "en", "Trace ID"),
    ("label.trace.id", "zh", "跟踪ID"),
    ("label.trace.id", "ja", "トレースID"),
    ("label.trace.id", "es", "ID de Traza"),
    ("label.trace.id", "fr", "ID de Trace")
  ]
  
  for (element_id, lang, text) in ui_elements {
    UILocalizer::add_element(ui_localizer, element_id, lang, text)
  }
  
  // 测试UI元素本地化
  let languages = ["en", "zh", "ja", "es", "fr"]
  let element_ids = ["button.start", "button.stop", "menu.dashboard", "label.trace.id"]
  
  for lang in languages {
    let localized_ui = UILocalizer::get_localized_ui(ui_localizer, lang)
    
    for element_id in element_ids {
      let localized_text = UILocalizer::get_element_text(ui_localizer, element_id, lang)
      assert_true(localized_text.length > 0, 
        "Should have localized text for " + element_id + " in " + lang)
      
      // 验证UI组件包含正确的文本
      assert_true(localized_ui.contains(localized_text), 
        "UI should contain localized text for " + element_id)
    }
  }
  
  // 测试回退机制
  let fallback_text = UILocalizer::get_element_text(ui_localizer, "button.start", "de")
  assert_eq(fallback_text, "Start", "Should fallback to English for unsupported language")
}

test "多文化数据格式验证" {
  // 测试多文化数据格式的验证和处理
  let validator = CulturalDataValidator::new()
  
  // 配置地区特定的数据格式
  validator.add_date_format("en-US", "MM/dd/yyyy")
  validator.add_date_format("en-GB", "dd/MM/yyyy")
  validator.add_date_format("zh-CN", "yyyy年MM月dd日")
  validator.add_date_format("ja-JP", "yyyy/MM/dd")
  validator.add_date_format("de-DE", "dd.MM.yyyy")
  
  validator.add_time_format("en-US", "hh:mm:ss a")
  validator.add_time_format("zh-CN", "HH:mm:ss")
  validator.add_time_format("ja-JP", "HH:mm:ss")
  validator.add_time_format("de-DE", "HH:mm:ss")
  
  validator.add_currency_format("en-US", "$#,##0.00")
  validator.add_currency_format("zh-CN", "¥#,##0.00")
  validator.add_currency_format("ja-JP", "¥#,##0.00")
  validator.add_currency_format("de-DE", "#.##0,00 €")
  
  // 测试日期格式验证
  let test_dates = [
    ("en-US", "12/25/2023"),
    ("en-GB", "25/12/2023"),
    ("zh-CN", "2023年12月25日"),
    ("ja-JP", "2023/12/25"),
    ("de-DE", "25.12.2023")
  ]
  
  for (locale, date_str) in test_dates {
    assert_true(validator.validate_date(date_str, locale), 
      "Should validate date format for " + locale)
    
    let parsed_date = validator.parse_date(date_str, locale)
    assert_true(parsed_date.is_some, "Should parse valid date for " + locale)
    
    // 验证解析结果一致性（所有日期都应该是2023年12月25日）
    let date = parsed_date.unwrap
    assert_eq(date.year, 2023, "Year should be 2023")
    assert_eq(date.month, 12, "Month should be 12")
    assert_eq(date.day, 25, "Day should be 25")
  }
  
  // 测试货币格式验证
  let test_currencies = [
    ("en-US", "$1,234.56"),
    ("zh-CN", "¥1,234.56"),
    ("ja-JP", "¥1,234.56"),
    ("de-DE", "1.234,56 €")
  ]
  
  for (locale, currency_str) in test_currencies {
    assert_true(validator.validate_currency(currency_str, locale), 
      "Should validate currency format for " + locale)
    
    let parsed_amount = validator.parse_currency(currency_str, locale)
    assert_true(parsed_amount.is_some, "Should parse valid currency for " + locale)
    
    // 验证解析结果一致性（所有金额都应该是1234.56）
    assert_true(Math::abs(parsed_amount.unwrap - 1234.56) < 0.01, 
      "Parsed amount should be 1234.56")
  }
  
  // 测试无效格式
  let invalid_dates = [
    ("en-US", "25/12/2023"), // 错误的格式
    ("zh-CN", "12/25/2023"), // 错误的格式
    ("de-DE", "25/12/2023")  // 错误的格式
  ]
  
  for (locale, date_str) in invalid_dates {
    assert_false(validator.validate_date(date_str, locale), 
      "Should reject invalid date format for " + locale)
  }
}