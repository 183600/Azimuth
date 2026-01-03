// Azimuth Telemetry System - Comprehensive Internationalization Tests
// This file contains comprehensive test cases for internationalization support

// Test 1: Multi-language Attribute Support
test "multi-language attribute support" {
  let attrs = Attributes::new()
  
  // Test Chinese attributes
  Attributes::set(attrs, "服务名称", StringValue("测试服务"))
  Attributes::set(attrs, "服务版本", StringValue("1.0.0"))
  Attributes::set(attrs, "主机名称", StringValue("测试主机"))
  
  let service_name = Attributes::get(attrs, "服务名称")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "测试服务")
    _ => assert_true(false)
  }
  
  // Test Japanese attributes
  Attributes::set(attrs, "サービス名", StringValue("テストサービス"))
  Attributes::set(attrs, "サービスバージョン", StringValue("1.0.0"))
  Attributes::set(attrs, "ホスト名", StringValue("テストホスト"))
  
  let service_name_ja = Attributes::get(attrs, "サービス名")
  match service_name_ja {
    Some(StringValue(name)) => assert_eq(name, "テストサービス")
    _ => assert_true(false)
  }
  
  // Test Korean attributes
  Attributes::set(attrs, "서비스 이름", StringValue("테스트 서비스"))
  Attributes::set(attrs, "서비스 버전", StringValue("1.0.0"))
  Attributes::set(attrs, "호스트 이름", StringValue("테스트 호스트"))
  
  let service_name_ko = Attributes::get(attrs, "서비스 이름")
  match service_name_ko {
    Some(StringValue(name)) => assert_eq(name, "테스트 서비스")
    _ => assert_true(false)
  }
  
  // Test Arabic attributes
  Attributes::set(attrs, "اسم الخدمة", StringValue("خدمة الاختبار"))
  Attributes::set(attrs, "إصدار الخدمة", StringValue("1.0.0"))
  Attributes::set(attrs, "اسم المضيف", StringValue("مضيف الاختبار"))
  
  let service_name_ar = Attributes::get(attrs, "اسم الخدمة")
  match service_name_ar {
    Some(StringValue(name)) => assert_eq(name, "خدمة الاختبار")
    _ => assert_true(false)
  }
  
  // Test Russian attributes
  Attributes::set(attrs, "имя сервиса", StringValue("тестовый сервис"))
  Attributes::set(attrs, "версия сервиса", StringValue("1.0.0"))
  Attributes::set(attrs, "имя хоста", StringValue("тестовый хост"))
  
  let service_name_ru = Attributes::get(attrs, "имя сервиса")
  match service_name_ru {
    Some(StringValue(name)) => assert_eq(name, "тестовый сервис")
    _ => assert_true(false)
  }
  
  // Verify all attributes are stored correctly
  assert_eq(Attributes::size(attrs), 15)
}

// Test 2: Unicode and Special Character Handling
test "unicode and special character handling" {
  let attrs = Attributes::new()
  
  // Test emoji characters
  Attributes::set(attrs, "status_emoji", StringValue("🚀 Success"))
  Attributes::set(attrs, "error_emoji", StringValue("❌ Error"))
  Attributes::set(attrs, "warning_emoji", StringValue("⚠️ Warning"))
  
  let status_emoji = Attributes::get(attrs, "status_emoji")
  match status_emoji {
    Some(StringValue(emoji)) => assert_eq(emoji, "🚀 Success")
    _ => assert_true(false)
  }
  
  // Test mathematical symbols
  Attributes::set(attrs, "math_symbols", StringValue("∑ ∏ ∫ ∆ ∇ ∂"))
  Attributes::set(attrs, "greek_letters", StringValue("α β γ δ ε ζ η θ"))
  
  let math_symbols = Attributes::get(attrs, "math_symbols")
  match math_symbols {
    Some(StringValue(symbols)) => assert_eq(symbols, "∑ ∏ ∫ ∆ ∇ ∂")
    _ => assert_true(false)
  }
  
  // Test currency symbols
  Attributes::set(attrs, "currency_symbols", StringValue("$ € £ ¥ ₹ ₽ ₩"))
  
  let currency_symbols = Attributes::get(attrs, "currency_symbols")
  match currency_symbols {
    Some(StringValue(symbols)) => assert_eq(symbols, "$ € £ ¥ ₹ ₽ ₩")
    _ => assert_true(false)
  }
  
  // Test combining characters
  Attributes::set(attrs, "combining_chars", StringValue("é ä ö ü ñ"))
  
  let combining_chars = Attributes::get(attrs, "combining_chars")
  match combining_chars {
    Some(StringValue(chars)) => assert_eq(chars, "é ä ö ü ñ")
    _ => assert_true(false)
  }
  
  // Test right-to-left text
  Attributes::set(attrs, "rtl_text", StringValue("مرحبا بالعالم"))
  Attributes::set(attrs, "mixed_text", StringValue("Hello مرحبا World"))
  
  let rtl_text = Attributes::get(attrs, "rtl_text")
  match rtl_text {
    Some(StringValue(text)) => assert_eq(text, "مرحبا بالعالم")
    _ => assert_true(false)
  }
}

// Test 3: Locale-aware Formatting
test "locale-aware formatting" {
  let formatter = LocaleFormatter::new()
  
  // Test number formatting with different locales
  let number = 1234567.89
  
  let us_formatted = LocaleFormatter::format_number(formatter, number, "en-US")
  assert_eq(us_formatted, "1,234,567.89")
  
  let de_formatted = LocaleFormatter::format_number(formatter, number, "de-DE")
  assert_eq(de_formatted, "1.234.567,89")
  
  let fr_formatted = LocaleFormatter::format_number(formatter, number, "fr-FR")
  assert_eq(fr_formatted, "1 234 567,89")
  
  let ar_formatted = LocaleFormatter::format_number(formatter, number, "ar-SA")
  assert_eq(ar_formatted, "١٬٢٣٤٬٥٦٧٫٨٩")
  
  let hi_formatted = LocaleFormatter::format_number(formatter, number, "hi-IN")
  assert_eq(hi_formatted, "१,२३,४५६,७.८९")
  
  // Test date formatting with different locales
  let timestamp = 1609459200000 // 2021-01-01 00:00:00 UTC
  
  let us_date = LocaleFormatter::format_date(formatter, timestamp, "en-US")
  assert_true(us_date.contains("2021") && us_date.contains("January"))
  
  let de_date = LocaleFormatter::format_date(formatter, timestamp, "de-DE")
  assert_true(de_date.contains("2021") && de_date.contains("Januar"))
  
  let fr_date = LocaleFormatter::format_date(formatter, timestamp, "fr-FR")
  assert_true(fr_date.contains("2021") && fr_date.contains("janvier"))
  
  let ja_date = LocaleFormatter::format_date(formatter, timestamp, "ja-JP")
  assert_true(ja_date.contains("2021") && ja_date.contains("1月"))
  
  let zh_date = LocaleFormatter::format_date(formatter, timestamp, "zh-CN")
  assert_true(zh_date.contains("2021") && zh_date.contains("1月"))
  
  // Test currency formatting with different locales
  let amount = 1234.56
  
  let us_currency = LocaleFormatter::format_currency(formatter, amount, "en-US", "USD")
  assert_eq(us_currency, "$1,234.56")
  
  let eu_currency = LocaleFormatter::format_currency(formatter, amount, "de-DE", "EUR")
  assert_eq(eu_currency, "1.234,56 €")
  
  let jp_currency = LocaleFormatter::format_currency(formatter, amount, "ja-JP", "JPY")
  assert_eq(jp_currency, "￥1,235") // Japanese yen doesn't use decimal places
  
  let gb_currency = LocaleFormatter::format_currency(formatter, amount, "en-GB", "GBP")
  assert_eq(gb_currency, "£1,234.56")
}

// Test 4: Multi-language Error Messages
test "multi-language error messages" {
  let error_handler = ErrorHandler::new()
  
  // Test error messages in different languages
  let error_code = "CONNECTION_FAILED"
  
  let en_error = ErrorHandler::get_error_message(error_handler, error_code, "en")
  assert_eq(en_error, "Connection failed")
  
  let zh_error = ErrorHandler::get_error_message(error_handler, error_code, "zh")
  assert_eq(zh_error, "连接失败")
  
  let ja_error = ErrorHandler::get_error_message(error_handler, error_code, "ja")
  assert_eq(ja_error, "接続に失敗しました")
  
  let ko_error = ErrorHandler::get_error_message(error_handler, error_code, "ko")
  assert_eq(ko_error, "연결에 실패했습니다")
  
  let ar_error = ErrorHandler::get_error_message(error_handler, error_code, "ar")
  assert_eq(ar_error, "فشل الاتصال")
  
  let ru_error = ErrorHandler::get_error_message(error_handler, error_code, "ru")
  assert_eq(ru_error, "Соединение не удалось")
  
  // Test parameterized error messages
  let timeout_error = "TIMEOUT_ERROR"
  let timeout_value = 5000
  
  let en_timeout = ErrorHandler::get_parameterized_error_message(error_handler, timeout_error, timeout_value, "en")
  assert_eq(en_timeout, "Operation timed out after 5000ms")
  
  let zh_timeout = ErrorHandler::get_parameterized_error_message(error_handler, timeout_error, timeout_value, "zh")
  assert_eq(zh_timeout, "操作在5000毫秒后超时")
  
  let ja_timeout = ErrorHandler::get_parameterized_error_message(error_handler, timeout_error, timeout_value, "ja")
  assert_eq(ja_timeout, "操作が5000ミリ秒後にタイムアウトしました")
  
  // Test fallback to English for unsupported languages
  let unsupported_error = ErrorHandler::get_error_message(error_handler, error_code, "unsupported")
  assert_eq(unsupported_error, "Connection failed") // Should fallback to English
}

// Test 5: Internationalized Log Messages
test "internationalized log messages" {
  let logger = Logger::new()
  
  // Test creating log records with internationalized messages
  let en_log = LogRecord::new_with_locale(Info, "Operation completed successfully", "en")
  let zh_log = LogRecord::new_with_locale(Info, "操作成功完成", "zh")
  let ja_log = LogRecord::new_with_locale(Info, "操作が正常に完了しました", "ja")
  let ko_log = LogRecord::new_with_locale(Info, "작업이 성공적으로 완료되었습니다", "ko")
  
  // Verify log messages are preserved correctly
  assert_eq(LogRecord::get_message(en_log), "Operation completed successfully")
  assert_eq(LogRecord::get_message(zh_log), "操作成功完成")
  assert_eq(LogRecord::get_message(ja_log), "操作が正常に完了しました")
  assert_eq(LogRecord::get_message(ko_log), "작업이 성공적으로 완료되었습니다")
  
  // Test log message templates with parameters
  let template = "User {username} logged in from {location}"
  let params = [
    ("username", "张三"),
    ("location", "北京")
  ]
  
  let zh_formatted_log = LogRecord::new_with_template_and_locale(Info, template, params, "zh")
  assert_eq(LogRecord::get_message(zh_formatted_log), "用户 张三 从 北京 登录")
  
  let en_params = [
    ("username", "John Doe"),
    ("location", "New York")
  ]
  
  let en_formatted_log = LogRecord::new_with_template_and_locale(Info, template, en_params, "en")
  assert_eq(LogRecord::get_message(en_formatted_log), "User John Doe logged in from New York")
  
  // Test log severity level names in different languages
  let severity_names = LogRecord::get_severity_names("en")
  assert_eq(severity_names[0], "TRACE")
  assert_eq(severity_names[1], "DEBUG")
  assert_eq(severity_names[2], "INFO")
  assert_eq(severity_names[3], "WARN")
  assert_eq(severity_names[4], "ERROR")
  assert_eq(severity_names[5], "FATAL")
  
  let severity_names_zh = LogRecord::get_severity_names("zh")
  assert_eq(severity_names_zh[0], "跟踪")
  assert_eq(severity_names_zh[1], "调试")
  assert_eq(severity_names_zh[2], "信息")
  assert_eq(severity_names_zh[3], "警告")
  assert_eq(severity_names_zh[4], "错误")
  assert_eq(severity_names_zh[5], "致命")
}

// Test 6: Internationalized Span Names and Descriptions
test "internationalized span names and descriptions" {
  let tracer = Tracer::new()
  
  // Test creating spans with internationalized names
  let en_span = tracer.create_span("Database Query", "en")
  let zh_span = tracer.create_span("数据库查询", "zh")
  let ja_span = tracer.create_span("データベースクエリ", "ja")
  let ko_span = tracer.create_span("데이터베이스 쿼리", "ko")
  
  // Verify span names are preserved correctly
  assert_eq(Span::get_name(en_span), "Database Query")
  assert_eq(Span::get_name(zh_span), "数据库查询")
  assert_eq(Span::get_name(ja_span), "データベースクエリ")
  assert_eq(Span::get_name(ko_span), "데이터베이스 쿼리")
  
  // Test adding internationalized events
  Span::add_event_with_locale(en_span, "Query started", "en")
  Span::add_event_with_locale(zh_span, "查询已开始", "zh")
  Span::add_event_with_locale(ja_span, "クエリが開始されました", "ja")
  Span::add_event_with_locale(ko_span, "쿼리가 시작되었습니다", "ko")
  
  // Test setting internationalized status descriptions
  Span::set_status_with_description(en_span, Ok, "Query completed successfully", "en")
  Span::set_status_with_description(zh_span, Ok, "查询成功完成", "zh")
  Span::set_status_with_description(ja_span, Ok, "クエリが正常に完了しました", "ja")
  Span::set_status_with_description(ko_span, Ok, "쿼리가 성공적으로 완료되었습니다", "ko")
  
  // Test span kind names in different languages
  let kind_names_en = Span::get_kind_names("en")
  assert_eq(kind_names_en[0], "Internal")
  assert_eq(kind_names_en[1], "Server")
  assert_eq(kind_names_en[2], "Client")
  assert_eq(kind_names_en[3], "Producer")
  assert_eq(kind_names_en[4], "Consumer")
  
  let kind_names_zh = Span::get_kind_names("zh")
  assert_eq(kind_names_zh[0], "内部")
  assert_eq(kind_names_zh[1], "服务器")
  assert_eq(kind_names_zh[2], "客户端")
  assert_eq(kind_names_zh[3], "生产者")
  assert_eq(kind_names_zh[4], "消费者")
}

// Test 7: Internationalized Metric Names and Descriptions
test "internationalized metric names and descriptions" {
  let meter = Meter::new()
  
  // Test creating metrics with internationalized names and descriptions
  let en_counter = meter.create_counter_with_locale("request.count", "Total number of requests", "en")
  let zh_counter = meter.create_counter_with_locale("请求计数", "请求总数", "zh")
  let ja_counter = meter.create_counter_with_locale("リクエスト数", "リクエストの総数", "ja")
  let ko_counter = meter.create_counter_with_locale("요청 수", "요청의 총 수", "ko")
  
  // Verify metric names and descriptions are preserved correctly
  assert_eq(Metric::get_name(en_counter), "request.count")
  assert_eq(Metric::get_description(en_counter), "Total number of requests")
  
  assert_eq(Metric::get_name(zh_counter), "请求计数")
  assert_eq(Metric::get_description(zh_counter), "请求总数")
  
  assert_eq(Metric::get_name(ja_counter), "リクエスト数")
  assert_eq(Metric::get_description(ja_counter), "リクエストの総数")
  
  assert_eq(Metric::get_name(ko_counter), "요청 수")
  assert_eq(Metric::get_description(ko_counter), "요청의 총 수")
  
  // Test metric unit names in different languages
  let unit_names_en = Metric::get_unit_names("en")
  assert_eq(unit_names_en[0], "count")
  assert_eq(unit_names_en[1], "bytes")
  assert_eq(unit_names_en[2], "milliseconds")
  assert_eq(unit_names_en[3], "percent")
  
  let unit_names_zh = Metric::get_unit_names("zh")
  assert_eq(unit_names_zh[0], "计数")
  assert_eq(unit_names_zh[1], "字节")
  assert_eq(unit_names_zh[2], "毫秒")
  assert_eq(unit_names_zh[3], "百分比")
  
  // Test adding internationalized labels
  Counter::add_with_labels(en_counter, 1.0, [("method", "GET"), ("status", "200")])
  Counter::add_with_labels(zh_counter, 1.0, [("方法", "获取"), ("状态", "200")])
  Counter::add_with_labels(ja_counter, 1.0, [("メソッド", "GET"), ("ステータス", "200")])
  Counter::add_with_labels(ko_counter, 1.0, [("방법", "GET"), ("상태", "200")])
}

// Test 8: Resource Internationalization
test "resource internationalization" {
  let resource = Resource::new()
  
  // Test adding internationalized resource attributes
  Resource::set_attribute_with_locale(resource, "service.name", "テストサービス", "ja")
  Resource::set_attribute_with_locale(resource, "service.description", "这是一个测试服务", "zh")
  Resource::set_attribute_with_locale(resource, "service.owner", "테스트팀", "ko")
  Resource::set_attribute_with_locale(resource, "service.environment", "بيئة الاختبار", "ar")
  
  // Verify attributes are preserved correctly
  let service_name = Resource::get_attribute(resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "テストサービス")
    _ => assert_true(false)
  }
  
  let service_desc = Resource::get_attribute(resource, "service.description")
  match service_desc {
    Some(StringValue(desc)) => assert_eq(desc, "这是一个测试服务")
    _ => assert_true(false)
  }
  
  let service_owner = Resource::get_attribute(resource, "service.owner")
  match service_owner {
    Some(StringValue(owner)) => assert_eq(owner, "테스트팀")
    _ => assert_true(false)
  }
  
  let service_env = Resource::get_attribute(resource, "service.environment")
  match service_env {
    Some(StringValue(env)) => assert_eq(env, "بيئة الاختبار")
    _ => assert_true(false)
  }
  
  // Test resource attribute key internationalization
  Resource::set_attribute(resource, "サービス名", StringValue("テストサービス"))
  Resource::set_attribute(resource, "服务名称", StringValue("测试服务"))
  Resource::set_attribute(resource, "서비스 이름", StringValue("테스트 서비스"))
  
  let service_name_ja = Resource::get_attribute(resource, "サービス名")
  match service_name_ja {
    Some(StringValue(name)) => assert_eq(name, "テストサービス")
    _ => assert_true(false)
  }
  
  let service_name_zh = Resource::get_attribute(resource, "服务名称")
  match service_name_zh {
    Some(StringValue(name)) => assert_eq(name, "测试服务")
    _ => assert_true(false)
  }
  
  let service_name_ko = Resource::get_attribute(resource, "서비스 이름")
  match service_name_ko {
    Some(StringValue(name)) => assert_eq(name, "테스트 서비스")
    _ => assert_true(false)
  }
}

// Test 9: Internationalization Configuration
test "internationalization configuration" {
  let config = I18nConfig::new()
  
  // Test setting default locale
  I18nConfig::set_default_locale(config, "zh-CN")
  assert_eq(I18nConfig::get_default_locale(config), "zh-CN")
  
  // Test adding supported locales
  I18nConfig::add_supported_locale(config, "en-US")
  I18nConfig::add_supported_locale(config, "ja-JP")
  I18nConfig::add_supported_locale(config, "ko-KR")
  I18nConfig::add_supported_locale(config, "ar-SA")
  I18nConfig::add_supported_locale(config, "ru-RU")
  
  let supported_locales = I18nConfig::get_supported_locales(config)
  assert_eq(supported_locales.length(), 6)
  assert_true(supported_locales.contains("zh-CN"))
  assert_true(supported_locales.contains("en-US"))
  assert_true(supported_locales.contains("ja-JP"))
  assert_true(supported_locales.contains("ko-KR"))
  assert_true(supported_locales.contains("ar-SA"))
  assert_true(supported_locales.contains("ru-RU"))
  
  // Test locale fallback chain
  I18nConfig::set_locale_fallback(config, "zh-HK", ["zh-TW", "zh-CN", "en"])
  
  let fallback_chain = I18nConfig::get_locale_fallback(config, "zh-HK")
  assert_eq(fallback_chain[0], "zh-HK")
  assert_eq(fallback_chain[1], "zh-TW")
  assert_eq(fallback_chain[2], "zh-CN")
  assert_eq(fallback_chain[3], "en")
  
  // Test locale-specific formatting rules
  I18nConfig::set_number_format(config, "en-US", NumberFormat {
    decimal_separator: ".",
    thousands_separator: ",",
    grouping: [3, 3]
  })
  
  I18nConfig::set_number_format(config, "de-DE", NumberFormat {
    decimal_separator: ",",
    thousands_separator: ".",
    grouping: [3, 3]
  })
  
  I18nConfig::set_number_format(config, "fr-FR", NumberFormat {
    decimal_separator: ",",
    thousands_separator: " ",
    grouping: [3, 3]
  })
  
  let en_format = I18nConfig::get_number_format(config, "en-US")
  assert_eq(en_format.decimal_separator, ".")
  assert_eq(en_format.thousands_separator, ",")
  
  let de_format = I18nConfig::get_number_format(config, "de-DE")
  assert_eq(de_format.decimal_separator, ",")
  assert_eq(de_format.thousands_separator, ".")
  
  let fr_format = I18nConfig::get_number_format(config, "fr-FR")
  assert_eq(fr_format.decimal_separator, ",")
  assert_eq(fr_format.thousands_separator, " ")
}

// Test 10: Internationalization Performance
test "internationalization performance" {
  let i18n_system = I18nSystem::new()
  
  // Initialize with multiple locales
  I18nSystem::add_locale(i18n_system, "en-US")
  I18nSystem::add_locale(i18n_system, "zh-CN")
  I18nSystem::add_locale(i18n_system, "ja-JP")
  I18nSystem::add_locale(i18n_system, "ko-KR")
  I18nSystem::add_locale(i18n_system, "ar-SA")
  I18nSystem::add_locale(i18n_system, "ru-RU")
  I18nSystem::add_locale(i18n_system, "de-DE")
  I18nSystem::add_locale(i18n_system, "fr-FR")
  
  let start_time = get_current_time_millis()
  
  // Test performance of locale-specific operations
  for i in 0..10000 {
    let locale = match i % 8 {
      0 => "en-US"
      1 => "zh-CN"
      2 => "ja-JP"
      3 => "ko-KR"
      4 => "ar-SA"
      5 => "ru-RU"
      6 => "de-DE"
      _ => "fr-FR"
    }
    
    // Test message translation
    let message = I18nSystem::translate(i18n_system, "OPERATION_SUCCESS", locale)
    
    // Test number formatting
    let number = I18nSystem::format_number(i18n_system, 1234.56, locale)
    
    // Test date formatting
    let date = I18nSystem::format_date(i18n_system, get_current_time_millis(), locale)
  }
  
  let end_time = get_current_time_millis()
  let total_time = end_time - start_time
  
  // Performance assertion - should complete within reasonable time
  assert_true(total_time < 5000) // Should complete within 5 seconds
  
  // Calculate operations per second
  let total_operations = 10000 * 3 // 3 operations per iteration
  let ops_per_second = total_operations / (total_time / 1000)
  assert_true(ops_per_second > 5000) // Should handle at least 5000 ops/sec
}

// Helper function to get current time in milliseconds
fn get_current_time_millis() -> Int {
  // Mock implementation - in real code would use system time
  1609459200000 + Random::int(0, 86400000) // Random time within a day
}