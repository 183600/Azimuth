// Azimuth Telemetry System - Internationalization Support Tests
// This file contains comprehensive test cases for internationalization (i18n) support

// Test 1: Multi-language Attribute Values
test "multi-language attribute values" {
  let i18n_manager = I18nManager::new()
  
  // Test creating spans with multi-language attributes
  let trace_id = "7af7651916cd43dd8448eb211c8031a3"
  let span_id = "t7ad6b7169203343"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "i18n_service")
  let span = Span::new("i18n_operation", Server, span_ctx)
  
  // Set attributes in different languages
  let localized_attributes = [
    ("error.message.en", StringValue("Connection timeout")),
    ("error.message.zh", StringValue("连接超时")),
    ("error.message.ja", StringValue("接続タイムアウト")),
    ("error.message.es", StringValue("Tiempo de conexión agotado")),
    ("error.message.fr", StringValue("Délai de connexion dépassé")),
    
    ("service.description.en", StringValue("User authentication service")),
    ("service.description.zh", StringValue("用户认证服务")),
    ("service.description.ja", StringValue("ユーザー認証サービス")),
    ("service.description.es", StringValue("Servicio de autenticación de usuarios")),
    ("service.description.fr", StringValue("Service d'authentification des utilisateurs"))
  ]
  
  for (key, value) in localized_attributes {
    Span::set_attribute(span, key, value)
  }
  
  // Test language-specific attribute retrieval
  let en_error_msg = Span::get_localized_attribute(span, "error.message", "en")
  match en_error_msg {
    Some(StringValue(msg)) => assert_eq(msg, "Connection timeout")
    _ => assert_true(false)
  }
  
  let zh_error_msg = Span::get_localized_attribute(span, "error.message", "zh")
  match zh_error_msg {
    Some(StringValue(msg)) => assert_eq(msg, "连接超时")
    _ => assert_true(false)
  }
  
  let ja_error_msg = Span::get_localized_attribute(span, "error.message", "ja")
  match ja_error_msg {
    Some(StringValue(msg)) => assert_eq(msg, "接続タイムアウト")
    _ => assert_true(false)
  }
  
  // Test fallback to default language
  let default_error_msg = Span::get_localized_attribute(span, "error.message", "de")
  match default_error_msg {
    Some(StringValue(msg)) => assert_eq(msg, "Connection timeout") // Should fallback to English
    _ => assert_true(false)
  }
  
  Span::end(span)
}

// Test 2: Localized Log Messages
test "localized log messages" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "i18n_logger")
  let i18n_manager = I18nManager::new()
  
  // Set up message templates for different languages
  I18nManager::add_message_template(i18n_manager, "user.login.success", "en", "User {username} logged in successfully")
  I18nManager::add_message_template(i18n_manager, "user.login.success", "zh", "用户 {username} 登录成功")
  I18nManager::add_message_template(i18n_manager, "user.login.success", "ja", "ユーザー {username} が正常にログインしました")
  I18nManager::add_message_template(i18n_manager, "user.login.success", "es", "El usuario {username} inició sesión correctamente")
  I18nManager::add_message_template(i18n_manager, "user.login.success", "fr", "L'utilisateur {username} s'est connecté avec succès")
  
  // Create localized log records
  let login_params = [("username", StringValue("john_doe"))]
  
  let en_log = I18nManager::create_localized_log(i18n_manager, Info, "user.login.success", "en", login_params)
  let zh_log = I18nManager::create_localized_log(i18n_manager, Info, "user.login.success", "zh", login_params)
  let ja_log = I18nManager::create_localized_log(i18n_manager, Info, "user.login.success", "ja", login_params)
  
  // Verify localized messages
  match LogRecord::body(en_log) {
    Some(body) => assert_eq(body, "User john_doe logged in successfully")
    None => assert_true(false)
  }
  
  match LogRecord::body(zh_log) {
    Some(body) => assert_eq(body, "用户 john_doe 登录成功")
    None => assert_true(false)
  }
  
  match LogRecord::body(ja_log) {
    Some(body) => assert_eq(body, "ユーザー john_doe が正常にログインしました")
    None => assert_true(false)
  }
  
  // Emit localized logs
  Logger::emit(logger, en_log)
  Logger::emit(logger, zh_log)
  Logger::emit(logger, ja_log)
  
  // Verify logs were recorded with correct localization
  let logs = Logger::get_logs(logger)
  assert_true(logs.length() >= 3)
  
  let en_log_found = Array::any(logs, fn(log) {
    match LogRecord::body(log) {
      Some(body) => String::contains(body, "User john_doe logged in successfully")
      None => false
    }
  })
  assert_true(en_log_found)
  
  let zh_log_found = Array::any(logs, fn(log) {
    match LogRecord::body(log) {
      Some(body) => String::contains(body, "用户 john_doe 登录成功")
      None => false
    }
  })
  assert_true(zh_log_found)
  
  let ja_log_found = Array::any(logs, fn(log) {
    match LogRecord::body(log) {
      Some(body) => String::contains(body, "ユーザー john_doe が正常にログインしました")
      None => false
    }
  })
  assert_true(ja_log_found)
}

// Test 3: Locale-aware Number and Date Formatting
test "locale-aware number and date formatting" {
  let i18n_manager = I18nManager::new()
  
  // Test number formatting for different locales
  let number = 1234567.89
  
  let en_formatted = I18nManager::format_number(i18n_manager, number, "en-US")
  assert_eq(en_formatted, "1,234,567.89")
  
  let de_formatted = I18nManager::format_number(i18n_manager, number, "de-DE")
  assert_eq(de_formatted, "1.234.567,89")
  
  let fr_formatted = I18nManager::format_number(i18n_manager, number, "fr-FR")
  assert_eq(fr_formatted, "1 234 567,89")
  
  let zh_formatted = I18nManager::format_number(i18n_manager, number, "zh-CN")
  assert_eq(zh_formatted, "1,234,567.89")
  
  let ja_formatted = I18nManager::format_number(i18n_manager, number, "ja-JP")
  assert_eq(ja_formatted, "1,234,567.89")
  
  // Test date formatting for different locales
  let timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  let en_date = I18nManager::format_date(i18n_manager, timestamp, "en-US", "medium")
  assert_eq(en_date, "Jan 1, 2021")
  
  let de_date = I18nManager::format_date(i18n_manager, timestamp, "de-DE", "medium")
  assert_eq(de_date, "1. Jan. 2021")
  
  let fr_date = I18nManager::format_date(i18n_manager, timestamp, "fr-FR", "medium")
  assert_eq(fr_date, "1 janv. 2021")
  
  let zh_date = I18nManager::format_date(i18n_manager, timestamp, "zh-CN", "medium")
  assert_eq(zh_date, "2021年1月1日")
  
  let ja_date = I18nManager::format_date(i18n_manager, timestamp, "ja-JP", "medium")
  assert_eq(ja_date, "2021/01/01")
  
  // Test locale-aware formatting in telemetry data
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "i18n_meter")
  let counter = Meter::create_counter(meter, "localized_counter", Some("Localized counter"), Some("count"))
  
  // Record metrics with locale-formatted labels
  let en_label = I18nManager::format_metric_label(i18n_manager, "request.count", "en-US")
  let zh_label = I18nManager::format_metric_label(i18n_manager, "request.count", "zh-CN")
  
  let en_attrs = Attributes::new()
  Attributes::set(en_attrs, "metric.name", StringValue(en_label))
  Attributes::set(en_attrs, "locale", StringValue("en-US"))
  Counter::add(counter, 100.0, Some(en_attrs))
  
  let zh_attrs = Attributes::new()
  Attributes::set(zh_attrs, "metric.name", StringValue(zh_label))
  Attributes::set(zh_attrs, "locale", StringValue("zh-CN"))
  Counter::add(counter, 200.0, Some(zh_attrs))
  
  // Verify metrics with localized labels
  let en_metric_value = Counter::get_value_with_attributes(counter, en_attrs)
  match en_metric_value {
    Some(value) => assert_eq(value, 100.0)
    None => assert_true(false)
  }
  
  let zh_metric_value = Counter::get_value_with_attributes(counter, zh_attrs)
  match zh_metric_value {
    Some(value) => assert_eq(value, 200.0)
    None => assert_true(false)
  }
}

// Test 4: Right-to-Left (RTL) Language Support
test "right-to-left language support" {
  let i18n_manager = I18nManager::new()
  
  // Test RTL language detection
  assert_true(I18nManager::is_rtl_language(i18n_manager, "ar")) // Arabic
  assert_true(I18nManager::is_rtl_language(i18n_manager, "he")) // Hebrew
  assert_true(I18nManager::is_rtl_language(i18n_manager, "fa")) // Persian
  assert_true(I18nManager::is_rtl_language(i18n_manager, "ur")) // Urdu
  
  assert_false(I18nManager::is_rtl_language(i18n_manager, "en")) // English
  assert_false(I18nManager::is_rtl_language(i18n_manager, "zh")) // Chinese
  assert_false(I18nManager::is_rtl_language(i18n_manager, "ja")) // Japanese
  
  // Test creating spans with RTL attributes
  let trace_id = "8af7651916cd43dd8448eb211c8031a4"
  let span_id = "u7ad6b7169203344"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "rtl_service")
  let span = Span::new("rtl_operation", Server, span_ctx)
  
  // Add RTL attributes
  let rtl_attributes = [
    ("error.message.ar", StringValue("انتهت مهلة الاتصال")),
    ("error.message.he", StringValue("פג תום הזמן הקצוב")),
    ("service.description.ar", StringValue("خدمة مصادقة المستخدم")),
    ("service.description.he", StringValue("שירות אימות משתמשים"))
  ]
  
  for (key, value) in rtl_attributes {
    Span::set_attribute(span, key, value)
  }
  
  // Test RTL text direction handling
  let ar_error_msg = Span::get_localized_attribute(span, "error.message", "ar")
  match ar_error_msg {
    Some(StringValue(msg)) => {
      assert_eq(msg, "انتهت مهلة الاتصال")
      assert_true(I18nManager::is_rtl_text(i18n_manager, msg))
    }
    None => assert_true(false)
  }
  
  let he_error_msg = Span::get_localized_attribute(span, "error.message", "he")
  match he_error_msg {
    Some(StringValue(msg)) => {
      assert_eq(msg, "פג תום הזמן הקצוב")
      assert_true(I18nManager::is_rtl_text(i18n_manager, msg))
    }
    None => assert_true(false)
  }
  
  // Test mixed LTR/RTL text handling
  let mixed_text = "Connection timeout: انتهت مهلة الاتصال"
  assert_true(I18nManager::contains_rtl_text(i18n_manager, mixed_text))
  
  // Test RTL text formatting for display
  let formatted_ar = I18nManager::format_for_display(i18n_manager, "انتهت مهلة الاتصال", "ar")
  assert_eq(formatted_ar, "\u200Eانتهت مهلة الاتصال\u200E") // Should include LRM markers
  
  let formatted_he = I18nManager::format_for_display(i18n_manager, "פג תום הזמן הקצוב", "he")
  assert_eq(formatted_he, "\u200Eפג תום הזמן הקצוב\u200E") // Should include LRM markers
  
  Span::end(span)
}

// Test 5: Currency and Unit Localization
test "currency and unit localization" {
  let i18n_manager = I18nManager::new()
  
  // Test currency formatting
  let amount = 1234.56
  
  let en_currency = I18nManager::format_currency(i18n_manager, amount, "USD", "en-US")
  assert_eq(en_currency, "$1,234.56")
  
  let en_currency_eur = I18nManager::format_currency(i18n_manager, amount, "EUR", "en-US")
  assert_eq(en_currency_eur, "€1,234.56")
  
  let de_currency = I18nManager::format_currency(i18n_manager, amount, "EUR", "de-DE")
  assert_eq(de_currency, "1.234,56 €")
  
  let fr_currency = I18nManager::format_currency(i18n_manager, amount, "EUR", "fr-FR")
  assert_eq(fr_currency, "1 234,56 €")
  
  let ja_currency = I18nManager::format_currency(i18n_manager, amount, "JPY", "ja-JP")
  assert_eq(ja_currency, "¥1,235") // Japanese Yen doesn't use decimal places
  
  let zh_currency = I18nManager::format_currency(i18n_manager, amount, "CNY", "zh-CN")
  assert_eq(zh_currency, "¥1,234.56")
  
  // Test unit formatting
  let distance = 1234.56
  let temperature = 23.5
  
  let en_distance = I18nManager::format_unit(i18n_manager, distance, "meter", "en-US")
  assert_eq(en_distance, "1,234.56 m")
  
  let de_distance = I18nManager::format_unit(i18n_manager, distance, "meter", "de-DE")
  assert_eq(de_distance, "1.234,56 m")
  
  let en_temperature = I18nManager::format_unit(i18n_manager, temperature, "celsius", "en-US")
  assert_eq(en_temperature, "23.5°C")
  
  let en_temperature_f = I18nManager::format_unit(i18n_manager, temperature, "fahrenheit", "en-US")
  assert_eq(en_temperature_f, "74.3°F")
  
  let ja_temperature = I18nManager::format_unit(i18n_manager, temperature, "celsius", "ja-JP")
  assert_eq(ja_temperature, "23.5°C")
  
  // Test localized units in telemetry
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "i18n_units_meter")
  let gauge = Meter::create_gauge(meter, "localized_gauge", Some("Localized gauge"), Some("value"))
  
  // Record gauge with localized units
  let en_attrs = Attributes::new()
  Attributes::set(en_attrs, "unit", StringValue("m"))
  Attributes::set(en_attrs, "unit.display", StringValue(I18nManager::format_unit(i18n_manager, distance, "meter", "en-US")))
  Attributes::set(en_attrs, "locale", StringValue("en-US"))
  Gauge::set(gauge, distance, None, Some(en_attrs))
  
  let de_attrs = Attributes::new()
  Attributes::set(de_attrs, "unit", StringValue("m"))
  Attributes::set(de_attrs, "unit.display", StringValue(I18nManager::format_unit(i18n_manager, distance, "meter", "de-DE")))
  Attributes::set(de_attrs, "locale", StringValue("de-DE"))
  Gauge::set(gauge, distance, None, Some(de_attrs))
  
  // Verify localized unit displays
  let en_unit_display = Attributes::get(en_attrs, "unit.display")
  match en_unit_display {
    Some(StringValue(display)) => assert_eq(display, "1,234.56 m")
    None => assert_true(false)
  }
  
  let de_unit_display = Attributes::get(de_attrs, "unit.display")
  match de_unit_display {
    Some(StringValue(display)) => assert_eq(display, "1.234,56 m")
    None => assert_true(false)
  }
}

// Test 6: Time Zone and Calendar Localization
test "time zone and calendar localization" {
  let i18n_manager = I18nManager::new()
  
  // Test time zone handling
  let timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  let utc_time = I18nManager::format_datetime_with_timezone(i18n_manager, timestamp, "UTC", "en-US", "medium")
  assert_eq(utc_time, "Jan 1, 2021, 12:00:00 AM")
  
  let est_time = I18nManager::format_datetime_with_timezone(i18n_manager, timestamp, "America/New_York", "en-US", "medium")
  assert_eq(est_time, "Dec 31, 2020, 7:00:00 PM")
  
  let jst_time = I18nManager::format_datetime_with_timezone(i18n_manager, timestamp, "Asia/Tokyo", "ja-JP", "medium")
  assert_eq(jst_time, "2021/01/01 9:00:00")
  
  let cst_time = I18nManager::format_datetime_with_timezone(i18n_manager, timestamp, "Asia/Shanghai", "zh-CN", "medium")
  assert_eq(cst_time, "2021年1月1日 上午8:00:00")
  
  // Test calendar system localization
  let gregorian_date = I18nManager::format_date_with_calendar(i18n_manager, timestamp, "gregorian", "en-US")
  assert_eq(gregorian_date, "1/1/2021")
  
  let chinese_date = I18nManager::format_date_with_calendar(i18n_manager, timestamp, "chinese", "zh-CN")
  // Chinese calendar date would be different (e.g., "庚子年冬月十八")
  assert_true(String::length(chinese_date) > 0)
  
  let islamic_date = I18nManager::format_date_with_calendar(i18n_manager, timestamp, "islamic", "ar-SA")
  // Islamic calendar date would be different
  assert_true(String::length(islamic_date) > 0)
  
  // Test localized time spans/durations
  let duration_ms = 3665000L // 1 hour, 1 minute, 5 seconds
  
  let en_duration = I18nManager::format_duration(i18n_manager, duration_ms, "en-US")
  assert_eq(en_duration, "1h 1m 5s")
  
  let de_duration = I18nManager::format_duration(i18n_manager, duration_ms, "de-DE")
  assert_eq(de_duration, "1 Std 1 Min 5 s")
  
  let ja_duration = I18nManager::format_duration(i18n_manager, duration_ms, "ja-JP")
  assert_eq(ja_duration, "1時間1分5秒")
  
  let zh_duration = I18nManager::format_duration(i18n_manager, duration_ms, "zh-CN")
  assert_eq(zh_duration, "1小时1分钟5秒")
  
  // Test localized time spans in telemetry
  let trace_id = "9af7651916cd43dd8448eb211c8031a5"
  let span_id = "v7ad6b7169203345"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "timezone_service")
  let span = Span::new("timezone_operation", Server, span_ctx)
  
  // Add localized duration
  let en_duration_attr = I18nManager::format_duration(i18n_manager, duration_ms, "en-US")
  let ja_duration_attr = I18nManager::format_duration(i18n_manager, duration_ms, "ja-JP")
  
  Span::set_attribute(span, "duration.en", StringValue(en_duration_attr))
  Span::set_attribute(span, "duration.ja", StringValue(ja_duration_attr))
  Span::set_attribute(span, "duration.ms", IntValue(duration_ms))
  
  // Verify localized duration attributes
  let en_duration_value = Span::get_attribute(span, "duration.en")
  match en_duration_value {
    Some(StringValue(duration)) => assert_eq(duration, "1h 1m 5s")
    None => assert_true(false)
  }
  
  let ja_duration_value = Span::get_attribute(span, "duration.ja")
  match ja_duration_value {
    Some(StringValue(duration)) => assert_eq(duration, "1時間1分5秒")
    None => assert_true(false)
  }
  
  Span::end(span)
}