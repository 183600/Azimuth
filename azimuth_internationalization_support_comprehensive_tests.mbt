// Internationalization Support Tests
// This file contains comprehensive test cases for internationalization support

// Test 1: Multi-Language String Handling
test "multi-language string handling" {
  let i18n_manager = I18nManager::new()
  
  // Test string handling with different languages
  let test_strings = [
    ("en", "Hello, World!"),
    ("zh", "你好，世界！"),
    ("ja", "こんにちは、世界！"),
    ("ko", "안녕하세요, 세계!"),
    ("es", "¡Hola, mundo!"),
    ("fr", "Bonjour, le monde!"),
    ("de", "Hallo, Welt!"),
    ("ru", "Привет, мир!"),
    ("ar", "مرحبا بالعالم!"),
    ("hi", "नमस्ते दुनिया!"),
    ("th", "สวัสดีชาวโลก!"),
    ("he", "שלום עולם!")
  ]
  
  // Add localized strings
  for (locale, string) in test_strings {
    I18nManager::addLocalizedString(i18n_manager, "greeting", locale, string)
  }
  
  // Test retrieval in different locales
  for (locale, expected_string) in test_strings {
    I18nManager::setLocale(i18n_manager, locale)
    let retrieved_string = I18nManager::getLocalizedString(i18n_manager, "greeting")
    
    match retrieved_string {
      Some(s) => assert_eq(s, expected_string),
      None => assert_true(false)
    }
  }
  
  // Test fallback to default locale
  I18nManager::setLocale(i18n_manager, "invalid_locale")
  let fallback_string = I18nManager::getLocalizedString(i18n_manager, "greeting")
  
  match fallback_string {
    Some(s) => assert_eq(s, "Hello, World!"), // Should fallback to English
    None => assert_true(false)
  }
}

// Test 2: Right-to-Left Language Support
test "right-to-left language support" {
  let i18n_manager = I18nManager::new()
  
  // Add RTL language strings
  I18nManager::addLocalizedString(i18n_manager, "welcome", "ar", "مرحبا بكم")
  I18nManager::addLocalizedString(i18n_manager, "goodbye", "he", "להתראות")
  I18nManager::addLocalizedString(i18n_manager, "error", "fa", "خطا")
  
  // Test RTL detection
  I18nManager::setLocale(i18n_manager, "ar")
  assert_true(I18nManager::isRTL(i18n_manager))
  
  I18nManager::setLocale(i18n_manager, "he")
  assert_true(I18nManager::isRTL(i18n_manager))
  
  I18nManager::setLocale(i18n_manager, "fa")
  assert_true(I18nManager::isRTL(i18n_manager))
  
  // Test LTR detection
  I18nManager::setLocale(i18n_manager, "en")
  assert_false(I18nManager::isRTL(i18n_manager))
  
  I18nManager::setLocale(i18n_manager, "zh")
  assert_false(I18nManager::isRTL(i18n_manager))
  
  // Test text direction handling
  I18nManager::setLocale(i18n_manager, "ar")
  let rtl_text = I18nManager::formatWithDirection(i18n_manager, "مرحبا بكم")
  assert_true(rtl_text.contains("dir=\"rtl\""))
  
  I18nManager::setLocale(i18n_manager, "en")
  let ltr_text = I18nManager::formatWithDirection(i18n_manager, "Welcome")
  assert_true(ltr_text.contains("dir=\"ltr\""))
}

// Test 3: Number Formatting with Locales
test "number formatting with locales" {
  let i18n_manager = I18nManager::new()
  
  let test_number = 1234567.89
  
  // Test number formatting in different locales
  let locale_formats = [
    ("en", "1,234,567.89"),
    ("de", "1.234.567,89"),
    ("fr", "1 234 567,89"),
    ("zh", "1,234,567.89"),
    ("ja", "1,234,567.89"),
    ("ru", "1 234 567,89"),
    ("ar", "1,234,567.89"),
    ("hi", "12,34,567.89")
  ]
  
  for (locale, expected_format) in locale_formats {
    I18nManager::setLocale(i18n_manager, locale)
    let formatted_number = I18nManager::formatNumber(i18n_manager, test_number)
    assert_eq(formatted_number, expected_format)
  }
  
  // Test percentage formatting
  I18nManager::setLocale(i18n_manager, "en")
  let percentage_en = I18nManager::formatPercentage(i18n_manager, 0.75)
  assert_eq(percentage_en, "75%")
  
  I18nManager::setLocale(i18n_manager, "fr")
  let percentage_fr = I18nManager::formatPercentage(i18n_manager, 0.75)
  assert_eq(percentage_fr, "75 %")
  
  // Test currency formatting
  I18nManager::setLocale(i18n_manager, "en")
  let currency_en = I18nManager::formatCurrency(i18n_manager, 1234.56, "USD")
  assert_eq(currency_en, "$1,234.56")
  
  I18nManager::setLocale(i18n_manager, "de")
  let currency_de = I18nManager::formatCurrency(i18n_manager, 1234.56, "EUR")
  assert_eq(currency_de, "1.234,56 €")
}

// Test 4: Date and Time Formatting with Locales
test "date and time formatting with locales" {
  let i18n_manager = I18nManager::new()
  
  // Test date: January 15, 2023 at 14:30:45
  let test_timestamp = 1673789445L
  
  // Test date formatting in different locales
  let locale_date_formats = [
    ("en", "1/15/2023"),
    ("en-GB", "15/01/2023"),
    ("de", "15.1.2023"),
    ("fr", "15/01/2023"),
    ("zh", "2023/1/15"),
    ("ja", "2023/01/15"),
    ("ru", "15.01.2023"),
    ("ar", "١٥/١/٢٠٢٣")
  ]
  
  for (locale, expected_format) in locale_date_formats {
    I18nManager::setLocale(i18n_manager, locale)
    let formatted_date = I18nManager::formatDate(i18n_manager, test_timestamp)
    assert_eq(formatted_date, expected_format)
  }
  
  // Test time formatting
  I18nManager::setLocale(i18n_manager, "en")
  let time_en = I18nManager::formatTime(i18n_manager, test_timestamp)
  assert_eq(time_en, "2:30:45 PM")
  
  I18nManager::setLocale(i18n_manager, "de")
  let time_de = I18nManager::formatTime(i18n_manager, test_timestamp)
  assert_eq(time_de, "14:30:45")
  
  // Test datetime formatting
  I18nManager::setLocale(i18n_manager, "en")
  let datetime_en = I18nManager::formatDateTime(i18n_manager, test_timestamp)
  assert_eq(datetime_en, "1/15/2023, 2:30:45 PM")
  
  I18nManager::setLocale(i18n_manager, "fr")
  let datetime_fr = I18nManager::formatDateTime(i18n_manager, test_timestamp)
  assert_eq(datetime_fr, "15/01/2023 à 14:30:45")
}

// Test 5: Telemetry Attribute Internationalization
test "telemetry attribute internationalization" {
  let i18n_manager = I18nManager::new()
  
  // Add localized attribute names and descriptions
  I18nManager::addLocalizedString(i18n_manager, "attr.response_time", "en", "Response Time")
  I18nManager::addLocalizedString(i18n_manager, "attr.response_time", "zh", "响应时间")
  I18nManager::addLocalizedString(i18n_manager, "attr.response_time", "ja", "応答時間")
  
  I18nManager::addLocalizedString(i18n_manager, "attr.error_rate", "en", "Error Rate")
  I18nManager::addLocalizedString(i18n_manager, "attr.error_rate", "zh", "错误率")
  I18nManager::addLocalizedString(i18n_manager, "attr.error_rate", "ja", "エラー率")
  
  // Create span with internationalized attributes
  let span_ctx = SpanContext::new("test_trace", "test_span", true, "")
  let span = Span::new("test_operation", Internal, span_ctx)
  
  // Add attributes with localized keys
  Span::add_attribute(span, "attr.response_time", FloatValue(150.5))
  Span::add_attribute(span, "attr.error_rate", FloatValue(0.02))
  
  // Test attribute name localization
  I18nManager::setLocale(i18n_manager, "en")
  let localized_attrs_en = I18nManager::localizeAttributes(i18n_manager, Span::attributes(span))
  
  match localized_attrs_en.get("Response Time") {
    Some(FloatValue(150.5)) => assert_true(true),
    _ => assert_true(false)
  }
  
  match localized_attrs_en.get("Error Rate") {
    Some(FloatValue(0.02)) => assert_true(true),
    _ => assert_true(false)
  }
  
  // Test in Chinese
  I18nManager::setLocale(i18n_manager, "zh")
  let localized_attrs_zh = I18nManager::localizeAttributes(i18n_manager, Span::attributes(span))
  
  match localized_attrs_zh.get("响应时间") {
    Some(FloatValue(150.5)) => assert_true(true),
    _ => assert_true(false)
  }
  
  match localized_attrs_zh.get("错误率") {
    Some(FloatValue(0.02)) => assert_true(true),
    _ => assert_true(false)
  }
}

// Test 6: Internationalized Error Messages
test "internationalized error messages" {
  let i18n_manager = I18nManager::new()
  
  // Add localized error messages
  let error_messages = [
    ("en", "error.connection_failed", "Connection failed"),
    ("zh", "error.connection_failed", "连接失败"),
    ("ja", "error.connection_failed", "接続に失敗しました"),
    ("es", "error.connection_failed", "Conexión fallida"),
    ("fr", "error.connection_failed", "Connexion échouée"),
    
    ("en", "error.timeout", "Request timeout"),
    ("zh", "error.timeout", "请求超时"),
    ("ja", "error.timeout", "リクエストがタイムアウトしました"),
    ("es", "error.timeout", "Tiempo de espera agotado"),
    ("fr", "error.timeout", "Délai d'attente dépassé")
  ]
  
  for (locale, key, message) in error_messages {
    I18nManager::addLocalizedString(i18n_manager, key, locale, message)
  }
  
  // Test error message localization
  let locales = ["en", "zh", "ja", "es", "fr"]
  let expected_messages = [
    "Connection failed",
    "连接失败",
    "接続に失敗しました",
    "Conexión fallida",
    "Connexion échouée"
  ]
  
  for i in 0..5 {
    I18nManager::setLocale(i18n_manager, locales[i])
    let localized_error = I18nManager::getErrorMessage(i18n_manager, "error.connection_failed")
    
    match localized_error {
      Some(message) => assert_eq(message, expected_messages[i]),
      None => assert_true(false)
    }
  }
  
  // Test error message with parameters
  I18nManager::setLocale(i18n_manager, "en")
  let param_error_en = I18nManager::formatErrorMessage(i18n_manager, "error.timeout", ["3000ms"])
  assert_eq(param_error_en, "Request timeout after 3000ms")
  
  I18nManager::setLocale(i18n_manager, "zh")
  let param_error_zh = I18nManager::formatErrorMessage(i18n_manager, "error.timeout", ["3000ms"])
  assert_eq(param_error_zh, "请求在3000ms后超时")
}

// Test 7: Internationalized Log Messages
test "internationalized log messages" {
  let i18n_manager = I18nManager::new()
  
  // Add localized log messages
  I18nManager::addLocalizedString(i18n_manager, "log.service_started", "en", "Service started")
  I18nManager::addLocalizedString(i18n_manager, "log.service_started", "zh", "服务已启动")
  I18nManager::addLocalizedString(i18n_manager, "log.service_started", "ja", "サービスが開始されました")
  
  I18nManager::addLocalizedString(i18n_manager, "log.request_processed", "en", "Request processed")
  I18nManager::addLocalizedString(i18n_manager, "log.request_processed", "zh", "请求已处理")
  I18nManager::addLocalizedString(i18n_manager, "log.request_processed", "ja", "リクエストが処理されました")
  
  // Create logger with internationalization support
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "i18n_logger")
  
  // Test log message localization
  I18nManager::setLocale(i18n_manager, "en")
  let log_en = LogRecord::new_with_i18n(Info, "log.service_started", i18n_manager)
  Logger::emit(logger, log_en)
  
  I18nManager::setLocale(i18n_manager, "zh")
  let log_zh = LogRecord::new_with_i18n(Info, "log.service_started", i18n_manager)
  Logger::emit(logger, log_zh)
  
  I18nManager::setLocale(i18n_manager, "ja")
  let log_ja = LogRecord::new_with_i18n(Info, "log.service_started", i18n_manager)
  Logger::emit(logger, log_ja)
  
  // Test log message with parameters
  I18nManager::setLocale(i18n_manager, "en")
  let param_log_en = LogRecord::new_with_i18n_params(Info, "log.request_processed", 
    [IntValue(123), IntValue(45)], i18n_manager)
  Logger::emit(logger, param_log_en)
  
  I18nManager::setLocale(i18n_manager, "zh")
  let param_log_zh = LogRecord::new_with_i18n_params(Info, "log.request_processed", 
    [IntValue(123), IntValue(45)], i18n_manager)
  Logger::emit(logger, param_log_zh)
}

// Test 8: Internationalized Metrics Names and Descriptions
test "internationalized metrics names and descriptions" {
  let i18n_manager = I18nManager::new()
  
  // Add localized metric names and descriptions
  I18nManager::addLocalizedString(i18n_manager, "metric.http_requests", "en", "HTTP Requests")
  I18nManager::addLocalizedString(i18n_manager, "metric.http_requests", "zh", "HTTP请求")
  I18nManager::addLocalizedString(i18n_manager, "metric.http_requests", "ja", "HTTPリクエスト")
  
  I18nManager::addLocalizedString(i18n_manager, "metric.http_requests.desc", "en", "Total number of HTTP requests")
  I18nManager::addLocalizedString(i18n_manager, "metric.http_requests.desc", "zh", "HTTP请求总数")
  I18nManager::addLocalizedString(i18n_manager, "metric.http_requests.desc", "ja", "HTTPリクエストの総数")
  
  // Create metrics with internationalization support
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "i18n_meter")
  
  // Create metrics with localized names and descriptions
  I18nManager::setLocale(i18n_manager, "en")
  let counter_en = Meter::create_counter_with_i18n(meter, "metric.http_requests", i18n_manager)
  
  I18nManager::setLocale(i18n_manager, "zh")
  let counter_zh = Meter::create_counter_with_i18n(meter, "metric.http_requests", i18n_manager)
  
  I18nManager::setLocale(i18n_manager, "ja")
  let counter_ja = Meter::create_counter_with_i18n(meter, "metric.http_requests", i18n_manager)
  
  // Test metric name localization
  I18nManager::setLocale(i18n_manager, "en")
  let metric_name_en = I18nManager::getMetricName(i18n_manager, "metric.http_requests")
  assert_eq(metric_name_en, "HTTP Requests")
  
  let metric_desc_en = I18nManager::getMetricDescription(i18n_manager, "metric.http_requests")
  assert_eq(metric_desc_en, "Total number of HTTP requests")
  
  I18nManager::setLocale(i18n_manager, "zh")
  let metric_name_zh = I18nManager::getMetricName(i18n_manager, "metric.http_requests")
  assert_eq(metric_name_zh, "HTTP请求")
  
  let metric_desc_zh = I18nManager::getMetricDescription(i18n_manager, "metric.http_requests")
  assert_eq(metric_desc_zh, "HTTP请求总数")
  
  I18nManager::setLocale(i18n_manager, "ja")
  let metric_name_ja = I18nManager::getMetricName(i18n_manager, "metric.http_requests")
  assert_eq(metric_name_ja, "HTTPリクエスト")
  
  let metric_desc_ja = I18nManager::getMetricDescription(i18n_manager, "metric.http_requests")
  assert_eq(metric_desc_ja, "HTTPリクエストの総数")
}

// Test 9: Character Encoding Handling
test "character encoding handling" {
  let i18n_manager = I18nManager::new()
  
  // Test various character encodings
  let unicode_strings = [
    ("en", "Hello World"),
    ("zh", "你好世界"),
    ("ja", "こんにちは世界"),
    ("ko", "안녕하세요 세계"),
    ("ar", "مرحبا بالعالم"),
    ("ru", "Привет мир"),
    ("hi", "नमस्ते दुनिया"),
    ("th", "สวัสดีชาวโลก"),
    ("he", "שלום עולם"),
    ("emoji", "Hello 🌍 World 🚀")
  ]
  
  for (locale, string) in unicode_strings {
    I18nManager::addLocalizedString(i18n_manager, "greeting", locale, string)
  }
  
  // Test UTF-8 encoding/decoding
  for (locale, expected_string) in unicode_strings {
    I18nManager::setLocale(i18n_manager, locale)
    let retrieved_string = I18nManager::getLocalizedString(i18n_manager, "greeting")
    
    match retrieved_string {
      Some(s) => {
        // Verify encoding is preserved
        assert_eq(s, expected_string)
        
        // Test UTF-8 byte length
        let byte_length = I18nManager::getUTF8ByteLength(i18n_manager, s)
        assert_true(byte_length > 0)
      }
      None => assert_true(false)
    }
  }
  
  // Test character counting vs byte counting
  I18nManager::setLocale(i18n_manager, "zh")
  let chinese_string = I18nManager::getLocalizedString(i18n_manager, "greeting").unwrap()
  let char_count = I18nManager::getCharacterCount(i18n_manager, chinese_string)
  let byte_count = I18nManager::getUTF8ByteLength(i18n_manager, chinese_string)
  
  // Chinese characters take multiple bytes in UTF-8
  assert_eq(char_count, 4) // "你好世界" has 4 characters
  assert_eq(byte_count, 12) // Each Chinese character takes 3 bytes in UTF-8
  
  // Test normalization
  let test_string = "café"
  let normalized = I18nManager::normalizeUTF8(i18n_manager, test_string)
  assert_eq(normalized, "café") // Should be properly normalized
}

// Test 10: Locale-Aware Telemetry Dashboard
test "locale-aware telemetry dashboard" {
  let i18n_manager = I18nManager::new()
  
  // Add dashboard UI strings
  let dashboard_strings = [
    ("en", "dashboard.title", "Telemetry Dashboard"),
    ("zh", "dashboard.title", "遥测仪表板"),
    ("ja", "dashboard.title", "テレメトリーダッシュボード"),
    
    ("en", "dashboard.metrics", "Metrics"),
    ("zh", "dashboard.metrics", "指标"),
    ("ja", "dashboard.metrics", "メトリクス"),
    
    ("en", "dashboard.traces", "Traces"),
    ("zh", "dashboard.traces", "追踪"),
    ("ja", "dashboard.traces", "トレース"),
    
    ("en", "dashboard.logs", "Logs"),
    ("zh", "dashboard.logs", "日志"),
    ("ja", "dashboard.logs", "ログ")
  ]
  
  for (locale, key, value) in dashboard_strings {
    I18nManager::addLocalizedString(i18n_manager, key, locale, value)
  }
  
  // Create dashboard with internationalization
  let dashboard = TelemetryDashboard::new_with_i18n(i18n_manager)
  
  // Test dashboard in different locales
  let locales = ["en", "zh", "ja"]
  let expected_titles = ["Telemetry Dashboard", "遥测仪表板", "テレメトリーダッシュボード"]
  
  for i in 0..3 {
    I18nManager::setLocale(i18n_manager, locales[i])
    TelemetryDashboard::setLocale(dashboard, locales[i])
    
    let title = TelemetryDashboard::getTitle(dashboard)
    assert_eq(title, expected_titles[i])
    
    let metrics_label = TelemetryDashboard::getMetricsLabel(dashboard)
    let traces_label = TelemetryDashboard::getTracesLabel(dashboard)
    let logs_label = TelemetryDashboard::getLogsLabel(dashboard)
    
    match locales[i] {
      "en" => {
        assert_eq(metrics_label, "Metrics")
        assert_eq(traces_label, "Traces")
        assert_eq(logs_label, "Logs")
      }
      "zh" => {
        assert_eq(metrics_label, "指标")
        assert_eq(traces_label, "追踪")
        assert_eq(logs_label, "日志")
      }
      "ja" => {
        assert_eq(metrics_label, "メトリクス")
        assert_eq(traces_label, "トレース")
        assert_eq(logs_label, "ログ")
      }
      _ => assert_true(false)
    }
  }
  
  // Test RTL layout adjustment
  I18nManager::setLocale(i18n_manager, "ar")
  TelemetryDashboard::setLocale(dashboard, "ar")
  
  assert_true(TelemetryDashboard::isRTLLayout(dashboard))
  assert_eq(TelemetryDashboard::getTextDirection(dashboard), "rtl")
  
  I18nManager::setLocale(i18n_manager, "en")
  TelemetryDashboard::setLocale(dashboard, "en")
  
  assert_false(TelemetryDashboard::isRTLLayout(dashboard))
  assert_eq(TelemetryDashboard::getTextDirection(dashboard), "ltr")
}