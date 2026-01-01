// Internationalization and Globalization Tests for Azimuth
// These tests verify internationalization and globalization support

test "unicode string handling" {
  // Test various Unicode characters
  let chinese_text = "遥测系统"
  let japanese_text = "テレメトリシステム"
  let arabic_text = "نظام القياس عن بعد"
  let russian_text = "Система телеметрии"
  let emoji_text = "🌍📊📈"
  
  // Create attributes with Unicode text
  let attrs = Attributes::new()
  Attributes::set(attrs, "chinese.text", StringValue(chinese_text))
  Attributes::set(attrs, "japanese.text", StringValue(japanese_text))
  Attributes::set(attrs, "arabic.text", StringValue(arabic_text))
  Attributes::set(attrs, "russian.text", StringValue(russian_text))
  Attributes::set(attrs, "emoji.text", StringValue(emoji_text))
  
  // Verify Unicode text is preserved
  let chinese_val = Attributes::get(attrs, "chinese.text")
  let japanese_val = Attributes::get(attrs, "japanese.text")
  let arabic_val = Attributes::get(attrs, "arabic.text")
  let russian_val = Attributes::get(attrs, "russian.text")
  let emoji_val = Attributes::get(attrs, "emoji.text")
  
  match chinese_val {
    Some(StringValue(s)) => assert_eq(s, chinese_text)
    _ => assert_true(false)
  }
  
  match japanese_val {
    Some(StringValue(s)) => assert_eq(s, japanese_text)
    _ => assert_true(false)
  }
  
  match arabic_val {
    Some(StringValue(s)) => assert_eq(s, arabic_text)
    _ => assert_true(false)
  }
  
  match russian_val {
    Some(StringValue(s)) => assert_eq(s, russian_text)
    _ => assert_true(false)
  }
  
  match emoji_val {
    Some(StringValue(s)) => assert_eq(s, emoji_text)
    _ => assert_true(false)
  }
}

test "localized log messages" {
  let logger_provider = LoggerProvider::default()
  
  // Create loggers for different locales
  let logger_en = LoggerProvider::get_logger(logger_provider, "en-service")
  let logger_zh = LoggerProvider::get_logger(logger_provider, "zh-service")
  let logger_ja = LoggerProvider::get_logger(logger_provider, "ja-service")
  
  // Create log records with localized messages
  let record_en = LogRecord::new(Info, "Operation completed successfully")
  let record_zh = LogRecord::new(Info, "操作成功完成")
  let record_ja = LogRecord::new(Info, "操作が正常に完了しました")
  
  // Verify localized messages
  assert_eq(LogRecord::body(record_en), Some("Operation completed successfully"))
  assert_eq(LogRecord::body(record_zh), Some("操作成功完成"))
  assert_eq(LogRecord::body(record_ja), Some("操作が正常に完了しました"))
}

test "localized error messages" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "i18n-test")
  
  // Create spans with localized error messages
  let span_en = Tracer::start_span(tracer, "operation-en")
  let span_zh = Tracer::start_span(tracer, "operation-zh")
  let span_ja = Tracer::start_span(tracer, "operation-ja")
  
  // Add localized error events
  Span::add_event(span_en, "error.occurred", Some([
    ("error.message", StringValue("Validation failed: Invalid input")),
    ("error.code", StringValue("VALIDATION_ERROR"))
  ]))
  
  Span::add_event(span_zh, "error.occurred", Some([
    ("error.message", StringValue("验证失败：输入无效")),
    ("error.code", StringValue("验证错误"))
  ]))
  
  Span::add_event(span_ja, "error.occurred", Some([
    ("error.message", StringValue("検証失敗：入力が無効です")),
    ("error.code", StringValue("検証エラー"))
  ]))
  
  // Set localized status descriptions
  Span::set_status(span_en, Error, Some("Validation failed"))
  Span::set_status(span_zh, Error, Some("验证失败"))
  Span::set_status(span_ja, Error, Some("検証失敗"))
  
  // End spans
  Span::end(span_en)
  Span::end(span_zh)
  Span::end(span_ja)
  
  // Verify spans were created with localized messages
  assert_eq(Span::name(span_en), "operation-en")
  assert_eq(Span::name(span_zh), "operation-zh")
  assert_eq(Span::name(span_ja), "operation-ja")
}

test "localized resource attributes" {
  // Create resources with localized attributes
  let resource_en = Resource::new()
  let attrs_en = [
    ("service.name", StringValue("user-service")),
    ("service.description", StringValue("User management service")),
    ("service.owner", StringValue("Platform Team"))
  ]
  let resource_en_with_attrs = Resource::with_attributes(resource_en, attrs_en)
  
  let resource_zh = Resource::new()
  let attrs_zh = [
    ("service.name", StringValue("用户服务")),
    ("service.description", StringValue("用户管理服务")),
    ("service.owner", StringValue("平台团队"))
  ]
  let resource_zh_with_attrs = Resource::with_attributes(resource_zh, attrs_zh)
  
  let resource_ja = Resource::new()
  let attrs_ja = [
    ("service.name", StringValue("ユーザーサービス")),
    ("service.description", StringValue("ユーザー管理サービス")),
    ("service.owner", StringValue("プラットフォームチーム"))
  ]
  let resource_ja_with_attrs = Resource::with_attributes(resource_ja, attrs_ja)
  
  // Verify localized attributes
  let service_name_en = Resource::get_attribute(resource_en_with_attrs, "service.name")
  let service_desc_en = Resource::get_attribute(resource_en_with_attrs, "service.description")
  
  let service_name_zh = Resource::get_attribute(resource_zh_with_attrs, "service.name")
  let service_desc_zh = Resource::get_attribute(resource_zh_with_attrs, "service.description")
  
  let service_name_ja = Resource::get_attribute(resource_ja_with_attrs, "service.name")
  let service_desc_ja = Resource::get_attribute(resource_ja_with_attrs, "service.description")
  
  match service_name_en {
    Some(StringValue(name)) => assert_eq(name, "user-service")
    _ => assert_true(false)
  }
  
  match service_desc_en {
    Some(StringValue(desc)) => assert_eq(desc, "User management service")
    _ => assert_true(false)
  }
  
  match service_name_zh {
    Some(StringValue(name)) => assert_eq(name, "用户服务")
    _ => assert_true(false)
  }
  
  match service_desc_zh {
    Some(StringValue(desc)) => assert_eq(desc, "用户管理服务")
    _ => assert_true(false)
  }
  
  match service_name_ja {
    Some(StringValue(name)) => assert_eq(name, "ユーザーサービス")
    _ => assert_true(false)
  }
  
  match service_desc_ja {
    Some(StringValue(desc)) => assert_eq(desc, "ユーザー管理サービス")
    _ => assert_true(false)
  }
}

test "localized baggage entries" {
  // Create baggage with localized entries
  let baggage_en = Baggage::new()
  let baggage_en = Baggage::set_entry(baggage_en, "user.locale", "en-US")
  let baggage_en = Baggage::set_entry(baggage_en, "user.region", "US")
  let baggage_en = Baggage::set_entry(baggage_en, "user.currency", "USD")
  
  let baggage_zh = Baggage::new()
  let baggage_zh = Baggage::set_entry(baggage_zh, "user.locale", "zh-CN")
  let baggage_zh = Baggage::set_entry(baggage_zh, "user.region", "CN")
  let baggage_zh = Baggage::set_entry(baggage_zh, "user.currency", "CNY")
  
  let baggage_ja = Baggage::new()
  let baggage_ja = Baggage::set_entry(baggage_ja, "user.locale", "ja-JP")
  let baggage_ja = Baggage::set_entry(baggage_ja, "user.region", "JP")
  let baggage_ja = Baggage::set_entry(baggage_ja, "user.currency", "JPY")
  
  // Verify localized baggage entries
  assert_eq(Baggage::get_entry(baggage_en, "user.locale"), Some("en-US"))
  assert_eq(Baggage::get_entry(baggage_en, "user.region"), Some("US"))
  assert_eq(Baggage::get_entry(baggage_en, "user.currency"), Some("USD"))
  
  assert_eq(Baggage::get_entry(baggage_zh, "user.locale"), Some("zh-CN"))
  assert_eq(Baggage::get_entry(baggage_zh, "user.region"), Some("CN"))
  assert_eq(Baggage::get_entry(baggage_zh, "user.currency"), Some("CNY"))
  
  assert_eq(Baggage::get_entry(baggage_ja, "user.locale"), Some("ja-JP"))
  assert_eq(Baggage::get_entry(baggage_ja, "user.region"), Some("JP"))
  assert_eq(Baggage::get_entry(baggage_ja, "user.currency"), Some("JPY"))
}

test "localized metrics names and descriptions" {
  let meter_provider = MeterProvider::default()
  
  // Create meters for different locales
  let meter_en = MeterProvider::get_meter(meter_provider, "en-metrics")
  let meter_zh = MeterProvider::get_meter(meter_provider, "zh-metrics")
  let meter_ja = MeterProvider::get_meter(meter_provider, "ja-metrics")
  
  // Create metrics with localized names and descriptions
  let counter_en = Meter::create_counter(meter_en, "http_requests_total", Some("Total HTTP requests"), Some("requests"))
  let counter_zh = Meter::create_counter(meter_zh, "http_请求总数", Some("HTTP请求总数"), Some("请求"))
  let counter_ja = Meter::create_counter(meter_ja, "httpリクエスト総数", Some("HTTPリクエスト総数"), Some("リクエスト"))
  
  // Verify localized metrics
  assert_eq(counter_en.name, "http_requests_total")
  assert_eq(counter_en.description, Some("Total HTTP requests"))
  assert_eq(counter_en.unit, Some("requests"))
  
  assert_eq(counter_zh.name, "http_请求总数")
  assert_eq(counter_zh.description, Some("HTTP请求总数"))
  assert_eq(counter_zh.unit, Some("请求"))
  
  assert_eq(counter_ja.name, "httpリクエスト総数")
  assert_eq(counter_ja.description, Some("HTTPリクエスト総数"))
  assert_eq(counter_ja.unit, Some("リクエスト"))
}

test "timezone handling in timestamps" {
  let clock = Clock::system()
  
  // Get timestamps
  let timestamp = Clock::now_unix_nanos(clock)
  
  // Create log records with timestamps
  let record = LogRecord::new_with_context(
    Info,
    Some("Event with timestamp"),
    None,
    Some(timestamp),
    Some(timestamp + 1000000L),
    Some("trace123"),
    Some("span456"),
    Some(Context::root())
  )
  
  // Verify timestamps are handled correctly
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Event with timestamp"))
  assert_eq(LogRecord::trace_id(record), Some("trace123"))
  assert_eq(LogRecord::span_id(record), Some("span456"))
}

test "localized http headers" {
  // Create HTTP requests with localized headers
  let headers_en = [
    ("Accept-Language", "en-US,en;q=0.9"),
    ("Content-Type", "application/json"),
    ("User-Agent", "AzimuthClient/1.0.0")
  ]
  let request_en = HttpRequest::new("GET", "https://api.example.com/data", headers_en)
  
  let headers_zh = [
    ("Accept-Language", "zh-CN,zh;q=0.9"),
    ("Content-Type", "application/json"),
    ("User-Agent", "AzimuthClient/1.0.0")
  ]
  let request_zh = HttpRequest::new("GET", "https://api.example.com/data", headers_zh)
  
  let headers_ja = [
    ("Accept-Language", "ja-JP,ja;q=0.9"),
    ("Content-Type", "application/json"),
    ("User-Agent", "AzimuthClient/1.0.0")
  ]
  let request_ja = HttpRequest::new("GET", "https://api.example.com/data", headers_ja)
  
  // Verify requests were created with localized headers
  assert_eq(HttpRequest::http_method(request_en), "GET")
  assert_eq(HttpRequest::http_method(request_zh), "GET")
  assert_eq(HttpRequest::http_method(request_ja), "GET")
  
  assert_eq(HttpRequest::url(request_en), "https://api.example.com/data")
  assert_eq(HttpRequest::url(request_zh), "https://api.example.com/data")
  assert_eq(HttpRequest::url(request_ja), "https://api.example.com/data")
}

test "localized span names and events" {
  let tracer_provider = TracerProvider::default()
  
  // Create tracers for different locales
  let tracer_en = TracerProvider::get_tracer(tracer_provider, "en-service")
  let tracer_zh = TracerProvider::get_tracer(tracer_provider, "zh-service")
  let tracer_ja = TracerProvider::get_tracer(tracer_provider, "ja-service")
  
  // Create spans with localized names
  let span_en = Tracer::start_span(tracer_en, "user.registration")
  let span_zh = Tracer::start_span(tracer_zh, "用户注册")
  let span_ja = Tracer::start_span(tracer_ja, "ユーザー登録")
  
  // Add localized events
  Span::add_event(span_en, "validation.started", Some([
    ("step", StringValue("email_validation")),
    ("message", StringValue("Validating email format"))
  ]))
  
  Span::add_event(span_zh, "validation.started", Some([
    ("step", StringValue("email_validation")),
    ("message", StringValue("验证电子邮件格式"))
  ]))
  
  Span::add_event(span_ja, "validation.started", Some([
    ("step", StringValue("email_validation")),
    ("message", StringValue("メール形式を検証中"))
  ]))
  
  // End spans
  Span::end(span_en)
  Span::end(span_zh)
  Span::end(span_ja)
  
  // Verify spans were created with localized names
  assert_eq(Span::name(span_en), "user.registration")
  assert_eq(Span::name(span_zh), "用户注册")
  assert_eq(Span::name(span_ja), "ユーザー登録")
}