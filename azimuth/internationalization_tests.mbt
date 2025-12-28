// Azimuth Telemetry System - Internationalization Tests
// 国际化和本地化测试用例

test "unicode_attribute_handling" {
  // Test handling of Unicode characters in attributes
  let attrs = Attributes::new()
  
  // Test various Unicode scripts
  Attributes::set(attrs, "chinese.text", StringValue("中文测试文本"))
  Attributes::set(attrs, "japanese.text", StringValue("日本語テキスト"))
  Attributes::set(attrs, "korean.text", StringValue("한국어 텍스트"))
  Attributes::set(attrs, "arabic.text", StringValue("نص عربي"))
  Attributes::set(attrs, "hebrew.text", StringValue("טקסט עברי"))
  Attributes::set(attrs, "russian.text", StringValue("Русский текст"))
  Attributes::set(attrs, "thai.text", StringValue("ข้อความภาษาไทย"))
  Attributes::set(attrs, "emoji.text", StringValue("🚀🌟💫✨🎯"))
  
  // Verify Unicode attributes can be retrieved correctly
  let chinese_result = Attributes::get(attrs, "chinese.text")
  match chinese_result {
    Some(StringValue(value)) => assert_eq(value, "中文测试文本")
    _ => assert_true(false)
  }
  
  let emoji_result = Attributes::get(attrs, "emoji.text")
  match emoji_result {
    Some(StringValue(value)) => assert_eq(value, "🚀🌟💫✨🎯")
    _ => assert_true(false)
  }
  
  let arabic_result = Attributes::get(attrs, "arabic.text")
  match arabic_result {
    Some(StringValue(value)) => assert_eq(value, "نص عربي")
    _ => assert_true(false)
  }
}

test "multilingual_error_messages" {
  // Test multilingual error messages in logs
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "i18n-logger")
  
  // Create error messages in different languages
  let log_record_en = LogRecord::new(Error, "Connection failed to remote server")
  let log_record_zh = LogRecord::new(Error, "连接远程服务器失败")
  let log_record_ja = LogRecord::new(Error, "リモートサーバーへの接続に失敗しました")
  let log_record_ko = LogRecord::new(Error, "원격 서버 연결 실패")
  
  Logger::emit(logger, log_record_en)
  Logger::emit(logger, log_record_zh)
  Logger::emit(logger, log_record_ja)
  Logger::emit(logger, log_record_ko)
  
  // Verify error messages
  match LogRecord::body(log_record_en) {
    Some(body) => assert_true(body.contains("Connection"))
    None => assert_true(false)
  }
  
  match LogRecord::body(log_record_zh) {
    Some(body) => assert_true(body.contains("连接"))
    None => assert_true(false)
  }
}

test "locale_specific_span_names" {
  // Test locale-specific span names
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "i18n-tracer")
  
  // Create spans with names in different languages
  let span_en = Tracer::start_span(tracer, "user.authentication")
  let span_zh = Tracer::start_span(tracer, "用户身份验证")
  let span_ja = Tracer::start_span(tracer, "ユーザー認証")
  let span_ko = Tracer::start_span(tracer, "사용자 인증")
  
  // Verify span names
  assert_eq(Span::name(span_en), "user.authentication")
  assert_eq(Span::name(span_zh), "用户身份验证")
  assert_eq(Span::name(span_ja), "ユーザー認証")
  assert_eq(Span::name(span_ko), "사용자 인증")
  
  Span::end(span_en)
  Span::end(span_zh)
  Span::end(span_ja)
  Span::end(span_ko)
}

test "rtl_text_handling" {
  // Test right-to-left text handling
  let attrs = Attributes::new()
  
  // Test RTL languages
  Attributes::set(attrs, "arabic.label", StringValue("اسم المستخدم"))
  Attributes::set(attrs, "hebrew.label", StringValue("שם משתמש"))
  Attributes::set(attrs, "persian.label", StringValue("نام کاربری"))
  Attributes::set(attrs, "urdu.label", StringValue("صارف کا نام"))
  
  // Test mixed LTR/RTL content
  Attributes::set(attrs, "mixed.text", StringValue("Hello مرحبا World"))
  Attributes::set(attrs, "arabic.with.numbers", StringValue("الرقم 123"))
  Attributes::set(attrs, "hebrew.with.english", StringValue("Test בדיקה"))
  
  // Verify RTL text is handled correctly
  let arabic_result = Attributes::get(attrs, "arabic.label")
  match arabic_result {
    Some(StringValue(value)) => assert_eq(value, "اسم المستخدم")
    _ => assert_true(false)
  }
  
  let mixed_result = Attributes::get(attrs, "mixed.text")
  match mixed_result {
    Some(StringValue(value)) => assert_eq(value, "Hello مرحبا World")
    _ => assert_true(false)
  }
}

test "localized_metric_names" {
  // Test localized metric names and descriptions
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "i18n-metrics")
  
  // Create metrics with localized names
  let counter_en = Meter::create_counter(meter, "response.time")
  let counter_zh = Meter::create_counter(meter, "响应时间")
  let counter_ja = Meter::create_counter(meter, "応答時間")
  let counter_ko = Meter::create_counter(meter, "응답 시간")
  
  // Verify metric properties
  assert_eq(counter_en.name, "response.time")
  assert_eq(counter_zh.name, "响应时间")
  assert_eq(counter_ja.name, "応答時間")
  assert_eq(counter_ko.name, "응답 시간")
  
  // Use the metrics
  Counter::add(counter_en, 100.0)
  Counter::add(counter_zh, 200.0)
  Counter::add(counter_ja, 150.0)
  Counter::add(counter_ko, 120.0)
}

test "cultural_number_formatting" {
  // Test cultural number formatting in attributes
  let attrs = Attributes::new()
  
  // Test different number formats
  Attributes::set(attrs, "western.number", StringValue("1,234.56"))
  Attributes::set(attrs, "european.number", StringValue("1.234,56"))
  Attributes::set(attrs, "arabic.number", StringValue("١٬٢٣٤٫٥٦"))
  Attributes::set(attrs, "hindi.number", StringValue("१,२३४.५६"))
  Attributes::set(attrs, "bengali.number", StringValue("১,২৩৪.৫৬"))
  
  // Test different date formats
  Attributes::set(attrs, "us.date", StringValue("12/28/2025"))
  Attributes::set(attrs, "european.date", StringValue("28/12/2025"))
  Attributes::set(attrs, "iso.date", StringValue("2025-12-28"))
  Attributes::set(attrs, "japanese.date", StringValue("2025年12月28日"))
  Attributes::set(attrs, "chinese.date", StringValue("2025年12月28日"))
  
  // Verify cultural formats are preserved
  let western_result = Attributes::get(attrs, "western.number")
  match western_result {
    Some(StringValue(value)) => assert_eq(value, "1,234.56")
    None => assert_true(false)
  }
  
  let japanese_date_result = Attributes::get(attrs, "japanese.date")
  match japanese_date_result {
    Some(StringValue(value)) => assert_eq(value, "2025年12月28日")
    None => assert_true(false)
  }
}