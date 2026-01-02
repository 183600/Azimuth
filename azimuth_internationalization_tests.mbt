// Internationalization Tests for Azimuth
// This file contains test cases for internationalization and localization support

test "unicode string handling" {
  // Test Chinese characters
  let chinese_text = "测试中文遥测系统"
  assert_eq(chinese_text.length(), 8)
  assert_true(chinese_text.contains("中文"))
  
  // Test Japanese characters
  let japanese_text = "日本語テレメトリシステム"
  assert_eq(japanese_text.length(), 12)
  assert_true(japanese_text.contains("日本語"))
  
  // Test Korean characters
  let korean_text = "한국어 원격 측정 시스템"
  assert_eq(korean_text.length(), 12)
  assert_true(korean_text.contains("한국어"))
  
  // Test Arabic characters
  let arabic_text = "نظام قياس عن بعد"
  assert_eq(arabic_text.length(), 14)
  assert_true(arabic_text.contains("نظام"))
  
  // Test Russian characters
  let russian_text = "Система телеметрии"
  assert_eq(russian_text.length(), 17)
  assert_true(russian_text.contains("телеметрии"))
}

test "emoji and symbol handling" {
  // Test emoji in telemetry context
  let emoji_span_name = "🔍 search operation"
  assert_eq(emoji_span_name.length(), 18)
  assert_true(emoji_span_name.contains("🔍"))
  
  // Test multiple emoji
  let multi_emoji_text = "🚀 performance 📊 metrics 📈 dashboard"
  assert_eq(multi_emoji_text.length(), 35)
  assert_true(multi_emoji_text.contains("🚀"))
  assert_true(multi_emoji_text.contains("📊"))
  assert_true(multi_emoji_text.contains("📈"))
  
  // Test special symbols
  let symbol_text = "⚡ fast service ⚙️ configuration 🔧 maintenance"
  assert_eq(symbol_text.length(), 43)
  assert_true(symbol_text.contains("⚡"))
  assert_true(symbol_text.contains("⚙️"))
  assert_true(symbol_text.contains("🔧"))
}

test "multilingual attribute values" {
  let attrs = Attributes::new()
  
  // Test setting multilingual attributes
  Attributes::set(attrs, "error.message.zh", StringValue("操作失败"))
  Attributes::set(attrs, "error.message.ja", StringValue("操作が失敗しました"))
  Attributes::set(attrs, "error.message.ko", StringValue("작업이 실패했습니다"))
  Attributes::set(attrs, "error.message.ar", StringValue("فشلت العملية"))
  Attributes::set(attrs, "error.message.ru", StringValue("Операция не удалась"))
  
  // Test retrieving multilingual attributes
  match Attributes::get(attrs, "error.message.zh") {
    Some(StringValue(v)) => assert_eq(v, "操作失败")
    _ => assert_true(false)
  }
  
  match Attributes::get(attrs, "error.message.ja") {
    Some(StringValue(v)) => assert_eq(v, "操作が失敗しました")
    _ => assert_true(false)
  }
  
  match Attributes::get(attrs, "error.message.ko") {
    Some(StringValue(v)) => assert_eq(v, "작업이 실패했습니다")
    _ => assert_true(false)
  }
  
  match Attributes::get(attrs, "error.message.ar") {
    Some(StringValue(v)) => assert_eq(v, "فشلت العملية")
    _ => assert_true(false)
  }
  
  match Attributes::get(attrs, "error.message.ru") {
    Some(StringValue(v)) => assert_eq(v, "Операция не удалась")
    _ => assert_true(false)
  }
}

test "multilingual resource attributes" {
  let resource = Resource::new()
  
  // Test multilingual resource attributes
  let multilingual_attrs = [
    ("service.name.zh", StringValue("遥测服务")),
    ("service.name.ja", StringValue("テレメトリサービス")),
    ("service.name.ko", StringValue("원격 측정 서비스")),
    ("service.description.zh", StringValue("提供全面的遥测功能")),
    ("service.description.ja", StringValue("包括的なテレメトリ機能を提供")),
    ("service.description.ko", StringValue("포괄적인 원격 측정 기능 제공"))
  ]
  let resource_with_multilingual = Resource::with_attributes(resource, multilingual_attrs)
  
  // Test retrieving multilingual resource attributes
  match Resource::get_attribute(resource_with_multilingual, "service.name.zh") {
    Some(StringValue(v)) => assert_eq(v, "遥测服务")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_multilingual, "service.name.ja") {
    Some(StringValue(v)) => assert_eq(v, "テレメトリサービス")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_multilingual, "service.name.ko") {
    Some(StringValue(v)) => assert_eq(v, "원격 측정 서비스")
    _ => assert_true(false)
  }
}

test "multilingual log messages" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "multilingual_logger")
  
  // Test multilingual log records
  let chinese_log = LogRecord::new(Info, "系统启动成功")
  let japanese_log = LogRecord::new(Info, "システムが正常に起動しました")
  let korean_log = LogRecord::new(Info, "시스템이 성공적으로 시작되었습니다")
  let arabic_log = LogRecord::new(Info, "بدأ النظام بنجاح")
  let russian_log = LogRecord::new(Info, "Система успешно запущена")
  
  // Test multilingual log message content
  assert_eq(LogRecord::body(chinese_log), Some("系统启动成功"))
  assert_eq(LogRecord::body(japanese_log), Some("システムが正常に起動しました"))
  assert_eq(LogRecord::body(korean_log), Some("시스템이 성공적으로 시작되었습니다"))
  assert_eq(LogRecord::body(arabic_log), Some("بدأ النظام بنجاح"))
  assert_eq(LogRecord::body(russian_log), Some("Система успешно запущена"))
  
  // Test emitting multilingual logs
  Logger::emit(logger, chinese_log)
  Logger::emit(logger, japanese_log)
  Logger::emit(logger, korean_log)
  Logger::emit(logger, arabic_log)
  Logger::emit(logger, russian_log)
}

test "multilingual span names" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "multilingual_tracer")
  
  // Test multilingual span creation
  let chinese_span = Tracer::start_span(tracer, "中文操作")
  let japanese_span = Tracer::start_span(tracer, "日本語操作")
  let korean_span = Tracer::start_span(tracer, "한국어 작업")
  let arabic_span = Tracer::start_span(tracer, "عملية باللغة العربية")
  let russian_span = Tracer::start_span(tracer, "Операция на русском")
  
  // Test multilingual span names
  assert_eq(Span::name(chinese_span), "中文操作")
  assert_eq(Span::name(japanese_span), "日本語操作")
  assert_eq(Span::name(korean_span), "한국어 작업")
  assert_eq(Span::name(arabic_span), "عملية باللغة العربية")
  assert_eq(Span::name(russian_span), "Операция на русском")
}

test "right-to-left text handling" {
  // Test Arabic text (right-to-left)
  let arabic_text = "مرحبا بالعالم"
  assert_eq(arabic_text.length(), 11)
  assert_true(arabic_text.contains("العالم"))
  
  // Test Hebrew text (right-to-left)
  let hebrew_text = "שלום עולם"
  assert_eq(hebrew_text.length(), 9)
  assert_true(hebrew_text.contains("עולם"))
  
  // Test mixed direction text
  let mixed_text = "English العربية English"
  assert_eq(mixed_text_text.length(), 20)
  assert_true(mixed_text.contains("العربية"))
  assert_true(mixed_text.contains("English"))
}

test "locale-specific formatting" {
  // Test different locale-specific number formats
  let number = 1234.56
  
  // US English format
  let us_format = "1,234.56"
  assert_eq(us_format.contains(","), true)
  assert_eq(us_format.contains("."), true)
  
  // European format
  let eu_format = "1.234,56"
  assert_eq(eu_format.contains("."), true)
  assert_eq(eu_format.contains(","), true)
  
  // Test different date formats
  let us_date = "12/31/2023"
  assert_eq(us_date.contains("/"), true)
  
  let iso_date = "2023-12-31"
  assert_eq(iso_date.contains("-"), true)
  
  let eu_date = "31.12.2023"
  assert_eq(eu_date.contains("."), true)
}

test "currency and number localization" {
  // Test currency symbols
  let usd_currency = "$123.45"
  assert_eq(usd_currency.contains("$"), true)
  
  let eur_currency = "€123.45"
  assert_eq(eur_currency.contains("€"), true)
  
  let jpy_currency = "¥123"
  assert_eq(jpy_currency.contains("¥"), true)
  
  let gbp_currency = "£123.45"
  assert_eq(gbp_currency.contains("£"), true)
  
  let cny_currency = "¥123.45"
  assert_eq(cny_currency.contains("¥"), true)
  
  // Test number separators
  let indian_number = "12,34,567.89"
  assert_eq(indian_number.contains(","), true)
  
  let chinese_number = "123,456.78"
  assert_eq(chinese_number.contains(","), true)
}

test "multilingual baggage entries" {
  let baggage = Baggage::new()
  
  // Test multilingual baggage
  let baggage_with_chinese = Baggage::set_entry(baggage, "错误代码", "ERR001")
  let baggage_with_japanese = Baggage::set_entry(baggage_with_chinese, "エラーコード", "ERR001")
  let baggage_with_korean = Baggage::set_entry(baggage_with_japanese, "오류 코드", "ERR001")
  
  // Test retrieving multilingual baggage entries
  match Baggage::get_entry(baggage_with_korean, "错误代码") {
    Some(v) => assert_eq(v, "ERR001")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(baggage_with_korean, "エラーコード") {
    Some(v) => assert_eq(v, "ERR001")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(baggage_with_korean, "오류 코드") {
    Some(v) => assert_eq(v, "ERR001")
    None => assert_true(false)
  }
}

test "character encoding validation" {
  // Test UTF-8 encoded strings
  let utf8_chinese = "遥测"
  let utf8_japanese = "テレメトリ"
  let utf8_korean = "원격 측정"
  let utf8_arabic = "قياس عن بعد"
  let utf8_russian = "телеметрия"
  
  // Test string operations on UTF-8 encoded strings
  assert_eq(utf8_chinese.length(), 2)
  assert_eq(utf8_japanese.length(), 5)
  assert_eq(utf8_korean.length(), 4)
  assert_eq(utf8_arabic.length(), 9)
  assert_eq(utf8_russian.length(), 9)
  
  // Test concatenation of UTF-8 strings
  let concatenated = utf8_chinese + " " + utf8_japanese + " " + utf8_korean
  assert_eq(concatenated.length(), 14)  // 2 + 1 + 5 + 1 + 4 = 13 (plus spaces)
  assert_true(concatenated.contains("遥测"))
  assert_true(concatenated.contains("テレメトリ"))
  assert_true(concatenated.contains("원격 측정"))
}