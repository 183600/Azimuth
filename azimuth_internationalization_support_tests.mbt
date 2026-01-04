// Azimuth Telemetry System - Internationalization Support Tests
// This file contains comprehensive test cases for internationalization and localization

// Test 1: Multi-language Attribute Values
test "multi-language attribute values" {
  let attrs = Attributes::new()
  
  // Test Chinese characters
  Attributes::set(attrs, "chinese_greeting", StringValue("你好世界"))
  Attributes::set(attrs, "chinese_description", StringValue("这是一个遥测系统测试"))
  
  // Test Japanese characters
  Attributes::set(attrs, "japanese_greeting", StringValue("こんにちは世界"))
  Attributes::set(attrs, "japanese_description", StringValue("これはテレメトリシステムのテストです"))
  
  // Test Korean characters
  Attributes::set(attrs, "korean_greeting", StringValue("안녕하세요 세계"))
  Attributes::set(attrs, "korean_description", StringValue("이것은 원격 측정 시스템 테스트입니다"))
  
  // Test Arabic characters
  Attributes::set(attrs, "arabic_greeting", StringValue("مرحبا بالعالم"))
  Attributes::set(attrs, "arabic_description", StringValue("هذا اختبار نظام القياس عن بعد"))
  
  // Test Russian characters
  Attributes::set(attrs, "russian_greeting", StringValue("Привет мир"))
  Attributes::set(attrs, "russian_description", StringValue("Это тест системы телеметрии"))
  
  // Verify retrieval of multi-language attributes
  match Attributes::get(attrs, "chinese_greeting") {
    Some(StringValue(value)) => assert_eq(value, "你好世界")
    _ => assert_true(false)
  }
  
  match Attributes::get(attrs, "japanese_greeting") {
    Some(StringValue(value)) => assert_eq(value, "こんにちは世界")
    _ => assert_true(false)
  }
  
  match Attributes::get(attrs, "korean_greeting") {
    Some(StringValue(value)) => assert_eq(value, "안녕하세요 세계")
    _ => assert_true(false)
  }
  
  match Attributes::get(attrs, "arabic_greeting") {
    Some(StringValue(value)) => assert_eq(value, "مرحبا بالعالم")
    _ => assert_true(false)
  }
  
  match Attributes::get(attrs, "russian_greeting") {
    Some(StringValue(value)) => assert_eq(value, "Привет мир")
    _ => assert_true(false)
  }
}

// Test 2: Localized Error Messages
test "localized error messages" {
  let error_manager = ErrorManager::new()
  
  // Set locale to Chinese
  ErrorManager::set_locale(error_manager, "zh-CN")
  
  // Generate error with localized message
  let chinese_error = ErrorManager::create_error(error_manager, TelemetryError::InvalidSpanContext)
  match ErrorManager::get_message(chinese_error) {
    Some(message) => assert_true(message.contains("无效") || message.contains("跨度"))
    None => assert_true(false)
  }
  
  // Set locale to Japanese
  ErrorManager::set_locale(error_manager, "ja-JP")
  
  // Generate error with localized message
  let japanese_error = ErrorManager::create_error(error_manager, TelemetryError::InvalidSpanContext)
  match ErrorManager::get_message(japanese_error) {
    Some(message) => assert_true(message.contains("無効") || message.contains("スパン"))
    None => assert_true(false)
  }
  
  // Set locale to Korean
  ErrorManager::set_locale(error_manager, "ko-KR")
  
  // Generate error with localized message
  let korean_error = ErrorManager::create_error(error_manager, TelemetryError::InvalidSpanContext)
  match ErrorManager::get_message(korean_error) {
    Some(message) => assert_true(message.contains("잘못") || message.contains("스팬"))
    None => assert_true(false)
  }
  
  // Set locale to Arabic
  ErrorManager::set_locale(error_manager, "ar-SA")
  
  // Generate error with localized message
  let arabic_error = ErrorManager::create_error(error_manager, TelemetryError::InvalidSpanContext)
  match ErrorManager::get_message(arabic_error) {
    Some(message) => assert_true(message.contains("غير صالح") || message.contains("امتداد"))
    None => assert_true(false)
  }
  
  // Set locale to Russian
  ErrorManager::set_locale(error_manager, "ru-RU")
  
  // Generate error with localized message
  let russian_error = ErrorManager::create_error(error_manager, TelemetryError::InvalidSpanContext)
  match ErrorManager::get_message(russian_error) {
    Some(message) => assert_true(message.contains("недействительный") || message.contains("промежуток"))
    None => assert_true(false)
  }
  
  // Reset locale to default
  ErrorManager::set_locale(error_manager, "en-US")
}

// Test 3: Localized Log Messages
test "localized log messages" {
  let logger = Logger::new("internationalization_test_logger")
  let localization_manager = LocalizationManager::new()
  
  // Test Chinese log messages
  LocalizationManager::set_locale(localization_manager, "zh-CN")
  let chinese_message = LocalizationManager::translate(localization_manager, "operation.started", "操作已开始")
  let chinese_log = LogRecord::new(Info, chinese_message)
  Logger::emit(logger, chinese_log)
  
  // Test Japanese log messages
  LocalizationManager::set_locale(localization_manager, "ja-JP")
  let japanese_message = LocalizationManager::translate(localization_manager, "operation.started", "操作が開始されました")
  let japanese_log = LogRecord::new(Info, japanese_message)
  Logger::emit(logger, japanese_log)
  
  // Test Korean log messages
  LocalizationManager::set_locale(localization_manager, "ko-KR")
  let korean_message = LocalizationManager::translate(localization_manager, "operation.started", "작업이 시작되었습니다")
  let korean_log = LogRecord::new(Info, korean_message)
  Logger::emit(logger, korean_log)
  
  // Test Arabic log messages
  LocalizationManager::set_locale(localization_manager, "ar-SA")
  let arabic_message = LocalizationManager::translate(localization_manager, "operation.started", "بدأت العملية")
  let arabic_log = LogRecord::new(Info, arabic_message)
  Logger::emit(logger, arabic_log)
  
  // Test Russian log messages
  LocalizationManager::set_locale(localization_manager, "ru-RU")
  let russian_message = LocalizationManager::translate(localization_manager, "operation.started", "Операция начата")
  let russian_log = LogRecord::new(Info, russian_message)
  Logger::emit(logger, russian_log)
  
  // Verify log messages were created with correct content
  let logs = Logger::get_logs(logger)
  assert_eq(logs.length(), 5)
  
  match LogRecord::body(logs[0]) {
    Some(message) => assert_eq(message, "操作已开始")
    None => assert_true(false)
  }
  
  match LogRecord::body(logs[1]) {
    Some(message) => assert_eq(message, "操作が開始されました")
    None => assert_true(false)
  }
  
  match LogRecord::body(logs[2]) {
    Some(message) => assert_eq(message, "작업이 시작되었습니다")
    None => assert_true(false)
  }
  
  match LogRecord::body(logs[3]) {
    Some(message) => assert_eq(message, "بدأت العملية")
    None => assert_true(false)
  }
  
  match LogRecord::body(logs[4]) {
    Some(message) => assert_eq(message, "Операция начата")
    None => assert_true(false)
  }
}

// Test 4: Localized Metric Names and Descriptions
test "localized metric names and descriptions" {
  let meter = Meter::new("internationalization_test_meter")
  let localization_manager = LocalizationManager::new()
  
  // Test Chinese metric names and descriptions
  LocalizationManager::set_locale(localization_manager, "zh-CN")
  let chinese_name = LocalizationManager::translate(localization_manager, "metric.request.count", "请求计数")
  let chinese_description = LocalizationManager::translate(localization_manager, "metric.request.count.desc", "处理的请求总数")
  let chinese_counter = Meter::create_counter(meter, chinese_name, Some(chinese_description), Some("个"))
  
  // Test Japanese metric names and descriptions
  LocalizationManager::set_locale(localization_manager, "ja-JP")
  let japanese_name = LocalizationManager::translate(localization_manager, "metric.request.count", "リクエスト数")
  let japanese_description = LocalizationManager::translate(localization_manager, "metric.request.count.desc", "処理されたリクエストの総数")
  let japanese_counter = Meter::create_counter(meter, japanese_name, Some(japanese_description), Some("件"))
  
  // Test Korean metric names and descriptions
  LocalizationManager::set_locale(localization_manager, "ko-KR")
  let korean_name = LocalizationManager::translate(localization_manager, "metric.request.count", "요청 수")
  let korean_description = LocalizationManager::translate(localization_manager, "metric.request.count.desc", "처리된 총 요청 수")
  let korean_counter = Meter::create_counter(meter, korean_name, Some(korean_description), Some("개"))
  
  // Test Arabic metric names and descriptions
  LocalizationManager::set_locale(localization_manager, "ar-SA")
  let arabic_name = LocalizationManager::translate(localization_manager, "metric.request.count", "عدد الطلبات")
  let arabic_description = LocalizationManager::translate(localization_manager, "metric.request.count.desc", "إجمالي عدد الطلبات المعالجة")
  let arabic_counter = Meter::create_counter(meter, arabic_name, Some(arabic_description), Some("طلب"))
  
  // Test Russian metric names and descriptions
  LocalizationManager::set_locale(localization_manager, "ru-RU")
  let russian_name = LocalizationManager::translate(localization_manager, "metric.request.count", "Количество запросов")
  let russian_description = LocalizationManager::translate(localization_manager, "metric.request.count.desc", "Общее количество обработанных запросов")
  let russian_counter = Meter::create_counter(meter, russian_name, Some(russian_description), Some("запрос"))
  
  // Verify metric names and descriptions
  assert_eq(Instrument::name(Counter::as_instrument(chinese_counter)), "请求计数")
  match Instrument::description(Counter::as_instrument(chinese_counter)) {
    Some(desc) => assert_eq(desc, "处理的请求总数")
    None => assert_true(false)
  }
  
  assert_eq(Instrument::name(Counter::as_instrument(japanese_counter)), "リクエスト数")
  match Instrument::description(Counter::as_instrument(japanese_counter)) {
    Some(desc) => assert_eq(desc, "処理されたリクエストの総数")
    None => assert_true(false)
  }
  
  assert_eq(Instrument::name(Counter::as_instrument(korean_counter)), "요청 수")
  match Instrument::description(Counter::as_instrument(korean_counter)) {
    Some(desc) => assert_eq(desc, "처리된 총 요청 수")
    None => assert_true(false)
  }
  
  assert_eq(Instrument::name(Counter::as_instrument(arabic_counter)), "عدد الطلبات")
  match Instrument::description(Counter::as_instrument(arabic_counter)) {
    Some(desc) => assert_eq(desc, "إجمالي عدد الطلبات المعالجة")
    None => assert_true(false)
  }
  
  assert_eq(Instrument::name(Counter::as_instrument(russian_counter)), "Количество запросов")
  match Instrument::description(Counter::as_instrument(russian_counter)) {
    Some(desc) => assert_eq(desc, "Общее количество обработанных запросов")
    None => assert_true(false)
  }
}

// Test 5: Localized Span Names and Events
test "localized span names and events" {
  let tracer = Tracer::new("internationalization_test_tracer")
  let localization_manager = LocalizationManager::new()
  
  // Test Chinese span names and events
  LocalizationManager::set_locale(localization_manager, "zh-CN")
  let chinese_span_name = LocalizationManager::translate(localization_manager, "span.database.query", "数据库查询")
  let chinese_span = Tracer::start_span(tracer, chinese_span_name)
  
  let chinese_event_name = LocalizationManager::translate(localization_manager, "event.query.start", "查询开始")
  Span::add_event(chinese_span, chinese_event_name, None)
  
  // Test Japanese span names and events
  LocalizationManager::set_locale(localization_manager, "ja-JP")
  let japanese_span_name = LocalizationManager::translate(localization_manager, "span.database.query", "データベースクエリ")
  let japanese_span = Tracer::start_span(tracer, japanese_span_name)
  
  let japanese_event_name = LocalizationManager::translate(localization_manager, "event.query.start", "クエリ開始")
  Span::add_event(japanese_span, japanese_event_name, None)
  
  // Test Korean span names and events
  LocalizationManager::set_locale(localization_manager, "ko-KR")
  let korean_span_name = LocalizationManager::translate(localization_manager, "span.database.query", "데이터베이스 쿼리")
  let korean_span = Tracer::start_span(tracer, korean_span_name)
  
  let korean_event_name = LocalizationManager::translate(localization_manager, "event.query.start", "쿼리 시작")
  Span::add_event(korean_span, korean_event_name, None)
  
  // Test Arabic span names and events
  LocalizationManager::set_locale(localization_manager, "ar-SA")
  let arabic_span_name = LocalizationManager::translate(localization_manager, "span.database.query", "استعلام قاعدة البيانات")
  let arabic_span = Tracer::start_span(tracer, arabic_span_name)
  
  let arabic_event_name = LocalizationManager::translate(localization_manager, "event.query.start", "بدء الاستعلام")
  Span::add_event(arabic_span, arabic_event_name, None)
  
  // Test Russian span names and events
  LocalizationManager::set_locale(localization_manager, "ru-RU")
  let russian_span_name = LocalizationManager::translate(localization_manager, "span.database.query", "Запрос к базе данных")
  let russian_span = Tracer::start_span(tracer, russian_span_name)
  
  let russian_event_name = LocalizationManager::translate(localization_manager, "event.query.start", "Начало запроса")
  Span::add_event(russian_span, russian_event_name, None)
  
  // Verify span names and events
  assert_eq(Span::name(chinese_span), "数据库查询")
  let chinese_events = Span::events(chinese_span)
  assert_eq(SpanEvent::name(chinese_events[0]), "查询开始")
  
  assert_eq(Span::name(japanese_span), "データベースクエリ")
  let japanese_events = Span::events(japanese_span)
  assert_eq(SpanEvent::name(japanese_events[0]), "クエリ開始")
  
  assert_eq(Span::name(korean_span), "데이터베이스 쿼리")
  let korean_events = Span::events(korean_span)
  assert_eq(SpanEvent::name(korean_events[0]), "쿼리 시작")
  
  assert_eq(Span::name(arabic_span), "استعلام قاعدة البيانات")
  let arabic_events = Span::events(arabic_span)
  assert_eq(SpanEvent::name(arabic_events[0]), "بدء الاستعلام")
  
  assert_eq(Span::name(russian_span), "Запрос к базе данных")
  let russian_events = Span::events(russian_span)
  assert_eq(SpanEvent::name(russian_events[0]), "Начало запроса")
  
  // End all spans
  Span::end(chinese_span)
  Span::end(japanese_span)
  Span::end(korean_span)
  Span::end(arabic_span)
  Span::end(russian_span)
}

// Test 6: Right-to-Left Text Support
test "right-to-left text support" {
  let attrs = Attributes::new()
  
  // Test Arabic RTL text
  Attributes::set(attrs, "arabic_text", StringValue("مرحبا بالعالم"))
  Attributes::set(attrs, "arabic_numbers", StringValue("١٢٣٤٥٦٧٨٩٠")) // Arabic-Indic digits
  
  // Test Hebrew RTL text
  Attributes::set(attrs, "hebrew_text", StringValue("שלום עולם"))
  Attributes::set(attrs, "hebrew_numbers", StringValue("אחד שניים שלושה")) // Hebrew numbers
  
  // Test mixed LTR/RTL text
  Attributes::set(attrs, "mixed_text", StringValue("Hello مرحبا World"))
  
  // Verify retrieval of RTL text
  match Attributes::get(attrs, "arabic_text") {
    Some(StringValue(value)) => {
      assert_eq(value, "مرحبا بالعالم")
      assert_true(TextDirection::is_rtl(value))
    }
    _ => assert_true(false)
  }
  
  match Attributes::get(attrs, "hebrew_text") {
    Some(StringValue(value)) => {
      assert_eq(value, "שלום עולם")
      assert_true(TextDirection::is_rtl(value))
    }
    _ => assert_true(false)
  }
  
  match Attributes::get(attrs, "mixed_text") {
    Some(StringValue(value)) => {
      assert_eq(value, "Hello مرحبا World")
      assert_true(TextDirection::is_mixed(value))
    }
    _ => assert_true(false)
  }
  
  // Test text direction detection
  assert_true(TextDirection::is_rtl("مرحبا"))
  assert_true(TextDirection::is_rtl("שלום"))
  assert_false(TextDirection::is_rtl("Hello"))
  assert_true(TextDirection::is_mixed("Hello مرحبا"))
}

// Test 7: Unicode Normalization
test "unicode normalization" {
  let unicode_normalizer = UnicodeNormalizer::new()
  
  // Test NFC normalization (canonical composition)
  let nfc_input = "e\u0301" // 'e' + combining acute accent
  let nfc_expected = "\u00e9" // 'é' as single character
  let nfc_result = UnicodeNormalizer::normalize_nfc(unicode_normalizer, nfc_input)
  assert_eq(nfc_result, nfc_expected)
  
  // Test NFD normalization (canonical decomposition)
  let nfd_input = "\u00e9" // 'é' as single character
  let nfd_expected = "e\u0301" // 'e' + combining acute accent
  let nfd_result = UnicodeNormalizer::normalize_nfd(unicode_normalizer, nfd_input)
  assert_eq(nfd_result, nfd_expected)
  
  // Test NFKC normalization (compatibility composition)
  let nfkc_input = "\ufb01" // 'fi' ligature
  let nfkc_expected = "fi" // 'f' + 'i'
  let nfkc_result = UnicodeNormalizer::normalize_nfkc(unicode_normalizer, nfkc_input)
  assert_eq(nfkc_result, nfkc_expected)
  
  // Test NFKD normalization (compatibility decomposition)
  let nfkd_input = "\ufb01" // 'fi' ligature
  let nfkd_expected = "fi" // 'f' + 'i'
  let nfkd_result = UnicodeNormalizer::normalize_nfkd(unicode_normalizer, nfkd_input)
  assert_eq(nfkd_result, nfkd_expected)
  
  // Test with Chinese characters
  let chinese_input = "你\u597d" // '你' + '好' (separate characters)
  let chinese_nfc_result = UnicodeNormalizer::normalize_nfc(unicode_normalizer, chinese_input)
  assert_eq(chinese_nfc_result, "你好") // Should be composed if possible
  
  // Test with Japanese characters
  let japanese_input = "は\u3099" // 'は' + combining dakuten
  let japanese_nfc_result = UnicodeNormalizer::normalize_nfc(unicode_normalizer, japanese_input)
  assert_eq(japanese_nfc_result, "ば") // Should be composed to 'ba'
}

// Test 8: Locale-Specific Number and Date Formatting
test "locale-specific number and date formatting" {
  let formatter = LocaleFormatter::new()
  
  // Test number formatting for different locales
  let number = 1234567.89
  
  // US English format
  LocaleFormatter::set_locale(formatter, "en-US")
  let us_format = LocaleFormatter::format_number(formatter, number)
  assert_eq(us_format, "1,234,567.89")
  
  // German format
  LocaleFormatter::set_locale(formatter, "de-DE")
  let german_format = LocaleFormatter::format_number(formatter, number)
  assert_eq(german_format, "1.234.567,89")
  
  // French format
  LocaleFormatter::set_locale(formatter, "fr-FR")
  let french_format = LocaleFormatter::format_number(formatter, number)
  assert_eq(french_format, "1 234 567,89")
  
  // Chinese format
  LocaleFormatter::set_locale(formatter, "zh-CN")
  let chinese_format = LocaleFormatter::format_number(formatter, number)
  assert_eq(chinese_format, "1,234,567.89")
  
  // Arabic format
  LocaleFormatter::set_locale(formatter, "ar-SA")
  let arabic_format = LocaleFormatter::format_number(formatter, number)
  assert_eq(arabic_format, "1,234,567.89")
  
  // Test date formatting for different locales
  let timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  // US English date format
  LocaleFormatter::set_locale(formatter, "en-US")
  let us_date = LocaleFormatter::format_date(formatter, timestamp)
  assert_eq(us_date, "1/1/2021")
  
  // German date format
  LocaleFormatter::set_locale(formatter, "de-DE")
  let german_date = LocaleFormatter::format_date(formatter, timestamp)
  assert_eq(german_date, "1.1.2021")
  
  // Japanese date format
  LocaleFormatter::set_locale(formatter, "ja-JP")
  let japanese_date = LocaleFormatter::format_date(formatter, timestamp)
  assert_eq(japanese_date, "2021/01/01")
  
  // Chinese date format
  LocaleFormatter::set_locale(formatter, "zh-CN")
  let chinese_date = LocaleFormatter::format_date(formatter, timestamp)
  assert_eq(chinese_date, "2021/1/1")
  
  // Arabic date format
  LocaleFormatter::set_locale(formatter, "ar-SA")
  let arabic_date = LocaleFormatter::format_date(formatter, timestamp)
  assert_eq(arabic_date, "١/١/٢٠٢١") // Using Arabic-Indic digits
}

// Test 9: Time Zone Support
test "time zone support" {
  let time_zone_manager = TimeZoneManager::new()
  let timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  // Test different time zones
  TimeZoneManager::set_time_zone(time_zone_manager, "UTC")
  let utc_time = TimeZoneManager::format_timestamp(time_zone_manager, timestamp)
  assert_eq(utc_time, "2021-01-01 00:00:00 UTC")
  
  TimeZoneManager::set_time_zone(time_zone_manager, "America/New_York")
  let ny_time = TimeZoneManager::format_timestamp(time_zone_manager, timestamp)
  assert_eq(ny_time, "2020-12-31 19:00:00 EST")
  
  TimeZoneManager::set_time_zone(time_zone_manager, "Europe/London")
  let london_time = TimeZoneManager::format_timestamp(time_zone_manager, timestamp)
  assert_eq(london_time, "2021-01-01 00:00:00 GMT")
  
  TimeZoneManager::set_time_zone(time_zone_manager, "Asia/Tokyo")
  let tokyo_time = TimeZoneManager::format_timestamp(time_zone_manager, timestamp)
  assert_eq(tokyo_time, "2021-01-01 09:00:00 JST")
  
  TimeZoneManager::set_time_zone(time_zone_manager, "Asia/Shanghai")
  let shanghai_time = TimeZoneManager::format_timestamp(time_zone_manager, timestamp)
  assert_eq(shanghai_time, "2021-01-01 08:00:00 CST")
  
  // Test time zone conversion
  let utc_timestamp = 1609459200000L
  let ny_timestamp = TimeZoneManager::convert_to_time_zone(time_zone_manager, utc_timestamp, "America/New_York")
  let tokyo_timestamp = TimeZoneManager::convert_to_time_zone(time_zone_manager, utc_timestamp, "Asia/Tokyo")
  
  // Verify time zone offsets
  assert_eq(tokyo_timestamp - utc_timestamp, 9 * 3600000L) // Tokyo is UTC+9
  assert_eq(ny_timestamp - utc_timestamp, -5 * 3600000L) // New York is UTC-5 in winter
}

// Test 10: Collation and String Comparison
test "collation and string comparison" {
  let collator = Collator::new()
  
  // Test different collation strengths
  
  // Primary collation (base characters only)
  Collator::set_strength(collator, CollationStrength::Primary)
  assert_true(Collator::compare(collator, "café", "cafe") == 0) // Should be equal with primary strength
  
  // Secondary collation (base characters + accents)
  Collator::set_strength(collator, CollationStrength::Secondary)
  assert_true(Collator::compare(collator, "café", "cafe") != 0) // Should be different with secondary strength
  assert_true(Collator::compare(collator, "café", "cafe") > 0) // café > cafe
  
  // Tertiary collation (base characters + accents + case)
  Collator::set_strength(collator, CollationStrength::Tertiary)
  assert_true(Collator::compare(collator, "apple", "Apple") > 0) // apple > Apple
  assert_true(Collator::compare(collator, "Apple", "apple") < 0) // Apple < apple
  
  // Quaternary collation (base characters + accents + case + other differences)
  Collator::set_strength(collator, CollationStrength::Quaternary)
  assert_true(Collator::compare(collator, "café", "caffé") < 0) // café < caffé
  
  // Test locale-specific collation
  Collator::set_locale(collator, "de-DE") // German
  
  // In German, 'ä' is treated like 'ae' in dictionary order
  assert_true(Collator::compare(collator, "äpfel", "apfel") > 0) // äpfel > apfel in German
  
  // Test with Swedish locale
  Collator::set_locale(collator, "sv-SE") // Swedish
  
  // In Swedish, 'å' comes after 'z'
  assert_true(Collator::compare(collator, "åland", "zambia") > 0) // åland > zambia in Swedish
  
  // Test with Japanese locale
  Collator::set_locale(collator, "ja-JP") // Japanese
  
  // In Japanese, hiragana and katakana are treated differently
  assert_true(Collator::compare(collator, "こんにちは", "コンニチハ") != 0) // Different scripts
  
  // Test with Chinese locale
  Collator::set_locale(collator, "zh-CN") // Chinese
  
  // In Chinese, characters are ordered by stroke count and other rules
  assert_true(Collator::compare(collator, "一", "二") < 0) // 一 comes before 二
  
  // Test string sorting with locale-specific collation
  let strings = ["café", "cane", "apple", "äpfel", "zebra"]
  let sorted_strings = Collator::sort_strings(collator, strings)
  
  // Verify sorting follows locale-specific rules
  for i in 1..=sorted_strings.length() {
    assert_true(Collator::compare(collator, sorted_strings[i-1], sorted_strings[i]) <= 0)
  }
}