// Premium Internationalization Support Tests for Azimuth
// This file contains comprehensive test cases for internationalization and localization

// Test 1: Multi-language Attribute Values
test "multi-language attribute values" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "i18n_test_tracer")
  
  // Create span with multi-language attributes
  let span = Tracer::start_span(tracer, "多语言测试_span")
  
  // Test Chinese attributes
  Span::set_attribute(span, "用户名", StringValue("张三"))
  Span::set_attribute(span, "操作", StringValue("用户登录"))
  Span::set_attribute(span, "描述", StringValue("用户从北京登录系统"))
  
  // Test Japanese attributes
  Span::set_attribute(span, "ユーザー名", StringValue("田中太郎"))
  Span::set_attribute(span, "操作", StringValue("ユーザーログイン"))
  Span::set_attribute(span, "説明", StringValue("東京からシステムにログイン"))
  
  // Test Korean attributes
  Span::set_attribute(span, "사용자명", StringValue("김철수"))
  Span::set_attribute(span, "작업", StringValue("사용자 로그인"))
  Span::set_attribute(span, "설명", StringValue("서울에서 시스템 로그인"))
  
  // Test Arabic attributes
  Span::set_attribute(span, "اسم_المستخدم", StringValue("أحمد محمد"))
  Span::set_attribute(span, "العملية", StringValue("تسجيل_دخول_المستخدم"))
  Span::set_attribute(span, "الوصف", StringValue("تسجيل الدخول إلى النظام من القاهرة"))
  
  // Test Russian attributes
  Span::set_attribute(span, "имя_пользователя", StringValue("Иван Петров"))
  Span::set_attribute(span, "операция", StringValue("вход_пользователя"))
  Span::set_attribute(span, "описание", StringValue("Вход в систему из Москвы"))
  
  // Verify attributes can be retrieved correctly
  let chinese_username = Span::get_attribute(span, "用户名")
  match chinese_username {
    Some(StringValue(value)) => assert_eq(value, "张三")
    _ => assert_true(false)
  }
  
  let japanese_username = Span::get_attribute(span, "ユーザー名")
  match japanese_username {
    Some(StringValue(value)) => assert_eq(value, "田中太郎")
    _ => assert_true(false)
  }
  
  let korean_username = Span::get_attribute(span, "사용자명")
  match korean_username {
    Some(StringValue(value)) => assert_eq(value, "김철수")
    _ => assert_true(false)
  }
  
  let arabic_username = Span::get_attribute(span, "اسم_المستخدم")
  match arabic_username {
    Some(StringValue(value)) => assert_eq(value, "أحمد محمد")
    _ => assert_true(false)
  }
  
  let russian_username = Span::get_attribute(span, "имя_пользователя")
  match russian_username {
    Some(StringValue(value)) => assert_eq(value, "Иван Петров")
    _ => assert_true(false)
  }
  
  Span::end(span)
}

// Test 2: Multi-language Log Messages
test "multi-language log messages" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "i18n_test_logger")
  
  // Test Chinese log messages
  let chinese_log = LogRecord::new(Info, "用户登录成功，用户ID：12345")
  Logger::emit(logger, chinese_log)
  
  // Test Japanese log messages
  let japanese_log = LogRecord::new(Info, "ユーザーログイン成功、ユーザーID：12345")
  Logger::emit(logger, japanese_log)
  
  // Test Korean log messages
  let korean_log = LogRecord::new(Info, "사용자 로그인 성공, 사용자 ID: 12345")
  Logger::emit(logger, korean_log)
  
  // Test Arabic log messages
  let arabic_log = LogRecord::new(Info, "تم تسجيل الدخول بنجاح، معرف المستخدم: 12345")
  Logger::emit(logger, arabic_log)
  
  // Test Russian log messages
  let russian_log = LogRecord::new(Info, "Вход пользователя выполнен успешно, ID пользователя: 12345")
  Logger::emit(logger, russian_log)
  
  // Test log with mixed languages
  let mixed_attrs = Attributes::new()
  Attributes::set(mixed_attrs, "中文消息", StringValue("这是中文消息"))
  Attributes::set(mixed_attrs, "日本語メッセージ", StringValue("これは日本語のメッセージです"))
  Attributes::set(mixed_attrs, "한국어 메시지", StringValue("이것은 한국어 메시지입니다"))
  
  let mixed_log = LogRecord::new_with_context(
    Warn,
    Some("Mixed language log message / 混合语言日志消息 / 混合言語ログメッセージ"),
    Some(mixed_attrs),
    None,
    None,
    None,
    None,
    None
  )
  Logger::emit(logger, mixed_log)
  
  // Verify log messages are handled correctly
  match LogRecord::body(chinese_log) {
    Some(message) => assert_true(message.contains("用户登录成功"))
    None => assert_true(false)
  }
  
  match LogRecord::body(japanese_log) {
    Some(message) => assert_true(message.contains("ユーザーログイン成功"))
    None => assert_true(false)
  }
}

// Test 3: Locale-specific Formatting
test "locale-specific formatting" {
  let formatter = LocaleFormatter::new()
  
  // Test date formatting for different locales
  let timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  let us_date = LocaleFormatter::format_date(formatter, timestamp, "en-US")
  let chinese_date = LocaleFormatter::format_date(formatter, timestamp, "zh-CN")
  let japanese_date = LocaleFormatter::format_date(formatter, timestamp, "ja-JP")
  let german_date = LocaleFormatter::format_date(formatter, timestamp, "de-DE")
  let arabic_date = LocaleFormatter::format_date(formatter, timestamp, "ar-SA")
  
  // Verify locale-specific formatting
  assert_true(us_date.contains("2021") && us_date.contains("01") && us_date.contains("01"))
  assert_true(chinese_date.contains("2021") && chinese_date.contains("01") && chinese_date.contains("01"))
  assert_true(japanese_date.contains("2021") && japanese_date.contains("01") && japanese_date.contains("01"))
  assert_true(german_date.contains("2021") && german_date.contains("01") && german_date.contains("01"))
  assert_true(arabic_date.contains("2021") && arabic_date.contains("01") && arabic_date.contains("01"))
  
  // Test number formatting for different locales
  let number = 1234567.89
  
  let us_number = LocaleFormatter::format_number(formatter, number, "en-US")
  let chinese_number = LocaleFormatter::format_number(formatter, number, "zh-CN")
  let german_number = LocaleFormatter::format_number(formatter, number, "de-DE")
  let french_number = LocaleFormatter::format_number(formatter, number, "fr-FR")
  let hindi_number = LocaleFormatter::format_number(formatter, number, "hi-IN")
  
  // Verify locale-specific number formatting
  assert_true(us_number.contains("1,234,567.89"))
  assert_true(chinese_number.contains("1,234,567.89")) // Chinese often uses similar format
  assert_true(german_number.contains("1.234.567,89")) // German uses comma as decimal separator
  assert_true(french_number.contains("1 234 567,89")) // French uses space as thousand separator
  assert_true(hindi_number.contains("12,34,567.89")) // Hindi uses different grouping
  
  // Test currency formatting for different locales
  let currency = 1234.56
  
  let us_currency = LocaleFormatter::format_currency(formatter, currency, "en-US", "USD")
  let euro_currency = LocaleFormatter::format_currency(formatter, currency, "de-DE", "EUR")
  let yen_currency = LocaleFormatter::format_currency(formatter, currency, "ja-JP", "JPY")
  let yuan_currency = LocaleFormatter::format_currency(formatter, currency, "zh-CN", "CNY")
  
  // Verify locale-specific currency formatting
  assert_true(us_currency.contains("$") || us_currency.contains("USD"))
  assert_true(euro_currency.contains("€") || euro_currency.contains("EUR"))
  assert_true(yen_currency.contains("¥") || yen_currency.contains("JPY"))
  assert_true(yuan_currency.contains("¥") || yuan_currency.contains("CNY"))
}

// Test 4: Right-to-Left Language Support
test "right-to-left language support" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "rtl_test_tracer")
  
  // Create span with RTL language attributes
  let span = Tracer::start_span(tracer, "اختبار_اللغة_العربية")
  
  // Test Arabic attributes
  Span::set_attribute(span, "اسم_الخدمة", StringValue("خدمة_المصادقة"))
  Span::set_attribute(span, "العملية", StringValue("تسجيل_الدخول"))
  Span::set_attribute(span, "النتيجة", StringValue("نجح"))
  Span::set_attribute(span, "الرسالة", StringValue("تم تسجيل الدخول بنجاح"))
  
  // Test Hebrew attributes
  Span::set_attribute(span, "שם_השירות", StringValue("שירות_אימות"))
  Span::set_attribute(span, "פעולה", StringValue("התחברות"))
  Span::set_attribute(span, "תוצאה", StringValue("הצליח"))
  Span::set_attribute(span, "הודעה", StringValue("ההתחברות הצליחה"))
  
  // Test Persian attributes
  Span::set_attribute(span, "نام_سرویس", StringValue("سرویس_احراز_هویت"))
  Span::set_attribute(span, "عملیات", StringValue("ورود_به_سیستم"))
  Span::set_attribute(span, "نتیجه", StringValue("موفقیت"))
  Span::set_attribute(span, "پیام", StringValue("ورود به سیستم با موفقیت انجام شد"))
  
  // Test Urdu attributes
  Span::set_attribute(span, "سروس_کا_نام", StringValue("تصدیق_کا_سروس"))
  Span::set_attribute(span, "عمل", StringValue("لاگ_ان"))
  Span::set_attribute(span, "نتیجہ", StringValue("کامیابی"))
  Span::set_attribute(span, "پیغام", StringValue("لاگ ان کامیابی سے ہوا"))
  
  // Verify RTL attributes can be retrieved correctly
  let arabic_service = Span::get_attribute(span, "اسم_الخدمة")
  match arabic_service {
    Some(StringValue(value)) => assert_eq(value, "خدمة_المصادقة")
    _ => assert_true(false)
  }
  
  let hebrew_service = Span::get_attribute(span, "שם_השירות")
  match hebrew_service {
    Some(StringValue(value)) => assert_eq(value, "שירות_אימות")
    _ => assert_true(false)
  }
  
  let persian_service = Span::get_attribute(span, "نام_سرویس")
  match persian_service {
    Some(StringValue(value)) => assert_eq(value, "سرویس_احراز_هویت")
    _ => assert_true(false)
  }
  
  let urdu_service = Span::get_attribute(span, "سروس_کا_نام")
  match urdu_service {
    Some(StringValue(value)) => assert_eq(value, "تصدیق_کا_سروس")
    _ => assert_true(false)
  }
  
  // Test RTL event messages
  Span::add_event(span, "بداية_العملية", Some([
    ("الوصف", StringValue("بدأت عملية تسجيل الدخول"))
  ]))
  
  Span::add_event(span, "סיום_הפעולה", Some([
    ("תיאור", StringValue("פעולת ההתחברות הסתיימה"))
  ]))
  
  Span::end(span)
}

// Test 5: Unicode and Special Character Support
test "unicode and special character support" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "unicode_test_tracer")
  
  // Create span with Unicode characters
  let span = Tracer::start_span(tracer, "🌍_Unicode_Test_🚀")
  
  // Test emoji support
  Span::set_attribute(span, "status", StringValue("✅ Success"))
  Span::set_attribute(span, "mood", StringValue("😊 Happy"))
  Span::set_attribute(span, "weather", StringValue("🌤️ Partly Cloudy"))
  
  // Test mathematical symbols
  Span::set_attribute(span, "formula", StringValue("E = mc²"))
  Span::set_attribute(span, "inequality", StringValue("x ≥ 5 ∧ y ≤ 10"))
  Span::set_attribute(span, "approximation", StringValue("π ≈ 3.14159"))
  
  // Test currency symbols
  Span::set_attribute(span, "price_usd", StringValue("$19.99"))
  Span::set_attribute(span, "price_eur", StringValue("€15.99"))
  Span::set_attribute(span, "price_jpy", StringValue("¥2,000"))
  Span::set_attribute(span, "price_gbp", StringValue("£12.99"))
  Span::set_attribute(span, "price_cny", StringValue("¥138.99"))
  Span::set_attribute(span, "price_inr", StringValue("₹1,499"))
  
  // Test special punctuation
  Span::set_attribute(span, "smart_quotes", StringValue(""Smart quotes""))
  Span::set_attribute(span, "em_dash", StringValue("Text—with em dash"))
  Span::set_attribute(span, "ellipsis", StringValue("Continuing…"))
  
  // Test combining characters
  Span::set_attribute(span, "accented", StringValue("café naïve résumé"))
  Span::set_attribute(span, "german_umlauts", StringValue("Müller Jäger Schön"))
  Span::set_attribute(span, "scandinavian", StringValue("Ångström Øresund"))
  
  // Verify Unicode attributes can be retrieved correctly
  let status = Span::get_attribute(span, "status")
  match status {
    Some(StringValue(value)) => assert_eq(value, "✅ Success")
    _ => assert_true(false)
  }
  
  let formula = Span::get_attribute(span, "formula")
  match formula {
    Some(StringValue(value)) => assert_eq(value, "E = mc²")
    _ => assert_true(false)
  }
  
  let price_usd = Span::get_attribute(span, "price_usd")
  match price_usd {
    Some(StringValue(value)) => assert_eq(value, "$19.99")
    _ => assert_true(false)
  }
  
  let accented = Span::get_attribute(span, "accented")
  match accented {
    Some(StringValue(value)) => assert_eq(value, "café naïve résumé")
    _ => assert_true(false)
  }
  
  // Test Unicode in events
  Span::add_event(span, "🎉 Celebration", Some([
    ("message", StringValue("Operation completed successfully! 🎊")),
    ("math_result", StringValue("∑(1..n) = n(n+1)/2"))
  ]))
  
  Span::end(span)
}

// Test 6: Time Zone Support
test "time zone support" {
  let time_zone_formatter = TimeZoneFormatter::new()
  
  // Test timestamp conversion for different time zones
  let utc_timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  let utc_time = TimeZoneFormatter::convert(utc_timestamp, "UTC")
  let est_time = TimeZoneFormatter::convert(utc_timestamp, "America/New_York")
  let pst_time = TimeZoneFormatter::convert(utc_timestamp, "America/Los_Angeles")
  let jst_time = TimeZoneFormatter::convert(utc_timestamp, "Asia/Tokyo")
  let cst_time = TimeZoneFormatter::convert(utc_timestamp, "Asia/Shanghai")
  let cet_time = TimeZoneFormatter::convert(utc_timestamp, "Europe/Paris")
  let ist_time = TimeZoneFormatter::convert(utc_timestamp, "Asia/Kolkata")
  
  // Verify time zone conversions
  assert_true(utc_time.contains("00:00:00")) // UTC time
  assert_true(est_time.contains("19:00:00")) // EST is UTC-5 (Dec 31, 2020)
  assert_true(pst_time.contains("16:00:00")) // PST is UTC-8 (Dec 31, 2020)
  assert_true(jst_time.contains("09:00:00")) // JST is UTC+9
  assert_true(cst_time.contains("08:00:00")) // CST is UTC+8
  assert_true(cet_time.contains("01:00:00")) // CET is UTC+1
  assert_true(ist_time.contains("05:30:00")) // IST is UTC+5:30
  
  // Test time zone aware logging
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "timezone_test_logger")
  
  let utc_log = LogRecord::new_with_context(
    Info,
    Some("UTC log message"),
    None,
    Some(utc_timestamp),
    None,
    None,
    None,
    None
  )
  
  let localized_log = TimeZoneFormatter::localize_log(time_zone_formatter, utc_log, "Asia/Tokyo")
  
  match LogRecord::body(localized_log) {
    Some(message) => assert_true(message.contains("UTC log message"))
    None => assert_true(false)
  }
  
  Logger::emit(logger, utc_log)
  Logger::emit(logger, localized_log)
}

// Test 7: Collation and Sorting
test "collation and sorting" {
  let collator = Collator::new()
  
  // Test strings from different languages
  let strings = [
    "苹果",        // Chinese
    "りんご",       // Japanese
    "사과",        // Korean
    "تفاح",        // Arabic
    "תפוח",        // Hebrew
    "Apple",       // English
    "Äpfel",       // German
    "Pomme",       // French
    "Manzana",     // Spanish
    "Яблоко"       // Russian
  ]
  
  // Sort using different collations
  let english_sort = Collator::sort(collator, strings, "en-US")
  let chinese_sort = Collator::sort(collator, strings, "zh-CN")
  let japanese_sort = Collator::sort(collator, strings, "ja-JP")
  let arabic_sort = Collator::sort(collator, strings, "ar-SA")
  
  // Verify sorting is different based on locale
  assert_ne(english_sort, chinese_sort)
  assert_ne(chinese_sort, japanese_sort)
  assert_ne(japanese_sort, arabic_sort)
  assert_ne(arabic_sort, english_sort)
  
  // Test search with different collations
  let search_term = "app"
  
  let english_results = Collator::search(collator, strings, search_term, "en-US")
  let german_results = Collator::search(collator, strings, search_term, "de-DE")
  let spanish_results = Collator::search(collator, strings, search_term, "es-ES")
  
  // Verify search results vary by locale
  assert_true(english_results.contains("Apple"))
  assert_true(german_results.contains("Äpfel"))
  assert_true(spanish_results.contains("Manzana"))
}

// Test 8: Localized Error Messages
test "localized error messages" {
  let error_localizer = ErrorLocalizer::new()
  
  // Create error with localized messages
  let error_code = "AUTHENTICATION_FAILED"
  
  let english_error = ErrorLocalizer::get_message(error_localizer, error_code, "en-US")
  let chinese_error = ErrorLocalizer::get_message(error_localizer, error_code, "zh-CN")
  let japanese_error = ErrorLocalizer::get_message(error_localizer, error_code, "ja-JP")
  let korean_error = ErrorLocalizer::get_message(error_localizer, error_code, "ko-KR")
  let arabic_error = ErrorLocalizer::get_message(error_localizer, error_code, "ar-SA")
  let russian_error = ErrorLocalizer::get_message(error_localizer, error_code, "ru-RU")
  
  // Verify localized error messages
  assert_true(english_error.contains("Authentication failed"))
  assert_true(chinese_error.contains("认证失败"))
  assert_true(japanese_error.contains("認証に失敗しました"))
  assert_true(korean_error.contains("인증 실패"))
  assert_true(arabic_error.contains("فشل المصادقة"))
  assert_true(russian_error.contains("Ошибка аутентификации"))
  
  // Test error with parameters
  let param_error_code = "INVALID_PARAMETER"
  let params = [("parameter", "username"), ("value", "user@")]
  
  let english_param_error = ErrorLocalizer::get_message_with_params(
    error_localizer, 
    param_error_code, 
    "en-US", 
    params
  )
  
  let chinese_param_error = ErrorLocalizer::get_message_with_params(
    error_localizer, 
    param_error_code, 
    "zh-CN", 
    params
  )
  
  // Verify parameter substitution
  assert_true(english_param_error.contains("username"))
  assert_true(english_param_error.contains("user@"))
  assert_true(chinese_param_error.contains("username"))
  assert_true(chinese_param_error.contains("user@"))
  
  // Test localized telemetry with errors
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error_test_tracer")
  
  let span = Tracer::start_span(tracer, "authentication_operation")
  
  // Add localized error attributes
  Span::set_attribute(span, "error.code", StringValue(error_code))
  Span::set_attribute(span, "error.message.en", StringValue(english_error))
  Span::set_attribute(span, "error.message.zh", StringValue(chinese_error))
  Span::set_attribute(span, "error.message.ja", StringValue(japanese_error))
  Span::set_attribute(span, "error.message.ko", StringValue(korean_error))
  
  // Set span status with localized description
  Span::set_status(span, Error, Some(english_error))
  
  Span::end(span)
}

// Test 9: Resource Bundle and Message Formatting
test "resource bundle and message formatting" {
  let resource_bundle = ResourceBundle::new()
  
  // Load resources for different locales
  ResourceBundle::load(resource_bundle, "en-US", "messages_en.properties")
  ResourceBundle::load(resource_bundle, "zh-CN", "messages_zh.properties")
  ResourceBundle::load(resource_bundle, "ja-JP", "messages_ja.properties")
  
  // Test message formatting
  let message_key = "user.login.success"
  let params = [("username", "john_doe"), ("location", "New York")]
  
  let english_message = ResourceBundle::format_message(resource_bundle, message_key, "en-US", params)
  let chinese_message = ResourceBundle::format_message(resource_bundle, message_key, "zh-CN", params)
  let japanese_message = ResourceBundle::format_message(resource_bundle, message_key, "ja-JP", params)
  
  // Verify formatted messages
  assert_true(english_message.contains("john_doe"))
  assert_true(english_message.contains("New York"))
  assert_true(chinese_message.contains("john_doe"))
  assert_true(chinese_message.contains("New York"))
  assert_true(japanese_message.contains("john_doe"))
  assert_true(japanese_message.contains("New York"))
  
  // Test pluralization
  let plural_key = "items.count"
  let count_params = [("count", "1")]
  let multiple_params = [("count", "5")]
  
  let english_singular = ResourceBundle::format_message(resource_bundle, plural_key, "en-US", count_params)
  let english_plural = ResourceBundle::format_message(resource_bundle, plural_key, "en-US", multiple_params)
  
  // Verify pluralization
  assert_ne(english_singular, english_plural)
  
  // Test gender-specific messages
  let gender_key = "user.welcome"
  let male_params = [("gender", "male"), ("name", "John")]
  let female_params = [("gender", "female"), ("name", "Jane")]
  
  let english_male = ResourceBundle::format_message(resource_bundle, gender_key, "en-US", male_params)
  let english_female = ResourceBundle::format_message(resource_bundle, gender_key, "en-US", female_params)
  
  // Verify gender-specific messages
  assert_ne(english_male, english_female)
  assert_true(english_male.contains("John"))
  assert_true(english_female.contains("Jane"))
}

// Test 10: Locale-aware Metrics and Aggregation
test "locale-aware metrics and aggregation" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "locale_test_meter")
  
  // Create locale-specific metrics
  let us_counter = Meter::create_counter(meter, "user_requests", Some("User requests by country"), Some("count"))
  let cn_counter = Meter::create_counter(meter, "user_requests", Some("User requests by country"), Some("count"))
  let jp_counter = Meter::create_counter(meter, "user_requests", Some("User requests by country"), Some("count"))
  
  // Record metrics with locale-specific attributes
  let us_attrs = Attributes::new()
  Attributes::set(us_attrs, "country", StringValue("US"))
  Attributes::set(us_attrs, "locale", StringValue("en-US"))
  Attributes::set(us_attrs, "currency", StringValue("USD"))
  
  let cn_attrs = Attributes::new()
  Attributes::set(cn_attrs, "country", StringValue("CN"))
  Attributes::set(cn_attrs, "locale", StringValue("zh-CN"))
  Attributes::set(cn_attrs, "currency", StringValue("CNY"))
  
  let jp_attrs = Attributes::new()
  Attributes::set(jp_attrs, "country", StringValue("JP"))
  Attributes::set(jp_attrs, "locale", StringValue("ja-JP"))
  Attributes::set(jp_attrs, "currency", StringValue("JPY"))
  
  // Record metrics
  Counter::add(us_counter, 1000.0, Some(us_attrs))
  Counter::add(cn_counter, 800.0, Some(cn_attrs))
  Counter::add(jp_counter, 600.0, Some(jp_attrs))
  
  // Create locale-specific histograms
  let response_time_histogram = Meter::create_histogram(meter, "response_time", Some("Response time"), Some("ms"))
  
  // Record response times with locale-specific expectations
  for i in 0..=100 {
    let us_time = (50 + i).to_float() // US expects faster response
    let cn_time = (100 + i * 2).to_float() // CN expects slower response
    let jp_time = (75 + i).to_float() // JP expects medium response
    
    Histogram::record(response_time_histogram, us_time, Some(us_attrs))
    Histogram::record(response_time_histogram, cn_time, Some(cn_attrs))
    Histogram::record(response_time_histogram, jp_time, Some(jp_attrs))
  }
  
  // Test locale-aware aggregation
  let aggregator = LocaleAwareAggregator::new()
  
  let us_metrics = LocaleAwareAggregator::aggregate_by_locale(aggregator, "en-US")
  let cn_metrics = LocaleAwareAggregator::aggregate_by_locale(aggregator, "zh-CN")
  let jp_metrics = LocaleAwareAggregator::aggregate_by_locale(aggregator, "ja-JP")
  
  // Verify locale-specific metrics
  match us_metrics {
    Some(metrics) => {
      assert_true(LocaleAwareMetrics::get_request_count(metrics) > 0)
      assert_true(LocaleAwareMetrics::get_avg_response_time(metrics) > 0.0)
    }
    None => assert_true(false)
  }
  
  match cn_metrics {
    Some(metrics) => {
      assert_true(LocaleAwareMetrics::get_request_count(metrics) > 0)
      assert_true(LocaleAwareMetrics::get_avg_response_time(metrics) > 0.0)
    }
    None => assert_true(false)
  }
  
  match jp_metrics {
    Some(metrics) => {
      assert_true(LocaleAwareMetrics::get_request_count(metrics) > 0)
      assert_true(LocaleAwareMetrics::get_avg_response_time(metrics) > 0.0)
    }
    None => assert_true(false)
  }
  
  // Test locale-specific reporting
  let reporter = LocaleAwareReporter::new()
  
  let us_report = LocaleAwareReporter::generate_report(reporter, us_metrics, "en-US")
  let cn_report = LocaleAwareReporter::generate_report(reporter, cn_metrics, "zh-CN")
  let jp_report = LocaleAwareReporter::generate_report(reporter, jp_metrics, "ja-JP")
  
  // Verify reports are localized
  assert_true(us_report.contains("requests") || us_report.contains("count"))
  assert_true(cn_report.contains("请求") || cn_report.contains("计数"))
  assert_true(jp_report.contains("リクエスト") || jp_report.contains("カウント"))
}