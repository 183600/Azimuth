// Azimuth Internationalization and Globalization Tests
// This file contains tests for internationalization and globalization features in the Azimuth telemetry system

// Test 1: Unicode and multi-language support in telemetry data
pub test "unicode and multi-language support in telemetry data" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "i18n-test")
  
  // Test spans with Unicode names and attributes
  let chinese_span = azimuth::Tracer::start_span(tracer, "中文操作追踪")
  azimuth::Span::set_attribute(chinese_span, "用户名", azimuth::StringValue("张三"))
  azimuth::Span::set_attribute(chinese_span, "操作描述", azimuth::StringValue("用户执行了数据查询操作"))
  azimuth::Span::add_event(chinese_span, "中文事件", Some([("事件详情", azimuth::StringValue("查询完成"))]))
  
  let japanese_span = azimuth::Tracer::start_span(tracer, "日本語オペレーション")
  azimuth::Span::set_attribute(japanese_span, "ユーザー名", azimuth::StringValue("田中太郎"))
  azimuth::Span::set_attribute(japanese_span, "操作説明", azimuth::StringValue("データ検索操作を実行"))
  azimuth::Span::add_event(japanese_span, "日本語イベント", Some([("イベント詳細", azimuth::StringValue("検索完了"))]))
  
  let arabic_span = azimuth::Tracer::start_span(tracer, "عملية باللغة العربية")
  azimuth::Span::set_attribute(arabic_span, "اسم المستخدم", azimuth::StringValue("أحمد محمد"))
  azimuth::Span::set_attribute(arabic_span, "وصف العملية", azimuth::StringValue("تنفيذ عملية استعلام البيانات"))
  azimuth::Span::add_event(arabic_span, "حدث باللغة العربية", Some([("تفاصيل الحدث", azimuth::StringValue("اكتمل الاستعلام"))]))
  
  let russian_span = azimuth::Tracer::start_span(tracer, "операция на русском языке")
  azimuth::Span::set_attribute(russian_span, "имя пользователя", azimuth::StringValue("Иван Петров"))
  azimuth::Span::set_attribute(russian_span, "описание операции", azimuth::StringValue("выполнение операции запроса данных"))
  azimuth::Span::add_event(russian_span, "событие на русском", Some([("детали события", azimuth::StringValue("запрос завершен"))]))
  
  // Test emoji and special characters
  let emoji_span = azimuth::Tracer::start_span(tracer, "Emoji Test 🚀🔥💯")
  azimuth::Span::set_attribute(emoji_span, "emoji.test", azimuth::StringValue("😀🎉🌟⭐💫"))
  azimuth::Span::set_attribute(emoji_span, "special.chars", azimuth::StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?"))
  azimuth::Span::add_event(emoji_span, "Emoji Event 🎊", Some([("emoji.data", azimuth::StringValue("🚀🔥🌟"))]))
  
  // End all spans
  azimuth::Span::end(chinese_span)
  azimuth::Span::end(japanese_span)
  azimuth::Span::end(arabic_span)
  azimuth::Span::end(russian_span)
  azimuth::Span::end(emoji_span)
  
  // Verify span properties
  assert_eq(azimuth::Span::name(chinese_span), "中文操作追踪")
  assert_eq(azimuth::Span::name(japanese_span), "日本語オペレーション")
  assert_eq(azimuth::Span::name(arabic_span), "عملية باللغة العربية")
  assert_eq(azimuth::Span::name(russian_span), "операция на русском языке")
  assert_eq(azimuth::Span::name(emoji_span), "Emoji Test 🚀🔥💯")
}

// Test 2: Locale-aware formatting and parsing
pub test "locale-aware formatting and parsing" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "locale-test")
  
  // Test locale-aware counter
  let counter = azimuth::Meter::create_counter(meter, "locale.counter", Some("国际化计数器"), Some("计数"))
  
  // Configure different locales
  let us_locale = azimuth::Locale::new("en-US")
  let french_locale = azimuth::Locale::new("fr-FR")
  let german_locale = azimuth::Locale::new("de-DE")
  let chinese_locale = azimuth::Locale::new("zh-CN")
  let japanese_locale = azimuth::Locale::new("ja-JP")
  let arabic_locale = azimuth::Locale::new("ar-SA")
  
  // Record metrics with locale-specific formatting
  azimuth::Counter::add(counter, 1234567.89, [("locale", "en-US")])
  azimuth::Counter::add(counter, 1234567.89, [("locale", "fr-FR")])
  azimuth::Counter::add(counter, 1234567.89, [("locale", "de-DE")])
  azimuth::Counter::add(counter, 1234567.89, [("locale", "zh-CN")])
  azimuth::Counter::add(counter, 1234567.89, [("locale", "ja-JP")])
  azimuth::Counter::add(counter, 1234567.89, [("locale", "ar-SA")])
  
  // Test locale-aware number formatting
  let us_formatted = azimuth::LocaleFormatter::format_number(us_locale, 1234567.89)
  let french_formatted = azimuth::LocaleFormatter::format_number(french_locale, 1234567.89)
  let german_formatted = azimuth::LocaleFormatter::format_number(german_locale, 1234567.89)
  let chinese_formatted = azimuth::LocaleFormatter::format_number(chinese_locale, 1234567.89)
  let japanese_formatted = azimuth::LocaleFormatter::format_number(japanese_locale, 1234567.89)
  let arabic_formatted = azimuth::LocaleFormatter::format_number(arabic_locale, 1234567.89)
  
  // Verify locale-specific formatting
  assert_eq(us_formatted, "1,234,567.89")      // US: comma separator, dot decimal
  assert_eq(french_formatted, "1 234 567,89")  // French: space separator, comma decimal
  assert_eq(german_formatted, "1.234.567,89")  // German: dot separator, comma decimal
  assert_eq(chinese_formatted, "1,234,567.89") // Chinese: similar to US
  assert_eq(japanese_formatted, "1,234,567.89") // Japanese: similar to US
  assert_eq(arabic_formatted, "1,234,567.89")   // Arabic: similar to US (numbers remain Western)
  
  // Test locale-aware date/time formatting
  let timestamp = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let us_date = azimuth::LocaleFormatter::format_date_time(us_locale, timestamp)
  let french_date = azimuth::LocaleFormatter::format_date_time(french_locale, timestamp)
  let german_date = azimuth::LocaleFormatter::format_date_time(german_locale, timestamp)
  let chinese_date = azimuth::LocaleFormatter::format_date_time(chinese_locale, timestamp)
  let japanese_date = azimuth::LocaleFormatter::format_date_time(japanese_locale, timestamp)
  let arabic_date = azimuth::LocaleFormatter::format_date_time(arabic_locale, timestamp)
  
  // Verify date formatting contains locale-specific elements
  assert_true(us_date.contains("/"))     // US: MM/DD/YYYY
  assert_true(french_date.contains("/")) // French: DD/MM/YYYY
  assert_true(german_date.contains(".")) // German: DD.MM.YYYY
  assert_true(chinese_date.contains("/") || chinese_date.contains("-")) // Chinese: YYYY/MM/DD or YYYY-MM-DD
  assert_true(japanese_date.contains("/") || japanese_date.contains("-")) // Japanese: similar to Chinese
  assert_true(arabic_date.contains("/") || arabic_date.contains("-")) // Arabic: varies by region
  
  // Test locale-aware currency formatting
  let us_currency = azimuth::LocaleFormatter::format_currency(us_locale, 1234.56, "USD")
  let euro_currency = azimuth::LocaleFormatter::format_currency(french_locale, 1234.56, "EUR")
  let yen_currency = azimuth::LocaleFormatter::format_currency(japanese_locale, 1234.56, "JPY")
  
  assert_true(us_currency.contains("$") || us_currency.contains("USD"))
  assert_true(euro_currency.contains("€") || euro_currency.contains("EUR"))
  assert_true(yen_currency.contains("¥") || yen_currency.contains("JPY"))
}

// Test 3: Right-to-left (RTL) language support
pub test "right-to-left language support" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "rtl-test")
  
  // Test Arabic log messages
  let arabic_log = azimuth::LogRecord::new(azimuth::Info, "تم تسجيل الدخول بنجاح")
  azimuth::LogRecord::set_attribute(arabic_log, "اسم المستخدم", azimuth::StringValue("أحمد محمد"))
  azimuth::LogRecord::set_attribute(arabic_log, "العملية", azimuth::StringValue("تسجيل الدخول"))
  azimuth::LogRecord::set_attribute(arabic_log, "النتيجة", azimuth::StringValue("نجاح"))
  azimuth::LogRecord::set_attribute(arabic_log, "الاتجاه", azimuth::StringValue("من اليمين إلى اليسار"))
  
  // Test Hebrew log messages
  let hebrew_log = azimuth::LogRecord::new(azimuth::Info, "ההתחברות בוצעה בהצלחה")
  azimuth::LogRecord::set_attribute(hebrew_log, "שם משתמש", azimuth::StringValue("דוד כהן"))
  azimuth::LogRecord::set_attribute(hebrew_log, "פעולה", azimuth::StringValue("התחברות"))
  azimuth::LogRecord::set_attribute(hebrew_log, "תוצאה", azimuth::StringValue("הצלחה"))
  azimuth::LogRecord::set_attribute(hebrew_log, "כיוון", azimuth::StringValue("ימין לשמאל"))
  
  // Test Persian log messages
  let persian_log = azimuth::LogRecord::new(azimuth::Info, "ورود با موفقیت انجام شد")
  azimuth::LogRecord::set_attribute(persian_log, "نام کاربری", azimuth::StringValue("علی رضایی"))
  azimuth::LogRecord::set_attribute(persian_log, "عملیات", azimuth::StringValue("ورود"))
  azimuth::LogRecord::set_attribute(persian_log, "نتیجه", azimuth::StringValue("موفقیت"))
  azimuth::LogRecord::set_attribute(persian_log, "جهت", azimuth::StringValue("راست به چپ"))
  
  // Test Urdu log messages
  let urdu_log = azimuth::LogRecord::new(azimuth::Info, "لاگ ان کامیابی سے ہوگیا")
  azimuth::LogRecord::set_attribute(urdu_log, "صارف کا نام", azimuth::StringValue("محمد علی"))
  azimuth::LogRecord::set_attribute(urdu_log, "عمل", azimuth::StringValue("لاگ ان"))
  azimuth::LogRecord::set_attribute(urdu_log, "نتیجہ", azimuth::StringValue("کامیابی"))
  azimuth::LogRecord::set_attribute(urdu_log, "سمت", azimuth::StringValue("دائیں سے بائیں"))
  
  // Emit all log records
  azimuth::Logger::emit(logger, arabic_log)
  azimuth::Logger::emit(logger, hebrew_log)
  azimuth::Logger::emit(logger, persian_log)
  azimuth::Logger::emit(logger, urdu_log)
  
  // Test RTL text direction detection
  let arabic_direction = azimuth::TextDirection::detect_direction("تم تسجيل الدخول بنجاح")
  let hebrew_direction = azimuth::TextDirection::detect_direction("ההתחברות בוצעה בהצלחה")
  let persian_direction = azimuth::TextDirection::detect_direction("ورود با موفقیت انجام شد")
  let urdu_direction = azimuth::TextDirection::detect_direction("لاگ ان کامیابی سے ہوگیا")
  let english_direction = azimuth::TextDirection::detect_direction("Login successful")
  
  assert_eq(arabic_direction, azimuth::RTL)
  assert_eq(hebrew_direction, azimuth::RTL)
  assert_eq(persian_direction, azimuth::RTL)
  assert_eq(urdu_direction, azimuth::RTL)
  assert_eq(english_direction, azimuth::LTR)
  
  // Test mixed LTR/RTL text handling
  let mixed_text = "Hello مرحبا world عالم"
  let mixed_direction = azimuth::TextDirection::detect_direction(mixed_text)
  assert_eq(mixed_direction, azimuth::Mixed)  // Should detect as mixed
  
  // Test RTL-aware text formatting
  let rtl_formatter = azimuth::RTLFormatter::new()
  let formatted_arabic = azimuth::RTLFormatter::format(rtl_formatter, "اسم المستخدم: أحمد")
  let formatted_hebrew = azimuth::RTLFormatter::format(rtl_formatter, "שם משתמש: דוד")
  
  // Verify RTL formatting preserves text direction
  assert_true(azimuth::TextDirection::detect_direction(formatted_arabic) == azimuth::RTL)
  assert_true(azimuth::TextDirection::detect_direction(formatted_hebrew) == azimuth::RTL)
}

// Test 4: Timezone and calendar system support
pub test "timezone and calendar system support" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "timezone-test")
  
  // Test different timezones
  let utc_timezone = azimuth::Timezone::new("UTC")
  let us_eastern = azimuth::Timezone::new("America/New_York")
  let us_pacific = azimuth::Timezone::new("America/Los_Angeles")
  let european = azimuth::Timezone::new("Europe/Paris")
  let asian = azimuth::Timezone::new("Asia/Tokyo")
  let australian = azimuth::Timezone::new("Australia/Sydney")
  
  // Create a fixed timestamp for testing
  let test_timestamp = 1672531200000000000L  // 2023-01-01 00:00:00 UTC
  
  // Test timezone conversions
  let utc_time = azimuth::Timezone::convert(utc_timezone, test_timestamp)
  let eastern_time = azimuth::Timezone::convert(us_eastern, test_timestamp)
  let pacific_time = azimuth::Timezone::convert(us_pacific, test_timestamp)
  let european_time = azimuth::Timezone::convert(european, test_timestamp)
  let asian_time = azimuth::Timezone::convert(asian, test_timestamp)
  let australian_time = azimuth::Timezone::convert(australian, test_timestamp)
  
  // Verify timezone offsets
  assert_eq(azimuth::Timezone::get_offset_hours(utc_timezone), 0)
  assert_eq(azimuth::Timezone::get_offset_hours(us_eastern), -5)  // EST (ignoring DST)
  assert_eq(azimuth::Timezone::get_offset_hours(us_pacific), -8)  // PST (ignoring DST)
  assert_eq(azimuth::Timezone::get_offset_hours(european), 1)     // CET (ignoring DST)
  assert_eq(azimuth::Timezone::get_offset_hours(asian), 9)        // JST
  assert_eq(azimuth::Timezone::get_offset_hours(australian), 11)  // AEDT (ignoring DST)
  
  // Test calendar systems
  let gregorian = azimuth::Calendar::new("gregorian")
  let islamic = azimuth::Calendar::new("islamic")
  let hebrew = azimuth::Calendar::new("hebrew")
  let chinese = azimuth::Calendar::new("chinese")
  let japanese = azimuth::Calendar::new("japanese")
  let indian = azimuth::Calendar::new("indian")
  
  // Convert timestamp to different calendar systems
  let gregorian_date = azimuth::Calendar::convert_to_calendar(gregorian, test_timestamp)
  let islamic_date = azimuth::Calendar::convert_to_calendar(islamic, test_timestamp)
  let hebrew_date = azimuth::Calendar::convert_to_calendar(hebrew, test_timestamp)
  let chinese_date = azimuth::Calendar::convert_to_calendar(chinese, test_timestamp)
  let japanese_date = azimuth::Calendar::convert_to_calendar(japanese, test_timestamp)
  let indian_date = azimuth::Calendar::convert_to_calendar(indian, test_timestamp)
  
  // Verify calendar system differences
  assert_eq(gregorian_date.year, 2023)
  assert_eq(gregorian_date.month, 1)
  assert_eq(gregorian_date.day, 1)
  
  // Islamic calendar should have different year/month/day
  assert_true(islamic_date.year != gregorian_date.year)
  assert_true(islamic_date.month >= 1 && islamic_date.month <= 12)
  assert_true(islamic_date.day >= 1 && islamic_date.day <= 30)
  
  // Hebrew calendar should have different year/month/day
  assert_true(hebrew_date.year != gregorian_date.year)
  assert_true(hebrew_date.month >= 1 && hebrew_date.month <= 13)
  assert_true(hebrew_date.day >= 1 && hebrew_date.day <= 30)
  
  // Test timezone-aware metric recording
  let timezone_counter = azimuth::Meter::create_counter(meter, "timezone.counter", Some("时区计数器"), Some("计数"))
  
  azimuth::Counter::add(timezone_counter, 1, [("timezone", "UTC"), ("local_time", utc_time)])
  azimuth::Counter::add(timezone_counter, 1, [("timezone", "US/Eastern"), ("local_time", eastern_time)])
  azimuth::Counter::add(timezone_counter, 1, [("timezone", "US/Pacific"), ("local_time", pacific_time)])
  azimuth::Counter::add(timezone_counter, 1, [("timezone", "Europe/Paris"), ("local_time", european_time)])
  azimuth::Counter::add(timezone_counter, 1, [("timezone", "Asia/Tokyo"), ("local_time", asian_time)])
  azimuth::Counter::add(timezone_counter, 1, [("timezone", "Australia/Sydney"), ("local_time", australian_time)])
  
  // Test daylight saving time transitions
  let dst_transition_spring = azimuth::Timezone::get_dst_transition(us_eastern, 2023, 3)  // Spring forward
  let dst_transition_fall = azimuth::Timezone::get_dst_transition(us_eastern, 2023, 11)  // Fall back
  
  assert_true(dst_transition_spring.is_some)
  assert_true(dst_transition_fall.is_some)
  
  match dst_transition_spring {
    Some(transition) => {
      assert_eq(transition.month, 3)
      assert_true(transition.hour >= 2 && transition.hour <= 3)
    }
    None => assert_true(false)
  }
  
  match dst_transition_fall {
    Some(transition) => {
      assert_eq(transition.month, 11)
      assert_true(transition.hour >= 1 && transition.hour <= 2)
    }
    None => assert_true(false)
  }
}

// Test 5: Cultural and regional conventions
pub test "cultural and regional conventions" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "cultural-test")
  
  // Test address formatting for different regions
  let us_address = azimuth::AddressFormatter::format("US", [
    ("street", "123 Main St"),
    ("city", "New York"),
    ("state", "NY"),
    ("postal", "10001"),
    ("country", "USA")
  ])
  
  let uk_address = azimuth::AddressFormatter::format("GB", [
    ("street", "123 High Street"),
    ("city", "London"),
    ("postal", "SW1A 1AA"),
    ("country", "UK")
  ])
  
  let japanese_address = azimuth::AddressFormatter::format("JP", [
    ("postal", "100-0001"),
    ("prefecture", "東京都"),
    ("city", "千代田区"),
    ("street", "千代田1-1"),
    ("country", "日本")
  ])
  
  let chinese_address = azimuth::AddressFormatter::format("CN", [
    ("country", "中国"),
    ("province", "北京市"),
    ("city", "北京市"),
    ("district", "东城区"),
    ("street", "东华门大街1号"),
    ("postal", "100006")
  ])
  
  // Verify address formatting follows regional conventions
  assert_true(us_address.contains("123 Main St"))
  assert_true(us_address.contains("New York, NY 10001"))
  
  assert_true(uk_address.contains("123 High Street"))
  assert_true(uk_address.contains("London"))
  assert_true(uk_address.contains("SW1A 1AA"))
  
  assert_true(japanese_address.contains("〒100-0001"))
  assert_true(japanese_address.contains("東京都千代田区"))
  
  assert_true(chinese_address.contains("中国北京市"))
  assert_true(chinese_address.contains("东城区"))
  
  // Test personal name formatting
  let western_name = azimuth::NameFormatter::format("western", [
    ("first", "John"),
    ("middle", "F."),
    ("last", "Kennedy"),
    ("title", "Mr.")
  ])
  
  let eastern_name = azimuth::NameFormatter::format("eastern", [
    ("family", "田中"),
    ("given", "太郎"),
    ("title", "様")
  ])
  
  let chinese_name = azimuth::NameFormatter::format("chinese", [
    ("family", "张"),
    ("given", "伟"),
    ("title", "先生")
  ])
  
  // Verify name formatting follows cultural conventions
  assert_true(western_name.contains("John F. Kennedy") || western_name.contains("Kennedy, John F."))
  assert_true(eastern_name.contains("田中太郎"))
  assert_true(chinese_name.contains("张伟"))
  
  // Test measurement units and conversions
  let metric_height = azimuth::UnitConverter::convert(180.0, "cm", "m")
  let imperial_height = azimuth::UnitConverter::convert(180.0, "cm", "ft")
  let metric_weight = azimuth::UnitConverter::convert(70.0, "kg", "g")
  let imperial_weight = azimuth::UnitConverter::convert(70.0, "kg", "lb")
  let metric_temp = azimuth::UnitConverter::convert(25.0, "C", "K")
  let imperial_temp = azimuth::UnitConverter::convert(25.0, "C", "F")
  
  // Verify unit conversions
  assert_eq(metric_height, 1.8)
  assert_true(imperial_height > 5.0 && imperial_height < 7.0)  // ~5.9 feet
  assert_eq(metric_weight, 70000.0)
  assert_true(imperial_weight > 150.0 && imperial_weight < 160.0)  // ~154 lbs
  assert_eq(metric_temp, 298.15)  // 25°C = 298.15K
  assert_eq(imperial_temp, 77.0)  // 25°C = 77°F
  
  // Create metrics with cultural context
  let cultural_counter = azimuth::Meter::create_counter(meter, "cultural.interactions", Some("文化交流计数器"), Some("计数"))
  
  azimuth::Counter::add(cultural_counter, 1, [
    ("region", "US"),
    ("language", "en"),
    ("date_format", "MM/DD/YYYY"),
    ("measurement_system", "imperial")
  ])
  
  azimuth::Counter::add(cultural_counter, 1, [
    ("region", "JP"),
    ("language", "ja"),
    ("date_format", "YYYY/MM/DD"),
    ("measurement_system", "metric")
  ])
  
  azimuth::Counter::add(cultural_counter, 1, [
    ("region", "CN"),
    ("language", "zh"),
    ("date_format", "YYYY-MM-DD"),
    ("measurement_system", "metric")
  ])
  
  azimuth::Counter::add(cultural_counter, 1, [
    ("region", "GB"),
    ("language", "en"),
    ("date_format", "DD/MM/YYYY"),
    ("measurement_system", "metric")
  ])
}