// Azimuth Internationalization Support Comprehensive Tests
// This file contains comprehensive test cases for internationalization support

// Test 1: Multi-language Attribute Support
test "multi-language attribute support" {
  let tracer = TracerProvider::get_tracer("i18n_test")
  let i18n_manager = InternationalizationManager::new()
  
  // Configure supported locales
  InternationalizationManager::add_locale(i18n_manager, "en-US")
  InternationalizationManager::add_locale(i18n_manager, "zh-CN")
  InternationalizationManager::add_locale(i18n_manager, "ja-JP")
  InternationalizationManager::add_locale(i18n_manager, "es-ES")
  InternationalizationManager::add_locale(i18n_manager, "fr-FR")
  InternationalizationManager::add_locale(i18n_manager, "de-DE")
  InternationalizationManager::add_locale(i18n_manager, "ru-RU")
  InternationalizationManager::add_locale(i18n_manager, "ar-SA")
  
  // Create span with multi-language attributes
  let span = Tracer::start_span(tracer, "internationalization_test")
  
  // Add attributes in different languages
  Span::set_attribute(span, "error.message.en", StringValue("Invalid user credentials"))
  Span::set_attribute(span, "error.message.zh-CN", StringValue("用户凭据无效"))
  Span::set_attribute(span, "error.message.ja-JP", StringValue("ユーザー資格情報が無効です"))
  Span::set_attribute(span, "error.message.es-ES", StringValue("Credenciales de usuario inválidas"))
  Span::set_attribute(span, "error.message.fr-FR", StringValue("Identifiants utilisateur invalides"))
  Span::set_attribute(span, "error.message.de-DE", StringValue("Ungültige Benutzeranmeldeinformationen"))
  Span::set_attribute(span, "error.message.ru-RU", StringValue("Недействительные учетные данные пользователя"))
  Span::set_attribute(span, "error.message.ar-SA", StringValue("بيانات اعتماد المستخدم غير صالحة"))
  
  // Add service name in different languages
  Span::set_attribute(span, "service.name.en", StringValue("Authentication Service"))
  Span::set_attribute(span, "service.name.zh-CN", StringValue("认证服务"))
  Span::set_attribute(span, "service.name.ja-JP", StringValue("認証サービス"))
  Span::set_attribute(span, "service.name.es-ES", StringValue("Servicio de Autenticación"))
  
  // Test attribute retrieval with locale fallback
  let en_message = InternationalizationManager::get_attribute(i18n_manager, span, "error.message", "en-US")
  match en_message {
    Some(StringValue(message)) => assert_eq(message, "Invalid user credentials")
    _ => assert_true(false)
  }
  
  let zh_message = InternationalizationManager::get_attribute(i18n_manager, span, "error.message", "zh-CN")
  match zh_message {
    Some(StringValue(message)) => assert_eq(message, "用户凭据无效")
    _ => assert_true(false)
  }
  
  // Test fallback to default locale when specific locale not available
  let pt_message = InternationalizationManager::get_attribute(i18n_manager, span, "error.message", "pt-BR")
  match pt_message {
    Some(StringValue(message)) => assert_eq(message, "Invalid user credentials") // Fallback to en-US
    _ => assert_true(false)
  }
  
  Span::end(span)
}

// Test 2: Unicode and Special Character Support
test "unicode and special character support" {
  let tracer = TracerProvider::get_tracer("unicode_test")
  
  // Create span with Unicode content
  let span = Tracer::start_span(tracer, "unicode_test_span")
  
  // Test various Unicode characters
  let unicode_texts = [
    ("emoji.test", StringValue("Test with emojis: 🚀 🔥 🎉 ✨")),
    ("chinese.test", StringValue("中文测试：你好世界！欢迎使用遥测系统")),
    ("japanese.test", StringValue("日本語テスト：こんにちは世界！テレメトリシステムへようこそ")),
    ("korean.test", StringValue("한국어 테스트: 안녕하세요 세계! 원격 측정 시스템에 오신 것을 환영합니다")),
    ("arabic.test", StringValue("اختبار عربي: مرحبا بالعالم! أهلا بكم في نظام القياس عن بعد")),
    ("russian.test", StringValue("Тест на русском: Привет мир! Добро пожаловать в систему телеметрии")),
    ("hebrew.test", StringValue("בדיקה בעברית: שלום עולם! ברוכים הבאים למערכת הטלמטריה")),
    ("thai.test", StringValue("การทดสอบภาษาไทย: สวัสดีชาวโลก! ยินดีต้อนรับสู่ระบบติดตามระยะไกล")),
    ("math.symbols", StringValue("Math symbols: ∑ ∏ ∫ ∂ ∇ ∆ ∞ ≤ ≥ ≠ ≈ ≡")),
    ("currency.symbols", StringValue("Currency: $ € £ ¥ ₩ ₽ ₪ ₫ ₡ ₦ ₨ ₩")),
    ("special.chars", StringValue("Special: © ® ™ § ¶ † ‡ • … ‰ ′ ″ ※"))
  ]
  
  for (key, value) in unicode_texts {
    Span::set_attribute(span, key, value)
  }
  
  // Verify Unicode attributes are stored and retrieved correctly
  for (key, expected_value) in unicode_texts {
    let retrieved_value = Span::get_attribute(span, key)
    match retrieved_value {
      Some(StringValue(value)) => assert_eq(value, expected_value)
      _ => assert_true(false, "Should retrieve Unicode attribute correctly")
    }
  }
  
  // Test Unicode in span names
  let unicode_span = Tracer::start_span(tracer, "Unicode测试🌟")
  assert_eq(Span::name(unicode_span), "Unicode测试🌟")
  Span::end(unicode_span)
  
  // Test Unicode in events
  Span::add_event(span, "Unicode事件🎯", Some([
    ("message", StringValue("这是一个包含Unicode的事件消息")),
    ("emoji", StringValue("🎉🚀✨"))
  ]))
  
  Span::end(span)
}

// Test 3: Locale-specific Formatting
test "locale-specific formatting" {
  let formatter = LocaleFormatter::new()
  let tracer = TracerProvider::get_tracer("locale_formatting_test")
  
  // Create span with locale-specific formatted data
  let span = Tracer::start_span(tracer, "locale_formatting_test")
  
  // Test number formatting
  let number = 1234567.89
  
  let us_formatted = LocaleFormatter::format_number(formatter, number, "en-US")
  assert_eq(us_formatted, "1,234,567.89")
  
  let german_formatted = LocaleFormatter::format_number(formatter, number, "de-DE")
  assert_eq(german_formatted, "1.234.567,89")
  
  let french_formatted = LocaleFormatter::format_number(formatter, number, "fr-FR")
  assert_eq(french_formatted, "1 234 567,89")
  
  let chinese_formatted = LocaleFormatter::format_number(formatter, number, "zh-CN")
  assert_eq(chinese_formatted, "1,234,567.89")
  
  // Add formatted numbers as attributes
  Span::set_attribute(span, "amount.en-US", StringValue(us_formatted))
  Span::set_attribute(span, "amount.de-DE", StringValue(german_formatted))
  Span::set_attribute(span, "amount.fr-FR", StringValue(french_formatted))
  Span::set_attribute(span, "amount.zh-CN", StringValue(chinese_formatted))
  
  // Test date/time formatting
  let timestamp = 1609459200L // 2021-01-01 00:00:00 UTC
  
  let us_date = LocaleFormatter::format_date(formatter, timestamp, "en-US")
  assert_eq(us_date, "1/1/2021")
  
  let iso_date = LocaleFormatter::format_date(formatter, timestamp, "en-GB")
  assert_eq(iso_date, "01/01/2021")
  
  let chinese_date = LocaleFormatter::format_date(formatter, timestamp, "zh-CN")
  assert_eq(chinese_date, "2021/1/1")
  
  let japanese_date = LocaleFormatter::format_date(formatter, timestamp, "ja-JP")
  assert_eq(japanese_date, "2021/01/01")
  
  // Add formatted dates as attributes
  Span::set_attribute(span, "date.en-US", StringValue(us_date))
  Span::set_attribute(span, "date.en-GB", StringValue(iso_date))
  Span::set_attribute(span, "date.zh-CN", StringValue(chinese_date))
  Span::set_attribute(span, "date.ja-JP", StringValue(japanese_date))
  
  // Test currency formatting
  let currency_amount = 1234.56
  
  let us_currency = LocaleFormatter::format_currency(formatter, currency_amount, "USD", "en-US")
  assert_eq(us_currency, "$1,234.56")
  
  let euro_currency = LocaleFormatter::format_currency(formatter, currency_amount, "EUR", "de-DE")
  assert_eq(euro_currency, "1.234,56 €")
  
  let yen_currency = LocaleFormatter::format_currency(formatter, currency_amount, "JPY", "ja-JP")
  assert_eq(yen_currency, "¥1,235") // Japanese yen doesn't show decimal places
  
  // Add formatted currencies as attributes
  Span::set_attribute(span, "price.USD", StringValue(us_currency))
  Span::set_attribute(span, "price.EUR", StringValue(euro_currency))
  Span::set_attribute(span, "price.JPY", StringValue(yen_currency))
  
  Span::end(span)
}

// Test 4: Right-to-Left Language Support
test "right-to-left language support" {
  let tracer = TracerProvider::get_tracer("rtl_test")
  let rtl_processor = RTLProcessor::new()
  
  // Create span with RTL content
  let span = Tracer::start_span(tracer, "rtl_test_span")
  
  // Test Arabic content
  let arabic_text = "مرحبا بكم في نظام المراقبة"
  let processed_arabic = RTLProcessor::process(rtl_processor, arabic_text, "ar-SA")
  Span::set_attribute(span, "message.ar", StringValue(processed_arabic))
  
  // Test Hebrew content
  let hebrew_text = "שלום וברכה למערכת המעקב"
  let processed_hebrew = RTLProcessor::process(rtl_processor, hebrew_text, "he-IL")
  Span::set_attribute(span, "message.he", StringValue(processed_hebrew))
  
  // Test mixed LTR/RTL content
  let mixed_text = "Hello مرحبا World عالم"
  let processed_mixed = RTLProcessor::process(rtl_processor, mixed_text, "en-US")
  Span::set_attribute(span, "message.mixed", StringValue(processed_mixed))
  
  // Test RTL in events
  Span::add_event(span, "حدث باللغة العربية", Some([
    ("message.ar", StringValue("تمت العملية بنجاح")),
    ("status.ar", StringValue("نجاح")),
    ("user.ar", StringValue("المستخدم_١٢٣"))
  ]))
  
  // Test RTL in span names
  let rtl_span = Tracer::start_span(tracer, "عملية باللغة العربية")
  assert_eq(Span::name(rtl_span), "عملية باللغة العربية")
  Span::end(rtl_span)
  
  Span::end(span)
}

// Test 5: Collation and Sorting
test "collation and sorting" {
  let collator = LocaleCollator::new()
  let tracer = TracerProvider::get_tracer("collation_test")
  
  // Create span with sortable multilingual content
  let span = Tracer::start_span(tracer, "collation_test_span")
  
  // Test strings in different languages
  let test_strings = [
    ("en", ["apple", "banana", "cherry", "date"]),
    ("zh-CN", ["苹果", "香蕉", "樱桃", "枣子"]),
    ("ja-JP", ["りんご", "バナナ", "チェリー", "デート"]),
    ("ar-SA", ["تفاح", "موز", "كرز", "تمر"]),
    ("ru-RU", ["яблоко", "банан", "вишня", "финик"])
  ]
  
  // Test sorting for each locale
  for (locale, strings) in test_strings {
    let sorted_strings = LocaleCollator::sort(collator, strings, locale)
    
    // Add sorted strings as attributes
    for i in 0..=sorted_strings.length() - 1 {
      let key = "sorted." + locale + "." + i.to_string()
      Span::set_attribute(span, key, StringValue(sorted_strings[i]))
    }
  }
  
  // Test specific collation rules
  let chinese_strings = ["北京", "上海", "广州", "深圳"]
  let chinese_sorted_pinyin = LocaleCollator::sort_with_rules(collator, chinese_strings, "zh-CN", "pinyin")
  let chinese_sorted_stroke = LocaleCollator::sort_with_rules(collator, chinese_strings, "zh-CN", "stroke")
  
  // Add different sorting results
  for i in 0..=chinese_sorted_pinyin.length() - 1 {
    Span::set_attribute(span, "chinese.pinyin." + i.to_string(), StringValue(chinese_sorted_pinyin[i]))
    Span::set_attribute(span, "chinese.stroke." + i.to_string(), StringValue(chinese_sorted_stroke[i]))
  }
  
  Span::end(span)
}

// Test 6: Timezone and Localization
test "timezone and localization" {
  let timezone_manager = TimezoneManager::new()
  let tracer = TracerProvider::get_tracer("timezone_test")
  
  // Create span with timezone-specific data
  let span = Tracer::start_span(tracer, "timezone_test_span")
  
  // Test timestamp in different timezones
  let utc_timestamp = 1609459200L // 2021-01-01 00:00:00 UTC
  
  let timezones = [
    ("UTC", "UTC"),
    ("US_Eastern", "America/New_York"),
    ("US_Pacific", "America/Los_Angeles"),
    ("Europe_London", "Europe/London"),
    ("Europe_Paris", "Europe/Paris"),
    ("Asia_Tokyo", "Asia/Tokyo"),
    ("Asia_Shanghai", "Asia/Shanghai"),
    ("Australia_Sydney", "Australia/Sydney")
  ]
  
  for (name, timezone) in timezones {
    let localized_time = TimezoneManager::format_timestamp(timezone_manager, utc_timestamp, timezone, "en-US")
    Span::set_attribute(span, "timestamp." + name, StringValue(localized_time))
  }
  
  // Test localized date formats
  let locales = ["en-US", "en-GB", "zh-CN", "ja-JP", "fr-FR", "de-DE", "es-ES", "ru-RU"]
  
  for locale in locales {
    let localized_date = TimezoneManager::format_date(timezone_manager, utc_timestamp, locale)
    Span::set_attribute(span, "date." + locale, StringValue(localized_date))
    
    let localized_time = TimezoneManager::format_time(timezone_manager, utc_timestamp, locale)
    Span::set_attribute(span, "time." + locale, StringValue(localized_time))
  }
  
  // Test timezone conversion
  let source_timezone = "America/New_York"
  let target_timezone = "Asia/Tokyo"
  let ny_timestamp = 1609459200L // 2021-01-01 00:00:00 EST
  
  let tokyo_timestamp = TimezoneManager::convert_timezone(timezone_manager, ny_timestamp, source_timezone, target_timezone)
  
  Span::set_attribute(span, "converted.timestamp", StringValue(tokyo_timestamp.to_string()))
  Span::set_attribute(span, "source.timezone", StringValue(source_timezone))
  Span::set_attribute(span, "target.timezone", StringValue(target_timezone))
  
  Span::end(span)
}

// Test 7: Cultural and Regional Formatting
test "cultural and regional formatting" {
  let cultural_formatter = CulturalFormatter::new()
  let tracer = TracerProvider::get_tracer("cultural_formatting_test")
  
  // Create span with culturally formatted content
  let span = Tracer::start_span(tracer, "cultural_formatting_test_span")
  
  // Test address formatting
  let address_components = AddressComponents {
    street: "123 Main St",
    city: "New York",
    state: "NY",
    postal_code: "10001",
    country: "USA"
  }
  
  let us_address = CulturalFormatter::format_address(cultural_formatter, address_components, "en-US")
  let uk_address = CulturalFormatter::format_address(cultural_formatter, address_components, "en-GB")
  let japanese_address = CulturalFormatter::format_address(cultural_formatter, address_components, "ja-JP")
  
  Span::set_attribute(span, "address.en-US", StringValue(us_address))
  Span::set_attribute(span, "address.en-GB", StringValue(uk_address))
  Span::set_attribute(span, "address.ja-JP", StringValue(japanese_address))
  
  // Test name formatting
  let name_components = NameComponents {
    first_name: "John",
    middle_name: "William",
    last_name: "Smith",
    title: "Dr.",
    suffix: "Jr."
  }
  
  let western_name = CulturalFormatter::format_name(cultural_formatter, name_components, "en-US")
  let eastern_name = CulturalFormatter::format_name(cultural_formatter, name_components, "ja-JP")
  let formal_name = CulturalFormatter::format_name(cultural_formatter, name_components, "de-DE")
  
  Span::set_attribute(span, "name.western", StringValue(western_name))
  Span::set_attribute(span, "name.eastern", StringValue(eastern_name))
  Span::set_attribute(span, "name.formal", StringValue(formal_name))
  
  // Test phone number formatting
  let phone_number = "+12125551234"
  
  let us_phone = CulturalFormatter::format_phone(cultural_formatter, phone_number, "en-US")
  let uk_phone = CulturalFormatter::format_phone(cultural_formatter, phone_number, "en-GB")
  let japanese_phone = CulturalFormatter::format_phone(cultural_formatter, phone_number, "ja-JP")
  
  Span::set_attribute(span, "phone.en-US", StringValue(us_phone))
  Span::set_attribute(span, "phone.en-GB", StringValue(uk_phone))
  Span::set_attribute(span, "phone.ja-JP", StringValue(japanese_phone))
  
  // Test measurement units
  let measurements = [
    ("length.meters", 1.75, ["m", "ft", "cm"]),
    ("weight.kg", 70.5, ["kg", "lbs", "stones"]),
    ("temperature.celsius", 20.0, ["°C", "°F", "K"])
  ]
  
  for (name, value, units) in measurements {
    for unit in units {
      let formatted_value = CulturalFormatter::format_measurement(cultural_formatter, value, unit, "en-US")
      Span::set_attribute(span, name + "." + unit, StringValue(formatted_value))
    }
  }
  
  Span::end(span)
}