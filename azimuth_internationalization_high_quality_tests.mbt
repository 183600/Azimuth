// Azimuth Telemetry System - High Quality Internationalization Support Tests
// This file contains comprehensive internationalization support test cases

// Test 1: Basic Unicode and Multilingual Support
test "basic unicode and multilingual support" {
  let attrs = Attributes::new()
  
  // Test Unicode characters in attribute keys and values
  let unicode_key = "测试键_🚀_テストキириллица"
  let unicode_value = "测试值_🌟_テスト値кириллица"
  
  Attributes::set(attrs, unicode_key, StringValue(unicode_value))
  let result = Attributes::get(attrs, unicode_key)
  
  match result {
    Some(StringValue(v)) => assert_eq(v, unicode_value)
    None => assert_true(false)
  }
  
  // Test various language scripts
  let chinese_text = "中文测试"
  let japanese_text = "日本語テスト"
  let korean_text = "한국어 테스트"
  let arabic_text = "اختبار عربي"
  let hebrew_text = "בדיקה עברית"
  let russian_text = "Русский тест"
  let hindi_text = "हिंदी परीक्षण"
  let thai_text = "การทดสอบภาษาไทย"
  
  // Set multilingual attributes
  Attributes::set(attrs, "chinese.text", StringValue(chinese_text))
  Attributes::set(attrs, "japanese.text", StringValue(japanese_text))
  Attributes::set(attrs, "korean.text", StringValue(korean_text))
  Attributes::set(attrs, "arabic.text", StringValue(arabic_text))
  Attributes::set(attrs, "hebrew.text", StringValue(hebrew_text))
  Attributes::set(attrs, "russian.text", StringValue(russian_text))
  Attributes::set(attrs, "hindi.text", StringValue(hindi_text))
  Attributes::set(attrs, "thai.text", StringValue(thai_text))
  
  // Verify all multilingual attributes
  let chinese_result = Attributes::get(attrs, "chinese.text")
  match chinese_result {
    Some(StringValue(v)) => assert_eq(v, chinese_text)
    None => assert_true(false)
  }
  
  let japanese_result = Attributes::get(attrs, "japanese.text")
  match japanese_result {
    Some(StringValue(v)) => assert_eq(v, japanese_text)
    None => assert_true(false)
  }
  
  let arabic_result = Attributes::get(attrs, "arabic.text")
  match arabic_result {
    Some(StringValue(v)) => assert_eq(v, arabic_text)
    None => assert_true(false)
  }
  
  // Test right-to-left languages
  let rtl_text = "עברית וערבית הן שפות ימין-לשמאל"
  Attributes::set(attrs, "rtl.text", StringValue(rtl_text))
  
  let rtl_result = Attributes::get(attrs, "rtl.text")
  match rtl_result {
    Some(StringValue(v)) => assert_eq(v, rtl_text)
    None => assert_true(false)
  }
}

// Test 2: Locale-Aware Formatting
test "locale aware formatting" {
  let formatter = Formatter::new()
  
  // Test date formatting with different locales
  let timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  // English (US) locale
  Formatter::set_locale(formatter, "en-US")
  let en_us_date = Formatter::format_date(formatter, timestamp)
  assert_true(en_us_date.contains("2021") && en_us_date.contains("1"))
  
  // German locale
  Formatter::set_locale(formatter, "de-DE")
  let de_de_date = Formatter::format_date(formatter, timestamp)
  assert_true(de_de_date.contains("2021") && de_de_date.contains("1"))
  
  // Japanese locale
  Formatter::set_locale(formatter, "ja-JP")
  let ja_jp_date = Formatter::format_date(formatter, timestamp)
  assert_true(ja_jp_date.contains("2021") && ja_jp_date.contains("1"))
  
  // Test number formatting with different locales
  let number = 1234567.89
  
  // English (US) locale
  Formatter::set_locale(formatter, "en-US")
  let en_us_number = Formatter::format_number(formatter, number)
  assert_true(en_us_number.contains("1,234,567.89"))
  
  // German locale
  Formatter::set_locale(formatter, "de-DE")
  let de_de_number = Formatter::format_number(formatter, number)
  assert_true(de_de_number.contains("1.234.567,89"))
  
  // French locale
  Formatter::set_locale(formatter, "fr-FR")
  let fr_fr_number = Formatter::format_number(formatter, number)
  assert_true(fr_fr_number.contains("1 234 567,89"))
  
  // Test currency formatting
  let currency = 1234.56
  
  // English (US) locale
  Formatter::set_locale(formatter, "en-US")
  let en_us_currency = Formatter::format_currency(formatter, currency, "USD")
  assert_true(en_us_currency.contains("$1,234.56"))
  
  // Euro with German locale
  Formatter::set_locale(formatter, "de-DE")
  let de_de_currency = Formatter::format_currency(formatter, currency, "EUR")
  assert_true(de_de_currency.contains("1.234,56 €"))
  
  // Japanese Yen
  Formatter::set_locale(formatter, "ja-JP")
  let ja_jp_currency = Formatter::format_currency(formatter, currency, "JPY")
  assert_true(ja_jp_currency.contains("¥1,235") // Yen doesn't use decimals
}

// Test 3: Localized Error Messages
test "localized error messages" {
  let error_manager = ErrorManager::new()
  
  // Test error messages in different languages
  let error_code = "CONNECTION_FAILED"
  
  // English error message
  ErrorManager::set_locale(error_manager, "en")
  let en_error = ErrorManager::get_error_message(error_manager, error_code)
  assert_eq(en_error, "Connection to server failed")
  
  // Spanish error message
  ErrorManager::set_locale(error_manager, "es")
  let es_error = ErrorManager::get_error_message(error_manager, error_code)
  assert_eq(es_error, "La conexión con el servidor falló")
  
  // French error message
  ErrorManager::set_locale(error_manager, "fr")
  let fr_error = ErrorManager::get_error_message(error_manager, error_code)
  assert_eq(fr_error, "La connexion au serveur a échoué")
  
  // German error message
  ErrorManager::set_locale(error_manager, "de")
  let de_error = ErrorManager::get_error_message(error_manager, error_code)
  assert_eq(de_error, "Verbindung zum Server fehlgeschlagen")
  
  // Japanese error message
  ErrorManager::set_locale(error_manager, "ja")
  let ja_error = ErrorManager::get_error_message(error_manager, error_code)
  assert_eq(ja_error, "サーバーへの接続に失敗しました")
  
  // Chinese error message
  ErrorManager::set_locale(error_manager, "zh")
  let zh_error = ErrorManager::get_error_message(error_manager, error_code)
  assert_eq(zh_error, "连接服务器失败")
  
  // Test error message with parameters
  let param_error_code = "FILE_NOT_FOUND"
  let params = [("filename", "config.json")]
  
  // English with parameters
  ErrorManager::set_locale(error_manager, "en")
  let en_param_error = ErrorManager::get_error_message_with_params(error_manager, param_error_code, params)
  assert_eq(en_param_error, "File 'config.json' not found")
  
  // Spanish with parameters
  ErrorManager::set_locale(error_manager, "es")
  let es_param_error = ErrorManager::get_error_message_with_params(error_manager, param_error_code, params)
  assert_eq(es_param_error, "Archivo 'config.json' no encontrado")
  
  // Test fallback to default locale
  ErrorManager::set_locale(error_manager, "unknown-locale")
  let fallback_error = ErrorManager::get_error_message(error_manager, error_code)
  assert_eq(fallback_error, "Connection to server failed") // Should fallback to English
}

// Test 4: Localized Log Messages
test "localized log messages" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "i18n_test_logger")
  
  // Create localized log messages
  let log_templates = LogTemplateManager::new()
  
  // Add templates in different languages
  LogTemplateManager::add_template(log_templates, "en", "user.login", "User '{username}' logged in from {ip}")
  LogTemplateManager::add_template(log_templates, "es", "user.login", "Usuario '{username}' inició sesión desde {ip}")
  LogTemplateManager::add_template(log_templates, "fr", "user.login", "Utilisateur '{username}' s'est connecté depuis {ip}")
  LogTemplateManager::add_template(log_templates, "ja", "user.login", "ユーザー'{username}'が{ip}からログインしました")
  LogTemplateManager::add_template(log_templates, "zh", "user.login", "用户'{username}'从{ip}登录")
  
  // Test localized log generation
  let params = [("username", "testuser"), ("ip", "192.168.1.100")]
  
  // English log
  LogTemplateManager::set_locale(log_templates, "en")
  let en_log_message = LogTemplateManager::format_message(log_templates, "user.login", params)
  assert_eq(en_log_message, "User 'testuser' logged in from 192.168.1.100")
  
  let en_log_record = LogRecord::new(Info, en_log_message)
  Logger::emit(logger, en_log_record)
  
  // Spanish log
  LogTemplateManager::set_locale(log_templates, "es")
  let es_log_message = LogTemplateManager::format_message(log_templates, "user.login", params)
  assert_eq(es_log_message, "Usuario 'testuser' inició sesión desde 192.168.1.100")
  
  let es_log_record = LogRecord::new(Info, es_log_message)
  Logger::emit(logger, es_log_record)
  
  // Japanese log
  LogTemplateManager::set_locale(log_templates, "ja")
  let ja_log_message = LogTemplateManager::format_message(log_templates, "user.login", params)
  assert_eq(ja_log_message, "ユーザー'testuser'が192.168.1.100からログインしました")
  
  let ja_log_record = LogRecord::new(Info, ja_log_message)
  Logger::emit(logger, ja_log_record)
  
  // Test localized log levels
  let localized_levels = [
    ("en", [("DEBUG", "Debug"), ("INFO", "Information"), ("WARN", "Warning"), ("ERROR", "Error")]),
    ("es", [("DEBUG", "Depuración"), ("INFO", "Información"), ("WARN", "Advertencia"), ("ERROR", "Error")]),
    ("fr", [("DEBUG", "Débogage"), ("INFO", "Information"), ("WARN", "Avertissement"), ("ERROR", "Erreur")]),
    ("ja", [("DEBUG", "デバッグ"), ("INFO", "情報"), ("WARN", "警告"), ("ERROR", "エラー")]),
    ("zh", [("DEBUG", "调试"), ("INFO", "信息"), ("WARN", "警告"), ("ERROR", "错误")])
  ]
  
  for (locale, levels) in localized_levels {
    LogTemplateManager::set_locale(log_templates, locale)
    
    for (level_key, localized_level) in levels {
      let formatted_level = LogTemplateManager::format_log_level(log_templates, level_key)
      assert_eq(formatted_level, localized_level)
    }
  }
}

// Test 5: Time Zone Support
test "time zone support" {
  let time_formatter = TimeFormatter::new()
  
  // Test timestamp
  let timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  // UTC timezone
  TimeFormatter::set_timezone(time_formatter, "UTC")
  let utc_time = TimeFormatter::format_datetime(time_formatter, timestamp)
  assert_true(utc_time.contains("2021-01-01") && utc_time.contains("00:00:00"))
  
  // US Eastern timezone
  TimeFormatter::set_timezone(time_formatter, "America/New_York")
  let est_time = TimeFormatter::format_datetime(time_formatter, timestamp)
  assert_true(est_time.contains("2020-12-31")) // Should be previous day in EST
  
  // Japan timezone
  TimeFormatter::set_timezone(time_formatter, "Asia/Tokyo")
  let jst_time = TimeFormatter::format_datetime(time_formatter, timestamp)
  assert_true(jst_time.contains("2021-01-01") && jst_time.contains("09:00:00")) // Should be 9:00 AM in JST
  
  // Test timezone conversion
  let source_timestamp = 1640995200000L // 2022-01-01 00:00:00 UTC
  let source_tz = "UTC"
  let target_tz = "Asia/Shanghai"
  
  let converted_time = TimeFormatter::convert_timezone(time_formatter, source_timestamp, source_tz, target_tz)
  assert_true(converted_time.contains("2022-01-01") && converted_time.contains("08:00:00")) // Should be 8:00 AM in Shanghai
  
  // Test timezone-aware parsing
  let datetime_string = "2021-12-31 23:00:00"
  let parse_tz = "America/Los_Angeles"
  
  let parsed_timestamp = TimeFormatter::parse_datetime(time_formatter, datetime_string, parse_tz)
  assert_true(parsed_timestamp > 0)
  
  // Verify parsed timestamp represents the correct moment in time
  TimeFormatter::set_timezone(time_formatter, "UTC")
  let utc_equivalent = TimeFormatter::format_datetime(time_formatter, parsed_timestamp)
  assert_true(utc_equivalent.contains("2022-01-01")) // Should be next day in UTC
}

// Test 6: Cultural and Regional Formatting
test "cultural and regional formatting" {
  let cultural_formatter = CulturalFormatter::new()
  
  // Test name formatting
  let given_name = "John"
  let family_name = "Doe"
  
  // Western name format
  CulturalFormatter::set_region(cultural_formatter, "US")
  let western_name = CulturalFormatter::format_name(cultural_formatter, given_name, family_name)
  assert_eq(western_name, "John Doe")
  
  // Eastern name format (family name first)
  CulturalFormatter::set_region(cultural_formatter, "JP")
  let eastern_name = CulturalFormatter::format_name(cultural_formatter, given_name, family_name)
  assert_eq(eastern_name, "Doe John")
  
  // Test address formatting
  let address_components = [
    ("street", "123 Main St"),
    ("city", "New York"),
    ("state", "NY"),
    ("postal_code", "10001"),
    ("country", "USA")
  ]
  
  // US address format
  CulturalFormatter::set_region(cultural_formatter, "US")
  let us_address = CulturalFormatter::format_address(cultural_formatter, address_components)
  assert_true(us_address.contains("123 Main St") && us_address.contains("New York, NY 10001"))
  
  // Japanese address format
  let jp_address_components = [
    ("postal_code", "100-0001"),
    ("prefecture", "東京都"),
    ("city", "千代田区"),
    ("street", "千代田1-1"),
    ("building", "皇居外苑")
  ]
  
  CulturalFormatter::set_region(cultural_formatter, "JP")
  let jp_address = CulturalFormatter::format_address(cultural_formatter, jp_address_components)
  assert_true(jp_address.contains("〒100-0001") && jp_address.contains("東京都千代田区"))
  
  // Test phone number formatting
  let phone_number = "+12125551234"
  
  // US phone format
  CulturalFormatter::set_region(cultural_formatter, "US")
  let us_phone = CulturalFormatter::format_phone(cultural_formatter, phone_number)
  assert_eq(us_phone, "+1 (212) 555-1234")
  
  // International format
  CulturalFormatter::set_region(cultural_formatter, "INTL")
  let intl_phone = CulturalFormatter::format_phone(cultural_formatter, phone_number)
  assert_eq(intl_phone, "+1 212 555 1234")
}

// Test 7: Bidirectional Text Support
test "bidirectional text support" {
  let bidi_processor = BidiProcessor::new()
  
  // Test mixed LTR and RTL text
  let mixed_text = "Hello مرحبا World عالم"
  let processed_mixed = BidiProcessor::process(bidi_processor, mixed_text)
  assert_true(processed_mixed.length() > 0)
  
  // Test RTL-only text
  let rtl_text = "مرحبا بالعالم"
  let processed_rtl = BidiProcessor::process(bidi_processor, rtl_text)
  assert_true(processed_rtl.contains("مرحبا"))
  
  // Test LTR-only text
  let ltr_text = "Hello World"
  let processed_ltr = BidiProcessor::process(bidi_processor, ltr_text)
  assert_eq(processed_ltr, "Hello World")
  
  // Test text with numbers in RTL context
  let rtl_with_numbers = "السنة 2021 والتاريخ 01/01/2021"
  let processed_rtl_numbers = BidiProcessor::process(bidi_processor, rtl_with_numbers)
  assert_true(processed_rtl_numbers.contains("2021"))
  
  // Test text with URLs in RTL context
  let rtl_with_url = "الموقع: https://example.com/path"
  let processed_rtl_url = BidiProcessor::process(bidi_processor, rtl_with_url)
  assert_true(processed_rtl_url.contains("https://example.com"))
  
  // Test text direction detection
  assert_eq(BidiProcessor::detect_direction(bidi_processor, "Hello"), "LTR")
  assert_eq(BidiProcessor::detect_direction(bidi_processor, "مرحبا"), "RTL")
  assert_eq(BidiProcessor::detect_direction(bidi_processor, "Hello مرحبا"), "MIXED")
}

// Test 8: Collation and Sorting
test "collation and sorting" {
  let collator = Collator::new()
  
  // Test sorting strings in different locales
  let strings = ["apple", "banana", "cherry", "åpple", "zebra"]
  
  // English collation
  Collator::set_locale(collator, "en-US")
  let en_sorted = Collator::sort(collator, strings)
  assert_eq(en_sorted[0], "apple")
  assert_eq(en_sorted[3], "cherry")
  assert_eq(en_sorted[4], "zebra")
  
  // Swedish collation (å comes after z)
  Collator::set_locale(collator, "sv-SE")
  let sv_sorted = Collator::sort(collator, strings)
  assert_eq(sv_sorted[0], "apple")
  assert_eq(sv_sorted[3], "zebra")
  assert_eq(sv_sorted[4], "åpple")
  
  // Test case sensitivity
  let mixed_case_strings = ["Apple", "apple", "Banana", "banana"]
  
  // Case-insensitive collation
  Collator::set_strength(collator, "SECONDARY") // Ignore case
  let case_insensitive_sorted = Collator::sort(collator, mixed_case_strings)
  assert_eq(case_insensitive_sorted[0], "Apple")
  assert_eq(case_insensitive_sorted[1], "apple")
  
  // Case-sensitive collation
  Collator::set_strength(collator, "TERTIARY") // Consider case
  let case_sensitive_sorted = Collator::sort(collator, mixed_case_strings)
  assert_eq(case_sensitive_sorted[0], "Apple")
  assert_eq(case_sensitive_sorted[2], "Banana")
  
  // Test string comparison
  Collator::set_locale(collator, "en-US")
  
  let comparison1 = Collator::compare(collator, "apple", "banana")
  assert_true(comparison1 < 0) // apple comes before banana
  
  let comparison2 = Collator::compare(collator, "zebra", "apple")
  assert_true(comparison2 > 0) // zebra comes after apple
  
  let comparison3 = Collator::compare(collator, "apple", "apple")
  assert_eq(comparison3, 0) // strings are equal
  
  // Test accented characters
  let accented_strings = ["café", "cane", "came", "cafe"]
  
  Collator::set_locale(collator, "fr-FR")
  let fr_sorted = Collator::sort(collator, accented_strings)
  // In French, accented characters are typically sorted after unaccented ones
  assert_eq(fr_sorted[0], "cane")
  assert_eq(fr_sorted[1], "came")
  assert_eq(fr_sorted[2], "cafe")
  assert_eq(fr_sorted[3], "café")
}

// Test 9: Measurement Units and Localization
test "measurement units and localization" {
  let unit_converter = UnitConverter::new()
  
  // Test length conversion
  let meters = 100.0
  
  // Convert to feet (imperial)
  let feet = UnitConverter::convert_length(unit_converter, meters, "m", "ft")
  assert_true(abs(feet - 328.084) < 0.001)
  
  // Convert to miles
  let miles = UnitConverter::convert_length(unit_converter, meters, "m", "mi")
  assert_true(abs(miles - 0.0621371) < 0.0001)
  
  // Test weight conversion
  let kilograms = 70.0
  
  // Convert to pounds
  let pounds = UnitConverter::convert_weight(unit_converter, kilograms, "kg", "lb")
  assert_true(abs(pounds - 154.324) < 0.001)
  
  // Convert to stones (UK)
  let stones = UnitConverter::convert_weight(unit_converter, kilograms, "kg", "st")
  assert_true(abs(stones - 11.0231) < 0.0001)
  
  // Test temperature conversion
  let celsius = 25.0
  
  // Convert to Fahrenheit
  let fahrenheit = UnitConverter::convert_temperature(unit_converter, celsius, "C", "F")
  assert_true(abs(fahrenheit - 77.0) < 0.001)
  
  // Convert to Kelvin
  let kelvin = UnitConverter::convert_temperature(unit_converter, celsius, "C", "K")
  assert_true(abs(kelvin - 298.15) < 0.001)
  
  // Test localized unit formatting
  let formatter = UnitFormatter::new()
  
  // US customary units
  UnitFormatter::set_locale(formatter, "en-US")
  let us_length = UnitFormatter::format_length(formatter, 1000.0, "m")
  assert_true(us_length.contains("3,280.84") && us_length.contains("ft"))
  
  let us_weight = UnitFormatter::format_weight(formatter, 70.0, "kg")
  assert_true(us_weight.contains("154.32") && us_weight.contains("lb"))
  
  // Metric units
  UnitFormatter::set_locale(formatter, "de-DE")
  let de_length = UnitFormatter::format_length(formatter, 1000.0, "m")
  assert_true(de_length.contains("1.000") && de_length.contains("m"))
  
  let de_weight = UnitFormatter::format_weight(formatter, 70.0, "kg")
  assert_true(de_weight.contains("70") && de_weight.contains("kg"))
  
  // UK units (mix of metric and imperial)
  UnitFormatter::set_locale(formatter, "en-GB")
  let uk_length = UnitFormatter::format_length(formatter, 1000.0, "m")
  assert_true(uk_length.contains("km")) // UK typically uses kilometers for longer distances
  
  let uk_weight = UnitFormatter::format_weight(formatter, 70.0, "kg")
  assert_true(uk_weight.contains("70") && uk_weight.contains("kg")) // UK typically uses kilograms
}

// Test 10: International Resource Bundle Management
test "international resource bundle management" {
  let bundle_manager = ResourceBundleManager::new()
  
  // Load resource bundles for different locales
  let en_bundle = ResourceBundleManager::load_bundle(bundle_manager, "messages", "en")
  let es_bundle = ResourceBundleManager::load_bundle(bundle_manager, "messages", "es")
  let fr_bundle = ResourceBundleManager::load_bundle(bundle_manager, "messages", "fr")
  let ja_bundle = ResourceBundleManager::load_bundle(bundle_manager, "messages", "ja")
  let zh_bundle = ResourceBundleManager::load_bundle(bundle_manager, "messages", "zh")
  
  // Verify bundles are loaded
  assert_true(ResourceBundleManager::is_loaded(bundle_manager, "messages", "en"))
  assert_true(ResourceBundleManager::is_loaded(bundle_manager, "messages", "es"))
  assert_true(ResourceBundleManager::is_loaded(bundle_manager, "messages", "fr"))
  assert_true(ResourceBundleManager::is_loaded(bundle_manager, "messages", "ja"))
  assert_true(ResourceBundleManager::is_loaded(bundle_manager, "messages", "zh"))
  
  // Test message retrieval from bundles
  let en_welcome = ResourceBundleManager::get_message(bundle_manager, "messages", "en", "welcome")
  assert_eq(en_welcome, "Welcome to our application!")
  
  let es_welcome = ResourceBundleManager::get_message(bundle_manager, "messages", "es", "welcome")
  assert_eq(es_welcome, "¡Bienvenido a nuestra aplicación!")
  
  let fr_welcome = ResourceBundleManager::get_message(bundle_manager, "messages", "fr", "welcome")
  assert_eq(fr_welcome, "Bienvenue dans notre application!")
  
  let ja_welcome = ResourceBundleManager::get_message(bundle_manager, "messages", "ja", "welcome")
  assert_eq(ja_welcome, "アプリケーションへようこそ！")
  
  let zh_welcome = ResourceBundleManager::get_message(bundle_manager, "messages", "zh", "welcome")
  assert_eq(zh_welcome, "欢迎使用我们的应用程序！")
  
  // Test message with parameters
  let params = [("name", "John"), ("count", "5")]
  
  let en_user_items = ResourceBundleManager::get_message_with_params(bundle_manager, "messages", "en", "user.items", params)
  assert_eq(en_user_items, "John has 5 items.")
  
  let es_user_items = ResourceBundleManager::get_message_with_params(bundle_manager, "messages", "es", "user.items", params)
  assert_eq(es_user_items, "John tiene 5 artículos.")
  
  let ja_user_items = ResourceBundleManager::get_message_with_params(bundle_manager, "messages", "ja", "user.items", params)
  assert_eq(ja_user_items, "Johnは5個のアイテムを持っています。")
  
  // Test fallback to parent locale
  let en_us_welcome = ResourceBundleManager::get_message(bundle_manager, "messages", "en-US", "welcome")
  assert_eq(en_us_welcome, "Welcome to our application!") // Should fallback to "en"
  
  // Test fallback to default locale
  let unknown_locale_welcome = ResourceBundleManager::get_message(bundle_manager, "messages", "unknown-locale", "welcome")
  assert_eq(unknown_locale_welcome, "Welcome to our application!") // Should fallback to default
  
  // Test bundle reloading
  ResourceBundleManager::reload_bundle(bundle_manager, "messages", "en")
  assert_true(ResourceBundleManager::is_loaded(bundle_manager, "messages", "en"))
  
  // Test bundle caching
  let cache_stats = ResourceBundleManager::get_cache_stats(bundle_manager)
  assert_true(cache_stats.hits > 0)
  assert_true(cache_stats.misses > 0)
  assert_true(cache_stats.hit_rate > 0.0)
  
  // Test bundle invalidation
  ResourceBundleManager::invalidate_bundle(bundle_manager, "messages", "es")
  
  // Reload invalidated bundle
  let reloaded_es_bundle = ResourceBundleManager::load_bundle(bundle_manager, "messages", "es")
  assert_true(ResourceBundleManager::is_loaded(bundle_manager, "messages", "es"))
  
  let reloaded_es_welcome = ResourceBundleManager::get_message(bundle_manager, "messages", "es", "welcome")
  assert_eq(reloaded_es_welcome, "¡Bienvenido a nuestra aplicación!")
}