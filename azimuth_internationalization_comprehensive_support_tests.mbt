// Azimuth Telemetry System - Internationalization Comprehensive Support Tests
// This file contains comprehensive test cases for internationalization features

// Test 1: Multi-language Error Messages
test "multi-language error messages" {
  let i18n_manager = I18nManager::new()
  
  // Load language resources
  i18n_manager.load_language("en", {
    "telemetry.error.connection_failed": "Connection to telemetry service failed",
    "telemetry.error.invalid_data": "Invalid telemetry data format",
    "telemetry.error.timeout": "Telemetry operation timed out"
  })
  
  i18n_manager.load_language("zh", {
    "telemetry.error.connection_failed": "遥测服务连接失败",
    "telemetry.error.invalid_data": "遥测数据格式无效",
    "telemetry.error.timeout": "遥测操作超时"
  })
  
  i18n_manager.load_language("es", {
    "telemetry.error.connection_failed": "Error de conexión al servicio de telemetría",
    "telemetry.error.invalid_data": "Formato de datos de telemetría inválido",
    "telemetry.error.timeout": "Operación de telemetría agotó el tiempo"
  })
  
  // Test English error messages
  i18n_manager.set_current_language("en")
  let en_error1 = i18n_manager.get_message("telemetry.error.connection_failed")
  let en_error2 = i18n_manager.get_message("telemetry.error.invalid_data")
  let en_error3 = i18n_manager.get_message("telemetry.error.timeout")
  
  assert_eq(en_error1, "Connection to telemetry service failed")
  assert_eq(en_error2, "Invalid telemetry data format")
  assert_eq(en_error3, "Telemetry operation timed out")
  
  // Test Chinese error messages
  i18n_manager.set_current_language("zh")
  let zh_error1 = i18n_manager.get_message("telemetry.error.connection_failed")
  let zh_error2 = i18n_manager.get_message("telemetry.error.invalid_data")
  let zh_error3 = i18n_manager.get_message("telemetry.error.timeout")
  
  assert_eq(zh_error1, "遥测服务连接失败")
  assert_eq(zh_error2, "遥测数据格式无效")
  assert_eq(zh_error3, "遥测操作超时")
  
  // Test Spanish error messages
  i18n_manager.set_current_language("es")
  let es_error1 = i18n_manager.get_message("telemetry.error.connection_failed")
  let es_error2 = i18n_manager.get_message("telemetry.error.invalid_data")
  let es_error3 = i18n_manager.get_message("telemetry.error.timeout")
  
  assert_eq(es_error1, "Error de conexión al servicio de telemetría")
  assert_eq(es_error2, "Formato de datos de telemetría inválido")
  assert_eq(es_error3, "Operación de telemetría agotó el tiempo")
}

// Test 2: Locale-specific Date and Time Formatting
test "locale-specific date and time formatting" {
  let locale_formatter = LocaleFormatter::new()
  
  // Create a timestamp
  let timestamp = 1609459200000L // January 1, 2021 00:00:00 UTC
  
  // Test English (US) formatting
  let en_us_locale = Locale::new("en-US")
  let en_us_datetime = locale_formatter.format_datetime(timestamp, en_us_locale, DateTimeFormat::Full)
  assert_eq(en_us_datetime, "Friday, January 1, 2021 at 12:00:00 AM UTC")
  
  let en_us_date = locale_formatter.format_date(timestamp, en_us_locale, DateFormat::Medium)
  assert_eq(en_us_date, "Jan 1, 2021")
  
  let en_us_time = locale_formatter.format_time(timestamp, en_us_locale, TimeFormat::Short)
  assert_eq(en_us_time, "12:00 AM")
  
  // Test Chinese (China) formatting
  let zh_cn_locale = Locale::new("zh-CN")
  let zh_cn_datetime = locale_formatter.format_datetime(timestamp, zh_cn_locale, DateTimeFormat::Full)
  assert_eq(zh_cn_datetime, "2021年1月1日星期五 UTC 00:00:00")
  
  let zh_cn_date = locale_formatter.format_date(timestamp, zh_cn_locale, DateFormat::Medium)
  assert_eq(zh_cn_date, "2021年1月1日")
  
  let zh_cn_time = locale_formatter.format_time(timestamp, zh_cn_locale, TimeFormat::Short)
  assert_eq(zh_cn_time, "00:00")
  
  // Test Spanish (Spain) formatting
  let es_es_locale = Locale::new("es-ES")
  let es_es_datetime = locale_formatter.format_datetime(timestamp, es_es_locale, DateTimeFormat::Full)
  assert_eq(es_es_datetime, "viernes, 1 de enero de 2021, 0:00:00 UTC")
  
  let es_es_date = locale_formatter.format_date(timestamp, es_es_locale, DateFormat::Medium)
  assert_eq(es_es_date, "1 ene 2021")
  
  let es_es_time = locale_formatter.format_time(timestamp, es_es_locale, TimeFormat::Short)
  assert_eq(es_es_time, "0:00")
}

// Test 3: Number Formatting with Locale-specific Conventions
test "number formatting with locale-specific conventions" {
  let locale_formatter = LocaleFormatter::new()
  
  // Test decimal numbers
  let decimal_number = 1234.5678
  
  // English (US) formatting
  let en_us_locale = Locale::new("en-US")
  let en_us_decimal = locale_formatter.format_decimal(decimal_number, en_us_locale, 2)
  assert_eq(en_us_decimal, "1,234.57")
  
  // German (Germany) formatting
  let de_de_locale = Locale::new("de-DE")
  let de_de_decimal = locale_formatter.format_decimal(decimal_number, de_de_locale, 2)
  assert_eq(de_de_decimal, "1.234,57")
  
  // French (France) formatting
  let fr_fr_locale = Locale::new("fr-FR")
  let fr_fr_decimal = locale_formatter.format_decimal(decimal_number, fr_fr_locale, 2)
  assert_eq(fr_fr_decimal, "1 234,57")
  
  // Test currency formatting
  let currency_amount = 1234.56
  
  // English (US) currency
  let en_us_currency = locale_formatter.format_currency(currency_amount, en_us_locale, "USD")
  assert_eq(en_us_currency, "$1,234.56")
  
  // Euro formatting in Germany
  let de_de_currency = locale_formatter.format_currency(currency_amount, de_de_locale, "EUR")
  assert_eq(de_de_currency, "1.234,56 €")
  
  // Euro formatting in France
  let fr_fr_currency = locale_formatter.format_currency(currency_amount, fr_fr_locale, "EUR")
  assert_eq(fr_fr_currency, "1 234,56 €")
  
  // Test percentage formatting
  let percentage_value = 0.5678
  
  let en_us_percentage = locale_formatter.format_percentage(percentage_value, en_us_locale, 1)
  assert_eq(en_us_percentage, "56.8%")
  
  let de_de_percentage = locale_formatter.format_percentage(percentage_value, de_de_locale, 1)
  assert_eq(de_de_percentage, "56,8 %")
}

// Test 4: Text Direction Support (LTR/RTL)
test "text direction support (LTR/RTL)" {
  let text_direction_manager = TextDirectionManager::new()
  
  // Test LTR languages
  let en_text = "Telemetry data processing"
  text_direction_manager.set_language("en")
  let en_direction = text_direction_manager.get_text_direction(en_text)
  assert_eq(en_direction, LTR)
  
  let zh_text = "遥测数据处理"
  text_direction_manager.set_language("zh")
  let zh_direction = text_direction_manager.get_text_direction(zh_text)
  assert_eq(zh_direction, LTR)
  
  // Test RTL languages
  let ar_text = "معالجة بيانات القياس عن بعد"
  text_direction_manager.set_language("ar")
  let ar_direction = text_direction_manager.get_text_direction(ar_text)
  assert_eq(ar_direction, RTL)
  
  let he_text = "עיבוד נתוני טלמטריה"
  text_direction_manager.set_language("he")
  let he_direction = text_direction_manager.get_text_direction(he_text)
  assert_eq(he_direction, RTL)
  
  // Test mixed direction text
  let mixed_text = "Telemetry معالجة بيانات"
  text_direction_manager.set_language("en") // Default LTR
  let mixed_direction = text_direction_manager.get_text_direction(mixed_text)
  assert_eq(mixed_direction, LTR)
  
  // Test UI layout adaptation
  let ui_layout = text_direction_manager.get_ui_layout()
  match ui_layout {
    LTRLayout => assert_true(true)
    RTLLayout => assert_true(false)
  }
  
  text_direction_manager.set_language("ar")
  let rtl_ui_layout = text_direction_manager.get_ui_layout()
  match rtl_ui_layout {
    LTRLayout => assert_true(false)
    RTLLayout => assert_true(true)
  }
}

// Test 5: Collation and Sorting
test "collation and sorting with locale-specific rules" {
  let collation_manager = CollationManager::new()
  
  // Test strings with different locales
  let strings = ["apple", "banana", "cherry", "ápple", "banana", "cherry"]
  
  // English collation
  let en_collator = Collator::new("en")
  let en_sorted = collation_manager.sort(strings, en_collator)
  assert_eq(en_sorted, ["ápple", "apple", "banana", "banana", "cherry", "cherry"])
  
  // Spanish collation (treats accented characters differently)
  let es_collator = Collator::new("es")
  let es_sorted = collation_manager.sort(strings, es_collator)
  assert_eq(es_sorted, ["apple", "ápple", "banana", "banana", "cherry", "cherry"])
  
  // Test Chinese collation
  let chinese_strings = ["北京", "上海", "广州", "深圳"]
  let zh_collator = Collator::new("zh")
  let zh_sorted = collation_manager.sort(chinese_strings, zh_collator)
  assert_eq(zh_sorted, ["上海", "北京", "广州", "深圳"])
  
  // Test search with collation
  let search_result = collation_manager.search(strings, "apple", en_collator)
  assert_eq(search_result, ["apple", "ápple"])
  
  let spanish_search_result = collation_manager.search(strings, "apple", es_collator)
  assert_eq(spanish_search_result, ["apple"])
}

// Test 6: Character Encoding and Unicode Support
test "character encoding and unicode support" {
  let encoding_manager = EncodingManager::new()
  
  // Test UTF-8 encoding and decoding
  let unicode_strings = [
    "Hello, 世界", // English and Chinese
    "مرحبا بالعالم", // Arabic
    "Привет мир", // Russian
    "🚀 Telemetry 📊", // Emoji and text
    "Café résumé naïve", // Accented characters
    "東京", // Japanese
    "서울", // Korean
    "עברית" // Hebrew
  ]
  
  for unicode_string in unicode_strings {
    // Test encoding
    let encoded = encoding_manager.encode_utf8(unicode_string)
    assert_true(encoded.length() > 0)
    
    // Test decoding
    let decoded = encoding_manager.decode_utf8(encoded)
    assert_eq(decoded, unicode_string)
    
    // Test character count
    let char_count = encoding_manager.get_character_count(unicode_string)
    assert_true(char_count > 0)
    
    // Test byte count
    let byte_count = encoding_manager.get_byte_count(unicode_string)
    assert_true(byte_count >= char_count)
  }
  
  // Test normalization
  let composed = "café"
  let decomposed = "cafe\u0301" // 'e' + combining acute accent
  
  let normalized_composed = encoding_manager.normalize(composed, NormalizationForm::NFC)
  let normalized_decomposed = encoding_manager.normalize(decomposed, NormalizationForm::NFC)
  
  assert_eq(normalized_composed, normalized_decomposed)
}

// Test 7: Time Zone Support
test "time zone support" {
  let timezone_manager = TimezoneManager::new()
  
  // Create a timestamp
  let timestamp = 1609459200000L // January 1, 2021 00:00:00 UTC
  
  // Test different time zones
  let utc_time = timezone_manager.convert(timestamp, "UTC")
  assert_eq(utc_time.format("yyyy-MM-dd HH:mm:ss"), "2021-01-01 00:00:00")
  
  let est_time = timezone_manager.convert(timestamp, "America/New_York")
  assert_eq(est_time.format("yyyy-MM-dd HH:mm:ss"), "2020-12-31 19:00:00")
  
  let pst_time = timezone_manager.convert(timestamp, "America/Los_Angeles")
  assert_eq(pst_time.format("yyyy-MM-dd HH:mm:ss"), "2020-12-31 16:00:00")
  
  let cet_time = timezone_manager.convert(timestamp, "Europe/Paris")
  assert_eq(cet_time.format("yyyy-MM-dd HH:mm:ss"), "2021-01-01 01:00:00")
  
  let jst_time = timezone_manager.convert(timestamp, "Asia/Tokyo")
  assert_eq(jst_time.format("yyyy-MM-dd HH:mm:ss"), "2021-01-01 09:00:00")
  
  let cst_time = timezone_manager.convert(timestamp, "Asia/Shanghai")
  assert_eq(cst_time.format("yyyy-MM-dd HH:mm:ss"), "2021-01-01 08:00:00")
  
  // Test time zone detection from locale
  let en_us_locale = Locale::new("en-US")
  let detected_us_timezone = timezone_manager.detect_timezone(en_us_locale)
  assert_eq(detected_us_timezone, "America/New_York")
  
  let zh_cn_locale = Locale::new("zh-CN")
  let detected_cn_timezone = timezone_manager.detect_timezone(zh_cn_locale)
  assert_eq(detected_cn_timezone, "Asia/Shanghai")
  
  let ja_jp_locale = Locale::new("ja-JP")
  let detected_jp_timezone = timezone_manager.detect_timezone(ja_jp_locale)
  assert_eq(detected_jp_timezone, "Asia/Tokyo")
}

// Test 8: Localized Telemetry Data
test "localized telemetry data" {
  let localized_telemetry = LocalizedTelemetry::new()
  
  // Create telemetry data with localized values
  let telemetry_data = TelemetryData::new()
  
  // Add localized error messages
  localized_telemetry.add_localized_attribute(telemetry_data, "error_message", {
    "en": "Connection failed",
    "zh": "连接失败",
    "es": "Error de conexión",
    "fr": "Échec de connexion",
    "de": "Verbindung fehlgeschlagen",
    "ja": "接続失敗",
    "ar": "فشل الاتصال"
  })
  
  // Add localized descriptions
  localized_telemetry.add_localized_attribute(telemetry_data, "description", {
    "en": "Telemetry service is temporarily unavailable",
    "zh": "遥测服务暂时不可用",
    "es": "El servicio de telemetría no está disponible temporalmente",
    "fr": "Le service de télémétrie est temporairement indisponible",
    "de": "Telemetriedienst ist vorübergehend nicht verfügbar",
    "ja": "テレメトリサービスは一時的に利用できません",
    "ar": "خدمة القياس عن بعد غير متاحة مؤقتًا"
  })
  
  // Test retrieval in different languages
  localized_telemetry.set_current_language("en")
  let en_error_msg = localized_telemetry.get_localized_attribute(telemetry_data, "error_message")
  let en_description = localized_telemetry.get_localized_attribute(telemetry_data, "description")
  
  assert_eq(en_error_msg, "Connection failed")
  assert_eq(en_description, "Telemetry service is temporarily unavailable")
  
  localized_telemetry.set_current_language("zh")
  let zh_error_msg = localized_telemetry.get_localized_attribute(telemetry_data, "error_message")
  let zh_description = localized_telemetry.get_localized_attribute(telemetry_data, "description")
  
  assert_eq(zh_error_msg, "连接失败")
  assert_eq(zh_description, "遥测服务暂时不可用")
  
  localized_telemetry.set_current_language("ar")
  let ar_error_msg = localized_telemetry.get_localized_attribute(telemetry_data, "error_message")
  let ar_description = localized_telemetry.get_localized_attribute(telemetry_data, "description")
  
  assert_eq(ar_error_msg, "فشل الاتصال")
  assert_eq(ar_description, "خدمة القياس عن بعد غير متاحة مؤقتًا")
}

// Test 9: Cultural Conventions in Data Presentation
test "cultural conventions in data presentation" {
  let cultural_formatter = CulturalFormatter::new()
  
  // Test address formatting
  let address_components = {
    "street": "123 Main St",
    "city": "New York",
    "state": "NY",
    "postal_code": "10001",
    "country": "USA"
  }
  
  // US address format
  let us_address = cultural_formatter.format_address(address_components, "en-US")
  assert_eq(us_address, "123 Main St\nNew York, NY 10001\nUSA")
  
  // Japanese address format
  let jp_address_components = {
    "postal_code": "100-0001",
    "prefecture": "東京都",
    "city": "千代田区",
    "street": "千代田1-1",
    "country": "日本"
  }
  
  let jp_address = cultural_formatter.format_address(jp_address_components, "ja-JP")
  assert_eq(jp_address, "〒100-0001\n東京都千代田区千代田1-1\n日本")
  
  // Test name formatting
  let name_components = {
    "first_name": "John",
    "last_name": "Doe",
    "middle_name": "William"
  }
  
  // Western name format
  let western_name = cultural_formatter.format_name(name_components, "en-US")
  assert_eq(western_name, "John William Doe")
  
  // Eastern name format (family name first)
  let eastern_name = cultural_formatter.format_name(name_components, "ja-JP")
  assert_eq(eastern_name, "Doe John William")
  
  // Test phone number formatting
  let phone_number = "+12125551234"
  
  // US format
  let us_phone = cultural_formatter.format_phone_number(phone_number, "en-US")
  assert_eq(us_phone, "+1 (212) 555-1234")
  
  // International format
  let international_phone = cultural_formatter.format_phone_number(phone_number, "fr-FR")
  assert_eq(international_phone, "+1 212-555-1234")
}

// Test 10: Language Detection and Fallback
test "language detection and fallback mechanisms" {
  let language_detector = LanguageDetector::new()
  
  // Test language detection from text
  let en_text = "This is an English text about telemetry systems"
  let detected_en = language_detector.detect(en_text)
  assert_eq(detected_en, "en")
  
  let zh_text = "这是关于遥测系统的中文文本"
  let detected_zh = language_detector.detect(zh_text)
  assert_eq(detected_zh, "zh")
  
  let es_text = "Este es un texto en español sobre sistemas de telemetría"
  let detected_es = language_detector.detect(es_text)
  assert_eq(detected_es, "es")
  
  let ar_text = "هذا نص باللغة العربية حول أنظمة القياس عن بعد"
  let detected_ar = language_detector.detect(ar_text)
  assert_eq(detected_ar, "ar")
  
  // Test language detection from HTTP headers
  let http_header = "zh-CN,zh;q=0.9,en;q=0.8"
  let preferred_languages = language_detector.parse_http_accept_language(http_header)
  assert_eq(preferred_languages[0], "zh-CN")
  assert_eq(preferred_languages[1], "zh")
  assert_eq(preferred_languages[2], "en")
  
  // Test fallback mechanism
  let fallback_manager = FallbackManager::new()
  fallback_manager.set_fallback_chain(["zh-CN", "zh", "en"])
  
  // Test resource availability
  fallback_manager.add_resource("zh-CN", "message", "中文消息")
  fallback_manager.add_resource("en", "message", "English message")
  
  // Try to get resource in Chinese (China)
  let zh_cn_resource = fallback_manager.get_resource("message", "zh-CN")
  assert_eq(zh_cn_resource, "中文消息")
  
  // Try to get resource in Chinese (Taiwan) - should fallback to zh-CN
  let zh_tw_resource = fallback_manager.get_resource("message", "zh-TW")
  assert_eq(zh_tw_resource, "中文消息")
  
  // Try to get resource in French - should fallback to en
  let fr_resource = fallback_manager.get_resource("message", "fr")
  assert_eq(fr_resource, "English message")
}