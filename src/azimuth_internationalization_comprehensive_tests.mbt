// Azimuth Telemetry System - Internationalization Comprehensive Tests
// This file contains comprehensive test cases for internationalization and localization support

// Test 1: Locale Detection and Management
test "locale detection and management" {
  let locale_manager = LocaleManager::new()
  
  // Test default locale
  let default_locale = locale_manager.get_default_locale()
  assert_eq(default_locale.language, "en")
  assert_eq(default_locale.region, "US")
  
  // Test locale parsing
  let locale1 = locale_manager.parse_locale("zh-CN")
  assert_eq(locale1.language, "zh")
  assert_eq(locale1.region, "CN")
  
  let locale2 = locale_manager.parse_locale("ja-JP")
  assert_eq(locale2.language, "ja")
  assert_eq(locale2.region, "JP")
  
  let locale3 = locale_manager.parse_locale("fr")
  assert_eq(locale3.language, "fr")
  assert_eq(locale3.region, "")  // No region specified
  
  // Test locale fallback chain
  let fallback_chain = locale_manager.get_fallback_chain("zh-CN")
  assert_eq(fallback_chain[0], "zh-CN")
  assert_eq(fallback_chain[1], "zh")
  assert_eq(fallback_chain[2], "en-US")  // Default fallback
  
  // Test locale matching
  let supported_locales = ["en-US", "zh-CN", "ja-JP", "fr-FR"]
  let best_match = locale_manager.find_best_match("zh-TW", supported_locales)
  assert_eq(best_match, "zh")  // Language match without region
  
  let exact_match = locale_manager.find_best_match("ja-JP", supported_locales)
  assert_eq(exact_match, "ja-JP")  // Exact match
  
  let no_match = locale_manager.find_best_match("ko-KR", supported_locales)
  assert_eq(no_match, "en-US")  // Default fallback
}

// Test 2: Message Translation
test "message translation" {
  let translator = MessageTranslator::new()
  
  // Load translation resources
  translator.load_resource("en-US", [
    ("telemetry.error.network", "Network error occurred"),
    ("telemetry.error.timeout", "Request timed out"),
    ("telemetry.metrics.response_time", "Response time: {value}ms"),
    ("telemetry.service.status", "Service {name} is {status}")
  ])
  
  translator.load_resource("zh-CN", [
    ("telemetry.error.network", "发生网络错误"),
    ("telemetry.error.timeout", "请求超时"),
    ("telemetry.metrics.response_time", "响应时间：{value}毫秒"),
    ("telemetry.service.status", "服务 {name} 状态为 {status}")
  ])
  
  translator.load_resource("ja-JP", [
    ("telemetry.error.network", "ネットワークエラーが発生しました"),
    ("telemetry.error.timeout", "リクエストがタイムアウトしました"),
    ("telemetry.metrics.response_time", "応答時間：{value}ミリ秒"),
    ("telemetry.service.status", "サービス {name} の状態は {status} です")
  ])
  
  // Test simple translation
  let en_message = translator.translate("en-US", "telemetry.error.network")
  assert_eq(en_message, "Network error occurred")
  
  let zh_message = translator.translate("zh-CN", "telemetry.error.network")
  assert_eq(zh_message, "发生网络错误")
  
  let ja_message = translator.translate("ja-JP", "telemetry.error.network")
  assert_eq(ja_message, "ネットワークエラーが発生しました")
  
  // Test parameterized translation
  let en_param = translator.translate_with_params("en-US", "telemetry.metrics.response_time", [
    ("value", "250")
  ])
  assert_eq(en_param, "Response time: 250ms")
  
  let zh_param = translator.translate_with_params("zh-CN", "telemetry.metrics.response_time", [
    ("value", "250")
  ])
  assert_eq(zh_param, "响应时间：250毫秒")
  
  // Test missing translation fallback
  let missing_message = translator.translate("fr-FR", "telemetry.error.network")
  assert_eq(missing_message, "Network error occurred")  // Fallback to default
  
  // Test missing key fallback
  let missing_key = translator.translate("zh-CN", "telemetry.error.unknown")
  assert_eq(missing_key, "telemetry.error.unknown")  // Return key as fallback
}

// Test 3: Date and Time Localization
test "date and time localization" {
  let datetime_localizer = DateTimeLocalizer::new()
  
  // Create test timestamp
  let timestamp = 1609459200  // 2021-01-01 00:00:00 UTC
  
  // Test date formatting
  let us_date = datetime_localizer.format_date("en-US", timestamp)
  assert_eq(us_date, "1/1/2021")
  
  let cn_date = datetime_localizer.format_date("zh-CN", timestamp)
  assert_eq(cn_date, "2021/1/1")
  
  let jp_date = datetime_localizer.format_date("ja-JP", timestamp)
  assert_eq(jp_date, "2021/01/01")
  
  let fr_date = datetime_localizer.format_date("fr-FR", timestamp)
  assert_eq(fr_date, "01/01/2021")
  
  // Test time formatting
  let us_time = datetime_localizer.format_time("en-US", timestamp)
  assert_eq(us_time, "12:00:00 AM")
  
  let cn_time = datetime_localizer.format_time("zh-CN", timestamp)
  assert_eq(cn_time, "上午12:00:00")
  
  let jp_time = datetime_localizer.format_time("ja-JP", timestamp)
  assert_eq(jp_time, "午前12:00:00")
  
  let fr_time = datetime_localizer.format_time("fr-FR", timestamp)
  assert_eq(fr_time, "00:00:00")
  
  // Test datetime formatting
  let us_datetime = datetime_localizer.format_datetime("en-US", timestamp)
  assert_eq(us_datetime, "1/1/2021, 12:00:00 AM")
  
  let cn_datetime = datetime_localizer.format_datetime("zh-CN", timestamp)
  assert_eq(cn_datetime, "2021/1/1 上午12:00:00")
  
  // Test relative time formatting
  let now = 1609459200
  let past = now - 3600  // 1 hour ago
  let future = now + 7200  // 2 hours in future
  
  let en_relative_past = datetime_localizer.format_relative_time("en-US", past, now)
  assert_eq(en_relative_past, "1 hour ago")
  
  let en_relative_future = datetime_localizer.format_relative_time("en-US", future, now)
  assert_eq(en_relative_future, "in 2 hours")
  
  let zh_relative_past = datetime_localizer.format_relative_time("zh-CN", past, now)
  assert_eq(zh_relative_past, "1小时前")
  
  let zh_relative_future = datetime_localizer.format_relative_time("zh-CN", future, now)
  assert_eq(zh_relative_future, "2小时后")
}

// Test 4: Number and Currency Localization
test "number and currency localization" {
  let number_localizer = NumberLocalizer::new()
  
  // Test decimal number formatting
  let us_decimal = number_localizer.format_decimal("en-US", 12345.6789)
  assert_eq(us_decimal, "12,345.679")
  
  let cn_decimal = number_localizer.format_decimal("zh-CN", 12345.6789)
  assert_eq(cn_decimal, "12,345.679")
  
  let fr_decimal = number_localizer.format_decimal("fr-FR", 12345.6789)
  assert_eq(fr_decimal, "12 345,679")
  
  let de_decimal = number_localizer.format_decimal("de-DE", 12345.6789)
  assert_eq(de_decimal, "12.345,679")
  
  // Test percentage formatting
  let us_percent = number_localizer.format_percentage("en-US", 0.7543)
  assert_eq(us_percent, "75.43%")
  
  let cn_percent = number_localizer.format_percentage("zh-CN", 0.7543)
  assert_eq(cn_percent, "75.43%")
  
  let fr_percent = number_localizer.format_percentage("fr-FR", 0.7543)
  assert_eq(fr_percent, "75,43 %")
  
  // Test currency formatting
  let us_currency = number_localizer.format_currency("en-US", 1234.56, "USD")
  assert_eq(us_currency, "$1,234.56")
  
  let cn_currency = number_localizer.format_currency("zh-CN", 1234.56, "CNY")
  assert_eq(cn_currency, "¥1,234.56")
  
  let jp_currency = number_localizer.format_currency("ja-JP", 1234.56, "JPY")
  assert_eq(jp_currency, "¥1,235")  // JPY has no decimal places
  
  let eu_currency = number_localizer.format_currency("fr-FR", 1234.56, "EUR")
  assert_eq(eu_currency, "1 234,56 €")
  
  // Test unit formatting
  let us_bytes = number_localizer.format_unit("en-US", 1048576, "byte")
  assert_eq(us_bytes, "1 MB")
  
  let cn_bytes = number_localizer.format_unit("zh-CN", 1048576, "byte")
  assert_eq(cn_bytes, "1 MB")
  
  let us_duration = number_localizer.format_unit("en-US", 3661, "second")
  assert_eq(us_duration, "1 hr, 1 min, 1 sec")
  
  let cn_duration = number_localizer.format_unit("zh-CN", 3661, "second")
  assert_eq(cn_duration, "1小时1分钟1秒")
}

// Test 5: Text Direction and Layout
test "text direction and layout" {
  let layout_manager = LayoutManager::new()
  
  // Test text direction detection
  let ltr_text = "Hello World"
  let rtl_text = "مرحبا بالعالم"
  let mixed_text = "Hello مرحبا World"
  
  assert_eq(layout_manager.get_text_direction(ltr_text), TextDirection::LTR)
  assert_eq(layout_manager.get_text_direction(rtl_text), TextDirection::RTL)
  assert_eq(layout_manager.get_text_direction(mixed_text), TextDirection::LTR)  // Default to LTR for mixed
  
  // Test locale-based direction
  assert_eq(layout_manager.get_locale_direction("en-US"), TextDirection::LTR)
  assert_eq(layout_manager.get_locale_direction("ar-SA"), TextDirection::RTL)
  assert_eq(layout_manager.get_locale_direction("he-IL"), TextDirection::RTL)
  assert_eq(layout_manager.get_locale_direction("zh-CN"), TextDirection::LTR)
  
  // Test layout mirroring
  let ltr_layout = layout_manager.create_layout("en-US")
  assert_eq(ltr_layout.direction, TextDirection::LTR)
  assert_eq(ltr_layout.alignment, TextAlignment::Left)
  
  let rtl_layout = layout_manager.create_layout("ar-SA")
  assert_eq(rtl_layout.direction, TextDirection::RTL)
  assert_eq(rtl_layout.alignment, TextAlignment::Right)
  
  // Test UI element positioning
  let ltr_positions = layout_manager.calculate_positions("en-US", [
    UIElement::button("OK"),
    UIElement::button("Cancel"),
    UIElement::text("Message")
  ])
  assert_true(ltr_positions[0].x < ltr_positions[1].x)  // OK before Cancel
  
  let rtl_positions = layout_manager.calculate_positions("ar-SA", [
    UIElement::button("موافق"),
    UIElement::button("إلغاء"),
    UIElement::text("رسالة")
  ])
  assert_true(rtl_positions[0].x > rtl_positions[1].x)  // Cancel before OK in RTL
}

// Test 6: Collation and Sorting
test "collation and sorting" {
  let collator = Collator::new()
  
  // Test string sorting in different locales
  let english_strings = ["apple", "banana", "cherry", "date"]
  let sorted_en = collator.sort_strings("en-US", english_strings)
  assert_eq(sorted_en, ["apple", "banana", "cherry", "date"])
  
  // Test Chinese sorting
  let chinese_strings = ["苹果", "香蕉", "樱桃", "枣子"]
  let sorted_zh = collator.sort_strings("zh-CN", chinese_strings)
  // Note: Actual sorting depends on pinyin or stroke order
  assert_eq(sorted_zh.length(), 4)
  
  // Test Japanese sorting
  let japanese_strings = ["りんご", "バナナ", "チェリー", "デート"]
  let sorted_ja = collator.sort_strings("ja-JP", japanese_strings)
  assert_eq(sorted_ja.length(), 4)
  
  // Test German sorting (with special characters)
  let german_strings = ["äpfel", "bananen", "kirschen", "datteln"]
  let sorted_de = collator.sort_strings("de-DE", german_strings)
  assert_eq(sorted_de, ["äpfel", "bananen", "datteln", "kirschen"])
  
  // Test case sensitivity
  let mixed_case = ["Apple", "banana", "Cherry", "date"]
  let case_sensitive = collator.sort_strings("en-US", mixed_case, case_sensitive: true)
  assert_eq(case_sensitive, ["Apple", "Cherry", "banana", "date"])
  
  let case_insensitive = collator.sort_strings("en-US", mixed_case, case_sensitive: false)
  assert_eq(case_insensitive, ["Apple", "banana", "Cherry", "date"])
  
  // Test numeric collation
  let numeric_strings = ["file10", "file2", "file1", "file20"]
  let numeric_sorted = collator.sort_strings("en-US", numeric_strings, numeric: true)
  assert_eq(numeric_sorted, ["file1", "file2", "file10", "file20"])
}

// Test 7: Resource Bundle Management
test "resource bundle management" {
  let resource_manager = ResourceBundleManager::new()
  
  // Create resource bundles for different locales
  let en_bundle = ResourceBundle::new("en-US", [
    ("telemetry.dashboard.title", "Telemetry Dashboard"),
    ("telemetry.metrics.cpu", "CPU Usage"),
    ("telemetry.metrics.memory", "Memory Usage"),
    ("telemetry.metrics.network", "Network I/O"),
    ("telemetry.alerts.title", "System Alerts")
  ])
  
  let zh_bundle = ResourceBundle::new("zh-CN", [
    ("telemetry.dashboard.title", "遥测仪表板"),
    ("telemetry.metrics.cpu", "CPU使用率"),
    ("telemetry.metrics.memory", "内存使用率"),
    ("telemetry.metrics.network", "网络I/O"),
    ("telemetry.alerts.title", "系统警报")
  ])
  
  let ja_bundle = ResourceBundle::new("ja-JP", [
    ("telemetry.dashboard.title", "テレメトリーダッシュボード"),
    ("telemetry.metrics.cpu", "CPU使用率"),
    ("telemetry.metrics.memory", "メモリ使用率"),
    ("telemetry.metrics.network", "ネットワークI/O"),
    ("telemetry.alerts.title", "システムアラート")
  ])
  
  // Register resource bundles
  resource_manager.register_bundle(en_bundle)
  resource_manager.register_bundle(zh_bundle)
  resource_manager.register_bundle(ja_bundle)
  
  // Test resource retrieval
  let en_title = resource_manager.get_string("en-US", "telemetry.dashboard.title")
  assert_eq(en_title, "Telemetry Dashboard")
  
  let zh_title = resource_manager.get_string("zh-CN", "telemetry.dashboard.title")
  assert_eq(zh_title, "遥测仪表板")
  
  let ja_title = resource_manager.get_string("ja-JP", "telemetry.dashboard.title")
  assert_eq(ja_title, "テレメトリーダッシュボード")
  
  // Test resource fallback
  let missing_locale = resource_manager.get_string("fr-FR", "telemetry.dashboard.title")
  assert_eq(missing_locale, "Telemetry Dashboard")  // Fallback to default locale
  
  // Test missing key handling
  let missing_key = resource_manager.get_string("zh-CN", "telemetry.metrics.disk")
  assert_eq(missing_key, "telemetry.metrics.disk")  // Return key as fallback
  
  // Test bundle inheritance
  let zh_tw_bundle = ResourceBundle::with_parent("zh-TW", zh_bundle, [
    ("telemetry.metrics.disk", "磁碟使用率")
  ])
  resource_manager.register_bundle(zh_tw_bundle)
  
  let zh_tw_cpu = resource_manager.get_string("zh-TW", "telemetry.metrics.cpu")
  assert_eq(zh_tw_cpu, "CPU使用率")  // Inherited from zh-CN
  
  let zh_tw_disk = resource_manager.get_string("zh-TW", "telemetry.metrics.disk")
  assert_eq(zh_tw_disk, "磁碟使用率")  // Override in zh-TW
  
  // Test bundle hot-reload
  let updated_en_bundle = ResourceBundle::new("en-US", [
    ("telemetry.dashboard.title", "Telemetry Dashboard v2"),
    ("telemetry.metrics.cpu", "CPU Utilization")
  ])
  resource_manager.update_bundle(updated_en_bundle)
  
  let updated_title = resource_manager.get_string("en-US", "telemetry.dashboard.title")
  assert_eq(updated_title, "Telemetry Dashboard v2")
}

// Test 8: Pluralization Support
test "pluralization support" {
  let pluralizer = Pluralizer::new()
  
  // Define pluralization rules
  pluralizer.add_rule("en", PluralRule::new(
    categories: ["one", "other"],
    rule_fn: fn(n) { if n == 1 { "one" } else { "other" } }
  ))
  
  pluralizer.add_rule("zh", PluralRule::new(
    categories: ["other"],
    rule_fn: fn(n) { "other" }
  ))
  
  pluralizer.add_rule("ar", PluralRule::new(
    categories: ["zero", "one", "two", "few", "many", "other"],
    rule_fn: fn(n) {
      if n == 0 { "zero" }
      else if n == 1 { "one" }
      else if n == 2 { "two" }
      else if n >= 3 && n <= 10 { "few" }
      else if n >= 11 && n <= 99 { "many" }
      else { "other" }
    }
  ))
  
  // Load pluralized messages
  let pluralized_messages = [
    ("en", "telemetry.items.count", [
      ("one", "Found {count} item"),
      ("other", "Found {count} items")
    ]),
    ("zh", "telemetry.items.count", [
      ("other", "找到 {count} 个项目")
    ]),
    ("ar", "telemetry.items.count", [
      ("zero", "لم يتم العثور على عناصر"),
      ("one", "تم العثور على عنصر واحد"),
      ("two", "تم العثور على عنصرين"),
      ("few", "تم العثور على {count} عناصر"),
      ("many", "تم العثور على {count} عنصر"),
      ("other", "تم العثور على {count} عنصر")
    ])
  ]
  
  for (locale, key, forms) in pluralized_messages {
    pluralizer.add_pluralized_message(locale, key, forms)
  }
  
  // Test English pluralization
  let en_one = pluralizer.pluralize("en", "telemetry.items.count", 1)
  assert_eq(en_one, "Found 1 item")
  
  let en_many = pluralizer.pluralize("en", "telemetry.items.count", 5)
  assert_eq(en_many, "Found 5 items")
  
  // Test Chinese pluralization (no plural forms)
  let zh_one = pluralizer.pluralize("zh", "telemetry.items.count", 1)
  assert_eq(zh_one, "找到 1 个项目")
  
  let zh_many = pluralizer.pluralize("zh", "telemetry.items.count", 5)
  assert_eq(zh_many, "找到 5 个项目")
  
  // Test Arabic pluralization (complex rules)
  let ar_zero = pluralizer.pluralize("ar", "telemetry.items.count", 0)
  assert_eq(ar_zero, "لم يتم العثور على عناصر")
  
  let ar_one = pluralizer.pluralize("ar", "telemetry.items.count", 1)
  assert_eq(ar_one, "تم العثور على عنصر واحد")
  
  let ar_two = pluralizer.pluralize("ar", "telemetry.items.count", 2)
  assert_eq(ar_two, "تم العثور على عنصرين")
  
  let ar_few = pluralizer.pluralize("ar", "telemetry.items.count", 5)
  assert_eq(ar_few, "تم العثور على 5 عناصر")
  
  let ar_many = pluralizer.pluralize("ar", "telemetry.items.count", 50)
  assert_eq(ar_many, "تم العثور على 50 عنصر")
  
  let ar_other = pluralizer.pluralize("ar", "telemetry.items.count", 150)
  assert_eq(ar_other, "تم العثور على 150 عنصر")
}

// Test 9: Character Encoding and Conversion
test "character encoding and conversion" {
  let encoding_converter = EncodingConverter::new()
  
  // Test UTF-8 encoding validation
  let valid_utf8 = "Hello 世界 🌍"
  assert_true(encoding_converter.is_valid_utf8(valid_utf8))
  
  let invalid_utf8 = [0xFF, 0xFE, 0xFD]  // Invalid UTF-8 sequence
  assert_false(encoding_converter.is_valid_utf8(invalid_utf8))
  
  // Test character set detection
  let ascii_text = "Hello World"
  let detected_ascii = encoding_converter.detect_encoding(ascii_text)
  assert_eq(detected_ascii, "ASCII")
  
  let utf8_text = "Hello 世界"
  let detected_utf8 = encoding_converter.detect_encoding(utf8_text)
  assert_eq(detected_utf8, "UTF-8")
  
  // Test encoding conversion
  let utf16_bytes = encoding_converter.convert_to_utf16("Hello 世界")
  assert_true(utf16_bytes.length() > 0)
  
  let converted_back = encoding_converter.convert_from_utf16(utf16_bytes, "UTF-8")
  assert_eq(converted_back, "Hello 世界")
  
  // Test Unicode normalization
  let composed = "é"  // U+00E9 (composed form)
  let decomposed = "e\u0301"  // e + combining acute accent
  
  let normalized_composed = encoding_converter.normalize_unicode(composed, "NFC")
  let normalized_decomposed = encoding_converter.normalize_unicode(decomposed, "NFC")
  
  assert_eq(normalized_composed, normalized_decomposed)  // Should be identical after NFC normalization
  
  // Test case conversion with locale awareness
  let turkish_i = "i"
  let turkish_upper = encoding_converter.uppercase(turkish_i, "tr-TR")
  assert_eq(turkish_upper, "İ")  // Turkish dotted capital I
  
  let english_i = "i"
  let english_upper = encoding_converter.uppercase(english_i, "en-US")
  assert_eq(english_upper, "I")  // Regular capital I
  
  // Test width handling (full-width vs half-width characters)
  let half_width = "ABC123"
  let full_width = "ＡＢＣ１２３"
  
  let half_to_full = encoding_converter.convert_width(half_width, "full-width")
  assert_eq(half_to_full, full_width)
  
  let full_to_half = encoding_converter.convert_width(full_width, "half-width")
  assert_eq(full_to_half, half_width)
}

// Test 10: Time Zone Handling
test "time zone handling" {
  let timezone_manager = TimezoneManager::new()
  
  // Create test timestamp
  let timestamp = 1609459200  // 2021-01-01 00:00:00 UTC
  
  // Test timezone conversion
  let utc_time = timezone_manager.convert_timezone(timestamp, "UTC", "UTC")
  assert_eq(utc_time.format("YYYY-MM-DD HH:mm:ss"), "2021-01-01 00:00:00")
  
  let est_time = timezone_manager.convert_timezone(timestamp, "UTC", "America/New_York")
  assert_eq(est_time.format("YYYY-MM-DD HH:mm:ss"), "2020-12-31 19:00:00")
  
  let pst_time = timezone_manager.convert_timezone(timestamp, "UTC", "America/Los_Angeles")
  assert_eq(pst_time.format("YYYY-MM-DD HH:mm:ss"), "2020-12-31 16:00:00")
  
  let cet_time = timezone_manager.convert_timezone(timestamp, "UTC", "Europe/Paris")
  assert_eq(cet_time.format("YYYY-MM-DD HH:mm:ss"), "2021-01-01 01:00:00")
  
  let jst_time = timezone_manager.convert_timezone(timestamp, "UTC", "Asia/Tokyo")
  assert_eq(jst_time.format("YYYY-MM-DD HH:mm:ss"), "2021-01-01 09:00:00")
  
  let cst_time = timezone_manager.convert_timezone(timestamp, "UTC", "Asia/Shanghai")
  assert_eq(cst_time.format("YYYY-MM-DD HH:mm:ss"), "2021-01-01 08:00:00")
  
  // Test timezone-aware formatting
  let ny_formatted = timezone_manager.format_with_timezone(timestamp, "en-US", "America/New_York")
  assert_eq(ny_formatted, "12/31/2020, 7:00:00 PM EST")
  
  let tokyo_formatted = timezone_manager.format_with_timezone(timestamp, "ja-JP", "Asia/Tokyo")
  assert_eq(tokyo_formatted, "2021/01/01 9:00:00 JST")
  
  // Test daylight saving time transitions
  let dst_timestamp = 1614556800  // 2021-03-01 00:00:00 UTC (before DST)
  let dst_est = timezone_manager.convert_timezone(dst_timestamp, "UTC", "America/New_York")
  assert_eq(dst_est.format("YYYY-MM-DD HH:mm:ss Z"), "2021-02-28 19:00:00 EST")
  
  let post_dst_timestamp = 1617235200  // 2021-04-01 00:00:00 UTC (after DST)
  let post_dst_est = timezone_manager.convert_timezone(post_dst_timestamp, "UTC", "America/New_York")
  assert_eq(post_dst_est.format("YYYY-MM-DD HH:mm:ss Z"), "2021-03-31 20:00:00 EDT")
  
  // Test timezone detection from locale
  let detected_us = timezone_manager.detect_timezone("en-US")
  assert_true(detected_us.contains("America"))
  
  let detected_jp = timezone_manager.detect_timezone("ja-JP")
  assert_eq(detected_jp, "Asia/Tokyo")
  
  let detected_cn = timezone_manager.detect_timezone("zh-CN")
  assert_eq(detected_cn, "Asia/Shanghai")
  
  // Test timezone offset calculation
  let utc_offset = timezone_manager.get_timezone_offset("UTC")
  assert_eq(utc_offset, 0)
  
  let est_offset = timezone_manager.get_timezone_offset("America/New_York")
  assert_true(est_offset == -300 || est_offset == -240)  // EST or EDT
  
  let jst_offset = timezone_manager.get_timezone_offset("Asia/Tokyo")
  assert_eq(jst_offset, 540)  // UTC+9
  
  // Test timezone-aware relative time
  let now = 1609459200
  let past = now - 3600
  let relative_ny = timezone_manager.format_relative_time_with_timezone(
    past, now, "en-US", "America/New_York")
  assert_eq(relative_ny, "1 hour ago")
  
  let relative_tokyo = timezone_manager.format_relative_time_with_timezone(
    past, now, "ja-JP", "Asia/Tokyo")
  assert_eq(relative_tokyo, "1時間前")
}