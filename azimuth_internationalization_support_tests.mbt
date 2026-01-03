// Azimuth Telemetry System - Internationalization Support Tests
// This file contains test cases for internationalization functionality

// Test 1: Locale Creation and Management
test "locale creation and management" {
  // Create different locales
  let en_us_locale = Locale::new("en-US", "English (United States)", "latn", "metric")
  let zh_cn_locale = Locale::new("zh-CN", "中文 (中国)", "hans", "metric")
  let ja_jp_locale = Locale::new("ja-JP", "日本語 (日本)", "jpan", "metric")
  let de_de_locale = Locale::new("de-DE", "Deutsch (Deutschland)", "latn", "metric")
  let ar_sa_locale = Locale::new("ar-SA", "العربية (المملكة العربية السعودية)", "arab", "metric")
  
  // Verify locale properties
  assert_eq(Locale::code(en_us_locale), "en-US")
  match Locale::display_name(en_us_locale) {
    Some(name) => assert_eq(name, "English (United States)")
    None => assert_true(false)
  }
  match Locale::script(en_us_locale) {
    Some(script) => assert_eq(script, "latn")
    None => assert_true(false)
  }
  match Locale::measurement_system(en_us_locale) {
    Some(system) => assert_eq(system, "metric")
    None => assert_true(false)
  }
  
  // Test locale with right-to-left script
  assert_eq(Locale::is_rtl(ar_sa_locale), true)
  assert_eq(Locale::is_rtl(en_us_locale), false)
  assert_eq(Locale::is_rtl(zh_cn_locale), false)
  
  // Test locale with different measurement systems
  let en_us_imperial = Locale::new("en-US", "English (United States)", "latn", "us_customary")
  match Locale::measurement_system(en_us_imperial) {
    Some(system) => assert_eq(system, "us_customary")
    None => assert_true(false)
  }
  
  // Test locale comparison
  assert_true(Locale::matches(en_us_locale, "en-US"))
  assert_true(Locale::matches_language(en_us_locale, "en"))
  assert_true(Locale::matches_region(en_us_locale, "US"))
  assert_false(Locale::matches(en_us_locale, "zh-CN"))
  assert_false(Locale::matches_language(en_us_locale, "zh"))
}

// Test 2: Message Translation and Localization
test "message translation and localization" {
  // Create translation manager
  let translation_manager = TranslationManager::new()
  
  // Add translations for different locales
  let en_us_translations = [
    ("cpu.usage.high", "CPU usage is high"),
    ("memory.usage.low", "Memory usage is low"),
    ("disk.space.warning", "Disk space warning"),
    ("network.error", "Network error"),
    ("service.unavailable", "Service unavailable")
  ]
  
  let zh_cn_translations = [
    ("cpu.usage.high", "CPU使用率过高"),
    ("memory.usage.low", "内存使用率过低"),
    ("disk.space.warning", "磁盘空间警告"),
    ("network.error", "网络错误"),
    ("service.unavailable", "服务不可用")
  ]
  
  let ja_jp_translations = [
    ("cpu.usage.high", "CPU使用率が高いです"),
    ("memory.usage.low", "メモリ使用率が低いです"),
    ("disk.space.warning", "ディスク容量の警告"),
    ("network.error", "ネットワークエラー"),
    ("service.unavailable", "サービスが利用できません")
  ]
  
  // Add translations to manager
  let manager_with_translations = TranslationManager::add_translations(
    TranslationManager::add_translations(
      TranslationManager::add_translations(translation_manager, "en-US", en_us_translations),
      "zh-CN",
      zh_cn_translations),
    "ja-JP",
    ja_jp_translations
  )
  
  // Test translation for English
  let en_cpu_message = TranslationManager::translate(manager_with_translations, "en-US", "cpu.usage.high")
  match en_cpu_message {
    Some(message) => assert_eq(message, "CPU usage is high")
    None => assert_true(false)
  }
  
  // Test translation for Chinese
  let zh_cpu_message = TranslationManager::translate(manager_with_translations, "zh-CN", "cpu.usage.high")
  match zh_cpu_message {
    Some(message) => assert_eq(message, "CPU使用率过高")
    None => assert_true(false)
  }
  
  // Test translation for Japanese
  let ja_cpu_message = TranslationManager::translate(manager_with_translations, "ja-JP", "cpu.usage.high")
  match ja_cpu_message {
    Some(message) => assert_eq(message, "CPU使用率が高いです")
    None => assert_true(false)
  }
  
  // Test fallback to English when translation is not available
  let fr_cpu_message = TranslationManager::translate(manager_with_translations, "fr-FR", "cpu.usage.high")
  match fr_cpu_message {
    Some(message) => assert_eq(message, "CPU usage is high") // Fallback to English
    None => assert_true(false)
  }
  
  // Test when key doesn't exist
  let non_existent_message = TranslationManager::translate(manager_with_translations, "en-US", "non.existent.key")
  match non_existent_message {
    Some(message) => assert_eq(message, "non.existent.key") // Return key as fallback
    None => assert_true(false)
  }
}

// Test 3: Number and Currency Formatting
test "number and currency formatting" {
  // Create formatter manager
  let formatter_manager = FormatterManager::new()
  
  // Test number formatting for different locales
  let en_us_number = FormatterManager::format_number(formatter_manager, "en-US", 1234567.89)
  assert_eq(en_us_number, "1,234,567.89")
  
  let de_de_number = FormatterManager::format_number(formatter_manager, "de-DE", 1234567.89)
  assert_eq(de_de_number, "1.234.567,89")
  
  let zh_cn_number = FormatterManager::format_number(formatter_manager, "zh-CN", 1234567.89)
  assert_eq(zh_cn_number, "1,234,567.89")
  
  let fr_fr_number = FormatterManager::format_number(formatter_manager, "fr-FR", 1234567.89)
  assert_eq(fr_fr_number, "1\u{202F}234\u{202F}567,89") // Using non-breaking space
  
  let ar_sa_number = FormatterManager::format_number(formatter_manager, "ar-SA", 1234567.89)
  assert_eq(ar_sa_number, "١٬٢٣٤٬٥٦٧٫٨٩") // Arabic-Indic digits
  
  // Test percentage formatting
  let en_us_percentage = FormatterManager::format_percentage(formatter_manager, "en-US", 0.7542)
  assert_eq(en_us_percentage, "75.42%")
  
  let zh_cn_percentage = FormatterManager::format_percentage(formatter_manager, "zh-CN", 0.7542)
  assert_eq(zh_cn_percentage, "75.42%")
  
  // Test currency formatting
  let en_us_currency = FormatterManager::format_currency(formatter_manager, "en-US", 1234.56, "USD")
  assert_eq(en_us_currency, "$1,234.56")
  
  let en_gb_currency = FormatterManager::format_currency(formatter_manager, "en-GB", 1234.56, "GBP")
  assert_eq(en_gb_currency, "£1,234.56")
  
  let ja_jp_currency = FormatterManager::format_currency(formatter_manager, "ja-JP", 1234.56, "JPY")
  assert_eq(ja_jp_currency, "￥1,235") // Japanese yen doesn't use decimal places
  
  let de_de_currency = FormatterManager::format_currency(formatter_manager, "de-DE", 1234.56, "EUR")
  assert_eq(de_de_currency, "1.234,56\u{A0}€")
  
  let fr_fr_currency = FormatterManager::format_currency(formatter_manager, "fr-FR", 1234.56, "EUR")
  assert_eq(fr_fr_currency, "1\u{202F}234,56\u{A0}€")
}

// Test 4: Date and Time Formatting
test "date and time formatting" {
  // Create formatter manager
  let formatter_manager = FormatterManager::new()
  
  // Test timestamp: 2022-01-01 12:34:56 UTC (1641039296000L)
  let timestamp = 1641039296000L
  
  // Test date formatting for different locales
  let en_us_date = FormatterManager::format_date(formatter_manager, "en-US", timestamp, "medium")
  assert_eq(en_us_date, "Jan 1, 2022")
  
  let zh_cn_date = FormatterManager::format_date(formatter_manager, "zh-CN", timestamp, "medium")
  assert_eq(zh_cn_date, "2022年1月1日")
  
  let ja_jp_date = FormatterManager::format_date(formatter_manager, "ja-JP", timestamp, "medium")
  assert_eq(ja_jp_date, "2022/01/01")
  
  let de_de_date = FormatterManager::format_date(formatter_manager, "de-DE", timestamp, "medium")
  assert_eq(de_de_date, "1. Jan. 2022")
  
  let fr_fr_date = FormatterManager::format_date(formatter_manager, "fr-FR", timestamp, "medium")
  assert_eq(fr_fr_date, "1 janv. 2022")
  
  // Test time formatting for different locales
  let en_us_time = FormatterManager::format_time(formatter_manager, "en-US", timestamp, "medium")
  assert_eq(en_us_time, "12:34:56 PM")
  
  let zh_cn_time = FormatterManager::format_time(formatter_manager, "zh-CN", timestamp, "medium")
  assert_eq(zh_cn_time, "下午 12:34:56")
  
  let ja_jp_time = FormatterManager::format_time(formatter_manager, "ja-JP", timestamp, "medium")
  assert_eq(ja_jp_time, "12:34:56")
  
  let de_de_time = FormatterManager::format_time(formatter_manager, "de-DE", timestamp, "medium")
  assert_eq(de_de_time, "12:34:56")
  
  // Test datetime formatting for different locales
  let en_us_datetime = FormatterManager::format_datetime(formatter_manager, "en-US", timestamp, "medium")
  assert_eq(en_us_datetime, "Jan 1, 2022, 12:34:56 PM")
  
  let zh_cn_datetime = FormatterManager::format_datetime(formatter_manager, "zh-CN", timestamp, "medium")
  assert_eq(zh_cn_datetime, "2022年1月1日 下午12:34:56")
  
  let ja_jp_datetime = FormatterManager::format_datetime(formatter_manager, "ja-JP", timestamp, "medium")
  assert_eq(ja_jp_datetime, "2022/01/01 12:34:56")
  
  // Test relative time formatting
  let now = 1641039296000L
  let past = now - 3600000L // 1 hour ago
  let future = now + 7200000L // 2 hours in future
  
  let en_us_relative_past = FormatterManager::format_relative_time(formatter_manager, "en-US", past, now)
  assert_eq(en_us_relative_past, "1 hour ago")
  
  let en_us_relative_future = FormatterManager::format_relative_time(formatter_manager, "en-US", future, now)
  assert_eq(en_us_relative_future, "in 2 hours")
  
  let zh_cn_relative_past = FormatterManager::format_relative_time(formatter_manager, "zh-CN", past, now)
  assert_eq(zh_cn_relative_past, "1小时前")
  
  let zh_cn_relative_future = FormatterManager::format_relative_time(formatter_manager, "zh-CN", future, now)
  assert_eq(zh_cn_relative_future, "2小时后")
}

// Test 5: Unit and Measurement Localization
test "unit and measurement localization" {
  // Create measurement manager
  let measurement_manager = MeasurementManager::new()
  
  // Test temperature conversion and formatting
  let celsius_temp = 25.0
  
  // Metric system (Celsius)
  let metric_temp = MeasurementManager::format_temperature(measurement_manager, "zh-CN", celsius_temp, "celsius")
  assert_eq(metric_temp, "25°C")
  
  // US customary system (Fahrenheit)
  let us_temp = MeasurementManager::format_temperature(measurement_manager, "en-US", celsius_temp, "fahrenheit")
  assert_eq(us_temp, "77°F") // 25°C = 77°F
  
  // Test distance conversion and formatting
  let km_distance = 10.5
  
  // Metric system
  let metric_distance = MeasurementManager::format_distance(measurement_manager, "de-DE", km_distance, "kilometers")
  assert_eq(metric_distance, "10,5 km")
  
  // US customary system
  let us_distance = MeasurementManager::format_distance(measurement_manager, "en-US", km_distance, "miles")
  assert_eq(us_distance, "6.52 mi") // 10.5 km ≈ 6.52 miles
  
  // Test weight conversion and formatting
  let kg_weight = 5.0
  
  // Metric system
  let metric_weight = MeasurementManager::format_weight(measurement_manager, "fr-FR", kg_weight, "kilograms")
  assert_eq(metric_weight, "5 kg")
  
  // US customary system
  let us_weight = MeasurementManager::format_weight(measurement_manager, "en-US", kg_weight, "pounds")
  assert_eq(us_weight, "11.02 lb") // 5 kg ≈ 11.02 pounds
  
  // Test data size formatting
  let bytes_size = 1073741824L // 1 GB
  
  let en_us_size = MeasurementManager::format_data_size(measurement_manager, "en-US", bytes_size)
  assert_eq(en_us_size, "1 GB")
  
  let zh_cn_size = MeasurementManager::format_data_size(measurement_manager, "zh-CN", bytes_size)
  assert_eq(zh_cn_size, "1 GB")
  
  let ja_jp_size = MeasurementManager::format_data_size(measurement_manager, "ja-JP", bytes_size)
  assert_eq(ja_jp_size, "1 GB")
  
  // Test rate formatting (e.g., requests per second)
  let rate_value = 1500.5
  
  let en_us_rate = MeasurementManager::format_rate(measurement_manager, "en-US", rate_value, "requests", "second")
  assert_eq(en_us_rate, "1,500.5 requests/second")
  
  let zh_cn_rate = MeasurementManager::format_rate(measurement_manager, "zh-CN", rate_value, "请求", "秒")
  assert_eq(zh_cn_rate, "1,500.5 请求/秒")
  
  let ja_jp_rate = MeasurementManager::format_rate(measurement_manager, "ja-JP", rate_value, "リクエスト", "秒")
  assert_eq(ja_jp_rate, "1,500.5 リクエスト/秒")
}

// Test 6: Text Direction and Script Handling
test "text direction and script handling" {
  // Create text direction manager
  let direction_manager = TextDirectionManager::new()
  
  // Test text direction detection
  let latin_text = "Hello World"
  let arabic_text = "مرحبا بالعالم"
  let hebrew_text = "שלום עולם"
  let mixed_text = "Hello مرحبا World"
  
  assert_eq(TextDirectionManager::detect_direction(latin_text), "ltr")
  assert_eq(TextDirectionManager::detect_direction(arabic_text), "rtl")
  assert_eq(TextDirectionManager::detect_direction(hebrew_text), "rtl")
  assert_eq(TextDirectionManager::detect_direction(mixed_text), "ltr") // Default to first strong character
  
  // Test text wrapping for different directions
  let ltr_wrapped = TextDirectionManager::wrap_text(direction_manager, latin_text, "ltr")
  assert_eq(ltr_wrapped, "Hello World")
  
  let rtl_wrapped = TextDirectionManager::wrap_text(direction_manager, arabic_text, "rtl")
  assert_eq(rtl_wrapped, "\u{202B}مرحبا بالعالم\u{202C}") // Add RTL markers
  
  // Test script detection
  assert_eq(TextDirectionManager::detect_script(latin_text), "Latin")
  assert_eq(TextDirectionManager::detect_script(arabic_text), "Arabic")
  assert_eq(TextDirectionManager::detect_script(hebrew_text), "Hebrew")
  
  // Test Chinese script detection
  let chinese_text = "你好世界"
  assert_eq(TextDirectionManager::detect_script(chinese_text), "Han")
  
  // Test Japanese script detection (mixed)
  let japanese_text = "こんにちは世界"
  assert_eq(TextDirectionManager::detect_script(japanese_text), "Hiragana")
  
  // Test Korean script detection
  let korean_text = "안녕하세요 세계"
  assert_eq(TextDirectionManager::detect_script(korean_text), "Hangul")
  
  // Test alignment for different text directions
  let ltr_alignment = TextDirectionManager::get_alignment(direction_manager, "ltr")
  assert_eq(ltr_alignment, "left")
  
  let rtl_alignment = TextDirectionManager::get_alignment(direction_manager, "rtl")
  assert_eq(rtl_alignment, "right")
  
  // Test margin/padding adjustments for RTL
  let ltr_margins = TextDirectionManager::adjust_margins(direction_manager, [10, 20, 30, 40], "ltr") // top, right, bottom, left
  assert_eq(ltr_margins, [10, 20, 30, 40])
  
  let rtl_margins = TextDirectionManager::adjust_margins(direction_manager, [10, 20, 30, 40], "rtl") // top, right, bottom, left
  assert_eq(rtl_margins, [10, 40, 30, 20]) // Swap right and left
}

// Test 7: Localized Error Messages
test "localized error messages" {
  // Create error manager
  let error_manager = ErrorManager::new()
  
  // Add localized error messages
  let en_us_errors = [
    ("connection.failed", "Connection failed"),
    ("timeout.error", "Request timeout"),
    ("authentication.failed", "Authentication failed"),
    ("permission.denied", "Permission denied"),
    ("resource.not.found", "Resource not found")
  ]
  
  let zh_cn_errors = [
    ("connection.failed", "连接失败"),
    ("timeout.error", "请求超时"),
    ("authentication.failed", "身份验证失败"),
    ("permission.denied", "权限被拒绝"),
    ("resource.not.found", "资源未找到")
  ]
  
  let ja_jp_errors = [
    ("connection.failed", "接続に失敗しました"),
    ("timeout.error", "リクエストタイムアウト"),
    ("authentication.failed", "認証に失敗しました"),
    ("permission.denied", "アクセス拒否"),
    ("resource.not.found", "リソースが見つかりません")
  ]
  
  // Add error messages to manager
  let manager_with_errors = ErrorManager::add_error_messages(
    ErrorManager::add_error_messages(
      ErrorManager::add_error_messages(error_manager, "en-US", en_us_errors),
      "zh-CN",
      zh_cn_errors),
    "ja-JP",
    ja_jp_errors
  )
  
  // Test localized error messages
  let en_error = ErrorManager::get_error_message(manager_with_errors, "en-US", "connection.failed")
  match en_error {
    Some(message) => assert_eq(message, "Connection failed")
    None => assert_true(false)
  }
  
  let zh_error = ErrorManager::get_error_message(manager_with_errors, "zh-CN", "connection.failed")
  match zh_error {
    Some(message) => assert_eq(message, "连接失败")
    None => assert_true(false)
  }
  
  let ja_error = ErrorManager::get_error_message(manager_with_errors, "ja-JP", "connection.failed")
  match ja_error {
    Some(message) => assert_eq(message, "接続に失敗しました")
    None => assert_true(false)
  }
  
  // Test error message with parameters
  let param_error = ErrorManager::format_error_message(
    manager_with_errors,
    "en-US",
    "timeout.error",
    [("timeout", "30s"), ("endpoint", "/api/data")]
  )
  match param_error {
    Some(message) => assert_eq(message, "Request timeout after 30s to endpoint /api/data")
    None => assert_true(false)
  }
  
  let zh_param_error = ErrorManager::format_error_message(
    manager_with_errors,
    "zh-CN",
    "timeout.error",
    [("timeout", "30s"), ("endpoint", "/api/data")]
  )
  match zh_param_error {
    Some(message) => assert_eq(message, "请求超时，30秒后到端点/api/data")
    None => assert_true(false)
  }
  
  // Test error message fallback
  let non_existent_locale = ErrorManager::get_error_message(manager_with_errors, "fr-FR", "connection.failed")
  match non_existent_locale {
    Some(message) => assert_eq(message, "Connection failed") // Fallback to English
    None => assert_true(false)
  }
}

// Test 8: Locale-Aware Sorting and Collation
test "locale aware sorting and collation" {
  // Create collation manager
  let collation_manager = CollationManager::new()
  
  // Test strings with different locales
  let strings = ["apple", "banana", "cherry", "date", "elderberry"]
  
  // English sorting
  let en_sorted = CollationManager::sort_strings(collation_manager, "en-US", strings)
  assert_eq(en_sorted, ["apple", "banana", "cherry", "date", "elderberry"])
  
  // Test Chinese sorting
  let chinese_strings = ["苹果", "香蕉", "樱桃", "枣", "接骨木"]
  let zh_sorted = CollationManager::sort_strings(collation_manager, "zh-CN", chinese_strings)
  assert_eq(zh_sorted, ["枣", "苹果", "接骨木", "樱桃", "香蕉"]) // Pinyin order
  
  // Test Japanese sorting
  let japanese_strings = ["りんご", "バナナ", "チェリー", "デート", "エルダーベリー"]
  let ja_sorted = CollationManager::sort_strings(collation_manager, "ja-JP", japanese_strings)
  assert_eq(ja_sorted, ["りんご", "バナナ", "チェリー", "デート", "エルダーベリー"]) // Gojuon order
  
  // Test German sorting (with umlauts)
  let german_strings = ["äpfel", "bananen", "kirschen", "datteln", "holunderbeeren"]
  let de_sorted = CollationManager::sort_strings(collation_manager, "de-DE", german_strings)
  assert_eq(de_sorted, ["äpfel", "bananen", "datteln", "holunderbeeren", "kirschen"]) // ä treated like ae
  
  // Test case sensitivity
  let mixed_case_strings = ["Apple", "banana", "Cherry", "date", "Elderberry"]
  
  // Case-insensitive sorting (default)
  let case_insensitive = CollationManager::sort_strings(collation_manager, "en-US", mixed_case_strings)
  assert_eq(case_insensitive, ["Apple", "banana", "Cherry", "date", "Elderberry"])
  
  // Case-sensitive sorting
  let case_sensitive = CollationManager::sort_strings_with_options(
    collation_manager,
    "en-US",
    mixed_case_strings,
    [("case_sensitive", "true")]
  )
  assert_eq(case_sensitive, ["Apple", "Cherry", "Elderberry", "banana", "date"]) // Uppercase before lowercase
  
  // Test numeric sorting
  let numeric_strings = ["file1.txt", "file10.txt", "file2.txt", "file20.txt"]
  let numeric_sorted = CollationManager::sort_strings_with_options(
    collation_manager,
    "en-US",
    numeric_strings,
    [("numeric", "true")]
  )
  assert_eq(numeric_sorted, ["file1.txt", "file2.txt", "file10.txt", "file20.txt"]) // Numeric order
  
  // Test comparison
  let comparison_result = CollationManager::compare_strings(collation_manager, "en-US", "apple", "banana")
  assert_true(comparison_result < 0) // apple comes before banana
  
  let reverse_comparison = CollationManager::compare_strings(collation_manager, "en-US", "banana", "apple")
  assert_true(reverse_comparison > 0) // banana comes after apple
  
  let equal_comparison = CollationManager::compare_strings(collation_manager, "en-US", "apple", "apple")
  assert_eq(equal_comparison, 0) // strings are equal
}