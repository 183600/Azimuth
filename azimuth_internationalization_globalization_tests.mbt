// Azimuth Telemetry System - Internationalization and Globalization Tests
// This file contains comprehensive test cases for internationalization and globalization features

// Test 1: Multi-Language Error Messages
test "multi-language error messages" {
  let i18n_manager = I18nManager::new()
  
  // Register error messages in multiple languages
  I18nManager::register_error_messages(i18n_manager, "en", [
    ("connection.timeout", "Connection timeout occurred"),
    ("authentication.failed", "Authentication failed"),
    ("resource.not_found", "Resource not found"),
    ("permission.denied", "Permission denied"),
    ("server.error", "Internal server error")
  ])
  
  I18nManager::register_error_messages(i18n_manager, "zh", [
    ("connection.timeout", "连接超时"),
    ("authentication.failed", "身份验证失败"),
    ("resource.not_found", "资源未找到"),
    ("permission.denied", "权限被拒绝"),
    ("server.error", "内部服务器错误")
  ])
  
  I18nManager::register_error_messages(i18n_manager, "es", [
    ("connection.timeout", "Tiempo de conexión agotado"),
    ("authentication.failed", "Autenticación fallida"),
    ("resource.not_found", "Recurso no encontrado"),
    ("permission.denied", "Permiso denegado"),
    ("server.error", "Error del servidor interno")
  ])
  
  I18nManager::register_error_messages(i18n_manager, "fr", [
    ("connection.timeout", "Délai de connexion dépassé"),
    ("authentication.failed", "Échec de l'authentification"),
    ("resource.not_found", "Ressource non trouvée"),
    ("permission.denied", "Permission refusée"),
    ("server.error", "Erreur interne du serveur")
  ])
  
  // Test error message retrieval in different languages
  let en_error = I18nManager::get_error_message(i18n_manager, "en", "connection.timeout")
  assert_eq(en_error, "Connection timeout occurred")
  
  let zh_error = I18nManager::get_error_message(i18n_manager, "zh", "connection.timeout")
  assert_eq(zh_error, "连接超时")
  
  let es_error = I18nManager::get_error_message(i18n_manager, "es", "connection.timeout")
  assert_eq(es_error, "Tiempo de conexión agotado")
  
  let fr_error = I18nManager::get_error_message(i18n_manager, "fr", "connection.timeout")
  assert_eq(fr_error, "Délai de connexion dépassé")
  
  // Test fallback to default language
  let fallback_error = I18nManager::get_error_message(i18n_manager, "de", "connection.timeout")
  assert_eq(fallback_error, "Connection timeout occurred") // Falls back to English
  
  // Test parameterized error messages
  I18nManager::register_parametrized_error(i18n_manager, "en", "user.not_found", 
    "User {username} not found")
  I18nManager::register_parametrized_error(i18n_manager, "zh", "user.not_found", 
    "用户 {username} 未找到")
  
  let en_user_error = I18nManager::get_parametrized_error(i18n_manager, "en", 
    "user.not_found", [("username", "john_doe")])
  assert_eq(en_user_error, "User john_doe not found")
  
  let zh_user_error = I18nManager::get_parametrized_error(i18n_manager, "zh", 
    "user.not_found", [("username", "john_doe")])
  assert_eq(zh_user_error, "用户 john_doe 未找到")
}

// Test 2: Locale-Aware Date and Time Formatting
test "locale-aware date and time formatting" {
  let locale_formatter = LocaleFormatter::new()
  
  // Test timestamp
  let timestamp = 1672531200L // January 1, 2023 00:00:00 UTC
  
  // Format in different locales
  let en_date = LocaleFormatter::format_date(locale_formatter, timestamp, "en-US", "YYYY-MM-DD")
  assert_eq(en_date, "2023-01-01")
  
  let en_datetime = LocaleFormatter::format_datetime(locale_formatter, timestamp, "en-US", 
    "YYYY-MM-DD HH:mm:ss")
  assert_eq(en_datetime, "2023-01-01 00:00:00")
  
  let zh_date = LocaleFormatter::format_date(locale_formatter, timestamp, "zh-CN", "YYYY年MM月DD日")
  assert_eq(zh_date, "2023年01月01日")
  
  let zh_datetime = LocaleFormatter::format_datetime(locale_formatter, timestamp, "zh-CN", 
    "YYYY年MM月DD日 HH:mm:ss")
  assert_eq(zh_datetime, "2023年01月01日 00:00:00")
  
  let es_date = LocaleFormatter::format_date(locale_formatter, timestamp, "es-ES", "DD/MM/YYYY")
  assert_eq(es_date, "01/01/2023")
  
  let fr_date = LocaleFormatter::format_date(locale_formatter, timestamp, "fr-FR", "DD/MM/YYYY")
  assert_eq(fr_date, "01/01/2023")
  
  // Test relative time formatting
  let now = 1672531200L
  let past = 1672527600L // 1 hour ago
  let future = 1672534800L // 1 hour from now
  
  let en_relative_past = LocaleFormatter::format_relative_time(locale_formatter, past, now, "en-US")
  assert_eq(en_relative_past, "1 hour ago")
  
  let zh_relative_past = LocaleFormatter::format_relative_time(locale_formatter, past, now, "zh-CN")
  assert_eq(zh_relative_past, "1小时前")
  
  let en_relative_future = LocaleFormatter::format_relative_time(locale_formatter, future, now, "en-US")
  assert_eq(en_relative_future, "in 1 hour")
  
  let zh_relative_future = LocaleFormatter::format_relative_time(locale_formatter, future, now, "zh-CN")
  assert_eq(zh_relative_future, "1小时后")
  
  // Test time zone handling
  let utc_time = 1672531200L
  let est_time = LocaleFormatter::convert_timezone(locale_formatter, utc_time, "UTC", "America/New_York")
  let pst_time = LocaleFormatter::convert_timezone(locale_formatter, utc_time, "UTC", "America/Los_Angeles")
  
  // Verify time zone conversion (EST is UTC-5, PST is UTC-8 in January)
  assert_true(est_time < utc_time)
  assert_true(pst_time < utc_time)
  assert_true(est_time > pst_time)
}

// Test 3: Number and Currency Formatting
test "number and currency formatting" {
  let number_formatter = NumberFormatter::new()
  
  // Test basic number formatting
  let number = 1234567.89
  
  let en_number = NumberFormatter::format_number(number_formatter, number, "en-US")
  assert_eq(en_number, "1,234,567.89")
  
  let zh_number = NumberFormatter::format_number(number_formatter, number, "zh-CN")
  assert_eq(zh_number, "1,234,567.89")
  
  let de_number = NumberFormatter::format_number(number_formatter, number, "de-DE")
  assert_eq(de_number, "1.234.567,89")
  
  let fr_number = NumberFormatter::format_number(number_formatter, number, "fr-FR")
  assert_eq(fr_number, "1 234 567,89")
  
  // Test currency formatting
  let currency = 1234.56
  
  let usd = NumberFormatter::format_currency(number_formatter, currency, "en-US", "USD")
  assert_eq(usd, "$1,234.56")
  
  let eur = NumberFormatter::format_currency(number_formatter, currency, "de-DE", "EUR")
  assert_eq(eur, "1.234,56 €")
  
  let cny = NumberFormatter::format_currency(number_formatter, currency, "zh-CN", "CNY")
  assert_eq(cny, "¥1,234.56")
  
  let jpy = NumberFormatter::format_currency(number_formatter, currency, "ja-JP", "JPY")
  assert_eq(jpy, "¥1,235") // Japanese Yen doesn't use decimal places
  
  // Test percentage formatting
  let percentage = 0.7543
  
  let en_percent = NumberFormatter::format_percentage(number_formatter, percentage, "en-US")
  assert_eq(en_percent, "75.43%")
  
  let zh_percent = NumberFormatter::format_percentage(number_formatter, percentage, "zh-CN")
  assert_eq(zh_percent, "75.43%")
  
  // Test unit formatting
  let measurement = 1500.75
  
  let en_distance = NumberFormatter::format_unit(number_formatter, measurement, "en-US", "meter")
  assert_eq(en_distance, "1,500.75 meters")
  
  let zh_distance = NumberFormatter::format_unit(number_formatter, measurement, "zh-CN", "meter")
  assert_eq(zh_distance, "1,500.75米")
  
  // Test precision control
  let precise_number = 3.14159265359
  
  let low_precision = NumberFormatter::format_with_precision(number_formatter, precise_number, "en-US", 2)
  assert_eq(low_precision, "3.14")
  
  let high_precision = NumberFormatter::format_with_precision(number_formatter, precise_number, "en-US", 6)
  assert_eq(high_precision, "3.141593")
}

// Test 4: Text Direction and Script Support
test "text direction and script support" {
  let text_processor = TextProcessor::new()
  
  // Test left-to-right text (English)
  let ltr_text = "Hello World"
  assert_true(TextProcessor::is_ltr(text_processor, ltr_text))
  assert_false(TextProcessor::is_rtl(text_processor, ltr_text))
  
  // Test right-to-left text (Arabic)
  let rtl_text = "مرحبا بالعالم"
  assert_false(TextProcessor::is_ltr(text_processor, rtl_text))
  assert_true(TextProcessor::is_rtl(text_processor, rtl_text))
  
  // Test bidirectional text
  let bidi_text = "Hello مرحبا World"
  assert_true(TextProcessor::is_bidi(text_processor, bidi_text))
  
  // Test script detection
  let latin_script = TextProcessor::detect_script(text_processor, "Hello World")
  assert_eq(latin_script, "Latin")
  
  let arabic_script = TextProcessor::detect_script(text_processor, "مرحبا بالعالم")
  assert_eq(arabic_script, "Arabic")
  
  let chinese_script = TextProcessor::detect_script(text_processor, "你好世界")
  assert_eq(chinese_script, "Han")
  
  let cyrillic_script = TextProcessor::detect_script(text_processor, "Привет мир")
  assert_eq(cyrillic_script, "Cyrillic")
  
  // Test text normalization
  let accented_text = "café résumé naïve"
  let normalized_text = TextProcessor::normalize(text_processor, accented_text)
  assert_eq(normalized_text, "cafe resume naive")
  
  // Test text wrapping for different scripts
  let long_ltr_text = "This is a very long English sentence that should be wrapped properly"
  let ltr_wrapped = TextProcessor::wrap_text(text_processor, long_ltr_text, 20, "en")
  assert_true(ltr_wrapped.contains("\n"))
  
  let long_rtl_text = "هذه جملة عربية طويلة جدًا يجب أن يتم التفافها بشكل صحيح"
  let rtl_wrapped = TextProcessor::wrap_text(text_processor, long_rtl_text, 20, "ar")
  assert_true(rtl_wrapped.contains("\n"))
}

// Test 5: Collation and Sorting
test "collation and sorting" {
  let collator = Collator::new()
  
  // Test string sorting in different locales
  let strings = ["apple", "banana", "cherry", "date", "elderberry"]
  
  let en_sorted = Collator::sort_strings(collator, strings, "en-US")
  assert_eq(en_sorted, ["apple", "banana", "cherry", "date", "elderberry"])
  
  // Test case-insensitive sorting
  let mixed_case = ["Apple", "banana", "Cherry", "date", "Elderberry"]
  let case_insensitive = Collator::sort_strings_case_insensitive(collator, mixed_case, "en-US")
  assert_eq(case_insensitive, ["Apple", "banana", "Cherry", "date", "Elderberry"])
  
  // Test accent-sensitive sorting
  let accented_strings = ["café", "cane", "côte", "cote", "cant"]
  let accent_sensitive = Collator::sort_strings(collator, accented_strings, "fr-FR")
  assert_eq(accent_sensitive, ["cane", "cant", "café", "cote", "côte"])
  
  // Test Chinese character sorting by stroke count
  let chinese_chars = ["一", "三", "二", "五", "四"]
  let stroke_sorted = Collator::sort_chinese_by_stroke(collator, chinese_chars)
  assert_eq(stroke_sorted, ["一", "二", "三", "四", "五"])
  
  // Test Japanese character sorting (Hiragana)
  let japanese_hiragana = ["か", "き", "く", "け", "こ"]
  let hiragana_sorted = Collator::sort_strings(collator, japanese_hiragana, "ja-JP")
  assert_eq(hiragana_sorted, ["か", "き", "く", "け", "こ"])
  
  // Test numeric collation
  let numeric_strings = ["file1.txt", "file10.txt", "file2.txt", "file20.txt"]
  let numeric_sorted = Collator::sort_numeric(collator, numeric_strings, "en-US")
  assert_eq(numeric_sorted, ["file1.txt", "file2.txt", "file10.txt", "file20.txt"])
  
  // Test custom collation rules
  Collator::add_custom_rule(collator, "custom", ["ch", "ll", "ñ"])
  let spanish_strings = ["chocolate", "llama", "mañana", "nino"]
  let spanish_sorted = Collator::sort_strings_with_rules(collator, spanish_strings, "custom")
  assert_eq(spanish_sorted, ["chocolate", "llama", "mañana", "nino"])
}

// Test 6: Cultural and Regional Preferences
test "cultural and regional preferences" {
  let culture_manager = CultureManager::new()
  
  // Test week start day
  let us_week_start = CultureManager::get_week_start(culture_manager, "en-US")
  assert_eq(us_week_start, 0) // Sunday
  
  let uk_week_start = CultureManager::get_week_start(culture_manager, "en-GB")
  assert_eq(uk_week_start, 1) // Monday
  
  let sa_week_start = CultureManager::get_week_start(culture_manager, "ar-SA")
  assert_eq(sa_week_start, 6) // Saturday
  
  // Test measurement systems
  let us_measurement = CultureManager::get_measurement_system(culture_manager, "en-US")
  assert_eq(us_measurement, "Imperial")
  
  let uk_measurement = CultureManager::get_measurement_system(culture_manager, "en-GB")
  assert_eq(uk_measurement, "Metric")
  
  let jp_measurement = CultureManager::get_measurement_system(culture_manager, "ja-JP")
  assert_eq(jp_measurement, "Metric")
  
  // Test paper sizes
  let us_paper = CultureManager::get_default_paper_size(culture_manager, "en-US")
  assert_eq(us_paper, "Letter")
  
  let eu_paper = CultureManager::get_default_paper_size(culture_manager, "de-DE")
  assert_eq(eu_paper, "A4")
  
  // Test date format preferences
  let us_date_format = CultureManager::get_short_date_format(culture_manager, "en-US")
  assert_eq(us_date_format, "MM/DD/YYYY")
  
  let uk_date_format = CultureManager::get_short_date_format(culture_manager, "en-GB")
  assert_eq(uk_date_format, "DD/MM/YYYY")
  
  let iso_date_format = CultureManager::get_short_date_format(culture_manager, "sv-SE")
  assert_eq(iso_date_format, "YYYY-MM-DD")
  
  // Test time format preferences
  let us_time_format = CultureManager::get_time_format(culture_manager, "en-US")
  assert_eq(us_time_format, "12-hour")
  
  let eu_time_format = CultureManager::get_time_format(culture_manager, "de-DE")
  assert_eq(eu_time_format, "24-hour")
  
  // Test decimal separator
  let us_decimal = CultureManager::get_decimal_separator(culture_manager, "en-US")
  assert_eq(us_decimal, ".")
  
  let eu_decimal = CultureManager::get_decimal_separator(culture_manager, "de-DE")
  assert_eq(eu_decimal, ",")
  
  // Test thousands separator
  let us_thousands = CultureManager::get_thousands_separator(culture_manager, "en-US")
  assert_eq(us_thousands, ",")
  
  let eu_thousands = CultureManager::get_thousands_separator(culture_manager, "de-DE")
  assert_eq(eu_thousands, ".")
}

// Test 7: Pluralization Rules
test "pluralization rules" {
  let pluralizer = Pluralizer::new()
  
  // Test English pluralization
  Pluralizer::add_rule(pluralizer, "en", fn(n) {
    if n == 1 {
      return "one"
    } else {
      return "other"
    }
  })
  
  Pluralizer::add_plural_form(pluralizer, "en", "one", "{count} item")
  Pluralizer::add_plural_form(pluralizer, "en", "other", "{count} items")
  
  let en_one = Pluralizer::pluralize(pluralizer, "en", 1)
  assert_eq(en_one, "1 item")
  
  let en_other = Pluralizer::pluralize(pluralizer, "en", 5)
  assert_eq(en_other, "5 items")
  
  // Test Chinese pluralization (no plural forms)
  Pluralizer::add_rule(pluralizer, "zh", fn(n) {
    return "other"
  })
  
  Pluralizer::add_plural_form(pluralizer, "zh", "other", "{count} 项")
  
  let zh_one = Pluralizer::pluralize(pluralizer, "zh", 1)
  assert_eq(zh_one, "1 项")
  
  let zh_other = Pluralizer::pluralize(pluralizer, "zh", 5)
  assert_eq(zh_other, "5 项")
  
  // Test Arabic pluralization (complex rules)
  Pluralizer::add_rule(pluralizer, "ar", fn(n) {
    let n100 = n % 100
    if n == 0 {
      return "zero"
    } else if n == 1 {
      return "one"
    } else if n == 2 {
      return "two"
    } else if n100 >= 3 && n100 <= 10 {
      return "few"
    } else if n100 >= 11 && n100 <= 99 {
      return "many"
    } else {
      return "other"
    }
  })
  
  Pluralizer::add_plural_form(pluralizer, "ar", "zero", "لا عناصر")
  Pluralizer::add_plural_form(pluralizer, "ar", "one", "عنصر واحد")
  Pluralizer::add_plural_form(pluralizer, "ar", "two", "عنصران")
  Pluralizer::add_plural_form(pluralizer, "ar", "few", "{count} عناصر")
  Pluralizer::add_plural_form(pluralizer, "ar", "many", "{count} عنصر")
  Pluralizer::add_plural_form(pluralizer, "ar", "other", "{count} عنصر")
  
  let ar_zero = Pluralizer::pluralize(pluralizer, "ar", 0)
  assert_eq(ar_zero, "لا عناصر")
  
  let ar_one = Pluralizer::pluralize(pluralizer, "ar", 1)
  assert_eq(ar_one, "عنصر واحد")
  
  let ar_two = Pluralizer::pluralize(pluralizer, "ar", 2)
  assert_eq(ar_two, "عنصران")
  
  let ar_few = Pluralizer::pluralize(pluralizer, "ar", 5)
  assert_eq(ar_few, "5 عناصر")
  
  let ar_many = Pluralizer::pluralize(pluralizer, "ar", 25)
  assert_eq(ar_many, "25 عنصر")
  
  let ar_other = Pluralizer::pluralize(pluralizer, "ar", 105)
  assert_eq(ar_other, "105 عنصر")
}

// Test 8: Localization Resource Management
test "localization resource management" {
  let resource_manager = LocalizationResourceManager::new()
  
  // Register localization resources
  let en_resources = LocalizationResources::new("en", [
    ("welcome.message", "Welcome to our application"),
    ("user.greeting", "Hello, {name}!"),
    ("button.save", "Save"),
    ("button.cancel", "Cancel"),
    ("error.required", "The {field} field is required"),
    ("menu.home", "Home"),
    ("menu.settings", "Settings"),
    ("menu.logout", "Logout")
  ])
  
  let zh_resources = LocalizationResources::new("zh", [
    ("welcome.message", "欢迎使用我们的应用程序"),
    ("user.greeting", "你好，{name}！"),
    ("button.save", "保存"),
    ("button.cancel", "取消"),
    ("error.required", "{field} 字段是必填的"),
    ("menu.home", "首页"),
    ("menu.settings", "设置"),
    ("menu.logout", "退出")
  ])
  
  LocalizationResourceManager::register_resources(resource_manager, en_resources)
  LocalizationResourceManager::register_resources(resource_manager, zh_resources)
  
  // Test resource retrieval
  let en_welcome = LocalizationResourceManager::get_string(resource_manager, "en", "welcome.message")
  assert_eq(en_welcome, "Welcome to our application")
  
  let zh_welcome = LocalizationResourceManager::get_string(resource_manager, "zh", "welcome.message")
  assert_eq(zh_welcome, "欢迎使用我们的应用程序")
  
  // Test parameterized resources
  let en_greeting = LocalizationResourceManager::get_formatted_string(resource_manager, "en", 
    "user.greeting", [("name", "John")])
  assert_eq(en_greeting, "Hello, John!")
  
  let zh_greeting = LocalizationResourceManager::get_formatted_string(resource_manager, "zh", 
    "user.greeting", [("name", "张三")])
  assert_eq(zh_greeting, "你好，张三！")
  
  // Test resource fallback
  let fallback_string = LocalizationResourceManager::get_string_with_fallback(resource_manager, 
    "fr", "welcome.message", "en")
  assert_eq(fallback_string, "Welcome to our application") // Falls back to English
  
  // Test resource inheritance
  let en_us_resources = LocalizationResources::new("en-US", [
    ("menu.home", "Home Page") // Override specific resource
  ])
  
  LocalizationResourceManager::register_inherited_resources(resource_manager, en_us_resources, "en")
  
  let en_us_home = LocalizationResourceManager::get_string(resource_manager, "en-US", "menu.home")
  assert_eq(en_us_home, "Home Page") // Uses overridden value
  
  let en_us_settings = LocalizationResourceManager::get_string(resource_manager, "en-US", "menu.settings")
  assert_eq(en_us_settings, "Settings") // Inherits from parent
  
  // Test resource hot-reloading
  let updated_en_resources = LocalizationResources::new("en", [
    ("welcome.message", "Welcome to our updated application"),
    ("new.feature", "New Feature Available")
  ])
  
  LocalizationResourceManager::update_resources(resource_manager, updated_en_resources)
  
  let updated_welcome = LocalizationResourceManager::get_string(resource_manager, "en", "welcome.message")
  assert_eq(updated_welcome, "Welcome to our updated application")
  
  let new_feature = LocalizationResourceManager::get_string(resource_manager, "en", "new.feature")
  assert_eq(new_feature, "New Feature Available")
}

// Test 9: Time Zone and Calendar Support
test "time zone and calendar support" {
  let calendar_manager = CalendarManager::new()
  
  // Test different calendar systems
  let gregorian_date = DateTime::new(2023, 1, 15, 14, 30, 0)
  
  let gregorian_formatted = CalendarManager::format_date(calendar_manager, gregorian_date, "gregorian", "en-US")
  assert_eq(gregorian_formatted, "January 15, 2023")
  
  let islamic_date = CalendarManager::convert_to_calendar(calendar_manager, gregorian_date, "islamic")
  let islamic_formatted = CalendarManager::format_date(calendar_manager, islamic_date, "islamic", "ar-SA")
  assert_true(islamic_formatted.contains("1444")) // Islamic year
  
  let hebrew_date = CalendarManager::convert_to_calendar(calendar_manager, gregorian_date, "hebrew")
  let hebrew_formatted = CalendarManager::format_date(calendar_manager, hebrew_date, "hebrew", "he-IL")
  assert_true(hebrew_formatted.contains("5783")) // Hebrew year
  
  let chinese_date = CalendarManager::convert_to_calendar(calendar_manager, gregorian_date, "chinese")
  let chinese_formatted = CalendarManager::format_date(calendar_manager, chinese_date, "chinese", "zh-CN")
  assert_true(chinese_formatted.contains("2023")) // Chinese year
  
  // Test time zone conversions
  let utc_time = DateTime::new(2023, 1, 15, 12, 0, 0)
  
  let est_time = CalendarManager::convert_timezone(calendar_manager, utc_time, "UTC", "America/New_York")
  let pst_time = CalendarManager::convert_timezone(calendar_manager, utc_time, "UTC", "America/Los_Angeles")
  let cet_time = CalendarManager::convert_timezone(calendar_manager, utc_time, "UTC", "Europe/Paris")
  let jst_time = CalendarManager::convert_timezone(calendar_manager, utc_time, "UTC", "Asia/Tokyo")
  
  // Verify time zone differences
  assert_true(est_time.hour < utc_time.hour) // EST is UTC-5 in January
  assert_true(pst_time.hour < utc_time.hour) // PST is UTC-8 in January
  assert_true(cet_time.hour > utc_time.hour) // CET is UTC+1
  assert_true(jst_time.hour > utc_time.hour) // JST is UTC+9
  
  // Test daylight saving time awareness
  let summer_time = DateTime::new(2023, 7, 15, 12, 0, 0)
  let summer_est = CalendarManager::convert_timezone(calendar_manager, summer_time, "UTC", "America/New_York")
  
  let winter_time = DateTime::new(2023, 1, 15, 12, 0, 0)
  let winter_est = CalendarManager::convert_timezone(calendar_manager, winter_time, "UTC", "America/New_York")
  
  // EST should be UTC-4 in summer (EDT) and UTC-5 in winter
  assert_true(summer_est.hour > winter_est.hour)
  
  // Test time zone database
  let time_zones = CalendarManager::get_available_time_zones(calendar_manager)
  assert_true(time_zones.contains("America/New_York"))
  assert_true(time_zones.contains("Europe/London"))
  assert_true(time_zones.contains("Asia/Tokyo"))
  assert_true(time_zones.contains("Australia/Sydney"))
  
  // Test time zone offset calculation
  let utc_offset = CalendarManager::get_timezone_offset(calendar_manager, "America/New_York", utc_time)
  assert_eq(utc_offset, -18000) // -5 hours in seconds
  
  let summer_offset = CalendarManager::get_timezone_offset(calendar_manager, "America/New_York", summer_time)
  assert_eq(summer_offset, -14400) // -4 hours in seconds (DST)
}

// Test 10: International Telemetry Data Processing
test "international telemetry data processing" {
  let telemetry_processor = InternationalTelemetryProcessor::new()
  
  // Process telemetry data with international content
  let telemetry_events = [
    TelemetryEvent::new("user_login", "en-US", [
      ("user.id", StringValue("user123")),
      ("user.name", StringValue("John Doe")),
      ("location", StringValue("New York, USA")),
      ("browser.language", StringValue("en-US"))
    ]),
    TelemetryEvent::new("user_login", "zh-CN", [
      ("user.id", StringValue("user456")),
      ("user.name", StringValue("张三")),
      ("location", StringValue("北京, 中国")),
      ("browser.language", StringValue("zh-CN"))
    ]),
    TelemetryEvent::new("user_login", "ja-JP", [
      ("user.id", StringValue("user789")),
      ("user.name", StringValue("田中太郎")),
      ("location", StringValue("東京, 日本")),
      ("browser.language", StringValue("ja-JP"))
    ]),
    TelemetryEvent::new("user_login", "ar-SA", [
      ("user.id", StringValue("user101")),
      ("user.name", StringValue("أحمد محمد")),
      ("location", StringValue("الرياض، السعودية")),
      ("browser.language", StringValue("ar-SA"))
    ])
  ]
  
  // Process international telemetry
  let processed_data = InternationalTelemetryProcessor::process_events(telemetry_processor, telemetry_events)
  
  // Verify language distribution
  let language_stats = InternationalTelemetryProcessor::get_language_distribution(telemetry_processor, processed_data)
  assert_eq(language_stats["en-US"], 1)
  assert_eq(language_stats["zh-CN"], 1)
  assert_eq(language_stats["ja-JP"], 1)
  assert_eq(language_stats["ar-SA"], 1)
  
  // Verify text direction analysis
  let direction_stats = InternationalTelemetryProcessor::get_text_direction_stats(telemetry_processor, processed_data)
  assert_eq(direction_stats["ltr"], 3) // English, Chinese, Japanese
  assert_eq(direction_stats["rtl"], 1) // Arabic
  
  // Verify character encoding analysis
  let encoding_stats = InternationalTelemetryProcessor::get_character_encoding_stats(telemetry_processor, processed_data)
  assert_true(encoding_stats["latin"] > 0)
  assert_true(encoding_stats["han"] > 0)
  assert_true(encoding_stats["arabic"] > 0)
  
  // Test localized aggregation
  let localized_metrics = InternationalTelemetryProcessor::aggregate_by_locale(telemetry_processor, processed_data)
  
  let en_metrics = localized_metrics["en-US"]
  assert_eq(en_metrics.event_count, 1)
  assert_eq(en_metrics.user_names[0], "John Doe")
  
  let zh_metrics = localized_metrics["zh-CN"]
  assert_eq(zh_metrics.event_count, 1)
  assert_eq(zh_metrics.user_names[0], "张三")
  
  // Test international error reporting
  let error_events = [
    TelemetryError::new("validation_failed", "en-US", [
      ("field", StringValue("email")),
      ("message", StringValue("Invalid email format"))
    ]),
    TelemetryError::new("validation_failed", "zh-CN", [
      ("field", StringValue("email")),
      ("message", StringValue("邮箱格式无效"))
    ]),
    TelemetryError::new("validation_failed", "ja-JP", [
      ("field", StringValue("email")),
      ("message", StringValue("メール形式が無効です"))
    ])
  ]
  
  let error_analysis = InternationalTelemetryProcessor::analyze_errors(telemetry_processor, error_events)
  
  // Verify error message localization
  let en_errors = error_analysis["en-US"]
  assert_eq(en_errors[0].message, "Invalid email format")
  
  let zh_errors = error_analysis["zh-CN"]
  assert_eq(zh_errors[0].message, "邮箱格式无效")
  
  let ja_errors = error_analysis["ja-JP"]
  assert_eq(ja_errors[0].message, "メール形式が無効です")
  
  // Test international performance metrics
  let performance_data = InternationalTelemetryProcessor::calculate_performance_metrics(telemetry_processor, processed_data)
  
  assert_true(performance_data.total_events > 0)
  assert_true(performance_data.average_processing_time > 0)
  assert_true(performance_data.locale_processing_times.contains("en-US"))
  assert_true(performance_data.locale_processing_times.contains("zh-CN"))
  assert_true(performance_data.locale_processing_times.contains("ja-JP"))
  assert_true(performance_data.locale_processing_times.contains("ar-SA"))
}