// Azimuth Telemetry System - Internationalization Tests
// This file contains comprehensive test cases for internationalization functionality

// Test 1: Basic Localization Tests
test "basic localization operations" {
  // Test localization manager
  let i18n = I18nManager::new()
  
  // Load language resources
  I18nManager::load_language(i18n, "en", {
    "welcome.message": "Welcome to Azimuth Telemetry",
    "service.status": "Service is {status}",
    "error.not_found": "Resource not found",
    "items.count": "{count, plural, one {# item} other {# items}}",
    "user.greeting": "Hello, {name}!"
  })
  
  I18nManager::load_language(i18n, "zh", {
    "welcome.message": "欢迎使用 Azimuth 遥测系统",
    "service.status": "服务状态：{status}",
    "error.not_found": "未找到资源",
    "items.count": "{count, plural, other {# 项}}",
    "user.greeting": "你好，{name}！"
  })
  
  I18nManager::load_language(i18n, "es", {
    "welcome.message": "Bienvenido a Azimuth Telemetry",
    "service.status": "El servicio está {status}",
    "error.not_found": "Recurso no encontrado",
    "items.count": "{count, plural, one {# elemento} other {# elementos}}",
    "user.greeting": "¡Hola, {name}!"
  })
  
  // Set default language
  I18nManager::set_default_language(i18n, "en")
  
  // Test basic translation
  let welcome_en = I18nManager::translate(i18n, "welcome.message")
  assert_eq(welcome_en, "Welcome to Azimuth Telemetry")
  
  // Test language switching
  I18nManager::set_language(i18n, "zh")
  let welcome_zh = I18nManager::translate(i18n, "welcome.message")
  assert_eq(welcome_zh, "欢迎使用 Azimuth 遥测系统")
  
  I18nManager::set_language(i18n, "es")
  let welcome_es = I18nManager::translate(i18n, "welcome.message")
  assert_eq(welcome_es, "Bienvenido a Azimuth Telemetry")
  
  // Test parameter substitution
  I18nManager::set_language(i18n, "en")
  let status_en = I18nManager::translate(i18n, "service.status", {"status": "running"})
  assert_eq(status_en, "Service is running")
  
  I18nManager::set_language(i18n, "zh")
  let status_zh = I18nManager::translate(i18n, "service.status", {"status": "运行中"})
  assert_eq(status_zh, "服务状态：运行中")
  
  // Test pluralization
  I18nManager::set_language(i18n, "en")
  let items_one_en = I18nManager::translate(i18n, "items.count", {"count": 1})
  assert_eq(items_one_en, "1 item")
  
  let items_many_en = I18nManager::translate(i18n, "items.count", {"count": 5})
  assert_eq(items_many_en, "5 items")
  
  // Test fallback to default language
  I18nManager::set_language(i18n, "fr")  // Language not loaded
  let fallback_result = I18nManager::translate(i18n, "welcome.message")
  assert_eq(fallback_result, "Welcome to Azimuth Telemetry")  // Should fall back to English
  
  // Test missing key
  I18nManager::set_language(i18n, "en")
  let missing_key = I18nManager::translate(i18n, "nonexistent.key")
  assert_eq(missing_key, "nonexistent.key")  // Should return key itself
}

// Test 2: Number and Date Formatting Tests
test "number and date formatting operations" {
  // Test formatter manager
  let formatter = FormatterManager::new()
  
  // Test number formatting with different locales
  FormatterManager::set_locale(formatter, "en-US")
  let number_en = FormatterManager::format_number(formatter, 1234567.89)
  assert_eq(number_en, "1,234,567.89")
  
  FormatterManager::set_locale(formatter, "de-DE")
  let number_de = FormatterManager::format_number(formatter, 1234567.89)
  assert_eq(number_de, "1.234.567,89")
  
  FormatterManager::set_locale(formatter, "zh-CN")
  let number_zh = FormatterManager::format_number(formatter, 1234567.89)
  assert_eq(number_zh, "1,234,567.89")
  
  // Test currency formatting
  FormatterManager::set_locale(formatter, "en-US")
  let currency_en = FormatterManager::format_currency(formatter, 1234.56, "USD")
  assert_eq(currency_en, "$1,234.56")
  
  FormatterManager::set_locale(formatter, "de-DE")
  let currency_de = FormatterManager::format_currency(formatter, 1234.56, "EUR")
  assert_eq(currency_de, "1.234,56 €")
  
  FormatterManager::set_locale(formatter, "ja-JP")
  let currency_ja = FormatterManager::format_currency(formatter, 1234.56, "JPY")
  assert_eq(currency_ja, "￥1,235")  // Japanese Yen doesn't use decimal places
  
  // Test percentage formatting
  FormatterManager::set_locale(formatter, "en-US")
  let percent_en = FormatterManager::format_percent(formatter, 0.7543)
  assert_eq(percent_en, "75%")
  
  FormatterManager::set_locale(formatter, "fr-FR")
  let percent_fr = FormatterManager::format_percent(formatter, 0.7543)
  assert_eq(percent_fr, "75 %")
  
  // Test date formatting
  FormatterManager::set_locale(formatter, "en-US")
  let date_en = FormatterManager::format_date(formatter, "2023-01-15T14:30:00Z", "full")
  assert_eq(date_en, "Sunday, January 15, 2023")
  
  let time_en = FormatterManager::format_time(formatter, "2023-01-15T14:30:00Z", "short")
  assert_eq(time_en, "2:30 PM")
  
  let datetime_en = FormatterManager::format_datetime(formatter, "2023-01-15T14:30:00Z", "medium")
  assert_eq(datetime_en, "Jan 15, 2023, 2:30:00 PM")
  
  FormatterManager::set_locale(formatter, "zh-CN")
  let date_zh = FormatterManager::format_date(formatter, "2023-01-15T14:30:00Z", "full")
  assert_eq(date_zh, "2023年1月15日星期日")
  
  let time_zh = FormatterManager::format_time(formatter, "2023-01-15T14:30:00Z", "short")
  assert_eq(time_zh, "下午2:30")
  
  let datetime_zh = FormatterManager::format_datetime(formatter, "2023-01-15T14:30:00Z", "medium")
  assert_eq(datetime_zh, "2023年1月15日 下午2:30:00")
  
  FormatterManager::set_locale(formatter, "de-DE")
  let date_de = FormatterManager::format_date(formatter, "2023-01-15T14:30:00Z", "full")
  assert_eq(date_de, "Sonntag, 15. Januar 2023")
  
  let time_de = FormatterManager::format_time(formatter, "2023-01-15T14:30:00Z", "short")
  assert_eq(time_de, "14:30")
  
  let datetime_de = FormatterManager::format_datetime(formatter, "2023-01-15T14:30:00Z", "medium")
  assert_eq(datetime_de, "15.01.2023, 14:30:00")
  
  // Test relative time formatting
  FormatterManager::set_locale(formatter, "en-US")
  let relative_now_en = FormatterManager::format_relative_time(formatter, 0, "second")
  assert_eq(relative_now_en, "now")
  
  let relative_past_en = FormatterManager::format_relative_time(formatter, -60, "second")
  assert_eq(relative_past_en, "1 minute ago")
  
  let relative_future_en = FormatterManager::format_relative_time(formatter, 3600, "second")
  assert_eq(relative_future_en, "in 1 hour")
  
  FormatterManager::set_locale(formatter, "zh-CN")
  let relative_past_zh = FormatterManager::format_relative_time(formatter, -60, "second")
  assert_eq(relative_past_zh, "1分钟前")
  
  let relative_future_zh = FormatterManager::format_relative_time(formatter, 3600, "second")
  assert_eq(relative_future_zh, "1小时后")
}

// Test 3: Text Direction and Script Tests
test "text direction and script operations" {
  // Test text direction manager
  let direction_manager = TextDirectionManager::new()
  
  // Test left-to-right languages
  assert_eq(TextDirectionManager::get_direction(direction_manager, "en"), "ltr")
  assert_eq(TextDirectionManager::get_direction(direction_manager, "es"), "ltr")
  assert_eq(TextDirectionManager::get_direction(direction_manager, "fr"), "ltr")
  assert_eq(TextDirectionManager::get_direction(direction_manager, "de"), "ltr")
  
  // Test right-to-left languages
  assert_eq(TextDirectionManager::get_direction(direction_manager, "ar"), "rtl")
  assert_eq(TextDirectionManager::get_direction(direction_manager, "he"), "rtl")
  assert_eq(TextDirectionManager::get_direction(direction_manager, "fa"), "rtl")
  assert_eq(TextDirectionManager::get_direction(direction_manager, "ur"), "rtl")
  
  // Test top-to-bottom languages
  assert_eq(TextDirectionManager::get_direction(direction_manager, "zh"), "ttb")
  assert_eq(TextDirectionManager::get_direction(direction_manager, "ja"), "ttb")
  assert_eq(TextDirectionManager::get_direction(direction_manager, "ko"), "ttb")
  
  // Test text alignment
  let ltr_text = "Hello World"
  let rtl_text = "مرحبا بالعالم"
  let ttb_text = "こんにちは世界"
  
  assert_eq(TextDirectionManager::get_alignment(direction_manager, ltr_text, "en"), "left")
  assert_eq(TextDirectionManager::get_alignment(direction_manager, rtl_text, "ar"), "right")
  assert_eq(TextDirectionManager::get_alignment(direction_manager, ttb_text, "ja"), "center")
  
  // Test script detection
  let latin_text = "Hello World"
  let arabic_text = "مرحبا بالعالم"
  let chinese_text = "你好世界"
  let mixed_text = "Hello مرحبا 你好"
  
  assert_eq(TextDirectionManager::detect_script(direction_manager, latin_text), "Latin")
  assert_eq(TextDirectionManager::detect_script(direction_manager, arabic_text), "Arabic")
  assert_eq(TextDirectionManager::detect_script(direction_manager, chinese_text), "Han")
  assert_eq(TextDirectionManager::detect_script(direction_manager, mixed_text), "Mixed")
  
  // Test text layout
  let layout_manager = TextLayoutManager::new()
  
  // Test LTR layout
  let ltr_layout = TextLayoutManager::layout_text(layout_manager, "Hello World", "en")
  assert_eq(ltr_layout.direction, "ltr")
  assert_eq(ltr_layout.text, "Hello World")
  
  // Test RTL layout
  let rtl_layout = TextLayoutManager::layout_text(layout_manager, "مرحبا بالعالم", "ar")
  assert_eq(rtl_layout.direction, "rtl")
  assert_eq(rtl_layout.text, "مرحبا بالعالم")
  
  // Test mixed layout
  let mixed_layout = TextLayoutManager::layout_text(layout_manager, "Hello مرحبا World", "en")
  assert_eq(mixed_layout.direction, "ltr")  // Base language is English
  assert_eq(mixed_layout.text, "Hello مرحبا World")
  
  // Test bidirectional text handling
  let bidi_manager = BidiTextManager::new()
  
  let bidi_text = "Hello مرحبا بالعالم World"
  let bidi_processed = BidiTextManager::process(bidi_manager, bidi_text)
  
  // The Arabic part should be in correct order
  assert_true(bidi_processed.contains("Hello"))
  assert_true(bidi_processed.contains("مرحبا بالعالم"))
  assert_true(bidi_processed.contains("World"))
}

// Test 4: Locale-Aware Collation and Sorting Tests
test "locale-aware collation and sorting operations" {
  // Test collation manager
  let collation = CollationManager::new()
  
  // Test string comparison with different locales
  let strings = ["apple", "banana", "cherry", "äpple", "banana", "cherry"]
  
  // English collation
  CollationManager::set_locale(collation, "en")
  let sorted_en = CollationManager::sort_strings(collation, strings)
  assert_eq(sorted_en, ["äpple", "apple", "banana", "banana", "cherry", "cherry"])
  
  // German collation (ä sorts after a)
  CollationManager::set_locale(collation, "de")
  let sorted_de = CollationManager::sort_strings(collation, strings)
  assert_eq(sorted_de, ["apple", "äpple", "banana", "banana", "cherry", "cherry"])
  
  // Swedish collation (ä sorts after z)
  CollationManager::set_locale(collation, "sv")
  let sorted_sv = CollationManager::sort_strings(collation, strings)
  assert_eq(sorted_sv, ["apple", "banana", "banana", "cherry", "cherry", "äpple"])
  
  // Test Chinese collation
  let chinese_strings = ["北京", "上海", "广州", "深圳"]
  CollationManager::set_locale(collation, "zh-CN")
  let sorted_zh = CollationManager::sort_strings(collation, chinese_strings)
  assert_eq(sorted_zh, ["北京", "上海", "广州", "深圳"])
  
  // Test Japanese collation
  let japanese_strings = ["東京", "大阪", "京都", "札幌"]
  CollationManager::set_locale(collation, "ja")
  let sorted_ja = CollationManager::sort_strings(collation, japanese_strings)
  assert_eq(sorted_ja, ["京都", "札幌", "大阪", "東京"])
  
  // Test case sensitivity
  let mixed_case_strings = ["Apple", "banana", "Cherry", "apple"]
  
  // Case insensitive (default)
  CollationManager::set_locale(collation, "en")
  CollationManager::set_case_sensitive(collation, false)
  let sorted_case_insensitive = CollationManager::sort_strings(collation, mixed_case_strings)
  assert_eq(sorted_case_insensitive, ["Apple", "apple", "banana", "Cherry"])
  
  // Case sensitive
  CollationManager::set_case_sensitive(collation, true)
  let sorted_case_sensitive = CollationManager::sort_strings(collation, mixed_case_strings)
  assert_eq(sorted_case_sensitive, ["Apple", "Cherry", "apple", "banana"])
  
  // Test accent sensitivity
  let accented_strings = ["café", "cafe", "càfé", "cafe"]
  
  // Accent insensitive
  CollationManager::set_locale(collation, "fr")
  CollationManager::set_accent_sensitive(collation, false)
  let sorted_accent_insensitive = CollationManager::sort_strings(collation, accented_strings)
  assert_eq(sorted_accent_insensitive, ["café", "cafe", "cafe", "càfé"])
  
  // Accent sensitive
  CollationManager::set_accent_sensitive(collation, true)
  let sorted_accent_sensitive = CollationManager::sort_strings(collation, accented_strings)
  assert_eq(sorted_accent_sensitive, ["cafe", "cafe", "càfé", "café"])
  
  // Test numeric collation
  let numeric_strings = ["file1.txt", "file10.txt", "file2.txt", "file20.txt"]
  
  // Lexicographic sort
  CollationManager::set_numeric_collation(collation, false)
  let sorted_lexicographic = CollationManager::sort_strings(collation, numeric_strings)
  assert_eq(sorted_lexicographic, ["file1.txt", "file10.txt", "file2.txt", "file20.txt"])
  
  // Numeric sort
  CollationManager::set_numeric_collation(collation, true)
  let sorted_numeric = CollationManager::sort_strings(collation, numeric_strings)
  assert_eq(sorted_numeric, ["file1.txt", "file2.txt", "file10.txt", "file20.txt"])
}

// Test 5: Time Zone and Calendar Tests
test "time zone and calendar operations" {
  // Test time zone manager
  let timezone_manager = TimeZoneManager::new()
  
  // Test time zone conversion
  let utc_time = "2023-01-15T14:30:00Z"
  
  let ny_time = TimeZoneManager::convert(timezone_manager, utc_time, "UTC", "America/New_York")
  assert_eq(ny_time, "2023-01-15T09:30:00-05:00")  // UTC-5 in January
  
  let tokyo_time = TimeZoneManager::convert(timezone_manager, utc_time, "UTC", "Asia/Tokyo")
  assert_eq(tokyo_time, "2023-01-15T23:30:00+09:00")  // UTC+9
  
  let london_time = TimeZoneManager::convert(timezone_manager, utc_time, "UTC", "Europe/London")
  assert_eq(london_time, "2023-01-15T14:30:00+00:00")  // UTC+0 in January
  
  // Test daylight saving time
  let summer_time = "2023-07-15T14:30:00Z"
  
  let ny_summer_time = TimeZoneManager::convert(timezone_manager, summer_time, "UTC", "America/New_York")
  assert_eq(ny_summer_time, "2023-07-15T10:30:00-04:00")  // UTC-4 in July (DST)
  
  // Test time zone detection
  TimeZoneManager::set_system_timezone(timezone_manager, "America/New_York")
  let detected_timezone = TimeZoneManager::get_system_timezone(timezone_manager)
  assert_eq(detected_timezone, "America/New_York")
  
  // Test calendar manager
  let calendar_manager = CalendarManager::new()
  
  // Test Gregorian calendar
  CalendarManager::set_calendar(calendar_manager, "gregorian")
  let gregorian_date = CalendarManager::create_date(calendar_manager, 2023, 1, 15)
  assert_eq(gregorian_date.year, 2023)
  assert_eq(gregorian_date.month, 1)
  assert_eq(gregorian_date.day, 15)
  
  // Test Islamic calendar
  CalendarManager::set_calendar(calendar_manager, "islamic")
  let islamic_date = CalendarManager::convert_to_calendar(calendar_manager, gregorian_date, "islamic")
  
  // Islamic calendar should have different year/month/day
  assert_true(islamic_date.year != 2023)
  assert_true(islamic_date.month >= 1 && islamic_date.month <= 12)
  assert_true(islamic_date.day >= 1 && islamic_date.day <= 30)
  
  // Test Hebrew calendar
  CalendarManager::set_calendar(calendar_manager, "hebrew")
  let hebrew_date = CalendarManager::convert_to_calendar(calendar_manager, gregorian_date, "hebrew")
  
  // Hebrew calendar should have different year/month/day
  assert_true(hebrew_date.year != 2023)
  assert_true(hebrew_date.month >= 1 && hebrew_date.month <= 13)
  assert_true(hebrew_date.day >= 1 && hebrew_date.day <= 30)
  
  // Test Chinese calendar
  CalendarManager::set_calendar(calendar_manager, "chinese")
  let chinese_date = CalendarManager::convert_to_calendar(calendar_manager, gregorian_date, "chinese")
  
  // Chinese calendar should have different year/month/day
  assert_true(chinese_date.year != 2023)
  assert_true(chinese_date.month >= 1 && chinese_date.month <= 12)
  assert_true(chinese_date.day >= 1 && chinese_date.day <= 30)
  
  // Test week calculation
  CalendarManager::set_calendar(calendar_manager, "gregorian")
  CalendarManager::set_locale(calendar_manager, "en-US")
  let week_info = CalendarManager::get_week_info(calendar_manager, gregorian_date)
  
  assert_eq(week_info.week_year, 2023)
  assert_true(week_info.week_number >= 1 && week_info.week_number <= 53)
  assert_true(week_info.day_of_week >= 0 && week_info.day_of_week <= 6)
  
  // Test different week start days
  CalendarManager::set_locale(calendar_manager, "en-US")  // Week starts on Sunday
  let us_week_info = CalendarManager::get_week_info(calendar_manager, gregorian_date)
  
  CalendarManager::set_locale(calendar_manager, "de-DE")  // Week starts on Monday
  let de_week_info = CalendarManager::get_week_info(calendar_manager, gregorian_date)
  
  // Week numbers might differ based on locale
  assert_true(us_week_info.day_of_week != de_week_info.day_of_week)
}

// Test 6: Resource Bundle Management Tests
test "resource bundle management operations" {
  // Test resource bundle manager
  let bundle_manager = ResourceBundleManager::new()
  
  // Create resource bundles for different languages
  ResourceBundleManager::create_bundle(bundle_manager, "base", {
    "app.name": "Azimuth Telemetry",
    "app.version": "1.0.0",
    "button.ok": "OK",
    "button.cancel": "Cancel"
  })
  
  ResourceBundleManager::create_bundle(bundle_manager, "en", {
    "welcome.title": "Welcome",
    "welcome.message": "Welcome to Azimuth Telemetry",
    "error.network": "Network error occurred"
  })
  
  ResourceBundleManager::create_bundle(bundle_manager, "zh", {
    "welcome.title": "欢迎",
    "welcome.message": "欢迎使用 Azimuth 遥测系统",
    "error.network": "发生网络错误"
  })
  
  ResourceBundleManager::create_bundle(bundle_manager, "es", {
    "welcome.title": "Bienvenido",
    "welcome.message": "Bienvenido a Azimuth Telemetry",
    "error.network": "Ocurrió un error de red"
  })
  
  // Test bundle inheritance
  ResourceBundleManager::set_inheritance(bundle_manager, "en", "base")
  ResourceBundleManager::set_inheritance(bundle_manager, "zh", "base")
  ResourceBundleManager::set_inheritance(bundle_manager, "es", "base")
  
  // Test resource resolution with inheritance
  let en_bundle = ResourceBundleManager::get_bundle(bundle_manager, "en")
  
  // Should get from en bundle
  assert_eq(ResourceBundleManager::get_string(en_bundle, "welcome.title"), "Welcome")
  assert_eq(ResourceBundleManager::get_string(en_bundle, "welcome.message"), "Welcome to Azimuth Telemetry")
  
  // Should get from base bundle (inherited)
  assert_eq(ResourceBundleManager::get_string(en_bundle, "app.name"), "Azimuth Telemetry")
  assert_eq(ResourceBundleManager::get_string(en_bundle, "button.ok"), "OK")
  
  // Test Chinese bundle
  let zh_bundle = ResourceBundleManager::get_bundle(bundle_manager, "zh")
  
  // Should get from zh bundle
  assert_eq(ResourceBundleManager::get_string(zh_bundle, "welcome.title"), "欢迎")
  assert_eq(ResourceBundleManager::get_string(zh_bundle, "welcome.message"), "欢迎使用 Azimuth 遥测系统")
  
  // Should get from base bundle (inherited)
  assert_eq(ResourceBundleManager::get_string(zh_bundle, "app.name"), "Azimuth Telemetry")
  assert_eq(ResourceBundleManager::get_string(zh_bundle, "button.cancel"), "Cancel")
  
  // Test bundle fallback chain
  ResourceBundleManager::set_fallback_chain(bundle_manager, ["zh", "en", "base"])
  
  let fallback_bundle = ResourceBundleManager::get_bundle_with_fallback(bundle_manager, "zh")
  
  // Should get from zh bundle
  assert_eq(ResourceBundleManager::get_string(fallback_bundle, "welcome.title"), "欢迎")
  
  // Should get from en bundle (fallback)
  assert_eq(ResourceBundleManager::get_string(fallback_bundle, "error.network"), "发生网络错误")
  
  // Should get from base bundle (fallback)
  assert_eq(ResourceBundleManager::get_string(fallback_bundle, "button.ok"), "OK")
  
  // Test bundle reloading
  ResourceBundleManager::update_bundle(bundle_manager, "en", {
    "welcome.title": "Welcome Updated",
    "new.key": "New value"
  })
  
  ResourceBundleManager::reload_bundle(bundle_manager, "en")
  
  let updated_bundle = ResourceBundleManager::get_bundle(bundle_manager, "en")
  
  // Should get updated value
  assert_eq(ResourceBundleManager::get_string(updated_bundle, "welcome.title"), "Welcome Updated")
  
  // Should get new value
  assert_eq(ResourceBundleManager::get_string(updated_bundle, "new.key"), "New value")
  
  // Should still get inherited values
  assert_eq(ResourceBundleManager::get_string(updated_bundle, "app.name"), "Azimuth Telemetry")
  
  // Test bundle validation
  let validation_result = ResourceBundleManager::validate_bundle(bundle_manager, "en", {
    "welcome.title": {"required": true, "type": "string"},
    "welcome.message": {"required": true, "type": "string"},
    "app.version": {"required": true, "type": "string"},
    "button.ok": {"required": true, "type": "string"},
    "button.cancel": {"required": true, "type": "string"}
  })
  
  assert_true(validation_result.is_valid)
  
  // Test missing required keys
  let incomplete_bundle = ResourceBundleManager::create_bundle(bundle_manager, "incomplete", {
    "welcome.title": "Incomplete"
    // Missing welcome.message
  })
  
  let incomplete_validation = ResourceBundleManager::validate_bundle(bundle_manager, "incomplete", {
    "welcome.title": {"required": true, "type": "string"},
    "welcome.message": {"required": true, "type": "string"}
  })
  
  assert_false(incomplete_validation.is_valid)
  assert_true(incomplete_validation.missing_keys.contains("welcome.message"))
}

// Test 7: Pluralization and Gender Rules Tests
test "pluralization and gender rules operations" {
  // Test pluralization manager
  let plural_manager = PluralizationManager::new()
  
  // Define pluralization rules for different languages
  PluralizationManager::set_rules(plural_manager, "en", {
    "one": "n == 1",
    "other": "true"
  })
  
  PluralizationManager::set_rules(plural_manager, "zh", {
    "other": "true"  // Chinese doesn't have plural forms
  })
  
  PluralizationManager::set_rules(plural_manager, "ar", {
    "zero": "n == 0",
    "one": "n == 1",
    "two": "n == 2",
    "few": "n % 100 >= 3 && n % 100 <= 10",
    "many": "n % 100 >= 11 && n % 100 <= 99",
    "other": "true"
  })
  
  // Test English pluralization
  let en_one = PluralizationManager::get_plural_form(plural_manager, "en", 1)
  assert_eq(en_one, "one")
  
  let en_other = PluralizationManager::get_plural_form(plural_manager, "en", 5)
  assert_eq(en_other, "other")
  
  // Test Chinese pluralization
  let zh_zero = PluralizationManager::get_plural_form(plural_manager, "zh", 0)
  assert_eq(zh_zero, "other")
  
  let zh_one = PluralizationManager::get_plural_form(plural_manager, "zh", 1)
  assert_eq(zh_one, "other")
  
  let zh_many = PluralizationManager::get_plural_form(plural_manager, "zh", 5)
  assert_eq(zh_many, "other")
  
  // Test Arabic pluralization
  let ar_zero = PluralizationManager::get_plural_form(plural_manager, "ar", 0)
  assert_eq(ar_zero, "zero")
  
  let ar_one = PluralizationManager::get_plural_form(plural_manager, "ar", 1)
  assert_eq(ar_one, "one")
  
  let ar_two = PluralizationManager::get_plural_form(plural_manager, "ar", 2)
  assert_eq(ar_two, "two")
  
  let ar_few = PluralizationManager::get_plural_form(plural_manager, "ar", 5)
  assert_eq(ar_few, "few")
  
  let ar_many = PluralizationManager::get_plural_form(plural_manager, "ar", 25)
  assert_eq(ar_many, "many")
  
  let ar_other = PluralizationManager::get_plural_form(plural_manager, "ar", 150)
  assert_eq(ar_other, "other")
  
  // Test gender rules
  let gender_manager = GenderManager::new()
  
  // Define gender rules for different languages
  GenderManager::set_rules(gender_manager, "en", {
    "masculine": ["he", "his", "him"],
    "feminine": ["she", "her", "hers"],
    "neuter": ["it", "its"]
  })
  
  GenderManager::set_rules(gender_manager, "es", {
    "masculine": ["el", "un", "él"],
    "feminine": ["la", "una", "ella"],
    "neuter": ["lo"]
  })
  
  GenderManager::set_rules(gender_manager, "de", {
    "masculine": ["der", "ein", "er"],
    "feminine": ["die", "eine", "sie"],
    "neuter": ["das", "ein", "es"]
  })
  
  // Test gender detection
  let en_masculine = GenderManager::detect_gender(gender_manager, "en", "he")
  assert_eq(en_masculine, "masculine")
  
  let en_feminine = GenderManager::detect_gender(gender_manager, "en", "she")
  assert_eq(en_feminine, "feminine")
  
  let en_neuter = GenderManager::detect_gender(gender_manager, "en", "it")
  assert_eq(en_neuter, "neuter")
  
  let es_masculine = GenderManager::detect_gender(gender_manager, "es", "el")
  assert_eq(es_masculine, "masculine")
  
  let es_feminine = GenderManager::detect_gender(gender_manager, "es", "la")
  assert_eq(es_feminine, "feminine")
  
  // Test gender-aware translations
  let gender_i18n = GenderAwareI18n::new()
  
  // Load gender-aware translations
  GenderAwareI18n::load_language(gender_i18n, "en", {
    "user.greeting": {
      "masculine": "He is online",
      "feminine": "She is online",
      "neuter": "It is online"
    },
    "user.action": {
      "masculine": "He updated his profile",
      "feminine": "She updated her profile",
      "neuter": "It updated its profile"
    }
  })
  
  // Test gender-aware translation
  let masculine_greeting = GenderAwareI18n::translate(gender_i18n, "en", "user.greeting", "masculine")
  assert_eq(masculine_greeting, "He is online")
  
  let feminine_greeting = GenderAwareI18n::translate(gender_i18n, "en", "user.greeting", "feminine")
  assert_eq(feminine_greeting, "She is online")
  
  let neuter_greeting = GenderAwareI18n::translate(gender_i18n, "en", "user.greeting", "neuter")
  assert_eq(neuter_greeting, "It is online")
}

// Test 8: Character Encoding and Conversion Tests
test "character encoding and conversion operations" {
  // Test encoding manager
  let encoding_manager = EncodingManager::new()
  
  // Test UTF-8 encoding
  let utf8_text = "Hello 世界 ¡Hola!"
  let utf8_bytes = EncodingManager::encode(encoding_manager, utf8_text, "utf-8")
  let decoded_utf8 = EncodingManager::decode(encoding_manager, utf8_bytes, "utf-8")
  
  assert_eq(decoded_utf8, utf8_text)
  
  // Test UTF-16 encoding
  let utf16_bytes = EncodingManager::encode(encoding_manager, utf8_text, "utf-16")
  let decoded_utf16 = EncodingManager::decode(encoding_manager, utf16_bytes, "utf-16")
  
  assert_eq(decoded_utf16, utf8_text)
  
  // Test ISO-8859-1 encoding
  let latin1_text = "Hello ¡Hola!"  // Contains characters within ISO-8859-1
  let latin1_bytes = EncodingManager::encode(encoding_manager, latin1_text, "iso-8859-1")
  let decoded_latin1 = EncodingManager::decode(encoding_manager, latin1_bytes, "iso-8859-1")
  
  assert_eq(decoded_latin1, latin1_text)
  
  // Test encoding detection
  let text_with_bom = "\uFEFFHello World"  // UTF-8 BOM
  let detected_encoding = EncodingManager::detect_encoding(encoding_manager, text_with_bom)
  assert_eq(detected_encoding, "utf-8")
  
  // Test character set conversion
  let converter = CharacterConverter::new()
  
  // Test NFC normalization
  let nfc_input = "e\u0301"  // e + combining acute accent
  let nfc_output = CharacterConverter::normalize(converter, nfc_input, "nfc")
  assert_eq(nfc_output, "é")  // Should be normalized to single character
  
  // Test NFD normalization
  let nfd_input = "é"  // Single character
  let nfd_output = CharacterConverter::normalize(converter, nfd_input, "nfd")
  assert_eq(nfd_output, "e\u0301")  // Should be decomposed
  
  // Test case conversion with locale
  let locale_case_converter = LocaleCaseConverter::new()
  
  // Test Turkish case conversion (special rules for i/I)
  let turkish_text = "Istanbul"
  let turkish_lower = LocaleCaseConverter::to_lower(locale_case_converter, turkish_text, "tr")
  assert_eq(turkish_lower, "ıstanbul")  // Should use dotless i
  
  let turkish_upper = LocaleCaseConverter::to_upper(locale_case_converter, "ıstanbul", "tr")
  assert_eq(turkish_upper, "ISTANBUL")  // Should use dotted I
  
  // Test German case conversion (ß to SS)
  let german_text = "straße"
  let german_upper = LocaleCaseConverter::to_upper(locale_case_converter, german_text, "de")
  assert_eq(german_upper, "STRASSE")  // ß should become SS
  
  // Test Greek case conversion
  let greek_text = "αθήνα"
  let greek_upper = LocaleCaseConverter::to_upper(locale_case_converter, greek_text, "el")
  assert_eq(greek_upper, "ΑΘΉΝΑ")
  
  // Test character width conversion
  let width_converter = CharacterWidthConverter::new()
  
  // Test half-width to full-width conversion
  let half_width_text = "ABC123"
  let full_width_text = CharacterWidthConverter::to_full_width(width_converter, half_width_text)
  assert_eq(full_width_text, "ＡＢＣ１２３")
  
  // Test full-width to half-width conversion
  let back_to_half = CharacterWidthConverter::to_half_width(width_converter, full_width_text)
  assert_eq(back_to_half, half_width_text)
  
  // Test bidirectional text conversion
  let bidi_converter = BidiConverter::new()
  
  let mixed_text = "Hello مرحبا World"
  let logical_to_visual = BidiConverter::logical_to_visual(bidi_converter, mixed_text)
  let visual_to_logical = BidiConverter::visual_to_logical(bidi_converter, logical_to_visual)
  
  assert_eq(visual_to_logical, mixed_text)
}

// Test 9: Keyboard and Input Method Tests
test "keyboard and input method operations" {
  // Test keyboard layout manager
  let keyboard_manager = KeyboardLayoutManager::new()
  
  // Test keyboard layout detection
  KeyboardManager::set_system_layout(keyboard_manager, "US")
  let detected_layout = KeyboardManager::get_system_layout(keyboard_manager)
  assert_eq(detected_layout, "US")
  
  // Test keyboard layout mapping
  let us_mapping = KeyboardManager::get_mapping(keyboard_manager, "US")
  assert_eq(us_mapping.name, "US")
  assert_eq(us_mapping.layout, "QWERTY")
  
  let german_mapping = KeyboardManager::get_mapping(keyboard_manager, "German")
  assert_eq(german_mapping.name, "German")
  assert_eq(german_mapping.layout, "QWERTZ")
  
  // Test key translation
  let us_key = KeyboardManager::translate_key(keyboard_manager, "a", "US", "German")
  assert_eq(us_key, "a")  // 'a' is the same in both layouts
  
  let german_key = KeyboardManager::translate_key(keyboard_manager, "y", "German", "US")
  assert_eq(german_key, "z")  // 'y' and 'z' are swapped in German layout
  
  // Test input method manager
  let ime_manager = InputMethodManager::new()
  
  // Register input methods
  InputMethodManager::register_ime(ime_manager, {
    "id": "pinyin",
    "name": "Pinyin",
    "language": "zh-CN",
    "description": "Chinese Pinyin input method"
  })
  
  InputMethodManager::register_ime(ime_manager, {
    "id": "hiragana",
    "name": "Hiragana",
    "language": "ja",
    "description": "Japanese Hiragana input method"
  })
  
  InputMethodManager::register_ime(ime_manager, {
    "id": "hangul",
    "name": "Hangul",
    "language": "ko",
    "description": "Korean Hangul input method"
  })
  
  // Test input method activation
  InputMethodManager::activate_ime(ime_manager, "pinyin")
  let active_ime = InputMethodManager::get_active_ime(ime_manager)
  assert_eq(active_ime.id, "pinyin")
  assert_eq(active_ime.language, "zh-CN")
  
  // Test input method conversion
  let pinyin_input = "nihao"
  let pinyin_output = InputMethodManager::convert(ime_manager, pinyin_input)
  assert_eq(pinyin_output, "你好")  // Should convert Pinyin to Chinese characters
  
  let hiragana_input = "konnichiha"
  InputMethodManager::activate_ime(ime_manager, "hiragana")
  let hiragana_output = InputMethodManager::convert(ime_manager, hiragana_input)
  assert_eq(hiragana_output, "こんにちは")  // Should convert Romaji to Hiragana
  
  let hangul_input = "annyeonghaseyo"
  InputMethodManager::activate_ime(ime_manager, "hangul")
  let hangul_output = InputMethodManager::convert(ime_manager, hangul_input)
  assert_eq(hangul_output, "안녕하세요")  // Should convert Romaji to Hangul
  
  // Test input method candidates
  InputMethodManager::activate_ime(ime_manager, "pinyin")
  let candidates = InputMethodManager::get_candidates(ime_manager, "zhong")
  
  assert_true(candidates.length() > 0)
  assert_true(candidates.any((candidate) => { candidate == "中" }))
  assert_true(candidates.any((candidate) => { candidate == "重" }))
  assert_true(candidates.any((candidate) => { candidate == "众" }))
  
  // Test input method prediction
  let predictions = InputMethodManager::get_predictions(ime_manager, "ni hao")
  
  assert_true(predictions.length() > 0)
  assert_true(predictions.any((prediction) => { prediction == "你好" }))
  assert_true(predictions.any((prediction) => { prediction == "您好" }))
  
  // Test custom input method
  let custom_ime = {
    "id": "custom",
    "name": "Custom IME",
    "language": "en",
    "description": "Custom input method for testing",
    "converter": (input) => {
      // Simple converter that adds prefix and suffix
      return "[" + input + "]"
    }
  }
  
  InputMethodManager::register_custom_ime(ime_manager, custom_ime)
  InputMethodManager::activate_ime(ime_manager, "custom")
  
  let custom_output = InputMethodManager::convert(ime_manager, "test")
  assert_eq(custom_output, "[test]")
}

// Test 10: Accessibility and Localization Tests
test "accessibility and localization operations" {
  // Test accessibility manager
  let accessibility_manager = AccessibilityManager::new()
  
  // Test screen reader support
  AccessibilityManager::enable_screen_reader(accessibility_manager, true)
  assert_true(AccessibilityManager::is_screen_reader_enabled(accessibility_manager))
  
  // Test high contrast mode
  AccessibilityManager::set_high_contrast(accessibility_manager, true)
  assert_true(AccessibilityManager::is_high_contrast(accessibility_manager))
  
  // Test font size adjustment
  AccessibilityManager::set_font_size(accessibility_manager, "large")
  assert_eq(AccessibilityManager::get_font_size(accessibility_manager), "large")
  
  // Test localized accessibility messages
  let a11n_i18n = AccessibilityI18n::new()
  
  // Load accessibility messages
  AccessibilityI18n::load_language(a11n_i18n, "en", {
    "screen_reader.welcome": "Welcome to Azimuth Telemetry",
    "screen_reader.navigation": "Use arrow keys to navigate",
    "high_contrast.enabled": "High contrast mode enabled",
    "font_size.increased": "Font size increased to large"
  })
  
  AccessibilityI18n::load_language(a11n_i18n, "zh", {
    "screen_reader.welcome": "欢迎使用 Azimuth 遥测系统",
    "screen_reader.navigation": "使用方向键导航",
    "high_contrast.enabled": "高对比度模式已启用",
    "font_size.increased": "字体大小已增大到大号"
  })
  
  // Test localized accessibility messages
  AccessibilityI18n::set_language(a11n_i18n, "en")
  let welcome_en = AccessibilityI18n::get_message(a11n_i18n, "screen_reader.welcome")
  assert_eq(welcome_en, "Welcome to Azimuth Telemetry")
  
  AccessibilityI18n::set_language(a11n_i18n, "zh")
  let welcome_zh = AccessibilityI18n::get_message(a11n_i18n, "screen_reader.welcome")
  assert_eq(welcome_zh, "欢迎使用 Azimuth 遥测系统")
  
  // Test RTL accessibility
  let rtl_accessibility = RTLAccessibilityManager::new()
  
  // Test RTL layout adjustments
  RTLAccessibilityManager::enable_rtl(rtl_accessibility, true)
  assert_true(RTLAccessibilityManager::is_rtl_enabled(rtl_accessibility))
  
  // Test text alignment for accessibility
  let en_text = "Welcome"
  let ar_text = "مرحبا"
  
  let en_alignment = RTLAccessibilityManager::get_accessible_alignment(rtl_accessibility, en_text, "en")
  assert_eq(en_alignment, "left")
  
  let ar_alignment = RTLAccessibilityManager::get_accessible_alignment(rtl_accessibility, ar_text, "ar")
  assert_eq(ar_alignment, "right")
  
  // Test color contrast for different languages
  let contrast_manager = ColorContrastManager::new()
  
  // Test contrast ratios
  let black_on_white = ColorContrastManager::calculate_contrast(contrast_manager, "#000000", "#FFFFFF")
  assert_true(black_on_white >= 21.0)  // Maximum contrast
  
  let red_on_green = ColorContrastManager::calculate_contrast(contrast_manager, "#FF0000", "#00FF00")
  assert_true(red_on_green < 3.0)  // Poor contrast
  
  // Test color combinations for different cultural contexts
  let cultural_colors = CulturalColorManager::new()
  
  // Test color meanings in different cultures
  let red_meaning_us = CulturalColorManager::get_meaning(cultural_colors, "red", "US")
  assert_true(red_meaning_us.contains("danger"))
  assert_true(red_meaning_us.contains("stop"))
  
  let red_meaning_cn = CulturalColorManager::get_meaning(cultural_colors, "red", "CN")
  assert_true(red_meaning_cn.contains("luck"))
  assert_true(red_meaning_cn.contains("prosperity"))
  
  let red_meaning_in = CulturalColorManager::get_meaning(cultural_colors, "red", "IN")
  assert_true(red_meaning_in.contains("purity"))
  assert_true(red_meaning_in.contains("marriage"))
  
  // Test accessible color palettes for different cultures
  let us_palette = CulturalColorManager::get_accessible_palette(cultural_colors, "US")
  assert_true(us_palette.primary.length() > 0)
  assert_true(us_palette.secondary.length() > 0)
  assert_true(us_palette.accent.length() > 0)
  
  let cn_palette = CulturalColorManager::get_accessible_palette(cultural_colors, "CN")
  assert_true(cn_palette.primary.length() > 0)
  assert_true(cn_palette.secondary.length() > 0)
  assert_true(cn_palette.accent.length() > 0)
  
  // Test localized icon and image handling
  let icon_localization = IconLocalizationManager::new()
  
  // Register icons for different cultures
  IconLocalizationManager::register_icon(icon_localization, "warning", {
    "default": "⚠️",
    "US": "⚠️",
    "CN": "⚠️",
    "JP": "⚠️"
  })
  
  IconLocalizationManager::register_icon(icon_localization, "check", {
    "default": "✅",
    "US": "✅",
    "CN": "✅",
    "JP": "✅"
  })
  
  IconLocalizationManager::register_icon(icon_localization, "error", {
    "default": "❌",
    "US": "❌",
    "CN": "❌",
    "JP": "❌"
  })
  
  // Test icon retrieval
  let warning_icon = IconLocalizationManager::get_icon(icon_localization, "warning", "US")
  assert_eq(warning_icon, "⚠️")
  
  let check_icon = IconLocalizationManager::get_icon(icon_localization, "check", "CN")
  assert_eq(check_icon, "✅")
  
  let error_icon = IconLocalizationManager::get_icon(icon_localization, "error", "JP")
  assert_eq(error_icon, "❌")
  
  // Test icon fallback
  let unknown_icon = IconLocalizationManager::get_icon(icon_localization, "unknown", "FR")
  assert_eq(unknown_icon, "")  // Should return empty string for unknown icon
  
  // Test localized text alternatives for images
  let image_localization = ImageLocalizationManager::new()
  
  // Register image alternatives
  ImageLocalizationManager::register_image(image_localization, "logo", {
    "default": {
      "src": "/images/logo.png",
      "alt": "Azimuth Logo"
    },
    "en": {
      "src": "/images/logo-en.png",
      "alt": "Azimuth Logo"
    },
    "zh": {
      "src": "/images/logo-zh.png",
      "alt": "Azimuth 遥测系统标志"
    },
    "ar": {
      "src": "/images/logo-ar.png",
      "alt": "شعار Azimuth للقياس عن بعد"
    }
  })
  
  // Test image retrieval
  let en_image = ImageLocalizationManager::get_image(image_localization, "logo", "en")
  assert_eq(en_image.src, "/images/logo-en.png")
  assert_eq(en_image.alt, "Azimuth Logo")
  
  let zh_image = ImageLocalizationManager::get_image(image_localization, "logo", "zh")
  assert_eq(zh_image.src, "/images/logo-zh.png")
  assert_eq(zh_image.alt, "Azimuth 遥测系统标志")
  
  let ar_image = ImageLocalizationManager::get_image(image_localization, "logo", "ar")
  assert_eq(ar_image.src, "/images/logo-ar.png")
  assert_eq(ar_image.alt, "شعار Azimuth للقياس عن بعد")
  
  // Test image fallback
  let unknown_locale_image = ImageLocalizationManager::get_image(image_localization, "logo", "fr")
  assert_eq(unknown_locale_image.src, "/images/logo.png")  // Should fall back to default
  assert_eq(unknown_locale_image.alt, "Azimuth Logo")
}