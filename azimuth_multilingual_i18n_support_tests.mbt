// Azimuth Multi-language Internationalization Support Tests
// This file contains test cases for internationalization and localization functionality

// Test 1: Basic Locale Detection and Configuration
test "basic locale detection and configuration" {
  // Test locale detection from system
  let system_locale = LocaleDetector::detect_system_locale()
  assert_eq(system_locale.language.length(), 2)  // Language code should be 2 characters
  assert_eq(system_locale.language != "", true)
  
  // Test locale parsing
  let locale1 = Locale::parse("en-US")
  assert_eq(locale1.language, "en")
  assert_eq(locale1.region, Some("US"))
  assert_eq(locale1.script, None)
  
  let locale2 = Locale::parse("zh-Hans-CN")
  assert_eq(locale2.language, "zh")
  assert_eq(locale2.script, Some("Hans"))
  assert_eq(locale2.region, Some("CN"))
  
  let locale3 = Locale::parse("fr")
  assert_eq(locale3.language, "fr")
  assert_eq(locale3.region, None)
  assert_eq(locale3.script, None)
  
  // Test locale configuration
  let i18n = I18nManager::new()
  I18nManager::set_locale(i18n, locale1)
  assert_eq(I18nManager::current_locale(i18n).language, "en")
  assert_eq(I18nManager::current_locale(i18n).region, Some("US"))
}

// Test 2: Message Translation
test "message translation" {
  let i18n = I18nManager::new()
  
  // Load translation resources
  let en_translations = [
    ("welcome.message", "Welcome to Azimuth Telemetry"),
    ("error.connection_failed", "Connection failed"),
    ("status.active", "Active"),
    ("status.inactive", "Inactive"),
    ("button.save", "Save"),
    ("button.cancel", "Cancel")
  ]
  
  let zh_translations = [
    ("welcome.message", "欢迎使用 Azimuth 遥测系统"),
    ("error.connection_failed", "连接失败"),
    ("status.active", "活动"),
    ("status.inactive", "非活动"),
    ("button.save", "保存"),
    ("button.cancel", "取消")
  ]
  
  let fr_translations = [
    ("welcome.message", "Bienvenue dans Azimuth Télémétrie"),
    ("error.connection_failed", "Échec de connexion"),
    ("status.active", "Actif"),
    ("status.inactive", "Inactif"),
    ("button.save", "Enregistrer"),
    ("button.cancel", "Annuler")
  ]
  
  // Load translations for different locales
  I18nManager::load_translations(i18n, Locale::parse("en-US"), en_translations)
  I18nManager::load_translations(i18n, Locale::parse("zh-CN"), zh_translations)
  I18nManager::load_translations(i18n, Locale::parse("fr-FR"), fr_translations)
  
  // Test English translations
  I18nManager::set_locale(i18n, Locale::parse("en-US"))
  assert_eq(I18nManager::translate(i18n, "welcome.message"), Some("Welcome to Azimuth Telemetry"))
  assert_eq(I18nManager::translate(i18n, "error.connection_failed"), Some("Connection failed"))
  assert_eq(I18nManager::translate(i18n, "status.active"), Some("Active"))
  
  // Test Chinese translations
  I18nManager::set_locale(i18n, Locale::parse("zh-CN"))
  assert_eq(I18nManager::translate(i18n, "welcome.message"), Some("欢迎使用 Azimuth 遥测系统"))
  assert_eq(I18nManager::translate(i18n, "error.connection_failed"), Some("连接失败"))
  assert_eq(I18nManager::translate(i18n, "status.active"), Some("活动"))
  
  // Test French translations
  I18nManager::set_locale(i18n, Locale::parse("fr-FR"))
  assert_eq(I18nManager::translate(i18n, "welcome.message"), Some("Bienvenue dans Azimuth Télémétrie"))
  assert_eq(I18nManager::translate(i18n, "error.connection_failed"), Some("Échec de connexion"))
  assert_eq(I18nManager::translate(i18n, "status.active"), Some("Actif"))
  
  // Test fallback to default locale when translation is missing
  I18nManager::set_locale(i18n, Locale::parse("es-ES"))  // Spanish not loaded
  assert_eq(I18nManager::translate(i18n, "welcome.message"), Some("Welcome to Azimuth Telemetry"))  // Should fallback to English
  
  // Test missing key
  assert_eq(I18nManager::translate(i18n, "nonexistent.key"), None)
}

// Test 3: Parameterized Translations
test "parameterized translations" {
  let i18n = I18nManager::new()
  
  // Load parameterized translations
  let en_translations = [
    ("user.greeting", "Hello, {name}!"),
    ("items.count", "You have {count, plural, one{# item} other{# items}}"),
    ("memory.usage", "Memory usage: {percentage}%"),
    ("time.elapsed", "Time elapsed: {hours, plural, one{# hour} other{# hours}} {minutes, plural, one{# minute} other{# minutes}}")
  ]
  
  let zh_translations = [
    ("user.greeting", "你好，{name}！"),
    ("items.count", "你有 {count} 个项目"),
    ("memory.usage", "内存使用率：{percentage}%"),
    ("time.elapsed", "已用时间：{hours} 小时 {minutes} 分钟")
  ]
  
  I18nManager::load_translations(i18n, Locale::parse("en-US"), en_translations)
  I18nManager::load_translations(i18n, Locale::parse("zh-CN"), zh_translations)
  
  // Test English parameterized translations
  I18nManager::set_locale(i18n, Locale::parse("en-US"))
  
  let params1 = [("name", "Alice")]
  assert_eq(I18nManager::translate_with_params(i18n, "user.greeting", params1), Some("Hello, Alice!"))
  
  let params2 = [("count", "1")]
  assert_eq(I18nManager::translate_with_params(i18n, "items.count", params2), Some("You have 1 item"))
  
  let params3 = [("count", "5")]
  assert_eq(I18nManager::translate_with_params(i18n, "items.count", params3), Some("You have 5 items"))
  
  let params4 = [("percentage", "75.5")]
  assert_eq(I18nManager::translate_with_params(i18n, "memory.usage", params4), Some("Memory usage: 75.5%"))
  
  let params5 = [("hours", "1"), ("minutes", "30")]
  assert_eq(I18nManager::translate_with_params(i18n, "time.elapsed", params5), Some("Time elapsed: 1 hour 30 minutes"))
  
  // Test Chinese parameterized translations
  I18nManager::set_locale(i18n, Locale::parse("zh-CN"))
  
  assert_eq(I18nManager::translate_with_params(i18n, "user.greeting", params1), Some("你好，Alice！"))
  assert_eq(I18nManager::translate_with_params(i18n, "items.count", params2), Some("你有 1 个项目"))
  assert_eq(I18nManager::translate_with_params(i18n, "memory.usage", params4), Some("内存使用率：75.5%"))
  assert_eq(I18nManager::translate_with_params(i18n, "time.elapsed", params5), Some("已用时间：1 小时 30 分钟"))
}

// Test 4: Date and Time Formatting
test "date and time formatting" {
  let i18n = I18nManager::new()
  
  // Test date formatting for different locales
  let timestamp = 1640995200000L  // Jan 1, 2022 00:00:00 UTC
  
  // English (US) format
  I18nManager::set_locale(i18n, Locale::parse("en-US"))
  let en_date = I18nManager::format_date(i18n, timestamp, DateFormat::Medium)
  assert_eq(en_date.contains("2022"), true)
  assert_eq(en_date.contains("Jan"), true)
  assert_eq(en_date.contains("1"), true)
  
  let en_time = I18nManager::format_time(i18n, timestamp, TimeFormat::Medium)
  assert_eq(en_time.contains("12"), true)  // 12-hour format
  assert_eq(en_time.contains("AM") || en_time.contains("PM"), true)
  
  // Chinese format
  I18nManager::set_locale(i18n, Locale::parse("zh-CN"))
  let zh_date = I18nManager::format_date(i18n, timestamp, DateFormat::Medium)
  assert_eq(zh_date.contains("2022"), true)
  assert_eq(zh_date.contains("年"), true)
  assert_eq(zh_date.contains("1"), true)
  assert_eq(zh_date.contains("月"), true)
  assert_eq(zh_date.contains("日"), true)
  
  let zh_time = I18nManager::format_time(i18n, timestamp, TimeFormat::Medium)
  assert_eq(zh_time.contains("00"), true)  // 24-hour format
  
  // French format
  I18nManager::set_locale(i18n, Locale::parse("fr-FR"))
  let fr_date = I18nManager::format_date(i18n, timestamp, DateFormat::Medium)
  assert_eq(fr_date.contains("2022"), true)
  assert_eq(fr_date.contains("janv"), true)  // "janvier" abbreviated
  
  // Test relative time formatting
  let now = Clock::now()
  let one_hour_ago = now - 3600000L
  let one_day_ago = now - 86400000L
  let one_week_ago = now - 604800000L
  
  I18nManager::set_locale(i18n, Locale::parse("en-US"))
  assert_eq(I18nManager::format_relative_time(i18n, one_hour_ago, now), Some("1 hour ago"))
  assert_eq(I18nManager::format_relative_time(i18n, one_day_ago, now), Some("1 day ago"))
  assert_eq(I18nManager::format_relative_time(i18n, one_week_ago, now), Some("1 week ago"))
  
  I18nManager::set_locale(i18n, Locale::parse("zh-CN"))
  assert_eq(I18nManager::format_relative_time(i18n, one_hour_ago, now), Some("1小时前"))
  assert_eq(I18nManager::format_relative_time(i18n, one_day_ago, now), Some("1天前"))
  assert_eq(I18nManager::format_relative_time(i18n, one_week_ago, now), Some("1周前"))
}

// Test 5: Number and Currency Formatting
test "number and currency formatting" {
  let i18n = I18nManager::new()
  
  // Test number formatting
  let test_number = 1234567.89
  
  // English (US) format
  I18nManager::set_locale(i18n, Locale::parse("en-US"))
  let en_number = I18nManager::format_number(i18n, test_number, NumberFormat::Decimal)
  assert_eq(en_number, "1,234,567.89")
  
  let en_currency = I18nManager::format_currency(i18n, test_number, "USD")
  assert_eq(en_currency.contains("$"), true)
  assert_eq(en_currency.contains("1,234,567.89"), true)
  
  let en_percent = I18nManager::format_percent(i18n, 0.7542)
  assert_eq(en_percent, "75%")
  
  // Chinese format
  I18nManager::set_locale(i18n, Locale::parse("zh-CN"))
  let zh_number = I18nManager::format_number(i18n, test_number, NumberFormat::Decimal)
  assert_eq(zh_number, "1,234,567.89")
  
  let zh_currency = I18nManager::format_currency(i18n, test_number, "CNY")
  assert_eq(zh_currency.contains("¥"), true)
  assert_eq(zh_currency.contains("1,234,567.89"), true)
  
  let zh_percent = I18nManager::format_percent(i18n, 0.7542)
  assert_eq(zh_percent, "75%")
  
  // French format
  I18nManager::set_locale(i18n, Locale::parse("fr-FR"))
  let fr_number = I18nManager::format_number(i18n, test_number, NumberFormat::Decimal)
  assert_eq(fr_number, "1 234 567,89")  // Space as thousands separator, comma as decimal
  
  let fr_currency = I18nManager::format_currency(i18n, test_number, "EUR")
  assert_eq(fr_currency.contains("€"), true)
  
  let fr_percent = I18nManager::format_percent(i18n, 0.7542)
  assert_eq(fr_percent, "75 %")
  
  // Test compact number formatting
  let large_number = 1500000
  
  I18nManager::set_locale(i18n, Locale::parse("en-US"))
  let en_compact = I18nManager::format_compact_number(i18n, large_number, CompactStyle::Short)
  assert_eq(en_compact, "1.5M")
  
  I18nManager::set_locale(i18n, Locale::parse("zh-CN"))
  let zh_compact = I18nManager::format_compact_number(i18n, large_number, CompactStyle::Short)
  assert_eq(zh_compact, "150万")
}

// Test 6: Text Direction and RTL Support
test "text direction and rtl support" {
  let i18n = I18nManager::new()
  
  // Test LTR locales
  I18nManager::set_locale(i18n, Locale::parse("en-US"))
  assert_eq(I18nManager::text_direction(i18n), TextDirection::LTR)
  assert_eq(I18nManager::is_rtl(i18n), false)
  
  I18nManager::set_locale(i18n, Locale::parse("zh-CN"))
  assert_eq(I18nManager::text_direction(i18n), TextDirection::LTR)
  assert_eq(I18nManager::is_rtl(i18n), false)
  
  // Test RTL locales
  I18nManager::set_locale(i18n, Locale::parse("ar-SA"))
  assert_eq(I18nManager::text_direction(i18n), TextDirection::RTL)
  assert_eq(I18nManager::is_rtl(i18n), true)
  
  I18nManager::set_locale(i18n, Locale::parse("he-IL"))
  assert_eq(I18nManager::text_direction(i18n), TextDirection::RTL)
  assert_eq(I18nManager::is_rtl(i18n), true)
  
  // Test text alignment based on direction
  I18nManager::set_locale(i18n, Locale::parse("en-US"))
  assert_eq(I18nManager::default_text_align(i18n), TextAlign::Left)
  
  I18nManager::set_locale(i18n, Locale::parse("ar-SA"))
  assert_eq(I18nManager::default_text_align(i18n), TextAlign::Right)
  
  // Test layout mirroring
  I18nManager::set_locale(i18n, Locale::parse("en-US"))
  assert_eq(I18nManager::should_mirror_layout(i18n), false)
  
  I18nManager::set_locale(i18n, Locale::parse("ar-SA"))
  assert_eq(I18nManager::should_mirror_layout(i18n), true)
}

// Test 7: Pluralization Rules
test "pluralization rules" {
  let i18n = I18nManager::new()
  
  // Load translations with plural forms
  let en_translations = [
    ("items.count", "{count, plural, one{# item} other{# items}}"),
    ("messages.count", "{count, plural, =0{No messages} one{# message} other{# messages}}")
  ]
  
  let zh_translations = [
    ("items.count", "{count} 个项目"),  // Chinese doesn't typically use plural forms
    ("messages.count", "{count} 条消息")
  ]
  
  let ar_translations = [
    ("items.count", "{count, plural, zero{# عناصر} one{# عنصر} two{# عنصران} few{# عناصر} many{# عنصر} other{# عنصر}}")
  ]
  
  I18nManager::load_translations(i18n, Locale::parse("en-US"), en_translations)
  I18nManager::load_translations(i18n, Locale::parse("zh-CN"), zh_translations)
  I18nManager::load_translations(i18n, Locale::parse("ar-SA"), ar_translations)
  
  // Test English pluralization (has multiple forms)
  I18nManager::set_locale(i18n, Locale::parse("en-US"))
  
  let en_params1 = [("count", "0")]
  assert_eq(I18nManager::translate_with_params(i18n, "messages.count", en_params1), Some("No messages"))
  
  let en_params2 = [("count", "1")]
  assert_eq(I18nManager::translate_with_params(i18n, "items.count", en_params2), Some("1 item"))
  
  let en_params3 = [("count", "2")]
  assert_eq(I18nManager::translate_with_params(i18n, "items.count", en_params3), Some("2 items"))
  
  let en_params4 = [("count", "10")]
  assert_eq(I18nManager::translate_with_params(i18n, "items.count", en_params4), Some("10 items"))
  
  // Test Chinese pluralization (typically no plural forms)
  I18nManager::set_locale(i18n, Locale::parse("zh-CN"))
  
  assert_eq(I18nManager::translate_with_params(i18n, "items.count", en_params2), Some("1 个项目"))
  assert_eq(I18nManager::translate_with_params(i18n, "items.count", en_params3), Some("2 个项目"))
  assert_eq(I18nManager::translate_with_params(i18n, "items.count", en_params4), Some("10 个项目"))
  
  // Test Arabic pluralization (has complex plural forms)
  I18nManager::set_locale(i18n, Locale::parse("ar-SA"))
  
  let ar_params0 = [("count", "0")]
  let ar_params1 = [("count", "1")]
  let ar_params2 = [("count", "2")]
  let ar_params3 = [("count", "5")]
  
  // Verify Arabic plural forms are different
  let ar_result0 = I18nManager::translate_with_params(i18n, "items.count", ar_params0)
  let ar_result1 = I18nManager::translate_with_params(i18n, "items.count", ar_params1)
  let ar_result2 = I18nManager::translate_with_params(i18n, "items.count", ar_params2)
  let ar_result3 = I18nManager::translate_with_params(i18n, "items.count", ar_params3)
  
  assert_eq(ar_result0 != ar_result1, true)
  assert_eq(ar_result1 != ar_result2, true)
  assert_eq(ar_result2 != ar_result3, true)
}

// Test 8: Locale-specific Formatting
test "locale specific formatting" {
  let i18n = I18nManager::new()
  
  // Test address formatting
  let address_components = [
    ("street", "123 Main St"),
    ("city", "New York"),
    ("state", "NY"),
    ("postal_code", "10001"),
    ("country", "United States")
  ]
  
  // US address format
  I18nManager::set_locale(i18n, Locale::parse("en-US"))
  let us_address = I18nManager::format_address(i18n, address_components)
  assert_eq(us_address.contains("123 Main St"), true)
  assert_eq(us_address.contains("New York, NY 10001"), true)
  
  // Chinese address format
  I18nManager::set_locale(i18n, Locale::parse("zh-CN"))
  let cn_address_components = [
    ("country", "中国"),
    ("state", "北京市"),
    ("city", "北京市"),
    ("district", "朝阳区"),
    ("street", "建国路123号"),
    ("postal_code", "100000")
  ]
  let cn_address = I18nManager::format_address(i18n, cn_address_components)
  assert_eq(cn_address.contains("中国"), true)
  assert_eq(cn_address.contains("北京市"), true)
  assert_eq(cn_address.contains("建国路123号"), true)
  
  // Test name formatting
  let name_components = [
    ("first_name", "John"),
    ("last_name", "Doe"),
    ("middle_name", "William")
  ]
  
  // Western name format
  I18nManager::set_locale(i18n, Locale::parse("en-US"))
  let western_name = I18nManager::format_name(i18n, name_components)
  assert_eq(western_name, "John William Doe")
  
  // Eastern name format
  I18nManager::set_locale(i18n, Locale::parse("zh-CN"))
  let eastern_name_components = [
    ("family_name", "张"),
    ("given_name", "伟")
  ]
  let eastern_name = I18nManager::format_name(i18n, eastern_name_components)
  assert_eq(eastern_name, "张伟")
  
  // Test phone number formatting
  I18nManager::set_locale(i18n, Locale::parse("en-US"))
  let us_phone = I18nManager::format_phone_number(i18n, "+15551234567")
  assert_eq(us_phone, "+1 (555) 123-4567")
  
  I18nManager::set_locale(i18n, Locale::parse("fr-FR"))
  let fr_phone = I18nManager::format_phone_number(i18n, "+33612345678")
  assert_eq(fr_phone.contains("+33"), true)
  assert_eq(fr_phone.contains("6 12 34 56 78"), true)
}

// Test 9: Dynamic Locale Switching
test "dynamic locale switching" {
  let i18n = I18nManager::new()
  
  // Load translations
  let en_translations = [
    ("welcome", "Welcome"),
    ("goodbye", "Goodbye")
  ]
  
  let zh_translations = [
    ("welcome", "欢迎"),
    ("goodbye", "再见")
  ]
  
  I18nManager::load_translations(i18n, Locale::parse("en-US"), en_translations)
  I18nManager::load_translations(i18n, Locale::parse("zh-CN"), zh_translations)
  
  // Test initial locale
  I18nManager::set_locale(i18n, Locale::parse("en-US"))
  assert_eq(I18nManager::translate(i18n, "welcome"), Some("Welcome"))
  
  // Test locale change
  I18nManager::set_locale(i18n, Locale::parse("zh-CN"))
  assert_eq(I18nManager::translate(i18n, "welcome"), Some("欢迎"))
  
  // Test locale change with callback
  let mut callback_called = false
  let locale_change_callback = fn(old_locale, new_locale) {
    callback_called = true
    assert_eq(old_locale.language, "zh")
    assert_eq(new_locale.language, "fr")
  }
  
  I18nManager::on_locale_change(i18n, locale_change_callback)
  
  // Load French translations
  let fr_translations = [
    ("welcome", "Bienvenue"),
    ("goodbye", "Au revoir")
  ]
  I18nManager::load_translations(i18n, Locale::parse("fr-FR"), fr_translations)
  
  // Change locale to trigger callback
  I18nManager::set_locale(i18n, Locale::parse("fr-FR"))
  assert_eq(callback_called, true)
  assert_eq(I18nManager::translate(i18n, "welcome"), Some("Bienvenue"))
  
  // Test locale persistence
  let saved_locale = I18nManager::current_locale(i18n)
  assert_eq(saved_locale.language, "fr")
  assert_eq(saved_locale.region, Some("FR"))
}

// Test 10: Translation Fallback Chain
test "translation fallback chain" {
  let i18n = I18nManager::new()
  
  // Configure fallback chain: zh-CN -> zh -> en
  let fallback_chain = [
    Locale::parse("zh-CN"),
    Locale::parse("zh"),
    Locale::parse("en-US")
  ]
  I18nManager::set_fallback_chain(i18n, fallback_chain)
  
  // Load translations for different levels
  let zh_cn_translations = [
    ("specific.zh.cn", "Specific to zh-CN"),
    ("common.key", "Chinese common")
  ]
  
  let zh_translations = [
    ("common.key", "Chinese generic"),
    ("zh.only", "Only in zh")
  ]
  
  let en_translations = [
    ("common.key", "English common"),
    ("en.only", "Only in en")
  ]
  
  I18nManager::load_translations(i18n, Locale::parse("zh-CN"), zh_cn_translations)
  I18nManager::load_translations(i18n, Locale::parse("zh"), zh_translations)
  I18nManager::load_translations(i18n, Locale::parse("en-US"), en_translations)
  
  // Set locale to zh-HK (not loaded, should use fallback chain)
  I18nManager::set_locale(i18n, Locale::parse("zh-HK"))
  
  // Test fallback behavior
  // zh-HK -> zh-CN -> zh -> en-US
  assert_eq(I18nManager::translate(i18n, "specific.zh.cn"), Some("Specific to zh-CN"))  // Found in zh-CN
  assert_eq(I18nManager::translate(i18n, "zh.only"), Some("Only in zh"))  // Found in zh
  assert_eq(I18nManager::translate(i18n, "en.only"), Some("Only in en"))  // Found in en-US
  assert_eq(I18nManager::translate(i18n, "common.key"), Some("Chinese common"))  // Found in zh-CN (first in chain)
  assert_eq(I18nManager::translate(i18n, "nonexistent"), None)  // Not found anywhere
  
  // Test with different locale
  I18nManager::set_locale(i18n, Locale::parse("fr-FR"))  // Not in chain, should use default fallback
  assert_eq(I18nManager::translate(i18n, "en.only"), Some("Only in en"))  // Should fallback to en-US
  
  // Test fallback metrics
  let metrics = I18nManager::get_fallback_metrics(i18n)
  assert_eq(metrics.total_requests > 0, true)
  assert_eq(metrics.direct_hits + metrics.fallback_hits == metrics.total_requests, true)
}