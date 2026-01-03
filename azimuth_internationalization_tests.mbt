// Azimuth Internationalization Support Tests
// This file contains test cases for internationalization (i18n) and localization (l10n)

// Test 1: Basic Localization
test "basic localization" {
  let locale_manager = LocaleManager::new()
  
  // Set up locale resources
  LocaleManager::add_translation(locale_manager, "en", "welcome.message", "Welcome to Azimuth")
  LocaleManager::add_translation(locale_manager, "zh", "welcome.message", "欢迎使用 Azimuth")
  LocaleManager::add_translation(locale_manager, "es", "welcome.message", "Bienvenido a Azimuth")
  LocaleManager::add_translation(locale_manager, "fr", "welcome.message", "Bienvenue à Azimuth")
  
  // Set locale to English
  LocaleManager::set_locale(locale_manager, "en")
  let english_message = LocaleManager::translate(locale_manager, "welcome.message")
  assert_eq(english_message, "Welcome to Azimuth")
  
  // Set locale to Chinese
  LocaleManager::set_locale(locale_manager, "zh")
  let chinese_message = LocaleManager::translate(locale_manager, "welcome.message")
  assert_eq(chinese_message, "欢迎使用 Azimuth")
  
  // Set locale to Spanish
  LocaleManager::set_locale(locale_manager, "es")
  let spanish_message = LocaleManager::translate(locale_manager, "welcome.message")
  assert_eq(spanish_message, "Bienvenido a Azimuth")
  
  // Set locale to French
  LocaleManager::set_locale(locale_manager, "fr")
  let french_message = LocaleManager::translate(locale_manager, "welcome.message")
  assert_eq(french_message, "Bienvenue à Azimuth")
}

// Test 2: Parameterized Localization
test "parameterized localization" {
  let locale_manager = LocaleManager::new()
  
  // Set up parameterized locale resources
  LocaleManager::add_translation(locale_manager, "en", "user.greeting", "Hello, {name}!")
  LocaleManager::add_translation(locale_manager, "zh", "user.greeting", "你好，{name}！")
  LocaleManager::add_translation(locale_manager, "es", "user.greeting", "¡Hola, {name}!")
  LocaleManager::add_translation(locale_manager, "fr", "user.greeting", "Bonjour, {name}!")
  
  // Test parameter substitution in English
  LocaleManager::set_locale(locale_manager, "en")
  let english_greeting = LocaleManager::translate_with_params(
    locale_manager, 
    "user.greeting", 
    [("name", "Alice")]
  )
  assert_eq(english_greeting, "Hello, Alice!")
  
  // Test parameter substitution in Chinese
  LocaleManager::set_locale(locale_manager, "zh")
  let chinese_greeting = LocaleManager::translate_with_params(
    locale_manager, 
    "user.greeting", 
    [("name", "张三")]
  )
  assert_eq(chinese_greeting, "你好，张三！")
  
  // Test parameter substitution in Spanish
  LocaleManager::set_locale(locale_manager, "es")
  let spanish_greeting = LocaleManager::translate_with_params(
    locale_manager, 
    "user.greeting", 
    [("name", "Carlos")]
  )
  assert_eq(spanish_greeting, "¡Hola, Carlos!")
  
  // Test parameter substitution in French
  LocaleManager::set_locale(locale_manager, "fr")
  let french_greeting = LocaleManager::translate_with_params(
    locale_manager, 
    "user.greeting", 
    [("name", "Pierre")]
  )
  assert_eq(french_greeting, "Bonjour, Pierre!")
}

// Test 3: Pluralization
test "pluralization" {
  let locale_manager = LocaleManager::new()
  
  // Set up pluralization rules
  LocaleManager::add_pluralization(locale_manager, "en", "item.count", {
    1 => "1 item"
    _ => "{count} items"
  })
  
  LocaleManager::add_pluralization(locale_manager, "zh", "item.count", {
    _ => "{count} 项"
  })
  
  LocaleManager::add_pluralization(locale_manager, "es", "item.count", {
    1 => "1 artículo"
    _ => "{count} artículos"
  })
  
  // Test English pluralization
  LocaleManager::set_locale(locale_manager, "en")
  let english_singular = LocaleManager::translate_plural(
    locale_manager, 
    "item.count", 
    [("count", 1)]
  )
  assert_eq(english_singular, "1 item")
  
  let english_plural = LocaleManager::translate_plural(
    locale_manager, 
    "item.count", 
    [("count", 5)]
  )
  assert_eq(english_plural, "5 items")
  
  // Test Chinese pluralization (Chinese doesn't typically have plural forms)
  LocaleManager::set_locale(locale_manager, "zh")
  let chinese_singular = LocaleManager::translate_plural(
    locale_manager, 
    "item.count", 
    [("count", 1)]
  )
  assert_eq(chinese_singular, "1 项")
  
  let chinese_plural = LocaleManager::translate_plural(
    locale_manager, 
    "item.count", 
    [("count", 5)]
  )
  assert_eq(chinese_plural, "5 项")
  
  // Test Spanish pluralization
  LocaleManager::set_locale(locale_manager, "es")
  let spanish_singular = LocaleManager::translate_plural(
    locale_manager, 
    "item.count", 
    [("count", 1)]
  )
  assert_eq(spanish_singular, "1 artículo")
  
  let spanish_plural = LocaleManager::translate_plural(
    locale_manager, 
    "item.count", 
    [("count", 5)]
  )
  assert_eq(spanish_plural, "5 artículos")
}

// Test 4: Date and Time Formatting
test "date and time formatting" {
  let locale_manager = LocaleManager::new()
  
  // Create a test date: 2023-12-25 15:30:00 UTC
  let test_date = DateTime::new(2023, 12, 25, 15, 30, 0, UTC)
  
  // Test English date formatting
  LocaleManager::set_locale(locale_manager, "en")
  let english_date = LocaleManager::format_date(locale_manager, test_date, "medium")
  assert_eq(english_date, "Dec 25, 2023")
  
  let english_time = LocaleManager::format_time(locale_manager, test_date, "short")
  assert_eq(english_time, "3:30 PM")
  
  let english_datetime = LocaleManager::format_datetime(locale_manager, test_date, "long")
  assert_eq(english_datetime, "December 25, 2023 at 3:30:00 PM UTC")
  
  // Test Chinese date formatting
  LocaleManager::set_locale(locale_manager, "zh")
  let chinese_date = LocaleManager::format_date(locale_manager, test_date, "medium")
  assert_eq(chinese_date, "2023年12月25日")
  
  let chinese_time = LocaleManager::format_time(locale_manager, test_date, "short")
  assert_eq(chinese_time, "下午3:30")
  
  let chinese_datetime = LocaleManager::format_datetime(locale_manager, test_date, "long")
  assert_eq(chinese_datetime, "2023年12月25日 UTC时间 下午3时30分00秒")
  
  // Test Spanish date formatting
  LocaleManager::set_locale(locale_manager, "es")
  let spanish_date = LocaleManager::format_date(locale_manager, test_date, "medium")
  assert_eq(spanish_date, "25 dic 2023")
  
  let spanish_time = LocaleManager::format_time(locale_manager, test_date, "short")
  assert_eq(spanish_time, "15:30")
  
  let spanish_datetime = LocaleManager::format_datetime(locale_manager, test_date, "long")
  assert_eq(spanish_datetime, "25 de diciembre de 2023, 15:30:00 UTC")
}

// Test 5: Number Formatting
test "number formatting" {
  let locale_manager = LocaleManager::new()
  
  // Test English number formatting
  LocaleManager::set_locale(locale_manager, "en")
  let english_number = LocaleManager::format_number(locale_manager, 1234567.89)
  assert_eq(english_number, "1,234,567.89")
  
  let english_currency = LocaleManager::format_currency(locale_manager, 1234.56, "USD")
  assert_eq(english_currency, "$1,234.56")
  
  let english_percent = LocaleManager::format_percent(locale_manager, 0.1234)
  assert_eq(english_percent, "12.34%")
  
  // Test Chinese number formatting
  LocaleManager::set_locale(locale_manager, "zh")
  let chinese_number = LocaleManager::format_number(locale_manager, 1234567.89)
  assert_eq(chinese_number, "1,234,567.89")
  
  let chinese_currency = LocaleManager::format_currency(locale_manager, 1234.56, "CNY")
  assert_eq(chinese_currency, "¥1,234.56")
  
  let chinese_percent = LocaleManager::format_percent(locale_manager, 0.1234)
  assert_eq(chinese_percent, "12.34%")
  
  // Test Spanish number formatting
  LocaleManager::set_locale(locale_manager, "es")
  let spanish_number = LocaleManager::format_number(locale_manager, 1234567.89)
  assert_eq(spanish_number, "1.234.567,89")
  
  let spanish_currency = LocaleManager::format_currency(locale_manager, 1234.56, "EUR")
  assert_eq(spanish_currency, "1.234,56 €")
  
  let spanish_percent = LocaleManager::format_percent(locale_manager, 0.1234)
  assert_eq(spanish_percent, "12,34 %")
}

// Test 6: Text Direction (RTL/LTR)
test "text direction" {
  let locale_manager = LocaleManager::new()
  
  // Test LTR languages
  LocaleManager::set_locale(locale_manager, "en")
  assert_eq(LocaleManager::get_text_direction(locale_manager), LTR)
  
  LocaleManager::set_locale(locale_manager, "zh")
  assert_eq(LocaleManager::get_text_direction(locale_manager), LTR)
  
  LocaleManager::set_locale(locale_manager, "es")
  assert_eq(LocaleManager::get_text_direction(locale_manager), LTR)
  
  LocaleManager::set_locale(locale_manager, "fr")
  assert_eq(LocaleManager::get_text_direction(locale_manager), LTR)
  
  // Test RTL languages
  LocaleManager::set_locale(locale_manager, "ar")
  assert_eq(LocaleManager::get_text_direction(locale_manager), RTL)
  
  LocaleManager::set_locale(locale_manager, "he")
  assert_eq(LocaleManager::get_text_direction(locale_manager), RTL)
  
  LocaleManager::set_locale(locale_manager, "fa")
  assert_eq(LocaleManager::get_text_direction(locale_manager), RTL)
}

// Test 7: Locale Fallback
test "locale fallback" {
  let locale_manager = LocaleManager::new()
  
  // Set up translations for specific locales
  LocaleManager::add_translation(locale_manager, "en-US", "regional.term", "Color")
  LocaleManager::add_translation(locale_manager, "en-GB", "regional.term", "Colour")
  LocaleManager::add_translation(locale_manager, "en", "regional.term", "Color (fallback)")
  LocaleManager::add_translation(locale_manager, "zh-CN", "regional.term", "颜色")
  LocaleManager::add_translation(locale_manager, "zh", "regional.term", "颜色 (fallback)")
  
  // Test specific locale (en-US)
  LocaleManager::set_locale(locale_manager, "en-US")
  let us_english = LocaleManager::translate(locale_manager, "regional.term")
  assert_eq(us_english, "Color")
  
  // Test specific locale (en-GB)
  LocaleManager::set_locale(locale_manager, "en-GB")
  let uk_english = LocaleManager::translate(locale_manager, "regional.term")
  assert_eq(uk_english, "Colour")
  
  // Test fallback to generic locale (en-AU -> en)
  LocaleManager::set_locale(locale_manager, "en-AU")
  let au_english = LocaleManager::translate(locale_manager, "regional.term")
  assert_eq(au_english, "Color (fallback)")
  
  // Test specific locale (zh-CN)
  LocaleManager::set_locale(locale_manager, "zh-CN")
  let cn_chinese = LocaleManager::translate(locale_manager, "regional.term")
  assert_eq(cn_chinese, "颜色")
  
  // Test fallback to generic locale (zh-TW -> zh)
  LocaleManager::set_locale(locale_manager, "zh-TW")
  let tw_chinese = LocaleManager::translate(locale_manager, "regional.term")
  assert_eq(tw_chinese, "颜色 (fallback)")
  
  // Test fallback to default (unsupported locale)
  LocaleManager::set_default_locale(locale_manager, "en")
  LocaleManager::set_locale(locale_manager, "xx-YY")
  let unsupported = LocaleManager::translate(locale_manager, "regional.term")
  assert_eq(unsupported, "Color (fallback)")
}

// Test 8: Locale Detection
test "locale detection" {
  let locale_manager = LocaleManager::new()
  
  // Test detection from Accept-Language header
  let accept_language = "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6"
  let detected = LocaleManager::detect_from_header(locale_manager, accept_language)
  assert_eq(detected, "zh-CN")
  
  // Test detection with fallback
  let accept_language2 = "xx-YY,xx;q=0.9,en;q=0.8"
  LocaleManager::set_default_locale(locale_manager, "en")
  let detected2 = LocaleManager::detect_from_header(locale_manager, accept_language2)
  assert_eq(detected2, "en")
  
  // Test detection from system locale
  let system_locale = LocaleManager::detect_from_system(locale_manager)
  assert_true(system_locale.length() > 0) // Should detect something
  
  // Test detection from user preferences
  let user_preferences = ["zh-CN", "en-US", "en"]
  let detected3 = LocaleManager::detect_from_preferences(locale_manager, user_preferences)
  assert_eq(detected3, "zh-CN")
}

// Test 9: Resource Bundle Loading
test "resource bundle loading" {
  let locale_manager = LocaleManager::new()
  
  // Create test resource bundles
  let en_bundle = ResourceBundle::new("en")
  ResourceBundle::add_translation(en_bundle, "common.ok", "OK")
  ResourceBundle::add_translation(en_bundle, "common.cancel", "Cancel")
  ResourceBundle::add_translation(en_bundle, "error.not_found", "Resource not found")
  
  let zh_bundle = ResourceBundle::new("zh")
  ResourceBundle::add_translation(zh_bundle, "common.ok", "确定")
  ResourceBundle::add_translation(zh_bundle, "common.cancel", "取消")
  ResourceBundle::add_translation(zh_bundle, "error.not_found", "未找到资源")
  
  // Load resource bundles
  LocaleManager::load_bundle(locale_manager, en_bundle)
  LocaleManager::load_bundle(locale_manager, zh_bundle)
  
  // Test loaded translations
  LocaleManager::set_locale(locale_manager, "en")
  assert_eq(LocaleManager::translate(locale_manager, "common.ok"), "OK")
  assert_eq(LocaleManager::translate(locale_manager, "common.cancel"), "Cancel")
  assert_eq(LocaleManager::translate(locale_manager, "error.not_found"), "Resource not found")
  
  LocaleManager::set_locale(locale_manager, "zh")
  assert_eq(LocaleManager::translate(locale_manager, "common.ok"), "确定")
  assert_eq(LocaleManager::translate(locale_manager, "common.cancel"), "取消")
  assert_eq(LocaleManager::translate(locale_manager, "error.not_found"), "未找到资源")
}

// Test 10: Telemetry Localization
test "telemetry localization" {
  let locale_manager = LocaleManager::new()
  
  // Set up telemetry translations
  LocaleManager::add_translation(locale_manager, "en", "telemetry.metric.cpu_usage", "CPU Usage")
  LocaleManager::add_translation(locale_manager, "en", "telemetry.metric.memory_usage", "Memory Usage")
  LocaleManager::add_translation(locale_manager, "en", "telemetry.unit.percentage", "%")
  LocaleManager::add_translation(locale_manager, "en", "telemetry.unit.megabytes", "MB")
  
  LocaleManager::add_translation(locale_manager, "zh", "telemetry.metric.cpu_usage", "CPU 使用率")
  LocaleManager::add_translation(locale_manager, "zh", "telemetry.metric.memory_usage", "内存使用量")
  LocaleManager::add_translation(locale_manager, "zh", "telemetry.unit.percentage", "%")
  LocaleManager::add_translation(locale_manager, "zh", "telemetry.unit.megabytes", "MB")
  
  // Create telemetry data
  let telemetry_data = TelemetryData::new(
    "service-123",
    "cpu_monitor",
    [
      ("metric.name", StringValue("cpu_usage")),
      ("metric.value", FloatValue(75.5)),
      ("metric.unit", StringValue("percentage"))
    ],
    1234567890L
  )
  
  // Test English telemetry localization
  LocaleManager::set_locale(locale_manager, "en")
  let localized_telemetry = LocaleManager::localize_telemetry(locale_manager, telemetry_data)
  
  let metric_name = TelemetryData::get_attribute(localized_telemetry, "metric.name")
  match metric_name {
    Some(StringValue(name)) => assert_eq(name, "CPU Usage")
    _ => assert_true(false)
  }
  
  let metric_unit = TelemetryData::get_attribute(localized_telemetry, "metric.unit")
  match metric_unit {
    Some(StringValue(unit)) => assert_eq(unit, "%")
    _ => assert_true(false)
  }
  
  // Test Chinese telemetry localization
  LocaleManager::set_locale(locale_manager, "zh")
  let localized_telemetry_zh = LocaleManager::localize_telemetry(locale_manager, telemetry_data)
  
  let metric_name_zh = TelemetryData::get_attribute(localized_telemetry_zh, "metric.name")
  match metric_name_zh {
    Some(StringValue(name)) => assert_eq(name, "CPU 使用率")
    _ => assert_true(false)
  }
  
  let metric_unit_zh = TelemetryData::get_attribute(localized_telemetry_zh, "metric.unit")
  match metric_unit_zh {
    Some(StringValue(unit)) => assert_eq(unit, "%")
    _ => assert_true(false)
  }
}