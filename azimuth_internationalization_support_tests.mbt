// Azimuth Telemetry System - Internationalization Support Tests
// This file contains test cases for internationalization and localization functionality

// Test 1: Locale Detection and Setting
test "locale detection and setting" {
  // Create an internationalization manager
  let i18n = I18nManager::new()
  
  // Test default locale
  let default_locale = I18nManager::get_default_locale(i18n)
  assert_eq(default_locale.language, "en")
  assert_eq(default_locale.region, "US")
  
  // Test setting locale
  let french_locale = Locale::new("fr", "FR")
  I18nManager::set_locale(i18n, french_locale)
  
  let current_locale = I18nManager::get_current_locale(i18n)
  assert_eq(current_locale.language, "fr")
  assert_eq(current_locale.region, "FR")
  
  // Test locale with language only
  let chinese_locale = Locale::new("zh", "")
  I18nManager::set_locale(i18n, chinese_locale)
  
  let chinese_current = I18nManager::get_current_locale(i18n)
  assert_eq(chinese_current.language, "zh")
  assert_eq(chinese_current.region, "")
  
  // Test locale parsing from string
  let parsed_locale = I18nManager::parse_locale_string("ja-JP")
  assert_eq(parsed_locale.language, "ja")
  assert_eq(parsed_locale.region, "JP")
  
  let language_only = I18nManager::parse_locale_string("de")
  assert_eq(language_only.language, "de")
  assert_eq(language_only.region, "")
}

// Test 2: Message Translation
test "message translation" {
  // Create an internationalization manager with translations
  let i18n = I18nManager::new()
  
  // Add translations
  I18nManager::add_translation(i18n, "en", "US", "welcome.message", "Welcome to Azimuth Telemetry")
  I18nManager::add_translation(i18n, "fr", "FR", "welcome.message", "Bienvenue à Azimuth Télémétrie")
  I18nManager::add_translation(i18n, "zh", "CN", "welcome.message", "欢迎使用方位遥测系统")
  I18nManager::add_translation(i18n, "es", "ES", "welcome.message", "Bienvenido a Azimuth Telemetría")
  
  // Test translation in different locales
  I18nManager::set_locale(i18n, Locale::new("en", "US"))
  let english_message = I18nManager::translate(i18n, "welcome.message")
  assert_eq(english_message, "Welcome to Azimuth Telemetry")
  
  I18nManager::set_locale(i18n, Locale::new("fr", "FR"))
  let french_message = I18nManager::translate(i18n, "welcome.message")
  assert_eq(french_message, "Bienvenue à Azimuth Télémétrie")
  
  I18nManager::set_locale(i18n, Locale::new("zh", "CN"))
  let chinese_message = I18nManager::translate(i18n, "welcome.message")
  assert_eq(chinese_message, "欢迎使用方位遥测系统")
  
  I18nManager::set_locale(i18n, Locale::new("es", "ES"))
  let spanish_message = I18nManager::translate(i18n, "welcome.message")
  assert_eq(spanish_message, "Bienvenido a Azimuth Telemetría")
  
  // Test fallback to default locale when translation is missing
  I18nManager::set_locale(i18n, Locale::new("ja", "JP"))
  let fallback_message = I18nManager::translate(i18n, "welcome.message")
  assert_eq(fallback_message, "Welcome to Azimuth Telemetry")  // Should fallback to English
  
  // Test missing key
  I18nManager::set_locale(i18n, Locale::new("en", "US"))
  let missing_message = I18nManager::translate(i18n, "non.existent.key")
  assert_eq(missing_message, "non.existent.key")  // Should return the key itself
}

// Test 3: Parameterized Translations
test "parameterized translations" {
  // Create an internationalization manager
  let i18n = I18nManager::new()
  
  // Add parameterized translations
  I18nManager::add_translation(i18n, "en", "US", "user.greeting", "Hello, {name}! Welcome to {service}.")
  I18nManager::add_translation(i18n, "fr", "FR", "user.greeting", "Bonjour, {name}! Bienvenue à {service}.")
  I18nManager::add_translation(i18n, "zh", "CN", "user.greeting", "你好，{name}！欢迎使用{service}。")
  
  // Test parameter substitution in English
  I18nManager::set_locale(i18n, Locale::new("en", "US"))
  let params = [
    ("name", "Alice"),
    ("service", "Azimuth")
  ]
  let english_greeting = I18nManager::translate_with_params(i18n, "user.greeting", params)
  assert_eq(english_greeting, "Hello, Alice! Welcome to Azimuth.")
  
  // Test parameter substitution in French
  I18nManager::set_locale(i18n, Locale::new("fr", "FR"))
  let french_greeting = I18nManager::translate_with_params(i18n, "user.greeting", params)
  assert_eq(french_greeting, "Bonjour, Alice! Bienvenue à Azimuth.")
  
  // Test parameter substitution in Chinese
  I18nManager::set_locale(i18n, Locale::new("zh", "CN"))
  let chinese_greeting = I18nManager::translate_with_params(i18n, "user.greeting", params)
  assert_eq(chinese_greeting, "你好，Alice！欢迎使用Azimuth。")
  
  // Test with missing parameter
  let incomplete_params = [
    ("name", "Bob")
    // Missing "service" parameter
  ]
  let incomplete_greeting = I18nManager::translate_with_params(i18n, "user.greeting", incomplete_params)
  assert_eq(incomplete_greeting, "Hello, Bob! Welcome to {service}.")  // Unsubstituted parameter remains
}

// Test 4: Pluralization Rules
test "pluralization rules" {
  // Create an internationalization manager
  let i18n = I18nManager::new()
  
  // Add pluralized translations
  I18nManager::add_plural_translation(i18n, "en", "US", "item.count", [
    (PluralRule::Zero, "No items"),
    (PluralRule::One, "One item"),
    (PluralRule::Other, "{count} items")
  ])
  
  I18nManager::add_plural_translation(i18n, "fr", "FR", "item.count", [
    (PluralRule::Zero, "Aucun élément"),
    (PluralRule::One, "Un élément"),
    (PluralRule::Other, "{count} éléments")
  ])
  
  I18nManager::add_plural_translation(i18n, "zh", "CN", "item.count", [
    (PluralRule::Other, "{count}个项目")
  ])
  
  // Test English pluralization
  I18nManager::set_locale(i18n, Locale::new("en", "US"))
  let zero_items = I18nManager::translate_plural(i18n, "item.count", 0)
  assert_eq(zero_items, "No items")
  
  let one_item = I18nManager::translate_plural(i18n, "item.count", 1)
  assert_eq(one_item, "One item")
  
  let many_items = I18nManager::translate_plural(i18n, "item.count", 5)
  assert_eq(many_items, "5 items")
  
  // Test French pluralization
  I18nManager::set_locale(i18n, Locale::new("fr", "FR"))
  let zero_items_fr = I18nManager::translate_plural(i18n, "item.count", 0)
  assert_eq(zero_items_fr, "Aucun élément")
  
  let one_item_fr = I18nManager::translate_plural(i18n, "item.count", 1)
  assert_eq(one_item_fr, "Un élément")
  
  let many_items_fr = I18nManager::translate_plural(i18n, "item.count", 5)
  assert_eq(many_items_fr, "5 éléments")
  
  // Test Chinese pluralization (Chinese typically doesn't have plural forms)
  I18nManager::set_locale(i18n, Locale::new("zh", "CN"))
  let zero_items_zh = I18nManager::translate_plural(i18n, "item.count", 0)
  assert_eq(zero_items_zh, "0个项目")
  
  let one_item_zh = I18nManager::translate_plural(i18n, "item.count", 1)
  assert_eq(one_item_zh, "1个项目")
  
  let many_items_zh = I18nManager::translate_plural(i18n, "item.count", 5)
  assert_eq(many_items_zh, "5个项目")
}

// Test 5: Date and Time Formatting
test "date and time formatting" {
  // Create an internationalization manager
  let i18n = I18nManager::new()
  
  // Create a date/time to format (2023-01-15 14:30:00 UTC)
  let date_time = DateTime::new(2023, 1, 15, 14, 30, 0)
  
  // Test US English formatting
  I18nManager::set_locale(i18n, Locale::new("en", "US"))
  let us_date = I18nManager::format_date(i18n, date_time, DateFormat::Medium)
  assert_eq(us_date, "Jan 15, 2023")
  
  let us_time = I18nManager::format_time(i18n, date_time, TimeFormat::Medium)
  assert_eq(us_time, "2:30:00 PM")
  
  let us_datetime = I18nManager::format_datetime(i18n, date_time, DateTimeFormat::Medium)
  assert_eq(us_datetime, "Jan 15, 2023, 2:30:00 PM")
  
  // Test French formatting
  I18nManager::set_locale(i18n, Locale::new("fr", "FR"))
  let fr_date = I18nManager::format_date(i18n, date_time, DateFormat::Medium)
  assert_eq(fr_date, "15 janv. 2023")
  
  let fr_time = I18nManager::format_time(i18n, date_time, TimeFormat::Medium)
  assert_eq(fr_time, "14:30:00")
  
  let fr_datetime = I18nManager::format_datetime(i18n, date_time, DateTimeFormat::Medium)
  assert_eq(fr_datetime, "15 janv. 2023 à 14:30:00")
  
  // Test Chinese formatting
  I18nManager::set_locale(i18n, Locale::new("zh", "CN"))
  let zh_date = I18nManager::format_date(i18n, date_time, DateFormat::Medium)
  assert_eq(zh_date, "2023年1月15日")
  
  let zh_time = I18nManager::format_time(i18n, date_time, TimeFormat::Medium)
  assert_eq(zh_time, "下午2:30:00")
  
  let zh_datetime = I18nManager::format_datetime(i18n, date_time, DateTimeFormat::Medium)
  assert_eq(zh_datetime, "2023年1月15日 下午2:30:00")
  
  // Test custom formatting
  I18nManager::set_locale(i18n, Locale::new("en", "US"))
  let custom_format = I18nManager::format_datetime_custom(i18n, date_time, "yyyy-MM-dd HH:mm:ss")
  assert_eq(custom_format, "2023-01-15 14:30:00")
}

// Test 6: Number Formatting
test "number formatting" {
  // Create an internationalization manager
  let i18n = I18nManager::new()
  
  // Test US English formatting
  I18nManager::set_locale(i18n, Locale::new("en", "US"))
  let us_integer = I18nManager::format_integer(i18n, 1234567)
  assert_eq(us_integer, "1,234,567")
  
  let us_decimal = I18nManager::format_decimal(i18n, 1234567.89)
  assert_eq(us_decimal, "1,234,567.89")
  
  let us_currency = I18nManager::format_currency(i18n, 1234567.89, "USD")
  assert_eq(us_currency, "$1,234,567.89")
  
  let us_percent = I18nManager::format_percent(i18n, 0.1234)
  assert_eq(us_percent, "12.34%")
  
  // Test French formatting
  I18nManager::set_locale(i18n, Locale::new("fr", "FR"))
  let fr_integer = I18nManager::format_integer(i18n, 1234567)
  assert_eq(fr_integer, "1\u{202F}234\u{202F}567")  // Uses non-breaking space as separator
  
  let fr_decimal = I18nManager::format_decimal(i18n, 1234567.89)
  assert_eq(fr_decimal, "1\u{202F}234\u{202F}567,89")  // Comma as decimal separator
  
  let fr_currency = I18nManager::format_currency(i18n, 1234567.89, "EUR")
  assert_eq(fr_currency, "1\u{202F}234\u{202F}567,89\u{A0}€")
  
  let fr_percent = I18nManager::format_percent(i18n, 0.1234)
  assert_eq(fr_percent, "12,34\u{A0}%")
  
  // Test Chinese formatting
  I18nManager::set_locale(i18n, Locale::new("zh", "CN"))
  let zh_integer = I18nManager::format_integer(i18n, 1234567)
  assert_eq(zh_integer, "1,234,567")
  
  let zh_decimal = I18nManager::format_decimal(i18n, 1234567.89)
  assert_eq(zh_decimal, "1,234,567.89")
  
  let zh_currency = I18nManager::format_currency(i18n, 1234567.89, "CNY")
  assert_eq(zh_currency, "￥1,234,567.89")
  
  let zh_percent = I18nManager::format_percent(i18n, 0.1234)
  assert_eq(zh_percent, "12.34%")
  
  // Test precision control
  I18nManager::set_locale(i18n, Locale::new("en", "US"))
  let high_precision = I18nManager::format_decimal_with_precision(i18n, 3.14159265359, 6)
  assert_eq(high_precision, "3.141593")
  
  let low_precision = I18nManager::format_decimal_with_precision(i18n, 3.14159265359, 2)
  assert_eq(low_precision, "3.14")
}

// Test 7: Text Direction (RTL/LTR) Support
test "text direction support" {
  // Create an internationalization manager
  let i18n = I18nManager::new()
  
  // Test LTR languages
  I18nManager::set_locale(i18n, Locale::new("en", "US"))
  assert_eq(I18nManager::get_text_direction(i18n), TextDirection::LTR)
  
  I18nManager::set_locale(i18n, Locale::new("fr", "FR"))
  assert_eq(I18nManager::get_text_direction(i18n), TextDirection::LTR)
  
  I18nManager::set_locale(i18n, Locale::new("zh", "CN"))
  assert_eq(I18nManager::get_text_direction(i18n), TextDirection::LTR)
  
  // Test RTL languages
  I18nManager::set_locale(i18n, Locale::new("ar", "SA"))
  assert_eq(I18nManager::get_text_direction(i18n), TextDirection::RTL)
  
  I18nManager::set_locale(i18n, Locale::new("he", "IL"))
  assert_eq(I18nManager::get_text_direction(i18n), TextDirection::RTL)
  
  // Test text alignment
  I18nManager::set_locale(i18n, Locale::new("en", "US"))
  let ltr_alignment = I18nManager::get_text_alignment(i18n)
  assert_eq(ltr_alignment, TextAlign::Left)
  
  I18nManager::set_locale(i18n, Locale::new("ar", "SA"))
  let rtl_alignment = I18nManager::get_text_alignment(i18n)
  assert_eq(rtl_alignment, TextAlign::Right)
  
  // Test layout direction
  I18nManager::set_locale(i18n, Locale::new("en", "US"))
  let ltr_layout = I18nManager::get_layout_direction(i18n)
  assert_eq(ltr_layout, LayoutDirection::LTR)
  
  I18nManager::set_locale(i18n, Locale::new("ar", "SA"))
  let rtl_layout = I18nManager::get_layout_direction(i18n)
  assert_eq(rtl_layout, LayoutDirection::RTL)
  
  // Test text wrapping with mixed content
  I18nManager::add_translation(i18n, "en", "US", "mixed.text", "English text with العربية Arabic words")
  I18nManager::add_translation(i18n, "ar", "SA", "mixed.text", "نص عربي مع English كلمات إنجليزية")
  
  I18nManager::set_locale(i18n, Locale::new("en", "US"))
  let ltr_mixed = I18nManager::translate(i18n, "mixed.text")
  let ltr_wrapped = I18nManager::wrap_text(i18n, ltr_mixed, 20)
  
  I18nManager::set_locale(i18n, Locale::new("ar", "SA"))
  let rtl_mixed = I18nManager::translate(i18n, "mixed.text")
  let rtl_wrapped = I18nManager::wrap_text(i18n, rtl_mixed, 20)
  
  // The wrapped text should maintain proper text direction
  assert_true(ltr_wrapped.length() > 0)
  assert_true(rtl_wrapped.length() > 0)
}

// Test 8: Character Encoding and Unicode Support
test "character encoding and unicode support" {
  // Create an internationalization manager
  let i18n = I18nManager::new()
  
  // Test various Unicode characters
  let unicode_texts = [
    ("en", "Hello, world!"),
    ("fr", "Bonjour le monde!"),
    ("es", "¡Hola, mundo!"),
    ("de", "Hallo, Welt!"),
    ("ru", "Привет, мир!"),
    ("zh", "你好，世界！"),
    ("ja", "こんにちは、世界！"),
    ("ar", "مرحبا بالعالم!"),
    ("hi", "नमस्ते दुनिया!"),
    ("th", "สวัสดีชาวโลก!")
  ]
  
  for (locale, text) in unicode_texts {
    I18nManager::add_translation(i18n, locale, "", "greeting.message", text)
  }
  
  // Test each language
  for (locale, expected_text) in unicode_texts {
    let parsed_locale = I18nManager::parse_locale_string(locale)
    I18nManager::set_locale(i18n, parsed_locale)
    
    let translated_text = I18nManager::translate(i18n, "greeting.message")
    assert_eq(translated_text, expected_text)
    
    // Test character count
    let char_count = I18nManager::get_character_count(i18n, translated_text)
    assert_true(char_count > 0)
    
    // Test display width (some characters take more width)
    let display_width = I18nManager::get_display_width(i18n, translated_text)
    assert_true(display_width > 0)
  }
  
  // Test emoji support
  I18nManager::add_translation(i18n, "en", "", "emoji.message", "Hello 🌍! How are you 😊?")
  I18nManager::set_locale(i18n, Locale::new("en", "US"))
  let emoji_text = I18nManager::translate(i18n, "emoji.message")
  assert_eq(emoji_text, "Hello 🌍! How are you 😊?")
  
  // Test normalization
  let normalized_text = I18nManager::normalize_text(i18n, "café")  // Should normalize accented characters
  assert_eq(normalized_text, "café")  // Should be properly normalized
  
  // Test case conversion for different locales
  I18nManager::set_locale(i18n, Locale::new("en", "US"))
  let us_upper = I18nManager::to_upper_case(i18n, "hello")
  assert_eq(us_upper, "HELLO")
  
  let us_lower = I18nManager::to_lower_case(i18n, "WORLD")
  assert_eq(us_lower, "world")
  
  I18nManager::set_locale(i18n, Locale::new("tr", "TR"))  // Turkish has special case rules
  let tr_upper = I18nManager::to_upper_case(i18n, "istanbul")  // 'i' becomes 'İ' in Turkish
  assert_eq(tr_upper, "İSTANBUL")
}

// Test 9: Resource Bundle Management
test "resource bundle management" {
  // Create an internationalization manager
  let i18n = I18nManager::new()
  
  // Create resource bundles for different locales
  let en_bundle = ResourceBundle::new("en", "US")
  ResourceBundle::add_translation(en_bundle, "app.title", "Azimuth Telemetry")
  ResourceBundle::add_translation(en_bundle, "button.save", "Save")
  ResourceBundle::add_translation(en_bundle, "button.cancel", "Cancel")
  
  let fr_bundle = ResourceBundle::new("fr", "FR")
  ResourceBundle::add_translation(fr_bundle, "app.title", "Télémétrie Azimuth")
  ResourceBundle::add_translation(fr_bundle, "button.save", "Enregistrer")
  ResourceBundle::add_translation(fr_bundle, "button.cancel", "Annuler")
  
  let zh_bundle = ResourceBundle::new("zh", "CN")
  ResourceBundle::add_translation(zh_bundle, "app.title", "方位遥测")
  ResourceBundle::add_translation(zh_bundle, "button.save", "保存")
  ResourceBundle::add_translation(zh_bundle, "button.cancel", "取消")
  
  // Register bundles
  I18nManager::register_bundle(i18n, en_bundle)
  I18nManager::register_bundle(i18n, fr_bundle)
  I18nManager::register_bundle(i18n, zh_bundle)
  
  // Test loading bundles
  I18nManager::set_locale(i18n, Locale::new("en", "US"))
  assert_eq(I18nManager::translate(i18n, "app.title"), "Azimuth Telemetry")
  assert_eq(I18nManager::translate(i18n, "button.save"), "Save")
  assert_eq(I18nManager::translate(i18n, "button.cancel"), "Cancel")
  
  I18nManager::set_locale(i18n, Locale::new("fr", "FR"))
  assert_eq(I18nManager::translate(i18n, "app.title"), "Télémétrie Azimuth")
  assert_eq(I18nManager::translate(i18n, "button.save"), "Enregistrer")
  assert_eq(I18nManager::translate(i18n, "button.cancel"), "Annuler")
  
  I18nManager::set_locale(i18n, Locale::new("zh", "CN"))
  assert_eq(I18nManager::translate(i18n, "app.title"), "方位遥测")
  assert_eq(I18nManager::translate(i18n, "button.save"), "保存")
  assert_eq(I18nManager::translate(i18n, "button.cancel"), "取消")
  
  // Test bundle serialization
  let serialized_bundle = ResourceBundle::serialize(en_bundle)
  assert_true(serialized_bundle.length() > 0)
  
  let deserialized_bundle = ResourceBundle::deserialize(serialized_bundle)
  assert_eq(ResourceBundle::get_locale(deserialized_bundle).language, "en")
  assert_eq(ResourceBundle::get_locale(deserialized_bundle).region, "US")
  assert_eq(ResourceBundle::get_translation(deserialized_bundle, "app.title"), "Azimuth Telemetry")
  
  // Test bundle inheritance
  let en_gb_bundle = ResourceBundle::new("en", "GB")
  ResourceBundle::set_parent_bundle(en_gb_bundle, en_bundle)
  ResourceBundle::add_translation(en_gb_bundle, "button.cancel", "Close")  // Override
  
  I18nManager::register_bundle(i18n, en_gb_bundle)
  I18nManager::set_locale(i18n, Locale::new("en", "GB"))
  
  // Should inherit from parent bundle
  assert_eq(I18nManager::translate(i18n, "app.title"), "Azimuth Telemetry")  // From parent
  assert_eq(I18nManager::translate(i18n, "button.save"), "Save")            // From parent
  assert_eq(I18nManager::translate(i18n, "button.cancel"), "Close")          // Override
}

// Test 10: Dynamic Locale Switching
test "dynamic locale switching" {
  // Create an internationalization manager
  let i18n = I18nManager::new()
  
  // Add translations for multiple locales
  I18nManager::add_translation(i18n, "en", "US", "welcome", "Welcome")
  I18nManager::add_translation(i18n, "fr", "FR", "welcome", "Bienvenue")
  I18nManager::add_translation(i18n, "zh", "CN", "welcome", "欢迎")
  
  // Register locale change listeners
  let mut locale_change_events = []
  
  I18nManager::add_locale_change_listener(i18n, |old_locale, new_locale| {
    locale_change_events.push((old_locale, new_locale))
  })
  
  // Test initial locale
  let initial_locale = I18nManager::get_current_locale(i18n)
  assert_eq(initial_locale.language, "en")
  assert_eq(initial_locale.region, "US")
  
  // Test locale switching
  I18nManager::set_locale(i18n, Locale::new("fr", "FR"))
  
  let french_locale = I18nManager::get_current_locale(i18n)
  assert_eq(french_locale.language, "fr")
  assert_eq(french_locale.region, "FR")
  assert_eq(I18nManager::translate(i18n, "welcome"), "Bienvenue")
  
  // Verify locale change event was fired
  assert_eq(locale_change_events.length(), 1)
  assert_eq(locale_change_events[0].0.language, "en")  // Old locale
  assert_eq(locale_change_events[0].0.region, "US")
  assert_eq(locale_change_events[0].1.language, "fr")  // New locale
  assert_eq(locale_change_events[0].1.region, "FR")
  
  // Test another locale switch
  I18nManager::set_locale(i18n, Locale::new("zh", "CN"))
  
  let chinese_locale = I18nManager::get_current_locale(i18n)
  assert_eq(chinese_locale.language, "zh")
  assert_eq(chinese_locale.region, "CN")
  assert_eq(I18nManager::translate(i18n, "welcome"), "欢迎")
  
  // Verify another locale change event was fired
  assert_eq(locale_change_events.length(), 2)
  assert_eq(locale_change_events[1].0.language, "fr")  // Old locale
  assert_eq(locale_change_events[1].0.region, "FR")
  assert_eq(locale_change_events[1].1.language, "zh")  // New locale
  assert_eq(locale_change_events[1].1.region, "CN")
  
  // Test locale preference persistence
  let preferences = I18nManager::get_locale_preferences(i18n)
  assert_eq(preferences.primary_locale.language, "zh")
  assert_eq(preferences.primary_locale.region, "CN")
  
  // Test locale fallback chain
  I18nManager::set_locale_fallback_chain(i18n, [
    Locale::new("zh", "TW"),
    Locale::new("zh", "CN"),
    Locale::new("en", "US")
  ])
  
  // Add translation for Traditional Chinese
  I18nManager::add_translation(i18n, "zh", "TW", "welcome", "歡迎")
  
  I18nManager::set_locale(i18n, Locale::new("zh", "TW"))
  assert_eq(I18nManager::translate(i18n, "welcome"), "歡迎")  // Should use zh-TW
  
  // Remove zh-TW translation and test fallback
  I18nManager::remove_translation(i18n, "zh", "TW", "welcome")
  assert_eq(I18nManager::translate(i18n, "welcome"), "欢迎")  // Should fallback to zh-CN
  
  // Remove zh-CN translation and test fallback
  I18nManager::remove_translation(i18n, "zh", "CN", "welcome")
  assert_eq(I18nManager::translate(i18n, "welcome"), "Welcome")  // Should fallback to en-US
  
  // Test removing locale change listener
  I18nManager::remove_locale_change_listener(i18n, locale_change_events[0])  // Remove first listener
  
  // Change locale again
  I18nManager::set_locale(i18n, Locale::new("en", "US"))
  
  // Should not have triggered another event (listener was removed)
  assert_eq(locale_change_events.length(), 2)  // Still 2, not 3
}