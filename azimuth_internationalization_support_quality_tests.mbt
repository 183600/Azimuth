// Azimuth Telemetry System - Internationalization Support Quality Tests
// This file contains comprehensive test cases for internationalization functionality

// Test 1: Basic Localization
test "basic localization" {
  let i18n = Internationalization::new()
  
  // Add translations for different languages
  Internationalization::add_translation(i18n, "en", "welcome_message", "Welcome to Azimuth Telemetry")
  Internationalization::add_translation(i18n, "es", "welcome_message", "Bienvenido a Azimuth Telemetry")
  Internationalization::add_translation(i18n, "fr", "welcome_message", "Bienvenue dans Azimuth Telemetry")
  Internationalization::add_translation(i18n, "zh", "welcome_message", "欢迎使用 Azimuth 遥测系统")
  Internationalization::add_translation(i18n, "ja", "welcome_message", "Azimuth Telemetry へようこそ")
  
  // Set language to English
  Internationalization::set_language(i18n, "en")
  let english_message = Internationalization::translate(i18n, "welcome_message")
  match english_message {
    Some(message) => assert_eq(message, "Welcome to Azimuth Telemetry")
    None => assert_true(false)
  }
  
  // Set language to Spanish
  Internationalization::set_language(i18n, "es")
  let spanish_message = Internationalization::translate(i18n, "welcome_message")
  match spanish_message {
    Some(message) => assert_eq(message, "Bienvenido a Azimuth Telemetry")
    None => assert_true(false)
  }
  
  // Set language to French
  Internationalization::set_language(i18n, "fr")
  let french_message = Internationalization::translate(i18n, "welcome_message")
  match french_message {
    Some(message) => assert_eq(message, "Bienvenue dans Azimuth Telemetry")
    None => assert_true(false)
  }
  
  // Set language to Chinese
  Internationalization::set_language(i18n, "zh")
  let chinese_message = Internationalization::translate(i18n, "welcome_message")
  match chinese_message {
    Some(message) => assert_eq(message, "欢迎使用 Azimuth 遥测系统")
    None => assert_true(false)
  }
  
  // Set language to Japanese
  Internationalization::set_language(i18n, "ja")
  let japanese_message = Internationalization::translate(i18n, "welcome_message")
  match japanese_message {
    Some(message) => assert_eq(message, "Azimuth Telemetry へようこそ")
    None => assert_true(false)
  }
}

// Test 2: Parameterized Translations
test "parameterized translations" {
  let i18n = Internationalization::new()
  
  // Add parameterized translations
  Internationalization::add_translation(i18n, "en", "operation_completed", "Operation {name} completed in {duration}ms")
  Internationalization::add_translation(i18n, "es", "operation_completed", "Operación {name} completada en {duration}ms")
  Internationalization::add_translation(i18n, "fr", "operation_completed", "Opération {name} terminée en {duration}ms")
  
  // Test English parameterized translation
  Internationalization::set_language(i18n, "en")
  let english_result = Internationalization::translate_with_params(i18n, "operation_completed", [
    ("name", "database_query"),
    ("duration", "150")
  ])
  match english_result {
    Some(message) => assert_eq(message, "Operation database_query completed in 150ms")
    None => assert_true(false)
  }
  
  // Test Spanish parameterized translation
  Internationalization::set_language(i18n, "es")
  let spanish_result = Internationalization::translate_with_params(i18n, "operation_completed", [
    ("name", "consulta_base_de_datos"),
    ("duration", "250")
  ])
  match spanish_result {
    Some(message) => assert_eq(message, "Operación consulta_base_de_datos completada en 250ms")
    None => assert_true(false)
  }
  
  // Test French parameterized translation
  Internationalization::set_language(i18n, "fr")
  let french_result = Internationalization::translate_with_params(i18n, "operation_completed", [
    ("name", "requête_base_de_données"),
    ("duration", "180")
  ])
  match french_result {
    Some(message) => assert_eq(message, "Opération requête_base_de_données terminée en 180ms")
    None => assert_true(false)
  }
}

// Test 3: Pluralization Rules
test "pluralization rules" {
  let i18n = Internationalization::new()
  
  // Add pluralized translations
  Internationalization::add_plural_translation(i18n, "en", "items_count", [
    (1, "1 item"),
    (PluralRule::Other, "{count} items")
  ])
  
  Internationalization::add_plural_translation(i18n, "es", "items_count", [
    (1, "1 elemento"),
    (PluralRule::Other, "{count} elementos")
  ])
  
  Internationalization::add_plural_translation(i18n, "fr", "items_count", [
    (0, "0 élément"),
    (1, "1 élément"),
    (PluralRule::Other, "{count} éléments")
  ])
  
  // Test English pluralization
  Internationalization::set_language(i18n, "en")
  
  let english_one = Internationalization::translate_plural(i18n, "items_count", 1)
  match english_one {
    Some(message) => assert_eq(message, "1 item")
    None => assert_true(false)
  }
  
  let english_many = Internationalization::translate_plural(i18n, "items_count", 5)
  match english_many {
    Some(message) => assert_eq(message, "5 items")
    None => assert_true(false)
  }
  
  // Test Spanish pluralization
  Internationalization::set_language(i18n, "es")
  
  let spanish_one = Internationalization::translate_plural(i18n, "items_count", 1)
  match spanish_one {
    Some(message) => assert_eq(message, "1 elemento")
    None => assert_true(false)
  }
  
  let spanish_many = Internationalization::translate_plural(i18n, "items_count", 7)
  match spanish_many {
    Some(message) => assert_eq(message, "7 elementos")
    None => assert_true(false)
  }
  
  // Test French pluralization
  Internationalization::set_language(i18n, "fr")
  
  let french_zero = Internationalization::translate_plural(i18n, "items_count", 0)
  match french_zero {
    Some(message) => assert_eq(message, "0 élément")
    None => assert_true(false)
  }
  
  let french_one = Internationalization::translate_plural(i18n, "items_count", 1)
  match french_one {
    Some(message) => assert_eq(message, "1 élément")
    None => assert_true(false)
  }
  
  let french_many = Internationalization::translate_plural(i18n, "items_count", 3)
  match french_many {
    Some(message) => assert_eq(message, "3 éléments")
    None => assert_true(false)
  }
}

// Test 4: Date and Time Formatting
test "date and time formatting" {
  let i18n = Internationalization::new()
  
  // Set up locale-specific date/time formats
  Internationalization::set_date_format(i18n, "en", "MM/DD/YYYY")
  Internationalization::set_date_format(i18n, "es", "DD/MM/YYYY")
  Internationalization::set_date_format(i18n, "fr", "DD/MM/YYYY")
  Internationalization::set_date_format(i18n, "zh", "YYYY年MM月DD日")
  Internationalization::set_date_format(i18n, "ja", "YYYY年MM月DD日")
  
  Internationalization::set_time_format(i18n, "en", "h:mm:ss A")
  Internationalization::set_time_format(i18n, "es", "HH:mm:ss")
  Internationalization::set_time_format(i18n, "fr", "HH:mm:ss")
  Internationalization::set_time_format(i18n, "zh", "HH:mm:ss")
  Internationalization::set_time_format(i18n, "ja", "HH:mm:ss")
  
  let test_date_time = DateTime::new(2023, 6, 15, 14, 30, 45)
  
  // Test English date/time formatting
  Internationalization::set_language(i18n, "en")
  let english_date = Internationalization::format_date(i18n, test_date_time)
  let english_time = Internationalization::format_time(i18n, test_date_time)
  let english_datetime = Internationalization::format_datetime(i18n, test_date_time)
  
  assert_eq(english_date, "06/15/2023")
  assert_eq(english_time, "2:30:45 PM")
  assert_eq(english_datetime, "06/15/2023 2:30:45 PM")
  
  // Test Spanish date/time formatting
  Internationalization::set_language(i18n, "es")
  let spanish_date = Internationalization::format_date(i18n, test_date_time)
  let spanish_time = Internationalization::format_time(i18n, test_date_time)
  let spanish_datetime = Internationalization::format_datetime(i18n, test_date_time)
  
  assert_eq(spanish_date, "15/06/2023")
  assert_eq(spanish_time, "14:30:45")
  assert_eq(spanish_datetime, "15/06/2023 14:30:45")
  
  // Test Chinese date/time formatting
  Internationalization::set_language(i18n, "zh")
  let chinese_date = Internationalization::format_date(i18n, test_date_time)
  let chinese_time = Internationalization::format_time(i18n, test_date_time)
  let chinese_datetime = Internationalization::format_datetime(i18n, test_date_time)
  
  assert_eq(chinese_date, "2023年06月15日")
  assert_eq(chinese_time, "14:30:45")
  assert_eq(chinese_datetime, "2023年06月15日 14:30:45")
}

// Test 5: Number Formatting
test "number formatting" {
  let i18n = Internationalization::new()
  
  // Set up locale-specific number formats
  Internationalization::set_number_format(i18n, "en", NumberFormat::new(".", ",", 2))
  Internationalization::set_number_format(i18n, "es", NumberFormat::new(",", ".", 2))
  Internationalization::set_number_format(i18n, "fr", NumberFormat::new(",", " ", 2))
  Internationalization::set_number_format(i18n, "zh", NumberFormat::new(".", ",", 2))
  Internationalization::set_number_format(i18n, "ja", NumberFormat::new(".", ",", 2))
  
  let test_number = 1234567.8912
  
  // Test English number formatting
  Internationalization::set_language(i18n, "en")
  let english_number = Internationalization::format_number(i18n, test_number)
  assert_eq(english_number, "1,234,567.89")
  
  // Test Spanish number formatting
  Internationalization::set_language(i18n, "es")
  let spanish_number = Internationalization::format_number(i18n, test_number)
  assert_eq(spanish_number, "1.234.567,89")
  
  // Test French number formatting
  Internationalization::set_language(i18n, "fr")
  let french_number = Internationalization::format_number(i18n, test_number)
  assert_eq(french_number, "1 234 567,89")
  
  // Test Chinese number formatting
  Internationalization::set_language(i18n, "zh")
  let chinese_number = Internationalization::format_number(i18n, test_number)
  assert_eq(chinese_number, "1,234,567.89")
  
  // Test currency formatting
  Internationalization::set_currency_format(i18n, "en", "USD", "$")
  Internationalization::set_currency_format(i18n, "es", "EUR", "€")
  Internationalization::set_currency_format(i18n, "fr", "EUR", "€")
  Internationalization::set_currency_format(i18n, "zh", "CNY", "¥")
  Internationalization::set_currency_format(i18n, "ja", "JPY", "¥")
  
  // Test English currency formatting
  Internationalization::set_language(i18n, "en")
  let english_currency = Internationalization::format_currency(i18n, test_number)
  assert_eq(english_currency, "$1,234,567.89")
  
  // Test Spanish currency formatting
  Internationalization::set_language(i18n, "es")
  let spanish_currency = Internationalization::format_currency(i18n, test_number)
  assert_eq(spanish_currency, "1.234.567,89€")
  
  // Test Chinese currency formatting
  Internationalization::set_language(i18n, "zh")
  let chinese_currency = Internationalization::format_currency(i18n, test_number)
  assert_eq(chinese_currency, "¥1,234,567.89")
}

// Test 6: Text Direction (RTL/LTR)
test "text direction" {
  let i18n = Internationalization::new()
  
  // Set up text directions for different languages
  Internationalization::set_text_direction(i18n, "en", TextDirection::LTR)
  Internationalization::set_text_direction(i18n, "es", TextDirection::LTR)
  Internationalization::set_text_direction(i18n, "fr", TextDirection::LTR)
  Internationalization::set_text_direction(i18n, "ar", TextDirection::RTL)
  Internationalization::set_text_direction(i18n, "he", TextDirection::RTL)
  
  // Test LTR languages
  Internationalization::set_language(i18n, "en")
  assert_eq(Internationalization::get_text_direction(i18n), TextDirection::LTR)
  assert_false(Internationalization::is_rtl(i18n))
  
  Internationalization::set_language(i18n, "es")
  assert_eq(Internationalization::get_text_direction(i18n), TextDirection::LTR)
  assert_false(Internationalization::is_rtl(i18n))
  
  // Test RTL languages
  Internationalization::set_language(i18n, "ar")
  assert_eq(Internationalization::get_text_direction(i18n), TextDirection::RTL)
  assert_true(Internationalization::is_rtl(i18n))
  
  Internationalization::set_language(i18n, "he")
  assert_eq(Internationalization::get_text_direction(i18n), TextDirection::RTL)
  assert_true(Internationalization::is_rtl(i18n))
  
  // Test text alignment based on direction
  Internationalization::set_language(i18n, "en")
  assert_eq(Internationalization::get_text_align(i18n), "left")
  assert_eq(Internationalization::get_text_align_reverse(i18n), "right")
  
  Internationalization::set_language(i18n, "ar")
  assert_eq(Internationalization::get_text_align(i18n), "right")
  assert_eq(Internationalization::get_text_align_reverse(i18n), "left")
}

// Test 7: Fallback Language
test "fallback language" {
  let i18n = Internationalization::new()
  
  // Add translations for primary language
  Internationalization::add_translation(i18n, "en", "welcome", "Welcome")
  Internationalization::add_translation(i18n, "en", "goodbye", "Goodbye")
  
  // Add partial translations for secondary language
  Internationalization::add_translation(i18n, "es", "welcome", "Bienvenido")
  // Note: "goodbye" is not translated in Spanish
  
  // Set fallback language
  Internationalization::set_fallback_language(i18n, "en")
  
  // Test with Spanish (partial translations)
  Internationalization::set_language(i18n, "es")
  
  // Should get Spanish translation
  let welcome = Internationalization::translate(i18n, "welcome")
  match welcome {
    Some(message) => assert_eq(message, "Bienvenido")
    None => assert_true(false)
  }
  
  // Should fall back to English for missing translation
  let goodbye = Internationalization::translate(i18n, "goodbye")
  match goodbye {
    Some(message) => assert_eq(message, "Goodbye")
    None => assert_true(false)
  }
  
  // Test with language that has no translations
  Internationalization::set_language(i18n, "fr")
  
  // Should fall back to English for all translations
  let welcome_fr = Internationalization::translate(i18n, "welcome")
  match welcome_fr {
    Some(message) => assert_eq(message, "Welcome")
    None => assert_true(false)
  }
  
  let goodbye_fr = Internationalization::translate(i18n, "goodbye")
  match goodbye_fr {
    Some(message) => assert_eq(message, "Goodbye")
    None => assert_true(false)
  }
}

// Test 8: Loading Translations from Files
test "loading translations from files" {
  let i18n = Internationalization::new()
  
  // Create translation files
  let en_translations = {
    "welcome": "Welcome",
    "goodbye": "Goodbye",
    "items": "{count} items"
  }
  
  let es_translations = {
    "welcome": "Bienvenido",
    "goodbye": "Adiós",
    "items": "{count} elementos"
  }
  
  FileManager::write_file("en.json", JsonSerializer::serialize(en_translations))
  FileManager::write_file("es.json", JsonSerializer::serialize(es_translations))
  
  // Load translations from files
  let en_load_result = Internationalization::load_translations_from_file(i18n, "en", "en.json")
  match en_load_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  let es_load_result = Internationalization::load_translations_from_file(i18n, "es", "es.json")
  match es_load_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // Test loaded translations
  Internationalization::set_language(i18n, "en")
  let en_welcome = Internationalization::translate(i18n, "welcome")
  match en_welcome {
    Some(message) => assert_eq(message, "Welcome")
    None => assert_true(false)
  }
  
  Internationalization::set_language(i18n, "es")
  let es_welcome = Internationalization::translate(i18n, "welcome")
  match es_welcome {
    Some(message) => assert_eq(message, "Bienvenido")
    None => assert_true(false)
  }
  
  // Clean up
  FileManager::delete_file("en.json")
  FileManager::delete_file("es.json")
}

// Test 9: Locale-Specific String Operations
test "locale-specific string operations" {
  let i18n = Internationalization::new()
  
  // Test with different locales
  Internationalization::set_language(i18n, "en")
  
  // Test case conversion
  assert_eq(Internationalization::to_upper(i18n, "hello"), "HELLO")
  assert_eq(Internationalization::to_lower(i18n, "WORLD"), "world")
  
  // Test with Turkish locale (has special case rules)
  Internationalization::set_language(i18n, "tr")
  
  // Turkish has special rules for 'i' and 'I'
  assert_eq(Internationalization::to_upper(i18n, "ı"), "I")  // Dotless i to I
  assert_eq(Internationalization::to_upper(i18n, "i"), "İ")  // Dotted i to İ
  assert_eq(Internationalization::to_lower(i18n, "I"), "ı")  // I to dotless i
  assert_eq(Internationalization::to_lower(i18n, "İ"), "i")  // İ to dotted i
  
  // Test string comparison with locale
  Internationalization::set_language(i18n, "en")
  assert_true(Internationalization::compare_strings(i18n, "apple", "banana") < 0)
  assert_true(Internationalization::compare_strings(i18n, "zebra", "apple") > 0)
  assert_eq(Internationalization::compare_strings(i18n, "hello", "hello"), 0)
  
  // Test with Swedish locale (different sorting rules)
  Internationalization::set_language(i18n, "sv")
  // In Swedish, 'å' comes after 'z'
  assert_true(Internationalization::compare_strings(i18n, "apple", "zebra") < 0)
  assert_true(Internationalization::compare_strings(i18n, "zebra", "åland") < 0)
}

// Test 10: Time Zone Handling
test "time zone handling" {
  let i18n = Internationalization::new()
  
  // Create a test datetime in UTC
  let utc_time = DateTime::new(2023, 6, 15, 14, 30, 0)
  
  // Test time zone conversions
  Internationalization::set_timezone(i18n, "UTC")
  let utc_formatted = Internationalization::format_datetime_with_timezone(i18n, utc_time)
  assert_eq(utc_formatted, "2023-06-15 14:30:00 UTC")
  
  Internationalization::set_timezone(i18n, "America/New_York")
  let ny_formatted = Internationalization::format_datetime_with_timezone(i18n, utc_time)
  assert_eq(ny_formatted, "2023-06-15 10:30:00 EDT")
  
  Internationalization::set_timezone(i18n, "Europe/London")
  let london_formatted = Internationalization::format_datetime_with_timezone(i18n, utc_time)
  assert_eq(london_formatted, "2023-06-15 15:30:00 BST")
  
  Internationalization::set_timezone(i18n, "Asia/Tokyo")
  let tokyo_formatted = Internationalization::format_datetime_with_timezone(i18n, utc_time)
  assert_eq(tokyo_formatted, "2023-06-15 23:30:00 JST")
  
  Internationalization::set_timezone(i18n, "Australia/Sydney")
  let sydney_formatted = Internationalization::format_datetime_with_timezone(i18n, utc_time)
  assert_eq(sydney_formatted, "2023-06-16 00:30:00 AEST")
  
  // Test relative time formatting
  Internationalization::set_language(i18n, "en")
  Internationalization::set_timezone(i18n, "UTC")
  
  let now = DateTime::now()
  let one_hour_ago = DateTime::subtract_hours(now, 1)
  let two_days_ago = DateTime::subtract_days(now, 2)
  
  let one_hour_ago_relative = Internationalization::format_relative_time(i18n, one_hour_ago)
  assert_eq(one_hour_ago_relative, "1 hour ago")
  
  let two_days_ago_relative = Internationalization::format_relative_time(i18n, two_days_ago)
  assert_eq(two_days_ago_relative, "2 days ago")
  
  // Test in Spanish
  Internationalization::set_language(i18n, "es")
  
  let one_hour_ago_relative_es = Internationalization::format_relative_time(i18n, one_hour_ago)
  assert_eq(one_hour_ago_relative_es, "hace 1 hora")
  
  let two_days_ago_relative_es = Internationalization::format_relative_time(i18n, two_days_ago)
  assert_eq(two_days_ago_relative_es, "hace 2 días")
}