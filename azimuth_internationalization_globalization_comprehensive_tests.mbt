// Azimuth Internationalization Globalization Comprehensive Tests
// This file contains comprehensive tests for internationalization and globalization features

// Test 1: Locale-Aware String Operations
test "locale-aware string operations" {
  // Create locale managers for different regions
  let us_locale = LocaleManager::new("en-US")
  let fr_locale = LocaleManager::new("fr-FR")
  let de_locale = LocaleManager::new("de-DE")
  let ja_locale = LocaleManager::new("ja-JP")
  let ar_locale = LocaleManager::new("ar-SA")
  
  // Test number formatting
  let number = 1234567.89
  
  let us_formatted = LocaleManager::format_number(us_locale, number)
  assert_eq(us_formatted, "1,234,567.89")
  
  let fr_formatted = LocaleManager::format_number(fr_locale, number)
  assert_eq(fr_formatted, "1 234 567,89") // Space as thousands separator, comma as decimal
  
  let de_formatted = LocaleManager::format_number(de_locale, number)
  assert_eq(de_formatted, "1.234.567,89") // Period as thousands separator, comma as decimal
  
  // Test currency formatting
  let currency = 1234.56
  
  let us_currency = LocaleManager::format_currency(us_locale, currency, "USD")
  assert_eq(us_currency, "$1,234.56")
  
  let fr_currency = LocaleManager::format_currency(fr_locale, currency, "EUR")
  assert_eq(fr_currency, "1 234,56 €")
  
  let ja_currency = LocaleManager::format_currency(ja_locale, currency, "JPY")
  assert_eq(ja_currency, "¥1,235") // Japanese Yen doesn't have decimal places
  
  // Test date formatting
  let timestamp = 1672531200000L // 2023-01-01 00:00:00 UTC
  
  let us_date = LocaleManager::format_date(us_locale, timestamp)
  assert_eq(us_date, "1/1/2023")
  
  let fr_date = LocaleManager::format_date(fr_locale, timestamp)
  assert_eq(fr_date, "01/01/2023")
  
  let de_date = LocaleManager::format_date(de_locale, timestamp)
  assert_eq(de_date, "1.1.2023")
  
  let ja_date = LocaleManager::format_date(ja_locale, timestamp)
  assert_eq(ja_date, "2023/01/01")
  
  // Test time formatting
  let time_timestamp = 1672534920000L // 2023-01-01 01:02:00 UTC
  
  let us_time = LocaleManager::format_time(us_locale, time_timestamp)
  assert_eq(us_time, "1:02:00 AM")
  
  let fr_time = LocaleManager::format_time(fr_locale, time_timestamp)
  assert_eq(fr_time, "01:02:00")
  
  let ja_time = LocaleManager::format_time(ja_locale, time_timestamp)
  assert_eq(ja_time, "1:02:00")
}

// Test 2: Text Direction and RTL/LTR Support
test "text direction and right-to-left support" {
  // Create text direction manager
  let text_direction = TextDirectionManager::new()
  
  // Test LTR text detection
  let ltr_text = "Hello World"
  assert_eq(TextDirectionManager::detect_direction(text_direction, ltr_text), TextDirection::LTR)
  
  // Test RTL text detection
  let rtl_text = "مرحبا بالعالم"
  assert_eq(TextDirectionManager::detect_direction(text_direction, rtl_text), TextDirection::RTL)
  
  // Test mixed text detection
  let mixed_text = "Hello مرحبا World"
  assert_eq(TextDirectionManager::detect_direction(text_direction, mixed_text), TextDirection::Mixed)
  
  // Test bidirectional text processing
  let bidi_text = "The number ١٢٣ is Arabic"
  let processed_bidi = TextDirectionManager::process_bidi(text_direction, bidi_text)
  assert_true(processed_bidi.contains("123")) // Arabic numerals should be displayed correctly
  
  // Test text alignment based on direction
  let ltr_aligned = TextDirectionManager::get_alignment(text_direction, TextDirection::LTR)
  assert_eq(ltr_aligned, TextAlign::Left)
  
  let rtl_aligned = TextDirectionManager::get_alignment(text_direction, TextDirection::RTL)
  assert_eq(rtl_aligned, TextAlign::Right)
  
  // Test margin/padding adjustments for RTL
  let ltr_margins = TextDirectionManager::adjust_margins(text_direction, 10, 20, TextDirection::LTR)
  assert_eq(ltr_margins.left, 10)
  assert_eq(ltr_margins.right, 20)
  
  let rtl_margins = TextDirectionManager::adjust_margins(text_direction, 10, 20, TextDirection::RTL)
  assert_eq(rtl_margins.left, 20) // Swapped for RTL
  assert_eq(rtl_margins.right, 10)
}

// Test 3: Multi-Language Resource Management
test "multi-language resource management" {
  // Create resource manager
  let resource_manager = ResourceManager::new()
  
  // Load resources for different languages
  ResourceManager::load_language(resource_manager, "en", [
    ("welcome.message", "Welcome to our application"),
    ("goodbye.message", "Thank you for using our application"),
    ("error.network", "Network connection failed"),
    ("button.save", "Save"),
    ("button.cancel", "Cancel")
  ])
  
  ResourceManager::load_language(resource_manager, "fr", [
    ("welcome.message", "Bienvenue dans notre application"),
    ("goodbye.message", "Merci d'utiliser notre application"),
    ("error.network", "La connexion réseau a échoué"),
    ("button.save", "Enregistrer"),
    ("button.cancel", "Annuler")
  ])
  
  ResourceManager::load_language(resource_manager, "ja", [
    ("welcome.message", "アプリケーションへようこそ"),
    ("goodbye.message", "アプリケーションをご利用いただきありがとうございます"),
    ("error.network", "ネットワーク接続が失敗しました"),
    ("button.save", "保存"),
    ("button.cancel", "キャンセル")
  ])
  
  // Test resource retrieval
  ResourceManager::set_language(resource_manager, "en")
  assert_eq(ResourceManager::get_string(resource_manager, "welcome.message"), "Welcome to our application")
  assert_eq(ResourceManager::get_string(resource_manager, "button.save"), "Save")
  
  // Test language switching
  ResourceManager::set_language(resource_manager, "fr")
  assert_eq(ResourceManager::get_string(resource_manager, "welcome.message"), "Bienvenue dans notre application")
  assert_eq(ResourceManager::get_string(resource_manager, "button.save"), "Enregistrer")
  
  ResourceManager::set_language(resource_manager, "ja")
  assert_eq(ResourceManager::get_string(resource_manager, "welcome.message"), "アプリケーションへようこそ")
  assert_eq(ResourceManager::get_string(resource_manager, "button.save"), "保存")
  
  // Test fallback to default language
  ResourceManager::set_language(resource_manager, "de") // German not loaded
  assert_eq(ResourceManager::get_string(resource_manager, "welcome.message"), "Welcome to our application") // Falls back to English
  
  // Test parameterized strings
  ResourceManager::load_language(resource_manager, "en", [
    ("user.greeting", "Hello, {name}! You have {count} new messages.")
  ])
  
  ResourceManager::load_language(resource_manager, "fr", [
    ("user.greeting", "Bonjour, {name}! Vous avez {count} nouveaux messages.")
  ])
  
  ResourceManager::set_language(resource_manager, "en")
  let en_greeting = ResourceManager::get_formatted_string(resource_manager, "user.greeting", [
    ("name", "John"),
    ("count", "5")
  ])
  assert_eq(en_greeting, "Hello, John! You have 5 new messages.")
  
  ResourceManager::set_language(resource_manager, "fr")
  let fr_greeting = ResourceManager::get_formatted_string(resource_manager, "user.greeting", [
    ("name", "Jean"),
    ("count", "3")
  ])
  assert_eq(fr_greeting, "Bonjour, Jean! Vous avez 3 nouveaux messages.")
}

// Test 4: Cultural Formatting and Conventions
test "cultural formatting and conventions" {
  // Create cultural formatter
  let formatter = CulturalFormatter::new()
  
  // Test name formatting
  let western_name = PersonName::new("John", "Doe")
  let western_formatted = formatter::format_name(western_name, NamingConvention::Western)
  assert_eq(western_formatted, "John Doe")
  
  let eastern_name = PersonName::new("Tanaka", "Yuki")
  let eastern_formatted = formatter::format_name(eastern_name, NamingConvention::Eastern)
  assert_eq(eastern_formatted, "Tanaka Yuki")
  
  // Test address formatting
  let us_address = Address::new(
    "123 Main St",
    "New York",
    "NY",
    "10001",
    "USA"
  )
  
  let us_formatted_address = formatter::format_address(us_address, AddressConvention::US)
  assert_eq(us_formatted_address, "123 Main St\nNew York, NY 10001\nUSA")
  
  let jp_address = Address::new(
    "〒100-0001",
    "千代田区千代田",
    "1-1",
    "東京都",
    "日本"
  )
  
  let jp_formatted_address = formatter::format_address(jp_address, AddressConvention::Japan)
  assert_eq(jp_formatted_address, "〒100-0001\n東京都千代田区千代田1-1\n日本")
  
  // Test phone number formatting
  let us_phone = PhoneNumber::new("1", "2125551234")
  let us_formatted_phone = formatter::format_phone_number(us_phone, PhoneConvention::US)
  assert_eq(us_formatted_phone, "(212) 555-1234")
  
  let fr_phone = PhoneNumber::new("33", "1234567890")
  let fr_formatted_phone = formatter::format_phone_number(fr_phone, PhoneConvention::France)
  assert_eq(fr_formatted_phone, "01 23 45 67 89")
  
  let jp_phone = PhoneNumber::new("81", "9012345678")
  let jp_formatted_phone = formatter::format_phone_number(jp_phone, PhoneConvention::Japan)
  assert_eq(jp_formatted_phone, "090-1234-5678")
  
  // Test measurement units
  let weight_kg = 70.5
  let us_weight = formatter::convert_weight(weight_kg, WeightUnit::Kilogram, WeightUnit::Pound)
  assert_true(Math::abs(us_weight - 155.4) < 0.1)
  
  let length_m = 1.8
  let us_length = formatter::convert_length(length_m, LengthUnit::Meter, LengthUnit::FootInch)
  assert_true(us_length.contains("5'11\""))
  
  let temperature_c = 25.0
  let us_temperature = formatter::convert_temperature(temperature_c, TemperatureUnit::Celsius, TemperatureUnit::Fahrenheit)
  assert_true(Math::abs(us_temperature - 77.0) < 0.1)
}

// Test 5: Collation and String Comparison
test "collation and string comparison" {
  // Create collation manager
  let collation = CollationManager::new()
  
  // Test different collation strengths
  let strings = ["apple", "Apple", "äpple", "Banana", "banana"]
  
  // Primary collation (base letters only, case and accents ignored)
  let primary_sorted = collation::sort(strings, CollationStrength::Primary)
  assert_eq(primary_sorted, ["apple", "Apple", "äpple", "Banana", "banana"])
  
  // Secondary collation (base letters + accents, case ignored)
  let secondary_sorted = collation::sort(strings, CollationStrength::Secondary)
  assert_eq(secondary_sorted, ["apple", "Apple", "äpple", "Banana", "banana"])
  
  // Tertiary collation (base letters + accents + case)
  let tertiary_sorted = collation::sort(strings, CollationStrength::Tertiary)
  assert_eq(tertiary_sorted, ["Apple", "apple", "äpple", "Banana", "banana"])
  
  // Test locale-specific collation
  let us_collator = collation::create_collator("en-US")
  let de_collator = collation::create_collator("de-DE")
  let sv_collator = collation::create_collator("sv-SE")
  
  let german_strings = ["äpfel", "apfel", "öffnen", "ofen"]
  
  // German collation treats ä like ae
  let de_sorted = collation::sort_with_collator(german_strings, de_collator)
  assert_eq(de_sorted, ["apfel", "äpfel", "ofen", "öffnen"])
  
  // Swedish collation treats ä and ö as separate letters at end of alphabet
  let sv_sorted = collation::sort_with_collator(german_strings, sv_collator)
  assert_eq(sv_sorted, ["apfel", "ofen", "öffnen", "äpfel"])
  
  // Test string searching with collation
  let text = "Café naïve résumé"
  let search_term = "naive"
  
  let found = collation::contains(text, search_term, CollationStrength::Primary)
  assert_true(found) // Should find "naïve" when searching for "naive"
  
  // Test diacritic handling
  let diacritic_strings = ["resume", "résumé", "Resume", "Résumé"]
  let diacritic_sorted = collation::sort(diacritic_strings, CollationStrength::Primary)
  assert_eq(diacritic_sorted, ["resume", "résumé", "Resume", "Résumé"])
}

// Test 6: Calendar and Time Zone Support
test "calendar and time zone support" {
  // Create calendar manager
  let calendar_manager = CalendarManager::new()
  
  // Test different calendar systems
  let gregorian_date = DateTime::new(2023, 1, 1, 12, 0, 0)
  
  // Convert to Islamic calendar
  let islamic_date = calendar_manager::convert_to_calendar(gregorian_date, CalendarSystem::Islamic)
  assert_eq(islamic_date.year, 1444)
  assert_eq(islamic_date.month, 6)
  assert_eq(islamic_date.day, 9)
  
  // Convert to Hebrew calendar
  let hebrew_date = calendar_manager::convert_to_calendar(gregorian_date, CalendarSystem::Hebrew)
  assert_eq(hebrew_date.year, 5783)
  assert_eq(hebrew_date.month, 10)
  assert_eq(hebrew_date.day, 8)
  
  // Convert to Chinese calendar
  let chinese_date = calendar_manager::convert_to_calendar(gregorian_date, CalendarSystem::Chinese)
  assert_eq(chinese_date.year, 4721)
  assert_eq(chinese_date.month, 12)
  assert_eq(chinese_date.day, 10)
  
  // Test time zone conversions
  let utc_time = DateTime::new(2023, 1, 1, 12, 0, 0)
  
  let est_time = calendar_manager::convert_timezone(utc_time, "UTC", "America/New_York")
  assert_eq(est_time.hour, 7) // UTC-5 in winter
  assert_eq(est_time.day, 1)
  
  let jst_time = calendar_manager::convert_timezone(utc_time, "UTC", "Asia/Tokyo")
  assert_eq(jst_time.hour, 21) // UTC+9
  assert_eq(jst_time.day, 1)
  
  // Test daylight saving time
  let summer_utc = DateTime::new(2023, 7, 1, 12, 0, 0)
  let summer_est = calendar_manager::convert_timezone(summer_utc, "UTC", "America/New_York")
  assert_eq(summer_est.hour, 8) // UTC-4 in summer (DST)
  
  // Test week start day
  let us_week_start = calendar_manager::get_week_start_day("en-US")
  assert_eq(us_week_start, WeekDay::Sunday)
  
  let uk_week_start = calendar_manager::get_week_start_day("en-GB")
  assert_eq(uk_week_start, WeekDay::Monday)
  
  let islamic_week_start = calendar_manager::get_week_start_day("ar-SA")
  assert_eq(islamic_week_start, WeekDay::Saturday)
  
  // Test working days
  let us_working_days = calendar_manager::get_working_days("en-US")
  assert_eq(us_working_days, [WeekDay::Monday, WeekDay::Tuesday, WeekDay::Wednesday, WeekDay::Thursday, WeekDay::Friday])
  
  let islamic_working_days = calendar_manager::get_working_days("ar-SA")
  assert_eq(islamic_working_days, [WeekDay::Sunday, WeekDay::Monday, WeekDay::Tuesday, WeekDay::Wednesday, WeekDay::Thursday])
}

// Test 7: Pluralization and Gender Agreement
test "pluralization and gender agreement" {
  // Create grammar manager
  let grammar_manager = GrammarManager::new()
  
  // Test pluralization rules for different languages
  grammar_manager::load_plural_rules("en", [
    (1, "item"),
    (2, "items")
  ])
  
  grammar_manager::load_plural_rules("fr", [
    (0, "élément"),
    (1, "élément"),
    (2, "éléments")
  ])
  
  grammar_manager::load_plural_rules("ar", [
    (0, "عنصر"),
    (1, "عنصر"),
    (2, "عنصران"),
    (3, "عناصر"),
    (11, "عنصرًا")
  ])
  
  // Test English pluralization
  grammar_manager::set_language("en")
  assert_eq(grammar_manager::pluralize("item", 1), "item")
  assert_eq(grammar_manager::pluralize("item", 2), "items")
  assert_eq(grammar_manager::pluralize("item", 0), "items") // English treats 0 as plural
  
  // Test French pluralization
  grammar_manager::set_language("fr")
  assert_eq(grammar_manager::pluralize("élément", 0), "élément")
  assert_eq(grammar_manager::pluralize("élément", 1), "élément")
  assert_eq(grammar_manager::pluralize("élément", 2), "éléments")
  
  // Test Arabic pluralization
  grammar_manager::set_language("ar")
  assert_eq(grammar_manager::pluralize("عنصر", 0), "عنصر")
  assert_eq(grammar_manager::pluralize("عنصر", 1), "عنصر")
  assert_eq(grammar_manager::pluralize("عنصر", 2), "عنصران")
  assert_eq(grammar_manager::pluralize("عنصر", 3), "عناصر")
  assert_eq(grammar_manager::pluralize("عنصر", 11), "عنصرًا")
  
  // Test gender agreement
  grammar_manager::load_gender_rules("fr", [
    ("book", Gender::Masculine),
    ("table", Gender::Feminine)
  ])
  
  grammar_manager::load_gender_rules("de", [
    ("book", Gender::Masculine),
    ("table", Gender::Feminine),
    ("child", Gender::Neuter)
  ])
  
  grammar_manager::set_language("fr")
  assert_eq(grammar_manager::get_article("book", ArticleType::Definite), "le")
  assert_eq(grammar_manager::get_article("table", ArticleType::Definite), "la")
  assert_eq(grammar_manager::get_article("book", ArticleType::Indefinite), "un")
  assert_eq(grammar_manager::get_article("table", ArticleType::Indefinite), "une")
  
  grammar_manager::set_language("de")
  assert_eq(grammar_manager::get_article("book", ArticleType::Definite), "der")
  assert_eq(grammar_manager::get_article("table", ArticleType::Definite), "die")
  assert_eq(grammar_manager::get_article("child", ArticleType::Definite), "das")
  
  // Test adjective agreement
  grammar_manager::load_adjective_rules("fr", [
    ("beau", Gender::Masculine, "beau"),
    ("beau", Gender::Feminine, "belle")
  ])
  
  grammar_manager::set_language("fr")
  assert_eq(grammar_manager::agree_adjective("beau", "book"), "beau")
  assert_eq(grammar_manager::agree_adjective("beau", "table"), "belle")
}

// Test 8: Input Method and Character Set Support
test "input method and character set support" {
  // Create input method manager
  let input_manager = InputMethodManager::new()
  
  // Test character set detection
  let latin_text = "Hello World"
  let latin_charset = input_manager::detect_charset(latin_text)
  assert_eq(latin_charset, CharacterSet::Latin)
  
  let cyrillic_text = "Привет мир"
  let cyrillic_charset = input_manager::detect_charset(cyrillic_text)
  assert_eq(cyrillic_charset, CharacterSet::Cyrillic)
  
  let arabic_text = "مرحبا بالعالم"
  let arabic_charset = input_manager::detect_charset(arabic_text)
  assert_eq(arabic_charset, CharacterSet::Arabic)
  
  let chinese_text = "你好世界"
  let chinese_charset = input_manager::detect_charset(chinese_text)
  assert_eq(chinese_charset, CharacterSet::Chinese)
  
  let japanese_text = "こんにちは世界"
  let japanese_charset = input_manager::detect_charset(japanese_text)
  assert_eq(japanese_charset, CharacterSet::Japanese)
  
  let mixed_text = "Hello こんにちは Привет مرحبا"
  let mixed_charset = input_manager::detect_charset(mixed_text)
  assert_eq(mixed_charset, CharacterSet::Mixed)
  
  // Test input method prediction
  let japanese_input = "konnichiha"
  let japanese_predictions = input_manager::get_predictions(japanese_input, InputMethod::Japanese)
  assert_true(japanese_predictions.contains("こんにちは"))
  assert_true(japanese_predictions.contains("こんばんは"))
  
  let chinese_input = "ni hao"
  let chinese_predictions = input_manager::get_predictions(chinese_input, InputMethod::Chinese)
  assert_true(chinese_predictions.contains("你好"))
  assert_true(chinese_predictions.contains("您好"))
  
  let arabic_input = "ahlan"
  let arabic_predictions = input_manager::get_predictions(arabic_input, InputMethod::Arabic)
  assert_true(arabic_predictions.contains("أهلا"))
  assert_true(arabic_predictions.contains("أهلاً"))
  
  // Test character composition
  let base_char = "a"
  let combining_accent = "\u0301" // Combining acute accent
  let composed_char = input_manager::compose_characters([base_char, combining_accent])
  assert_eq(composed_char, "á")
  
  // Test character decomposition
  let composed_text = "café"
  let decomposed = input_manager::decompose_text(composed_text)
  assert_true(decomposed.contains("e") && decomposed.contains("\u0301"))
  
  // Test text normalization
  let unnormalized_text = "cafe\u0301" // 'e' + combining acute accent
  let normalized_text = input_manager::normalize_text(unnormalized_text, NormalizationForm::NFC)
  assert_eq(normalized_text, "café") // Should be normalized to composed form
  
  // Test bidirectional input handling
  let bidi_input = "Hello مرحبا"
  let logical_order = input_manager::get_logical_order(bidi_input)
  let visual_order = input_manager::get_visual_order(bidi_input)
  
  assert_eq(logical_order, "Hello مرحبا")
  assert_true(visual_order.contains("مرحبا") && visual_order.contains("Hello"))
  
  // Test right-to-left input method
  input_manager::set_input_direction(InputDirection::RTL)
  let rtl_input_sequence = input_manager::process_input_sequence("ABC")
  assert_eq(rtl_input_sequence, "CBA") // Characters should be reversed for RTL input
}