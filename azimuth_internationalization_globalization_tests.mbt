// Azimuth Telemetry System - Internationalization and Globalization Tests
// This file contains comprehensive test cases for internationalization and globalization features

// Test 1: Locale Management
test "locale management" {
  // Test locale creation and validation
  let en_us_locale = Locale::new("en-US")
  assert_eq(Locale::get_language(en_us_locale), "en")
  assert_eq(Locale::get_country(en_us_locale), "US")
  assert_eq(Locale::to_string(en_us_locale), "en-US")
  
  let zh_cn_locale = Locale::new("zh-CN")
  assert_eq(Locale::get_language(zh_cn_locale), "zh")
  assert_eq(Locale::get_country(zh_cn_locale), "CN")
  assert_eq(Locale::to_string(zh_cn_locale), "zh-CN")
  
  let fr_locale = Locale::new("fr")
  assert_eq(Locale::get_language(fr_locale), "fr")
  assert_eq(Locale::get_country(fr_locale), "")
  assert_eq(Locale::to_string(fr_locale), "fr")
  
  // Test locale validation
  assert_true(Locale::is_valid("en-US"))
  assert_true(Locale::is_valid("zh-CN"))
  assert_true(Locale::is_valid("fr"))
  assert_false(Locale::is_valid("invalid-locale"))
  assert_false(Locale::is_valid(""))
  
  // Test locale matching
  let user_locales = [Locale::new("en-US"), Locale::new("fr"), Locale::new("de-DE")]
  let supported_locales = [Locale::new("en"), Locale::new("fr-FR"), Locale::new("zh-CN")]
  
  let matched_locale = Locale::find_best_match(user_locales, supported_locales)
  match matched_locale {
    Some(locale) => assert_eq(Locale::get_language(locale), "fr") // fr matches fr-FR
    None => assert_true(false)
  }
  
  // Test locale fallback chain
  let fallback_chain = Locale::get_fallback_chain(Locale::new("de-DE"))
  assert_eq(fallback_chain.length(), 3)
  assert_eq(Locale::to_string(fallback_chain[0]), "de-DE")
  assert_eq(Locale::to_string(fallback_chain[1]), "de")
  assert_eq(Locale::to_string(fallback_chain[2]), "root") // Root locale
  
  // Test locale manager
  let locale_manager = LocaleManager::new()
  
  LocaleManager::add_locale(locale_manager, en_us_locale)
  LocaleManager::add_locale(locale_manager, zh_cn_locale)
  LocaleManager::add_locale(locale_manager, fr_locale)
  
  assert_eq(LocaleManager::get_supported_locales(locale_manager).length(), 3)
  assert_true(LocaleManager::is_supported(locale_manager, "en-US"))
  assert_true(LocaleManager::is_supported(locale_manager, "zh-CN"))
  assert_false(LocaleManager::is_supported(locale_manager, "ja-JP"))
  
  // Test default locale
  LocaleManager::set_default_locale(locale_manager, en_us_locale)
  assert_eq(Locale::to_string(LocaleManager::get_default_locale(locale_manager)), "en-US")
}

// Test 2: Resource Bundle Management
test "resource bundle management" {
  // Test resource bundle creation
  let resource_manager = ResourceBundleManager::new()
  
  // Test adding resources for different locales
  let en_messages = [
    ("telemetry.welcome", "Welcome to Azimuth Telemetry"),
    ("telemetry.error", "An error occurred"),
    ("telemetry.success", "Operation completed successfully"),
    ("telemetry.items.count", "{0} items found")
  ]
  
  let zh_messages = [
    ("telemetry.welcome", "欢迎使用 Azimuth 遥测系统"),
    ("telemetry.error", "发生错误"),
    ("telemetry.success", "操作成功完成"),
    ("telemetry.items.count", "找到 {0} 个项目")
  ]
  
  let fr_messages = [
    ("telemetry.welcome", "Bienvenue dans Azimuth Télémétrie"),
    ("telemetry.error", "Une erreur s'est produite"),
    ("telemetry.success", "Opération terminée avec succès"),
    ("telemetry.items.count", "{0} éléments trouvés")
  ]
  
  ResourceBundleManager::add_bundle(resource_manager, Locale::new("en-US"), en_messages)
  ResourceBundleManager::add_bundle(resource_manager, Locale::new("zh-CN"), zh_messages)
  ResourceBundleManager::add_bundle(resource_manager, Locale::new("fr"), fr_messages)
  
  // Test message retrieval
  let en_welcome = ResourceBundleManager::get_message(resource_manager, Locale::new("en-US"), "telemetry.welcome")
  match en_welcome {
    Some(message) => assert_eq(message, "Welcome to Azimuth Telemetry")
    None => assert_true(false)
  }
  
  let zh_welcome = ResourceBundleManager::get_message(resource_manager, Locale::new("zh-CN"), "telemetry.welcome")
  match zh_welcome {
    Some(message) => assert_eq(message, "欢迎使用 Azimuth 遥测系统")
    None => assert_true(false)
  }
  
  let fr_welcome = ResourceBundleManager::get_message(resource_manager, Locale::new("fr"), "telemetry.welcome")
  match fr_welcome {
    Some(message) => assert_eq(message, "Bienvenue dans Azimuth Télémétrie")
    None => assert_true(false)
  }
  
  // Test message fallback
  let en_gb_welcome = ResourceBundleManager::get_message(resource_manager, Locale::new("en-GB"), "telemetry.welcome")
  match en_gb_welcome {
    Some(message) => assert_eq(message, "Welcome to Azimuth Telemetry") // Fallback to en-US
    None => assert_true(false)
  }
  
  // Test parameterized messages
  let en_count = ResourceBundleManager::format_message(resource_manager, Locale::new("en-US"), "telemetry.items.count", ["5"])
  match en_count {
    Some(message) => assert_eq(message, "5 items found")
    None => assert_true(false)
  }
  
  let zh_count = ResourceBundleManager::format_message(resource_manager, Locale::new("zh-CN"), "telemetry.items.count", ["5"])
  match zh_count {
    Some(message) => assert_eq(message, "找到 5 个项目")
    None => assert_true(false)
  }
  
  // Test missing message handling
  let missing_message = ResourceBundleManager::get_message(resource_manager, Locale::new("en-US"), "telemetry.nonexistent")
  match missing_message {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test bundle statistics
  let bundle_stats = ResourceBundleManager::get_statistics(resource_manager)
  assert_eq(BundleStatistics::get_total_locales(bundle_stats), 3)
  assert_eq(BundleStatistics::get_total_messages(bundle_stats), 12) // 4 messages × 3 locales
}

// Test 3: Number and Date Formatting
test "number and date formatting" {
  // Test number formatting
  let number_formatter = NumberFormatter::new()
  
  // Test formatting in different locales
  let en_number = NumberFormatter::format(number_formatter, 1234567.89, Locale::new("en-US"))
  assert_eq(en_number, "1,234,567.89") // US format: comma as thousands separator, dot as decimal
  
  let fr_number = NumberFormatter::format(number_formatter, 1234567.89, Locale::new("fr"))
  assert_eq(fr_number, "1\u{00A0}234\u{00A0}567,89") // French format: space as thousands separator, comma as decimal
  
  let de_number = NumberFormatter::format(number_formatter, 1234567.89, Locale::new("de-DE"))
  assert_eq(de_number, "1.234.567,89") // German format: dot as thousands separator, comma as decimal
  
  // Test percentage formatting
  let en_percentage = NumberFormatter::format_percentage(number_formatter, 0.75, Locale::new("en-US"))
  assert_eq(en_percentage, "75%")
  
  let fr_percentage = NumberFormatter::format_percentage(number_formatter, 0.75, Locale::new("fr"))
  assert_eq(fr_percentage, "75\u{00A0}%") // French uses non-breaking space before %
  
  // Test currency formatting
  let en_currency = NumberFormatter::format_currency(number_formatter, 1234.56, "USD", Locale::new("en-US"))
  assert_eq(en_currency, "$1,234.56")
  
  let fr_currency = NumberFormatter::format_currency(number_formatter, 1234.56, "EUR", Locale::new("fr"))
  assert_eq(fr_currency, "1\u{00A0}234,56\u{00A0}€")
  
  let jp_currency = NumberFormatter::format_currency(number_formatter, 1234.56, "JPY", Locale::new("ja-JP"))
  assert_eq(jp_currency, "¥1,235") // Japanese Yen has no decimal places
  
  // Test date formatting
  let date_formatter = DateFormatter::new()
  let test_date = DateTime::new(2023, 1, 15, 14, 30, 45)
  
  // Test date formatting in different locales
  let en_date = DateFormatter::format(date_formatter, test_date, "medium", Locale::new("en-US"))
  assert_eq(en_date, "Jan 15, 2023")
  
  let fr_date = DateFormatter::format(date_formatter, test_date, "medium", Locale::new("fr"))
  assert_eq(fr_date, "15 janv. 2023")
  
  let zh_date = DateFormatter::format(date_formatter, test_date, "medium", Locale::new("zh-CN"))
  assert_eq(zh_date, "2023年1月15日")
  
  // Test date-time formatting
  let en_datetime = DateFormatter::format_datetime(date_formatter, test_date, "short", Locale::new("en-US"))
  assert_eq(en_datetime, "1/15/23, 2:30 PM")
  
  let fr_datetime = DateFormatter::format_datetime(date_formatter, test_date, "short", Locale::new("fr"))
  assert_eq(fr_datetime, "15/01/2023 14:30")
  
  // Test relative time formatting
  let now = DateTime::now()
  let past_date = DateTime::subtract_days(now, 2) // 2 days ago
  
  let en_relative = DateFormatter::format_relative(date_formatter, past_date, Locale::new("en-US"))
  assert_eq(en_relative, "2 days ago")
  
  let fr_relative = DateFormatter::format_relative(date_formatter, past_date, Locale::new("fr"))
  assert_eq(fr_relative, "il y a 2 jours")
  
  let zh_relative = DateFormatter::format_relative(date_formatter, past_date, Locale::new("zh-CN"))
  assert_eq(zh_relative, "2天前")
}

// Test 4: Text Direction and RTL Support
test "text direction and rtl support" {
  // Test text direction detection
  assert_eq(TextDirection::detect("Hello World"), LeftToRight)
  assert_eq(TextDirection::detect("مرحبا بالعالم"), RightToLeft)
  assert_eq(TextDirection::detect("こんにちは世界"), LeftToRight) // Japanese is LTR
  assert_eq(TextDirection::detect("Hello مرحبا World"), Mixed) // Mixed content
  
  // Test bidirectional text processing
  let bidi_processor = BidiProcessor::new()
  
  let mixed_text = "Hello مرحبا World"
  let processed_text = BidiProcessor::process(bidi_processor, mixed_text)
  assert_true(processed_text.length() > 0)
  
  // Test layout direction for different locales
  assert_eq(Locale::get_text_direction(Locale::new("en-US")), LeftToRight)
  assert_eq(Locale::get_text_direction(Locale::new("fr")), LeftToRight)
  assert_eq(Locale::get_text_direction(Locale::new("zh-CN")), LeftToRight)
  assert_eq(Locale::get_text_direction(Locale::new("ar")), RightToLeft)
  assert_eq(Locale::get_text_direction(Locale::new("he")), RightToLeft)
  
  // Test UI layout adaptation
  let ui_layout = UILayout::new()
  
  UILayout::set_direction(ui_layout, LeftToRight)
  assert_eq(UILayout::get_direction(ui_layout), LeftToRight)
  
  UILayout::set_direction(ui_layout, RightToLeft)
  assert_eq(UILayout::get_direction(ui_layout), RightToLeft)
  
  // Test layout mirroring
  let original_layout = UILayout::create_element(ui_layout, "sidebar", "left", 200)
  let mirrored_layout = UILayout::mirror_for_rtl(ui_layout, original_layout)
  
  assert_eq(UILayoutElement::get_position(mirrored_layout), "right") // Left becomes right in RTL
  assert_eq(UILayoutElement::get_width(mirrored_layout), 200)
  
  // Test text alignment for different locales
  assert_eq(Locale::get_default_text_alignment(Locale::new("en-US")), Left)
  assert_eq(Locale::get_default_text_alignment(Locale::new("fr")), Left)
  assert_eq(Locale::get_default_text_alignment(Locale::new("zh-CN")), Left)
  assert_eq(Locale::get_default_text_alignment(Locale::new("ar")), Right)
  assert_eq(Locale::get_default_text_alignment(Locale::new("he")), Right)
}

// Test 5: Character Encoding and Unicode
test "character encoding and unicode" {
  // Test UTF-8 encoding and decoding
  let utf8_encoder = UTF8Encoder::new()
  
  // Test encoding different character sets
  let latin_text = "Hello, World!"
  let latin_encoded = UTF8Encoder::encode(utf8_encoder, latin_text)
  let latin_decoded = UTF8Encoder::decode(utf8_encoder, latin_encoded)
  assert_eq(latin_decoded, latin_text)
  
  let chinese_text = "你好，世界！"
  let chinese_encoded = UTF8Encoder::encode(utf8_encoder, chinese_text)
  let chinese_decoded = UTF8Encoder::decode(utf8_encoder, chinese_encoded)
  assert_eq(chinese_decoded, chinese_text)
  
  let arabic_text = "مرحبا بالعالم!"
  let arabic_encoded = UTF8Encoder::encode(utf8_encoder, arabic_text)
  let arabic_decoded = UTF8Encoder::decode(utf8_encoder, arabic_encoded)
  assert_eq(arabic_decoded, arabic_text)
  
  let emoji_text = "Hello 👋 World 🌍"
  let emoji_encoded = UTF8Encoder::encode(utf8_encoder, emoji_text)
  let emoji_decoded = UTF8Encoder::decode(utf8_encoder, emoji_encoded)
  assert_eq(emoji_decoded, emoji_text)
  
  // Test character set detection
  let charset_detector = CharsetDetector::new()
  
  let detected_charset = CharsetDetector::detect(charset_detector, latin_encoded)
  assert_eq(detected_charset, "UTF-8")
  
  // Test Unicode normalization
  let unicode_normalizer = UnicodeNormalizer::new()
  
  // Test composed vs decomposed forms
  let composed = "é" // Single code point
  let decomposed = "é" // 'e' + combining acute accent
  
  let normalized_composed = UnicodeNormalizer::normalize(unicode_normalizer, decomposed, "NFC")
  assert_eq(normalized_composed, composed) // Should normalize to composed form
  
  let normalized_decomposed = UnicodeNormalizer::normalize(unicode_normalizer, composed, "NFD")
  assert_eq(normalized_decomposed, decomposed) // Should normalize to decomposed form
  
  // Test case conversion with locale awareness
  let case_converter = CaseConverter::new()
  
  let turkish_i = "i"
  let turkish_upper = CaseConverter::to_upper(case_converter, turkish_i, Locale::new("tr"))
  assert_eq(turkish_upper, "İ") // Turkish dotted uppercase I
  
  let english_i = "i"
  let english_upper = CaseConverter::to_upper(case_converter, english_i, Locale::new("en"))
  assert_eq(english_upper, "I") // English uppercase I
  
  // Test string collation (sorting) for different locales
  let collator = Collator::new()
  
  let strings = ["apple", "banana", "cherry", "date"]
  
  // Test English sorting
  let en_sorted = Collator::sort(collator, strings, Locale::new("en"))
  assert_eq(en_sorted, ["apple", "banana", "cherry", "date"])
  
  // Test Swedish sorting (å, ä, ö at end)
  let swedish_strings = ["apple", "zebra", "ångström", "banana"]
  let sv_sorted = Collator::sort(collator, swedish_strings, Locale::new("sv"))
  assert_eq(sv_sorted, ["apple", "banana", "zebra", "ångström"])
  
  // Test German sorting (ä treated as ae)
  let german_strings = ["apple", "äpfel", "banana"]
  let de_sorted = Collator::sort(collator, german_strings, Locale::new("de"))
  assert_eq(de_sorted, ["äpfel", "apple", "banana"]) // äpfel comes before apple in German
}

// Test 6: Pluralization and Gender Rules
test "pluralization and gender rules" {
  // Test pluralization rules
  let pluralizer = Pluralizer::new()
  
  // Test English pluralization (one, other)
  let en_one = Pluralizer::get_plural_form(pluralizer, 1, Locale::new("en"))
  assert_eq(en_one, "one")
  
  let en_other = Pluralizer::get_plural_form(pluralizer, 5, Locale::new("en"))
  assert_eq(en_other, "other")
  
  let en_zero = Pluralizer::get_plural_form(pluralizer, 0, Locale::new("en"))
  assert_eq(en_zero, "other") // Zero uses "other" form in English
  
  // Test Arabic pluralization (zero, one, two, few, many, other)
  let ar_zero = Pluralizer::get_plural_form(pluralizer, 0, Locale::new("ar"))
  assert_eq(ar_zero, "zero")
  
  let ar_one = Pluralizer::get_plural_form(pluralizer, 1, Locale::new("ar"))
  assert_eq(ar_one, "one")
  
  let ar_two = Pluralizer::get_plural_form(pluralizer, 2, Locale::new("ar"))
  assert_eq(ar_two, "two")
  
  let ar_few = Pluralizer::get_plural_form(pluralizer, 5, Locale::new("ar"))
  assert_eq(ar_few, "few")
  
  let ar_many = Pluralizer::get_plural_form(pluralizer, 15, Locale::new("ar"))
  assert_eq(ar_many, "many")
  
  let ar_other = Pluralizer::get_plural_form(pluralizer, 101, Locale::new("ar"))
  assert_eq(ar_other, "other")
  
  // Test Chinese (no pluralization)
  let zh_zero = Pluralizer::get_plural_form(pluralizer, 0, Locale::new("zh"))
  assert_eq(zh_zero, "other")
  
  let zh_one = Pluralizer::get_plural_form(pluralizer, 1, Locale::new("zh"))
  assert_eq(zh_one, "other")
  
  let zh_many = Pluralizer::get_plural_form(pluralizer, 100, Locale::new("zh"))
  assert_eq(zh_many, "other")
  
  // Test plural message formatting
  let resource_manager = ResourceBundleManager::new()
  
  let en_plural_messages = [
    ("items.count.one", "{0} item"),
    ("items.count.other", "{0} items")
  ]
  
  let ar_plural_messages = [
    ("items.count.zero", "لا توجد عناصر"),
    ("items.count.one", "عنصر واحد"),
    ("items.count.two", "عنصران"),
    ("items.count.few", "{0} عناصر"),
    ("items.count.many", "{0} عنصرًا"),
    ("items.count.other", "{0} عنصر")
  ]
  
  ResourceBundleManager::add_bundle(resource_manager, Locale::new("en"), en_plural_messages)
  ResourceBundleManager::add_bundle(resource_manager, Locale::new("ar"), ar_plural_messages)
  
  // Test English plural formatting
  let en_one_item = ResourceBundleManager::format_plural(resource_manager, Locale::new("en"), "items.count", 1)
  match en_one_item {
    Some(message) => assert_eq(message, "1 item")
    None => assert_true(false)
  }
  
  let en_five_items = ResourceBundleManager::format_plural(resource_manager, Locale::new("en"), "items.count", 5)
  match en_five_items {
    Some(message) => assert_eq(message, "5 items")
    None => assert_true(false)
  }
  
  // Test Arabic plural formatting
  let ar_zero_items = ResourceBundleManager::format_plural(resource_manager, Locale::new("ar"), "items.count", 0)
  match ar_zero_items {
    Some(message) => assert_eq(message, "لا توجد عناصر")
    None => assert_true(false)
  }
  
  let ar_one_item = ResourceBundleManager::format_plural(resource_manager, Locale::new("ar"), "items.count", 1)
  match ar_one_item {
    Some(message) => assert_eq(message, "عنصر واحد")
    None => assert_true(false)
  }
  
  let ar_five_items = ResourceBundleManager::format_plural(resource_manager, Locale::new("ar"), "items.count", 5)
  match ar_five_items {
    Some(message) => assert_eq(message, "5 عناصر")
    None => assert_true(false)
  }
  
  // Test gender-aware formatting
  let gender_formatter = GenderFormatter::new()
  
  let gender_messages = [
    ("user.male", "He is online"),
    ("user.female", "She is online"),
    ("user.other", "They are online")
  ]
  
  ResourceBundleManager::add_bundle(resource_manager, Locale::new("en"), gender_messages)
  
  let male_message = GenderFormatter::format(gender_formatter, resource_manager, Locale::new("en"), "user", "male")
  match male_message {
    Some(message) => assert_eq(message, "He is online")
    None => assert_true(false)
  }
  
  let female_message = GenderFormatter::format(gender_formatter, resource_manager, Locale::new("en"), "user", "female")
  match female_message {
    Some(message) => assert_eq(message, "She is online")
    None => assert_true(false)
  }
  
  let neutral_message = GenderFormatter::format(gender_formatter, resource_manager, Locale::new("en"), "user", "other")
  match neutral_message {
    Some(message) => assert_eq(message, "They are online")
    None => assert_true(false)
  }
}

// Test 7: Time Zone and Calendar Support
test "time zone and calendar support" {
  // Test time zone handling
  let time_zone_manager = TimeZoneManager::new()
  
  // Test time zone conversion
  let utc_time = DateTime::new(2023, 1, 15, 14, 30, 0)
  
  let pst_time = TimeZoneManager::convert(time_zone_manager, utc_time, "America/Los_Angeles")
  assert_eq(DateTime::get_hour(pst_time), 6) // 14:30 UTC = 6:30 PST (UTC-8 in January)
  
  let jst_time = TimeZoneManager::convert(time_zone_manager, utc_time, "Asia/Tokyo")
  assert_eq(DateTime::get_hour(jst_time), 23) // 14:30 UTC = 23:30 JST (UTC+9)
  
  // Test time zone aware formatting
  let date_formatter = DateFormatter::new()
  
  let pst_formatted = DateFormatter::format_with_timezone(date_formatter, utc_time, "medium", "America/Los_Angeles", Locale::new("en-US"))
  assert_true(pst_formatted.contains("PST") || pst_formatted.contains("PDT"))
  
  let jst_formatted = DateFormatter::format_with_timezone(date_formatter, utc_time, "medium", "Asia/Tokyo", Locale::new("ja-JP"))
  assert_true(jst_formatted.contains("JST"))
  
  // Test calendar systems
  let calendar_manager = CalendarManager::new()
  
  // Test Gregorian calendar (default)
  let gregorian_date = CalendarManager::create_date(calendar_manager, 2023, 1, 15, "gregorian")
  assert_eq(CalendarDate::get_year(gregorian_date), 2023)
  assert_eq(CalendarDate::get_month(gregorian_date), 1)
  assert_eq(CalendarDate::get_day(gregorian_date), 15)
  
  // Test Islamic calendar
  let islamic_date = CalendarManager::convert_to_calendar(calendar_manager, gregorian_date, "islamic")
  assert_eq(CalendarDate::get_calendar_system(islamic_date), "islamic")
  
  // Test Hebrew calendar
  let hebrew_date = CalendarManager::convert_to_calendar(calendar_manager, gregorian_date, "hebrew")
  assert_eq(CalendarDate::get_calendar_system(hebrew_date), "hebrew")
  
  // Test Chinese calendar
  let chinese_date = CalendarManager::convert_to_calendar(calendar_manager, gregorian_date, "chinese")
  assert_eq(CalendarDate::get_calendar_system(chinese_date), "chinese")
  
  // Test calendar-specific formatting
  let islamic_formatted = DateFormatter::format_with_calendar(date_formatter, islamic_date, "long", Locale::new("ar"))
  assert_true(islamic_formatted.length() > 0)
  
  // Test time zone detection
  let detected_timezone = TimeZoneManager::detect_system_timezone(time_zone_manager)
  assert_true(detected_timezone.length() > 0)
  
  // Test time zone validation
  assert_true(TimeZoneManager::is_valid_timezone(time_zone_manager, "America/New_York"))
  assert_true(TimeZoneManager::is_valid_timezone(time_zone_manager, "Europe/London"))
  assert_true(TimeZoneManager::is_valid_timezone(time_zone_manager, "Asia/Tokyo"))
  assert_false(TimeZoneManager::is_valid_timezone(time_zone_manager, "Invalid/Timezone"))
}

// Test 8: Cultural Adaptation
test "cultural adaptation" {
  // Test cultural preferences
  let cultural_preferences = CulturalPreferences::new()
  
  // Test color associations
  let western_colors = CulturalPreferences::get_color_associations(cultural_preferences, "western")
  assert_eq(western_colors.get("success"), Some("green"))
  assert_eq(western_colors.get("error"), Some("red"))
  assert_eq(western_colors.get("warning"), Some("yellow"))
  
  let eastern_colors = CulturalPreferences::get_color_associations(cultural_preferences, "eastern")
  assert_eq(eastern_colors.get("success"), Some("red")) // Red is positive in many Eastern cultures
  assert_eq(eastern_colors.get("error"), Some("black"))
  assert_eq(eastern_colors.get("warning"), Some("yellow"))
  
  // Test number superstitions
  let western_numbers = CulturalPreferences::get_number_associations(cultural_preferences, "western")
  assert_eq(western_numbers.get("13"), Some("unlucky"))
  assert_eq(western_numbers.get("7"), Some("lucky"))
  
  let chinese_numbers = CulturalPreferences::get_number_associations(cultural_preferences, "chinese")
  assert_eq(chinese_numbers.get("4"), Some("unlucky")) // Sounds like "death"
  assert_eq(chinese_numbers.get("8"), Some("lucky")) // Sounds like "prosperity"
  
  // Test reading patterns
  let western_reading = CulturalPreferences::get_reading_pattern(cultural_preferences, "western")
  assert_eq(western_reading.direction, LeftToRight)
  assert_eq(western_reading.pattern, "Z-pattern") // Top-left to bottom-right
  
  let eastern_reading = CulturalPreferences::get_reading_pattern(cultural_preferences, "eastern")
  assert_eq(eastern_reading.direction, TopToBottom)
  assert_eq(eastern_reading.pattern, "columnar") // Top to bottom, right to left
  
  let middle_eastern_reading = CulturalPreferences::get_reading_pattern(cultural_preferences, "middle-eastern")
  assert_eq(middle_eastern_reading.direction, RightToLeft)
  assert_eq(middle_eastern_reading.pattern, "Z-pattern-mirrored") // Top-right to bottom-left
  
  // Test cultural date format preferences
  let us_date_format = CulturalPreferences::get_date_format_preference(cultural_preferences, "US")
  assert_eq(us_date_format, "MM/DD/YYYY") // Month/Day/Year
  
  let european_date_format = CulturalPreferences::get_date_format_preference(cultural_preferences, "European")
  assert_eq(european_date_format, "DD/MM/YYYY") // Day/Month/Year
  
  let iso_date_format = CulturalPreferences::get_date_format_preference(cultural_preferences, "ISO")
  assert_eq(iso_date_format, "YYYY-MM-DD") // Year-Month-Day
  
  // Test cultural number format preferences
  let us_number_format = CulturalPreferences::get_number_format_preference(cultural_preferences, "US")
  assert_eq(us_number_format.decimal_separator, ".")
  assert_eq(us_number_format.thousands_separator, ",")
  
  let european_number_format = CulturalPreferences::get_number_format_preference(cultural_preferences, "European")
  assert_eq(european_number_format.decimal_separator, ",")
  assert_eq(european_number_format.thousands_separator, ".")
  
  // Test cultural UI adaptation
  let ui_adapter = UIAdapter::new()
  
  // Test icon adaptation
  let western_mail_icon = UIAdapter::get_mail_icon(ui_adapter, "western")
  assert_eq(western_mail_icon, "envelope") // Standard envelope icon
  
  let japanese_mail_icon = UIAdapter::get_mail_icon(ui_adapter, "japanese")
  assert_eq(japanese_mail_icon, "post-office") // Japanese post office symbol
  
  // Test layout adaptation
  let western_layout = UIAdapter::get_layout_preference(ui_adapter, "western")
  assert_eq(western_layout.density, "medium")
  assert_eq(western_layout.navigation_position, "top")
  
  let japanese_layout = UIAdapter::get_layout_preference(ui_adapter, "japanese")
  assert_eq(japanese_layout.density, "compact") // More information-dense
  assert_eq(japanese_layout.navigation_position, "side")
}

// Test 9: Translation Management
test "translation management" {
  // Test translation manager
  let translation_manager = TranslationManager::new()
  
  // Test adding translations
  let en_translations = [
    ("telemetry.dashboard.title", "Telemetry Dashboard"),
    ("telemetry.dashboard.overview", "Overview"),
    ("telemetry.dashboard.metrics", "Metrics"),
    ("telemetry.dashboard.alerts", "Alerts")
  ]
  
  let zh_translations = [
    ("telemetry.dashboard.title", "遥测仪表板"),
    ("telemetry.dashboard.overview", "概览"),
    ("telemetry.dashboard.metrics", "指标"),
    ("telemetry.dashboard.alerts", "警报")
  ]
  
  let fr_translations = [
    ("telemetry.dashboard.title", "Tableau de Bord Télémétrie"),
    ("telemetry.dashboard.overview", "Aperçu"),
    ("telemetry.dashboard.metrics", "Métriques"),
    ("telemetry.dashboard.alerts", "Alertes")
  ]
  
  TranslationManager::add_translations(translation_manager, Locale::new("en"), en_translations)
  TranslationManager::add_translations(translation_manager, Locale::new("zh-CN"), zh_translations)
  TranslationManager::add_translations(translation_manager, Locale::new("fr"), fr_translations)
  
  // Test translation retrieval
  let en_title = TranslationManager::get_translation(translation_manager, Locale::new("en"), "telemetry.dashboard.title")
  match en_title {
    Some(translation) => assert_eq(translation, "Telemetry Dashboard")
    None => assert_true(false)
  }
  
  let zh_title = TranslationManager::get_translation(translation_manager, Locale::new("zh-CN"), "telemetry.dashboard.title")
  match zh_title {
    Some(translation) => assert_eq(translation, "遥测仪表板")
    None => assert_true(false)
  }
  
  // Test translation fallback
  let en_gb_title = TranslationManager::get_translation(translation_manager, Locale::new("en-GB"), "telemetry.dashboard.title")
  match en_gb_title {
    Some(translation) => assert_eq(translation, "Telemetry Dashboard") // Fallback to en
    None => assert_true(false)
  }
  
  // Test translation context
  TranslationManager::add_contextual_translation(translation_manager, Locale::new("en"), "telemetry.dashboard.title", "Telemetry Dashboard", "dashboard")
  TranslationManager::add_contextual_translation(translation_manager, Locale::new("en"), "telemetry.dashboard.title", "Telemetry", "short")
  
  let full_title = TranslationManager::get_contextual_translation(translation_manager, Locale::new("en"), "telemetry.dashboard.title", "dashboard")
  match full_title {
    Some(translation) => assert_eq(translation, "Telemetry Dashboard")
    None => assert_true(false)
  }
  
  let short_title = TranslationManager::get_contextual_translation(translation_manager, Locale::new("en"), "telemetry.dashboard.title", "short")
  match short_title {
    Some(translation) => assert_eq(translation, "Telemetry")
    None => assert_true(false)
  }
  
  // Test translation validation
  let validation_result = TranslationManager::validate_translations(translation_manager)
  assert_true(validation_result.is_valid)
  
  // Test missing translations detection
  let missing_keys = TranslationManager::find_missing_translations(translation_manager)
  assert_eq(missing_keys.length(), 0) // All keys have translations for all supported locales
  
  // Add a new translation for only one locale
  TranslationManager::add_translation(translation_manager, Locale::new("en"), "telemetry.new.feature", "New Feature")
  
  let new_missing_keys = TranslationManager::find_missing_translations(translation_manager)
  assert_eq(new_missing_keys.length(), 2) // Missing for zh-CN and fr
  
  // Test translation export
  let en_export = TranslationManager::export_translations(translation_manager, Locale::new("en"), "json")
  assert_true(en_export.contains("telemetry.dashboard.title"))
  assert_true(en_export.contains("Telemetry Dashboard"))
  
  // Test translation import
  let import_data = "{\"telemetry.imported.key\": \"Imported Translation\"}"
  let import_result = TranslationManager::import_translations(translation_manager, Locale::new("en"), import_data, "json")
  assert_true(import_result)
  
  let imported_translation = TranslationManager::get_translation(translation_manager, Locale::new("en"), "telemetry.imported.key")
  match imported_translation {
    Some(translation) => assert_eq(translation, "Imported Translation")
    None => assert_true(false)
  }
}

// Test 10: Localization Testing
test "localization testing" {
  // Test localization validator
  let localization_validator = LocalizationValidator::new()
  
  // Test text expansion
  let en_text = "Submit"
  let de_text = "Absenden" // German is often longer
  let ja_text = "送信" // Japanese can be shorter
  
  let en_length = LocalizationValidator::measure_text(localization_validator, en_text, "Arial", 12)
  let de_length = LocalizationValidator::measure_text(localization_validator, de_text, "Arial", 12)
  let ja_length = LocalizationValidator::measure_text(localization_validator, ja_text, "Arial", 12)
  
  assert_true(de_length > en_length) // German text should be longer
  assert_true(ja_length < en_length) // Japanese text should be shorter
  
  // Test UI layout validation
  let ui_validator = UIValidator::new()
  
  let test_ui_element = UIElement::new("button", 100, 30) // Width 100, Height 30
  let localized_texts = [
    (Locale::new("en"), "Submit"),
    (Locale::new("de"), "Absenden"),
    (Locale::new("fr"), "Soumettre"),
    (Locale::new("es"), "Enviar")
  ]
  
  let layout_issues = UIValidator::validate_text_fitting(ui_validator, test_ui_element, localized_texts, "Arial", 12)
  
  for issue in layout_issues {
    assert_eq(UIValidationIssue::get_element_id(issue), "button")
    assert_eq(UIValidationIssue::get_issue_type(issue), "text_overflow")
  }
  
  // Test pseudolocalization
  let pseudolocalizer = Pseudolocalizer::new()
  
  let original_text = "Welcome to Azimuth Telemetry"
  let pseudolocalized = Pseudolocalizer::pseudolocalize(pseudolocalizer, original_text)
  
  assert_true(pseudolocalized.length() > original_text.length()) // Pseudolocalized text should be longer
  assert_true(pseudolocalized.contains("[") && pseudolocalized.contains("]")) // Should contain markers
  
  // Test right-to-left simulation
  let rtl_simulator = RTLSimulator::new()
  
  let ltr_text = "Hello World"
  let rtl_simulated = RTLSimulator::simulate_rtl(rtl_simulator, ltr_text)
  
  assert_true(rtl_simulated.length() == ltr_text.length()) // Length should be the same
  // Content should be logically the same but visually reversed in RTL context
  
  // Test locale-specific functionality
  let locale_tester = LocaleTester::new()
  
  // Test date input validation for different locales
  let us_date_input = "01/15/2023" // MM/DD/YYYY
  let us_date_valid = LocaleTester::validate_date_input(locale_tester, us_date_input, Locale::new("en-US"))
  assert_true(us_date_valid)
  
  let eu_date_input = "15/01/2023" // DD/MM/YYYY
  let eu_date_valid = LocaleTester::validate_date_input(locale_tester, eu_date_input, Locale::new("fr"))
  assert_true(eu_date_valid)
  
  // Test number input validation for different locales
  let us_number_input = "1,234.56" // US format
  let us_number_valid = LocaleTester::validate_number_input(locale_tester, us_number_input, Locale::new("en-US"))
  assert_true(us_number_valid)
  
  let de_number_input = "1.234,56" // German format
  let de_number_valid = LocaleTester::validate_number_input(locale_tester, de_number_input, Locale::new("de"))
  assert_true(de_number_valid)
  
  // Test localization coverage report
  let coverage_reporter = LocalizationCoverageReporter::new()
  
  let test_keys = [
    "telemetry.dashboard.title",
    "telemetry.dashboard.overview",
    "telemetry.dashboard.metrics",
    "telemetry.dashboard.alerts",
    "telemetry.new.feature" // Only translated in English
  ]
  
  let coverage_report = LocalizationCoverageReporter::generate_report(coverage_reporter, translation_manager, test_keys)
  
  assert_eq(CoverageReport::get_total_keys(coverage_report), 5)
  assert_eq(CoverageReport::get_coverage_percentage(coverage_report, Locale::new("en")), 100) // 100% coverage for English
  assert_eq(CoverageReport::get_coverage_percentage(coverage_report, Locale::new("zh-CN")), 80) // 80% coverage for Chinese (4/5)
  assert_eq(CoverageReport::get_coverage_percentage(coverage_report, Locale::new("fr")), 80) // 80% coverage for French (4/5)
  
  // Test localization quality assessment
  let quality_assessor = LocalizationQualityAssessor::new()
  
  let quality_metrics = LocalizationQualityAssessor::assess_quality(quality_assessor, translation_manager)
  
  assert_true(QualityMetrics::get_translation_completeness(quality_metrics) > 0.7) // At least 70% complete
  assert_true(QualityMetrics::get_consistency_score(quality_metrics) > 0.8) // At least 80% consistent
  assert_true(QualityMetrics::get_terminology_accuracy(quality_metrics) > 0.9) // At least 90% accurate terminology
}