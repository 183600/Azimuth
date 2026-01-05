// Azimuth Telemetry System - Internationalization Comprehensive Support Tests
// This file contains comprehensive test cases for internationalization and localization features

// Test 1: Multi-language Support
test "multi-language support" {
  // Create internationalization manager
  let i18n_manager = I18nManager::new()
  
  // Load language resources
  I18nManager::load_language(i18n_manager, "en", "locales/en.json")
  I18nManager::load_language(i18n_manager, "zh", "locales/zh.json")
  I18nManager::load_language(i18n_manager, "es", "locales/es.json")
  I18nManager::load_language(i18n_manager, "fr", "locales/fr.json")
  I18nManager::load_language(i18n_manager, "ja", "locales/ja.json")
  I18nManager::load_language(i18n_manager, "ar", "locales/ar.json")
  
  // Set default language
  I18nManager::set_default_language(i18n_manager, "en")
  
  // Test English translations
  I18nManager::set_language(i18n_manager, "en")
  
  let en_service_name = I18nManager::translate(i18n_manager, "service.name")
  let en_description = I18nManager::translate(i18n_manager, "service.description")
  let en_error_message = I18nManager::translate(i18n_manager, "error.connection_failed")
  
  assert_eq(en_service_name, Some("Azimuth Telemetry Service"))
  assert_eq(en_description, Some("Advanced telemetry data collection and analysis platform"))
  assert_eq(en_error_message, Some("Failed to connect to the telemetry server"))
  
  // Test Chinese translations
  I18nManager::set_language(i18n_manager, "zh")
  
  let zh_service_name = I18nManager::translate(i18n_manager, "service.name")
  let zh_description = I18nManager::translate(i18n_manager, "service.description")
  let zh_error_message = I18nManager::translate(i18n_manager, "error.connection_failed")
  
  assert_eq(zh_service_name, Some("Azimuth 遥测服务"))
  assert_eq(zh_description, Some("高级遥测数据收集和分析平台"))
  assert_eq(zh_error_message, Some("连接遥测服务器失败"))
  
  // Test Spanish translations
  I18nManager::set_language(i18n_manager, "es")
  
  let es_service_name = I18nManager::translate(i18n_manager, "service.name")
  let es_description = I18nManager::translate(i18n_manager, "service.description")
  let es_error_message = I18nManager::translate(i18n_manager, "error.connection_failed")
  
  assert_eq(es_service_name, Some("Servicio de Telemetría Azimuth"))
  assert_eq(es_description, Some("Plataforma avanzada de recopilación y análisis de datos de telemetría"))
  assert_eq(es_error_message, Some("Error al conectar con el servidor de telemetría"))
  
  // Test French translations
  I18nManager::set_language(i18n_manager, "fr")
  
  let fr_service_name = I18nManager::translate(i18n_manager, "service.name")
  let fr_description = I18nManager::translate(i18n_manager, "service.description")
  let fr_error_message = I18nManager::translate(i18n_manager, "error.connection_failed")
  
  assert_eq(fr_service_name, Some("Service de Télémétrie Azimuth"))
  assert_eq(fr_description, Some("Plateforme avancée de collecte et d'analyse de données de télémétrie"))
  assert_eq(fr_error_message, Some("Échec de la connexion au serveur de télémétrie"))
  
  // Test Japanese translations
  I18nManager::set_language(i18n_manager, "ja")
  
  let ja_service_name = I18nManager::translate(i18n_manager, "service.name")
  let ja_description = I18nManager::translate(i18n_manager, "service.description")
  let ja_error_message = I18nManager::translate(i18n_manager, "error.connection_failed")
  
  assert_eq(ja_service_name, Some("Azimuth テレメトリサービス"))
  assert_eq(ja_description, Some("高度なテレメトリデータ収集・分析プラットフォーム"))
  assert_eq(ja_error_message, Some("テレメトリサーバーへの接続に失敗しました"))
  
  // Test Arabic translations (RTL language)
  I18nManager::set_language(i18n_manager, "ar")
  
  let ar_service_name = I18nManager::translate(i18n_manager, "service.name")
  let ar_description = I18nManager::translate(i18n_manager, "service.description")
  let ar_error_message = I18nManager::translate(i18n_manager, "error.connection_failed")
  
  assert_eq(ar_service_name, Some("خدمة تيليمتري أزيموث"))
  assert_eq(ar_description, Some("منصة متقدمة لجمع وتحليل بيانات التيليمتري"))
  assert_eq(ar_error_message, Some("فشل في الاتصال بخادم التيليمتري"))
  
  // Test missing translation fallback
  I18nManager::set_language(i18n_manager, "en")
  
  let missing_translation = I18nManager::translate(i18n_manager, "non.existent.key")
  assert_eq(missing_translation, Some("non.existent.key")) // Should return key as fallback
  
  // Test translation with parameters
  I18nManager::set_language(i18n_manager, "en")
  
  let en_param_result = I18nManager::translate_with_params(
    i18n_manager, 
    "metric.count", 
    [("count", IntValue(42))]
  )
  assert_eq(en_param_result, Some("Total metrics: 42"))
  
  I18nManager::set_language(i18n_manager, "zh")
  
  let zh_param_result = I18nManager::translate_with_params(
    i18n_manager, 
    "metric.count", 
    [("count", IntValue(42))]
  )
  assert_eq(zh_param_result, Some("总指标数：42"))
}

// Test 2: Locale-specific Formatting
test "locale-specific formatting" {
  // Create locale formatter
  let formatter = LocaleFormatter::new()
  
  // Test date formatting
  let test_date = DateTime::new(2023, 12, 25, 15, 30, 45)
  
  // US English format (MM/DD/YYYY)
  let us_date = LocaleFormatter::format_date(formatter, test_date, "en-US")
  assert_eq(us_date, "12/25/2023")
  
  // UK English format (DD/MM/YYYY)
  let uk_date = LocaleFormatter::format_date(formatter, test_date, "en-GB")
  assert_eq(uk_date, "25/12/2023")
  
  // Chinese format (YYYY年MM月DD日)
  let zh_date = LocaleFormatter::format_date(formatter, test_date, "zh-CN")
  assert_eq(zh_date, "2023年12月25日")
  
  // Japanese format (YYYY年MM月DD日)
  let ja_date = LocaleFormatter::format_date(formatter, test_date, "ja-JP")
  assert_eq(ja_date, "2023年12月25日")
  
  // Arabic format (DD/MM/YYYY)
  let ar_date = LocaleFormatter::format_date(formatter, test_date, "ar-SA")
  assert_eq(ar_date, "٢٥/١٢/٢٠٢٣") // Using Arabic numerals
  
  // Test time formatting
  let test_time = Time::new(15, 30, 45)
  
  // 12-hour format (US)
  let us_time = LocaleFormatter::format_time(formatter, test_time, "en-US", false)
  assert_eq(us_time, "3:30:45 PM")
  
  // 24-hour format (European)
  let eu_time = LocaleFormatter::format_time(formatter, test_time, "fr-FR", true)
  assert_eq(eu_time, "15:30:45")
  
  // Test number formatting
  let test_number = 1234567.89
  
  // US format (comma as thousands separator, period as decimal)
  let us_number = LocaleFormatter::format_number(formatter, test_number, "en-US")
  assert_eq(us_number, "1,234,567.89")
  
  // European format (period as thousands separator, comma as decimal)
  let de_number = LocaleFormatter::format_number(formatter, test_number, "de-DE")
  assert_eq(de_number, "1.234.567,89")
  
  // Chinese format (no thousands separator)
  let zh_number = LocaleFormatter::format_number(formatter, test_number, "zh-CN")
  assert_eq(zh_number, "1234567.89")
  
  // Arabic format (Arabic numerals)
  let ar_number = LocaleFormatter::format_number(formatter, test_number, "ar-SA")
  assert_eq(ar_number, "١٬٢٣٤٬٥٦٧٫٨٩")
  
  // Test currency formatting
  let test_currency = 1234.56
  
  // US Dollar
  let usd = LocaleFormatter::format_currency(formatter, test_currency, "en-US", "USD")
  assert_eq(usd, "$1,234.56")
  
  // Euro
  let euro = LocaleFormatter::format_currency(formatter, test_currency, "de-DE", "EUR")
  assert_eq(euro, "1.234,56 €")
  
  // Chinese Yuan
  let cny = LocaleFormatter::format_currency(formatter, test_currency, "zh-CN", "CNY")
  assert_eq(cny, "¥1,234.56")
  
  // Japanese Yen
  let jpy = LocaleFormatter::format_currency(formatter, test_currency, "ja-JP", "JPY")
  assert_eq(jpy, "¥1,235") // Yen doesn't have decimal places
  
  // Test percentage formatting
  let test_percentage = 0.7542
  
  // US format
  let us_percent = LocaleFormatter::format_percentage(formatter, test_percentage, "en-US", 2)
  assert_eq(us_percent, "75.42%")
  
  // French format
  let fr_percent = LocaleFormatter::format_percentage(formatter, test_percentage, "fr-FR", 2)
  assert_eq(fr_percent, "75,42 %")
}

// Test 3: Text Direction and RTL Support
test "text direction and rtl support" {
  // Create text direction manager
  let text_dir_manager = TextDirectionManager::new()
  
  // Test LTR languages
  assert_eq(TextDirectionManager::get_direction(text_dir_manager, "en"), "LTR")
  assert_eq(TextDirectionManager::get_direction(text_dir_manager, "fr"), "LTR")
  assert_eq(TextDirectionManager::get_direction(text_dir_manager, "de"), "LTR")
  assert_eq(TextDirectionManager::get_direction(text_dir_manager, "es"), "LTR")
  assert_eq(TextDirectionManager::get_direction(text_dir_manager, "zh"), "LTR")
  assert_eq(TextDirectionManager::get_direction(text_dir_manager, "ja"), "LTR")
  
  // Test RTL languages
  assert_eq(TextDirectionManager::get_direction(text_dir_manager, "ar"), "RTL")
  assert_eq(TextDirectionManager::get_direction(text_dir_manager, "he"), "RTL")
  assert_eq(TextDirectionManager::get_direction(text_dir_manager, "fa"), "RTL")
  assert_eq(TextDirectionManager::get_direction(text_dir_manager, "ur"), "RTL")
  
  // Test text alignment
  let ltr_text = "Hello, World!"
  let rtl_text = "مرحبا بالعالم!"
  
  // LTR text should be left-aligned in LTR context
  let ltr_alignment = TextDirectionManager::get_alignment(text_dir_manager, ltr_text, "en")
  assert_eq(ltr_alignment, "left")
  
  // RTL text should be right-aligned in RTL context
  let rtl_alignment = TextDirectionManager::get_alignment(text_dir_manager, rtl_text, "ar")
  assert_eq(rtl_alignment, "right")
  
  // Test text ordering in mixed content
  let mixed_text = "Hello مرحبا World"
  let ordered_text = TextDirectionManager::order_mixed_text(text_dir_manager, mixed_text, "en")
  assert_eq(ordered_text, "Hello مرحبا World") // Should maintain logical order
  
  // Test UI layout direction
  let layout_en = TextDirectionManager::get_layout_direction(text_dir_manager, "en")
  assert_eq(layout_en, "LTR")
  
  let layout_ar = TextDirectionManager::get_layout_direction(text_dir_manager, "ar")
  assert_eq(layout_ar, "RTL")
  
  // Test margin/padding direction
  let margin_en = TextDirectionManager::get_margin_start(text_dir_manager, "en")
  assert_eq(margin_en, "left")
  
  let margin_ar = TextDirectionManager::get_margin_start(text_dir_manager, "ar")
  assert_eq(margin_ar, "right")
  
  // Test text input direction
  let input_en = TextDirectionManager::get_input_direction(text_dir_manager, "en")
  assert_eq(input_en, "LTR")
  
  let input_ar = TextDirectionManager::get_input_direction(text_dir_manager, "ar")
  assert_eq(input_ar, "RTL")
}

// Test 4: Cultural and Regional Adaptations
test "cultural and regional adaptations" {
  // Create cultural adaptation manager
  let culture_manager = CultureManager::new()
  
  // Test first day of week
  assert_eq(CultureManager::get_first_day_of_week(culture_manager, "en-US"), "sunday")
  assert_eq(CultureManager::get_first_day_of_week(culture_manager, "en-GB"), "monday")
  assert_eq(CultureManager::get_first_day_of_week(culture_manager, "fr-FR"), "monday")
  assert_eq(CultureManager::get_first_day_of_week(culture_manager, "ar-SA"), "sunday")
  
  // Test weekend days
  let us_weekend = CultureManager::get_weekend_days(culture_manager, "en-US")
  assert_eq(us_weekend, ["saturday", "sunday"])
  
  let fr_weekend = CultureManager::get_weekend_days(culture_manager, "fr-FR")
  assert_eq(fr_weekend, ["saturday", "sunday"])
  
  let sa_weekend = CultureManager::get_weekend_days(culture_manager, "ar-SA")
  assert_eq(sa_weekend, ["friday", "saturday"])
  
  // Test date formats
  let us_date_format = CultureManager::get_date_format(culture_manager, "en-US")
  assert_eq(us_date_format, "MM/DD/YYYY")
  
  let uk_date_format = CultureManager::get_date_format(culture_manager, "en-GB")
  assert_eq(uk_date_format, "DD/MM/YYYY")
  
  let iso_date_format = CultureManager::get_date_format(culture_manager, "sv-SE")
  assert_eq(iso_date_format, "YYYY-MM-DD")
  
  // Test time formats
  let us_time_format = CultureManager::get_time_format(culture_manager, "en-US")
  assert_eq(us_time_format, "h:mm:ss A") // 12-hour format
  
  let eu_time_format = CultureManager::get_time_format(culture_manager, "fr-FR")
  assert_eq(eu_time_format, "HH:mm:ss") // 24-hour format
  
  // Test measurement systems
  let us_system = CultureManager::get_measurement_system(culture_manager, "en-US")
  assert_eq(us_system, "imperial")
  
  let uk_system = CultureManager::get_measurement_system(culture_manager, "en-GB")
  assert_eq(uk_system, "imperial")
  
  let eu_system = CultureManager::get_measurement_system(culture_manager, "fr-FR")
  assert_eq(eu_system, "metric")
  
  let cn_system = CultureManager::get_measurement_system(culture_manager, "zh-CN")
  assert_eq(cn_system, "metric")
  
  // Test number grouping
  let us_grouping = CultureManager::get_number_grouping(culture_manager, "en-US")
  assert_eq(us_grouping, [3, 3]) // Groups of 3 digits
  
  let in_grouping = CultureManager::get_number_grouping(culture_manager, "en-IN")
  assert_eq(in_grouping, [3, 2]) // First group of 3, then groups of 2
  
  // Test paper sizes
  let us_paper = CultureManager::get_default_paper_size(culture_manager, "en-US")
  assert_eq(us_paper, "letter")
  
  let eu_paper = CultureManager::get_default_paper_size(culture_manager, "fr-FR")
  assert_eq(eu_paper, "a4")
  
  // Test address formats
  let us_address = CultureManager::get_address_format(culture_manager, "en-US")
  assert_eq(us_address, "{street}\n{city}, {state} {zip}\n{country}")
  
  let jp_address = CultureManager::get_address_format(culture_manager, "ja-JP")
  assert_eq(jp_address, "〒{zip}\n{state}{city}{street}\n{country}")
  
  // Test name formats
  let western_name = CultureManager::get_name_format(culture_manager, "en-US")
  assert_eq(western_name, "{given} {family}")
  
  let eastern_name = CultureManager::get_name_format(culture_manager, "ja-JP")
  assert_eq(eastern_name, "{family} {given}")
  
  let hungarian_name = CultureManager::get_name_format(culture_manager, "hu-HU")
  assert_eq(hungarian_name, "{family} {given}")
}

// Test 5: Character Encoding and Unicode Support
test "character encoding and unicode support" {
  // Create encoding manager
  let encoding_manager = EncodingManager::new()
  
  // Test UTF-8 encoding
  let utf8_text = "Hello 世界 こんにちは مرحبا 🌍"
  let utf8_encoded = EncodingManager::encode_utf8(encoding_manager, utf8_text)
  let utf8_decoded = EncodingManager::decode_utf8(encoding_manager, utf8_encoded)
  
  assert_eq(utf8_decoded, utf8_text)
  
  // Test UTF-16 encoding
  let utf16_encoded = EncodingManager::encode_utf16(encoding_manager, utf8_text)
  let utf16_decoded = EncodingManager::decode_utf16(encoding_manager, utf16_encoded)
  
  assert_eq(utf16_decoded, utf8_text)
  
  // Test character detection
  let latin_text = "Hello, World!"
  let chinese_text = "你好，世界！"
  let arabic_text = "مرحبا بالعالم!"
  let emoji_text = "Hello 🌍🚀🔥"
  
  assert_eq(EncodingManager::detect_script(encoding_manager, latin_text), "Latin")
  assert_eq(EncodingManager::detect_script(encoding_manager, chinese_text), "Han")
  assert_eq(EncodingManager::detect_script(encoding_manager, arabic_text), "Arabic")
  assert_eq(EncodingManager::detect_script(encoding_manager, emoji_text), "Common") // Emojis are in Common script
  
  // Test character normalization
  let composed_text = "é" // Composed form
  let decomposed_text = "e\u0301" // Decomposed form (e + combining acute accent)
  
  let normalized_composed = EncodingManager::normalize_nfc(encoding_manager, decomposed_text)
  assert_eq(normalized_composed, composed_text)
  
  let normalized_decomposed = EncodingManager::normalize_nfd(encoding_manager, composed_text)
  assert_eq(normalized_decomposed, decomposed_text)
  
  // Test bidirectional text handling
  let bidi_text = "Hello مرحبا こんにちは"
  let bidi_visual = EncodingManager::bidi_visual(encoding_manager, bidi_text)
  let bidi_logical = EncodingManager::bidi_logical(encoding_manager, bidi_visual)
  
  assert_eq(bidi_logical, bidi_text)
  
  // Test text width calculation
  let latin_width = EncodingManager::get_text_width(encoding_manager, latin_text, "Arial", 12)
  let chinese_width = EncodingManager::get_text_width(encoding_manager, chinese_text, "Arial", 12)
  let arabic_width = EncodingManager::get_text_width(encoding_manager, arabic_text, "Arial", 12)
  
  // CJK characters are typically wider than Latin characters
  assert_true(chinese_width > latin_width)
  // Arabic characters might have different width depending on font
  assert_true(arabic_width > 0)
  
  // Test font fallback
  let fonts = ["Arial", "Noto Sans CJK", "Noto Sans Arabic", "Segoe UI Emoji"]
  let fallback_font = EncodingManager::get_font_fallback(encoding_manager, utf8_text, fonts)
  
  // Should select appropriate font for mixed content
  assert_true(fallback_font == "Noto Sans CJK" || fallback_font == "Noto Sans Arabic" || fallback_font == "Segoe UI Emoji")
  
  // Test text segmentation
  let english_words = EncodingManager::segment_words(encoding_manager, "Hello World!", "en")
  assert_eq(english_words, ["Hello", "World"])
  
  let chinese_words = EncodingManager::segment_words(encoding_manager, "你好世界", "zh")
  assert_eq(chinese_words, ["你", "好", "世", "界"]) // Chinese characters are segmented individually
  
  let japanese_words = EncodingManager::segment_words(encoding_manager, "こんにちは世界", "ja")
  assert_eq(japanese_words, ["こんにちは", "世界"]) // Japanese has word boundaries
  
  // Test transliteration
  let japanese_to_latin = EncodingManager::transliterate(encoding_manager, "こんにちは", "ja", "Latn")
  assert_eq(japanese_to_latin, "konnichiwa")
  
  let chinese_to_latin = EncodingManager::transliterate(encoding_manager, "你好", "zh", "Latn")
  assert_eq(chinese_to_latin, "ni hao")
  
  let arabic_to_latin = EncodingManager::transliterate(encoding_manager, "مرحبا", "ar", "Latn")
  assert_eq(arabic_to_latin, "mrhba")
}

// Test 6: Pluralization and Gender Agreement
test "pluralization and gender agreement" {
  // Create grammar manager
  let grammar_manager = GrammarManager::new()
  
  // Test English pluralization
  let en_singular = GrammarManager::pluralize(grammar_manager, "item", 1, "en")
  assert_eq(en_singular, "item")
  
  let en_plural = GrammarManager::pluralize(grammar_manager, "item", 2, "en")
  assert_eq(en_plural, "items")
  
  let en_zero = GrammarManager::pluralize(grammar_manager, "item", 0, "en")
  assert_eq(en_zero, "items") // Zero uses plural form in English
  
  // Test Chinese pluralization (Chinese doesn't typically pluralize nouns)
  let zh_singular = GrammarManager::pluralize(grammar_manager, "项目", 1, "zh")
  assert_eq(zh_singular, "项目")
  
  let zh_plural = GrammarManager::pluralize(grammar_manager, "项目", 2, "zh")
  assert_eq(zh_plural, "项目") // Same form for singular and plural
  
  // Test Arabic pluralization (complex rules)
  let ar_zero = GrammarManager::pluralize(grammar_manager, "كتاب", 0, "ar")
  assert_eq(ar_zero, "كتب") // Zero form
  
  let ar_singular = GrammarManager::pluralize(grammar_manager, "كتاب", 1, "ar")
  assert_eq(ar_singular, "كتاب") // Singular form
  
  let ar_dual = GrammarManager::pluralize(grammar_manager, "كتاب", 2, "ar")
  assert_eq(ar_dual, "كتابان") // Dual form
  
  let ar_small_plural = GrammarManager::pluralize(grammar_manager, "كتاب", 3, "ar")
  assert_eq(ar_small_plural, "كتب") // Small plural (3-10)
  
  let ar_large_plural = GrammarManager::pluralize(grammar_manager, "كتاب", 11, "ar")
  assert_eq(ar_large_plural, "كتاب") // Large plural (11+)
  
  // Test Russian pluralization (three forms)
  let ru_singular = GrammarManager::pluralize(grammar_manager, "элемент", 1, "ru")
  assert_eq(ru_singular, "элемент") // Singular (1, 21, 31, etc.)
  
  let ru_few = GrammarManager::pluralize(grammar_manager, "элемент", 2, "ru")
  assert_eq(ru_few, "элемента") // Few (2-4, 22-24, 32-34, etc.)
  
  let ru_many = GrammarManager::pluralize(grammar_manager, "элемент", 5, "ru")
  assert_eq(ru_many, "элементов") // Many (0, 5-20, 25-30, etc.)
  
  // Test gender agreement
  let en_male = GrammarManager::agree_gender(grammar_manager, "happy", "male", "en")
  assert_eq(en_male, "happy") // English doesn't have gender agreement for adjectives
  
  let en_female = GrammarManager::agree_gender(grammar_manager, "happy", "female", "en")
  assert_eq(en_female, "happy")
  
  let es_male = GrammarManager::agree_gender(grammar_manager, "feliz", "male", "es")
  assert_eq(es_male, "feliz") // "feliz" is the same for both genders
  
  let es_female = GrammarManager::agree_gender(grammar_manager, "feliz", "female", "es")
  assert_eq(es_female, "feliz")
  
  let fr_male = GrammarManager::agree_gender(grammar_manager, "petit", "male", "fr")
  assert_eq(fr_male, "petit") // Masculine form
  
  let fr_female = GrammarManager::agree_gender(grammar_manager, "petit", "female", "fr")
  assert_eq(fr_female, "petite") // Feminine form
  
  // Test ordinal numbers
  let en_ordinal = GrammarManager::ordinalize(grammar_manager, 1, "en")
  assert_eq(en_ordinal, "1st")
  
  let en_ordinal_2 = GrammarManager::ordinalize(grammar_manager, 2, "en")
  assert_eq(en_ordinal_2, "2nd")
  
  let en_ordinal_3 = GrammarManager::ordinalize(grammar_manager, 3, "en")
  assert_eq(en_ordinal_3, "3rd")
  
  let en_ordinal_4 = GrammarManager::ordinalize(grammar_manager, 4, "en")
  assert_eq(en_ordinal_4, "4th")
  
  let es_ordinal = GrammarManager::ordinalize(grammar_manager, 1, "es")
  assert_eq(es_ordinal, "1º") // Masculine ordinal
  
  let es_ordinal_female = GrammarManager::ordinalize(grammar_manager, 1, "es", "female")
  assert_eq(es_ordinal_female, "1ª") // Feminine ordinal
  
  // Test contextual translation
  let en_context = GrammarManager::translate_contextual(grammar_manager, "book", "noun", "en")
  assert_eq(en_context, "book") // Noun form
  
  let en_context_verb = GrammarManager::translate_contextual(grammar_manager, "book", "verb", "en")
  assert_eq(en_context_verb, "book") // Verb form (same in English)
  
  let fr_context_noun = GrammarManager::translate_contextual(grammar_manager, "livre", "noun", "fr")
  assert_eq(fr_context_noun, "livre") // Noun form (masculine)
  
  let fr_context_adj = GrammarManager::translate_contextual(grammar_manager, "livre", "adjective", "fr")
  assert_eq(fr_context_adj, "libre") // Adjective form (different word)
}

// Test 7: Time Zone and Calendar Support
test "time zone and calendar support" {
  // Create time zone manager
  let tz_manager = TimeZoneManager::new()
  
  // Test time zone conversion
  let utc_time = DateTime::new(2023, 12, 25, 12, 0, 0)
  
  // Convert to US Eastern Time (UTC-5 in winter)
  let est_time = TimeZoneManager::convert(tz_manager, utc_time, "UTC", "America/New_York")
  assert_eq(est_time.hour(), 7) // 12:00 UTC = 7:00 EST
  
  // Convert to Japan Standard Time (UTC+9)
  let jst_time = TimeZoneManager::convert(tz_manager, utc_time, "UTC", "Asia/Tokyo")
  assert_eq(jst_time.hour(), 21) // 12:00 UTC = 21:00 JST
  
  // Convert to Central European Time (UTC+1 in winter)
  let cet_time = TimeZoneManager::convert(tz_manager, utc_time, "UTC", "Europe/Paris")
  assert_eq(cet_time.hour(), 13) // 12:00 UTC = 13:00 CET
  
  // Test daylight saving time
  let summer_time = DateTime::new(2023, 7, 25, 12, 0, 0)
  
  // US Eastern Daylight Time (UTC-4 in summer)
  let edt_time = TimeZoneManager::convert(tz_manager, summer_time, "UTC", "America/New_York")
  assert_eq(edt_time.hour(), 8) // 12:00 UTC = 8:00 EDT
  
  // Central European Summer Time (UTC+2 in summer)
  let cest_time = TimeZoneManager::convert(tz_manager, summer_time, "UTC", "Europe/Paris")
  assert_eq(cest_time.hour(), 14) // 12:00 UTC = 14:00 CEST
  
  // Test time zone detection
  let detected_tz = TimeZoneManager::detect_system_time_zone(tz_manager)
  assert_true(detected_tz.length() > 0) // Should detect a time zone
  
  // Test time zone abbreviations
  let est_abbrev = TimeZoneManager::get_abbreviation(tz_manager, "America/New_York", utc_time)
  assert_eq(est_abbrev, "EST") // Eastern Standard Time in winter
  
  let edt_abbrev = TimeZoneManager::get_abbreviation(tz_manager, "America/New_York", summer_time)
  assert_eq(edt_abbrev, "EDT") // Eastern Daylight Time in summer
  
  // Test calendar systems
  let calendar_manager = CalendarManager::new()
  
  // Test Gregorian calendar (default)
  let gregorian_date = CalendarManager::create_date(calendar_manager, 2023, 12, 25, "gregorian")
  assert_eq(gregorian_date.year(), 2023)
  assert_eq(gregorian_date.month(), 12)
  assert_eq(gregorian_date.day(), 25)
  
  // Test Islamic calendar
  let islamic_date = CalendarManager::convert_calendar(calendar_manager, gregorian_date, "gregorian", "islamic")
  assert_eq(islamic_date.year(), 1445) // Approximate
  assert_eq(islamic_date.month(), 6) // Approximate
  assert_eq(islamic_date.day(), 11) // Approximate
  
  // Test Hebrew calendar
  let hebrew_date = CalendarManager::convert_calendar(calendar_manager, gregorian_date, "gregorian", "hebrew")
  assert_eq(hebrew_date.year(), 5784) // Approximate
  assert_eq(hebrew_date.month(), 10) // Approximate
  assert_eq(hebrew_date.day(), 12) // Approximate
  
  // Test Chinese calendar
  let chinese_date = CalendarManager::convert_calendar(calendar_manager, gregorian_date, "gregorian", "chinese")
  assert_eq(chinese_date.year(), 4660) // Approximate
  assert_eq(chinese_date.month(), 11) // Approximate
  assert_eq(chinese_date.day(), 13) // Approximate
  
  // Test calendar-specific formatting
  let gregorian_formatted = CalendarManager::format_date(calendar_manager, gregorian_date, "gregorian", "en-US")
  assert_eq(gregorian_formatted, "12/25/2023")
  
  let islamic_formatted = CalendarManager::format_date(calendar_manager, islamic_date, "islamic", "ar-SA")
  assert_true(islamic_formatted.contains("١٤٤٥")) // Should contain Islamic year in Arabic numerals
  
  // Test working day calculation
  let us_working_days = CalendarManager::get_working_days(calendar_manager, 2023, 12, "en-US")
  assert_eq(us_working_days, ["monday", "tuesday", "wednesday", "thursday", "friday"])
  
  let sa_working_days = CalendarManager::get_working_days(calendar_manager, 2023, 12, "ar-SA")
  assert_eq(sa_working_days, ["sunday", "monday", "tuesday", "wednesday", "thursday"])
  
  // Test holiday detection
  let us_holidays = CalendarManager::get_holidays(calendar_manager, 2023, 12, "en-US")
  assert_true(us_holidays.contains("Christmas")) // December 25
  
  let islamic_holidays = CalendarManager::get_holidays(calendar_manager, 1445, 10, "ar-SA")
  assert_true(islamic_holidays.length() > 0) // Should have Islamic holidays
}