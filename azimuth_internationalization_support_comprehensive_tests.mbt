// Azimuth Telemetry System - Internationalization Support Comprehensive Tests
// This file contains comprehensive test cases for internationalization functionality

// Test 1: Locale Detection and Management
test "locale detection and management" {
  let locale_manager = LocaleManager::new()
  
  // Test default locale
  let default_locale = LocaleManager::get_default_locale(locale_manager)
  assert_eq(default_locale.language, "en")
  assert_eq(default_locale.region, "US")
  
  // Test locale setting
  let french_locale = Locale::new("fr", "FR")
  LocaleManager::set_current_locale(locale_manager, french_locale)
  
  let current_locale = LocaleManager::get_current_locale(locale_manager)
  assert_eq(current_locale.language, "fr")
  assert_eq(current_locale.region, "FR")
  
  // Test locale fallback chain
  LocaleManager::add_fallback_locale(locale_manager, Locale::new("en", "US"))
  LocaleManager::add_fallback_locale(locale_manager, Locale::new("en", ""))
  
  let fallback_chain = LocaleManager::get_fallback_chain(locale_manager)
  assert_eq(fallback_chain.length(), 3)
  assert_eq(fallback_chain[0].language, "fr")
  assert_eq(fallback_chain[0].region, "FR")
  assert_eq(fallback_chain[1].language, "en")
  assert_eq(fallback_chain[1].region, "US")
  assert_eq(fallback_chain[2].language, "en")
  assert_eq(fallback_chain[2].region, "")
  
  // Test locale parsing from string
  let parsed_locale = Locale::from_string("zh-CN")
  match parsed_locale {
    Some(locale) => {
      assert_eq(locale.language, "zh")
      assert_eq(locale.region, "CN")
    }
    None => assert_true(false) // Should parse successfully
  }
  
  let parsed_locale2 = Locale::from_string("de")
  match parsed_locale2 {
    Some(locale) => {
      assert_eq(locale.language, "de")
      assert_eq(locale.region, "")
    }
    None => assert_true(false) // Should parse successfully
  }
  
  let invalid_locale = Locale::from_string("invalid-locale-string")
  match invalid_locale {
    Some(_) => assert_true(false) // Should not parse
    None => assert_true(true)
  }
  
  // Test locale matching
  let request_locale = Locale::new("en", "GB")
  let supported_locales = [
    Locale::new("en", "US"),
    Locale::new("fr", "FR"),
    Locale::new("de", "DE")
  ]
  
  let matched_locale = LocaleManager::find_best_match(locale_manager, request_locale, supported_locales)
  match matched_locale {
    Some(locale) => {
      assert_eq(locale.language, "en") // Should match language even if region differs
      assert_eq(locale.region, "US")
    }
    None => assert_true(false) // Should find a match
  }
}

// Test 2: Message Translation
test "message translation" {
  let translator = MessageTranslator::new()
  
  // Add translations for different locales
  MessageTranslator::add_translation(translator, "telemetry.metrics.cpu", "en", "CPU Usage")
  MessageTranslator::add_translation(translator, "telemetry.metrics.cpu", "fr", "Utilisation du CPU")
  MessageTranslator::add_translation(translator, "telemetry.metrics.cpu", "de", "CPU-Auslastung")
  MessageTranslator::add_translation(translator, "telemetry.metrics.cpu", "zh", "CPU使用率")
  
  MessageTranslator::add_translation(translator, "telemetry.metrics.memory", "en", "Memory Usage")
  MessageTranslator::add_translation(translator, "telemetry.metrics.memory", "fr", "Utilisation de la mémoire")
  MessageTranslator::add_translation(translator, "telemetry.metrics.memory", "de", "Speichernutzung")
  MessageTranslator::add_translation(translator, "telemetry.metrics.memory", "zh", "内存使用率")
  
  // Test translation for different locales
  let en_translation = MessageTranslator::translate(translator, "telemetry.metrics.cpu", Locale::new("en", "US"))
  match en_translation {
    Some(text) => assert_eq(text, "CPU Usage")
    None => assert_true(false) // Should translate
  }
  
  let fr_translation = MessageTranslator::translate(translator, "telemetry.metrics.cpu", Locale::new("fr", "FR"))
  match fr_translation {
    Some(text) => assert_eq(text, "Utilisation du CPU")
    None => assert_true(false) // Should translate
  }
  
  let de_translation = MessageTranslator::translate(translator, "telemetry.metrics.cpu", Locale::new("de", "DE"))
  match de_translation {
    Some(text) => assert_eq(text, "CPU-Auslastung")
    None => assert_true(false) // Should translate
  }
  
  let zh_translation = MessageTranslator::translate(translator, "telemetry.metrics.cpu", Locale::new("zh", "CN"))
  match zh_translation {
    Some(text) => assert_eq(text, "CPU使用率")
    None => assert_true(false) // Should translate
  }
  
  // Test fallback translation
  let fallback_locale = Locale::new("es", "ES") // Spanish not directly supported
  let fallback_translation = MessageTranslator::translate(translator, "telemetry.metrics.cpu", fallback_locale)
  match fallback_translation {
    Some(text) => assert_eq(text, "CPU Usage") // Should fallback to English
    None => assert_true(false) // Should translate
  }
  
  // Test translation with parameters
  MessageTranslator::add_translation(translator, "telemetry.alert.threshold", "en", "Alert: {metric} is {value}% which exceeds the {threshold}% threshold")
  MessageTranslator::add_translation(translator, "telemetry.alert.threshold", "fr", "Alerte : {metric} est à {value}% ce qui dépasse le seuil de {threshold}%")
  
  let en_param_translation = MessageTranslator::translate_with_params(
    translator, 
    "telemetry.alert.threshold", 
    Locale::new("en", "US"),
    [
      ("metric", "CPU Usage"),
      ("value", "85"),
      ("threshold", "80")
    ]
  )
  
  match en_param_translation {
    Some(text) => assert_eq(text, "Alert: CPU Usage is 85% which exceeds the 80% threshold")
    None => assert_true(false) // Should translate
  }
  
  let fr_param_translation = MessageTranslator::translate_with_params(
    translator, 
    "telemetry.alert.threshold", 
    Locale::new("fr", "FR"),
    [
      ("metric", "Utilisation du CPU"),
      ("value", "85"),
      ("threshold", "80")
    ]
  )
  
  match fr_param_translation {
    Some(text) => assert_eq(text, "Alerte : Utilisation du CPU est à 85% ce qui dépasse le seuil de 80%")
    None => assert_true(false) // Should translate
  }
  
  // Test pluralization
  MessageTranslator::add_plural_translation(translator, "telemetry.item.count", "en", "1 item", "{count} items")
  MessageTranslator::add_plural_translation(translator, "telemetry.item.count", "fr", "1 élément", "{count} éléments")
  
  let en_singular = MessageTranslator::translate_plural(translator, "telemetry.item.count", Locale::new("en", "US"), 1)
  match en_singular {
    Some(text) => assert_eq(text, "1 item")
    None => assert_true(false) // Should translate
  }
  
  let en_plural = MessageTranslator::translate_plural(translator, "telemetry.item.count", Locale::new("en", "US"), 5)
  match en_plural {
    Some(text) => assert_eq(text, "5 items")
    None => assert_true(false) // Should translate
  }
  
  let fr_singular = MessageTranslator::translate_plural(translator, "telemetry.item.count", Locale::new("fr", "FR"), 1)
  match fr_singular {
    Some(text) => assert_eq(text, "1 élément")
    None => assert_true(false) // Should translate
  }
  
  let fr_plural = MessageTranslator::translate_plural(translator, "telemetry.item.count", Locale::new("fr", "FR"), 5)
  match fr_plural {
    Some(text) => assert_eq(text, "5 éléments")
    None => assert_true(false) // Should translate
  }
}

// Test 3: Date and Time Formatting
test "date and time formatting" {
  let date_formatter = DateFormatter::new()
  
  // Create a timestamp for testing
  let timestamp = 1634567890L // October 18, 2021 15:38:10 UTC
  
  // Test date formatting for different locales
  let en_date = DateFormatter::format_date(date_formatter, timestamp, Locale::new("en", "US"))
  assert_eq(en_date, "10/18/2021") // MM/DD/YYYY format
  
  let fr_date = DateFormatter::format_date(date_formatter, timestamp, Locale::new("fr", "FR"))
  assert_eq(fr_date, "18/10/2021") // DD/MM/YYYY format
  
  let de_date = DateFormatter::format_date(date_formatter, timestamp, Locale::new("de", "DE"))
  assert_eq(de_date, "18.10.2021") // DD.MM.YYYY format
  
  let zh_date = DateFormatter::format_date(date_formatter, timestamp, Locale::new("zh", "CN"))
  assert_eq(zh_date, "2021年10月18日") // YYYY年MM月DD日 format
  
  // Test time formatting for different locales
  let en_time = DateFormatter::format_time(date_formatter, timestamp, Locale::new("en", "US"))
  assert_eq(en_time, "3:38:10 PM") // 12-hour format with AM/PM
  
  let fr_time = DateFormatter::format_time(date_formatter, timestamp, Locale::new("fr", "FR"))
  assert_eq(fr_time, "15:38:10") // 24-hour format
  
  let de_time = DateFormatter::format_time(date_formatter, timestamp, Locale::new("de", "DE"))
  assert_eq(de_time, "15:38:10") // 24-hour format
  
  let zh_time = DateFormatter::format_time(date_formatter, timestamp, Locale::new("zh", "CN"))
  assert_eq(zh_time, "下午3:38:10") // 12-hour format with AM/PM in Chinese
  
  // Test date-time formatting for different locales
  let en_datetime = DateFormatter::format_datetime(date_formatter, timestamp, Locale::new("en", "US"))
  assert_eq(en_datetime, "10/18/2021, 3:38:10 PM")
  
  let fr_datetime = DateFormatter::format_datetime(date_formatter, timestamp, Locale::new("fr", "FR"))
  assert_eq(fr_datetime, "18/10/2021 à 15:38:10")
  
  let de_datetime = DateFormatter::format_datetime(date_formatter, timestamp, Locale::new("de", "DE"))
  assert_eq(de_datetime, "18.10.2021, 15:38:10")
  
  let zh_datetime = DateFormatter::format_datetime(date_formatter, timestamp, Locale::new("zh", "CN"))
  assert_eq(zh_datetime, "2021年10月18日 下午3:38:10")
  
  // Test relative time formatting
  let now = 1634567890L
  let past = now - 3600L // 1 hour ago
  let future = now + 86400L // 1 day in the future
  
  let en_relative_past = DateFormatter::format_relative(date_formatter, past, now, Locale::new("en", "US"))
  assert_eq(en_relative_past, "1 hour ago")
  
  let en_relative_future = DateFormatter::format_relative(date_formatter, future, now, Locale::new("en", "US"))
  assert_eq(en_relative_future, "in 1 day")
  
  let fr_relative_past = DateFormatter::format_relative(date_formatter, past, now, Locale::new("fr", "FR"))
  assert_eq(fr_relative_past, "il y a 1 heure")
  
  let fr_relative_future = DateFormatter::format_relative(date_formatter, future, now, Locale::new("fr", "FR"))
  assert_eq(fr_relative_future, "dans 1 jour")
  
  let de_relative_past = DateFormatter::format_relative(date_formatter, past, now, Locale::new("de", "DE"))
  assert_eq(de_relative_past, "vor 1 Stunde")
  
  let de_relative_future = DateFormatter::format_relative(date_formatter, future, now, Locale::new("de", "DE"))
  assert_eq(de_relative_future, "in 1 Tag")
  
  let zh_relative_past = DateFormatter::format_relative(date_formatter, past, now, Locale::new("zh", "CN"))
  assert_eq(zh_relative_past, "1小时前")
  
  let zh_relative_future = DateFormatter::format_relative(date_formatter, future, now, Locale::new("zh", "CN"))
  assert_eq(zh_relative_future, "1天后")
}

// Test 4: Number Formatting
test "number formatting" {
  let number_formatter = NumberFormatter::new()
  
  // Test decimal formatting for different locales
  let en_decimal = NumberFormatter::format_decimal(number_formatter, 1234.5678, Locale::new("en", "US"))
  assert_eq(en_decimal, "1,234.568") // Comma as thousands separator, dot as decimal
  
  let fr_decimal = NumberFormatter::format_decimal(number_formatter, 1234.5678, Locale::new("fr", "FR"))
  assert_eq(fr_decimal, "1 234,568") // Space as thousands separator, comma as decimal
  
  let de_decimal = NumberFormatter::format_decimal(number_formatter, 1234.5678, Locale::new("de", "DE"))
  assert_eq(de_decimal, "1.234,568") // Dot as thousands separator, comma as decimal
  
  let zh_decimal = NumberFormatter::format_decimal(number_formatter, 1234.5678, Locale::new("zh", "CN"))
  assert_eq(zh_decimal, "1,234.568") // Comma as thousands separator, dot as decimal
  
  // Test percentage formatting for different locales
  let en_percentage = NumberFormatter::format_percentage(number_formatter, 0.7543, Locale::new("en", "US"))
  assert_eq(en_percentage, "75.43%")
  
  let fr_percentage = NumberFormatter::format_percentage(number_formatter, 0.7543, Locale::new("fr", "FR"))
  assert_eq(fr_percentage, "75,43 %")
  
  let de_percentage = NumberFormatter::format_percentage(number_formatter, 0.7543, Locale::new("de", "DE"))
  assert_eq(de_percentage, "75,43 %")
  
  let zh_percentage = NumberFormatter::format_percentage(number_formatter, 0.7543, Locale::new("zh", "CN"))
  assert_eq(zh_percentage, "75.43%")
  
  // Test currency formatting for different locales
  let en_currency = NumberFormatter::format_currency(number_formatter, 1234.56, Locale::new("en", "US"), "USD")
  assert_eq(en_currency, "$1,234.56")
  
  let fr_currency = NumberFormatter::format_currency(number_formatter, 1234.56, Locale::new("fr", "FR"), "EUR")
  assert_eq(fr_currency, "1 234,56 €")
  
  let de_currency = NumberFormatter::format_currency(number_formatter, 1234.56, Locale::new("de", "DE"), "EUR")
  assert_eq(de_currency, "1.234,56 €")
  
  let zh_currency = NumberFormatter::format_currency(number_formatter, 1234.56, Locale::new("zh", "CN"), "CNY")
  assert_eq(zh_currency, "¥1,234.56")
  
  // Test unit formatting for different locales
  let en_bytes = NumberFormatter::format_bytes(number_formatter, 1572864, Locale::new("en", "US"))
  assert_eq(en_bytes, "1.5 MB")
  
  let fr_bytes = NumberFormatter::format_bytes(number_formatter, 1572864, Locale::new("fr", "FR"))
  assert_eq(fr_bytes, "1,5 Mo")
  
  let de_bytes = NumberFormatter::format_bytes(number_formatter, 1572864, Locale::new("de", "DE"))
  assert_eq(de_bytes, "1,5 MB")
  
  let zh_bytes = NumberFormatter::format_bytes(number_formatter, 1572864, Locale::new("zh", "CN"))
  assert_eq(zh_bytes, "1.5 MB")
  
  // Test formatting with custom precision
  let en_custom = NumberFormatter::format_with_precision(number_formatter, 1234.5678, 2, Locale::new("en", "US"))
  assert_eq(en_custom, "1,234.57")
  
  let fr_custom = NumberFormatter::format_with_precision(number_formatter, 1234.5678, 2, Locale::new("fr", "FR"))
  assert_eq(fr_custom, "1 234,57")
  
  // Test scientific notation
  let en_scientific = NumberFormatter::format_scientific(number_formatter, 1234567.89, Locale::new("en", "US"))
  assert_eq(en_scientific, "1.235E6")
  
  let fr_scientific = NumberFormatter::format_scientific(number_formatter, 1234567.89, Locale::new("fr", "FR"))
  assert_eq(fr_scientific, "1,235E6")
}

// Test 5: Text Direction and Right-to-Left Support
test "text direction and right to left support" {
  let text_processor = TextProcessor::new()
  
  // Test text direction detection
  let ltr_text = "Hello World"
  let rtl_text = "مرحبا بالعالم"
  let mixed_text = "Hello مرحبا World"
  
  assert_eq(TextProcessor::get_direction(text_processor, ltr_text), LTR)
  assert_eq(TextProcessor::get_direction(text_processor, rtl_text), RTL)
  assert_eq(TextProcessor::get_direction(text_processor, mixed_text), Mixed)
  
  // Test text alignment for different locales
  let en_alignment = TextProcessor::get_alignment(text_processor, Locale::new("en", "US"))
  assert_eq(en_alignment, Left)
  
  let ar_alignment = TextProcessor::get_alignment(text_processor, Locale::new("ar", "SA"))
  assert_eq(ar_alignment, Right)
  
  let he_alignment = TextProcessor::get_alignment(text_processor, Locale::new("he", "IL"))
  assert_eq(he_alignment, Right)
  
  let zh_alignment = TextProcessor::get_alignment(text_processor, Locale::new("zh", "CN"))
  assert_eq(zh_alignment, Left)
  
  // Test bidirectional text processing
  let bidi_text = "The title is 'العنوان' in Arabic"
  let processed_bidi = TextProcessor::process_bidi(text_processor, bidi_text)
  
  // Verify the text is properly processed for display
  assert_true(String::contains(processed_bidi, "The title is"))
  assert_true(String::contains(processed_bidi, "العنوان"))
  assert_true(String::contains(processed_bidi, "in Arabic"))
  
  // Test text wrapping for RTL languages
  let rtl_long_text = "هذا نص طويل جدًا يحتاج إلى التفاف مناسبة لعرضه بشكل صحيح في الواجهة من اليمين إلى اليسار"
  
  let wrapped_rtl = TextProcessor::wrap_text(text_processor, rtl_long_text, 20, Locale::new("ar", "SA"))
  
  // Verify the text is wrapped correctly
  let lines = String::split(wrapped_rtl, "\n")
  assert_true(lines.length() > 1) // Should be wrapped to multiple lines
  
  for line in lines {
    assert_true(line.length() <= 20) // Each line should not exceed the limit
  }
  
  // Test text ordering for mixed content
  let mixed_content = "English 123 العربية 456 עברית 789"
  let ordered_mixed = TextProcessor::order_mixed_text(text_processor, mixed_content)
  
  // Verify the text is ordered correctly for display
  assert_true(String::contains(ordered_mixed, "English"))
  assert_true(String::contains(ordered_mixed, "123"))
  assert_true(String::contains(ordered_mixed, "العربية"))
  assert_true(String::contains(ordered_mixed, "456"))
  assert_true(String::contains(ordered_mixed, "עברית"))
  assert_true(String::contains(ordered_mixed, "789"))
}

// Test 6: Character Encoding and Unicode Support
test "character encoding and unicode support" {
  let encoding_processor = EncodingProcessor::new()
  
  // Test UTF-8 encoding and decoding
  let unicode_text = "Hello 世界 🌍 مرحبا שלום"
  let utf8_bytes = EncodingProcessor::encode_utf8(encoding_processor, unicode_text)
  let decoded_text = EncodingProcessor::decode_utf8(encoding_processor, utf8_bytes)
  
  assert_eq(decoded_text, unicode_text)
  
  // Test different character sets
  let latin_text = "Café résumé naïve"
  let latin_bytes = EncodingProcessor::encode_latin1(encoding_processor, latin_text)
  let decoded_latin = EncodingProcessor::decode_latin1(encoding_processor, latin_bytes)
  
  assert_eq(decoded_latin, latin_text)
  
  // Test emoji and symbol support
  let emoji_text = "📊 📈 📉 📋 📌 📍 📎 📏"
  let emoji_bytes = EncodingProcessor::encode_utf8(encoding_processor, emoji_text)
  let decoded_emoji = EncodingProcessor::decode_utf8(encoding_processor, emoji_bytes)
  
  assert_eq(decoded_emoji, emoji_text)
  
  // Test normalization
  let composed_text = "é" // Composed form (e + combining acute accent)
  let decomposed_text = "é" // Decomposed form (e + combining acute accent)
  
  let normalized_composed = EncodingProcessor::normalize_nfc(encoding_processor, decomposed_text)
  let normalized_decomposed = EncodingProcessor::normalize_nfd(encoding_processor, composed_text)
  
  assert_eq(normalized_composed, composed_text)
  assert_eq(normalized_decomposed, decomposed_text)
  
  // Test case conversion for different locales
  let turkish_text = "İstanbul"
  let turkish_lower = EncodingProcessor::to_lower_case(encoding_processor, turkish_text, Locale::new("tr", "TR"))
  assert_eq(turkish_lower, "i̇stanbul") // Turkish dotted lowercase i
  
  let turkish_upper = EncodingProcessor::to_upper_case(encoding_processor, turkish_lower, Locale::new("tr", "TR"))
  assert_eq(turkish_upper, "İSTANBUL") // Turkish dotted uppercase I
  
  // Test string length calculation with grapheme clusters
  let grapheme_text = "e\u0301" // 'e' + combining acute accent
  let byte_length = String::length(grapheme_text)
  let grapheme_length = EncodingProcessor::grapheme_length(encoding_processor, grapheme_text)
  
  assert_eq(byte_length, 2) // Two bytes/characters
  assert_eq(grapheme_length, 1) // One grapheme cluster
  
  // Test text search with diacritics
  let text_with_diacritics = "Café résumé naïve façade"
  let search_term = "cafe"
  
  let found = EncodingProcessor::contains_ignoring_diacritics(encoding_processor, text_with_diacritics, search_term)
  assert_true(found)
  
  // Test collation (sorting) for different locales
  let strings = ["apple", "banana", "cherry", "éclair", "façade"]
  
  let en_sorted = EncodingProcessor::sort_strings(encoding_processor, strings, Locale::new("en", "US"))
  let fr_sorted = EncodingProcessor::sort_strings(encoding_processor, strings, Locale::new("fr", "FR"))
  
  // English sorting: éclair should come after eclair if it existed
  assert_eq(en_sorted[0], "apple")
  assert_eq(en_sorted[3], "éclair")
  
  // French sorting: éclair should come before eclair if it existed
  assert_eq(fr_sorted[0], "apple")
  assert_eq(fr_sorted[3], "éclair")
}

// Test 7: Resource Bundle Management
test "resource bundle management" {
  let bundle_manager = ResourceBundleManager::new()
  
  // Create resource bundles for different locales
  let en_bundle = ResourceBundle::new(Locale::new("en", "US"))
  ResourceBundle::add_string(en_bundle, "welcome.message", "Welcome to the Telemetry System")
  ResourceBundle::add_string(en_bundle, "error.connection", "Connection failed")
  ResourceBundle::add_string(en_bundle, "button.save", "Save")
  ResourceBundle::add_string(en_bundle, "button.cancel", "Cancel")
  
  let fr_bundle = ResourceBundle::new(Locale::new("fr", "FR"))
  ResourceBundle::add_string(fr_bundle, "welcome.message", "Bienvenue dans le système de télémétrie")
  ResourceBundle::add_string(fr_bundle, "error.connection", "Échec de connexion")
  ResourceBundle::add_string(fr_bundle, "button.save", "Enregistrer")
  ResourceBundle::add_string(fr_bundle, "button.cancel", "Annuler")
  
  let de_bundle = ResourceBundle::new(Locale::new("de", "DE"))
  ResourceBundle::add_string(de_bundle, "welcome.message", "Willkommen im Telemetriesystem")
  ResourceBundle::add_string(de_bundle, "error.connection", "Verbindung fehlgeschlagen")
  ResourceBundle::add_string(de_bundle, "button.save", "Speichern")
  ResourceBundle::add_string(de_bundle, "button.cancel", "Abbrechen")
  
  // Register resource bundles
  ResourceBundleManager::register_bundle(bundle_manager, en_bundle)
  ResourceBundleManager::register_bundle(bundle_manager, fr_bundle)
  ResourceBundleManager::register_bundle(bundle_manager, de_bundle)
  
  // Test resource retrieval for different locales
  let en_welcome = ResourceBundleManager::get_string(bundle_manager, "welcome.message", Locale::new("en", "US"))
  match en_welcome {
    Some(text) => assert_eq(text, "Welcome to the Telemetry System")
    None => assert_true(false) // Should find resource
  }
  
  let fr_welcome = ResourceBundleManager::get_string(bundle_manager, "welcome.message", Locale::new("fr", "FR"))
  match fr_welcome {
    Some(text) => assert_eq(text, "Bienvenue dans le système de télémétrie")
    None => assert_true(false) // Should find resource
  }
  
  let de_welcome = ResourceBundleManager::get_string(bundle_manager, "welcome.message", Locale::new("de", "DE"))
  match de_welcome {
    Some(text) => assert_eq(text, "Willkommen im Telemetriesystem")
    None => assert_true(false) // Should find resource
  }
  
  // Test fallback when resource is not found in specific locale
  let es_welcome = ResourceBundleManager::get_string(bundle_manager, "welcome.message", Locale::new("es", "ES"))
  match es_welcome {
    Some(text) => assert_eq(text, "Welcome to the Telemetry System") // Should fallback to English
    None => assert_true(false) // Should find resource
  }
  
  // Test when resource key doesn't exist
  let missing_resource = ResourceBundleManager::get_string(bundle_manager, "non.existent.key", Locale::new("en", "US"))
  match missing_resource {
    Some(_) => assert_true(false) // Should not find resource
    None => assert_true(true)
  }
  
  // Test loading resource bundles from files
  let file_bundle_result = ResourceBundleManager::load_bundle_from_file(bundle_manager, "locales/messages", Locale::new("ja", "JP"))
  match file_bundle_result {
    Ok(_) => assert_true(true) // Should load successfully
    Error(_) => assert_true(false) // Should succeed
  }
  
  // Test resource bundle inheritance
  let parent_bundle = ResourceBundle::new(Locale::new("en", ""))
  ResourceBundle::add_string(parent_bundle, "common.ok", "OK")
  ResourceBundle::add_string(parent_bundle, "common.error", "Error")
  
  let child_bundle = ResourceBundle::new(Locale::new("en", "US"))
  ResourceBundle::add_string(child_bundle, "common.cancel", "Cancel")
  ResourceBundle::set_parent(child_bundle, parent_bundle)
  
  ResourceBundleManager::register_bundle(bundle_manager, child_bundle)
  
  let us_ok = ResourceBundleManager::get_string(bundle_manager, "common.ok", Locale::new("en", "US"))
  match us_ok {
    Some(text) => assert_eq(text, "OK") // Should inherit from parent
    None => assert_true(false) // Should find resource
  }
  
  let us_cancel = ResourceBundleManager::get_string(bundle_manager, "common.cancel", Locale::new("en", "US"))
  match us_cancel {
    Some(text) => assert_eq(text, "Cancel") // Should use child's value
    None => assert_true(false) // Should find resource
  }
}

// Test 8: Internationalized UI Components
test "internationalized ui components" {
  let ui_localizer = UILocalizer::new()
  
  // Test localized button
  let button = Button::new("save")
  UILocalizer::localize_button(ui_localizer, button, Locale::new("en", "US"))
  assert_eq(Button::get_text(button), "Save")
  
  UILocalizer::localize_button(ui_localizer, button, Locale::new("fr", "FR"))
  assert_eq(Button::get_text(button), "Enregistrer")
  
  UILocalizer::localize_button(ui_localizer, button, Locale::new("de", "DE"))
  assert_eq(Button::get_text(button), "Speichern")
  
  // Test localized label
  let label = Label::new("cpu_usage")
  UILocalizer::localize_label(ui_localizer, label, Locale::new("en", "US"))
  assert_eq(Label::get_text(label), "CPU Usage")
  
  UILocalizer::localize_label(ui_localizer, label, Locale::new("fr", "FR"))
  assert_eq(Label::get_text(label), "Utilisation du CPU")
  
  UILocalizer::localize_label(ui_localizer, label, Locale::new("de", "DE"))
  assert_eq(Label::get_text(label), "CPU-Auslastung")
  
  // Test localized table headers
  let table = Table::new()
  Table::add_column(table, "metric")
  Table::add_column(table, "value")
  Table::add_column(table, "timestamp")
  
  UILocalizer::localize_table(ui_localizer, table, Locale::new("en", "US"))
  assert_eq(Table::get_header_text(table, 0), "Metric")
  assert_eq(Table::get_header_text(table, 1), "Value")
  assert_eq(Table::get_header_text(table, 2), "Timestamp")
  
  UILocalizer::localize_table(ui_localizer, table, Locale::new("fr", "FR"))
  assert_eq(Table::get_header_text(table, 0), "Métrique")
  assert_eq(Table::get_header_text(table, 1), "Valeur")
  assert_eq(Table::get_header_text(table, 2), "Horodatage")
  
  UILocalizer::localize_table(ui_localizer, table, Locale::new("de", "DE"))
  assert_eq(Table::get_header_text(table, 0), "Metrik")
  assert_eq(Table::get_header_text(table, 1), "Wert")
  assert_eq(Table::get_header_text(table, 2), "Zeitstempel")
  
  // Test localized error messages
  let error_dialog = ErrorDialog::new("connection_failed")
  UILocalizer::localize_error_dialog(ui_localizer, error_dialog, Locale::new("en", "US"))
  assert_eq(ErrorDialog::get_title(error_dialog), "Connection Failed")
  assert_eq(ErrorDialog::get_message(error_dialog), "Unable to connect to the telemetry service. Please check your network connection and try again.")
  
  UILocalizer::localize_error_dialog(ui_localizer, error_dialog, Locale::new("fr", "FR"))
  assert_eq(ErrorDialog::get_title(error_dialog), "Échec de Connexion")
  assert_eq(ErrorDialog::get_message(error_dialog), "Impossible de se connecter au service de télémétrie. Veuillez vérifier votre connexion réseau et réessayer.")
  
  UILocalizer::localize_error_dialog(ui_localizer, error_dialog, Locale::new("de", "DE"))
  assert_eq(ErrorDialog::get_title(error_dialog), "Verbindung Fehlgeschlagen")
  assert_eq(ErrorDialog::get_message(error_dialog), "Verbindung zum Telemetriedienst nicht möglich. Bitte überprüfen Sie Ihre Netzwerkverbindung und versuchen Sie es erneut.")
  
  // Test localized menu
  let menu = Menu::new()
  Menu::add_item(menu, "file")
  Menu::add_item(menu, "edit")
  Menu::add_item(menu, "view")
  Menu::add_item(menu, "help")
  
  UILocalizer::localize_menu(ui_localizer, menu, Locale::new("en", "US"))
  assert_eq(Menu::get_item_text(menu, 0), "File")
  assert_eq(Menu::get_item_text(menu, 1), "Edit")
  assert_eq(Menu::get_item_text(menu, 2), "View")
  assert_eq(Menu::get_item_text(menu, 3), "Help")
  
  UILocalizer::localize_menu(ui_localizer, menu, Locale::new("fr", "FR"))
  assert_eq(Menu::get_item_text(menu, 0), "Fichier")
  assert_eq(Menu::get_item_text(menu, 1), "Éditer")
  assert_eq(Menu::get_item_text(menu, 2), "Affichage")
  assert_eq(Menu::get_item_text(menu, 3), "Aide")
  
  UILocalizer::localize_menu(ui_localizer, menu, Locale::new("de", "DE"))
  assert_eq(Menu::get_item_text(menu, 0), "Datei")
  assert_eq(Menu::get_item_text(menu, 1), "Bearbeiten")
  assert_eq(Menu::get_item_text(menu, 2), "Ansicht")
  assert_eq(Menu::get_item_text(menu, 3), "Hilfe")
  
  // Test UI layout adaptation for RTL languages
  let rtl_panel = Panel::new()
  Panel::add_button(rtl_panel, "left_button")
  Panel::add_button(rtl_panel, "right_button")
  
  UILocalizer::localize_panel(ui_localizer, rtl_panel, Locale::new("en", "US"))
  assert_eq(Panel::get_button_order(rtl_panel), ["left_button", "right_button"]) // LTR order
  
  UILocalizer::localize_panel(ui_localizer, rtl_panel, Locale::new("ar", "SA"))
  assert_eq(Panel::get_button_order(rtl_panel), ["right_button", "left_button"]) // RTL order
}

// Test 9: Internationalized Telemetry Data
test "internationalized telemetry data" {
  let telemetry_localizer = TelemetryLocalizer::new()
  
  // Create telemetry data with internationalized values
  let telemetry_data = InternationalizedTelemetryData::new("cpu_usage")
  InternationalizedTelemetryData::set_localized_value(telemetry_data, "en", "CPU Usage")
  InternationalizedTelemetryData::set_localized_value(telemetry_data, "fr", "Utilisation du CPU")
  InternationalizedTelemetryData::set_localized_value(telemetry_data, "de", "CPU-Auslastung")
  InternationalizedTelemetryData::set_localized_value(telemetry_data, "zh", "CPU使用率")
  
  // Test retrieving localized values
  let en_value = TelemetryLocalizer::get_localized_value(telemetry_localizer, telemetry_data, Locale::new("en", "US"))
  match en_value {
    Some(value) => assert_eq(value, "CPU Usage")
    None => assert_true(false) // Should find value
  }
  
  let fr_value = TelemetryLocalizer::get_localized_value(telemetry_localizer, telemetry_data, Locale::new("fr", "FR"))
  match fr_value {
    Some(value) => assert_eq(value, "Utilisation du CPU")
    None => assert_true(false) // Should find value
  }
  
  let de_value = TelemetryLocalizer::get_localized_value(telemetry_localizer, telemetry_data, Locale::new("de", "DE"))
  match de_value {
    Some(value) => assert_eq(value, "CPU-Auslastung")
    None => assert_true(false) // Should find value
  }
  
  let zh_value = TelemetryLocalizer::get_localized_value(telemetry_localizer, telemetry_data, Locale::new("zh", "CN"))
  match zh_value {
    Some(value) => assert_eq(value, "CPU使用率")
    None => assert_true(false) // Should find value
  }
  
  // Test fallback when specific locale is not available
  let es_value = TelemetryLocalizer::get_localized_value(telemetry_localizer, telemetry_data, Locale::new("es", "ES"))
  match es_value {
    Some(value) => assert_eq(value, "CPU Usage") // Should fallback to English
    None => assert_true(false) // Should find value
  }
  
  // Test internationalized alert messages
  let alert_data = InternationalizedTelemetryData::new("high_cpu_alert")
  InternationalizedTelemetryData::set_localized_value(alert_data, "en", "High CPU Usage Alert: CPU usage is {value}%, which exceeds the threshold of {threshold}%")
  InternationalizedTelemetryData::set_localized_value(alert_data, "fr", "Alerte d'utilisation élevée du CPU : l'utilisation du CPU est de {value}%, ce qui dépasse le seuil de {threshold}%")
  InternationalizedTelemetryData::set_localized_value(alert_data, "de", "Warnung bei hoher CPU-Auslastung: CPU-Auslastung ist {value}%, was den Schwellenwert von {threshold}% übersteigt")
  
  let en_alert = TelemetryLocalizer::format_localized_alert(
    telemetry_localizer, 
    alert_data, 
    Locale::new("en", "US"),
    [("value", "85"), ("threshold", "80")]
  )
  match en_alert {
    Some(message) => assert_eq(message, "High CPU Usage Alert: CPU usage is 85%, which exceeds the threshold of 80%")
    None => assert_true(false) // Should format alert
  }
  
  let fr_alert = TelemetryLocalizer::format_localized_alert(
    telemetry_localizer, 
    alert_data, 
    Locale::new("fr", "FR"),
    [("value", "85"), ("threshold", "80")]
  )
  match fr_alert {
    Some(message) => assert_eq(message, "Alerte d'utilisation élevée du CPU : l'utilisation du CPU est de 85%, ce qui dépasse le seuil de 80%")
    None => assert_true(false) // Should format alert
  }
  
  let de_alert = TelemetryLocalizer::format_localized_alert(
    telemetry_localizer, 
    alert_data, 
    Locale::new("de", "DE"),
    [("value", "85"), ("threshold", "80")]
  )
  match de_alert {
    Some(message) => assert_eq(message, "Warnung bei hoher CPU-Auslastung: CPU-Auslastung ist 85%, was den Schwellenwert von 80% übersteigt")
    None => assert_true(false) // Should format alert
  }
  
  // Test internationalized chart labels
  let chart_data = InternationalizedTelemetryData::new("chart")
  InternationalizedTelemetryData::set_localized_value(chart_data, "en", "CPU Usage Over Time")
  InternationalizedTelemetryData::set_localized_value(chart_data, "fr", "Utilisation du CPU au fil du temps")
  InternationalizedTelemetryData::set_localized_value(chart_data, "de", "CPU-Auslastung über Zeit")
  
  let chart = Chart::new()
  Chart::set_title(chart, TelemetryLocalizer::get_localized_value(telemetry_localizer, chart_data, Locale::new("en", "US")).unwrap())
  assert_eq(Chart::get_title(chart), "CPU Usage Over Time")
  
  Chart::set_title(chart, TelemetryLocalizer::get_localized_value(telemetry_localizer, chart_data, Locale::new("fr", "FR")).unwrap())
  assert_eq(Chart::get_title(chart), "Utilisation du CPU au fil du temps")
  
  Chart::set_title(chart, TelemetryLocalizer::get_localized_value(telemetry_localizer, chart_data, Locale::new("de", "DE")).unwrap())
  assert_eq(Chart::get_title(chart), "CPU-Auslastung über Zeit")
  
  // Test internationalized report generation
  let report_data = InternationalizedTelemetryData::new("report")
  InternationalizedTelemetryData::set_localized_value(report_data, "en", "Telemetry Report")
  InternationalizedTelemetryData::set_localized_value(report_data, "fr", "Rapport de télémétrie")
  InternationalizedTelemetryData::set_localized_value(report_data, "de", "Telemetriebericht")
  
  let en_report = TelemetryLocalizer::generate_report(telemetry_localizer, report_data, Locale::new("en", "US"))
  assert_true(String::contains(en_report, "Telemetry Report"))
  
  let fr_report = TelemetryLocalizer::generate_report(telemetry_localizer, report_data, Locale::new("fr", "FR"))
  assert_true(String::contains(fr_report, "Rapport de télémétrie"))
  
  let de_report = TelemetryLocalizer::generate_report(telemetry_localizer, report_data, Locale::new("de", "DE"))
  assert_true(String::contains(de_report, "Telemetriebericht"))
}

// Test 10: Performance and Memory Management
test "performance and memory management" {
  let i18n_manager = I18nManager::new()
  
  // Test resource bundle caching
  let start_time = Time::now()
  
  // Load resource bundles
  for i in 0..=100 {
    let locale = Locale::new("en", "US")
    I18nManager::get_string(i18n_manager, "welcome.message", locale)
  }
  
  let cached_time = Time::duration(start_time, Time::now())
  
  // Clear cache and measure again
  I18nManager::clear_cache(i18n_manager)
  
  let uncached_start = Time::now()
  
  for i in 0..=100 {
    let locale = Locale::new("en", "US")
    I18nManager::get_string(i18n_manager, "welcome.message", locale)
  }
  
  let uncached_time = Time::duration(uncached_start, Time::now())
  
  // Cached access should be faster
  assert_true(cached_time < uncached_time)
  
  // Test memory usage with multiple locales
  let initial_memory = MemoryStats::current_usage()
  
  // Load multiple resource bundles
  let locales = [
    Locale::new("en", "US"),
    Locale::new("fr", "FR"),
    Locale::new("de", "DE"),
    Locale::new("es", "ES"),
    Locale::new("it", "IT"),
    Locale::new("pt", "BR"),
    Locale::new("ru", "RU"),
    Locale::new("ja", "JP"),
    Locale::new("zh", "CN"),
    Locale::new("ko", "KR")
  ]
  
  for locale in locales {
    I18nManager::load_bundle(i18n_manager, locale)
  }
  
  let loaded_memory = MemoryStats::current_usage()
  let memory_increase = loaded_memory - initial_memory
  
  // Memory usage should be reasonable
  assert_true(memory_increase < 50 * 1024 * 1024) // Less than 50MB
  
  // Test lazy loading of resource bundles
  I18nManager::clear_cache(i18n_manager)
  I18nManager::enable_lazy_loading(i18n_manager)
  
  let lazy_start = Time::now()
  
  // Access resources for multiple locales
  for locale in locales {
    I18nManager::get_string(i18n_manager, "welcome.message", locale)
  }
  
  let lazy_time = Time::duration(lazy_start, Time::now())
  
  // Lazy loading should be efficient
  assert_true(lazy_time < uncached_time)
  
  // Test memory cleanup
  I18nManager::cleanup_unused_resources(i18n_manager)
  
  let cleaned_memory = MemoryStats::current_usage()
  let memory_reduced = loaded_memory - cleaned_memory
  
  // Memory should be reduced after cleanup
  assert_true(memory_reduced > 0)
  
  // Test concurrent access to internationalization resources
  let mut handles = []
  
  for i in 0..=10 {
    let handle = Thread::spawn(func() {
      let locale = Locale::new("en", "US")
      for j in 0..=100 {
        I18nManager::get_string(i18n_manager, "welcome.message", locale)
        I18nManager::format_date(i18n_manager, 1634567890L, locale)
        I18nManager::format_number(i18n_manager, 1234.56, locale)
      }
    })
    handles.push(handle)
  }
  
  // Wait for all threads to complete
  for handle in handles {
    Thread::join(handle)
  }
  
  // Concurrent access should not cause errors
  assert_true(true)
}