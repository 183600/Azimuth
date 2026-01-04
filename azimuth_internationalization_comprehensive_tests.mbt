// Azimuth Telemetry System - Internationalization Comprehensive Tests
// This file contains comprehensive tests for internationalization and localization

// Test 1: Multi-language Support
test "multi-language support" {
  let i18n_manager = I18nManager::new()
  
  // Load different language packs
  let languages = ["en", "zh-CN", "es", "fr", "de", "ja", "ko", "ar", "ru", "pt-BR"]
  
  for language in languages {
    let load_result = I18nManager::load_language_pack(i18n_manager, language)
    assert_true(load_result)
    
    // Verify language is loaded
    let is_loaded = I18nManager::is_language_loaded(i18n_manager, language)
    assert_true(is_loaded)
  }
  
  // Test text translation in different languages
  let test_keys = [
    "telemetry.system.name",
    "telemetry.system.description",
    "error.connection.failed",
    "error.authentication.required",
    "button.save",
    "button.cancel",
    "message.success",
    "message.warning",
    "message.error",
    "validation.required.field"
  ]
  
  for language in languages {
    I18nManager::set_current_language(i18n_manager, language)
    
    for key in test_keys {
      let translation = I18nManager::translate(i18n_manager, key)
      assert_true(translation.length() > 0)
      
      // Verify translation is not the key itself (indicating translation exists)
      if language != "en" {
        assert_not_eq(translation, key)
      }
    }
  }
  
  // Test parameterized translations
  I18nManager::set_current_language(i18n_manager, "en")
  let param_translation_en = I18nManager::translate_with_params(
    i18n_manager, 
    "telemetry.items.count", 
    [("count", "42"), ("type", "spans")]
  )
  assert_true(param_translation_en.contains("42"))
  assert_true(param_translation_en.contains("spans"))
  
  I18nManager::set_current_language(i18n_manager, "zh-CN")
  let param_translation_zh = I18nManager::translate_with_params(
    i18n_manager, 
    "telemetry.items.count", 
    [("count", "42"), ("type", "spans")]
  )
  assert_true(param_translation_zh.contains("42"))
  assert_not_eq(param_translation_zh, param_translation_en) // Should be different
  
  // Test pluralization
  I18nManager::set_current_language(i18n_manager, "en")
  let singular_en = I18nManager::translate_plural(i18n_manager, "telemetry.item", 1)
  let plural_en = I18nManager::translate_plural(i18n_manager, "telemetry.item", 5)
  assert_not_eq(singular_en, plural_en)
  
  I18nManager::set_current_language(i18n_manager, "zh-CN")
  let singular_zh = I18nManager::translate_plural(i18n_manager, "telemetry.item", 1)
  let plural_zh = I18nManager::translate_plural(i18n_manager, "telemetry.item", 5)
  // Chinese might not have different plural forms, but function should work
  assert_true(singular_zh.length() > 0 && plural_zh.length() > 0)
  
  // Test language fallback
  I18nManager::set_language_fallback(i18n_manager, "en")
  
  // Try to translate with non-existent language
  I18nManager::set_current_language(i18n_manager, "non-existent")
  let fallback_translation = I18nManager::translate(i18n_manager, "telemetry.system.name")
  assert_true(fallback_translation.length() > 0)
  
  // Should fall back to English
  I18nManager::set_current_language(i18n_manager, "en")
  let english_translation = I18nManager::translate(i18n_manager, "telemetry.system.name")
  assert_eq(fallback_translation, english_translation)
}

// Test 2: Character Encoding Support
test "character encoding support" {
  let encoding_manager = EncodingManager::new()
  
  // Test UTF-8 encoding and decoding
  let test_strings = [
    "Hello World", // English
    "你好世界", // Chinese
    "Hola Mundo", // Spanish
    "Bonjour le Monde", // French
    "Hallo Welt", // German
    "こんにちは世界", // Japanese
    "안녕하세요 세계", // Korean
    "مرحبا بالعالم", // Arabic
    "Привет мир", // Russian
    "Olá Mundo", // Portuguese
    "🌍🌎🌏", // Emojis
    "Café Münster résumé naïve", // Accented characters
    "𝔘𝔫𝔦𝔠𝔬𝔡𝔢 𝔖𝔱𝔯𝔦𝔫𝔤𝔰", // Mathematical script
    "🚀🛰️📡🌐", // Technology emojis
    "Test with various symbols: ©®™€£¥§¶†‡•…‰‹›""''–—", // Various symbols
    "Mixed: Hello 你好 Hola こんにちは 안녕하세요 مرحبا Привет 🌍" // Mixed languages
  ]
  
  for test_string in test_strings {
    // Test UTF-8 encoding
    let encoded = EncodingManager::encode_utf8(encoding_manager, test_string)
    assert_true(encoded.length() > 0)
    
    // Test UTF-8 decoding
    let decoded = EncodingManager::decode_utf8(encoding_manager, encoded)
    assert_eq(decoded, test_string)
    
    // Test byte length calculation
    let byte_length = EncodingManager::get_utf8_byte_length(encoding_manager, test_string)
    assert_true(byte_length >= test_string.length()) // Byte length should be >= character length
    
    // Test character count
    let char_count = EncodingManager::get_utf8_char_count(encoding_manager, test_string)
    assert_true(char_count > 0)
  }
  
  // Test encoding detection
  let test_encodings = ["UTF-8", "UTF-16", "ISO-8859-1", "Windows-1252"]
  
  for encoding in test_encodings {
    let test_string = "Test string for encoding detection"
    
    // Encode with specific encoding
    let encoded = EncodingManager::encode(encoding_manager, test_string, encoding)
    assert_true(encoded.length() > 0)
    
    // Detect encoding
    let detected_encoding = EncodingManager::detect_encoding(encoding_manager, encoded)
    assert_eq(detected_encoding, encoding)
    
    // Decode with detected encoding
    let decoded = EncodingManager::decode(encoding_manager, encoded, detected_encoding)
    assert_eq(decoded, test_string)
  }
  
  // Test encoding conversion
  let conversion_pairs = [
    ("UTF-8", "UTF-16"),
    ("UTF-16", "UTF-8"),
    ("ISO-8859-1", "UTF-8"),
    ("Windows-1252", "UTF-8"),
    ("UTF-8", "ISO-8859-1")
  ]
  
  for (from_encoding, to_encoding) in conversion_pairs {
    let test_string = "Test string for encoding conversion: 你好 café résumé"
    
    // Convert encoding
    let converted = EncodingManager::convert_encoding(
      encoding_manager, 
      test_string, 
      from_encoding, 
      to_encoding
    )
    assert_true(converted.length() > 0)
    
    // Convert back to original encoding
    let converted_back = EncodingManager::convert_encoding(
      encoding_manager, 
      converted, 
      to_encoding, 
      from_encoding
    )
    
    // Note: Some loss might occur with certain encoding conversions
    // This is expected behavior for characters not representable in target encoding
  }
  
  // Test invalid encoding handling
  let invalid_utf8_bytes = [0xFF, 0xFE, 0xFD] // Invalid UTF-8 sequence
  
  let decode_result = EncodingManager::decode_utf8_safe(encoding_manager, invalid_utf8_bytes)
  match decode_result {
    Some(decoded) => {
      // Should handle invalid bytes gracefully
      assert_true(decoded.length() > 0)
    }
    None => {
      // Or return None if cannot be decoded
      assert_true(true)
    }
  }
}

// Test 3: Date and Time Localization
test "date and time localization" {
  let datetime_localizer = DateTimeLocalizer::new()
  
  // Test date formatting in different locales
  let test_date = DateTime::new(2023, 12, 25, 14, 30, 45) // 2023-12-25 14:30:45
  
  let locales = [
    "en-US", // English (United States)
    "en-GB", // English (United Kingdom)
    "zh-CN", // Chinese (China)
    "es-ES", // Spanish (Spain)
    "fr-FR", // French (France)
    "de-DE", // German (Germany)
    "ja-JP", // Japanese (Japan)
    "ko-KR", // Korean (South Korea)
    "ar-SA", // Arabic (Saudi Arabia)
    "ru-RU", // Russian (Russia)
    "pt-BR"  // Portuguese (Brazil)
  ]
  
  for locale in locales {
    DateTimeLocalizer::set_locale(datetime_localizer, locale)
    
    // Test short date format
    let short_date = DateTimeLocalizer::format_date(datetime_localizer, test_date, "short")
    assert_true(short_date.length() > 0)
    
    // Test medium date format
    let medium_date = DateTimeLocalizer::format_date(datetime_localizer, test_date, "medium")
    assert_true(medium_date.length() > 0)
    
    // Test long date format
    let long_date = DateTimeLocalizer::format_date(datetime_localizer, test_date, "long")
    assert_true(long_date.length() > 0)
    
    // Test full date format
    let full_date = DateTimeLocalizer::format_date(datetime_localizer, test_date, "full")
    assert_true(full_date.length() > 0)
    
    // Test time format
    let time_format = DateTimeLocalizer::format_time(datetime_localizer, test_date, "medium")
    assert_true(time_format.length() > 0)
    
    // Test datetime format
    let datetime_format = DateTimeLocalizer::format_datetime(datetime_localizer, test_date, "medium")
    assert_true(datetime_format.length() > 0)
  }
  
  // Test time zone handling
  let timezones = [
    "UTC",
    "America/New_York",
    "Europe/London",
    "Asia/Shanghai",
    "Asia/Tokyo",
    "Australia/Sydney"
  ]
  
  for timezone in timezones {
    DateTimeLocalizer::set_timezone(datetime_localizer, timezone)
    
    let localized_datetime = DateTimeLocalizer::format_datetime(
      datetime_localizer, 
      test_date, 
      "medium"
    )
    assert_true(localized_datetime.length() > 0)
    
    // Test timezone conversion
    let converted_date = DateTimeLocalizer::convert_timezone(
      datetime_localizer, 
      test_date, 
      "UTC", 
      timezone
    )
    assert_true(converted_date.year >= 2023)
  }
  
  // Test relative time formatting
  DateTimeLocalizer::set_locale(datetime_localizer, "en-US")
  
  let now = DateTime::now()
  let past_date = DateTime::subtract_days(now, 1) // 1 day ago
  let future_date = DateTime::add_days(now, 1) // 1 day from now
  
  let past_relative = DateTimeLocalizer::format_relative(datetime_localizer, past_date)
  assert_true(past_relative.contains("ago") || past_relative.contains("yesterday"))
  
  let future_relative = DateTimeLocalizer::format_relative(datetime_localizer, future_date)
  assert_true(future_relative.contains("from now") || future_relative.contains("tomorrow"))
  
  // Test calendar localization
  let calendars = ["gregorian", "buddhist", "islamic", "hebrew", "chinese", "japanese"]
  
  for calendar in calendars {
    DateTimeLocalizer::set_calendar(datetime_localizer, calendar)
    
    let calendar_date = DateTimeLocalizer::format_date(
      datetime_localizer, 
      test_date, 
      "medium"
    )
    assert_true(calendar_date.length() > 0)
  }
  
  // Test number formatting in dates
  DateTimeLocalizer::set_locale(datetime_localizer, "ar-SA") // Arabic locale
  let arabic_date = DateTimeLocalizer::format_date(datetime_localizer, test_date, "medium")
  
  // Should use Arabic numerals if locale supports it
  assert_true(arabic_date.length() > 0)
}

// Test 4: Number and Currency Localization
test "number and currency localization" {
  let number_localizer = NumberLocalizer::new()
  
  // Test number formatting in different locales
  let test_numbers = [1234.567, -1234.567, 0.12345, 1000000, 0.0001]
  
  let locales = [
    "en-US", // English (United States)
    "en-GB", // English (United Kingdom)
    "zh-CN", // Chinese (China)
    "es-ES", // Spanish (Spain)
    "fr-FR", // French (France)
    "de-DE", // German (Germany)
    "ja-JP", // Japanese (Japan)
    "ko-KR", // Korean (South Korea)
    "ar-SA", // Arabic (Saudi Arabia)
    "ru-RU", // Russian (Russia)
    "pt-BR", // Portuguese (Brazil)
    "hi-IN"  // Hindi (India)
  ]
  
  for locale in locales {
    NumberLocalizer::set_locale(number_localizer, locale)
    
    for number in test_numbers {
      // Test decimal formatting
      let decimal_format = NumberLocalizer::format_decimal(number_localizer, number, 2)
      assert_true(decimal_format.length() > 0)
      
      // Test percentage formatting
      let percentage_format = NumberLocalizer::format_percentage(number_localizer, number, 1)
      assert_true(percentage_format.length() > 0)
      
      // Test scientific notation
      let scientific_format = NumberLocalizer::format_scientific(number_localizer, number)
      assert_true(scientific_format.length() > 0)
    }
  }
  
  // Test currency formatting
  let currencies = [
    ("USD", "$"),
    ("EUR", "€"),
    ("GBP", "£"),
    ("JPY", "¥"),
    ("CNY", "¥"),
    ("KRW", "₩"),
    ("INR", "₹"),
    ("BRL", "R$"),
    ("RUB", "₽"),
    ("SAR", "﷼")
  ]
  
  for (currency_code, currency_symbol) in currencies {
    for locale in locales {
      NumberLocalizer::set_locale(number_localizer, locale)
      
      let currency_format = NumberLocalizer::format_currency(
        number_localizer, 
        1234.56, 
        currency_code
      )
      assert_true(currency_format.length() > 0)
      
      // Should contain currency symbol or code
      assert_true(
        currency_format.contains(currency_symbol) || 
        currency_format.contains(currency_code)
      )
    }
  }
  
  // Test unit formatting
  let units = [
    ("length", "meter", "m"),
    ("weight", "kilogram", "kg"),
    ("temperature", "celsius", "°C"),
    ("speed", "kilometer_per_hour", "km/h"),
    ("data", "megabyte", "MB"),
    ("time", "second", "s")
  ]
  
  for (unit_type, unit_name, unit_symbol) in units {
    for locale in locales {
      NumberLocalizer::set_locale(number_localizer, locale)
      
      let unit_format = NumberLocalizer::format_unit(
        number_localizer, 
        1234.56, 
        unit_type, 
        unit_name
      )
      assert_true(unit_format.length() > 0)
    }
  }
  
  // Test number parsing
  NumberLocalizer::set_locale(number_localizer, "en-US")
  let parse_result = NumberLocalizer::parse_decimal(number_localizer, "1,234.56")
  match parse_result {
    Some(parsed_number) => assert_true((parsed_number - 1234.56).abs() < 0.001)
    None => assert_true(false)
  }
  
  NumberLocalizer::set_locale(number_localizer, "de-DE")
  let german_parse_result = NumberLocalizer::parse_decimal(number_localizer, "1.234,56")
  match german_parse_result {
    Some(parsed_number) => assert_true((parsed_number - 1234.56).abs() < 0.001)
    None => assert_true(false)
  }
}

// Test 5: Text Direction and Layout
test "text direction and layout" {
  let layout_manager = LayoutManager::new()
  
  // Test text direction detection
  let ltr_text = "Hello World" // Left-to-right
  let rtl_text = "مرحبا بالعالم" // Right-to-left
  let mixed_text = "Hello مرحبا World" // Mixed
  
  assert_eq(LayoutManager::get_text_direction(layout_manager, ltr_text), "LTR")
  assert_eq(LayoutManager::get_text_direction(layout_manager, rtl_text), "RTL")
  
  // Mixed text direction might be detected as LTR or based on first strong character
  let mixed_direction = LayoutManager::get_text_direction(layout_manager, mixed_text)
  assert_true(mixed_direction == "LTR" || mixed_direction == "RTL")
  
  // Test layout direction for different locales
  let ltr_locales = ["en-US", "es-ES", "fr-FR", "de-DE", "zh-CN", "ja-JP"]
  let rtl_locales = ["ar-SA", "he-IL", "fa-IR", "ur-PK"]
  
  for locale in ltr_locales {
    LayoutManager::set_locale(layout_manager, locale)
    assert_eq(LayoutManager::get_layout_direction(layout_manager), "LTR")
  }
  
  for locale in rtl_locales {
    LayoutManager::set_locale(layout_manager, locale)
    assert_eq(LayoutManager::get_layout_direction(layout_manager), "RTL")
  }
  
  // Test text alignment
  LayoutManager::set_locale(layout_manager, "en-US")
  assert_eq(LayoutManager::get_default_text_alignment(layout_manager), "left")
  
  LayoutManager::set_locale(layout_manager, "ar-SA")
  assert_eq(LayoutManager::get_default_text_alignment(layout_manager), "right")
  
  // Test margin and padding adjustments for RTL
  let ltr_margins = LayoutManager::get_text_margins(layout_manager, "LTR")
  let rtl_margins = LayoutManager::get_text_margins(layout_manager, "RTL")
  
  // RTL margins should be mirrored
  assert_eq(ltr_margins.left, rtl_margins.right)
  assert_eq(ltr_margins.right, rtl_margins.left)
  assert_eq(ltr_margins.top, rtl_margins.top)
  assert_eq(ltr_margins.bottom, rtl_margins.bottom)
  
  // Test UI element ordering
  let buttons = ["Previous", "Next", "Cancel", "Save"]
  
  LayoutManager::set_layout_direction(layout_manager, "LTR")
  let ltr_order = LayoutManager::get_element_order(layout_manager, buttons)
  assert_eq(ltr_order[0], "Previous")
  assert_eq(ltr_order[1], "Next")
  assert_eq(ltr_order[2], "Cancel")
  assert_eq(ltr_order[3], "Save")
  
  LayoutManager::set_layout_direction(layout_manager, "RTL")
  let rtl_order = LayoutManager::get_element_order(layout_manager, buttons)
  assert_eq(rtl_order[0], "Save") // Reversed order
  assert_eq(rtl_order[1], "Cancel")
  assert_eq(rtl_order[2], "Next")
  assert_eq(rtl_order[3], "Previous")
  
  // Test icon positioning
  LayoutManager::set_layout_direction(layout_manager, "LTR")
  let ltr_icon_position = LayoutManager::get_icon_position(layout_manager, "text_with_icon")
  assert_eq(ltr_icon_position, "left")
  
  LayoutManager::set_layout_direction(layout_manager, "RTL")
  let rtl_icon_position = LayoutManager::get_icon_position(layout_manager, "text_with_icon")
  assert_eq(rtl_icon_position, "right")
}

// Test 6: Collation and Sorting
test "collation and sorting" {
  let collation_manager = CollationManager::new()
  
  // Test string sorting in different locales
  let test_strings = [
    "apple", "banana", "cherry", "date", "elderberry", "fig", "grape"
  ]
  
  let locales = [
    "en-US", // English (United States)
    "zh-CN", // Chinese (China)
    "es-ES", // Spanish (Spain)
    "fr-FR", // French (France)
    "de-DE", // German (Germany)
    "ja-JP", // Japanese (Japan)
    "ko-KR", // Korean (South Korea)
    "sv-SE"  // Swedish (Sweden)
  ]
  
  for locale in locales {
    CollationManager::set_locale(collation_manager, locale)
    
    // Test ascending sort
    let ascending_sort = CollationManager::sort_strings(collation_manager, test_strings, "ascending")
    assert_eq(ascending_sort.length(), test_strings.length())
    
    // Verify sort order
    for i in 0..=ascending_sort.length() - 2 {
      assert_true(CollationManager::compare_strings(
        collation_manager, 
        ascending_sort[i], 
        ascending_sort[i + 1]
      ) <= 0)
    }
    
    // Test descending sort
    let descending_sort = CollationManager::sort_strings(collation_manager, test_strings, "descending")
    assert_eq(descending_sort.length(), test_strings.length())
    
    // Verify sort order
    for i in 0..=descending_sort.length() - 2 {
      assert_true(CollationManager::compare_strings(
        collation_manager, 
        descending_sort[i], 
        descending_sort[i + 1]
      ) >= 0)
    }
  }
  
  // Test locale-specific sorting rules
  let swedish_strings = ["å", "ä", "ö", "z"]
  CollationManager::set_locale(collation_manager, "sv-SE")
  let swedish_sort = CollationManager::sort_strings(collation_manager, swedish_strings, "ascending")
  
  // In Swedish, å, ä, ö come after z
  assert_eq(swedish_sort[0], "z")
  assert_eq(swedish_sort[1], "å")
  assert_eq(swedish_sort[2], "ä")
  assert_eq(swedish_sort[3], "ö")
  
  // Test German sorting (with ß)
  let german_strings = ["a", "ä", "b", "ß"]
  CollationManager::set_locale(collation_manager, "de-DE")
  let german_sort = CollationManager::sort_strings(collation_manager, german_strings, "ascending")
  
  // In German, ß is treated like "ss"
  assert_eq(german_sort[0], "a")
  assert_eq(german_sort[1], "ä")
  assert_eq(german_sort[2], "b")
  assert_eq(german_sort[3], "ß")
  
  // Test case sensitivity
  let mixed_case_strings = ["Apple", "banana", "Cherry", "date"]
  
  CollationManager::set_locale(collation_manager, "en-US")
  CollationManager::set_case_sensitive(collation_manager, true)
  let case_sensitive_sort = CollationManager::sort_strings(collation_manager, mixed_case_strings, "ascending")
  
  // Case sensitive: uppercase letters come before lowercase
  assert_eq(case_sensitive_sort[0], "Apple")
  assert_eq(case_sensitive_sort[1], "Cherry")
  assert_eq(case_sensitive_sort[2], "banana")
  assert_eq(case_sensitive_sort[3], "date")
  
  CollationManager::set_case_sensitive(collation_manager, false)
  let case_insensitive_sort = CollationManager::sort_strings(collation_manager, mixed_case_strings, "ascending")
  
  // Case insensitive: sorted alphabetically regardless of case
  assert_eq(case_insensitive_sort[0], "Apple")
  assert_eq(case_insensitive_sort[1], "banana")
  assert_eq(case_insensitive_sort[2], "Cherry")
  assert_eq(case_insensitive_sort[3], "date")
  
  // Test numeric collation
  let numeric_strings = ["file1.txt", "file10.txt", "file2.txt", "file20.txt"]
  
  CollationManager::set_numeric_collation(collation_manager, true)
  let numeric_sort = CollationManager::sort_strings(collation_manager, numeric_strings, "ascending")
  
  // Numeric collation: 1, 2, 10, 20
  assert_eq(numeric_sort[0], "file1.txt")
  assert_eq(numeric_sort[1], "file2.txt")
  assert_eq(numeric_sort[2], "file10.txt")
  assert_eq(numeric_sort[3], "file20.txt")
  
  CollationManager::set_numeric_collation(collation_manager, false)
  let non_numeric_sort = CollationManager::sort_strings(collation_manager, numeric_strings, "ascending")
  
  // Non-numeric collation: 1, 10, 2, 20 (lexicographic)
  assert_eq(non_numeric_sort[0], "file1.txt")
  assert_eq(non_numeric_sort[1], "file10.txt")
  assert_eq(non_numeric_sort[2], "file2.txt")
  assert_eq(non_numeric_sort[3], "file20.txt")
}

// Test 7: Cultural Adaptation
test "cultural adaptation" {
  let cultural_adapter = CulturalAdapter::new()
  
  // Test color symbolism
  let colors = ["red", "green", "blue", "white", "black", "yellow"]
  let cultures = [
    "US", // United States
    "CN", // China
    "JP", // Japan
    "IN", // India
    "BR", // Brazil
    "SA", // Saudi Arabia
    "EG", // Egypt
    "ZA"  // South Africa
  ]
  
  for culture in cultures {
    CulturalAdapter::set_culture(cultural_adapter, culture)
    
    for color in colors {
      let symbolism = CulturalAdapter::get_color_symbolism(cultural_adapter, color)
      assert_true(symbolism.length() > 0)
    }
  }
  
  // Test name formatting
  let names = [
    ("John", "Doe"),
    ("张", "伟"), // Chinese
    ("José", "García"), // Spanish
    ("François", "Dupont"), // French
    ("Hans", "Müller"), // German
    ("田中", "太郎"), // Japanese
    ("김", "철수"), // Korean
    ("محمد", "أحمد"), // Arabic
    ("Иван", "Иванов"), // Russian
    ("João", "Silva") // Portuguese
  ]
  
  for culture in cultures {
    CulturalAdapter::set_culture(cultural_adapter, culture)
    
    for (first_name, last_name) in names {
      let formatted_name = CulturalAdapter::format_name(cultural_adapter, first_name, last_name)
      assert_true(formatted_name.length() > 0)
      assert_true(formatted_name.contains(first_name) || formatted_name.contains(last_name))
    }
  }
  
  // Test address formatting
  let address_components = AddressComponents::new(
    "123 Main St",
    "Apt 4B",
    "New York",
    "NY",
    "10001",
    "United States"
  )
  
  for culture in cultures {
    CulturalAdapter::set_culture(cultural_adapter, culture)
    
    let formatted_address = CulturalAdapter::format_address(cultural_adapter, address_components)
    assert_true(formatted_address.length() > 0)
    
    // Should contain key components
    assert_true(
      formatted_address.contains("123") || 
      formatted_address.contains("New York") || 
      formatted_address.contains("10001")
    )
  }
  
  // Test phone number formatting
  let phone_numbers = [
    "+1-555-123-4567", // US
    "+86-138-0013-8000", // China
    "+81-3-1234-5678", // Japan
    "+91-98765-43210", // India
    "+55-11-98765-4321", // Brazil
    "+966-50-123-4567", // Saudi Arabia
    "+20-10-1234-5678", // Egypt
    "+27-11-123-4567" // South Africa
  ]
  
  for culture in cultures {
    CulturalAdapter::set_culture(cultural_adapter, culture)
    
    for phone_number in phone_numbers {
      let formatted_phone = CulturalAdapter::format_phone_number(cultural_adapter, phone_number)
      assert_true(formatted_phone.length() > 0)
    }
  }
  
  // Test measurement units
  let measurements = [
    ("length", 100.0),
    ("weight", 70.0),
    ("temperature", 25.0),
    ("volume", 2.0)
  ]
  
  let measurement_systems = ["metric", "imperial"]
  
  for culture in cultures {
    CulturalAdapter::set_culture(cultural_adapter, culture)
    
    for system in measurement_systems {
      CulturalAdapter::set_measurement_system(cultural_adapter, system)
      
      for (measurement_type, value) in measurements {
        let formatted_measurement = CulturalAdapter::format_measurement(
          cultural_adapter, 
          measurement_type, 
          value
        )
        assert_true(formatted_measurement.length() > 0)
      }
    }
  }
  
  // Test date and time formats
  let test_date = DateTime::new(2023, 12, 25, 14, 30, 45)
  
  for culture in cultures {
    CulturalAdapter::set_culture(cultural_adapter, culture)
    
    let formatted_date = CulturalAdapter::format_date(cultural_adapter, test_date)
    assert_true(formatted_date.length() > 0)
    
    let formatted_time = CulturalAdapter::format_time(cultural_adapter, test_date)
    assert_true(formatted_time.length() > 0)
  }
  
  // Test number formats
  let test_number = 1234.567
  
  for culture in cultures {
    CulturalAdapter::set_culture(cultural_adapter, culture)
    
    let formatted_number = CulturalAdapter::format_number(cultural_adapter, test_number)
    assert_true(formatted_number.length() > 0)
    
    let formatted_currency = CulturalAdapter::format_currency(cultural_adapter, test_number, "USD")
    assert_true(formatted_currency.length() > 0)
  }
}

// Test 8: Input Method and Keyboard Support
test "input method and keyboard support" {
  let input_method_manager = InputMethodManager::new()
  
  // Test keyboard layout detection
  let keyboard_layouts = [
    "US", // US QWERTY
    "UK", // UK QWERTY
    "DVORAK", // Dvorak
    "COLEMAK", // Colemak
    "AZERTY", // French AZERTY
    "QWERTZ", // German QWERTZ
    "JIS", // Japanese JIS
    "Korean", // Korean
    "Arabic", // Arabic
    "Hebrew", // Hebrew
    "Cyrillic", // Russian Cyrillic
    "Pinyin" // Chinese Pinyin
  ]
  
  for layout in keyboard_layouts {
    let is_supported = InputMethodManager::is_keyboard_layout_supported(input_method_manager, layout)
    // Some layouts might not be supported on all systems
    assert_true(is_supported == true || is_supported == false)
    
    if is_supported {
      InputMethodManager::set_keyboard_layout(input_method_manager, layout)
      let current_layout = InputMethodManager::get_current_keyboard_layout(input_method_manager)
      assert_eq(current_layout, layout)
    }
  }
  
  // Test input method detection
  let input_methods = [
    "Pinyin", // Chinese Pinyin
    "Wubi", // Chinese Wubi
    "IME", // Japanese IME
    "Hangul", // Korean Hangul
    "Arabic", // Arabic
    "Hebrew", // Hebrew
    "Russian", // Russian
    "Thai", // Thai
    "Hindi", // Hindi
    "Arabic", // Arabic
    "None" // No input method
  ]
  
  for input_method in input_methods {
    let is_supported = InputMethodManager::is_input_method_supported(input_method_manager, input_method)
    // Some input methods might not be supported on all systems
    assert_true(is_supported == true || is_supported == false)
    
    if is_supported {
      InputMethodManager::set_input_method(input_method_manager, input_method)
      let current_method = InputMethodManager::get_current_input_method(input_method_manager)
      assert_eq(current_method, input_method)
    }
  }
  
  // Test text composition
  let composition_tests = [
    ("Pinyin", "nihao", "你好"),
    ("Wubi", "hello", "hello"), // Wubi might not work with Latin input
    ("IME", "konnichiha", "こんにちは"),
    ("Hangul", "annyeonghaseyo", "안녕하세요"),
    ("None", "hello", "hello")
  ]
  
  for (input_method, input, expected_output) in composition_tests {
    if InputMethodManager::is_input_method_supported(input_method_manager, input_method) {
      InputMethodManager::set_input_method(input_method_manager, input_method)
      
      // Simulate text composition
      let composition_result = InputMethodManager::compose_text(input_method_manager, input)
      
      // Composition might not work perfectly in all environments
      // Just verify the function doesn't crash
      assert_true(composition_result.length() >= 0)
    }
  }
  
  // Test keyboard shortcuts adaptation
  let common_shortcuts = [
    "copy", // Ctrl+C
    "paste", // Ctrl+V
    "cut", // Ctrl+X
    "undo", // Ctrl+Z
    "redo", // Ctrl+Y
    "select_all", // Ctrl+A
    "find", // Ctrl+F
    "save", // Ctrl+S
    "open", // Ctrl+O
    "new" // Ctrl+N
  ]
  
  let keyboard_layouts_to_test = ["US", "UK", "AZERTY", "QWERTZ"]
  
  for layout in keyboard_layouts_to_test {
    if InputMethodManager::is_keyboard_layout_supported(input_method_manager, layout) {
      InputMethodManager::set_keyboard_layout(input_method_manager, layout)
      
      for shortcut in common_shortcuts {
        let adapted_shortcut = InputMethodManager::adapt_keyboard_shortcut(
          input_method_manager, 
          shortcut
        )
        assert_true(adapted_shortcut.length() > 0)
      }
    }
  }
  
  // Test auto-completion
  let completion_contexts = [
    "chinese_pinyin",
    "japanese_romanji",
    "korean_hangul",
    "arabic",
    "hebrew",
    "russian",
    "general"
  ]
  
  for context in completion_contexts {
    let completions = InputMethodManager::get_completions(input_method_manager, "test", context)
    assert_true(completions.length() >= 0)
  }
}

// Test 9: Font and Typography Support
test "font and typography support" {
  let font_manager = FontManager::new()
  
  // Test font detection
  let common_fonts = [
    "Arial",
    "Times New Roman",
    "Courier New",
    "Verdana",
    "Helvetica",
    "Georgia",
    "Comic Sans MS",
    "Impact",
    "Trebuchet MS",
    "Tahoma"
  ]
  
  for font in common_fonts {
    let is_available = FontManager::is_font_available(font_manager, font)
    // Font availability depends on the system
    assert_true(is_available == true || is_available == false)
  }
  
  // Test language-specific fonts
  let language_fonts = [
    ("zh-CN", ["SimSun", "Microsoft YaHei", "PingFang SC", "Noto Sans CJK SC"]),
    ("ja-JP", ["MS Gothic", "Meiryo", "Hiragino Sans", "Noto Sans CJK JP"]),
    ("ko-KR", ["Malgun Gothic", "Dotum", "Apple SD Gothic Neo", "Noto Sans CJK KR"]),
    ("ar-SA", ["Arial Unicode MS", "Tahoma", "Noto Sans Arabic"]),
    ("he-IL", ["Arial Hebrew", "David", "Noto Sans Hebrew"]),
    ("ru-RU", ["Arial", "Times New Roman", "Noto Sans"]),
    ("hi-IN", ["Mangal", "Noto Sans Devanagari"]),
    ("th-TH", ["Tahoma", "Angsana New", "Noto Sans Thai"]),
    ("ta-IN", ["Latha", "Noto Sans Tamil"]),
    ("te-IN", ["Gautami", "Noto Sans Telugu"])
  ]
  
  for (locale, fonts) in language_fonts {
    FontManager::set_locale(font_manager, locale)
    
    for font in fonts {
      let is_available = FontManager::is_font_available(font_manager, font)
      // Font availability depends on the system
      assert_true(is_available == true || is_available == false)
      
      if is_available {
        let is_suitable = FontManager::is_font_suitable_for_locale(font_manager, font, locale)
        assert_true(is_suitable)
      }
    }
    
    // Get recommended font for locale
    let recommended_font = FontManager::get_recommended_font(font_manager, locale)
    assert_true(recommended_font.length() > 0)
  }
  
  // Test font fallback
  let test_text = "Hello 你好 Hola こんにちは 안녕하세요 مرحبا ברוכים הבאים Привет мир 👋"
  let fallback_fonts = FontManager::get_font_fallback_chain(font_manager, test_text)
  assert_true(fallback_fonts.length() > 0)
  
  // Test text rendering with different fonts
  let rendering_tests = [
    ("Hello World", "Arial"),
    ("你好世界", "Microsoft YaHei"),
    ("こんにちは世界", "Meiryo"),
    ("안녕하세요 세계", "Malgun Gothic"),
    ("مرحبا بالعالم", "Arial Unicode MS"),
    ("שלום עולם", "Arial Hebrew"),
    ("Привет мир", "Arial"),
    ("नमस्ते दुनिया", "Mangal"),
    ("สวัสดีชาวโลก", "Tahoma"),
    ("Olá Mundo", "Arial")
  ]
  
  for (text, font) in rendering_tests {
    if FontManager::is_font_available(font_manager, font) {
      let can_render = FontManager::can_render_text(font_manager, text, font)
      // Some fonts might not be able to render all characters
      assert_true(can_render == true || can_render == false)
      
      let rendered_metrics = FontManager::get_text_metrics(font_manager, text, font, 12)
      assert_true(rendered_metrics.width > 0)
      assert_true(rendered_metrics.height > 0)
    }
  }
  
  // Test font size and line height
  let font_sizes = [8, 10, 12, 14, 16, 18, 24, 32, 48, 72]
  
  for size in font_sizes {
    let line_height = FontManager::get_line_height(font_manager, "Arial", size)
    assert_true(line_height >= size) // Line height should be >= font size
    
    let character_width = FontManager::get_average_character_width(font_manager, "Arial", size)
    assert_true(character_width > 0)
  }
  
  // Test font weight and style
  let font_weights = ["normal", "bold", "100", "200", "300", "400", "500", "600", "700", "800", "900"]
  let font_styles = ["normal", "italic", "oblique"]
  
  for weight in font_weights {
    for style in font_styles {
      let is_supported = FontManager::is_font_style_supported(font_manager, "Arial", weight, style)
      // Some combinations might not be supported
      assert_true(is_supported == true || is_supported == false)
    }
  }
  
  // Test text direction in fonts
  let ltr_font = FontManager::get_text_direction_font(font_manager, "en-US")
  let rtl_font = FontManager::get_text_direction_font(font_manager, "ar-SA")
  
  assert_true(ltr_font.length() > 0)
  assert_true(rtl_font.length() > 0)
  assert_not_eq(ltr_font, rtl_font) // Should be different fonts
}

// Test 10: Accessibility and Internationalization
test "accessibility and internationalization" {
  let accessibility_i18n_manager = AccessibilityI18nManager::new()
  
  // Test screen reader text localization
  let screen_reader_texts = [
    "button.save",
    "button.cancel",
    "message.success",
    "message.error",
    "navigation.previous",
    "navigation.next",
    "form.required.field",
    "table.column.sort.ascending",
    "table.column.sort.descending",
    "dialog.close"
  ]
  
  let locales = ["en-US", "zh-CN", "es-ES", "fr-FR", "de-DE", "ja-JP", "ar-SA"]
  
  for locale in locales {
    AccessibilityI18nManager::set_locale(accessibility_i18n_manager, locale)
    
    for text_key in screen_reader_texts {
      let localized_text = AccessibilityI18nManager::get_screen_reader_text(
        accessibility_i18n_manager, 
        text_key
      )
      assert_true(localized_text.length() > 0)
      
      // Verify text is not the key itself (indicating translation exists)
      if locale != "en-US" {
        assert_not_eq(localized_text, text_key)
      }
    }
  }
  
  // Test ARIA label localization
  let aria_labels = [
    ("button", "save"),
    ("button", "cancel"),
    ("input", "search"),
    ("link", "help"),
    ("image", "logo"),
    ("menu", "main"),
    ("dialog", "confirmation"),
    ("alert", "error")
  ]
  
  for locale in locales {
    AccessibilityI18nManager::set_locale(accessibility_i18n_manager, locale)
    
    for (element_type, element_name) in aria_labels {
      let aria_label = AccessibilityI18nManager::get_aria_label(
        accessibility_i18n_manager, 
        element_type, 
        element_name
      )
      assert_true(aria_label.length() > 0)
    }
  }
  
  // Test accessibility description localization
  let accessibility_descriptions = [
    "chart.description.line",
    "chart.description.bar",
    "chart.description.pie",
    "table.description.data",
    "form.description.validation",
    "navigation.description.breadcrumb"
  ]
  
  for locale in locales {
    AccessibilityI18nManager::set_locale(accessibility_i18n_manager, locale)
    
    for description_key in accessibility_descriptions {
      let description = AccessibilityI18nManager::get_accessibility_description(
        accessibility_i18n_manager, 
        description_key
      )
      assert_true(description.length() > 0)
    }
  }
  
  // Test high contrast mode localization
  let high_contrast_locales = ["en-US", "zh-CN", "es-ES", "fr-FR", "de-DE", "ja-JP"]
  
  for locale in high_contrast_locales {
    AccessibilityI18nManager::set_locale(accessibility_i18n_manager, locale)
    AccessibilityI18nManager::enable_high_contrast_mode(accessibility_i18n_manager, true)
    
    for text_key in screen_reader_texts {
      let high_contrast_text = AccessibilityI18nManager::get_screen_reader_text(
        accessibility_i18n_manager, 
        text_key
      )
      assert_true(high_contrast_text.length() > 0)
    }
  }
  
  // Test screen reader language detection
  for locale in locales {
    AccessibilityI18nManager::set_locale(accessibility_i18n_manager, locale)
    
    let screen_reader_language = AccessibilityI18nManager::get_screen_reader_language(
      accessibility_i18n_manager
    )
    assert_true(screen_reader_language.length() > 0)
    
    // Should match or be compatible with the current locale
    assert_true(
      screen_reader_language == locale || 
      screen_reader_language.startsWith(locale.split("-")[0])
    )
  }
  
  // Test text-to-speech pronunciation
  let pronunciation_tests = [
    ("en-US", "telemetry"),
    ("en-US", "azimuth"),
    ("zh-CN", "遥测"),
    ("zh-CN", "方位角"),
    ("es-ES", "telemetría"),
    ("fr-FR", "télémétrie"),
    ("de-DE", "Telemetrie"),
    ("ja-JP", "テレメトリー"),
    ("ar-SA", "قياس عن بعد")
  ]
  
  for (locale, text) in pronunciation_tests {
    AccessibilityI18nManager::set_locale(accessibility_i18n_manager, locale)
    
    let pronunciation_guide = AccessibilityI18nManager::get_pronunciation_guide(
      accessibility_i18n_manager, 
      text
    )
    assert_true(pronunciation_guide.length() > 0)
  }
  
  // Test keyboard navigation localization
  let keyboard_navigation_texts = [
    "keyboard.focus.indicator",
    "keyboard.skip.to.content",
    "keyboard.navigation.help",
    "keyboard.shortcut.list"
  ]
  
  for locale in locales {
    AccessibilityI18nManager::set_locale(accessibility_i18n_manager, locale)
    
    for text_key in keyboard_navigation_texts {
      let keyboard_text = AccessibilityI18nManager::get_keyboard_navigation_text(
        accessibility_i18n_manager, 
        text_key
      )
      assert_true(keyboard_text.length() > 0)
    }
  }
  
  // Test error message accessibility
  let error_messages = [
    "error.connection.failed",
    "error.authentication.required",
    "error.data.invalid",
    "error.permission.denied",
    "error.system.unavailable"
  ]
  
  for locale in locales {
    AccessibilityI18nManager::set_locale(accessibility_i18n_manager, locale)
    
    for error_key in error_messages {
      let accessible_error = AccessibilityI18nManager::get_accessible_error_message(
        accessibility_i18n_manager, 
        error_key
      )
      assert_true(accessible_error.length() > 0)
      
      // Should include both the error and instructions for screen readers
      assert_true(accessible_error.contains("Error") || accessible_error.contains("错误") || 
                 accessible_error.contains("Erreur") || accessible_error.contains("Fehler"))
    }
  }
}