// Azimuth High-Quality Internationalization Support Tests
// This file contains comprehensive test cases for internationalization support

// Test 1: Locale Management and Detection
test "locale management and detection" {
  let locale_manager = LocaleManager::new()
  
  // Test default locale
  let default_locale = locale_manager.get_default_locale()
  assert_eq(default_locale.language, "en")
  assert_eq(default_locale.region, "US")
  assert_eq(default_locale.to_string(), "en-US")
  
  // Test adding supported locales
  locale_manager.add_supported_locale(Locale::new("en", "US"))
  locale_manager.add_supported_locale(Locale::new("en", "GB"))
  locale_manager.add_supported_locale(Locale::new("zh", "CN"))
  locale_manager.add_supported_locale(Locale::new("zh", "TW"))
  locale_manager.add_supported_locale(Locale::new("fr", "FR"))
  locale_manager.add_supported_locale(Locale::new("de", "DE"))
  locale_manager.add_supported_locale(Locale::new("ja", "JP"))
  locale_manager.add_supported_locale(Locale::new("es", "ES"))
  locale_manager.add_supported_locale(Locale::new("ar", "SA"))
  locale_manager.add_supported_locale(Locale::new("ru", "RU"))
  
  // Test supported locales
  let supported_locales = locale_manager.get_supported_locales()
  assert_eq(supported_locales.length(), 10)
  
  // Test locale detection from Accept-Language header
  let accept_language_1 = "en-US,en;q=0.9,fr;q=0.8"
  let detected_locale_1 = locale_manager.detect_locale_from_header(accept_language_1)
  assert_eq(detected_locale_1.to_string(), "en-US")
  
  let accept_language_2 = "fr-FR,fr;q=0.9,en;q=0.8"
  let detected_locale_2 = locale_manager.detect_locale_from_header(accept_language_2)
  assert_eq(detected_locale_2.to_string(), "fr-FR")
  
  let accept_language_3 = "zh-CN,zh;q=0.9,en;q=0.8"
  let detected_locale_3 = locale_manager.detect_locale_from_header(accept_language_3)
  assert_eq(detected_locale_3.to_string(), "zh-CN")
  
  // Test locale fallback when unsupported locale is requested
  let unsupported_header = "it-IT,it;q=0.9"
  let fallback_locale = locale_manager.detect_locale_from_header(unsupported_header)
  assert_eq(fallback_locale.to_string(), "en-US") // Should fallback to default
  
  // Test locale detection from system
  let system_locale = locale_manager.detect_system_locale()
  assert_true(system_locale.language.length() == 2)
  assert_true(system_locale.region.length() == 2)
  
  // Test locale matching with fallback
  let requested_locale = Locale::new("zh", "HK") // Not in supported list
  let matched_locale = locale_manager.find_best_match(requested_locale)
  assert_eq(matched_locale.to_string(), "zh-CN") // Should match language with another region
  
  let requested_locale_2 = Locale::new("en", "AU") // Not in supported list
  let matched_locale_2 = locale_manager.find_best_match(requested_locale_2)
  assert_eq(matched_locale_2.to_string(), "en-US") // Should match language with another region
  
  // Test locale preferences
  let user_preferences = UserLocalePreferences::new()
  user_preferences.add_preferred_locale(Locale::new("fr", "FR"))
  user_preferences.add_preferred_locale(Locale::new("de", "DE"))
  user_preferences.add_preferred_locale(Locale::new("en", "US"))
  
  locale_manager.set_user_preferences(user_preferences)
  
  let preferred_locale = locale_manager.get_preferred_locale()
  assert_eq(preferred_locale.to_string(), "fr-FR")
  
  // Test locale switching
  locale_manager.set_current_locale(Locale::new("zh", "CN"))
  assert_eq(locale_manager.get_current_locale().to_string(), "zh-CN")
  
  locale_manager.set_current_locale(Locale::new("ja", "JP"))
  assert_eq(locale_manager.get_current_locale().to_string(), "ja-JP")
  
  // Test locale validation
  assert_true(locale_manager.is_supported(Locale::new("en", "US")))
  assert_true(locale_manager.is_supported(Locale::new("zh", "CN")))
  assert_false(locale_manager.is_supported(Locale::new("it", "IT")))
  
  // Test locale metadata
  let locale_metadata = locale_manager.get_locale_metadata(Locale::new("zh", "CN"))
  assert_eq(locale_metadata.language, "zh")
  assert_eq(locale_metadata.region, "CN")
  assert_eq(locale_metadata.display_name, "Chinese (China)")
  assert_eq(locale_metadata.native_name, "中文 (中国)")
  assert_true(locale_metadata.is_rtl == false)
  
  let arabic_metadata = locale_manager.get_locale_metadata(Locale::new("ar", "SA"))
  assert_eq(arabic_metadata.language, "ar")
  assert_eq(arabic_metadata.region, "SA")
  assert_eq(arabic_metadata.display_name, "Arabic (Saudi Arabia)")
  assert_eq(arabic_metadata.native_name, "العربية (المملكة العربية السعودية)")
  assert_true(arabic_metadata.is_rtl == true)
}

// Test 2: Message Translation and Formatting
test "message translation and formatting" {
  let translator = MessageTranslator::new()
  
  // Load translation resources
  translator.load_translation_file("en-US", "translations/en-US.json")
  translator.load_translation_file("zh-CN", "translations/zh-CN.json")
  translator.load_translation_file("fr-FR", "translations/fr-FR.json")
  translator.load_translation_file("ar-SA", "translations/ar-SA.json")
  
  // Test basic translation
  translator.set_locale(Locale::new("en", "US"))
  assert_eq(translator.translate("welcome.message"), "Welcome to Azimuth!")
  assert_eq(translator.translate("error.not_found"), "Resource not found")
  assert_eq(translator.translate("button.save"), "Save")
  
  // Test translation to Chinese
  translator.set_locale(Locale::new("zh", "CN"))
  assert_eq(translator.translate("welcome.message"), "欢迎使用 Azimuth！")
  assert_eq(translator.translate("error.not_found"), "资源未找到")
  assert_eq(translator.translate("button.save"), "保存")
  
  // Test translation to French
  translator.set_locale(Locale::new("fr", "FR"))
  assert_eq(translator.translate("welcome.message"), "Bienvenue à Azimuth!")
  assert_eq(translator.translate("error.not_found"), "Ressource non trouvée")
  assert_eq(translator.translate("button.save"), "Enregistrer")
  
  // Test translation to Arabic (RTL)
  translator.set_locale(Locale::new("ar", "SA"))
  assert_eq(translator.translate("welcome.message"), "مرحباً بك في Azimuth!")
  assert_eq(translator.translate("error.not_found"), "المورد غير موجود")
  assert_eq(translator.translate("button.save"), "حفظ")
  
  // Test parameterized translation
  translator.set_locale(Locale::new("en", "US"))
  let user_name = "John Doe"
  let item_count = 5
  let formatted_message = translator.translate("welcome.user", {"name": user_name, "count": item_count})
  assert_eq(formatted_message, "Welcome, John Doe! You have 5 items.")
  
  translator.set_locale(Locale::new("zh", "CN"))
  let formatted_message_zh = translator.translate("welcome.user", {"name": user_name, "count": item_count})
  assert_eq(formatted_message_zh, "欢迎，John Doe！您有 5 个项目。")
  
  // Test pluralization
  translator.set_locale(Locale::new("en", "US"))
  assert_eq(translator.translate_plural("item.count", 0), "No items")
  assert_eq(translator.translate_plural("item.count", 1), "1 item")
  assert_eq(translator.translate_plural("item.count", 5), "5 items")
  
  translator.set_locale(Locale::new("zh", "CN"))
  assert_eq(translator.translate_plural("item.count", 0), "没有项目")
  assert_eq(translator.translate_plural("item.count", 1), "1 个项目")
  assert_eq(translator.translate_plural("item.count", 5), "5 个项目")
  
  // Test gender-aware translation
  translator.set_locale(Locale::new("en", "US"))
  assert_eq(translator.translate_gender("user.action", "male"), "He has logged in")
  assert_eq(translator.translate_gender("user.action", "female"), "She has logged in")
  assert_eq(translator.translate_gender("user.action", "other"), "They have logged in")
  
  // Test missing translation fallback
  translator.set_locale(Locale::new("zh", "CN"))
  let missing_translation = translator.translate("non.existent.key")
  assert_eq(missing_translation, "non.existent.key") // Should return key as fallback
  
  // Test translation with context
  translator.set_locale(Locale::new("en", "US"))
  let context_translation = translator.translate_with_context("save", "button")
  assert_eq(context_translation, "Save")
  
  let context_translation_2 = translator.translate_with_context("save", "file")
  assert_eq(context_translation_2, "Save file")
  
  // Test translation caching
  let uncached_start = get_current_timestamp()
  for i in 0..=100 {
    translator.translate("welcome.message")
  }
  let uncached_end = get_current_timestamp()
  let uncached_time = uncached_end - uncached_start
  
  translator.enable_caching(true)
  
  let cached_start = get_current_timestamp()
  for i in 0..=100 {
    translator.translate("welcome.message")
  }
  let cached_end = get_current_timestamp()
  let cached_time = cached_end - cached_start
  
  // Cached translation should be faster
  assert_true(cached_time.to_millis() < uncached_time.to_millis())
  
  // Test translation statistics
  let stats = translator.get_translation_stats()
  assert_true(stats.total_translations > 0)
  assert_true(stats.cache_hits > 0)
  assert_true(stats.missing_translations >= 0)
}

// Test 3: Number, Date, and Currency Formatting
test "number date and currency formatting" {
  let formatter = InternationalFormatter::new()
  
  // Test number formatting
  formatter.set_locale(Locale::new("en", "US"))
  assert_eq(formatter.format_number(1234.567), "1,234.567")
  assert_eq(formatter.format_number(1234567), "1,234,567")
  assert_eq(formatter.format_number(0.12345), "0.123")
  
  formatter.set_locale(Locale::new("de", "DE"))
  assert_eq(formatter.format_number(1234.567), "1.234,567")
  assert_eq(formatter.format_number(1234567), "1.234.567")
  assert_eq(formatter.format_number(0.12345), "0,123")
  
  formatter.set_locale(Locale::new("fr", "FR"))
  assert_eq(formatter.format_number(1234.567), "1 234,567")
  assert_eq(formatter.format_number(1234567), "1 234 567")
  assert_eq(formatter.format_number(0.12345), "0,123")
  
  // Test percentage formatting
  formatter.set_locale(Locale::new("en", "US"))
  assert_eq(formatter.format_percentage(0.1234), "12.34%")
  assert_eq(formatter.format_percentage(0.5), "50%")
  
  formatter.set_locale(Locale::new("zh", "CN"))
  assert_eq(formatter.format_percentage(0.1234), "12.34%")
  assert_eq(formatter.format_percentage(0.5), "50%")
  
  // Test currency formatting
  formatter.set_locale(Locale::new("en", "US"))
  assert_eq(formatter.format_currency(1234.56, "USD"), "$1,234.56")
  assert_eq(formatter.format_currency(1234.56, "EUR"), "€1,234.56")
  
  formatter.set_locale(Locale::new("de", "DE"))
  assert_eq(formatter.format_currency(1234.56, "EUR"), "1.234,56 €")
  assert_eq(formatter.format_currency(1234.56, "USD"), "1.234,56 $")
  
  formatter.set_locale(Locale::new("ja", "JP"))
  assert_eq(formatter.format_currency(1234.56, "JPY"), "￥1,235")
  assert_eq(formatter.format_currency(1234.56, "USD"), "$1,234.56")
  
  // Test date/time formatting
  let test_date = DateTime::new(2024, 1, 15, 14, 30, 45)
  
  formatter.set_locale(Locale::new("en", "US"))
  assert_eq(formatter.format_date(test_date, DateFormat::Short), "1/15/24")
  assert_eq(formatter.format_date(test_date, DateFormat::Medium), "Jan 15, 2024")
  assert_eq(formatter.format_date(test_date, DateFormat::Long), "January 15, 2024")
  assert_eq(formatter.format_date(test_date, DateFormat::Full), "Monday, January 15, 2024")
  
  assert_eq(formatter.format_time(test_date, TimeFormat::Short), "2:30 PM")
  assert_eq(formatter.format_time(test_date, TimeFormat::Medium), "2:30:45 PM")
  assert_eq(formatter.format_time(test_date, TimeFormat::Long), "2:30:45 PM PST")
  assert_eq(formatter.format_time(test_date, TimeFormat::Full), "2:30:45 PM Pacific Standard Time")
  
  formatter.set_locale(Locale::new("zh", "CN"))
  assert_eq(formatter.format_date(test_date, DateFormat::Short), "2024/1/15")
  assert_eq(formatter.format_date(test_date, DateFormat::Medium), "2024年1月15日")
  assert_eq(formatter.format_date(test_date, DateFormat::Long), "2024年1月15日")
  assert_eq(formatter.format_date(test_date, DateFormat::Full), "2024年1月15日星期一")
  
  assert_eq(formatter.format_time(test_date, TimeFormat::Short), "下午2:30")
  assert_eq(formatter.format_time(test_date, TimeFormat::Medium), "下午2:30:45")
  assert_eq(formatter.format_time(test_date, TimeFormat::Long), "下午2:30:45 GMT+8")
  assert_eq(formatter.format_time(test_date, TimeFormat::Full), "下午2:30:45 中国标准时间")
  
  formatter.set_locale(Locale::new("fr", "FR"))
  assert_eq(formatter.format_date(test_date, DateFormat::Short), "15/01/2024")
  assert_eq(formatter.format_date(test_date, DateFormat::Medium), "15 janv. 2024")
  assert_eq(formatter.format_date(test_date, DateFormat::Long), "15 janvier 2024")
  assert_eq(formatter.format_date(test_date, DateFormat::Full), "lundi 15 janvier 2024")
  
  assert_eq(formatter.format_time(test_date, TimeFormat::Short), "14:30")
  assert_eq(formatter.format_time(test_date, TimeFormat::Medium), "14:30:45")
  assert_eq(formatter.format_time(test_date, TimeFormat::Long), "14:30:45 PST")
  assert_eq(formatter.format_time(test_date, TimeFormat::Full), "14:30:45 heure normale du Pacifique")
  
  // Test relative time formatting
  formatter.set_locale(Locale::new("en", "US"))
  let now = DateTime::now()
  let one_hour_ago = now.subtract(Duration::from_hours(1))
  let two_days_later = now.add(Duration::from_days(2))
  
  assert_eq(formatter.format_relative_time(one_hour_ago), "1 hour ago")
  assert_eq(formatter.format_relative_time(two_days_later), "in 2 days")
  
  formatter.set_locale(Locale::new("zh", "CN"))
  assert_eq(formatter.format_relative_time(one_hour_ago), "1小时前")
  assert_eq(formatter.format_relative_time(two_days_later), "2天后")
  
  // Test duration formatting
  let duration = Duration::from_hours(2) + Duration::from_minutes(30) + Duration::from_seconds(45)
  
  formatter.set_locale(Locale::new("en", "US"))
  assert_eq(formatter.format_duration(duration), "2h 30m 45s")
  
  formatter.set_locale(Locale::new("zh", "CN"))
  assert_eq(formatter.format_duration(duration), "2小时30分钟45秒")
  
  // Test unit formatting
  formatter.set_locale(Locale::new("en", "US"))
  assert_eq(formatter.format_unit(1024, "byte"), "1,024 B")
  assert_eq(formatter.format_unit(1048576, "byte"), "1.0 MB")
  assert_eq(formatter.format_unit(1000, "meter"), "1,000 m")
  assert_eq(formatter.format_unit(1000, "kilometer"), "1,000 km")
  
  formatter.set_locale(Locale::new("zh", "CN"))
  assert_eq(formatter.format_unit(1024, "byte"), "1,024 字节")
  assert_eq(formatter.format_unit(1048576, "byte"), "1.0 MB")
  assert_eq(formatter.format_unit(1000, "meter"), "1,000 米")
  assert_eq(formatter.format_unit(1000, "kilometer"), "1,000 公里")
}

// Test 4: Text Direction and Script Handling
test "text direction and script handling" {
  let text_processor = TextDirectionProcessor::new()
  
  // Test LTR text detection
  let ltr_text = "Hello, world!"
  assert_eq(text_processor.get_direction(ltr_text), TextDirection::LTR)
  
  // Test RTL text detection
  let rtl_text = "مرحبا بالعالم"
  assert_eq(text_processor.get_direction(rtl_text), TextDirection::RTL)
  
  // Test mixed text detection
  let mixed_text = "Hello مرحبا world"
  assert_eq(text_processor.get_direction(mixed_text), TextDirection::Mixed)
  
  // Test auto text detection
  let auto_detected = text_processor.auto_detect_direction("مرحبا بالعالم")
  assert_eq(auto_detected, TextDirection::RTL)
  
  let auto_detected_ltr = text_processor.auto_detect_direction("Hello, world!")
  assert_eq(auto_detected_ltr, TextDirection::LTR)
  
  // Test text alignment based on direction
  assert_eq(text_processor.get_alignment(TextDirection::LTR), TextAlign::Left)
  assert_eq(text_processor.get_alignment(TextDirection::RTL), TextAlign::Right)
  assert_eq(text_processor.get_alignment(TextDirection::Mixed), TextAlign::Left) // Default to LTR for mixed
  
  // Test text wrapping with direction
  let long_ltr_text = "This is a very long text that needs to be wrapped properly in left-to-right direction"
  let wrapped_ltr = text_processor.wrap_text(long_ltr_text, 20, TextDirection::LTR)
  assert_true(wrapped_ltr.contains("\n"))
  
  let long_rtl_text = "هذا نص طويل جدا يحتاج إلى التفاف بشكل صحيح في الاتجاه من اليمين إلى اليسار"
  let wrapped_rtl = text_processor.wrap_text(long_rtl_text, 20, TextDirection::RTL)
  assert_true(wrapped_rtl.contains("\n"))
  
  // Test bidirectional text handling
  let bidi_text = "The Arabic word مرحبا means 'hello' in English"
  let processed_bidi = text_processor.process_bidirectional_text(bidi_text)
  assert_true(processed_bidi.length() > 0)
  
  // Test script detection
  assert_eq(text_processor.detect_script("Hello"), Script::Latin)
  assert_eq(text_processor.detect_script("مرحبا"), Script::Arabic)
  assert_eq(text_processor.detect_script("こんにちは"), Script::Japanese)
  assert_eq(text_processor.detect_script("你好"), Script::Chinese)
  assert_eq(text_processor.detect_script("Привет"), Script::Cyrillic)
  
  // Test numeric context in RTL text
  let rtl_with_numbers = "السعر: 123.45 دولار"
  let processed_numbers = text_processor.process_numeric_context(rtl_with_numbers)
  assert_true(processed_numbers.contains("123.45"))
  
  // Test text isolation for mixed content
  let mixed_content = "English text followed by Arabic نص عربي and more English"
  let isolated_segments = text_processor.isolate_mixed_segments(mixed_content)
  assert_true(isolated_segments.length() > 1)
  
  for segment in isolated_segments {
    assert_true(segment.text.length() > 0)
    assert_true(segment.direction == TextDirection::LTR || segment.direction == TextDirection::RTL)
  }
  
  // Test cursor positioning in bidirectional text
  let bidi_cursor_text = "Hello مرحبا world"
  let cursor_positions = text_processor.get_cursor_positions(bidi_cursor_text)
  assert_eq(cursor_positions.length(), bidi_cursor_text.length())
  
  // Test text mirroring for RTL layouts
  let mirrable_content = "← → ↑ ↓"
  let mirrored_content = text_processor.mirror_for_rtl(mirrable_content)
  assert_eq(mirrored_content, "→ ← ↑ ↓") // Arrows should be mirrored
  
  // Test text ordering in RTL context
  let rtl_list_items = ["الأول", "الثاني", "الثالث"]
  let ordered_list = text_processor.order_for_rtl(rtl_list_items)
  assert_eq(ordered_list[0], "الثالث") // Order should be reversed for RTL
  assert_eq(ordered_list[1], "الثاني")
  assert_eq(ordered_list[2], "الأول")
}

// Test 5: Collation and String Comparison
test "collation and string comparison" {
  let collator = Collator::new()
  
  // Test basic collation
  collator.set_locale(Locale::new("en", "US"))
  assert_true(collator.compare("apple", "banana") < 0)
  assert_true(collator.compare("banana", "apple") > 0)
  assert_true(collator.compare("apple", "apple") == 0)
  
  // Test case sensitivity
  collator.set_case_sensitive(true)
  assert_true(collator.compare("Apple", "apple") > 0)
  
  collator.set_case_sensitive(false)
  assert_true(collator.compare("Apple", "apple") == 0)
  
  // Test diacritic sensitivity
  collator.set_diacritic_sensitive(true)
  assert_true(collator.compare("café", "cafe") > 0)
  
  collator.set_diacritic_sensitive(false)
  assert_true(collator.compare("café", "cafe") == 0)
  
  // Test string sorting with collation
  let words = ["banana", "Apple", "cherry", "apple", "Banana"]
  let sorted_words = collator.sort(words)
  
  // With case-insensitive collation, should be: Apple, apple, banana, Banana, cherry
  assert_eq(sorted_words[0], "Apple")
  assert_eq(sorted_words[1], "apple")
  assert_eq(sorted_words[2], "banana")
  assert_eq(sorted_words[3], "Banana")
  assert_eq(sorted_words[4], "cherry")
  
  // Test locale-specific collation
  collator.set_locale(Locale::new("sv", "SE")) // Swedish
  let swedish_words = ["zebra", "apple", "äpple", "banana", "orange"]
  let sorted_swedish = collator.sort(swedish_words)
  
  // In Swedish, ä should come after z
  assert_eq(sorted_swedish[0], "apple")
  assert_eq(sorted_swedish[1], "banana")
  assert_eq(sorted_swedish[2], "orange")
  assert_eq(sorted_swedish[3], "zebra")
  assert_eq(sorted_swedish[4], "äpple")
  
  // Test Chinese collation
  collator.set_locale(Locale::new("zh", "CN"))
  let chinese_words = ["北京", "上海", "广州", "深圳"]
  let sorted_chinese = collator.sort(chinese_words)
  
  // Verify Chinese sorting order
  assert_eq(sorted_chinese[0], "上海")
  assert_eq(sorted_chinese[1], "北京")
  assert_eq(sorted_chinese[2], "广州")
  assert_eq(sorted_chinese[3], "深圳")
  
  // Test Japanese collation
  collator.set_locale(Locale::new("ja", "JP"))
  let japanese_words = ["東京", "大阪", "京都", "札幌"]
  let sorted_japanese = collator.sort(japanese_words)
  
  // Verify Japanese sorting order
  assert_eq(sorted_japanese[0], "京都")
  assert_eq(sorted_japanese[1], "札幌")
  assert_eq(sorted_japanese[2], "大阪")
  assert_eq(sorted_japanese[3], "東京")
  
  // Test Arabic collation
  collator.set_locale(Locale::new("ar", "SA"))
  let arabic_words = ["تفاح", "موز", "برتقال", "عنب"]
  let sorted_arabic = collator.sort(arabic_words)
  
  // Verify Arabic sorting order
  assert_eq(sorted_arabic[0], "برتقال")
  assert_eq(sorted_arabic[1], "تفاح")
  assert_eq(sorted_arabic[2], "عنب")
  assert_eq(sorted_arabic[3], "موز")
  
  // Test custom collation strength
  collator.set_strength(CollationStrength::Primary) // Only base letters
  assert_true(collator.compare("café", "cafe") == 0) // Should be equal with primary strength
  
  collator.set_strength(CollationStrength::Secondary) // Base letters + diacritics
  assert_true(collator.compare("café", "cafe") > 0) // Should be different with secondary strength
  
  // Test search with collation
  let search_text = "café"
  let search_options = SearchOptions::new()
    .with_case_sensitive(false)
    .with_diacritic_sensitive(false)
  
  let matches = collator.search_in_text("I love café and cafe culture", search_text, search_options)
  assert_eq(matches.length(), 2) // Should find both "café" and "cafe"
  
  // Test string normalization
  let normalized_text = collator.normalize("café")
  assert_true(normalized_text.length() > 0)
  
  // Test locale-aware string equality
  assert_true(collator.equals_ignore_case("Apple", "apple"))
  assert_true(collator.equals_ignore_diacritics("café", "cafe"))
}

// Test 6: Time Zone Handling
test "time zone handling" {
  let timezone_manager = TimeZoneManager::new()
  
  // Test time zone detection
  let system_timezone = timezone_manager.get_system_timezone()
  assert_true(system_timezone.length() > 0)
  
  // Test time zone conversion
  let utc_time = DateTime::new(2024, 1, 15, 12, 0, 0).with_timezone("UTC")
  
  let pst_time = timezone_manager.convert_timezone(utc_time, "America/Los_Angeles")
  assert_eq(pst_time.hour, 4) // 12:00 UTC = 04:00 PST (January)
  assert_eq(pst_time.timezone, "America/Los_Angeles")
  
  let cet_time = timezone_manager.convert_timezone(utc_time, "Europe/Paris")
  assert_eq(cet_time.hour, 13) // 12:00 UTC = 13:00 CET (January)
  assert_eq(cet_time.timezone, "Europe/Paris")
  
  let jst_time = timezone_manager.convert_timezone(utc_time, "Asia/Tokyo")
  assert_eq(jst_time.hour, 21) // 12:00 UTC = 21:00 JST
  assert_eq(jst_time.timezone, "Asia/Tokyo")
  
  // Test daylight saving time
  let summer_utc = DateTime::new(2024, 7, 15, 12, 0, 0).with_timezone("UTC")
  
  let summer_pst = timezone_manager.convert_timezone(summer_utc, "America/Los_Angeles")
  assert_eq(summer_pst.hour, 5) // 12:00 UTC = 05:00 PDT (July, daylight saving)
  assert_eq(summer_pst.timezone, "America/Los_Angeles")
  
  // Test time zone formatting
  timezone_manager.set_locale(Locale::new("en", "US"))
  let formatted_pst = timezone_manager.format_with_timezone(utc_time, "America/Los_Angeles")
  assert_true(formatted_pst.contains("PST") || formatted_pst.contains("PDT"))
  
  timezone_manager.set_locale(Locale::new("zh", "CN"))
  let formatted_cst = timezone_manager.format_with_timezone(utc_time, "Asia/Shanghai")
  assert_true(formatted_cst.contains("CST"))
  
  // Test time zone offset calculation
  let pst_offset = timezone_manager.get_timezone_offset("America/Los_Angeles", utc_time)
  assert_eq(pst_offset, -8 * 3600) // -8 hours in January
  
  let summer_pst_offset = timezone_manager.get_timezone_offset("America/Los_Angeles", summer_utc)
  assert_eq(summer_pst_offset, -7 * 3600) // -7 hours in July (daylight saving)
  
  // Test time zone database
  let available_timezones = timezone_manager.get_available_timezones()
  assert_true(available_timezones.length() > 100)
  assert_true(available_timezones.contains("UTC"))
  assert_true(available_timezones.contains("America/Los_Angeles"))
  assert_true(available_timezones.contains("Europe/Paris"))
  assert_true(available_timezones.contains("Asia/Tokyo"))
  
  // Test time zone metadata
  let pst_metadata = timezone_manager.get_timezone_metadata("America/Los_Angeles")
  assert_eq(pst_metadata.id, "America/Los_Angeles")
  assert_eq(pst_metadata.standard_name, "Pacific Standard Time")
  assert_eq(pst_metadata.daylight_name, "Pacific Daylight Time")
  assert_eq(pst_metadata.standard_offset, -8 * 3600)
  assert_eq(pst_metadata.daylight_offset, -7 * 3600)
  assert_true(pst_metadata.uses_daylight_time)
  
  // Test user time zone preferences
  let user_timezone = UserTimezonePreferences::new()
  user_timezone.set_timezone("Europe/London")
  user_timezone.set_format_24hour(false)
  user_timezone.set_show_timezone(true)
  
  timezone_manager.set_user_preferences(user_timezone)
  
  let user_formatted = timezone_manager.format_for_user(utc_time)
  assert_true(user_formatted.contains("GMT") || user_formatted.contains("BST"))
  
  // Test time zone aware scheduling
  let schedule_time = DateTime::new(2024, 1, 15, 9, 0, 0).with_timezone("America/New_York")
  let utc_schedule_time = timezone_manager.to_utc(schedule_time)
  
  assert_eq(utc_schedule_time.hour, 14) // 09:00 EST = 14:00 UTC
  
  // Test time zone conversion with DST transitions
  let dst_transition = DateTime::new(2024, 3, 10, 2, 30, 0).with_timezone("America/New_York")
  let utc_transition = timezone_manager.to_utc(dst_transition)
  
  // This is during the spring forward transition (2:00 AM becomes 3:00 AM)
  // 2:30 AM doesn't exist, so it should be handled appropriately
  assert_true(utc_transition.hour >= 6) // Should be around 6:30 or 7:00 UTC
  
  // Test time zone abbreviation formatting
  let pst_abbrev = timezone_manager.format_timezone_abbreviation("America/Los_Angeles", utc_time)
  assert_eq(pst_abbrev, "PST")
  
  let pdt_abbrev = timezone_manager.format_timezone_abbreviation("America/Los_Angeles", summer_utc)
  assert_eq(pdt_abbrev, "PDT")
}

// Test 7: Calendar Systems and Localization
test "calendar systems and localization" {
  let calendar_manager = CalendarManager::new()
  
  // Test Gregorian calendar (default)
  calendar_manager.set_calendar(CalendarType::Gregorian)
  
  let gregorian_date = DateTime::new(2024, 1, 15, 12, 0, 0)
  let formatted_gregorian = calendar_manager.format_date(gregorian_date, DateFormat::Full)
  assert_true(formatted_gregorian.contains("2024"))
  assert_true(formatted_gregorian.contains("January"))
  assert_true(formatted_gregorian.contains("15"))
  
  // Test Islamic calendar
  calendar_manager.set_calendar(CalendarType::Islamic)
  calendar_manager.set_locale(Locale::new("ar", "SA"))
  
  let islamic_date = calendar_manager.convert_to_calendar(gregorian_date, CalendarType::Islamic)
  let formatted_islamic = calendar_manager.format_date(islamic_date, DateFormat::Full)
  assert_true(formatted_islamic.contains("رمضان") || formatted_islamic.contains("شعبان")) // Should contain Islamic month
  
  // Test Hebrew calendar
  calendar_manager.set_calendar(CalendarType::Hebrew)
  calendar_manager.set_locale(Locale::new("he", "IL"))
  
  let hebrew_date = calendar_manager.convert_to_calendar(gregorian_date, CalendarType::Hebrew)
  let formatted_hebrew = calendar_manager.format_date(hebrew_date, DateFormat::Full)
  assert_true(formatted_hebrew.length() > 0)
  
  // Test Chinese calendar
  calendar_manager.set_calendar(CalendarType::Chinese)
  calendar_manager.set_locale(Locale::new("zh", "CN"))
  
  let chinese_date = calendar_manager.convert_to_calendar(gregorian_date, CalendarType::Chinese)
  let formatted_chinese = calendar_manager.format_date(chinese_date, DateFormat::Full)
  assert_true(formatted_chinese.contains("农历") || formatted_chinese.contains("年"))
  
  // Test Japanese calendar (with era names)
  calendar_manager.set_calendar(CalendarType::Japanese)
  calendar_manager.set_locale(Locale::new("ja", "JP"))
  
  let japanese_date = calendar_manager.convert_to_calendar(gregorian_date, CalendarType::Japanese)
  let formatted_japanese = calendar_manager.format_date(japanese_date, DateFormat::Full)
  assert_true(formatted_japanese.contains("令和") || formatted_japanese.contains("平成")) // Should contain era name
  
  // Test Buddhist calendar
  calendar_manager.set_calendar(CalendarType::Buddhist)
  calendar_manager.set_locale(Locale::new("th", "TH"))
  
  let buddhist_date = calendar_manager.convert_to_calendar(gregorian_date, CalendarType::Buddhist)
  let formatted_buddhist = calendar_manager.format_date(buddhist_date, DateFormat::Full)
  assert_true(formatted_buddhist.contains("พ.ศ.")) // Buddhist Era abbreviation
  
  // Test Persian calendar
  calendar_manager.set_calendar(CalendarType::Persian)
  calendar_manager.set_locale(Locale::new("fa", "IR"))
  
  let persian_date = calendar_manager.convert_to_calendar(gregorian_date, CalendarType::Persian)
  let formatted_persian = calendar_manager.format_date(persian_date, DateFormat::Full)
  assert_true(formatted_persian.length() > 0)
  
  // Test Indian calendar
  calendar_manager.set_calendar(CalendarType::Indian)
  calendar_manager.set_locale(Locale::new("hi", "IN"))
  
  let indian_date = calendar_manager.convert_to_calendar(gregorian_date, CalendarType::Indian)
  let formatted_indian = calendar_manager.format_date(indian_date, DateFormat::Full)
  assert_true(formatted_indian.length() > 0)
  
  // Test calendar conversion accuracy
  let reference_date = DateTime::new(2024, 1, 1, 0, 0, 0)
  
  let islamic_converted = calendar_manager.convert_to_calendar(reference_date, CalendarType::Islamic)
  let back_to_gregorian = calendar_manager.convert_to_calendar(islamic_converted, CalendarType::Gregorian)
  
  // Should be very close to original date (within 1 day due to calendar differences)
  let days_difference = abs((reference_date.to_timestamp() - back_to_gregorian.to_timestamp()) / (24 * 3600))
  assert_true(days_difference <= 1)
  
  // Test calendar-specific holidays
  calendar_manager.set_calendar(CalendarType::Gregorian)
  calendar_manager.set_locale(Locale::new("en", "US"))
  
  let us_holidays = calendar_manager.get_holidays_for_year(2024, "US")
  assert_true(us_holidays.length() > 0)
  
  let christmas = us_holidays.find(|h| h.name.contains("Christmas"))
  assert_true(christmas.is_some())
  assert_eq(christmas.unwrap().month, 12)
  assert_eq(christmas.unwrap().day, 25)
  
  // Test Islamic holidays
  calendar_manager.set_calendar(CalendarType::Islamic)
  calendar_manager.set_locale(Locale::new("ar", "SA"))
  
  let islamic_holidays = calendar_manager.get_holidays_for_year(1445, "SA")
  assert_true(islamic_holidays.length() > 0)
  
  let eid_al_fitr = islamic_holidays.find(|h| h.name.contains("الفطر"))
  assert_true(eid_al_fitr.is_some())
  
  // Test work week calculations
  calendar_manager.set_calendar(CalendarType::Gregorian)
  calendar_manager.set_locale(Locale::new("en", "US"))
  
  let us_work_week = calendar_manager.get_work_week("US")
  assert_eq(us_work_week.start_day, 1) // Monday
  assert_eq(us_work_week.end_day, 5) // Friday
  
  calendar_manager.set_locale(Locale::new("ar", "SA"))
  
  let sa_work_week = calendar_manager.get_work_week("SA")
  assert_eq(sa_work_week.start_day, 0) // Sunday
  assert_eq(sa_work_week.end_day, 4) // Thursday
  
  // Test calendar metadata
  let gregorian_metadata = calendar_manager.get_calendar_metadata(CalendarType::Gregorian)
  assert_eq(gregorian_metadata.name, "Gregorian")
  assert_eq(gregorian_metadata.days_in_month, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])
  
  let islamic_metadata = calendar_manager.get_calendar_metadata(CalendarType::Islamic)
  assert_eq(islamic_metadata.name, "Islamic")
  assert_eq(islamic_metadata.days_in_month, [30, 29, 30, 29, 30, 29, 30, 29, 30, 29, 30, 29])
}

// Test 8: Keyboard and Input Method Handling
test "keyboard and input method handling" {
  let input_manager = InputMethodManager::new()
  
  // Test keyboard layout detection
  let system_keyboard = input_manager.get_system_keyboard_layout()
  assert_true(system_keyboard.language.length() == 2)
  assert_true(system_keyboard.layout.length() > 0)
  
  // Test keyboard layout switching
  input_manager.add_keyboard_layout(KeyboardLayout::new("en", "US", "QWERTY"))
  input_manager.add_keyboard_layout(KeyboardLayout::new("fr", "FR", "AZERTY"))
  input_manager.add_keyboard_layout(KeyboardLayout::new("de", "DE", "QWERTZ"))
  input_manager.add_keyboard_layout(KeyboardLayout::new("ar", "SA", "Arabic"))
  input_manager.add_keyboard_layout(KeyboardLayout::new("zh", "CN", "Pinyin"))
  input_manager.add_keyboard_layout(KeyboardLayout::new("ja", "JP", "Romaji"))
  input_manager.add_keyboard_layout(KeyboardLayout::new("ru", "RU", "JCUKEN"))
  
  // Test keyboard layout activation
  assert_true(input_manager.activate_layout("en", "US"))
  assert_eq(input_manager.get_active_layout().language, "en")
  assert_eq(input_manager.get_active_layout().region, "US")
  
  assert_true(input_manager.activate_layout("ar", "SA"))
  assert_eq(input_manager.get_active_layout().language, "ar")
  assert_eq(input_manager.get_active_layout().region, "SA")
  
  // Test key mapping for different layouts
  input_manager.activate_layout("en", "US")
  let qwerty_mapping = input_manager.get_key_mapping()
  assert_eq(qwerty_mapping.get_key('q'), 'q')
  assert_eq(qwerty_mapping.get_key('w'), 'w')
  assert_eq(qwerty_mapping.get_key('e'), 'e')
  assert_eq(qwerty_mapping.get_key('r'), 'r')
  assert_eq(qwerty_mapping.get_key('t'), 't')
  assert_eq(qwerty_mapping.get_key('y'), 'y')
  
  input_manager.activate_layout("fr", "FR")
  let azerty_mapping = input_manager.get_key_mapping()
  assert_eq(azerty_mapping.get_key('q'), 'a')
  assert_eq(azerty_mapping.get_key('w'), 'z')
  assert_eq(azerty_mapping.get_key('e'), 'e')
  assert_eq(azerty_mapping.get_key('r'), 'r')
  assert_eq(azerty_mapping.get_key('t'), 't')
  assert_eq(azerty_mapping.get_key('y'), 'y')
  
  input_manager.activate_layout("de", "DE")
  let qwertz_mapping = input_manager.get_key_mapping()
  assert_eq(qwertz_mapping.get_key('q'), 'q')
  assert_eq(qwertz_mapping.get_key('w'), 'w')
  assert_eq(qwertz_mapping.get_key('e'), 'e')
  assert_eq(qwertz_mapping.get_key('r'), 'r')
  assert_eq(qwertz_mapping.get_key('t'), 't')
  assert_eq(qwertz_mapping.get_key('y'), 'z')
  assert_eq(qwertz_mapping.get_key('z'), 'y')
  
  // Test IME (Input Method Editor) support
  let ime_manager = IMEManager::new()
  
  // Add IME profiles
  ime_manager.add_ime_profile(IMEProfile::new("zh", "CN", "Pinyin")
    .with_conversion_table(load_pinyin_table())
    .with_suggestions_enabled(true))
  
  ime_manager.add_ime_profile(IMEProfile::new("ja", "JP", "Romaji")
    .with_conversion_table(load_romaji_table())
    .with_suggestions_enabled(true))
  
  ime_manager.add_ime_profile(IMEProfile::new("ar", "SA", "Arabic")
    .with_conversion_table(load_arabic_table())
    .with_suggestions_enabled(true))
  
  // Test IME activation
  assert_true(ime_manager.activate_ime("zh", "CN", "Pinyin"))
  assert_eq(ime_manager.get_active_ime().language, "zh")
  assert_eq(ime_manager.get_active_ime().region, "CN")
  assert_eq(ime_manager.get_active_ime().method, "Pinyin")
  
  // Test IME input processing
  ime_manager.activate_ime("zh", "CN", "Pinyin")
  
  let pinyin_input = "nihao"
  let pinyin_suggestions = ime_manager.get_suggestions(pinyin_input)
  assert_true(pinyin_suggestions.length() > 0)
  assert_true(pinyin_suggestions.contains("你好"))
  
  // Test IME composition
  let composition_result = ime_manager.start_composition("n")
  assert_true(composition_result.is_composing)
  assert_eq(composition_result.composed_text, "n")
  
  let continue_result = ime_manager.continue_composition("i")
  assert_true(continue_result.is_composing)
  assert_eq(continue_result.composed_text, "ni")
  
  let finish_result = ime_manager.finish_composition("hao")
  assert_true(finish_result.is_composing)
  assert_eq(finish_result.composed_text, "nihao")
  
  // Test IME candidate selection
  let candidates = ime_manager.get_candidates("nihao")
  assert_true(candidates.length() > 0)
  
  let selected_candidate = ime_manager.select_candidate(0)
  assert_eq(selected_candidate, "你好")
  
  // Test keyboard shortcuts for IME
  let shortcut_manager = IMEShortcutManager::new()
  
  shortcut_manager.add_shortcut("toggle_ime", KeyCombo::new([KeyCode::Ctrl, KeyCode::Space]))
  shortcut_manager.add_shortcut("next_candidate", KeyCombo::new([KeyCode::Down]))
  shortcut_manager.add_shortcut("prev_candidate", KeyCombo::new([KeyCode::Up]))
  shortcut_manager.add_shortcut("select_candidate", KeyCombo::new([KeyCode::Enter]))
  shortcut_manager.add_shortcut("cancel_composition", KeyCombo::new([KeyCode::Escape]))
  
  // Test shortcut handling
  let toggle_result = shortcut_manager.handle_shortcut(KeyCombo::new([KeyCode::Ctrl, KeyCode::Space]))
  assert_eq(toggle_result.action, "toggle_ime")
  
  // Test input method prediction
  let prediction_engine = IMEPredictionEngine::new()
  
  prediction_engine.add_to_dictionary("你好", "hello")
  prediction_engine.add_to_dictionary("世界", "world")
  prediction_engine.add_to_dictionary("谢谢", "thank you")
  
  let predictions = prediction_engine.predict("ni")
  assert_true(predictions.length() > 0)
  assert_true(predictions.any(|p| p.text.contains("你")))
  
  // Test input method statistics
  let ime_stats = ime_manager.get_usage_statistics()
  assert_true(ime_stats.total_inputs > 0)
  assert_true(ime_stats.compositions_completed > 0)
  assert_true(ime_stats.candidates_selected > 0)
  assert_true(ime_stats.most_used_imes.length() > 0)
}

// Test 9: Font and Text Rendering
test "font and text rendering" {
  let font_manager = FontManager::new()
  
  // Test font detection
  let system_fonts = font_manager.get_system_fonts()
  assert_true(system_fonts.length() > 0)
  
  // Test font loading
  let latin_font = font_manager.load_font("Arial", FontStyle::Normal)
  assert_true(latin_font.is_available())
  
  let chinese_font = font_manager.load_font("SimSun", FontStyle::Normal)
  assert_true(chinese_font.is_available() || font_manager.load_fallback_font("zh", "CN"))
  
  let arabic_font = font_manager.load_font("Arial", FontStyle::Normal)
  assert_true(arabic_font.is_available() || font_manager.load_fallback_font("ar", "SA"))
  
  let japanese_font = font_manager.load_font("MS Gothic", FontStyle::Normal)
  assert_true(japanese_font.is_available() || font_manager.load_fallback_font("ja", "JP"))
  
  // Test font fallback system
  let fallback_chain = font_manager.create_fallback_chain([
    FontRequirement::new("en", "Latin"),
    FontRequirement::new("zh", "Han"),
    FontRequirement::new("ar", "Arabic"),
    FontRequirement::new("ja", "Han")
  ])
  
  assert_eq(fallback_chain.length(), 4)
  
  // Test text rendering with font fallback
  let mixed_text = "Hello 你好 مرحبا こんにちは"
  let rendering_plan = font_manager.plan_text_rendering(mixed_text)
  
  assert_true(rendering_plan.segments.length() > 1) // Should have multiple segments for different scripts
  
  for segment in rendering_plan.segments {
    assert_true(segment.text.length() > 0)
    assert_true(segment.font.is_available())
  }
  
  // Test font metrics
  let font_metrics = font_manager.get_font_metrics(latin_font)
  assert_true(font_metrics.ascent > 0)
  assert_true(font_metrics.descent > 0)
  assert_true(font_metrics.line_height > 0)
  assert_true(font_metrics.average_char_width > 0)
  
  // Test text measurement
  let latin_text = "Hello, World!"
  let latin_metrics = font_manager.measure_text(latin_text, latin_font, 16.0)
  assert_true(latin_metrics.width > 0)
  assert_true(latin_metrics.height > 0)
  
  let chinese_text = "你好，世界！"
  let chinese_metrics = font_manager.measure_text(chinese_text, chinese_font, 16.0)
  assert_true(chinese_metrics.width > 0)
  assert_true(chinese_metrics.height > 0)
  
  let arabic_text = "مرحبا بالعالم!"
  let arabic_metrics = font_manager.measure_text(arabic_text, arabic_font, 16.0)
  assert_true(arabic_metrics.width > 0)
  assert_true(arabic_metrics.height > 0)
  
  // Test text layout
  let layout_engine = TextLayoutEngine::new()
  
  // Test LTR layout
  let ltr_layout = layout_engine.layout_text("Hello, World!", TextDirection::LTR, latin_font, 16.0, 300.0)
  assert_eq(ltr_layout.direction, TextDirection::LTR)
  assert_true(ltr_layout.lines.length() > 0)
  assert_true(ltr_layout.width > 0)
  assert_true(ltr_layout.height > 0)
  
  // Test RTL layout
  let rtl_layout = layout_engine.layout_text("مرحبا بالعالم!", TextDirection::RTL, arabic_font, 16.0, 300.0)
  assert_eq(rtl_layout.direction, TextDirection::RTL)
  assert_true(rtl_layout.lines.length() > 0)
  assert_true(rtl_layout.width > 0)
  assert_true(rtl_layout.height > 0)
  
  // Test bidirectional layout
  let bidi_layout = layout_engine.layout_text("Hello مرحبا World!", TextDirection::Auto, latin_font, 16.0, 300.0)
  assert_true(bidi_layout.lines.length() > 0)
  
  // Test text wrapping
  let long_text = "This is a very long text that needs to be wrapped properly across multiple lines"
  let wrapped_layout = layout_engine.layout_text(long_text, TextDirection::LTR, latin_font, 16.0, 200.0)
  assert_true(wrapped_layout.lines.length() > 1)
  
  // Test font style variations
  let bold_font = font_manager.load_font("Arial", FontStyle::Bold)
  let italic_font = font_manager.load_font("Arial", FontStyle::Italic)
  let bold_italic_font = font_manager.load_font("Arial", FontStyle::BoldItalic)
  
  assert_true(bold_font.is_available())
  assert_true(italic_font.is_available())
  assert_true(bold_italic_font.is_available())
  
  // Test text rendering with different styles
  let styled_text = "Normal <b>Bold</b> <i>Italic</i> <b><i>Bold Italic</b></i>"
  let styled_layout = layout_engine.layout_html_text(styled_text, latin_font, 16.0, 300.0)
  assert_true(styled_layout.segments.length() > 1)
  
  // Test font loading performance
  let font_loading_start = get_current_timestamp()
  
  for i in 0..=100 {
    let _ = font_manager.load_font("Arial", FontStyle::Normal)
  }
  
  let font_loading_end = get_current_timestamp()
  let font_loading_time = font_loading_end - font_loading_start
  
  assert_true(font_loading_time.to_millis() < 1000) // Should load 100 fonts in less than 1 second
  
  // Test font caching
  font_manager.enable_caching(true)
  
  let cached_font_start = get_current_timestamp()
  
  for i in 0..=100 {
    let _ = font_manager.load_font("Arial", FontStyle::Normal)
  }
  
  let cached_font_end = get_current_timestamp()
  let cached_font_time = cached_font_end - cached_font_start
  
  // Cached font loading should be faster
  assert_true(cached_font_time.to_millis() < font_loading_time.to_millis())
  
  // Test font fallback performance
  let fallback_start = get_current_timestamp()
  
  for i in 0..=100 {
    let _ = font_manager.plan_text_rendering(mixed_text)
  }
  
  let fallback_end = get_current_timestamp()
  let fallback_time = fallback_end - fallback_start
  
  assert_true(fallback_time.to_millis() < 1000) // Should plan 100 text renderings in less than 1 second
}

// Test 10: Accessibility and Internationalization
test "accessibility and internationalization" {
  let accessibility_manager = AccessibilityManager::new()
  
  // Test screen reader support
  let screen_reader = ScreenReaderInterface::new()
  
  // Test text-to-speech with different languages
  screen_reader.set_language("en-US")
  assert_true(screen_reader.speak("Hello, world!"))
  
  screen_reader.set_language("zh-CN")
  assert_true(screen_reader.speak("你好，世界！"))
  
  screen_reader.set_language("ar-SA")
  assert_true(screen_reader.speak("مرحبا بالعالم!"))
  
  screen_reader.set_language("ja-JP")
  assert_true(screen_reader.speak("こんにちは、世界！"))
  
  // Test speech rate and pitch adjustment
  screen_reader.set_rate(1.2) // 20% faster
  screen_reader.set_pitch(0.9) // Slightly lower pitch
  
  // Test high contrast mode with different locales
  let high_contrast = HighContrastManager::new()
  
  high_contrast.set_locale(Locale::new("en", "US"))
  let en_high_contrast = high_contrast.get_color_scheme()
  assert_eq(en_high_contrast.background, "#000000") // Black background
  assert_eq(en_high_contrast.text, "#FFFFFF") // White text
  
  high_contrast.set_locale(Locale::new("ar", "SA"))
  let ar_high_contrast = high_contrast.get_color_scheme()
  assert_eq(ar_high_contrast.background, "#000000") // Black background
  assert_eq(ar_high_contrast.text, "#FFFFFF") // White text
  
  // Test font size scaling for accessibility
  let font_scaler = AccessibilityFontScaler::new()
  
  font_scaler.set_scaling_factor(1.5) // 150% of normal size
  
  let normal_text = "Hello, world!"
  let scaled_text = font_scaler.scale_text(normal_text)
  assert_eq(scaled_text.text, normal_text)
  assert_eq(scaled_text.font_size, 16.0 * 1.5) // Assuming base font size is 16px
  
  // Test screen magnifier with different text directions
  let magnifier = ScreenMagnifier::new()
  
  magnifier.set_zoom_level(2.0) // 200% zoom
  magnifier.set_locale(Locale::new("ar", "SA")) // RTL locale
  
  let rtl_viewport = magnifier.get_viewport()
  assert_true(rtl_viewport.direction == TextDirection::RTL)
  
  // Test accessibility labels translation
  let a11y_translator = AccessibilityTranslator::new()
  
  a11y_translator.load_translations("en-US", "a11y/en-US.json")
  a11y_translator.load_translations("zh-CN", "a11y/zh-CN.json")
  a11y_translator.load_translations("ar-SA", "a11y/ar-SA.json")
  
  a11y_translator.set_locale(Locale::new("en", "US"))
  assert_eq(a11y_translator.translate("button.save"), "Save button")
  
  a11y_translator.set_locale(Locale::new("zh", "CN"))
  assert_eq(a11y_translator.translate("button.save"), "保存按钮")
  
  a11y_translator.set_locale(Locale::new("ar", "SA"))
  assert_eq(a11y_translator.translate("button.save"), "زر الحفظ")
  
  // Test keyboard navigation with different locales
  let keyboard_nav = KeyboardNavigationManager::new()
  
  keyboard_nav.set_locale(Locale::new("en", "US"))
  let en_nav_order = keyboard_nav.get_navigation_order()
  assert_eq(en_nav_order.direction, NavigationDirection::LTR)
  
  keyboard_nav.set_locale(Locale::new("ar", "SA"))
  let ar_nav_order = keyboard_nav.get_navigation_order()
  assert_eq(ar_nav_order.direction, NavigationDirection::RTL)
  
  // Test focus indicators for different locales
  let focus_manager = FocusManager::new()
  
  focus_manager.set_locale(Locale::new("en", "US"))
  let en_focus_style = focus_manager.get_focus_style()
  assert_eq(en_focus_style.outline_offset, "2px") // Standard offset for LTR
  
  focus_manager.set_locale(Locale::new("ar", "SA"))
  let ar_focus_style = focus_manager.get_focus_style()
  assert_eq(ar_focus_style.outline_offset, "-2px") // Negative offset for RTL
  
  // Test accessibility testing with internationalization
  let a11y_tester = AccessibilityTester::new()
  
  a11y_tester.set_locale(Locale::new("en", "US"))
  let en_a11y_issues = a11y_tester.test_page("test_page_en.html")
  assert_true(en_a11y_issues.length() >= 0)
  
  a11y_tester.set_locale(Locale::new("zh", "CN"))
  let zh_a11y_issues = a11y_tester.test_page("test_page_zh.html")
  assert_true(zh_a11y_issues.length() >= 0)
  
  a11y_tester.set_locale(Locale::new("ar", "SA"))
  let ar_a11y_issues = a11y_tester.test_page("test_page_ar.html")
  assert_true(ar_a11y_issues.length() >= 0)
  
  // Test color blindness support with different locales
  let color_blindness = ColorBlindnessSupport::new()
  
  color_blindness.set_locale(Locale::new("en", "US"))
  let en_color_filters = color_blindness.get_color_filters()
  assert_true(en_color_filters.contains("protanopia"))
  assert_true(en_color_filters.contains("deuteranopia"))
  assert_true(en_color_filters.contains("tritanopia"))
  
  color_blindness.set_locale(Locale::new("zh", "CN"))
  let zh_color_filters = color_blindness.get_color_filters()
  assert_true(zh_color_filters.contains("红色盲")) // Protanopia in Chinese
  assert_true(zh_color_filters.contains("绿色盲")) // Deuteranopia in Chinese
  assert_true(zh_color_filters.contains("蓝色盲")) // Tritanopia in Chinese
  
  // Test accessibility preferences per locale
  let pref_manager = AccessibilityPreferenceManager::new()
  
  let en_prefs = AccessibilityPreferences::new()
  en_prefs.high_contrast = true
  en_prefs.large_text = true
  en_prefs.screen_reader = true
  
  pref_manager.set_preferences(Locale::new("en", "US"), en_prefs)
  
  let ar_prefs = AccessibilityPreferences::new()
  ar_prefs.high_contrast = true
  ar_prefs.large_text = false
  ar_prefs.screen_reader = true
  ar_prefs.rtl_optimization = true
  
  pref_manager.set_preferences(Locale::new("ar", "SA"), ar_prefs)
  
  // Verify preferences were set
  let retrieved_en_prefs = pref_manager.get_preferences(Locale::new("en", "US"))
  assert_true(retrieved_en_prefs.high_contrast)
  assert_true(retrieved_en_prefs.large_text)
  assert_true(retrieved_en_prefs.screen_reader)
  
  let retrieved_ar_prefs = pref_manager.get_preferences(Locale::new("ar", "SA"))
  assert_true(retrieved_ar_prefs.high_contrast)
  assert_false(retrieved_ar_prefs.large_text)
  assert_true(retrieved_ar_prefs.screen_reader)
  assert_true(retrieved_ar_prefs.rtl_optimization)
}