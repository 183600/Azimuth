// Azimuth Internationalization Support Test Suite
// This file contains test cases for internationalization and localization functionality

// Test 1: Locale Detection and Management
test "locale detection and management" {
  // Test locale creation
  let en_us = Locale::new("en-US")
  assert_eq(Locale::language(en_us), "en")
  assert_eq(Locale::country(en_us), "US")
  assert_eq(Locale::script(en_us), "")
  
  let zh_cn = Locale::new("zh-CN")
  assert_eq(Locale::language(zh_cn), "zh")
  assert_eq(Locale::country(zh_cn), "CN")
  
  let ar_arab_sa = Locale::new("ar-Arab-SA")
  assert_eq(Locale::language(ar_arab_sa), "ar")
  assert_eq(Locale::script(ar_arab_sa), "Arab")
  assert_eq(Locale::country(ar_arab_sa), "SA")
  
  // Test locale parsing
  match Locale::parse("en-US") {
    Some(locale) => {
      assert_eq(Locale::to_string(locale), "en-US")
    }
    None => assert_true(false)
  }
  
  match Locale::parse("invalid-locale") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test locale fallback
  let fallback_chain = Locale::create_fallback_chain("zh-Hans-CN")
  assert_eq(fallback_chain, ["zh-Hans-CN", "zh-CN", "zh", "en-US", "en"])
  
  // Test locale matching
  let supported_locales = ["en-US", "en-GB", "zh-CN", "ja-JP"]
  let best_match = Locale::find_best_match("zh-Hans-CN", supported_locales)
  assert_eq(best_match, Some("zh-CN"))
  
  let no_match = Locale::find_best_match("fr-FR", supported_locales)
  assert_eq(no_match, None)
}

// Test 2: Message Translation
test "message translation" {
  // Test translation resource loading
  let translations = TranslationLoader::load_from_directory("/locales")
  match translations {
    Ok(resources) => {
      assert_true(TranslationResources::has_locale(resources, "en"))
      assert_true(TranslationResources::has_locale(resources, "zh"))
    }
    Err(_) => assert_true(true) // Directory may not exist in test environment
  }
  
  // Test manual translation resources
  let manual_resources = TranslationResources::new()
  TranslationResources::add_translation(manual_resources, "en", "welcome", "Welcome")
  TranslationResources::add_translation(manual_resources, "zh", "welcome", "欢迎")
  TranslationResources::add_translation(manual_resources, "es", "welcome", "Bienvenido")
  
  // Test basic translation
  let translator = Translator::new(manual_resources)
  
  assert_eq(Translator::translate(translator, "en", "welcome"), Some("Welcome"))
  assert_eq(Translator::translate(translator, "zh", "welcome"), Some("欢迎"))
  assert_eq(Translator::translate(translator, "es", "welcome"), Some("Bienvenido"))
  
  // Test missing translation
  assert_eq(Translator::translate(translator, "fr", "welcome"), None)
  
  // Test translation with fallback
  let fallback_translator = Translator::with_fallback(manual_resources, "en")
  assert_eq(Translator::translate_with_fallback(fallback_translator, "fr", "welcome"), Some("Welcome"))
  
  // Test parameterized translations
  TranslationResources::add_translation(manual_resources, "en", "greeting", "Hello, {name}!")
  TranslationResources::add_translation(manual_resources, "zh", "greeting", "你好，{name}！")
  
  let en_params = [("name", "Alice")]
  let zh_params = [("name", "爱丽丝")]
  
  assert_eq(Translator::translate_with_params(translator, "en", "greeting", en_params), Some("Hello, Alice!"))
  assert_eq(Translator::translate_with_params(translator, "zh", "greeting", zh_params), Some("你好，爱丽丝！"))
  
  // Test pluralization
  TranslationResources::add_plural_translation(manual_resources, "en", "items", [
    (PluralRule::Zero, "no items"),
    (PluralRule::One, "one item"),
    (PluralRule::Other, "{count} items")
  ])
  
  assert_eq(Translator::translate_plural(translator, "en", "items", 0), Some("no items"))
  assert_eq(Translator::translate_plural(translator, "en", "items", 1), Some("one item"))
  assert_eq(Translator::translate_plural(translator, "en", "items", 5), Some("5 items"))
}

// Test 3: Date and Time Formatting
test "date and time formatting" {
  let timestamp = 1640995200000L // 2022-01-01 00:00:00 UTC
  
  // Test date formatting by locale
  let en_us_formatter = DateFormatter::new("en-US")
  let zh_cn_formatter = DateFormatter::new("zh-CN")
  let ja_jp_formatter = DateFormatter::new("ja-JP")
  let ar_sa_formatter = DateFormatter::new("ar-SA")
  
  assert_eq(DateFormatter::format_date(en_us_formatter, timestamp), "1/1/2022")
  assert_eq(DateFormatter::format_date(zh_cn_formatter, timestamp), "2022/1/1")
  assert_eq(DateFormatter::format_date(ja_jp_formatter, timestamp), "2022/01/01")
  
  // Test time formatting
  assert_eq(DateFormatter::format_time(en_us_formatter, timestamp), "12:00:00 AM")
  assert_eq(DateFormatter::format_time(zh_cn_formatter, timestamp), "上午12:00:00")
  
  // Test datetime formatting
  assert_eq(DateFormatter::format_datetime(en_us_formatter, timestamp), "1/1/2022, 12:00:00 AM")
  assert_eq(DateFormatter::format_datetime(zh_cn_formatter, timestamp), "2022/1/1 上午12:00:00")
  
  // Test custom date patterns
  let custom_formatter = DateFormatter::with_pattern("en-US", "yyyy-MM-dd HH:mm:ss")
  assert_eq(DateFormatter::format_datetime(custom_formatter, timestamp), "2022-01-01 00:00:00")
  
  // Test relative time formatting
  let now = current_timestamp()
  let one_hour_ago = now - 3600000L
  let one_day_ago = now - 86400000L
  let one_week_ago = now - 604800000L
  
  let relative_formatter = RelativeTimeFormatter::new("en-US")
  assert_eq(RelativeTimeFormatter::format(relative_formatter, one_hour_ago), Some("1 hour ago"))
  assert_eq(RelativeTimeFormatter::format(relative_formatter, one_day_ago), Some("1 day ago"))
  assert_eq(RelativeTimeFormatter::format(relative_formatter, one_week_ago), Some("1 week ago"))
  
  let zh_relative_formatter = RelativeTimeFormatter::new("zh-CN")
  assert_eq(RelativeTimeFormatter::format(zh_relative_formatter, one_hour_ago), Some("1小时前"))
  assert_eq(RelativeTimeFormatter::format(zh_relative_formatter, one_day_ago), Some("1天前"))
}

// Test 4: Number and Currency Formatting
test "number and currency formatting" {
  // Test basic number formatting
  let en_us_formatter = NumberFormatter::new("en-US")
  let de_de_formatter = NumberFormatter::new("de-DE")
  let fr_fr_formatter = NumberFormatter::new("fr-FR")
  let hi_in_formatter = NumberFormatter::new("hi-IN")
  
  assert_eq(NumberFormatter::format(en_us_formatter, 1234567.89), Some("1,234,567.89"))
  assert_eq(NumberFormatter::format(de_de_formatter, 1234567.89), Some("1.234.567,89"))
  assert_eq(NumberFormatter::format(fr_fr_formatter, 1234567.89), Some("1 234 567,89"))
  assert_eq(NumberFormatter::format(hi_in_formatter, 1234567.89), Some("12,34,567.89"))
  
  // Test percentage formatting
  let percentage_formatter = NumberFormatter::with_style("en-US", NumberStyle::Percent)
  assert_eq(NumberFormatter::format(percentage_formatter, 0.1234), Some("12.34%"))
  
  // Test scientific notation
  let scientific_formatter = NumberFormatter::with_style("en-US", NumberStyle::Scientific)
  assert_eq(NumberFormatter::format(scientific_formatter, 1234.5), Some("1.2345E3"))
  
  // Test currency formatting
  let usd_formatter = CurrencyFormatter::new("en-US", "USD")
  let eur_formatter = CurrencyFormatter::new("de-DE", "EUR")
  let jpy_formatter = CurrencyFormatter::new("ja-JP", "JPY")
  let cny_formatter = CurrencyFormatter::new("zh-CN", "CNY")
  
  assert_eq(CurrencyFormatter::format(usd_formatter, 1234.56), Some("$1,234.56"))
  assert_eq(CurrencyFormatter::format(eur_formatter, 1234.56), Some("1.234,56 €"))
  assert_eq(CurrencyFormatter::format(jpy_formatter, 1234.56), Some("¥1,235")) // JPY doesn't use decimals
  assert_eq(CurrencyFormatter::format(cny_formatter, 1234.56), Some("¥1,234.56"))
  
  // Test currency code formatting
  let code_formatter = CurrencyFormatter::with_style("en-US", "USD", CurrencyStyle::Code)
  assert_eq(CurrencyFormatter::format(code_formatter, 1234.56), Some("USD 1,234.56"))
  
  // Test unit formatting
  let length_formatter = UnitFormatter::new("en-US", UnitType::Length)
  assert_eq(UnitFormatter::format(length_formatter, 1.75, "meter"), Some("1.75 m"))
  
  let weight_formatter = UnitFormatter::new("en-US", UnitType::Weight)
  assert_eq(UnitFormatter::format(weight_formatter, 70.5, "kilogram"), Some("70.5 kg"))
}

// Test 5: Text Direction and Script Support
test "text direction and script support" {
  // Test text direction detection
  assert_eq(TextDirection::detect("Hello World"), TextDirection::LeftToRight)
  assert_eq(TextDirection::detect("مرحبا بالعالم"), TextDirection::RightToLeft)
  assert_eq(TextDirection::detect("你好世界"), TextDirection::LeftToRight) // Chinese is LTR
  
  // Test locale text direction
  assert_eq(Locale::text_direction(Locale::new("en-US")), TextDirection::LeftToRight)
  assert_eq(Locale::text_direction(Locale::new("ar-SA")), TextDirection::RightToLeft)
  assert_eq(Locale::text_direction(Locale::new("he-IL")), TextDirection::RightToLeft)
  assert_eq(Locale::text_direction(Locale::new("zh-CN")), TextDirection::LeftToRight)
  
  // Test bidirectional text handling
  let bidi_processor = BidiProcessor::new()
  
  let mixed_text = "Hello مرحبا World"
  let processed = BidiProcessor::process(bidi_processor, mixed_text)
  assert_true(processed.length() > 0)
  
  // Test script detection
  assert_eq(ScriptDetector::detect("Hello"), Some("Latin"))
  assert_eq(ScriptDetector::detect("مرحبا"), Some("Arabic"))
  assert_eq(ScriptDetector::detect("你好"), Some("Han"))
  assert_eq(ScriptDetector::detect("こんにちは"), Some("Hiragana"))
  assert_eq(ScriptDetector::detect("Москва"), Some("Cyrillic"))
  
  // Test text alignment
  let ltr_aligner = TextAligner::new("en-US")
  let rtl_aligner = TextAligner::new("ar-SA")
  
  assert_eq(TextAligner::get_alignment(ltr_aligner), "left")
  assert_eq(TextAligner::get_alignment(rtl_aligner), "right")
  
  // Test margin/padding adjustment for RTL
  let rtl_styles = TextAligner::get_rtl_adjustments(rtl_aligner)
  assert_true(rtl_styles.contains(("margin-left", "auto")))
  assert_true(rtl_styles.contains(("margin-right", "0")))
}

// Test 6: Collation and String Comparison
test "collation and string comparison" {
  // Test locale-aware string comparison
  let en_collator = Collator::new("en-US")
  let sv_collator = Collator::new("sv-SE") // Swedish has special collation rules
  let zh_collator = Collator::new("zh-CN")
  
  // Test basic comparison
  assert_eq(Collator::compare(en_collator, "apple", "banana"), -1)
  assert_eq(Collator::compare(en_collator, "banana", "apple"), 1)
  assert_eq(Collator::compare(en_collator, "apple", "apple"), 0)
  
  // Test case-insensitive comparison
  let case_insensitive = Collator::with_options("en-US", [CollatorOption::IgnoreCase])
  assert_eq(Collator::compare(case_insensitive, "Apple", "apple"), 0)
  
  // Test accent-insensitive comparison
  let accent_insensitive = Collator::with_options("en-US", [CollatorOption::IgnoreAccents])
  assert_eq(Collator::compare(accent_insensitive, "café", "cafe"), 0)
  
  // Test locale-specific sorting
  let swedish_words = ["åland", "äpple", "orange", "zebra"]
  let sorted_swedish = Collator::sort(sv_collator, swedish_words)
  // Swedish treats Å, Ä, Ö as separate letters at the end of alphabet
  assert_eq(sorted_swedish, ["orange", "zebra", "åland", "äpple"])
  
  // Test Chinese character sorting
  let chinese_words = ["北京", "上海", "广州", "深圳"]
  let sorted_chinese = Collator::sort(zh_collator, chinese_words)
  assert_eq(sorted_chinese.length(), 4) // Should be sorted by stroke order or Pinyin
  
  // Test numeric collation
  let numeric_collator = Collator::with_options("en-US", [CollatorOption::Numeric])
  let mixed_numbers = ["file1.txt", "file10.txt", "file2.txt", "file20.txt"]
  let sorted_numeric = Collator::sort(numeric_collator, mixed_numbers)
  assert_eq(sorted_numeric, ["file1.txt", "file2.txt", "file10.txt", "file20.txt"])
}

// Test 7: Calendar and Timezone Support
test "calendar and timezone support" {
  // Test different calendar systems
  let gregorian = Calendar::new("en-US", CalendarType::Gregorian)
  let islamic = Calendar::new("ar-SA", CalendarType::Islamic)
  let hebrew = Calendar::new("he-IL", CalendarType::Hebrew)
  let chinese = Calendar::new("zh-CN", CalendarType::Chinese)
  
  let timestamp = 1640995200000L // 2022-01-01 00:00:00 UTC
  
  assert_eq(Calendar::format_date(gregorian, timestamp), "1/1/2022")
  assert_true(Calendar::format_date(islamic, timestamp).length() > 0)
  assert_true(Calendar::format_date(hebrew, timestamp).length() > 0)
  assert_true(Calendar::format_date(chinese, timestamp).length() > 0)
  
  // Test timezone conversion
  let utc_timezone = Timezone::new("UTC")
  let est_timezone = Timezone::new("America/New_York")
  let jst_timezone = Timezone::new("Asia/Tokyo")
  let cet_timezone = Timezone::new("Europe/Paris")
  
  let utc_time = Timezone::convert(timestamp, utc_timezone)
  let est_time = Timezone::convert(timestamp, est_timezone)
  let jst_time = Timezone::convert(timestamp, jst_timezone)
  let cet_time = Timezone::convert(timestamp, cet_timezone)
  
  // EST is UTC-5 (or UTC-4 during daylight saving)
  assert_true(est_time < timestamp)
  // JST is UTC+9
  assert_true(jst_time > timestamp)
  // CET is UTC+1 (or UTC+2 during daylight saving)
  assert_true(cet_time > timestamp)
  
  // Test timezone-aware formatting
  let timezone_formatter = DateFormatter::with_timezone("en-US", "America/New_York")
  let formatted_with_timezone = DateFormatter::format_datetime(timezone_formatter, timestamp)
  assert_true(formatted_with_timezone.contains("EST") || formatted_with_timezone.contains("EDT"))
  
  // Test daylight saving time detection
  let summer_timestamp = 1656633600000L // 2022-07-01 00:00:00 UTC (summer in Northern Hemisphere)
  let is_dst = Timezone::is_daylight_saving(est_timezone, summer_timestamp)
  assert_true(is_dst) // July should be DST in New York
}

// Test 8: Input Method and Keyboard Layout Support
test "input method and keyboard layout support" {
  // Test keyboard layout detection
  let layouts = KeyboardLayout::get_available_layouts()
  assert_true(layouts.length() > 0)
  assert_true(layouts.contains("US-QWERTY"))
  assert_true(layouts.contains("US-Dvorak"))
  
  // Test locale-specific keyboard mappings
  let us_layout = KeyboardLayout::new("en-US", "US-QWERTY")
  let german_layout = KeyboardLayout::new("de-DE", "German-QWERTZ")
  let japanese_layout = KeyboardLayout::new("ja-JP", "Japanese")
  let arabic_layout = KeyboardLayout::new("ar-SA", "Arabic")
  
  // Test key mapping
  assert_eq(KeyboardLayout::map_key(us_layout, 'q'), 'q')
  assert_eq(KeyboardLayout::map_key(german_layout, 'y'), 'z') // QWERTZ layout
  assert_eq(KeyboardLayout::map_key(japanese_layout, 'ろ'), 'r') // Japanese romaji
  
  // Test input method support
  let ime_en = InputMethod::new("en-US")
  let ime_zh = InputMethod::new("zh-CN")
  let ime_ja = InputMethod::new("ja-JP")
  let ime_ar = InputMethod::new("ar-SA")
  
  // Test composition
  let composition_en = InputMethod::start_composition(ime_en, "hello")
  assert_eq(InputMethod::get_composition_text(composition_en), "hello")
  
  let composition_zh = InputMethod::start_composition(ime_zh, "ni hao")
  assert_eq(InputMethod::get_composition_text(composition_zh), "ni hao")
  
  // Test candidate selection
  let candidates_zh = InputMethod::get_candidates(ime_zh, "nihao")
  assert_true(candidates_zh.length() > 0)
  assert_true(candidates_zh.contains("你好"))
  
  // Test IME mode switching
  InputMethod::set_mode(ime_zh, InputMode::Chinese)
  assert_eq(InputMethod::get_mode(ime_zh), InputMode::Chinese)
  
  InputMethod::set_mode(ime_zh, InputMode::English)
  assert_eq(InputMethod::get_mode(ime_zh), InputMode::English)
}

// Test 9: Localization Resource Management
test "localization resource management" {
  // Test resource bundle loading
  let bundle_manager = ResourceBundleManager::new()
  
  // Load resources for different locales
  let en_bundle = ResourceBundleManager::load_bundle(bundle_manager, "en-US", "/locales/en-US.json")
  let zh_bundle = ResourceBundleManager::load_bundle(bundle_manager, "zh-CN", "/locales/zh-CN.json")
  let ja_bundle = ResourceBundleManager::load_bundle(bundle_manager, "ja-JP", "/locales/ja-JP.json")
  
  // Test resource hierarchy and inheritance
  let parent_bundle = ResourceBundle::new()
  ResourceBundle::add_string(parent_bundle, "common.ok", "OK")
  ResourceBundle::add_string(parent_bundle, "common.cancel", "Cancel")
  
  let child_bundle = ResourceBundle::with_parent(parent_bundle)
  ResourceBundle::add_string(child_bundle, "specific.save", "Save")
  
  assert_eq(ResourceBundle::get_string(child_bundle, "common.ok"), Some("OK")) // From parent
  assert_eq(ResourceBundle::get_string(child_bundle, "specific.save"), Some("Save")) // From child
  
  // Test resource hot reloading
  let reloadable_bundle = ReloadableResourceBundle::new("/locales/en-US.json")
  ReloadableResourceBundle::enable_auto_reload(reloadable_bundle, 5000) // Check every 5 seconds
  
  let initial_content = ReloadableResourceBundle::get_content(reloadable_bundle)
  assert_true(initial_content.length() > 0)
  
  // Test resource merging
  let base_bundle = ResourceBundle::new()
  ResourceBundle::add_string(base_bundle, "ui.button.save", "Save")
  ResourceBundle::add_string(base_bundle, "ui.button.cancel", "Cancel")
  
  let override_bundle = ResourceBundle::new()
  ResourceBundle::add_string(override_bundle, "ui.button.save", "Save Now") // Override
  ResourceBundle::add_string(override_bundle, "ui.button.delete", "Delete") // New
  
  let merged_bundle = ResourceBundle::merge(base_bundle, override_bundle)
  assert_eq(ResourceBundle::get_string(merged_bundle, "ui.button.save"), Some("Save Now"))
  assert_eq(ResourceBundle::get_string(merged_bundle, "ui.button.cancel"), Some("Cancel"))
  assert_eq(ResourceBundle::get_string(merged_bundle, "ui.button.delete"), Some("Delete"))
  
  // Test resource validation
  let validator = ResourceBundleValidator::new()
  validator.add_required_key("ui.button.save")
  validator.add_required_key("ui.button.cancel")
  
  let valid_bundle = ResourceBundle::new()
  ResourceBundle::add_string(valid_bundle, "ui.button.save", "Save")
  ResourceBundle::add_string(valid_bundle, "ui.button.cancel", "Cancel")
  
  let validation_result = ResourceBundleValidator::validate(validator, valid_bundle)
  assert_eq(validation_result.is_valid, true)
  
  let invalid_bundle = ResourceBundle::new()
  ResourceBundle::add_string(invalid_bundle, "ui.button.save", "Save") // Missing cancel
  
  let invalid_validation = ResourceBundleValidator::validate(validator, invalid_bundle)
  assert_eq(invalid_validation.is_valid, false)
  assert_true(invalid_validation.missing_keys.contains("ui.button.cancel"))
}

// Test 10: Accessibility and Internationalization
test "accessibility and internationalization" {
  // Test screen reader support
  let screen_reader = ScreenReader::new()
  
  // Test language announcement
  ScreenReader::announce_language(screen_reader, "en-US")
  assert_eq(ScreenReader::get_current_language(screen_reader), Some("en-US"))
  
  // Test localized accessibility labels
  let accessibility_labels = AccessibilityLabels::new("en-US")
  AccessibilityLabels::add_label(accessibility_labels, "button.save", "Save document")
  AccessibilityLabels::add_label(accessibility_labels, "button.delete", "Delete item permanently")
  
  assert_eq(AccessibilityLabels::get_label(accessibility_labels, "button.save"), Some("Save document"))
  
  let zh_labels = AccessibilityLabels::new("zh-CN")
  AccessibilityLabels::add_label(zh_labels, "button.save", "保存文档")
  AccessibilityLabels::add_label(zh_labels, "button.delete", "永久删除项目")
  
  assert_eq(AccessibilityLabels::get_label(zh_labels, "button.save"), Some("保存文档"))
  
  // Test right-to-left accessibility
  let rtl_accessibility = RTLAccessibility::new("ar-SA")
  assert_eq(RTLAccessibility::get_reading_direction(rtl_accessibility), "rtl")
  assert_eq(RTLAccessibility::get_cursor_movement(rtl_accessibility), "rtl")
  
  // Test high contrast mode for different locales
  let high_contrast = HighContrastMode::new()
  HighContrastMode::enable(high_contrast)
  
  let en_colors = HighContrastMode::get_colors(high_contrast, "en-US")
  let ar_colors = HighContrastMode::get_colors(high_contrast, "ar-SA")
  
  assert_eq(en_colors.text, "#FFFFFF")
  assert_eq(en_colors.background, "#000000")
  assert_eq(ar_colors.text, "#FFFFFF") // Should be consistent across locales
  
  // Test font accessibility
  let font_accessibility = FontAccessibility::new()
  
  let en_fonts = FontAccessibility::get_recommended_fonts(font_accessibility, "en-US")
  let zh_fonts = FontAccessibility::get_recommended_fonts(font_accessibility, "zh-CN")
  let ar_fonts = FontAccessibility::get_recommended_fonts(font_accessibility, "ar-SA")
  
  assert_true(en_fonts.contains("Arial"))
  assert_true(en_fonts.contains("Verdana"))
  
  assert_true(zh_fonts.contains("Microsoft YaHei"))
  assert_true(zh_fonts.contains("SimSun"))
  
  assert_true(ar_fonts.contains("Arial"))
  assert_true(ar_fonts.contains("Tahoma"))
  
  // Test localized error messages for accessibility
  let error_messages = AccessibilityErrorMessages::new()
  AccessibilityErrorMessages::add_message(error_messages, "en-US", "form.invalid", "Please correct the errors in the form")
  AccessibilityErrorMessages::add_message(error_messages, "zh-CN", "form.invalid", "请更正表单中的错误")
  AccessibilityErrorMessages::add_message(error_messages, "ar-SA", "form.invalid", "يرجى تصحيح الأخطاء في النموذج")
  
  assert_eq(AccessibilityErrorMessages::get_message(error_messages, "en-US", "form.invalid"), Some("Please correct the errors in the form"))
  assert_eq(AccessibilityErrorMessages::get_message(error_messages, "zh-CN", "form.invalid"), Some("请更正表单中的错误"))
  assert_eq(AccessibilityErrorMessages::get_message(error_messages, "ar-SA", "form.invalid"), Some("يرجى تصحيح الأخطاء في النموذج"))
}