// Azimuth Telemetry System - Internationalization Support Tests
// This file contains comprehensive test cases for internationalization functionality

// Test 1: Basic Localization Support
test "basic localization support" {
  let localization_manager = LocalizationManager::new()
  
  // Add supported locales
  LocalizationManager::add_locale(localization_manager, "en-US")
  LocalizationManager::add_locale(localization_manager, "zh-CN")
  LocalizationManager::add_locale(localization_manager, "fr-FR")
  LocalizationManager::add_locale(localization_manager, "es-ES")
  LocalizationManager::add_locale(localization_manager, "ja-JP")
  
  // Set default locale
  LocalizationManager::set_default_locale(localization_manager, "en-US")
  
  // Test locale detection
  assert_eq(LocalizationManager::get_current_locale(localization_manager), "en-US")
  
  // Test locale switching
  LocalizationManager::set_locale(localization_manager, "zh-CN")
  assert_eq(LocalizationManager::get_current_locale(localization_manager), "zh-CN")
  
  LocalizationManager::set_locale(localization_manager, "fr-FR")
  assert_eq(LocalizationManager::get_current_locale(localization_manager), "fr-FR")
  
  // Test locale validation
  assert_true(LocalizationManager::is_supported_locale(localization_manager, "ja-JP"))
  assert_false(LocalizationManager::is_supported_locale(localization_manager, "ko-KR"))
  
  // Get available locales
  let available_locales = LocalizationManager::get_available_locales(localization_manager)
  assert_eq(available_locales.length(), 5)
  assert_true(available_locales.contains("en-US"))
  assert_true(available_locales.contains("zh-CN"))
  assert_true(available_locales.contains("fr-FR"))
  assert_true(available_locales.contains("es-ES"))
  assert_true(available_locales.contains("ja-JP"))
}

// Test 2: Message Translation
test "message translation" {
  let translation_manager = TranslationManager::new()
  
  // Add translations for different locales
  TranslationManager::add_translation(translation_manager, "en-US", "welcome.message", "Welcome to Azimuth Telemetry")
  TranslationManager::add_translation(translation_manager, "zh-CN", "welcome.message", "欢迎使用 Azimuth 遥测系统")
  TranslationManager::add_translation(translation_manager, "fr-FR", "welcome.message", "Bienvenue dans Azimuth Télémétrie")
  TranslationManager::add_translation(translation_manager, "es-ES", "welcome.message", "Bienvenido a Azimuth Telemetría")
  TranslationManager::add_translation(translation_manager, "ja-JP", "welcome.message", "Azimuth テレメトリーへようこそ")
  
  // Add more complex translations with parameters
  TranslationManager::add_translation(translation_manager, "en-US", "user.greeting", "Hello, {name}! You have {count} new messages.")
  TranslationManager::add_translation(translation_manager, "zh-CN", "user.greeting", "你好，{name}！您有 {count} 条新消息。")
  TranslationManager::add_translation(translation_manager, "fr-FR", "user.greeting", "Bonjour, {name}! Vous avez {count} nouveaux messages.")
  TranslationManager::add_translation(translation_manager, "es-ES", "user.greeting", "¡Hola, {name}! Tienes {count} nuevos mensajes.")
  TranslationManager::add_translation(translation_manager, "ja-JP", "user.greeting", "こんにちは、{name}さん！{count}件の新しいメッセージがあります。")
  
  // Test basic translation
  TranslationManager::set_locale(translation_manager, "en-US")
  let english_welcome = TranslationManager::translate(translation_manager, "welcome.message")
  match english_welcome {
    Some(message) => assert_eq(message, "Welcome to Azimuth Telemetry")
    None => assert_true(false)
  }
  
  TranslationManager::set_locale(translation_manager, "zh-CN")
  let chinese_welcome = TranslationManager::translate(translation_manager, "welcome.message")
  match chinese_welcome {
    Some(message) => assert_eq(message, "欢迎使用 Azimuth 遥测系统")
    None => assert_true(false)
  }
  
  TranslationManager::set_locale(translation_manager, "fr-FR")
  let french_welcome = TranslationManager::translate(translation_manager, "welcome.message")
  match french_welcome {
    Some(message) => assert_eq(message, "Bienvenue dans Azimuth Télémétrie")
    None => assert_true(false)
  }
  
  // Test parameterized translation
  let params = [
    ("name", "Alice"),
    ("count", "5")
  ]
  
  TranslationManager::set_locale(translation_manager, "en-US")
  let english_greeting = TranslationManager::translate_with_params(translation_manager, "user.greeting", params)
  match english_greeting {
    Some(message) => assert_eq(message, "Hello, Alice! You have 5 new messages.")
    None => assert_true(false)
  }
  
  TranslationManager::set_locale(translation_manager, "zh-CN")
  let chinese_greeting = TranslationManager::translate_with_params(translation_manager, "user.greeting", params)
  match chinese_greeting {
    Some(message) => assert_eq(message, "你好，Alice！您有 5 条新消息。")
    None => assert_true(false)
  }
  
  TranslationManager::set_locale(translation_manager, "ja-JP")
  let japanese_greeting = TranslationManager::translate_with_params(translation_manager, "user.greeting", params)
  match japanese_greeting {
    Some(message) => assert_eq(message, "こんにちは、Aliceさん！5件の新しいメッセージがあります。")
    None => assert_true(false)
  }
  
  // Test missing translation fallback
  let missing_translation = TranslationManager::translate(translation_manager, "nonexistent.key")
  match missing_translation {
    Some(message) => assert_eq(message, "nonexistent.key") // Should return key as fallback
    None => assert_true(false)
  }
}

// Test 3: Number and Date Formatting
test "number and date formatting" {
  let formatter = LocalizationFormatter::new()
  
  // Test number formatting for different locales
  Formatter::set_locale(formatter, "en-US")
  let english_number = Formatter::format_number(formatter, 1234567.89)
  assert_eq(english_number, "1,234,567.89")
  
  Formatter::set_locale(formatter, "fr-FR")
  let french_number = Formatter::format_number(formatter, 1234567.89)
  assert_eq(french_number, "1 234 567,89")
  
  Formatter::set_locale(formatter, "zh-CN")
  let chinese_number = Formatter::format_number(formatter, 1234567.89)
  assert_eq(chinese_number, "1,234,567.89")
  
  // Test currency formatting
  Formatter::set_locale(formatter, "en-US")
  let english_currency = Formatter::format_currency(formatter, 1234.56, "USD")
  assert_eq(english_currency, "$1,234.56")
  
  Formatter::set_locale(formatter, "fr-FR")
  let french_currency = Formatter::format_currency(formatter, 1234.56, "EUR")
  assert_eq(french_currency, "1 234,56 €")
  
  Formatter::set_locale(formatter, "ja-JP")
  let japanese_currency = Formatter::format_currency(formatter, 1234.56, "JPY")
  assert_eq(japanese_currency, "￥1,235") // Japanese yen doesn't use decimal places
  
  // Test percentage formatting
  Formatter::set_locale(formatter, "en-US")
  let english_percent = Formatter::format_percentage(formatter, 0.7543)
  assert_eq(english_percent, "75.43%")
  
  Formatter::set_locale(formatter, "fr-FR")
  let french_percent = Formatter::format_percentage(formatter, 0.7543)
  assert_eq(french_percent, "75,43 %")
  
  // Test date formatting
  let test_date = DateTime::new(2023, 12, 25, 14, 30, 0)
  
  Formatter::set_locale(formatter, "en-US")
  let english_date = Formatter::format_date(formatter, test_date, "long")
  assert_eq(english_date, "December 25, 2023")
  
  Formatter::set_locale(formatter, "fr-FR")
  let french_date = Formatter::format_date(formatter, test_date, "long")
  assert_eq(french_date, "25 décembre 2023")
  
  Formatter::set_locale(formatter, "zh-CN")
  let chinese_date = Formatter::format_date(formatter, test_date, "long")
  assert_eq(chinese_date, "2023年12月25日")
  
  // Test time formatting
  Formatter::set_locale(formatter, "en-US")
  let english_time = Formatter::format_time(formatter, test_date, "medium")
  assert_eq(english_time, "2:30:00 PM")
  
  Formatter::set_locale(formatter, "fr-FR")
  let french_time = Formatter::format_time(formatter, test_date, "medium")
  assert_eq(french_time, "14:30:00")
  
  // Test date-time formatting
  Formatter::set_locale(formatter, "en-US")
  let english_datetime = Formatter::format_datetime(formatter, test_date, "medium")
  assert_eq(english_datetime, "Dec 25, 2023, 2:30:00 PM")
  
  Formatter::set_locale(formatter, "zh-CN")
  let chinese_datetime = Formatter::format_datetime(formatter, test_date, "medium")
  assert_eq(chinese_datetime, "2023年12月25日 下午2:30:00")
}

// Test 4: Pluralization Rules
test "pluralization rules" {
  let pluralization_manager = PluralizationManager::new()
  
  // Add pluralization rules for different locales
  PluralizationManager::add_rule(pluralization_manager, "en-US", EnglishPluralRule::new())
  PluralizationManager::add_rule(pluralization_manager, "zh-CN", ChinesePluralRule::new())
  PluralizationManager::add_rule(pluralization_manager, "fr-FR", FrenchPluralRule::new())
  PluralizationManager::add_rule(pluralization_manager, "ja-JP", JapanesePluralRule::new())
  PluralizationManager::add_rule(pluralization_manager, "ar-SA", ArabicPluralRule::new())
  
  // Add pluralized translations
  PluralizationManager::add_plural_translation(pluralization_manager, "en-US", "message.count", [
    (One, "You have {count} message"),
    (Other, "You have {count} messages")
  ])
  
  PluralizationManager::add_plural_translation(pluralization_manager, "zh-CN", "message.count", [
    (Other, "您有 {count} 条消息")
  ])
  
  PluralizationManager::add_plural_translation(pluralization_manager, "fr-FR", "message.count", [
    (One, "Vous avez {count} message"),
    (Other, "Vous avez {count} messages")
  ])
  
  PluralizationManager::add_plural_translation(pluralization_manager, "ja-JP", "message.count", [
    (Other, "{count}件のメッセージがあります")
  ])
  
  PluralizationManager::add_plural_translation(pluralization_manager, "ar-SA", "message.count", [
    (Zero, "ليس لديك رسائل"),
    (One, "لديك رسالة واحدة"),
    (Two, "لديك رسالتان"),
    (Few, "لديك {count} رسائل"),
    (Many, "لديك {count} رسالة"),
    (Other, "لديك {count} رسالة")
  ])
  
  // Test English pluralization
  PluralizationManager::set_locale(pluralization_manager, "en-US")
  
  let english_one = PluralizationManager::translate_plural(pluralization_manager, "message.count", 1)
  match english_one {
    Some(message) => assert_eq(message, "You have 1 message")
    None => assert_true(false)
  }
  
  let english_many = PluralizationManager::translate_plural(pluralization_manager, "message.count", 5)
  match english_many {
    Some(message) => assert_eq(message, "You have 5 messages")
    None => assert_true(false)
  }
  
  // Test Chinese pluralization (doesn't have plural forms)
  PluralizationManager::set_locale(pluralization_manager, "zh-CN")
  
  let chinese_one = PluralizationManager::translate_plural(pluralization_manager, "message.count", 1)
  match chinese_one {
    Some(message) => assert_eq(message, "您有 1 条消息")
    None => assert_true(false)
  }
  
  let chinese_many = PluralizationManager::translate_plural(pluralization_manager, "message.count", 5)
  match chinese_many {
    Some(message) => assert_eq(message, "您有 5 条消息")
    None => assert_true(false)
  }
  
  // Test Arabic pluralization (complex rules)
  PluralizationManager::set_locale(pluralization_manager, "ar-SA")
  
  let arabic_zero = PluralizationManager::translate_plural(pluralization_manager, "message.count", 0)
  match arabic_zero {
    Some(message) => assert_eq(message, "ليس لديك رسائل")
    None => assert_true(false)
  }
  
  let arabic_one = PluralizationManager::translate_plural(pluralization_manager, "message.count", 1)
  match arabic_one {
    Some(message) => assert_eq(message, "لديك رسالة واحدة")
    None => assert_true(false)
  }
  
  let arabic_two = PluralizationManager::translate_plural(pluralization_manager, "message.count", 2)
  match arabic_two {
    Some(message) => assert_eq(message, "لديك رسالتان")
    None => assert_true(false)
  }
  
  let arabic_few = PluralizationManager::translate_plural(pluralization_manager, "message.count", 5)
  match arabic_few {
    Some(message) => assert_eq(message, "لديك 5 رسائل")
    None => assert_true(false)
  }
  
  let arabic_many = PluralizationManager::translate_plural(pluralization_manager, "message.count", 11)
  match arabic_many {
    Some(message) => assert_eq(message, "لديك 11 رسالة")
    None => assert_true(false)
  }
}

// Test 5: Right-to-Left (RTL) Language Support
test "right-to-left language support" {
  let rtl_manager = RTLManager::new()
  
  // Add RTL locales
  RTLManager::add_rtl_locale(rtl_manager, "ar-SA")
  RTLManager::add_rtl_locale(rtl_manager, "he-IL")
  RTLManager::add_rtl_locale(rtl_manager, "fa-IR")
  RTLManager::add_rtl_locale(rtl_manager, "ur-PK")
  
  // Test RTL detection
  assert_true(RTLManager::is_rtl_locale(rtl_manager, "ar-SA"))
  assert_true(RTLManager::is_rtl_locale(rtl_manager, "he-IL"))
  assert_false(RTLManager::is_rtl_locale(rtl_manager, "en-US"))
  assert_false(RTLManager::is_rtl_locale(rtl_manager, "zh-CN"))
  
  // Test text direction
  assert_eq(RTLManager::get_text_direction(rtl_manager, "ar-SA"), "rtl")
  assert_eq(RTLManager::get_text_direction(rtl_manager, "en-US"), "ltr")
  
  // Test layout mirroring
  let ltr_layout = {
    header: "left",
    content: "center",
    sidebar: "right",
    navigation: "left"
  }
  
  let rtl_layout = RTLManager::mirror_layout(rtl_manager, ltr_layout, "ar-SA")
  
  assert_eq(rtl_layout.header, "right")
  assert_eq(rtl_layout.content, "center") // Center stays the same
  assert_eq(rtl_layout.sidebar, "left")
  assert_eq(rtl_layout.navigation, "right")
  
  // Test text alignment
  assert_eq(RTLManager::get_text_align(rtl_manager, "ar-SA", "start"), "right")
  assert_eq(RTLManager::get_text_align(rtl_manager, "ar-SA", "end"), "left")
  assert_eq(RTLManager::get_text_align(rtl_manager, "en-US", "start"), "left")
  assert_eq(RTLManager::get_text_align(rtl_manager, "en-US", "end"), "right")
  
  // Test margin/padding direction
  let ltr_styles = {
    margin_start: "10px",
    margin_end: "20px",
    padding_start: "5px",
    padding_end: "15px"
  }
  
  let rtl_styles = RTLManager::mirror_styles(rtl_manager, ltr_styles, "ar-SA")
  
  assert_eq(rtl_styles.margin_start, "20px")
  assert_eq(rtl_styles.margin_end, "10px")
  assert_eq(rtl_styles.padding_start, "15px")
  assert_eq(rtl_styles.padding_end, "5px")
}

// Test 6: Cultural and Regional Adaptations
test "cultural and regional adaptations" {
  let cultural_manager = CulturalAdaptationManager::new()
  
  // Add cultural rules for different regions
  CulturalAdaptationManager::add_rules(cultural_manager, "en-US", {
    date_format: "MM/DD/YYYY",
    time_format: "12-hour",
    week_start: "sunday",
    measurement_system: "imperial",
    paper_size: "letter",
    currency_symbol: "$",
    currency_position: "before"
  })
  
  CulturalAdaptationManager::add_rules(cultural_manager, "zh-CN", {
    date_format: "YYYY-MM-DD",
    time_format: "24-hour",
    week_start: "monday",
    measurement_system: "metric",
    paper_size: "a4",
    currency_symbol: "¥",
    currency_position: "before"
  })
  
  CulturalAdaptationManager::add_rules(cultural_manager, "fr-FR", {
    date_format: "DD/MM/YYYY",
    time_format: "24-hour",
    week_start: "monday",
    measurement_system: "metric",
    paper_size: "a4",
    currency_symbol: "€",
    currency_position: "after"
  })
  
  CulturalAdaptationManager::add_rules(cultural_manager, "ja-JP", {
    date_format: "YYYY/MM/DD",
    time_format: "24-hour",
    week_start: "sunday",
    measurement_system: "metric",
    paper_size: "a4",
    currency_symbol: "¥",
    currency_position: "before"
  })
  
  // Test date format adaptation
  CulturalAdaptationManager::set_region(cultural_manager, "en-US")
  let us_date_format = CulturalAdaptationManager::get_date_format(cultural_manager)
  assert_eq(us_date_format, "MM/DD/YYYY")
  
  CulturalAdaptationManager::set_region(cultural_manager, "fr-FR")
  let fr_date_format = CulturalAdaptationManager::get_date_format(cultural_manager)
  assert_eq(fr_date_format, "DD/MM/YYYY")
  
  // Test measurement system adaptation
  CulturalAdaptationManager::set_region(cultural_manager, "en-US")
  assert_eq(CulturalAdaptationManager::get_measurement_system(cultural_manager), "imperial")
  
  CulturalAdaptationManager::set_region(cultural_manager, "zh-CN")
  assert_eq(CulturalAdaptationManager::get_measurement_system(cultural_manager), "metric")
  
  // Test unit conversion
  let weight_kg = 70.0
  let weight_lb = CulturalAdaptationManager::convert_weight(cultural_manager, weight_kg, "kg", "lb")
  assert_true(weight_lb > 150.0 && weight_lb < 155.0) // 70kg ≈ 154.3lb
  
  let distance_miles = 10.0
  let distance_km = CulturalAdaptationManager::convert_distance(cultural_manager, distance_miles, "miles", "km")
  assert_true(distance_km > 16.0 && distance_km < 17.0) // 10 miles ≈ 16.09km
  
  // Test currency formatting position
  CulturalAdaptationManager::set_region(cultural_manager, "en-US")
  let us_currency = CulturalAdaptationManager::format_currency(cultural_manager, 1234.56, "USD")
  assert_eq(us_currency, "$1,234.56")
  
  CulturalAdaptationManager::set_region(cultural_manager, "fr-FR")
  let fr_currency = CulturalAdaptationManager::format_currency(cultural_manager, 1234.56, "EUR")
  assert_eq(fr_currency, "1 234,56 €")
  
  // Test cultural color adaptations
  let color_meanings = CulturalAdaptationManager::get_color_meanings(cultural_manager, "red")
  match color_meanings {
    Some(meanings) => {
      assert_true(meanings.contains("danger"))
      assert_true(meanings.contains("stop"))
      assert_true(meanings.contains("error"))
    }
    None => assert_true(false)
  }
  
  // Test cultural name formatting
  let name = {
    first: "John",
    middle: "F.",
    last: "Kennedy"
  }
  
  CulturalAdaptationManager::set_region(cultural_manager, "en-US")
  let us_name_format = CulturalAdaptationManager::format_name(cultural_manager, name)
  assert_eq(us_name_format, "John F. Kennedy")
  
  CulturalAdaptationManager::set_region(cultural_manager, "ja-JP")
  let jp_name_format = CulturalAdaptationManager::format_name(cultural_manager, name)
  assert_eq(jp_name_format, "Kennedy John F.")
}

// Test 7: Time Zone and Calendar Support
test "time zone and calendar support" {
  let timezone_manager = TimezoneManager::new()
  
  // Add time zones
  TimezoneManager::add_timezone(timezone_manager, "America/New_York", "UTC-5")
  TimezoneManager::add_timezone(timezone_manager, "Europe/London", "UTC+0")
  Timezone_manager::add_timezone(timezone_manager, "Asia/Shanghai", "UTC+8")
  TimezoneManager::add_timezone(timezone_manager, "Asia/Tokyo", "UTC+9")
  TimezoneManager::add_timezone(timezone_manager, "Australia/Sydney", "UTC+10")
  
  // Test time zone conversion
  let utc_time = DateTime::new(2023, 12, 25, 12, 0, 0)
  
  let ny_time = TimezoneManager::convert_from_utc(timezone_manager, utc_time, "America/New_York")
  assert_eq(ny_time.hour, 7) // 12:00 UTC = 7:00 EST
  
  let shanghai_time = TimezoneManager::convert_from_utc(timezone_manager, utc_time, "Asia/Shanghai")
  assert_eq(shanghai_time.hour, 20) // 12:00 UTC = 20:00 CST
  
  let tokyo_time = TimezoneManager::convert_from_utc(timezone_manager, utc_time, "Asia/Tokyo")
  assert_eq(tokyo_time.hour, 21) // 12:00 UTC = 21:00 JST
  
  // Test time zone detection
  TimezoneManager::set_system_timezone(timezone_manager, "America/New_York")
  assert_eq(TimezoneManager::get_system_timezone(timezone_manager), "America/New_York")
  
  // Test calendar systems
  let calendar_manager = CalendarManager::new()
  
  CalendarManager::add_calendar(calendar_manager, "gregorian", GregorianCalendar::new())
  CalendarManager::add_calendar(calendar_manager, "islamic", IslamicCalendar::new())
  CalendarManager::add_calendar(calendar_manager, "hebrew", HebrewCalendar::new())
  CalendarManager::add_calendar(calendar_manager, "chinese", ChineseCalendar::new())
  CalendarManager::add_calendar(calendar_manager, "japanese", JapaneseCalendar::new())
  
  // Test calendar conversion
  let gregorian_date = DateTime::new(2023, 12, 25, 0, 0, 0)
  
  CalendarManager::set_calendar(calendar_manager, "islamic")
  let islamic_date = CalendarManager::convert_date(calendar_manager, gregorian_date)
  assert_eq(islamic_date.year, 1445) // Approximate Islamic year
  
  CalendarManager::set_calendar(calendar_manager, "hebrew")
  let hebrew_date = CalendarManager::convert_date(calendar_manager, gregorian_date)
  assert_eq(hebrew_date.year, 5784) // Approximate Hebrew year
  
  CalendarManager::set_calendar(calendar_manager, "chinese")
  let chinese_date = CalendarManager::convert_date(calendar_manager, gregorian_date)
  assert_eq(chinese_date.year, 4721) // Approximate Chinese year
  
  // Test holiday detection
  let us_holidays = CalendarManager::get_holidays(calendar_manager, "US", 2023)
  assert_true(us_holidays.length() > 0)
  
  let christmas = us_holidays.find(|h| Holiday::name(h) == "Christmas Day")
  match christmas {
    Some(holiday) => {
      assert_eq(Holiday::date(holiday).month, 12)
      assert_eq(Holiday::date(holiday).day, 25)
    }
    None => assert_true(false)
  }
  
  let chinese_holidays = CalendarManager::get_holidays(calendar_manager, "CN", 2023)
  assert_true(chinese_holidays.length() > 0)
  
  let spring_festival = chinese_holidays.find(|h| Holiday::name(h) == "Spring Festival")
  match spring_festival {
    Some(holiday) => {
      assert_eq(Holiday::date(holiday).month, 1) // Usually in January
    }
    None => assert_true(false)
  }
}

// Test 8: Input Method and Keyboard Layout Support
test "input method and keyboard layout support" {
  let input_manager = InputMethodManager::new()
  
  // Add keyboard layouts
  InputMethodManager::add_layout(input_manager, "en-US", QWERTYLayout::new())
  InputMethodManager::add_layout(input_manager, "fr-FR", AZERTYLayout::new())
  InputMethodManager::add_layout(input_manager, "de-DE", QWERTZLayout::new())
  InputMethodManager::add_layout(input_manager, "zh-CN", PinyinLayout::new())
  InputMethodManager::add_layout(input_manager, "ja-JP", RomajiLayout::new())
  InputMethodManager::add_layout(input_manager, "ar-SA", ArabicLayout::new())
  InputMethodManager::add_layout(input_manager, "ru-RU", CyrillicLayout::new())
  
  // Test keyboard layout detection
  InputMethodManager::set_layout(input_manager, "en-US")
  assert_eq(InputMethodManager::get_current_layout(input_manager), "en-US")
  
  // Test character input prediction
  let english_prediction = InputMethodManager::predict_character(input_manager, "a")
  assert_eq(english_prediction, "a")
  
  InputMethodManager::set_layout(input_manager, "fr-FR")
  let french_prediction = InputMethodManager::predict_character(input_manager, "a")
  assert_eq(french_prediction, "q") // 'a' key produces 'q' in AZERTY layout
  
  // Test IME (Input Method Editor) support
  let ime_manager = IMEManager::new()
  
  IMEManager::add_ime(ime_manager, "zh-CN", PinyinIME::new())
  IMEManager::add_ime(ime_manager, "ja-JP", RomajiIME::new())
  IMEManager::add_ime(ime_manager, "ko-KR", HangulIME::new())
  IMEManager::add_ime(ime_manager, "ar-SA", ArabicIME::new())
  
  // Test pinyin input
  IMEManager::set_ime(ime_manager, "zh-CN")
  
  let pinyin_input = "nihao"
  let pinyin_candidates = IMEManager::get_candidates(ime_manager, pinyin_input)
  
  assert_true(pinyin_candidates.length() > 0)
  assert_true(pinyin_candidates.contains("你好"))
  assert_true(pinyin_candidates.contains("你号"))
  
  // Test romaji input
  IMEManager::set_ime(ime_manager, "ja-JP")
  
  let romaji_input = "konnichiha"
  let romaji_candidates = IMEManager::get_candidates(ime_manager, romaji_input)
  
  assert_true(romaji_candidates.length() > 0)
  assert_true(romaji_candidates.contains("こんにちは"))
  
  // Test text direction in input
  InputMethodManager::set_layout(input_manager, "ar-SA")
  let arabic_text_direction = InputMethodManager::get_input_direction(input_manager)
  assert_eq(arabic_text_direction, "rtl")
  
  InputMethodManager::set_layout(input_manager, "en-US")
  let english_text_direction = InputMethodManager::get_input_direction(input_manager)
  assert_eq(english_text_direction, "ltr")
}

// Test 9: Font and Text Rendering Support
test "font and text rendering support" {
  let font_manager = FontManager::new()
  
  // Add font families for different locales
  FontManager::add_font_family(font_manager, "en-US", [
    "Arial",
    "Helvetica",
    "Times New Roman",
    "Georgia"
  ])
  
  FontManager::add_font_family(font_manager, "zh-CN", [
    "Microsoft YaHei",
    "SimSun",
    "SimHei",
    "KaiTi"
  ])
  
  FontManager::add_font_family(font_manager, "ja-JP", [
    "MS Gothic",
    "MS Mincho",
    "Hiragino Sans",
    "Yu Gothic"
  ])
  
  FontManager::add_font_family(font_manager, "ar-SA", [
    "Arial Unicode MS",
    "Tahoma",
    "Traditional Arabic",
    "Scheherazade"
  ])
  
  FontManager::add_font_family(font_manager, "ko-KR", [
    "Malgun Gothic",
    "Batang",
    "Gulim",
    "Dotum"
  ])
  
  // Test font selection
  FontManager::set_locale(font_manager, "en-US")
  let english_fonts = FontManager::get_available_fonts(font_manager)
  assert_eq(english_fonts[0], "Arial")
  
  FontManager::set_locale(font_manager, "zh-CN")
  let chinese_fonts = FontManager::get_available_fonts(font_manager)
  assert_eq(chinese_fonts[0], "Microsoft YaHei")
  
  FontManager::set_locale(font_manager, "ar-SA")
  let arabic_fonts = FontManager::get_available_fonts(font_manager)
  assert_eq(arabic_fonts[0], "Arial Unicode MS")
  
  // Test font fallback
  let test_text = "Hello 你好 こんにちは مرحبا 안녕하세요"
  let font_fallbacks = FontManager::get_font_fallbacks(font_manager, test_text)
  
  assert_true(font_fallbacks.length() > 1)
  assert_true(font_fallbacks.contains("Arial")) // For English
  assert_true(font_fallbacks.contains("Microsoft YaHei")) // For Chinese
  assert_true(font_fallbacks.contains("MS Gothic")) // For Japanese
  assert_true(font_fallbacks.contains("Arial Unicode MS")) // For Arabic
  
  // Test text rendering metrics
  let renderer = TextRenderer::new()
  
  Renderer::set_locale(renderer, "en-US")
  let english_metrics = Renderer::measure_text(renderer, "Hello World", "Arial", 16)
  assert_true(english_metrics.width > 0)
  assert_eq(english_metrics.height, 16)
  
  Renderer::set_locale(renderer, "zh-CN")
  let chinese_metrics = Renderer::measure_text(renderer, "你好世界", "Microsoft YaHei", 16)
  assert_true(chinese_metrics.width > 0)
  assert_eq(chinese_metrics.height, 16)
  
  // Test line breaking for different locales
  let english_text = "This is a long English sentence that should be broken into multiple lines when it exceeds the maximum width."
  let english_lines = Renderer::break_lines(renderer, english_text, "Arial", 16, 300)
  assert_true(english_lines.length() > 1)
  
  let chinese_text = "这是一个很长的中文句子，当它超过最大宽度时应该被分成多行。"
  let chinese_lines = Renderer::break_lines(renderer, chinese_text, "Microsoft YaHei", 16, 300)
  assert_true(chinese_lines.length() > 1)
  
  // Test text shaping for complex scripts
  Renderer::set_locale(renderer, "ar-SA")
  let arabic_text = "مرحبا بالعالم"
  let arabic_shaped = Renderer::shape_text(renderer, arabic_text, "Arial Unicode MS", 16)
  assert_true(arabic_shaped.length() > 0)
  
  // Test bidirectional text rendering
  let bidi_text = "Hello مرحبا World"
  let bidi_rendered = Renderer::render_bidi_text(renderer, bidi_text, "Arial Unicode MS", 16)
  assert_true(bidi_rendered.length() > 0)
}

// Test 10: Accessibility and Internationalization
test "accessibility and internationalization" {
  let accessibility_manager = AccessibilityManager::new()
  
  // Add accessibility adaptations for different locales
  AccessibilityManager::add_adaptation(accessibility_manager, "en-US", {
    screen_reader_language: "en",
    high_contrast_colors: ["#FFFFFF", "#000000", "#FF0000", "#0000FF"],
    font_size_scaling: 1.0,
    keyboard_shortcuts: {
      "next": "Tab",
      "previous": "Shift+Tab",
      "activate": "Enter",
      "menu": "Alt"
    }
  })
  
  AccessibilityManager::add_adaptation(accessibility_manager, "zh-CN", {
    screen_reader_language: "zh",
    high_contrast_colors: ["#FFFFFF", "#000000", "#FF0000", "#0000FF"],
    font_size_scaling: 1.2, // Larger fonts for Chinese characters
    keyboard_shortcuts: {
      "next": "Tab",
      "previous": "Shift+Tab",
      "activate": "Enter",
      "menu": "Alt"
    }
  })
  
  AccessibilityManager::add_adaptation(accessibility_manager, "ar-SA", {
    screen_reader_language: "ar",
    high_contrast_colors: ["#FFFFFF", "#000000", "#FF0000", "#0000FF"],
    font_size_scaling: 1.1,
    keyboard_shortcuts: {
      "next": "Tab",
      "previous": "Shift+Tab",
      "activate": "Enter",
      "menu": "Alt"
    }
  })
  
  // Test screen reader language
  AccessibilityManager::set_locale(accessibility_manager, "en-US")
  assert_eq(AccessibilityManager::get_screen_reader_language(accessibility_manager), "en")
  
  AccessibilityManager::set_locale(accessibility_manager, "zh-CN")
  assert_eq(AccessibilityManager::get_screen_reader_language(accessibility_manager), "zh")
  
  // Test high contrast colors
  AccessibilityManager::set_locale(accessibility_manager, "en-US")
  let us_colors = AccessibilityManager::get_high_contrast_colors(accessibility_manager)
  assert_eq(us_colors[0], "#FFFFFF")
  assert_eq(us_colors[1], "#000000")
  
  // Test font size scaling
  AccessibilityManager::set_locale(accessibility_manager, "zh-CN")
  let cn_scaling = AccessibilityManager::get_font_size_scaling(accessibility_manager)
  assert_eq(cn_scaling, 1.2)
  
  // Test ARIA (Accessible Rich Internet Applications) localization
  let aria_manager = ARIALocalizationManager::new()
  
  ARIALocalizationManager::add_translation(aria_manager, "en-US", "aria.label.close", "Close dialog")
  ARIALocalizationManager::add_translation(aria_manager, "zh-CN", "aria.label.close", "关闭对话框")
  ARIALocalizationManager::add_translation(aria_manager, "fr-FR", "aria.label.close", "Fermer la boîte de dialogue")
  ARIALocalizationManager::add_translation(aria_manager, "ar-SA", "aria.label.close", "إغلاق مربع الحوار")
  
  ARIALocalizationManager::set_locale(aria_manager, "zh-CN")
  let cn_aria_label = ARIALocalizationManager::translate(aria_manager, "aria.label.close")
  match cn_aria_label {
    Some(label) => assert_eq(label, "关闭对话框")
    None => assert_true(false)
  }
  
  // Test keyboard navigation for RTL languages
  AccessibilityManager::set_locale(accessibility_manager, "ar-SA")
  let rtl_navigation = AccessibilityManager::get_keyboard_navigation(accessibility_manager)
  
  // In RTL, "next" should move right visually but left in DOM order
  assert_eq(rtl_navigation.next_visual_direction, "right")
  assert_eq(rtl_navigation.next_dom_direction, "left")
  
  // Test accessibility validation for internationalized content
  let validator = AccessibilityValidator::new()
  
  Validator::set_locale(validator, "en-US")
  let us_validation = Validator::validate_i18n_content(validator, "Welcome to our application")
  assert_true(us_validation.passed)
  
  Validator::set_locale(validator, "zh-CN")
  let cn_validation = Validator::validate_i18n_content(validator, "欢迎使用我们的应用程序")
  assert_true(cn_validation.passed)
  
  Validator::set_locale(validator, "ar-SA")
  let ar_validation = Validator::validate_i18n_content(validator, "مرحبا بكم في تطبيقنا")
  assert_true(ar_validation.passed)
  
  // Test accessibility report generation
  let report = AccessibilityManager::generate_accessibility_report(accessibility_manager)
  
  match report {
    AccessibilityReport(locale, screen_reader_support, high_contrast_support, keyboard_navigation_support, font_scaling, recommendations) => {
      assert_true(locale.length() > 0)
      assert_true(screen_reader_support)
      assert_true(high_contrast_support)
      assert_true(keyboard_navigation_support)
      assert_true(font_scaling > 0.0)
      assert_true(recommendations.length() >= 0)
    }
    _ => assert_true(false)
  }
}