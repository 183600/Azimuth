// Azimuth Telemetry System - Internationalization Support Tests
// This file contains comprehensive test cases for internationalization and localization

// Test 1: Basic Locale Handling
test "basic locale handling" {
  // Test default locale
  let default_locale = Locale::default()
  assert_eq(default_locale.language, "en")
  assert_eq(default_locale.region, "US")
  assert_eq(default_locale.to_string(), "en-US")
  
  // Test locale creation from string
  let us_locale = Locale::from_string("en-US")
  assert_eq(us_locale.language, "en")
  assert_eq(us_locale.region, "US")
  
  let gb_locale = Locale::from_string("en-GB")
  assert_eq(gb_locale.language, "en")
  assert_eq(gb_locale.region, "GB")
  
  let zh_locale = Locale::from_string("zh-CN")
  assert_eq(zh_locale.language, "zh")
  assert_eq(zh_locale.region, "CN")
  
  let fr_locale = Locale::from_string("fr-FR")
  assert_eq(fr_locale.language, "fr")
  assert_eq(fr_locale.region, "FR")
  
  // Test locale with language only
  let lang_only_locale = Locale::from_string("es")
  assert_eq(lang_only_locale.language, "es")
  assert_eq(lang_only_locale.region, "")
  
  // Test locale comparison
  assert_eq(us_locale, Locale::from_string("en-US"))
  assert_true(us_locale != gb_locale)
  assert_true(us_locale != zh_locale)
  
  // Test locale fallback
  let fallback_locale = Locale::fallback(zh_locale, [Locale::from_string("zh-TW"), Locale::from_string("en-US")])
  assert_eq(fallback_locale, Locale::from_string("en-US"))
  
  let exact_match_locale = Locale::fallback(zh_locale, [Locale::from_string("fr-FR"), Locale::from_string("zh-CN")])
  assert_eq(exact_match_locale, Locale::from_string("zh-CN"))
  
  let language_match_locale = Locale::fallback(zh_locale, [Locale::from_string("fr-FR"), Locale::from_string("zh-TW")])
  assert_eq(language_match_locale, Locale::from_string("zh-TW"))
}

// Test 2: Number Formatting
test "number formatting" {
  // Test decimal number formatting
  let number = 1234567.89
  
  // US English formatting
  let us_formatter = NumberFormatter::new(Locale::from_string("en-US"))
  let us_formatted = us_formatter.format(number)
  assert_eq(us_formatted, "1,234,567.89")
  
  // German formatting
  let de_formatter = NumberFormatter::new(Locale::from_string("de-DE"))
  let de_formatted = de_formatter.format(number)
  assert_eq(de_formatted, "1.234.567,89")
  
  // French formatting
  let fr_formatter = NumberFormatter::new(Locale::from_string("fr-FR"))
  let fr_formatted = fr_formatter.format(number)
  assert_eq(fr_formatted, "1\u{00A0}234\u{00A0}567,89")  // Non-breaking space as thousands separator
  
  // Chinese formatting
  let zh_formatter = NumberFormatter::new(Locale::from_string("zh-CN"))
  let zh_formatted = zh_formatter.format(number)
  assert_eq(zh_formatted, "1,234,567.89")
  
  // Indian formatting
  let hi_formatter = NumberFormatter::new(Locale::from_string("hi-IN"))
  let hi_formatted = hi_formatter.format(number)
  assert_eq(hi_formatted, "12,34,567.89")
  
  // Test parsing formatted numbers
  let us_parsed = us_formatter.parse("1,234,567.89")
  match us_parsed {
    Some(value) => assert_eq(value, number),
    None => assert_true(false)
  }
  
  let de_parsed = de_formatter.parse("1.234.567,89")
  match de_parsed {
    Some(value) => assert_eq(value, number),
    None => assert_true(false)
  }
  
  // Test percentage formatting
  let percentage = 0.1234
  let us_percentage = us_formatter.format_percentage(percentage)
  assert_eq(us_percentage, "12.34%")
  
  let fr_percentage = fr_formatter.format_percentage(percentage)
  assert_eq(fr_percentage, "12,34\u{00A0}%")
  
  // Test currency formatting
  let currency = 1234.56
  let us_currency = us_formatter.format_currency(currency, "USD")
  assert_eq(us_currency, "$1,234.56")
  
  let eu_currency = de_formatter.format_currency(currency, "EUR")
  assert_eq(eu_currency, "1.234,56\u{00A0}\u{20AC}")
  
  let jp_currency = NumberFormatter::new(Locale::from_string("ja-JP")).format_currency(currency, "JPY")
  assert_eq(jp_currency, "\u{00A5}1,235")  // No decimal places for JPY
}

// Test 3: Date and Time Formatting
test "date and time formatting" {
  // Test timestamp
  let timestamp = 1589273771000  // 2020-05-12 12:16:11 UTC
  
  // US English date formatting
  let us_formatter = DateFormatter::new(Locale::from_string("en-US"))
  let us_date = us_formatter.format_date(timestamp)
  assert_eq(us_date, "5/12/2020")
  
  let us_time = us_formatter.format_time(timestamp)
  assert_eq(us_time, "12:16:11 PM")
  
  let us_datetime = us_formatter.format_datetime(timestamp)
  assert_eq(us_datetime, "5/12/2020, 12:16:11 PM")
  
  // British English date formatting
  let gb_formatter = DateFormatter::new(Locale::from_string("en-GB"))
  let gb_date = gb_formatter.format_date(timestamp)
  assert_eq(gb_date, "12/05/2020")
  
  let gb_time = gb_formatter.format_time(timestamp)
  assert_eq(gb_time, "12:16:11")
  
  let gb_datetime = gb_formatter.format_datetime(timestamp)
  assert_eq(gb_datetime, "12/05/2020, 12:16:11")
  
  // Chinese date formatting
  let zh_formatter = DateFormatter::new(Locale::from_string("zh-CN"))
  let zh_date = zh_formatter.format_date(timestamp)
  assert_eq(zh_date, "2020/5/12")
  
  let zh_time = zh_formatter.format_time(timestamp)
  assert_eq(zh_time, "下午12:16:11")
  
  let zh_datetime = zh_formatter.format_datetime(timestamp)
  assert_eq(zh_datetime, "2020/5/12 下午12:16:11")
  
  // German date formatting
  let de_formatter = DateFormatter::new(Locale::from_string("de-DE"))
  let de_date = de_formatter.format_date(timestamp)
  assert_eq(de_date, "12.5.2020")
  
  let de_time = de_formatter.format_time(timestamp)
  assert_eq(de_time, "12:16:11")
  
  let de_datetime = de_formatter.format_datetime(timestamp)
  assert_eq(de_datetime, "12.5.2020, 12:16:11")
  
  // Japanese date formatting
  let ja_formatter = DateFormatter::new(Locale::from_string("ja-JP"))
  let ja_date = ja_formatter.format_date(timestamp)
  assert_eq(ja_date, "2020/05/12")
  
  let ja_time = ja_formatter.format_time(timestamp)
  assert_eq(ja_time, "12:16:11")
  
  let ja_datetime = ja_formatter.format_datetime(timestamp)
  assert_eq(ja_datetime, "2020/05/12 12:16:11")
  
  // Test custom date format
  let custom_format = "yyyy-MM-dd HH:mm:ss"
  let custom_date = us_formatter.format_custom(timestamp, custom_format)
  assert_eq(custom_date, "2020-05-12 12:16:11")
  
  // Test parsing formatted dates
  let us_parsed = us_formatter.parse_date("5/12/2020")
  match us_parsed {
    Some(value) => {
      let parsed_date = us_formatter.format_date(value)
      assert_eq(parsed_date, "5/12/2020")
    }
    None => assert_true(false)
  }
}

// Test 4: Message Translation
test "message translation" {
  // Create translation resources
  let translations = Translations::new()
  
  // Add English translations
  translations.add("en-US", "welcome.message", "Welcome to our application!")
  translations.add("en-US", "goodbye.message", "Thank you for using our application. Goodbye!")
  translations.add("en-US", "error.not_found", "The requested resource was not found.")
  translations.add("en-US", "items.count", "You have {0} items in your cart.")
  translations.add("en-US", "items.count_plural", "You have {0} items in your cart.")
  
  // Add Chinese translations
  translations.add("zh-CN", "welcome.message", "欢迎使用我们的应用程序！")
  translations.add("zh-CN", "goodbye.message", "感谢使用我们的应用程序。再见！")
  translations.add("zh-CN", "error.not_found", "未找到请求的资源。")
  translations.add("zh-CN", "items.count", "您的购物车中有 {0} 件商品。")
  translations.add("zh-CN", "items.count_plural", "您的购物车中有 {0} 件商品。")
  
  // Add French translations
  translations.add("fr-FR", "welcome.message", "Bienvenue dans notre application !")
  translations.add("fr-FR", "goodbye.message", "Merci d'utiliser notre application. Au revoir !")
  translations.add("fr-FR", "error.not_found", "La ressource demandée n'a pas été trouvée.")
  translations.add("fr-FR", "items.count", "Vous avez {0} article dans votre panier.")
  translations.add("fr-FR", "items.count_plural", "Vous avez {0} articles dans votre panier.")
  
  // Test English translations
  let en_locale = Locale::from_string("en-US")
  let en_welcome = translations.get(en_locale, "welcome.message")
  assert_eq(en_welcome, "Welcome to our application!")
  
  let en_goodbye = translations.get(en_locale, "goodbye.message")
  assert_eq(en_goodbye, "Thank you for using our application. Goodbye!")
  
  let en_error = translations.get(en_locale, "error.not_found")
  assert_eq(en_error, "The requested resource was not found.")
  
  // Test Chinese translations
  let zh_locale = Locale::from_string("zh-CN")
  let zh_welcome = translations.get(zh_locale, "welcome.message")
  assert_eq(zh_welcome, "欢迎使用我们的应用程序！")
  
  let zh_goodbye = translations.get(zh_locale, "goodbye.message")
  assert_eq(zh_goodbye, "感谢使用我们的应用程序。再见！")
  
  let zh_error = translations.get(zh_locale, "error.not_found")
  assert_eq(zh_error, "未找到请求的资源。")
  
  // Test French translations
  let fr_locale = Locale::from_string("fr-FR")
  let fr_welcome = translations.get(fr_locale, "welcome.message")
  assert_eq(fr_welcome, "Bienvenue dans notre application !")
  
  let fr_goodbye = translations.get(fr_locale, "goodbye.message")
  assert_eq(fr_goodbye, "Merci d'utiliser notre application. Au revoir !")
  
  let fr_error = translations.get(fr_locale, "error.not_found")
  assert_eq(fr_error, "La ressource demandée n'a pas été trouvée.")
  
  // Test parameterized translations
  let en_items_1 = translations.format(en_locale, "items.count", [1])
  assert_eq(en_items_1, "You have 1 items in your cart.")
  
  let en_items_5 = translations.format(en_locale, "items.count_plural", [5])
  assert_eq(en_items_5, "You have 5 items in your cart.")
  
  let fr_items_1 = translations.format(fr_locale, "items.count", [1])
  assert_eq(fr_items_1, "Vous avez 1 article dans votre panier.")
  
  let fr_items_5 = translations.format(fr_locale, "items.count_plural", [5])
  assert_eq(fr_items_5, "Vous avez 5 articles dans votre panier.")
  
  // Test fallback translations
  let es_locale = Locale::from_string("es-ES")
  let es_welcome = translations.get(es_locale, "welcome.message")
  assert_eq(es_welcome, "Welcome to our application!")  // Falls back to English
  
  // Test missing translation
  let missing_key = translations.get(en_locale, "nonexistent.key")
  assert_eq(missing_key, "nonexistent.key")  // Returns key when translation is missing
}

// Test 5: Right-to-Left Language Support
test "right-to-left language support" {
  // Test Arabic locale
  let ar_locale = Locale::from_string("ar-SA")
  
  // Test text direction
  assert_true(ar_locale.is_rtl())
  assert_false(Locale::from_string("en-US").is_rtl())
  
  // Test Arabic number formatting
  let ar_formatter = NumberFormatter::new(ar_locale)
  let ar_number = ar_formatter.format(1234567.89)
  assert_eq(ar_number, "1,234,567.89")
  
  // Test Arabic date formatting
  let ar_date_formatter = DateFormatter::new(ar_locale)
  let ar_date = ar_date_formatter.format_date(1589273771000)
  assert_eq(ar_date, "12/5/2020")
  
  // Test Hebrew locale
  let he_locale = Locale::from_string("he-IL")
  assert_true(he_locale.is_rtl())
  
  // Test Hebrew number formatting
  let he_formatter = NumberFormatter::new(he_locale)
  let he_number = he_formatter.format(1234567.89)
  assert_eq(he_number, "1,234,567.89")
  
  // Test mixed direction text handling
  let mixed_text = "Hello \u{05E9}\u{05DC}\u{05D5}\u{05DD} World"
  let processed_text = TextDirectionProcessor::process(mixed_text, ar_locale)
  assert_true(processed_text.contains("\u{202B}"))  // Contains RTL mark
}

// Test 6: Calendar and Time Zone Support
test "calendar and time zone support" {
  // Test Gregorian calendar (default)
  let gregorian_calendar = Calendar::new("gregorian")
  assert_eq(gregorian_calendar.name, "gregorian")
  
  // Test Islamic calendar
  let islamic_calendar = Calendar::new("islamic")
  assert_eq(islamic_calendar.name, "islamic")
  
  // Test Hebrew calendar
  let hebrew_calendar = Calendar::new("hebrew")
  assert_eq(hebrew_calendar.name, "hebrew")
  
  // Test Chinese calendar
  let chinese_calendar = Calendar::new("chinese")
  assert_eq(chinese_calendar.name, "chinese")
  
  // Test date conversion between calendars
  let gregorian_date = DateTime::new(2020, 5, 12, 12, 16, 11)
  let islamic_date = gregorian_date.to_calendar(islamic_calendar)
  
  // Verify conversion (approximate values for Islamic calendar)
  assert_eq(islamic_date.year, 1441)
  assert_eq(islamic_date.month, 9)
  assert_eq(islamic_date.day, 19)
  
  // Test time zone handling
  let utc_timezone = TimeZone::new("UTC")
  let pst_timezone = TimeZone::new("America/Los_Angeles")
  let jst_timezone = TimeZone::new("Asia/Tokyo")
  
  let utc_time = DateTime::new(2020, 5, 12, 12, 0, 0)
  let pst_time = utc_time.to_timezone(pst_timezone)
  let jst_time = utc_time.to_timezone(jst_timezone)
  
  // Verify time zone conversion
  assert_eq(pst_time.hour, 5)  // UTC-7 in May (daylight saving)
  assert_eq(pst_time.day, 12)
  
  assert_eq(jst_time.hour, 21)  // UTC+9
  assert_eq(jst_time.day, 12)
  
  // Test formatting with time zones
  let us_formatter = DateFormatter::new(Locale::from_string("en-US"))
  let utc_formatted = us_formatter.format_datetime_with_timezone(utc_time, utc_timezone)
  let pst_formatted = us_formatter.format_datetime_with_timezone(utc_time, pst_timezone)
  let jst_formatted = us_formatter.format_datetime_with_timezone(utc_time, jst_timezone)
  
  assert_true(utc_formatted.contains("12:00:00 PM"))
  assert_true(utc_formatted.contains("UTC"))
  
  assert_true(pst_formatted.contains("5:00:00 AM"))
  assert_true(pst_formatted.contains("PDT") || pst_formatted.contains("PST"))
  
  assert_true(jst_formatted.contains("9:00:00 PM"))
  assert_true(jst_formatted.contains("JST"))
}

// Test 7: Collation and Sorting
test "collation and sorting" {
  // Test English collation
  let en_collator = Collator::new(Locale::from_string("en-US"))
  let english_words = ["apple", "banana", "cherry", "date"]
  let sorted_english = Collator::sort(en_collator, english_words)
  assert_eq(sorted_english, ["apple", "banana", "cherry", "date"])
  
  // Test French collation (with accent sensitivity)
  let fr_collator = Collator::new(Locale::from_string("fr-FR"))
  let french_words = ["cote", "côte", "coté", "côté"]
  let sorted_french = Collator::sort(fr_collator, french_words)
  assert_eq(sorted_french, ["cote", "coté", "côte", "côté"])
  
  // Test German collation (with special characters)
  let de_collator = Collator::new(Locale::from_string("de-DE"))
  let german_words = ["äpfel", "bananen", "kirschen", "datteln"]
  let sorted_german = Collator::sort(de_collator, german_words)
  assert_eq(sorted_german, ["äpfel", "bananen", "datteln", "kirschen"])
  
  // Test Chinese collation
  let zh_collator = Collator::new(Locale::from_string("zh-CN"))
  let chinese_words = ["苹果", "香蕉", "樱桃", "枣"]
  let sorted_chinese = Collator::sort(zh_collator, chinese_words)
  assert_eq(sorted_chinese, ["枣", "苹果", "樱桃", "香蕉"])
  
  // Test Japanese collation
  let ja_collator = Collator::new(Locale::from_string("ja-JP"))
  let japanese_words = ["りんご", "バナナ", "チェリー", "なつめ"]
  let sorted_japanese = Collator::sort(ja_collator, japanese_words)
  assert_eq(sorted_japanese, ["りんご", "なつめ", "バナナ", "チェリー"])
  
  // Test comparison
  assert_true(Collator::compare(en_collator, "apple", "banana") < 0)
  assert_true(Collator::compare(en_collator, "banana", "apple") > 0)
  assert_true(Collator::compare(en_collator, "apple", "apple") == 0)
  
  // Test case sensitivity
  let case_sensitive_collator = Collator::new_with_options(Locale::from_string("en-US"), {"case_sensitive": true})
  assert_true(Collator::compare(case_sensitive_collator, "Apple", "apple") > 0)
  
  let case_insensitive_collator = Collator::new_with_options(Locale::from_string("en-US"), {"case_sensitive": false})
  assert_true(Collator::compare(case_insensitive_collator, "Apple", "apple") == 0)
}

// Test 8: Text Segmentation
test "text segmentation" {
  // Test word segmentation
  let en_segmenter = TextSegmenter::new(Locale::from_string("en-US"))
  let english_text = "Hello world! This is a test."
  let english_words = TextSegmenter::segment_words(en_segmenter, english_text)
  assert_eq(english_words, ["Hello", "world", "This", "is", "a", "test"])
  
  // Test sentence segmentation
  let english_sentences = TextSegmenter::segment_sentences(en_segmenter, english_text)
  assert_eq(english_sentences, ["Hello world!", "This is a test."])
  
  // Test Chinese text segmentation (Chinese doesn't use spaces between words)
  let zh_segmenter = TextSegmenter::new(Locale::from_string("zh-CN"))
  let chinese_text = "你好世界！这是一个测试。"
  let chinese_words = TextSegmenter::segment_words(zh_segmenter, chinese_text)
  assert_eq(chinese_words, ["你好", "世界", "这", "是", "一个", "测试"])
  
  let chinese_sentences = TextSegmenter::segment_sentences(zh_segmenter, chinese_text)
  assert_eq(chinese_sentences, ["你好世界！", "这是一个测试。"])
  
  // Test Japanese text segmentation
  let ja_segmenter = TextSegmenter::new(Locale::from_string("ja-JP"))
  let japanese_text = "こんにちは世界！これはテストです。"
  let japanese_words = TextSegmenter::segment_words(ja_segmenter, japanese_text)
  assert_eq(japanese_words, ["こんにちは", "世界", "これ", "は", "テスト", "です"])
  
  let japanese_sentences = TextSegmenter::segment_sentences(ja_segmenter, japanese_text)
  assert_eq(japanese_sentences, ["こんにちは世界！", "これはテストです。"])
  
  // Test Thai text segmentation (Thai doesn't use spaces between words)
  let th_segmenter = TextSegmenter::new(Locale::from_string("th-TH"))
  let thai_text = "สวัสดีชาวโลก! นี่คือการทดสอบ"
  let thai_words = TextSegmenter::segment_words(th_segmenter, thai_text)
  assert_eq(thai_words.length(), 6)  // Should be segmented into 6 words
  
  let thai_sentences = TextSegmenter::segment_sentences(th_segmenter, thai_text)
  assert_eq(thai_sentences, ["สวัสดีชาวโลก!", "นี่คือการทดสอบ"])
  
  // Test grapheme cluster segmentation (for complex scripts)
  let complex_text = "e\u{0301}"  // e + combining acute accent
  let graphemes = TextSegmenter::segment_graphemes(en_segmenter, complex_text)
  assert_eq(graphemes, ["\u{00E9}"])  // Should be combined into single grapheme
}

// Test 9: Internationalized Telemetry Data
test "internationalized telemetry data" {
  // Test localized span names
  let en_span = Span::with_localized_name("user.login", Locale::from_string("en-US"))
  assert_eq(en_span.name, "User Login")
  
  let zh_span = Span::with_localized_name("user.login", Locale::from_string("zh-CN"))
  assert_eq(zh_span.name, "用户登录")
  
  let fr_span = Span::with_localized_name("user.login", Locale::from_string("fr-FR"))
  assert_eq(fr_span.name, "Connexion Utilisateur")
  
  // Test localized attribute values
  let en_attributes = Attributes::new()
  Attributes::set_localized(en_attributes, "error.message", "invalid_credentials", Locale::from_string("en-US"))
  
  let zh_attributes = Attributes::new()
  Attributes::set_localized(zh_attributes, "error.message", "invalid_credentials", Locale::from_string("zh-CN"))
  
  match Attributes::get(en_attributes, "error.message") {
    Some(StringValue(value)) => assert_eq(value, "Invalid username or password"),
    _ => assert_true(false)
  }
  
  match Attributes::get(zh_attributes, "error.message") {
    Some(StringValue(value)) => assert_eq(value, "用户名或密码无效"),
    _ => assert_true(false)
  }
  
  // Test localized log messages
  let en_logger = LocalizedLogger::new(Locale::from_string("en-US"))
  let zh_logger = LocalizedLogger::new(Locale::from_string("zh-CN"))
  
  let en_log = en_logger.log(Info, "user.login.success", ["user123"])
  let zh_log = zh_logger.log(Info, "user.login.success", ["user123"])
  
  match LogRecord::body(en_log) {
    Some(StringValue(message)) => assert_eq(message, "User user123 logged in successfully"),
    _ => assert_true(false)
  }
  
  match LogRecord::body(zh_log) {
    Some(StringValue(message)) => assert_eq(message, "用户 user123 登录成功"),
    _ => assert_true(false)
  }
  
  // Test localized metric descriptions
  let en_meter = LocalizedMeter::new("http.requests", Locale::from_string("en-US"))
  let zh_meter = LocalizedMeter::new("http.requests", Locale::from_string("zh-CN"))
  
  assert_eq(en_meter.description, "Total number of HTTP requests")
  assert_eq(zh_meter.description, "HTTP请求总数")
  
  // Test localized dashboard labels
  let en_dashboard = LocalizedDashboard::new(Locale::from_string("en-US"))
  let zh_dashboard = LocalizedDashboard::new(Locale::from_string("zh-CN"))
  
  assert_eq(en_dashboard.get_label("response_time"), "Response Time")
  assert_eq(zh_dashboard.get_label("response_time"), "响应时间")
  
  assert_eq(en_dashboard.get_label("error_rate"), "Error Rate")
  assert_eq(zh_dashboard.get_label("error_rate"), "错误率")
}

// Test 10: Comprehensive Internationalization Workflow
test "comprehensive internationalization workflow" {
  // Create a comprehensive internationalization workflow
  let i18n_manager = I18nManager::new()
  
  // Add supported locales
  i18n_manager.add_locale(Locale::from_string("en-US"))
  i18n_manager.add_locale(Locale::from_string("zh-CN"))
  i18n_manager.add_locale(Locale::from_string("fr-FR"))
  i18n_manager.add_locale(Locale::from_string("ja-JP"))
  i18n_manager.add_locale(Locale::from_string("ar-SA"))
  
  // Add translations
  i18n_manager.add_translations("en-US", {
    "dashboard.title": "Telemetry Dashboard",
    "metric.response_time": "Response Time",
    "metric.error_rate": "Error Rate",
    "message.welcome": "Welcome to the Telemetry Dashboard",
    "message.user_count": "Current users: {0}",
    "button.refresh": "Refresh"
  })
  
  i18n_manager.add_translations("zh-CN", {
    "dashboard.title": "遥测仪表板",
    "metric.response_time": "响应时间",
    "metric.error_rate": "错误率",
    "message.welcome": "欢迎使用遥测仪表板",
    "message.user_count": "当前用户数：{0}",
    "button.refresh": "刷新"
  })
  
  i18n_manager.add_translations("fr-FR", {
    "dashboard.title": "Tableau de Bord de Télémétrie",
    "metric.response_time": "Temps de Réponse",
    "metric.error_rate": "Taux d'Erreur",
    "message.welcome": "Bienvenue sur le Tableau de Bord de Télémétrie",
    "message.user_count": "Utilisateurs actuels : {0}",
    "button.refresh": "Actualiser"
  })
  
  i18n_manager.add_translations("ja-JP", {
    "dashboard.title": "テレメトリーダッシュボード",
    "metric.response_time": "応答時間",
    "metric.error_rate": "エラー率",
    "message.welcome": "テレメトリーダッシュボードへようこそ",
    "message.user_count": "現在のユーザー数: {0}",
    "button.refresh": "更新"
  })
  
  i18n_manager.add_translations("ar-SA", {
    "dashboard.title": "لوحة القياس عن بعد",
    "metric.response_time": "وقت الاستجابة",
    "metric.error_rate": "معدل الخطأ",
    "message.welcome": "مرحبًا بك في لوحة القياس عن بعد",
    "message.user_count": "المستخدمون الحاليون: {0}",
    "button.refresh": "تحديث"
  })
  
  // Test English locale
  i18n_manager.set_current_locale(Locale::from_string("en-US"))
  
  let en_dashboard_title = i18n_manager.translate("dashboard.title")
  assert_eq(en_dashboard_title, "Telemetry Dashboard")
  
  let en_welcome_message = i18n_manager.translate("message.welcome")
  assert_eq(en_welcome_message, "Welcome to the Telemetry Dashboard")
  
  let en_user_count = i18n_manager.format("message.user_count", [1234])
  assert_eq(en_user_count, "Current users: 1234")
  
  let en_formatted_number = i18n_manager.format_number(1234567.89)
  assert_eq(en_formatted_number, "1,234,567.89")
  
  let en_formatted_date = i18n_manager.format_date(1589273771000)
  assert_eq(en_formatted_date, "5/12/2020")
  
  // Test Chinese locale
  i18n_manager.set_current_locale(Locale::from_string("zh-CN"))
  
  let zh_dashboard_title = i18n_manager.translate("dashboard.title")
  assert_eq(zh_dashboard_title, "遥测仪表板")
  
  let zh_welcome_message = i18n_manager.translate("message.welcome")
  assert_eq(zh_welcome_message, "欢迎使用遥测仪表板")
  
  let zh_user_count = i18n_manager.format("message.user_count", [1234])
  assert_eq(zh_user_count, "当前用户数：1234")
  
  let zh_formatted_number = i18n_manager.format_number(1234567.89)
  assert_eq(zh_formatted_number, "1,234,567.89")
  
  let zh_formatted_date = i18n_manager.format_date(1589273771000)
  assert_eq(zh_formatted_date, "2020/5/12")
  
  // Test French locale
  i18n_manager.set_current_locale(Locale::from_string("fr-FR"))
  
  let fr_dashboard_title = i18n_manager.translate("dashboard.title")
  assert_eq(fr_dashboard_title, "Tableau de Bord de Télémétrie")
  
  let fr_welcome_message = i18n_manager.translate("message.welcome")
  assert_eq(fr_welcome_message, "Bienvenue sur le Tableau de Bord de Télémétrie")
  
  let fr_user_count = i18n_manager.format("message.user_count", [1234])
  assert_eq(fr_user_count, "Utilisateurs actuels : 1234")
  
  let fr_formatted_number = i18n_manager.format_number(1234567.89)
  assert_eq(fr_formatted_number, "1\u{00A0}234\u{00A0}567,89")
  
  let fr_formatted_date = i18n_manager.format_date(1589273771000)
  assert_eq(fr_formatted_date, "12/05/2020")
  
  // Test Japanese locale
  i18n_manager.set_current_locale(Locale::from_string("ja-JP"))
  
  let ja_dashboard_title = i18n_manager.translate("dashboard.title")
  assert_eq(ja_dashboard_title, "テレメトリーダッシュボード")
  
  let ja_welcome_message = i18n_manager.translate("message.welcome")
  assert_eq(ja_welcome_message, "テレメトリーダッシュボードへようこそ")
  
  let ja_user_count = i18n_manager.format("message.user_count", [1234])
  assert_eq(ja_user_count, "現在のユーザー数: 1234")
  
  let ja_formatted_number = i18n_manager.format_number(1234567.89)
  assert_eq(ja_formatted_number, "1,234,567.89")
  
  let ja_formatted_date = i18n_manager.format_date(1589273771000)
  assert_eq(ja_formatted_date, "2020/05/12")
  
  // Test Arabic locale (RTL)
  i18n_manager.set_current_locale(Locale::from_string("ar-SA"))
  
  let ar_dashboard_title = i18n_manager.translate("dashboard.title")
  assert_eq(ar_dashboard_title, "لوحة القياس عن بعد")
  
  let ar_welcome_message = i18n_manager.translate("message.welcome")
  assert_eq(ar_welcome_message, "مرحبًا بك في لوحة القياس عن بعد")
  
  let ar_user_count = i18n_manager.format("message.user_count", [1234])
  assert_eq(ar_user_count, "المستخدمون الحاليون: 1234")
  
  let ar_formatted_number = i18n_manager.format_number(1234567.89)
  assert_eq(ar_formatted_number, "1,234,567.89")
  
  let ar_formatted_date = i18n_manager.format_date(1589273771000)
  assert_eq(ar_formatted_date, "12/5/2020")
  
  // Verify text direction for Arabic
  assert_true(i18n_manager.is_rtl())
  
  // Test locale switching
  i18n_manager.set_current_locale(Locale::from_string("en-US"))
  assert_false(i18n_manager.is_rtl())
  
  // Test missing translation fallback
  let missing_translation = i18n_manager.translate("nonexistent.key")
  assert_eq(missing_translation, "nonexistent.key")
  
  // Test unsupported locale fallback
  i18n_manager.set_current_locale(Locale::from_string("ko-KR"))
  let korean_dashboard_title = i18n_manager.translate("dashboard.title")
  assert_eq(korean_dashboard_title, "Telemetry Dashboard")  // Falls back to English
}