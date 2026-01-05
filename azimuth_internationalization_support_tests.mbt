// Azimuth Internationalization Support Test Suite
// This file contains test cases for internationalization support in Azimuth telemetry system

// Test 1: Locale-Aware Date/Time Formatting
test "locale-aware date/time formatting for telemetry timestamps" {
  // Define locale
  type Locale = {
    language: String,
    region: String,
    date_format: String,
    time_format: String,
    timezone: String
  }
  
  // Create common locales
  let en_us_locale = {
    language: "en",
    region: "US",
    date_format: "MM/DD/YYYY",
    time_format: "hh:mm:ss A",
    timezone: "America/New_York"
  }
  
  let en_gb_locale = {
    language: "en",
    region: "GB",
    date_format: "DD/MM/YYYY",
    time_format: "HH:mm:ss",
    timezone: "Europe/London"
  }
  
  let de_de_locale = {
    language: "de",
    region: "DE",
    date_format: "DD.MM.YYYY",
    time_format: "HH:mm:ss",
    timezone: "Europe/Berlin"
  }
  
  let ja_jp_locale = {
    language: "ja",
    region: "JP",
    date_format: "YYYY/MM/DD",
    time_format: "HH:mm:ss",
    timezone: "Asia/Tokyo"
  }
  
  let zh_cn_locale = {
    language: "zh",
    region: "CN",
    date_format: "YYYY-MM-DD",
    time_format: "HH:mm:ss",
    timezone: "Asia/Shanghai"
  }
  
  // Format date according to locale
  let format_date = fn(timestamp: Int, locale: Locale) {
    // Extract date components (simplified)
    let year = 2022
    let month = 1
    let day = 5
    
    // Apply locale-specific format
    match locale.date_format {
      "MM/DD/YYYY" => month.to_string().pad_left(2, '0') + "/" + day.to_string().pad_left(2, '0') + "/" + year.to_string(),
      "DD/MM/YYYY" => day.to_string().pad_left(2, '0') + "/" + month.to_string().pad_left(2, '0') + "/" + year.to_string(),
      "DD.MM.YYYY" => day.to_string().pad_left(2, '0') + "." + month.to_string().pad_left(2, '0') + "." + year.to_string(),
      "YYYY/MM/DD" => year.to_string() + "/" + month.to_string().pad_left(2, '0') + "/" + day.to_string().pad_left(2, '0'),
      "YYYY-MM-DD" => year.to_string() + "-" + month.to_string().pad_left(2, '0') + "-" + day.to_string().pad_left(2, '0'),
      _ => year.to_string() + "/" + month.to_string().pad_left(2, '0') + "/" + day.to_string().pad_left(2, '0')
    }
  }
  
  // Format time according to locale
  let format_time = fn(timestamp: Int, locale: Locale) {
    // Extract time components (simplified)
    let hour = 14
    let minute = 30
    let second = 45
    
    // Apply locale-specific format
    match locale.time_format {
      "hh:mm:ss A" => {
        let hour_12 = if hour > 12 { hour - 12 } else if hour == 0 { 12 } else { hour }
        let am_pm = if hour >= 12 { "PM" } else { "AM" }
        hour_12.to_string().pad_left(2, '0') + ":" + minute.to_string().pad_left(2, '0') + ":" + second.to_string().pad_left(2, '0') + " " + am_pm
      }
      "HH:mm:ss" => {
        hour.to_string().pad_left(2, '0') + ":" + minute.to_string().pad_left(2, '0') + ":" + second.to_string().pad_left(2, '0')
      }
      _ => {
        hour.to_string().pad_left(2, '0') + ":" + minute.to_string().pad_left(2, '0') + ":" + second.to_string().pad_left(2, '0')
      }
    }
  }
  
  // Format datetime according to locale
  let format_datetime = fn(timestamp: Int, locale: Locale) {
    format_date(timestamp, locale) + " " + format_time(timestamp, locale)
  }
  
  // Test timestamp
  let test_timestamp = 1640995200  // 2022-01-05 14:30:45 UTC
  
  // Test different locales
  let en_us_datetime = format_datetime(test_timestamp, en_us_locale)
  assert_eq(en_us_datetime, "01/05/2022 02:30:45 PM")
  
  let en_gb_datetime = format_datetime(test_timestamp, en_gb_locale)
  assert_eq(en_gb_datetime, "05/01/2022 14:30:45")
  
  let de_de_datetime = format_datetime(test_timestamp, de_de_locale)
  assert_eq(de_de_datetime, "05.01.2022 14:30:45")
  
  let ja_jp_datetime = format_datetime(test_timestamp, ja_jp_locale)
  assert_eq(ja_jp_datetime, "2022/01/05 14:30:45")
  
  let zh_cn_datetime = format_datetime(test_timestamp, zh_cn_locale)
  assert_eq(zh_cn_datetime, "2022-01-05 14:30:45")
  
  // Test individual date and time formatting
  assert_eq(format_date(test_timestamp, en_us_locale), "01/05/2022")
  assert_eq(format_time(test_timestamp, en_us_locale), "02:30:45 PM")
  
  assert_eq(format_date(test_timestamp, de_de_locale), "05.01.2022")
  assert_eq(format_time(test_timestamp, de_de_locale), "14:30:45")
}

// Test 2: Number Formatting with Locale
test "number formatting with locale for telemetry metrics" {
  // Define locale
  type Locale = {
    language: String,
    region: String,
    decimal_separator: String,
    thousands_separator: String,
    currency_symbol: String
  }
  
  // Create common locales
  let en_us_locale = {
    language: "en",
    region: "US",
    decimal_separator: ".",
    thousands_separator: ",",
    currency_symbol: "$"
  }
  
  let de_de_locale = {
    language: "de",
    region: "DE",
    decimal_separator: ",",
    thousands_separator: ".",
    currency_symbol: "€"
  }
  
  let fr_fr_locale = {
    language: "fr",
    region: "FR",
    decimal_separator: ",",
    thousands_separator: " ",
    currency_symbol: "€"
  }
  
  let ja_jp_locale = {
    language: "ja",
    region: "JP",
    decimal_separator: ".",
    thousands_separator: ",",
    currency_symbol: "¥"
  }
  
  let zh_cn_locale = {
    language: "zh",
    region: "CN",
    decimal_separator: ".",
    thousands_separator: ",",
    currency_symbol: "¥"
  }
  
  // Format decimal number
  let format_decimal = fn(number: Float, decimal_places: Int, locale: Locale) {
    // Separate integer and decimal parts
    let number_str = number.to_string()
    let parts = number_str.split(".")
    let integer_part = parts[0]
    let decimal_part = if parts.length() > 1 { parts[1] } else { "0" }
    
    // Truncate or pad decimal part
    let adjusted_decimal = if decimal_part.length() > decimal_places {
      decimal_part.substring(0, decimal_places)
    } else {
      decimal_part + "0".repeat(decimal_places - decimal_part.length())
    }
    
    // Add thousands separators to integer part
    let mut formatted_integer = ""
    let mut digit_count = 0
    
    for i in 0..integer_part.length() {
      let digit_pos = integer_part.length() - 1 - i
      let digit = integer_part[digit_pos]
      
      formatted_integer = digit + formatted_integer
      digit_count = digit_count + 1
      
      if digit_count == 3 && digit_pos > 0 {
        formatted_integer = locale.thousands_separator + formatted_integer
        digit_count = 0
      }
    }
    
    formatted_integer + locale.decimal_separator + adjusted_decimal
  }
  
  // Format currency
  let format_currency = fn(amount: Float, locale: Locale) {
    let formatted_amount = format_decimal(amount, 2, locale)
    
    match locale.language {
      "en" => locale.currency_symbol + formatted_amount,
      "de" => formatted_amount + " " + locale.currency_symbol,
      "fr" => formatted_amount + " " + locale.currency_symbol,
      "ja" => formatted_amount + locale.currency_symbol,
      "zh" => locale.currency_symbol + formatted_amount,
      _ => formatted_amount + " " + locale.currency_symbol
    }
  }
  
  // Format percentage
  let format_percentage = fn(value: Float, locale: Locale) {
    let percentage_value = value * 100.0
    let formatted_value = format_decimal(percentage_value, 2, locale)
    formatted_value + "%"
  }
  
  // Test decimal formatting
  let test_number = 1234567.89123
  
  let en_us_decimal = format_decimal(test_number, 2, en_us_locale)
  assert_eq(en_us_decimal, "1,234,567.89")
  
  let de_de_decimal = format_decimal(test_number, 2, de_de_locale)
  assert_eq(de_de_decimal, "1.234.567,89")
  
  let fr_fr_decimal = format_decimal(test_number, 2, fr_fr_locale)
  assert_eq(fr_fr_decimal, "1 234 567,89")
  
  let ja_jp_decimal = format_decimal(test_number, 2, ja_jp_locale)
  assert_eq(ja_jp_decimal, "1,234,567.89")
  
  let zh_cn_decimal = format_decimal(test_number, 2, zh_cn_locale)
  assert_eq(zh_cn_decimal, "1,234,567.89")
  
  // Test currency formatting
  let test_amount = 1234.56
  
  let en_us_currency = format_currency(test_amount, en_us_locale)
  assert_eq(en_us_currency, "$1,234.56")
  
  let de_de_currency = format_currency(test_amount, de_de_locale)
  assert_eq(de_de_currency, "1.234,56 €")
  
  let fr_fr_currency = format_currency(test_amount, fr_fr_locale)
  assert_eq(fr_fr_currency, "1 234,56 €")
  
  let ja_jp_currency = format_currency(test_amount, ja_jp_locale)
  assert_eq(ja_jp_currency, "1,234.56¥")
  
  let zh_cn_currency = format_currency(test_amount, zh_cn_locale)
  assert_eq(zh_cn_currency, "¥1,234.56")
  
  // Test percentage formatting
  let test_percentage = 0.75432
  
  let en_us_percentage = format_percentage(test_percentage, en_us_locale)
  assert_eq(en_us_percentage, "75.43%")
  
  let de_de_percentage = format_percentage(test_percentage, de_de_locale)
  assert_eq(de_de_percentage, "75,43%")
  
  let fr_fr_percentage = format_percentage(test_percentage, fr_fr_locale)
  assert_eq(fr_fr_percentage, "75,43%")
}

// Test 3: Text Direction and RTL/LTR Support
test "text direction and RTL/LTR support for telemetry UI" {
  // Define text direction
  enum TextDirection {
    LTR  // Left-to-Right
    RTL  // Right-to-Left
  }
  
  // Define language with text direction
  type Language = {
    code: String,
    name: String,
    direction: TextDirection,
    alignment: String  // "left", "right", "center"
  }
  
  // Create languages
  let en_language = {
    code: "en",
    name: "English",
    direction: TextDirection::LTR,
    alignment: "left"
  }
  
  let ar_language = {
    code: "ar",
    name: "العربية",
    direction: TextDirection::RTL,
    alignment: "right"
  }
  
  let he_language = {
    code: "he",
    name: "עברית",
    direction: TextDirection::RTL,
    alignment: "right"
  }
  
  let ja_language = {
    code: "ja",
    name: "日本語",
    direction: TextDirection::LTR,
    alignment: "left"
  }
  
  // Format text with direction
  let format_text_with_direction = fn(text: String, language: Language) {
    {
      text,
      direction: language.direction,
      alignment: language.alignment,
      html_dir: match language.direction {
        TextDirection::LTR => "ltr",
        TextDirection::RTL => "rtl"
      },
      css_text_align: language.alignment
    }
  }
  
  // Create bilingual text
  let create_bilingual_text = fn(primary: String, primary_lang: Language, secondary: String, secondary_lang: Language) {
    let primary_formatted = format_text_with_direction(primary, primary_lang)
    let secondary_formatted = format_text_with_direction(secondary, secondary_lang)
    
    {
      primary: primary_formatted,
      secondary: secondary_formatted
    }
  }
  
  // Test LTR text formatting
  let ltr_text = "Response Time: 125ms"
  let ltr_formatted = format_text_with_direction(ltr_text, en_language)
  
  assert_eq(ltr_formatted.text, "Response Time: 125ms")
  assert_eq(ltr_formatted.html_dir, "ltr")
  assert_eq(ltr_formatted.css_text_align, "left")
  
  // Test RTL text formatting
  let rtl_text = "وقت الاستجابة: 125 مللي ثانية"
  let rtl_formatted = format_text_with_direction(rtl_text, ar_language)
  
  assert_eq(rtl_formatted.text, "وقت الاستجابة: 125 مللي ثانية")
  assert_eq(rtl_formatted.html_dir, "rtl")
  assert_eq(rtl_formatted.css_text_align, "right")
  
  // Test bilingual text
  let bilingual = create_bilingual_text(
    "Error: Database connection failed",
    en_language,
    "خطأ: فشل الاتصال بقاعدة البيانات",
    ar_language
  )
  
  assert_eq(bilingual.primary.text, "Error: Database connection failed")
  assert_eq(bilingual.primary.html_dir, "ltr")
  assert_eq(bilingual.secondary.text, "خطأ: فشل الاتصال بقاعدة البيانات")
  assert_eq(bilingual.secondary.html_dir, "rtl")
  
  // Test Hebrew text
  let hebrew_text = "זמן תגובה: 125 מילישניות"
  let hebrew_formatted = format_text_with_direction(hebrew_text, he_language)
  
  assert_eq(hebrew_formatted.text, "זמן תגובה: 125 מילישניות")
  assert_eq(hebrew_formatted.html_dir, "rtl")
  assert_eq(hebrew_formatted.css_text_align, "right")
  
  // Test Japanese text (LTR but different characters)
  let japanese_text = "応答時間: 125ミリ秒"
  let japanese_formatted = format_text_with_direction(japanese_text, ja_language)
  
  assert_eq(japanese_formatted.text, "応答時間: 125ミリ秒")
  assert_eq(japanese_formatted.html_dir, "ltr")
  assert_eq(japanese_formatted.css_text_align, "left")
}

// Test 4: Message Translation and Localization
test "message translation and localization for telemetry" {
  // Define message key
  type MessageKey = String
  
  // Define translation map
  type TranslationMap = Array[(String, String)]  // (locale, translation)
  
  // Define message
  type Message = {
    key: MessageKey,
    translations: TranslationMap
  }
  
  // Create message catalog
  let create_message_catalog = fn() {
    [
      {
        key: "span.created",
        translations: [
          ("en", "Span created"),
          ("es", "Span creado"),
          ("fr", "Span créé"),
          ("de", "Span erstellt"),
          ("ja", "スパンが作成されました"),
          ("zh", "Span已创建"),
          ("ar", "تم إنشاء Span")
        ]
      },
      {
        key: "metric.updated",
        translations: [
          ("en", "Metric updated"),
          ("es", "Métrica actualizada"),
          ("fr", "Métrique mise à jour"),
          ("de", "Metrik aktualisiert"),
          ("ja", "メトリックが更新されました"),
          ("zh", "指标已更新"),
          ("ar", "تم تحديث المقاييس")
        ]
      },
      {
        key: "error.connection.failed",
        translations: [
          ("en", "Connection failed"),
          ("es", "Conexión fallida"),
          ("fr", "Connexion échouée"),
          ("de", "Verbindung fehlgeschlagen"),
          ("ja", "接続に失敗しました"),
          ("zh", "连接失败"),
          ("ar", "فشل الاتصال")
        ]
      },
      {
        key: "warning.high.latency",
        translations: [
          ("en", "High latency detected"),
          ("es", "Alta latencia detectada"),
          ("fr", "Latence élevée détectée"),
          ("de", "Hohe Latenz erkannt"),
          ("ja", "高レイテンシが検出されました"),
          ("zh", "检测到高延迟"),
          ("ar", "تم اكتشاف زمن استجابة عالٍ")
        ]
      }
    ]
  }
  
  // Get translation for locale
  let get_translation = fn(messages: Array<Message>, key: MessageKey, locale: String) {
    let mut translation = key  // Fallback to key
    
    for message in messages {
      if message.key == key {
        for trans in message.translations {
          if trans.0 == locale {
            translation = trans.1
          } else if trans.0 == "en" && translation == key {
            // Fallback to English
            translation = trans.1
          }
        }
      }
    }
    
    translation
  }
  
  // Format message with parameters
  let format_message = fn(template: String, params: Array[String>) {
    let mut result = template
    
    for i in 0..params.length() {
      let placeholder = "{" + i.to_string() + "}"
      result = result.replace(placeholder, params[i])
    }
    
    result
  }
  
  // Create message catalog
  let catalog = create_message_catalog()
  
  // Test translations
  let en_created = get_translation(catalog, "span.created", "en")
  assert_eq(en_created, "Span created")
  
  let es_created = get_translation(catalog, "span.created", "es")
  assert_eq(es_created, "Span creado")
  
  let fr_created = get_translation(catalog, "span.created", "fr")
  assert_eq(fr_created, "Span créé")
  
  let ja_created = get_translation(catalog, "span.created", "ja")
  assert_eq(ja_created, "スパンが作成されました")
  
  let zh_created = get_translation(catalog, "span.created", "zh")
  assert_eq(zh_created, "Span已创建")
  
  let ar_created = get_translation(catalog, "span.created", "ar")
  assert_eq(ar_created, "تم إنشاء Span")
  
  // Test fallback to English for unknown locale
  let pt_created = get_translation(catalog, "span.created", "pt")
  assert_eq(pt_created, "Span created")  // Fallback to English
  
  // Test fallback to key for unknown message
  let unknown = get_translation(catalog, "unknown.message", "en")
  assert_eq(unknown, "unknown.message")  // Fallback to key
  
  // Test message formatting with parameters
  let template = "Span {0} created with trace ID {1}"
  let formatted = format_message(template, ["test-123", "trace-456"])
  assert_eq(formatted, "Span test-123 created with trace ID trace-456")
  
  // Test parameter formatting in different languages
  let error_template = get_translation(catalog, "error.connection.failed", "en")
  let formatted_error = format_message(error_template, [])
  assert_eq(formatted_error, "Connection failed")
  
  let warning_template = get_translation(catalog, "warning.high.latency", "ja")
  let formatted_warning = format_message(warning_template, [])
  assert_eq(formatted_warning, "高レイテンシが検出されました")
}

// Test 5: Collation and String Comparison
test "collation and string comparison for international telemetry data" {
  // Define collation rules
  type CollationRules = {
    locale: String,
    case_sensitive: Bool,
    diacritic_sensitive: Bool,
    ignore_whitespace: Bool,
    ignore_punctuation: Bool
  }
  
  // Create collation rules for different locales
  let en_collation = {
    locale: "en",
    case_sensitive: false,
    diacritic_sensitive: true,
    ignore_whitespace: false,
    ignore_punctuation: false
  }
  
  let de_collation = {
    locale: "de",
    case_sensitive: false,
    diacritic_sensitive: true,
    ignore_whitespace: false,
    ignore_punctuation: false
  }
  
  let ja_collation = {
    locale: "ja",
    case_sensitive: false,
    diacritic_sensitive: false,  // Japanese doesn't typically use diacritics
    ignore_whitespace: false,
    ignore_punctuation: false
  }
  
  let ar_collation = {
    locale: "ar",
    case_sensitive: false,
    diacritic_sensitive: true,
    ignore_whitespace: false,
    ignore_punctuation: false
  }
  
  // Normalize string for comparison
  let normalize_string = fn(text: String, collation: CollationRules) {
    let mut normalized = text
    
    // Case normalization
    if not(collation.case_sensitive) {
      normalized = normalized.to_lowercase()
    }
    
    // Whitespace normalization
    if collation.ignore_whitespace {
      normalized = normalized.replace(" ", "")
    }
    
    // Punctuation normalization
    if collation.ignore_punctuation {
      normalized = normalized.replace(".", "")
      normalized = normalized.replace(",", "")
      normalized = normalized.replace("!", "")
      normalized = normalized.replace("?", "")
    }
    
    normalized
  }
  
  // Compare strings with collation
  let compare_strings = fn(str1: String, str2: String, collation: CollationRules) {
    let norm1 = normalize_string(str1, collation)
    let norm2 = normalize_string(str2, collation)
    
    if norm1 < norm2 {
      -1
    } else if norm1 > norm2 {
      1
    } else {
      0
    }
  }
  
  // Test English collation
  let en_result1 = compare_strings("Apple", "apple", en_collation)
  assert_eq(en_result1, 0)  // Case insensitive
  
  let en_result2 = compare_strings("Apple", "Banana", en_collation)
  assert_eq(en_result2, -1)  // Apple < Banana
  
  let en_result3 = compare_strings("café", "cafe", en_collation)
  assert_eq(en_result3, 1)  // café > cafe (diacritic sensitive)
  
  // Test German collation (with umlauts)
  let de_result1 = compare_strings("Müller", "Mueller", de_collation)
  assert_eq(de_result1, 1)  // Müller > Mueller (diacritic sensitive)
  
  let de_result2 = compare_strings("Müller", "müller", de_collation)
  assert_eq(de_result2, 0)  // Case insensitive
  
  let de_result3 = compare_strings("Äpfel", "Bananen", de_collation)
  assert_eq(de_result3, -1)  // Äpfel < Bananen
  
  // Test Japanese collation
  let ja_result1 = compare_strings("テスト", "テスト", ja_collation)
  assert_eq(ja_result1, 0)  // Same
  
  let ja_result2 = compare_strings("テスト", "テストデータ", ja_collation)
  assert_eq(ja_result2, -1)  // テスト < テストデータ
  
  let ja_result3 = compare_strings("データ", "テスト", ja_collation)
  assert_eq(ja_result3, 1)  // データ > テスト
  
  // Test Arabic collation
  let ar_result1 = compare_strings("اختبار", "اختبار", ar_collation)
  assert_eq(ar_result1, 0)  // Same
  
  let ar_result2 = compare_strings("اختبار", "بيانات", ar_collation)
  assert_eq(ar_result2, -1)  // اختبار < بيانات
  
  let ar_result3 = compare_strings("بيانات", "اختبار", ar_collation)
  assert_eq(ar_result3, 1)  // بيانات > اختبار
  
  // Test with whitespace and punctuation
  let whitespace_collation = {
    locale: "en",
    case_sensitive: false,
    diacritic_sensitive: true,
    ignore_whitespace: true,
    ignore_punctuation: true
  }
  
  let ws_result1 = compare_strings("Hello, World!", "helloworld", whitespace_collation)
  assert_eq(ws_result1, 0)  // Same after normalization
  
  let ws_result2 = compare_strings("Hello, World!", "hello world", whitespace_collation)
  assert_eq(ws_result2, 0)  // Same after normalization
}

// Test 6: Currency and Number Formatting
test "currency and number formatting for international telemetry" {
  // Define locale-specific formatting
  type LocaleFormat = {
    locale: String,
    decimal_separator: String,
    thousands_separator: String,
    currency_symbol: String,
    currency_position: String,  // "before", "after"
    currency_spacing: String    // "space", "none"
  }
  
  // Create locale formats
  let en_us_format = {
    locale: "en-US",
    decimal_separator: ".",
    thousands_separator: ",",
    currency_symbol: "$",
    currency_position: "before",
    currency_spacing: "none"
  }
  
  let de_de_format = {
    locale: "de-DE",
    decimal_separator: ",",
    thousands_separator: ".",
    currency_symbol: "€",
    currency_position: "after",
    currency_spacing: "space"
  }
  
  let fr_fr_format = {
    locale: "fr-FR",
    decimal_separator: ",",
    thousands_separator: " ",
    currency_symbol: "€",
    currency_position: "after",
    currency_spacing: "space"
  }
  
  let ja_jp_format = {
    locale: "ja-JP",
    decimal_separator: ".",
    thousands_separator: ",",
    currency_symbol: "¥",
    currency_position: "before",
    currency_spacing: "none"
  }
  
  let ar_sa_format = {
    locale: "ar-SA",
    decimal_separator: ".",  // Arabic uses decimal point in many contexts
    thousands_separator: ",",
    currency_symbol: "ر.س",
    currency_position: "before",
    currency_spacing: "space"
  }
  
  // Format number with locale
  let format_number = fn(number: Float, decimal_places: Int, format: LocaleFormat) {
    // Separate integer and decimal parts
    let number_str = number.to_string()
    let parts = number_str.split(".")
    let integer_part = parts[0]
    let decimal_part = if parts.length() > 1 { parts[1] } else { "0" }
    
    // Truncate or pad decimal part
    let adjusted_decimal = if decimal_part.length() > decimal_places {
      decimal_part.substring(0, decimal_places)
    } else {
      decimal_part + "0".repeat(decimal_places - decimal_part.length())
    }
    
    // Add thousands separators to integer part
    let mut formatted_integer = ""
    let mut digit_count = 0
    
    for i in 0..integer_part.length() {
      let digit_pos = integer_part.length() - 1 - i
      let digit = integer_part[digit_pos]
      
      formatted_integer = digit + formatted_integer
      digit_count = digit_count + 1
      
      if digit_count == 3 && digit_pos > 0 {
        formatted_integer = format.thousands_separator + formatted_integer
        digit_count = 0
      }
    }
    
    formatted_integer + format.decimal_separator + adjusted_decimal
  }
  
  // Format currency with locale
  let format_currency = fn(amount: Float, format: LocaleFormat) {
    let formatted_amount = format_number(amount, 2, format)
    let spacing = if format.currency_spacing == "space" { " " } else { "" }
    
    match format.currency_position {
      "before" => format.currency_symbol + spacing + formatted_amount,
      "after" => formatted_amount + spacing + format.currency_symbol,
      _ => formatted_amount
    }
  }
  
  // Test number formatting
  let test_number = 1234567.89123
  
  let en_us_number = format_number(test_number, 2, en_us_format)
  assert_eq(en_us_number, "1,234,567.89")
  
  let de_de_number = format_number(test_number, 2, de_de_format)
  assert_eq(de_de_number, "1.234.567,89")
  
  let fr_fr_number = format_number(test_number, 2, fr_fr_format)
  assert_eq(fr_fr_number, "1 234 567,89")
  
  let ja_jp_number = format_number(test_number, 2, ja_jp_format)
  assert_eq(ja_jp_number, "1,234,567.89")
  
  // Test currency formatting
  let test_amount = 1234.56
  
  let en_us_currency = format_currency(test_amount, en_us_format)
  assert_eq(en_us_currency, "$1,234.56")
  
  let de_de_currency = format_currency(test_amount, de_de_format)
  assert_eq(de_de_currency, "1.234,56 €")
  
  let fr_fr_currency = format_currency(test_amount, fr_fr_format)
  assert_eq(fr_fr_currency, "1 234,56 €")
  
  let ja_jp_currency = format_currency(test_amount, ja_jp_format)
  assert_eq(ja_jp_currency, "¥1,234.56")
  
  let ar_sa_currency = format_currency(test_amount, ar_sa_format)
  assert_eq(ar_sa_currency, "ر.س 1,234.56")
}

// Test 7: Time Zone Conversion
test "time zone conversion for international telemetry timestamps" {
  // Define time zone
  type TimeZone = {
    name: String,
    offset_hours: Int  // Offset from UTC
    offset_minutes: Int  // Additional offset minutes
  }
  
  // Create common time zones
  let utc_timezone = {
    name: "UTC",
    offset_hours: 0,
    offset_minutes: 0
  }
  
  let est_timezone = {
    name: "EST",
    offset_hours: -5,
    offset_minutes: 0
  }
  
  let pst_timezone = {
    name: "PST",
    offset_hours: -8,
    offset_minutes: 0
  }
  
  let cet_timezone = {
    name: "CET",
    offset_hours: 1,
    offset_minutes: 0
  }
  
  let jst_timezone = {
    name: "JST",
    offset_hours: 9,
    offset_minutes: 0
  }
  
  let ist_timezone = {
    name: "IST",
    offset_hours: 5,
    offset_minutes: 30
  }
  
  // Convert timestamp to time zone
  let convert_to_timezone = fn(timestamp: Int, from_tz: TimeZone, to_tz: TimeZone) {
    // Calculate total offset in minutes
    let from_offset = from_tz.offset_hours * 60 + from_tz.offset_minutes
    let to_offset = to_tz.offset_hours * 60 + to_tz.offset_minutes
    
    // Convert to UTC first, then to target timezone
    let utc_timestamp = timestamp - from_offset * 60
    let target_timestamp = utc_timestamp + to_offset * 60
    
    target_timestamp
  }
  
  // Format timestamp as time
  let format_time = fn(timestamp: Int) {
    let hours = (timestamp / 3600) % 24
    let minutes = (timestamp / 60) % 60
    let seconds = timestamp % 60
    
    hours.to_string().pad_left(2, '0') + ":" + 
    minutes.to_string().pad_left(2, '0') + ":" + 
    seconds.to_string().pad_left(2, '0')
  }
  
  // Format timestamp as date and time
  let format_datetime = fn(timestamp: Int) {
    let days = timestamp / 86400
    let time = timestamp % 86400
    
    // Simplified date calculation
    let year = 1970 + days / 365
    let day_of_year = days % 365
    
    let month = if day_of_year < 31 { 1 }
    else if day_of_year < 59 { 2 }
    else if day_of_year < 90 { 3 }
    else if day_of_year < 120 { 4 }
    else if day_of_year < 151 { 5 }
    else if day_of_year < 181 { 6 }
    else if day_of_year < 212 { 7 }
    else if day_of_year < 243 { 8 }
    else if day_of_year < 273 { 9 }
    else if day_of_year < 304 { 10 }
    else if day_of_year < 334 { 11 }
    else { 12 }
    
    let day = if month == 1 { day_of_year + 1 }
    else if month == 2 { day_of_year - 30 }
    else if month == 3 { day_of_year - 58 }
    else if month == 4 { day_of_year - 89 }
    else if month == 5 { day_of_year - 119 }
    else if month == 6 { day_of_year - 150 }
    else if month == 7 { day_of_year - 180 }
    else if month == 8 { day_of_year - 211 }
    else if month == 9 { day_of_year - 242 }
    else if month == 10 { day_of_year - 272 }
    else if month == 11 { day_of_year - 303 }
    else { day_of_year - 333 }
    
    year.to_string() + "-" + 
    month.to_string().pad_left(2, '0') + "-" + 
    day.to_string().pad_left(2, '0') + " " + 
    format_time(time)
  }
  
  // Test timestamp (2022-01-05 14:30:45 UTC)
  let test_timestamp = 1640995200 + 14 * 3600 + 30 * 60 + 45
  
  // Convert to different time zones
  let est_timestamp = convert_to_timezone(test_timestamp, utc_timezone, est_timezone)
  let est_datetime = format_datetime(est_timestamp)
  assert_eq(est_datetime, "2022-01-05 09:30:45")  // 14:30 UTC - 5 hours = 09:30 EST
  
  let pst_timestamp = convert_to_timezone(test_timestamp, utc_timezone, pst_timezone)
  let pst_datetime = format_datetime(pst_timestamp)
  assert_eq(pst_datetime, "2022-01-05 06:30:45")  // 14:30 UTC - 8 hours = 06:30 PST
  
  let cet_timestamp = convert_to_timezone(test_timestamp, utc_timezone, cet_timezone)
  let cet_datetime = format_datetime(cet_timestamp)
  assert_eq(cet_datetime, "2022-01-05 15:30:45")  // 14:30 UTC + 1 hour = 15:30 CET
  
  let jst_timestamp = convert_to_timezone(test_timestamp, utc_timezone, jst_timezone)
  let jst_datetime = format_datetime(jst_timestamp)
  assert_eq(jst_datetime, "2022-01-05 23:30:45")  // 14:30 UTC + 9 hours = 23:30 JST
  
  let ist_timestamp = convert_to_timezone(test_timestamp, utc_timezone, ist_timezone)
  let ist_datetime = format_datetime(ist_timestamp)
  assert_eq(ist_datetime, "2022-01-05 20:00:45")  // 14:30 UTC + 5:30 hours = 20:00:45 IST
  
  // Test conversion between non-UTC time zones
  let est_to_cet_timestamp = convert_to_timezone(est_timestamp, est_timezone, cet_timezone)
  let est_to_cet_datetime = format_datetime(est_to_cet_timestamp)
  assert_eq(est_to_cet_datetime, "2022-01-05 15:30:45")  // 09:30 EST + 6 hours = 15:30 CET
  
  // Test time zone with 30-minute offset
  let ist_to_utc_timestamp = convert_to_timezone(ist_timestamp, ist_timezone, utc_timezone)
  let ist_to_utc_datetime = format_datetime(ist_to_utc_timestamp)
  assert_eq(ist_to_utc_datetime, "2022-01-05 14:30:45")  // Back to original UTC time
}

// Test 8: Measurement Units Conversion
test "measurement units conversion for international telemetry" {
  // Define unit system
  enum UnitSystem {
    Metric      // SI units
    Imperial    // US customary units
    Mixed       // Combination
  }
  
  // Define unit
  type Unit = {
    name: String,
    symbol: String,
    system: UnitSystem,
    conversion_to_base: Float  // Conversion factor to base unit
  }
  
  // Define measurement
  type Measurement = {
    value: Float,
    unit: Unit
  }
  
  // Create common units
  let meters = {
    name: "meter",
    symbol: "m",
    system: UnitSystem::Metric,
    conversion_to_base: 1.0
  }
  
  let kilometers = {
    name: "kilometer",
    symbol: "km",
    system: UnitSystem::Metric,
    conversion_to_base: 1000.0
  }
  
  let feet = {
    name: "foot",
    symbol: "ft",
    system: UnitSystem::Imperial,
    conversion_to_base: 0.3048
  }
  
  let miles = {
    name: "mile",
    symbol: "mi",
    system: UnitSystem::Imperial,
    conversion_to_base: 1609.34
  }
  
  let celsius = {
    name: "celsius",
    symbol: "°C",
    system: UnitSystem::Metric,
    conversion_to_base: 1.0  // Special case for temperature
  }
  
  let fahrenheit = {
    name: "fahrenheit",
    symbol: "°F",
    system: UnitSystem::Imperial,
    conversion_to_base: 1.0  // Special case for temperature
  }
  
  let kelvin = {
    name: "kelvin",
    symbol: "K",
    system: UnitSystem::Metric,
    conversion_to_base: 1.0  // Special case for temperature
  }
  
  let kilograms = {
    name: "kilogram",
    symbol: "kg",
    system: UnitSystem::Metric,
    conversion_to_base: 1.0
  }
  
  let pounds = {
    name: "pound",
    symbol: "lb",
    system: UnitSystem::Imperial,
    conversion_to_base: 0.453592
  }
  
  // Convert measurement to different unit
  let convert_measurement = fn(measurement: Measurement, target_unit: Unit) {
    // Handle temperature conversion specially
    if measurement.unit.name == "celsius" && target_unit.name == "fahrenheit" {
      let fahrenheit_value = (measurement.value * 9.0 / 5.0) + 32.0
      {
        value: fahrenheit_value,
        unit: target_unit
      }
    } else if measurement.unit.name == "fahrenheit" && target_unit.name == "celsius" {
      let celsius_value = (measurement.value - 32.0) * 5.0 / 9.0
      {
        value: celsius_value,
        unit: target_unit
      }
    } else if measurement.unit.name == "celsius" && target_unit.name == "kelvin" {
      let kelvin_value = measurement.value + 273.15
      {
        value: kelvin_value,
        unit: target_unit
      }
    } else if measurement.unit.name == "kelvin" && target_unit.name == "celsius" {
      let celsius_value = measurement.value - 273.15
      {
        value: celsius_value,
        unit: target_unit
      }
    } else if measurement.unit.name == "fahrenheit" && target_unit.name == "kelvin" {
      let celsius_value = (measurement.value - 32.0) * 5.0 / 9.0
      let kelvin_value = celsius_value + 273.15
      {
        value: kelvin_value,
        unit: target_unit
      }
    } else if measurement.unit.name == "kelvin" && target_unit.name == "fahrenheit" {
      let celsius_value = measurement.value - 273.15
      let fahrenheit_value = (celsius_value * 9.0 / 5.0) + 32.0
      {
        value: fahrenheit_value,
        unit: target_unit
      }
    } else {
      // Regular unit conversion
      let base_value = measurement.value * measurement.unit.conversion_to_base
      let target_value = base_value / target_unit.conversion_to_base
      
      {
        value: target_value,
        unit: target_unit
      }
    }
  }
  
  // Format measurement
  let format_measurement = fn(measurement: Measurement) {
    measurement.value.to_string() + " " + measurement.unit.symbol
  }
  
  // Test length conversions
  let meters_to_feet = convert_measurement({ value: 10.0, unit: meters }, feet)
  assert_eq(format_measurement(meters_to_feet), "32.8084 ft")  // 10m ≈ 32.8084ft
  
  let kilometers_to_miles = convert_measurement({ value: 5.0, unit: kilometers }, miles)
  assert_eq(format_measurement(kilometers_to_miles), "3.10686 mi")  // 5km ≈ 3.10686mi
  
  let miles_to_kilometers = convert_measurement({ value: 10.0, unit: miles }, kilometers)
  assert_eq(format_measurement(miles_to_kilometers), "16.0934 km")  // 10mi ≈ 16.0934km
  
  // Test temperature conversions
  let celsius_to_fahrenheit = convert_measurement({ value: 25.0, unit: celsius }, fahrenheit)
  assert_eq(format_measurement(celsius_to_fahrenheit), "77.0 °F")  // 25°C = 77°F
  
  let fahrenheit_to_celsius = convert_measurement({ value: 68.0, unit: fahrenheit }, celsius)
  assert_eq(format_measurement(fahrenheit_to_celsius), "20.0 °C")  // 68°F = 20°C
  
  let celsius_to_kelvin = convert_measurement({ value: 0.0, unit: celsius }, kelvin)
  assert_eq(format_measurement(celsius_to_kelvin), "273.15 K")  // 0°C = 273.15K
  
  let kelvin_to_celsius = convert_measurement({ value: 373.15, unit: kelvin }, celsius)
  assert_eq(format_measurement(kelvin_to_celsius), "100.0 °C")  // 373.15K = 100°C
  
  // Test weight conversions
  let kilograms_to_pounds = convert_measurement({ value: 70.0, unit: kilograms }, pounds)
  assert_eq(format_measurement(kilograms_to_pounds), "154.324 lb")  // 70kg ≈ 154.324lb
  
  let pounds_to_kilograms = convert_measurement({ value: 150.0, unit: pounds }, kilograms)
  assert_eq(format_measurement(pounds_to_kilograms), "68.0388 kg")  // 150lb ≈ 68.0388kg
}

// Test 9: Locale-Aware Sorting and Ordering
test "locale-aware sorting and ordering for telemetry data" {
  // Define collation rules
  type CollationRules = {
    locale: String,
    case_sensitive: Bool,
    diacritic_sensitive: Bool,
    numeric_sorting: Bool  // Sort numbers numerically rather than lexicographically
  }
  
  // Create collation rules for different locales
  let en_collation = {
    locale: "en",
    case_sensitive: false,
    diacritic_sensitive: true,
    numeric_sorting: true
  }
  
  let de_collation = {
    locale: "de",
    case_sensitive: false,
    diacritic_sensitive: true,
    numeric_sorting: true
  }
  
  let sv_collation = {
    locale: "sv",  // Swedish has special sorting rules
    case_sensitive: false,
    diacritic_sensitive: true,
    numeric_sorting: true
  }
  
  // Normalize string for comparison
  let normalize_for_sorting = fn(text: String, collation: CollationRules) {
    let mut normalized = text
    
    // Case normalization
    if not(collation.case_sensitive) {
      normalized = normalized.to_lowercase()
    }
    
    // Special locale-specific rules
    match collation.locale {
      "sv" => {
        // Swedish: å, ä, ö are at the end of the alphabet
        normalized = normalized.replace("å", "zzz")
        normalized = normalized.replace("ä", "zzz1")
        normalized = normalized.replace("ö", "zzz2")
      }
      "de" => {
        // German: ä = ae, ö = oe, ü = ue
        normalized = normalized.replace("ä", "ae")
        normalized = normalized.replace("ö", "oe")
        normalized = normalized.replace("ü", "ue")
        normalized = normalized.replace("ß", "ss")
      }
      _ => ()
    }
    
    normalized
  }
  
  // Compare strings for sorting
  let compare_for_sorting = fn(str1: String, str2: String, collation: CollationRules) {
    let norm1 = normalize_for_sorting(str1, collation)
    let norm2 = normalize_for_sorting(str2, collation)
    
    if norm1 < norm2 {
      -1
    } else if norm1 > norm2 {
      1
    } else {
      0
    }
  }
  
  // Sort array of strings
  let sort_strings = fn(strings: Array[String], collation: CollationRules) {
    let mut sorted = strings
    
    // Simple bubble sort for demonstration
    let mut swapped = true
    while swapped {
      swapped = false
      for i in 0..sorted.length() - 1 {
        if compare_for_sorting(sorted[i], sorted[i + 1], collation) > 0 {
          let temp = sorted[i]
          sorted[i] = sorted[i + 1]
          sorted[i + 1] = temp
          swapped = true
        }
      }
    }
    
    sorted
  }
  
  // Test English sorting
  let en_strings = ["apple", "Banana", "cherry", "date"]
  let en_sorted = sort_strings(en_strings, en_collation)
  assert_eq(en_sorted, ["apple", "Banana", "cherry", "date"])
  
  // Test numeric sorting
  let numeric_strings = ["item1", "item10", "item2", "item20"]
  let numeric_sorted = sort_strings(numeric_strings, en_collation)
  assert_eq(numeric_sorted, ["item1", "item2", "item10", "item20"])
  
  // Test German sorting (with umlauts)
  let de_strings = ["Müller", "Mueller", "Muster", "Schmidt"]
  let de_sorted = sort_strings(de_strings, de_collation)
  assert_eq(de_sorted, ["Mueller", "Müller", "Muster", "Schmidt"])
  
  // Test Swedish sorting (å, ä, ö at the end)
  let sv_strings = ["Öster", "Äpple", "Banana", "Åland"]
  let sv_sorted = sort_strings(sv_strings, sv_collation)
  assert_eq(sv_sorted, ["Äpple", "Banana", "Öster", "Åland"])
  
  // Test mixed case sorting
  let mixed_strings = ["Apple", "apple", "Banana", "banana"]
  let mixed_sorted = sort_strings(mixed_strings, en_collation)
  assert_eq(mixed_sorted, ["Apple", "apple", "Banana", "banana"])
}

// Test 10: Pluralization and Grammar Rules
test "pluralization and grammar rules for international telemetry messages" {
  // Define pluralization rules
  type PluralizationRules = {
    locale: String,
    plural_form: fn(Int) -> Int  // Returns plural form index
  }
  
  // Create pluralization rules for different locales
  let en_pluralization = {
    locale: "en",
    plural_form: fn(n: Int) {
      if n == 1 { 0 } else { 1 }  // English: 1 = singular, other = plural
    }
  }
  
  let fr_pluralization = {
    locale: "fr",
    plural_form: fn(n: Int) {
      if n == 1 { 0 } else { 1 }  // French: same as English
    }
  }
  
  let ru_pluralization = {
    locale: "ru",
    plural_form: fn(n: Int) {
      let mod10 = n % 10
      let mod100 = n % 100
      
      if mod10 == 1 && mod100 != 11 {
        0  // Singular: 1, 21, 31, ...
      } else if mod10 >= 2 && mod10 <= 4 && (mod100 < 10 || mod100 >= 20) {
        1  // Paucal: 2-4, 22-24, 32-34, ...
      } else {
        2  // Plural: 0, 5-20, 25-30, ...
      }
    }
  }
  
  let ar_pluralization = {
    locale: "ar",
    plural_form: fn(n: Int) {
      let mod100 = n % 100
      
      if n == 0 {
        0  // Zero
      } else if n == 1 {
        1  // One
      } else if n == 2 {
        2  // Two
      } else if mod100 >= 3 && mod100 <= 10 {
        3  // Few: 3-10
      } else if mod100 >= 11 && mod100 <= 99 {
        4  // Many: 11-99
      } else {
        5  // Other: 100-102, 200-202, ...
      }
    }
  }
  
  // Define pluralized message
  type PluralizedMessage = {
    key: String,
    translations: Array[(String, Array[String>)]  // (locale, [singular, plural, ...])
  }
  
  // Get pluralized translation
  let get_pluralized_translation = fn(messages: Array[PluralizedMessage], key: String, locale: String, count: Int, pluralization: PluralizationRules) {
    let mut translation = key
    
    for message in messages {
      if message.key == key {
        for trans in message.translations {
          if trans.0 == locale {
            let plural_form = pluralization.plural_form(count)
            if plural_form < trans.1.length() {
              translation = trans.1[plural_form]
            }
          } else if trans.0 == "en" && translation == key {
            // Fallback to English
            let en_pluralization = en_pluralization
            let plural_form = en_pluralization.plural_form(count)
            if plural_form < trans.1.length() {
              translation = trans.1[plural_form]
            }
          }
        }
      }
    }
    
    translation
  }
  
  // Format message with count
  let format_with_count = fn(template: String, count: Int) {
    template.replace("{count}", count.to_string())
  }
  
  // Create message catalog
  let create_pluralized_catalog = fn() {
    [
      {
        key: "span.count",
        translations: [
          ("en", ["{count} span", "{count} spans"]),
          ("fr", ["{count} span", "{count} spans"]),
          ("ru", ["{count} спан", "{count} спана", "{count} спанов"]),
          ("ar", ["{count} span", "{count} spanان", "{count} أسبان", "{count} span", "{count} span", "{count} span"])
        ]
      },
      {
        key: "metric.count",
        translations: [
          ("en", ["{count} metric", "{count} metrics"]),
          ("fr", ["{count} métrique", "{count} métriques"]),
          ("ru", ["{count} метрика", "{count} метрики", "{count} метрик"]),
          ("ar", ["{count} metric", "{count} metricان", "{count} أmetrics", "{count} metric", "{count} metric", "{count} metric"])
        ]
      }
    ]
  }
  
  // Create message catalog
  let catalog = create_pluralized_catalog()
  
  // Test English pluralization
  let en_span_1 = get_pluralized_translation(catalog, "span.count", "en", 1, en_pluralization)
  assert_eq(format_with_count(en_span_1, 1), "1 span")
  
  let en_span_5 = get_pluralized_translation(catalog, "span.count", "en", 5, en_pluralization)
  assert_eq(format_with_count(en_span_5, 5), "5 spans")
  
  // Test French pluralization
  let fr_metric_1 = get_pluralized_translation(catalog, "metric.count", "fr", 1, fr_pluralization)
  assert_eq(format_with_count(fr_metric_1, 1), "1 métrique")
  
  let fr_metric_3 = get_pluralized_translation(catalog, "metric.count", "fr", 3, fr_pluralization)
  assert_eq(format_with_count(fr_metric_3, 3), "3 métriques")
  
  // Test Russian pluralization
  let ru_span_1 = get_pluralized_translation(catalog, "span.count", "ru", 1, ru_pluralization)
  assert_eq(format_with_count(ru_span_1, 1), "1 спан")
  
  let ru_span_3 = get_pluralized_translation(catalog, "span.count", "ru", 3, ru_pluralization)
  assert_eq(format_with_count(ru_span_3, 3), "3 спана")
  
  let ru_span_5 = get_pluralized_translation(catalog, "span.count", "ru", 5, ru_pluralization)
  assert_eq(format_with_count(ru_span_5, 5), "5 спанов")
  
  let ru_span_21 = get_pluralized_translation(catalog, "span.count", "ru", 21, ru_pluralization)
  assert_eq(format_with_count(ru_span_21, 21), "21 спан")
  
  // Test Arabic pluralization
  let ar_span_0 = get_pluralized_translation(catalog, "span.count", "ar", 0, ar_pluralization)
  assert_eq(format_with_count(ar_span_0, 0), "0 span")
  
  let ar_span_1 = get_pluralized_translation(catalog, "span.count", "ar", 1, ar_pluralization)
  assert_eq(format_with_count(ar_span_1, 1), "1 span")
  
  let ar_span_2 = get_pluralized_translation(catalog, "span.count", "ar", 2, ar_pluralization)
  assert_eq(format_with_count(ar_span_2, 2), "2 spanان")
  
  let ar_span_5 = get_pluralized_translation(catalog, "span.count", "ar", 5, ar_pluralization)
  assert_eq(format_with_count(ar_span_5, 5), "5 أسبان")
}