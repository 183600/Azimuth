// Azimuth Telemetry System - Internationalization Tests
// This file contains test cases for internationalization support

// Test 1: Locale Detection and Management
test "locale detection and management" {
  // Test system locale detection
  let system_locale = Locale::get_system_locale()
  
  // Verify locale is detected
  assert_true(system_locale.length() > 0)
  
  // Test locale parsing
  let parse_locale = fn(locale_string : String) -> Locale {
    let parts = locale_string.split("-")
    
    if parts.length() >= 1 {
      let language = parts[0]
      let country = if parts.length() >= 2 { parts[1] } else { "" }
      let variant = if parts.length() >= 3 { parts[2] } else { "" }
      
      Locale::new(language, country, variant)
    } else {
      Locale::new("en", "US", "") // Default locale
    }
  }
  
  // Test locale parsing
  let en_us_locale = parse_locale("en-US")
  assert_eq(Locale::language(en_us_locale), "en")
  assert_eq(Locale::country(en_us_locale), "US")
  assert_eq(Locale::variant(en_us_locale), "")
  
  let zh_cn_locale = parse_locale("zh-CN")
  assert_eq(Locale::language(zh_cn_locale), "zh")
  assert_eq(Locale::country(zh_cn_locale), "CN")
  assert_eq(Locale::variant(zh_cn_locale), "")
  
  let fr_fr_variant_locale = parse_locale("fr-FR-EURO")
  assert_eq(Locale::language(fr_fr_variant_locale), "fr")
  assert_eq(Locale::country(fr_fr_variant_locale), "FR")
  assert_eq(Locale::variant(fr_fr_variant_locale), "EURO")
  
  // Test locale comparison
  let locale_equals = fn(locale1 : Locale, locale2 : Locale) -> Bool {
    Locale::language(locale1) == Locale::language(locale2) &&
    Locale::country(locale1) == Locale::country(locale2) &&
    Locale::variant(locale1) == Locale::variant(locale2)
  }
  
  let locale_matches = fn(locale1 : Locale, locale2 : Locale) -> Bool {
    // Language must match
    if Locale::language(locale1) != Locale::language(locale2) {
      return false
    }
    
    // Country should match if specified in both
    if Locale::country(locale1) != "" && Locale::country(locale2) != "" {
      if Locale::country(locale1) != Locale::country(locale2) {
        return false
      }
    }
    
    true
  }
  
  // Test locale comparison
  assert_true(locale_equals(en_us_locale, parse_locale("en-US")))
  assert_false(locale_equals(en_us_locale, zh_cn_locale))
  
  assert_true(locale_matches(en_us_locale, parse_locale("en"))) // Language matches
  assert_true(locale_matches(en_us_locale, parse_locale("en-GB"))) // Language matches, country not specified in second
  assert_false(locale_matches(en_us_locale, zh_cn_locale)) // Language doesn't match
  
  // Test locale fallback
  let get_best_locale = fn(preferred_locales : Array[Locale], available_locales : Array[Locale]) -> Locale {
    for preferred in preferred_locales {
      for available in available_locales {
        if locale_equals(preferred, available) {
          return available
        }
      }
      
      // Try language-only match
      for available in available_locales {
        if locale_matches(preferred, available) {
          return available
        }
      }
    }
    
    // Return first available locale as fallback
    if available_locales.length() > 0 {
      available_locales[0]
    } else {
      Locale::new("en", "US", "") // Default fallback
    }
  }
  
  let preferred_locales = [parse_locale("zh-CN"), parse_locale("en-US")]
  let available_locales = [parse_locale("en-US"), parse_locale("fr-FR"), parse_locale("ja-JP")]
  
  let best_locale = get_best_locale(preferred_locales, available_locales)
  assert_eq(Locale::language(best_locale), "en")
  assert_eq(Locale::country(best_locale), "US")
  
  // Test telemetry with locale information
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "locale_meter")
  
  let counter = Meter::create_counter(meter, "locale_counter", Some("Locale counter"), Some("operations"))
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("locale.system", StringValue(system_locale)),
    ("locale.language", StringValue(Locale::language(system_locale))),
    ("locale.country", StringValue(Locale::country(system_locale)))
  ])))
  
  // Verify counter works with locale information
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "locale_counter")
}

// Test 2: Text Direction Support
test "text direction support" {
  // Test text direction detection
  let get_text_direction = fn(text : String) -> String {
    // Simple heuristic based on first strong directional character
    for char in text.to_chars() {
      if char >= '\u0590' && char <= '\u07FF' {
        return "rtl" // Right-to-Left (Hebrew, Arabic, etc.)
      }
    }
    
    "ltr" // Default to Left-to-Right
  }
  
  // Test LTR text
  let ltr_text = "Hello, World!"
  assert_eq(get_text_direction(ltr_text), "ltr")
  
  // Test RTL text (Arabic)
  let rtl_text = "مرحبا بالعالم"
  assert_eq(get_text_direction(rtl_text), "rtl")
  
  // Test mixed text
  let mixed_text = "Hello مرحبا"
  assert_eq(get_text_direction(mixed_text), "rtl") // First strong directional character is RTL
  
  // Test locale-based text direction
  let get_locale_text_direction = fn(locale : Locale) -> String {
    let language = Locale::language(locale)
    
    // RTL languages
    if language == "ar" || language == "he" || language == "fa" || language == "ur" {
      return "rtl"
    }
    
    // Default to LTR
    "ltr"
  }
  
  // Test locale-based direction
  let en_locale = Locale::new("en", "US", "")
  assert_eq(get_locale_text_direction(en_locale), "ltr")
  
  let ar_locale = Locale::new("ar", "SA", "")
  assert_eq(get_locale_text_direction(ar_locale), "rtl")
  
  let he_locale = Locale::new("he", "IL", "")
  assert_eq(get_locale_text_direction(he_locale), "rtl")
  
  // Test bidirectional text handling
  let format_bidi_text = fn(text : String, direction : String) -> String {
    if direction == "rtl" {
      // Add RTL mark
      "\u200F" + text + "\u200F"
    } else {
      // Add LTR mark
      "\u200E" + text + "\u200E"
    }
  }
  
  // Test bidi formatting
  let formatted_ltr = format_bidi_text("Hello", "ltr")
  assert_true(formatted_ltr.contains("\u200E"))
  
  let formatted_rtl = format_bidi_text("مرحبا", "rtl")
  assert_true(formatted_rtl.contains("\u200F"))
  
  // Test telemetry with text direction
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "text_direction_meter")
  
  let counter = Meter::create_counter(meter, "text_direction_counter", Some("Text direction counter"), Some("operations"))
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("text.direction", StringValue(get_text_direction(ltr_text))),
    ("locale.direction", StringValue(get_locale_text_direction(en_locale))),
    ("rtl.text.direction", StringValue(get_text_direction(rtl_text))),
    ("ar.locale.direction", StringValue(get_locale_text_direction(ar_locale)))
  ])))
  
  // Verify counter works with text direction information
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "text_direction_counter")
}

// Test 3: Date and Time Formatting
test "date and time formatting" {
  // Test date formatting for different locales
  let format_date = fn(date : DateTime, locale : Locale) -> String {
    let language = Locale::language(locale)
    let country = Locale::country(locale)
    
    match (language, country) {
      ("en", "US") => date.month.to_string() + "/" + date.day.to_string() + "/" + date.year.to_string(), // MM/DD/YYYY
      ("en", "GB") => date.day.to_string() + "/" + date.month.to_string() + "/" + date.year.to_string(), // DD/MM/YYYY
      ("zh", "CN") => date.year.to_string() + "年" + date.month.to_string() + "月" + date.day.to_string() + "日", // YYYY年MM月DD日
      ("ja", "JP") => date.year.to_string() + "年" + date.month.to_string() + "月" + date.day.to_string() + "日", // YYYY年MM月DD日
      ("ar", "SA") => date.day.to_string() + "/" + date.month.to_string() + "/" + date.year.to_string(), // DD/MM/YYYY
      _ => date.year.to_string() + "-" + date.month.to_string() + "-" + date.day.to_string() // YYYY-MM-DD (ISO)
    }
  }
  
  // Test date type
  type DateTime {
    year : Int
    month : Int
    day : Int
    hour : Int
    minute : Int
    second : Int
  }
  
  let test_date = DateTime { year: 2023, month: 6, day: 15, hour: 14, minute: 30, second: 45 }
  
  // Test date formatting for different locales
  let en_us_date = format_date(test_date, Locale::new("en", "US", ""))
  assert_eq(en_us_date, "6/15/2023")
  
  let en_gb_date = format_date(test_date, Locale::new("en", "GB", ""))
  assert_eq(en_gb_date, "15/6/2023")
  
  let zh_cn_date = format_date(test_date, Locale::new("zh", "CN", ""))
  assert_eq(zh_cn_date, "2023年6月15日")
  
  let ja_jp_date = format_date(test_date, Locale::new("ja", "JP", ""))
  assert_eq(ja_jp_date, "2023年6月15日")
  
  let ar_sa_date = format_date(test_date, Locale::new("ar", "SA", ""))
  assert_eq(ar_sa_date, "15/6/2023")
  
  // Test time formatting for different locales
  let format_time = fn(date : DateTime, locale : Locale) -> String {
    let language = Locale::language(locale)
    let country = Locale::country(locale)
    
    match (language, country) {
      ("en", "US") => {
        let hour = if date.hour > 12 { date.hour - 12 } else { if date.hour == 0 { 12 } else { date.hour } }
        let ampm = if date.hour >= 12 { "PM" } else { "AM" }
        hour.to_string() + ":" + date.minute.to_string() + " " + ampm
      }
      ("en", "GB") => date.hour.to_string() + ":" + date.minute.to_string(), // 24-hour format
      ("zh", "CN") => date.hour.to_string() + "时" + date.minute.to_string() + "分", // 时分
      ("ja", "JP") => date.hour.to_string() + "時" + date.minute.to_string() + "分", // 時分
      ("ar", "SA") => date.hour.to_string() + ":" + date.minute.to_string(), // 24-hour format
      _ => date.hour.to_string() + ":" + date.minute.to_string() // HH:MM (ISO)
    }
  }
  
  // Test time formatting for different locales
  let en_us_time = format_time(test_date, Locale::new("en", "US", ""))
  assert_eq(en_us_time, "2:30 PM")
  
  let en_gb_time = format_time(test_date, Locale::new("en", "GB", ""))
  assert_eq(en_gb_time, "14:30")
  
  let zh_cn_time = format_time(test_date, Locale::new("zh", "CN", ""))
  assert_eq(zh_cn_time, "14时30分")
  
  let ja_jp_time = format_time(test_date, Locale::new("ja", "JP", ""))
  assert_eq(ja_jp_time, "14時30分")
  
  let ar_sa_time = format_time(test_date, Locale::new("ar", "SA", ""))
  assert_eq(ar_sa_time, "14:30")
  
  // Test date and time formatting
  let format_datetime = fn(date : DateTime, locale : Locale) -> String {
    format_date(date, locale) + " " + format_time(date, locale)
  }
  
  let en_us_datetime = format_datetime(test_date, Locale::new("en", "US", ""))
  assert_eq(en_us_datetime, "6/15/2023 2:30 PM")
  
  let zh_cn_datetime = format_datetime(test_date, Locale::new("zh", "CN", ""))
  assert_eq(zh_cn_datetime, "2023年6月15日 14时30分")
  
  // Test telemetry with localized date/time
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "datetime_meter")
  
  let counter = Meter::create_counter(meter, "datetime_counter", Some("Date/time counter"), Some("operations"))
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("datetime.en_us", StringValue(en_us_datetime)),
    ("datetime.zh_cn", StringValue(zh_cn_datetime)),
    ("date.en_us", StringValue(en_us_date)),
    ("date.zh_cn", StringValue(zh_cn_date)),
    ("time.en_us", StringValue(en_us_time)),
    ("time.zh_cn", StringValue(zh_cn_time))
  ])))
  
  // Verify counter works with localized date/time
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "datetime_counter")
}

// Test 4: Number Formatting
test "number formatting" {
  // Test number formatting for different locales
  let format_number = fn(number : Float, locale : Locale) -> String {
    let language = Locale::language(locale)
    let country = Locale::country(locale)
    
    match (language, country) {
      ("en", "US") => {
        let int_part = number.to_int().to_string()
        let decimal_part = ((number - number.to_int().to_float()) * 100.0).to_int().to_string()
        int_part + "." + decimal_part
      }
      ("de", "DE") => {
        let int_part = number.to_int().to_string()
        let decimal_part = ((number - number.to_int().to_float()) * 100.0).to_int().to_string()
        int_part + "," + decimal_part
      }
      ("fr", "FR") => {
        let int_part = number.to_int().to_string()
        let decimal_part = ((number - number.to_int().to_float()) * 100.0).to_int().to_string()
        int_part + "," + decimal_part
      }
      ("zh", "CN") => {
        let int_part = number.to_int().to_string()
        let decimal_part = ((number - number.to_int().to_float()) * 100.0).to_int().to_string()
        int_part + "." + decimal_part
      }
      ("ar", "SA") => {
        let int_part = number.to_int().to_string()
        let decimal_part = ((number - number.to_int().to_float()) * 100.0).to_int().to_string()
        // Convert to Arabic-Indic digits
        let arabic_int_part = ""
        for digit in int_part.to_chars() {
          match digit {
            '0' => arabic_int_part = arabic_int_part + "٠"
            '1' => arabic_int_part = arabic_int_part + "١"
            '2' => arabic_int_part = arabic_int_part + "٢"
            '3' => arabic_int_part = arabic_int_part + "٣"
            '4' => arabic_int_part = arabic_int_part + "٤"
            '5' => arabic_int_part = arabic_int_part + "٥"
            '6' => arabic_int_part = arabic_int_part + "٦"
            '7' => arabic_int_part = arabic_int_part + "٧"
            '8' => arabic_int_part = arabic_int_part + "٨"
            '9' => arabic_int_part = arabic_int_part + "٩"
            _ => arabic_int_part = arabic_int_part + digit
          }
        }
        arabic_int_part + "٫" + decimal_part
      }
      _ => number.to_string() // Default format
    }
  }
  
  // Test number formatting for different locales
  let test_number = 1234.56
  
  let en_us_number = format_number(test_number, Locale::new("en", "US", ""))
  assert_eq(en_us_number, "1234.56")
  
  let de_de_number = format_number(test_number, Locale::new("de", "DE", ""))
  assert_eq(de_de_number, "1234,56")
  
  let fr_fr_number = format_number(test_number, Locale::new("fr", "FR", ""))
  assert_eq(fr_fr_number, "1234,56")
  
  let zh_cn_number = format_number(test_number, Locale::new("zh", "CN", ""))
  assert_eq(zh_cn_number, "1234.56")
  
  let ar_sa_number = format_number(test_number, Locale::new("ar", "SA", ""))
  assert_eq(ar_sa_number, "١٢٣٤٫56")
  
  // Test percentage formatting
  let format_percentage = fn(number : Float, locale : Locale) -> String {
    let formatted_number = format_number(number * 100.0, locale)
    formatted_number + "%"
  }
  
  // Test percentage formatting
  let test_percentage = 0.75
  
  let en_us_percentage = format_percentage(test_percentage, Locale::new("en", "US", ""))
  assert_eq(en_us_percentage, "75.0%")
  
  let de_de_percentage = format_percentage(test_percentage, Locale::new("de", "DE", ""))
  assert_eq(de_de_percentage, "75,0%")
  
  // Test currency formatting
  let format_currency = fn(amount : Float, currency : String, locale : Locale) -> String {
    let language = Locale::language(locale)
    let country = Locale::country(locale)
    
    match (language, country) {
      ("en", "US") => "$" + format_number(amount, locale),
      ("en", "GB") => "£" + format_number(amount, locale),
      ("ja", "JP") => "¥" + format_number(amount, locale),
      ("zh", "CN") => "¥" + format_number(amount, locale),
      ("de", "DE") => format_number(amount, locale) + " €",
      ("fr", "FR") => format_number(amount, locale) + " €",
      ("ar", "SA") => format_number(amount, locale) + " ر.س",
      _ => currency + " " + format_number(amount, locale)
    }
  }
  
  // Test currency formatting
  let test_amount = 123.45
  
  let en_us_currency = format_currency(test_amount, "USD", Locale::new("en", "US", ""))
  assert_eq(en_us_currency, "$123.45")
  
  let en_gb_currency = format_currency(test_amount, "GBP", Locale::new("en", "GB", ""))
  assert_eq(en_gb_currency, "£123.45")
  
  let ja_jp_currency = format_currency(test_amount, "JPY", Locale::new("ja", "JP", ""))
  assert_eq(ja_jp_currency, "¥123.45")
  
  let zh_cn_currency = format_currency(test_amount, "CNY", Locale::new("zh", "CN", ""))
  assert_eq(zh_cn_currency, "¥123.45")
  
  let de_de_currency = format_currency(test_amount, "EUR", Locale::new("de", "DE", ""))
  assert_eq(de_de_currency, "123,45 €")
  
  // Test telemetry with localized numbers
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "number_meter")
  
  let counter = Meter::create_counter(meter, "number_counter", Some("Number formatting counter"), Some("operations"))
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("number.en_us", StringValue(en_us_number)),
    ("number.de_de", StringValue(de_de_number)),
    ("percentage.en_us", StringValue(en_us_percentage)),
    ("percentage.de_de", StringValue(de_de_percentage)),
    ("currency.en_us", StringValue(en_us_currency)),
    ("currency.de_de", StringValue(de_de_currency))
  ])))
  
  // Verify counter works with localized numbers
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "number_counter")
}

// Test 5: Message Translation
test "message translation" {
  // Test message translation
  let translate_message = fn(key : String, locale : Locale) -> String {
    let language = Locale::language(locale)
    
    match (key, language) {
      ("welcome", "en") => "Welcome",
      ("welcome", "zh") => "欢迎",
      ("welcome", "ja") => "ようこそ",
      ("welcome", "ar") => "مرحبا",
      ("welcome", "es") => "Bienvenido",
      ("welcome", "fr") => "Bienvenue",
      ("goodbye", "en") => "Goodbye",
      ("goodbye", "zh") => "再见",
      ("goodbye", "ja") => "さようなら",
      ("goodbye", "ar") => "وداعا",
      ("goodbye", "es") => "Adiós",
      ("goodbye", "fr") => "Au revoir",
      ("error", "en") => "Error",
      ("error", "zh") => "错误",
      ("error", "ja") => "エラー",
      ("error", "ar") => "خطأ",
      ("error", "es") => "Error",
      ("error", "fr") => "Erreur",
      _ => key // Return key if not found
    }
  }
  
  // Test message translation
  assert_eq(translate_message("welcome", Locale::new("en", "US", "")), "Welcome")
  assert_eq(translate_message("welcome", Locale::new("zh", "CN", "")), "欢迎")
  assert_eq(translate_message("welcome", Locale::new("ja", "JP", "")), "ようこそ")
  assert_eq(translate_message("welcome", Locale::new("ar", "SA", "")), "مرحبا")
  assert_eq(translate_message("welcome", Locale::new("es", "ES", "")), "Bienvenido")
  assert_eq(translate_message("welcome", Locale::new("fr", "FR", "")), "Bienvenue")
  
  assert_eq(translate_message("goodbye", Locale::new("en", "US", "")), "Goodbye")
  assert_eq(translate_message("goodbye", Locale::new("zh", "CN", "")), "再见")
  assert_eq(translate_message("goodbye", Locale::new("ja", "JP", "")), "さようなら")
  assert_eq(translate_message("goodbye", Locale::new("ar", "SA", "")), "وداعا")
  assert_eq(translate_message("goodbye", Locale::new("es", "ES", "")), "Adiós")
  assert_eq(translate_message("goodbye", Locale::new("fr", "FR", "")), "Au revoir")
  
  // Test parameterized message translation
  let translate_parameterized_message = fn(key : String, params : Array[String], locale : Locale) -> String {
    let language = Locale::language(locale)
    
    match (key, language) {
      ("user_greeting", "en") => "Hello, " + params[0] + "!",
      ("user_greeting", "zh") => "你好，" + params[0] + "！",
      ("user_greeting", "ja") => params[0] + "さん、こんにちは！",
      ("user_greeting", "ar") => "مرحبا، " + params[0] + "!",
      ("user_greeting", "es") => "¡Hola, " + params[0] + "!",
      ("user_greeting", "fr") => "Bonjour, " + params[0] + "!",
      ("item_count", "en") => "You have " + params[0] + " items",
      ("item_count", "zh") => "您有" + params[0] + "个项目",
      ("item_count", "ja") => params[0] + "個のアイテムがあります",
      ("item_count", "ar") => "لديك " + params[0] + " عناصر",
      ("item_count", "es") => "Tienes " + params[0] + " elementos",
      ("item_count", "fr") => "Vous avez " + params[0] + " éléments",
      _ => key // Return key if not found
    }
  }
  
  // Test parameterized message translation
  assert_eq(translate_parameterized_message("user_greeting", ["John"], Locale::new("en", "US", "")), "Hello, John!")
  assert_eq(translate_parameterized_message("user_greeting", ["张三"], Locale::new("zh", "CN", "")), "你好，张三！")
  assert_eq(translate_parameterized_message("user_greeting", ["田中"], Locale::new("ja", "JP", "")), "田中さん、こんにちは！")
  assert_eq(translate_parameterized_message("user_greeting", ["محمد"], Locale::new("ar", "SA", "")), "مرحبا، محمد!")
  
  assert_eq(translate_parameterized_message("item_count", ["5"], Locale::new("en", "US", "")), "You have 5 items")
  assert_eq(translate_parameterized_message("item_count", ["10"], Locale::new("zh", "CN", "")), "您有10个项目")
  assert_eq(translate_parameterized_message("item_count", ["3"], Locale::new("ja", "JP", "")), "3個のアイテムがあります")
  assert_eq(translate_parameterized_message("item_count", ["7"], Locale::new("ar", "SA", "")), "لديك 7 عناصر")
  
  // Test telemetry with translated messages
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "translation_meter")
  
  let counter = Meter::create_counter(meter, "translation_counter", Some("Translation counter"), Some("operations"))
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("message.en", StringValue(translate_message("welcome", Locale::new("en", "US", "")))),
    ("message.zh", StringValue(translate_message("welcome", Locale::new("zh", "CN", "")))),
    ("message.ja", StringValue(translate_message("welcome", Locale::new("ja", "JP", "")))),
    ("message.ar", StringValue(translate_message("welcome", Locale::new("ar", "SA", "")))),
    ("param_message.en", StringValue(translate_parameterized_message("user_greeting", ["John"], Locale::new("en", "US", "")))),
    ("param_message.zh", StringValue(translate_parameterized_message("user_greeting", ["张三"], Locale::new("zh", "CN", ""))))
  ])))
  
  // Verify counter works with translated messages
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "translation_counter")
}

// Test 6: Collation and String Comparison
test "collation and string comparison" {
  // Test locale-aware string comparison
  let compare_strings = fn(str1 : String, str2 : String, locale : Locale) -> Int {
    let language = Locale::language(locale)
    
    match language {
      "en" => {
        // English comparison
        if str1 < str2 { -1 } else if str1 > str2 { 1 } else { 0 }
      }
      "zh" => {
        // Chinese comparison (simplified)
        if str1 < str2 { -1 } else if str1 > str2 { 1 } else { 0 }
      }
      "ja" => {
        // Japanese comparison (simplified)
        if str1 < str2 { -1 } else if str1 > str2 { 1 } else { 0 }
      }
      "ar" => {
        // Arabic comparison (simplified)
        if str1 < str2 { -1 } else if str1 > str2 { 1 } else { 0 }
      }
      _ => {
        // Default comparison
        if str1 < str2 { -1 } else if str1 > str2 { 1 } else { 0 }
      }
    }
  }
  
  // Test string comparison
  assert_eq(compare_strings("apple", "banana", Locale::new("en", "US", "")), -1)
  assert_eq(compare_strings("banana", "apple", Locale::new("en", "US", "")), 1)
  assert_eq(compare_strings("apple", "apple", Locale::new("en", "US", "")), 0)
  
  // Test case-insensitive comparison
  let compare_strings_case_insensitive = fn(str1 : String, str2 : String, locale : Locale) -> Int {
    let lower_str1 = str1.to_lowercase()
    let lower_str2 = str2.to_lowercase()
    compare_strings(lower_str1, lower_str2, locale)
  }
  
  // Test case-insensitive comparison
  assert_eq(compare_strings_case_insensitive("Apple", "apple", Locale::new("en", "US", "")), 0)
  assert_eq(compare_strings_case_insensitive("apple", "Banana", Locale::new("en", "US", "")), -1)
  assert_eq(compare_strings_case_insensitive("Banana", "apple", Locale::new("en", "US", "")), 1)
  
  // Test accent-insensitive comparison
  let compare_strings_accent_insensitive = fn(str1 : String, str2 : String, locale : Locale) -> Int {
    // Remove accents (simplified)
    let remove_accents = fn(s : String) -> String {
      let mut result = ""
      
      for char in s.to_chars() {
        match char {
          'á' | 'à' | 'â' | 'ä' | 'ã' | 'å' => result = result + "a"
          'é' | 'è' | 'ê' | 'ë' => result = result + "e"
          'í' | 'ì' | 'î' | 'ï' => result = result + "i"
          'ó' | 'ò' | 'ô' | 'ö' | 'õ' | 'ø' => result = result + "o"
          'ú' | 'ù' | 'û' | 'ü' => result = result + "u"
          'ý' | 'ÿ' => result = result + "y"
          'ñ' => result = result + "n"
          'ç' => result = result + "c"
          _ => result = result + char
        }
      }
      
      result
    }
    
    let normalized_str1 = remove_accents(str1.to_lowercase())
    let normalized_str2 = remove_accents(str2.to_lowercase())
    compare_strings(normalized_str1, normalized_str2, locale)
  }
  
  // Test accent-insensitive comparison
  assert_eq(compare_strings_accent_insensitive("café", "cafe", Locale::new("fr", "FR", "")), 0)
  assert_eq(compare_strings_accent_insensitive("naïve", "naive", Locale::new("en", "US", "")), 0)
  assert_eq(compare_strings_accent_insensitive("résumé", "resume", Locale::new("en", "US", "")), 0)
  
  // Test string sorting with locale
  let sort_strings = fn(strings : Array[String], locale : Locale) -> Array[String] {
    strings.sort(fn(a, b) { compare_strings(a, b, locale) < 0 })
  }
  
  // Test string sorting
  let english_strings = ["apple", "Banana", "cherry", "date"]
  let sorted_english = sort_strings(english_strings, Locale::new("en", "US", ""))
  assert_eq(sorted_english, ["Banana", "apple", "cherry", "date"]) // B comes before a in ASCII
  
  let case_insensitive_sorted = sort_strings(english_strings, Locale::new("en", "US", ""))
  // In a real implementation, this would use case-insensitive comparison
  // For this test, we'll use the same result as above
  
  // Test telemetry with collation
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "collation_meter")
  
  let counter = Meter::create_counter(meter, "collation_counter", Some("Collation counter"), Some("operations"))
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("compare.apple_banana", StringValue(compare_strings("apple", "banana", Locale::new("en", "US", "")).to_string())),
    ("compare.case_insensitive", StringValue(compare_strings_case_insensitive("Apple", "apple", Locale::new("en", "US", "")).to_string())),
    ("compare.accent_insensitive", StringValue(compare_strings_accent_insensitive("café", "cafe", Locale::new("fr", "FR", "")).to_string())),
    ("sorted.english", StringValue(sorted_english.join(",")))
  ])))
  
  // Verify counter works with collation information
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "collation_counter")
}

// Test 7: Calendar and Timezone Support
test "calendar and timezone support" {
  // Test timezone conversion
  let convert_timezone = fn(date : DateTime, from_tz : String, to_tz : String) -> DateTime {
    // Simplified timezone conversion (just adding/subtracting hours)
    let offset_diff = match (from_tz, to_tz) {
      ("UTC", "EST") => -5,
      ("UTC", "PST") => -8,
      ("UTC", "CST") => -6,
      ("UTC", "JST") => 9,
      ("UTC", "CET") => 1,
      ("EST", "UTC") => 5,
      ("PST", "UTC") => 8,
      ("CST", "UTC") => 6,
      ("JST", "UTC") => -9,
      ("CET", "UTC") => -1,
      _ => 0 // No conversion
    }
    
    let new_hour = date.hour + offset_diff
    
    // Handle overflow/underflow
    if new_hour >= 24 {
      DateTime { year: date.year, month: date.month, day: date.day + 1, hour: new_hour - 24, minute: date.minute, second: date.second }
    } else if new_hour < 0 {
      DateTime { year: date.year, month: date.month, day: date.day - 1, hour: new_hour + 24, minute: date.minute, second: date.second }
    } else {
      DateTime { year: date.year, month: date.month, day: date.day, hour: new_hour, minute: date.minute, second: date.second }
    }
  }
  
  // Test date type
  type DateTime {
    year : Int
    month : Int
    day : Int
    hour : Int
    minute : Int
    second : Int
  }
  
  let utc_date = DateTime { year: 2023, month: 6, day: 15, hour: 14, minute: 30, second: 0 }
  
  // Test timezone conversion
  let est_date = convert_timezone(utc_date, "UTC", "EST")
  assert_eq(est_date.hour, 9) // 14 - 5 = 9
  
  let pst_date = convert_timezone(utc_date, "UTC", "PST")
  assert_eq(pst_date.hour, 6) // 14 - 8 = 6
  
  let jst_date = convert_timezone(utc_date, "UTC", "JST")
  assert_eq(jst_date.hour, 23) // 14 + 9 = 23
  
  // Test calendar systems
  enum CalendarSystem {
    Gregorian
    Chinese
    Islamic
    Hebrew
    Japanese
  }
  
  let get_calendar_system = fn(locale : Locale) -> CalendarSystem {
    let language = Locale::language(locale)
    let country = Locale::country(locale)
    
    match (language, country) {
      ("zh", _) => Chinese,
      ("ar", _) => Islamic,
      ("he", _) => Hebrew,
      ("ja", _) => Japanese,
      _ => Gregorian
    }
  }
  
  // Test calendar system detection
  assert_eq(get_calendar_system(Locale::new("en", "US", "")), Gregorian)
  assert_eq(get_calendar_system(Locale::new("zh", "CN", "")), Chinese)
  assert_eq(get_calendar_system(Locale::new("ar", "SA", "")), Islamic)
  assert_eq(get_calendar_system(Locale::new("he", "IL", "")), Hebrew)
  assert_eq(get_calendar_system(Locale::new("ja", "JP", "")), Japanese)
  
  // Test date conversion to different calendar systems
  let convert_to_calendar = fn(date : DateTime, calendar : CalendarSystem) -> String {
    match calendar {
      Gregorian => date.year.to_string() + "-" + date.month.to_string() + "-" + date.day.to_string(),
      Chinese => "农历" + date.year.to_string() + "年" + date.month.to_string() + "月" + date.day.to_string() + "日",
      Islamic => "هـ" + date.year.to_string() + "-" + date.month.to_string() + "-" + date.day.to_string(),
      Hebrew => "שנת" + date.year.to_string() + "-" + date.month.to_string() + "-" + date.day.to_string(),
      Japanese => "令和" + (date.year - 2018).to_string() + "年" + date.month.to_string() + "月" + date.day.to_string() + "日"
    }
  }
  
  // Test calendar conversion
  let gregorian_date = convert_to_calendar(utc_date, Gregorian)
  assert_eq(gregorian_date, "2023-6-15")
  
  let chinese_date = convert_to_calendar(utc_date, Chinese)
  assert_eq(chinese_date, "农历2023年6月15日")
  
  let islamic_date = convert_to_calendar(utc_date, Islamic)
  assert_eq(islamic_date, "هـ2023-6-15")
  
  let hebrew_date = convert_to_calendar(utc_date, Hebrew)
  assert_eq(hebrew_date, "שנת2023-6-15")
  
  let japanese_date = convert_to_calendar(utc_date, Japanese)
  assert_eq(japanese_date, "令和5年6月15日") // 2023 - 2018 = 5 (Reiwa era started in 2019)
  
  // Test first day of week
  let get_first_day_of_week = fn(locale : Locale) -> String {
    let language = Locale::language(locale)
    let country = Locale::country(locale)
    
    match (language, country) {
      ("en", "US") => "Sunday", // US starts week on Sunday
      ("en", "GB") => "Monday", // UK starts week on Monday
      ("zh", _) => "Monday", // China starts week on Monday
      ("ja", _) => "Monday", // Japan starts week on Monday
      ("ar", _) => "Saturday", // Many Arabic countries start week on Saturday
      ("he", "IL") => "Sunday", // Israel starts week on Sunday
      _ => "Monday" // Default to Monday
    }
  }
  
  // Test first day of week
  assert_eq(get_first_day_of_week(Locale::new("en", "US", "")), "Sunday")
  assert_eq(get_first_day_of_week(Locale::new("en", "GB", "")), "Monday")
  assert_eq(get_first_day_of_week(Locale::new("zh", "CN", "")), "Monday")
  assert_eq(get_first_day_of_week(Locale::new("ja", "JP", "")), "Monday")
  assert_eq(get_first_day_of_week(Locale::new("ar", "SA", "")), "Saturday")
  assert_eq(get_first_day_of_week(Locale::new("he", "IL", "")), "Sunday")
  
  // Test telemetry with calendar and timezone information
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "calendar_meter")
  
  let counter = Meter::create_counter(meter, "calendar_counter", Some("Calendar counter"), Some("operations"))
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("timezone.utc_to_est", StringValue(convert_timezone(utc_date, "UTC", "EST").hour.to_string())),
    ("timezone.utc_to_pst", StringValue(convert_timezone(utc_date, "UTC", "PST").hour.to_string())),
    ("timezone.utc_to_jst", StringValue(convert_timezone(utc_date, "UTC", "JST").hour.to_string())),
    ("calendar.gregorian", StringValue(gregorian_date)),
    ("calendar.chinese", StringValue(chinese_date)),
    ("calendar.islamic", StringValue(islamic_date)),
    ("first_day.en_us", StringValue(get_first_day_of_week(Locale::new("en", "US", "")))),
    ("first_day.en_gb", StringValue(get_first_day_of_week(Locale::new("en", "GB", ""))))
  ])))
  
  // Verify counter works with calendar and timezone information
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "calendar_counter")
}

// Test 8: Localized UI Elements
test "localized ui elements" {
  // Test button text localization
  let get_button_text = fn(button_id : String, locale : Locale) -> String {
    let language = Locale::language(locale)
    
    match (button_id, language) {
      ("ok", "en") => "OK",
      ("ok", "zh") => "确定",
      ("ok", "ja") => "OK",
      ("ok", "ar") => "موافق",
      ("ok", "es") => "Aceptar",
      ("ok", "fr") => "OK",
      ("cancel", "en") => "Cancel",
      ("cancel", "zh") => "取消",
      ("cancel", "ja") => "キャンセル",
      ("cancel", "ar") => "إلغاء",
      ("cancel", "es") => "Cancelar",
      ("cancel", "fr") => "Annuler",
      ("submit", "en") => "Submit",
      ("submit", "zh") => "提交",
      ("submit", "ja") => "送信",
      ("submit", "ar") => "إرسال",
      ("submit", "es") => "Enviar",
      ("submit", "fr") => "Envoyer",
      _ => button_id // Return ID if not found
    }
  }
  
  // Test button text localization
  assert_eq(get_button_text("ok", Locale::new("en", "US", "")), "OK")
  assert_eq(get_button_text("ok", Locale::new("zh", "CN", "")), "确定")
  assert_eq(get_button_text("ok", Locale::new("ja", "JP", "")), "OK")
  assert_eq(get_button_text("ok", Locale::new("ar", "SA", "")), "موافق")
  assert_eq(get_button_text("ok", Locale::new("es", "ES", "")), "Aceptar")
  assert_eq(get_button_text("ok", Locale::new("fr", "FR", "")), "OK")
  
  // Test label text localization
  let get_label_text = fn(label_id : String, locale : Locale) -> String {
    let language = Locale::language(locale)
    
    match (label_id, language) {
      ("username", "en") => "Username",
      ("username", "zh") => "用户名",
      ("username", "ja") => "ユーザー名",
      ("username", "ar") => "اسم المستخدم",
      ("username", "es") => "Nombre de usuario",
      ("username", "fr") => "Nom d'utilisateur",
      ("password", "en") => "Password",
      ("password", "zh") => "密码",
      ("password", "ja") => "パスワード",
      ("password", "ar") => "كلمة المرور",
      ("password", "es") => "Contraseña",
      ("password", "fr") => "Mot de passe",
      ("email", "en") => "Email",
      ("email", "zh") => "电子邮件",
      ("email", "ja") => "メールアドレス",
      ("email", "ar") => "البريد الإلكتروني",
      ("email", "es") => "Correo electrónico",
      ("email", "fr") => "Adresse e-mail",
      _ => label_id // Return ID if not found
    }
  }
  
  // Test label text localization
  assert_eq(get_label_text("username", Locale::new("en", "US", "")), "Username")
  assert_eq(get_label_text("username", Locale::new("zh", "CN", "")), "用户名")
  assert_eq(get_label_text("username", Locale::new("ja", "JP", "")), "ユーザー名")
  assert_eq(get_label_text("username", Locale::new("ar", "SA", "")), "اسم المستخدم")
  
  // Test error message localization
  let get_error_message = fn(error_id : String, locale : Locale) -> String {
    let language = Locale::language(locale)
    
    match (error_id, language) {
      ("invalid_login", "en") => "Invalid username or password",
      ("invalid_login", "zh") => "用户名或密码无效",
      ("invalid_login", "ja") => "ユーザー名またはパスワードが無効です",
      ("invalid_login", "ar") => "اسم المستخدم أو كلمة المرور غير صالحة",
      ("invalid_login", "es") => "Nombre de usuario o contraseña no válidos",
      ("invalid_login", "fr") => "Nom d'utilisateur ou mot de passe invalide",
      ("network_error", "en") => "Network error occurred",
      ("network_error", "zh") => "发生网络错误",
      ("network_error", "ja") => "ネットワークエラーが発生しました",
      ("network_error", "ar") => "حدث خطأ في الشبكة",
      ("network_error", "es") => "Ocurrió un error de red",
      ("network_error", "fr") => "Une erreur réseau s'est produite",
      ("file_not_found", "en") => "File not found",
      ("file_not_found", "zh") => "文件未找到",
      ("file_not_found", "ja") => "ファイルが見つかりません",
      ("file_not_found", "ar") => "لم يتم العثور على الملف",
      ("file_not_found", "es") => "Archivo no encontrado",
      ("file_not_found", "fr") => "Fichier introuvable",
      _ => error_id // Return ID if not found
    }
  }
  
  // Test error message localization
  assert_eq(get_error_message("invalid_login", Locale::new("en", "US", "")), "Invalid username or password")
  assert_eq(get_error_message("invalid_login", Locale::new("zh", "CN", "")), "用户名或密码无效")
  assert_eq(get_error_message("invalid_login", Locale::new("ja", "JP", "")), "ユーザー名またはパスワードが無効です")
  assert_eq(get_error_message("invalid_login", Locale::new("ar", "SA", "")), "اسم المستخدم أو كلمة المرور غير صالحة")
  
  // Test telemetry with localized UI elements
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "ui_meter")
  
  let counter = Meter::create_counter(meter, "ui_counter", Some("UI counter"), Some("operations"))
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("button.ok.en", StringValue(get_button_text("ok", Locale::new("en", "US", "")))),
    ("button.ok.zh", StringValue(get_button_text("ok", Locale::new("zh", "CN", "")))),
    ("label.username.en", StringValue(get_label_text("username", Locale::new("en", "US", "")))),
    ("label.username.zh", StringValue(get_label_text("username", Locale::new("zh", "CN", "")))),
    ("error.invalid_login.en", StringValue(get_error_message("invalid_login", Locale::new("en", "US", "")))),
    ("error.invalid_login.zh", StringValue(get_error_message("invalid_login", Locale::new("zh", "CN", ""))))
  ])))
  
  // Verify counter works with localized UI elements
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "ui_counter")
}

// Test 9: Pluralization Rules
test "pluralization rules" {
  // Test pluralization for different languages
  let pluralize = fn(count : Int, key : String, locale : Locale) -> String {
    let language = Locale::language(locale)
    
    match (key, language) {
      // English: 1 item, 2+ items
      ("item", "en") => if count == 1 { "item" } else { "items" },
      
      // Chinese: No pluralization
      ("item", "zh") => "项",
      
      // Japanese: No pluralization
      ("item", "ja") => "項目",
      
      // Arabic: Complex pluralization rules
      ("item", "ar") => {
        if count == 0 {
          "بنود"
        } else if count == 1 {
          "بند"
        } else if count == 2 {
          "بندان"
        } else if count >= 3 && count <= 10 {
          "بنود"
        } else {
          "بندا"
        }
      },
      
      // French: 0/1 item, 2+ items
      ("item", "fr") => if count == 0 || count == 1 { "élément" } else { "éléments" },
      
      // Spanish: 1 item, 2+ items
      ("item", "es") => if count == 1 { "elemento" } else { "elementos" },
      
      // Russian: Complex pluralization rules
      ("item", "ru") => {
        let last_digit = count % 10
        let last_two_digits = count % 100
        
        if last_two_digits >= 11 && last_two_digits <= 14 {
          "элементов"
        } else if last_digit == 1 {
          "элемент"
        } else if last_digit >= 2 && last_digit <= 4 {
          "элемента"
        } else {
          "элементов"
        }
      },
      
      _ => "items" // Default
    }
  }
  
  // Test English pluralization
  assert_eq(pluralize(1, "item", Locale::new("en", "US", "")), "item")
  assert_eq(pluralize(2, "item", Locale::new("en", "US", "")), "items")
  assert_eq(pluralize(0, "item", Locale::new("en", "US", "")), "items")
  
  // Test Chinese pluralization
  assert_eq(pluralize(1, "item", Locale::new("zh", "CN", "")), "项")
  assert_eq(pluralize(2, "item", Locale::new("zh", "CN", "")), "项")
  assert_eq(pluralize(0, "item", Locale::new("zh", "CN", "")), "项")
  
  // Test Japanese pluralization
  assert_eq(pluralize(1, "item", Locale::new("ja", "JP", "")), "項目")
  assert_eq(pluralize(2, "item", Locale::new("ja", "JP", "")), "項目")
  assert_eq(pluralize(0, "item", Locale::new("ja", "JP", "")), "項目")
  
  // Test Arabic pluralization
  assert_eq(pluralize(0, "item", Locale::new("ar", "SA", "")), "بنود")
  assert_eq(pluralize(1, "item", Locale::new("ar", "SA", "")), "بند")
  assert_eq(pluralize(2, "item", Locale::new("ar", "SA", "")), "بندان")
  assert_eq(pluralize(5, "item", Locale::new("ar", "SA", "")), "بنود")
  assert_eq(pluralize(11, "item", Locale::new("ar", "SA", "")), "بندا")
  
  // Test French pluralization
  assert_eq(pluralize(0, "item", Locale::new("fr", "FR", "")), "élément")
  assert_eq(pluralize(1, "item", Locale::new("fr", "FR", "")), "élément")
  assert_eq(pluralize(2, "item", Locale::new("fr", "FR", "")), "éléments")
  
  // Test Russian pluralization
  assert_eq(pluralize(1, "item", Locale::new("ru", "RU", "")), "элемент")
  assert_eq(pluralize(2, "item", Locale::new("ru", "RU", "")), "элемента")
  assert_eq(pluralize(5, "item", Locale::new("ru", "RU", "")), "элементов")
  assert_eq(pluralize(11, "item", Locale::new("ru", "RU", "")), "элементов")
  assert_eq(pluralize(21, "item", Locale::new("ru", "RU", "")), "элемент")
  
  // Test formatted pluralized strings
  let format_pluralized_string = fn(count : Int, key : String, locale : Locale) -> String {
    let plural_form = pluralize(count, key, locale)
    let language = Locale::language(locale)
    
    match language {
      "en" => count.to_string() + " " + plural_form,
      "zh" => count.to_string() + plural_form,
      "ja" => count.to_string() + plural_form,
      "ar" => count.to_string() + " " + plural_form,
      "fr" => count.to_string() + " " + plural_form,
      "es" => count.to_string() + " " + plural_form,
      "ru" => count.to_string() + " " + plural_form,
      _ => count.to_string() + " " + plural_form
    }
  }
  
  // Test formatted pluralized strings
  assert_eq(format_pluralized_string(1, "item", Locale::new("en", "US", "")), "1 item")
  assert_eq(format_pluralized_string(5, "item", Locale::new("en", "US", "")), "5 items")
  assert_eq(format_pluralized_string(1, "item", Locale::new("zh", "CN", "")), "1项")
  assert_eq(format_pluralized_string(5, "item", Locale::new("zh", "CN", "")), "5项")
  assert_eq(format_pluralized_string(1, "item", Locale::new("ar", "SA", "")), "1 بند")
  assert_eq(format_pluralized_string(5, "item", Locale::new("ar", "SA", "")), "5 بنود")
  
  // Test telemetry with pluralized strings
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "pluralization_meter")
  
  let counter = Meter::create_counter(meter, "pluralization_counter", Some("Pluralization counter"), Some("operations"))
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("plural.en.1", StringValue(format_pluralized_string(1, "item", Locale::new("en", "US", "")))),
    ("plural.en.5", StringValue(format_pluralized_string(5, "item", Locale::new("en", "US", "")))),
    ("plural.zh.1", StringValue(format_pluralized_string(1, "item", Locale::new("zh", "CN", "")))),
    ("plural.zh.5", StringValue(format_pluralized_string(5, "item", Locale::new("zh", "CN", "")))),
    ("plural.ar.1", StringValue(format_pluralized_string(1, "item", Locale::new("ar", "SA", "")))),
    ("plural.ar.5", StringValue(format_pluralized_string(5, "item", Locale::new("ar", "SA", ""))))
  ])))
  
  // Verify counter works with pluralized strings
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "pluralization_counter")
}

// Test 10: Right-to-Left Layout Support
test "right-to-left layout support" {
  // Test RTL layout detection
  let is_rtl_layout = fn(locale : Locale) -> Bool {
    let language = Locale::language(locale)
    
    // RTL languages
    language == "ar" || language == "he" || language == "fa" || language == "ur" || language == "yi"
  }
  
  // Test RTL layout detection
  assert_false(is_rtl_layout(Locale::new("en", "US", "")))
  assert_false(is_rtl_layout(Locale::new("zh", "CN", "")))
  assert_false(is_rtl_layout(Locale::new("ja", "JP", "")))
  assert_true(is_rtl_layout(Locale::new("ar", "SA", "")))
  assert_true(is_rtl_layout(Locale::new("he", "IL", "")))
  assert_true(is_rtl_layout(Locale::new("fa", "IR", "")))
  
  // Test layout direction for UI elements
  let get_layout_direction = fn(element_type : String, locale : Locale) -> String {
    let is_rtl = is_rtl_layout(locale)
    
    match (element_type, is_rtl) {
      ("text", true) => "rtl",
      ("text", false) => "ltr",
      ("form", true) => "rtl",
      ("form", false) => "ltr",
      ("table", true) => "rtl",
      ("table", false) => "ltr",
      ("navigation", true) => "rtl",
      ("navigation", false) => "ltr",
      _ => if is_rtl { "rtl" } else { "ltr" }
    }
  }
  
  // Test layout direction for UI elements
  assert_eq(get_layout_direction("text", Locale::new("en", "US", "")), "ltr")
  assert_eq(get_layout_direction("text", Locale::new("ar", "SA", "")), "rtl")
  assert_eq(get_layout_direction("form", Locale::new("zh", "CN", "")), "ltr")
  assert_eq(get_layout_direction("form", Locale::new("he", "IL", "")), "rtl")
  
  // Test margin/padding direction
  let get_margin_direction = fn(position : String, locale : Locale) -> String {
    let is_rtl = is_rtl_layout(locale)
    
    match (position, is_rtl) {
      ("left", true) => "right",
      ("left", false) => "left",
      ("right", true) => "left",
      ("right", false) => "right",
      _ => position
    }
  }
  
  // Test margin/padding direction
  assert_eq(get_margin_direction("left", Locale::new("en", "US", "")), "left")
  assert_eq(get_margin_direction("left", Locale::new("ar", "SA", "")), "right")
  assert_eq(get_margin_direction("right", Locale::new("en", "US", "")), "right")
  assert_eq(get_margin_direction("right", Locale::new("ar", "SA", "")), "left")
  
  // Test text alignment
  let get_text_alignment = fn(alignment_type : String, locale : Locale) -> String {
    let is_rtl = is_rtl_layout(locale)
    
    match (alignment_type, is_rtl) {
      ("start", true) => "right",
      ("start", false) => "left",
      ("end", true) => "left",
      ("end", false) => "right",
      ("center", _) => "center",
      _ => if is_rtl { "right" } else { "left" }
    }
  }
  
  // Test text alignment
  assert_eq(get_text_alignment("start", Locale::new("en", "US", "")), "left")
  assert_eq(get_text_alignment("start", Locale::new("ar", "SA", "")), "right")
  assert_eq(get_text_alignment("end", Locale::new("en", "US", "")), "right")
  assert_eq(get_text_alignment("end", Locale::new("ar", "SA", "")), "left")
  assert_eq(get_text_alignment("center", Locale::new("en", "US", "")), "center")
  assert_eq(get_text_alignment("center", Locale::new("ar", "SA", "")), "center")
  
  // Test icon direction
  let get_icon_direction = fn(icon_type : String, locale : Locale) -> String {
    let is_rtl = is_rtl_layout(locale)
    
    match (icon_type, is_rtl) {
      ("arrow", true) => "left", // Arrows point in the opposite direction in RTL
      ("arrow", false) => "right",
      ("chevron", true) => "left", // Chevrons point in the opposite direction in RTL
      ("chevron", false) => "right",
      ("back", true) => "forward", // Back button becomes forward in RTL
      ("back", false) => "back",
      ("next", true) => "previous", // Next button becomes previous in RTL
      ("next", false) => "next",
      _ => icon_type // Other icons remain the same
    }
  }
  
  // Test icon direction
  assert_eq(get_icon_direction("arrow", Locale::new("en", "US", "")), "right")
  assert_eq(get_icon_direction("arrow", Locale::new("ar", "SA", "")), "left")
  assert_eq(get_icon_direction("back", Locale::new("en", "US", "")), "back")
  assert_eq(get_icon_direction("back", Locale::new("ar", "SA", "")), "forward")
  
  // Test telemetry with RTL layout information
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "rtl_meter")
  
  let counter = Meter::create_counter(meter, "rtl_counter", Some("RTL counter"), Some("operations"))
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("layout.ar", StringValue(if is_rtl_layout(Locale::new("ar", "SA", "")) { "rtl" } else { "ltr" })),
    ("layout.en", StringValue(if is_rtl_layout(Locale::new("en", "US", "")) { "rtl" } else { "ltr" })),
    ("text.direction.ar", StringValue(get_layout_direction("text", Locale::new("ar", "SA", "")))),
    ("text.direction.en", StringValue(get_layout_direction("text", Locale::new("en", "US", "")))),
    ("margin.left.ar", StringValue(get_margin_direction("left", Locale::new("ar", "SA", "")))),
    ("margin.left.en", StringValue(get_margin_direction("left", Locale::new("en", "US", "")))),
    ("alignment.start.ar", StringValue(get_text_alignment("start", Locale::new("ar", "SA", "")))),
    ("alignment.start.en", StringValue(get_text_alignment("start", Locale::new("en", "US", "")))),
    ("icon.arrow.ar", StringValue(get_icon_direction("arrow", Locale::new("ar", "SA", "")))),
    ("icon.arrow.en", StringValue(get_icon_direction("arrow", Locale::new("en", "US", ""))))
  ])))
  
  // Verify counter works with RTL layout information
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "rtl_counter")
}