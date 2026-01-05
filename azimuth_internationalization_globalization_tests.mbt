// Azimuth Telemetry System - Internationalization and Globalization Tests
// This file contains comprehensive test cases for internationalization and globalization

// Test 1: Locale Management
test "locale management for telemetry" {
  type Locale = {
    language: String,
    region: String,
    script: Option[String>
  }
  
  let locale_new = fn(language: String, region: String) -> Locale {
    { language, region, script: None }
  }
  
  let locale_with_script = fn(language: String, region: String, script: String) -> Locale {
    { language, region, script: Some(script) }
  }
  
  let locale_to_string = fn(locale: Locale) -> String {
    match locale.script {
      Some(script) => locale.language + "-" + script + "-" + locale.region
      None => locale.language + "-" + locale.region
    }
  }
  
  let locale_parse = fn(locale_string: String) -> Locale {
    let parts = locale_string.split("-")
    
    if parts.length() == 2 {
      locale_new(parts[0], parts[1])
    } else if parts.length() == 3 {
      locale_with_script(parts[0], parts[2], parts[1])
    } else {
      locale_new("en", "US")  // Default locale
    }
  }
  
  // Test locale creation and conversion
  let en_us = locale_new("en", "US")
  assert_eq(locale_to_string(en_us), "en-US")
  
  let zh_cn = locale_new("zh", "CN")
  assert_eq(locale_to_string(zh_cn), "zh-CN")
  
  let zh_hans_cn = locale_with_script("zh", "CN", "Hans")
  assert_eq(locale_to_string(zh_hans_cn), "zh-Hans-CN")
  
  // Test locale parsing
  let parsed1 = locale_parse("en-US")
  assert_eq(parsed1.language, "en")
  assert_eq(parsed1.region, "US")
  
  let parsed2 = locale_parse("zh-Hans-CN")
  assert_eq(parsed2.language, "zh")
  assert_eq(parsed2.region, "CN")
  match parsed2.script {
    Some(script) => assert_eq(script, "Hans")
    None => assert_true(false)
  }
  
  // Test default locale
  let default = locale_parse("invalid")
  assert_eq(default.language, "en")
  assert_eq(default.region, "US")
}

// Test 2: Message Formatting
test "message formatting with locale" {
  type MessageFormatter = {
    locale: Locale,
    messages: Array[(String, String)>
  }
  
  let message_formatter_new = fn(locale: Locale) -> MessageFormatter {
    { locale, messages: [] }
  }
  
  let message_formatter_add = fn(formatter: MessageFormatter, key: String, message: String) -> MessageFormatter {
    {
      locale: formatter.locale,
      messages: formatter.messages + [(key, message)]
    }
  }
  
  let message_formatter_format = fn(formatter: MessageFormatter, key: String, args: Array[String>) -> String {
    let mut message = ""
    
    // Find the message
    for (k, msg) in formatter.messages {
      if k == key {
        message = msg
        break
      }
    }
    
    // Replace placeholders
    let mut result = message
    for i in 0..=args.length()-1 {
      let placeholder = "{" + i.to_string() + "}"
      result = result.replace(placeholder, args[i])
    }
    
    result
  }
  
  // Test message formatting
  let en_formatter = message_formatter_new(locale_new("en", "US"))
  let en_formatter1 = message_formatter_add(en_formatter, "welcome", "Welcome to {0}")
  let en_formatter2 = message_formatter_add(en_formatter1, "request_count", "Total requests: {0}")
  
  let zh_formatter = message_formatter_new(locale_new("zh", "CN"))
  let zh_formatter1 = message_formatter_add(zh_formatter, "welcome", "欢迎来到{0}")
  let zh_formatter2 = message_formatter_add(zh_formatter1, "request_count", "总请求数：{0}")
  
  // Test English formatting
  let en_welcome = message_formatter_format(en_formatter2, "welcome", ["Telemetry System"])
  assert_eq(en_welcome, "Welcome to Telemetry System")
  
  let en_count = message_formatter_format(en_formatter2, "request_count", ["42"])
  assert_eq(en_count, "Total requests: 42")
  
  // Test Chinese formatting
  let zh_welcome = message_formatter_format(zh_formatter2, "welcome", ["遥测系统"])
  assert_eq(zh_welcome, "欢迎来到遥测系统")
  
  let zh_count = message_formatter_format(zh_formatter2, "request_count", ["42"])
  assert_eq(zh_count, "总请求数：42")
}

// Test 3: Number Formatting
test "number formatting with locale" {
  type NumberFormat = {
    decimal_separator: String,
    thousands_separator: String,
    grouping_size: Int
  }
  
  let number_format_for_locale = fn(locale: Locale) -> NumberFormat {
    match locale.language {
      "en" => { decimal_separator: ".", thousands_separator: ",", grouping_size: 3 }
      "de" => { decimal_separator: ",", thousands_separator: ".", grouping_size: 3 }
      "fr" => { decimal_separator: ",", thousands_separator: " ", grouping_size: 3 }
      "zh" => { decimal_separator: ".", thousands_separator: ",", grouping_size: 4 }
      _ => { decimal_separator: ".", thousands_separator: ",", grouping_size: 3 }
    }
  }
  
  let format_int = fn(value: Int, format: NumberFormat) -> String {
    let s = value.to_string()
    let mut result = ""
    let mut count = 0
    
    for i in 0..=s.length()-1 {
      let char = s[s.length()-1-i]
      result = char.to_string() + result
      count = count + 1
      
      if count == format.grouping_size && i < s.length()-1 {
        result = format.thousands_separator + result
        count = 0
      }
    }
    
    result
  }
  
  let format_float = fn(value: Float, decimal_places: Int, format: NumberFormat) -> String {
    // Simplified float formatting
    let int_part = value.to_int()
    let decimal_part = ((value - Float::from_int(int_part)) * Float::from_int(10 ^ decimal_places)).to_int()
    
    let int_str = format_int(int_part, format)
    let decimal_str = decimal_part.to_string()
    
    // Pad decimal part if needed
    let mut padded_decimal = decimal_str
    for i in 0..=decimal_places-1-decimal_str.length() {
      padded_decimal = padded_decimal + "0"
    }
    
    int_str + format.decimal_separator + padded_decimal
  }
  
  // Test number formatting
  let en_format = number_format_for_locale(locale_new("en", "US"))
  let de_format = number_format_for_locale(locale_new("de", "DE"))
  let zh_format = number_format_for_locale(locale_new("zh", "CN"))
  
  // Test integer formatting
  let en_int = format_int(1234567, en_format)
  assert_eq(en_int, "1,234,567")
  
  let de_int = format_int(1234567, de_format)
  assert_eq(de_int, "1.234.567")
  
  let zh_int = format_int(12345678, zh_format)
  assert_eq(zh_int, "1234,5678")
  
  // Test float formatting
  let en_float = format_float(1234.567, 2, en_format)
  assert_eq(en_float, "1,234.57")
  
  let de_float = format_float(1234.567, 2, de_format)
  assert_eq(de_float, "1.234,57")
  
  let zh_float = format_float(1234.567, 2, zh_format)
  assert_eq(zh_float, "1,234.57")
}

// Test 4: Date and Time Formatting
test "date and time formatting with locale" {
  type DateTimeFormat = {
    date_format: String,
    time_format: String,
    date_time_format: String,
    timezone: String
  }
  
  let date_time_format_for_locale = fn(locale: Locale) -> DateTimeFormat {
    match (locale.language, locale.region) {
      ("en", "US") => {
        {
          date_format: "MM/DD/YYYY",
          time_format: "h:mm:ss A",
          date_time_format: "MM/DD/YYYY h:mm:ss A",
          timezone: "UTC"
        }
      }
      ("en", "GB") => {
        {
          date_format: "DD/MM/YYYY",
          time_format: "HH:mm:ss",
          date_time_format: "DD/MM/YYYY HH:mm:ss",
          timezone: "UTC"
        }
      }
      ("zh", "CN") => {
        {
          date_format: "YYYY年MM月DD日",
          time_format: "HH时mm分ss秒",
          date_time_format: "YYYY年MM月DD日 HH时mm分ss秒",
          timezone: "UTC"
        }
      }
      ("de", "DE") => {
        {
          date_format: "DD.MM.YYYY",
          time_format: "HH:mm:ss",
          date_time_format: "DD.MM.YYYY HH:mm:ss",
          timezone: "UTC"
        }
      }
      _ => {
        {
          date_format: "YYYY-MM-DD",
          time_format: "HH:mm:ss",
          date_time_format: "YYYY-MM-DD HH:mm:ss",
          timezone: "UTC"
        }
      }
    }
  }
  
  let format_date_time = fn(year: Int, month: Int, day: Int, hour: Int, minute: Int, second: Int, format: DateTimeFormat) -> String {
    let date_str = format.date_format
      .replace("YYYY", year.to_string())
      .replace("MM", if month < 10 { "0" + month.to_string() } else { month.to_string() })
      .replace("DD", if day < 10 { "0" + day.to_string() } else { day.to_string() })
    
    let time_str = format.time_format
      .replace("HH", if hour < 10 { "0" + hour.to_string() } else { hour.to_string() })
      .replace("mm", if minute < 10 { "0" + minute.to_string() } else { minute.to_string() })
      .replace("ss", if second < 10 { "0" + second.to_string() } else { second.to_string() })
      .replace("h", if hour > 12 { (hour - 12).to_string() } else { hour.to_string() })
      .replace("A", if hour >= 12 { "PM" } else { "AM" })
    
    format.date_time_format
      .replace("YYYY", year.to_string())
      .replace("MM", if month < 10 { "0" + month.to_string() } else { month.to_string() })
      .replace("DD", if day < 10 { "0" + day.to_string() } else { day.to_string() })
      .replace("HH", if hour < 10 { "0" + hour.to_string() } else { hour.to_string() })
      .replace("mm", if minute < 10 { "0" + minute.to_string() } else { minute.to_string() })
      .replace("ss", if second < 10 { "0" + second.to_string() } else { second.to_string() })
      .replace("h", if hour > 12 { (hour - 12).to_string() } else { hour.to_string() })
      .replace("A", if hour >= 12 { "PM" } else { "AM" })
  }
  
  // Test date/time formatting
  let en_us_format = date_time_format_for_locale(locale_new("en", "US"))
  let en_gb_format = date_time_format_for_locale(locale_new("en", "GB"))
  let zh_cn_format = date_time_format_for_locale(locale_new("zh", "CN"))
  let de_de_format = date_time_format_for_locale(locale_new("de", "DE"))
  
  // Test formatting
  let en_us_dt = format_date_time(2023, 1, 15, 14, 30, 45, en_us_format)
  assert_eq(en_us_dt, "01/15/2023 2:30:45 PM")
  
  let en_gb_dt = format_date_time(2023, 1, 15, 14, 30, 45, en_gb_format)
  assert_eq(en_gb_dt, "15/01/2023 14:30:45")
  
  let zh_cn_dt = format_date_time(2023, 1, 15, 14, 30, 45, zh_cn_format)
  assert_eq(zh_cn_dt, "2023年01月15日 14时30分45秒")
  
  let de_de_dt = format_date_time(2023, 1, 15, 14, 30, 45, de_de_format)
  assert_eq(de_de_dt, "15.01.2023 14:30:45")
}

// Test 5: Currency Formatting
test "currency formatting with locale" {
  type CurrencyFormat = {
    symbol: String,
    symbol_position: String,  // "before" or "after"
    decimal_places: Int,
    decimal_separator: String,
    thousands_separator: String
  }
  
  let currency_format_for_locale = fn(locale: Locale, currency_code: String) -> CurrencyFormat {
    match (locale.language, locale.region, currency_code) {
      ("en", "US", "USD") => {
        {
          symbol: "$",
          symbol_position: "before",
          decimal_places: 2,
          decimal_separator: ".",
          thousands_separator: ","
        }
      }
      ("en", "GB", "GBP") => {
        {
          symbol: "£",
          symbol_position: "before",
          decimal_places: 2,
          decimal_separator: ".",
          thousands_separator: ","
        }
      }
      ("de", "DE", "EUR") => {
        {
          symbol: "€",
          symbol_position: "after",
          decimal_places: 2,
          decimal_separator: ",",
          thousands_separator: "."
        }
      }
      ("zh", "CN", "CNY") => {
        {
          symbol: "¥",
          symbol_position: "before",
          decimal_places: 2,
          decimal_separator: ".",
          thousands_separator: ","
        }
      }
      ("ja", "JP", "JPY") => {
        {
          symbol: "¥",
          symbol_position: "before",
          decimal_places: 0,
          decimal_separator: ".",
          thousands_separator: ","
        }
      }
      _ => {
        {
          symbol: "$",
          symbol_position: "before",
          decimal_places: 2,
          decimal_separator: ".",
          thousands_separator: ","
        }
      }
    }
  }
  
  let format_currency = fn(value: Float, format: CurrencyFormat) -> String {
    // Format the number part
    let int_part = value.to_int()
    let decimal_part = ((value - Float::from_int(int_part)) * Float::from_int(10 ^ format.decimal_places)).to_int()
    
    // Format integer part with thousands separator
    let int_str = int_part.to_string()
    let mut formatted_int = ""
    let mut count = 0
    
    for i in 0..=int_str.length()-1 {
      let char = int_str[int_str.length()-1-i]
      formatted_int = char.to_string() + formatted_int
      count = count + 1
      
      if count == 3 && i < int_str.length()-1 {
        formatted_int = format.thousands_separator + formatted_int
        count = 0
      }
    }
    
    // Format decimal part
    let decimal_str = decimal_part.to_string()
    let mut padded_decimal = decimal_str
    for i in 0..=format.decimal_places-1-decimal_str.length() {
      padded_decimal = padded_decimal + "0"
    }
    
    let number_str = if format.decimal_places > 0 {
      formatted_int + format.decimal_separator + padded_decimal
    } else {
      formatted_int
    }
    
    // Add currency symbol
    match format.symbol_position {
      "before" => format.symbol + number_str
      "after" => number_str + " " + format.symbol
      _ => number_str
    }
  }
  
  // Test currency formatting
  let usd_format = currency_format_for_locale(locale_new("en", "US"), "USD")
  let gbp_format = currency_format_for_locale(locale_new("en", "GB"), "GBP")
  let eur_format = currency_format_for_locale(locale_new("de", "DE"), "EUR")
  let cny_format = currency_format_for_locale(locale_new("zh", "CN"), "CNY")
  let jpy_format = currency_format_for_locale(locale_new("ja", "JP"), "JPY")
  
  // Test formatting
  let usd_value = format_currency(1234.56, usd_format)
  assert_eq(usd_value, "$1,234.56")
  
  let gbp_value = format_currency(1234.56, gbp_format)
  assert_eq(gbp_value, "£1,234.56")
  
  let eur_value = format_currency(1234.56, eur_format)
  assert_eq(eur_value, "1.234,56 €")
  
  let cny_value = format_currency(1234.56, cny_format)
  assert_eq(cny_value, "¥1,234.56")
  
  let jpy_value = format_currency(1234.0, jpy_format)
  assert_eq(jpy_value, "¥1,234")
}

// Test 6: Text Direction and RTL Support
test "text direction and RTL support" {
  type TextDirection = {
    direction: String,  // "ltr" or "rtl"
    alignment: String   // "left", "right", or "auto"
  }
  
  let text_direction_for_locale = fn(locale: Locale) -> TextDirection {
    match locale.language {
      "ar" => { direction: "rtl", alignment: "right" }
      "he" => { direction: "rtl", alignment: "right" }
      "fa" => { direction: "rtl", alignment: "right" }
      "ur" => { direction: "rtl", alignment: "right" }
      _ => { direction: "ltr", alignment: "left" }
    }
  }
  
  let apply_text_direction = fn(text: String, direction: TextDirection) -> String {
    match direction.direction {
      "rtl" => "\u202B" + text + "\u202C"  // RLE + PDF
      "ltr" => "\u202A" + text + "\u202C"   // LRE + PDF
      _ => text
    }
  }
  
  let is_rtl_text = fn(text: String) -> Bool {
    // Check if text contains RTL characters
    for char in text.to_char_array() {
      let code = char.to_int()
      if (code >= 0x0590 && code <= 0x05FF) ||  // Hebrew
         (code >= 0x0600 && code <= 0x06FF) ||  // Arabic
         (code >= 0x0750 && code <= 0x077F) ||  // Arabic Supplement
         (code >= 0xFB50 && code <= 0xFDFF) {   // Arabic Presentation Forms-A
        return true
      }
    }
    false
  }
  
  // Test text direction
  let en_direction = text_direction_for_locale(locale_new("en", "US"))
  assert_eq(en_direction.direction, "ltr")
  assert_eq(en_direction.alignment, "left")
  
  let ar_direction = text_direction_for_locale(locale_new("ar", "SA"))
  assert_eq(ar_direction.direction, "rtl")
  assert_eq(ar_direction.alignment, "right")
  
  // Test applying text direction
  let en_text = apply_text_direction("Hello World", en_direction)
  assert_eq(en_text, "\u202AHello World\u202C")
  
  let ar_text = apply_text_direction("مرحبا بالعالم", ar_direction)
  assert_eq(ar_text, "\u202Bمرحبا بالعالم\u202C")
  
  // Test RTL detection
  assert_false(is_rtl_text("Hello World"))
  assert_true(is_rtl_text("مرحبا بالعالم"))
  assert_true(is_rtl_text("שלום עולם"))
  assert_false(is_rtl_text("Bonjour le monde"))
}

// Test 7: Collation and String Comparison
test "collation and string comparison with locale" {
  type CollationRules = {
    case_sensitive: Bool,
    ignore_accents: Bool,
    numeric_sorting: Bool
  }
  
  let collation_rules_for_locale = fn(locale: Locale) -> CollationRules {
    match locale.language {
      "en" => { case_sensitive: true, ignore_accents: false, numeric_sorting: false }
      "de" => { case_sensitive: false, ignore_accents: true, numeric_sorting: true }
      "fr" => { case_sensitive: true, ignore_accents: false, numeric_sorting: false }
      "zh" => { case_sensitive: true, ignore_accents: false, numeric_sorting: true }
      _ => { case_sensitive: true, ignore_accents: false, numeric_sorting: false }
    }
  }
  
  let normalize_string = fn(s: String, rules: CollationRules) -> String {
    let mut result = s
    
    if !rules.case_sensitive {
      result = result.to_lowercase()
    }
    
    if rules.ignore_accents {
      // Simple accent removal (simplified)
      result = result
        .replace("á", "a")
        .replace("à", "a")
        .replace("ä", "a")
        .replace("â", "a")
        .replace("ã", "a")
        .replace("å", "a")
        .replace("é", "e")
        .replace("è", "e")
        .replace("ë", "e")
        .replace("ê", "e")
        .replace("í", "i")
        .replace("ì", "i")
        .replace("ï", "i")
        .replace("î", "i")
        .replace("ó", "o")
        .replace("ò", "o")
        .replace("ö", "o")
        .replace("ô", "o")
        .replace("õ", "o")
        .replace("ø", "o")
        .replace("ú", "u")
        .replace("ù", "u")
        .replace("ü", "u")
        .replace("û", "u")
        .replace("ý", "y")
        .replace("ÿ", "y")
        .replace("ñ", "n")
        .replace("ç", "c")
    }
    
    result
  }
  
  let compare_strings = fn(s1: String, s2: String, rules: CollationRules) -> Int {
    let norm1 = normalize_string(s1, rules)
    let norm2 = normalize_string(s2, rules)
    
    if norm1 < norm2 {
      -1
    } else if norm1 > norm2 {
      1
    } else {
      0
    }
  }
  
  // Test collation
  let en_rules = collation_rules_for_locale(locale_new("en", "US"))
  let de_rules = collation_rules_for_locale(locale_new("de", "DE"))
  
  // Test case sensitivity
  let case_result = compare_strings("Apple", "apple", en_rules)
  assert_eq(case_result, 1)  // "Apple" > "apple" (case-sensitive)
  
  let de_case_result = compare_strings("Apple", "apple", de_rules)
  assert_eq(de_case_result, 0)  // "Apple" == "apple" (case-insensitive)
  
  // Test accent handling
  let accent_result = compare_strings("café", "cafe", en_rules)
  assert_eq(accent_result, 1)  // "café" > "cafe" (accents matter)
  
  let de_accent_result = compare_strings("café", "cafe", de_rules)
  assert_eq(de_accent_result, 0)  // "café" == "cafe" (ignore accents)
}

// Test 8: Pluralization Rules
test "pluralization rules for different languages" {
  enum PluralCategory {
    Zero
    One
    Two
    Few
    Many
    Other
  }
  
  let plural_category = fn(value: Int, locale: Locale) -> PluralCategory {
    match locale.language {
      "en" => {
        if value == 0 {
          Zero
        } else if value == 1 {
          One
        } else {
          Other
        }
      }
      "zh" => {
        // Chinese has no plural forms
        Other
      }
      "ar" => {
        // Arabic has complex plural rules
        let abs_value = if value < 0 { -value } else { value }
        let mod100 = abs_value % 100
        
        if value == 0 {
          Zero
        } else if value == 1 {
          One
        } else if value == 2 {
          Two
        } else if mod100 >= 3 && mod100 <= 10 {
          Few
        } else if mod100 >= 11 && mod100 <= 99 {
          Many
        } else {
          Other
        }
      }
      "ru" => {
        // Russian has three plural forms
        let mod10 = value % 10
        let mod100 = value % 100
        
        if mod10 == 1 && mod100 != 11 {
          One
        } else if mod10 >= 2 && mod10 <= 4 && (mod100 < 10 || mod100 >= 20) {
          Few
        } else {
          Many
        }
      }
      _ => {
        if value == 1 {
          One
        } else {
          Other
        }
      }
    }
  }
  
  let pluralize = fn(value: Int, locale: Locale, forms: Array[String>) -> String {
    let category = plural_category(value, locale)
    
    match category {
      Zero => if forms.length() > 0 { forms[0] } else { "" }
      One => if forms.length() > 1 { forms[1] } else { forms[0] }
      Two => if forms.length() > 2 { forms[2] } else { forms[1] }
      Few => if forms.length() > 3 { forms[3] } else { forms[1] }
      Many => if forms.length() > 4 { forms[4] } else { forms[1] }
      Other => if forms.length() > 5 { forms[5] } else { if forms.length() > 1 { forms[1] } else { forms[0] } }
    }
  }
  
  // Test pluralization
  let en_locale = locale_new("en", "US")
  let zh_locale = locale_new("zh", "CN")
  let ar_locale = locale_new("ar", "SA")
  let ru_locale = locale_new("ru", "RU")
  
  // English: "file", "files"
  let en_forms = ["files", "file"]
  assert_eq(pluralize(0, en_locale, en_forms), "files")
  assert_eq(pluralize(1, en_locale, en_forms), "file")
  assert_eq(pluralize(2, en_locale, en_forms), "files")
  assert_eq(pluralize(5, en_locale, en_forms), "files")
  
  // Chinese: only one form
  let zh_forms = ["个文件"]
  assert_eq(pluralize(0, zh_locale, zh_forms), "个文件")
  assert_eq(pluralize(1, zh_locale, zh_forms), "个文件")
  assert_eq(pluralize(2, zh_locale, zh_forms), "个文件")
  assert_eq(pluralize(5, zh_locale, zh_forms), "个文件")
  
  // Arabic: complex forms
  let ar_forms = ["ملفات", "ملف", "ملفان", "ملفات", "ملفات"]
  assert_eq(pluralize(0, ar_locale, ar_forms), "ملفات")  // Zero
  assert_eq(pluralize(1, ar_locale, ar_forms), "ملف")    // One
  assert_eq(pluralize(2, ar_locale, ar_forms), "ملفان")  // Two
  assert_eq(pluralize(5, ar_locale, ar_forms), "ملفات")  // Few
  assert_eq(pluralize(15, ar_locale, ar_forms), "ملفات") // Many
  
  // Russian: three forms
  let ru_forms = ["файлов", "файл", "файла", "файла", "файлов"]
  assert_eq(pluralize(1, ru_locale, ru_forms), "файл")    // One
  assert_eq(pluralize(2, ru_locale, ru_forms), "файла")   // Few
  assert_eq(pluralize(5, ru_locale, ru_forms), "файлов")  // Many
  assert_eq(pluralize(11, ru_locale, ru_forms), "файлов") // Many
}

// Test 9: Timezone Handling
test "timezone handling for international telemetry" {
  type TimezoneInfo = {
    name: String,
    offset_hours: Int,
    offset_minutes: Int,
    dst_offset_hours: Int
  }
  
  let timezone_for_region = fn(region: String) -> TimezoneInfo {
    match region {
      "US" => { name: "America/New_York", offset_hours: -5, offset_minutes: 0, dst_offset_hours: 1 }
      "GB" => { name: "Europe/London", offset_hours: 0, offset_minutes: 0, dst_offset_hours: 1 }
      "CN" => { name: "Asia/Shanghai", offset_hours: 8, offset_minutes: 0, dst_offset_hours: 0 }
      "JP" => { name: "Asia/Tokyo", offset_hours: 9, offset_minutes: 0, dst_offset_hours: 0 }
      "DE" => { name: "Europe/Berlin", offset_hours: 1, offset_minutes: 0, dst_offset_hours: 1 }
      _ => { name: "UTC", offset_hours: 0, offset_minutes: 0, dst_offset_hours: 0 }
    }
  }
  
  let convert_to_timezone = fn(utc_hour: Int, utc_minute: Int, timezone: TimezoneInfo, is_dst: Bool) -> (Int, Int) {
    let total_minutes = utc_hour * 60 + utc_minute
    let offset_total = timezone.offset_hours * 60 + timezone.offset_minutes
    
    let dst_offset = if is_dst { timezone.dst_offset_hours * 60 } else { 0 }
    
    let local_total = total_minutes + offset_total + dst_offset
    
    // Normalize to 0-23:59
    let normalized = if local_total < 0 {
      local_total + 24 * 60
    } else if local_total >= 24 * 60 {
      local_total - 24 * 60
    } else {
      local_total
    }
    
    (normalized / 60, normalized % 60)
  }
  
  // Test timezone conversion
  let us_tz = timezone_for_region("US")
  let gb_tz = timezone_for_region("GB")
  let cn_tz = timezone_for_region("CN")
  let jp_tz = timezone_for_region("JP")
  let de_tz = timezone_for_region("DE")
  
  // Convert 12:00 UTC to different timezones
  let (us_hour, us_minute) = convert_to_timezone(12, 0, us_tz, false)
  assert_eq(us_hour, 7)  // 12:00 UTC - 5 hours = 7:00 EST
  
  let (gb_hour, gb_minute) = convert_to_timezone(12, 0, gb_tz, false)
  assert_eq(gb_hour, 12)  // 12:00 UTC + 0 hours = 12:00 GMT
  
  let (cn_hour, cn_minute) = convert_to_timezone(12, 0, cn_tz, false)
  assert_eq(cn_hour, 20)  // 12:00 UTC + 8 hours = 20:00 CST
  
  let (jp_hour, jp_minute) = convert_to_timezone(12, 0, jp_tz, false)
  assert_eq(jp_hour, 21)  // 12:00 UTC + 9 hours = 21:00 JST
  
  let (de_hour, de_minute) = convert_to_timezone(12, 0, de_tz, false)
  assert_eq(de_hour, 13)  // 12:00 UTC + 1 hour = 13:00 CET
  
  // Test DST
  let (us_dst_hour, us_dst_minute) = convert_to_timezone(12, 0, us_tz, true)
  assert_eq(us_dst_hour, 8)  // 12:00 UTC - 5 hours + 1 hour DST = 8:00 EDT
}

// Test 10: Resource Bundle Management
test "resource bundle management for i18n" {
  type ResourceBundle = {
    locale: Locale,
    resources: Array[(String, String)>
  }
  
  let resource_bundle_new = fn(locale: Locale) -> ResourceBundle {
    { locale, resources: [] }
  }
  
  let resource_bundle_add = fn(bundle: ResourceBundle, key: String, value: String) -> ResourceBundle {
    {
      locale: bundle.locale,
      resources: bundle.resources + [(key, value)]
    }
  }
  
  let resource_bundle_get = fn(bundle: ResourceBundle, key: String) -> Option[String] {
    for (k, v) in bundle.resources {
      if k == key {
        return Some(v)
      }
    }
    None
  }
  
  let resource_bundle_merge = fn(primary: ResourceBundle, fallback: ResourceBundle) -> ResourceBundle {
    let mut merged_resources = primary.resources
    
    // Add fallback resources for keys not in primary
    for (key, value) in fallback.resources {
      let mut found = false
      for (k, _) in primary.resources {
        if k == key {
          found = true
          break
        }
      }
      
      if !found {
        merged_resources = merged_resources + [(key, value)]
      }
    }
    
    { locale: primary.locale, resources: merged_resources }
  }
  
  // Test resource bundle
  let en_bundle = resource_bundle_new(locale_new("en", "US"))
  let en_bundle1 = resource_bundle_add(en_bundle, "welcome", "Welcome")
  let en_bundle2 = resource_bundle_add(en_bundle1, "goodbye", "Goodbye")
  
  let zh_bundle = resource_bundle_new(locale_new("zh", "CN"))
  let zh_bundle1 = resource_bundle_add(zh_bundle, "welcome", "欢迎")
  let zh_bundle2 = resource_bundle_add(zh_bundle1, "thank_you", "谢谢")
  
  // Test getting resources
  match resource_bundle_get(en_bundle2, "welcome") {
    Some(value) => assert_eq(value, "Welcome")
    None => assert_true(false)
  }
  
  match resource_bundle_get(zh_bundle2, "goodbye") {
    Some(_) => assert_true(false)  // Not in Chinese bundle
    None => assert_true(true)
  }
  
  // Test resource bundle merging
  let merged_bundle = resource_bundle_merge(zh_bundle2, en_bundle2)
  
  match resource_bundle_get(merged_bundle, "welcome") {
    Some(value) => assert_eq(value, "欢迎")  // From Chinese bundle
    None => assert_true(false)
  }
  
  match resource_bundle_get(merged_bundle, "goodbye") {
    Some(value) => assert_eq(value, "Goodbye")  // From English bundle
    None => assert_true(false)
  }
  
  match resource_bundle_get(merged_bundle, "thank_you") {
    Some(value) => assert_eq(value, "谢谢")  // From Chinese bundle
    None => assert_true(false)
  }
}