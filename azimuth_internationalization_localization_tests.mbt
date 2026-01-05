// Azimuth Internationalization and Localization Test Suite
// This file contains tests for internationalization (i18n) and localization (l10n) features

// Test 1: Locale Detection and Management
test "locale detection and management" {
  // Locale structure
  type Locale = {
    language: String,
    region: String,
    script: Option[String],
    variant: Option[String]
  }
  
  // Locale parsing result
  type LocaleParseResult = {
    success: Bool,
    locale: Option[Locale],
    error: Option[String]
  }
  
  // Parse locale string
  let parse_locale = fn(locale_str: String) {
    if locale_str.length() == 0 {
      return {
        success: false,
        locale: None,
        error: Some("Empty locale string")
      }
    }
    
    let parts = locale_str.split("-")
    
    if parts.length() == 0 {
      return {
        success: false,
        locale: None,
        error: Some("Invalid locale format")
      }
    }
    
    let language = parts[0]
    if language.length() != 2 {
      return {
        success: false,
        locale: None,
        error: Some("Invalid language code")
      }
    }
    
    let region = if parts.length() > 1 { parts[1] } else { "" }
    let script = if parts.length() > 2 { Some(parts[2]) } else { None }
    let variant = if parts.length() > 3 { Some(parts[3]) } else { None }
    
    {
      success: true,
      locale: Some({
        language,
        region,
        script,
        variant
      }),
      error: None
    }
  }
  
  // Format locale to string
  let format_locale = fn(locale: Locale) {
    let mut result = locale.language
    
    if locale.region != "" {
      result = result + "-" + locale.region
    }
    
    match locale.script {
      Some(script) => result = result + "-" + script
      None => {}
    }
    
    match locale.variant {
      Some(variant) => result = result + "-" + variant
      None => {}
    }
    
    result
  }
  
  // Check if locale matches pattern
  let matches_locale = fn(locale: Locale, language: String, region: String) {
    locale.language == language and (region == "" or locale.region == region)
  }
  
  // Test valid locale parsing
  let en_us_result = parse_locale("en-US")
  assert_true(en_us_result.success)
  assert_true(en_us_result.locale.is_some())
  
  let en_us = en_us_result.locale.unwrap()
  assert_eq(en_us.language, "en")
  assert_eq(en_us.region, "US")
  assert_eq(en_us.script, None)
  assert_eq(en_us.variant, None)
  
  // Test locale with script
  let zh_hans_cn_result = parse_locale("zh-Hans-CN")
  assert_true(zh_hans_cn_result.success)
  
  let zh_hans_cn = zh_hans_cn_result.locale.unwrap()
  assert_eq(zh_hans_cn.language, "zh")
  assert_eq(zh_hans_cn.region, "CN")
  assert_eq(zh_hans_cn.script, Some("Hans"))
  assert_eq(zh_hans_cn.variant, None)
  
  // Test locale with variant
  let de_de_variant_result = parse_locale("de-DE-1996")
  assert_true(de_de_variant_result.success)
  
  let de_de_variant = de_de_variant_result.unwrap()
  assert_eq(de_de_variant.language, "de")
  assert_eq(de_de_variant.region, "DE")
  assert_eq(de_de_variant.script, None)
  assert_eq(de_de_variant.variant, Some("1996"))
  
  // Test language only
  let fr_result = parse_locale("fr")
  assert_true(fr_result.success)
  
  let fr = fr_result.unwrap()
  assert_eq(fr.language, "fr")
  assert_eq(fr.region, "")
  
  // Test invalid locale
  let invalid_result = parse_locale("invalid-locale-format")
  assert_false(invalid_result.success)
  assert_true(invalid_result.error.is_some())
  
  let empty_result = parse_locale("")
  assert_false(empty_result.success)
  assert_true(empty_result.error.is_some())
  
  // Test locale formatting
  let formatted_en_us = format_locale(en_us)
  assert_eq(formatted_en_us, "en-US")
  
  let formatted_zh_hans_cn = format_locale(zh_hans_cn)
  assert_eq(formatted_zh_hans_cn, "zh-Hans-CN")
  
  let formatted_de_de_variant = format_locale(de_de_variant)
  assert_eq(formatted_de_de_variant, "de-DE-1996")
  
  // Test locale matching
  assert_true(matches_locale(en_us, "en", "US"))
  assert_true(matches_locale(en_us, "en", ""))  // Region not specified
  assert_false(matches_locale(en_us, "en", "GB"))  // Different region
  assert_false(matches_locale(en_us, "fr", "US"))  // Different language
  
  // Test locale array operations
  let locales = [
    parse_locale("en-US").unwrap().locale.unwrap(),
    parse_locale("fr-FR").unwrap().locale.unwrap(),
    parse_locale("zh-CN").unwrap().locale.unwrap(),
    parse_locale("de-DE").unwrap().locale.unwrap(),
    parse_locale("ja-JP").unwrap().locale.unwrap()
  ]
  
  // Find locales by language
  let english_locales = locales.filter(fn(locale) { matches_locale(locale, "en", "") })
  assert_eq(english_locales.length(), 1)
  assert_eq(english_locales[0].language, "en")
  
  // Find locales by region
  let us_locales = locales.filter(fn(locale) { locale.region == "US" })
  assert_eq(us_locales.length(), 1)
  assert_eq(us_locales[0].language, "en")
}

// Test 2: Message Formatting and Pluralization
test "message formatting and pluralization" {
  // Plural rule type
  enum PluralRule {
    Zero
    One
    Two
    Few
    Many
    Other
  }
  
  // Message structure
  type Message = {
    key: String,
    translations: Array[(String, String)]  // (locale, translation)
  }
  
  // Translation context
  type TranslationContext = {
    locale: Locale,
    messages: Array[Message]
  }
  
  // Determine plural rule for English
  let english_plural_rule = fn(count: Int) {
    if count == 0 {
      PluralRule::Zero
    } else if count == 1 {
      PluralRule::One
    } else {
      PluralRule::Other
    }
  }
  
  // Determine plural rule for Chinese (simplified)
  let chinese_plural_rule = fn(count: Int) {
    PluralRule::Other  // Chinese doesn't have plural forms
  }
  
  // Determine plural rule for French
  let french_plural_rule = fn(count: Int) {
    if count == 0 or count == 1 {
      PluralRule::One
    } else {
      PluralRule::Other
    }
  }
  
  // Get plural rule function for locale
  let get_plural_rule_fn = fn(locale: Locale) {
    match locale.language {
      "en" => english_plural_rule
      "zh" => chinese_plural_rule
      "fr" => french_plural_rule
      _ => english_plural_rule  // Default to English
    }
  }
  
  // Format message with pluralization
  let format_message = fn(context: TranslationContext, key: String, count: Int) {
    // Find message by key
    let mut message = None
    for msg in context.messages {
      if msg.key == key {
        message = Some(msg)
        break
      }
    }
    
    match message {
      Some(msg) => {
        // Find translation for locale
        let locale_str = format_locale(context.locale)
        let mut translation = ""
        
        for (locale, text) in msg.translations {
          if locale == locale_str {
            translation = text
            break
          }
        }
        
        // Fallback to English if no translation found
        if translation == "" {
          for (locale, text) in msg.translations {
            if locale.starts_with("en") {
              translation = text
              break
            }
          }
        }
        
        // Handle pluralization
        let plural_rule_fn = get_plural_rule_fn(context.locale)
        let plural_rule = plural_rule_fn(count)
        
        // Simple placeholder replacement
        let result = translation.replace("{count}", count.to_string())
        
        match plural_rule {
          PluralRule::Zero => result.replace("{plural}", "zero")
          PluralRule::One => result.replace("{plural}", "one")
          PluralRule::Two => result.replace("{plural}", "two")
          PluralRule::Few => result.replace("{plural}", "few")
          PluralRule::Many => result.replace("{plural}", "many")
          PluralRule::Other => result.replace("{plural}", "other")
        }
      }
      None => "[Missing key: " + key + "]"
    }
  }
  
  // Create test messages
  let messages = [
    {
      key: "items.count",
      translations: [
        ("en-US", "You have {count} {plural}"),
        ("fr-FR", "Vous avez {count} {plural}"),
        ("zh-CN", "您有{count}个{plural}")
      ]
    },
    {
      key: "welcome.message",
      translations: [
        ("en-US", "Welcome to Azimuth!"),
        ("fr-FR", "Bienvenue à Azimuth!"),
        ("zh-CN", "欢迎使用Azimuth！")
      ]
    }
  ]
  
  // Test English pluralization
  let en_context = {
    locale: parse_locale("en-US").unwrap().locale.unwrap(),
    messages
  }
  
  assert_eq(format_message(en_context, "items.count", 0), "You have 0 zero")
  assert_eq(format_message(en_context, "items.count", 1), "You have 1 one")
  assert_eq(format_message(en_context, "items.count", 2), "You have 2 other")
  assert_eq(format_message(en_context, "items.count", 10), "You have 10 other")
  
  // Test French pluralization
  let fr_context = {
    locale: parse_locale("fr-FR").unwrap().locale.unwrap(),
    messages
  }
  
  assert_eq(format_message(fr_context, "items.count", 0), "Vous avez 0 one")
  assert_eq(format_message(fr_context, "items.count", 1), "Vous avez 1 one")
  assert_eq(format_message(fr_context, "items.count", 2), "Vous avez 2 other")
  assert_eq(format_message(fr_context, "items.count", 10), "Vous avez 10 other")
  
  // Test Chinese pluralization
  let zh_context = {
    locale: parse_locale("zh-CN").unwrap().locale.unwrap(),
    messages
  }
  
  assert_eq(format_message(zh_context, "items.count", 0), "您有0个other")
  assert_eq(format_message(zh_context, "items.count", 1), "您有1个other")
  assert_eq(format_message(zh_context, "items.count", 2), "您有2个other")
  assert_eq(format_message(zh_context, "items.count", 10), "您有10个other")
  
  // Test non-plural message
  assert_eq(format_message(en_context, "welcome.message", 1), "Welcome to Azimuth!")
  assert_eq(format_message(fr_context, "welcome.message", 1), "Bienvenue à Azimuth!")
  assert_eq(format_message(zh_context, "welcome.message", 1), "欢迎使用Azimuth！")
  
  // Test missing key
  assert_eq(format_message(en_context, "missing.key", 1), "[Missing key: missing.key]")
  
  // Test unsupported locale (fallback to English)
  let unsupported_context = {
    locale: parse_locale("xx-XX").unwrap().locale.unwrap(),
    messages
  }
  
  assert_eq(format_message(unsupported_context, "welcome.message", 1), "Welcome to Azimuth!")
}

// Test 3: Date and Time Formatting
test "date and time formatting" {
  // Date structure
  type Date = {
    year: Int,
    month: Int,
    day: Int
  }
  
  // Time structure
  type Time = {
    hour: Int,
    minute: Int,
    second: Int
  }
  
  // DateTime structure
  type DateTime = {
    date: Date,
    time: Time,
    timezone_offset: Int  // Minutes from UTC
  }
  
  // Format date for locale
  let format_date = fn(date: Date, locale: Locale) {
    match locale.language {
      "en" => {
        let month_names = [
          "January", "February", "March", "April", "May", "June",
          "July", "August", "September", "October", "November", "December"
        ]
        month_names[date.month - 1] + " " + date.day.to_string() + ", " + date.year.to_string()
      }
      "fr" => {
        let month_names = [
          "janvier", "février", "mars", "avril", "mai", "juin",
          "juillet", "août", "septembre", "octobre", "novembre", "décembre"
        ]
        date.day.to_string() + " " + month_names[date.month - 1] + " " + date.year.to_string()
      }
      "zh" => {
        date.year.to_string() + "年" + date.month.to_string() + "月" + date.day.to_string() + "日"
      }
      "de" => {
        let month_names = [
          "Januar", "Februar", "März", "April", "Mai", "Juni",
          "Juli", "August", "September", "Oktober", "November", "Dezember"
        ]
        date.day.to_string() + ". " + month_names[date.month - 1] + " " + date.year.to_string()
      }
      _ => {
        date.year.to_string() + "-" + 
        (if date.month < 10 { "0" } else { "" }) + date.month.to_string() + "-" +
        (if date.day < 10 { "0" } else { "" }) + date.day.to_string()
      }
    }
  }
  
  // Format time for locale
  let format_time = fn(time: Time, locale: Locale) {
    match locale.language {
      "en" => {
        let hour = if time.hour == 0 { 12 } else if time.hour > 12 { time.hour - 12 } else { time.hour }
        let period = if time.hour < 12 { "AM" } else { "PM" }
        hour.to_string() + ":" + 
        (if time.minute < 10 { "0" } else { "" }) + time.minute.to_string() + 
        ":" + (if time.second < 10 { "0" } else { "" }) + time.second.to_string() + " " + period
      }
      "fr" | "de" => {
        time.hour.to_string() + ":" + 
        (if time.minute < 10 { "0" } else { "" }) + time.minute.to_string() + 
        ":" + (if time.second < 10 { "0" } else { "" }) + time.second.to_string()
      }
      "zh" => {
        time.hour.to_string() + "时" + 
        time.minute.to_string() + "分" + 
        time.second.to_string() + "秒"
      }
      _ => {
        time.hour.to_string() + ":" + 
        (if time.minute < 10 { "0" } else { "" }) + time.minute.to_string() + 
        ":" + (if time.second < 10 { "0" } else { "" }) + time.second.to_string()
      }
    }
  }
  
  // Format date and time for locale
  let format_datetime = fn(datetime: DateTime, locale: Locale) {
    let date_str = format_date(datetime.date, locale)
    let time_str = format_time(datetime.time, locale)
    
    match locale.language {
      "en" => date_str + " at " + time_str
      "fr" => date_str + " à " + time_str
      "zh" => date_str + " " + time_str
      _ => date_str + " " + time_str
    }
  }
  
  // Create test datetime
  let test_datetime = {
    date: { year: 2023, month: 5, day: 15 },
    time: { hour: 14, minute: 30, second: 45 },
    timezone_offset: 0
  }
  
  // Test English formatting
  let en_locale = parse_locale("en-US").unwrap().locale.unwrap()
  let en_date = format_date(test_datetime.date, en_locale)
  let en_time = format_time(test_datetime.time, en_locale)
  let en_datetime = format_datetime(test_datetime, en_locale)
  
  assert_eq(en_date, "May 15, 2023")
  assert_eq(en_time, "2:30:45 PM")
  assert_eq(en_datetime, "May 15, 2023 at 2:30:45 PM")
  
  // Test French formatting
  let fr_locale = parse_locale("fr-FR").unwrap().locale.unwrap()
  let fr_date = format_date(test_datetime.date, fr_locale)
  let fr_time = format_time(test_datetime.time, fr_locale)
  let fr_datetime = format_datetime(test_datetime, fr_locale)
  
  assert_eq(fr_date, "15 mai 2023")
  assert_eq(fr_time, "14:30:45")
  assert_eq(fr_datetime, "15 mai 2023 à 14:30:45")
  
  // Test Chinese formatting
  let zh_locale = parse_locale("zh-CN").unwrap().locale.unwrap()
  let zh_date = format_date(test_datetime.date, zh_locale)
  let zh_time = format_time(test_datetime.time, zh_locale)
  let zh_datetime = format_datetime(test_datetime, zh_locale)
  
  assert_eq(zh_date, "2023年5月15日")
  assert_eq(zh_time, "14时30分45秒")
  assert_eq(zh_datetime, "2023年5月15日 14时30分45秒")
  
  // Test German formatting
  let de_locale = parse_locale("de-DE").unwrap().locale.unwrap()
  let de_date = format_date(test_datetime.date, de_locale)
  let de_time = format_time(test_datetime.time, de_locale)
  let de_datetime = format_datetime(test_datetime, de_locale)
  
  assert_eq(de_date, "15. Mai 2023")
  assert_eq(de_time, "14:30:45")
  assert_eq(de_datetime, "15. Mai 2023 14:30:45")
  
  // Test edge cases
  let midnight = {
    date: { year: 2023, month: 1, day: 1 },
    time: { hour: 0, minute: 0, second: 0 },
    timezone_offset: 0
  }
  
  let midnight_en = format_time(midnight.time, en_locale)
  assert_eq(midnight_en, "12:00:00 AM")
  
  let noon = {
    date: { year: 2023, month: 1, day: 1 },
    time: { hour: 12, minute: 0, second: 0 },
    timezone_offset: 0
  }
  
  let noon_en = format_time(noon.time, en_locale)
  assert_eq(noon_en, "12:00:00 PM")
  
  // Test single-digit minutes and seconds
  let test_time = { hour: 9, minute: 5, second: 7 }
  let formatted_time = format_time(test_time, en_locale)
  assert_eq(formatted_time, "9:05:07 AM")
}

// Test 4: Number and Currency Formatting
test "number and currency formatting" {
  // Number formatting options
  type NumberFormatOptions = {
    style: String,  // "decimal", "currency", "percent"
    minimum_fraction_digits: Int,
    maximum_fraction_digits: Int,
    use_grouping: Bool
  }
  
  // Currency information
  type Currency = {
    code: String,
    symbol: String,
    symbol_position: String,  // "before", "after"
    decimal_separator: String,
    grouping_separator: String
  }
  
  // Get currency info by code
  let get_currency_info = fn(code: String) {
    match code {
      "USD" => {
        code: "USD",
        symbol: "$",
        symbol_position: "before",
        decimal_separator: ".",
        grouping_separator: ","
      }
      "EUR" => {
        code: "EUR",
        symbol: "€",
        symbol_position: "before",
        decimal_separator: ",",
        grouping_separator: "."
      }
      "CNY" => {
        code: "CNY",
        symbol: "¥",
        symbol_position: "before",
        decimal_separator: ".",
        grouping_separator: ","
      }
      "JPY" => {
        code: "JPY",
        symbol: "¥",
        symbol_position: "before",
        decimal_separator: ".",
        grouping_separator: ","
      }
      _ => {
        code: "USD",
        symbol: "$",
        symbol_position: "before",
        decimal_separator: ".",
        grouping_separator: ","
      }
    }
  }
  
  // Format number for locale
  let format_number = fn(number: Float, locale: Locale, options: NumberFormatOptions) {
    // Determine separators based on locale
    let (decimal_sep, grouping_sep) = match locale.language {
      "en" => (".", ",")
      "fr" | "de" => (",", ".")
      "zh" => (".", ",")
      _ => (".", ",")
    }
    
    // Handle different styles
    match options.style {
      "currency" => {
        // This is simplified - in a real implementation, we'd need currency info
        let formatted_number = format_number(number, locale, {
          options |
          style: "decimal"
        })
        
        match locale.language {
          "en" => "$" + formatted_number
          "fr" => formatted_number + " €"
          "de" => formatted_number + " €"
          "zh" => "¥" + formatted_number
          _ => "$" + formatted_number
        }
      }
      "percent" => {
        let percent_number = number * 100.0
        let formatted_number = format_number(percent_number, locale, {
          options |
          style: "decimal",
          maximum_fraction_digits: 0
        })
        formatted_number + "%"
      }
      _ => {  // "decimal"
        // Round to appropriate number of decimal places
        let factor = 10.0.pow(options.maximum_fraction_digits.to_float())
        let rounded = (number * factor).round() / factor
        
        // Split into integer and fractional parts
        let integer_part = rounded.to_int().to_string()
        let fractional_part = ((rounded - rounded.to_int().to_float()) * factor).round().to_int().to_string()
        
        // Add leading zeros to fractional part if needed
        let padded_fractional = fractional_part + "0".repeat(options.maximum_fraction_digits - fractional_part.length())
        
        // Add grouping separators to integer part
        let grouped_integer = if options.use_grouping {
          let mut reversed = integer_part.reverse()
          let mut with_separators = ""
          
          for i in 0..reversed.length() {
            if i > 0 and i % 3 == 0 {
              with_separators = with_separators + grouping_sep
            }
            with_separators = with_separators + reversed[i].to_string()
          }
          
          with_separators.reverse()
        } else {
          integer_part
        }
        
        if options.maximum_fraction_digits > 0 {
          grouped_integer + decimal_sep + padded_fractional
        } else {
          grouped_integer
        }
      }
    }
  }
  
  // Test decimal formatting
  let en_locale = parse_locale("en-US").unwrap().locale.unwrap()
  let fr_locale = parse_locale("fr-FR").unwrap().locale.unwrap()
  let zh_locale = parse_locale("zh-CN").unwrap().locale.unwrap()
  
  let decimal_options = {
    style: "decimal",
    minimum_fraction_digits: 0,
    maximum_fraction_digits: 2,
    use_grouping: true
  }
  
  // Test English decimal formatting
  assert_eq(format_number(1234.567, en_locale, decimal_options), "1,234.57")
  assert_eq(format_number(1234567.89, en_locale, decimal_options), "1,234,567.89")
  assert_eq(format_number(1234.5, en_locale, decimal_options), "1,234.5")
  assert_eq(format_number(1234.0, en_locale, decimal_options), "1,234")
  
  // Test French decimal formatting
  assert_eq(format_number(1234.567, fr_locale, decimal_options), "1.234,57")
  assert_eq(format_number(1234567.89, fr_locale, decimal_options), "1.234.567,89")
  assert_eq(format_number(1234.5, fr_locale, decimal_options), "1.234,5")
  assert_eq(format_number(1234.0, fr_locale, decimal_options), "1.234")
  
  // Test Chinese decimal formatting
  assert_eq(format_number(1234.567, zh_locale, decimal_options), "1,234.57")
  assert_eq(format_number(1234567.89, zh_locale, decimal_options), "1,234,567.89")
  
  // Test without grouping
  let no_grouping_options = {
    decimal_options |
    use_grouping: false
  }
  
  assert_eq(format_number(1234567.89, en_locale, no_grouping_options), "1234567.89")
  
  // Test different precision
  let high_precision_options = {
    decimal_options |
    maximum_fraction_digits: 4
  }
  
  assert_eq(format_number(1234.56789, en_locale, high_precision_options), "1,234.5679")
  
  // Test percent formatting
  let percent_options = {
    style: "percent",
    minimum_fraction_digits: 0,
    maximum_fraction_digits: 1,
    use_grouping: false
  }
  
  assert_eq(format_number(0.1234, en_locale, percent_options), "12.3%")
  assert_eq(format_number(0.5, en_locale, percent_options), "50%")
  assert_eq(format_number(1.0, en_locale, percent_options), "100%")
  
  // Test currency formatting
  let currency_options = {
    style: "currency",
    minimum_fraction_digits: 2,
    maximum_fraction_digits: 2,
    use_grouping: true
  }
  
  assert_eq(format_number(1234.56, en_locale, currency_options), "$1,234.56")
  assert_eq(format_number(1234.56, fr_locale, currency_options), "1.234,57 €")
  assert_eq(format_number(1234.56, zh_locale, currency_options), "¥1,234.56")
  
  // Test edge cases
  assert_eq(format_number(0.0, en_locale, decimal_options), "0")
  assert_eq(format_number(-1234.56, en_locale, decimal_options), "-1,234.56")
  assert_eq(format_number(999999999.99, en_locale, decimal_options), "999,999,999.99")
}

// Test 5: Text Direction and RTL/LTR Handling
test "text direction and rtl/ltr handling" {
  // Text direction
  enum TextDirection {
    LTR
    RTL
    Auto
  }
  
  // Localized text
  type LocalizedText = {
    text: String,
    direction: TextDirection,
    language: String
  }
  
  // Get text direction for language
  let get_text_direction = fn(language: String) {
    match language {
      "ar" | "he" | "fa" | "ur" => TextDirection::RTL
      "en" | "fr" | "de" | "zh" | "ja" => TextDirection::LTR
      _ => TextDirection::LTR  // Default to LTR
    }
  }
  
  // Detect text direction from content
  let detect_text_direction = fn(text: String) {
    let mut rtl_chars = 0
    let mut ltr_chars = 0
    
    for i in 0..text.length() {
      let char_code = text[i].to_int()
      
      // Simplified RTL character ranges (Hebrew, Arabic)
      if (char_code >= 0x0590 and char_code <= 0x05FF) or  // Hebrew
         (char_code >= 0x0600 and char_code <= 0x06FF) {  // Arabic
        rtl_chars = rtl_chars + 1
      } else if char_code >= 0x0041 and char_code <= 0x00FF {  // Basic Latin
        ltr_chars = ltr_chars + 1
      }
    }
    
    if rtl_chars > ltr_chars {
      TextDirection::RTL
    } else if ltr_chars > 0 {
      TextDirection::LTR
    } else {
      TextDirection::LTR  // Default to LTR
    }
  }
  
  // Create localized text
  let create_localized_text = fn(text: String, language: String) {
    {
      text,
      direction: get_text_direction(language),
      language
    }
  }
  
  // Auto-detect direction
  let auto_detect_text = fn(text: String) {
    {
      text,
      direction: detect_text_direction(text),
      language: "unknown"
    }
  }
  
  // Format text with direction markers
  let format_text_with_direction = fn(localized_text: LocalizedText) {
    match localized_text.direction {
      TextDirection::LTR => localized_text.text
      TextDirection::RTL => "\u202B" + localized_text.text + "\u202C"  // RLE + PDF
      TextDirection::Auto => {
        match detect_text_direction(localized_text.text) {
          TextDirection::RTL => "\u202B" + localized_text.text + "\u202C"
          TextDirection::LTR => localized_text.text
        }
      }
    }
  }
  
  // Test direction detection for languages
  assert_eq(get_text_direction("en"), TextDirection::LTR)
  assert_eq(get_text_direction("fr"), TextDirection::LTR)
  assert_eq(get_text_direction("ar"), TextDirection::RTL)
  assert_eq(get_text_direction("he"), TextDirection::RTL)
  assert_eq(get_text_direction("zh"), TextDirection::LTR)
  
  // Test auto-detection from content
  let ltr_text = auto_detect_text("Hello world")
  assert_eq(ltr_text.direction, TextDirection::LTR)
  
  // Simulate RTL text (using simplified approach)
  let rtl_text = auto_detect_text("مرحبا بالعالم")  // "Hello world" in Arabic
  assert_eq(rtl_text.direction, TextDirection::RTL)
  
  // Test creating localized text
  let en_text = create_localized_text("Welcome", "en")
  assert_eq(en_text.direction, TextDirection::LTR)
  assert_eq(en_text.language, "en")
  
  let ar_text = create_localized_text("أهلاً", "ar")  // "Welcome" in Arabic
  assert_eq(ar_text.direction, TextDirection::RTL)
  assert_eq(ar_text.language, "ar")
  
  // Test formatting with direction markers
  let formatted_ltr = format_text_with_direction(en_text)
  assert_eq(formatted_ltr, "Welcome")
  
  let formatted_rtl = format_text_with_direction(ar_text)
  assert_eq(formatted_rtl, "\u202Bأهلاً\u202C")
  
  // Test mixed content
  let mixed_text = {
    text: "Hello مرحبا",
    direction: TextDirection::Auto,
    language: "mixed"
  }
  
  let formatted_mixed = format_text_with_direction(mixed_text)
  // Should detect based on dominant characters
  assert_true(formatted_mixed.length() >= mixed_text.text.length())
  
  // Test empty text
  let empty_text = auto_detect_text("")
  assert_eq(empty_text.direction, TextDirection::LTR)  // Default
  
  // Test bidirectional array operations
  let texts = [
    create_localized_text("Hello", "en"),
    create_localized_text("Bonjour", "fr"),
    create_localized_text("مرحبا", "ar"),
    create_localized_text("שלום", "he"),  // "Hello" in Hebrew
    create_localized_text("你好", "zh")   // "Hello" in Chinese
  ]
  
  // Filter by direction
  let ltr_texts = texts.filter(fn(t) { t.direction == TextDirection::LTR })
  let rtl_texts = texts.filter(fn(t) { t.direction == TextDirection::RTL })
  
  assert_eq(ltr_texts.length(), 3)  // en, fr, zh
  assert_eq(rtl_texts.length(), 2)  // ar, he
  
  // Format array of texts with appropriate direction
  let formatted_texts = texts.map(fn(t) { format_text_with_direction(t) })
  
  // LTR texts should be unchanged
  assert_true(formatted_texts[0] == "Hello")
  assert_true(formatted_texts[1] == "Bonjour")
  assert_true(formatted_texts[4] == "你好")
  
  // RTL texts should have direction markers
  assert_true(formatted_texts[2].starts_with("\u202B"))
  assert_true(formatted_texts[3].starts_with("\u202B"))
}