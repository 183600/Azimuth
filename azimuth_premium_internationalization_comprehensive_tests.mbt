// Azimuth Premium Internationalization Comprehensive Tests
// This file contains comprehensive test cases for internationalization and localization support

// Test 1: Locale Detection and Management
test "locale detection and management" {
  // Define locale structure
  type Locale = {
    language: String,
    region: String,
    script: Option[String],
    variant: Option[String]
  }
  
  // Define locale manager
  type LocaleManager = {
    current_locale: Locale,
    supported_locales: Array[Locale],
    fallback_locale: Locale
  }
  
  // Parse locale from string
  let parse_locale = fn(locale_string: String) {
    let parts = locale_string.split("-")
    
    let language = if parts.length() > 0 { parts[0] } else { "en" }
    let region = if parts.length() > 1 { parts[1] } else { "" }
    let script = if parts.length() > 2 { Some(parts[2]) } else { None }
    let variant = if parts.length() > 3 { Some(parts[3]) } else { None }
    
    {
      language,
      region,
      script,
      variant
    }
  }
  
  // Create locale manager
  let create_locale_manager = fn(current_locale_str: String, supported_locale_strs: Array[String], fallback_locale_str: String) {
    let current_locale = parse_locale(current_locale_str)
    let supported_locales = supported_locale_strs.map(parse_locale)
    let fallback_locale = parse_locale(fallback_locale_str)
    
    {
      current_locale,
      supported_locales,
      fallback_locale
    }
  }
  
  // Test locale parsing
  let en_us = parse_locale("en-US")
  assert_eq(en_us.language, "en")
  assert_eq(en_us.region, "US")
  assert_eq(en_us.script, None)
  assert_eq(en_us.variant, None)
  
  let zh_hans_cn = parse_locale("zh-Hans-CN")
  assert_eq(zh_hans_cn.language, "zh")
  assert_eq(zh_hans_cn.region, "CN")
  match zh_hans_cn.script {
    Some(script) => assert_eq(script, "Hans")
    None => assert_true(false)
  }
  assert_eq(zh_hans_cn.variant, None)
  
  let sr_latn_rs = parse_locale("sr-Latn-RS")
  assert_eq(sr_latn_rs.language, "sr")
  assert_eq(sr_latn_rs.region, "RS")
  match sr_latn_rs.script {
    Some(script) => assert_eq(script, "Latn")
    None => assert_true(false)
  }
  
  // Test locale manager creation
  let locale_manager = create_locale_manager(
    "en-US",
    ["en-US", "zh-CN", "ja-JP", "fr-FR", "de-DE"],
    "en-US"
  )
  
  assert_eq(locale_manager.current_locale.language, "en")
  assert_eq(locale_manager.current_locale.region, "US")
  assert_eq(locale_manager.fallback_locale.language, "en")
  assert_eq(locale_manager.fallback_locale.region, "US")
  assert_eq(locale_manager.supported_locales.length(), 5)
  
  // Test locale matching
  let is_locale_supported = fn(manager: LocaleManager, locale: Locale) {
    manager.supported_locales.any(fn(supported) { 
      supported.language == locale.language && 
      supported.region == locale.region 
    })
  }
  
  let test_locale1 = { language: "en", region: "US", script: None, variant: None }
  assert_true(is_locale_supported(locale_manager, test_locale1))
  
  let test_locale2 = { language: "zh", region: "CN", script: None, variant: None }
  assert_true(is_locale_supported(locale_manager, test_locale2))
  
  let test_locale3 = { language: "es", region: "ES", script: None, variant: None }
  assert_false(is_locale_supported(locale_manager, test_locale3))
  
  // Test locale fallback
  let find_best_match = fn(manager: LocaleManager, requested_locale: Locale) {
    // Try exact match first
    if is_locale_supported(manager, requested_locale) {
      Some(requested_locale)
    } else {
      // Try language match without region
      let language_match = manager.supported_locales.find(fn(supported) { 
        supported.language == requested_locale.language 
      })
      
      match language_match {
        Some(matched_locale) => Some(matched_locale)
        None => Some(manager.fallback_locale)
      }
    }
  }
  
  let requested_locale1 = { language: "en", region: "US", script: None, variant: None }
  match find_best_match(locale_manager, requested_locale1) {
    Some(matched) => {
      assert_eq(matched.language, "en")
      assert_eq(matched.region, "US")
    }
    None => assert_true(false)
  }
  
  let requested_locale2 = { language: "zh", region: "TW", script: None, variant: None }
  match find_best_match(locale_manager, requested_locale2) {
    Some(matched) => {
      assert_eq(matched.language, "zh")
      assert_eq(matched.region, "CN")  // Fallback to zh-CN
    }
    None => assert_true(false)
  }
  
  let requested_locale3 = { language: "es", region: "ES", script: None, variant: None }
  match find_best_match(locale_manager, requested_locale3) {
    Some(matched) => {
      assert_eq(matched.language, "en")  // Fallback to en-US
      assert_eq(matched.region, "US")
    }
    None => assert_true(false)
  }
}

// Test 2: Message Formatting and Localization
test "message formatting and localization" {
  // Define message structure
  type Message = {
    key: String,
    translations: Array[(Locale, String)],
    default: String
  }
  
  // Define message formatter
  type MessageFormatter = {
    locale: Locale,
    messages: Array[Message]
  }
  
  // Create message
  let create_message = fn(key: String, translations: Array[(String, String)], default: String) {
    let locale_translations = translations.map(fn(pair) { 
      (parse_locale(pair.0), pair.1) 
    })
    
    {
      key,
      translations: locale_translations,
      default
    }
  }
  
  // Create message formatter
  let create_formatter = fn(locale: Locale, messages: Array[Message]) {
    {
      locale,
      messages
    }
  }
  
  // Format message
  let format_message = fn(formatter: MessageFormatter, key: String, parameters: Array[(String, String)]) {
    match formatter.messages.find(fn(msg) { msg.key == key }) {
      Some(message) => {
        // Find translation for current locale
        let translation = message.translations.find(fn(pair) { 
          pair.0.language == formatter.locale.language && 
          pair.0.region == formatter.locale.region 
        })
        
        let template = match translation {
          Some((_, text)) => text,
          None => {
            // Try language-only match
            let language_translation = message.translations.find(fn(pair) { 
              pair.0.language == formatter.locale.language 
            })
            
            match language_translation {
              Some((_, text)) => text,
              None => message.default
            }
          }
        }
        
        // Simple parameter substitution
        let mut result = template
        for (param, value) in parameters {
          result = result.replace("{" + param + "}", value)
        }
        
        result
      }
      None => key  // Return key if not found
    }
  }
  
  // Parse locale function (reused from previous test)
  let parse_locale = fn(locale_string: String) {
    let parts = locale_string.split("-")
    
    let language = if parts.length() > 0 { parts[0] } else { "en" }
    let region = if parts.length() > 1 { parts[1] } else { "" }
    let script = if parts.length() > 2 { Some(parts[2]) } else { None }
    let variant = if parts.length() > 3 { Some(parts[3]) } else { None }
    
    {
      language,
      region,
      script,
      variant
    }
  }
  
  // Create test messages
  let welcome_message = create_message(
    "welcome.message",
    [
      ("en-US", "Welcome to Azimuth, {username}!"),
      ("zh-CN", "欢迎使用 Azimuth，{username}！"),
      ("ja-JP", "Azimuth へようこそ、{username}さん！"),
      ("fr-FR", "Bienvenue sur Azimuth, {username} !"),
      ("de-DE", "Willkommen bei Azimuth, {username}!")
    ],
    "Welcome to Azimuth, {username}!"
  )
  
  let error_message = create_message(
    "error.network.timeout",
    [
      ("en-US", "Network timeout after {seconds} seconds"),
      ("zh-CN", "网络超时，已等待 {seconds} 秒"),
      ("ja-JP", "{seconds}秒後にネットワークタイムアウト"),
      ("fr-FR", "Délai d'attente réseau après {seconds} secondes"),
      ("de-DE", "Netzwerk-Timeout nach {seconds} Sekunden")
    ],
    "Network timeout after {seconds} seconds"
  )
  
  let items_count_message = create_message(
    "items.count",
    [
      ("en-US", "You have {count} items"),
      ("zh-CN", "您有 {count} 个项目"),
      ("ja-JP", "あなたは {count} 個のアイテムを持っています"),
      ("fr-FR", "Vous avez {count} éléments"),
      ("de-DE", "Sie haben {count} Elemente")
    ],
    "You have {count} items"
  )
  
  // Test English formatting
  let en_us_locale = { language: "en", region: "US", script: None, variant: None }
  let en_formatter = create_formatter(en_us_locale, [welcome_message, error_message, items_count_message])
  
  let en_welcome = format_message(en_formatter, "welcome.message", [("username", "John")])
  assert_eq(en_welcome, "Welcome to Azimuth, John!")
  
  let en_error = format_message(en_formatter, "error.network.timeout", [("seconds", "30")])
  assert_eq(en_error, "Network timeout after 30 seconds")
  
  let en_items = format_message(en_formatter, "items.count", [("count", "42")])
  assert_eq(en_items, "You have 42 items")
  
  // Test Chinese formatting
  let zh_cn_locale = { language: "zh", region: "CN", script: None, variant: None }
  let zh_formatter = create_formatter(zh_cn_locale, [welcome_message, error_message, items_count_message])
  
  let zh_welcome = format_message(zh_formatter, "welcome.message", [("username", "张三")])
  assert_eq(zh_welcome, "欢迎使用 Azimuth，张三！")
  
  let zh_error = format_message(zh_formatter, "error.network.timeout", [("seconds", "30")])
  assert_eq(zh_error, "网络超时，已等待 30 秒")
  
  let zh_items = format_message(zh_formatter, "items.count", [("count", "42")])
  assert_eq(zh_items, "您有 42 个项目")
  
  // Test Japanese formatting
  let ja_jp_locale = { language: "ja", region: "JP", script: None, variant: None }
  let ja_formatter = create_formatter(ja_jp_locale, [welcome_message, error_message, items_count_message])
  
  let ja_welcome = format_message(ja_formatter, "welcome.message", [("username", "田中")])
  assert_eq(ja_welcome, "Azimuth へようこそ、田中さん！")
  
  let ja_error = format_message(ja_formatter, "error.network.timeout", [("seconds", "30")])
  assert_eq(ja_error, "30秒後にネットワークタイムアウト")
  
  // Test fallback to language-only match
  let en_gb_locale = { language: "en", region: "GB", script: None, variant: None }
  let en_gb_formatter = create_formatter(en_gb_locale, [welcome_message, error_message, items_count_message])
  
  let en_gb_welcome = format_message(en_gb_formatter, "welcome.message", [("username", "Alice")])
  assert_eq(en_gb_welcome, "Welcome to Azimuth, Alice!")  // Falls back to en-US
  
  // Test fallback to default
  let es_es_locale = { language: "es", region: "ES", script: None, variant: None }
  let es_formatter = create_formatter(es_es_locale, [welcome_message, error_message, items_count_message])
  
  let es_welcome = format_message(es_formatter, "welcome.message", [("username", "Carlos")])
  assert_eq(es_welcome, "Welcome to Azimuth, Carlos!")  // Falls back to default
  
  // Test missing message key
  let missing_message = format_message(en_formatter, "missing.key", [])
  assert_eq(missing_message, "missing.key")  // Returns key if not found
}

// Test 3: Date and Time Formatting
test "date and time formatting" {
  // Define date formatter
  type DateFormatter = {
    locale: Locale,
    timezone: String
  }
  
  // Define date structure
  type DateTime = {
    year: Int,
    month: Int,
    day: Int,
    hour: Int,
    minute: Int,
    second: Int,
    timestamp: Int
  }
  
  // Create date formatter
  let create_date_formatter = fn(locale: Locale, timezone: String) {
    {
      locale,
      timezone
    }
  }
  
  // Format date according to locale
  let format_date = fn(formatter: DateFormatter, date_time: DateTime, format: String) {
    match formatter.locale.language {
      "en" => {
        match formatter.locale.region {
          "US" => {
            // MM/DD/YYYY format
            format
              .replace("YYYY", date_time.year.to_string())
              .replace("MM", if date_time.month < 10 { "0" + date_time.month.to_string() } else { date_time.month.to_string() })
              .replace("DD", if date_time.day < 10 { "0" + date_time.day.to_string() } else { date_time.day.to_string() })
          }
          _ => {
            // DD/MM/YYYY format
            format
              .replace("YYYY", date_time.year.to_string())
              .replace("DD", if date_time.day < 10 { "0" + date_time.day.to_string() } else { date_time.day.to_string() })
              .replace("MM", if date_time.month < 10 { "0" + date_time.month.to_string() } else { date_time.month.to_string() })
          }
        }
      }
      "zh" => {
        // YYYY年MM月DD日 format
        format
          .replace("YYYY", date_time.year.to_string())
          .replace("MM", date_time.month.to_string())
          .replace("DD", date_time.day.to_string())
          .replace("年", "年")
          .replace("月", "月")
          .replace("日", "日")
      }
      "ja" => {
        // YYYY年MM月DD日 format
        format
          .replace("YYYY", date_time.year.to_string())
          .replace("MM", date_time.month.to_string())
          .replace("DD", date_time.day.to_string())
          .replace("年", "年")
          .replace("月", "月")
          .replace("日", "日")
      }
      "fr" => {
        // DD/MM/YYYY format
        format
          .replace("YYYY", date_time.year.to_string())
          .replace("DD", if date_time.day < 10 { "0" + date_time.day.to_string() } else { date_time.day.to_string() })
          .replace("MM", if date_time.month < 10 { "0" + date_time.month.to_string() } else { date_time.month.to_string() })
      }
      "de" => {
        // DD.MM.YYYY format
        format
          .replace("YYYY", date_time.year.to_string())
          .replace("DD", if date_time.day < 10 { "0" + date_time.day.to_string() } else { date_time.day.to_string() })
          .replace("MM", if date_time.month < 10 { "0" + date_time.month.to_string() } else { date_time.month.to_string() })
          .replace("/", ".")
      }
      _ => {
        // Default format
        format
          .replace("YYYY", date_time.year.to_string())
          .replace("MM", if date_time.month < 10 { "0" + date_time.month.to_string() } else { date_time.month.to_string() })
          .replace("DD", if date_time.day < 10 { "0" + date_time.day.to_string() } else { date_time.day.to_string() })
      }
    }
  }
  
  // Format time according to locale
  let format_time = fn(formatter: DateFormatter, date_time: DateTime, format: String) {
    match formatter.locale.language {
      "en" => {
        // 12-hour format with AM/PM
        let hour = date_time.hour
        let display_hour = if hour <= 12 { 
          if hour == 0 { 12 } else { hour } 
        } else { 
          hour - 12 
        }
        let am_pm = if hour < 12 { "AM" } else { "PM" }
        
        format
          .replace("hh", if display_hour < 10 { "0" + display_hour.to_string() } else { display_hour.to_string() })
          .replace("mm", if date_time.minute < 10 { "0" + date_time.minute.to_string() } else { date_time.minute.to_string() })
          .replace("ss", if date_time.second < 10 { "0" + date_time.second.to_string() } else { date_time.second.to_string() })
          .replace("a", am_pm)
      }
      "zh" | "ja" => {
        // 24-hour format
        format
          .replace("HH", if date_time.hour < 10 { "0" + date_time.hour.to_string() } else { date_time.hour.to_string() })
          .replace("mm", if date_time.minute < 10 { "0" + date_time.minute.to_string() } else { date_time.minute.to_string() })
          .replace("ss", if date_time.second < 10 { "0" + date_time.second.to_string() } else { date_time.second.to_string() })
      }
      "fr" | "de" => {
        // 24-hour format
        format
          .replace("HH", if date_time.hour < 10 { "0" + date_time.hour.to_string() } else { date_time.hour.to_string() })
          .replace("mm", if date_time.minute < 10 { "0" + date_time.minute.to_string() } else { date_time.minute.to_string() })
          .replace("ss", if date_time.second < 10 { "0" + date_time.second.to_string() } else { date_time.second.to_string() })
      }
      _ => {
        // Default 24-hour format
        format
          .replace("HH", if date_time.hour < 10 { "0" + date_time.hour.to_string() } else { date_time.hour.to_string() })
          .replace("mm", if date_time.minute < 10 { "0" + date_time.minute.to_string() } else { date_time.minute.to_string() })
          .replace("ss", if date_time.second < 10 { "0" + date_time.second.to_string() } else { date_time.second.to_string() })
      }
    }
  }
  
  // Create test date
  let test_date = {
    year: 2023,
    month: 1,
    day: 15,
    hour: 14,
    minute: 30,
    second: 45,
    timestamp: 1673789445
  }
  
  // Test English (US) date formatting
  let en_us_locale = { language: "en", region: "US", script: None, variant: None }
  let en_us_formatter = create_date_formatter(en_us_locale, "America/New_York")
  
  let en_us_date = format_date(en_us_formatter, test_date, "MM/DD/YYYY")
  assert_eq(en_us_date, "01/15/2023")
  
  let en_us_time = format_time(en_us_formatter, test_date, "hh:mm:ss a")
  assert_eq(en_us_time, "02:30:45 PM")
  
  // Test English (UK) date formatting
  let en_gb_locale = { language: "en", region: "GB", script: None, variant: None }
  let en_gb_formatter = create_date_formatter(en_gb_locale, "Europe/London")
  
  let en_gb_date = format_date(en_gb_formatter, test_date, "DD/MM/YYYY")
  assert_eq(en_gb_date, "15/01/2023")
  
  // Test Chinese date formatting
  let zh_cn_locale = { language: "zh", region: "CN", script: None, variant: None }
  let zh_cn_formatter = create_date_formatter(zh_cn_locale, "Asia/Shanghai")
  
  let zh_cn_date = format_date(zh_cn_formatter, test_date, "YYYY年MM月DD日")
  assert_eq(zh_cn_date, "2023年1月15日")
  
  let zh_cn_time = format_time(zh_cn_formatter, test_date, "HH:mm:ss")
  assert_eq(zh_cn_time, "14:30:45")
  
  // Test Japanese date formatting
  let ja_jp_locale = { language: "ja", region: "JP", script: None, variant: None }
  let ja_jp_formatter = create_date_formatter(ja_jp_locale, "Asia/Tokyo")
  
  let ja_jp_date = format_date(ja_jp_formatter, test_date, "YYYY年MM月DD日")
  assert_eq(ja_jp_date, "2023年1月15日")
  
  let ja_jp_time = format_time(ja_jp_formatter, test_date, "HH:mm:ss")
  assert_eq(ja_jp_time, "14:30:45")
  
  // Test French date formatting
  let fr_fr_locale = { language: "fr", region: "FR", script: None, variant: None }
  let fr_fr_formatter = create_date_formatter(fr_fr_locale, "Europe/Paris")
  
  let fr_fr_date = format_date(fr_fr_formatter, test_date, "DD/MM/YYYY")
  assert_eq(fr_fr_date, "15/01/2023")
  
  let fr_fr_time = format_time(fr_fr_formatter, test_date, "HH:mm:ss")
  assert_eq(fr_fr_time, "14:30:45")
  
  // Test German date formatting
  let de_de_locale = { language: "de", region: "DE", script: None, variant: None }
  let de_de_formatter = create_date_formatter(de_de_locale, "Europe/Berlin")
  
  let de_de_date = format_date(de_de_formatter, test_date, "DD.MM.YYYY")
  assert_eq(de_de_date, "15.01.2023")
  
  let de_de_time = format_time(de_de_formatter, test_date, "HH:mm:ss")
  assert_eq(de_de_time, "14:30:45")
}

// Test 4: Number and Currency Formatting
test "number and currency formatting" {
  // Define number formatter
  type NumberFormatter = {
    locale: Locale,
    currency: String
  }
  
  // Create number formatter
  let create_number_formatter = fn(locale: Locale, currency: String) {
    {
      locale,
      currency
    }
  }
  
  // Format decimal number according to locale
  let format_decimal = fn(formatter: NumberFormatter, number: Float, decimal_places: Int) {
    match formatter.locale.language {
      "en" => {
        match formatter.locale.region {
          "US" => {
            // 1,234,567.89 format
            let rounded = (number * (10.0 ^ decimal_places.to_float())).round() / (10.0 ^ decimal_places.to_float())
            let parts = rounded.to_string().split(".")
            let integer_part = parts[0]
            let decimal_part = if parts.length() > 1 { parts[1] } else { "0" }
            
            // Add thousands separators
            let mut formatted_integer = ""
            let mut digit_count = 0
            for i in (integer_part.length() - 1)..=0 {
              digit_count = digit_count + 1
              formatted_integer = integer_part[i].to_string() + formatted_integer
              if digit_count % 3 == 0 && i > 0 {
                formatted_integer = "," + formatted_integer
              }
            }
            
            // Ensure correct decimal places
            let mut formatted_decimal = decimal_part
            while formatted_decimal.length() < decimal_places {
              formatted_decimal = formatted_decimal + "0"
            }
            if formatted_decimal.length() > decimal_places {
              formatted_decimal = formatted_decimal.substring(0, decimal_places)
            }
            
            if decimal_places > 0 {
              formatted_integer + "." + formatted_decimal
            } else {
              formatted_integer
            }
          }
          _ => {
            // 1 234 567,89 format (other English locales)
            let rounded = (number * (10.0 ^ decimal_places.to_float())).round() / (10.0 ^ decimal_places.to_float())
            let parts = rounded.to_string().split(".")
            let integer_part = parts[0]
            let decimal_part = if parts.length() > 1 { parts[1] } else { "0" }
            
            // Add thousands separators with spaces
            let mut formatted_integer = ""
            let mut digit_count = 0
            for i in (integer_part.length() - 1)..=0 {
              digit_count = digit_count + 1
              formatted_integer = integer_part[i].to_string() + formatted_integer
              if digit_count % 3 == 0 && i > 0 {
                formatted_integer = " " + formatted_integer
              }
            }
            
            // Ensure correct decimal places
            let mut formatted_decimal = decimal_part
            while formatted_decimal.length() < decimal_places {
              formatted_decimal = formatted_decimal + "0"
            }
            if formatted_decimal.length() > decimal_places {
              formatted_decimal = formatted_decimal.substring(0, decimal_places)
            }
            
            if decimal_places > 0 {
              formatted_integer + "," + formatted_decimal
            } else {
              formatted_integer
            }
          }
        }
      }
      "zh" => {
        // 1,234,567.89 format (Chinese uses period for decimal)
        let rounded = (number * (10.0 ^ decimal_places.to_float())).round() / (10.0 ^ decimal_places.to_float())
        let parts = rounded.to_string().split(".")
        let integer_part = parts[0]
        let decimal_part = if parts.length() > 1 { parts[1] } else { "0" }
        
        // Add thousands separators
        let mut formatted_integer = ""
        let mut digit_count = 0
        for i in (integer_part.length() - 1)..=0 {
          digit_count = digit_count + 1
          formatted_integer = integer_part[i].to_string() + formatted_integer
          if digit_count % 3 == 0 && i > 0 {
            formatted_integer = "," + formatted_integer
          }
        }
        
        // Ensure correct decimal places
        let mut formatted_decimal = decimal_part
        while formatted_decimal.length() < decimal_places {
          formatted_decimal = formatted_decimal + "0"
        }
        if formatted_decimal.length() > decimal_places {
          formatted_decimal = formatted_decimal.substring(0, decimal_places)
        }
        
        if decimal_places > 0 {
          formatted_integer + "." + formatted_decimal
        } else {
          formatted_integer
        }
      }
      "ja" => {
        // 1,234,567.89 format (Japanese uses period for decimal)
        let rounded = (number * (10.0 ^ decimal_places.to_float())).round() / (10.0 ^ decimal_places.to_float())
        let parts = rounded.to_string().split(".")
        let integer_part = parts[0]
        let decimal_part = if parts.length() > 1 { parts[1] } else { "0" }
        
        // Add thousands separators
        let mut formatted_integer = ""
        let mut digit_count = 0
        for i in (integer_part.length() - 1)..=0 {
          digit_count = digit_count + 1
          formatted_integer = integer_part[i].to_string() + formatted_integer
          if digit_count % 3 == 0 && i > 0 {
            formatted_integer = "," + formatted_integer
          }
        }
        
        // Ensure correct decimal places
        let mut formatted_decimal = decimal_part
        while formatted_decimal.length() < decimal_places {
          formatted_decimal = formatted_decimal + "0"
        }
        if formatted_decimal.length() > decimal_places {
          formatted_decimal = formatted_decimal.substring(0, decimal_places)
        }
        
        if decimal_places > 0 {
          formatted_integer + "." + formatted_decimal
        } else {
          formatted_integer
        }
      }
      "fr" => {
        // 1 234 567,89 format (French uses comma for decimal)
        let rounded = (number * (10.0 ^ decimal_places.to_float())).round() / (10.0 ^ decimal_places.to_float())
        let parts = rounded.to_string().split(".")
        let integer_part = parts[0]
        let decimal_part = if parts.length() > 1 { parts[1] } else { "0" }
        
        // Add thousands separators with spaces
        let mut formatted_integer = ""
        let mut digit_count = 0
        for i in (integer_part.length() - 1)..=0 {
          digit_count = digit_count + 1
          formatted_integer = integer_part[i].to_string() + formatted_integer
          if digit_count % 3 == 0 && i > 0 {
            formatted_integer = " " + formatted_integer
          }
        }
        
        // Ensure correct decimal places
        let mut formatted_decimal = decimal_part
        while formatted_decimal.length() < decimal_places {
          formatted_decimal = formatted_decimal + "0"
        }
        if formatted_decimal.length() > decimal_places {
          formatted_decimal = formatted_decimal.substring(0, decimal_places)
        }
        
        if decimal_places > 0 {
          formatted_integer + "," + formatted_decimal
        } else {
          formatted_integer
        }
      }
      "de" => {
        // 1.234.567,89 format (German uses period for thousands, comma for decimal)
        let rounded = (number * (10.0 ^ decimal_places.to_float())).round() / (10.0 ^ decimal_places.to_float())
        let parts = rounded.to_string().split(".")
        let integer_part = parts[0]
        let decimal_part = if parts.length() > 1 { parts[1] } else { "0" }
        
        // Add thousands separators with periods
        let mut formatted_integer = ""
        let mut digit_count = 0
        for i in (integer_part.length() - 1)..=0 {
          digit_count = digit_count + 1
          formatted_integer = integer_part[i].to_string() + formatted_integer
          if digit_count % 3 == 0 && i > 0 {
            formatted_integer = "." + formatted_integer
          }
        }
        
        // Ensure correct decimal places
        let mut formatted_decimal = decimal_part
        while formatted_decimal.length() < decimal_places {
          formatted_decimal = formatted_decimal + "0"
        }
        if formatted_decimal.length() > decimal_places {
          formatted_decimal = formatted_decimal.substring(0, decimal_places)
        }
        
        if decimal_places > 0 {
          formatted_integer + "," + formatted_decimal
        } else {
          formatted_integer
        }
      }
      _ => {
        // Default format
        number.to_string()
      }
    }
  }
  
  // Format currency according to locale
  let format_currency = fn(formatter: NumberFormatter, amount: Float) {
    let formatted_amount = format_decimal(formatter, amount, 2)
    
    match formatter.locale.language {
      "en" => {
        match formatter.locale.region {
          "US" => {
            match formatter.currency {
              "USD" => "$" + formatted_amount
              "EUR" => "€" + formatted_amount
              "JPY" => "¥" + formatted_amount
              _ => formatted_amount + " " + formatter.currency
            }
          }
          _ => {
            match formatter.currency {
              "USD" => "$" + formatted_amount
              "EUR" => "€" + formatted_amount
              "GBP" => "£" + formatted_amount
              _ => formatted_amount + " " + formatter.currency
            }
          }
        }
      }
      "zh" => {
        match formatter.currency {
          "CNY" => "¥" + formatted_amount
          "USD" => "$" + formatted_amount
          _ => formatted_amount + " " + formatter.currency
        }
      }
      "ja" => {
        match formatter.currency {
          "JPY" => "¥" + formatted_amount
          "USD" => "$" + formatted_amount
          _ => formatted_amount + formatter.currency
        }
      }
      "fr" => {
        match formatter.currency {
          "EUR" => formatted_amount + " €"
          "USD" => formatted_amount + " $"
          _ => formatted_amount + " " + formatter.currency
        }
      }
      "de" => {
        match formatter.currency {
          "EUR" => formatted_amount + " €"
          "USD" => formatted_amount + " $"
          _ => formatted_amount + " " + formatter.currency
        }
      }
      _ => {
        formatted_amount + " " + formatter.currency
      }
    }
  }
  
  // Test number formatting
  let test_number = 1234567.89123
  
  // English (US)
  let en_us_locale = { language: "en", region: "US", script: None, variant: None }
  let en_us_formatter = create_number_formatter(en_us_locale, "USD")
  
  let en_us_number = format_decimal(en_us_formatter, test_number, 2)
  assert_eq(en_us_number, "1,234,567.89")
  
  // Chinese
  let zh_cn_locale = { language: "zh", region: "CN", script: None, variant: None }
  let zh_cn_formatter = create_number_formatter(zh_cn_locale, "CNY")
  
  let zh_cn_number = format_decimal(zh_cn_formatter, test_number, 2)
  assert_eq(zh_cn_number, "1,234,567.89")
  
  // French
  let fr_fr_locale = { language: "fr", region: "FR", script: None, variant: None }
  let fr_fr_formatter = create_number_formatter(fr_fr_locale, "EUR")
  
  let fr_fr_number = format_decimal(fr_fr_formatter, test_number, 2)
  assert_eq(fr_fr_number, "1 234 567,89")
  
  // German
  let de_de_locale = { language: "de", region: "DE", script: None, variant: None }
  let de_de_formatter = create_number_formatter(de_de_locale, "EUR")
  
  let de_de_number = format_decimal(de_de_formatter, test_number, 2)
  assert_eq(de_de_number, "1.234.567,89")
  
  // Test currency formatting
  let test_amount = 1234.56
  
  // English (US) USD
  let en_us_currency = format_currency(en_us_formatter, test_amount)
  assert_eq(en_us_currency, "$1,234.56")
  
  // English (UK) GBP
  let en_gb_locale = { language: "en", region: "GB", script: None, variant: None }
  let en_gb_formatter = create_number_formatter(en_gb_locale, "GBP")
  
  let en_gb_currency = format_currency(en_gb_formatter, test_amount)
  assert_eq(en_gb_currency, "£1,234.56")
  
  // Chinese CNY
  let zh_cn_currency = format_currency(zh_cn_formatter, test_amount)
  assert_eq(zh_cn_currency, "¥1,234.56")
  
  // Japanese JPY
  let ja_jp_locale = { language: "ja", region: "JP", script: None, variant: None }
  let ja_jp_formatter = create_number_formatter(ja_jp_locale, "JPY")
  
  let ja_jp_currency = format_currency(ja_jp_formatter, test_amount)
  assert_eq(ja_jp_currency, "¥1,234.56")
  
  // French EUR
  let fr_fr_currency = format_currency(fr_fr_formatter, test_amount)
  assert_eq(fr_fr_currency, "1 234,56 €")
  
  // German EUR
  let de_de_currency = format_currency(de_de_formatter, test_amount)
  assert_eq(de_de_currency, "1.234,56 €")
}

// Test 5: Text Direction and RTL/LTR Support
test "text direction and rtl/ltr support" {
  // Define text direction
  enum TextDirection {
    LTR
    RTL
    Auto
  }
  
  // Define localized text
  type LocalizedText = {
    text: String,
    direction: TextDirection,
    locale: Locale
  }
  
  // Get text direction for locale
  let get_text_direction = fn(locale: Locale) {
    match locale.language {
      "ar" | "he" | "fa" | "ur" | "yi" => TextDirection::RTL
      _ => TextDirection::LTR
    }
  }
  
  // Create localized text
  let create_localized_text = fn(text: String, locale: Locale) {
    {
      text,
      direction: get_text_direction(locale),
      locale
    }
  }
  
  // Format text with direction markers
  let format_with_direction = fn(localized_text: LocalizedText) {
    match localized_text.direction {
      TextDirection::LTR => localized_text.text
      TextDirection::RTL => "\u202B" + localized_text.text + "\u202C"  // RLE + PDF
      TextDirection::Auto => {
        // Simple auto-detection based on first strong character
        let text = localized_text.text
        if text.length() > 0 {
          let first_char = text[0]
          if (first_char >= '\u0590' && first_char <= '\u08FF') ||  // Hebrew, Arabic, Syriac
             (first_char >= '\uFB50' && first_char <= '\uFEFC') {  // Arabic Presentation Forms
            "\u202B" + text + "\u202C"  // RLE + PDF
          } else {
            text
          }
        } else {
          text
        }
      }
    }
  }
  
  // Create test locales
  let en_us_locale = { language: "en", region: "US", script: None, variant: None }
  let ar_sa_locale = { language: "ar", region: "SA", script: None, variant: None }
  let he_il_locale = { language: "he", region: "IL", script: None, variant: None }
  let ja_jp_locale = { language: "ja", region: "JP", script: None, variant: None }
  
  // Test text direction detection
  assert_eq(get_text_direction(en_us_locale), TextDirection::LTR)
  assert_eq(get_text_direction(ar_sa_locale), TextDirection::RTL)
  assert_eq(get_text_direction(he_il_locale), TextDirection::RTL)
  assert_eq(get_text_direction(ja_jp_locale), TextDirection::LTR)
  
  // Test English text formatting
  let en_text = create_localized_text("Welcome to Azimuth", en_us_locale)
  assert_eq(en_text.direction, TextDirection::LTR)
  assert_eq(format_with_direction(en_text), "Welcome to Azimuth")
  
  // Test Arabic text formatting
  let ar_text = create_localized_text("مرحبا بكم في أزيموث", ar_sa_locale)
  assert_eq(ar_text.direction, TextDirection::RTL)
  assert_eq(format_with_direction(ar_text), "\u202Bمرحبا بكم في أزيموث\u202C")
  
  // Test Hebrew text formatting
  let he_text = create_localized_text("ברוכים הבאים לאזימות'", he_il_locale)
  assert_eq(he_text.direction, TextDirection::RTL)
  assert_eq(format_with_direction(he_text), "\u202Bברוכים הבאים לאזימות'\u202C")
  
  // Test Japanese text formatting
  let ja_text = create_localized_text("アジムスへようこそ", ja_jp_locale)
  assert_eq(ja_text.direction, TextDirection::LTR)
  assert_eq(format_with_direction(ja_text), "アジムスへようこそ")
  
  // Test auto-direction detection
  let auto_hebrew_text = {
    text: "שלום עולם",
    direction: TextDirection::Auto,
    locale: en_us_locale  // Using English locale but Hebrew text
  }
  assert_eq(format_with_direction(auto_hebrew_text), "\u202Bשלום עולם\u202C")
  
  let auto_english_text = {
    text: "Hello World",
    direction: TextDirection::Auto,
    locale: ar_sa_locale  // Using Arabic locale but English text
  }
  assert_eq(format_with_direction(auto_english_text), "Hello World")
  
  // Test mixed direction text
  let mixed_text = {
    text: "Hello שלום World עולם",
    direction: TextDirection::Auto,
    locale: en_us_locale
  }
  // Should detect based on first strong character (English)
  assert_eq(format_with_direction(mixed_text), "Hello שלום World עולם")
}