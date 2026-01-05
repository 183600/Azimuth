// Azimuth Internationalization Support Comprehensive Test Suite
// This file contains comprehensive test cases for internationalization functionality

// Test 1: Locale Management
test "locale management for telemetry system" {
  // Define locale types
  type Locale = {
    language: String,
    region: String,
    script: Option<String>,
    variant: Option<String>
  }
  
  type LocaleManager = {
    current_locale: Locale,
    supported_locales: Array[Locale>,
    fallback_locale: Locale
  }
  
  // Locale operations
  let create_locale = fn(language: String, region: String, script: Option<String>, variant: Option<String>) {
    {
      language,
      region,
      script,
      variant
    }
  }
  
  let create_locale_manager = fn(current: Locale, fallback: Locale) {
    {
      current_locale: current,
      supported_locales: [current, fallback],
      fallback_locale: fallback
    }
  }
  
  let add_supported_locale = fn(manager: LocaleManager, locale: Locale) {
    let mut supported = manager.supported_locales
    
    // Check if locale is already supported
    let already_supported = supported.any(fn(l) { 
      l.language == locale.language and 
      l.region == locale.region and 
      l.script == locale.script and 
      l.variant == locale.variant 
    })
    
    if not(already_supported) {
      supported = supported.push(locale)
    }
    
    {
      current_locale: manager.current_locale,
      supported_locales: supported,
      fallback_locale: manager.fallback_locale
    }
  }
  
  let set_current_locale = fn(manager: LocaleManager, locale: Locale) {
    let is_supported = manager.supported_locales.any(fn(l) { 
      l.language == locale.language and 
      l.region == locale.region and 
      l.script == locale.script and 
      l.variant == locale.variant 
    })
    
    if is_supported {
      {
        current_locale: locale,
        supported_locales: manager.supported_locales,
        fallback_locale: manager.fallback_locale
      }
    } else {
      manager // Locale not supported
    }
  }
  
  let locale_to_string = fn(locale: Locale) {
    let mut result = locale.language
    
    if locale.script.is_some() {
      result = result + "-" + locale.script.unwrap()
    }
    
    result = result + "-" + locale.region
    
    if locale.variant.is_some() {
      result = result + "-" + locale.variant.unwrap()
    }
    
    result
  }
  
  let parse_locale = fn(locale_str: String) {
    let parts = locale_str.split("-")
    
    if parts.length() >= 2 {
      let language = parts[0]
      let region = parts[1]
      let script = if parts.length() > 2 and parts[1].length() == 4 { Some(parts[1]) } else { None }
      let actual_region = if script.is_some() and parts.length() > 2 { parts[2] } else { region }
      let variant = if parts.length() > (if script.is_some() { 3 } else { 2 }) { 
        Some(parts[if script.is_some() { 3 } else { 2 }]) 
      } else { 
        None 
      }
      
      Some(create_locale(language, actual_region, script, variant))
    } else {
      None
    }
  }
  
  // Test locale creation
  let en_us = create_locale("en", "US", None, None)
  assert_eq(en_us.language, "en")
  assert_eq(en_us.region, "US")
  assert_eq(en_us.script, None)
  assert_eq(en_us.variant, None)
  
  let zh_cn = create_locale("zh", "CN", Some("Hans"), None)
  assert_eq(zh_cn.language, "zh")
  assert_eq(zh_cn.region, "CN")
  assert_eq(zh_cn.script, Some("Hans"))
  
  let de_de_variant = create_locale("de", "DE", None, Some("1996"))
  assert_eq(de_de_variant.language, "de")
  assert_eq(de_de_variant.region, "DE")
  assert_eq(de_de_variant.variant, Some("1996"))
  
  // Test locale manager
  let fallback = create_locale("en", "US", None, None)
  let current = create_locale("zh", "CN", Some("Hans"), None)
  let manager = create_locale_manager(current, fallback)
  
  assert_eq(manager.current_locale.language, "zh")
  assert_eq(manager.fallback_locale.language, "en")
  assert_eq(manager.supported_locales.length(), 2)
  
  // Test adding supported locales
  let manager1 = add_supported_locale(manager, create_locale("fr", "FR", None, None))
  assert_eq(manager1.supported_locales.length(), 3)
  
  // Test setting current locale
  let manager2 = set_current_locale(manager1, create_locale("fr", "FR", None, None))
  assert_eq(manager2.current_locale.language, "fr")
  
  // Test setting unsupported locale
  let manager3 = set_current_locale(manager2, create_locale("es", "ES", None, None))
  assert_eq(manager3.current_locale.language, "fr") // Should remain unchanged
  
  // Test locale to string
  assert_eq(locale_to_string(en_us), "en-US")
  assert_eq(locale_to_string(zh_cn), "zh-Hans-CN")
  assert_eq(locale_to_string(de_de_variant), "de-DE-1996")
  
  // Test parsing locale
  let parsed_en_us = parse_locale("en-US")
  assert_eq(parsed_en_us.unwrap().language, "en")
  assert_eq(parsed_en_us.unwrap().region, "US")
  
  let parsed_zh_cn = parse_locale("zh-Hans-CN")
  assert_eq(parsed_zh_cn.unwrap().language, "zh")
  assert_eq(parsed_zh_cn.unwrap().region, "CN")
  assert_eq(parsed_zh_cn.unwrap().script, Some("Hans"))
  
  let parsed_invalid = parse_locale("invalid")
  assert_eq(parsed_invalid, None)
}

// Test 2: Message Translation
test "message translation for telemetry system" {
  // Define translation types
  type TranslationKey = String
  type TranslationValue = String
  type TranslationMap = Array[(TranslationKey, TranslationValue)]
  
  type TranslationDictionary = {
    locale: Locale,
    translations: TranslationMap
  }
  
  type TranslationManager = {
    dictionaries: Array[TranslationDictionary>,
    fallback_manager: LocaleManager
  }
  
  // Translation operations
  let create_translation_dictionary = fn(locale: Locale, translations: TranslationMap) {
    {
      locale,
      translations
    }
  }
  
  let create_translation_manager = fn(fallback_manager: LocaleManager) {
    {
      dictionaries: [],
      fallback_manager
    }
  }
  
  let add_dictionary = fn(manager: TranslationManager, dictionary: TranslationDictionary) {
    { manager | dictionaries: manager.dictionaries.push(dictionary) }
  }
  
  let get_translation = fn(manager: TranslationManager, key: TranslationKey) {
    // Try to find translation in current locale
    let current_locale = manager.fallback_manager.current_locale
    
    let current_dict = manager.dictionaries.find(fn(dict) { 
      dict.locale.language == current_locale.language and 
      dict.locale.region == current_locale.region and
      dict.locale.script == current_locale.script and
      dict.locale.variant == current_locale.variant
    })
    
    match current_dict {
      Some(dict) => {
        match dict.translations.find(fn(pair) { pair.0 == key }) {
          Some((_, value)) => Some(value)
          None => {
            // Try fallback locale
            let fallback_locale = manager.fallback_manager.fallback_locale
            let fallback_dict = manager.dictionaries.find(fn(fallback_dict) { 
              fallback_dict.locale.language == fallback_locale.language and 
              fallback_dict.locale.region == fallback_locale.region
            })
            
            match fallback_dict {
              Some(f_dict) => {
                match f_dict.translations.find(fn(pair) { pair.0 == key }) {
                  Some((_, value)) => Some(value)
                  None => Some(key) // Return key as last resort
                }
              }
              None => Some(key) // Return key as last resort
            }
          }
        }
      }
      None => {
        // Try fallback locale
        let fallback_locale = manager.fallback_manager.fallback_locale
        let fallback_dict = manager.dictionaries.find(fn(dict) { 
          dict.locale.language == fallback_locale.language and 
          dict.locale.region == fallback_locale.region
        })
        
        match fallback_dict {
          Some(dict) => {
            match dict.translations.find(fn(pair) { pair.0 == key }) {
              Some((_, value)) => Some(value)
              None => Some(key) // Return key as last resort
            }
          }
          None => Some(key) // Return key as last resort
        }
      }
    }
  }
  
  let translate_with_params = fn(manager: TranslationManager, key: TranslationKey, params: Array[String]) {
    match get_translation(manager, key) {
      Some(template) => {
        let mut result = template
        
        for i in 0..params.length() {
          let placeholder = "{" + i.to_string() + "}"
          result = result.replace(placeholder, params[i])
        }
        
        result
      }
      None => key
    }
  }
  
  // Test translation dictionary creation
  let en_us_translations = [
    ("telemetry.status.ok", "OK"),
    ("telemetry.status.error", "Error"),
    ("telemetry.message.processing", "Processing {0} records"),
    ("telemetry.error.connection_failed", "Connection failed")
  ]
  
  let zh_cn_translations = [
    ("telemetry.status.ok", "正常"),
    ("telemetry.status.error", "错误"),
    ("telemetry.message.processing", "正在处理 {0} 条记录"),
    ("telemetry.error.connection_failed", "连接失败")
  ]
  
  let en_us_dict = create_translation_dictionary(create_locale("en", "US", None, None), en_us_translations)
  let zh_cn_dict = create_translation_dictionary(create_locale("zh", "CN", Some("Hans"), None), zh_cn_translations)
  
  assert_eq(en_us_dict.locale.language, "en")
  assert_eq(zh_cn_dict.locale.language, "zh")
  assert_eq(en_us_dict.translations.length(), 4)
  assert_eq(zh_cn_dict.translations.length(), 4)
  
  // Test translation manager
  let fallback = create_locale("en", "US", None, None)
  let current = create_locale("zh", "CN", Some("Hans"), None)
  let locale_manager = create_locale_manager(current, fallback)
  let translation_manager = create_translation_manager(locale_manager)
  
  let manager1 = add_dictionary(translation_manager, en_us_dict)
  let manager2 = add_dictionary(manager1, zh_cn_dict)
  
  // Test translation
  assert_eq(get_translation(manager2, "telemetry.status.ok"), Some("正常"))
  assert_eq(get_translation(manager2, "telemetry.status.error"), Some("错误"))
  
  // Test parameterized translation
  let param_result = translate_with_params(manager2, "telemetry.message.processing", ["100"])
  assert_eq(param_result, "正在处理 100 条记录")
  
  // Test translation fallback
  let non_existent_key = get_translation(manager2, "non.existent.key")
  assert_eq(non_existent_key, Some("non.existent.key"))
  
  // Test translation with different locale
  let en_locale_manager = { locale_manager | current_locale = create_locale("en", "US", None, None) }
  let en_translation_manager = { manager2 | fallback_manager = en_locale_manager }
  
  assert_eq(get_translation(en_translation_manager, "telemetry.status.ok"), Some("OK"))
  assert_eq(get_translation(en_translation_manager, "telemetry.error.connection_failed"), Some("Connection failed"))
}

// Test 3: Date and Time Formatting
test "date and time formatting for different locales" {
  // Define date/time types
  type DateTimeFormat = {
    date_format: String,
    time_format: String,
    date_time_format: String,
    timezone: String
  }
  
  type DateTimeFormatter = {
    formats: Array[(Locale, DateTimeFormat)],
    default_format: DateTimeFormat
  }
  
  // Date/time operations
  let create_date_time_format = fn(date_format: String, time_format: String, date_time_format: String, timezone: String) {
    {
      date_format,
      time_format,
      date_time_format,
      timezone
    }
  }
  
  let create_date_time_formatter = fn(default_format: DateTimeFormat) {
    {
      formats: [],
      default_format
    }
  }
  
  let add_locale_format = fn(formatter: DateTimeFormatter, locale: Locale, format: DateTimeFormat) {
    { formatter | formats: formatter.formats.push((locale, format)) }
  }
  
  let format_date = fn(formatter: DateTimeFormatter, year: Int, month: Int, day: Int, locale: Locale) {
    let format = match formatter.formats.find(fn(pair) { 
      pair.0.language == locale.language and 
      pair.0.region == locale.region 
    }) {
      Some((_, format)) => format
      None => formatter.default_format
    }
    
    // Simplified formatting
    match locale.language {
      "en" => {
        match locale.region {
          "US" => month.to_string() + "/" + day.to_string() + "/" + year.to_string()
          "GB" => day.to_string() + "/" + month.to_string() + "/" + year.to_string()
          _ => year.to_string() + "-" + month.to_string() + "-" + day.to_string()
        }
      }
      "zh" => {
        year.to_string() + "年" + month.to_string() + "月" + day.to_string() + "日"
      }
      "de" => {
        day.to_string() + "." + month.to_string() + "." + year.to_string()
      }
      "fr" => {
        day.to_string() + "/" + month.to_string() + "/" + year.to_string()
      }
      _ => {
        year.to_string() + "-" + month.to_string() + "-" + day.to_string()
      }
    }
  }
  
  let format_time = fn(formatter: DateTimeFormatter, hour: Int, minute: Int, second: Int, locale: Locale) {
    let format = match formatter.formats.find(fn(pair) { 
      pair.0.language == locale.language and 
      pair.0.region == locale.region 
    }) {
      Some((_, format)) => format
      None => formatter.default_format
    }
    
    // Simplified formatting
    match locale.language {
      "en" => {
        let period = if hour < 12 { "AM" } else { "PM" }
        let hour_12 = if hour == 0 { 12 } else if hour > 12 { hour - 12 } else { hour }
        hour_12.to_string() + ":" + minute.to_string() + ":" + second.to_string() + " " + period
      }
      "zh" => {
        hour.to_string() + ":" + minute.to_string() + ":" + second.to_string()
      }
      "de" => {
        hour.to_string() + ":" + minute.to_string() + ":" + second.to_string()
      }
      "fr" => {
        hour.to_string() + ":" + minute.to_string() + ":" + second.to_string()
      }
      _ => {
        hour.to_string() + ":" + minute.to_string() + ":" + second.to_string()
      }
    }
  }
  
  let format_date_time = fn(formatter: DateTimeFormatter, year: Int, month: Int, day: Int, 
                            hour: Int, minute: Int, second: Int, locale: Locale) {
    let date_str = format_date(formatter, year, month, day, locale)
    let time_str = format_time(formatter, hour, minute, second, locale)
    
    match locale.language {
      "en" => date_str + " " + time_str
      "zh" => date_str + " " + time_str
      "de" => date_str + " " + time_str
      "fr" => date_str + " à " + time_str
      _ => date_str + " " + time_str
    }
  }
  
  // Test date/time formatter creation
  let default_format = create_date_time_format("yyyy-MM-dd", "HH:mm:ss", "yyyy-MM-dd HH:mm:ss", "UTC")
  let formatter = create_date_time_formatter(default_format)
  
  assert_eq(formatter.default_format.date_format, "yyyy-MM-dd")
  assert_eq(formatter.default_format.time_format, "HH:mm:ss")
  
  // Test adding locale formats
  let en_us_format = create_date_time_format("MM/dd/yyyy", "hh:mm:ss a", "MM/dd/yyyy hh:mm:ss a", "America/New_York")
  let zh_cn_format = create_date_time_format("yyyy年M月d日", "HH:mm:ss", "yyyy年M月d日 HH:mm:ss", "Asia/Shanghai")
  let de_de_format = create_date_time_format("dd.MM.yyyy", "HH:mm:ss", "dd.MM.yyyy HH:mm:ss", "Europe/Berlin")
  
  let formatter1 = add_locale_format(formatter, create_locale("en", "US", None, None), en_us_format)
  let formatter2 = add_locale_format(formatter1, create_locale("zh", "CN", Some("Hans"), None), zh_cn_format)
  let formatter3 = add_locale_format(formatter2, create_locale("de", "DE", None, None), de_de_format)
  
  assert_eq(formatter3.formats.length(), 3)
  
  // Test date formatting
  let en_us_locale = create_locale("en", "US", None, None)
  let zh_cn_locale = create_locale("zh", "CN", Some("Hans"), None)
  let de_de_locale = create_locale("de", "DE", None, None)
  let fr_fr_locale = create_locale("fr", "FR", None, None)
  
  let en_us_date = format_date(formatter3, 2023, 1, 15, en_us_locale)
  assert_eq(en_us_date, "1/15/2023")
  
  let zh_cn_date = format_date(formatter3, 2023, 1, 15, zh_cn_locale)
  assert_eq(zh_cn_date, "2023年1月15日")
  
  let de_de_date = format_date(formatter3, 2023, 1, 15, de_de_locale)
  assert_eq(de_de_date, "15.1.2023")
  
  let fr_fr_date = format_date(formatter3, 2023, 1, 15, fr_fr_locale)
  assert_eq(fr_fr_date, "15/1/2023")
  
  // Test time formatting
  let en_us_time = format_time(formatter3, 14, 30, 45, en_us_locale)
  assert_eq(en_us_time, "2:30:45 PM")
  
  let zh_cn_time = format_time(formatter3, 14, 30, 45, zh_cn_locale)
  assert_eq(zh_cn_time, "14:30:45")
  
  let de_de_time = format_time(formatter3, 14, 30, 45, de_de_locale)
  assert_eq(de_de_time, "14:30:45")
  
  // Test date/time formatting
  let en_us_datetime = format_date_time(formatter3, 2023, 1, 15, 14, 30, 45, en_us_locale)
  assert_eq(en_us_datetime, "1/15/2023 2:30:45 PM")
  
  let zh_cn_datetime = format_date_time(formatter3, 2023, 1, 15, 14, 30, 45, zh_cn_locale)
  assert_eq(zh_cn_datetime, "2023年1月15日 14:30:45")
  
  let fr_fr_datetime = format_date_time(formatter3, 2023, 1, 15, 14, 30, 45, fr_fr_locale)
  assert_eq(fr_fr_datetime, "15/1/2023 à 14:30:45")
}

// Test 4: Number Formatting
test "number formatting for different locales" {
  // Define number formatting types
  type NumberFormat = {
    decimal_separator: String,
    thousands_separator: String,
    currency_symbol: String,
    currency_before: Bool,
    percent_symbol: String
  }
  
  type NumberFormatter = {
    formats: Array[(Locale, NumberFormat)],
    default_format: NumberFormat
  }
  
  // Number formatting operations
  let create_number_format = fn(decimal_separator: String, thousands_separator: String, 
                               currency_symbol: String, currency_before: Bool, percent_symbol: String) {
    {
      decimal_separator,
      thousands_separator,
      currency_symbol,
      currency_before,
      percent_symbol
    }
  }
  
  let create_number_formatter = fn(default_format: NumberFormat) {
    {
      formats: [],
      default_format
    }
  }
  
  let add_locale_number_format = fn(formatter: NumberFormatter, locale: Locale, format: NumberFormat) {
    { formatter | formats: formatter.formats.push((locale, format)) }
  }
  
  let get_number_format = fn(formatter: NumberFormatter, locale: Locale) {
    match formatter.formats.find(fn(pair) { 
      pair.0.language == locale.language and 
      pair.0.region == locale.region 
    }) {
      Some((_, format)) => format
      None => formatter.default_format
    }
  }
  
  let format_number = fn(formatter: NumberFormatter, number: Float, decimal_places: Int, locale: Locale) {
    let format = get_number_format(formatter, locale)
    
    // Round to specified decimal places
    let multiplier = 10.0.pow(decimal_places)
    let rounded = (number * multiplier).round() / multiplier
    
    // Split into integer and decimal parts
    let integer_part = rounded.floor().to_int()
    let decimal_part = ((rounded - integer_part.to_float()) * multiplier).round().to_int()
    
    // Format integer part with thousands separator
    let integer_str = integer_part.to_string()
    let mut formatted_integer = ""
    
    for i in 0..integer_str.length() {
      let pos_from_right = integer_str.length() - i
      
      formatted_integer = formatted_integer + integer_str[i].to_string()
      
      // Add thousands separator every 3 digits (except at the end)
      if pos_from_right > 1 and pos_from_right % 3 == 1 {
        formatted_integer = formatted_integer + format.thousands_separator
      }
    }
    
    // Combine with decimal part
    if decimal_places > 0 {
      formatted_integer + format.decimal_separator + decimal_part.to_string()
    } else {
      formatted_integer
    }
  }
  
  let format_currency = fn(formatter: NumberFormatter, amount: Float, locale: Locale) {
    let format = get_number_format(formatter, locale)
    let formatted_number = format_number(formatter, amount, 2, locale)
    
    if format.currency_before {
      format.currency_symbol + formatted_number
    } else {
      formatted_number + " " + format.currency_symbol
    }
  }
  
  let format_percent = fn(formatter: NumberFormatter, number: Float, locale: Locale) {
    let format = get_number_format(formatter, locale)
    let percent_value = number * 100.0
    let formatted_number = format_number(formatter, percent_value, 1, locale)
    
    formatted_number + format.percent_symbol
  }
  
  // Test number formatter creation
  let default_format = create_number_format(".", ",", "$", true, "%")
  let formatter = create_number_formatter(default_format)
  
  assert_eq(formatter.default_format.decimal_separator, ".")
  assert_eq(formatter.default_format.thousands_separator, ",")
  assert_eq(formatter.default_format.currency_symbol, "$")
  
  // Test adding locale formats
  let en_us_format = create_number_format(".", ",", "$", true, "%")
  let de_de_format = create_number_format(",", ".", "€", false, "%")
  let fr_fr_format = create_number_format(",", " ", "€", false, "%")
  let zh_cn_format = create_number_format(".", ",", "¥", true, "%")
  
  let formatter1 = add_locale_number_format(formatter, create_locale("en", "US", None, None), en_us_format)
  let formatter2 = add_locale_number_format(formatter1, create_locale("de", "DE", None, None), de_de_format)
  let formatter3 = add_locale_number_format(formatter2, create_locale("fr", "FR", None, None), fr_fr_format)
  let formatter4 = add_locale_number_format(formatter3, create_locale("zh", "CN", Some("Hans"), None), zh_cn_format)
  
  assert_eq(formatter4.formats.length(), 4)
  
  // Test number formatting
  let en_us_locale = create_locale("en", "US", None, None)
  let de_de_locale = create_locale("de", "DE", None, None)
  let fr_fr_locale = create_locale("fr", "FR", None, None)
  let zh_cn_locale = create_locale("zh", "CN", Some("Hans"), None)
  
  let en_us_number = format_number(formatter4, 1234567.89, 2, en_us_locale)
  assert_eq(en_us_number, "1,234,567.89")
  
  let de_de_number = format_number(formatter4, 1234567.89, 2, de_de_locale)
  assert_eq(de_de_number, "1.234.567,89")
  
  let fr_fr_number = format_number(formatter4, 1234567.89, 2, fr_fr_locale)
  assert_eq(fr_fr_number, "1 234 567,89")
  
  // Test currency formatting
  let en_us_currency = format_currency(formatter4, 1234.56, en_us_locale)
  assert_eq(en_us_currency, "$1,234.56")
  
  let de_de_currency = format_currency(formatter4, 1234.56, de_de_locale)
  assert_eq(de_de_currency, "1.234,56 €")
  
  let zh_cn_currency = format_currency(formatter4, 1234.56, zh_cn_locale)
  assert_eq(zh_cn_currency, "¥1,234.56")
  
  // Test percent formatting
  let en_us_percent = format_percent(formatter4, 0.2543, en_us_locale)
  assert_eq(en_us_percent, "25.4%")
  
  let de_de_percent = format_percent(formatter4, 0.2543, de_de_locale)
  assert_eq(de_de_percent, "25,4%")
}

// Test 5: Text Direction and Layout
test "text direction and layout for RTL languages" {
  // Define text direction types
  type TextDirection = 
    | LeftToRight
    | RightToLeft
    | TopToBottom
  
  type LayoutSettings = {
    text_direction: TextDirection,
    alignment: String, // "left", "right", "center"
    margin_start: Int,
    margin_end: Int,
    padding_start: Int,
    padding_end: Int
  }
  
  type LayoutManager = {
    layouts: Array[(Locale, LayoutSettings)],
    default_layout: LayoutSettings
  }
  
  // Layout operations
  let create_layout_settings = fn(text_direction: TextDirection, alignment: String, 
                                margin_start: Int, margin_end: Int, 
                                padding_start: Int, padding_end: Int) {
    {
      text_direction,
      alignment,
      margin_start,
      margin_end,
      padding_start,
      padding_end
    }
  }
  
  let create_layout_manager = fn(default_layout: LayoutSettings) {
    {
      layouts: [],
      default_layout
    }
  }
  
  let add_locale_layout = fn(manager: LayoutManager, locale: Locale, layout: LayoutSettings) {
    { manager | layouts = manager.layouts.push((locale, layout)) }
  }
  
  let get_layout = fn(manager: LayoutManager, locale: Locale) {
    match manager.layouts.find(fn(pair) { 
      pair.0.language == locale.language and 
      (pair.0.region == locale.region or pair.0.region == "")
    }) {
      Some((_, layout)) => layout
      None => manager.default_layout
    }
  }
  
  let apply_text_direction = fn(text: String, direction: TextDirection) {
    match direction {
      LeftToRight => text
      RightToLeft => {
        // Reverse the text for RTL (simplified)
        let mut reversed = ""
        for i in 0..text.length() {
          reversed = text[i].to_string() + reversed
        }
        reversed
      }
      TopToBottom => text // Simplified
    }
  }
  
  let format_mixed_direction_text = fn(manager: LayoutManager, locale: Locale, 
                                       primary_text: String, secondary_text: String) {
    let layout = get_layout(manager, locale)
    
    match layout.text_direction {
      LeftToRight => primary_text + " " + secondary_text
      RightToLeft => secondary_text + " " + primary_text
      TopToBottom => primary_text + "\n" + secondary_text // Simplified
    }
  }
  
  // Test layout manager creation
  let default_layout = create_layout_settings(LeftToRight, "left", 10, 10, 5, 5)
  let manager = create_layout_manager(default_layout)
  
  assert_eq(manager.default_layout.text_direction, LeftToRight)
  assert_eq(manager.default_layout.alignment, "left")
  
  // Test adding locale layouts
  let ar_layout = create_layout_settings(RightToLeft, "right", 15, 15, 8, 8)
  let ja_layout = create_layout_settings(LeftToRight, "center", 12, 12, 6, 6)
  
  let manager1 = add_locale_layout(manager, create_locale("ar", "SA", None, None), ar_layout)
  let manager2 = add_locale_layout(manager1, create_locale("ja", "JP", None, None), ja_layout)
  
  assert_eq(manager2.layouts.length(), 2)
  
  // Test getting layout
  let en_us_locale = create_locale("en", "US", None, None)
  let ar_sa_locale = create_locale("ar", "SA", None, None)
  let ja_jp_locale = create_locale("ja", "JP", None, None)
  
  let en_us_layout = get_layout(manager2, en_us_locale)
  assert_eq(en_us_layout.text_direction, LeftToRight)
  assert_eq(en_us_layout.alignment, "left")
  
  let ar_sa_layout = get_layout(manager2, ar_sa_locale)
  assert_eq(ar_sa_layout.text_direction, RightToLeft)
  assert_eq(ar_sa_layout.alignment, "right")
  
  let ja_jp_layout = get_layout(manager2, ja_jp_locale)
  assert_eq(ja_jp_layout.text_direction, LeftToRight)
  assert_eq(ja_jp_layout.alignment, "center")
  
  // Test text direction application
  let ltr_text = "Hello World"
  let rtl_text = apply_text_direction(ltr_text, RightToLeft)
  assert_eq(rtl_text, "dlroW olleH")
  
  // Test mixed direction text formatting
  let en_mixed = format_mixed_direction_text(manager2, en_us_locale, "Hello", "World")
  assert_eq(en_mixed, "Hello World")
  
  let ar_mixed = format_mixed_direction_text(manager2, ar_sa_locale, "مرحبا", "بالعالم")
  assert_eq(ar_mixed, "بالعالم مرحبا")
}

// Test 6: Cultural Adaptation
test "cultural adaptation for telemetry system" {
  // Define cultural adaptation types
  type CulturalSettings = {
    date_first_day_of_week: Int, // 0 = Sunday, 1 = Monday, etc.
    time_format_24h: Bool,
    measurement_system: String, // "metric" or "imperial"
    paper_size: String, // "A4", "Letter", etc.
    currency_decimal_places: Int
  }
  
  type CulturalAdapter = {
    settings: Array[(Locale, CulturalSettings)],
    default_settings: CulturalSettings
  }
  
  // Cultural adaptation operations
  let create_cultural_settings = fn(date_first_day_of_week: Int, time_format_24h: Bool, 
                                   measurement_system: String, paper_size: String, 
                                   currency_decimal_places: Int) {
    {
      date_first_day_of_week,
      time_format_24h,
      measurement_system,
      paper_size,
      currency_decimal_places
    }
  }
  
  let create_cultural_adapter = fn(default_settings: CulturalSettings) {
    {
      settings: [],
      default_settings
    }
  }
  
  let add_locale_cultural_settings = fn(adapter: CulturalAdapter, locale: Locale, settings: CulturalSettings) {
    { adapter | settings = adapter.settings.push((locale, settings)) }
  }
  
  let get_cultural_settings = fn(adapter: CulturalAdapter, locale: Locale) {
    match adapter.settings.find(fn(pair) { 
      pair.0.language == locale.language and 
      (pair.0.region == locale.region or pair.0.region == "")
    }) {
      Some((_, settings)) => settings
      None => adapter.default_settings
    }
  }
  
  let format_measurement = fn(adapter: CulturalAdapter, value: Float, unit: String, locale: Locale) {
    let settings = get_cultural_settings(adapter, locale)
    
    match (settings.measurement_system, unit) {
      ("metric", "kg") => value.to_string() + " kg"
      ("metric", "m") => value.to_string() + " m"
      ("metric", "km") => value.to_string() + " km"
      ("imperial", "kg") => (value * 2.20462).to_string() + " lbs"
      ("imperial", "m") => (value * 3.28084).to_string() + " ft"
      ("imperial", "km") => (value * 0.621371).to_string() + " miles"
      _ => value.to_string() + " " + unit
    }
  }
  
  let format_weekday = fn(adapter: CulturalAdapter, day_index: Int, locale: Locale) {
    let settings = get_cultural_settings(adapter, locale)
    let adjusted_index = (day_index + settings.date_first_day_of_week) % 7
    
    let day_names = match locale.language {
      "en" => ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]
      "zh" => ["星期日", "星期一", "星期二", "星期三", "星期四", "星期五", "星期六"]
      "es" => ["domingo", "lunes", "martes", "miércoles", "jueves", "viernes", "sábado"]
      "ar" => ["الأحد", "الاثنين", "الثلاثاء", "الأربعاء", "الخميس", "الجمعة", "السبت"]
      _ => ["Day 0", "Day 1", "Day 2", "Day 3", "Day 4", "Day 5", "Day 6"]
    }
    
    day_names[adjusted_index]
  }
  
  // Test cultural adapter creation
  let default_settings = create_cultural_settings(0, true, "metric", "A4", 2)
  let adapter = create_cultural_adapter(default_settings)
  
  assert_eq(adapter.default_settings.date_first_day_of_week, 0)
  assert_eq(adapter.default_settings.time_format_24h, true)
  assert_eq(adapter.default_settings.measurement_system, "metric")
  
  // Test adding locale cultural settings
  let us_settings = create_cultural_settings(0, false, "imperial", "Letter", 2)
  let uk_settings = create_cultural_settings(1, true, "imperial", "A4", 2)
  let cn_settings = create_cultural_settings(1, true, "metric", "A4", 2)
  
  let adapter1 = add_locale_cultural_settings(adapter, create_locale("en", "US", None, None), us_settings)
  let adapter2 = add_locale_cultural_settings(adapter1, create_locale("en", "GB", None, None), uk_settings)
  let adapter3 = add_locale_cultural_settings(adapter2, create_locale("zh", "CN", Some("Hans"), None), cn_settings)
  
  assert_eq(adapter3.settings.length(), 3)
  
  // Test getting cultural settings
  let en_us_locale = create_locale("en", "US", None, None)
  let en_gb_locale = create_locale("en", "GB", None, None)
  let zh_cn_locale = create_locale("zh", "CN", Some("Hans"), None)
  
  let en_us_cultural = get_cultural_settings(adapter3, en_us_locale)
  assert_eq(en_us_cultural.date_first_day_of_week, 0) // Sunday
  assert_eq(en_us_cultural.time_format_24h, false) // 12-hour format
  assert_eq(en_us_cultural.measurement_system, "imperial")
  assert_eq(en_us_cultural.paper_size, "Letter")
  
  let en_gb_cultural = get_cultural_settings(adapter3, en_gb_locale)
  assert_eq(en_gb_cultural.date_first_day_of_week, 1) // Monday
  assert_eq(en_gb_cultural.time_format_24h, true) // 24-hour format
  assert_eq(en_gb_cultural.measurement_system, "imperial")
  assert_eq(en_gb_cultural.paper_size, "A4")
  
  let zh_cn_cultural = get_cultural_settings(adapter3, zh_cn_locale)
  assert_eq(zh_cn_cultural.date_first_day_of_week, 1) // Monday
  assert_eq(zh_cn_cultural.time_format_24h, true) // 24-hour format
  assert_eq(zh_cn_cultural.measurement_system, "metric")
  assert_eq(zh_cn_cultural.paper_size, "A4")
  
  // Test measurement formatting
  let us_weight = format_measurement(adapter3, 70.0, "kg", en_us_locale)
  assert_eq(us_weight, "154.3234 lbs") // Converted to imperial
  
  let cn_weight = format_measurement(adapter3, 70.0, "kg", zh_cn_locale)
  assert_eq(cn_weight, "70.0 kg") // Stays metric
  
  let us_distance = format_measurement(adapter3, 100.0, "km", en_us_locale)
  assert_eq(us_distance, "62.1371 miles") // Converted to imperial
  
  let cn_distance = format_measurement(adapter3, 100.0, "km", zh_cn_locale)
  assert_eq(cn_distance, "100.0 km") // Stays metric
  
  // Test weekday formatting
  let en_us_monday = format_weekday(adapter3, 1, en_us_locale) // Monday (index 1)
  assert_eq(en_us_monday, "Monday") // Sunday is first day, so index 1 is Monday
  
  let en_gb_monday = format_weekday(adapter3, 1, en_gb_locale) // Monday (index 1)
  assert_eq(en_gb_monday, "Sunday") // Monday is first day, so index 1 is Sunday
  
  let zh_monday = format_weekday(adapter3, 1, zh_cn_locale) // Monday (index 1)
  assert_eq(zh_monday, "星期一") // Monday is first day, so index 1 is Monday
}

// Test 7: Collation and Sorting
test "collation and sorting for different locales" {
  // Define collation types
  type CollationRules = {
    case_sensitive: Bool,
    ignore_accents: Bool,
    numeric_sorting: Bool,
    custom_ordering: Array[(String, Int)] // (character, order)
  }
  
  type Collator = {
    rules: Array[(Locale, CollationRules)],
    default_rules: CollationRules
  }
  
  // Collation operations
  let create_collation_rules = fn(case_sensitive: Bool, ignore_accents: Bool, 
                                numeric_sorting: Bool, custom_ordering: Array[(String, Int)]) {
    {
      case_sensitive,
      ignore_accents,
      numeric_sorting,
      custom_ordering
    }
  }
  
  let create_collator = fn(default_rules: CollationRules) {
    {
      rules: [],
      default_rules
    }
  }
  
  let add_locale_collation_rules = fn(collator: Collator, locale: Locale, rules: CollationRules) {
    { collator | rules = collator.rules.push((locale, rules)) }
  }
  
  let get_collation_rules = fn(collator: Collator, locale: Locale) {
    match collator.rules.find(fn(pair) { 
      pair.0.language == locale.language and 
      (pair.0.region == locale.region or pair.0.region == "")
    }) {
      Some((_, rules)) => rules
      None => collator.default_rules
    }
  }
  
  let compare_strings = fn(collator: Collator, str1: String, str2: String, locale: Locale) {
    let rules = get_collation_rules(collator, locale)
    
    let s1 = if not(rules.case_sensitive) { str1.to_lowercase() } else { str1 }
    let s2 = if not(rules.case_sensitive) { str2.to_lowercase() } else { str2 }
    
    // Simplified comparison
    if s1 < s2 {
      -1
    } else if s1 > s2 {
      1
    } else {
      0
    }
  }
  
  let sort_strings = fn(collator: Collator, strings: Array[String], locale: Locale) {
    let mut sorted = strings
    
    // Simple bubble sort for demonstration
    for i in 0..sorted.length() {
      for j in i+1..sorted.length() {
        if compare_strings(collator, sorted[j], sorted[i], locale) < 0 {
          let temp = sorted[i]
          sorted = sorted.set(i, sorted[j])
          sorted = sorted.set(j, temp)
        }
      }
    }
    
    sorted
  }
  
  // Test collator creation
  let default_rules = create_collation_rules(true, false, false, [])
  let collator = create_collator(default_rules)
  
  assert_eq(collator.default_rules.case_sensitive, true)
  assert_eq(collator.default_rules.ignore_accents, false)
  
  // Test adding locale collation rules
  let en_rules = create_collation_rules(false, false, false, [])
  let sv_rules = create_collation_rules(false, false, false, [("å", 27), ("ä", 28), ("ö", 29)])
  
  let collator1 = add_locale_collation_rules(collator, create_locale("en", "US", None, None), en_rules)
  let collator2 = add_locale_collation_rules(collator1, create_locale("sv", "SE", None, None), sv_rules)
  
  assert_eq(collator2.rules.length(), 2)
  
  // Test string comparison
  let en_us_locale = create_locale("en", "US", None, None)
  let sv_se_locale = create_locale("sv", "SE", None, None)
  
  let en_comparison = compare_strings(collator2, "Apple", "banana", en_us_locale)
  assert_eq(en_comparison, -1) // "Apple" < "banana" (case insensitive)
  
  let sv_comparison = compare_strings(collator2, "Äpple", "banana", sv_se_locale)
  assert_eq(sv_comparison, 1) // "Äpple" > "banana" (Ä comes after Z in Swedish)
  
  // Test string sorting
  let test_strings = ["zebra", "Apple", "banana", "Örange"]
  
  let en_sorted = sort_strings(collator2, test_strings, en_us_locale)
  assert_eq(en_sorted, ["Apple", "banana", "Örange", "zebra"])
  
  let sv_sorted = sort_strings(collator2, test_strings, sv_se_locale)
  assert_eq(sv_sorted, ["Apple", "banana", "zebra", "Örange"]) // Ö comes after Z in Swedish
}

// Test 8: Pluralization and Gender
test "pluralization and gender rules for different languages" {
  // Define pluralization types
  type PluralRule = 
    | Zero
    | One
    | Two
    | Few
    | Many
    | Other
  
  type Gender = 
    | Masculine
    | Feminine
    | Neuter
  
  type PluralizationRules = {
    rules: Array[(PluralRule, String)],
    gender_rules: Array[(Gender, String)]
  }
  
  type Pluralizer = {
    rules: Array[(Locale, PluralizationRules)],
    default_rules: PluralizationRules
  }
  
  // Pluralization operations
  let create_pluralization_rules = fn(rules: Array[(PluralRule, String)], 
                                     gender_rules: Array[(Gender, String)]) {
    {
      rules,
      gender_rules
    }
  }
  
  let create_pluralizer = fn(default_rules: PluralizationRules) {
    {
      rules: [],
      default_rules
    }
  }
  
  let add_locale_pluralization_rules = fn(pluralizer: Pluralizer, locale: Locale, rules: PluralizationRules) {
    { pluralizer | rules = pluralizer.rules.push((locale, rules)) }
  }
  
  let get_pluralization_rules = fn(pluralizer: Pluralizer, locale: Locale) {
    match pluralizer.rules.find(fn(pair) { 
      pair.0.language == locale.language and 
      (pair.0.region == locale.region or pair.0.region == "")
    }) {
      Some((_, rules)) => rules
      None => pluralizer.default_rules
    }
  }
  
  let get_plural_rule = fn(count: Int, locale: Locale) {
    match locale.language {
      "en" => {
        if count == 1 {
          One
        } else {
          Other
        }
      }
      "zh" => Other // Chinese doesn't have plural forms
      "fr" => {
        if count == 0 or count == 1 {
          One
        } else {
          Other
        }
      }
      "ar" => {
        if count == 0 {
          Zero
        } else if count == 1 {
          One
        } else if count == 2 {
          Two
        } else if count >= 3 and count <= 10 {
          Few
        } else {
          Many
        }
      }
      _ => Other
    }
  }
  
  let pluralize = fn(pluralizer: Pluralizer, key: String, count: Int, locale: Locale) {
    let rules = get_pluralization_rules(pluralizer, locale)
    let plural_rule = get_plural_rule(count, locale)
    
    match rules.rules.find(fn(pair) { pair.0 == plural_rule }) {
      Some((_, template)) => {
        template.replace("{count}", count.to_string())
      }
      None => key + " (" + count.to_string() + ")"
    }
  }
  
  let apply_gender = fn(pluralizer: Pluralizer, key: String, gender: Gender, locale: Locale) {
    let rules = get_pluralization_rules(pluralizer, locale)
    
    match rules.gender_rules.find(fn(pair) { pair.0 == gender }) {
      Some((_, template)) => template
      None => key
    }
  }
  
  // Test pluralizer creation
  let default_rules = create_pluralization_rules(
    [(Other, "{count} items")],
    [(Masculine, "male"), (Feminine, "female"), (Neuter, "neutral")]
  )
  let pluralizer = create_pluralizer(default_rules)
  
  assert_eq(pluralizer.default_rules.rules.length(), 1)
  assert_eq(pluralizer.default_rules.gender_rules.length(), 3)
  
  // Test adding locale pluralization rules
  let en_rules = create_pluralization_rules(
    [(One, "{count} item"), (Other, "{count} items")],
    [(Masculine, "his"), (Feminine, "her"), (Neuter, "its")]
  )
  
  let zh_rules = create_pluralization_rules(
    [(Other, "{count} 项")],
    [(Masculine, "他的"), (Feminine, "她的"), (Neuter, "它的")]
  )
  
  let ar_rules = create_pluralization_rules(
    [
      (Zero, "لا شيء"),
      (One, "واحد"),
      (Two, "اثنان"),
      (Few, "{count} قليل"),
      (Many, "{count} الكثير"),
      (Other, "{count} أخرى")
    ],
    [(Masculine, "خاص به"), (Feminine, "خاصة بها"), (Neuter, "خاص بهما")]
  )
  
  let pluralizer1 = add_locale_pluralization_rules(pluralizer, create_locale("en", "US", None, None), en_rules)
  let pluralizer2 = add_locale_pluralization_rules(pluralizer1, create_locale("zh", "CN", Some("Hans"), None), zh_rules)
  let pluralizer3 = add_locale_pluralization_rules(pluralizer2, create_locale("ar", "SA", None, None), ar_rules)
  
  assert_eq(pluralizer3.rules.length(), 3)
  
  // Test pluralization
  let en_us_locale = create_locale("en", "US", None, None)
  let zh_cn_locale = create_locale("zh", "CN", Some("Hans"), None)
  let ar_sa_locale = create_locale("ar", "SA", None, None)
  
  let en_one = pluralize(pluralizer3, "item", 1, en_us_locale)
  assert_eq(en_one, "1 item")
  
  let en_many = pluralize(pluralizer3, "item", 5, en_us_locale)
  assert_eq(en_many, "5 items")
  
  let zh_one = pluralize(pluralizer3, "item", 1, zh_cn_locale)
  assert_eq(zh_one, "1 项")
  
  let zh_many = pluralize(pluralizer3, "item", 5, zh_cn_locale)
  assert_eq(zh_many, "5 项")
  
  let ar_zero = pluralize(pluralizer3, "item", 0, ar_sa_locale)
  assert_eq(ar_zero, "لا شيء")
  
  let ar_one = pluralize(pluralizer3, "item", 1, ar_sa_locale)
  assert_eq(ar_one, "واحد")
  
  let ar_two = pluralize(pluralizer3, "item", 2, ar_sa_locale)
  assert_eq(ar_two, "اثنان")
  
  let ar_few = pluralize(pluralizer3, "item", 5, ar_sa_locale)
  assert_eq(ar_few, "5 قليل")
  
  let ar_many = pluralize(pluralizer3, "item", 15, ar_sa_locale)
  assert_eq(ar_many, "15 الكثير")
  
  // Test gender application
  let en_masculine = apply_gender(pluralizer3, "possessive", Masculine, en_us_locale)
  assert_eq(en_masculine, "his")
  
  let en_feminine = apply_gender(pluralizer3, "possessive", Feminine, en_us_locale)
  assert_eq(en_feminine, "her")
  
  let zh_masculine = apply_gender(pluralizer3, "possessive", Masculine, zh_cn_locale)
  assert_eq(zh_masculine, "他的")
  
  let ar_masculine = apply_gender(pluralizer3, "possessive", Masculine, ar_sa_locale)
  assert_eq(ar_masculine, "خاص به")
}