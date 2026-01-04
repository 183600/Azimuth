// Azimuth Internationalization Comprehensive Support Tests
// This file contains comprehensive internationalization and localization tests

// Test 1: Multi-language Error Messages
test "multi-language error message handling and localization" {
  // Define localized error message
  type LocalizedError = {
    error_code: String,
    language: String,
    message: String,
    description: String,
    suggested_action: String
  }
  
  // Define error localization dictionary
  type ErrorLocalizationDict = {
    error_code: String,
    messages: Array<LocalizedError>
  }
  
  // Create error localization dictionaries
  let timeout_error_localizations = {
    error_code: "TIMEOUT_ERROR",
    messages: [
      {
        error_code: "TIMEOUT_ERROR",
        language: "en",
        message: "Request timeout occurred",
        description: "The operation took longer than the allowed time limit",
        suggested_action: "Try again or increase timeout value"
      },
      {
        error_code: "TIMEOUT_ERROR",
        language: "zh",
        message: "请求超时",
        description: "操作时间超过了允许的时间限制",
        suggested_action: "请重试或增加超时值"
      },
      {
        error_code: "TIMEOUT_ERROR",
        language: "es",
        message: "Se produjo un tiempo de espera de la solicitud",
        description: "La operación tardó más que el límite de tiempo permitido",
        suggested_action: "Inténtelo de nuevo o aumente el valor de tiempo de espera"
      },
      {
        error_code: "TIMEOUT_ERROR",
        language: "fr",
        message: "Délai d'attente de la demande",
        description: "L'opération a pris plus de temps que la limite autorisée",
        suggested_action: "Réessayez ou augmentez la valeur du délai d'attente"
      },
      {
        error_code: "TIMEOUT_ERROR",
        language: "ja",
        message: "リクエストタイムアウトが発生しました",
        description: "操作が許可された時間制限を超えました",
        suggested_action: "再試行するか、タイムアウト値を増やしてください"
      }
    ]
  }
  
  let auth_error_localizations = {
    error_code: "AUTH_ERROR",
    messages: [
      {
        error_code: "AUTH_ERROR",
        language: "en",
        message: "Authentication failed",
        description: "Invalid credentials provided",
        suggested_action: "Check your username and password"
      },
      {
        error_code: "AUTH_ERROR",
        language: "zh",
        message: "身份验证失败",
        description: "提供的凭据无效",
        suggested_action: "请检查您的用户名和密码"
      },
      {
        error_code: "AUTH_ERROR",
        language: "es",
        message: "Error de autenticación",
        description: "Se proporcionaron credenciales no válidas",
        suggested_action: "Verifique su nombre de usuario y contraseña"
      },
      {
        error_code: "AUTH_ERROR",
        language: "fr",
        message: "Échec de l'authentification",
        description: "Identifiants non valides fournis",
        suggested_action: "Vérifiez votre nom d'utilisateur et votre mot de passe"
      },
      {
        error_code: "AUTH_ERROR",
        language: "ja",
        message: "認証に失敗しました",
        description: "無効な資格情報が提供されました",
        suggested_action: "ユーザー名とパスワードを確認してください"
      }
    ]
  }
  
  let localization_dicts = [timeout_error_localizations, auth_error_localizations]
  
  // Get localized error message
  let get_localized_error = fn(error_code: String, language: String, dicts: Array<ErrorLocalizationDict>) {
    for dict in dicts {
      if dict.error_code == error_code {
        for localized_msg in dict.messages {
          if localized_msg.language == language {
            return Some(localized_msg)
          }
        }
        // Return English as fallback
        for localized_msg in dict.messages {
          if localized_msg.language == "en" {
            return Some(localized_msg)
          }
        }
      }
    }
    None
  }
  
  // Test error localization
  let timeout_en = get_localized_error("TIMEOUT_ERROR", "en", localization_dicts)
  let timeout_zh = get_localized_error("TIMEOUT_ERROR", "zh", localization_dicts)
  let timeout_es = get_localized_error("TIMEOUT_ERROR", "es", localization_dicts)
  let timeout_fr = get_localized_error("TIMEOUT_ERROR", "fr", localization_dicts)
  let timeout_ja = get_localized_error("TIMEOUT_ERROR", "ja", localization_dicts)
  
  // Verify English timeout error
  assert_true(timeout_en.length > 0)
  let timeout_en_msg = timeout_en.get
  assert_eq(timeout_en_msg.error_code, "TIMEOUT_ERROR")
  assert_eq(timeout_en_msg.language, "en")
  assert_eq(timeout_en_msg.message, "Request timeout occurred")
  assert_eq(timeout_en_msg.description, "The operation took longer than the allowed time limit")
  assert_eq(timeout_en_msg.suggested_action, "Try again or increase timeout value")
  
  // Verify Chinese timeout error
  assert_true(timeout_zh.length > 0)
  let timeout_zh_msg = timeout_zh.get
  assert_eq(timeout_zh_msg.error_code, "TIMEOUT_ERROR")
  assert_eq(timeout_zh_msg.language, "zh")
  assert_eq(timeout_zh_msg.message, "请求超时")
  assert_eq(timeout_zh_msg.description, "操作时间超过了允许的时间限制")
  assert_eq(timeout_zh_msg.suggested_action, "请重试或增加超时值")
  
  // Verify Spanish timeout error
  assert_true(timeout_es.length > 0)
  let timeout_es_msg = timeout_es.get
  assert_eq(timeout_es_msg.error_code, "TIMEOUT_ERROR")
  assert_eq(timeout_es_msg.language, "es")
  assert_eq(timeout_es_msg.message, "Se produjo un tiempo de espera de la solicitud")
  assert_eq(timeout_es_msg.description, "La operación tardó más que el límite de tiempo permitido")
  assert_eq(timeout_es_msg.suggested_action, "Inténtelo de nuevo o aumente el valor de tiempo de espera")
  
  // Verify French timeout error
  assert_true(timeout_fr.length > 0)
  let timeout_fr_msg = timeout_fr.get
  assert_eq(timeout_fr_msg.error_code, "TIMEOUT_ERROR")
  assert_eq(timeout_fr_msg.language, "fr")
  assert_eq(timeout_fr_msg.message, "Délai d'attente de la demande")
  assert_eq(timeout_fr_msg.description, "L'opération a pris plus de temps que la limite autorisée")
  assert_eq(timeout_fr_msg.suggested_action, "Réessayez ou augmentez la valeur du délai d'attente")
  
  // Verify Japanese timeout error
  assert_true(timeout_ja.length > 0)
  let timeout_ja_msg = timeout_ja.get
  assert_eq(timeout_ja_msg.error_code, "TIMEOUT_ERROR")
  assert_eq(timeout_ja_msg.language, "ja")
  assert_eq(timeout_ja_msg.message, "リクエストタイムアウトが発生しました")
  assert_eq(timeout_ja_msg.description, "操作が許可された時間制限を超えました")
  assert_eq(timeout_ja_msg.suggested_action, "再試行するか、タイムアウト値を増やしてください")
  
  // Test authentication error localization
  let auth_zh = get_localized_error("AUTH_ERROR", "zh", localization_dicts)
  assert_true(auth_zh.length > 0)
  let auth_zh_msg = auth_zh.get
  assert_eq(auth_zh_msg.error_code, "AUTH_ERROR")
  assert_eq(auth_zh_msg.language, "zh")
  assert_eq(auth_zh_msg.message, "身份验证失败")
  assert_eq(auth_zh_msg.description, "提供的凭据无效")
  assert_eq(auth_zh_msg.suggested_action, "请检查您的用户名和密码")
  
  // Test fallback language (English)
  let timeout_unknown = get_localized_error("TIMEOUT_ERROR", "de", localization_dicts)
  assert_true(timeout_unknown.length > 0)
  let timeout_unknown_msg = timeout_unknown.get
  assert_eq(timeout_unknown_msg.language, "en")  # Should fallback to English
  
  // Test non-existent error
  let non_existent = get_localized_error("UNKNOWN_ERROR", "en", localization_dicts)
  assert_true(non_existent.length == 0)
}

// Test 2: Locale-specific Date and Time Formatting
test "locale-specific date and time formatting for telemetry" {
  // Define date formatting options
  type DateFormatOptions = {
    locale: String,
    date_style: String,  # "short", "medium", "long", "full"
    time_style: String,  # "short", "medium", "long", "full"
    timezone: String
  }
  
  // Define formatted date/time
  type FormattedDateTime = {
    raw_timestamp: Int,
    formatted_string: String,
    locale: String,
    format_options: DateFormatOptions
  }
  
  // Format timestamp according to locale
  let format_timestamp = fn(timestamp: Int, options: DateFormatOptions) {
    // Simulate date/time formatting for different locales
    let date = timestamp / 86400  # Days since epoch
    let time = timestamp % 86400  # Seconds in day
    let hours = time / 3600
    let minutes = (time % 3600) / 60
    let seconds = time % 60
    
    match options.locale {
      "en-US" => {
        match (options.date_style, options.time_style) {
          ("short", "short") => "1/1/22, " + hours.to_string() + ":" + minutes.to_string() + " AM"
          ("medium", "medium") => "Jan 1, 2022, " + hours.to_string() + ":" + minutes.to_string() + ":" + seconds.to_string() + " AM"
          ("long", "short") => "January 1, 2022, " + hours.to_string() + ":" + minutes.to_string() + " AM"
          ("full", "full") => "Saturday, January 1, 2022, " + hours.to_string() + ":" + minutes.to_string() + ":" + seconds.to_string() + " AM " + options.timezone
          _ => "Jan 1, 2022, " + hours.to_string() + ":" + minutes.to_string() + " AM"
        }
      }
      "zh-CN" => {
        match (options.date_style, options.time_style) {
          ("short", "short") => "2022/1/1 " + hours.to_string() + ":" + minutes.to_string()
          ("medium", "medium") => "2022年1月1日 " + hours.to_string() + ":" + minutes.to_string() + ":" + seconds.to_string()
          ("long", "short") => "2022年1月1日 " + hours.to_string() + ":" + minutes.to_string()
          ("full", "full") => "2022年1月1日星期六 " + hours.to_string() + ":" + minutes.to_string() + ":" + seconds.to_string() + " " + options.timezone
          _ => "2022年1月1日 " + hours.to_string() + ":" + minutes.to_string()
        }
      }
      "es-ES" => {
        match (options.date_style, options.time_style) {
          ("short", "short") => "1/1/22 " + hours.to_string() + ":" + minutes.to_string()
          ("medium", "medium") => "1 ene. 2022, " + hours.to_string() + ":" + minutes.to_string() + ":" + seconds.to_string()
          ("long", "short") => "1 de enero de 2022, " + hours.to_string() + ":" + minutes.to_string()
          ("full", "full") => "sábado, 1 de enero de 2022, " + hours.to_string() + ":" + minutes.to_string() + ":" + seconds.to_string() + " " + options.timezone
          _ => "1 ene. 2022, " + hours.to_string() + ":" + minutes.to_string()
        }
      }
      "fr-FR" => {
        match (options.date_style, options.time_style) {
          ("short", "short") => "01/01/2022 " + hours.to_string() + ":" + minutes.to_string()
          ("medium", "medium") => "1 janv. 2022, " + hours.to_string() + ":" + minutes.to_string() + ":" + seconds.to_string()
          ("long", "short") => "1 janvier 2022, " + hours.to_string() + ":" + minutes.to_string()
          ("full", "full") => "samedi 1 janvier 2022, " + hours.to_string() + ":" + minutes.to_string() + ":" + seconds.to_string() + " " + options.timezone
          _ => "1 janv. 2022, " + hours.to_string() + ":" + minutes.to_string()
        }
      }
      "ja-JP" => {
        match (options.date_style, options.time_style) {
          ("short", "short") => "2022/01/01 " + hours.to_string() + ":" + minutes.to_string()
          ("medium", "medium") => "2022/01/01 " + hours.to_string() + ":" + minutes.to_string() + ":" + seconds.to_string()
          ("long", "short") => "2022年1月1日 " + hours.to_string() + ":" + minutes.to_string()
          ("full", "full") => "2022年1月1日土曜日 " + hours.to_string() + ":" + minutes.to_string() + ":" + seconds.to_string() + " " + options.timezone
          _ => "2022/01/01 " + hours.to_string() + ":" + minutes.to_string()
        }
      }
      _ => "2022-01-01 " + hours.to_string() + ":" + minutes.to_string()  # Default format
    }
  }
  
  // Test timestamp formatting
  let timestamp = 1640995200  # 2022-01-01 00:00:00 UTC
  
  // Test US English formatting
  let us_en_options = { locale: "en-US", date_style: "medium", time_style: "medium", timezone: "GMT" }
  let us_en_formatted = format_timestamp(timestamp, us_en_options)
  assert_eq(us_en_formatted, "Jan 1, 2022, 0:0:0 AM")
  
  // Test Chinese formatting
  let zh_cn_options = { locale: "zh-CN", date_style: "medium", time_style: "medium", timezone: "GMT" }
  let zh_cn_formatted = format_timestamp(timestamp, zh_cn_options)
  assert_eq(zh_cn_formatted, "2022年1月1日 0:0:0")
  
  // Test Spanish formatting
  let es_es_options = { locale: "es-ES", date_style: "medium", time_style: "medium", timezone: "GMT" }
  let es_es_formatted = format_timestamp(timestamp, es_es_options)
  assert_eq(es_es_formatted, "1 ene. 2022, 0:0:0")
  
  // Test French formatting
  let fr_fr_options = { locale: "fr-FR", date_style: "medium", time_style: "medium", timezone: "GMT" }
  let fr_fr_formatted = format_timestamp(timestamp, fr_fr_options)
  assert_eq(fr_fr_formatted, "1 janv. 2022, 0:0:0")
  
  // Test Japanese formatting
  let ja_jp_options = { locale: "ja-JP", date_style: "medium", time_style: "medium", timezone: "GMT" }
  let ja_jp_formatted = format_timestamp(timestamp, ja_jp_options)
  assert_eq(ja_jp_formatted, "2022/01/01 0:0:0")
  
  // Test different date/time styles
  let us_en_short_options = { locale: "en-US", date_style: "short", time_style: "short", timezone: "GMT" }
  let us_en_short_formatted = format_timestamp(timestamp, us_en_short_options)
  assert_eq(us_en_short_formatted, "1/1/22, 0:0 AM")
  
  let us_en_long_options = { locale: "en-US", date_style: "long", time_style: "short", timezone: "GMT" }
  let us_en_long_formatted = format_timestamp(timestamp, us_en_long_options)
  assert_eq(us_en_long_formatted, "January 1, 2022, 0:0 AM")
  
  let us_en_full_options = { locale: "en-US", date_style: "full", time_style: "full", timezone: "GMT" }
  let us_en_full_formatted = format_timestamp(timestamp, us_en_full_options)
  assert_eq(us_en_full_formatted, "Saturday, January 1, 2022, 0:0:0 AM GMT")
  
  // Test with different timestamps
  let afternoon_timestamp = 1641038400  # 2022-01-01 12:00:00 UTC
  let us_en_afternoon = format_timestamp(afternoon_timestamp, us_en_options)
  assert_eq(us_en_afternoon, "Jan 1, 2022, 12:0:0 PM")
  
  let zh_cn_afternoon = format_timestamp(afternoon_timestamp, zh_cn_options)
  assert_eq(zh_cn_afternoon, "2022年1月1日 12:0:0")
  
  // Test duration formatting across locales
  let format_duration = fn(duration_ms: Int, locale: String) {
    let seconds = duration_ms / 1000
    let minutes = seconds / 60
    let hours = minutes / 60
    let remaining_minutes = minutes % 60
    let remaining_seconds = seconds % 60
    
    match locale {
      "en-US" => {
        if hours > 0 {
          hours.to_string() + "h " + remaining_minutes.to_string() + "m " + remaining_seconds.to_string() + "s"
        } else if minutes > 0 {
          remaining_minutes.to_string() + "m " + remaining_seconds.to_string() + "s"
        } else {
          remaining_seconds.to_string() + "s"
        }
      }
      "zh-CN" => {
        if hours > 0 {
          hours.to_string() + "小时" + remaining_minutes.to_string() + "分" + remaining_seconds.to_string() + "秒"
        } else if minutes > 0 {
          remaining_minutes.to_string() + "分" + remaining_seconds.to_string() + "秒"
        } else {
          remaining_seconds.to_string() + "秒"
        }
      }
      "es-ES" => {
        if hours > 0 {
          hours.to_string() + "h " + remaining_minutes.to_string() + "m " + remaining_seconds.to_string() + "s"
        } else if minutes > 0 {
          remaining_minutes.to_string() + "m " + remaining_seconds.to_string() + "s"
        } else {
          remaining_seconds.to_string() + "s"
        }
      }
      "fr-FR" => {
        if hours > 0 {
          hours.to_string() + "h " + remaining_minutes.to_string() + "m " + remaining_seconds.to_string() + "s"
        } else if minutes > 0 {
          remaining_minutes.to_string() + "m " + remaining_seconds.to_string() + "s"
        } else {
          remaining_seconds.to_string() + "s"
        }
      }
      "ja-JP" => {
        if hours > 0 {
          hours.to_string() + "時間" + remaining_minutes.to_string() + "分" + remaining_seconds.to_string() + "秒"
        } else if minutes > 0 {
          remaining_minutes.to_string() + "分" + remaining_seconds.to_string() + "秒"
        } else {
          remaining_seconds.to_string() + "秒"
        }
      }
      _ => duration_ms.to_string() + "ms"  # Default
    }
  }
  
  // Test duration formatting
  let duration_1h30m45s = 5445000  # 1 hour, 30 minutes, 45 seconds in milliseconds
  
  let us_en_duration = format_duration(duration_1h30m45s, "en-US")
  assert_eq(us_en_duration, "1h 30m 45s")
  
  let zh_cn_duration = format_duration(duration_1h30m45s, "zh-CN")
  assert_eq(zh_cn_duration, "1小时30分45秒")
  
  let ja_jp_duration = format_duration(duration_1h30m45s, "ja-JP")
  assert_eq(ja_jp_duration, "1時間30分45秒")
  
  let duration_45s = 45000  # 45 seconds in milliseconds
  
  let us_en_duration_45s = format_duration(duration_45s, "en-US")
  assert_eq(us_en_duration_45s, "45s")
  
  let zh_cn_duration_45s = format_duration(duration_45s, "zh-CN")
  assert_eq(zh_cn_duration_45s, "45秒")
}

// Test 3: Number and Currency Formatting
test "number and currency formatting across locales" {
  // Define number formatting options
  type NumberFormatOptions = {
    locale: String,
    style: String,  # "decimal", "currency", "percent"
    minimum_fraction_digits: Int,
    maximum_fraction_digits: Int,
    currency: Option<String>
  }
  
  // Format number according to locale
  let format_number = fn(number: Float, options: NumberFormatOptions) {
    match options.locale {
      "en-US" => {
        match options.style {
          "decimal" => {
            # English uses period as decimal separator and comma as thousands separator
            let integer_part = number.to_int().to_string()
            let decimal_part = ((number - number.to_int().to_float()) * 100.0).to_int().to_string()
            integer_part + "." + decimal_part
          }
          "currency" => {
            match options.currency {
              Some("USD") => "$" + number.to_string()
              Some("EUR") => "€" + number.to_string()
              _ => "$" + number.to_string()
            }
          }
          "percent" => {
            (number * 100.0).to_string() + "%"
          }
          _ => number.to_string()
        }
      }
      "zh-CN" => {
        match options.style {
          "decimal" => {
            # Chinese doesn't typically use thousands separators
            number.to_string()
          }
          "currency" => {
            match options.currency {
              Some("CNY") => "¥" + number.to_string()
              Some("USD") => "US$" + number.to_string()
              _ => "¥" + number.to_string()
            }
          }
          "percent" => {
            (number * 100.0).to_string() + "%"
          }
          _ => number.to_string()
        }
      }
      "de-DE" => {
        match options.style {
          "decimal" => {
            # German uses comma as decimal separator and period as thousands separator
            let integer_part = number.to_int().to_string()
            let decimal_part = ((number - number.to_int().to_float()) * 100.0).to_int().to_string()
            integer_part + "," + decimal_part
          }
          "currency" => {
            match options.currency {
              Some("EUR") => number.to_string() + " €"
              Some("USD") => number.to_string() + " $"
              _ => number.to_string() + " €"
            }
          }
          "percent" => {
            (number * 100.0).to_string() + " %"
          }
          _ => number.to_string()
        }
      }
      "fr-FR" => {
        match options.style {
          "decimal" => {
            # French uses comma as decimal separator and space as thousands separator
            let integer_part = number.to_int().to_string()
            let decimal_part = ((number - number.to_int().to_float()) * 100.0).to_int().to_string()
            integer_part + "," + decimal_part
          }
          "currency" => {
            match options.currency {
              Some("EUR") => number.to_string() + " €"
              Some("USD") => number.to_string() + " $US"
              _ => number.to_string() + " €"
            }
          }
          "percent" => {
            (number * 100.0).to_string() + " %"
          }
          _ => number.to_string()
        }
      }
      "ja-JP" => {
        match options.style {
          "decimal" => {
            number.to_string()
          }
          "currency" => {
            match options.currency {
              Some("JPY") => "¥" + number.to_string()
              Some("USD") => "$" + number.to_string()
              _ => "¥" + number.to_string()
            }
          }
          "percent" => {
            (number * 100.0).to_string() + "%"
          }
          _ => number.to_string()
        }
      }
      _ => number.to_string()
    }
  }
  
  // Test decimal formatting
  let test_number = 1234.56
  
  let us_en_decimal = format_number(test_number, {
    locale: "en-US",
    style: "decimal",
    minimum_fraction_digits: 2,
    maximum_fraction_digits: 2,
    currency: None
  })
  assert_eq(us_en_decimal, "1234.56")
  
  let zh_cn_decimal = format_number(test_number, {
    locale: "zh-CN",
    style: "decimal",
    minimum_fraction_digits: 2,
    maximum_fraction_digits: 2,
    currency: None
  })
  assert_eq(zh_cn_decimal, "1234.56")
  
  let de_de_decimal = format_number(test_number, {
    locale: "de-DE",
    style: "decimal",
    minimum_fraction_digits: 2,
    maximum_fraction_digits: 2,
    currency: None
  })
  assert_eq(de_de_decimal, "1234,56")
  
  let fr_fr_decimal = format_number(test_number, {
    locale: "fr-FR",
    style: "decimal",
    minimum_fraction_digits: 2,
    maximum_fraction_digits: 2,
    currency: None
  })
  assert_eq(fr_fr_decimal, "1234,56")
  
  // Test currency formatting
  let us_en_usd = format_number(test_number, {
    locale: "en-US",
    style: "currency",
    minimum_fraction_digits: 2,
    maximum_fraction_digits: 2,
    currency: Some("USD")
  })
  assert_eq(us_en_usd, "$1234.56")
  
  let us_en_eur = format_number(test_number, {
    locale: "en-US",
    style: "currency",
    minimum_fraction_digits: 2,
    maximum_fraction_digits: 2,
    currency: Some("EUR")
  })
  assert_eq(us_en_eur, "€1234.56")
  
  let zh_cn_cny = format_number(test_number, {
    locale: "zh-CN",
    style: "currency",
    minimum_fraction_digits: 2,
    maximum_fraction_digits: 2,
    currency: Some("CNY")
  })
  assert_eq(zh_cn_cny, "¥1234.56")
  
  let de_de_eur = format_number(test_number, {
    locale: "de-DE",
    style: "currency",
    minimum_fraction_digits: 2,
    maximum_fraction_digits: 2,
    currency: Some("EUR")
  })
  assert_eq(de_de_eur, "1234,56 €")
  
  let fr_fr_eur = format_number(test_number, {
    locale: "fr-FR",
    style: "currency",
    minimum_fraction_digits: 2,
    maximum_fraction_digits: 2,
    currency: Some("EUR")
  })
  assert_eq(fr_fr_eur, "1234,56 €")
  
  let ja_jp_jpy = format_number(test_number, {
    locale: "ja-JP",
    style: "currency",
    minimum_fraction_digits: 0,
    maximum_fraction_digits: 0,
    currency: Some("JPY")
  })
  assert_eq(ja_jp_jpy, "¥1234.56")
  
  // Test percent formatting
  let test_percent = 0.2543
  
  let us_en_percent = format_number(test_percent, {
    locale: "en-US",
    style: "percent",
    minimum_fraction_digits: 2,
    maximum_fraction_digits: 2,
    currency: None
  })
  assert_eq(us_en_percent, "25.43%")
  
  let zh_cn_percent = format_number(test_percent, {
    locale: "zh-CN",
    style: "percent",
    minimum_fraction_digits: 2,
    maximum_fraction_digits: 2,
    currency: None
  })
  assert_eq(zh_cn_percent, "25.43%")
  
  let de_de_percent = format_number(test_percent, {
    locale: "de-DE",
    style: "percent",
    minimum_fraction_digits: 2,
    maximum_fraction_digits: 2,
    currency: None
  })
  assert_eq(de_de_percent, "25.43 %")
  
  // Test telemetry metric formatting
  let format_telemetry_metric = fn(value: Float, unit: String, locale: String) {
    let formatted_value = format_number(value, {
      locale: locale,
      style: "decimal",
      minimum_fraction_digits: 2,
      maximum_fraction_digits: 2,
      currency: None
    })
    
    match locale {
      "en-US" => formatted_value + " " + unit
      "zh-CN" => formatted_value + unit
      "de-DE" => formatted_value + " " + unit
      "fr-FR" => formatted_value + " " + unit
      "ja-JP" => formatted_value + unit
      _ => formatted_value + " " + unit
    }
  }
  
  // Test telemetry metric formatting
  let response_time = 245.67
  
  let us_en_response_time = format_telemetry_metric(response_time, "ms", "en-US")
  assert_eq(us_en_response_time, "245.67 ms")
  
  let zh_cn_response_time = format_telemetry_metric(response_time, "ms", "zh-CN")
  assert_eq(zh_cn_response_time, "245.67ms")
  
  let de_de_response_time = format_telemetry_metric(response_time, "ms", "de-DE")
  assert_eq(de_de_response_time, "245,67 ms")
  
  let memory_usage = 1024.5
  
  let us_en_memory = format_telemetry_metric(memory_usage, "MB", "en-US")
  assert_eq(us_en_memory, "1024.50 MB")
  
  let ja_jp_memory = format_telemetry_metric(memory_usage, "MB", "ja-JP")
  assert_eq(ja_jp_memory, "1024.50MB")
}

// Test 4: Right-to-Left (RTL) Language Support
test "right-to-left (RTL) language support for telemetry UI" {
  // Define text direction
  type TextDirection = {
    direction: String,  # "ltr", "rtl"
    language: String,
    alignment: String  # "left", "right", "center"
  }
  
  // Define localized UI text
  type LocalizedUIText = {
    key: String,
    text: String,
    direction: TextDirection
  }
  
  // Create UI text localizations
  let telemetry_ui_localizations = [
    # English (LTR)
    {
      key: "trace_id",
      text: "Trace ID",
      direction: { direction: "ltr", language: "en", alignment: "left" }
    },
    {
      key: "span_name",
      text: "Span Name",
      direction: { direction: "ltr", language: "en", alignment: "left" }
    },
    {
      key: "duration",
      text: "Duration",
      direction: { direction: "ltr", language: "en", alignment: "left" }
    },
    {
      key: "status",
      text: "Status",
      direction: { direction: "ltr", language: "en", alignment: "left" }
    },
    # Arabic (RTL)
    {
      key: "trace_id",
      text: "معرّف التتبع",
      direction: { direction: "rtl", language: "ar", alignment: "right" }
    },
    {
      key: "span_name",
      text: "اسم الامتداد",
      direction: { direction: "rtl", language: "ar", alignment: "right" }
    },
    {
      key: "duration",
      text: "المدة",
      direction: { direction: "rtl", language: "ar", alignment: "right" }
    },
    {
      key: "status",
      text: "الحالة",
      direction: { direction: "rtl", language: "ar", alignment: "right" }
    },
    # Hebrew (RTL)
    {
      key: "trace_id",
      text: "מזהה מעקב",
      direction: { direction: "rtl", language: "he", alignment: "right" }
    },
    {
      key: "span_name",
      text: "שם טווח",
      direction: { direction: "rtl", language: "he", alignment: "right" }
    },
    {
      key: "duration",
      text: "משך זמן",
      direction: { direction: "rtl", language: "he", alignment: "right" }
    },
    {
      key: "status",
      text: "סטטוס",
      direction: { direction: "rtl", language: "he", alignment: "right" }
    }
  ]
  
  // Get localized UI text
  let get_localized_ui_text = fn(key: String, language: String, localizations: Array<LocalizedUIText>) {
    for localization in localizations {
      if localization.key == key && localization.direction.language == language {
        return Some(localization)
      }
    }
    None
  }
  
  // Test English UI text
  let en_trace_id = get_localized_ui_text("trace_id", "en", telemetry_ui_localizations)
  assert_true(en_trace_id.length > 0)
  let en_trace_id_text = en_trace_id.get
  assert_eq(en_trace_id_text.text, "Trace ID")
  assert_eq(en_trace_id_text.direction.direction, "ltr")
  assert_eq(en_trace_id_text.direction.alignment, "left")
  
  let en_span_name = get_localized_ui_text("span_name", "en", telemetry_ui_localizations)
  assert_true(en_span_name.length > 0)
  let en_span_name_text = en_span_name.get
  assert_eq(en_span_name_text.text, "Span Name")
  assert_eq(en_span_name_text.direction.direction, "ltr")
  assert_eq(en_span_name_text.direction.alignment, "left")
  
  # Test Arabic UI text
  let ar_trace_id = get_localized_ui_text("trace_id", "ar", telemetry_ui_localizations)
  assert_true(ar_trace_id.length > 0)
  let ar_trace_id_text = ar_trace_id.get
  assert_eq(ar_trace_id_text.text, "معرّف التتبع")
  assert_eq(ar_trace_id_text.direction.direction, "rtl")
  assert_eq(ar_trace_id_text.direction.alignment, "right")
  
  let ar_span_name = get_localized_ui_text("span_name", "ar", telemetry_ui_localizations)
  assert_true(ar_span_name.length > 0)
  let ar_span_name_text = ar_span_name.get
  assert_eq(ar_span_name_text.text, "اسم الامتداد")
  assert_eq(ar_span_name_text.direction.direction, "rtl")
  assert_eq(ar_span_name_text.direction.alignment, "right")
  
  # Test Hebrew UI text
  let he_trace_id = get_localized_ui_text("trace_id", "he", telemetry_ui_localizations)
  assert_true(he_trace_id.length > 0)
  let he_trace_id_text = he_trace_id.get
  assert_eq(he_trace_id_text.text, "מזהה מעקב")
  assert_eq(he_trace_id_text.direction.direction, "rtl")
  assert_eq(he_trace_id_text.direction.alignment, "right")
  
  let he_span_name = get_localized_ui_text("span_name", "he", telemetry_ui_localizations)
  assert_true(he_span_name.length > 0)
  let he_span_name_text = he_span_name.get
  assert_eq(he_span_name_text.text, "שם טווח")
  assert_eq(he_span_name_text.direction.direction, "rtl")
  assert_eq(he_span_name_text.direction.alignment, "right")
  
  # Test mixed content formatting
  let format_mixed_content = fn(label: LocalizedUIText, value: String) {
    match label.direction.direction {
      "ltr" => label.text + ": " + value
      "rtl" => value + " : " + label.text  # Value comes before label in RTL
      _ => label.text + ": " + value
    }
  }
  
  # Test mixed content with LTR
  let en_mixed = format_mixed_content(en_trace_id_text.get, "trace-12345")
  assert_eq(en_mixed, "Trace ID: trace-12345")
  
  # Test mixed content with RTL
  let ar_mixed = format_mixed_content(ar_trace_id_text.get, "trace-12345")
  assert_eq(ar_mixed, "trace-12345 : معرّف التتبع")
  
  let he_mixed = format_mixed_content(he_trace_id_text.get, "trace-12345")
  assert_eq(he_mixed, "trace-12345 : מזהה מעקב")
  
  # Test table layout direction
  type TableLayout = {
    direction: String,
    header_alignment: String,
    cell_alignment: String
  }
  
  let get_table_layout = fn(language: String) {
    match language {
      "ar" => {
        direction: "rtl",
        header_alignment: "right",
        cell_alignment: "right"
      }
      "he" => {
        direction: "rtl",
        header_alignment: "right",
        cell_alignment: "right"
      }
      _ => {
        direction: "ltr",
        header_alignment: "left",
        cell_alignment: "left"
      }
    }
  }
  
  # Test table layout for different languages
  let en_table_layout = get_table_layout("en")
  assert_eq(en_table_layout.direction, "ltr")
  assert_eq(en_table_layout.header_alignment, "left")
  assert_eq(en_table_layout.cell_alignment, "left")
  
  let ar_table_layout = get_table_layout("ar")
  assert_eq(ar_table_layout.direction, "rtl")
  assert_eq(ar_table_layout.header_alignment, "right")
  assert_eq(ar_table_layout.cell_alignment, "right")
  
  let he_table_layout = get_table_layout("he")
  assert_eq(he_table_layout.direction, "rtl")
  assert_eq(he_table_layout.header_alignment, "right")
  assert_eq(he_table_layout.cell_alignment, "right")
  
  # Test bidirectional text handling
  let handle_bidirectional_text = fn(primary_language: String, text: String) {
    let is_rtl_language = primary_language == "ar" || primary_language == "he"
    
    if is_rtl_language {
      # For RTL languages, check if text contains LTR content
      let has_ltr_content = text.contains("trace-") || text.contains("http://") || text.contains("www.")
      
      if has_ltr_content {
        # Wrap LTR content in direction markers
        "ltr(" + text + ")"
      } else {
        text
      }
    } else {
      # For LTR languages, check if text contains RTL content
      let has_rtl_content = text.contains("العربية") || text.contains("עברית")
      
      if has_rtl_content {
        # Wrap RTL content in direction markers
        "rtl(" + text + ")"
      } else {
        text
      }
    }
  }
  
  # Test bidirectional text handling
  let en_with_rtl = handle_bidirectional_text("en", "Status: الحالة")
  assert_eq(en_with_rtl, "Status: rtl(الحالة)")
  
  let ar_with_ltr = handle_bidirectional_text("ar", "trace-12345")
  assert_eq(ar_with_ltr, "ltr(trace-12345)")
  
  let he_with_ltr = handle_bidirectional_text("he", "www.example.com")
  assert_eq(he_with_ltr, "ltr(www.example.com)")
  
  let en_only_ltr = handle_bidirectional_text("en", "Trace ID: trace-12345")
  assert_eq(en_only_ltr, "Trace ID: trace-12345")
  
  let ar_only_rtl = handle_bidirectional_text("ar", "الحالة")
  assert_eq(ar_only_rtl, "الحالة")
}

// Test 5: Cultural Adaptation in Telemetry Display
test "cultural adaptation in telemetry display and formatting" {
  // Define cultural preferences
  type CulturalPreferences = {
    locale: String,
    time_format: String,  # "12h", "24h"
    date_format: String,  # "MDY", "DMY", "YMD"
    number_system: String,  # "western", "eastern"
    week_start: String,  # "sunday", "monday", "saturday"
    color_meanings: Array<(String, String)>  # status -> color
  }
  
  // Create cultural preferences for different locales
  let cultural_preferences = [
    {
      locale: "en-US",
      time_format: "12h",
      date_format: "MDY",
      number_system: "western",
      week_start: "sunday",
      color_meanings: [
        ("success", "green"),
        ("warning", "yellow"),
        ("error", "red"),
        ("info", "blue")
      ]
    },
    {
      locale: "zh-CN",
      time_format: "24h",
      date_format: "YMD",
      number_system: "eastern",
      week_start: "monday",
      color_meanings: [
        ("success", "green"),
        ("warning", "orange"),
        ("error", "red"),
        ("info", "blue")
      ]
    },
    {
      locale: "ja-JP",
      time_format: "24h",
      date_format: "YMD",
      number_system: "western",
      week_start: "monday",
      color_meanings: [
        ("success", "green"),
        ("warning", "yellow"),
        ("error", "red"),
        ("info", "blue")
      ]
    },
    {
      locale: "ar-SA",
      time_format: "12h",
      date_format: "DMY",
      number_system: "eastern",
      week_start: "saturday",
      color_meanings: [
        ("success", "green"),
        ("warning", "yellow"),
        ("error", "red"),
        ("info", "blue")
      ]
    },
    {
      locale: "de-DE",
      time_format: "24h",
      date_format: "DMY",
      number_system: "western",
      week_start: "monday",
      color_meanings: [
        ("success", "green"),
        ("warning", "orange"),
        ("error", "red"),
        ("info", "blue")
      ]
    }
  ]
  
  // Get cultural preferences for locale
  let get_cultural_preferences = fn(locale: String, preferences: Array<CulturalPreferences>) {
    for pref in preferences {
      if pref.locale == locale {
        return Some(pref)
      }
    }
    None
  }
  
  // Test getting cultural preferences
  let us_preferences = get_cultural_preferences("en-US", cultural_preferences)
  assert_true(us_preferences.length > 0)
  let us_prefs = us_preferences.get
  assert_eq(us_prefs.locale, "en-US")
  assert_eq(us_prefs.time_format, "12h")
  assert_eq(us_prefs.date_format, "MDY")
  assert_eq(us_prefs.number_system, "western")
  assert_eq(us_prefs.week_start, "sunday")
  
  let cn_preferences = get_cultural_preferences("zh-CN", cultural_preferences)
  assert_true(cn_preferences.length > 0)
  let cn_prefs = cn_preferences.get
  assert_eq(cn_prefs.locale, "zh-CN")
  assert_eq(cn_prefs.time_format, "24h")
  assert_eq(cn_prefs.date_format, "YMD")
  assert_eq(cn_prefs.number_system, "eastern")
  assert_eq(cn_prefs.week_start, "monday")
  
  let sa_preferences = get_cultural_preferences("ar-SA", cultural_preferences)
  assert_true(sa_preferences.length > 0)
  let sa_prefs = sa_preferences.get
  assert_eq(sa_prefs.locale, "ar-SA")
  assert_eq(sa_prefs.time_format, "12h")
  assert_eq(sa_prefs.date_format, "DMY")
  assert_eq(sa_prefs.number_system, "eastern")
  assert_eq(sa_prefs.week_start, "saturday")
  
  // Format time according to cultural preferences
  let format_time_culturally = fn(hour: Int, minute: Int, preferences: CulturalPreferences) {
    match preferences.time_format {
      "12h" => {
        let period = if hour >= 12 { "PM" } else { "AM" }
        let display_hour = if hour > 12 { hour - 12 } else if hour == 0 { 12 } else { hour }
        display_hour.to_string() + ":" + minute.to_string() + " " + period
      }
      "24h" => {
        hour.to_string() + ":" + minute.to_string()
      }
      _ => hour.to_string() + ":" + minute.to_string()
    }
  }
  
  // Test time formatting
  let us_time = format_time_culturally(14, 30, us_prefs)
  assert_eq(us_time, "2:30 PM")
  
  let cn_time = format_time_culturally(14, 30, cn_prefs)
  assert_eq(cn_time, "14:30")
  
  let sa_time = format_time_culturally(14, 30, sa_prefs)
  assert_eq(sa_time, "2:30 PM")
  
  // Format date according to cultural preferences
  let format_date_culturally = fn(year: Int, month: Int, day: Int, preferences: CulturalPreferences) {
    match preferences.date_format {
      "MDY" => month.to_string() + "/" + day.to_string() + "/" + year.to_string()
      "DMY" => day.to_string() + "/" + month.to_string() + "/" + year.to_string()
      "YMD" => year.to_string() + "/" + month.to_string() + "/" + day.to_string()
      _ => year.to_string() + "-" + month.to_string() + "-" + day.to_string()
    }
  }
  
  // Test date formatting
  let us_date = format_date_culturally(2022, 1, 15, us_prefs)
  assert_eq(us_date, "1/15/2022")
  
  let cn_date = format_date_culturally(2022, 1, 15, cn_prefs)
  assert_eq(cn_date, "2022/1/15")
  
  let sa_date = format_date_culturally(2022, 1, 15, sa_prefs)
  assert_eq(sa_date, "15/1/2022")
  
  // Format numbers according to cultural preferences
  let format_number_culturally = fn(number: Float, preferences: CulturalPreferences) {
    match preferences.number_system {
      "western" => {
        # Western Arabic numerals: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
        number.to_string()
      }
      "eastern" => {
        # Eastern Arabic numerals: ٠, ١, ٢, ٣, ٤, ٥, ٦, ٧, ٨, ٩
        let western_str = number.to_string()
        let mut eastern_str = ""
        
        for i in 0..western_str.length() {
          let char = western_str[i]
          let eastern_char = match char {
            '0' => '٠'
            '1' => '١'
            '2' => '٢'
            '3' => '٣'
            '4' => '٤'
            '5' => '٥'
            '6' => '٦'
            '7' => '٧'
            '8' => '٨'
            '9' => '٩'
            '.' => '.'
            _ => char
          }
          eastern_str = eastern_str + eastern_char.to_string()
        }
        
        eastern_str
      }
      _ => number.to_string()
    }
  }
  
  // Test number formatting
  let us_number = format_number_culturally(123.45, us_prefs)
  assert_eq(us_number, "123.45")
  
  let cn_number = format_number_culturally(123.45, cn_prefs)
  assert_eq(cn_number, "١٢٣.٤٥")
  
  let sa_number = format_number_culturally(123.45, sa_prefs)
  assert_eq(sa_number, "١٢٣.٤٥")
  
  // Get status color according to cultural preferences
  let get_status_color = fn(status: String, preferences: CulturalPreferences) {
    for (status_key, color) in preferences.color_meanings {
      if status_key == status {
        return Some(color)
      }
    }
    None
  }
  
  // Test status colors
  let us_success_color = get_status_color("success", us_prefs)
  assert_eq(us_success_color, Some("green"))
  
  let cn_warning_color = get_status_color("warning", cn_prefs)
  assert_eq(cn_warning_color, Some("orange"))
  
  let de_warning_color = get_status_color("warning", get_cultural_preferences("de-DE", cultural_preferences).get)
  assert_eq(de_warning_color, Some("orange"))
  
  // Test complete cultural formatting
  let format_telemetry_culturally = fn(timestamp: Int, value: Float, status: String, locale: String) {
    let preferences = get_cultural_preferences(locale, cultural_preferences).get
    
    # Extract date and time
    let date = timestamp / 86400
    let time = timestamp % 86400
    let hour = time / 3600
    let minute = (time % 3600) / 60
    
    let year = 2022
    let month = 1
    let day = (date % 31) + 1
    
    # Format according to cultural preferences
    let formatted_date = format_date_culturally(year, month, day, preferences)
    let formatted_time = format_time_culturally(hour, minute, preferences)
    let formatted_value = format_number_culturally(value, preferences)
    let status_color = get_status_color(status, preferences)
    
    {
      locale,
      date: formatted_date,
      time: formatted_time,
      value: formatted_value,
      status,
      color: status_color
    }
  }
  
  // Test complete cultural formatting
  let us_telemetry = format_telemetry_culturally(1640995200, 123.45, "success", "en-US")
  assert_eq(us_telemetry.locale, "en-US")
  assert_eq(us_telemetry.date, "1/1/2022")
  assert_eq(us_telemetry.time, "12:0 AM")
  assert_eq(us_telemetry.value, "123.45")
  assert_eq(us_telemetry.status, "success")
  assert_eq(us_telemetry.color, Some("green"))
  
  let cn_telemetry = format_telemetry_culturally(1640995200, 123.45, "success", "zh-CN")
  assert_eq(cn_telemetry.locale, "zh-CN")
  assert_eq(cn_telemetry.date, "2022/1/1")
  assert_eq(cn_telemetry.time, "0:0")
  assert_eq(cn_telemetry.value, "١٢٣.٤٥")
  assert_eq(cn_telemetry.status, "success")
  assert_eq(cn_telemetry.color, Some("green"))
  
  let sa_telemetry = format_telemetry_culturally(1640995200, 123.45, "warning", "ar-SA")
  assert_eq(sa_telemetry.locale, "ar-SA")
  assert_eq(sa_telemetry.date, "1/1/2022")
  assert_eq(sa_telemetry.time, "12:0 AM")
  assert_eq(sa_telemetry.value, "١٢٣.٤٥")
  assert_eq(sa_telemetry.status, "warning")
  assert_eq(sa_telemetry.color, Some("yellow"))
}