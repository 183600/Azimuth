// Azimuth Internationalization and Localization Tests
// 国际化和本地化支持测试用例

test "locale-specific telemetry data formatting" {
  // 测试特定地区的遥测数据格式化
  let telemetry_data = @azimuth.TelemetryData {
    timestamp : 1640995200000L,
    trace_id : "abcdef1234567890abcdef1234567890",
    span_id : "1234567890abcdef",
    parent_span_id : Some("abcdef1234567890"),
    operation_name : "payment.process",
    status : @azimuth.SpanStatus::Ok,
    duration_ms : 1234L,
    attributes : [
      ("amount", @azimuth.FloatValue(1234.56)),
      ("currency", @azimuth.StringValue("USD")),
      ("user.id", @azimuth.StringValue("user123")),
      ("transaction.date", @azimuth.StringValue("2022-01-01"))
    ],
    events : []
  }
  
  // 测试美式英语格式化
  let en_us_locale = @azimuth.Locale {
    language : "en",
    region : Some("US"),
    number_format : @azimuth.NumberFormat {
      decimal_separator : ".",
      grouping_separator : ",",
      grouping_size : 3,
      min_fraction_digits : 2,
      max_fraction_digits : 2
    },
    date_format : @azimuth.DateFormat {
      pattern : "MM/dd/yyyy",
      timezone : "America/New_York"
    },
    time_format : @azimuth.TimeFormat {
      pattern : "hh:mm:ss a",
      timezone : "America/New_York",
      is_24_hour : false
    }
  }
  
  let en_us_formatted = @azimuth.format_telemetry_data(telemetry_data, en_us_locale)
  
  // 验证美式英语格式化结果
  assert_true(en_us_formatted.contains("1,234.56")) // 数字格式
  assert_true(en_us_formatted.contains("01/01/2022")) // 日期格式
  assert_true(en_us_formatted.contains("payment.process")) // 操作名称
  
  // 测试德语格式化
  let de_de_locale = @azimuth.Locale {
    language : "de",
    region : Some("DE"),
    number_format : @azimuth.NumberFormat {
      decimal_separator : ",",
      grouping_separator : ".",
      grouping_size : 3,
      min_fraction_digits : 2,
      max_fraction_digits : 2
    },
    date_format : @azimuth.DateFormat {
      pattern : "dd.MM.yyyy",
      timezone : "Europe/Berlin"
    },
    time_format : @azimuth.TimeFormat {
      pattern : "HH:mm:ss",
      timezone : "Europe/Berlin",
      is_24_hour : true
    }
  }
  
  let de_de_formatted = @azimuth.format_telemetry_data(telemetry_data, de_de_locale)
  
  // 验证德语格式化结果
  assert_true(de_de_formatted.contains("1.234,56")) // 数字格式
  assert_true(de_de_formatted.contains("01.01.2022")) // 日期格式
  
  // 测试中文格式化
  let zh_cn_locale = @azimuth.Locale {
    language : "zh",
    region : Some("CN"),
    number_format : @azimuth.NumberFormat {
      decimal_separator : ".",
      grouping_separator : ",",
      grouping_size : 4,
      min_fraction_digits : 2,
      max_fraction_digits : 2
    },
    date_format : @azimuth.DateFormat {
      pattern : "yyyy年MM月dd日",
      timezone : "Asia/Shanghai"
    },
    time_format : @azimuth.TimeFormat {
      pattern : "HH:mm:ss",
      timezone : "Asia/Shanghai",
      is_24_hour : true
    }
  }
  
  let zh_cn_formatted = @azimuth.format_telemetry_data(telemetry_data, zh_cn_locale)
  
  // 验证中文格式化结果
  assert_true(zh_cn_formatted.contains("1234.56")) // 数字格式
  assert_true(zh_cn_formatted.contains("2022年01月01日")) // 日期格式
}

test "error message localization" {
  // 测试错误消息本地化
  let error_types = [
    @azimuth.ErrorType::ConnectionTimeout,
    @azimuth.ErrorType::AuthenticationFailed,
    @azimuth.ErrorType::ResourceNotFound,
    @azimuth.ErrorType::RateLimitExceeded,
    @azimuth.ErrorType::InternalServerError
  ]
  
  // 定义多语言错误消息
  let error_messages = @azimuth.ErrorMessageBundle {
    messages : [
      // 英语
      ("en", "ConnectionTimeout", "Connection timed out after {timeout}ms"),
      ("en", "AuthenticationFailed", "Authentication failed: {reason}"),
      ("en", "ResourceNotFound", "Resource '{resource}' not found"),
      ("en", "RateLimitExceeded", "Rate limit exceeded. Try again in {retry_after}s"),
      ("en", "InternalServerError", "Internal server error: {error_code}"),
      
      // 中文
      ("zh", "ConnectionTimeout", "连接超时，已等待{timeout}毫秒"),
      ("zh", "AuthenticationFailed", "身份验证失败：{reason}"),
      ("zh", "ResourceNotFound", "资源'{resource}'未找到"),
      ("zh", "RateLimitExceeded", "请求频率超限，请在{retry_after}秒后重试"),
      ("zh", "InternalServerError", "内部服务器错误：{error_code}"),
      
      // 德语
      ("de", "ConnectionTimeout", "Verbindungstimeout nach {timeout}ms"),
      ("de", "AuthenticationFailed", "Authentifizierung fehlgeschlagen: {reason}"),
      ("de", "ResourceNotFound", "Ressource '{resource}' nicht gefunden"),
      ("de", "RateLimitExceeded", "Rate Limit überschritten. Versuchen Sie es in {retry_after}s erneut"),
      ("de", "InternalServerError", "Interner Serverfehler: {error_code}")
    ]
  }
  
  // 测试英语错误消息
  let en_timeout_error = @azimuth.format_error_message(
    @azimuth.ErrorType::ConnectionTimeout,
    "en",
    error_messages,
    [("timeout", @azimuth.StringValue("5000"))]
  )
  assert_eq(en_timeout_error, "Connection timed out after 5000ms")
  
  let en_auth_error = @azimuth.format_error_message(
    @azimuth.ErrorType::AuthenticationFailed,
    "en",
    error_messages,
    [("reason", @azimuth.StringValue("Invalid credentials"))]
  )
  assert_eq(en_auth_error, "Authentication failed: Invalid credentials")
  
  // 测试中文错误消息
  let zh_timeout_error = @azimuth.format_error_message(
    @azimuth.ErrorType::ConnectionTimeout,
    "zh",
    error_messages,
    [("timeout", @azimuth.StringValue("5000"))]
  )
  assert_eq(zh_timeout_error, "连接超时，已等待5000毫秒")
  
  let zh_auth_error = @azimuth.format_error_message(
    @azimuth.ErrorType::AuthenticationFailed,
    "zh",
    error_messages,
    [("reason", @azimuth.StringValue("凭据无效"))]
  )
  assert_eq(zh_auth_error, "身份验证失败：凭据无效")
  
  // 测试德语错误消息
  let de_timeout_error = @azimuth.format_error_message(
    @azimuth.ErrorType::ConnectionTimeout,
    "de",
    error_messages,
    [("timeout", @azimuth.StringValue("5000"))]
  )
  assert_eq(de_timeout_error, "Verbindungstimeout nach 5000ms")
  
  let de_auth_error = @azimuth.format_error_message(
    @azimuth.ErrorType::AuthenticationFailed,
    "de",
    error_messages,
    [("reason", @azimuth.StringValue("Ungültige Anmeldeinformationen"))]
  )
  assert_eq(de_auth_error, "Authentifizierung fehlgeschlagen: Ungültige Anmeldeinformationen")
  
  // 测试回退到默认语言（英语）
  let unknown_locale_error = @azimuth.format_error_message(
    @azimuth.ErrorType::ConnectionTimeout,
    "fr", // 不支持的语言
    error_messages,
    [("timeout", @azimuth.StringValue("5000"))]
  )
  assert_eq(unknown_locale_error, "Connection timed out after 5000ms")
}

test "timezone-aware timestamp handling" {
  // 测试时区感知的时间戳处理
  let base_timestamp = 1640995200000L // 2022-01-01 00:00:00 UTC
  
  // 测试不同时区的时间戳转换
  let timezones = [
    ("UTC", "UTC"),
    ("America/New_York", "EST"),
    ("Europe/London", "GMT"),
    ("Europe/Berlin", "CET"),
    ("Asia/Tokyo", "JST"),
    ("Asia/Shanghai", "CST"),
    ("Australia/Sydney", "AEDT")
  ]
  
  for timezone_info in timezones {
    let timezone_name = timezone_info.0
    let timezone_abbr = timezone_info.1
    
    let formatted_time = @azimuth.format_timestamp_with_timezone(
      base_timestamp,
      timezone_name,
      "yyyy-MM-dd HH:mm:ss z"
    )
    
    // 验证时区缩写包含在格式化结果中
    assert_true(formatted_time.contains(timezone_abbr))
    
    // 验证日期部分正确
    assert_true(formatted_time.contains("2022-01-01"))
  }
  
  // 测试夏令时处理
  let dst_timestamp = 1648771200000L // 2022-04-01 00:00:00 UTC (夏令时期间)
  
  let ny_dst_time = @azimuth.format_timestamp_with_timezone(
    dst_timestamp,
    "America/New_York",
    "yyyy-MM-dd HH:mm:ss z"
  )
  assert_true(ny_dst_time.contains("EDT")) // 东部夏令时
  
  let london_dst_time = @azimuth.format_timestamp_with_timezone(
    dst_timestamp,
    "Europe/London",
    "yyyy-MM-dd HH:mm:ss z"
  )
  assert_true(london_dst_time.contains("BST")) // 英国夏令时
  
  let berlin_dst_time = @azimuth.format_timestamp_with_timezone(
    dst_timestamp,
    "Europe/Berlin",
    "yyyy-MM-dd HH:mm:ss z"
  )
  assert_true(berlin_dst_time.contains("CEST")) // 中欧夏令时
  
  // 测试时区转换
  let utc_time = @azimuth.convert_timezone(
    base_timestamp,
    "UTC",
    "America/New_York"
  )
  // UTC 00:00 = EST 前一天 19:00
  assert_eq(utc_time, 1640985600000L) // 2021-12-31 19:00:00 EST
  
  let tokyo_time = @azimuth.convert_timezone(
    base_timestamp,
    "UTC",
    "Asia/Tokyo"
  )
  // UTC 00:00 = JST 同一天 09:00
  assert_eq(tokyo_time, 1640998800000L) // 2022-01-01 09:00:00 JST
}

test "localized metric names and descriptions" {
  // 测试本地化指标名称和描述
  let metric_definitions = @azimuth.LocalizedMetricDefinitions {
    metrics : [
      // HTTP请求持续时间
      @azimuth.LocalizedMetric {
        name : "http.request.duration",
        descriptions : [
          ("en", "Duration of HTTP requests in milliseconds"),
          ("zh", "HTTP请求持续时间（毫秒）"),
          ("de", "Dauer der HTTP-Anfragen in Millisekunden"),
          ("ja", "HTTPリクエストの継続時間（ミリ秒）")
        ],
        unit : "ms",
        type_ : @azimuth.MetricType::Histogram
      },
      
      // 数据库连接池使用率
      @azimuth.LocalizedMetric {
        name : "db.connection.pool.usage",
        descriptions : [
          ("en", "Database connection pool usage ratio"),
          ("zh", "数据库连接池使用率"),
          ("de", "Nutzungsverhältnis des Datenbank-Verbindungspools"),
          ("ja", "データベース接続プール使用率")
        ],
        unit : "ratio",
        type_ : @azimuth.MetricType::Gauge
      },
      
      // 错误计数
      @azimuth.LocalizedMetric {
        name : "error.count",
        descriptions : [
          ("en", "Total number of errors"),
          ("zh", "错误总数"),
          ("de", "Gesamtzahl der Fehler"),
          ("ja", "エラーの総数")
        ],
        unit : "errors",
        type_ : @azimuth.MetricType::Counter
      }
    ]
  }
  
  // 测试英语本地化
  let en_http_duration = @azimuth.get_localized_metric_description(
    metric_definitions,
    "http.request.duration",
    "en"
  )
  assert_eq(en_http_duration, "Duration of HTTP requests in milliseconds")
  
  let en_db_usage = @azimuth.get_localized_metric_description(
    metric_definitions,
    "db.connection.pool.usage",
    "en"
  )
  assert_eq(en_db_usage, "Database connection pool usage ratio")
  
  let en_error_count = @azimuth.get_localized_metric_description(
    metric_definitions,
    "error.count",
    "en"
  )
  assert_eq(en_error_count, "Total number of errors")
  
  // 测试中文本地化
  let zh_http_duration = @azimuth.get_localized_metric_description(
    metric_definitions,
    "http.request.duration",
    "zh"
  )
  assert_eq(zh_http_duration, "HTTP请求持续时间（毫秒）")
  
  let zh_db_usage = @azimuth.get_localized_metric_description(
    metric_definitions,
    "db.connection.pool.usage",
    "zh"
  )
  assert_eq(zh_db_usage, "数据库连接池使用率")
  
  let zh_error_count = @azimuth.get_localized_metric_description(
    metric_definitions,
    "error.count",
    "zh"
  )
  assert_eq(zh_error_count, "错误总数")
  
  // 测试德语本地化
  let de_http_duration = @azimuth.get_localized_metric_description(
    metric_definitions,
    "http.request.duration",
    "de"
  )
  assert_eq(de_http_duration, "Dauer der HTTP-Anfragen in Millisekunden")
  
  let de_db_usage = @azimuth.get_localized_metric_description(
    metric_definitions,
    "db.connection.pool.usage",
    "de"
  )
  assert_eq(de_db_usage, "Nutzungsverhältnis des Datenbank-Verbindungspools")
  
  let de_error_count = @azimuth.get_localized_metric_description(
    metric_definitions,
    "error.count",
    "de"
  )
  assert_eq(de_error_count, "Gesamtzahl der Fehler")
  
  // 测试日语本地化
  let ja_http_duration = @azimuth.get_localized_metric_description(
    metric_definitions,
    "http.request.duration",
    "ja"
  )
  assert_eq(ja_http_duration, "HTTPリクエストの継続時間（ミリ秒）")
  
  let ja_db_usage = @azimuth.get_localized_metric_description(
    metric_definitions,
    "db.connection.pool.usage",
    "ja"
  )
  assert_eq(ja_db_usage, "データベース接続プール使用率")
  
  let ja_error_count = @azimuth.get_localized_metric_description(
    metric_definitions,
    "error.count",
    "ja"
  )
  assert_eq(ja_error_count, "エラーの総数")
  
  // 测试回退到默认语言（英语）
  let unknown_locale_http_duration = @azimuth.get_localized_metric_description(
    metric_definitions,
    "http.request.duration",
    "fr" // 不支持的语言
  )
  assert_eq(unknown_locale_http_duration, "Duration of HTTP requests in milliseconds")
}

test "localized log message formatting" {
  // 测试本地化日志消息格式化
  let log_templates = @azimuth.LogTemplateBundle {
    templates : [
      // 用户登录
      ("en", "user.login", "User '{user_id}' logged in from {ip_address} at {timestamp}"),
      ("zh", "user.login", "用户'{user_id}'于{timestamp}从{ip_address}登录"),
      ("de", "user.login", "Benutzer '{user_id}' hat sich um {timestamp} von {ip_address} angemeldet"),
      ("ja", "user.login", "ユーザー'{user_id}'が{timestamp}に{ip_address}からログインしました"),
      
      // 支付处理
      ("en", "payment.processed", "Payment {payment_id} of {amount} {currency} processed successfully"),
      ("zh", "payment.processed", "支付{payment_id}，金额{amount}{currency}处理成功"),
      ("de", "payment.processed", "Zahlung {payment_id} über {amount} {currency} erfolgreich verarbeitet"),
      ("ja", "payment.processed", "支払い{payment_id}、金額{amount}{currency}が正常に処理されました"),
      
      // 错误发生
      ("en", "error.occurred", "Error occurred in {service}: {error_message}"),
      ("zh", "error.occurred", "服务{service}中发生错误：{error_message}"),
      ("de", "error.occurred", "Fehler aufgetreten in {service}: {error_message}"),
      ("ja", "error.occurred", "サービス{service}でエラーが発生しました：{error_message}")
    ]
  }
  
  // 测试英语日志消息
  let en_login_log = @azimuth.format_log_message(
    "user.login",
    "en",
    log_templates,
    [
      ("user_id", @azimuth.StringValue("user123")),
      ("ip_address", @azimuth.StringValue("192.168.1.100")),
      ("timestamp", @azimuth.StringValue("2022-01-01 12:34:56"))
    ]
  )
  assert_eq(en_login_log, "User 'user123' logged in from 192.168.1.100 at 2022-01-01 12:34:56")
  
  let en_payment_log = @azimuth.format_log_message(
    "payment.processed",
    "en",
    log_templates,
    [
      ("payment_id", @azimuth.StringValue("pay_123456")),
      ("amount", @azimuth.FloatValue(99.99)),
      ("currency", @azimuth.StringValue("USD"))
    ]
  )
  assert_eq(en_payment_log, "Payment pay_123456 of 99.99 USD processed successfully")
  
  // 测试中文日志消息
  let zh_login_log = @azimuth.format_log_message(
    "user.login",
    "zh",
    log_templates,
    [
      ("user_id", @azimuth.StringValue("用户123")),
      ("ip_address", @azimuth.StringValue("192.168.1.100")),
      ("timestamp", @azimuth.StringValue("2022-01-01 12:34:56"))
    ]
  )
  assert_eq(zh_login_log, "用户'用户123'于2022-01-01 12:34:56从192.168.1.100登录")
  
  let zh_payment_log = @azimuth.format_log_message(
    "payment.processed",
    "zh",
    log_templates,
    [
      ("payment_id", @azimuth.StringValue("支付_123456")),
      ("amount", @azimuth.FloatValue(99.99)),
      ("currency", @azimuth.StringValue("CNY"))
    ]
  )
  assert_eq(zh_payment_log, "支付支付_123456，金额99.99CNY处理成功")
  
  // 测试德语日志消息
  let de_login_log = @azimuth.format_log_message(
    "user.login",
    "de",
    log_templates,
    [
      ("user_id", @azimuth.StringValue("benutzer123")),
      ("ip_address", @azimuth.StringValue("192.168.1.100")),
      ("timestamp", @azimuth.StringValue("2022-01-01 12:34:56"))
    ]
  )
  assert_eq(de_login_log, "Benutzer 'benutzer123' hat sich um 2022-01-01 12:34:56 von 192.168.1.100 angemeldet")
  
  let de_payment_log = @azimuth.format_log_message(
    "payment.processed",
    "de",
    log_templates,
    [
      ("payment_id", @azimuth.StringValue("zahlung_123456")),
      ("amount", @azimuth.FloatValue(99.99)),
      ("currency", @azimuth.StringValue("EUR"))
    ]
  )
  assert_eq(de_payment_log, "Zahlung zahlung_123456 über 99.99 EUR erfolgreich verarbeitet")
  
  // 测试日语日志消息
  let ja_login_log = @azimuth.format_log_message(
    "user.login",
    "ja",
    log_templates,
    [
      ("user_id", @azimuth.StringValue("ユーザー123")),
      ("ip_address", @azimuth.StringValue("192.168.1.100")),
      ("timestamp", @azimuth.StringValue("2022-01-01 12:34:56"))
    ]
  )
  assert_eq(ja_login_log, "ユーザー'ユーザー123'が2022-01-01 12:34:56に192.168.1.100からログインしました")
  
  let ja_payment_log = @azimuth.format_log_message(
    "payment.processed",
    "ja",
    log_templates,
    [
      ("payment_id", @azimuth.StringValue("支払い_123456")),
      ("amount", @azimuth.FloatValue(99.99)),
      ("currency", @azimuth.StringValue("JPY"))
    ]
  )
  assert_eq(ja_payment_log, "支払い支払い_123456、金額99.99JPYが正常に処理されました")
  
  // 测试回退到默认语言（英语）
  let unknown_locale_log = @azimuth.format_log_message(
    "user.login",
    "fr", // 不支持的语言
    log_templates,
    [
      ("user_id", @azimuth.StringValue("user123")),
      ("ip_address", @azimuth.StringValue("192.168.1.100")),
      ("timestamp", @azimuth.StringValue("2022-01-01 12:34:56"))
    ]
  )
  assert_eq(unknown_locale_log, "User 'user123' logged in from 192.168.1.100 at 2022-01-01 12:34:56")
}