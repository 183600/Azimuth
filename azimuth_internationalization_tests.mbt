// Azimuth Internationalization Test Suite
// 测试遥测系统的国际化支持功能

test "多语言错误消息" {
  // 创建支持国际化的遥测提供者
  let telemetry_provider = TelemetryProvider::with_i18n_support()
  
  // 配置支持的语言
  let supported_locales = ["en-US", "zh-CN", "ja-JP", "es-ES", "fr-FR", "de-DE"]
  I18n::configure_supported_locales(telemetry_provider, supported_locales)
  
  // 配置错误消息的多语言翻译
  let error_translations = [
    // 英语
    ("en-US", "connection.failed", "Connection to telemetry server failed"),
    ("en-US", "invalid.config", "Invalid telemetry configuration"),
    ("en-US", "resource.limit.exceeded", "Resource limit exceeded"),
    
    // 中文
    ("zh-CN", "connection.failed", "连接遥测服务器失败"),
    ("zh-CN", "invalid.config", "遥测配置无效"),
    ("zh-CN", "resource.limit.exceeded", "超出资源限制"),
    
    // 日语
    ("ja-JP", "connection.failed", "テレメトリサーバーへの接続に失敗しました"),
    ("ja-JP", "invalid.config", "テレメトリ設定が無効です"),
    ("ja-JP", "resource.limit.exceeded", "リソース制限を超えました"),
    
    // 西班牙语
    ("es-ES", "connection.failed", "Error al conectar con el servidor de telemetría"),
    ("es-ES", "invalid.config", "Configuración de telemetría no válida"),
    ("es-ES", "resource.limit.exceeded", "Límite de recursos excedido"),
    
    // 法语
    ("fr-FR", "connection.failed", "Échec de la connexion au serveur de télémétrie"),
    ("fr-FR", "invalid.config", "Configuration de télémétrie invalide"),
    ("fr-FR", "resource.limit.exceeded", "Limite de ressources dépassée"),
    
    // 德语
    ("de-DE", "connection.failed", "Verbindung zum Telemetrieserver fehlgeschlagen"),
    ("de-DE", "invalid.config", "Ungültige Telemetrie-Konfiguration"),
    ("de-DE", "resource.limit.exceeded", "Ressourcenlimit überschritten")
  ]
  
  I18n::configure_error_translations(telemetry_provider, error_translations)
  
  // 创建tracer
  let tracer = TelemetryProvider::get_tracer(telemetry_provider, "i18n.test")
  
  // 测试不同语言环境下的错误消息
  let locales_to_test = ["en-US", "zh-CN", "ja-JP", "es-ES", "fr-FR", "de-DE"]
  let error_keys = ["connection.failed", "invalid.config", "resource.limit.exceeded"]
  
  for locale in locales_to_test {
    I18n::set_current_locale(telemetry_provider, locale)
    
    for error_key in error_keys {
      // 创建span并添加错误
      let span = Tracer::start_span(tracer, "error.test")
      Span::set_attribute(span, "error.key", error_key)
      
      // 获取本地化错误消息
      let localized_message = I18n::get_error_message(telemetry_provider, error_key)
      
      // 验证消息不为空
      assert_true(localized_message.length() > 0)
      
      // 验证消息已本地化（不是英语键名）
      assert_false(localized_message == error_key)
      
      Span::end(span)
    }
  }
  
  assert_true(true)
}

test "多语言日志消息" {
  // 创建支持国际化的遥测提供者
  let telemetry_provider = TelemetryProvider::with_i18n_support()
  
  // 配置支持的语言
  let supported_locales = ["en-US", "zh-CN", "ja-JP", "es-ES", "fr-FR", "de-DE"]
  I18n::configure_supported_locales(telemetry_provider, supported_locales)
  
  // 配置日志消息的多语言翻译
  let log_translations = [
    // 英语
    ("en-US", "operation.started", "Operation {0} started"),
    ("en-US", "operation.completed", "Operation {0} completed successfully"),
    ("en-US", "operation.failed", "Operation {0} failed with error: {1}"),
    
    // 中文
    ("zh-CN", "operation.started", "操作 {0} 已开始"),
    ("zh-CN", "operation.completed", "操作 {0} 已成功完成"),
    ("zh-CN", "operation.failed", "操作 {0} 失败，错误：{1}"),
    
    // 日语
    ("ja-JP", "operation.started", "操作 {0} が開始されました"),
    ("ja-JP", "operation.completed", "操作 {0} が正常に完了しました"),
    ("ja-JP", "operation.failed", "操作 {0} が失敗しました。エラー: {1}"),
    
    // 西班牙语
    ("es-ES", "operation.started", "Operación {0} iniciada"),
    ("es-ES", "operation.completed", "Operación {0} completada exitosamente"),
    ("es-ES", "operation.failed", "Operación {0} falló con error: {1}"),
    
    // 法语
    ("fr-FR", "operation.started", "Opération {0} démarrée"),
    ("fr-FR", "operation.completed", "Opération {0} terminée avec succès"),
    ("fr-FR", "operation.failed", "Opération {0} a échoué avec l'erreur: {1}"),
    
    // 德语
    ("de-DE", "operation.started", "Operation {0} gestartet"),
    ("de-DE", "operation.completed", "Operation {0} erfolgreich abgeschlossen"),
    ("de-DE", "operation.failed", "Operation {0} fehlgeschlagen mit Fehler: {1}")
  ]
  
  I18n::configure_log_translations(telemetry_provider, log_translations)
  
  // 创建logger
  let logger = TelemetryProvider::get_logger(telemetry_provider, "i18n.logger")
  
  // 测试不同语言环境下的日志消息
  let locales_to_test = ["en-US", "zh-CN", "ja-JP", "es-ES", "fr-FR", "de-DE"]
  let log_keys = ["operation.started", "operation.completed", "operation.failed"]
  let operation_name = "data.processing"
  let error_message = "timeout"
  
  for locale in locales_to_test {
    I18n::set_current_locale(telemetry_provider, locale)
    
    for log_key in log_keys {
      // 获取本地化日志消息
      let localized_message = I18n::format_log_message(telemetry_provider, log_key, [operation_name, error_message])
      
      // 验证消息不为空
      assert_true(localized_message.length() > 0)
      
      // 验证消息已本地化（不是英语键名）
      assert_false(localized_message == log_key)
      
      // 验证参数已正确插入
      assert_true(localized_message.contains(operation_name))
      
      // 记录本地化日志
      Logger::emit_log(logger, "INFO", localized_message, [("locale", locale)])
    }
  }
  
  assert_true(true)
}

test "本地化时间和日期格式" {
  // 创建支持国际化的遥测提供者
  let telemetry_provider = TelemetryProvider::with_i18n_support()
  
  // 配置支持的语言
  let supported_locales = ["en-US", "zh-CN", "ja-JP", "es-ES", "fr-FR", "de-DE"]
  I18n::configure_supported_locales(telemetry_provider, supported_locales)
  
  // 创建tracer
  let tracer = TelemetryProvider::get_tracer(telemetry_provider, "i18n.datetime.test")
  
  // 测试时间戳
  let timestamp = "2025-01-02T10:30:45.123Z"
  
  // 测试不同语言环境下的日期时间格式
  let locales_to_test = ["en-US", "zh-CN", "ja-JP", "es-ES", "fr-FR", "de-DE"]
  
  for locale in locales_to_test {
    I18n::set_current_locale(telemetry_provider, locale)
    
    // 创建span
    let span = Tracer::start_span(tracer, "datetime.test")
    
    // 获取本地化的日期时间格式
    let localized_datetime = I18n::format_datetime(telemetry_provider, timestamp)
    
    // 验证日期时间不为空
    assert_true(localized_datetime.length() > 0)
    
    // 验证日期时间格式符合本地化规则
    match locale {
      "en-US" => assert_true(localized_datetime.contains("1/2/2025") || localized_datetime.contains("Jan"))
      "zh-CN" => assert_true(localized_datetime.contains("2025年1月2日"))
      "ja-JP" => assert_true(localized_datetime.contains("2025年1月2日"))
      "es-ES" => assert_true(localized_datetime.contains("2/1/2025") || localized_datetime.contains("ene"))
      "fr-FR" => assert_true(localized_datetime.contains("02/01/2025") || localized_datetime.contains("janv"))
      "de-DE" => assert_true(localized_datetime.contains("02.01.2025") || localized_datetime.contains("Jan"))
      _ => assert_true(true) // 其他格式也有效
    }
    
    // 添加本地化时间戳属性
    Span::set_attribute(span, "localized.timestamp", localized_datetime)
    Span::set_attribute(span, "locale", locale)
    
    Span::end(span)
  }
  
  assert_true(true)
}

test "数字和货币格式本地化" {
  // 创建支持国际化的遥测提供者
  let telemetry_provider = TelemetryProvider::with_i18n_support()
  
  // 配置支持的语言
  let supported_locales = ["en-US", "zh-CN", "ja-JP", "es-ES", "fr-FR", "de-DE"]
  I18n::configure_supported_locales(telemetry_provider, supported_locales)
  
  // 创建meter
  let meter = TelemetryProvider::get_meter(telemetry_provider, "i18n.number.test")
  
  // 测试数字
  let test_number = 1234567.89
  let test_currency = 1234.56
  
  // 测试不同语言环境下的数字格式
  let locales_to_test = ["en-US", "zh-CN", "ja-JP", "es-ES", "fr-FR", "de-DE"]
  
  for locale in locales_to_test {
    I18n::set_current_locale(telemetry_provider, locale)
    
    // 创建计数器
    let counter = Meter::create_counter(meter, "localized.counter")
    
    // 获取本地化的数字格式
    let localized_number = I18n::format_number(telemetry_provider, test_number)
    let localized_currency = I18n::format_currency(telemetry_provider, test_currency, "USD")
    
    // 验证数字不为空
    assert_true(localized_number.length() > 0)
    assert_true(localized_currency.length() > 0)
    
    // 验证数字格式符合本地化规则
    match locale {
      "en-US" => {
        assert_true(localized_number.contains("1,234,567.89"))
        assert_true(localized_currency.contains("$1,234.56"))
      }
      "zh-CN" => {
        assert_true(localized_number.contains("1,234,567.89"))
        assert_true(localized_currency.contains("US$1,234.56"))
      }
      "ja-JP" => {
        assert_true(localized_number.contains("1,234,567.89"))
        assert_true(localized_currency.contains("$1,234.56"))
      }
      "de-DE" => {
        assert_true(localized_number.contains("1.234.567,89"))
        assert_true(localized_currency.contains("1.234,56 $"))
      }
      "es-ES" => {
        assert_true(localized_number.contains("1.234.567,89"))
        assert_true(localized_currency.contains("1.234,56 $"))
      }
      "fr-FR" => {
        assert_true(localized_number.contains("1 234 567,89"))
        assert_true(localized_currency.contains("1 234,56 $"))
      }
      _ => assert_true(true) // 其他格式也有效
    }
    
    // 添加本地化数字属性
    Counter::add_with_attributes(counter, test_number, [
      ("locale", locale),
      ("localized.number", localized_number),
      ("localized.currency", localized_currency)
    ])
  }
  
  assert_true(true)
}

test "Unicode和特殊字符处理" {
  // 创建支持国际化的遥测提供者
  let telemetry_provider = TelemetryProvider::with_i18n_support()
  
  // 配置支持的语言
  let supported_locales = ["en-US", "zh-CN", "ja-JP", "ar-SA", "ru-RU", "ko-KR"]
  I18n::configure_supported_locales(telemetry_provider, supported_locales)
  
  // 创建tracer
  let tracer = TelemetryProvider::get_tracer(telemetry_provider, "i18n.unicode.test")
  
  // 测试不同语言的Unicode字符
  let test_strings = [
    ("en-US", "User authentication successful"),
    ("zh-CN", "用户认证成功"),
    ("ja-JP", "ユーザー認証成功"),
    ("ar-SA", "مصادقة المستخدم بنجاح"),
    ("ru-RU", "Аутентификация пользователя прошла успешно"),
    ("ko-KR", "사용자 인증 성공")
  ]
  
  for (locale, test_string) in test_strings {
    I18n::set_current_locale(telemetry_provider, locale)
    
    // 创建span
    let span = Tracer::start_span(tracer, "unicode.test")
    
    // 添加Unicode字符串属性
    Span::set_attribute(span, "unicode.message", test_string)
    Span::set_attribute(span, "locale", locale)
    
    // 添加事件
    Span::add_event(span, "unicode.event", [("message", test_string)])
    
    // 验证字符串可以正确处理
    let stored_message = Span::get_attribute(span, "unicode.message")
    match stored_message {
      Some(AttributeValue::StringValue(message)) => {
        assert_eq(message, test_string)
        assert_true(message.length() > 0)
      }
      _ => assert_true(false)
    }
    
    Span::end(span)
  }
  
  // 创建logger
  let logger = TelemetryProvider::get_logger(telemetry_provider, "i18n.unicode.logger")
  
  // 记录包含Unicode字符的日志
  for (locale, test_string) in test_strings {
    I18n::set_current_locale(telemetry_provider, locale)
    
    Logger::emit_log(logger, "INFO", test_string, [
      ("locale", locale),
      ("message.type", "unicode")
    ])
  }
  
  assert_true(true)
}

test "时区和地区特定处理" {
  // 创建支持国际化的遥测提供者
  let telemetry_provider = TelemetryProvider::with_i18n_support()
  
  // 配置支持的时区和地区
  let timezones = [
    ("en-US", "America/New_York"),
    ("en-GB", "Europe/London"),
    ("zh-CN", "Asia/Shanghai"),
    ("ja-JP", "Asia/Tokyo"),
    ("au-AU", "Australia/Sydney"),
    ("de-DE", "Europe/Berlin")
  ]
  
  I18n::configure_timezones(telemetry_provider, timezones)
  
  // 创建tracer
  let tracer = TelemetryProvider::get_tracer(telemetry_provider, "i18n.timezone.test")
  
  // 测试时区转换
  let utc_timestamp = "2025-01-02T10:30:00Z"
  
  for (locale, timezone) in timezones {
    I18n::set_current_locale(telemetry_provider, locale)
    I18n::set_timezone(telemetry_provider, timezone)
    
    // 创建span
    let span = Tracer::start_span(tracer, "timezone.test")
    
    // 获取本地化的时间戳
    let localized_timestamp = I18n::format_datetime_with_timezone(telemetry_provider, utc_timestamp)
    
    // 验证时间戳不为空
    assert_true(localized_timestamp.length() > 0)
    
    // 验证时区信息包含在格式化结果中
    match timezone {
      "America/New_York" => {
        // 应该比UTC晚5小时（标准时间）或4小时（夏令时）
        assert_true(localized_timestamp.contains("EST") || localized_timestamp.contains("EDT"))
      }
      "Europe/London" => {
        // 应该与UTC相同或晚1小时（夏令时）
        assert_true(localized_timestamp.contains("GMT") || localized_timestamp.contains("BST"))
      }
      "Asia/Shanghai" => {
        // 应该比UTC早8小时
        assert_true(localized_timestamp.contains("CST") || localized_timestamp.contains("GMT+8"))
      }
      "Asia/Tokyo" => {
        // 应该比UTC早9小时
        assert_true(localized_timestamp.contains("JST") || localized_timestamp.contains("GMT+9"))
      }
      "Australia/Sydney" => {
        // 应该比UTC早11小时（标准时间）或10小时（夏令时）
        assert_true(localized_timestamp.contains("AEDT") || localized_timestamp.contains("AEST"))
      }
      "Europe/Berlin" => {
        // 应该比UTC早1小时（标准时间）或2小时（夏令时）
        assert_true(localized_timestamp.contains("CET") || localized_timestamp.contains("CEST"))
      }
      _ => assert_true(true) // 其他时区也有效
    }
    
    // 添加本地化时间戳属性
    Span::set_attribute(span, "localized.timestamp", localized_timestamp)
    Span::set_attribute(span, "locale", locale)
    Span::set_attribute(span, "timezone", timezone)
    
    Span::end(span)
  }
  
  assert_true(true)
}