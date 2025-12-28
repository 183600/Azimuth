// Internationalization and Globalization Tests for Azimuth Telemetry System
// These tests focus on multi-language support, locale handling, and cultural adaptations

// Test 1: Multi-Language Attribute Support
pub test "multi-language attribute support" {
  let attrs = azimuth::Attributes::new()
  
  // Test attributes with different languages
  azimuth::Attributes::set(attrs, "error.message.en", azimuth::StringValue("Database connection failed"))
  azimuth::Attributes::set(attrs, "error.message.zh", azimuth::StringValue("数据库连接失败"))
  azimuth::Attributes::set(attrs, "error.message.ja", azimuth::StringValue("データベース接続に失敗しました"))
  azimuth::Attributes::set(attrs, "error.message.es", azimuth::StringValue("Error de conexión a la base de datos"))
  azimuth::Attributes::set(attrs, "error.message.fr", azimuth::StringValue("Échec de la connexion à la base de données"))
  azimuth::Attributes::set(attrs, "error.message.de", azimuth::StringValue("Datenbankverbindung fehlgeschlagen"))
  azimuth::Attributes::set(attrs, "error.message.ko", azimuth::StringValue("데이터베이스 연결 실패"))
  azimuth::Attributes::set(attrs, "error.message.ru", azimuth::StringValue("Ошибка подключения к базе данных"))
  
  // Test retrieval of different language versions
  let error_en = azimuth::Attributes::get(attrs, "error.message.en")
  let error_zh = azimuth::Attributes::get(attrs, "error.message.zh")
  let error_ja = azimuth::Attributes::get(attrs, "error.message.ja")
  let error_es = azimuth::Attributes::get(attrs, "error.message.es")
  let error_fr = azimuth::Attributes::get(attrs, "error.message.fr")
  let error_de = azimuth::Attributes::get(attrs, "error.message.de")
  let error_ko = azimuth::Attributes::get(attrs, "error.message.ko")
  let error_ru = azimuth::Attributes::get(attrs, "error.message.ru")
  
  assert_eq(error_en, Some(azimuth::StringValue("Database connection failed")))
  assert_eq(error_zh, Some(azimuth::StringValue("数据库连接失败")))
  assert_eq(error_ja, Some(azimuth::StringValue("データベース接続に失敗しました")))
  assert_eq(error_es, Some(azimuth::StringValue("Error de conexión a la base de datos")))
  assert_eq(error_fr, Some(azimuth::StringValue("Échec de la connexion à la base de données")))
  assert_eq(error_de, Some(azimuth::StringValue("Datenbankverbindung fehlgeschlagen")))
  assert_eq(error_ko, Some(azimuth::StringValue("데이터베이스 연결 실패")))
  assert_eq(error_ru, Some(azimuth::StringValue("Ошибка подключения к базе данных")))
  
  // Test service names in different languages
  azimuth::Attributes::set(attrs, "service.name.en", azimuth::StringValue("Payment Processing Service"))
  azimuth::Attributes::set(attrs, "service.name.zh", azimuth::StringValue("支付处理服务"))
  azimuth::Attributes::set(attrs, "service.name.ja", azimuth::StringValue("決済処理サービス"))
  azimuth::Attributes::set(attrs, "service.name.es", azimuth::StringValue("Servicio de procesamiento de pagos"))
  
  let service_en = azimuth::Attributes::get(attrs, "service.name.en")
  let service_zh = azimuth::Attributes::get(attrs, "service.name.zh")
  let service_ja = azimuth::Attributes::get(attrs, "service.name.ja")
  let service_es = azimuth::Attributes::get(attrs, "service.name.es")
  
  assert_eq(service_en, Some(azimuth::StringValue("Payment Processing Service")))
  assert_eq(service_zh, Some(azimuth::StringValue("支付处理服务")))
  assert_eq(service_ja, Some(azimuth::StringValue("決済処理サービス")))
  assert_eq(service_es, Some(azimuth::StringValue("Servicio de procesamiento de pagos")))
}

// Test 2: Locale-Specific Resource Attributes
pub test "locale-specific resource attributes" {
  let base_resource = azimuth::Resource::new()
  
  // Create locale-specific resource attributes
  let en_attrs = [
    ("service.name", azimuth::StringValue("User Authentication Service")),
    ("service.description", azimuth::StringValue("Handles user authentication and authorization")),
    ("service.region", azimuth::StringValue("us-west-2")),
    ("service.language", azimuth::StringValue("en-US")),
    ("service.timezone", azimuth::StringValue("America/Los_Angeles"))
  ]
  
  let zh_attrs = [
    ("service.name", azimuth::StringValue("用户认证服务")),
    ("service.description", azimuth::StringValue("处理用户认证和授权")),
    ("service.region", azimuth::StringValue("ap-northeast-1")),
    ("service.language", azimuth::StringValue("zh-CN")),
    ("service.timezone", azimuth::StringValue("Asia/Shanghai"))
  ]
  
  let ja_attrs = [
    ("service.name", azimuth::StringValue("ユーザー認証サービス")),
    ("service.description", azimuth::StringValue("ユーザー認証と認可を処理します")),
    ("service.region", azimuth::StringValue("ap-northeast-1")),
    ("service.language", azimuth::StringValue("ja-JP")),
    ("service.timezone", azimuth::StringValue("Asia/Tokyo"))
  ]
  
  let es_attrs = [
    ("service.name", azimuth::StringValue("Servicio de Autenticación de Usuario")),
    ("service.description", azimuth::StringValue("Maneja la autenticación y autorización de usuarios")),
    ("service.region", azimuth::StringValue("sa-east-1")),
    ("service.language", azimuth::StringValue("es-ES")),
    ("service.timezone", azimuth::StringValue("Europe/Madrid"))
  ]
  
  // Create locale-specific resources
  let en_resource = azimuth::Resource::with_attributes(base_resource, en_attrs)
  let zh_resource = azimuth::Resource::with_attributes(base_resource, zh_attrs)
  let ja_resource = azimuth::Resource::with_attributes(base_resource, ja_attrs)
  let es_resource = azimuth::Resource::with_attributes(base_resource, es_attrs)
  
  // Verify locale-specific attributes
  let en_name = azimuth::Resource::get_attribute(en_resource, "service.name")
  let zh_name = azimuth::Resource::get_attribute(zh_resource, "service.name")
  let ja_name = azimuth::Resource::get_attribute(ja_resource, "service.name")
  let es_name = azimuth::Resource::get_attribute(es_resource, "service.name")
  
  assert_eq(en_name, Some(azimuth::StringValue("User Authentication Service")))
  assert_eq(zh_name, Some(azimuth::StringValue("用户认证服务")))
  assert_eq(ja_name, Some(azimuth::StringValue("ユーザー認証サービス")))
  assert_eq(es_name, Some(azimuth::StringValue("Servicio de Autenticación de Usuario")))
  
  let en_language = azimuth::Resource::get_attribute(en_resource, "service.language")
  let zh_language = azimuth::Resource::get_attribute(zh_resource, "service.language")
  let ja_language = azimuth::Resource::get_attribute(ja_resource, "service.language")
  let es_language = azimuth::Resource::get_attribute(es_resource, "service.language")
  
  assert_eq(en_language, Some(azimuth::StringValue("en-US")))
  assert_eq(zh_language, Some(azimuth::StringValue("zh-CN")))
  assert_eq(ja_language, Some(azimuth::StringValue("ja-JP")))
  assert_eq(es_language, Some(azimuth::StringValue("es-ES")))
  
  let en_timezone = azimuth::Resource::get_attribute(en_resource, "service.timezone")
  let zh_timezone = azimuth::Resource::get_attribute(zh_resource, "service.timezone")
  let ja_timezone = azimuth::Resource::get_attribute(ja_resource, "service.timezone")
  let es_timezone = azimuth::Resource::get_attribute(es_resource, "service.timezone")
  
  assert_eq(en_timezone, Some(azimuth::StringValue("America/Los_Angeles")))
  assert_eq(zh_timezone, Some(azimuth::StringValue("Asia/Shanghai")))
  assert_eq(ja_timezone, Some(azimuth::StringValue("Asia/Tokyo")))
  assert_eq(es_timezone, Some(azimuth::StringValue("Europe/Madrid")))
}

// Test 3: Multi-Language Log Messages
pub test "multi-language log messages" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "internationalization.logger")
  
  // Create log records in different languages
  let en_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("User login successful for user ID: 12345"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000000L),
    None,
    Some("trace-intl-001"),
    Some("span-intl-001"),
    Some(azimuth::Context::root())
  )
  
  let zh_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("用户登录成功，用户ID：12345"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000000L),
    None,
    Some("trace-intl-001"),
    Some("span-intl-002"),
    Some(azimuth::Context::root())
  )
  
  let ja_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("ユーザーID: 12345のログインが成功しました"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000000L),
    None,
    Some("trace-intl-001"),
    Some("span-intl-003"),
    Some(azimuth::Context::root())
  )
  
  let es_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Inicio de sesión exitoso para el usuario ID: 12345"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000000L),
    None,
    Some("trace-intl-001"),
    Some("span-intl-004"),
    Some(azimuth::Context::root())
  )
  
  // Create error logs in different languages
  let en_error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Payment processing failed: Invalid credit card number"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000001L),
    None,
    Some("trace-intl-002"),
    Some("span-intl-005"),
    Some(azimuth::Context::root())
  )
  
  let zh_error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("支付处理失败：信用卡号码无效"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000001L),
    None,
    Some("trace-intl-002"),
    Some("span-intl-006"),
    Some(azimuth::Context::root())
  )
  
  // Emit all internationalized logs
  azimuth::Logger::emit(logger, en_log)
  azimuth::Logger::emit(logger, zh_log)
  azimuth::Logger::emit(logger, ja_log)
  azimuth::Logger::emit(logger, es_log)
  azimuth::Logger::emit(logger, en_error_log)
  azimuth::Logger::emit(logger, zh_error_log)
  
  // Verify log message content
  assert_eq(azimuth::LogRecord::body(en_log), Some("User login successful for user ID: 12345"))
  assert_eq(azimuth::LogRecord::body(zh_log), Some("用户登录成功，用户ID：12345"))
  assert_eq(azimuth::LogRecord::body(ja_log), Some("ユーザーID: 12345のログインが成功しました"))
  assert_eq(azimuth::LogRecord::body(es_log), Some("Inicio de sesión exitoso para el usuario ID: 12345"))
  
  assert_eq(azimuth::LogRecord::body(en_error_log), Some("Payment processing failed: Invalid credit card number"))
  assert_eq(azimuth::LogRecord::body(zh_error_log), Some("支付处理失败：信用卡号码无效"))
  
  // Verify log severity
  assert_eq(azimuth::LogRecord::severity_number(en_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(zh_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(ja_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(es_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(en_error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(zh_error_log), azimuth::Error)
}

// Test 4: Internationalized Span Names and Events
pub test "internationalized span names and events" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "internationalization.tracer")
  
  // Create spans with internationalized names
  let en_span = azimuth::Tracer::start_span(tracer, "Process User Payment")
  let zh_span = azimuth::Tracer::start_span(tracer, "处理用户支付")
  let ja_span = azimuth::Tracer::start_span(tracer, "ユーザー決済処理")
  let es_span = azimuth::Tracer::start_span(tracer, "Procesar Pago del Usuario")
  
  // Add internationalized events
  azimuth::Span::add_event(en_span, "Payment validation started", 
    Some([("step", azimuth::StringValue("validation")),
          ("language", azimuth::StringValue("en"))]))
  
  azimuth::Span::add_event(zh_span, "支付验证开始", 
    Some([("step", azimuth::StringValue("validation")),
          ("language", azimuth::StringValue("zh"))]))
  
  azimuth::Span::add_event(ja_span, "決済検証開始", 
    Some([("step", azimuth::StringValue("validation")),
          ("language", azimuth::StringValue("ja"))]))
  
  azimuth::Span::add_event(es_span, "Validación de pago iniciada", 
    Some([("step", azimuth::StringValue("validation")),
          ("language", azimuth::StringValue("es"))]))
  
  // Add more internationalized events
  azimuth::Span::add_event(en_span, "Payment processed successfully", 
    Some([("step", azimuth::StringValue("completion")),
          ("language", azimuth::StringValue("en")),
          ("result", azimuth::StringValue("success"))]))
  
  azimuth::Span::add_event(zh_span, "支付处理成功", 
    Some([("step", azimuth::StringValue("completion")),
          ("language", azimuth::StringValue("zh")),
          ("result", azimuth::StringValue("success"))]))
  
  azimuth::Span::add_event(ja_span, "決済処理が正常に完了", 
    Some([("step", azimuth::StringValue("completion")),
          ("language", azimuth::StringValue("ja")),
          ("result", azimuth::StringValue("success"))]))
  
  azimuth::Span::add_event(es_span, "Pago procesado exitosamente", 
    Some([("step", azimuth::StringValue("completion")),
          ("language", azimuth::StringValue("es")),
          ("result", azimuth::StringValue("success"))]))
  
  // Set internationalized status messages
  azimuth::Span::set_status(en_span, azimuth::Ok, Some("Payment processing completed successfully"))
  azimuth::Span::set_status(zh_span, azimuth::Ok, Some("支付处理成功完成"))
  azimuth::Span::set_status(ja_span, azimuth::Ok, Some("決済処理が正常に完了しました"))
  azimuth::Span::set_status(es_span, azimuth::Ok, Some("Procesamiento de pago completado exitosamente"))
  
  // Verify span names
  assert_eq(azimuth::Span::name(en_span), "Process User Payment")
  assert_eq(azimuth::Span::name(zh_span), "处理用户支付")
  assert_eq(azimuth::Span::name(ja_span), "ユーザー決済処理")
  assert_eq(azimuth::Span::name(es_span), "Procesar Pago del Usuario")
  
  // Complete spans
  azimuth::Span::end(en_span)
  azimuth::Span::end(zh_span)
  azimuth::Span::end(ja_span)
  azimuth::Span::end(es_span)
}

// Test 5: Internationalized Metrics and Labels
pub test "internationalized metrics and labels" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "internationalization.meter")
  
  // Create metrics with internationalized descriptions
  let en_counter = azimuth::Meter::create_counter(meter, "user.login.attempts", 
    Some("Total number of user login attempts"), Some("attempts"))
  let zh_counter = azimuth::Meter::create_counter(meter, "user.login.attempts", 
    Some("用户登录尝试总次数"), Some("次数"))
  let ja_counter = azimuth::Meter::create_counter(meter, "user.login.attempts", 
    Some("ユーザーログイン試行総数"), Some("回"))
  let es_counter = azimuth::Meter::create_counter(meter, "user.login.attempts", 
    Some("Número total de intentos de inicio de sesión de usuario"), Some("intentos"))
  
  let en_histogram = azimuth::Meter::create_histogram(meter, "response.time", 
    Some("Response time in milliseconds"), Some("ms"))
  let zh_histogram = azimuth::Meter::create_histogram(meter, "response.time", 
    Some("响应时间（毫秒）"), Some("毫秒"))
  let ja_histogram = azimuth::Meter::create_histogram(meter, "response.time", 
    Some("応答時間（ミリ秒）"), Some("ミリ秒"))
  let es_histogram = azimuth::Meter::create_histogram(meter, "response.time", 
    Some("Tiempo de respuesta en milisegundos"), Some("ms"))
  
  // Record metrics with internationalized context
  azimuth::Counter::add(en_counter, 1.0)
  azimuth::Counter::add(zh_counter, 1.0)
  azimuth::Counter::add(ja_counter, 1.0)
  azimuth::Counter::add(es_counter, 1.0)
  
  azimuth::Histogram::record(en_histogram, 150.0)
  azimuth::Histogram::record(zh_histogram, 150.0)
  azimuth::Histogram::record(ja_histogram, 150.0)
  azimuth::Histogram::record(es_histogram, 150.0)
  
  // Verify metric descriptions
  let en_counter_instrument = azimuth::Counter::as_instrument(en_counter)
  let zh_counter_instrument = azimuth::Counter::as_instrument(zh_counter)
  let ja_counter_instrument = azimuth::Counter::as_instrument(ja_counter)
  let es_counter_instrument = azimuth::Counter::as_instrument(es_counter)
  
  assert_eq(azimuth::Instrument::description(en_counter_instrument), Some("Total number of user login attempts"))
  assert_eq(azimuth::Instrument::description(zh_counter_instrument), Some("用户登录尝试总次数"))
  assert_eq(azimuth::Instrument::description(ja_counter_instrument), Some("ユーザーログイン試行総数"))
  assert_eq(azimuth::Instrument::description(es_counter_instrument), Some("Número total de intentos de inicio de sesión de usuario"))
  
  let en_histogram_instrument = azimuth::Histogram::as_instrument(en_histogram)
  let zh_histogram_instrument = azimuth::Histogram::as_instrument(zh_histogram)
  let ja_histogram_instrument = azimuth::Histogram::as_instrument(ja_histogram)
  let es_histogram_instrument = azimuth::Histogram::as_instrument(es_histogram)
  
  assert_eq(azimuth::Instrument::description(en_histogram_instrument), Some("Response time in milliseconds"))
  assert_eq(azimuth::Instrument::description(zh_histogram_instrument), Some("响应时间（毫秒）"))
  assert_eq(azimuth::Instrument::description(ja_histogram_instrument), Some("応答時間（ミリ秒）"))
  assert_eq(azimuth::Instrument::description(es_histogram_instrument), Some("Tiempo de respuesta en milisegundos"))
  
  // Verify metric units
  assert_eq(azimuth::Instrument::unit(en_counter_instrument), Some("attempts"))
  assert_eq(azimuth::Instrument::unit(zh_counter_instrument), Some("次数"))
  assert_eq(azimuth::Instrument::unit(ja_counter_instrument), Some("回"))
  assert_eq(azimuth::Instrument::unit(es_counter_instrument), Some("intentos"))
  
  assert_eq(azimuth::Instrument::unit(en_histogram_instrument), Some("ms"))
  assert_eq(azimuth::Instrument::unit(zh_histogram_instrument), Some("毫秒"))
  assert_eq(azimuth::Instrument::unit(ja_histogram_instrument), Some("ミリ秒"))
  assert_eq(azimuth::Instrument::unit(es_histogram_instrument), Some("ms"))
}

// Test 6: Cross-Cultural Date and Time Formatting
pub test "cross-cultural date and time formatting" {
  let clock = azimuth::Clock::system()
  let base_timestamp = azimuth::Clock::now_unix_nanos(clock)
  
  // Create attributes with different date/time formats
  let attrs = azimuth::Attributes::new()
  
  // ISO 8601 format (international standard)
  azimuth::Attributes::set(attrs, "timestamp.iso8601", 
    azimuth::StringValue("2025-01-28T10:30:00.000Z"))
  
  // US format
  azimuth::Attributes::set(attrs, "timestamp.us", 
    azimuth::StringValue("01/28/2025 10:30:00 AM"))
  
  // Chinese format
  azimuth::Attributes::set(attrs, "timestamp.zh", 
    azimuth::StringValue("2025年01月28日 10:30:00"))
  
  // Japanese format
  azimuth::Attributes::set(attrs, "timestamp.ja", 
    azimuth::StringValue("2025年01月28日 10時30分00秒"))
  
  // Spanish format
  azimuth::Attributes::set(attrs, "timestamp.es", 
    azimuth::StringValue("28/01/2025 10:30:00"))
  
  // German format
  azimuth::Attributes::set(attrs, "timestamp.de", 
    azimuth::StringValue("28.01.2025 10:30:00"))
  
  // French format
  azimuth::Attributes::set(attrs, "timestamp.fr", 
    azimuth::StringValue("28/01/2025 10:30:00"))
  
  // Korean format
  azimuth::Attributes::set(attrs, "timestamp.ko", 
    azimuth::StringValue("2025년 01월 28일 10:30:00"))
  
  // Russian format
  azimuth::Attributes::set(attrs, "timestamp.ru", 
    azimuth::StringValue("28.01.2025 10:30:00"))
  
  // Verify date/time format attributes
  let iso8601_timestamp = azimuth::Attributes::get(attrs, "timestamp.iso8601")
  let us_timestamp = azimuth::Attributes::get(attrs, "timestamp.us")
  let zh_timestamp = azimuth::Attributes::get(attrs, "timestamp.zh")
  let ja_timestamp = azimuth::Attributes::get(attrs, "timestamp.ja")
  let es_timestamp = azimuth::Attributes::get(attrs, "timestamp.es")
  let de_timestamp = azimuth::Attributes::get(attrs, "timestamp.de")
  let fr_timestamp = azimuth::Attributes::get(attrs, "timestamp.fr")
  let ko_timestamp = azimuth::Attributes::get(attrs, "timestamp.ko")
  let ru_timestamp = azimuth::Attributes::get(attrs, "timestamp.ru")
  
  assert_eq(iso8601_timestamp, Some(azimuth::StringValue("2025-01-28T10:30:00.000Z")))
  assert_eq(us_timestamp, Some(azimuth::StringValue("01/28/2025 10:30:00 AM")))
  assert_eq(zh_timestamp, Some(azimuth::StringValue("2025年01月28日 10:30:00")))
  assert_eq(ja_timestamp, Some(azimuth::StringValue("2025年01月28日 10時30分00秒")))
  assert_eq(es_timestamp, Some(azimuth::StringValue("28/01/2025 10:30:00")))
  assert_eq(de_timestamp, Some(azimuth::StringValue("28.01.2025 10:30:00")))
  assert_eq(fr_timestamp, Some(azimuth::StringValue("28/01/2025 10:30:00")))
  assert_eq(ko_timestamp, Some(azimuth::StringValue("2025년 01월 28일 10:30:00")))
  assert_eq(ru_timestamp, Some(azimuth::StringValue("28.01.2025 10:30:00")))
  
  // Test timezone-specific attributes
  let timezone_attrs = [
    ("timezone.utc", azimuth::StringValue("UTC+00:00")),
    ("timezone.est", azimuth::StringValue("UTC-05:00")),
    ("timezone.pst", azimuth::StringValue("UTC-08:00")),
    ("timezone.cst", azimuth::StringValue("UTC+08:00")),
    ("timezone.jst", azimuth::StringValue("UTC+09:00")),
    ("timezone.cet", azimuth::StringValue("UTC+01:00")),
    ("timezone.kst", azimuth::StringValue("UTC+09:00")),
    ("timezone.msk", azimuth::StringValue("UTC+03:00"))
  ]
  
  let timezone_resource = azimuth::Resource::new()
  let timezone_resource_final = azimuth::Resource::with_attributes(timezone_resource, timezone_attrs)
  
  // Verify timezone attributes
  let utc_timezone = azimuth::Resource::get_attribute(timezone_resource_final, "timezone.utc")
  let est_timezone = azimuth::Resource::get_attribute(timezone_resource_final, "timezone.est")
  let pst_timezone = azimuth::Resource::get_attribute(timezone_resource_final, "timezone.pst")
  let cst_timezone = azimuth::Resource::get_attribute(timezone_resource_final, "timezone.cst")
  let jst_timezone = azimuth::Resource::get_attribute(timezone_resource_final, "timezone.jst")
  let cet_timezone = azimuth::Resource::get_attribute(timezone_resource_final, "timezone.cet")
  let kst_timezone = azimuth::Resource::get_attribute(timezone_resource_final, "timezone.kst")
  let msk_timezone = azimuth::Resource::get_attribute(timezone_resource_final, "timezone.msk")
  
  assert_eq(utc_timezone, Some(azimuth::StringValue("UTC+00:00")))
  assert_eq(est_timezone, Some(azimuth::StringValue("UTC-05:00")))
  assert_eq(pst_timezone, Some(azimuth::StringValue("UTC-08:00")))
  assert_eq(cst_timezone, Some(azimuth::StringValue("UTC+08:00")))
  assert_eq(jst_timezone, Some(azimuth::StringValue("UTC+09:00")))
  assert_eq(cet_timezone, Some(azimuth::StringValue("UTC+01:00")))
  assert_eq(kst_timezone, Some(azimuth::StringValue("UTC+09:00")))
  assert_eq(msk_timezone, Some(azimuth::StringValue("UTC+03:00")))
}

// Test 7: Internationalized Currency and Number Formatting
pub test "internationalized currency and number formatting" {
  let attrs = azimuth::Attributes::new()
  
  // Test currency formatting in different locales
  azimuth::Attributes::set(attrs, "amount.en_US", azimuth::StringValue("$1,234.56"))
  azimuth::Attributes::set(attrs, "amount.en_GB", azimuth::StringValue("£1,234.56"))
  azimuth::Attributes::set(attrs, "amount.zh_CN", azimuth::StringValue("¥1,234.56"))
  azimuth::Attributes::set(attrs, "amount.ja_JP", azimuth::StringValue("¥1,234"))
  azimuth::Attributes::set(attrs, "amount.es_ES", azimuth::StringValue("1.234,56 €"))
  azimuth::Attributes::set(attrs, "amount.de_DE", azimuth::StringValue("1.234,56 €"))
  azimuth::Attributes::set(attrs, "amount.fr_FR", azimuth::StringValue("1 234,56 €"))
  azimuth::Attributes::set(attrs, "amount.ko_KR", azimuth::StringValue("₩1,235"))
  azimuth::Attributes::set(attrs, "amount.ru_RU", azimuth::StringValue("1 234,56 ₽"))
  
  // Test number formatting in different locales
  azimuth::Attributes::set(attrs, "number.en_US", azimuth::StringValue("1,234,567.89"))
  azimuth::Attributes::set(attrs, "number.zh_CN", azimuth::StringValue("1,234,567.89"))
  azimuth::Attributes::set(attrs, "number.ja_JP", azimuth::StringValue("1,234,567.89"))
  azimuth::Attributes::set(attrs, "number.es_ES", azimuth::StringValue("1.234.567,89"))
  azimuth::Attributes::set(attrs, "number.de_DE", azimuth::StringValue("1.234.567,89"))
  azimuth::Attributes::set(attrs, "number.fr_FR", azimuth::StringValue("1 234 567,89"))
  azimuth::Attributes::set(attrs, "number.ko_KR", azimuth::StringValue("1,234,567.89"))
  azimuth::Attributes::set(attrs, "number.ru_RU", azimuth::StringValue("1 234 567,89"))
  
  // Test percentage formatting
  azimuth::Attributes::set(attrs, "percentage.en_US", azimuth::StringValue("12.34%"))
  azimuth::Attributes::set(attrs, "percentage.zh_CN", azimuth::StringValue("12.34%"))
  azimuth::Attributes::set(attrs, "percentage.ja_JP", azimuth::StringValue("12.34%"))
  azimuth::Attributes::set(attrs, "percentage.es_ES", azimuth::StringValue("12,34 %"))
  azimuth::Attributes::set(attrs, "percentage.de_DE", azimuth::StringValue("12,34 %"))
  azimuth::Attributes::set(attrs, "percentage.fr_FR", azimuth::StringValue("12,34 %"))
  azimuth::Attributes::set(attrs, "percentage.ko_KR", azimuth::StringValue("12.34%"))
  azimuth::Attributes::set(attrs, "percentage.ru_RU", azimuth::StringValue("12,34 %"))
  
  // Verify currency formatting
  let amount_us = azimuth::Attributes::get(attrs, "amount.en_US")
  let amount_gb = azimuth::Attributes::get(attrs, "amount.en_GB")
  let amount_cn = azimuth::Attributes::get(attrs, "amount.zh_CN")
  let amount_jp = azimuth::Attributes::get(attrs, "amount.ja_JP")
  let amount_es = azimuth::Attributes::get(attrs, "amount.es_ES")
  let amount_de = azimuth::Attributes::get(attrs, "amount.de_DE")
  let amount_fr = azimuth::Attributes::get(attrs, "amount.fr_FR")
  let amount_ko = azimuth::Attributes::get(attrs, "amount.ko_KR")
  let amount_ru = azimuth::Attributes::get(attrs, "amount.ru_RU")
  
  assert_eq(amount_us, Some(azimuth::StringValue("$1,234.56")))
  assert_eq(amount_gb, Some(azimuth::StringValue("£1,234.56")))
  assert_eq(amount_cn, Some(azimuth::StringValue("¥1,234.56")))
  assert_eq(amount_jp, Some(azimuth::StringValue("¥1,234")))
  assert_eq(amount_es, Some(azimuth::StringValue("1.234,56 €")))
  assert_eq(amount_de, Some(azimuth::StringValue("1.234,56 €")))
  assert_eq(amount_fr, Some(azimuth::StringValue("1 234,56 €")))
  assert_eq(amount_ko, Some(azimuth::StringValue("₩1,235")))
  assert_eq(amount_ru, Some(azimuth::StringValue("1 234,56 ₽")))
  
  // Verify number formatting
  let number_us = azimuth::Attributes::get(attrs, "number.en_US")
  let number_es = azimuth::Attributes::get(attrs, "number.es_ES")
  let number_fr = azimuth::Attributes::get(attrs, "number.fr_FR")
  
  assert_eq(number_us, Some(azimuth::StringValue("1,234,567.89")))
  assert_eq(number_es, Some(azimuth::StringValue("1.234.567,89")))
  assert_eq(number_fr, Some(azimuth::StringValue("1 234 567,89")))
  
  // Verify percentage formatting
  let percentage_us = azimuth::Attributes::get(attrs, "percentage.en_US")
  let percentage_es = azimuth::Attributes::get(attrs, "percentage.es_ES")
  let percentage_fr = azimuth::Attributes::get(attrs, "percentage.fr_FR")
  
  assert_eq(percentage_us, Some(azimuth::StringValue("12.34%")))
  assert_eq(percentage_es, Some(azimuth::StringValue("12,34 %")))
  assert_eq(percentage_fr, Some(azimuth::StringValue("12,34 %")))
}

// Test 8: Internationalized Error Messages and Status Codes
pub test "internationalized error messages and status codes" {
  let attrs = azimuth::Attributes::new()
  
  // Test error messages in different languages
  azimuth::Attributes::set(attrs, "error.auth.failed.en", azimuth::StringValue("Authentication failed: Invalid credentials"))
  azimuth::Attributes::set(attrs, "error.auth.failed.zh", azimuth::StringValue("认证失败：凭据无效"))
  azimuth::Attributes::set(attrs, "error.auth.failed.ja", azimuth::StringValue("認証失敗：資格情報が無効です"))
  azimuth::Attributes::set(attrs, "error.auth.failed.es", azimuth::StringValue("Autenticación fallida: Credenciales inválidas"))
  azimuth::Attributes::set(attrs, "error.auth.failed.fr", azimuth::StringValue("Échec de l'authentification: Identifiants invalides"))
  azimuth::Attributes::set(attrs, "error.auth.failed.de", azimuth::StringValue("Authentifizierung fehlgeschlagen: Ungültige Anmeldeinformationen"))
  azimuth::Attributes::set(attrs, "error.auth.failed.ko", azimuth::StringValue("인증 실패: 자격 증명이 유효하지 않습니다"))
  azimuth::Attributes::set(attrs, "error.auth.failed.ru", azimuth::StringValue("Ошибка аутентификации: Недействительные учетные данные"))
  
  // Test validation error messages
  azimuth::Attributes::set(attrs, "error.validation.required.en", azimuth::StringValue("Required field cannot be empty"))
  azimuth::Attributes::set(attrs, "error.validation.required.zh", azimuth::StringValue("必填字段不能为空"))
  azimuth::Attributes::set(attrs, "error.validation.required.ja", azimuth::StringValue("必須フィールドは空にできません"))
  azimuth::Attributes::set(attrs, "error.validation.required.es", azimuth::StringValue("El campo requerido no puede estar vacío"))
  azimuth::Attributes::set(attrs, "error.validation.required.fr", azimuth::StringValue("Le champ obligatoire ne peut pas être vide"))
  azimuth::Attributes::set(attrs, "error.validation.required.de", azimuth::StringValue("Erforderliches Feld darf nicht leer sein"))
  azimuth::Attributes::set(attrs, "error.validation.required.ko", azimuth::StringValue("필수 필드는 비워둘 수 없습니다"))
  azimuth::Attributes::set(attrs, "error.validation.required.ru", azimuth::StringValue("Обязательное поле не может быть пустым"))
  
  // Test success messages
  azimuth::Attributes::set(attrs, "success.operation.en", azimuth::StringValue("Operation completed successfully"))
  azimuth::Attributes::set(attrs, "success.operation.zh", azimuth::StringValue("操作成功完成"))
  azimuth::Attributes::set(attrs, "success.operation.ja", azimuth::StringValue("操作が正常に完了しました"))
  azimuth::Attributes::set(attrs, "success.operation.es", azimuth::StringValue("Operación completada exitosamente"))
  azimuth::Attributes::set(attrs, "success.operation.fr", azimuth::StringValue("Opération terminée avec succès"))
  azimuth::Attributes::set(attrs, "success.operation.de", azimuth::StringValue("Vorgang erfolgreich abgeschlossen"))
  azimuth::Attributes::set(attrs, "success.operation.ko", azimuth::StringValue("작업이 성공적으로 완료되었습니다"))
  azimuth::Attributes::set(attrs, "success.operation.ru", azimuth::StringValue("Операция успешно завершена"))
  
  // Verify error messages
  let auth_failed_en = azimuth::Attributes::get(attrs, "error.auth.failed.en")
  let auth_failed_zh = azimuth::Attributes::get(attrs, "error.auth.failed.zh")
  let auth_failed_ja = azimuth::Attributes::get(attrs, "error.auth.failed.ja")
  let auth_failed_es = azimuth::Attributes::get(attrs, "error.auth.failed.es")
  
  assert_eq(auth_failed_en, Some(azimuth::StringValue("Authentication failed: Invalid credentials")))
  assert_eq(auth_failed_zh, Some(azimuth::StringValue("认证失败：凭据无效")))
  assert_eq(auth_failed_ja, Some(azimuth::StringValue("認証失敗：資格情報が無効です")))
  assert_eq(auth_failed_es, Some(azimuth::StringValue("Autenticación fallida: Credenciales inválidas")))
  
  // Verify validation messages
  let validation_required_en = azimuth::Attributes::get(attrs, "error.validation.required.en")
  let validation_required_zh = azimuth::Attributes::get(attrs, "error.validation.required.zh")
  let validation_required_ja = azimuth::Attributes::get(attrs, "error.validation.required.ja")
  let validation_required_fr = azimuth::Attributes::get(attrs, "error.validation.required.fr")
  
  assert_eq(validation_required_en, Some(azimuth::StringValue("Required field cannot be empty")))
  assert_eq(validation_required_zh, Some(azimuth::StringValue("必填字段不能为空")))
  assert_eq(validation_required_ja, Some(azimuth::StringValue("必須フィールドは空にできません")))
  assert_eq(validation_required_fr, Some(azimuth::StringValue("Le champ obligatoire ne peut pas être vide")))
  
  // Verify success messages
  let success_en = azimuth::Attributes::get(attrs, "success.operation.en")
  let success_zh = azimuth::Attributes::get(attrs, "success.operation.zh")
  let success_ja = azimuth::Attributes::get(attrs, "success.operation.ja")
  let success_ko = azimuth::Attributes::get(attrs, "success.operation.ko")
  
  assert_eq(success_en, Some(azimuth::StringValue("Operation completed successfully")))
  assert_eq(success_zh, Some(azimuth::StringValue("操作成功完成")))
  assert_eq(success_ja, Some(azimuth::StringValue("操作が正常に完了しました")))
  assert_eq(success_ko, Some(azimuth::StringValue("작업이 성공적으로 완료되었습니다")))
}

// Test 9: Internationalized Baggage Propagation
pub test "internationalized baggage propagation" {
  let baggage = azimuth::Baggage::new()
  
  // Test baggage entries with internationalized content
  let baggage_with_en = azimuth::Baggage::set_entry(baggage, "user.locale", "en-US")
  let baggage_with_zh = azimuth::Baggage::set_entry(baggage_with_en, "user.timezone", "Asia/Shanghai")
  let baggage_with_ja = azimuth::Baggage::set_entry(baggage_with_zh, "user.currency", "JPY")
  let baggage_with_es = azimuth::Baggage::set_entry(baggage_with_ja, "user.region", "es-ES")
  let baggage_with_content = azimuth::Baggage::set_entry(baggage_with_es, "content.language", "es")
  
  // Test baggage with localized values
  let localized_baggage = azimuth::Baggage::new()
  let localized_with_greeting = azimuth::Baggage::set_entry(localized_baggage, "greeting.en", "Hello")
  let localized_with_greeting_zh = azimuth::Baggage::set_entry(localized_with_greeting, "greeting.zh", "你好")
  let localized_with_greeting_ja = azimuth::Baggage::set_entry(localized_with_greeting_zh, "greeting.ja", "こんにちは")
  let localized_with_greeting_es = azimuth::Baggage::set_entry(localized_with_greeting_ja, "greeting.es", "Hola")
  let localized_with_greeting_fr = azimuth::Baggage::set_entry(localized_with_greeting_es, "greeting.fr", "Bonjour")
  let localized_final = azimuth::Baggage::set_entry(localized_with_greeting_fr, "greeting.de", "Hallo")
  
  // Verify baggage entries
  let user_locale = azimuth::Baggage::get_entry(baggage_with_content, "user.locale")
  let user_timezone = azimuth::Baggage::get_entry(baggage_with_content, "user.timezone")
  let user_currency = azimuth::Baggage::get_entry(baggage_with_content, "user.currency")
  let user_region = azimuth::Baggage::get_entry(baggage_with_content, "user.region")
  let content_language = azimuth::Baggage::get_entry(baggage_with_content, "content.language")
  
  assert_eq(user_locale, Some("en-US"))
  assert_eq(user_timezone, Some("Asia/Shanghai"))
  assert_eq(user_currency, Some("JPY"))
  assert_eq(user_region, Some("es-ES"))
  assert_eq(content_language, Some("es"))
  
  // Verify localized greetings
  let greeting_en = azimuth::Baggage::get_entry(localized_final, "greeting.en")
  let greeting_zh = azimuth::Baggage::get_entry(localized_final, "greeting.zh")
  let greeting_ja = azimuth::Baggage::get_entry(localized_final, "greeting.ja")
  let greeting_es = azimuth::Baggage::get_entry(localized_final, "greeting.es")
  let greeting_fr = azimuth::Baggage::get_entry(localized_final, "greeting.fr")
  let greeting_de = azimuth::Baggage::get_entry(localized_final, "greeting.de")
  
  assert_eq(greeting_en, Some("Hello"))
  assert_eq(greeting_zh, Some("你好"))
  assert_eq(greeting_ja, Some("こんにちは"))
  assert_eq(greeting_es, Some("Hola"))
  assert_eq(greeting_fr, Some("Bonjour"))
  assert_eq(greeting_de, Some("Hallo"))
}

// Test 10: Comprehensive Internationalization Integration
pub test "comprehensive internationalization integration" {
  // Setup internationalized telemetry stack
  let i18n_resource = azimuth::Resource::new()
  let i18n_attrs = [
    ("service.name.en", azimuth::StringValue("International Payment Service")),
    ("service.name.zh", azimuth::StringValue("国际支付服务")),
    ("service.name.ja", azimuth::StringValue("国際決済サービス")),
    ("service.name.es", azimuth::StringValue("Servicio de Pagos Internacional")),
    ("service.region", azimuth::StringValue("global")),
    ("service.supported_locales", azimuth::ArrayStringValue(["en-US", "zh-CN", "ja-JP", "es-ES", "fr-FR", "de-DE", "ko-KR", "ru-RU"]))
  ]
  let i18n_resource_final = azimuth::Resource::with_attributes(i18n_resource, i18n_attrs)
  
  // Initialize internationalized components
  let tracer_provider = azimuth::TracerProvider::default()
  let meter_provider = azimuth::MeterProvider::default()
  let logger_provider = azimuth::LoggerProvider::default()
  
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "i18n.tracer")
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "i18n.meter")
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "i18n.logger")
  
  // Create internationalized span
  let i18n_span = azimuth::Tracer::start_span(tracer, "Process International Payment")
  azimuth::Span::add_event(i18n_span, "Payment validation started", 
    Some([("locale", azimuth::StringValue("en-US")),
          ("currency", azimuth::StringValue("USD")),
          ("amount", azimuth::FloatValue(299.99))]))
  
  // Create internationalized metrics
  let payment_counter = azimuth::Meter::create_counter(meter, "international.payments", 
    Some("Total international payments processed"), Some("payments"))
  let currency_histogram = azimuth::Meter::create_histogram(meter, "payment.amount.by_currency", 
    Some("Payment amount distribution by currency"), Some("local_currency"))
  
  // Record internationalized metrics
  azimuth::Counter::add(payment_counter, 1.0)
  azimuth::Histogram::record(currency_histogram, 299.99)
  
  // Create internationalized log
  let i18n_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("International payment processed successfully: $299.99 USD"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(i18n_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(i18n_span))),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(logger, i18n_log)
  
  // Complete internationalized operations
  azimuth::Span::set_status(i18n_span, azimuth::Ok, Some("International payment processed successfully"))
  azimuth::Span::end(i18n_span)
  
  // Verify internationalization integration
  let service_name_en = azimuth::Resource::get_attribute(i18n_resource_final, "service.name.en")
  let service_name_zh = azimuth::Resource::get_attribute(i18n_resource_final, "service.name.zh")
  let service_name_ja = azimuth::Resource::get_attribute(i18n_resource_final, "service.name.ja")
  let service_name_es = azimuth::Resource::get_attribute(i18n_resource_final, "service.name.es")
  
  assert_eq(service_name_en, Some(azimuth::StringValue("International Payment Service")))
  assert_eq(service_name_zh, Some(azimuth::StringValue("国际支付服务")))
  assert_eq(service_name_ja, Some(azimuth::StringValue("国際決済サービス")))
  assert_eq(service_name_es, Some(azimuth::StringValue("Servicio de Pagos Internacional")))
  
  let supported_locales = azimuth::Resource::get_attribute(i18n_resource_final, "service.supported_locales")
  assert_eq(supported_locales, Some(azimuth::ArrayStringValue(["en-US", "zh-CN", "ja-JP", "es-ES", "fr-FR", "de-DE", "ko-KR", "ru-RU"])))
  
  // Verify telemetry components
  assert_eq(azimuth::Span::name(i18n_span), "Process International Payment")
  assert_eq(payment_counter.name, "international.payments")
  assert_eq(logger.scope.name, "i18n.logger")
  assert_eq(azimuth::LogRecord::body(i18n_log), Some("International payment processed successfully: $299.99 USD"))
}