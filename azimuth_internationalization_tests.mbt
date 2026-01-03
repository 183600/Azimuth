// Azimuth Telemetry System - Internationalization Support Tests
// This file contains comprehensive internationalization support test cases

// Test 1: Multi-language Attribute Values
test "multi-language attribute values" {
  let provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(provider, "i18n_test_tracer")
  let span = Tracer::start_span(tracer, "i18n_test_span")
  
  // Set attributes with different language values
  Span::set_attribute(span, "error.message.zh", StringValue("操作失败"))
  Span::set_attribute(span, "error.message.en", StringValue("Operation failed"))
  Span::set_attribute(span, "error.message.ja", StringValue("操作が失敗しました"))
  Span::set_attribute(span, "error.message.fr", StringValue("L'opération a échoué"))
  Span::set_attribute(span, "error.message.de", StringValue("Operation fehlgeschlagen"))
  Span::set_attribute(span, "error.message.es", StringValue("La operación falló"))
  Span::set_attribute(span, "error.message.ru", StringValue("Операция не удалась"))
  Span::set_attribute(span, "error.message.ar", StringValue("فشلت العملية"))
  
  // Verify attributes are set correctly
  let zh_message = Span::get_attribute(span, "error.message.zh")
  match zh_message {
    Some(StringValue(v)) => assert_eq(v, "操作失败")
    _ => assert_true(false)
  }
  
  let en_message = Span::get_attribute(span, "error.message.en")
  match en_message {
    Some(StringValue(v)) => assert_eq(v, "Operation failed")
    _ => assert_true(false)
  }
  
  let ja_message = Span::get_attribute(span, "error.message.ja")
  match ja_message {
    Some(StringValue(v)) => assert_eq(v, "操作が失敗しました")
    _ => assert_true(false)
  }
  
  // Test language-specific attribute retrieval
  let localized_message = I18n::get_localized_attribute(span, "error.message", "zh")
  match localized_message {
    Some(StringValue(v)) => assert_eq(v, "操作失败")
    _ => assert_true(false)
  }
  
  let localized_message_en = I18n::get_localized_attribute(span, "error.message", "en")
  match localized_message_en {
    Some(StringValue(v)) => assert_eq(v, "Operation failed")
    _ => assert_true(false)
  }
  
  // Test fallback to default language when requested language is not available
  let fallback_message = I18n::get_localized_attribute_with_fallback(span, "error.message", "ko", "en")
  match fallback_message {
    Some(StringValue(v)) => assert_eq(v, "Operation failed")
    _ => assert_true(false)
  }
  
  Span::end(span)
  TracerProvider::shutdown(provider)
}

// Test 2: Locale-Aware Formatting
test "locale-aware formatting" {
  // Test number formatting with different locales
  let number = 1234567.89
  
  let formatted_en = I18n::format_number(number, "en-US")
  assert_eq(formatted_en, "1,234,567.89")
  
  let formatted_de = I18n::format_number(number, "de-DE")
  assert_eq(formatted_de, "1.234.567,89")
  
  let formatted_fr = I18n::format_number(number, "fr-FR")
  assert_eq(formatted_fr, "1 234 567,89")
  
  let formatted_zh = I18n::format_number(number, "zh-CN")
  assert_eq(formatted_zh, "1,234,567.89")
  
  let formatted_ja = I18n::format_number(number, "ja-JP")
  assert_eq(formatted_ja, "1,234,567.89")
  
  let formatted_ar = I18n::format_number(number, "ar-SA")
  assert_eq(formatted_ar, "1,234,567.89")
  
  // Test date formatting with different locales
  let timestamp = 1672574400000L // 2023-01-01 12:00:00 UTC
  
  let date_en = I18n::format_date(timestamp, "en-US", "YYYY-MM-DD HH:mm:ss")
  assert_eq(date_en, "2023-01-01 12:00:00")
  
  let date_de = I18n::format_date(timestamp, "de-DE", "DD.MM.YYYY HH:mm:ss")
  assert_eq(date_de, "01.01.2023 12:00:00")
  
  let date_fr = I18n::format_date(timestamp, "fr-FR", "DD/MM/YYYY HH:mm:ss")
  assert_eq(date_fr, "01/01/2023 12:00:00")
  
  let date_zh = I18n::format_date(timestamp, "zh-CN", "YYYY年MM月DD日 HH:mm:ss")
  assert_eq(date_zh, "2023年01月01日 12:00:00")
  
  let date_ja = I18n::format_date(timestamp, "ja-JP", "YYYY年MM月DD日 HH:mm:ss")
  assert_eq(date_ja, "2023年01月01日 12:00:00")
  
  // Test currency formatting with different locales
  let amount = 1234.56
  
  let currency_en = I18n::format_currency(amount, "en-US", "USD")
  assert_eq(currency_en, "$1,234.56")
  
  let currency_de = I18n::format_currency(amount, "de-DE", "EUR")
  assert_eq(currency_de, "1.234,56 €")
  
  let currency_fr = I18n::format_currency(amount, "fr-FR", "EUR")
  assert_eq(currency_fr, "1 234,56 €")
  
  let currency_ja = I18n::format_currency(amount, "ja-JP", "JPY")
  assert_eq(currency_ja, "￥1,235")
  
  let currency_zh = I18n::format_currency(amount, "zh-CN", "CNY")
  assert_eq(currency_zh, "¥1,234.56")
}

// Test 3: Right-to-Left Language Support
test "right-to-left language support" {
  let provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(provider, "rtl_test_tracer")
  let span = Tracer::start_span(tracer, "rtl_test_span")
  
  // Set Arabic attributes
  Span::set_attribute(span, "user.name.ar", StringValue("أحمد محمد"))
  Span::set_attribute(span, "error.message.ar", StringValue("فشلت العملية"))
  Span::set_attribute(span, "operation.description.ar", StringValue("قراءة البيانات من قاعدة البيانات"))
  
  // Set Hebrew attributes
  Span::set_attribute(span, "user.name.he", StringValue("דוד כהן"))
  Span::set_attribute(span, "error.message.he", StringValue("הפעולה נכשלה"))
  Span::set_attribute(span, "operation.description.he", StringValue("קריאת נתונים ממסד הנתונים"))
  
  // Verify Arabic attributes
  let arabic_name = Span::get_attribute(span, "user.name.ar")
  match arabic_name {
    Some(StringValue(v)) => assert_eq(v, "أحمد محمد")
    _ => assert_true(false)
  }
  
  let arabic_error = Span::get_attribute(span, "error.message.ar")
  match arabic_error {
    Some(StringValue(v)) => assert_eq(v, "فشلت العملية")
    _ => assert_true(false)
  }
  
  // Verify Hebrew attributes
  let hebrew_name = Span::get_attribute(span, "user.name.he")
  match hebrew_name {
    Some(StringValue(v)) => assert_eq(v, "דוד כהן")
    _ => assert_true(false)
  }
  
  let hebrew_error = Span::get_attribute(span, "error.message.he")
  match hebrew_error {
    Some(StringValue(v)) => assert_eq(v, "הפעולה נכשלה")
    _ => assert_true(false)
  }
  
  // Test text direction detection
  assert_true(I18n::is_right_to_left("أحمد محمد"))
  assert_true(I18n::is_right_to_left("הפעולה נכשלה"))
  assert_false(I18n::is_right_to_left("Operation failed"))
  assert_false(I18n::is_right_to_left("操作失败"))
  
  Span::end(span)
  TracerProvider::shutdown(provider)
}

// Test 4: Multi-language Log Messages
test "multi-language log messages" {
  let provider = LoggerProvider::new()
  let logger = LoggerProvider::get_logger(provider, "i18n_test_logger")
  
  // Create log records in different languages
  let log_zh = LogRecord::new(Info, "用户登录成功")
  LogRecord::add_attribute(log_zh, "language", StringValue("zh"))
  LogRecord::add_attribute(log_zh, "user.id", StringValue("user123"))
  LogRecord::add_attribute(log_zh, "login.time", StringValue("2023-01-01 12:00:00"))
  
  let log_en = LogRecord::new(Info, "User login successful")
  LogRecord::add_attribute(log_en, "language", StringValue("en"))
  LogRecord::add_attribute(log_en, "user.id", StringValue("user456"))
  LogRecord::add_attribute(log_en, "login.time", StringValue("2023-01-01 12:05:00"))
  
  let log_ja = LogRecord::new(Info, "ユーザーログイン成功")
  LogRecord::add_attribute(log_ja, "language", StringValue("ja"))
  LogRecord::add_attribute(log_ja, "user.id", StringValue("user789"))
  LogRecord::add_attribute(log_ja, "login.time", StringValue("2023-01-01 12:10:00"))
  
  // Emit log records
  Logger::emit(logger, log_zh)
  Logger::emit(logger, log_en)
  Logger::emit(logger, log_ja)
  
  // Verify log records
  assert_eq(LogRecord::severity_number(log_zh), Info)
  match LogRecord::body(log_zh) {
    Some(body) => assert_eq(body, "用户登录成功")
    None => assert_true(false)
  }
  
  assert_eq(LogRecord::severity_number(log_en), Info)
  match LogRecord::body(log_en) {
    Some(body) => assert_eq(body, "User login successful")
    None => assert_true(false)
  }
  
  assert_eq(LogRecord::severity_number(log_ja), Info)
  match LogRecord::body(log_ja) {
    Some(body) => assert_eq(body, "ユーザーログイン成功")
    None => assert_true(false)
  }
  
  // Test language-based log filtering
  let zh_logs = I18n::filter_logs_by_language([log_zh, log_en, log_ja], "zh")
  assert_eq(zh_logs.length(), 1)
  match LogRecord::body(zh_logs[0]) {
    Some(body) => assert_eq(body, "用户登录成功")
    None => assert_true(false)
  }
  
  let en_logs = I18n::filter_logs_by_language([log_zh, log_en, log_ja], "en")
  assert_eq(en_logs.length(), 1)
  match LogRecord::body(en_logs[0]) {
    Some(body) => assert_eq(body, "User login successful")
    None => assert_true(false)
  }
  
  LoggerProvider::shutdown(provider)
}

// Test 5: Localized Error Messages
test "localized error messages" {
  let provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(provider, "error_i18n_tracer")
  
  // Create error codes with localized messages
  let error_codes = [
    ("E001", [
      ("en", "Invalid user credentials"),
      ("zh", "用户凭据无效"),
      ("ja", "ユーザー認証情報が無効です"),
      ("fr", "Identifiants utilisateur invalides"),
      ("de", "Ungültige Benutzeranmeldedaten"),
      ("es", "Credenciales de usuario inválidas"),
      ("ru", "Недействительные учетные данные пользователя"),
      ("ar", "بيانات اعتماد المستخدم غير صالحة")
    ]),
    ("E002", [
      ("en", "Database connection failed"),
      ("zh", "数据库连接失败"),
      ("ja", "データベース接続に失敗しました"),
      ("fr", "Échec de la connexion à la base de données"),
      ("de", "Datenbankverbindung fehlgeschlagen"),
      ("es", "Error en la conexión a la base de datos"),
      ("ru", "Не удалось подключиться к базе данных"),
      ("ar", "فشل الاتصال بقاعدة البيانات")
    ]),
    ("E003", [
      ("en", "Resource not found"),
      ("zh", "未找到资源"),
      ("ja", "リソースが見つかりません"),
      ("fr", "Ressource introuvable"),
      ("de", "Ressource nicht gefunden"),
      ("es", "Recurso no encontrado"),
      ("ru", "Ресурс не найден"),
      ("ar", "لم يتم العثور على المورد")
    ])
  ]
  
  // Test error message localization
  for (code, messages) in error_codes {
    for (locale, expected_message) in messages {
      let localized_message = I18n::get_error_message(code, locale)
      match localized_message {
        Some(message) => assert_eq(message, expected_message)
        None => assert_true(false)
      }
    }
  }
  
  // Test fallback to default language when locale is not supported
  let fallback_message = I18n::get_error_message_with_fallback("E001", "ko", "en")
  match fallback_message {
    Some(message) => assert_eq(message, "Invalid user credentials")
    None => assert_true(false)
  }
  
  // Create spans with localized error messages
  let span1 = Tracer::start_span(tracer, "operation_with_error")
  let localized_error1 = I18n::get_error_message("E001", "zh")
  match localized_error1 {
    Some(message) => {
      Span::set_status(span1, Error, Some(message))
      Span::add_event(span1, "error_occurred", Some([
        ("error.code", StringValue("E001")),
        ("error.message.zh", StringValue(message))
      ]))
    }
    None => assert_true(false)
  }
  
  let span2 = Tracer::start_span(tracer, "operation_with_error2")
  let localized_error2 = I18n::get_error_message("E002", "ja")
  match localized_error2 {
    Some(message) => {
      Span::set_status(span2, Error, Some(message))
      Span::add_event(span2, "error_occurred", Some([
        ("error.code", StringValue("E002")),
        ("error.message.ja", StringValue(message))
      ]))
    }
    None => assert_true(false)
  }
  
  Span::end(span2)
  Span::end(span1)
  TracerProvider::shutdown(provider)
}

// Test 6: Time Zone Support
test "time zone support" {
  let timestamp = 1672574400000L // 2023-01-01 12:00:00 UTC
  
  // Test time zone conversion
  let utc_time = I18n::convert_timezone(timestamp, "UTC", "UTC")
  assert_eq(utc_time, "2023-01-01 12:00:00 UTC")
  
  let est_time = I18n::convert_timezone(timestamp, "UTC", "America/New_York")
  assert_eq(est_time, "2023-01-01 07:00:00 EST")
  
  let pst_time = I18n::convert_timezone(timestamp, "UTC", "America/Los_Angeles")
  assert_eq(pst_time, "2023-01-01 04:00:00 PST")
  
  let cet_time = I18n::convert_timezone(timestamp, "UTC", "Europe/Paris")
  assert_eq(cet_time, "2023-01-01 13:00:00 CET")
  
  let jst_time = I18n::convert_timezone(timestamp, "UTC", "Asia/Tokyo")
  assert_eq(jst_time, "2023-01-01 21:00:00 JST")
  
  let cst_time = I18n::convert_timezone(timestamp, "UTC", "Asia/Shanghai")
  assert_eq(cst_time, "2023-01-01 20:00:00 CST")
  
  // Test time zone-aware date formatting
  let formatted_date_utc = I18n::format_date_with_timezone(timestamp, "en-US", "UTC", "YYYY-MM-DD HH:mm:ss z")
  assert_eq(formatted_date_utc, "2023-01-01 12:00:00 UTC")
  
  let formatted_date_est = I18n::format_date_with_timezone(timestamp, "en-US", "America/New_York", "YYYY-MM-DD HH:mm:ss z")
  assert_eq(formatted_date_est, "2023-01-01 07:00:00 EST")
  
  let formatted_date_jst = I18n::format_date_with_timezone(timestamp, "ja-JP", "Asia/Tokyo", "YYYY年MM月DD日 HH:mm:ss z")
  assert_eq(formatted_date_jst, "2023年01月01日 21:00:00 JST")
  
  // Test time zone validation
  assert_true(I18n::is_valid_timezone("UTC"))
  assert_true(I18n::is_valid_timezone("America/New_York"))
  assert_true(I18n::is_valid_timezone("Europe/Paris"))
  assert_true(I18n::is_valid_timezone("Asia/Tokyo"))
  assert_false(I18n::is_valid_timezone("Invalid/Timezone"))
  assert_false(I18n::is_valid_timezone(""))
}

// Test 7: Cultural Formatting Conventions
test "cultural formatting conventions" {
  // Test name formatting for different cultures
  let western_name = I18n::format_name("John", "Doe", "en-US")
  assert_eq(western_name, "John Doe")
  
  let eastern_name = I18n::format_name("张", "三", "zh-CN")
  assert_eq(eastern_name, "张三")
  
  let japanese_name = I18n::format_name("田中", "太郎", "ja-JP")
  assert_eq(japanese_name, "田中太郎")
  
  let korean_name = I18n::format_name("김", "철수", "ko-KR")
  assert_eq(korean_name, "김철수")
  
  // Test address formatting for different cultures
  let us_address = I18n::format_address("123 Main St", "New York", "NY", "10001", "en-US")
  assert_eq(us_address, "123 Main St\nNew York, NY 10001")
  
  let jp_address = I18n::format_address("〒100-0001", "東京都千代田区千代田", "1", "1", "ja-JP")
  assert_eq(jp_address, "〒100-0001\n東京都千代田区千代田1-1")
  
  let cn_address = I18n::format_address("100000", "北京市", "东城区", "长安街1号", "zh-CN")
  assert_eq(cn_address, "中国北京市东城区\n长安街1号\n100000")
  
  // Test phone number formatting for different regions
  let us_phone = I18n::format_phone_number("+12125551234", "en-US")
  assert_eq(us_phone, "+1 (212) 555-1234")
  
  let jp_phone = I18n::format_phone_number("+81312345678", "ja-JP")
  assert_eq(jp_phone, "+81-3-1234-5678")
  
  let cn_phone = I18n::format_phone_number("+8613812345678", "zh-CN")
  assert_eq(cn_phone, "+86 138 1234 5678")
  
  let de_phone = I18n::format_phone_number("+493012345678", "de-DE")
  assert_eq(de_phone, "+49 30 12345678")
}

// Test 8: Multi-language Resource Descriptions
test "multi-language resource descriptions" {
  // Create resource with multi-language descriptions
  let resource_attrs = [
    ("service.name", StringValue("payment-service")),
    ("service.description.en", StringValue("Payment processing service")),
    ("service.description.zh", StringValue("支付处理服务")),
    ("service.description.ja", StringValue("支払い処理サービス")),
    ("service.description.fr", StringValue("Service de traitement des paiements")),
    ("service.description.de", StringValue("Zahlungsverarbeitungsdienst")),
    ("service.description.es", StringValue("Servicio de procesamiento de pagos")),
    ("service.description.ru", StringValue("Сервис обработки платежей")),
    ("service.description.ar", StringValue("خدمة معالجة الدفعات")),
    
    ("service.owner.name.en", StringValue("Payment Team")),
    ("service.owner.name.zh", StringValue("支付团队")),
    ("service.owner.name.ja", StringValue("支払いチーム")),
    ("service.owner.email", StringValue("payment@example.com")),
    
    ("service.version", StringValue("1.2.3")),
    ("service.environment", StringValue("production"))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), resource_attrs)
  
  // Test retrieving localized descriptions
  let en_description = I18n::get_localized_resource_attribute(resource, "service.description", "en")
  match en_description {
    Some(StringValue(v)) => assert_eq(v, "Payment processing service")
    _ => assert_true(false)
  }
  
  let zh_description = I18n::get_localized_resource_attribute(resource, "service.description", "zh")
  match zh_description {
    Some(StringValue(v)) => assert_eq(v, "支付处理服务")
    _ => assert_true(false)
  }
  
  let ja_description = I18n::get_localized_resource_attribute(resource, "service.description", "ja")
  match ja_description {
    Some(StringValue(v)) => assert_eq(v, "支払い処理サービス")
    _ => assert_true(false)
  }
  
  // Test retrieving localized owner information
  let en_owner = I18n::get_localized_resource_attribute(resource, "service.owner.name", "en")
  match en_owner {
    Some(StringValue(v)) => assert_eq(v, "Payment Team")
    _ => assert_true(false)
  }
  
  let zh_owner = I18n::get_localized_resource_attribute(resource, "service.owner.name", "zh")
  match zh_owner {
    Some(StringValue(v)) => assert_eq(v, "支付团队")
    _ => assert_true(false)
  }
  
  // Test fallback to non-localized attribute
  let version = Resource::get_attribute(resource, "service.version")
  match version {
    Some(StringValue(v)) => assert_eq(v, "1.2.3")
    _ => assert_true(false)
  }
  
  // Test resource description serialization with localization
  let en_resource_json = I18n::serialize_resource_with_localization(resource, "en")
  assert_true(en_resource_json.contains("Payment processing service"))
  assert_true(en_resource_json.contains("Payment Team"))
  
  let zh_resource_json = I18n::serialize_resource_with_localization(resource, "zh")
  assert_true(zh_resource_json.contains("支付处理服务"))
  assert_true(zh_resource_json.contains("支付团队"))
}

// Test 9: Locale-Aware Metric Names and Descriptions
test "locale-aware metric names and descriptions" {
  let provider = MeterProvider::new()
  let meter = MeterProvider::get_meter(provider, "i18n_test_meter")
  
  // Create metrics with localized names and descriptions
  let counter = Meter::create_counter(meter, "user_login_count", None, None)
  I18n::set_metric_localization(counter, "name", [
    ("en", "User Login Count"),
    ("zh", "用户登录次数"),
    ("ja", "ユーザーログイン数"),
    ("fr", "Nombre de connexions utilisateur"),
    ("de", "Benutzeranmeldezahl"),
    ("es", "Recuento de inicios de sesión de usuario"),
    ("ru", "Количество входов пользователей"),
    ("ar", "عدد عمليات تسجيل دخول المستخدم")
  ])
  
  I18n::set_metric_localization(counter, "description", [
    ("en", "Total number of user logins"),
    ("zh", "用户登录总数"),
    ("ja", "ユーザーログインの総数"),
    ("fr", "Nombre total de connexions utilisateur"),
    ("de", "Gesamtzahl der Benutzeranmeldungen"),
    ("es", "Número total de inicios de sesión de usuario"),
    ("ru", "Общее количество входов пользователей"),
    ("ar", "إجمالي عدد عمليات تسجيل دخول المستخدم")
  ])
  
  // Test retrieving localized metric names
  let en_name = I18n::get_localized_metric_name(counter, "en")
  match en_name {
    Some(name) => assert_eq(name, "User Login Count")
    None => assert_true(false)
  }
  
  let zh_name = I18n::get_localized_metric_name(counter, "zh")
  match zh_name {
    Some(name) => assert_eq(name, "用户登录次数")
    None => assert_true(false)
  }
  
  let ja_name = I18n::get_localized_metric_name(counter, "ja")
  match ja_name {
    Some(name) => assert_eq(name, "ユーザーログイン数")
    None => assert_true(false)
  }
  
  // Test retrieving localized metric descriptions
  let en_description = I18n::get_localized_metric_description(counter, "en")
  match en_description {
    Some(desc) => assert_eq(desc, "Total number of user logins")
    None => assert_true(false)
  }
  
  let zh_description = I18n::get_localized_metric_description(counter, "zh")
  match zh_description {
    Some(desc) => assert_eq(desc, "用户登录总数")
    None => assert_true(false)
  }
  
  let ja_description = I18n::get_localized_metric_description(counter, "ja")
  match ja_description {
    Some(desc) => assert_eq(desc, "ユーザーログインの総数")
    None => assert_true(false)
  }
  
  // Test metric serialization with localization
  let en_metric_json = I18n::serialize_metric_with_localization(counter, "en")
  assert_true(en_metric_json.contains("User Login Count"))
  assert_true(en_metric_json.contains("Total number of user logins"))
  
  let zh_metric_json = I18n::serialize_metric_with_localization(counter, "zh")
  assert_true(zh_metric_json.contains("用户登录次数"))
  assert_true(zh_metric_json.contains("用户登录总数"))
  
  MeterProvider::shutdown(provider)
}

// Test 10: Unicode and Special Character Handling
test "unicode and special character handling" {
  let provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(provider, "unicode_test_tracer")
  let span = Tracer::start_span(tracer, "unicode_test_span")
  
  // Test Unicode characters in attributes
  Span::set_attribute(span, "emoji.test", StringValue("🚀🔥💯"))
  Span::set_attribute(span, "chinese.test", StringValue("中文测试"))
  Span::set_attribute(span, "japanese.test", StringValue("日本語テスト"))
  Span::set_attribute(span, "korean.test", StringValue("한국어 테스트"))
  Span::set_attribute(span, "arabic.test", StringValue("اختبار عربي"))
  Span::set_attribute(span, "hebrew.test", StringValue("בדיקה עברית"))
  Span::set_attribute(span, "russian.test", StringValue("Русский тест"))
  Span::set_attribute(span, "thai.test", StringValue("การทดสอบภาษาไทย"))
  Span::set_attribute(span, "math.symbols", StringValue("∑∏∫∆∇∂"))
  Span::set_attribute(span, "currency.symbols", StringValue("$€£¥₹₽₩"))
  
  // Verify Unicode attributes
  let emoji_attr = Span::get_attribute(span, "emoji.test")
  match emoji_attr {
    Some(StringValue(v)) => assert_eq(v, "🚀🔥💯")
    _ => assert_true(false)
  }
  
  let chinese_attr = Span::get_attribute(span, "chinese.test")
  match chinese_attr {
    Some(StringValue(v)) => assert_eq(v, "中文测试")
    _ => assert_true(false)
  }
  
  let japanese_attr = Span::get_attribute(span, "japanese.test")
  match japanese_attr {
    Some(StringValue(v)) => assert_eq(v, "日本語テスト")
    _ => assert_true(false)
  }
  
  let korean_attr = Span::get_attribute(span, "korean.test")
  match korean_attr {
    Some(StringValue(v)) => assert_eq(v, "한국어 테스트")
    _ => assert_true(false)
  }
  
  let arabic_attr = Span::get_attribute(span, "arabic.test")
  match arabic_attr {
    Some(StringValue(v)) => assert_eq(v, "اختبار عربي")
    _ => assert_true(false)
  }
  
  let hebrew_attr = Span::get_attribute(span, "hebrew.test")
  match hebrew_attr {
    Some(StringValue(v)) => assert_eq(v, "בדיקה עברית")
    _ => assert_true(false)
  }
  
  let russian_attr = Span::get_attribute(span, "russian.test")
  match russian_attr {
    Some(StringValue(v)) => assert_eq(v, "Русский тест")
    _ => assert_true(false)
  }
  
  let thai_attr = Span::get_attribute(span, "thai.test")
  match thai_attr {
    Some(StringValue(v)) => assert_eq(v, "การทดสอบภาษาไทย")
    _ => assert_true(false)
  }
  
  let math_attr = Span::get_attribute(span, "math.symbols")
  match math_attr {
    Some(StringValue(v)) => assert_eq(v, "∑∏∫∆∇∂")
    _ => assert_true(false)
  }
  
  let currency_attr = Span::get_attribute(span, "currency.symbols")
  match currency_attr {
    Some(StringValue(v)) => assert_eq(v, "$€£¥₹₽₩")
    _ => assert_true(false)
  }
  
  // Test Unicode string operations
  let unicode_string = "Hello 世界 🌍"
  assert_eq(I18n::unicode_length(unicode_string), 9)
  assert_eq(I18n::unicode_substring(unicode_string, 6, 3), "世界 🌍")
  assert_true(I18n::contains_unicode(unicode_string))
  assert_false(I18n::contains_unicode("Hello World"))
  
  // Test Unicode normalization
  let composed = "café"
  let decomposed = I18n::unicode_normalize(composed, "NFD")
  assert_not_eq(composed, decomposed)
  assert_eq(I18n::unicode_normalize(decomposed, "NFC"), composed)
  
  Span::end(span)
  TracerProvider::shutdown(provider)
}