// Internationalization and Localization Tests
// This file contains test cases for internationalization and localization support

test "unicode and multilingual attribute handling" {
  // Test handling of Unicode and multilingual content in attributes
  let attrs = azimuth::Attributes::new()
  
  // Test various Unicode scripts
  azimuth::Attributes::set(attrs, "chinese.text", azimuth::StringValue("测试中文字符串支持"))
  azimuth::Attributes::set(attrs, "japanese.text", azimuth::StringValue("日本語テスト"))
  azimuth::Attributes::set(attrs, "korean.text", azimuth::StringValue("한국어 테스트"))
  azimuth::Attributes::set(attrs, "arabic.text", azimuth::StringValue("اختبار اللغة العربية"))
  azimuth::Attributes::set(attrs, "hebrew.text", azimuth::StringValue("בדיקת שפה עברית"))
  azimuth::Attributes::set(attrs, "russian.text", azimuth::StringValue("Тест на русском языке"))
  azimuth::Attributes::set(attrs, "thai.text", azimuth::StringValue("การทดสอบภาษาไทย"))
  azimuth::Attributes::set(attrs, "emoji.text", azimuth::StringValue("🚀🔥💡🎉✨"))
  
  // Test right-to-left languages
  azimuth::Attributes::set(attrs, "rtl.hebrew", azimuth::StringValue("שלום עולם"))
  azimuth::Attributes::set(attrs, "rtl.arabic", azimuth::StringValue("مرحبا بالعالم"))
  
  // Test mixed language content
  azimuth::Attributes::set(attrs, "mixed.content", azimuth::StringValue("Hello 世界 🌍 Bonjour le monde"))
  
  // Test special Unicode characters
  azimuth::Attributes::set(attrs, "special.chars", azimuth::StringValue("©®™€£¥§¶†‡•…‰‹›""''""–—"))
  azimuth::Attributes::set(attrs, "math.symbols", azimuth::StringValue("∑∏∫∆∇∂√∞≈≠≤≥±×÷"))
  azimuth::Attributes::set(attrs, "currency.symbols", azimuth::StringValue("$€£¥₹₽₩₪₫₡₦₨₱₲₴₸₼₽"))
  
  // Verify attribute retrieval and integrity
  let chinese_text = azimuth::Attributes::get(attrs, "chinese.text")
  let japanese_text = azimuth::Attributes::get(attrs, "japanese.text")
  let korean_text = azimuth::Attributes::get(attrs, "korean.text")
  let arabic_text = azimuth::Attributes::get(attrs, "arabic.text")
  let hebrew_text = azimuth::Attributes::get(attrs, "hebrew.text")
  let russian_text = azimuth::Attributes::get(attrs, "russian.text")
  let thai_text = azimuth::Attributes::get(attrs, "thai.text")
  let emoji_text = azimuth::Attributes::get(attrs, "emoji.text")
  
  assert_eq(chinese_text, Some(azimuth::StringValue("测试中文字符串支持")))
  assert_eq(japanese_text, Some(azimuth::StringValue("日本語テスト")))
  assert_eq(korean_text, Some(azimuth::StringValue("한국어 테스트")))
  assert_eq(arabic_text, Some(azimuth::StringValue("اختبار اللغة العربية")))
  assert_eq(hebrew_text, Some(azimuth::StringValue("בדיקת שפה עברית")))
  assert_eq(russian_text, Some(azimuth::StringValue("Тест на русском языке")))
  assert_eq(thai_text, Some(azimuth::StringValue("การทดสอบภาษาไทย")))
  assert_eq(emoji_text, Some(azimuth::StringValue("🚀🔥💡🎉✨")))
  
  // Test array attributes with multilingual content
  let multilingual_array = azimuth::ArrayStringValue([
    "Hello World", "Hola Mundo", "Bonjour le monde", "Hallo Welt", 
    "Ciao Mondo", "Olá Mundo", "こんにちは世界", "안녕하세요 세계",
    "مرحبا بالعالم", "שלום עולם", "Привет мир"
  ])
  azimuth::Attributes::set(attrs, "multilingual.greetings", multilingual_array)
  
  let retrieved_array = azimuth::Attributes::get(attrs, "multilingual.greetings")
  assert_eq(retrieved_array, Some(multilingual_array))
}

test "localized log messages" {
  // Test localized log messages
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "i18n-logger")
  
  // Create localized messages
  let messages = [
    ("en", "User login successful"),
    ("es", "Inicio de sesión de usuario exitoso"),
    ("fr", "Connexion utilisateur réussie"),
    ("de", "Benutzeranmeldung erfolgreich"),
    ("ja", "ユーザーログイン成功"),
    ("ko", "사용자 로그인 성공"),
    ("zh", "用户登录成功"),
    ("ar", "تسجيل دخول المستخدم بنجاح"),
    ("ru", "Вход пользователя выполнен успешно"),
    ("th", "การเข้าสู่ระบบของผู้ใช้สำเร็จ")
  ]
  
  // Log messages in different languages
  for (locale, message) in messages {
    let localized_attrs = [
      ("locale", azimuth::StringValue(locale)),
      ("user.id", azimuth::StringValue("user-12345")),
      ("timestamp", azimuth::StringValue(azimuth::Time::now().to_string()))
    ]
    
    azimuth::Logger::log(logger, azimuth::Info, message, localized_attrs)
  }
  
  // Test error messages in different languages
  let error_messages = [
    ("en", "Database connection failed"),
    ("es", "Error de conexión a la base de datos"),
    ("fr", "Échec de connexion à la base de données"),
    ("de", "Datenbankverbindung fehlgeschlagen"),
    ("ja", "データベース接続に失敗しました"),
    ("ko", "데이터베이스 연결 실패"),
    ("zh", "数据库连接失败"),
    ("ar", "فشل الاتصال بقاعدة البيانات"),
    ("ru", "Не удалось подключиться к базе данных"),
    ("th", "การเชื่อมต่อฐานข้อมูลล้มเหลว")
  ]
  
  for (locale, message) in error_messages {
    let error_attrs = [
      ("locale", azimuth::StringValue(locale)),
      ("error.code", azimuth::IntValue(500)),
      ("retry.count", azimuth::IntValue(3))
    ]
    
    azimuth::Logger::log(logger, azimuth::Error, message, error_attrs)
  }
  
  // Collect and verify log records
  let log_records = azimuth::Logger::get_log_records(logger)
  assert_eq(log_records.length, messages.length + error_messages.length)
  
  // Verify message integrity
  for i = 0; i < messages.length; i = i + 1 {
    let (locale, expected_message) = messages[i]
    let log_record = log_records[i]
    
    assert_eq(azimuth::LogRecord::body(log_record), expected_message)
    assert_eq(azimuth::LogRecord::get_attribute(log_record, "locale"), Some(azimuth::StringValue(locale)))
  }
  
  // Verify error messages
  for i = 0; i < error_messages.length; i = i + 1 {
    let (locale, expected_message) = error_messages[i]
    let log_record = log_records[messages.length + i]
    
    assert_eq(azimuth::LogRecord::body(log_record), expected_message)
    assert_eq(azimuth::LogRecord::severity(log_record), azimuth::Error)
    assert_eq(azimuth::LogRecord::get_attribute(log_record, "locale"), Some(azimuth::StringValue(locale)))
  }
}

test "localized span names and events" {
  // Test localized span names and events
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "i18n-tracer")
  
  // Create spans with localized names
  let span_operations = [
    ("en", "HTTP Request Processing"),
    ("es", "Procesamiento de Solicitud HTTP"),
    ("fr", "Traitement de la Requête HTTP"),
    ("de", "HTTP-Anfrageverarbeitung"),
    ("ja", "HTTPリクエスト処理"),
    ("ko", "HTTP 요청 처리"),
    ("zh", "HTTP请求处理"),
    ("ar", "معالجة طلب HTTP"),
    ("ru", "Обработка HTTP-запроса"),
    ("th", "การประมวลผลคำขอ HTTP")
  ]
  
  for (locale, operation_name) in span_operations {
    let span = azimuth::Tracer::start_span(tracer, operation_name)
    
    // Add localized attributes
    let localized_attrs = [
      ("locale", azimuth::StringValue(locale)),
      ("operation.type", azimuth::StringValue("http.request")),
      ("service.name", azimuth::StringValue("web-service"))
    ]
    
    for (key, value) in localized_attrs {
      azimuth::Span::set_attribute(span, key, value)
    }
    
    // Add localized events
    let events = [
      ("request.received", [
        ("en", "HTTP request received"),
        ("es", "Solicitud HTTP recibida"),
        ("fr", "Requête HTTP reçue"),
        ("de", "HTTP-Anfrage empfangen"),
        ("ja", "HTTPリクエスト受信"),
        ("ko", "HTTP 요청 수신"),
        ("zh", "收到HTTP请求"),
        ("ar", "تم استلام طلب HTTP"),
        ("ru", "HTTP-запрос получен"),
        ("th", "ได้รับคำขอ HTTP")
      ]),
      ("processing.started", [
        ("en", "Started processing request"),
        ("es", "Comenzó el procesamiento de la solicitud"),
        ("fr", "Traitement de la requête démarré"),
        ("de", "Verarbeitung der Anfrage gestartet"),
        ("ja", "リクエストの処理を開始"),
        ("ko", "요청 처리 시작"),
        ("zh", "开始处理请求"),
        ("ar", "بدأ معالجة الطلب"),
        ("ru", "Начата обработка запроса"),
        ("th", "เริ่มประมวลผลคำขอ")
      ]),
      ("request.completed", [
        ("en", "Request processing completed"),
        ("es", "Procesamiento de solicitud completado"),
        ("fr", "Traitement de la requête terminé"),
        ("de", "Anfrageverarbeitung abgeschlossen"),
        ("ja", "リクエストの処理が完了"),
        ("ko", "요청 처리 완료"),
        ("zh", "请求处理完成"),
        ("ar", "اكتملت معالجة الطلب"),
        ("ru", "Обработка запроса завершена"),
        ("th", "การประมวลผลคำขอเสร็จสมบูรณ์")
      ])
    ]
    
    for (event_key, event_messages) in events {
      let message = match event_messages.get(locale) {
        Some(msg) => msg,
        None => event_messages.get("en").unwrap_or("Unknown message")
      }
      
      azimuth::Span::add_event(span, event_key, [
        ("message", azimuth::StringValue(message)),
        ("locale", azimuth::StringValue(locale))
      ])
    }
    
    azimuth::Span::end(span)
  }
  
  // Verify span integrity
  let spans = azimuth::Tracer::get_finished_spans(tracer)
  assert_eq(spans.length, span_operations.length)
  
  for i = 0; i < spans.length; i = i + 1 {
    let (expected_locale, expected_name) = span_operations[i]
    let span = spans[i]
    
    assert_eq(azimuth::Span::name(span), expected_name)
    assert_eq(azimuth::Span::get_attribute(span, "locale"), Some(azimuth::StringValue(expected_locale)))
    
    // Verify events
    let events = azimuth::Span::get_events(span)
    assert_eq(events.length, 3)
    
    for event in events {
      let event_locale = azimuth::SpanEvent::get_attribute(event, "locale")
      assert_eq(event_locale, Some(azimuth::StringValue(expected_locale)))
    }
  }
}

test "localized metric names and descriptions" {
  // Test localized metric names and descriptions
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "i18n-metrics")
  
  // Define localized metric configurations
  let metric_configs = [
    {
      "name": "request.count",
      "descriptions": [
        ("en", "Total number of HTTP requests"),
        ("es", "Número total de solicitudes HTTP"),
        ("fr", "Nombre total de requêtes HTTP"),
        ("de", "Gesamtzahl der HTTP-Anfragen"),
        ("ja", "HTTPリクエストの総数"),
        ("ko", "HTTP 요청의 총수"),
        ("zh", "HTTP请求总数"),
        ("ar", "إجمالي عدد طلبات HTTP"),
        ("ru", "Общее количество HTTP-запросов"),
        ("th", "จำนวนคำขอ HTTP ทั้งหมด")
      ]
    },
    {
      "name": "response.time",
      "descriptions": [
        ("en", "HTTP response time in milliseconds"),
        ("es", "Tiempo de respuesta HTTP en milisegundos"),
        ("fr", "Temps de réponse HTTP en millisecondes"),
        ("de", "HTTP-Antwortzeit in Millisekunden"),
        ("ja", "HTTP応答時間（ミリ秒）"),
        ("ko", "HTTP 응답 시간(밀리초)"),
        ("zh", "HTTP响应时间（毫秒）"),
        ("ar", "وقت استجابة HTTP بالمللي ثانية"),
        ("ru", "Время ответа HTTP в миллисекундах"),
        ("th", "เวลาตอบสนอง HTTP ในมิลลิวินาที")
      ]
    },
    {
      "name": "error.rate",
      "descriptions": [
        ("en", "Rate of errors per minute"),
        ("es", "Tasa de errores por minuto"),
        ("fr", "Taux d'erreurs par minute"),
        ("de", "Fehlerrate pro Minute"),
        ("ja", "1分あたりのエラー率"),
        ("ko", "분당 오류율"),
        ("zh", "每分钟错误率"),
        ("ar", "معدل الأخطاء في الدقيقة"),
        ("ru", "Количество ошибок в минуту"),
        ("th", "อัตราความผิดพลาดต่อนาที")
      ]
    }
  ]
  
  // Create metrics with localized descriptions
  for config in metric_configs {
    let name = config["name"]
    
    // Create metrics with English description as default
    let english_description = match config["descriptions"].find(|(locale, _)| locale == "en") {
      Some((_, desc)) => desc,
      None => "Metric description"
    }
    
    let counter = azimuth::Meter::create_counter(meter, name, english_description, "1")
    
    // Add localized descriptions as attributes
    for (locale, description) in config["descriptions"] {
      let attribute_key = "description." + locale
      azimuth::Counter::add(counter, 0, [(attribute_key, azimuth::StringValue(description))])
    }
    
    // Record some sample data
    azimuth::Counter::add(counter, 10, [
      ("locale", azimuth::StringValue("en")),
      ("endpoint", azimuth::StringValue("/api/test"))
    ])
    
    azimuth::Counter::add(counter, 5, [
      ("locale", azimuth::StringValue("es")),
      ("endpoint", azimuth::StringValue("/api/prueba"))
    ])
  }
  
  // Collect and verify metrics
  let metrics = azimuth::Meter::collect_metrics(meter)
  assert_true(metrics.length >= metric_configs.length)
  
  // Verify localized descriptions are preserved
  for config in metric_configs {
    let name = config["name"]
    let metric_data = azimuth::MetricData::get_by_name(metrics, name)
    
    match metric_data {
      Some(data) => {
        // Verify English description is primary
        let english_desc = match config["descriptions"].find(|(locale, _)| locale == "en") {
          Some((_, desc)) => desc,
          None => ""
        }
        assert_eq(azimuth::MetricData::description(data), Some(english_desc))
        
        // Verify localized descriptions are in attributes
        for (locale, expected_description) in config["descriptions"] {
          if locale != "en" {
            let attribute_key = "description." + locale
            let found_desc = azimuth::MetricData::get_attribute(data, attribute_key)
            assert_eq(found_desc, Some(azimuth::StringValue(expected_description)))
          }
        }
      }
      None => assert_true(false, "Metric not found: " + name)
    }
  }
}