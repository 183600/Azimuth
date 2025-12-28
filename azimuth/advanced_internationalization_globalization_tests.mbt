// Advanced Internationalization and Globalization Tests for Azimuth
// Tests internationalization, localization, and multicultural support

test "multilingual_attribute_values" {
  // Test attribute values with different languages and character sets
  
  // Chinese characters
  let chinese_text = "中文测试文本"
  let chinese_attr = StringValue(chinese_text)
  
  // Japanese characters
  let japanese_text = "日本語テキスト"
  let japanese_attr = StringValue(japanese_text)
  
  // Korean characters
  let korean_text = "한국어 텍스트"
  let korean_attr = StringValue(korean_text)
  
  // Arabic characters
  let arabic_text = "نص عربي"
  let arabic_attr = StringValue(arabic_text)
  
  // Hebrew characters
  let hebrew_text = "טקסט עברי"
  let hebrew_attr = StringValue(hebrew_text)
  
  // Russian characters
  let russian_text = "Русский текст"
  let russian_attr = StringValue(russian_text)
  
  // Hindi characters
  let hindi_text = "हिंदी पाठ"
  let hindi_attr = StringValue(hindi_text)
  
  // Thai characters
  let thai_text = "ข้อความภาษาไทย"
  let thai_attr = StringValue(thai_text)
  
  // Verify all character sets are preserved
  match chinese_attr {
    StringValue(s) => assert_eq(s, chinese_text)
    _ => @test.fail("Expected StringValue")
  }
  
  match japanese_attr {
    StringValue(s) => assert_eq(s, japanese_text)
    _ => @test.fail("Expected StringValue")
  }
  
  match korean_attr {
    StringValue(s) => assert_eq(s, korean_text)
    _ => @test.fail("Expected StringValue")
  }
  
  match arabic_attr {
    StringValue(s) => assert_eq(s, arabic_text)
    _ => @test.fail("Expected StringValue")
  }
  
  match hebrew_attr {
    StringValue(s) => assert_eq(s, hebrew_text)
    _ => @test.fail("Expected StringValue")
  }
  
  match russian_attr {
    StringValue(s) => assert_eq(s, russian_text)
    _ => @test.fail("Expected StringValue")
  }
  
  match hindi_attr {
    StringValue(s) => assert_eq(s, hindi_text)
    _ => @test.fail("Expected StringValue")
  }
  
  match thai_attr {
    StringValue(s) => assert_eq(s, thai_text)
    _ => @test.fail("Expected StringValue")
  }
}

test "emoji_and_symbol_support" {
  // Test emoji and symbol support in telemetry data
  
  // Various emoji categories
  let faces_emoji = "😀😃😄😁😆😅😂🤣😊😇"
  let animals_emoji = "🐶🐱🐭🐹🐰🦊🐻🐼🐨🐯"
  let food_emoji = "🍎🍊🍋🍌🍉🍇🍓🫐🍈🍒"
  let activities_emoji = "⚽️🏀🏈⚾️🥎🎾🏐🏉🥏🎱"
  let objects_emoji = "⌚️📱💻⌨️🖥️🖨️🖱️🖲️🕹️🗒️"
  let symbols_emoji = "❤️🧡💛💚💙💜🖤🤍🤎💔"
  let flags_emoji = "🏳️🏴🏁🚩🏳️‍🌈🏳️‍⚧️🏴‍☠️"
  
  let faces_attr = StringValue(faces_emoji)
  let animals_attr = StringValue(animals_emoji)
  let food_attr = StringValue(food_emoji)
  let activities_attr = StringValue(activities_emoji)
  let objects_attr = StringValue(objects_emoji)
  let symbols_attr = StringValue(symbols_emoji)
  let flags_attr = StringValue(flags_emoji)
  
  // Verify emoji preservation
  match faces_attr {
    StringValue(s) => assert_eq(s, faces_emoji)
    _ => @test.fail("Expected StringValue")
  }
  
  match animals_attr {
    StringValue(s) => assert_eq(s, animals_emoji)
    _ => @test.fail("Expected StringValue")
  }
  
  match food_attr {
    StringValue(s) => assert_eq(s, food_emoji)
    _ => @test.fail("Expected StringValue")
  }
  
  match activities_attr {
    StringValue(s) => assert_eq(s, activities_emoji)
    _ => @test.fail("Expected StringValue")
  }
  
  match objects_attr {
    StringValue(s) => assert_eq(s, objects_emoji)
    _ => @test.fail("Expected StringValue")
  }
  
  match symbols_attr {
    StringValue(s) => assert_eq(s, symbols_emoji)
    _ => @test.fail("Expected StringValue")
  }
  
  match flags_attr {
    StringValue(s) => assert_eq(s, flags_emoji)
    _ => @test.fail("Expected StringValue")
  }
  
  // Test mathematical symbols
  let math_symbols = "∑∏∫∆∇∂∞±×÷≤≥≠≈∝∈∉∪∩⊂⊃⊆⊇"
  let math_attr = StringValue(math_symbols)
  
  match math_attr {
    StringValue(s) => assert_eq(s, math_symbols)
    _ => @test.fail("Expected StringValue")
  }
  
  // Test currency symbols
  let currency_symbols = "$€£¥₹₽₩₪₫₡₨₦₱₲₴₸₼₽"
  let currency_attr = StringValue(currency_symbols)
  
  match currency_attr {
    StringValue(s) => assert_eq(s, currency_symbols)
    _ => @test.fail("Expected StringValue")
  }
}

test "right_to_left_text_support" {
  // Test right-to-left text support (Arabic, Hebrew, etc.)
  
  // Arabic text with mixed content
  let arabic_mixed = "مرحبا بالعالم! Hello World!"
  let arabic_numbers = "الأرقام: ١٢٣٤٥٦٧٨٩٠"
  let arabic_with_english = "اسمي John وأنا من USA"
  
  // Hebrew text with mixed content
  let hebrew_mixed = "שלום עולם! Hello World!"
  let hebrew_numbers = "מספרים: אבגדהוזחטי"
  let hebrew_with_english = "השם שלי הוא John ואני מ־USA"
  
  // Persian text
  let persian_text = "سلام دنیا! Hello World!"
  let persian_numbers = "اعداد: ۱۲۳۴۵۶۷۸۹۰"
  
  // Urdu text
  let urdu_text = "ہیلو ورلڈ! Hello World!"
  let urdu_numbers = "اعداد: ۱۲۳۴۵۶۷۸۹۰"
  
  let arabic_mixed_attr = StringValue(arabic_mixed)
  let arabic_numbers_attr = StringValue(arabic_numbers)
  let arabic_with_english_attr = StringValue(arabic_with_english)
  
  let hebrew_mixed_attr = StringValue(hebrew_mixed)
  let hebrew_numbers_attr = StringValue(hebrew_numbers)
  let hebrew_with_english_attr = StringValue(hebrew_with_english)
  
  let persian_text_attr = StringValue(persian_text)
  let persian_numbers_attr = StringValue(persian_numbers)
  
  let urdu_text_attr = StringValue(urdu_text)
  let urdu_numbers_attr = StringValue(urdu_numbers)
  
  // Verify RTL text preservation
  match arabic_mixed_attr {
    StringValue(s) => assert_eq(s, arabic_mixed)
    _ => @test.fail("Expected StringValue")
  }
  
  match arabic_numbers_attr {
    StringValue(s) => assert_eq(s, arabic_numbers)
    _ => @test.fail("Expected StringValue")
  }
  
  match arabic_with_english_attr {
    StringValue(s) => assert_eq(s, arabic_with_english)
    _ => @test.fail("Expected StringValue")
  }
  
  match hebrew_mixed_attr {
    StringValue(s) => assert_eq(s, hebrew_mixed)
    _ => @test.fail("Expected StringValue")
  }
  
  match hebrew_numbers_attr {
    StringValue(s) => assert_eq(s, hebrew_numbers)
    _ => @test.fail("Expected StringValue")
  }
  
  match hebrew_with_english_attr {
    StringValue(s) => assert_eq(s, hebrew_with_english)
    _ => @test.fail("Expected StringValue")
  }
  
  match persian_text_attr {
    StringValue(s) => assert_eq(s, persian_text)
    _ => @test.fail("Expected StringValue")
  }
  
  match persian_numbers_attr {
    StringValue(s) => assert_eq(s, persian_numbers)
    _ => @test.fail("Expected StringValue")
  }
  
  match urdu_text_attr {
    StringValue(s) => assert_eq(s, urdu_text)
    _ => @test.fail("Expected StringValue")
  }
  
  match urdu_numbers_attr {
    StringValue(s) => assert_eq(s, urdu_numbers)
    _ => @test.fail("Expected StringValue")
  }
}

test "localized_logging_messages" {
  // Test logging messages in different languages
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "i18n-logger")
  
  // Create log records in different languages
  let english_log = LogRecord::new(Info, "User login successful")
  let spanish_log = LogRecord::new(Info, "Inicio de sesión de usuario exitoso")
  let french_log = LogRecord::new(Info, "Connexion utilisateur réussie")
  let german_log = LogRecord::new(Info, "Benutzeranmeldung erfolgreich")
  let chinese_log = LogRecord::new(Info, "用户登录成功")
  let japanese_log = LogRecord::new(Info, "ユーザーログイン成功")
  let korean_log = LogRecord::new(Info, "사용자 로그인 성공")
  let arabic_log = LogRecord::new(Info, "تسجيل دخول المستخدم بنجاح")
  let russian_log = LogRecord::new(Info, "Вход пользователя выполнен успешно")
  let hindi_log = LogRecord::new(Info, "उपयोगकर्ता लॉगिन सफल")
  
  // Verify all log messages are preserved
  assert_eq(LogRecord::body(english_log), Some("User login successful"))
  assert_eq(LogRecord::body(spanish_log), Some("Inicio de sesión de usuario exitoso"))
  assert_eq(LogRecord::body(french_log), Some("Connexion utilisateur réussie"))
  assert_eq(LogRecord::body(german_log), Some("Benutzeranmeldung erfolgreich"))
  assert_eq(LogRecord::body(chinese_log), Some("用户登录成功"))
  assert_eq(LogRecord::body(japanese_log), Some("ユーザーログイン成功"))
  assert_eq(LogRecord::body(korean_log), Some("사용자 로그인 성공"))
  assert_eq(LogRecord::body(arabic_log), Some("تسجيل دخول المستخدم بنجاح"))
  assert_eq(LogRecord::body(russian_log), Some("Вход пользователя выполнен успешно"))
  assert_eq(LogRecord::body(hindi_log), Some("उपयोगकर्ता लॉगिन सफल"))
  
  // Test error messages in different languages
  let english_error = LogRecord::new(Error, "Database connection failed")
  let spanish_error = LogRecord::new(Error, "Error de conexión a la base de datos")
  let french_error = LogRecord::new(Error, "Échec de connexion à la base de données")
  let chinese_error = LogRecord::new(Error, "数据库连接失败")
  let arabic_error = LogRecord::new(Error, "فشل الاتصال بقاعدة البيانات")
  
  assert_eq(LogRecord::severity_number(english_error), Error)
  assert_eq(LogRecord::severity_number(spanish_error), Error)
  assert_eq(LogRecord::severity_number(french_error), Error)
  assert_eq(LogRecord::severity_number(chinese_error), Error)
  assert_eq(LogRecord::severity_number(arabic_error), Error)
  
  assert_eq(LogRecord::body(english_error), Some("Database connection failed"))
  assert_eq(LogRecord::body(spanish_error), Some("Error de conexión a la base de datos"))
  assert_eq(LogRecord::body(french_error), Some("Échec de connexion à la base de données"))
  assert_eq(LogRecord::body(chinese_error), Some("数据库连接失败"))
  assert_eq(LogRecord::body(arabic_error), Some("فشل الاتصال بقاعدة البيانات"))
}

test "localized_span_names" {
  // Test span names in different languages
  
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "i18n-tracer")
  
  // Create spans with localized names
  let english_span = Tracer::start_span(tracer, "HTTP Request Processing")
  let spanish_span = Tracer::start_span(tracer, "Procesamiento de Solicitud HTTP")
  let french_span = Tracer::start_span(tracer, "Traitement de la Requête HTTP")
  let german_span = Tracer::start_span(tracer, "HTTP-Anfrageverarbeitung")
  let chinese_span = Tracer::start_span(tracer, "HTTP 请求处理")
  let japanese_span = Tracer::start_span(tracer, "HTTP リクエスト処理")
  let korean_span = Tracer::start_span(tracer, "HTTP 요청 처리")
  let arabic_span = Tracer::start_span(tracer, "معالجة طلب HTTP")
  let russian_span = Tracer::start_span(tracer, "Обработка HTTP-запроса")
  
  // Verify all span names are preserved
  assert_eq(Span::name(english_span), "HTTP Request Processing")
  assert_eq(Span::name(spanish_span), "Procesamiento de Solicitud HTTP")
  assert_eq(Span::name(french_span), "Traitement de la Requête HTTP")
  assert_eq(Span::name(german_span), "HTTP-Anfrageverarbeitung")
  assert_eq(Span::name(chinese_span), "HTTP 请求处理")
  assert_eq(Span::name(japanese_span), "HTTP リクエスト処理")
  assert_eq(Span::name(korean_span), "HTTP 요청 처리")
  assert_eq(Span::name(arabic_span), "معالجة طلب HTTP")
  assert_eq(Span::name(russian_span), "Обработка HTTP-запроса")
  
  // Test span events with localized messages
  Span::add_event(english_span, "Request received", None)
  Span::add_event(spanish_span, "Solicitud recibida", None)
  Span::add_event(french_span, "Requête reçue", None)
  Span::add_event(german_span, "Anfrage empfangen", None)
  Span::add_event(chinese_span, "收到请求", None)
  Span::add_event(japanese_span, "リクエストを受信", None)
  Span::add_event(korean_span, "요청 수신", None)
  Span::add_event(arabic_span, "تم استلام الطلب", None)
  Span::add_event(russian_span, "Запрос получен", None)
  
  // End all spans
  Span::end(english_span)
  Span::end(spanish_span)
  Span::end(french_span)
  Span::end(german_span)
  Span::end(chinese_span)
  Span::end(japanese_span)
  Span::end(korean_span)
  Span::end(arabic_span)
  Span::end(russian_span)
}

test "localized_metric_names" {
  // Test metric names and descriptions in different languages
  
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "i18n-metrics")
  
  // Create metrics with localized names and descriptions
  let english_counter = Meter::create_counter(meter, "http_requests_total", Some("Total HTTP requests"), Some("count"))
  let spanish_counter = Meter::create_counter(meter, "solicitudes_http_total", Some("Total de solicitudes HTTP"), Some("conteo"))
  let french_counter = Meter::create_counter(meter, "requetes_http_total", Some("Total des requêtes HTTP"), Some("compte"))
  let german_counter = Meter::create_counter(meter, "http_anfragen_gesamt", Some("Gesamtzahl der HTTP-Anfragen"), Some("anzahl"))
  let chinese_counter = Meter::create_counter(meter, "http_请求总数", Some("HTTP 请求总数"), Some("计数"))
  let japanese_counter = Meter::create_counter(meter, "httpリクエスト合計", Some("HTTPリクエスト合計"), Some("カウント"))
  let korean_counter = Meter::create_counter(meter, "http_요청_총계", Some("HTTP 요청 총계"), Some("개수"))
  let arabic_counter = Meter::create_counter(meter, "إجمالي_طلبات_http", Some("إجمالي طلبات HTTP"), Some("عدد"))
  
  // Verify all metric names and descriptions are preserved
  assert_eq(english_counter.name, "http_requests_total")
  assert_eq(english_counter.description, Some("Total HTTP requests"))
  assert_eq(english_counter.unit, Some("count"))
  
  assert_eq(spanish_counter.name, "solicitudes_http_total")
  assert_eq(spanish_counter.description, Some("Total de solicitudes HTTP"))
  assert_eq(spanish_counter.unit, Some("conteo"))
  
  assert_eq(french_counter.name, "requetes_http_total")
  assert_eq(french_counter.description, Some("Total des requêtes HTTP"))
  assert_eq(french_counter.unit, Some("compte"))
  
  assert_eq(german_counter.name, "http_anfragen_gesamt")
  assert_eq(german_counter.description, Some("Gesamtzahl der HTTP-Anfragen"))
  assert_eq(german_counter.unit, Some("anzahl"))
  
  assert_eq(chinese_counter.name, "http_请求总数")
  assert_eq(chinese_counter.description, Some("HTTP 请求总数"))
  assert_eq(chinese_counter.unit, Some("计数"))
  
  assert_eq(japanese_counter.name, "httpリクエスト合計")
  assert_eq(japanese_counter.description, Some("HTTPリクエスト合計"))
  assert_eq(japanese_counter.unit, Some("カウント"))
  
  assert_eq(korean_counter.name, "http_요청_총계")
  assert_eq(korean_counter.description, Some("HTTP 요청 총계"))
  assert_eq(korean_counter.unit, Some("개수"))
  
  assert_eq(arabic_counter.name, "إجمالي_طلبات_http")
  assert_eq(arabic_counter.description, Some("إجمالي طلبات HTTP"))
  assert_eq(arabic_counter.unit, Some("عدد"))
  
  // Test metric recording
  Counter::add(english_counter, 100.0)
  Counter::add(spanish_counter, 200.0)
  Counter::add(french_counter, 150.0)
  Counter::add(german_counter, 120.0)
  Counter::add(chinese_counter, 80.0)
  Counter::add(japanese_counter, 90.0)
  Counter::add(korean_counter, 110.0)
  Counter::add(arabic_counter, 130.0)
}

test "localized_resource_attributes" {
  // Test resource attributes with localized values
  
  let resource = Resource::new()
  
  // Service names in different languages
  let localized_attrs = [
    ("service.name.en", StringValue("User Service")),
    ("service.name.es", StringValue("Servicio de Usuario")),
    ("service.name.fr", StringValue("Service Utilisateur")),
    ("service.name.de", StringValue("Benutzerservice")),
    ("service.name.zh", StringValue("用户服务")),
    ("service.name.ja", StringValue("ユーザーサービス")),
    ("service.name.ko", StringValue("사용자 서비스")),
    ("service.name.ar", StringValue("خدمة المستخدم")),
    ("service.name.ru", StringValue("Сервис пользователей")),
    ("service.name.hi", StringValue("उपयोगकर्ता सेवा")),
    
    // Descriptions in different languages
    ("service.description.en", StringValue("Handles user authentication and profile management")),
    ("service.description.es", StringValue("Maneja la autenticación de usuarios y la gestión de perfiles")),
    ("service.description.fr", StringValue("Gère l'authentification des utilisateurs et la gestion des profils")),
    ("service.description.de", StringValue("Verarbeitet Benutzerauthentifizierung und Profilverwaltung")),
    ("service.description.zh", StringValue("处理用户身份验证和配置文件管理")),
    ("service.description.ja", StringValue("ユーザー認証とプロファイル管理を処理します")),
    ("service.description.ko", StringValue("사용자 인증 및 프로필 관리를 처리합니다")),
    ("service.description.ar", StringValue("يتعامل مع مصادقة المستخدم وإدارة الملفات الشخصية")),
    ("service.description.ru", StringValue("Обрабатывает аутентификацию пользователей и управление профилями")),
    ("service.description.hi", StringValue("उपयोगकर्ता प्रमाणीकरण और प्रोफ़ाइल प्रबंधन को संभालता है")),
    
    // Region-specific attributes
    ("deployment.region.en", StringValue("US-East")),
    ("deployment.region.zh", StringValue("美国东部")),
    ("deployment.region.ja", StringValue("米国東部")),
    ("deployment.region.ko", StringValue("미국 동부")),
    ("deployment.region.ar", StringValue("الشرق الأمريكي")),
    
    // Environment names in different languages
    ("environment.en", StringValue("production")),
    ("environment.es", StringValue("producción")),
    ("environment.fr", StringValue("production")),
    ("environment.de", StringValue("Produktion")),
    ("environment.zh", StringValue("生产")),
    ("environment.ja", StringValue("本番")),
    ("environment.ko", StringValue("프로덕션")),
    ("environment.ar", StringValue("الإنتاج")),
    ("environment.ru", StringValue("продукт")),
    ("environment.hi", StringValue("उत्पादन"))
  ]
  
  let localized_resource = Resource::with_attributes(resource, localized_attrs)
  
  // Verify all localized attributes are preserved
  assert_eq(Resource::get_attribute(localized_resource, "service.name.en"), Some(StringValue("User Service")))
  assert_eq(Resource::get_attribute(localized_resource, "service.name.es"), Some(StringValue("Servicio de Usuario")))
  assert_eq(Resource::get_attribute(localized_resource, "service.name.fr"), Some(StringValue("Service Utilisateur")))
  assert_eq(Resource::get_attribute(localized_resource, "service.name.de"), Some(StringValue("Benutzerservice")))
  assert_eq(Resource::get_attribute(localized_resource, "service.name.zh"), Some(StringValue("用户服务")))
  assert_eq(Resource::get_attribute(localized_resource, "service.name.ja"), Some(StringValue("ユーザーサービス")))
  assert_eq(Resource::get_attribute(localized_resource, "service.name.ko"), Some(StringValue("사용자 서비스")))
  assert_eq(Resource::get_attribute(localized_resource, "service.name.ar"), Some(StringValue("خدمة المستخدم")))
  assert_eq(Resource::get_attribute(localized_resource, "service.name.ru"), Some(StringValue("Сервис пользователей")))
  assert_eq(Resource::get_attribute(localized_resource, "service.name.hi"), Some(StringValue("उपयोगकर्ता सेवा")))
  
  assert_eq(Resource::get_attribute(localized_resource, "deployment.region.zh"), Some(StringValue("美国东部")))
  assert_eq(Resource::get_attribute(localized_resource, "deployment.region.ja"), Some(StringValue("米国東部")))
  assert_eq(Resource::get_attribute(localized_resource, "deployment.region.ko"), Some(StringValue("미국 동부")))
  assert_eq(Resource::get_attribute(localized_resource, "deployment.region.ar"), Some(StringValue("الشرق الأمريكي")))
  
  assert_eq(Resource::get_attribute(localized_resource, "environment.zh"), Some(StringValue("生产")))
  assert_eq(Resource::get_attribute(localized_resource, "environment.ja"), Some(StringValue("本番")))
  assert_eq(Resource::get_attribute(localized_resource, "environment.ko"), Some(StringValue("프로덕션")))
  assert_eq(Resource::get_attribute(localized_resource, "environment.ar"), Some(StringValue("الإنتاج")))
}

test "localized_baggage_entries" {
  // Test baggage entries with localized keys and values
  
  let baggage = Baggage::new()
  
  // Add baggage entries with localized keys and values
  let baggage_with_en = Baggage::set_entry(baggage, "user.role.en", "administrator")
  let baggage_with_es = Baggage::set_entry(baggage_with_en, "user.role.es", "administrador")
  let baggage_with_fr = Baggage::set_entry(baggage_with_es, "user.role.fr", "administrateur")
  let baggage_with_de = Baggage::set_entry(baggage_with_fr, "user.role.de", "Administrator")
  let baggage_with_zh = Baggage::set_entry(baggage_with_de, "user.role.zh", "管理员")
  let baggage_with_ja = Baggage::set_entry(baggage_with_zh, "user.role.ja", "管理者"))
  let baggage_with_ko = Baggage::set_entry(baggage_with_ja, "user.role.ko", "관리자"))
  let baggage_with_ar = Baggage::set_entry(baggage_with_ko, "user.role.ar", "مدير"))
  let baggage_with_ru = Baggage::set_entry(baggage_with_ar, "user.role.ru", "администратор"))
  let baggage_with_hi = Baggage::set_entry(baggage_with_ru, "user.role.hi", "व्यवस्थापक"))
  
  // Add more localized baggage entries
  let final_baggage = Baggage::set_entry(baggage_with_hi, "request.purpose.zh", "用户验证")
  
  // Verify all localized baggage entries are preserved
  assert_eq(Baggage::get_entry(final_baggage, "user.role.en"), Some("administrator"))
  assert_eq(Baggage::get_entry(final_baggage, "user.role.es"), Some("administrador"))
  assert_eq(Baggage::get_entry(final_baggage, "user.role.fr"), Some("administrateur"))
  assert_eq(Baggage::get_entry(final_baggage, "user.role.de"), Some("Administrator"))
  assert_eq(Baggage::get_entry(final_baggage, "user.role.zh"), Some("管理员"))
  assert_eq(Baggage::get_entry(final_baggage, "user.role.ja"), Some("管理者"))
  assert_eq(Baggage::get_entry(final_baggage, "user.role.ko"), Some("관리자"))
  assert_eq(Baggage::get_entry(final_baggage, "user.role.ar"), Some("مدير"))
  assert_eq(Baggage::get_entry(final_baggage, "user.role.ru"), Some("администратор"))
  assert_eq(Baggage::get_entry(final_baggage, "user.role.hi"), Some("व्यवस्थापक"))
  assert_eq(Baggage::get_entry(final_baggage, "request.purpose.zh"), Some("用户验证"))
}

test "locale_specific_formatting" {
  // Test locale-specific formatting in telemetry data
  
  // Date and time formats in different locales
  let us_date_format = StringValue("12/28/2025 14:30:00")
  let eu_date_format = StringValue("28/12/2025 14:30:00")
  let iso_date_format = StringValue("2025-12-28T14:30:00Z")
  let chinese_date_format = StringValue("2025年12月28日 14:30:00")
  let japanese_date_format = StringValue("2025年12月28日 14時30分00秒")
  let korean_date_format = StringValue("2025년 12월 28일 14:30:00")
  
  // Number formats in different locales
  let us_number_format = StringValue("1,234,567.89")
  let eu_number_format = StringValue("1.234.567,89")
  let indian_number_format = StringValue("12,34,567.89")
  let chinese_number_format = StringValue("1,234,567.89")
  
  // Currency formats in different locales
  let us_currency = StringValue("$1,234.56")
  let eu_currency = StringValue("€1.234,56")
  let uk_currency = StringValue("£1,234.56")
  let japanese_currency = StringValue("¥1,235"))
  let chinese_currency = StringValue("¥1,234.56"))
  let korean_currency = StringValue("₩1,235"))
  
  // Test with attributes
  let date_attrs = [
    ("timestamp.us", us_date_format),
    ("timestamp.eu", eu_date_format),
    ("timestamp.iso", iso_date_format),
    ("timestamp.zh", chinese_date_format),
    ("timestamp.ja", japanese_date_format),
    ("timestamp.ko", korean_date_format)
  ]
  
  let number_attrs = [
    ("amount.us", us_number_format),
    ("amount.eu", eu_number_format),
    ("amount.in", indian_number_format),
    ("amount.zh", chinese_number_format)
  ]
  
  let currency_attrs = [
    ("price.us", us_currency),
    ("price.eu", eu_currency),
    ("price.uk", uk_currency),
    ("price.jp", japanese_currency),
    ("price.cn", chinese_currency),
    ("price.kr", korean_currency)
  ]
  
  let resource = Resource::new()
  let date_resource = Resource::with_attributes(resource, date_attrs)
  let number_resource = Resource::with_attributes(date_resource, number_attrs)
  let currency_resource = Resource::with_attributes(number_resource, currency_attrs)
  
  // Verify all locale-specific formats are preserved
  assert_eq(Resource::get_attribute(currency_resource, "timestamp.us"), Some(us_date_format))
  assert_eq(Resource::get_attribute(currency_resource, "timestamp.eu"), Some(eu_date_format))
  assert_eq(Resource::get_attribute(currency_resource, "timestamp.iso"), Some(iso_date_format))
  assert_eq(Resource::get_attribute(currency_resource, "timestamp.zh"), Some(chinese_date_format))
  assert_eq(Resource::get_attribute(currency_resource, "timestamp.ja"), Some(japanese_date_format))
  assert_eq(Resource::get_attribute(currency_resource, "timestamp.ko"), Some(korean_date_format))
  
  assert_eq(Resource::get_attribute(currency_resource, "amount.us"), Some(us_number_format))
  assert_eq(Resource::get_attribute(currency_resource, "amount.eu"), Some(eu_number_format))
  assert_eq(Resource::get_attribute(currency_resource, "amount.in"), Some(indian_number_format))
  assert_eq(Resource::get_attribute(currency_resource, "amount.zh"), Some(chinese_number_format))
  
  assert_eq(Resource::get_attribute(currency_resource, "price.us"), Some(us_currency))
  assert_eq(Resource::get_attribute(currency_resource, "price.eu"), Some(eu_currency))
  assert_eq(Resource::get_attribute(currency_resource, "price.uk"), Some(uk_currency))
  assert_eq(Resource::get_attribute(currency_resource, "price.jp"), Some(japanese_currency))
  assert_eq(Resource::get_attribute(currency_resource, "price.cn"), Some(chinese_currency))
  assert_eq(Resource::get_attribute(currency_resource, "price.kr"), Some(korean_currency))
}