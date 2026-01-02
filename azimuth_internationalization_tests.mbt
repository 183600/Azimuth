// Azimuth Telemetry System - 国际化支持测试
// 测试遥测系统对多语言和多地区环境的支持

test "多语言属性值测试" {
  // 创建包含多语言属性的资源
  let multilingual_resource = Resource::with_attributes(Resource::new(), [
    // 英文属性
    ("service.name", StringValue("user.service")),
    ("service.description", StringValue("User management service")),
    
    // 中文属性
    ("service.name.zh", StringValue("用户服务")),
    ("service.description.zh", StringValue("用户管理服务")),
    
    // 日文属性
    ("service.name.ja", StringValue("ユーザーサービス")),
    ("service.description.ja", StringValue("ユーザー管理サービス")),
    
    // 韩文属性
    ("service.name.ko", StringValue("사용자 서비스")),
    ("service.description.ko", StringValue("사용자 관리 서비스")),
    
    // 法文属性
    ("service.name.fr", StringValue("service utilisateur")),
    ("service.description.fr", StringValue("Service de gestion des utilisateurs")),
    
    // 德文属性
    ("service.name.de", StringValue("Benutzerdienst")),
    ("service.description.de", StringValue("Benutzerverwaltungsdienst")),
    
    // 西班牙文属性
    ("service.name.es", StringValue("servicio de usuario")),
    ("service.description.es", StringValue("Servicio de gestión de usuarios")),
    
    // 俄文属性
    ("service.name.ru", StringValue("служба пользователей")),
    ("service.description.ru", StringValue("Служба управления пользователями")),
    
    // 阿拉伯文属性
    ("service.name.ar", StringValue("خدمة المستخدم")),
    ("service.description.ar", StringValue("خدمة إدارة المستخدمين")),
    
    // 印地文属性
    ("service.name.hi", StringValue("उपयोगकर्ता सेवा")),
    ("service.description.hi", StringValue("उपयोगकर्ता प्रबंधन सेवा"))
  ])
  
  // 验证各种语言的属性值
  let service_name_en = Resource::get_attribute(multilingual_resource, "service.name")
  match service_name_en {
    Some(StringValue(name)) => assert_eq(name, "user.service")
    _ => assert_true(false)
  }
  
  let service_name_zh = Resource::get_attribute(multilingual_resource, "service.name.zh")
  match service_name_zh {
    Some(StringValue(name)) => assert_eq(name, "用户服务")
    _ => assert_true(false)
  }
  
  let service_name_ja = Resource::get_attribute(multilingual_resource, "service.name.ja")
  match service_name_ja {
    Some(StringValue(name)) => assert_eq(name, "ユーザーサービス")
    _ => assert_true(false)
  }
  
  let service_name_ko = Resource::get_attribute(multilingual_resource, "service.name.ko")
  match service_name_ko {
    Some(StringValue(name)) => assert_eq(name, "사용자 서비스")
    _ => assert_true(false)
  }
  
  let service_name_fr = Resource::get_attribute(multilingual_resource, "service.name.fr")
  match service_name_fr {
    Some(StringValue(name)) => assert_eq(name, "service utilisateur")
    _ => assert_true(false)
  }
  
  let service_name_de = Resource::get_attribute(multilingual_resource, "service.name.de")
  match service_name_de {
    Some(StringValue(name)) => assert_eq(name, "Benutzerdienst")
    _ => assert_true(false)
  }
  
  let service_name_es = Resource::get_attribute(multilingual_resource, "service.name.es")
  match service_name_es {
    Some(StringValue(name)) => assert_eq(name, "servicio de usuario")
    _ => assert_true(false)
  }
  
  let service_name_ru = Resource::get_attribute(multilingual_resource, "service.name.ru")
  match service_name_ru {
    Some(StringValue(name)) => assert_eq(name, "служба пользователей")
    _ => assert_true(false)
  }
  
  let service_name_ar = Resource::get_attribute(multilingual_resource, "service.name.ar")
  match service_name_ar {
    Some(StringValue(name)) => assert_eq(name, "خدمة المستخدم")
    _ => assert_true(false)
  }
  
  let service_name_hi = Resource::get_attribute(multilingual_resource, "service.name.hi")
  match service_name_hi {
    Some(StringValue(name)) => assert_eq(name, "उपयोगकर्ता सेवा")
    _ => assert_true(false)
  }
}

test "多语言日志消息测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "multilingual.logger")
  
  // 创建多语言日志消息
  let log_en = LogRecord::new(Info, "User login successful")
  let log_zh = LogRecord::new(Info, "用户登录成功")
  let log_ja = LogRecord::new(Info, "ユーザーログイン成功")
  let log_ko = LogRecord::new(Info, "사용자 로그인 성공")
  let log_fr = LogRecord::new(Info, "Connexion utilisateur réussie")
  let log_de = LogRecord::new(Info, "Benutzeranmeldung erfolgreich")
  let log_es = LogRecord::new(Info, "Inicio de sesión de usuario exitoso")
  let log_ru = LogRecord::new(Info, "Вход пользователя выполнен успешно")
  let log_ar = LogRecord::new(Info, "تم تسجيل دخول المستخدم بنجاح")
  let log_hi = LogRecord::new(Info, "उपयोगकर्ता लॉगिन सफल")
  
  // 验证多语言日志消息
  match LogRecord::body(log_en) {
    Some(body) => assert_eq(body, "User login successful")
    None => assert_true(false)
  }
  
  match LogRecord::body(log_zh) {
    Some(body) => assert_eq(body, "用户登录成功")
    None => assert_true(false)
  }
  
  match LogRecord::body(log_ja) {
    Some(body) => assert_eq(body, "ユーザーログイン成功")
    None => assert_true(false)
  }
  
  match LogRecord::body(log_ko) {
    Some(body) => assert_eq(body, "사용자 로그인 성공")
    None => assert_true(false)
  }
  
  match LogRecord::body(log_fr) {
    Some(body) => assert_eq(body, "Connexion utilisateur réussie")
    None => assert_true(false)
  }
  
  match LogRecord::body(log_de) {
    Some(body) => assert_eq(body, "Benutzeranmeldung erfolgreich")
    None => assert_true(false)
  }
  
  match LogRecord::body(log_es) {
    Some(body) => assert_eq(body, "Inicio de sesión de usuario exitoso")
    None => assert_true(false)
  }
  
  match LogRecord::body(log_ru) {
    Some(body) => assert_eq(body, "Вход пользователя выполнен успешно")
    None => assert_true(false)
  }
  
  match LogRecord::body(log_ar) {
    Some(body) => assert_eq(body, "تم تسجيل دخول المستخدم بنجاح")
    None => assert_true(false)
  }
  
  match LogRecord::body(log_hi) {
    Some(body) => assert_eq(body, "उपयोगकर्ता लॉगिन सफल")
    None => assert_true(false)
  }
  
  // 发出多语言日志
  Logger::emit(logger, log_en)
  Logger::emit(logger, log_zh)
  Logger::emit(logger, log_ja)
  Logger::emit(logger, log_ko)
  Logger::emit(logger, log_fr)
  Logger::emit(logger, log_de)
  Logger::emit(logger, log_es)
  Logger::emit(logger, log_ru)
  Logger::emit(logger, log_ar)
  Logger::emit(logger, log_hi)
  
  // 应该优雅处理，不崩溃
  assert_true(true)
}

test "多语言Span名称和事件测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "multilingual.tracer")
  
  // 创建多语言Span
  let span_en = Tracer::start_span(tracer, "user.authentication")
  let span_zh = Tracer::start_span(tracer, "用户认证")
  let span_ja = Tracer::start_span(tracer, "ユーザー認証")
  let span_ko = Tracer::start_span(tracer, "사용자 인증")
  let span_fr = Tracer::start_span(tracer, "authentification utilisateur")
  let span_de = Tracer::start_span(tracer, "Benutzerauthentifizierung")
  let span_es = Tracer::start_span(tracer, "autenticación de usuario")
  let span_ru = Tracer::start_span(tracer, "аутентификация пользователя")
  let span_ar = Tracer::start_span(tracer, "مصادقة المستخدم")
  let span_hi = Tracer::start_span(tracer, "उपयोगकर्ता प्रमाणीकरण")
  
  // 验证多语言Span名称
  assert_eq(Span::name(span_en), "user.authentication")
  assert_eq(Span::name(span_zh), "用户认证")
  assert_eq(Span::name(span_ja), "ユーザー認証")
  assert_eq(Span::name(span_ko), "사용자 인증")
  assert_eq(Span::name(span_fr), "authentification utilisateur")
  assert_eq(Span::name(span_de), "Benutzerauthentifizierung")
  assert_eq(Span::name(span_es), "autenticación de usuario")
  assert_eq(Span::name(span_ru), "аутентификация пользователя")
  assert_eq(Span::name(span_ar), "مصادقة المستخدم")
  assert_eq(Span::name(span_hi), "उपयोगकर्ता प्रमाणीकरण")
  
  // 添加多语言事件
  Span::add_event(span_en, "authentication.started", Some([("step", StringValue("begin"))]))
  Span::add_event(span_zh, "认证开始", Some([("步骤", StringValue("开始"))]))
  Span::add_event(span_ja, "認証開始", Some([("ステップ", StringValue("開始"))]))
  Span::add_event(span_ko, "인증 시작", Some([("단계", StringValue("시작"))]))
  Span::add_event(span_fr, "authentification démarrée", Some([("étape", StringValue("début"))]))
  Span::add_event(span_de, "Authentifizierung gestartet", Some([("Schritt", StringValue("Beginn"))]))
  Span::add_event(span_es, "autenticación iniciada", Some([("paso", StringValue("inicio"))]))
  Span::add_event(span_ru, "аутентификация начата", Some([("шаг", StringValue("начало"))]))
  Span::add_event(span_ar, "بدأت المصادقة", Some([("خطوة", StringValue("بداية"))]))
  Span::add_event(span_hi, "प्रमाणीकरण शुरू", Some([("चरण", StringValue("शुरू"))]))
  
  // 设置多语言状态
  Span::set_status(span_en, Ok, Some("Authentication successful"))
  Span::set_status(span_zh, Ok, Some("认证成功"))
  Span::set_status(span_ja, Ok, Some("認証成功"))
  Span::set_status(span_ko, Ok, Some("인증 성공"))
  Span::set_status(span_fr, Ok, Some("Authentification réussie"))
  Span::set_status(span_de, Ok, Some("Authentifizierung erfolgreich"))
  Span::set_status(span_es, Ok, Some("Autenticación exitosa"))
  Span::set_status(span_ru, Ok, Some("Аутентификация прошла успешно"))
  Span::set_status(span_ar, Ok, Some("تمت المصادقة بنجاح"))
  Span::set_status(span_hi, Ok, Some("प्रमाणीकरण सफल"))
  
  // 结束所有Span
  Span::end(span_en)
  Span::end(span_zh)
  Span::end(span_ja)
  Span::end(span_ko)
  Span::end(span_fr)
  Span::end(span_de)
  Span::end(span_es)
  Span::end(span_ru)
  Span::end(span_ar)
  Span::end(span_hi)
  
  // 应该优雅处理，不崩溃
  assert_true(true)
}

test "多语言Baggage测试" {
  // 创建多语言Baggage
  let baggage = Baggage::new()
  let baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage = Baggage::set_entry(baggage, "user.role", "admin")
  
  // 添加多语言用户信息
  let baggage = Baggage::set_entry(baggage, "user.name.en", "John Doe")
  let baggage = Baggage::set_entry(baggage, "user.name.zh", "张三")
  let baggage = Baggage::set_entry(baggage, "user.name.ja", "田中太郎")
  let baggage = Baggage::set_entry(baggage, "user.name.ko", "김철수")
  let baggage = Baggage::set_entry(baggage, "user.name.fr", "Jean Dupont")
  let baggage = Baggage::set_entry(baggage, "user.name.de", "Max Mustermann")
  let baggage = Baggage::set_entry(baggage, "user.name.es", "Juan Pérez")
  let baggage = Baggage::set_entry(baggage, "user.name.ru", "Иван Иванов")
  let baggage = Baggage::set_entry(baggage, "user.name.ar", "محمد أحمد")
  let baggage = Baggage::set_entry(baggage, "user.name.hi", "राम कुमार")
  
  // 添加多语言地区信息
  let baggage = Baggage::set_entry(baggage, "user.region.en", "United States")
  let baggage = Baggage::set_entry(baggage, "user.region.zh", "美国")
  let baggage = Baggage::set_entry(baggage, "user.region.ja", "アメリカ合衆国")
  let baggage = Baggage::set_entry(baggage, "user.region.ko", "미국")
  let baggage = Baggage::set_entry(baggage, "user.region.fr", "États-Unis")
  let baggage = Baggage::set_entry(baggage, "user.region.de", "Vereinigte Staaten")
  let baggage = Baggage::set_entry(baggage, "user.region.es", "Estados Unidos")
  let baggage = Baggage::set_entry(baggage, "user.region.ru", "Соединенные Штаты")
  let baggage = Baggage::set_entry(baggage, "user.region.ar", "الولايات المتحدة")
  let baggage = Baggage::set_entry(baggage, "user.region.hi", "संयुक्त राज्य")
  
  // 验证多语言Baggage条目
  assert_eq(Baggage::get_entry(baggage, "user.id"), Some("12345"))
  assert_eq(Baggage::get_entry(baggage, "user.role"), Some("admin"))
  
  assert_eq(Baggage::get_entry(baggage, "user.name.en"), Some("John Doe"))
  assert_eq(Baggage::get_entry(baggage, "user.name.zh"), Some("张三"))
  assert_eq(Baggage::get_entry(baggage, "user.name.ja"), Some("田中太郎"))
  assert_eq(Baggage::get_entry(baggage, "user.name.ko"), Some("김철수"))
  assert_eq(Baggage::get_entry(baggage, "user.name.fr"), Some("Jean Dupont"))
  assert_eq(Baggage::get_entry(baggage, "user.name.de"), Some("Max Mustermann"))
  assert_eq(Baggage::get_entry(baggage, "user.name.es"), Some("Juan Pérez"))
  assert_eq(Baggage::get_entry(baggage, "user.name.ru"), Some("Иван Иванов"))
  assert_eq(Baggage::get_entry(baggage, "user.name.ar"), Some("محمد أحمد"))
  assert_eq(Baggage::get_entry(baggage, "user.name.hi"), Some("राम कुमार"))
  
  assert_eq(Baggage::get_entry(baggage, "user.region.en"), Some("United States"))
  assert_eq(Baggage::get_entry(baggage, "user.region.zh"), Some("美国"))
  assert_eq(Baggage::get_entry(baggage, "user.region.ja"), Some("アメリカ合衆国"))
  assert_eq(Baggage::get_entry(baggage, "user.region.ko"), Some("미국"))
  assert_eq(Baggage::get_entry(baggage, "user.region.fr"), Some("États-Unis"))
  assert_eq(Baggage::get_entry(baggage, "user.region.de"), Some("Vereinigte Staaten"))
  assert_eq(Baggage::get_entry(baggage, "user.region.es"), Some("Estados Unidos"))
  assert_eq(Baggage::get_entry(baggage, "user.region.ru"), Some("Соединенные Штаты"))
  assert_eq(Baggage::get_entry(baggage, "user.region.ar"), Some("الولايات المتحدة"))
  assert_eq(Baggage::get_entry(baggage, "user.region.hi"), Some("संयुक्त राज्य"))
}

test "多语言错误消息测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error.test.tracer")
  
  // 创建多语言错误Span
  let error_span_en = Tracer::start_span(tracer, "user.operation")
  let error_span_zh = Tracer::start_span(tracer, "用户操作")
  let error_span_ja = Tracer::start_span(tracer, "ユーザー操作")
  let error_span_ko = Tracer::start_span(tracer, "사용자 작업")
  let error_span_fr = Tracer::start_span(tracer, "opération utilisateur")
  let error_span_de = Tracer::start_span(tracer, "Benutzeroperation")
  let error_span_es = Tracer::start_span(tracer, "operación de usuario")
  let error_span_ru = Tracer::start_span(tracer, "операция пользователя")
  let error_span_ar = Tracer::start_span(tracer, "عملية المستخدم")
  let error_span_hi = Tracer::start_span(tracer, "उपयोगकर्ता ऑपरेशन")
  
  // 添加多语言错误事件
  Span::add_event(error_span_en, "error.occurred", Some([
    ("error.type", StringValue("validation.error")),
    ("error.message", StringValue("Invalid user input")),
    ("error.code", StringValue("VAL001"))
  ]))
  
  Span::add_event(error_span_zh, "错误发生", Some([
    ("error.type", StringValue("validation.error")),
    ("error.message", StringValue("无效的用户输入")),
    ("error.code", StringValue("VAL001"))
  ]))
  
  Span::add_event(error_span_ja, "エラー発生", Some([
    ("error.type", StringValue("validation.error")),
    ("error.message", StringValue("無効なユーザー入力")),
    ("error.code", StringValue("VAL001"))
  ]))
  
  Span::add_event(error_span_ko, "오류 발생", Some([
    ("error.type", StringValue("validation.error")),
    ("error.message", StringValue("잘못된 사용자 입력")),
    ("error.code", StringValue("VAL001"))
  ]))
  
  Span::add_event(error_span_fr, "erreur.survenue", Some([
    ("error.type", StringValue("validation.error")),
    ("error.message", StringValue("Entrée utilisateur invalide")),
    ("error.code", StringValue("VAL001"))
  ]))
  
  Span::add_event(error_span_de, "Fehler aufgetreten", Some([
    ("error.type", StringValue("validation.error")),
    ("error.message", StringValue("Ungültige Benutzereingabe")),
    ("error.code", StringValue("VAL001"))
  ]))
  
  Span::add_event(error_span_es, "error.ocurrido", Some([
    ("error.type", StringValue("validation.error")),
    ("error.message", StringValue("Entrada de usuario no válida")),
    ("error.code", StringValue("VAL001"))
  ]))
  
  Span::add_event(error_span_ru, "произошла ошибка", Some([
    ("error.type", StringValue("validation.error")),
    ("error.message", StringValue("Недопустимый ввод пользователя")),
    ("error.code", StringValue("VAL001"))
  ]))
  
  Span::add_event(error_span_ar, "حدث خطأ", Some([
    ("error.type", StringValue("validation.error")),
    ("error.message", StringValue("إدخال مستخدم غير صالح")),
    ("error.code", StringValue("VAL001"))
  ]))
  
  Span::add_event(error_span_hi, "त्रुटि हुई", Some([
    ("error.type", StringValue("validation.error")),
    ("error.message", StringValue("अमान्य उपयोगकर्ता इनपुट")),
    ("error.code", StringValue("VAL001"))
  ]))
  
  // 设置多语言错误状态
  Span::set_status(error_span_en, Error, Some("Validation failed"))
  Span::set_status(error_span_zh, Error, Some("验证失败"))
  Span::set_status(error_span_ja, Error, Some("検証失敗"))
  Span::set_status(error_span_ko, Error, Some("검증 실패"))
  Span::set_status(error_span_fr, Error, Some("Échec de la validation"))
  Span::set_status(error_span_de, Error, Some("Validierung fehlgeschlagen"))
  Span::set_status(error_span_es, Error, Some("Validación fallida"))
  Span::set_status(error_span_ru, Error, Some("Ошибка валидации"))
  Span::set_status(error_span_ar, Error, Some("فشل التحقق"))
  Span::set_status(error_span_hi, Error, Some("सत्यापन विफल"))
  
  // 结束所有Span
  Span::end(error_span_en)
  Span::end(error_span_zh)
  Span::end(error_span_ja)
  Span::end(error_span_ko)
  Span::end(error_span_fr)
  Span::end(error_span_de)
  Span::end(error_span_es)
  Span::end(error_span_ru)
  Span::end(error_span_ar)
  Span::end(error_span_hi)
  
  // 应该优雅处理，不崩溃
  assert_true(true)
}

test "地区特定格式测试" {
  // 测试不同地区的日期和时间格式
  let date_formats = [
    ("en-US", "01/02/2025 12:34:56 PM"),     // 美国英语
    ("en-GB", "02/01/2025 12:34:56"),        // 英国英语
    ("zh-CN", "2025/01/02 12:34:56"),        // 中国大陆
    ("ja-JP", "2025/01/02 12:34:56"),        // 日本
    ("ko-KR", "2025. 01. 02. 오후 12:34:56"), // 韩国
    ("fr-FR", "02/01/2025 12:34:56"),        // 法国
    ("de-DE", "02.01.2025, 12:34:56"),       // 德国
    ("es-ES", "02/01/2025 12:34:56"),        // 西班牙
    ("ru-RU", "02.01.2025 12:34:56"),        // 俄罗斯
    ("ar-SA", "02/01/2025 12:34:56 م"),      // 沙特阿拉伯
    ("hi-IN", "02/01/2025 12:34:56 PM")      // 印度
  ]
  
  // 测试不同地区的数字格式
  let number_formats = [
    ("en-US", "1,234.56"),    // 美国英语
    ("en-GB", "1,234.56"),    // 英国英语
    ("zh-CN", "1,234.56"),    // 中国大陆
    ("ja-JP", "1,234.56"),    // 日本
    ("ko-KR", "1,234.56"),    // 韩国
    ("fr-FR", "1 234,56"),    // 法国
    ("de-DE", "1.234,56"),    // 德国
    ("es-ES", "1.234,56"),    // 西班牙
    ("ru-RU", "1 234,56"),    // 俄罗斯
    ("ar-SA", "1,234.56"),    // 沙特阿拉伯
    ("hi-IN", "1,234.56")     // 印度
  ]
  
  // 测试不同地区的货币格式
  let currency_formats = [
    ("en-US", "$1,234.56"),           // 美国英语
    ("en-GB", "£1,234.56"),           // 英国英语
    ("zh-CN", "¥1,234.56"),           // 中国大陆
    ("ja-JP", "¥1,234"),              // 日本
    ("ko-KR", "₩1,235"),              // 韩国
    ("fr-FR", "1 234,56 €"),          // 法国
    ("de-DE", "1.234,56 €"),          // 德国
    ("es-ES", "1.234,56 €"),          // 西班牙
    ("ru-RU", "1 234,56 ₽"),          // 俄罗斯
    ("ar-SA", "١٬٢٣٤٫٥٦ ر.س"),       // 沙特阿拉伯
    ("hi-IN", "₹1,234.56")            // 印度
  ]
  
  // 创建包含地区特定格式的资源
  let mut locale_attrs = []
  
  // 添加日期格式属性
  for (locale, format) in date_formats {
    let key = "date.format." + locale
    locale_attrs.push((key, StringValue(format)))
  }
  
  // 添加数字格式属性
  for (locale, format) in number_formats {
    let key = "number.format." + locale
    locale_attrs.push((key, StringValue(format)))
  }
  
  // 添加货币格式属性
  for (locale, format) in currency_formats {
    let key = "currency.format." + locale
    locale_attrs.push((key, StringValue(format)))
  }
  
  let locale_resource = Resource::with_attributes(Resource::new(), locale_attrs)
  
  // 验证地区特定格式
  assert_eq(Resource::get_attribute(locale_resource, "date.format.en-US"), Some(StringValue("01/02/2025 12:34:56 PM")))
  assert_eq(Resource::get_attribute(locale_resource, "date.format.zh-CN"), Some(StringValue("2025/01/02 12:34:56")))
  assert_eq(Resource::get_attribute(locale_resource, "date.format.ja-JP"), Some(StringValue("2025/01/02 12:34:56")))
  assert_eq(Resource::get_attribute(locale_resource, "date.format.ko-KR"), Some(StringValue("2025. 01. 02. 오후 12:34:56")))
  assert_eq(Resource::get_attribute(locale_resource, "date.format.fr-FR"), Some(StringValue("02/01/2025 12:34:56")))
  assert_eq(Resource::get_attribute(locale_resource, "date.format.de-DE"), Some(StringValue("02.01.2025, 12:34:56")))
  assert_eq(Resource::get_attribute(locale_resource, "date.format.es-ES"), Some(StringValue("02/01/2025 12:34:56")))
  assert_eq(Resource::get_attribute(locale_resource, "date.format.ru-RU"), Some(StringValue("02.01.2025 12:34:56")))
  assert_eq(Resource::get_attribute(locale_resource, "date.format.ar-SA"), Some(StringValue("02/01/2025 12:34:56 م")))
  assert_eq(Resource::get_attribute(locale_resource, "date.format.hi-IN"), Some(StringValue("02/01/2025 12:34:56 PM")))
  
  assert_eq(Resource::get_attribute(locale_resource, "number.format.fr-FR"), Some(StringValue("1 234,56")))
  assert_eq(Resource::get_attribute(locale_resource, "number.format.de-DE"), Some(StringValue("1.234,56")))
  assert_eq(Resource::get_attribute(locale_resource, "number.format.es-ES"), Some(StringValue("1.234,56")))
  assert_eq(Resource::get_attribute(locale_resource, "number.format.ru-RU"), Some(StringValue("1 234,56")))
  
  assert_eq(Resource::get_attribute(locale_resource, "currency.format.en-US"), Some(StringValue("$1,234.56")))
  assert_eq(Resource::get_attribute(locale_resource, "currency.format.en-GB"), Some(StringValue("£1,234.56")))
  assert_eq(Resource::get_attribute(locale_resource, "currency.format.zh-CN"), Some(StringValue("¥1,234.56")))
  assert_eq(Resource::get_attribute(locale_resource, "currency.format.ja-JP"), Some(StringValue("¥1,234")))
  assert_eq(Resource::get_attribute(locale_resource, "currency.format.ko-KR"), Some(StringValue("₩1,235")))
  assert_eq(Resource::get_attribute(locale_resource, "currency.format.fr-FR"), Some(StringValue("1 234,56 €")))
  assert_eq(Resource::get_attribute(locale_resource, "currency.format.de-DE"), Some(StringValue("1.234,56 €")))
  assert_eq(Resource::get_attribute(locale_resource, "currency.format.es-ES"), Some(StringValue("1.234,56 €")))
  assert_eq(Resource::get_attribute(locale_resource, "currency.format.ru-RU"), Some(StringValue("1 234,56 ₽")))
  assert_eq(Resource::get_attribute(locale_resource, "currency.format.ar-SA"), Some(StringValue("١٬٢٣٤٫٥٦ ر.س")))
  assert_eq(Resource::get_attribute(locale_resource, "currency.format.hi-IN"), Some(StringValue("₹1,234.56")))
}