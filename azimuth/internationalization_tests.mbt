// 国际化和本地化测试用例
// 测试系统在不同语言和地区设置下的行为

pub test "unicode string handling in attributes" {
  let attrs = azimuth::Attributes::new()
  
  // 测试中文字符
  azimuth::Attributes::set(attrs, "用户.名称", azimuth::StringValue("张三"))
  let chinese_name = azimuth::Attributes::get(attrs, "用户.名称")
  assert_eq(chinese_name, Some(azimuth::StringValue("张三")))
  
  // 测试日文字符
  azimuth::Attributes::set(attrs, "ユーザー名", azimuth::StringValue("田中太郎"))
  let japanese_name = azimuth::Attributes::get(attrs, "ユーザー名")
  assert_eq(japanese_name, Some(azimuth::StringValue("田中太郎")))
  
  // 测试韩文字符
  azimuth::Attributes::set(attrs, "사용자명", azimuth::StringValue("김철수"))
  let korean_name = azimuth::Attributes::get(attrs, "사용자명")
  assert_eq(korean_name, Some(azimuth::StringValue("김철수")))
  
  // 测试阿拉伯文字符
  azimuth::Attributes::set(attrs, "اسم.المستخدم", azimuth::StringValue("أحمد محمد"))
  let arabic_name = azimuth::Attributes::get(attrs, "اسم.المستخدم")
  assert_eq(arabic_name, Some(azimuth::StringValue("أحمد محمد")))
  
  // 测试希伯来文字符
  azimuth::Attributes::set(attrs, "שם.משתמש", azimuth::StringValue("דוד כהן"))
  let hebrew_name = azimuth::Attributes::get(attrs, "שם.משתמש")
  assert_eq(hebrew_name, Some(azimuth::StringValue("דוד כהן")))
  
  // 测试俄文字符
  azimuth::Attributes::set(attrs, "имя.пользователя", azimuth::StringValue("Иван Петров"))
  let russian_name = azimuth::Attributes::get(attrs, "имя.пользователя")
  assert_eq(russian_name, Some(azimuth::StringValue("Иван Петров")))
  
  // 测试混合语言
  azimuth::Attributes::set(attrs, "mixed.language", azimuth::StringValue("Hello 世界 こんにちは 안녕하세요 مرحبا שלום"))
  let mixed_value = azimuth::Attributes::get(attrs, "mixed.language")
  assert_eq(mixed_value, Some(azimuth::StringValue("Hello 世界 こんにちは 안녕하세요 مرحبا שלום")))
}

pub test "unicode string handling in context and baggage" {
  let ctx = azimuth::Context::root()
  
  // 测试多语言上下文键值
  let chinese_key = azimuth::ContextKey::new("中文键")
  let ctx_with_chinese = azimuth::Context::with_value(ctx, chinese_key, "中文值")
  let chinese_value = azimuth::Context::get(ctx_with_chinese, chinese_key)
  assert_eq(chinese_value, Some("中文值"))
  
  let japanese_key = azimuth::ContextKey::new("日本語キー")
  let ctx_with_japanese = azimuth::Context::with_value(ctx, japanese_key, "日本語値")
  let japanese_value = azimuth::Context::get(ctx_with_japanese, japanese_key)
  assert_eq(japanese_value, Some("日本語値"))
  
  // 测试多语言baggage
  let baggage = azimuth::Baggage::new()
  let multilingual_baggage = azimuth::Baggage::set_entry(baggage, "corrélation.id", "fr-FR-12345")
  let french_value = azimuth::Baggage::get_entry(multilingual_baggage, "corrélation.id")
  assert_eq(french_value, Some("fr-FR-12345"))
  
  let german_baggage = azimuth::Baggage::set_entry(baggage, "korrelations.id", "de-DE-67890")
  let german_value = azimuth::Baggage::get_entry(german_baggage, "korrelations.id")
  assert_eq(german_value, Some("de-DE-67890"))
  
  // 测试西班牙语
  let spanish_baggage = azimuth::Baggage::set_entry(baggage, "id.de.correlación", "es-ES-11111")
  let spanish_value = azimuth::Baggage::get_entry(spanish_baggage, "id.de.correlación")
  assert_eq(spanish_value, Some("es-ES-11111"))
}

pub test "unicode string handling in log messages" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "国际化测试记录器")
  
  // 测试中文日志消息
  let chinese_log = azimuth::LogRecord::new(azimuth::Info, "用户登录成功：用户名为张三")
  assert_eq(azimuth::LogRecord::body(chinese_log), Some("用户登录成功：用户名为张三"))
  azimuth::Logger::emit(logger, chinese_log)
  
  // 测试日文日志消息
  let japanese_log = azimuth::LogRecord::new(azimuth::Warn, "ユーザーログイン成功：田中太郎")
  assert_eq(azimuth::LogRecord::body(japanese_log), Some("ユーザーログイン成功：田中太郎"))
  azimuth::Logger::emit(logger, japanese_log)
  
  // 测试韩文日志消息
  let korean_log = azimuth::LogRecord::new(azimuth::Error, "사용자 로그인 성공: 김철수")
  assert_eq(azimuth::LogRecord::body(korean_log), Some("사용자 로그인 성공: 김철수"))
  azimuth::Logger::emit(logger, korean_log)
  
  // 测试阿拉伯文日志消息
  let arabic_log = azimuth::LogRecord::new(azimuth::Info, "نجح تسجيل دخول المستخدم: أحمد محمد")
  assert_eq(azimuth::LogRecord::body(arabic_log), Some("نجح تسجيل دخول المستخدم: أحمد محمد"))
  azimuth::Logger::emit(logger, arabic_log)
  
  // 测试混合语言日志消息
  let mixed_log = azimuth::LogRecord::new(azimuth::Debug, "Mixed language: English 中文 日本語 한국어 العربية עברית Русский")
  assert_eq(azimuth::LogRecord::body(mixed_log), Some("Mixed language: English 中文 日本語 한국어 العربية עברית Русский"))
  azimuth::Logger::emit(logger, mixed_log)
}

pub test "unicode string handling in span names and operations" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "国际化追踪器")
  
  // 测试中文span名称
  let chinese_span = azimuth::Tracer::start_span(tracer, "用户认证流程")
  assert_eq(azimuth::Span::name(chinese_span), "用户认证流程")
  azimuth::Span::add_event(chinese_span, "开始验证用户凭据")
  azimuth::Span::end(chinese_span)
  
  // 测试日文span名称
  let japanese_span = azimuth::Tracer::start_span(tracer, "ユーザー認証フロー")
  assert_eq(azimuth::Span::name(japanese_span), "ユーザー認証フロー")
  azimuth::Span::add_event(japanese_span, "ユーザー認証情報の検証を開始")
  azimuth::Span::end(japanese_span)
  
  // 测试韩文span名称
  let korean_span = azimuth::Tracer::start_span(tracer, "사용자 인증 흐름")
  assert_eq(azimuth::Span::name(korean_span), "사용자 인증 흐름")
  azimuth::Span::add_event(korean_span, "사용자 자격 증명 검증 시작")
  azimuth::Span::end(korean_span)
  
  // 测试带重音符号的span名称（法语、德语等）
  let french_span = azimuth::Tracer::start_span(tracer, "Processus d'authentification")
  assert_eq(azimuth::Span::name(french_span), "Processus d'authentification")
  azimuth::Span::add_event(french_span, "Début de la vérification des identifiants")
  azimuth::Span::end(french_span)
  
  let german_span = azimuth::Tracer::start_span(tracer, "Authentifizierungsprozess")
  assert_eq(azimuth::Span::name(german_span), "Authentifizierungsprozess")
  azimuth::Span::add_event(german_span, "Beginn der Überprüfung der Anmeldeinformationen")
  azimuth::Span::end(german_span)
}

pub test "unicode string handling in metrics" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "国际化指标器")
  
  // 测试中文指标名称
  let chinese_counter = azimuth::Meter::create_counter(meter, "请求计数器")
  assert_eq(chinese_counter.name, "请求计数器")
  azimuth::Counter::add(chinese_counter, 1.0)
  
  let chinese_histogram = azimuth::Meter::create_histogram(
    meter, 
    "响应时间", 
    Some("响应时间分布统计"), 
    Some("毫秒")
  )
  assert_eq(chinese_histogram.name, "响应时间")
  assert_eq(chinese_histogram.description, Some("响应时间分布统计"))
  assert_eq(chinese_histogram.unit, Some("毫秒"))
  azimuth::Histogram::record(chinese_histogram, 150.5)
  
  // 测试日文指标名称
  let japanese_counter = azimuth::Meter::create_counter(meter, "リクエストカウンター")
  assert_eq(japanese_counter.name, "リクエストカウンター")
  azimuth::Counter::add(japanese_counter, 1.0)
  
  let japanese_histogram = azimuth::Meter::create_histogram(
    meter, 
    "レスポンスタイム", 
    Some("レスポンスタイム分布統計"), 
    Some("ミリ秒")
  )
  assert_eq(japanese_histogram.name, "レスポンスタイム")
  assert_eq(japanese_histogram.description, Some("レスポンスタイム分布統計"))
  assert_eq(japanese_histogram.unit, Some("ミリ秒"))
  azimuth::Histogram::record(japanese_histogram, 200.0)
  
  // 测试韩文指标名称
  let korean_counter = azimuth::Meter::create_counter(meter, "요청 카운터")
  assert_eq(korean_counter.name, "요청 카운터")
  azimuth::Counter::add(korean_counter, 1.0)
  
  let korean_histogram = azimuth::Meter::create_histogram(
    meter, 
    "응답 시간", 
    Some("응답 시간 분포 통계"), 
    Some("밀리초")
  )
  assert_eq(korean_histogram.name, "응답 시간")
  assert_eq(korean_histogram.description, Some("응답 시간 분포 통계"))
  assert_eq(korean_histogram.unit, Some("밀리초"))
  azimuth::Histogram::record(korean_histogram, 180.0)
}

pub test "resource attributes with international content" {
  let resource = azimuth::Resource::new()
  
  // 测试多语言服务属性
  let chinese_attrs = [
    ("service.name", azimuth::StringValue("用户服务")),
    ("service.description", azimuth::StringValue("处理用户认证和授权的服务")),
    ("service.owner", azimuth::StringValue("产品团队")),
    ("environment", azimuth::StringValue("生产环境"))
  ]
  let chinese_resource = azimuth::Resource::with_attributes(resource, chinese_attrs)
  
  assert_eq(azimuth::Resource::get_attribute(chinese_resource, "service.name"), 
            Some(azimuth::StringValue("用户服务")))
  assert_eq(azimuth::Resource::get_attribute(chinese_resource, "service.description"), 
            Some(azimuth::StringValue("处理用户认证和授权的服务")))
  
  // 测试日文服务属性
  let japanese_attrs = [
    ("service.name", azimuth::StringValue("ユーザーサービス")),
    ("service.description", azimuth::StringValue("ユーザー認証と認可を処理するサービス")),
    ("service.owner", azimuth::StringValue("プロダクトチーム")),
    ("environment", azimuth::StringValue("本番環境"))
  ]
  let japanese_resource = azimuth::Resource::with_attributes(resource, japanese_attrs)
  
  assert_eq(azimuth::Resource::get_attribute(japanese_resource, "service.name"), 
            Some(azimuth::StringValue("ユーザーサービス")))
  assert_eq(azimuth::Resource::get_attribute(japanese_resource, "service.description"), 
            Some(azimuth::StringValue("ユーザー認証と認可を処理するサービス")))
  
  // 测试混合语言资源属性
  let mixed_attrs = [
    ("service.name", azimuth::StringValue("User Service 用户服务 ユーザーサービス")),
    ("service.tags", azimuth::StringValue("auth,authentication,认证,認証")),
    ("deployment.region", azimuth::StringValue("asia-northeast-1 東京")),
    ("support.languages", azimuth::StringValue("zh-CN,ja-JP,ko-KR,en-US"))
  ]
  let mixed_resource = azimuth::Resource::with_attributes(resource, mixed_attrs)
  
  assert_eq(azimuth::Resource::get_attribute(mixed_resource, "service.name"), 
            Some(azimuth::StringValue("User Service 用户服务 ユーザーサービス")))
  assert_eq(azimuth::Resource::get_attribute(mixed_resource, "service.tags"), 
            Some(azimuth::StringValue("auth,authentication,认证,認証")))
}

pub test "propagation with international headers" {
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  let ctx = azimuth::Context::root()
  
  // 设置多语言上下文
  let chinese_key = azimuth::ContextKey::new("用户标识")
  let ctx_with_chinese = azimuth::Context::with_value(ctx, chinese_key, "用户123")
  
  let japanese_key = azimuth::ContextKey::new("リクエストID")
  let ctx_with_japanese = azimuth::Context::with_value(ctx_with_chinese, japanese_key, "リクエスト456")
  
  let korean_key = azimuth::ContextKey::new("세션ID")
  let final_ctx = azimuth::Context::with_value(ctx_with_japanese, korean_key, "세션789")
  
  // 测试注入
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, final_ctx, carrier)
  
  // 验证traceparent头部存在
  let trace_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  assert_true(trace_header != None, "traceparent头部应该存在")
  
  // 测试提取
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  
  // 验证提取的上下文包含预期值
  let extracted_key = azimuth::ContextKey::new("extracted")
  let extracted_value = azimuth::Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
}

pub test "error messages with international content" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "多语言错误处理器")
  
  // 测试中文错误消息
  let chinese_error = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("数据库连接失败：无法连接到数据库服务器"),
    None,
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("error-trace-123"),
    Some("error-span-456"),
    Some(azimuth::Context::root())
  )
  assert_eq(azimuth::LogRecord::body(chinese_error), Some("数据库连接失败：无法连接到数据库服务器"))
  assert_eq(azimuth::LogRecord::severity_number(chinese_error), azimuth::Error)
  azimuth::Logger::emit(logger, chinese_error)
  
  // 测试日文错误消息
  let japanese_error = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("データベース接続に失敗しました：データベースサーバーに接続できません"),
    None,
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("error-trace-789"),
    Some("error-span-012"),
    Some(azimuth::Context::root())
  )
  assert_eq(azimuth::LogRecord::body(japanese_error), Some("データベース接続に失敗しました：データベースサーバーに接続できません"))
  assert_eq(azimuth::LogRecord::severity_number(japanese_error), azimuth::Error)
  azimuth::Logger::emit(logger, japanese_error)
  
  // 测试韩文错误消息
  let korean_error = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("데이터베이스 연결 실패: 데이터베이스 서버에 연결할 수 없습니다"),
    None,
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("error-trace-345"),
    Some("error-span-678"),
    Some(azimuth::Context::root())
  )
  assert_eq(azimuth::LogRecord::body(korean_error), Some("데이터베이스 연결 실패: 데이터베이스 서버에 연결할 수 없습니다"))
  assert_eq(azimuth::LogRecord::severity_number(korean_error), azimuth::Error)
  azimuth::Logger::emit(logger, korean_error)
}

pub test "locale-specific formatting and data" {
  let attrs = azimuth::Attributes::new()
  
  // 测试不同地区的日期时间格式
  azimuth::Attributes::set(attrs, "timestamp.cn", azimuth::StringValue("2025年12月28日 14:30:00"))
  azimuth::Attributes::set(attrs, "timestamp.jp", azimuth::StringValue("2025年12月28日 14時30分00秒"))
  azimuth::Attributes::set(attrs, "timestamp.kr", azimuth::StringValue("2025년 12월 28일 14:30:00"))
  azimuth::Attributes::set(attrs, "timestamp.us", azimuth::StringValue("12/28/2025 2:30:00 PM"))
  azimuth::Attributes::set(attrs, "timestamp.eu", azimuth::StringValue("28/12/2025 14:30:00"))
  
  assert_eq(azimuth::Attributes::get(attrs, "timestamp.cn"), 
            Some(azimuth::StringValue("2025年12月28日 14:30:00")))
  assert_eq(azimuth::Attributes::get(attrs, "timestamp.jp"), 
            Some(azimuth::StringValue("2025年12月28日 14時30分00秒")))
  assert_eq(azimuth::Attributes::get(attrs, "timestamp.kr"), 
            Some(azimuth::StringValue("2025년 12월 28일 14:30:00")))
  assert_eq(azimuth::Attributes::get(attrs, "timestamp.us"), 
            Some(azimuth::StringValue("12/28/2025 2:30:00 PM")))
  assert_eq(azimuth::Attributes::get(attrs, "timestamp.eu"), 
            Some(azimuth::StringValue("28/12/2025 14:30:00")))
  
  // 测试不同地区的数字格式
  azimuth::Attributes::set(attrs, "currency.cn", azimuth::StringValue("¥1,234.56"))
  azimuth::Attributes::set(attrs, "currency.jp", azimuth::StringValue("¥1,234"))
  azimuth::Attributes::set(attrs, "currency.kr", azimuth::StringValue("₩1,235"))
  azimuth::Attributes::set(attrs, "currency.us", azimuth::StringValue("$1,234.56"))
  azimuth::Attributes::set(attrs, "currency.eu", azimuth::StringValue("€1.234,56"))
  
  assert_eq(azimuth::Attributes::get(attrs, "currency.cn"), 
            Some(azimuth::StringValue("¥1,234.56")))
  assert_eq(azimuth::Attributes::get(attrs, "currency.jp"), 
            Some(azimuth::StringValue("¥1,234")))
  assert_eq(azimuth::Attributes::get(attrs, "currency.kr"), 
            Some(azimuth::StringValue("₩1,235")))
  assert_eq(azimuth::Attributes::get(attrs, "currency.us"), 
            Some(azimuth::StringValue("$1,234.56")))
  assert_eq(azimuth::Attributes::get(attrs, "currency.eu"), 
            Some(azimuth::StringValue("€1.234,56")))
}

pub test "right-to-left language support" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "RTL测试记录器")
  
  // 测试阿拉伯语日志（从右到左）
  let arabic_log = azimuth::LogRecord::new(azimuth::Info, "تم تسجيل دخول المستخدم بنجاح")
  assert_eq(azimuth::LogRecord::body(arabic_log), Some("تم تسجيل دخول المستخدم بنجاح"))
  azimuth::Logger::emit(logger, arabic_log)
  
  // 测试希伯来语日志（从右到左）
  let hebrew_log = azimuth::LogRecord::new(azimuth::Info, "התחברות המשתמש בוצעה בהצלחה")
  assert_eq(azimuth::LogRecord::body(hebrew_log), Some("התחברות המשתמש בוצעה בהצלחה"))
  azimuth::Logger::emit(logger, hebrew_log)
  
  // 测试波斯语日志（从右到左）
  let persian_log = azimuth::LogRecord::new(azimuth::Info, "ورود کاربر با موفقیت انجام شد")
  assert_eq(azimuth::LogRecord::body(persian_log), Some("ورود کاربر با موفقیت انجام شد"))
  azimuth::Logger::emit(logger, persian_log)
  
  // 测试 Urdu 日志（从右到左）
  let urdu_log = azimuth::LogRecord::new(azimuth::Info, "صارف کا داخل ہونا کامیابی سے ہوا")
  assert_eq(azimuth::LogRecord::body(urdu_log), Some("صارف کا داخل ہونا کامیابی سے ہوا"))
  azimuth::Logger::emit(logger, urdu_log)
  
  // 测试RTL语言中的span名称
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "RTL追踪器")
  
  let arabic_span = azimuth::Tracer::start_span(tracer, "عملية مصادقة المستخدم")
  assert_eq(azimuth::Span::name(arabic_span), "عملية مصادقة المستخدم")
  azimuth::Span::add_event(arabic_span, "بدء التحقق من هوية المستخدم")
  azimuth::Span::end(arabic_span)
  
  let hebrew_span = azimuth::Tracer::start_span(tracer, "תהליך אימות משתמש")
  assert_eq(azimuth::Span::name(hebrew_span), "תהליך אימות משתמש")
  azimuth::Span::add_event(hebrew_span, "התחלת בדיקת זהות המשתמש")
  azimuth::Span::end(hebrew_span)
}