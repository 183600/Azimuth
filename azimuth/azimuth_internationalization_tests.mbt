// Azimuth Internationalization and Localization Test Suite
// This file contains internationalization and localization test cases for the Azimuth telemetry system

// Test 1: Unicode and multi-language attribute handling
pub test "internationalization unicode attributes" {
  let attrs = azimuth::Attributes::new()
  
  // Test with Chinese characters
  azimuth::Attributes::set(attrs, "中文键", azimuth::StringValue("中文值"))
  azimuth::Attributes::set(attrs, "用户名", azimuth::StringValue("张三"))
  azimuth::Attributes::set(attrs, "服务名称", azimuth::StringValue("支付服务"))
  
  // Test with Japanese characters
  azimuth::Attributes::set(attrs, "日本語キー", azimuth::StringValue("日本語値"))
  azimuth::Attributes::set(attrs, "ユーザー名", azimuth::StringValue("田中太郎"))
  azimuth::Attributes::set(attrs, "サービス名", azimuth::StringValue("決済サービス"))
  
  // Test with Korean characters
  azimuth::Attributes::set(attrs, "한국어 키", azimuth::StringValue("한국어 값"))
  azimuth::Attributes::set(attrs, "사용자 이름", azimuth::StringValue("김철수"))
  azimuth::Attributes::set(attrs, "서비스 이름", azimuth::StringValue("결제 서비스"))
  
  // Test with Arabic characters
  azimuth::Attributes::set(attrs, "مفتاح عربي", azimuth::StringValue("قيمة عربية"))
  azimuth::Attributes::set(attrs, "اسم المستخدم", azimuth::StringValue("أحمد محمد"))
  azimuth::Attributes::set(attrs, "اسم الخدمة", azimuth::StringValue("خدمة الدفع"))
  
  // Test with Russian characters
  azimuth::Attributes::set(attrs, "русский ключ", azimuth::StringValue("русское значение"))
  azimuth::Attributes::set(attrs, "имя пользователя", azimuth::StringValue("Иван Петров"))
  azimuth::Attributes::set(attrs, "имя сервиса", azimuth::StringValue("платежный сервис"))
  
  // Test with emoji characters
  azimuth::Attributes::set(attrs, "emoji-key-🚀", azimuth::StringValue("emoji-value-🎉"))
  azimuth::Attributes::set(attrs, "status-😀", azimuth::StringValue("happy-😊"))
  azimuth::Attributes::set(attrs, "error-❌", azimuth::StringValue("failed-❌"))
  
  // Test with mixed language content
  azimuth::Attributes::set(attrs, "mixed-content", azimuth::StringValue("English 中文 日本語 한국ة العربية русский"))
  azimuth::Attributes::set(attrs, "international-emoji", azimuth::StringValue("🌍🌎🌏🌐🗺️"))
  
  // Verify Unicode attributes (sample checks)
  let chinese_result = azimuth::Attributes::get(attrs, "中文键")
  assert_eq(chinese_result, Some(azimuth::StringValue("中文值")))
  
  let japanese_result = azimuth::Attributes::get(attrs, "日本語キー")
  assert_eq(japanese_result, Some(azimuth::StringValue("日本語値")))
  
  let korean_result = azimuth::Attributes::get(attrs, "한국어 키")
  assert_eq(korean_result, Some(azimuth::StringValue("한국어 값")))
  
  let arabic_result = azimuth::Attributes::get(attrs, "مفتاح عربي")
  assert_eq(arabic_result, Some(azimuth::StringValue("قيمة عربية")))
  
  let russian_result = azimuth::Attributes::get(attrs, "русский ключ")
  assert_eq(russian_result, Some(azimuth::StringValue("русское значение")))
  
  let emoji_result = azimuth::Attributes::get(attrs, "emoji-key-🚀")
  assert_eq(emoji_result, Some(azimuth::StringValue("emoji-value-🎉")))
}

// Test 2: Multi-language log messages
pub test "internationalization log messages" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "i18n-logger")
  
  // Create log records in different languages
  let chinese_log = azimuth::LogRecord::new(azimuth::Info, "用户登录成功，用户ID: 12345")
  let japanese_log = azimuth::LogRecord::new(azimuth::Info, "ユーザーログイン成功、ユーザーID: 12345")
  let korean_log = azimuth::LogRecord::new(azimuth::Info, "사용자 로그인 성공, 사용자 ID: 12345")
  let arabic_log = azimuth::LogRecord::new(azimuth::Info, "نجح تسجيل دخول المستخدم، معرف المستخدم: 12345")
  let russian_log = azimuth::LogRecord::new(azimuth::Info, "Пользователь успешно вошел в систему, ID пользователя: 12345")
  
  // Create log records with mixed content
  let mixed_log = azimuth::LogRecord::new(azimuth::Warn, "Mixed language warning: English/中文/日本語/한국어/العربية/русский")
  let emoji_log = azimuth::LogRecord::new(azimuth::Error, "System error occurred: 🚨❌⚠️ Please check the system status")
  
  // Emit all log records
  azimuth::Logger::emit(logger, chinese_log)
  azimuth::Logger::emit(logger, japanese_log)
  azimuth::Logger::emit(logger, korean_log)
  azimuth::Logger::emit(logger, arabic_log)
  azimuth::Logger::emit(logger, russian_log)
  azimuth::Logger::emit(logger, mixed_log)
  azimuth::Logger::emit(logger, emoji_log)
  
  // Verify log messages
  assert_eq(azimuth::LogRecord::body(chinese_log), Some("用户登录成功，用户ID: 12345"))
  assert_eq(azimuth::LogRecord::body(japanese_log), Some("ユーザーログイン成功、ユーザーID: 12345"))
  assert_eq(azimuth::LogRecord::body(korean_log), Some("사용자 로그인 성공, 사용자 ID: 12345"))
  assert_eq(azimuth::LogRecord::body(arabic_log), Some("نجح تسجيل دخول المستخدم، معرف المستخدم: 12345"))
  assert_eq(azimuth::LogRecord::body(russian_log), Some("Пользователь успешно вошел в систему, ID пользователя: 12345"))
  assert_eq(azimuth::LogRecord::body(mixed_log), Some("Mixed language warning: English/中文/日本語/한국어/العربية/русский"))
  assert_eq(azimuth::LogRecord::body(emoji_log), Some("System error occurred: 🚨❌⚠️ Please check the system status"))
}

// Test 3: Internationalized span names and operations
pub test "internationalization span operations" {
  // Create spans with internationalized names
  let chinese_span_ctx = azimuth::SpanContext::new("trace-中文-12345", "span-中文-67890", true, "")
  let chinese_span = azimuth::Span::new("中文操作", azimuth::Internal, chinese_span_ctx)
  
  let japanese_span_ctx = azimuth::SpanContext::new("trace-日本語-12345", "span-日本語-67890", true, "")
  let japanese_span = azimuth::Span::new("日本語操作", azimuth::Server, japanese_span_ctx)
  
  let korean_span_ctx = azimuth::SpanContext::new("trace-한국어-12345", "span-한국어-67890", true, "")
  let korean_span = azimuth::Span::new("한국어 작업", azimuth::Client, korean_span_ctx)
  
  let arabic_span_ctx = azimuth::SpanContext::new("trace-العربية-12345", "span-العربية-67890", true, "")
  let arabic_span = azimuth::Span::new("عملية عربية", azimuth::Producer, arabic_span_ctx)
  
  let russian_span_ctx = azimuth::SpanContext::new("trace-русский-12345", "span-русский-67890", true, "")
  let russian_span = azimuth::Span::new("русская операция", azimuth::Consumer, russian_span_ctx)
  
  let emoji_span_ctx = azimuth::SpanContext::new("trace-🚀-12345", "span-🎉-67890", true, "")
  let emoji_span = azimuth::Span::new("emoji-operation-🌟", azimuth::Internal, emoji_span_ctx)
  
  // Add events with internationalized names
  azimuth::Span::add_event(chinese_span, "开始处理", [])
  azimuth::Span::add_event(chinese_span, "处理完成", [])
  
  azimuth::Span::add_event(japanese_span, "処理開始", [])
  azimuth::Span::add_event(japanese_span, "処理完了", [])
  
  azimuth::Span::add_event(korean_span, "처리 시작", [])
  azimuth::Span::add_event(korean_span, "처리 완료", [])
  
  azimuth::Span::add_event(arabic_span, "بدء المعالجة", [])
  azimuth::Span::add_event(arabic_span, "اكتملت المعالجة", [])
  
  azimuth::Span::add_event(russian_span, "Начало обработки", [])
  azimuth::Span::add_event(russian_span, "Обработка завершена", [])
  
  azimuth::Span::add_event(emoji_span, "start-🚀", [])
  azimuth::Span::add_event(emoji_span, "complete-✅", [])
  
  // Set status with internationalized descriptions
  azimuth::Span::set_status(chinese_span, azimuth::Ok, Some("操作成功完成"))
  azimuth::Span::set_status(japanese_span, azimuth::Ok, Some("操作が正常に完了しました"))
  azimuth::Span::set_status(korean_span, azimuth::Ok, Some("작업이 성공적으로 완료되었습니다"))
  azimuth::Span::set_status(arabic_span, azimuth::Ok, Some("اكتملت العملية بنجاح"))
  azimuth::Span::set_status(russian_span, azimuth::Ok, Some("Операция успешно завершена"))
  azimuth::Span::set_status(emoji_span, azimuth::Ok, Some("success-🎉"))
  
  // Verify span properties
  assert_eq(azimuth::Span::name(chinese_span), "中文操作")
  assert_eq(azimuth::Span::name(japanese_span), "日本語操作")
  assert_eq(azimuth::Span::name(korean_span), "한국어 작업")
  assert_eq(azimuth::Span::name(arabic_span), "عملية عربية")
  assert_eq(azimuth::Span::name(russian_span), "русская операция")
  assert_eq(azimuth::Span::name(emoji_span), "emoji-operation-🌟")
  
  assert_eq(azimuth::Span::kind(chinese_span), azimuth::Internal)
  assert_eq(azimuth::Span::kind(japanese_span), azimuth::Server)
  assert_eq(azimuth::Span::kind(korean_span), azimuth::Client)
  assert_eq(azimuth::Span::kind(arabic_span), azimuth::Producer)
  assert_eq(azimuth::Span::kind(russian_span), azimuth::Consumer)
  assert_eq(azimuth::Span::kind(emoji_span), azimuth::Internal)
  
  assert_eq(azimuth::Span::status(chinese_span), azimuth::Ok)
  assert_eq(azimuth::Span::status(japanese_span), azimuth::Ok)
  assert_eq(azimuth::Span::status(korean_span), azimuth::Ok)
  assert_eq(azimuth::Span::status(arabic_span), azimuth::Ok)
  assert_eq(azimuth::Span::status(russian_span), azimuth::Ok)
  assert_eq(azimuth::Span::status(emoji_span), azimuth::Ok)
}

// Test 4: Internationalized metrics names and descriptions
pub test "internationalization metrics" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "i18n-metrics")
  
  // Create metrics with internationalized names
  let chinese_counter = azimuth::Meter::create_counter(meter, "中文计数器", Some("中文计数器描述"), Some("个"))
  let japanese_counter = azimuth::Meter::create_counter(meter, "日本語カウンター", Some("日本語カウンターの説明"), Some("個"))
  let korean_counter = azimuth::Meter::create_counter(meter, "한국어 카운터", Some("한국어 카운터 설명"), Some("개"))
  let arabic_counter = azimuth::Meter::create_counter(meter, "العداد العربي", Some("وصف العداد العربي"), Some("عدد"))
  let russian_counter = azimuth::Meter::create_counter(meter, "русский счетчик", Some("описание русского счетчика"), Some("шт"))
  
  // Create histograms with internationalized names
  let chinese_histogram = azimuth::Meter::create_histogram(meter, "中文直方图", Some("中文直方图描述"), Some("毫秒"))
  let japanese_histogram = azimuth::Meter::create_histogram(meter, "日本語ヒストグラム", Some("日本語ヒストグラムの説明"), Some("ミリ秒"))
  let korean_histogram = azimuth::Meter::create_histogram(meter, "한국어 히스토그램", Some("한국어 히스토그램 설명"), Some("밀리초"))
  let arabic_histogram = azimuth::Meter::create_histogram(meter, "المدرج التكراري العربي", Some("وصف المدرج التكراري العربي"), Some("مللي ثانية"))
  let russian_histogram = azimuth::Meter::create_histogram(meter, "русская гистограмма", Some("описание русской гистограммы"), Some("мс"))
  
  // Create gauges with internationalized names
  let chinese_gauge = azimuth::Meter::create_gauge(meter, "中文仪表", Some("中文仪表描述"), Some("百分比"))
  let japanese_gauge = azimuth::Meter::create_gauge(meter, "日本語ゲージ", Some("日本語ゲージの説明"), Some("パーセント"))
  let korean_gauge = azimuth::Meter::create_gauge(meter, "한국어 게이지", Some("한국어 게이지 설명"), Some("퍼센트"))
  let arabic_gauge = azimuth::Meter::create_gauge(meter, "المقياس العربي", Some("وصف المقياس العربي"), Some("نسبة مئوية"))
  let russian_gauge = azimuth::Meter::create_gauge(meter, "русский датчик", Some("описание русского датчика"), Some("процент"))
  
  // Perform operations on internationalized metrics
  azimuth::Counter::add(chinese_counter, 10.0)
  azimuth::Counter::add(japanese_counter, 20.0)
  azimuth::Counter::add(korean_counter, 30.0)
  azimuth::Counter::add(arabic_counter, 40.0)
  azimuth::Counter::add(russian_counter, 50.0)
  
  azimuth::Histogram::record(chinese_histogram, 100.0)
  azimuth::Histogram::record(japanese_histogram, 200.0)
  azimuth::Histogram::record(korean_histogram, 300.0)
  azimuth::Histogram::record(arabic_histogram, 400.0)
  azimuth::Histogram::record(russian_histogram, 500.0)
  
  azimuth::UpDownCounter::add(azimuth::Meter::create_updown_counter(meter, "中文上下计数器"), 5.0)
  azimuth::UpDownCounter::add(azimuth::Meter::create_updown_counter(meter, "日本語アップダウンカウンター"), 10.0)
  azimuth::UpDownCounter::add(azimuth::Meter::create_updown_counter(meter, "한국어 업다운 카운터"), 15.0)
  azimuth::UpDownCounter::add(azimuth::Meter::create_updown_counter(meter, "العداد الصعودي والهبوطي العربي"), 20.0)
  azimuth::UpDownCounter::add(azimuth::Meter::create_updown_counter(meter, "русский счетчик вверх-вниз"), 25.0)
  
  // Verify metrics properties
  assert_eq(chinese_counter.name, "中文计数器")
  assert_eq(chinese_counter.description, Some("中文计数器描述"))
  assert_eq(chinese_counter.unit, Some("个"))
  
  assert_eq(japanese_counter.name, "日本語カウンター")
  assert_eq(japanese_counter.description, Some("日本語カウンターの説明"))
  assert_eq(japanese_counter.unit, Some("個"))
  
  assert_eq(korean_counter.name, "한국어 카운터")
  assert_eq(korean_counter.description, Some("한국어 카운터 설명"))
  assert_eq(korean_counter.unit, Some("개"))
  
  assert_eq(arabic_counter.name, "العداد العربي")
  assert_eq(arabic_counter.description, Some("وصف العداد العربي"))
  assert_eq(arabic_counter.unit, Some("عدد"))
  
  assert_eq(russian_counter.name, "русский счетчик")
  assert_eq(russian_counter.description, Some("описание русского счетчика"))
  assert_eq(russian_counter.unit, Some("шт"))
  
  assert_eq(chinese_histogram.name, "中文直方图")
  assert_eq(chinese_histogram.description, Some("中文直方图描述"))
  assert_eq(chinese_histogram.unit, Some("毫秒"))
  
  assert_eq(japanese_histogram.name, "日本語ヒストグラム")
  assert_eq(japanese_histogram.description, Some("日本語ヒストグラムの説明"))
  assert_eq(japanese_histogram.unit, Some("ミリ秒"))
  
  assert_eq(korean_histogram.name, "한국어 히스토그램")
  assert_eq(korean_histogram.description, Some("한국어 히스토그램 설명"))
  assert_eq(korean_histogram.unit, Some("밀리초"))
  
  assert_eq(arabic_histogram.name, "المدرج التكراري العربي")
  assert_eq(arabic_histogram.description, Some("وصف المدرج التكراري العربي"))
  assert_eq(arabic_histogram.unit, Some("مللي ثانية"))
  
  assert_eq(russian_histogram.name, "русская гистограмма")
  assert_eq(russian_histogram.description, Some("описание русской гистограммы"))
  assert_eq(russian_histogram.unit, Some("мс"))
}

// Test 5: Internationalized context and baggage
pub test "internationalization context baggage" {
  // Test context with internationalized keys
  let ctx = azimuth::Context::root()
  
  let chinese_key = azimuth::ContextKey::new("中文键")
  let ctx_with_chinese = azimuth::Context::with_value(ctx, chinese_key, "中文值")
  
  let japanese_key = azimuth::ContextKey::new("日本語キー")
  let ctx_with_japanese = azimuth::Context::with_value(ctx_with_chinese, japanese_key, "日本語値")
  
  let korean_key = azimuth::ContextKey::new("한국어 키")
  let ctx_with_korean = azimuth::Context::with_value(ctx_with_japanese, korean_key, "한국어 값")
  
  let arabic_key = azimuth::ContextKey::new("مفتاح عربي")
  let ctx_with_arabic = azimuth::Context::with_value(ctx_with_korean, arabic_key, "قيمة عربية")
  
  let russian_key = azimuth::ContextKey::new("русский ключ")
  let ctx_with_russian = azimuth::Context::with_value(ctx_with_arabic, russian_key, "русское значение")
  
  let emoji_key = azimuth::ContextKey::new("emoji-key-🚀")
  let ctx_with_emoji = azimuth::Context::with_value(ctx_with_russian, emoji_key, "emoji-value-🎉")
  
  // Verify context values
  assert_eq(azimuth::Context::get(ctx_with_chinese, chinese_key), Some("中文值"))
  assert_eq(azimuth::Context::get(ctx_with_japanese, japanese_key), Some("日本語値"))
  assert_eq(azimuth::Context::get(ctx_with_korean, korean_key), Some("한국어 값"))
  assert_eq(azimuth::Context::get(ctx_with_arabic, arabic_key), Some("قيمة عربية"))
  assert_eq(azimuth::Context::get(ctx_with_russian, russian_key), Some("русское значение"))
  assert_eq(azimuth::Context::get(ctx_with_emoji, emoji_key), Some("emoji-value-🎉"))
  
  // Test baggage with internationalized keys and values
  let baggage = azimuth::Baggage::new()
  
  let baggage_with_chinese = azimuth::Baggage::set_entry(baggage, "中文行李键", "中文行李值")
  let baggage_with_japanese = azimuth::Baggage::set_entry(baggage_with_chinese, "日本語バゲッジキー", "日本語バゲッジ値")
  let baggage_with_korean = azimuth::Baggage::set_entry(baggage_with_japanese, "한국어 수하물 키", "한국어 수하물 값")
  let baggage_with_arabic = azimuth::Baggage::set_entry(baggage_with_korean, "مفتاح الأمتعة العربية", "قيمة الأمتعة العربية")
  let baggage_with_russian = azimuth::Baggage::set_entry(baggage_with_arabic, "русский ключ багажа", "русское значение багажа")
  let baggage_with_emoji = azimuth::Baggage::set_entry(baggage_with_russian, "emoji-baggage-🎒", "emoji-baggage-value-🌍")
  
  // Verify baggage entries
  assert_eq(azimuth::Baggage::get_entry(baggage_with_chinese, "中文行李键"), Some("中文行李值"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_japanese, "日本語バゲッジキー"), Some("日本語バゲッジ値"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_korean, "한국어 수하물 키"), Some("한국어 수하물 값"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_arabic, "مفتاح الأمتعة العربية"), Some("قيمة الأمتعة العربية"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_russian, "русский ключ багажа"), Some("русское значение багажа"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_emoji, "emoji-baggage-🎒"), Some("emoji-baggage-value-🌍"))
}

// Test 6: Internationalized resource attributes
pub test "internationalization resource attributes" {
  let resource = azimuth::Resource::new()
  
  // Create resource with internationalized attributes
  let internationalized_attributes = [
    // Chinese attributes
    ("服务名称", azimuth::StringValue("支付服务")),
    ("服务版本", azimuth::StringValue("1.2.3")),
    ("环境", azimuth::StringValue("生产")),
    
    // Japanese attributes
    ("サービス名", azimuth::StringValue("決済サービス")),
    ("サービスバージョン", azimuth::StringValue("1.2.3")),
    ("環境", azimuth::StringValue("本番")),
    
    // Korean attributes
    ("서비스 이름", azimuth::StringValue("결제 서비스")),
    ("서비스 버전", azimuth::StringValue("1.2.3")),
    ("환경", azimuth::StringValue("프로덕션")),
    
    // Arabic attributes
    ("اسم الخدمة", azimuth::StringValue("خدمة الدفع")),
    ("إصدار الخدمة", azimuth::StringValue("1.2.3")),
    ("البيئة", azimuth::StringValue("الإنتاج")),
    
    // Russian attributes
    ("имя сервиса", azimuth::StringValue("платежный сервис")),
    ("версия сервиса", azimuth::StringValue("1.2.3")),
    ("окружение", azimuth::StringValue("продукт")),
    
    // Emoji attributes
    ("service-emoji-🚀", azimuth::StringValue("payment-service-💳")),
    ("status-emoji-✅", azimuth::StringValue("healthy-🟢"))
  ]
  
  let internationalized_resource = azimuth::Resource::with_attributes(resource, internationalized_attributes)
  
  // Verify resource attributes
  assert_eq(azimuth::Resource::get_attribute(internationalized_resource, "服务名称"), Some(azimuth::StringValue("支付服务")))
  assert_eq(azimuth::Resource::get_attribute(internationalized_resource, "サービス名"), Some(azimuth::StringValue("決済サービス")))
  assert_eq(azimuth::Resource::get_attribute(internationalized_resource, "서비스 이름"), Some(azimuth::StringValue("결제 서비스")))
  assert_eq(azimuth::Resource::get_attribute(internationalized_resource, "اسم الخدمة"), Some(azimuth::StringValue("خدمة الدفع")))
  assert_eq(azimuth::Resource::get_attribute(internationalized_resource, "имя сервиса"), Some(azimuth::StringValue("платежный сервис")))
  assert_eq(azimuth::Resource::get_attribute(internationalized_resource, "service-emoji-🚀"), Some(azimuth::StringValue("payment-service-💳")))
}

// Test 7: Internationalized propagator headers
pub test "internationalization propagator headers" {
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CTraceContextPropagator::new()  // Using trace propagator for simplicity
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // Create context with internationalized values
  let ctx = azimuth::Context::root()
  
  let chinese_key = azimuth::ContextKey::new("中文键")
  let ctx_with_chinese = azimuth::Context::with_value(ctx, chinese_key, "中文值")
  
  let japanese_key = azimuth::ContextKey::new("日本語キー")
  let ctx_with_japanese = azimuth::Context::with_value(ctx_with_chinese, japanese_key, "日本語値")
  
  let korean_key = azimuth::ContextKey::new("한국어 키")
  let ctx_with_korean = azimuth::Context::with_value(ctx_with_japanese, korean_key, "한국어 값")
  
  let arabic_key = azimuth::ContextKey::new("مفتاح عربي")
  let ctx_with_arabic = azimuth::Context::with_value(ctx_with_korean, arabic_key, "قيمة عربية")
  
  let russian_key = azimuth::ContextKey::new("русский ключ")
  let ctx_with_russian = azimuth::Context::with_value(ctx_with_arabic, russian_key, "русское значение")
  
  let emoji_key = azimuth::ContextKey::new("emoji-key-🚀")
  let ctx_with_emoji = azimuth::Context::with_value(ctx_with_russian, emoji_key, "emoji-value-🎉")
  
  // Create baggage with internationalized entries
  let baggage = azimuth::Baggage::new()
  let baggage_with_international = azimuth::Baggage::set_entry(baggage, "国际化测试", "internationalization-test")
  let baggage_with_japanese = azimuth::Baggage::set_entry(baggage_with_international, "日本語テスト", "japanese-test")
  let baggage_with_korean = azimuth::Baggage::set_entry(baggage_with_japanese, "한국어 테스트", "korean-test")
  let baggage_with_arabic = azimuth::Baggage::set_entry(baggage_with_korean, "اختبار عربي", "arabic-test")
  let baggage_with_russian = azimuth::Baggage::set_entry(baggage_with_arabic, "русский тест", "russian-test")
  let baggage_with_emoji = azimuth::Baggage::set_entry(baggage_with_russian, "emoji-test-🌍", "emoji-test-value-🎉")
  
  // Inject context and baggage into carrier
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, ctx_with_emoji, carrier)
  
  // Extract context from carrier
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify extraction
  let extracted_key = azimuth::ContextKey::new("extracted")
  let extracted_value = azimuth::Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
}

// Test 8: Internationalized error messages and status codes
pub test "internationalization error messages" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "i18n-error-logger")
  
  // Create error logs in different languages
  let chinese_error_log = azimuth::LogRecord::new(azimuth::Error, "用户登录失败：用户名或密码错误")
  let japanese_error_log = azimuth::LogRecord::new(azimuth::Error, "ユーザーログイン失敗：ユーザー名またはパスワードが間違っています")
  let korean_error_log = azimuth::LogRecord::new(azimuth::Error, "사용자 로그인 실패: 사용자 이름 또는 비밀번호가 잘못되었습니다")
  let arabic_error_log = azimuth::LogRecord::new(azimuth::Error, "فشل تسجيل دخول المستخدم: اسم المستخدم أو كلمة المرور غير صحيحة")
  let russian_error_log = azimuth::LogRecord::new(azimuth::Error, "Ошибка входа пользователя: неверное имя пользователя или пароль")
  
  // Create warning logs in different languages
  let chinese_warning_log = azimuth::LogRecord::new(azimuth::Warn, "系统资源使用率过高，请及时处理")
  let japanese_warning_log = azimuth::LogRecord::new(azimuth::Warn, "システムリソース使用率が高すぎます。速やかに対処してください")
  let korean_warning_log = azimuth::LogRecord::new(azimuth::Warn, "시스템 리소스 사용률이 너무 높습니다. 즉시 처리하세요")
  let arabic_warning_log = azimuth::LogRecord::new(azimuth::Warn, "استخدام موارد النظام مرتفع جدًا، يرجى المعالجة في الوقت المناسب")
  let russian_warning_log = azimuth::LogRecord::new(azimuth::Warn, "Использование ресурсов системы слишком высоко, пожалуйста, обработайте вовремя")
  
  // Emit all error and warning logs
  azimuth::Logger::emit(logger, chinese_error_log)
  azimuth::Logger::emit(logger, japanese_error_log)
  azimuth::Logger::emit(logger, korean_error_log)
  azimuth::Logger::emit(logger, arabic_error_log)
  azimuth::Logger::emit(logger, russian_error_log)
  
  azimuth::Logger::emit(logger, chinese_warning_log)
  azimuth::Logger::emit(logger, japanese_warning_log)
  azimuth::Logger::emit(logger, korean_warning_log)
  azimuth::Logger::emit(logger, arabic_warning_log)
  azimuth::Logger::emit(logger, russian_warning_log)
  
  // Verify error and warning logs
  assert_eq(azimuth::LogRecord::severity_number(chinese_error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::body(chinese_error_log), Some("用户登录失败：用户名或密码错误"))
  
  assert_eq(azimuth::LogRecord::severity_number(japanese_error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::body(japanese_error_log), Some("ユーザーログイン失敗：ユーザー名またはパスワードが間違っています"))
  
  assert_eq(azimuth::LogRecord::severity_number(korean_error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::body(korean_error_log), Some("사용자 로그인 실패: 사용자 이름 또는 비밀번호가 잘못되었습니다"))
  
  assert_eq(azimuth::LogRecord::severity_number(arabic_error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::body(arabic_error_log), Some("فشل تسجيل دخول المستخدم: اسم المستخدم أو كلمة المرور غير صحيحة"))
  
  assert_eq(azimuth::LogRecord::severity_number(russian_error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::body(russian_error_log), Some("Ошибка входа пользователя: неверное имя пользователя или пароль"))
  
  assert_eq(azimuth::LogRecord::severity_number(chinese_warning_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::body(chinese_warning_log), Some("系统资源使用率过高，请及时处理"))
  
  assert_eq(azimuth::LogRecord::severity_number(japanese_warning_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::body(japanese_warning_log), Some("システムリソース使用率が高すぎます。速やかに対処してください"))
  
  assert_eq(azimuth::LogRecord::severity_number(korean_warning_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::body(korean_warning_log), Some("시스템 리소스 사용률이 너무 높습니다. 즉시 처리하세요"))
  
  assert_eq(azimuth::LogRecord::severity_number(arabic_warning_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::body(arabic_warning_log), Some("استخدام موارد النظام مرتفع جدًا، يرجى المعالجة في الوقت المناسب"))
  
  assert_eq(azimuth::LogRecord::severity_number(russian_warning_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::body(russian_warning_log), Some("Использование ресурсов системы слишком высоко, пожалуйста, обработайте вовремя"))
  
  // Create spans with internationalized error status
  let chinese_error_span_ctx = azimuth::SpanContext::new("trace-中文-error", "span-中文-error", true, "")
  let chinese_error_span = azimuth::Span::new("中文错误操作", azimuth::Internal, chinese_error_span_ctx)
  azimuth::Span::set_status(chinese_error_span, azimuth::Error, Some("操作失败：网络连接超时"))
  
  let japanese_error_span_ctx = azimuth::SpanContext::new("trace-日本語-error", "span-日本語-error", true, "")
  let japanese_error_span = azimuth::Span::new("日本語エラー操作", azimuth::Internal, japanese_error_span_ctx)
  azimuth::Span::set_status(japanese_error_span, azimuth::Error, Some("操作失敗：ネットワーク接続タイムアウト"))
  
  let korean_error_span_ctx = azimuth::SpanContext::new("trace-한국어-error", "span-한국어-error", true, "")
  let korean_error_span = azimuth::Span::new("한국어 오류 작업", azimuth::Internal, korean_error_span_ctx)
  azimuth::Span::set_status(korean_error_span, azimuth::Error, Some("작업 실패: 네트워크 연결 시간 초과"))
  
  // Verify span error status
  assert_eq(azimuth::Span::status(chinese_error_span), azimuth::Error)
  assert_eq(azimuth::Span::name(chinese_error_span), "中文错误操作")
  
  assert_eq(azimuth::Span::status(japanese_error_span), azimuth::Error)
  assert_eq(azimuth::Span::name(japanese_error_span), "日本語エラー操作")
  
  assert_eq(azimuth::Span::status(korean_error_span), azimuth::Error)
  assert_eq(azimuth::Span::name(korean_error_span), "한국어 오류 작업")
}