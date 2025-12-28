// 国际化支持和本地化测试用例
// 测试遥测系统在不同语言环境下的兼容性和本地化支持

test "多语言属性值处理测试" {
  let attrs = Attributes::new()
  
  // 测试中文属性值
  Attributes::set(attrs, "服务名称", StringValue("遥测服务"))
  Attributes::set(attrs, "服务版本", StringValue("1.0.0"))
  Attributes::set(attrs, "描述", StringValue("这是一个测试用的遥测服务"))
  
  let chinese_service_name = Attributes::get(attrs, "服务名称")
  match chinese_service_name {
    Some(StringValue(value)) => assert_eq(value, "遥测服务")
    _ => assert_true(false) // 简化实现可能不支持中文键
  }
  
  // 测试英文属性值
  Attributes::set(attrs, "service.name", StringValue("telemetry-service"))
  Attributes::set(attrs, "service.version", StringValue("1.0.0"))
  Attributes::set(attrs, "description", StringValue("This is a test telemetry service"))
  
  let english_service_name = Attributes::get(attrs, "service.name")
  match english_service_name {
    Some(StringValue(value)) => assert_eq(value, "telemetry-service")
    _ => assert_true(false)
  }
  
  // 测试日文属性值
  Attributes::set(attrs, "サービス名", StringValue("テレメトリーサービス"))
  Attributes::set(attrs, "説明", StringValue("これはテスト用のテレメトリサービスです"))
  
  // 测试韩文属性值
  Attributes::set(attrs, "서비스이름", StringValue("원격측정서비스"))
  Attributes::set(attrs, "설명", StringValue("이것은 테스트용 원격측정 서비스입니다"))
  
  // 测试阿拉伯文属性值
  Attributes::set(attrs, "اسم_الخدمة", StringValue("خدمة_القياس_عن_بعد"))
  Attributes::set(attrs, "الوصف", StringValue("هذه خدمة قياس عن بعد للاختبار"))
  
  // 测试俄文属性值
  Attributes::set(attrs, "имя_службы", StringValue("служба_телеметрии"))
  Attributes::set(attrs, "описание", StringValue("Это тестовая служба телеметрии"))
  
  // 测试混合语言属性值
  Attributes::set(attrs, "mixed.language", StringValue="中文English日本語한국االعربيةРусский")
  
  let mixed_value = Attributes::get(attrs, "mixed.language")
  match mixed_value {
    Some(StringValue(value)) => assert_eq(value, "中文English日本語한국االعربيةРусский")
    _ => assert_true(false)
  }
  
  assert_true(true)
}

test "多语言LogRecord处理测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "国际化测试")
  
  // 测试中文日志消息
  let chinese_log = LogRecord::new(Info, "这是一个中文信息日志")
  Logger::emit(logger, chinese_log)
  assert_eq(LogRecord::severity_number(chinese_log), Info)
  match LogRecord::body(chinese_log) {
    Some(body) => assert_eq(body, "这是一个中文信息日志")
    None => assert_true(false)
  }
  
  // 测试英文日志消息
  let english_log = LogRecord::new(Warn, "This is an English warning log")
  Logger::emit(logger, english_log)
  assert_eq(LogRecord::severity_number(english_log), Warn)
  match LogRecord::body(english_log) {
    Some(body) => assert_eq(body, "This is an English warning log")
    None => assert_true(false)
  }
  
  // 测试日文日志消息
  let japanese_log = LogRecord::new(Error, "これは日本語のエラーログです")
  Logger::emit(logger, japanese_log)
  
  // 测试韩文日志消息
  let korean_log = LogRecord::new(Debug, "이것은 한국어 디버그 로그입니다")
  Logger::emit(logger, korean_log)
  
  // 测试阿拉伯文日志消息
  let arabic_log = LogRecord::new(Fatal, "هذا سجل خطأ باللغة العربية")
  Logger::emit(logger, arabic_log)
  
  // 测试俄文日志消息
  let russian_log = LogRecord::new(Trace, "Это русская трассировочная запись")
  Logger::emit(logger, russian_log)
  
  // 测试包含特殊字符和emoji的日志消息
  let emoji_log = LogRecord::new(Info, "测试日志 🚀 包含 emoji 和特殊字符 !@#$%^&*()")
  Logger::emit(logger, emoji_log)
  
  // 测试带有多语言属性的复杂日志记录
  let multilingual_attrs = Attributes::new()
  Attributes::set(multilingual_attrs, "用户ID", StringValue("用户123"))
  Attributes::set(multilingual_attrs, "操作类型", StringValue="データ更新")
  Attributes::set(multilingual_attrs, "상태", StringValue="완료"))
  Attributes::set(multilingual_attrs, "الحالة", StringValue="مكتمل"))
  Attributes::set(multilingual_attrs, "статус", StringValue="завершено"))
  
  let complex_log = LogRecord::new_with_context(
    Error,
    Some("复合多语言错误日志：用户操作失败"),
    Some(multilingual_attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("多语言追踪ID"),
    Some("多语言SpanID"),
    Some(Context::root())
  )
  
  Logger::emit(logger, complex_log)
  
  assert_true(true)
}

test "多语言Span操作测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "多语言测试追踪器")
  
  // 测试中文Span名称
  let chinese_span = Tracer::start_span(tracer, "中文操作Span")
  assert_eq(Span::name(chinese_span), "中文操作Span")
  
  // 添加中文事件
  Span::add_event(chinese_span, "用户登录", Some([
    ("用户名", StringValue="张三"),
    ("登录方式", StringValue="密码"),
    ("IP地址", StringValue="192.168.1.100")
  ]))
  
  Span::set_status(chinese_span, Ok)
  Span::end(chinese_span)
  
  // 测试日文Span名称
  let japanese_span = Tracer::start_span(tracer, "日本語操作スパン")
  assert_eq(Span::name(japanese_span), "日本語操作スパン")
  
  // 添加日文事件
  Span::add_event(japanese_span, "ユーザーログイン", Some([
    ("ユーザー名", StringValue="田中太郎"),
    ("ログイン方法", StringValue="パスワード"),
    ("IPアドレス", StringValue="192.168.1.101")
  ]))
  
  Span::set_status(japanese_span, Error)
  Span::end(japanese_span)
  
  // 测试韩文Span名称
  let korean_span = Tracer::start_span(tracer, "한국어 작업 스팬")
  assert_eq(Span::name(korean_span), "한국어 작업 스팬")
  
  // 测试阿拉伯文Span名称
  let arabic_span = Tracer::start_span(tracer, "عملية باللغة العربية")
  assert_eq(Span::name(arabic_span), "عملية باللغة العربية")
  
  // 测试俄文Span名称
  let russian_span = Tracer::start_span(tracer, "операция на русском языке")
  assert_eq(Span::name(russian_span), "операция на русском языке")
  
  // 测试混合语言Span名称和事件
  let mixed_span = Tracer::start_span(tracer, "Mixed Language Span 混合语言スパン 혼합언어")
  
  Span::add_event(mixed_span, "多语言事件", Some([
    ("中文属性", StringValue="中文值"),
    ("English Attribute", StringValue="English Value"),
    ("日本語属性", StringValue="日本語値"),
    ("한국어 속성", StringValue="한국어 값"),
    ("الخاصية العربية", StringValue="القيمة العربية"),
    ("Русский атрибут", StringValue="Русское значение")
  ]))
  
  Span::end(mixed_span)
  
  assert_true(true)
}

test "多语言Resource和InstrumentationScope测试" {
  // 测试多语言Resource属性
  let resource = Resource::new()
  
  let multilingual_attributes = [
    ("服务名称", StringValue("遥测服务")),
    ("service.name", StringValue("telemetry-service")),
    ("サービス名", StringValue="テレメトリーサービス")),
    ("서비스이름", StringValue="원격측정서비스")),
    ("اسم_الخدمة", StringValue="خدمة_القياس_عن_بعد")),
    ("имя_службы", StringValue("служба_телеметрии")),
    ("服务版本", StringValue("1.0.0")),
    ("服务描述", StringValue="支持多语言的遥测服务")),
    ("服务环境", StringValue="生产环境")
  ]
  
  let resource_with_i18n = Resource::with_attributes(resource, multilingual_attributes)
  
  // 验证各种语言的属性可以正确获取
  let chinese_service_name = Resource::get_attribute(resource_with_i18n, "服务名称")
  match chinese_service_name {
    Some(StringValue(value)) => assert_eq(value, "遥测服务")
    _ => assert_true(false) // 简化实现可能不支持
  }
  
  let english_service_name = Resource::get_attribute(resource_with_i18n, "service.name")
  match english_service_name {
    Some(StringValue(value)) => assert_eq(value, "telemetry-service")
    _ => assert_true(false)
  }
  
  // 测试多语言InstrumentationScope
  let scope_chinese = InstrumentationScope::{
    name: "中文检测器",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema")
  }
  
  let scope_japanese = InstrumentationScope::{
    name: "日本語インスツルメンテーションスコープ",
    version: Some("2.0.0"),
    schema_url: Some("https://example.co.jp/schema")
  }
  
  let scope_korean = InstrumentationScope::{
    name: "한국어 계측 범위",
    version: Some("3.0.0"),
    schema_url: Some("https://example.co.kr/schema")
  }
  
  // 验证多语言Scope的属性
  assert_eq(scope_chinese.name, "中文检测器")
  assert_eq(scope_japanese.name, "日本語インスツルメンテーションスコープ")
  assert_eq(scope_korean.name, "한국어 계측 범위")
  
  assert_true(true)
}

test "多语言Context和Baggage测试" {
  // 测试多语言Context键值对
  let ctx = Context::root()
  
  let chinese_key = ContextKey::new("中文上下文键")
  let ctx_with_chinese = Context::with_value(ctx, chinese_key, "中文上下文值")
  
  let chinese_value = Context::get(ctx_with_chinese, chinese_key)
  match chinese_value {
    Some(value) => assert_eq(value, "中文上下文值")
    None => assert_true(false) // 简化实现可能不支持
  }
  
  let japanese_key = ContextKey::new("日本語コンテキストキー")
  let ctx_with_japanese = Context::with_value(ctx_with_chinese, japanese_key, "日本語コンテキスト値")
  
  let korean_key = ContextKey::new("한국어 컨텍스트 키")
  let ctx_with_korean = Context::with_value(ctx_with_japanese, korean_key, "한국어 컨텍스트 값")
  
  let arabic_key = ContextKey::new("مفتاح السياق باللغة العربية")
  let ctx_with_arabic = Context::with_value(ctx_with_korean, arabic_key, "قيمة السياق باللغة العربية")
  
  let russian_key = ContextKey::new("ключ контекста на русском языке")
  let ctx_with_russian = Context::with_value(ctx_with_arabic, russian_key, "значение контекста на русском языке")
  
  // 验证多语言Context值
  let russian_value = Context::get(ctx_with_russian, russian_key)
  match russian_value {
    Some(value) => assert_eq(value, "значение контекста на русском языке")
    None => assert_true(false)
  }
  
  // 测试多语言Baggage
  let baggage = Baggage::new()
  
  let baggage_with_chinese = Baggage::set_entry(baggage, "中文条目", "中文值")
  let baggage_with_japanese = Baggage::set_entry(baggage_with_chinese, "日本語エントリ", "日本語値")
  let baggage_with_korean = Baggage::set_entry(baggage_with_japanese, "한국어 항목", "한국어 값")
  let baggage_with_arabic = Baggage::set_entry(baggage_with_korean, "بند باللغة العربية", "قيمة باللغة العربية")
  let baggage_with_russian = Baggage::set_entry(baggage_with_arabic, "запись на русском языке", "значение на русском языке")
  
  // 验证多语言Baggage条目
  let chinese_entry = Baggage::get_entry(baggage_with_russian, "中文条目")
  match chinese_entry {
    Some(value) => assert_eq(value, "中文值")
    None => assert_true(false) // 简化实现可能不支持
  }
  
  let japanese_entry = Baggage::get_entry(baggage_with_russian, "日本語エントリ")
  match japanese_entry {
    Some(value) => assert_eq(value, "日本語値")
    None => assert_true(false)
  }
  
  assert_true(true)
}

test "多语言Propagation测试" {
  // 测试多语言Context的传播
  let propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([propagator])
  
  let ctx = Context::root()
  
  // 添加多语言上下文值
  let ctx_with_multilingual = Context::with_value(
    Context::with_value(
      Context::with_value(
        Context::with_value(
          Context::with_value(ctx, ContextKey::new("中文信息"), "这是中文信息"),
          ContextKey::new("English Info"), "This is English info"
        ),
        ContextKey::new("日本語情報"), "これは日本語の情報です"
      ),
      ContextKey::new("한국어 정보"), "이것은 한국어 정보입니다"
    ),
    ContextKey::new("معلومات عربية"), "هذه معلومات باللغة العربية"
  )
  
  // 创建载体并注入多语言上下文
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, ctx_with_multilingual, carrier)
  
  // 添加自定义的多语言头部
  TextMapCarrier::set(carrier, "X-中文-头部", "中文头部值")
  TextMapCarrier::set(carrier, "X-English-Header", "English header value")
  TextMapCarrier::set(carrier, "X-日本語-ヘッダー", "日本語ヘッダー値")
  TextMapCarrier::set(carrier, "X-한국어-헤더", "한국어 헤더 값")
  TextMapCarrier::set(carrier, "X-العربية-رأس", "قيمة الرأس العربية")
  TextMapCarrier::set(carrier, "X-Русский-Заголовок", "Русское значение заголовка")
  
  // 验证注入的多语言头部
  let chinese_header = TextMapCarrier::get(carrier, "X-中文-头部")
  match chinese_header {
    Some(value) => assert_eq(value, "中文头部值")
    None => assert_true(false) // 简化实现可能不支持
  }
  
  let english_header = TextMapCarrier::get(carrier, "X-English-Header")
  match english_header {
    Some(value) => assert_eq(value, "English header value")
    None => assert_true(false)
  }
  
  // 从载体中提取上下文
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // 验证提取的上下文
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  match extracted_value {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false)
  }
  
  assert_true(true)
}

test "本地化时间戳和格式化测试" {
  let clock = Clock::system()
  let timestamp = Clock::now_unix_nanos(clock)
  
  // 测试不同语言环境的时间戳表示
  let log_record_chinese = LogRecord::new_with_context(
    Info,
    Some("中文日志记录，时间戳：" + timestamp.to_string()),
    None,
    Some(timestamp),
    Some(timestamp + 1000000L),
    Some("中文追踪ID"),
    Some("中文SpanID"),
    Some(Context::root())
  )
  
  let log_record_english = LogRecord::new_with_context(
    Info,
    Some("English log record, timestamp: " + timestamp.to_string()),
    None,
    Some(timestamp),
    Some(timestamp + 1000000L),
    Some("English TraceID"),
    Some("English SpanID"),
    Some(Context::root())
  )
  
  let log_record_japanese = LogRecord::new_with_context(
    Info,
    Some("日本語ログ記録、タイムスタンプ：" + timestamp.to_string()),
    None,
    Some(timestamp),
    Some(timestamp + 1000000L),
    Some("日本語トレースID"),
    Some("日本語スパンID"),
    Some(Context::root())
  )
  
  // 验证不同语言环境的日志记录
  assert_eq(LogRecord::trace_id(log_record_chinese), Some("中文追踪ID"))
  assert_eq(LogRecord::trace_id(log_record_english), Some("English TraceID"))
  assert_eq(LogRecord::trace_id(log_record_japanese), Some("日本語トレースID"))
  
  match LogRecord::body(log_record_chinese) {
    Some(body) => assert_true(body.contains("中文日志记录"))
    None => assert_true(false)
  }
  
  match LogRecord::body(log_record_english) {
    Some(body) => assert_true(body.contains("English log record"))
    None => assert_true(false)
  }
  
  match LogRecord::body(log_record_japanese) {
    Some(body) => assert_true(body.contains("日本語ログ記録"))
    None => assert_true(false)
  }
  
  assert_true(true)
}

test "多语言错误消息和状态处理测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "多语言错误测试")
  
  // 测试多语言错误状态和消息
  let error_span_chinese = Tracer::start_span(tracer, "中文错误操作")
  Span::add_event(error_span_chinese, "错误发生", Some([
    ("错误代码", StringValue="E001"),
    ("错误消息", StringValue="用户认证失败"),
    ("错误详情", StringValue="用户名或密码不正确")
  ]))
  Span::set_status(error_span_chinese, Error)
  Span::end(error_span_chinese)
  
  let error_span_english = Tracer::start_span(tracer, "English Error Operation")
  Span::add_event(error_span_english, "Error Occurred", Some([
    ("Error Code", StringValue="E001"),
    ("Error Message", StringValue="User authentication failed"),
    ("Error Details", StringValue="Username or password is incorrect")
  ]))
  Span::set_status(error_span_english, Error)
  Span::end(error_span_english)
  
  let error_span_japanese = Tracer::start_span(tracer, "日本語エラー操作")
  Span::add_event(error_span_japanese, "エラーが発生しました", Some([
    ("エラーコード", StringValue="E001"),
    ("エラーメッセージ", StringValue="ユーザー認証失敗"),
    ("エラー詳細", StringValue="ユーザー名またはパスワードが正しくありません")
  ]))
  Span::set_status(error_span_japanese, Error)
  Span::end(error_span_japanese)
  
  // 测试多语言错误日志
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "多语言错误日志")
  
  let error_log_chinese = LogRecord::new_with_context(
    Error,
    Some("严重错误：数据库连接失败"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("错误追踪ID"),
    Some("错误SpanID"),
    Some(Context::root())
  )
  
  let error_log_english = LogRecord::new_with_context(
    Error,
    Some("Critical error: Database connection failed"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("Error TraceID"),
    Some("Error SpanID"),
    Some(Context::root())
  )
  
  let error_log_japanese = LogRecord::new_with_context(
    Error,
    Some("重大なエラー：データベース接続に失敗しました"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("エラートレースID"),
    Some("エラースパンID"),
    Some(Context::root())
  )
  
  Logger::emit(logger, error_log_chinese)
  Logger::emit(logger, error_log_english)
  Logger::emit(logger, error_log_japanese)
  
  assert_true(true)
}

test "Unicode和特殊字符处理测试" {
  // 测试各种Unicode字符和特殊符号的处理
  let attrs = Attributes::new()
  
  // 测试各种Unicode字符
  Attributes::set(attrs, "emoji.test", StringValue("测试emoji 🚀🎉💻📱⚡"))
  Attributes::set(attrs, "math.symbols", StringValue("数学符号: ∑∏∫∆∇∂√∞≈≠≤≥"))
  Attributes::set(attrs, "currency.symbols", StringValue="货币符号: $¥€£¢₹₽₩"))
  Attributes::set(attrs, "arrows.symbols", StringValue("箭头符号: ←→↑↓↖↗↘↙⇐⇒⇑⇓"))
  Attributes::set(attrs, "punctuation.symbols", StringValue="标点符号: ，。；：？！""''（）【】《》"))
  
  // 测试复合脚本文本
  Attributes::set(attrs, "mixed.script", StringValue="English中文日本語한국االعبيةРусский")
  
  // 测试右侧到左侧的文本
  Attributes::set(attrs, "rtl.text", StringValue="العربية النص من اليمين إلى اليسار"))
  
  // 测试组合字符
  Attributes::set(attrs, "combining.chars", StringValue="组合字符测试: é = e + ́")
  
  // 验证特殊字符可以正确处理
  let emoji_test = Attributes::get(attrs, "emoji.test")
  match emoji_test {
    Some(StringValue(value)) => assert_true(value.contains("🚀"))
    None => assert_true(false)
  }
  
  let math_symbols = Attributes::get(attrs, "math.symbols")
  match math_symbols {
    Some(StringValue(value)) => assert_true(value.contains("∑"))
    None => assert_true(false)
  }
  
  // 测试包含特殊字符的Span和日志
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "特殊字符测试")
  
  let special_span = Tracer::start_span(tracer, "Special Characters Span 🌟")
  Span::add_event(special_span, "特殊字符事件", Some([
    ("emoji", StringValue="🎉🚀💯"),
    ("数学", StringValue="∑∏∫∆∇"),
    ("箭头", StringValue="←→↑↓")
  ]))
  Span::end(special_span)
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "特殊字符日志")
  
  let special_log = LogRecord::new(Info, "特殊字符日志记录 🎊 包含各种符号: ∑∏∫ ←→↑↓ $¥€£")
  Logger::emit(logger, special_log)
  
  assert_true(true)
}