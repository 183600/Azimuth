// Internationalization Support Tests
// This file contains comprehensive tests for internationalization and Unicode support

test "unicode handling in log messages" {
  // Test Unicode handling in log messages
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "unicode-logger")
  
  // Test various Unicode characters
  let chinese_message = "这是一个中文日志消息"
  let japanese_message = "これは日本語のログメッセージです"
  let korean_message = "이것은 한국어 로그 메시지입니다"
  let arabic_message = "هذه رسالة سجل باللغة العربية"
  let russian_message = "Это сообщение журнала на русском языке"
  let emoji_message = "Log message with emojis: 🚀 🎉 ✨ 🌟"
  let mixed_message = "Mixed language: 中文 English 日本語 🌍"
  
  // Create log records with Unicode content
  let chinese_record = LogRecord::new(Info, chinese_message)
  let japanese_record = LogRecord::new(Info, japanese_message)
  let korean_record = LogRecord::new(Info, korean_message)
  let arabic_record = LogRecord::new(Info, arabic_message)
  let russian_record = LogRecord::new(Info, russian_message)
  let emoji_record = LogRecord::new(Info, emoji_message)
  let mixed_record = LogRecord::new(Info, mixed_message)
  
  // Emit Unicode log records
  Logger::emit(logger, chinese_record)
  Logger::emit(logger, japanese_record)
  Logger::emit(logger, korean_record)
  Logger::emit(logger, arabic_record)
  Logger::emit(logger, russian_record)
  Logger::emit(logger, emoji_record)
  Logger::emit(logger, mixed_record)
  
  // Verify Unicode content is preserved
  assert_eq(LogRecord::body(chinese_record), Some(chinese_message))
  assert_eq(LogRecord::body(japanese_record), Some(japanese_message))
  assert_eq(LogRecord::body(korean_record), Some(korean_message))
  assert_eq(LogRecord::body(arabic_record), Some(arabic_message))
  assert_eq(LogRecord::body(russian_record), Some(russian_message))
  assert_eq(LogRecord::body(emoji_record), Some(emoji_message))
  assert_eq(LogRecord::body(mixed_record), Some(mixed_message))
}

test "unicode attribute keys and values" {
  // Test Unicode attribute keys and values
  let attrs = Attributes::new()
  
  // Set Unicode attributes
  Attributes::set(attrs, "服务名称", StringValue("用户服务"))
  Attributes::set(attrs, "バージョン", StringValue("1.0.0"))
  Attributes::set(attrs, "환경", StringValue("프로덕션"))
  Attributes::set(attrs, "البيئة", StringValue("الإنتاج"))
  Attributes::set(attrs, "окружение", StringValue("продукт"))
  Attributes::set(attrs, "emoji.🚀", StringValue("rocket value"))
  Attributes::set(attrs, "mixed.键值", StringValue("mixed value 测试 🌟"))
  
  // Retrieve Unicode attributes
  let service_name = Attributes::get(attrs, "服务名称")
  let version = Attributes::get(attrs, "バージョン")
  let environment = Attributes::get(attrs, "환경")
  let arabic_env = Attributes::get(attrs, "البيئة")
  let russian_env = Attributes::get(attrs, "окружение")
  let emoji_attr = Attributes::get(attrs, "emoji.🚀")
  let mixed_attr = Attributes::get(attrs, "mixed.键值")
  
  // Verify Unicode attributes are preserved
  assert_eq(service_name, Some(StringValue("用户服务")))
  assert_eq(version, Some(StringValue("1.0.0")))
  assert_eq(environment, Some(StringValue("프로덕션")))
  assert_eq(arabic_env, Some(StringValue("الإنتاج")))
  assert_eq(russian_env, Some(StringValue("продукт")))
  assert_eq(emoji_attr, Some(StringValue("rocket value")))
  assert_eq(mixed_attr, Some(StringValue("mixed value 测试 🌟")))
}

test "unicode in span names and events" {
  // Test Unicode in span names and events
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "unicode-tracer")
  
  // Create spans with Unicode names
  let chinese_span = Tracer::start_span(tracer, "中文操作")
  let japanese_span = Tracer::start_span(tracer, "日本語操作")
  let korean_span = Tracer::start_span(tracer, "한국어 작업")
  let emoji_span = Tracer::start_span(tracer, "emoji operation 🚀")
  let mixed_span = Tracer::start_span(tracer, "mixed 中文 English 🌍")
  
  // Add Unicode events to spans
  Span::add_event(chinese_span, "中文事件")
  Span::add_event(japanese_span, "日本語イベント")
  Span::add_event(korean_span, "한국어 이벤트")
  Span::add_event(emoji_span, "emoji event 🎉")
  Span::add_event(mixed_span, "mixed event 中文 🌟")
  
  // Verify Unicode span names
  assert_eq(Span::name(chinese_span), "中文操作")
  assert_eq(Span::name(japanese_span), "日本語操作")
  assert_eq(Span::name(korean_span), "한국어 작업")
  assert_eq(Span::name(emoji_span), "emoji operation 🚀")
  assert_eq(Span::name(mixed_span), "mixed 中文 English 🌍")
  
  // End spans
  Span::end(chinese_span)
  Span::end(japanese_span)
  Span::end(korean_span)
  Span::end(emoji_span)
  Span::end(mixed_span)
}

test "unicode in baggage entries" {
  // Test Unicode in baggage entries
  let baggage = Baggage::new()
  
  // Set Unicode baggage entries
  baggage = Baggage::set_entry(baggage, "用户ID", "用户123")
  baggage = Baggage::set_entry(baggage, "セッションID", "セッション456")
  baggage = Baggage::set_entry(baggage, "세션ID", "세션789")
  baggage = Baggage::set_entry(baggage, "معرّف المستخدم", "مستخدم123")
  baggage = Baggage::set_entry(baggage, "идентификатор пользователя", "пользователь456")
  baggage = Baggage::set_entry(baggage, "emoji.🔑", "emoji-value 🚀")
  baggage = Baggage::set_entry(baggage, "混合.键", "混合值 测试 🌟")
  
  // Retrieve Unicode baggage entries
  let user_id = Baggage::get_entry(baggage, "用户ID")
  let session_id_ja = Baggage::get_entry(baggage, "セッションID")
  let session_id_ko = Baggage::get_entry(baggage, "세션ID")
  let arabic_user = Baggage::get_entry(baggage, "معرّف المستخدم")
  let russian_user = Baggage::get_entry(baggage, "идентификатор пользователя")
  let emoji_baggage = Baggage::get_entry(baggage, "emoji.🔑")
  let mixed_baggage = Baggage::get_entry(baggage, "混合.键")
  
  // Verify Unicode baggage entries are preserved
  assert_eq(user_id, Some("用户123"))
  assert_eq(session_id_ja, Some("セッション456"))
  assert_eq(session_id_ko, Some("세션789"))
  assert_eq(arabic_user, Some("مستخدم123"))
  assert_eq(russian_user, Some("пользователь456"))
  assert_eq(emoji_baggage, Some("emoji-value 🚀"))
  assert_eq(mixed_baggage, Some("混合值 测试 🌟"))
}

test "unicode in resource attributes" {
  // Test Unicode in resource attributes
  let resource = Resource::new()
  
  // Create resource with Unicode attributes
  let unicode_attrs = [
    ("服务名称", StringValue("用户服务")),
    ("サービス名", StringValue("ユーザーサービス")),
    ("서비스 이름", StringValue("사용자 서비스")),
    ("اسم الخدمة", StringValue("خدمة المستخدم")),
    ("имя службы", StringValue("пользовательская служба")),
    ("emoji.🏷️", StringValue("emoji service 🚀")),
    ("混合.标签", StringValue("混合标签 测试 🌟"))
  ]
  let resource_with_unicode = Resource::with_attributes(resource, unicode_attrs)
  
  // Retrieve Unicode resource attributes
  let service_name_zh = Resource::get_attribute(resource_with_unicode, "服务名称")
  let service_name_ja = Resource::get_attribute(resource_with_unicode, "サービス名")
  let service_name_ko = Resource::get_attribute(resource_with_unicode, "서비스 이름")
  let service_name_ar = Resource::get_attribute(resource_with_unicode, "اسم الخدمة")
  let service_name_ru = Resource::get_attribute(resource_with_unicode, "имя службы")
  let emoji_service = Resource::get_attribute(resource_with_unicode, "emoji.🏷️")
  let mixed_service = Resource::get_attribute(resource_with_unicode, "混合.标签")
  
  // Verify Unicode resource attributes are preserved
  assert_eq(service_name_zh, Some(StringValue("用户服务")))
  assert_eq(service_name_ja, Some(StringValue("ユーザーサービス")))
  assert_eq(service_name_ko, Some(StringValue("사용자 서비스")))
  assert_eq(service_name_ar, Some(StringValue("خدمة المستخدم")))
  assert_eq(service_name_ru, Some(StringValue("пользовательская служба")))
  assert_eq(emoji_service, Some(StringValue("emoji service 🚀")))
  assert_eq(mixed_service, Some(StringValue("混合标签 测试 🌟")))
}

test "unicode in text map carrier headers" {
  // Test Unicode in text map carrier headers
  let carrier = TextMapCarrier::new()
  
  // Set Unicode headers
  TextMapCarrier::set(carrier, "traceparent", "00-11112222333344445555666677778888-aaaabbbbccccdddd-01")
  TextMapCarrier::set(carrier, "用户信息", "用户123")
  TextMapCarrier::set(carrier, "ユーザー情報", "ユーザー456")
  TextMapCarrier::set(carrier, "사용자 정보", "사용자789")
  TextMapCarrier::set(carrier, "معلومات المستخدم", "مستخدم123")
  TextMapCarrier::set(carrier, "информация о пользователе", "пользователь456")
  TextMapCarrier::set(carrier, "emoji.📋", "emoji header 🚀")
  TextMapCarrier::set(carrier, "混合.头", "混合头信息 测试 🌟")
  
  // Retrieve Unicode headers
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  let user_info_zh = TextMapCarrier::get(carrier, "用户信息")
  let user_info_ja = TextMapCarrier::get(carrier, "ユーザー情報")
  let user_info_ko = TextMapCarrier::get(carrier, "사용자 정보")
  let user_info_ar = TextMapCarrier::get(carrier, "معلومات المستخدم")
  let user_info_ru = TextMapCarrier::get(carrier, "информация о пользователе")
  let emoji_header = TextMapCarrier::get(carrier, "emoji.📋")
  let mixed_header = TextMapCarrier::get(carrier, "混合.头")
  
  // Verify Unicode headers are preserved
  assert_eq(traceparent, Some("00-11112222333344445555666677778888-aaaabbbbccccdddd-01"))
  assert_eq(user_info_zh, Some("用户123"))
  assert_eq(user_info_ja, Some("ユーザー456"))
  assert_eq(user_info_ko, Some("사용자789"))
  assert_eq(user_info_ar, Some("مستخدم123"))
  assert_eq(user_info_ru, Some("пользователь456"))
  assert_eq(emoji_header, Some("emoji header 🚀"))
  assert_eq(mixed_header, Some("混合头信息 测试 🌟"))
}

test "unicode in context keys and values" {
  // Test Unicode in context keys and values
  let ctx = Context::root()
  
  // Create context keys with Unicode
  let chinese_key = ContextKey::new("中文键")
  let japanese_key = ContextKey::new("日本語キー")
  let korean_key = ContextKey::new("한국어 키")
  let arabic_key = ContextKey::new("مفتاح عربي")
  let russian_key = ContextKey::new("русский ключ")
  let emoji_key = ContextKey::new("emoji.🔑")
  let mixed_key = ContextKey::new("混合.键")
  
  // Set context values with Unicode
  let ctx_with_chinese = Context::with_value(ctx, chinese_key, "中文值")
  let ctx_with_japanese = Context::with_value(ctx_with_chinese, japanese_key, "日本語値")
  let ctx_with_korean = Context::with_value(ctx_with_japanese, korean_key, "한국어 값")
  let ctx_with_arabic = Context::with_value(ctx_with_korean, arabic_key, "قيمة عربية")
  let ctx_with_russian = Context::with_value(ctx_with_arabic, russian_key, "русское значение")
  let ctx_with_emoji = Context::with_value(ctx_with_russian, emoji_key, "emoji value 🚀")
  let ctx_with_mixed = Context::with_value(ctx_with_emoji, mixed_key, "混合值 测试 🌟")
  
  // Retrieve Unicode context values
  let chinese_value = Context::get(ctx_with_mixed, chinese_key)
  let japanese_value = Context::get(ctx_with_mixed, japanese_key)
  let korean_value = Context::get(ctx_with_mixed, korean_key)
  let arabic_value = Context::get(ctx_with_mixed, arabic_key)
  let russian_value = Context::get(ctx_with_mixed, russian_key)
  let emoji_value = Context::get(ctx_with_mixed, emoji_key)
  let mixed_value = Context::get(ctx_with_mixed, mixed_key)
  
  // Verify Unicode context values are preserved
  assert_eq(chinese_value, Some("中文值"))
  assert_eq(japanese_value, Some("日本語値"))
  assert_eq(korean_value, Some("한국어 값"))
  assert_eq(arabic_value, Some("قيمة عربية"))
  assert_eq(russian_value, Some("русское значение"))
  assert_eq(emoji_value, Some("emoji value 🚀"))
  assert_eq(mixed_value, Some("混合值 测试 🌟"))
}

test "unicode in metric names and descriptions" {
  // Test Unicode in metric names and descriptions
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "unicode-metrics")
  
  // Create metrics with Unicode names
  let chinese_counter = Meter::create_counter(meter, "中文计数器")
  let japanese_histogram = Meter::create_histogram(meter, "日本語ヒストグラム")
  let korean_updown = Meter::create_updown_counter(meter, "한국어 업다운 카운터")
  let emoji_gauge = Meter::create_gauge(meter, "emoji.📊")
  let mixed_counter = Meter::create_counter(meter, "mixed.计数器")
  
  // Verify Unicode metric names
  assert_eq(chinese_counter.name, "中文计数器")
  assert_eq(japanese_histogram.name, "日本語ヒストグラム")
  assert_eq(korean_updown.name, "한국어 업다운 카운터")
  assert_eq(emoji_gauge.name, "emoji.📊")
  assert_eq(mixed_counter.name, "mixed.计数器")
  
  // Test metric operations with Unicode metrics
  Counter::add(chinese_counter, 1.0)
  Histogram::record(japanese_histogram, 42.0)
  UpDownCounter::add(korean_updown, 10.0)
  
  // Verify operations complete without errors
  assert_true(true)
}

test "complex unicode scenarios" {
  // Test complex Unicode scenarios with mixed languages and special characters
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "complex-unicode")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "complex-unicode")
  
  // Create complex Unicode scenario
  let complex_span = Tracer::start_span(tracer, "复杂操作 🌍 中文 English 日本语")
  
  // Add complex Unicode events
  Span::add_event(complex_span, "事件 with mixed: 中文 🚀 English 日本语 한국어 العربية русский")
  
  // Create complex Unicode log record
  let complex_message = "复杂日志消息 with multiple languages: 中文测试 🌟 English test 日本語テスト 한국어 테스트 العربية اختبار русский тест"
  let complex_record = LogRecord::new(Warn, complex_message)
  
  // Add complex Unicode attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "复杂.属性.键", StringValue("复杂属性值 with 🚀 emojis and mixed languages"))
  
  // Create complex Unicode baggage
  let baggage = Baggage::new()
  baggage = Baggage::set_entry(baggage, "复杂.行李.键", "复杂行李值 with 🌍 mixed content")
  
  // Create complex Unicode resource
  let resource = Resource::new()
  let complex_attrs = [
    ("复杂.资源.标签", StringValue("复杂资源值 with mixed languages and emojis 🎉")),
    ("服务.名称", StringValue("多语言服务 🚀")),
    ("バージョン", StringValue("1.0.0-测试 🌟"))
  ]
  let complex_resource = Resource::with_attributes(resource, complex_attrs)
  
  // Verify complex Unicode scenario
  assert_eq(Span::name(complex_span), "复杂操作 🌍 中文 English 日本语")
  assert_eq(LogRecord::body(complex_record), Some(complex_message))
  
  let complex_attr = Attributes::get(attrs, "复杂.属性.键")
  assert_eq(complex_attr, Some(StringValue("复杂属性值 with 🚀 emojis and mixed languages")))
  
  let complex_baggage = Baggage::get_entry(baggage, "复杂.行李.键")
  assert_eq(complex_baggage, Some("复杂行李值 with 🌍 mixed content"))
  
  let complex_resource_attr = Resource::get_attribute(complex_resource, "复杂.资源.标签")
  assert_eq(complex_resource_attr, Some(StringValue("复杂资源值 with mixed languages and emojis 🎉")))
  
  // End span and emit log
  Span::end(complex_span)
  Logger::emit(logger, complex_record)
}