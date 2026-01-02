// Internationalization Tests for Azimuth Telemetry System
// This file contains test cases covering internationalization and localization support

test "unicode string handling in attributes" {
  // Test Unicode characters in attribute values
  let attributes = Attributes::new()
  
  // Test Chinese characters
  Attributes::set(attributes, "chinese.message", StringValue("你好，世界！"))
  assert_eq(Attributes::get(attributes, "chinese.message"), Some(StringValue("你好，世界！")))
  
  // Test Japanese characters
  Attributes::set(attributes, "japanese.message", StringValue("こんにちは、世界！"))
  assert_eq(Attributes::get(attributes, "japanese.message"), Some(StringValue("こんにちは、世界！")))
  
  // Test Korean characters
  Attributes::set(attributes, "korean.message", StringValue("안녕하세요, 세계!"))
  assert_eq(Attributes::get(attributes, "korean.message"), Some(StringValue("안녕하세요, 세계!")))
  
  // Test Arabic characters
  Attributes::set(attributes, "arabic.message", StringValue("مرحبا بالعالم!"))
  assert_eq(Attributes::get(attributes, "arabic.message"), Some(StringValue("مرحبا بالعالم!")))
  
  // Test Russian characters
  Attributes::set(attributes, "russian.message", StringValue("Привет, мир!"))
  assert_eq(Attributes::get(attributes, "russian.message"), Some(StringValue("Привет, мир!")))
  
  // Test Hindi characters
  Attributes::set(attributes, "hindi.message", StringValue("नमस्ते दुनिया!"))
  assert_eq(Attributes::get(attributes, "hindi.message"), Some(StringValue("नमस्ते दुनिया!")))
  
  // Test Emoji characters
  Attributes::set(attributes, "emoji.message", StringValue("Hello 🌍! 🚀 📊 📈"))
  assert_eq(Attributes::get(attributes, "emoji.message"), Some(StringValue("Hello 🌍! 🚀 📊 📈")))
  
  // Test mixed Unicode characters
  Attributes::set(attributes, "mixed.message", StringValue("Hello 你好 こんにちは 안녕하세요 مرحبا Привет नमस्ते 🌍"))
  assert_eq(Attributes::get(attributes, "mixed.message"), Some(StringValue("Hello 你好 こんにちは 안녕하세요 مرحبا Привет नमस्ते 🌍")))
}

test "unicode string handling in baggage" {
  // Test Unicode characters in baggage entries
  let baggage = Baggage::new()
  
  // Test Chinese characters in baggage
  let chinese_baggage = Baggage::set_entry(baggage, "中文键", "中文值")
  assert_eq(Baggage::get_entry(chinese_baggage, "中文键"), Some("中文值"))
  
  // Test Japanese characters in baggage
  let japanese_baggage = Baggage::set_entry(baggage, "日本語キー", "日本語値")
  assert_eq(Baggage::get_entry(japanese_baggage, "日本語キー"), Some("日本語値"))
  
  // Test Korean characters in baggage
  let korean_baggage = Baggage::set_entry(baggage, "한국어 키", "한국어 값")
  assert_eq(Baggage::get_entry(korean_baggage, "한국어 키"), Some("한국어 값"))
  
  // Test Arabic characters in baggage
  let arabic_baggage = Baggage::set_entry(baggage, "مفتاح عربي", "قيمة عربية")
  assert_eq(Baggage::get_entry(arabic_baggage, "مفتاح عربي"), Some("قيمة عربية"))
  
  // Test Russian characters in baggage
  let russian_baggage = Baggage::set_entry(baggage, "русский ключ", "русское значение")
  assert_eq(Baggage::get_entry(russian_baggage, "русский ключ"), Some("русское значение"))
  
  // Test Hindi characters in baggage
  let hindi_baggage = Baggage::set_entry(baggage, "हिंदी कुंजी", "हिंदी मूल्य")
  assert_eq(Baggage::get_entry(hindi_baggage, "हिंदी कुंजी"), Some("हिंदी मूल्य"))
  
  // Test Emoji in baggage
  let emoji_baggage = Baggage::set_entry(baggage, "emoji🔑", "emoji🔢")
  assert_eq(Baggage::get_entry(emoji_baggage, "emoji🔑"), Some("emoji🔢"))
}

test "unicode string handling in resource attributes" {
  // Test Unicode characters in resource attributes
  let resource = Resource::new()
  
  // Test Chinese characters in resource attributes
  let chinese_attributes = [("服务名称", StringValue("测试服务")), ("服务版本", StringValue("1.0.0"))]
  let chinese_resource = Resource::with_attributes(resource, chinese_attributes)
  assert_eq(Resource::get_attribute(chinese_resource, "服务名称"), Some(StringValue("测试服务")))
  assert_eq(Resource::get_attribute(chinese_resource, "服务版本"), Some(StringValue("1.0.0")))
  
  // Test Japanese characters in resource attributes
  let japanese_attributes = [("サービス名", StringValue("テストサービス")), ("サービスバージョン", StringValue("1.0.0"))]
  let japanese_resource = Resource::with_attributes(resource, japanese_attributes)
  assert_eq(Resource::get_attribute(japanese_resource, "サービス名"), Some(StringValue("テストサービス")))
  assert_eq(Resource::get_attribute(japanese_resource, "サービスバージョン"), Some(StringValue("1.0.0")))
  
  // Test Korean characters in resource attributes
  let korean_attributes = [("서비스 이름", StringValue("테스트 서비스")), ("서비스 버전", StringValue("1.0.0"))]
  let korean_resource = Resource::with_attributes(resource, korean_attributes)
  assert_eq(Resource::get_attribute(korean_resource, "서비스 이름"), Some(StringValue("테스트 서비스")))
  assert_eq(Resource::get_attribute(korean_resource, "서비스 버전"), Some(StringValue("1.0.0")))
  
  // Test Arabic characters in resource attributes
  let arabic_attributes = [("اسم الخدمة", StringValue("خدمة الاختبار")), ("إصدار الخدمة", StringValue("1.0.0"))]
  let arabic_resource = Resource::with_attributes(resource, arabic_attributes)
  assert_eq(Resource::get_attribute(arabic_resource, "اسم الخدمة"), Some(StringValue("خدمة الاختبار")))
  assert_eq(Resource::get_attribute(arabic_resource, "إصدار الخدمة"), Some(StringValue("1.0.0")))
  
  // Test Russian characters in resource attributes
  let russian_attributes = [("имя службы", StringValue("тестовая служба")), ("версия службы", StringValue("1.0.0"))]
  let russian_resource = Resource::with_attributes(resource, russian_attributes)
  assert_eq(Resource::get_attribute(russian_resource, "имя службы"), Some(StringValue("тестовая служба")))
  assert_eq(Resource::get_attribute(russian_resource, "версия службы"), Some(StringValue("1.0.0")))
  
  // Test Hindi characters in resource attributes
  let hindi_attributes = [("सेवा का नाम", StringValue("परीक्षण सेवा")), ("सेवा संस्करण", StringValue("1.0.0"))]
  let hindi_resource = Resource::with_attributes(resource, hindi_attributes)
  assert_eq(Resource::get_attribute(hindi_resource, "सेवा का नाम"), Some(StringValue("परीक्षण सेवा")))
  assert_eq(Resource::get_attribute(hindi_resource, "सेवा संस्करण"), Some(StringValue("1.0.0")))
}

test "unicode string handling in log records" {
  // Test Unicode characters in log records
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "internationalization_logger")
  
  // Test Chinese log message
  let chinese_log = LogRecord::new(Info, "这是一条中文日志消息")
  assert_eq(LogRecord::body(chinese_log), Some("这是一条中文日志消息"))
  Logger::emit(logger, chinese_log)
  
  // Test Japanese log message
  let japanese_log = LogRecord::new(Info, "これは日本語のログメッセージです")
  assert_eq(LogRecord::body(japanese_log), Some("これは日本語のログメッセージです"))
  Logger::emit(logger, japanese_log)
  
  // Test Korean log message
  let korean_log = LogRecord::new(Info, "이것은 한국어 로그 메시지입니다")
  assert_eq(LogRecord::body(korean_log), Some("이것은 한국어 로그 메시지입니다"))
  Logger::emit(logger, korean_log)
  
  // Test Arabic log message
  let arabic_log = LogRecord::new(Info, "هذه رسالة سجل باللغة العربية")
  assert_eq(LogRecord::body(arabic_log), Some("هذه رسالة سجل باللغة العربية"))
  Logger::emit(logger, arabic_log)
  
  // Test Russian log message
  let russian_log = LogRecord::new(Info, "Это сообщение журнала на русском языке")
  assert_eq(LogRecord::body(russian_log), Some("Это сообщение журнала на русском языке"))
  Logger::emit(logger, russian_log)
  
  // Test Hindi log message
  let hindi_log = LogRecord::new(Info, "यह हिंदी में एक लॉग संदेश है")
  assert_eq(LogRecord::body(hindi_log), Some("यह हिंदी में एक लॉग संदेश है"))
  Logger::emit(logger, hindi_log)
  
  // Test Emoji log message
  let emoji_log = LogRecord::new(Info, "Log message with emoji: 🌍 🚀 📊 📈 ✅ ❌")
  assert_eq(LogRecord::body(emoji_log), Some("Log message with emoji: 🌍 🚀 📊 📈 ✅ ❌"))
  Logger::emit(logger, emoji_log)
  
  // Test mixed Unicode log message
  let mixed_log = LogRecord::new(Info, "Mixed Unicode: 你好 こんにちは 안녕하세요 مرحبا Привет नमस्ते 🌍")
  assert_eq(LogRecord::body(mixed_log), Some("Mixed Unicode: 你好 こんにちは 안녕하세요 مرحبا Привет नमस्ते 🌍"))
  Logger::emit(logger, mixed_log)
}

test "unicode string handling in span names and events" {
  // Test Unicode characters in span names and events
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "internationalization_tracer")
  
  // Test Chinese span name and event
  let chinese_span = Tracer::start_span(tracer, "中文操作")
  assert_eq(Span::name(chinese_span), "中文操作")
  Span::add_event(chinese_span, "中文事件", Some([("中文属性", StringValue("中文值"))]))
  Span::end(chinese_span)
  
  // Test Japanese span name and event
  let japanese_span = Tracer::start_span(tracer, "日本語操作")
  assert_eq(Span::name(japanese_span), "日本語操作")
  Span::add_event(japanese_span, "日本語イベント", Some([("日本語属性", StringValue("日本語値"))]))
  Span::end(japanese_span)
  
  // Test Korean span name and event
  let korean_span = Tracer::start_span(tracer, "한국어 작업")
  assert_eq(Span::name(korean_span), "한국어 작업")
  Span::add_event(korean_span, "한국어 이벤트", Some([("한국어 속성", StringValue("한국어 값"))]))
  Span::end(korean_span)
  
  // Test Arabic span name and event
  let arabic_span = Tracer::start_span(tracer, "عملية عربية")
  assert_eq(Span::name(arabic_span), "عملية عربية")
  Span::add_event(arabic_span, "حدث عربي", Some([("سمة عربية", StringValue("قيمة عربية"))]))
  Span::end(arabic_span)
  
  // Test Russian span name and event
  let russian_span = Tracer::start_span(tracer, "русская операция")
  assert_eq(Span::name(russian_span), "русская операция")
  Span::add_event(russian_span, "русское событие", Some([("русский атрибут", StringValue("русское значение"))]))
  Span::end(russian_span)
  
  // Test Hindi span name and event
  let hindi_span = Tracer::start_span(tracer, "हिंदी ऑपरेशन")
  assert_eq(Span::name(hindi_span), "हिंदी ऑपरेशन")
  Span::add_event(hindi_span, "हिंदी घटना", Some([("हिंदी विशेषता", StringValue("हिंदी मूल्य"))]))
  Span::end(hindi_span)
  
  // Test Emoji span name and event
  let emoji_span = Tracer::start_span(tracer, "Emoji Operation 🚀")
  assert_eq(Span::name(emoji_span), "Emoji Operation 🚀")
  Span::add_event(emoji_span, "Emoji Event 📊", Some([("emoji_attribute", StringValue("emoji_value 📈"))]))
  Span::end(emoji_span)
}

test "unicode string handling in context keys and values" {
  // Test Unicode characters in context keys and values
  let root_context = Context::root()
  
  // Test Chinese context key and value
  let chinese_key = ContextKey::new("中文键")
  let chinese_context = Context::with_value(root_context, chinese_key, "中文值")
  assert_eq(Context::get(chinese_context, chinese_key), Some("中文值"))
  
  // Test Japanese context key and value
  let japanese_key = ContextKey::new("日本語キー")
  let japanese_context = Context::with_value(root_context, japanese_key, "日本語値")
  assert_eq(Context::get(japanese_context, japanese_key), Some("日本語値"))
  
  // Test Korean context key and value
  let korean_key = ContextKey::new("한국어 키")
  let korean_context = Context::with_value(root_context, korean_key, "한국어 값")
  assert_eq(Context::get(korean_context, korean_key), Some("한국어 값"))
  
  // Test Arabic context key and value
  let arabic_key = ContextKey::new("مفتاح عربي")
  let arabic_context = Context::with_value(root_context, arabic_key, "قيمة عربية")
  assert_eq(Context::get(arabic_context, arabic_key), Some("قيمة عربية"))
  
  // Test Russian context key and value
  let russian_key = ContextKey::new("русский ключ")
  let russian_context = Context::with_value(root_context, russian_key, "русское значение")
  assert_eq(Context::get(russian_context, russian_key), Some("русское значение"))
  
  // Test Hindi context key and value
  let hindi_key = ContextKey::new("हिंदी कुंजी")
  let hindi_context = Context::with_value(root_context, hindi_key, "हिंदी मूल्य")
  assert_eq(Context::get(hindi_context, hindi_key), Some("हिंदी मूल्य"))
  
  // Test Emoji context key and value
  let emoji_key = ContextKey::new("emoji🔑")
  let emoji_context = Context::with_value(root_context, emoji_key, "emoji🔢")
  assert_eq(Context::get(emoji_context, emoji_key), Some("emoji🔢"))
}

test "unicode string handling in text map carrier" {
  // Test Unicode characters in text map carrier
  let carrier = TextMapCarrier::new()
  
  // Test Chinese headers
  TextMapCarrier::set(carrier, "中文头", "中文值")
  assert_eq(TextMapCarrier::get(carrier, "中文头"), Some("中文值"))
  
  // Test Japanese headers
  TextMapCarrier::set(carrier, "日本語ヘッダー", "日本語値")
  assert_eq(TextMapCarrier::get(carrier, "日本語ヘッダー"), Some("日本語値"))
  
  // Test Korean headers
  TextMapCarrier::set(carrier, "한국어 헤더", "한국어 값")
  assert_eq(TextMapCarrier::get(carrier, "한국어 헤더"), Some("한국어 값"))
  
  // Test Arabic headers
  TextMapCarrier::set(carrier, "رأس عربي", "قيمة عربية")
  assert_eq(TextMapCarrier::get(carrier, "رأس عربي"), Some("قيمة عربية"))
  
  // Test Russian headers
  TextMapCarrier::set(carrier, "русский заголовок", "русское значение")
  assert_eq(TextMapCarrier::get(carrier, "русский заголовок"), Some("русское значение"))
  
  // Test Hindi headers
  TextMapCarrier::set(carrier, "हिंदी शीर्षक", "हिंदी मूल्य")
  assert_eq(TextMapCarrier::get(carrier, "हिंदी शीर्षक"), Some("हिंदी मूल्य"))
  
  // Test Emoji headers
  TextMapCarrier::set(carrier, "emoji🔑", "emoji🔢")
  assert_eq(TextMapCarrier::get(carrier, "emoji🔑"), Some("emoji🔢"))
}

test "unicode string handling in instrumentation scope" {
  // Test Unicode characters in instrumentation scope
  let chinese_scope = InstrumentationScope::{ 
    name: "中文仪表库", 
    version: Some("1.0.0"), 
    schema_url: Some("https://example.com/中文模式") 
  }
  assert_eq(chinese_scope.name, "中文仪表库")
  assert_eq(chinese_scope.version, Some("1.0.0"))
  assert_eq(chinese_scope.schema_url, Some("https://example.com/中文模式"))
  
  let japanese_scope = InstrumentationScope::{ 
    name: "日本語計器ライブラリ", 
    version: Some("1.0.0"), 
    schema_url: Some("https://example.com/日本語スキーマ") 
  }
  assert_eq(japanese_scope.name, "日本語計器ライブラリ")
  assert_eq(japanese_scope.version, Some("1.0.0"))
  assert_eq(japanese_scope.schema_url, Some("https://example.com/日本語スキーマ"))
  
  let korean_scope = InstrumentationScope::{ 
    name: "한국어 계기 라이브러리", 
    version: Some("1.0.0"), 
    schema_url: Some("https://example.com/한국어 스키마") 
  }
  assert_eq(korean_scope.name, "한국어 계기 라이브러리")
  assert_eq(korean_scope.version, Some("1.0.0"))
  assert_eq(korean_scope.schema_url, Some("https://example.com/한국어 스키마"))
  
  let arabic_scope = InstrumentationScope::{ 
    name: "مكتبة الأجهزة العربية", 
    version: Some("1.0.0"), 
    schema_url: Some("https://example.com/المخطط العربي") 
  }
  assert_eq(arabic_scope.name, "مكتبة الأجهزة العربية")
  assert_eq(arabic_scope.version, Some("1.0.0"))
  assert_eq(arabic_scope.schema_url, Some("https://example.com/المخطط العربي"))
  
  let russian_scope = InstrumentationScope::{ 
    name: "русская библиотека инструментов", 
    version: Some("1.0.0"), 
    schema_url: Some("https://example.com/русская-схема") 
  }
  assert_eq(russian_scope.name, "русская библиотека инструментов")
  assert_eq(russian_scope.version, Some("1.0.0"))
  assert_eq(russian_scope.schema_url, Some("https://example.com/русская-схема"))
  
  let hindi_scope = InstrumentationScope::{ 
    name: "हिंदी इंस्ट्रूमेंटेशन लाइब्रेरी", 
    version: Some("1.0.0"), 
    schema_url: Some("https://example.com/हिंदी-स्कीमा") 
  }
  assert_eq(hindi_scope.name, "हिंदी इंस्ट्रूमेंटेशन लाइब्रेरी")
  assert_eq(hindi_scope.version, Some("1.0.0"))
  assert_eq(hindi_scope.schema_url, Some("https://example.com/हिंदी-स्कीमा"))
  
  let emoji_scope = InstrumentationScope::{ 
    name: "Emoji Instrumentation Library 🚀", 
    version: Some("1.0.0"), 
    schema_url: Some("https://example.com/emoji-schema📊") 
  }
  assert_eq(emoji_scope.name, "Emoji Instrumentation Library 🚀")
  assert_eq(emoji_scope.version, Some("1.0.0"))
  assert_eq(emoji_scope.schema_url, Some("https://example.com/emoji-schema📊"))
}

test "unicode string handling in metric names and descriptions" {
  // Test Unicode characters in metric names and descriptions
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "internationalization_meter")
  
  // Test Chinese metric name and description
  let chinese_counter = Meter::create_counter(meter, "中文计数器", Some("这是一个中文计数器"), Some("个"))
  assert_eq(chinese_counter.name, "中文计数器")
  assert_eq(chinese_counter.description, Some("这是一个中文计数器"))
  assert_eq(chinese_counter.unit, Some("个"))
  
  // Test Japanese metric name and description
  let japanese_histogram = Meter::create_histogram(meter, "日本語ヒストグラム", Some("これは日本語のヒストグラムです"), Some("ms"))
  assert_eq(japanese_histogram.name, "日本語ヒストグラム")
  assert_eq(japanese_histogram.description, Some("これは日本語のヒストグラムです"))
  assert_eq(japanese_histogram.unit, Some("ms"))
  
  // Test Korean metric name and description
  let korean_updown_counter = Meter::create_updown_counter(meter, "한국어 업다운 카운터", Some("이것은 한국어 업다운 카운터입니다"), Some("개"))
  assert_eq(korean_updown_counter.name, "한국어 업다운 카운터")
  assert_eq(korean_updown_counter.description, Some("이것은 한국어 업다운 카운터입니다"))
  assert_eq(korean_updown_counter.unit, Some("개"))
  
  // Test Arabic metric name and description
  let arabic_gauge = Meter::create_gauge(meter, "مقياس عربي", Some("هذا مقياس عربي"), Some("وحدة"))
  assert_eq(arabic_gauge.name, "مقياس عربي")
  assert_eq(arabic_gauge.description, Some("هذا مقياس عربي"))
  assert_eq(arabic_gauge.unit, Some("وحدة"))
  
  // Test Russian metric name and description
  let russian_counter = Meter::create_counter(meter, "русский счетчик", Some("это русский счетчик"), Some("шт"))
  assert_eq(russian_counter.name, "русский счетчик")
  assert_eq(russian_counter.description, Some("это русский счетчик"))
  assert_eq(russian_counter.unit, Some("шт"))
  
  // Test Hindi metric name and description
  let hindi_histogram = Meter::create_histogram(meter, "हिंदी हिस्टोग्राम", Some("यह एक हिंदी हिस्टोग्राम है"), Some("इकाई"))
  assert_eq(hindi_histogram.name, "हिंदी हिस्टोग्राम")
  assert_eq(hindi_histogram.description, Some("यह एक हिंदी हिस्टोग्राम है"))
  assert_eq(hindi_histogram.unit, Some("इकाई"))
  
  // Test Emoji metric name and description
  let emoji_gauge = Meter::create_gauge(meter, "Emoji Gauge 📊", Some("This is an emoji gauge 📈"), Some("unit🔢"))
  assert_eq(emoji_gauge.name, "Emoji Gauge 📊")
  assert_eq(emoji_gauge.description, Some("This is an emoji gauge 📈"))
  assert_eq(emoji_gauge.unit, Some("unit🔢"))
}