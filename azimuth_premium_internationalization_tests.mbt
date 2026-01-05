// Azimuth Premium Internationalization Tests
// This file contains high-quality test cases for internationalization and localization support

// Test 1: Unicode and multilingual attribute handling
pub test "premium unicode multilingual attributes" {
  // Test attributes with various Unicode characters
  let attrs = Attributes::new()
  
  // Test Chinese characters
  Attributes::set(attrs, "中文.键", StringValue("中文值"))
  Attributes::set(attrs, "chinese.key", StringValue("中文测试值"))
  let chinese_result = Attributes::get(attrs, "中文.键")
  let chinese_english_result = Attributes::get(attrs, "chinese.key")
  assert_eq(chinese_result, Some(StringValue("中文值")))
  assert_eq(chinese_english_result, Some(StringValue("中文测试值")))
  
  // Test Japanese characters
  Attributes::set(attrs, "日本語.キー", StringValue("日本語の値"))
  Attributes::set(attrs, "japanese.key", StringValue("日本語テスト値"))
  let japanese_result = Attributes::get(attrs, "日本語.キー")
  let japanese_english_result = Attributes::get(attrs, "japanese.key")
  assert_eq(japanese_result, Some(StringValue("日本語の値")))
  assert_eq(japanese_english_result, Some(StringValue("日本語テスト値")))
  
  // Test Korean characters
  Attributes::set(attrs, "한국어.키", StringValue("한국어 값"))
  Attributes::set(attrs, "korean.key", StringValue("한국어 테스트 값"))
  let korean_result = Attributes::get(attrs, "한국어.키")
  let korean_english_result = Attributes::get(attrs, "korean.key")
  assert_eq(korean_result, Some(StringValue("한국어 값")))
  assert_eq(korean_english_result, Some(StringValue("한국어 테스트 값")))
  
  // Test Arabic characters
  Attributes::set(attrs, "العربية.مفتاح", StringValue("قيمة العربية"))
  Attributes::set(attrs, "arabic.key", StringValue("Arabic test value"))
  let arabic_result = Attributes::get(attrs, "العربية.مفتاح")
  let arabic_english_result = Attributes::get(attrs, "arabic.key")
  assert_eq(arabic_result, Some(StringValue("قيمة العربية")))
  assert_eq(arabic_english_result, Some(StringValue("Arabic test value")))
  
  // Test Hebrew characters
  Attributes::set(attrs, "עברית.מפתח", StringValue("ערך עברית"))
  Attributes::set(attrs, "hebrew.key", StringValue("Hebrew test value"))
  let hebrew_result = Attributes::get(attrs, "עברית.מפתח")
  let hebrew_english_result = Attributes::get(attrs, "hebrew.key")
  assert_eq(hebrew_result, Some(StringValue("ערך עברית")))
  assert_eq(hebrew_english_result, Some(StringValue("Hebrew test value")))
  
  // Test Cyrillic characters (Russian)
  Attributes::set(attrs, "русский.ключ", StringValue("русское значение"))
  Attributes::set(attrs, "russian.key", StringValue("Russian test value"))
  let russian_result = Attributes::get(attrs, "русский.ключ")
  let russian_english_result = Attributes::get(attrs, "russian.key")
  assert_eq(russian_result, Some(StringValue("русское значение")))
  assert_eq(russian_english_result, Some(StringValue("Russian test value")))
  
  // Test emoji characters
  Attributes::set(attrs, "emoji.key", StringValue("😀🎉🔥🚀✨"))
  Attributes::set(attrs, "mixed.emoji.🚀", StringValue("Mixed emoji key 😀"))
  let emoji_result = Attributes::get(attrs, "emoji.key")
  let mixed_emoji_result = Attributes::get(attrs, "mixed.emoji.🚀")
  assert_eq(emoji_result, Some(StringValue("😀🎉🔥🚀✨")))
  assert_eq(mixed_emoji_result, Some(StringValue("Mixed emoji key 😀")))
  
  // Test combining characters and diacritics
  Attributes::set(attrs, "diacritic.key", StringValue("café naïve résumé"))
  Attributes::set(attrs, "combining.key", StringValue("e\u0301 a\u0300 o\u0302 u\u0308"))
  let diacritic_result = Attributes::get(attrs, "diacritic.key")
  let combining_result = Attributes::get(attrs, "combining.key")
  assert_eq(diacritic_result, Some(StringValue("café naïve résumé")))
  assert_eq(combining_result, Some(StringValue("e\u0301 a\u0300 o\u0302 u\u0308")))
  
  // Test right-to-left text
  Attributes::set(attrs, "rtl.key", StringValue("שלום עולם"))
  Attributes::set(attrs, "mixed.rtl", StringValue("Hello שלום World עולם"))
  let rtl_result = Attributes::get(attrs, "rtl.key")
  let mixed_rtl_result = Attributes::get(attrs, "mixed.rtl")
  assert_eq(rtl_result, Some(StringValue("שלום עולם")))
  assert_eq(mixed_rtl_result, Some(StringValue("Hello שלום World עולם")))
  
  // Test array attributes with Unicode
  Attributes::set(attrs, "unicode.array", ArrayStringValue([
    "中文", "日本語", "한국어", "العربية", "עברית", "русский", "English"
  ]))
  let unicode_array_result = Attributes::get(attrs, "unicode.array")
  assert_true(unicode_array_result.is_some())
}

// Test 2: Internationalized span names and operations
pub test "premium internationalized spans" {
  // Test span creation with internationalized names
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "国际化测试跟踪器")
  
  // Test spans with various language names
  let chinese_span = Tracer::start_span(tracer, "中文操作")
  let japanese_span = Tracer::start_span(tracer, "日本語操作")
  let korean_span = Tracer::start_span(tracer, "한국어 작업")
  let arabic_span = Tracer::start_span(tracer, "عملية عربية")
  let hebrew_span = Tracer::start_span(tracer, "פעולה עברית")
  let russian_span = Tracer::start_span(tracer, "русская операция")
  let emoji_span = Tracer::start_span(tracer, "Emoji Operation 🚀🔥")
  
  // Verify span names
  assert_eq(Span::name(chinese_span), "中文操作")
  assert_eq(Span::name(japanese_span), "日本語操作")
  assert_eq(Span::name(korean_span), "한국어 작업")
  assert_eq(Span::name(arabic_span), "عملية عربية")
  assert_eq(Span::name(hebrew_span), "פעולה עברית")
  assert_eq(Span::name(russian_span), "русская операция")
  assert_eq(Span::name(emoji_span), "Emoji Operation 🚀🔥")
  
  // Test span events with internationalized messages
  Span::add_event(chinese_span, "中文事件消息")
  Span::add_event(japanese_span, "日本語イベントメッセージ")
  Span::add_event(korean_span, "한국어 이벤트 메시지")
  Span::add_event(arabic_span, "رسالة حدث عربية")
  Span::add_event(hebrew_span, "הודעת אירוע עברית")
  Span::add_event(russian_span, "русское сообщение о событии")
  Span::add_event(emoji_span, "Emoji Event Message ✨🎉")
  
  // Test span status with internationalized descriptions
  Span::set_status(chinese_span, Error, Some("中文错误描述"))
  Span::set_status(japanese_span, Error, Some("日本語エラー説明"))
  Span::set_status(korean_span, Error, Some("한국어 오류 설명"))
  Span::set_status(arabic_span, Error, Some("وصف الخطأ باللغة العربية"))
  Span::set_status(hebrew_span, Error, Some("תיאור שגיאה בעברית"))
  Span::set_status(russian_span, Error, Some("Описание ошибки на русском"))
  Span::set_status(emoji_span, Error, Some("Emoji Error Description ❌⚠️"))
  
  // End all spans
  Span::end(chinese_span)
  Span::end(japanese_span)
  Span::end(korean_span)
  Span::end(arabic_span)
  Span::end(hebrew_span)
  Span::end(russian_span)
  Span::end(emoji_span)
  
  // Test tracer with internationalized name
  let chinese_tracer = TracerProvider::get_tracer(tracer_provider, "中文跟踪器")
  let japanese_tracer = TracerProvider::get_tracer(tracer_provider, "日本語トレーサー")
  let korean_tracer = TracerProvider::get_tracer(tracer_provider, "한국어 추적기")
  
  assert_eq(chinese_tracer.scope.name, "中文跟踪器")
  assert_eq(japanese_tracer.scope.name, "日本語トレーサー")
  assert_eq(korean_tracer.scope.name, "한국어 추적기")
}

// Test 3: Internationalized metrics operations
pub test "premium internationalized metrics" {
  // Test metrics with internationalized names
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "国际化测试仪表")
  
  // Test counters with various language names
  let chinese_counter = Meter::create_counter(meter, "中文计数器", Some("中文计数器描述"), Some("个"))
  let japanese_counter = Meter::create_counter(meter, "日本語カウンター", Some("日本語カウンター説明"), Some("個"))
  let korean_counter = Meter::create_counter(meter, "한국어 카운터", Some("한국어 카운터 설명"), Some("개"))
  let arabic_counter = Meter::create_counter(meter, "العداد العربي", Some("وصف العداد العربي"), Some("وحدة"))
  let hebrew_counter = Meter::create_counter(meter, "מונה עברי", Some("תיאור מונה עברי"), Some("יחידה"))
  let russian_counter = Meter::create_counter(meter, "русский счетчик", Some("описание русского счетчика"), Some("штука"))
  let emoji_counter = Meter::create_counter(meter, "Emoji Counter 🚀", Some("Emoji counter description 🔥"), Some("unit ✨"))
  
  // Verify counter names, descriptions, and units
  assert_eq(chinese_counter.name, "中文计数器")
  assert_eq(chinese_counter.description, Some("中文计数器描述"))
  assert_eq(chinese_counter.unit, Some("个"))
  
  assert_eq(japanese_counter.name, "日本語カウンター")
  assert_eq(japanese_counter.description, Some("日本語カウンター説明"))
  assert_eq(japanese_counter.unit, Some("個"))
  
  assert_eq(korean_counter.name, "한국어 카운터")
  assert_eq(korean_counter.description, Some("한국어 카운터 설명"))
  assert_eq(korean_counter.unit, Some("개"))
  
  assert_eq(arabic_counter.name, "العداد العربي")
  assert_eq(arabic_counter.description, Some("وصف العداد العربي"))
  assert_eq(arabic_counter.unit, Some("وحدة"))
  
  assert_eq(hebrew_counter.name, "מונה עברי")
  assert_eq(hebrew_counter.description, Some("תיאור מונה עברי"))
  assert_eq(hebrew_counter.unit, Some("יחידה"))
  
  assert_eq(russian_counter.name, "русский счетчик")
  assert_eq(russian_counter.description, Some("описание русского счетчика"))
  assert_eq(russian_counter.unit, Some("штука"))
  
  assert_eq(emoji_counter.name, "Emoji Counter 🚀")
  assert_eq(emoji_counter.description, Some("Emoji counter description 🔥"))
  assert_eq(emoji_counter.unit, Some("unit ✨"))
  
  // Test histogram with internationalized names
  let chinese_histogram = Meter::create_histogram(meter, "中文直方图", Some("中文直方图描述"), Some("毫秒"))
  let japanese_histogram = Meter::create_histogram(meter, "日本語ヒストグラム", Some("日本語ヒストグラム説明"), Some("ミリ秒"))
  let korean_histogram = Meter::create_histogram(meter, "한국어 히스토그램", Some("한국어 히스토그램 설명"), Some("밀리초"))
  
  // Verify histogram names, descriptions, and units
  assert_eq(chinese_histogram.name, "中文直方图")
  assert_eq(chinese_histogram.description, Some("中文直方图描述"))
  assert_eq(chinese_histogram.unit, Some("毫秒"))
  
  assert_eq(japanese_histogram.name, "日本語ヒストグラム")
  assert_eq(japanese_histogram.description, Some("日本語ヒストグラム説明"))
  assert_eq(japanese_histogram.unit, Some("ミリ秒"))
  
  assert_eq(korean_histogram.name, "한국어 히스토그램")
  assert_eq(korean_histogram.description, Some("한국어 히스토그램 설명"))
  assert_eq(korean_histogram.unit, Some("밀리초"))
  
  // Test meter with internationalized name
  let chinese_meter = MeterProvider::get_meter(meter_provider, "中文仪表")
  let japanese_meter = MeterProvider::get_meter(meter_provider, "日本語メーター")
  let korean_meter = MeterProvider::get_meter(meter_provider, "한국어 미터")
  
  assert_eq(chinese_meter.scope.name, "中文仪表")
  assert_eq(japanese_meter.scope.name, "日本語メーター")
  assert_eq(korean_meter.scope.name, "한국어 미터")
}

// Test 4: Internationalized logging operations
pub test "premium internationalized logging" {
  // Test logging with internationalized messages
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "国际化测试记录器")
  
  // Test log records with various language messages
  let chinese_log = LogRecord::new(Info, "中文信息日志消息")
  let japanese_log = LogRecord::new(Info, "日本語情報ログメッセージ")
  let korean_log = LogRecord::new(Info, "한국어 정보 로그 메시지")
  let arabic_log = LogRecord::new(Info, "رسالة سجل معلومات عربية")
  let hebrew_log = LogRecord::new(Info, "הודעת יומן מידע בעברית")
  let russian_log = LogRecord::new(Info, "русское информационное сообщение журнала")
  let emoji_log = LogRecord::new(Info, "Emoji Info Log Message 📝✨")
  
  // Verify log messages
  assert_eq(LogRecord::body(chinese_log), Some("中文信息日志消息"))
  assert_eq(LogRecord::body(japanese_log), Some("日本語情報ログメッセージ"))
  assert_eq(LogRecord::body(korean_log), Some("한국어 정보 로그 메시지"))
  assert_eq(LogRecord::body(arabic_log), Some("رسالة سجل معلومات عربية"))
  assert_eq(LogRecord::body(hebrew_log), Some("הודעת יומן מידע בעברית"))
  assert_eq(LogRecord::body(russian_log), Some("русское информационное сообщение журнала"))
  assert_eq(LogRecord::body(emoji_log), Some("Emoji Info Log Message 📝✨"))
  
  // Test detailed log records with internationalized context
  let chinese_detailed_log = LogRecord::new_with_context(
    Error,
    Some("中文详细错误消息"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("中文跟踪ID"),
    Some("中文跨度ID"),
    Some(Context::root())
  )
  
  let japanese_detailed_log = LogRecord::new_with_context(
    Error,
    Some("日本語詳細エラーメッセージ"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("日本語トレースID"),
    Some("日本語スパンID"),
    Some(Context::root())
  )
  
  // Verify detailed log records
  assert_eq(LogRecord::body(chinese_detailed_log), Some("中文详细错误消息"))
  assert_eq(LogRecord::trace_id(chinese_detailed_log), Some("中文跟踪ID"))
  assert_eq(LogRecord::span_id(chinese_detailed_log), Some("中文跨度ID"))
  
  assert_eq(LogRecord::body(japanese_detailed_log), Some("日本語詳細エラーメッセージ"))
  assert_eq(LogRecord::trace_id(japanese_detailed_log), Some("日本語トレースID"))
  assert_eq(LogRecord::span_id(japanese_detailed_log), Some("日本語スパンID"))
  
  // Emit all log records
  Logger::emit(logger, chinese_log)
  Logger::emit(logger, japanese_log)
  Logger::emit(logger, korean_log)
  Logger::emit(logger, arabic_log)
  Logger::emit(logger, hebrew_log)
  Logger::emit(logger, russian_log)
  Logger::emit(logger, emoji_log)
  Logger::emit(logger, chinese_detailed_log)
  Logger::emit(logger, japanese_detailed_log)
  
  // Test logger with internationalized name
  let chinese_logger = LoggerProvider::get_logger(logger_provider, "中文记录器")
  let japanese_logger = LoggerProvider::get_logger(logger_provider, "日本語ロガー")
  let korean_logger = LoggerProvider::get_logger(logger_provider, "한국어 로거")
  
  assert_eq(chinese_logger.scope.name, "中文记录器")
  assert_eq(japanese_logger.scope.name, "日本語ロガー")
  assert_eq(korean_logger.scope.name, "한국어 로거")
}

// Test 5: Internationalized context and baggage operations
pub test "premium internationalized context baggage" {
  // Test context operations with internationalized keys and values
  let root_ctx = Context::root()
  
  // Test context with various language keys and values
  let chinese_key = ContextKey::new("中文上下文键")
  let ctx_with_chinese = Context::with_value(root_ctx, chinese_key, "中文上下文值")
  let chinese_result = Context::get(ctx_with_chinese, chinese_key)
  assert_eq(chinese_result, Some("中文上下文值"))
  
  let japanese_key = ContextKey::new("日本語コンテキストキー")
  let ctx_with_japanese = Context::with_value(root_ctx, japanese_key, "日本語コンテキスト値")
  let japanese_result = Context::get(ctx_with_japanese, japanese_key)
  assert_eq(japanese_result, Some("日本語コンテキスト値"))
  
  let korean_key = ContextKey::new("한국어 컨텍스트 키")
  let ctx_with_korean = Context::with_value(root_ctx, korean_key, "한국어 컨텍스트 값")
  let korean_result = Context::get(ctx_with_korean, korean_key)
  assert_eq(korean_result, Some("한국어 컨텍스트 값"))
  
  let arabic_key = ContextKey::new("مفتاح السياق العربي")
  let ctx_with_arabic = Context::with_value(root_ctx, arabic_key, "قيمة السياق العربية")
  let arabic_result = Context::get(ctx_with_arabic, arabic_key)
  assert_eq(arabic_result, Some("قيمة السياق العربية"))
  
  let hebrew_key = ContextKey::new("מפתח הקשר עברי")
  let ctx_with_hebrew = Context::with_value(root_ctx, hebrew_key, "ערך הקשר עברי")
  let hebrew_result = Context::get(ctx_with_hebrew, hebrew_key)
  assert_eq(hebrew_result, Some("ערך הקשר עברי"))
  
  let russian_key = ContextKey::new("русский ключ контекста")
  let ctx_with_russian = Context::with_value(root_ctx, russian_key, "русское значение контекста")
  let russian_result = Context::get(ctx_with_russian, russian_key)
  assert_eq(russian_result, Some("русское значение контекста"))
  
  let emoji_key = ContextKey::new("emoji.context.key 🚀")
  let ctx_with_emoji = Context::with_value(root_ctx, emoji_key, "Emoji context value 🔥")
  let emoji_result = Context::get(ctx_with_emoji, emoji_key)
  assert_eq(emoji_result, Some("Emoji context value 🔥"))
  
  // Test baggage operations with internationalized keys and values
  let baggage = Baggage::new()
  
  // Test baggage with various language keys and values
  let baggage_with_chinese = Baggage::set_entry(baggage, "中文行李键", "中文行李值")
  let chinese_baggage_result = Baggage::get_entry(baggage_with_chinese, "中文行李键")
  assert_eq(chinese_baggage_result, Some("中文行李值"))
  
  let baggage_with_japanese = Baggage::set_entry(baggage, "日本語バゲッジキー", "日本語バゲッジ値")
  let japanese_baggage_result = Baggage::get_entry(baggage_with_japanese, "日本語バゲッジキー")
  assert_eq(japanese_baggage_result, Some("日本語バゲッジ値"))
  
  let baggage_with_korean = Baggage::set_entry(baggage, "한국어 수하물 키", "한국어 수하물 값")
  let korean_baggage_result = Baggage::get_entry(baggage_with_korean, "한국어 수하물 키")
  assert_eq(korean_baggage_result, Some("한국어 수하물 값"))
  
  let baggage_with_arabic = Baggage::set_entry(baggage, "مفتاح الأمتعة العربية", "قيمة الأمتعة العربية")
  let arabic_baggage_result = Baggage::get_entry(baggage_with_arabic, "مفتاح الأمتعة العربية")
  assert_eq(arabic_baggage_result, Some("قيمة الأمتعة العربية"))
  
  let baggage_with_hebrew = Baggage::set_entry(baggage, "מפתח כבודה עברי", "ערך כבודה עברי")
  let hebrew_baggage_result = Baggage::get_entry(baggage_with_hebrew, "מפתח כבודה עברי")
  assert_eq(hebrew_baggage_result, Some("ערך כבודה עברי"))
  
  let baggage_with_russian = Baggage::set_entry(baggage, "русский ключ багажа", "русское значение багажа")
  let russian_baggage_result = Baggage::get_entry(baggage_with_russian, "русский ключ багажа")
  assert_eq(russian_baggage_result, Some("русское значение багажа"))
  
  let baggage_with_emoji = Baggage::set_entry(baggage, "emoji.baggage.key 🎉", "Emoji baggage value ✨")
  let emoji_baggage_result = Baggage::get_entry(baggage_with_emoji, "emoji.baggage.key 🎉")
  assert_eq(emoji_baggage_result, Some("Emoji baggage value ✨"))
}

// Test 6: Internationalized resource operations
pub test "premium internationalized resources" {
  // Test resource with internationalized attributes
  let resource = Resource::new()
  
  // Test resource attributes with various languages
  let internationalized_resource = Resource::with_attributes(resource, [
    ("service.name", StringValue("国际化服务")),
    ("service.description", StringValue("这是一个国际化测试服务")),
    ("service.name.ja", StringValue("国際化サービス")),
    ("service.description.ja", StringValue("これは国際化テストサービスです")),
    ("service.name.ko", StringValue("국제화 서비스")),
    ("service.description.ko", StringValue("이것은 국제화 테스트 서비스입니다")),
    ("service.name.ar", StringValue("خدمة دولية")),
    ("service.description.ar", StringValue("هذه خدمة اختبار دولية")),
    ("service.name.he", StringValue("שירות בינלאומי")),
    ("service.description.he", StringValue("זוהי שירות בדיקה בינלאומית")),
    ("service.name.ru", StringValue("международная служба")),
    ("service.description.ru", StringValue("Это международная тестовая служба")),
    ("service.name.emoji", StringValue("International Service 🌍")),
    ("service.description.emoji", StringValue("This is an international test service 🚀✨"))
  ])
  
  // Verify resource attributes
  assert_eq(Resource::get_attribute(internationalized_resource, "service.name"), Some(StringValue("国际化服务")))
  assert_eq(Resource::get_attribute(internationalized_resource, "service.description"), Some(StringValue("这是一个国际化测试服务")))
  assert_eq(Resource::get_attribute(internationalized_resource, "service.name.ja"), Some(StringValue("国際化サービス")))
  assert_eq(Resource::get_attribute(internationalized_resource, "service.description.ja"), Some(StringValue("これは国際化テストサービスです")))
  assert_eq(Resource::get_attribute(internationalized_resource, "service.name.ko"), Some(StringValue("국제화 서비스")))
  assert_eq(Resource::get_attribute(internationalized_resource, "service.description.ko"), Some(StringValue("이것은 국제화 테스트 서비스입니다")))
  assert_eq(Resource::get_attribute(internationalized_resource, "service.name.ar"), Some(StringValue("خدمة دولية")))
  assert_eq(Resource::get_attribute(internationalized_resource, "service.description.ar"), Some(StringValue("هذه خدمة اختبار دولية")))
  assert_eq(Resource::get_attribute(internationalized_resource, "service.name.he"), Some(StringValue("שירות בינלאומי")))
  assert_eq(Resource::get_attribute(internationalized_resource, "service.description.he"), Some(StringValue("זוהי שירות בדיקה בינלאומית")))
  assert_eq(Resource::get_attribute(internationalized_resource, "service.name.ru"), Some(StringValue("международная служба")))
  assert_eq(Resource::get_attribute(internationalized_resource, "service.description.ru"), Some(StringValue("Это международная тестовая служба")))
  assert_eq(Resource::get_attribute(internationalized_resource, "service.name.emoji"), Some(StringValue("International Service 🌍")))
  assert_eq(Resource::get_attribute(internationalized_resource, "service.description.emoji"), Some(StringValue("This is an international test service 🚀✨")))
  
  // Test resource merging with internationalized attributes
  let base_resource = Resource::with_attributes(resource, [
    ("base.service.name", StringValue("基础服务")),
    ("base.service.name.ja", StringValue("ベースサービス")),
    ("base.service.name.ko", StringValue("기본 서비스"))
  ])
  
  let override_resource = Resource::with_attributes(resource, [
    ("override.service.name", StringValue("覆盖服务")),
    ("override.service.name.ja", StringValue("オーバーライドサービス")),
    ("override.service.name.ko", StringValue("재정의 서비스")),
    ("base.service.name", StringValue("覆盖的基础服务"))  // Override base attribute
  ])
  
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // Verify merged resource
  assert_eq(Resource::get_attribute(merged_resource, "base.service.name"), Some(StringValue("覆盖的基础服务")))
  assert_eq(Resource::get_attribute(merged_resource, "base.service.name.ja"), Some(StringValue("ベースサービス")))
  assert_eq(Resource::get_attribute(merged_resource, "base.service.name.ko"), Some(StringValue("기본 서비스")))
  assert_eq(Resource::get_attribute(merged_resource, "override.service.name"), Some(StringValue("覆盖服务")))
  assert_eq(Resource::get_attribute(merged_resource, "override.service.name.ja"), Some(StringValue("オーバーライドサービス")))
  assert_eq(Resource::get_attribute(merged_resource, "override.service.name.ko"), Some(StringValue("재정의 서비스")))
}

// Test 7: Internationalized propagator operations
pub test "premium internationalized propagators" {
  // Test propagator operations with internationalized context
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // Test injection and extraction with internationalized context
  let ctx = Context::root()
  let chinese_key = ContextKey::new("中文传播键")
  let ctx_with_chinese = Context::with_value(ctx, chinese_key, "中文传播值")
  
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, ctx_with_chinese, carrier)
  
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_value = Context::get(extracted_ctx, chinese_key)
  
  // Test carrier operations with internationalized headers
  TextMapCarrier::set(carrier, "中文头", "中文头值")
  TextMapCarrier::set(carrier, "日本語ヘッダー", "日本語ヘッダー値")
  TextMapCarrier::set(carrier, "한국어 헤더", "한국어 헤더 값")
  TextMapCarrier::set(carrier, "العربية رأس", "قيمة الرأس العربية")
  TextMapCarrier::set(carrier, "עברית כותרת", "ערך כותרת עברית")
  TextMapCarrier::set(carrier, "русский заголовок", "русское значение заголовка")
  TextMapCarrier::set(carrier, "emoji.header 🚀", "Emoji header value 🔥")
  
  // Verify carrier headers
  assert_eq(TextMapCarrier::get(carrier, "中文头"), Some("中文头值"))
  assert_eq(TextMapCarrier::get(carrier, "日本語ヘッダー"), Some("日本語ヘッダー値"))
  assert_eq(TextMapCarrier::get(carrier, "한국어 헤더"), Some("한국어 헤더 값"))
  assert_eq(TextMapCarrier::get(carrier, "العربية رأس"), Some("قيمة الرأس العربية"))
  assert_eq(TextMapCarrier::get(carrier, "עברית כותרת"), Some("ערך כותרת עברית"))
  assert_eq(TextMapCarrier::get(carrier, "русский заголовок"), Some("русское значение заголовка"))
  assert_eq(TextMapCarrier::get(carrier, "emoji.header 🚀"), Some("Emoji header value 🔥"))
  
  // Test baggage propagation with internationalized entries
  let baggage = Baggage::new()
  let baggage_with_chinese = Baggage::set_entry(baggage, "中文行李键", "中文行李值")
  let baggage_with_japanese = Baggage::set_entry(baggage_with_chinese, "日本語バゲッジキー", "日本語バゲッジ値")
  let baggage_with_korean = Baggage::set_entry(baggage_with_japanese, "한국어 수하물 키", "한국어 수하물 값")
  
  // In a real implementation, this would propagate internationalized baggage entries
  // For testing purposes, we verify the operations complete without errors
  assert_eq(Baggage::get_entry(baggage_with_korean, "中文行李键"), Some("中文行李值"))
  assert_eq(Baggage::get_entry(baggage_with_korean, "日本語バゲッジキー"), Some("日本語バゲッジ値"))
  assert_eq(Baggage::get_entry(baggage_with_korean, "한국어 수하물 키"), Some("한국어 수하물 값"))
}