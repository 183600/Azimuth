// Azimuth Internationalization Comprehensive Tests
// 测试遥测系统的国际化功能

test "internationalization_unicode_attribute_values" {
  // 测试 Unicode 属性值处理
  let attrs = Attributes::new()
  
  // 设置各种 Unicode 字符的属性值
  Attributes::set(attrs, "chinese.attribute", StringValue("中文属性值"))
  Attributes::set(attrs, "japanese.attribute", StringValue("日本語属性値"))
  Attributes::set(attrs, "korean.attribute", StringValue("한국어 속성값"))
  Attributes::set(attrs, "arabic.attribute", StringValue("قيمة السمة باللغة العربية"))
  Attributes::set(attrs, "russian.attribute", StringValue("значение атрибута на русском"))
  Attributes::set(attrs, "hebrew.attribute", StringValue("ערך תכונה בעברית"))
  Attributes::set(attrs, "thai.attribute", StringValue("ค่าแอตทริบิวต์ภาษาไทย"))
  Attributes::set(attrs, "hindi.attribute", StringValue("हिंदी विशेषता मान"))
  Attributes::set(attrs, "emoji.attribute", StringValue("🚀🌍💻📊"))
  
  // 验证 Unicode 属性值获取
  @assertion.assert_eq(Attributes::get(attrs, "chinese.attribute"), Some(StringValue("中文属性值")))?
  @assertion.assert_eq(Attributes::get(attrs, "japanese.attribute"), Some(StringValue("日本語属性値")))?
  @assertion.assert_eq(Attributes::get(attrs, "korean.attribute"), Some(StringValue("한국어 속성값")))?
  @assertion.assert_eq(Attributes::get(attrs, "arabic.attribute"), Some(StringValue("قيمة السمة باللغة العربية")))?
  @assertion.assert_eq(Attributes::get(attrs, "russian.attribute"), Some(StringValue("значение атрибута на русском")))?
  @assertion.assert_eq(Attributes::get(attrs, "hebrew.attribute"), Some(StringValue("ערך תכונה בעברית")))?
  @assertion.assert_eq(Attributes::get(attrs, "thai.attribute"), Some(StringValue("ค่าแอตทริบิวต์ภาษาไทย")))?
  @assertion.assert_eq(Attributes::get(attrs, "hindi.attribute"), Some(StringValue("हिंदी विशेषता मान")))?
  @assertion.assert_eq(Attributes::get(attrs, "emoji.attribute"), Some(StringValue("🚀🌍💻📊")))?
}

test "internationalization_unicode_attribute_names" {
  // 测试 Unicode 属性名处理
  let attrs = Attributes::new()
  
  // 设置各种 Unicode 字符的属性名
  Attributes::set(attrs, "中文属性", StringValue("Chinese value"))
  Attributes::set(attrs, "日本語属性", StringValue("Japanese value"))
  Attributes::set(attrs, "한국어속성", StringValue("Korean value"))
  Attributes::set(attrs, "属性名.العربية", StringValue("Arabic value"))
  Attributes::set(attrs, "атрибут.русский", StringValue("Russian value"))
  Attributes::set(attrs, "תכונה.עברית", StringValue("Hebrew value"))
  Attributes::set(attrs, "แอตทริบิวต์.ไทย", StringValue("Thai value"))
  Attributes::set(attrs, "विशेषता.हिंदी", StringValue("Hindi value"))
  Attributes::set(attrs, "🚀.emoji.attribute", StringValue("Emoji value"))
  
  // 验证 Unicode 属性名获取
  @assertion.assert_eq(Attributes::get(attrs, "中文属性"), Some(StringValue("Chinese value")))?
  @assertion.assert_eq(Attributes::get(attrs, "日本語属性"), Some(StringValue("Japanese value")))?
  @assertion.assert_eq(Attributes::get(attrs, "한국어속성"), Some(StringValue("Korean value")))?
  @assertion.assert_eq(Attributes::get(attrs, "属性名.العربية"), Some(StringValue("Arabic value")))?
  @assertion.assert_eq(Attributes::get(attrs, "атрибут.русский"), Some(StringValue("Russian value")))?
  @assertion.assert_eq(Attributes::get_attrs, "תכונה.עברית"), Some(StringValue("Hebrew value")))?
  @assertion.assert_eq(Attributes::get(attrs, "แอตทริบิวต์.ไทย"), Some(StringValue("Thai value")))?
  @assertion.assert_eq(Attributes::get(attrs, "विशेषता.हिंदी"), Some(StringValue("Hindi value")))?
  @assertion.assert_eq(Attributes::get(attrs, "🚀.emoji.attribute"), Some(StringValue("Emoji value")))?
}

test "internationalization_unicode_span_operations" {
  // 测试 Unicode Span 操作
  let span_ctx = SpanContext::new("跟踪123", "跨度456", true, "")
  let span = Span::new("测试操作", Server, span_ctx)
  
  // 验证 Unicode Span 名称
  @assertion.assert_eq(Span::name(span), "测试操作")?
  
  // 添加 Unicode 事件
  Span::add_event(span, "中文事件", Some([
    ("中文键", StringValue("中文值")),
    ("日本語キー", StringValue("日本語値")),
    ("한국어키", StringValue("한국어값"))
  ]))
  
  // 设置 Unicode 状态
  Span::set_status(span, Ok, Some("操作成功完成"))
  
  // 验证 Span 属性
  @assertion.assert_eq(Span::name(span), "测试操作")?
  @assertion.assert_eq(Span::kind(span), Server)?
  @assertion.assert_eq(Span::is_recording(span), true)?
}

test "internationalization_unicode_log_messages" {
  // 测试 Unicode 日志消息
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "国际化日志器")
  
  // 创建各种语言的日志记录
  let chinese_log = LogRecord::new(Info, "这是一条中文日志消息")
  let japanese_log = LogRecord::new(Warn, "これは日本語のログメッセージです")
  let korean_log = LogRecord::new(Error, "이것은 한국어 로그 메시지입니다")
  let arabic_log = LogRecord::new(Debug, "هذه رسالة سجل باللغة العربية")
  let russian_log = LogRecord::new(Fatal, "Это сообщение журнала на русском языке")
  let hebrew_log = LogRecord::new(Trace, "זוהי הודעת יומן בעברית")
  let thai_log = LogRecord::new(Info, "นี่คือข้อความบันทึกภาษาไทย")
  let hindi_log = LogRecord::new(Error, "यह हिंदी में लॉग संदेश है")
  let emoji_log = LogRecord::new(Warn, "🚀 🌍 💻 📊 📈")
  
  // 发送日志记录
  Logger::emit(logger, chinese_log)
  Logger::emit(logger, japanese_log)
  Logger::emit(logger, korean_log)
  Logger::emit(logger, arabic_log)
  Logger::emit(logger, russian_log)
  Logger::emit(logger, hebrew_log)
  Logger::emit(logger, thai_log)
  Logger::emit(logger, hindi_log)
  Logger::emit(logger, emoji_log)
  
  // 验证日志记录属性
  @assertion.assert_eq(LogRecord::severity_number(chinese_log), Info)?
  @assertion.assert_eq(LogRecord::body(chinese_log), Some("这是一条中文日志消息"))?
  
  @assertion.assert_eq(LogRecord::severity_number(japanese_log), Warn)?
  @assertion.assert_eq(LogRecord::body(japanese_log), Some("これは日本語のログメッセージです"))?
  
  @assertion.assert_eq(LogRecord::severity_number(korean_log), Error)?
  @assertion.assert_eq(LogRecord::body(korean_log), Some("이것은 한국어 로그 메시지입니다"))?
  
  @assertion.assert_eq(LogRecord::severity_number(arabic_log), Debug)?
  @assertion.assert_eq(LogRecord::body(arabic_log), Some("هذه رسالة سجل باللغة العربية"))?
  
  @assertion.assert_eq(LogRecord::severity_number(russian_log), Fatal)?
  @assertion.assert_eq(LogRecord::body(russian_log), Some("Это сообщение журнала на русском языке"))?
  
  @assertion.assert_eq(LogRecord::severity_number(hebrew_log), Trace)?
  @assertion.assert_eq(LogRecord::body(hebrew_log), Some("זוהי הודעת יומן בעברית"))?
  
  @assertion.assert_eq(LogRecord::severity_number(thai_log), Info)?
  @assertion.assert_eq(LogRecord::body(thai_log), Some("นี่คือข้อความบันทึกภาษาไทย"))?
  
  @assertion.assert_eq(LogRecord::severity_number(hindi_log), Error)?
  @assertion.assert_eq(LogRecord::body(hindi_log), Some("यह हिंदी में लॉग संदेश है"))?
  
  @assertion.assert_eq(LogRecord::severity_number(emoji_log), Warn)?
  @assertion.assert_eq(LogRecord::body(emoji_log), Some("🚀 🌍 💻 📊 📈"))?
}

test "internationalization_unicode_context_operations" {
  // 测试 Unicode 上下文操作
  let ctx = Context::root()
  
  // 创建各种语言的上下文键
  let chinese_key = ContextKey::new("中文键")
  let japanese_key = ContextKey::new("日本語キー")
  let korean_key = ContextKey::new("한국어키")
  let arabic_key = ContextKey::new("مفتاح عربي")
  let russian_key = ContextKey::new("русский ключ")
  let hebrew_key = ContextKey::new("מפתח עברי")
  let thai_key = ContextKey::new("คีย์ภาษาไทย")
  let hindi_key = ContextKey::new("हिंदी कुंजी")
  let emoji_key = ContextKey::new("🚀🔑")
  
  // 设置各种语言的上下文值
  let ctx1 = Context::with_value(ctx, chinese_key, "中文值")
  let ctx2 = Context::with_value(ctx1, japanese_key, "日本語値")
  let ctx3 = Context::with_value(ctx2, korean_key, "한국어값")
  let ctx4 = Context::with_value(ctx3, arabic_key, "قيمة عربية")
  let ctx5 = Context::with_value(ctx4, russian_key, "русское значение")
  let ctx6 = Context::with_value(ctx5, hebrew_key, "ערך עברי")
  let ctx7 = Context::with_value(ctx6, thai_key, "ค่าภาษาไทย")
  let ctx8 = Context::with_value(ctx7, hindi_key, "हिंदी मूल्य")
  let ctx9 = Context::with_value(ctx8, emoji_key, "🚀🌍💻")
  
  // 验证各种语言的上下文值获取
  @assertion.assert_eq(Context::get(ctx9, chinese_key), Some("中文值"))?
  @assertion.assert_eq(Context::get(ctx9, japanese_key), Some("日本語値"))?
  @assertion.assert_eq(Context::get(ctx9, korean_key), Some("한국어값"))?
  @assertion.assert_eq(Context::get(ctx9, arabic_key), Some("قيمة عربية"))?
  @assertion.assert_eq(Context::get(ctx9, russian_key), Some("русское значение"))?
  @assertion.assert_eq(Context::get(ctx9, hebrew_key), Some("ערך עברי"))?
  @assertion.assert_eq(Context::get(ctx9, thai_key), Some("ค่าภาษาไทย"))?
  @assertion.assert_eq(Context::get(ctx9, hindi_key), Some("हिंदी मूल्य"))?
  @assertion.assert_eq(Context::get(ctx9, emoji_key), Some("🚀🌍💻"))?
}

test "internationalization_unicode_baggage_operations" {
  // 测试 Unicode Baggage 操作
  let baggage = Baggage::new()
  
  // 添加各种语言的 Baggage 条目
  let baggage1 = Baggage::set_entry(baggage, "中文键", "中文值")
  let baggage2 = Baggage::set_entry(baggage1, "日本語キー", "日本語値")
  let baggage3 = Baggage::set_entry(baggage2, "한국어키", "한국어값")
  let baggage4 = Baggage::set_entry(baggage3, "مفتاح عربي", "قيمة عربية")
  let baggage5 = Baggage::set_entry(baggage4, "русский ключ", "русское значение")
  let baggage6 = Baggage::set_entry(baggage5, "מפתח עברי", "ערך עברי")
  let baggage7 = Baggage::set_entry(baggage6, "คีย์ภาษาไทย", "ค่าภาษาไทย")
  let baggage8 = Baggage::set_entry(baggage7, "हिंदी कुंजी", "हिंदी मूल्य")
  let baggage9 = Baggage::set_entry(baggage8, "🚀🔑", "🚀🌍💻")
  
  // 验证各种语言的 Baggage 条目获取
  @assertion.assert_eq(Baggage::get_entry(baggage9, "中文键"), Some("中文值"))?
  @assertion.assert_eq(Baggage::get_entry(baggage9, "日本語キー"), Some("日本語値"))?
  @assertion.assert_eq(Baggage::get_entry(baggage9, "한국어키"), Some("한국어값"))?
  @assertion.assert_eq(Baggage::get_entry(baggage9, "مفتاح عربي"), Some("قيمة عربية"))?
  @assertion.assert_eq(Baggage::get_entry(baggage9, "русский ключ"), Some("русское значение"))?
  @assertion.assert_eq(Baggage::get_entry(baggage9, "מפתח עברי"), Some("ערך עברי"))?
  @assertion.assert_eq(Baggage::get_entry(baggage9, "คีย์ภาษาไทย"), Some("ค่าภาษาไทย"))?
  @assertion.assert_eq(Baggage::get_entry(baggage9, "हिंदी कुंजी"), Some("हिंदी मूल्य"))?
  @assertion.assert_eq(Baggage::get_entry(baggage9, "🚀🔑"), Some("🚀🌍💻"))?
}

test "internationalization_unicode_resource_operations" {
  // 测试 Unicode 资源操作
  let resource = Resource::new()
  
  // 创建包含各种语言属性的资源
  let resource_with_unicode = Resource::with_attributes(resource, [
    // 中文属性
    ("服务名称", StringValue("支付服务")),
    ("服务版本", StringValue("1.0.0")),
    ("实例标识", StringValue("实例-001")),
    
    // 日文属性
    ("サービス名", StringValue("決済サービス")),
    ("サービスバージョン", StringValue("1.0.0")),
    ("インスタンスID", StringValue("インスタンス-001")),
    
    // 韩文属性
    ("서비스 이름", StringValue("결제 서비스")),
    ("서비스 버전", StringValue("1.0.0")),
    ("인스턴스 ID", StringValue("인스턴스-001")),
    
    // 阿拉伯文属性
    ("اسم الخدمة", StringValue("خدمة الدفع")),
    ("إصدار الخدمة", StringValue("1.0.0")),
    ("معرف المثيل", StringValue("مثيل-001")),
    
    // 俄文属性
    ("имя службы", StringValue("платежная служба")),
    ("версия службы", StringValue("1.0.0")),
    ("идентификатор экземпляра", StringValue("экземпляр-001")),
    
    // 希伯来文属性
    ("שם השירות", StringValue("שירות תשלומים")),
    ("גרסת שירות", StringValue("1.0.0")),
    ("מזהה מופע", StringValue("מופע-001")),
    
    // 泰文属性
    ("ชื่อบริการ", StringValue("บริการชำระเงิน")),
    ("เวอร์ชันบริการ", StringValue("1.0.0")),
    ("รหัสอินสแตนซ์", StringValue("อินสแตนซ์-001")),
    
    // 印地文属性
    ("सेवा का नाम", StringValue("भुगतान सेवा")),
    ("सेवा संस्करण", StringValue("1.0.0")),
    ("इंस्टेंस आईडी", StringValue("इंस्टेंस-001")),
    
    // Emoji 属性
    ("🚀.service.name", StringValue("🚀 Payment Service")),
    ("🌍.service.region", StringValue("🌍 Global")),
    ("💻.service.platform", StringValue("💻 Multi-Platform"))
  ])
  
  // 验证各种语言的资源属性获取
  @assertion.assert_eq(Resource::get_attribute(resource_with_unicode, "服务名称"), Some(StringValue("支付服务")))?
  @assertion.assert_eq(Resource::get_attribute(resource_with_unicode, "サービス名"), Some(StringValue("決済サービス")))?
  @assertion.assert_eq(Resource::get_attribute(resource_with_unicode, "서비스 이름"), Some(StringValue("결제 서비스")))?
  @assertion.assert_eq(Resource::get_attribute(resource_with_unicode, "اسم الخدمة"), Some(StringValue("خدمة الدفع")))?
  @assertion.assert_eq(Resource::get_attribute(resource_with_unicode, "имя службы"), Some(StringValue("платежная служба")))?
  @assertion.assert_eq(Resource::get_attribute(resource_with_unicode, "שם השירות"), Some(StringValue("שירות תשלומים")))?
  @assertion.assert_eq(Resource::get_attribute(resource_with_unicode, "ชื่อบริการ"), Some(StringValue("บริการชำระเงิน")))?
  @assertion.assert_eq(Resource::get_attribute(resource_with_unicode, "सेवा का नाम"), Some(StringValue("भुगतान सेवा")))?
  @assertion.assert_eq(Resource::get_attribute(resource_with_unicode, "🚀.service.name"), Some(StringValue("🚀 Payment Service")))?
}

test "internationalization_unicode_metrics_operations" {
  // 测试 Unicode 指标操作
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "国际化仪表器")
  
  // 创建各种语言的指标
  let chinese_counter = Meter::create_counter(meter, "中文计数器")
  let japanese_counter = Meter::create_counter(meter, "日本語カウンター")
  let korean_counter = Meter::create_counter(meter, "한국어 카운터")
  let arabic_counter = Meter::create_counter(meter, "العداد العربي")
  let russian_counter = Meter::create_counter(meter, "русский счетчик")
  let hebrew_counter = Meter::create_counter(meter, "מונה עברי")
  let thai_counter = Meter::create_counter(meter, "ตัวนับภาษาไทย")
  let hindi_counter = Meter::create_counter(meter, "हिंदी काउंटर")
  let emoji_counter = Meter::create_counter(meter, "🚀📊カウンター")
  
  // 记录指标值
  Counter::add(chinese_counter, 1.0)
  Counter::add(japanese_counter, 2.0)
  Counter::add(korean_counter, 3.0)
  Counter::add(arabic_counter, 4.0)
  Counter::add(russian_counter, 5.0)
  Counter::add(hebrew_counter, 6.0)
  Counter::add(thai_counter, 7.0)
  Counter::add(hindi_counter, 8.0)
  Counter::add(emoji_counter, 9.0)
  
  // 验证指标名称
  @assertion.assert_eq(Instrument::name(Counter("中文计数器", None, None)), "中文计数器")?
  @assertion.assert_eq(Instrument::name(Counter("日本語カウンター", None, None)), "日本語カウンター")?
  @assertion.assert_eq(Instrument::name(Counter("한국어 카운터", None, None)), "한국어 카운터")?
  @assertion.assert_eq(Instrument::name(Counter("العداد العربي", None, None)), "العداد العربي")?
  @assertion.assert_eq(Instrument::name(Counter("русский счетчик", None, None)), "русский счетчик")?
  @assertion.assert_eq(Instrument::name(Counter("מונה עברי", None, None)), "מונה עברי")?
  @assertion.assert_eq(Instrument::name(Counter("ตัวนับภาษาไทย", None, None)), "ตัวนับภาษาไทย")?
  @assertion.assert_eq(Instrument::name(Counter("हिंदी काउंटर", None, None)), "हिंदी काउंटर")?
  @assertion.assert_eq(Instrument::name(Counter("🚀📊カウンター", None, None)), "🚀📊カウンター")?
}

test "internationalization_unicode_propagation_operations" {
  // 测试 Unicode 传播操作
  let carrier = TextMapCarrier::new()
  let ctx = Context::root()
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // 创建包含 Unicode 值的上下文
  let chinese_key = ContextKey::new("中文键")
  let ctx_with_chinese = Context::with_value(ctx, chinese_key, "中文值")
  
  // 注入和提取上下文
  CompositePropagator::inject(propagator, ctx_with_chinese, carrier)
  let extracted_ctx = CompositePropagator::extract(propagator, carrier)
  
  // 验证提取的上下文
  let extract_key = ContextKey::new("extracted")
  @assertion.assert_eq(Context::get(extracted_ctx, extract_key), Some("true"))?
  
  // 验证载体中的 traceparent
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  @assertion.assert_eq(traceparent.is_some(), true)?
  
  // 测试 Unicode 载体操作
  TextMapCarrier::set(carrier, "中文头", "中文值")
  TextMapCarrier::set(carrier, "日本語ヘッダー", "日本語値")
  TextMapCarrier::set(carrier, "한국어 헤더", "한국어값")
  TextMapCarrier::set(carrier, "رأس عربي", "قيمة عربية")
  TextMapCarrier::set(carrier, "русский заголовок", "русское значение")
  TextMapCarrier::set(carrier, "כותרת עברית", "ערך עברי")
  TextMapCarrier::set(carrier, "ส่วนหัวภาษาไทย", "ค่าภาษาไทย")
  TextMapCarrier::set(carrier, "हिंदी हेडर", "हिंदी मूल्य")
  TextMapCarrier::set(carrier, "🚀.header", "🚀🌍💻")
  
  // 验证 Unicode 载体值获取
  @assertion.assert_eq(TextMapCarrier::get(carrier, "中文头"), Some("中文值"))?
  @assertion.assert_eq(TextMapCarrier::get(carrier, "日本語ヘッダー"), Some("日本語値"))?
  @assertion.assert_eq(TextMapCarrier::get(carrier, "한국어 헤더"), Some("한국어값"))?
  @assertion.assert_eq(TextMapCarrier::get(carrier, "رأس عربي"), Some("قيمة عربية"))?
  @assertion.assert_eq(TextMapCarrier::get(carrier, "русский заголовок"), Some("русское значение"))?
  @assertion.assert_eq(TextMapCarrier::get(carrier, "כותרת עברית"), Some("ערך עברי"))?
  @assertion.assert_eq(TextMapCarrier::get(carrier, "ส่วนหัวภาษาไทย"), Some("ค่าภาษาไทย"))?
  @assertion.assert_eq(TextMapCarrier::get(carrier, "हिंदी हेडर"), Some("हिंदी मूल्य"))?
  @assertion.assert_eq(TextMapCarrier::get(carrier, "🚀.header"), Some("🚀🌍💻"))?
}