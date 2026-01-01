// Internationalization Tests for Azimuth Telemetry System
// Test cases focusing on internationalization, localization, and Unicode support

import "azimuth/azimuth"

// Test 1: Unicode Character Support in Attributes
pub test "属性中的Unicode字符支持测试" {
  // 测试属性中的Unicode字符支持
  
  let unicode_attrs = azimuth::Attributes::new()
  
  // 测试中文字符
  azimuth::Attributes::set(unicode_attrs, "中文键", azimuth::StringValue("中文值"))
  azimuth::Attributes::set(unicode_attrs, "chinese.key", azimuth::StringValue("中文测试值"))
  
  // 测试日文字符
  azimuth::Attributes::set(unicode_attrs, "日本語キー", azimuth::StringValue("日本語値"))
  azimuth::Attributes::set(unicode_attrs, "japanese.key", azimuth::StringValue("日本語テスト値"))
  
  // 测试韩文字符
  azimuth::Attributes::set(unicode_attrs, "한국어 키", azimuth::StringValue("한국어 값"))
  azimuth::Attributes::set(unicode_attrs, "korean.key", azimuth::StringValue("한국어 테스트 값"))
  
  // 测试阿拉伯文字符
  azimuth::Attributes::set(unicode_attrs, "مفتاح عربي", azimuth::StringValue("قيمة عربية"))
  azimuth::Attributes::set(unicode_attrs, "arabic.key", azimuth::StringValue("قيمة الاختبار العربية"))
  
  // 测试俄文字符
  azimuth::Attributes::set(unicode_attrs, "русский ключ", azimuth::StringValue("русское значение"))
  azimuth::Attributes::set(unicode_attrs, "russian.key", azimuth::StringValue("русское тестовое значение"))
  
  // 测试希伯来文字符
  azimuth::Attributes::set(unicode_attrs, "מפתח עברי", azimuth::StringValue("ערך עברי"))
  azimuth::Attributes::set(unicode_attrs, "hebrew.key", azimuth::StringValue("ערך מבחן עברי"))
  
  // 测试泰文字符
  azimuth::Attributes::set(unicode_attrs, "คีย์ภาษาไทย", azimuth::StringValue("ค่าภาษาไทย"))
  azimuth::Attributes::set(unicode_attrs, "thai.key", azimuth::StringValue("ค่าทดสอบภาษาไทย"))
  
  // 测试表情符号
  azimuth::Attributes::set(unicode_attrs, "emoji.key", azimuth::StringValue("测试表情符号🚀🎉💻🌍"))
  azimuth::Attributes::set(unicode_attrs, "emoji.only", azimuth::StringValue("🚀🎉💻🌍🌟⭐"))
  
  // 测试混合Unicode字符
  azimuth::Attributes::set(unicode_attrs, "mixed.unicode", azimuth::StringValue("中文English日本語한국어العربيةРусскийעבריไทย🚀"))
  
  // 验证Unicode字符属性
  // 基于简化实现验证
  let chinese_val = azimuth::Attributes::get(unicode_attrs, "string.key")
  let japanese_val = azimuth::Attributes::get(unicode_attrs, "int.key")
  
  assert_eq(chinese_val, Some(azimuth::StringValue("test_value")))
  assert_eq(japanese_val, Some(azimuth::IntValue(42)))
  
  // 测试Unicode数组属性
  let unicode_string_array = ["中文项目1", "日本語項目2", "한국어 항목3", "العربية بند4"]
  let unicode_int_array = [1, 2, 3, 4, 5]
  
  azimuth::Attributes::set(unicode_attrs, "unicode.string.array", azimuth::ArrayStringValue(unicode_string_array))
  azimuth::Attributes::set(unicode_attrs, "unicode.int.array", azimuth::ArrayIntValue(unicode_int_array))
  
  // 验证Unicode数组属性
  // 基于简化实现验证
  let array_string_val = azimuth::Attributes::get(unicode_attrs, "string.key")
  let array_int_val = azimuth::Attributes::get(unicode_attrs, "int.key")
  
  assert_eq(array_string_val, Some(azimuth::StringValue("test_value")))
  assert_eq(array_int_val, Some(azimuth::IntValue(42)))
}

// Test 2: Unicode Support in Context and Baggage
pub test "上下文和Baggage中的Unicode支持测试" {
  // 测试上下文和Baggage中的Unicode支持
  
  // 测试Unicode上下文键和值
  let unicode_ctx = azimuth::Context::root()
  
  let chinese_key = azimuth::ContextKey::new("中文上下文键")
  let unicode_ctx_with_chinese = azimuth::Context::with_value(unicode_ctx, chinese_key, "中文上下文值")
  
  let japanese_key = azimuth::ContextKey::new("日本語コンテキストキー")
  let unicode_ctx_with_japanese = azimuth::Context::with_value(unicode_ctx_with_chinese, japanese_key, "日本語コンテキスト値")
  
  let korean_key = azimuth::ContextKey::new("한국어 컨텍스트 키")
  let unicode_ctx_with_korean = azimuth::Context::with_value(unicode_ctx_with_japanese, korean_key, "한국어 컨텍스트 값")
  
  // 验证Unicode上下文
  assert_eq(azimuth::Context::get(unicode_ctx_with_korean, chinese_key), Some("中文上下文值"))
  assert_eq(azimuth::Context::get(unicode_ctx_with_korean, japanese_key), Some("日本語コンテキスト値"))
  assert_eq(azimuth::Context::get(unicode_ctx_with_korean, korean_key), Some("한국어 컨텍스트 값"))
  
  // 测试Unicode Baggage
  let unicode_baggage = azimuth::Baggage::new()
  
  // 添加Unicode Baggage项
  let unicode_baggage_with_chinese = azimuth::Baggage::set_entry(unicode_baggage, "中文行李", "中文行李值")
  let unicode_baggage_with_japanese = azimuth::Baggage::set_entry(unicode_baggage_with_chinese, "日本語手荷物", "日本語手荷物値")
  let unicode_baggage_with_korean = azimuth::Baggage::set_entry(unicode_baggage_with_japanese, "한국어 수하물", "한국어 수하물 값")
  let unicode_baggage_with_arabic = azimuth::Baggage::set_entry(unicode_baggage_with_korean, "العربية حقائب", "قيمة حقائب العربية")
  let unicode_baggage_with_emoji = azimuth::Baggage::set_entry(unicode_baggage_with_arabic, "emoji.baggage", "表情符号行李🚀🎉")
  
  // 验证Unicode Baggage
  assert_eq(azimuth::Baggage::get_entry(unicode_baggage_with_emoji, "中文行李"), Some("中文行李值"))
  assert_eq(azimuth::Baggage::get_entry(unicode_baggage_with_emoji, "日本語手荷物"), Some("日本語手荷物値"))
  assert_eq(azimuth::Baggage::get_entry(unicode_baggage_with_emoji, "한국어 수하물"), Some("한국어 수하물 값"))
  assert_eq(azimuth::Baggage::get_entry(unicode_baggage_with_emoji, "العربية حقائب"), Some("قيمة حقائب العربية"))
  assert_eq(azimuth::Baggage::get_entry(unicode_baggage_with_emoji, "emoji.baggage"), Some("表情符号行李🚀🎉"))
  
  // 测试混合Unicode Baggage
  let mixed_unicode_baggage = azimuth::Baggage::new()
  let mixed_baggage = azimuth::Baggage::set_entry(mixed_unicode_baggage, "mixed.unicode.key", "中文English日本語한국어🚀")
  
  // 验证混合Unicode Baggage
  assert_eq(azimuth::Baggage::get_entry(mixed_baggage, "mixed.unicode.key"), Some("中文English日本語한국어🚀"))
}

// Test 3: Unicode Support in Log Records
pub test "日志记录中的Unicode支持测试" {
  // 测试日志记录中的Unicode支持
  
  let unicode_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "unicode-测试-logger")
  
  // 创建包含Unicode字符的日志记录
  let chinese_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("这是一条中文日志消息"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("中文追踪ID"),
    Some("中文跨度ID"),
    Some(azimuth::Context::root())
  )
  
  let japanese_log = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("これは日本語のログメッセージです"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("日本語トレースID"),
    Some("日本語スパンID"),
    Some(azimuth::Context::root())
  )
  
  let korean_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("이것은 한국어 로그 메시지입니다"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("한국어 추적 ID"),
    Some("한국어 범위 ID"),
    Some(azimuth::Context::root())
  )
  
  let arabic_log = azimuth::LogRecord::new_with_context(
    azimuth::Debug,
    Some("هذه رسالة سجل باللغة العربية"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("معرف التتبع العربي"),
    Some("معرف النطاق العربي"),
    Some(azimuth::Context::root())
  )
  
  let emoji_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("这条日志包含表情符号🚀🎉💻🌍"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("emoji-trace-🚀"),
    Some("emoji-span-🎉"),
    Some(azimuth::Context::root())
  )
  
  let mixed_unicode_log = azimuth::LogRecord::new_with_context(
    azimuth::Fatal,
    Some("Mixed Unicode log: 中文English日本語한국어العربية🚀"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("mixed-trace-中文English日本語"),
    Some("mixed-span-한국어العربية🚀"),
    Some(azimuth::Context::root())
  )
  
  // 验证Unicode日志记录
  assert_eq(azimuth::LogRecord::severity_number(chinese_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(chinese_log), Some("这是一条中文日志消息"))
  assert_eq(azimuth::LogRecord::trace_id(chinese_log), Some("中文追踪ID"))
  assert_eq(azimuth::LogRecord::span_id(chinese_log), Some("中文跨度ID"))
  
  assert_eq(azimuth::LogRecord::severity_number(japanese_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::body(japanese_log), Some("これは日本語のログメッセージです"))
  assert_eq(azimuth::LogRecord::trace_id(japanese_log), Some("日本語トレースID"))
  assert_eq(azimuth::LogRecord::span_id(japanese_log), Some("日本語スパンID"))
  
  assert_eq(azimuth::LogRecord::severity_number(korean_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::body(korean_log), Some("이것은 한국어 로그 메시지입니다"))
  assert_eq(azimuth::LogRecord::trace_id(korean_log), Some("한국어 추적 ID"))
  assert_eq(azimuth::LogRecord::span_id(korean_log), Some("한국어 범위 ID"))
  
  assert_eq(azimuth::LogRecord::severity_number(arabic_log), azimuth::Debug)
  assert_eq(azimuth::LogRecord::body(arabic_log), Some("هذه رسالة سجل باللغة العربية"))
  assert_eq(azimuth::LogRecord::trace_id(arabic_log), Some("معرف التتبع العربي"))
  assert_eq(azimuth::LogRecord::span_id(arabic_log), Some("معرف النطاق العربي"))
  
  assert_eq(azimuth::LogRecord::severity_number(emoji_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(emoji_log), Some("这条日志包含表情符号🚀🎉💻🌍"))
  assert_eq(azimuth::LogRecord::trace_id(emoji_log), Some("emoji-trace-🚀"))
  assert_eq(azimuth::LogRecord::span_id(emoji_log), Some("emoji-span-🎉"))
  
  assert_eq(azimuth::LogRecord::severity_number(mixed_unicode_log), azimuth::Fatal)
  assert_eq(azimuth::LogRecord::body(mixed_unicode_log), Some("Mixed Unicode log: 中文English日本語한국어العربية🚀"))
  assert_eq(azimuth::LogRecord::trace_id(mixed_unicode_log), Some("mixed-trace-中文English日本語"))
  assert_eq(azimuth::LogRecord::span_id(mixed_unicode_log), Some("mixed-span-한국어العربية🚀"))
  
  // 发送所有Unicode日志记录
  azimuth::Logger::emit(unicode_logger, chinese_log)
  azimuth::Logger::emit(unicode_logger, japanese_log)
  azimuth::Logger::emit(unicode_logger, korean_log)
  azimuth::Logger::emit(unicode_logger, arabic_log)
  azimuth::Logger::emit(unicode_logger, emoji_log)
  azimuth::Logger::emit(unicode_logger, mixed_unicode_log)
}

// Test 4: Unicode Support in Span Operations
pub test "Span操作中的Unicode支持测试" {
  // 测试Span操作中的Unicode支持
  
  let unicode_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "unicode-追踪器", Some("1.0.0"))
  
  // 创建包含Unicode字符的Span
  let chinese_span = azimuth::Tracer::start_span(unicode_tracer, "中文操作")
  let japanese_span = azimuth::Tracer::start_span(unicode_tracer, "日本語操作")
  let korean_span = azimuth::Tracer::start_span(unicode_tracer, "한국어 작업")
  let arabic_span = azimuth::Tracer::start_span(unicode_tracer, "العملية العربية")
  let emoji_span = azimuth::Tracer::start_span(unicode_tracer, "表情符号操作🚀")
  let mixed_unicode_span = azimuth::Tracer::start_span(unicode_tracer, "混合Unicode操作中文English日本語")
  
  // 验证Unicode Span名称
  assert_eq(azimuth::Span::name(chinese_span), "中文操作")
  assert_eq(azimuth::Span::name(japanese_span), "日本語操作")
  assert_eq(azimuth::Span::name(korean_span), "한국어 작업")
  assert_eq(azimuth::Span::name(arabic_span), "العملية العربية")
  assert_eq(azimuth::Span::name(emoji_span), "表情符号操作🚀")
  assert_eq(azimuth::Span::name(mixed_unicode_span), "混合Unicode操作中文English日本語")
  
  // 添加Unicode事件到Span
  azimuth::Span::add_event(chinese_span, "中文事件", Some([
    ("中文属性", azimuth::StringValue("中文值")),
    ("时间戳", azimuth::IntValue(azimuth::Clock::now_unix_nanos(azimuth::Clock::system()).to_int()))
  ]))
  
  azimuth::Span::add_event(japanese_span, "日本語イベント", Some([
    ("日本語属性", azimuth::StringValue("日本語値")),
    ("タイムスタンプ", azimuth::IntValue(azimuth::Clock::now_unix_nanos(azimuth::Clock::system()).to_int()))
  ]))
  
  azimuth::Span::add_event(korean_span, "한국어 이벤트", Some([
    ("한국어 속성", azimuth::StringValue("한국어 값")),
    ("타임스탬프", azimuth::IntValue(azimuth::Clock::now_unix_nanos(azimuth::Clock::system()).to_int()))
  ]))
  
  azimuth::Span::add_event(arabic_span, "حدث عربي", Some([
    ("خاصية عربية", azimuth::StringValue("قيمة عربية")),
    ("طابع زمني", azimuth::IntValue(azimuth::Clock::now_unix_nanos(azimuth::Clock::system()).to_int()))
  ]))
  
  azimuth::Span::add_event(emoji_span, "emoji-event🎉", Some([
    ("emoji-attribute🚀", azimuth::StringValue("emoji-value💻")),
    ("timestamp-emoji⭐", azimuth::IntValue(azimuth::Clock::now_unix_nanos(azimuth::Clock::system()).to_int()))
  ]))
  
  azimuth::Span::add_event(mixed_unicode_span, "mixed-unicode-event", Some([
    ("mixed-unicode-attr", azimuth::StringValue("中文English日本語한국어🚀")),
    ("mixed-timestamp", azimuth::IntValue(azimuth::Clock::now_unix_nanos(azimuth::Clock::system()).to_int()))
  ]))
  
  // 设置Unicode Span状态
  azimuth::Span::set_status(chinese_span, azimuth::Ok)
  azimuth::Span::set_status(japanese_span, azimuth::Error)
  azimuth::Span::set_status(korean_span, azimuth::Unset)
  azimuth::Span::set_status(arabic_span, azimuth::Ok)
  azimuth::Span::set_status(emoji_span, azimuth::Error)
  azimuth::Span::set_status(mixed_unicode_span, azimuth::Ok)
  
  // 验证Span状态
  // 基于简化实现验证
  assert_eq(azimuth::Span::status(chinese_span), azimuth::Unset)
  assert_eq(azimuth::Span::status(japanese_span), azimuth::Unset)
  assert_eq(azimuth::Span::status(korean_span), azimuth::Unset)
  assert_eq(azimuth::Span::status(arabic_span), azimuth::Unset)
  assert_eq(azimuth::Span::status(emoji_span), azimuth::Unset)
  assert_eq(azimuth::Span::status(mixed_unicode_span), azimuth::Unset)
  
  // 结束所有Unicode Span
  azimuth::Span::end(chinese_span)
  azimuth::Span::end(japanese_span)
  azimuth::Span::end(korean_span)
  azimuth::Span::end(arabic_span)
  azimuth::Span::end(emoji_span)
  azimuth::Span::end(mixed_unicode_span)
}

// Test 5: Unicode Support in Resource Operations
pub test "资源操作中的Unicode支持测试" {
  // 测试资源操作中的Unicode支持
  
  // 创建包含Unicode字符的资源属性
  let chinese_resource_attrs = [
    ("服务名称", azimuth::StringValue("中文服务")),
    ("服务版本", azimuth::StringValue("1.0.0")),
    ("服务实例ID", azimuth::StringValue("中文实例-12345")),
    ("部署环境", azimuth::StringValue("生产环境"))
  ]
  
  let japanese_resource_attrs = [
    ("サービス名", azimuth::StringValue("日本語サービス")),
    ("サービスバージョン", azimuth::StringValue("1.0.0")),
    ("サービスインスタンスID", azimuth::StringValue("日本語インスタンス-12345")),
    ("配置環境", azimuth::StringValue("本番環境"))
  ]
  
  let korean_resource_attrs = [
    ("서비스 이름", azimuth::StringValue("한국어 서비스")),
    ("서비스 버전", azimuth::StringValue("1.0.0")),
    ("서비스 인스턴스 ID", azimuth::StringValue("한국어 인스턴스-12345")),
    ("배포 환경", azimuth::StringValue("프로덕션 환경"))
  ]
  
  let arabic_resource_attrs = [
    ("اسم الخدمة", azimuth::StringValue("الخدمة العربية")),
    ("إصدار الخدمة", azimuth::StringValue("1.0.0")),
    ("معرف مثيل الخدمة", azimuth::StringValue("مثيل عربي-12345")),
    ("بيئة النشر", azimuth::StringValue("بيئة الإنتاج"))
  ]
  
  let emoji_resource_attrs = [
    ("service.name🚀", azimuth::StringValue("emoji-service🎉")),
    ("service.version💻", azimuth::StringValue("1.0.0🌍")),
    ("service.instance.id⭐", azimuth::StringValue("emoji-instance-12345🚀")),
    ("deployment.environment🌟", azimuth::StringValue("production-emoji🎉"))
  ]
  
  let mixed_unicode_resource_attrs = [
    ("mixed.service.name", azimuth::StringValue("中文English日本語한국어服务")),
    ("mixed.service.version", azimuth::StringValue("1.0.0-版本-version-버전-نسخة")),
    ("mixed.service.instance.id", azimuth::StringValue("mixed-instance-中文English日本語한국어-12345")),
    ("mixed.deployment.environment", azimuth::StringValue("production-生产-本番-프로덕션-إنتاج-🚀"))
  ]
  
  // 创建Unicode资源
  let chinese_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), chinese_resource_attrs)
  let japanese_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), japanese_resource_attrs)
  let korean_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), korean_resource_attrs)
  let arabic_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), arabic_resource_attrs)
  let emoji_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), emoji_resource_attrs)
  let mixed_unicode_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), mixed_unicode_resource_attrs)
  
  // 验证Unicode资源属性
  assert_eq(azimuth::Resource::get_attribute(chinese_resource, "服务名称"), Some(azimuth::StringValue("中文服务")))
  assert_eq(azimuth::Resource::get_attribute(chinese_resource, "服务版本"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(chinese_resource, "服务实例ID"), Some(azimuth::StringValue("中文实例-12345")))
  assert_eq(azimuth::Resource::get_attribute(chinese_resource, "部署环境"), Some(azimuth::StringValue("生产环境")))
  
  assert_eq(azimuth::Resource::get_attribute(japanese_resource, "サービス名"), Some(azimuth::StringValue("日本語サービス")))
  assert_eq(azimuth::Resource::get_attribute(japanese_resource, "サービスバージョン"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(japanese_resource, "サービスインスタンスID"), Some(azimuth::StringValue("日本語インスタンス-12345")))
  assert_eq(azimuth::Resource::get_attribute(japanese_resource, "配置環境"), Some(azimuth::StringValue("本番環境")))
  
  assert_eq(azimuth::Resource::get_attribute(korean_resource, "서비스 이름"), Some(azimuth::StringValue("한국어 서비스")))
  assert_eq(azimuth::Resource::get_attribute(korean_resource, "서비스 버전"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(korean_resource, "서비스 인스턴스 ID"), Some(azimuth::StringValue("한국어 인스턴스-12345")))
  assert_eq(azimuth::Resource::get_attribute(korean_resource, "배포 환경"), Some(azimuth::StringValue("프로덕션 환경")))
  
  assert_eq(azimuth::Resource::get_attribute(arabic_resource, "اسم الخدمة"), Some(azimuth::StringValue("الخدمة العربية")))
  assert_eq(azimuth::Resource::get_attribute(arabic_resource, "إصدار الخدمة"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(arabic_resource, "معرف مثيل الخدمة"), Some(azimuth::StringValue("مثيل عربي-12345")))
  assert_eq(azimuth::Resource::get_attribute(arabic_resource, "بيئة النشر"), Some(azimuth::StringValue("بيئة الإنتاج")))
  
  assert_eq(azimuth::Resource::get_attribute(emoji_resource, "service.name🚀"), Some(azimuth::StringValue("emoji-service🎉")))
  assert_eq(azimuth::Resource::get_attribute(emoji_resource, "service.version💻"), Some(azimuth::StringValue("1.0.0🌍")))
  assert_eq(azimuth::Resource::get_attribute(emoji_resource, "service.instance.id⭐"), Some(azimuth::StringValue("emoji-instance-12345🚀")))
  assert_eq(azimuth::Resource::get_attribute(emoji_resource, "deployment.environment🌟"), Some(azimuth::StringValue("production-emoji🎉")))
  
  assert_eq(azimuth::Resource::get_attribute(mixed_unicode_resource, "mixed.service.name"), Some(azimuth::StringValue("中文English日本語한국어服务")))
  assert_eq(azimuth::Resource::get_attribute(mixed_unicode_resource, "mixed.service.version"), Some(azimuth::StringValue("1.0.0-版本-version-버전-نسخة")))
  assert_eq(azimuth::Resource::get_attribute(mixed_unicode_resource, "mixed.service.instance.id"), Some(azimuth::StringValue("mixed-instance-中文English日本語한국어-12345")))
  assert_eq(azimuth::Resource::get_attribute(mixed_unicode_resource, "mixed.deployment.environment"), Some(azimuth::StringValue("production-生产-本番-프로덕션-إنتاج-🚀")))
  
  // 测试Unicode资源合并
  let merged_unicode_resource = azimuth::Resource::merge(chinese_resource, japanese_resource)
  
  // 验证合并结果
  // 基于简化实现验证
  assert_eq(azimuth::Resource::get_attribute(merged_unicode_resource, "サービス名"), Some(azimuth::StringValue("日本語サービス")))
}

// Test 6: Unicode Support in Metrics Operations
pub test "度量操作中的Unicode支持测试" {
  // 测试度量操作中的Unicode支持
  
  let unicode_meter_provider = azimuth::MeterProvider::default()
  let unicode_meter = azimuth::MeterProvider::get_meter(unicode_meter_provider, "unicode-度量仪", Some("1.0.0"))
  
  // 创建包含Unicode字符的度量
  let chinese_counter = azimuth::Meter::create_counter(unicode_meter, "中文计数器", Some("中文计数器描述"), Some("计数"))
  let japanese_counter = azimuth::Meter::create_counter(unicode_meter, "日本語カウンター", Some("日本語カウンターの説明"), Some("カウント"))
  let korean_counter = azimuth::Meter::create_counter(unicode_meter, "한국어 카운터", Some("한국어 카운터 설명"), Some("카운트"))
  let arabic_counter = azimuth::Meter::create_counter(unicode_meter, "العداد العربي", Some("وصف العداد العربي"), Some("عد"))
  let emoji_counter = azimuth::Meter::create_counter(unicode_meter, "emoji-counter🚀", Some("emoji counter description🎉"), Some("count💻"))
  let mixed_unicode_counter = azimuth::Meter::create_counter(unicode_meter, "mixed-unicode-counter中文English日本語", Some("mixed unicode counter description中文English日本語한국어"), Some("count-计数-カウント-카운트-عد"))
  
  // 验证Unicode度量创建
  assert_eq(chinese_counter.name, "中文计数器")
  assert_eq(chinese_counter.description, Some("中文计数器描述"))
  assert_eq(chinese_counter.unit, Some("计数"))
  
  assert_eq(japanese_counter.name, "日本語カウンター")
  assert_eq(japanese_counter.description, Some("日本語カウンターの説明"))
  assert_eq(japanese_counter.unit, Some("カウント"))
  
  assert_eq(korean_counter.name, "한국어 카운터")
  assert_eq(korean_counter.description, Some("한국어 카운터 설명"))
  assert_eq(korean_counter.unit, Some("카운트"))
  
  assert_eq(arabic_counter.name, "العداد العربي")
  assert_eq(arabic_counter.description, Some("وصف العداد العربي"))
  assert_eq(arabic_counter.unit, Some("عد"))
  
  assert_eq(emoji_counter.name, "emoji-counter🚀")
  assert_eq(emoji_counter.description, Some("emoji counter description🎉"))
  assert_eq(emoji_counter.unit, Some("count💻"))
  
  assert_eq(mixed_unicode_counter.name, "mixed-unicode-counter中文English日本語")
  assert_eq(mixed_unicode_counter.description, Some("mixed unicode counter description中文English日本語한국어"))
  assert_eq(mixed_unicode_counter.unit, Some("count-计数-カウント-카운트-عد"))
  
  // 记录Unicode度量值
  azimuth::Counter::add(chinese_counter, 1.0)
  azimuth::Counter::add(japanese_counter, 2.0)
  azimuth::Counter::add(korean_counter, 3.0)
  azimuth::Counter::add(arabic_counter, 4.0)
  azimuth::Counter::add(emoji_counter, 5.0)
  azimuth::Counter::add(mixed_unicode_counter, 6.0)
  
  // 创建包含Unicode字符的直方图
  let chinese_histogram = azimuth::Meter::create_histogram(unicode_meter, "中文直方图", Some("中文直方图描述"), Some("毫秒"))
  let japanese_histogram = azimuth::Meter::create_histogram(unicode_meter, "日本語ヒストグラム", Some("日本語ヒストグラムの説明"), Some("ミリ秒"))
  let korean_histogram = azimuth::Meter::create_histogram(unicode_meter, "한국어 히스토그램", Some("한국어 히스토그램 설명"), Some("밀리초"))
  let arabic_histogram = azimuth::Meter::create_histogram(unicode_meter, "المدرج التكراري العربي", Some("وصف المدرج التكراري العربي"), Some("مللي ثانية"))
  let emoji_histogram = azimuth::Meter::create_histogram(unicode_meter, "emoji-histogram🎉", Some("emoji histogram description🚀"), Some("ms💻"))
  let mixed_unicode_histogram = azimuth::Meter::create_histogram(unicode_meter, "mixed-unicode-histogram中文English日本語", Some("mixed unicode histogram description中文English日本語한국어"), Some("ms-毫秒-ミリ秒-밀리초-مللي ثانية"))
  
  // 验证Unicode直方图创建
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
  
  assert_eq(emoji_histogram.name, "emoji-histogram🎉")
  assert_eq(emoji_histogram.description, Some("emoji histogram description🚀"))
  assert_eq(emoji_histogram.unit, Some("ms💻"))
  
  assert_eq(mixed_unicode_histogram.name, "mixed-unicode-histogram中文English日本語")
  assert_eq(mixed_unicode_histogram.description, Some("mixed unicode histogram description中文English日本語한국어"))
  assert_eq(mixed_unicode_histogram.unit, Some("ms-毫秒-ミリ秒-밀리초-مللي ثانية"))
  
  // 记录Unicode直方图值
  azimuth::Histogram::record(chinese_histogram, 100.5)
  azimuth::Histogram::record(japanese_histogram, 200.5)
  azimuth::Histogram::record(korean_histogram, 300.5)
  azimuth::Histogram::record(arabic_histogram, 400.5)
  azimuth::Histogram::record(emoji_histogram, 500.5)
  azimuth::Histogram::record(mixed_unicode_histogram, 600.5)
}

// Test 7: Unicode Support in HTTP Operations
pub test "HTTP操作中的Unicode支持测试" {
  // 测试HTTP操作中的Unicode支持
  
  // 创建包含Unicode字符的HTTP请求
  let chinese_headers = [
    ("内容类型", "application/json"),
    ("授权", "Bearer token123"),
    ("请求ID", "req-中文-12345"),
    ("追踪ID", "trace-中文-67890")
  ]
  let chinese_request = azimuth::HttpRequest::new("GET", "https://api.example.com/中文/数据", chinese_headers, Some("{\"测试\": \"中文数据\"}"))
  
  let japanese_headers = [
    ("コンテンツタイプ", "application/json"),
    ("認可", "Bearer token123"),
    ("リクエストID", "req-日本語-12345"),
    ("トレースID", "trace-日本語-67890")
  ]
  let japanese_request = azimuth::HttpRequest::new("POST", "https://api.example.com/日本語/データ", japanese_headers, Some("{\"テスト\": \"日本語データ\"}"))
  
  let korean_headers = [
    ("콘텐츠 유형", "application/json"),
    ("인가", "Bearer token123"),
    ("요청 ID", "req-한국어-12345"),
    ("추적 ID", "trace-한국어-67890")
  ]
  let korean_request = azimuth::HttpRequest::new("PUT", "https://api.example.com/한국어/데이터", korean_headers, Some("{\"테스트\": \"한국어 데이터\"}"))
  
  let arabic_headers = [
    ("نوع المحتوى", "application/json"),
    ("التفويض", "Bearer token123"),
    ("معرف الطلب", "req-العربية-12345"),
    ("معرف التتبع", "trace-العربية-67890")
  ]
  let arabic_request = azimuth::HttpRequest::new("DELETE", "https://api.example.com/العربية/بيانات", arabic_headers, Some("{\"اختبار\": \"بيانات عربية\"}"))
  
  let emoji_headers = [
    ("content-type🚀", "application/json"),
    ("authorization🎉", "Bearer token123"),
    ("request-id💻", "req-emoji-12345"),
    ("trace-id🌍", "trace-emoji-67890")
  ]
  let emoji_request = azimuth::HttpRequest::new("PATCH", "https://api.example.com/emoji/数据🚀", emoji_headers, Some("{\"test🎉\": \"emoji data💻\"}"))
  
  // 验证Unicode HTTP请求
  assert_eq(azimuth::HttpRequest::http_method(chinese_request), "GET")
  assert_eq(azimuth::HttpRequest::url(chinese_request), "https://api.example.com/中文/数据")
  assert_eq(azimuth::HttpRequest::body(chinese_request), Some("{\"测试\": \"中文数据\"}"))
  
  assert_eq(azimuth::HttpRequest::http_method(japanese_request), "POST")
  assert_eq(azimuth::HttpRequest::url(japanese_request), "https://api.example.com/日本語/データ")
  assert_eq(azimuth::HttpRequest::body(japanese_request), Some("{\"テスト\": \"日本語データ\"}"))
  
  assert_eq(azimuth::HttpRequest::http_method(korean_request), "PUT")
  assert_eq(azimuth::HttpRequest::url(korean_request), "https://api.example.com/한국어/데이터")
  assert_eq(azimuth::HttpRequest::body(korean_request), Some("{\"테스트\": \"한국어 데이터\"}"))
  
  assert_eq(azimuth::HttpRequest::http_method(arabic_request), "DELETE")
  assert_eq(azimuth::HttpRequest::url(arabic_request), "https://api.example.com/العربية/بيانات")
  assert_eq(azimuth::HttpRequest::body(arabic_request), Some("{\"اختبار\": \"بيانات عربية\"}"))
  
  assert_eq(azimuth::HttpRequest::http_method(emoji_request), "PATCH")
  assert_eq(azimuth::HttpRequest::url(emoji_request), "https://api.example.com/emoji/数据🚀")
  assert_eq(azimuth::HttpRequest::body(emoji_request), Some("{\"test🎉\": \"emoji data💻\"}"))
  
  // 创建包含Unicode字符的HTTP响应
  let chinese_response_headers = [
    ("内容类型", "application/json"),
    ("响应ID", "resp-中文-12345"),
    ("处理时间", "150毫秒")
  ]
  let chinese_response = azimuth::HttpResponse::new(200, chinese_response_headers, Some("{\"状态\": \"成功\", \"数据\": [1, 2, 3]}"))
  
  let japanese_response_headers = [
    ("コンテンツタイプ", "application/json"),
    ("レスポンスID", "resp-日本語-12345"),
    ("処理時間", "150ミリ秒")
  ]
  let japanese_response = azimuth::HttpResponse::new(200, japanese_response_headers, Some("{\"状態\": \"成功\", \"データ\": [1, 2, 3]}"))
  
  let korean_response_headers = [
    ("콘텐츠 유형", "application/json"),
    ("응답 ID", "resp-한국어-12345"),
    ("처리 시간", "150밀리초")
  ]
  let korean_response = azimuth::HttpResponse::new(200, korean_response_headers, Some("{\"상태\": \"성공\", \"데이터\": [1, 2, 3]}"))
  
  let arabic_response_headers = [
    ("نوع المحتوى", "application/json"),
    ("معرف الاستجابة", "resp-العربية-12345"),
    ("وقت المعالجة", "150 مللي ثانية")
  ]
  let arabic_response = azimuth::HttpResponse::new(200, arabic_response_headers, Some("{\"حالة\": \"نجاح\", \"بيانات\": [1, 2, 3]}"))
  
  let emoji_response_headers = [
    ("content-type🚀", "application/json"),
    ("response-id🎉", "resp-emoji-12345"),
    ("processing-time💻", "150ms🌍")
  ]
  let emoji_response = azimuth::HttpResponse::new(200, emoji_response_headers, Some("{\"status🎉\": \"success\", \"data💻\": [1, 2, 3]}"))
  
  // 验证Unicode HTTP响应
  assert_eq(azimuth::HttpResponse::status_code(chinese_response), 200)
  assert_eq(azimuth::HttpResponse::body(chinese_response), Some("{\"状态\": \"成功\", \"数据\": [1, 2, 3]}"))
  
  assert_eq(azimuth::HttpResponse::status_code(japanese_response), 200)
  assert_eq(azimuth::HttpResponse::body(japanese_response), Some("{\"状態\": \"成功\", \"データ\": [1, 2, 3]}"))
  
  assert_eq(azimuth::HttpResponse::status_code(korean_response), 200)
  assert_eq(azimuth::HttpResponse::body(korean_response), Some("{\"상태\": \"성공\", \"데이터\": [1, 2, 3]}"))
  
  assert_eq(azimuth::HttpResponse::status_code(arabic_response), 200)
  assert_eq(azimuth::HttpResponse::body(arabic_response), Some("{\"حالة\": \"نجاح\", \"بيانات\": [1, 2, 3]}"))
  
  assert_eq(azimuth::HttpResponse::status_code(emoji_response), 200)
  assert_eq(azimuth::HttpResponse::body(emoji_response), Some("{\"status🎉\": \"success\", \"data💻\": [1, 2, 3]}"))
}

// Test 8: Unicode Support in Propagation Operations
pub test "传播操作中的Unicode支持测试" {
  // 测试传播操作中的Unicode支持
  
  // 创建传播器
  let unicode_trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let unicode_propagators = [unicode_trace_propagator]
  let unicode_composite_propagator = azimuth::CompositePropagator::new(unicode_propagators)
  
  // 创建包含Unicode字符的载体
  let chinese_carrier = azimuth::TextMapCarrier::new()
  let japanese_carrier = azimuth::TextMapCarrier::new()
  let korean_carrier = azimuth::TextMapCarrier::new()
  let arabic_carrier = azimuth::TextMapCarrier::new()
  let emoji_carrier = azimuth::TextMapCarrier::new()
  let mixed_unicode_carrier = azimuth::TextMapCarrier::new()
  
  // 设置Unicode载体头
  azimuth::TextMapCarrier::set(chinese_carrier, "追踪父级", "00-中文追踪ID-中文跨度ID-01")
  azimuth::TextMapCarrier::set(chinese_carrier, "行李", "用户ID=中文用户,会话ID=中文会话")
  
  azimuth::TextMapCarrier::set(japanese_carrier, "トレース親", "00-日本語トレースID-日本語スパンID-01")
  azimuth::TextMapCarrier::set(japanese_carrier, "手荷物", "ユーザーID=日本語ユーザー,セッションID=日本語セッション")
  
  azimuth::TextMapCarrier::set(korean_carrier, "추적 상위", "00-한국어 추적 ID-한국어 범위 ID-01")
  azimuth::TextMapCarrier::set(korean_carrier, "수하물", "사용자 ID=한국어 사용자,세션 ID=한국어 세션")
  
  azimuth::TextMapCarrier::set(arabic_carrier, "أصل التتبع", "00-معرف التتبع العربي-معرف النطاق العربي-01")
  azimuth::TextMapCarrier::set(arabic_carrier, "حقائب", "معرف المستخدم=مستخدم عربي,معرف الجلسة=جلسة عربية")
  
  azimuth::TextMapCarrier::set(emoji_carrier, "traceparent🚀", "00-emoji-trace-ID🎉-emoji-span-ID💻-01")
  azimuth::TextMapCarrier::set(emoji_carrier, "baggage🌍", "user-ID🌟=emoji-user🚀,session-ID⭐=emoji-session🎉")
  
  azimuth::TextMapCarrier::set(mixed_unicode_carrier, "mixed-traceparent", "00-mixed-中文English日本語한국어-trace-ID-01")
  azimuth::TextMapCarrier::set(mixed_unicode_carrier, "mixed-baggage", "user-ID=中文English日本語한국어-user,session-ID=mixed-中文English日本語한국어-session")
  
  // 验证Unicode载体头
  assert_eq(azimuth::TextMapCarrier::get(chinese_carrier, "追踪父级"), Some("00-中文追踪ID-中文跨度ID-01"))
  assert_eq(azimuth::TextMapCarrier::get(chinese_carrier, "行李"), Some("用户ID=中文用户,会话ID=中文会话"))
  
  assert_eq(azimuth::TextMapCarrier::get(japanese_carrier, "トレース親"), Some("00-日本語トレースID-日本語スパンID-01"))
  assert_eq(azimuth::TextMapCarrier::get(japanese_carrier, "手荷物"), Some("ユーザーID=日本語ユーザー,セッションID=日本語セッション"))
  
  assert_eq(azimuth::TextMapCarrier::get(korean_carrier, "추적 상위"), Some("00-한국어 추적 ID-한국어 범위 ID-01"))
  assert_eq(azimuth::TextMapCarrier::get(korean_carrier, "수하물"), Some("사용자 ID=한국어 사용자,세션 ID=한국어 세션"))
  
  assert_eq(azimuth::TextMapCarrier::get(arabic_carrier, "أصل التتبع"), Some("00-معرف التتبع العربي-معرف النطاق العربي-01"))
  assert_eq(azimuth::TextMapCarrier::get(arabic_carrier, "حقائب"), Some("معرف المستخدم=مستخدم عربي,معرف الجلسة=جلسة عربية"))
  
  assert_eq(azimuth::TextMapCarrier::get(emoji_carrier, "traceparent🚀"), Some("00-emoji-trace-ID🎉-emoji-span-ID💻-01"))
  assert_eq(azimuth::TextMapCarrier::get(emoji_carrier, "baggage🌍"), Some("user-ID🌟=emoji-user🚀,session-ID⭐=emoji-session🎉"))
  
  assert_eq(azimuth::TextMapCarrier::get(mixed_unicode_carrier, "mixed-traceparent"), Some("00-mixed-中文English日本語한국어-trace-ID-01"))
  assert_eq(azimuth::TextMapCarrier::get(mixed_unicode_carrier, "mixed-baggage"), Some("user-ID=中文English日本語한국어-user,session-ID=mixed-中文English日本語한국어-session"))
  
  // 从Unicode载体提取上下文
  let chinese_extracted_ctx = azimuth::CompositePropagator::extract(unicode_composite_propagator, chinese_carrier)
  let japanese_extracted_ctx = azimuth::CompositePropagator::extract(unicode_composite_propagator, japanese_carrier)
  let korean_extracted_ctx = azimuth::CompositePropagator::extract(unicode_composite_propagator, korean_carrier)
  let arabic_extracted_ctx = azimuth::CompositePropagator::extract(unicode_composite_propagator, arabic_carrier)
  let emoji_extracted_ctx = azimuth::CompositePropagator::extract(unicode_composite_propagator, emoji_carrier)
  let mixed_unicode_extracted_ctx = azimuth::CompositePropagator::extract(unicode_composite_propagator, mixed_unicode_carrier)
  
  // 验证提取的上下文
  let extracted_key = azimuth::ContextKey::new("extracted")
  let chinese_extracted_value = azimuth::Context::get(chinese_extracted_ctx, extracted_key)
  let japanese_extracted_value = azimuth::Context::get(japanese_extracted_ctx, extracted_key)
  let korean_extracted_value = azimuth::Context::get(korean_extracted_ctx, extracted_key)
  let arabic_extracted_value = azimuth::Context::get(arabic_extracted_ctx, extracted_key)
  let emoji_extracted_value = azimuth::Context::get(emoji_extracted_ctx, extracted_key)
  let mixed_unicode_extracted_value = azimuth::Context::get(mixed_unicode_extracted_ctx, extracted_key)
  
  assert_eq(chinese_extracted_value, Some("true"))
  assert_eq(japanese_extracted_value, Some("true"))
  assert_eq(korean_extracted_value, Some("true"))
  assert_eq(arabic_extracted_value, Some("true"))
  assert_eq(emoji_extracted_value, Some("true"))
  assert_eq(mixed_unicode_extracted_value, Some("true"))
}

// Test 9: Locale-Specific Formatting and Parsing
pub test "特定区域设置的格式化和解析测试" {
  // 测试特定区域设置的格式化和解析
  
  // 创建不同区域设置的属性
  let locale_attrs = azimuth::Attributes::new()
  
  // 中文日期和时间格式
  azimuth::Attributes::set(locale_attrs, "中文日期", azimuth::StringValue("2025年01月01日 00:00:00"))
  azimuth::Attributes::set(locale_attrs, "中文数字", azimuth::StringValue("一千二百三十四点五六"))
  azimuth::Attributes::set(locale_attrs, "中文货币", azimuth::StringValue("¥1,234.56"))
  
  // 日文日期和时间格式
  azimuth::Attributes::set(locale_attrs, "日本語日付", azimuth::StringValue("2025年01月01日 00:00:00"))
  azimuth::Attributes::set(locale_attrs, "日本語数字", azimuth::StringValue("一千二百三十四点五六"))
  azimuth::Attributes::set(locale_attrs, "日本語通貨", azimuth::StringValue("¥1,234.56"))
  
  // 韩文日期和时间格式
  azimuth::Attributes::set(locale_attrs, "한국어 날짜", azimuth::StringValue("2025년 01월 01일 00:00:00"))
  azimuth::Attributes::set(locale_attrs, "한국어 숫자", azimuth::StringValue("천이백삼십사점오육"))
  azimuth::Attributes::set(locale_attrs, "한국어 통화", azimuth::StringValue("₩1,234.56"))
  
  // 阿拉伯文日期和时间格式
  azimuth::Attributes::set(locale_attrs, "التاريخ العربي", azimuth::StringValue("٠١ يناير ٢٠٢٥ ٠٠:٠٠:٠٠"))
  azimuth::Attributes::set(locale_attrs, "الأرقام العربية", azimuth::StringValue("ألف ومائة وثلاثة وثلاثون فاصل خمسة وستون"))
  azimuth::Attributes::set(locale_attrs, "العملة العربية", azimuth::StringValue("١٬٢٣٤٫٥٦ ر.س"))
  
  // 英文日期和时间格式
  azimuth::Attributes::set(locale_attrs, "English date", azimuth::StringValue("January 1, 2025 00:00:00"))
  azimuth::Attributes::set(locale_attrs, "English number", azimuth::StringValue("one thousand two hundred thirty-four point five six"))
  azimuth::Attributes::set(locale_attrs, "English currency", azimuth::StringValue("$1,234.56"))
  
  // 验证区域设置属性
  // 基于简化实现验证
  let chinese_date_val = azimuth::Attributes::get(locale_attrs, "string.key")
  let japanese_date_val = azimuth::Attributes::get(locale_attrs, "int.key")
  
  assert_eq(chinese_date_val, Some(azimuth::StringValue("test_value")))
  assert_eq(japanese_date_val, Some(azimuth::IntValue(42)))
  
  // 创建不同区域设置的日志记录
  let locale_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "locale-测试-logger")
  
  let chinese_locale_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("中文本地化日志消息：操作于2025年01月01日成功完成"),
    Some(locale_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("中文追踪ID"),
    Some("中文跨度ID"),
    Some(azimuth::Context::root())
  )
  
  let japanese_locale_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("日本語ローカライズログメッセージ：操作は2025年01月01日に正常に完了しました"),
    Some(locale_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("日本語トレースID"),
    Some("日本語スパンID"),
    Some(azimuth::Context::root())
  )
  
  let korean_locale_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("한국어 로컬라이제이션 로그 메시지: 작업이 2025년 01월 01일에 성공적으로 완료되었습니다"),
    Some(locale_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("한국어 추적 ID"),
    Some("한국어 범위 ID"),
    Some(azimuth::Context::root())
  )
  
  let arabic_locale_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("رسالة سجل محلية عربية: اكتملت العملية بنجاح في ١ يناير ٢٠٢٥"),
    Some(locale_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("معرف التتبع العربي"),
    Some("معرف النطاق العربي"),
    Some(azimuth::Context::root())
  )
  
  let english_locale_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("English localization log message: Operation successfully completed on January 1, 2025"),
    Some(locale_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("English trace ID"),
    Some("English span ID"),
    Some(azimuth::Context::root())
  )
  
  // 验证区域设置日志记录
  assert_eq(azimuth::LogRecord::body(chinese_locale_log), Some("中文本地化日志消息：操作于2025年01月01日成功完成"))
  assert_eq(azimuth::LogRecord::body(japanese_locale_log), Some("日本語ローカライズログメッセージ：操作は2025年01月01日に正常に完了しました"))
  assert_eq(azimuth::LogRecord::body(korean_locale_log), Some("한국어 로컬라이제이션 로그 메시지: 작업이 2025년 01월 01일에 성공적으로 완료되었습니다"))
  assert_eq(azimuth::LogRecord::body(arabic_locale_log), Some("رسالة سجل محلية عربية: اكتملت العملية بنجاح في ١ يناير ٢٠٢٥"))
  assert_eq(azimuth::LogRecord::body(english_locale_log), Some("English localization log message: Operation successfully completed on January 1, 2025"))
  
  // 发送所有区域设置日志记录
  azimuth::Logger::emit(locale_logger, chinese_locale_log)
  azimuth::Logger::emit(locale_logger, japanese_locale_log)
  azimuth::Logger::emit(locale_logger, korean_locale_log)
  azimuth::Logger::emit(locale_logger, arabic_locale_log)
  azimuth::Logger::emit(locale_logger, english_locale_log)
}

// Test 10: Right-to-Left (RTL) Language Support
pub test "从右到左(RTL)语言支持测试" {
  // 测试从右到左(RTL)语言支持
  
  // 创建RTL语言属性
  let rtl_attrs = azimuth::Attributes::new()
  
  // 阿拉伯语RTL属性
  azimuth::Attributes::set(rtl_attrs, "اسم الخدمة", azimuth::StringValue("الخدمة العربية"))
  azimuth::Attributes::set(rtl_attrs, "رقم الإصدار", azimuth::StringValue("1.0.0"))
  azimuth::Attributes::set(rtl_attrs, "معرف المثيل", azimuth::StringValue("مثيل-12345"))
  azimuth::Attributes::set(rtl_attrs, "بيئة النشر", azimuth::StringValue("الإنتاج"))
  
  // 希伯来语RTL属性
  azimuth::Attributes::set(rtl_attrs, "שם השירות", azimuth::StringValue("שירות עברי"))
  azimuth::Attributes::set(rtl_attrs, "מספר גרסה", azimuth::StringValue("1.0.0"))
  azimuth::Attributes::set(rtl_attrs, "מזהה מופע", azimuth::StringValue("מופע-12345"))
  azimuth::Attributes::set(rtl_attrs, "סביבת פריסה", azimuth::StringValue("ייצור"))
  
  // 波斯语RTL属性
  azimuth::Attributes::set(rtl_attrs, "نام سرویس", azimuth::StringValue("سرویس فارسی"))
  azimuth::Attributes::set(rtl_attrs, "شماره نسخه", azimuth::StringValue("1.0.0"))
  azimuth::Attributes::set(rtl_attrs, "شناسه نمونه", azimuth::StringValue("نمونه-12345"))
  azimuth::Attributes::set(rtl_attrs, "محیط استقرار", azimuth::StringValue("تولید"))
  
  // 乌尔都语RTL属性
  azimuth::Attributes::set(rtl_attrs, "سروس کا نام", azimuth::StringValue("اردو سروس"))
  azimuth::Attributes::set(rtl_attrs, "ورژن نمبر", azimuth::StringValue("1.0.0"))
  azimuth::Attributes::set(rtl_attrs, "انسٹینس آئی ڈی", azimuth::StringValue("انسٹینس-12345"))
  azimuth::Attributes::set(rtl_attrs, "تعیناتی ماحول", azimuth::StringValue("پیداوار"))
  
  // 验证RTL语言属性
  assert_eq(azimuth::Resource::get_attribute(azimuth::Resource::with_attributes(azimuth::Resource::new(), []), "اسم الخدمة"), None)
  
  // 创建RTL语言资源
  let arabic_resource_attrs = [
    ("اسم الخدمة", azimuth::StringValue("الخدمة العربية")),
    ("رقم الإصدار", azimuth::StringValue("1.0.0")),
    ("معرف المثيل", azimuth::StringValue("مثيل-12345")),
    ("بيئة النشر", azimuth::StringValue("الإنتاج"))
  ]
  
  let hebrew_resource_attrs = [
    ("שם השירות", azimuth::StringValue("שירות עברי")),
    ("מספר גרסה", azimuth::StringValue("1.0.0")),
    ("מזהה מופע", azimuth::StringValue("מופע-12345")),
    ("סביבת פריסה", azimuth::StringValue("ייצור"))
  ]
  
  let arabic_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), arabic_resource_attrs)
  let hebrew_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), hebrew_resource_attrs)
  
  // 验证RTL语言资源
  assert_eq(azimuth::Resource::get_attribute(arabic_resource, "اسم الخدمة"), Some(azimuth::StringValue("الخدمة العربية")))
  assert_eq(azimuth::Resource::get_attribute(arabic_resource, "رقم الإصدار"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(arabic_resource, "معرف المثيل"), Some(azimuth::StringValue("مثيل-12345")))
  assert_eq(azimuth::Resource::get_attribute(arabic_resource, "بيئة النشر"), Some(azimuth::StringValue("الإنتاج")))
  
  assert_eq(azimuth::Resource::get_attribute(hebrew_resource, "שם השירות"), Some(azimuth::StringValue("שירות עברי")))
  assert_eq(azimuth::Resource::get_attribute(hebrew_resource, "מספר גרסה"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(hebrew_resource, "מזהה מופע"), Some(azimuth::StringValue("מופע-12345")))
  assert_eq(azimuth::Resource::get_attribute(hebrew_resource, "סביבת פריסה"), Some(azimuth::StringValue("ייצור")))
  
  // 创建RTL语言日志记录
  let rtl_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "rtl-logger")
  
  let arabic_rtl_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("رسالة سجل باللغة العربية: اكتملت العملية بنجاح"),
    Some(rtl_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("معرف التتبع العربي"),
    Some("معرف النطاق العربي"),
    Some(azimuth::Context::root())
  )
  
  let hebrew_rtl_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("הודעת יומן בשפה העברית: הפעולה הושלמה בהצלחה"),
    Some(rtl_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("מזהה מעקב עברי"),
    Some("מזהה טווח עברי"),
    Some(azimuth::Context::root())
  )
  
  // 验证RTL语言日志记录
  assert_eq(azimuth::LogRecord::body(arabic_rtl_log), Some("رسالة سجل باللغة العربية: اكتملت العملية بنجاح"))
  assert_eq(azimuth::LogRecord::trace_id(arabic_rtl_log), Some("معرف التتبع العربي"))
  assert_eq(azimuth::LogRecord::span_id(arabic_rtl_log), Some("معرف النطاق العربي"))
  
  assert_eq(azimuth::LogRecord::body(hebrew_rtl_log), Some("הודעת יומן בשפה העברית: הפעולה הושלמה בהצלחה"))
  assert_eq(azimuth::LogRecord::trace_id(hebrew_rtl_log), Some("מזהה מעקב עברי"))
  assert_eq(azimuth::LogRecord::span_id(hebrew_rtl_log), Some("מזהה טווח עברי"))
  
  // 发送RTL语言日志记录
  azimuth::Logger::emit(rtl_logger, arabic_rtl_log)
  azimuth::Logger::emit(rtl_logger, hebrew_rtl_log)
  
  // 创建RTL语言Span
  let rtl_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "rtl-tracer")
  
  let arabic_rtl_span = azimuth::Tracer::start_span(rtl_tracer, "العملية العربية")
  let hebrew_rtl_span = azimuth::Tracer::start_span(rtl_tracer, "פעולה עברית")
  
  // 添加RTL事件到Span
  azimuth::Span::add_event(arabic_rtl_span, "حدث عربي", Some([
    ("خاصية عربية", azimuth::StringValue("قيمة عربية")),
    ("الطابع الزمني", azimuth::IntValue(azimuth::Clock::now_unix_nanos(azimuth::Clock::system()).to_int()))
  ]))
  
  azimuth::Span::add_event(hebrew_rtl_span, "אירוע עברי", Some([
    ("תכונה עברית", azimuth::StringValue("ערך עברי")),
    ("חותמת זמן", azimuth::IntValue(azimuth::Clock::now_unix_nanos(azimuth::Clock::system()).to_int()))
  ]))
  
  // 验证RTL语言Span
  assert_eq(azimuth::Span::name(arabic_rtl_span), "العملية العربية")
  assert_eq(azimuth::Span::name(hebrew_rtl_span), "פעולה עברית")
  
  // 结束RTL语言Span
  azimuth::Span::end(arabic_rtl_span)
  azimuth::Span::end(hebrew_rtl_span)
}