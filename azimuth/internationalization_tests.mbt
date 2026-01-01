// Internationalization Text Processing Tests for Azimuth Telemetry System
// This file contains test cases for internationalization and text processing

test "chinese text handling in attributes" {
  let attrs = Attributes::new()
  
  // Test Chinese characters in attribute values
  Attributes::set(attrs, "服务名称", StringValue("用户认证服务"))
  Attributes::set(attrs, "服务描述", StringValue("处理用户登录和身份验证"))
  Attributes::set(attrs, "错误信息", StringValue("用户名或密码错误"))
  
  let service_name = Attributes::get(attrs, "服务名称")
  let service_desc = Attributes::get(attrs, "服务描述")
  let error_msg = Attributes::get(attrs, "错误信息")
  
  assert_eq(service_name, Some(StringValue("用户认证服务")))
  assert_eq(service_desc, Some(StringValue("处理用户登录和身份验证")))
  assert_eq(error_msg, Some(StringValue("用户名或密码错误")))
}

test "japanese text handling in log records" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "日本語ロガー")
  
  let record = LogRecord::new(Info, "ユーザーログインが成功しました")
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("ユーザーログインが成功しました"))
  
  Logger::emit(logger, record)
  assert_true(true)
}

test "korean text handling in span names" {
  let span_ctx = SpanContext::new("korean-trace", "korean-span", true, "")
  let span = Span::new("한국어 작업", Internal, span_ctx)
  
  assert_eq(Span::name(span), "한국어 작업")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
}

test "arabic text handling in resource attributes" {
  let attrs = [
    ("اسم الخدمة", StringValue("خدمة المصادقة")),
    ("الوصف", StringValue("معالجة تسجيل الدخول والتحقق من الهوية")),
    ("رسالة الخطأ", StringValue("اسم المستخدم أو كلمة المرور غير صحيحة"))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), attrs)
  
  let service_name = Resource::get_attribute(resource, "اسم الخدمة")
  let description = Resource::get_attribute(resource, "الوصف")
  let error_msg = Resource::get_attribute(resource, "رسالة الخطأ")
  
  assert_eq(service_name, Some(StringValue("خدمة المصادقة")))
  assert_eq(description, Some(StringValue("معالجة تسجيل الدخول والتحقق من الهوية")))
  assert_eq(error_msg, Some(StringValue("اسم المستخدم أو كلمة المرور غير صحيحة")))
}

test "russian text handling in baggage" {
  let baggage = Baggage::new()
  let with_entries = Baggage::set_entry(baggage, "имя_пользователя", "Иван_Петров")
  let final_baggage = Baggage::set_entry(with_entries, "идентификатор_сессии", "сессия_12345")
  
  let username = Baggage::get_entry(final_baggage, "имя_пользователя")
  let session_id = Baggage::get_entry(final_baggage, "идентификатор_сессии")
  
  assert_eq(username, Some("Иван_Петров"))
  assert_eq(session_id, Some("сессия_12345"))
}

test "emoji and special character handling" {
  let attrs = Attributes::new()
  
  // Test emoji in attribute values
  Attributes::set(attrs, "status", StringValue("✅ 成功"))
  Attributes::set(attrs, "error", StringValue("❌ 失败"))
  Attributes::set(attrs, "warning", StringValue("⚠️ 警告"))
  Attributes::set(attrs, "performance", StringValue("📊 性能指标"))
  
  let status = Attributes::get(attrs, "status")
  let error = Attributes::get(attrs, "error")
  let warning = Attributes::get(attrs, "warning")
  let performance = Attributes::get(attrs, "performance")
  
  assert_eq(status, Some(StringValue("✅ 成功")))
  assert_eq(error, Some(StringValue("❌ 失败")))
  assert_eq(warning, Some(StringValue("⚠️ 警告")))
  assert_eq(performance, Some(StringValue("📊 性能指标")))
}

test "mixed language text processing" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "多语言日志记录器")
  
  let record = LogRecord::new_with_context(
    Warn,
    Some("Mixed language log: 用户登录失败 - User login failed - ユーザーログイン失敗"),
    None,
    Some(1735689600000000000L),
    None,
    Some("mixed-language-trace"),
    Some("mixed-language-span"),
    None
  )
  
  assert_eq(LogRecord::severity_number(record), Warn)
  assert_eq(LogRecord::body(record), Some("Mixed language log: 用户登录失败 - User login failed - ユーザーログイン失敗"))
  assert_eq(LogRecord::trace_id(record), Some("mixed-language-trace"))
  assert_eq(LogRecord::span_id(record), Some("mixed-language-span"))
  
  Logger::emit(logger, record)
  assert_true(true)
}

test "right-to-left text handling" {
  let attrs = Attributes::new()
  
  // Test RTL text (Hebrew and Arabic)
  Attributes::set(attrs, "עברית", StringValue("שם השירות"))
  Attributes::set(attrs, "العربية", StringValue("اسم الخدمة"))
  
  let hebrew = Attributes::get(attrs, "עברית")
  let arabic = Attributes::get(attrs, "العربية")
  
  assert_eq(hebrew, Some(StringValue("שם השירות")))
  assert_eq(arabic, Some(StringValue("اسم الخدمة")))
}

test "internationalized metric names and descriptions" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "国际化仪表板")
  
  let counter = Meter::create_counter(
    meter, 
    "http.请求.总数", 
    Some("HTTP请求总数计数器"), 
    Some("请求")
  )
  
  let histogram = Meter::create_histogram(
    meter,
    "响应时间.分布",
    Some("响应时间分布直方图"),
    Some("毫秒")
  )
  
  assert_eq(counter.name, "http.请求.总数")
  assert_eq(counter.description, Some("HTTP请求总数计数器"))
  assert_eq(counter.unit, Some("请求"))
  
  assert_eq(histogram.name, "响应时间.分布")
  assert_eq(histogram.description, Some("响应时间分布直方图"))
  assert_eq(histogram.unit, Some("毫秒"))
}

test "unicode normalization and comparison" {
  let attrs = Attributes::new()
  
  // Test different Unicode representations of the same text
  Attributes::set(attrs, "normalized1", StringValue("café"))  // Using é
  Attributes::set(attrs, "normalized2", StringValue("cafe\u0301"))  // Using e + combining acute accent
  
  let normalized1 = Attributes::get(attrs, "normalized1")
  let normalized2 = Attributes::get(attrs, "normalized2")
  
  // Note: In a real implementation, you might want to normalize Unicode before comparison
  // For this test, we just verify both values are stored as provided
  assert_eq(normalized1, Some(StringValue("café")))
  assert_eq(normalized2, Some(StringValue("cafe\u0301")))
}

test "international text in context propagation" {
  let ctx = Context::root()
  let key = ContextKey::new("国际化键")
  let ctx_with_value = Context::with_value(ctx, key, "国际化值")
  
  let retrieved_value = Context::get(ctx_with_value, key)
  assert_eq(retrieved_value, Some("国际化值"))
  
  // Test with different languages
  let english_key = ContextKey::new("english.key")
  let ctx_english = Context::with_value(ctx, english_key, "English value")
  
  let chinese_key = ContextKey::new("中文键")
  let ctx_chinese = Context::with_value(ctx, chinese_key, "中文值")
  
  let english_value = Context::get(ctx_english, english_key)
  let chinese_value = Context::get(ctx_chinese, chinese_key)
  
  assert_eq(english_value, Some("English value"))
  assert_eq(chinese_value, Some("中文值"))
}