// Internationalization Tests
// This file contains test cases for internationalization and localization support

// Test 1: Unicode and multilingual attribute handling
pub test "unicode_multilingual_attributes" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "i18n-test-service")
  
  let span = azimuth::Tracer::start_span(tracer, "多语言操作测试")
  
  // Test Unicode characters from different languages
  azimuth::Span::set_attribute(span, "user.name.cn", azimuth::StringValue("张三"))
  azimuth::Span::set_attribute(span, "user.name.jp", azimuth::StringValue("田中太郎"))
  azimuth::Span::set_attribute(span, "user.name.kr", azimuth::StringValue("김철수"))
  azimuth::Span::set_attribute(span, "user.name.ar", azimuth::StringValue("أحمد محمد"))
  azimuth::Span::set_attribute(span, "user.name.ru", azimuth::StringValue("Иван Петров"))
  azimuth::Span::set_attribute(span, "user.name.th", azimuth::StringValue("สมชาย ใจดี"))
  azimuth::Span::set_attribute(span, "user.name.he", azimuth::StringValue("דוד כהן"))
  azimuth::Span::set_attribute(span, "user.name.hi", azimuth::StringValue("राम कुमार"))
  
  // Test emoji and special symbols
  azimuth::Span::set_attribute(span, "message.emoji", azimuth::StringValue("🚀🌍💻📱"))
  azimuth::Span::set_attribute(span, "currency.symbols", azimuth::StringValue("$€£¥₹₽₩"))
  azimuth::Span::set_attribute(span, "math.symbols", azimuth::StringValue("∑∏∫∆∇∂"))
  
  // Test mixed language content
  azimuth::Span::set_attribute(span, "mixed.content", azimuth::StringValue("Hello 世界 こんにちは 안녕하세요 مرحبا"))
  
  // Test right-to-left languages
  azimuth::Span::set_attribute(span, "rtl.content", azimuth::StringValue("مرحبا بالعالم"))
  azimuth::Span::set_attribute(span, "rtl.mixed", azimuth::StringValue("Hello مرحبا World"))
  
  // Verify Unicode attributes are correctly stored
  assert_eq(azimuth::Span::get_attribute(span, "user.name.cn"), Some(azimuth::StringValue("张三")))
  assert_eq(azimuth::Span::get_attribute(span, "user.name.jp"), Some(azimuth::StringValue("田中太郎")))
  assert_eq(azimuth::Span::get_attribute(span, "message.emoji"), Some(azimuth::StringValue("🚀🌍💻📱")))
  assert_eq(azimuth::Span::get_attribute(span, "mixed.content"), Some(azimuth::StringValue("Hello 世界 こんにちは 안녕하세요 مرحبا")))
  
  azimuth::Span::set_status_code(span, azimuth::Ok)
  azimuth::Span::end(span)
}

// Test 2: Multilingual logging and error messages
pub test "multilingual_logging_error_messages" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "i18n-logger")
  
  // Create log records in different languages
  let chinese_log = azimuth::LogRecord::new(azimuth::Info, "用户登录成功")
  let chinese_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(chinese_attrs, "用户名", azimuth::StringValue("张三"))
  azimuth::Attributes::set(chinese_attrs, "登录时间", azimuth::StringValue("2023-01-01 10:30:00"))
  let chinese_enriched = azimuth::LogRecord::with_attributes(chinese_log, chinese_attrs)
  azimuth::Logger::emit(logger, chinese_enriched)
  
  let japanese_log = azimuth::LogRecord::new(azimuth::Warn, "データベース接続がタイムアウトしました")
  let japanese_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(japanese_attrs, "データベース", azimuth::StringValue("production_db"))
  azimuth::Attributes::set(japanese_attrs, "タイムアウト時間", azimuth::IntValue(30))
  let japanese_enriched = azimuth::LogRecord::with_attributes(japanese_log, japanese_attrs)
  azimuth::Logger::emit(logger, japanese_enriched)
  
  let korean_log = azimuth::LogRecord::new(azimuth::Error, "사용자 인증에 실패했습니다")
  let korean_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(korean_attrs, "사용자 ID", azimuth::StringValue("user123"))
  azimuth::Attributes::set(korean_attrs, "실패 사유", azimuth::StringValue("잘못된 비밀번호"))
  let korean_enriched = azimuth::LogRecord::with_attributes(korean_log, korean_attrs)
  azimuth::Logger::emit(logger, korean_enriched)
  
  let arabic_log = azimuth::LogRecord::new(azimuth::Fatal, "نفاد الذاكرة في النظام")
  let arabic_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(arabic_attrs, "الذاكرة المتاحة", azimuth::IntValue(128))
  azimuth::Attributes::set(arabic_attrs, "الذاكرة المطلوبة", azimuth::IntValue(1024))
  let arabic_enriched = azimuth::LogRecord::with_attributes(arabic_log, arabic_attrs)
  azimuth::Logger::emit(logger, arabic_enriched)
  
  // Create error messages in different languages
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "i18n-error-service")
  
  let error_span = azimuth::Tracer::start_span(tracer, "多语言错误处理")
  
  // Chinese error
  azimuth::Span::set_attribute(error_span, "错误.类型", azimuth::StringValue("验证错误"))
  azimuth::Span::set_attribute(error_span, "错误.消息", azimuth::StringValue("输入的邮箱格式不正确"))
  
  // Japanese error
  azimuth::Span::set_attribute(error_span, "エラー.タイプ", azimuth::StringValue("検証エラー"))
  azimuth::Span::set_attribute(error_span, "エラー.メッセージ", azimuth::StringValue("入力されたメールアドレスの形式が正しくありません"))
  
  // Korean error
  azimuth::Span::set_attribute(error_span, "오류.유형", azimuth::StringValue("검증 오류"))
  azimuth::Span::set_attribute(error_span, "오류.메시지", azimuth::StringValue("입력된 이메일 형식이 올바르지 않습니다"))
  
  azimuth::Span::set_status_code(error_span, azimuth::Error)
  azimuth::Span::end(error_span)
  
  // Verify logger functionality
  assert_eq(logger.scope.name, "i18n-logger")
}

// Test 3: Locale-specific formatting and data
pub test "locale_specific_formatting" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "locale-test-service")
  
  let span = azimuth::Tracer::start_span(tracer, "Locale-specific Operations")
  
  // Test different locale-specific number formats
  azimuth::Span::set_attribute(span, "number.us", azimuth::StringValue("1,234,567.89"))
  azimuth::Span::set_attribute(span, "number.eu", azimuth::StringValue("1.234.567,89"))
  azimuth::Span::set_attribute(span, "number.in", azimuth::StringValue("12,34,567.89"))
  azimuth::Span::set_attribute(span, "number.cn", azimuth::StringValue("1,234,567.89"))
  
  // Test different date formats
  azimuth::Span::set_attribute(span, "date.us", azimuth::StringValue("01/31/2023"))
  azimuth::Span::set_attribute(span, "date.eu", azimuth::StringValue("31/01/2023"))
  azimuth::Span::set_attribute(span, "date.iso", azimuth::StringValue("2023-01-31"))
  azimuth::Span::set_attribute(span, "date.jp", azimuth::StringValue("2023年01月31日"))
  azimuth::Span::set_attribute(span, "date.cn", azimuth::StringValue("2023年01月31日"))
  
  // Test different time formats
  azimuth::Span::set_attribute(span, "time.us", azimuth::StringValue("2:30:45 PM"))
  azimuth::Span::set_attribute(span, "time.eu", azimuth::StringValue("14:30:45"))
  azimuth::Span::set_attribute(span, "time.military", azimuth::StringValue("14:30:45"))
  
  // Test currency formats
  azimuth::Span::set_attribute(span, "currency.usd", azimuth::StringValue("$1,234.56"))
  azimuth::Span::set_attribute(span, "currency.eur", azimuth::StringValue("€1.234,56"))
  azimuth::Span::set_attribute(span, "currency.jpy", azimuth::StringValue("¥1,235"))
  azimuth::Span::set_attribute(span, "currency.cny", azimuth::StringValue("¥1,234.56"))
  azimuth::Span::set_attribute(span, "currency.inr", azimuth::StringValue("₹1,234.56"))
  
  // Test address formats
  azimuth::Span::set_attribute(span, "address.us", azimuth::StringValue("123 Main St, New York, NY 10001"))
  azimuth::Span::set_attribute(span, "address.jp", azimuth::StringValue("〒100-0001 東京都千代田区千代田1-1"))
  azimuth::Span::set_attribute(span, "address.cn", azimuth::StringValue("中国北京市朝阳区建国门外大街1号"))
  azimuth::Span::set_attribute(span, "address.kr", azimuth::StringValue("서울특별시 종로구 종로1가"))
  
  // Verify locale-specific attributes
  assert_eq(azimuth::Span::get_attribute(span, "number.us"), Some(azimuth::StringValue("1,234,567.89")))
  assert_eq(azimuth::Span::get_attribute(span, "date.jp"), Some(azimuth::StringValue("2023年01月31日")))
  assert_eq(azimuth::Span::get_attribute(span, "currency.eur"), Some(azimuth::StringValue("€1.234,56")))
  
  azimuth::Span::set_status_code(span, azimuth::Ok)
  azimuth::Span::end(span)
}

// Test 4: International resource and service names
pub test "international_resource_service_names" {
  // Create resources with international names
  let chinese_resource = azimuth::Resource::new()
  let chinese_resource_with_attrs = azimuth::Resource::with_attributes(chinese_resource, [
    ("服务.名称", azimuth::StringValue("用户服务")),
    ("服务.版本", azimuth::StringValue("1.0.0")),
    ("部署.环境", azimuth::StringValue("生产环境")),
    ("主机.名称", azimuth::StringValue("生产服务器-01"))
  ])
  
  let japanese_resource = azimuth::Resource::new()
  let japanese_resource_with_attrs = azimuth::Resource::with_attributes(japanese_resource, [
    ("サービス.名前", azimuth::StringValue("ユーザーサービス")),
    ("サービス.バージョン", azimuth::StringValue("1.0.0")),
    ("デプロイ環境", azimuth::StringValue("本番環境")),
    ("ホスト名", azimuth::StringValue("本番サーバー-01"))
  ])
  
  let korean_resource = azimuth::Resource::new()
  let korean_resource_with_attrs = azimuth::Resource::with_attributes(korean_resource, [
    ("서비스.이름", azimuth::StringValue("사용자 서비스")),
    ("서비스.버전", azimuth::StringValue("1.0.0")),
    ("배포.환경", azimuth::StringValue("프로덕션 환경")),
    ("호스트.이름", azimuth::StringValue("프로덕션-서버-01"))
  ])
  
  // Merge international resources
  let merged_resource = azimuth::Resource::merge(chinese_resource_with_attrs, japanese_resource_with_attrs)
  let final_resource = azimuth::Resource::merge(merged_resource, korean_resource_with_attrs)
  
  // Verify merged resource contains international attributes
  assert_eq(azimuth::Resource::get_attribute(final_resource, "服务.名称"), Some(azimuth::StringValue("用户服务")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "サービス.名前"), Some(azimuth::StringValue("ユーザーサービス")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "서비스.이름"), Some(azimuth::StringValue("사용자 서비스")))
  
  // Test international service names in spans
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "国际服务")
  
  let chinese_span = azimuth::Tracer::start_span(tracer, "用户认证操作")
  azimuth::Span::set_attribute(chinese_span, "操作.类型", azimuth::StringValue("用户登录"))
  azimuth::Span::set_attribute(chinese_span, "用户.地区", azimuth::StringValue("中国"))
  
  let japanese_span = azimuth::Tracer::start_span(tracer, "ユーザー認証操作")
  azimuth::Span::set_attribute(japanese_span, "操作.タイプ", azimuth::StringValue("ユーザーログイン"))
  azimuth::Span::set_attribute(japanese_span, "ユーザー.地域", azimuth::StringValue("日本"))
  
  let korean_span = azimuth::Tracer::start_span(tracer, "사용자 인증 작업")
  azimuth::Span::set_attribute(korean_span, "작업.유형", azimuth::StringValue("사용자 로그인"))
  azimuth::Span::set_attribute(korean_span, "사용자.지역", azimuth::StringValue("한국"))
  
  // Verify international span attributes
  assert_eq(azimuth::Span::get_attribute(chinese_span, "操作.类型"), Some(azimuth::StringValue("用户登录")))
  assert_eq(azimuth::Span::get_attribute(japanese_span, "操作.タイプ"), Some(azimuth::StringValue("ユーザーログイン")))
  assert_eq(azimuth::Span::get_attribute(korean_span, "작업.유형"), Some(azimuth::StringValue("사용자 로그인")))
  
  // End spans
  azimuth::Span::set_status_code(chinese_span, azimuth::Ok)
  azimuth::Span::set_status_code(japanese_span, azimuth::Ok)
  azimuth::Span::set_status_code(korean_span, azimuth::Ok)
  azimuth::Span::end(chinese_span)
  azimuth::Span::end(japanese_span)
  azimuth::Span::end(korean_span)
}

// Test 5: International baggage and context propagation
pub test "international_baggage_context_propagation" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "国际化传播服务")
  
  // Create context with international baggage
  let root_ctx = azimuth::Context::root()
  let baggage = azimuth::Baggage::new()
  
  // Add baggage entries in different languages
  let baggage_with_cn = azimuth::Baggage::set_entry(baggage, "用户.标识", "用户123")
  let baggage_with_jp = azimuth::Baggage::set_entry(baggage_with_cn, "ユーザー.ID", "ユーザー456")
  let baggage_with_kr = azimuth::Baggage::set_entry(baggage_with_jp, "사용자.ID", "사용자789")
  let baggage_with_ar = azimuth::Baggage::set_entry(baggage_with_kr, "معرف.المستخدم", "مستخدم101")
  
  let ctx_with_baggage = azimuth::Context::with_value(root_ctx, azimuth::ContextKey::new("国际化.baggage"), baggage_with_ar)
  
  // Create spans with international context
  let service_span = azimuth::Tracer::start_span_with_context(tracer, "国际化服务操作", ctx_with_baggage)
  azimuth::Span::set_attribute(service_span, "服务.名称", azimuth::StringValue("多语言用户服务"))
  
  // Extract and verify baggage
  let propagated_baggage = azimuth::Context::get(azimuth::Span::context(service_span), azimuth::ContextKey::new("国际化.baggage"))
  assert_eq(azimuth::Baggage::get_entry(propagated_baggage, "用户.标识"), Some("用户123"))
  assert_eq(azimuth::Baggage::get_entry(propagated_baggage, "ユーザー.ID"), Some("ユーザー456"))
  assert_eq(azimuth::Baggage::get_entry(propagated_baggage, "사용자.ID"), Some("사용자789"))
  assert_eq(azimuth::Baggage::get_entry(propagated_baggage, "معرف.المستخدم"), Some("مستخدم101"))
  
  // Add more international context
  let cn_key = azimuth::ContextKey::new("中文.键")
  let jp_key = azimuth::ContextKey::new("日本語.キー")
  let kr_key = azimuth::ContextKey::new("한국어.키")
  
  let ctx_with_cn = azimuth::Context::with_value(azimuth::Span::context(service_span), cn_key, "中文值")
  let ctx_with_jp = azimuth::Context::with_value(ctx_with_cn, jp_key, "日本語値")
  let ctx_with_kr = azimuth::Context::with_value(ctx_with_jp, kr_key, "한국어 값")
  
  // Create child span with international context
  let child_span = azimuth::Tracer::start_span_with_context(tracer, "国际化子操作", ctx_with_kr)
  azimuth::Span::set_attribute(child_span, "子操作.类型", azimuth::StringValue("多语言数据处理"))
  
  // Verify international context propagation
  let child_ctx = azimuth::Span::context(child_span)
  assert_eq(azimuth::Context::get(child_ctx, cn_key), Some("中文值"))
  assert_eq(azimuth::Context::get(child_ctx, jp_key), Some("日本語値"))
  assert_eq(azimuth::Context::get(child_ctx, kr_key), Some("한국어 값"))
  
  // End spans
  azimuth::Span::set_status_code(child_span, azimuth::Ok)
  azimuth::Span::set_status_code(service_span, azimuth::Ok)
  azimuth::Span::end(child_span)
  azimuth::Span::end(service_span)
}