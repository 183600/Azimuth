// High Quality Internationalization Tests for Azimuth Telemetry System
// This file contains comprehensive tests for internationalization and localization support

// Test 1: Unicode String Handling in Attributes
test "unicode string handling in attributes" {
  // Test Chinese characters
  let chinese_attrs = Attributes::new()
  Attributes::set(chinese_attrs, "用户.标识", StringValue("用户123"))
  Attributes::set(chinese_attrs, "会话.标识", StringValue("会话456"))
  Attributes::set(chinese_attrs, "租户.标识", StringValue("租户789"))
  Attributes::set(chinese_attrs, "操作.名称", StringValue("数据处理"))
  
  assert_eq(Attributes::get(chinese_attrs, "用户.标识"), Some(StringValue("用户123")))
  assert_eq(Attributes::get(chinese_attrs, "会话.标识"), Some(StringValue("会话456")))
  assert_eq(Attributes::get(chinese_attrs, "租户.标识"), Some(StringValue("租户789")))
  assert_eq(Attributes::get(chinese_attrs, "操作.名称"), Some(StringValue("数据处理")))
  
  // Test Japanese characters
  let japanese_attrs = Attributes::new()
  Attributes::set(japanese_attrs, "ユーザー.ID", StringValue("ユーザー123"))
  Attributes::set(japanese_attrs, "セッション.ID", StringValue("セッション456"))
  Attributes::set(japanese_attrs, "テナント.ID", StringValue("テナント789"))
  Attributes::set(japanese_attrs, "操作.名前", StringValue("データ処理"))
  
  assert_eq(Attributes::get(japanese_attrs, "ユーザー.ID"), Some(StringValue("ユーザー123")))
  assert_eq(Attributes::get(japanese_attrs, "セッション.ID"), Some(StringValue("セッション456")))
  assert_eq(Attributes::get(japanese_attrs, "テナント.ID"), Some(StringValue("テナント789")))
  assert_eq(Attributes::get(japanese_attrs, "操作.名前"), Some(StringValue("データ処理")))
  
  // Test Korean characters
  let korean_attrs = Attributes::new()
  Attributes::set(korean_attrs, "사용자.ID", StringValue("사용자123"))
  Attributes::set(korean_attrs, "세션.ID", StringValue("세션456"))
  Attributes::set(korean_attrs, "테넌트.ID", StringValue("테넌트789"))
  Attributes::set(korean_attrs, "작업.이름", StringValue("데이터처리"))
  
  assert_eq(Attributes::get(korean_attrs, "사용자.ID"), Some(StringValue("사용자123")))
  assert_eq(Attributes::get(korean_attrs, "세션.ID"), Some(StringValue("세션456")))
  assert_eq(Attributes::get(korean_attrs, "테넌트.ID"), Some(StringValue("테넌트789")))
  assert_eq(Attributes::get(korean_attrs, "작업.이름"), Some(StringValue("데이터처리")))
  
  // Test Arabic characters
  let arabic_attrs = Attributes::new()
  Attributes::set(arabic_attrs, "معرف.المستخدم", StringValue("مستخدم123"))
  Attributes::set(arabic_attrs, "معرف.الجلسة", StringValue("جلسة456"))
  Attributes::set(arabic_attrs, "معرف.المستأجر", StringValue("مستأجر789"))
  Attributes::set(arabic_attrs, "اسم.العملية", StringValue("معالجة.البيانات"))
  
  assert_eq(Attributes::get(arabic_attrs, "معرف.المستخدم"), Some(StringValue("مستخدم123")))
  assert_eq(Attributes::get(arabic_attrs, "معرف.الجلسة"), Some(StringValue("جلسة456")))
  assert_eq(Attributes::get(arabic_attrs, "معرف.المستأجر"), Some(StringValue("مستأجر789")))
  assert_eq(Attributes::get(arabic_attrs, "اسم.العملية"), Some(StringValue("معالجة.البيانات")))
}

// Test 2: Unicode String Handling in Baggage
test "unicode string handling in baggage" {
  // Test Chinese baggage entries
  let chinese_baggage = Baggage::new()
  let with_user = Baggage::set_entry(chinese_baggage, "用户标识", "用户123")
  let with_session = Baggage::set_entry(with_user, "会话标识", "会话456")
  let with_tenant = Baggage::set_entry(with_session, "租户标识", "租户789")
  
  assert_eq(Baggage::get_entry(with_tenant, "用户标识"), Some("用户123"))
  assert_eq(Baggage::get_entry(with_tenant, "会话标识"), Some("会话456"))
  assert_eq(Baggage::get_entry(with_tenant, "租户标识"), Some("租户789"))
  
  // Test Japanese baggage entries
  let japanese_baggage = Baggage::new()
  let with_jp_user = Baggage::set_entry(japanese_baggage, "ユーザーID", "ユーザー123")
  let with_jp_session = Baggage::set_entry(with_jp_user, "セッションID", "セッション456")
  let with_jp_tenant = Baggage::set_entry(with_jp_session, "テナントID", "テナント789")
  
  assert_eq(Baggage::get_entry(with_jp_tenant, "ユーザーID"), Some("ユーザー123"))
  assert_eq(Baggage::get_entry(with_jp_tenant, "セッションID"), Some("セッション456"))
  assert_eq(Baggage::get_entry(with_jp_tenant, "テナントID"), Some("テナント789"))
  
  // Test mixed language baggage entries
  let mixed_baggage = Baggage::new()
  let with_chinese = Baggage::set_entry(mixed_baggage, "中文键", "中文值")
  let with_japanese = Baggage::set_entry(with_chinese, "日本語キー", "日本語値")
  let with_english = Baggage::set_entry(with_japanese, "english.key", "english.value")
  let with_emoji = Baggage::set_entry(with_english, "表情符号键", "表情符号值🚀")
  
  assert_eq(Baggage::get_entry(with_emoji, "中文键"), Some("中文值"))
  assert_eq(Baggage::get_entry(with_emoji, "日本語キー"), Some("日本語値"))
  assert_eq(Baggage::get_entry(with_emoji, "english.key"), Some("english.value"))
  assert_eq(Baggage::get_entry(with_emoji, "表情符号键"), Some("表情符号值🚀"))
}

// Test 3: Unicode String Handling in Context
test "unicode string handling in context" {
  // Test context keys with Unicode characters
  let ctx = Context::root()
  
  let chinese_key = ContextKey::new("中文键")
  let ctx_with_chinese = Context::with_value(ctx, chinese_key, "中文值")
  assert_eq(Context::get(ctx_with_chinese, chinese_key), Some("中文值"))
  
  let japanese_key = ContextKey::new("日本語キー")
  let ctx_with_japanese = Context::with_value(ctx_with_chinese, japanese_key, "日本語値")
  assert_eq(Context::get(ctx_with_japanese, japanese_key), Some("日本語値"))
  
  let korean_key = ContextKey::new("한국어키")
  let ctx_with_korean = Context::with_value(ctx_with_japanese, korean_key, "한국어값")
  assert_eq(Context::get(ctx_with_korean, korean_key), Some("한국어값"))
  
  let arabic_key = ContextKey::new("مفتاح.عربي")
  let ctx_with_arabic = Context::with_value(ctx_with_korean, arabic_key, "قيمة.عربية")
  assert_eq(Context::get(ctx_with_arabic, arabic_key), Some("قيمة.عربية"))
  
  // Test context values with Unicode characters
  let emoji_key = ContextKey::new("emoji.test")
  let ctx_with_emoji = Context::with_value(ctx_with_arabic, emoji_key, "测试值🚀😀🎉")
  assert_eq(Context::get(ctx_with_emoji, emoji_key), Some("测试值🚀😀🎉"))
}

// Test 4: Unicode String Handling in Span Names and Attributes
test "unicode string handling in span names and attributes" {
  // Test span names with Unicode characters
  let tracer = TracerProvider::get_tracer(TracerProvider::new(), "国际化测试")
  
  let chinese_span = Tracer::start_span(tracer, "中文操作")
  assert_eq(Span::name(chinese_span), "中文操作")
  
  let japanese_span = Tracer::start_span(tracer, "日本語操作")
  assert_eq(Span::name(japanese_span), "日本語操作")
  
  let korean_span = Tracer::start_span(tracer, "한국어작업")
  assert_eq(Span::name(korean_span), "한국어작업")
  
  let emoji_span = Tracer::start_span(tracer, "表情符号操作🚀")
  assert_eq(Span::name(emoji_span), "表情符号操作🚀")
  
  // Test span attributes with Unicode characters
  Span::set_attribute(chinese_span, "用户标识", StringValue("用户123"))
  Span::set_attribute(chinese_span, "操作结果", StringValue("成功"))
  Span::set_attribute(chinese_span, "错误信息", StringValue("无错误"))
  
  Span::set_attribute(japanese_span, "ユーザーID", StringValue("ユーザー123"))
  Span::set_attribute(japanese_span, "操作結果", StringValue("成功"))
  Span::set_attribute(japanese_span, "エラー情報", StringValue("エラーなし"))
  
  Span::set_attribute(korean_span, "사용자ID", StringValue("사용자123"))
  Span::set_attribute(korean_span, "작업결과", StringValue("성공"))
  Span::set_attribute(korean_span, "오류정보", StringValue("오류없음"))
  
  Span::set_attribute(emoji_span, "user.id", StringValue("user123"))
  Span::set_attribute(emoji_span, "operation.result", StringValue("success 🎉"))
  Span::set_attribute(emoji_span, "error.message", StringValue("no error 😊"))
}

// Test 5: Unicode String Handling in Log Records
test "unicode string handling in log records" {
  // Test log records with Unicode characters
  let logger = LoggerProvider::get_logger(LoggerProvider::new(), "国际化日志器")
  
  // Chinese log record
  let chinese_log = LogRecord::new()
  LogRecord::set_body(chinese_log, "处理用户请求")
  LogRecord::set_severity(chinese_log, Info)
  LogRecord::set_attribute(chinese_log, "用户标识", StringValue("用户123"))
  LogRecord::set_attribute(chinese_log, "操作类型", StringValue("数据查询"))
  LogRecord::set_attribute(chinese_log, "结果", StringValue("成功"))
  
  assert_eq(LogRecord::body(chinese_log), "处理用户请求")
  assert_eq(LogRecord::severity(chinese_log), Info)
  
  // Japanese log record
  let japanese_log = LogRecord::new()
  LogRecord::set_body(japanese_log, "ユーザーリクエストを処理")
  LogRecord::set_severity(japanese_log, Info)
  LogRecord::set_attribute(japanese_log, "ユーザーID", StringValue("ユーザー123"))
  LogRecord::set_attribute(japanese_log, "操作タイプ", StringValue("データクエリ"))
  LogRecord::set_attribute(japanese_log, "結果", StringValue("成功"))
  
  assert_eq(LogRecord::body(japanese_log), "ユーザーリクエストを処理")
  assert_eq(LogRecord::severity(japanese_log), Info)
  
  // Korean log record
  let korean_log = LogRecord::new()
  LogRecord::set_body(korean_log, "사용자 요청 처리")
  LogRecord::set_severity(korean_log, Info)
  LogRecord::set_attribute(korean_log, "사용자ID", StringValue("사용자123"))
  LogRecord::set_attribute(korean_log, "작업유형", StringValue("데이터조회"))
  LogRecord::set_attribute(korean_log, "결과", StringValue("성공"))
  
  assert_eq(LogRecord::body(korean_log), "사용자 요청 처리")
  assert_eq(LogRecord::severity(korean_log), Info)
  
  // Emoji log record
  let emoji_log = LogRecord::new()
  LogRecord::set_body(emoji_log, "Processing request with emojis 🚀")
  LogRecord::set_severity(emoji_log, Info)
  LogRecord::set_attribute(emoji_log, "user.id", StringValue("user123"))
  LogRecord::set_attribute(emoji_log, "operation.type", StringValue("data query"))
  LogRecord::set_attribute(emoji_log, "result", StringValue("success 🎉"))
  
  assert_eq(LogRecord::body(emoji_log), "Processing request with emojis 🚀")
  assert_eq(LogRecord::severity(emoji_log), Info)
}

// Test 6: Unicode String Handling in Resource Attributes
test "unicode string handling in resource attributes" {
  // Test resource with Unicode attributes
  let chinese_resource = Resource::new()
  Resource::set_attribute(chinese_resource, "服务名称", StringValue("中文服务"))
  Resource::set_attribute(chinese_resource, "服务版本", StringValue("1.0.0"))
  Resource::set_attribute(chinese_resource, "实例标识", StringValue("实例123"))
  Resource::set_attribute(chinese_resource, "部署环境", StringValue("生产环境"))
  
  assert_eq(Resource::get_attribute(chinese_resource, "服务名称"), Some(StringValue("中文服务")))
  assert_eq(Resource::get_attribute(chinese_resource, "服务版本"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(chinese_resource, "实例标识"), Some(StringValue("实例123")))
  assert_eq(Resource::get_attribute(chinese_resource, "部署环境"), Some(StringValue("生产环境")))
  
  // Test resource with Japanese attributes
  let japanese_resource = Resource::new()
  Resource::set_attribute(japanese_resource, "サービス名", StringValue("日本語サービス"))
  Resource::set_attribute(japanese_resource, "サービスバージョン", StringValue("1.0.0"))
  Resource::set_attribute(japanese_resource, "インスタンスID", StringValue("インスタンス123"))
  Resource::set_attribute(japanese_resource, "配備環境", StringValue("本番環境"))
  
  assert_eq(Resource::get_attribute(japanese_resource, "サービス名"), Some(StringValue("日本語サービス")))
  assert_eq(Resource::get_attribute(japanese_resource, "サービスバージョン"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(japanese_resource, "インスタンスID"), Some(StringValue("インスタンス123")))
  assert_eq(Resource::get_attribute(japanese_resource, "配備環境"), Some(StringValue("本番環境")))
  
  // Test resource with Korean attributes
  let korean_resource = Resource::new()
  Resource::set_attribute(korean_resource, "서비스명", StringValue("한국어서비스"))
  Resource::set_attribute(korean_resource, "서비스버전", StringValue("1.0.0"))
  Resource::set_attribute(korean_resource, "인스턴스ID", StringValue("인스턴스123"))
  Resource::set_attribute(korean_resource, "배포환경", StringValue("프로덕션"))
  
  assert_eq(Resource::get_attribute(korean_resource, "서비스명"), Some(StringValue("한국어서비스")))
  assert_eq(Resource::get_attribute(korean_resource, "서비스버전"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(korean_resource, "인스턴스ID"), Some(StringValue("인스턴스123")))
  assert_eq(Resource::get_attribute(korean_resource, "배포환경"), Some(StringValue("프로덕션")))
}

// Test 7: Unicode String Handling in Instrumentation Scope
test "unicode string handling in instrumentation scope" {
  // Test instrumentation scope with Unicode characters
  let chinese_scope = InstrumentationScope::new(
    "中文遥测库",
    Some("1.0.0"),
    Some("https://example.com/中文/模式/v1")
  )
  
  assert_eq(chinese_scope.name, "中文遥测库")
  assert_eq(chinese_scope.version, Some("1.0.0"))
  assert_eq(chinese_scope.schema_url, Some("https://example.com/中文/模式/v1"))
  
  let japanese_scope = InstrumentationScope::new(
    "日本語テレメトリーライブラリ",
    Some("1.0.0"),
    Some("https://example.com/日本語/スキーマ/v1")
  )
  
  assert_eq(japanese_scope.name, "日本語テレメトリーライブラリ")
  assert_eq(japanese_scope.version, Some("1.0.0"))
  assert_eq(japanese_scope.schema_url, Some("https://example.com/日本語/スキーマ/v1"))
  
  let korean_scope = InstrumentationScope::new(
    "한국어원격측정라이브러리",
    Some("1.0.0"),
    Some("https://example.com/한국어/스키마/v1")
  )
  
  assert_eq(korean_scope.name, "한국어원격측정라이브러리")
  assert_eq(korean_scope.version, Some("1.0.0"))
  assert_eq(korean_scope.schema_url, Some("https://example.com/한국어/스키마/v1"))
}

// Test 8: Unicode String Handling in Text Map Carrier
test "unicode string handling in text map carrier" {
  // Test carrier with Unicode headers
  let carrier = TextMapCarrier::new()
  
  // Set headers with Unicode characters
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "中文 baggage", "用户标识=用户123,会话标识=会话456")
  TextMapCarrier::set(carrier, "日本語 baggage", "ユーザーID=ユーザー123,セッションID=セッション456")
  TextMapCarrier::set(carrier, "한국어 baggage", "사용자ID=사용자123,세션ID=세션456")
  TextMapCarrier::set(carrier, "emoji baggage", "user=user123,emoji=🚀😀🎉")
  
  // Verify headers are stored correctly
  assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(TextMapCarrier::get(carrier, "中文 baggage"), Some("用户标识=用户123,会话标识=会话456"))
  assert_eq(TextMapCarrier::get(carrier, "日本語 baggage"), Some("ユーザーID=ユーザー123,セッションID=セッション456"))
  assert_eq(TextMapCarrier::get(carrier, "한국어 baggage"), Some("사용자ID=사용자123,세션ID=세션456"))
  assert_eq(TextMapCarrier::get(carrier, "emoji baggage"), Some("user=user123,emoji=🚀😀🎉"))
  
  // Test propagation with Unicode baggage
  let baggage_propagator = W3CBaggagePropagator::new()
  let ctx = Context::root()
  
  // Create context with Unicode baggage
  let unicode_baggage = Baggage::new()
  let with_chinese = Baggage::set_entry(unicode_baggage, "中文键", "中文值")
  let with_japanese = Baggage::set_entry(with_chinese, "日本語キー", "日本語値")
  let with_korean = Baggage::set_entry(with_japanese, "한국어키", "한국어값")
  let with_emoji = Baggage::set_entry(with_korean, "emoji키", "emoji값🚀")
  
  let ctx_with_unicode = Context::with_value(ctx, ContextKey::new("baggage"), with_emoji)
  
  // Inject Unicode baggage
  let unicode_carrier = TextMapCarrier::new()
  W3CBaggagePropagator::inject(baggage_propagator, ctx_with_unicode, unicode_carrier)
  
  // Extract Unicode baggage
  let extracted_ctx = W3CBaggagePropagator::extract(baggage_propagator, unicode_carrier)
  let extracted_baggage = Context::get(extracted_ctx, ContextKey::new("baggage"))
  
  // Verify Unicode baggage is extracted correctly
  match (extracted_baggage) {
    Some(baggage) => {
      assert_eq(Baggage::get_entry(baggage, "中文键"), Some("中文值"))
      assert_eq(Baggage::get_entry(baggage, "日本語キー"), Some("日本語値"))
      assert_eq(Baggage::get_entry(baggage, "한국어키"), Some("한국어값"))
      assert_eq(Baggage::get_entry(baggage, "emoji키"), Some("emoji값🚀"))
    }
    None => assert_true(false) // Should not happen
  }
}

// Test 9: Unicode String Handling in Metrics
test "unicode string handling in metrics" {
  // Test metrics with Unicode characters
  let meter = MeterProvider::get_meter(MeterProvider::new(), "国际化仪表")
  
  // Create instruments with Unicode names
  let chinese_counter = Meter::create_counter(meter, "中文计数器")
  let japanese_counter = Meter::create_counter(meter, "日本語カウンター")
  let korean_counter = Meter::create_counter(meter, "한국어카운터")
  let emoji_counter = Meter::create_counter(meter, "emoji counter 🚀")
  
  assert_eq(chinese_counter.name, "中文计数器")
  assert_eq(japanese_counter.name, "日本語カウンター")
  assert_eq(korean_counter.name, "한국어카운터")
  assert_eq(emoji_counter.name, "emoji counter 🚀")
  
  // Record metrics with Unicode attributes
  Counter::add(chinese_counter, 1, [ ("操作类型", StringValue("数据查询")), ("用户类型", StringValue("管理员")) ])
  Counter::add(japanese_counter, 1, [ ("操作タイプ", StringValue("データクエリ")), ("ユーザータイプ", StringValue("管理者")) ])
  Counter::add(korean_counter, 1, [ ("작업유형", StringValue("데이터조회")), ("사용자유형", StringValue("관리자")) ])
  Counter::add(emoji_counter, 1, [ ("operation.type", StringValue("data query")), ("user.type", StringValue("admin 😊")) ])
  
  // Test histogram with Unicode names
  let chinese_histogram = Meter::create_histogram(meter, "中文直方图")
  let japanese_histogram = Meter::create_histogram(meter, "日本語ヒストグラム")
  let korean_histogram = Meter::create_histogram(meter, "한국어히스토그램")
  
  assert_eq(chinese_histogram.name, "中文直方图")
  assert_eq(japanese_histogram.name, "日本語ヒストグラム")
  assert_eq(korean_histogram.name, "한국어히스토그램")
  
  // Record histogram values with Unicode attributes
  Histogram::record(chinese_histogram, 150.0, [ ("操作类型", StringValue("数据查询")), ("响应时间", StringValue("正常")) ])
  Histogram::record(japanese_histogram, 200.0, [ ("操作タイプ", StringValue("データクエリ")), ("応答時間", StringValue("正常")) ])
  Histogram::record(korean_histogram, 100.0, [ ("작업유형", StringValue("데이터조회")), ("응답시간", StringValue("정상")) ])
}

// Test 10: Locale-Specific Formatting and Handling
test "locale-specific formatting and handling" {
  // Test locale-specific number formatting
  let chinese_attrs = Attributes::new()
  Attributes::set(chinese_attrs, "数值", IntValue(1234567))
  Attributes::set(chinese_attrs, "小数", FloatValue(1234.5678))
  Attributes::set(chinese_attrs, "百分比", FloatValue(0.1234))
  
  assert_eq(Attributes::get(chinese_attrs, "数值"), Some(IntValue(1234567)))
  assert_eq(Attributes::get(chinese_attrs, "小数"), Some(FloatValue(1234.5678)))
  assert_eq(Attributes::get(chinese_attrs, "百分比"), Some(FloatValue(0.1234)))
  
  // Test locale-specific date/time formatting
  let japanese_attrs = Attributes::new()
  Attributes::set(japanese_attrs, "日付", StringValue("2023-12-31"))
  Attributes::set(japanese_attrs, "時刻", StringValue("23:59:59"))
  Attributes::set(japanese_attrs, "タイムスタンプ", StringValue("2023-12-31T23:59:59.999Z"))
  
  assert_eq(Attributes::get(japanese_attrs, "日付"), Some(StringValue("2023-12-31")))
  assert_eq(Attributes::get(japanese_attrs, "時刻"), Some(StringValue("23:59:59")))
  assert_eq(Attributes::get(japanese_attrs, "タイムスタンプ"), Some(StringValue("2023-12-31T23:59:59.999Z")))
  
  // Test locale-specific currency formatting
  let korean_attrs = Attributes::new()
  Attributes::set(korean_attrs, "통화", StringValue("KRW"))
  Attributes::set(korean_attrs, "금액", IntValue(50000))
  Attributes::set(korean_attrs, "형식화된금액", StringValue("₩50,000"))
  
  assert_eq(Attributes::get(korean_attrs, "통화"), Some(StringValue("KRW")))
  assert_eq(Attributes::get(korean_attrs, "금액"), Some(IntValue(50000)))
  assert_eq(Attributes::get(korean_attrs, "형식화된금액"), Some(StringValue("₩50,000")))
  
  // Test locale-specific address formatting
  let address_attrs = Attributes::new()
  Attributes::set(address_attrs, "中文地址", StringValue("北京市朝阳区建国门外大街1号"))
  Attributes::set(address_attrs, "日本語住所", StringValue("東京都千代田区丸の内1-2-3"))
  Attributes::set(address_attrs, "한국어주소", StringValue("서울특별시 종로구 세종대로 1"))
  Attributes::set(address_attrs, "English Address", StringValue("123 Main St, New York, NY 10001"))
  
  assert_eq(Attributes::get(address_attrs, "中文地址"), Some(StringValue("北京市朝阳区建国门外大街1号")))
  assert_eq(Attributes::get(address_attrs, "日本語住所"), Some(StringValue("東京都千代田区丸の内1-2-3")))
  assert_eq(Attributes::get(address_attrs, "한국어주소"), Some(StringValue("서울특별시 종로구 세종대로 1")))
  assert_eq(Attributes::get(address_attrs, "English Address"), Some(StringValue("123 Main St, New York, NY 10001")))
}