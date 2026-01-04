// Azimuth Telemetry System - Comprehensive Internationalization Support Tests
// This file contains comprehensive test cases for internationalization and multilingual support

// Test 1: Unicode Character Support in Attributes
test "unicode character support in attributes" {
  let attrs = Attributes::new()
  
  // Test Chinese characters
  Attributes::set(attrs, "chinese.key", StringValue("中文测试"))
  let chinese_result = Attributes::get(attrs, "chinese.key")
  match chinese_result {
    Some(StringValue(value)) => assert_eq(value, "中文测试")
    _ => assert_true(false)
  }
  
  // Test Japanese characters
  Attributes::set(attrs, "japanese.key", StringValue("日本語テスト"))
  let japanese_result = Attributes::get(attrs, "japanese.key")
  match japanese_result {
    Some(StringValue(value)) => assert_eq(value, "日本語テスト")
    _ => assert_true(false)
  }
  
  // Test Korean characters
  Attributes::set(attrs, "korean.key", StringValue("한국어 테스트"))
  let korean_result = Attributes::get(attrs, "korean.key")
  match korean_result {
    Some(StringValue(value)) => assert_eq(value, "한국어 테스트")
    _ => assert_true(false)
  }
  
  // Test Arabic characters
  Attributes::set(attrs, "arabic.key", StringValue("اختبار عربي"))
  let arabic_result = Attributes::get(attrs, "arabic.key")
  match arabic_result {
    Some(StringValue(value)) => assert_eq(value, "اختبار عربي")
    _ => assert_true(false)
  }
  
  // Test Russian characters
  Attributes::set(attrs, "russian.key", StringValue("Тест на русском"))
  let russian_result = Attributes::get(attrs, "russian.key")
  match russian_result {
    Some(StringValue(value)) => assert_eq(value, "Тест на русском")
    _ => assert_true(false)
  }
  
  // Test Hebrew characters
  Attributes::set(attrs, "hebrew.key", StringValue("בדיקה עברית"))
  let hebrew_result = Attributes::get(attrs, "hebrew.key")
  match hebrew_result {
    Some(StringValue(value)) => assert_eq(value, "בדיקה עברית")
    _ => assert_true(false)
  }
  
  // Test Thai characters
  Attributes::set(attrs, "thai.key", StringValue("การทดสอบภาษาไทย"))
  let thai_result = Attributes::get(attrs, "thai.key")
  match thai_result {
    Some(StringValue(value)) => assert_eq(value, "การทดสอบภาษาไทย")
    _ => assert_true(false)
  }
  
  // Test Emoji characters
  Attributes::set(attrs, "emoji.key", StringValue("🚀🌍💻📊"))
  let emoji_result = Attributes::get(attrs, "emoji.key")
  match emoji_result {
    Some(StringValue(value)) => assert_eq(value, "🚀🌍💻📊")
    _ => assert_true(false)
  }
  
  // Test mixed Unicode characters
  Attributes::set(attrs, "mixed.key", StringValue("中文 🚀 English 日本語 🌍 한국어"))
  let mixed_result = Attributes::get(attrs, "mixed.key")
  match mixed_result {
    Some(StringValue(value)) => assert_eq(value, "中文 🚀 English 日本語 🌍 한국어")
    _ => assert_true(false)
  }
}

// Test 2: Unicode Support in Span Names and Events
test "unicode support in span names and events" {
  let trace_ctx = SpanContext::new("unicode_trace", "unicode_root", true, "")
  
  // Test span name with Chinese characters
  let chinese_span = Span::new("中文操作", Internal, trace_ctx)
  Span::add_attribute(chinese_span, "操作类型", StringValue("数据处理"))
  Span::add_event(chinese_span, "操作开始", Some([
    ("时间戳", IntValue(1234567890)),
    ("用户", StringValue("张三"))
  ]))
  Span::end(chinese_span)
  
  assert_eq(Span::name(chinese_span), "中文操作")
  
  // Test span name with Japanese characters
  let japanese_span = Span::new("日本語操作", Internal, trace_ctx)
  Span::add_attribute(japanese_span, "操作タイプ", StringValue("データ処理"))
  Span::add_event(japanese_span, "操作開始", Some([
    ("タイムスタンプ", IntValue(1234567891)),
    ("ユーザー", StringValue("田中"))
  ]))
  Span::end(japanese_span)
  
  assert_eq(Span::name(japanese_span), "日本語操作")
  
  // Test span name with Korean characters
  let korean_span = Span::new("한국어 작업", Internal, trace_ctx)
  Span::add_attribute(korean_span, "작업 유형", StringValue("데이터 처리"))
  Span::add_event(korean_span, "작업 시작", Some([
    ("타임스탬프", IntValue(1234567892)),
    ("사용자", StringValue("김철수"))
  ]))
  Span::end(korean_span)
  
  assert_eq(Span::name(korean_span), "한국어 작업")
  
  // Test span name with Emoji
  let emoji_span = Span::new("🚀 Rocket Operation 🌍", Internal, trace_ctx)
  Span::add_attribute(emoji_span, "operation.type", StringValue("rocket_launch"))
  Span::add_event(emoji_span, "🎯 Launch Started", Some([
    ("📍 Location", StringValue("🚀 Space Station")),
    ("👨‍🚀 Astronaut", StringValue("Neil Armstrong"))
  ]))
  Span::end(emoji_span)
  
  assert_eq(Span::name(emoji_span), "🚀 Rocket Operation 🌍")
  
  // Verify events are preserved correctly
  let chinese_events = Span::events(chinese_span)
  assert_eq(chinese_events.length(), 1)
  
  let chinese_event = chinese_events[0]
  let chinese_user = Event::get_attribute(chinese_event, "用户")
  match chinese_user {
    Some(StringValue(user)) => assert_eq(user, "张三")
    None => assert_true(false)
  }
}

// Test 3: Unicode Support in Log Records
test "unicode support in log records" {
  // Test log record with Chinese characters
  let chinese_log = LogRecord::new(Info, "中文日志消息：数据处理操作已完成")
  assert_eq(LogRecord::severity_number(chinese_log), Info)
  match LogRecord::body(chinese_log) {
    Some(body) => assert_eq(body, "中文日志消息：数据处理操作已完成")
    None => assert_true(false)
  }
  
  // Test log record with Japanese characters
  let japanese_log = LogRecord::new(Warning, "日本語ログメッセージ：データ処理操作が完了しました")
  assert_eq(LogRecord::severity_number(japanese_log), Warning)
  match LogRecord::body(japanese_log) {
    Some(body) => assert_eq(body, "日本語ログメッセージ：データ処理操作が完了しました")
    None => assert_true(false)
  }
  
  // Test log record with Korean characters
  let korean_log = LogRecord::new(Error, "한국어 로그 메시지: 데이터 처리 작업이 완료되었습니다")
  assert_eq(LogRecord::severity_number(korean_log), Error)
  match LogRecord::body(korean_log) {
    Some(body) => assert_eq(body, "한국어 로그 메시지: 데이터 처리 작업이 완료되었습니다")
    None => assert_true(false)
  }
  
  // Test log record with Arabic characters
  let arabic_log = LogRecord::new(Debug, "رسالة سجل باللغة العربية: اكتملت عملية معالجة البيانات")
  assert_eq(LogRecord::severity_number(arabic_log), Debug)
  match LogRecord::body(arabic_log) {
    Some(body) => assert_eq(body, "رسالة سجل باللغة العربية: اكتملت عملية معالجة البيانات")
    None => assert_true(false)
  }
  
  // Test log record with mixed Unicode and attributes
  let unicode_attrs = Attributes::new()
  Attributes::set(unicode_attrs, "用户名", StringValue("张三"))
  Attributes::set(unicode_attrs, "操作", StringValue("データ処理"))
  Attributes::set(unicode_attrs, "상태", StringValue("완료"))
  
  let mixed_log = LogRecord::new_with_context(
    Info,
    Some("Mixed Unicode log: 中文 🚀 English 日本語 🌍 한국어"),
    Some(unicode_attrs),
    Some(1234567890L),
    Some(1234567891L),
    Some("mixed_unicode_trace"),
    Some("mixed_unicode_span"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(mixed_log), Info)
  match LogRecord::body(mixed_log) {
    Some(body) => assert_eq(body, "Mixed Unicode log: 中文 🚀 English 日本語 🌍 한국어")
    None => assert_true(false)
  }
  
  // Verify Unicode attributes are preserved
  let log_attrs = LogRecord::attributes(mixed_log)
  let username = Attributes::get(log_attrs, "用户名")
  match username {
    Some(StringValue(name)) => assert_eq(name, "张三")
    None => assert_true(false)
  }
}

// Test 4: Unicode Support in Resource Attributes
test "unicode support in resource attributes" {
  // Test resource with Chinese attributes
  let chinese_attrs = [
    ("服务名称", StringValue("中文服务")),
    ("服务版本", StringValue("1.0.0")),
    ("环境", StringValue("生产环境")),
    ("描述", StringValue("这是一个中文服务的描述"))
  ]
  let chinese_resource = Resource::with_attributes(Resource::new(), chinese_attrs)
  
  let service_name = Resource::get_attribute(chinese_resource, "服务名称")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "中文服务")
    None => assert_true(false)
  }
  
  let description = Resource::get_attribute(chinese_resource, "描述")
  match description {
    Some(StringValue(desc)) => assert_eq(desc, "这是一个中文服务的描述")
    None => assert_true(false)
  }
  
  // Test resource with Japanese attributes
  let japanese_attrs = [
    ("サービス名", StringValue("日本語サービス")),
    ("サービスバージョン", StringValue("1.0.0")),
    ("環境", StringValue("本番環境")),
    ("説明", StringValue("これは日本語サービスの説明です"))
  ]
  let japanese_resource = Resource::with_attributes(Resource::new(), japanese_attrs)
  
  let service_name_ja = Resource::get_attribute(japanese_resource, "サービス名")
  match service_name_ja {
    Some(StringValue(name)) => assert_eq(name, "日本語サービス")
    None => assert_true(false)
  }
  
  let description_ja = Resource::get_attribute(japanese_resource, "説明")
  match description_ja {
    Some(StringValue(desc)) => assert_eq(desc, "これは日本語サービスの説明です")
    None => assert_true(false)
  }
  
  // Test resource with mixed Unicode attributes
  let mixed_attrs = [
    ("service.name", StringValue("multilingual-service")),
    ("服务名称", StringValue("多语言服务")),
    ("サービス名", StringValue("多言語サービス")),
    ("서비스명", StringValue("다국어 서비스")),
    ("emoji.tag", StringValue("🌍🚀💻"))
  ]
  let mixed_resource = Resource::with_attributes(Resource::new(), mixed_attrs)
  
  let emoji_tag = Resource::get_attribute(mixed_resource, "emoji.tag")
  match emoji_tag {
    Some(StringValue(tag)) => assert_eq(tag, "🌍🚀💻")
    None => assert_true(false)
  }
}

// Test 5: Unicode Support in Baggage
test "unicode support in baggage" {
  let baggage = Baggage::new()
  
  // Test baggage with Chinese entries
  let baggage_with_chinese = Baggage::set_entry(baggage, "用户ID", "用户123")
  let baggage_with_japanese = Baggage::set_entry(baggage_with_chinese, "トランザクションID", "トランザクション456")
  let baggage_with_korean = Baggage::set_entry(baggage_with_japanese, "세션ID", "세션789")
  let baggage_with_emoji = Baggage::set_entry(baggage_with_korean, "🎯特殊标记", "特殊值")
  
  // Verify Chinese entry
  let user_id = Baggage::get_entry(baggage_with_emoji, "用户ID")
  match user_id {
    Some(value) => assert_eq(value, "用户123")
    None => assert_true(false)
  }
  
  // Verify Japanese entry
  let transaction_id = Baggage::get_entry(baggage_with_emoji, "トランザクションID")
  match transaction_id {
    Some(value) => assert_eq(value, "トランザクション456")
    None => assert_true(false)
  }
  
  // Verify Korean entry
  let session_id = Baggage::get_entry(baggage_with_emoji, "세션ID")
  match session_id {
    Some(value) => assert_eq(value, "세션789")
    None => assert_true(false)
  }
  
  // Verify Emoji entry
  let special_tag = Baggage::get_entry(baggage_with_emoji, "🎯特殊标记")
  match special_tag {
    Some(value) => assert_eq(value, "特殊值")
    None => assert_true(false)
  }
}

// Test 6: Unicode Support in HTTP Headers and Bodies
test "unicode support in http headers and bodies" {
  // Test HTTP request with Unicode headers and body
  let unicode_headers = [
    ("Content-Type", "application/json; charset=utf-8"),
    ("X-User-Name", "张三"),
    ("X-Operation", "データ処理"),
    ("X-Status", "완료"),
    ("X-Special", "🚀 Rocket Launch")
  ]
  
  let unicode_body = "{\"message\": \"中文测试消息\", \"data\": \"テストデータ\", \"status\": \"테스트 완료\", \"emoji\": \"🌍\"}"
  let unicode_request = HttpRequest::new("POST", "https://unicode.example.com/api", unicode_headers, Some(unicode_body))
  
  assert_eq(HttpRequest::http_method(unicode_request), "POST")
  assert_eq(HttpRequest::url(unicode_request), "https://unicode.example.com/api")
  match HttpRequest::body(unicode_request) {
    Some(body) => assert_eq(body, unicode_body)
    None => assert_true(false)
  }
  
  // Test HTTP response with Unicode headers and body
  let unicode_response_headers = [
    ("Content-Type", "application/json; charset=utf-8"),
    ("X-Response-Message", "処理完了"),
    ("X-Result", "성공"),
    ("X-Unicode", "✅ 成功")
  ]
  
  let unicode_response_body = "{\"result\": \"成功\", \"message\": \"操作成功完成\", \"details\": \"詳細情報\", \"emoji\": \"🎉\"}"
  let unicode_response = HttpResponse::new(200, unicode_response_headers, Some(unicode_response_body))
  
  assert_eq(HttpResponse::status_code(unicode_response), 200)
  match HttpResponse::body(unicode_response) {
    Some(body) => assert_eq(body, unicode_response_body)
    None => assert_true(false)
  }
}

// Test 7: Unicode Support in Context Propagation
test "unicode support in context propagation" {
  // Create context with Unicode trace state
  let unicode_trace_state = "中文状态=测试值,日本語状態=テスト値,한국어상태=테스트값"
  let unicode_ctx = SpanContext::new("unicode_trace_123", "unicode_span_456", true, unicode_trace_state)
  
  assert_eq(SpanContext::trace_id(unicode_ctx), "unicode_trace_123")
  assert_eq(SpanContext::span_id(unicode_ctx), "unicode_span_456")
  assert_true(SpanContext::is_sampled(unicode_ctx))
  
  // Test context with Unicode baggage
  let unicode_baggage = Baggage::new()
  let baggage_with_unicode = Baggage::set_entry(unicode_baggage, "用户信息", "张三,25岁,北京")
  let baggage_with_more_unicode = Baggage::set_entry(baggage_with_unicode, "操作情報", "データ処理,重要")
  
  // Verify Unicode baggage entries
  let user_info = Baggage::get_entry(baggage_with_more_unicode, "用户信息")
  match user_info {
    Some(value) => assert_eq(value, "张三,25岁,北京")
    None => assert_true(false)
  }
  
  let operation_info = Baggage::get_entry(baggage_with_more_unicode, "操作情報")
  match operation_info {
    Some(value) => assert_eq(value, "データ処理,重要")
    None => assert_true(false)
  }
}

// Test 8: Unicode Support in Metrics
test "unicode support in metrics" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "unicode_meter")
  
  // Create instruments with Unicode names and descriptions
  let chinese_counter = Meter::create_counter(meter, "中文计数器", Some("中文计数器描述"), Some("个"))
  let japanese_histogram = Meter::create_histogram(meter, "日本語ヒストグラム", Some("日本語ヒストグラムの説明"), Some("ミリ秒"))
  let korean_updown = Meter::create_updown_counter(meter, "한국어 업다운카운터", Some("한국어 업다운카운터 설명"), Some("값"))
  
  // Record metrics with Unicode attributes
  Counter::add(chinese_counter, 1.0, Some([
    ("操作类型", StringValue("数据处理")),
    ("用户", StringValue("张三")),
    ("状态", StringValue("成功"))
  ]))
  
  Histogram::record(japanese_histogram, 100.5, Some([
    ("操作タイプ", StringValue("データ処理")),
    ("ユーザー", StringValue("田中")),
    ("ステータス", StringValue("成功"))
  ]))
  
  UpDownCounter::add(korean_updown, 10.0, Some([
    ("작업 유형", StringValue("데이터 처리")),
    ("사용자", StringValue("김철수")),
    ("상태", StringValue("성공"))
  ]))
  
  // Collect and verify metrics
  let metric_data = Meter::collect(meter)
  let metrics = MetricData::metrics(metric_data)
  
  assert_true(metrics.length() >= 3)
  
  // Verify metric names and descriptions
  for metric in metrics {
    let name = Metric::name(metric)
    let description = Metric::description(metric)
    
    if name == "中文计数器" {
      match description {
        Some(desc) => assert_eq(desc, "中文计数器描述")
        None => assert_true(false)
      }
    } else if name == "日本語ヒストグラム" {
      match description {
        Some(desc) => assert_eq(desc, "日本語ヒストグラムの説明")
        None => assert_true(false)
      }
    } else if name == "한국어 업다운카운터" {
      match description {
        Some(desc) => assert_eq(desc, "한국어 업다운카운터 설명")
        None => assert_true(false)
      }
    }
  }
}

// Test 9: Unicode Serialization and Deserialization
test "unicode serialization and deserialization" {
  // Create attributes with various Unicode characters
  let unicode_attrs = Attributes::new()
  Attributes::set(unicode_attrs, "中文键", StringValue("中文值"))
  Attributes::set(unicode_attrs, "日本語キー", StringValue("日本語値"))
  Attributes::set(unicode_attrs, "한국어 키", StringValue("한국어 값"))
  Attributes::set(unicode_attrs, "العربية", StringValue("قيمة عربية"))
  Attributes::set(unicode_attrs, "emoji", StringValue("🚀🌍💻📊"))
  
  // Serialize attributes
  let serialized = Attributes::serialize(unicode_attrs)
  assert_true(serialized.length() > 0)
  
  // Deserialize attributes
  let deserialized = Attributes::deserialize(serialized)
  
  match deserialized {
    Some(attrs) => {
      // Verify Chinese characters
      let chinese_value = Attributes::get(attrs, "中文键")
      match chinese_value {
        Some(StringValue(value)) => assert_eq(value, "中文值")
        None => assert_true(false)
      }
      
      // Verify Japanese characters
      let japanese_value = Attributes::get(attrs, "日本語キー")
      match japanese_value {
        Some(StringValue(value)) => assert_eq(value, "日本語値")
        None => assert_true(false)
      }
      
      // Verify Korean characters
      let korean_value = Attributes::get(attrs, "한국어 키")
      match korean_value {
        Some(StringValue(value)) => assert_eq(value, "한국어 값")
        None => assert_true(false)
      }
      
      // Verify Arabic characters
      let arabic_value = Attributes::get(attrs, "العربية")
      match arabic_value {
        Some(StringValue(value)) => assert_eq(value, "قيمة عربية")
        None => assert_true(false)
      }
      
      // Verify Emoji
      let emoji_value = Attributes::get(attrs, "emoji")
      match emoji_value {
        Some(StringValue(value)) => assert_eq(value, "🚀🌍💻📊")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test span serialization with Unicode
  let unicode_span_ctx = SpanContext::new("unicode_trace", "unicode_span", true, "unicode_state")
  let unicode_span = Span::new("Unicode操作 🚀", Internal, unicode_span_ctx)
  Span::add_attribute(unicode_span, "操作类型", StringValue("数据处理"))
  Span::add_event(unicode_span, "操作开始", Some([
    ("用户", StringValue("张三")),
    ("位置", StringValue("📍 北京"))
  ]))
  
  // Serialize span
  let serialized_span = Span::serialize(unicode_span)
  assert_true(serialized_span.length() > 0)
  
  // Deserialize span
  let deserialized_span = Span::deserialize(serialized_span)
  
  match deserialized_span {
    Some(span) => {
      assert_eq(Span::name(span), "Unicode操作 🚀")
      
      let operation_type = Span::get_attribute(span, "操作类型")
      match operation_type {
        Some(StringValue(value)) => assert_eq(value, "数据处理")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 10: Comprehensive Unicode Integration Test
test "comprehensive unicode integration test" {
  // Create a comprehensive multilingual telemetry scenario
  
  // 1. Create Unicode context
  let unicode_trace_state = "中文状态=运行中,日本語状態=実行中,한국어상태=실행중"
  let unicode_ctx = SpanContext::new("unicode_integration_trace", "unicode_integration_span", true, unicode_trace_state)
  
  // 2. Create Unicode resource
  let unicode_resource_attrs = [
    ("service.name", StringValue("多语言服务")),
    ("サービス名", StringValue("多言語サービス")),
    ("서비스명", StringValue("다국어 서비스")),
    ("service.version", StringValue("1.0.0")),
    ("环境", StringValue("生产环境")),
    ("環境", StringValue("本番環境")),
    ("환경", StringValue("프로덕션"))
  ]
  let unicode_resource = Resource::with_attributes(Resource::new(), unicode_resource_attrs)
  
  // 3. Create Unicode span
  let unicode_span = Span::new("多语言集成测试 🌍", Server, unicode_ctx)
  Span::add_attribute(unicode_span, "操作类型", StringValue("集成测试"))
  Span::add_attribute(unicode_span, "操作タイプ", StringValue("統合テスト"))
  Span::add_attribute(unicode_span, "작업 유형", StringValue("통합 테스트"))
  
  // 4. Add Unicode events
  Span::add_event(unicode_span, "测试开始", Some([
    ("开始时间", StringValue("2023-01-01 00:00:00")),
    ("测试人员", StringValue("张三")),
    ("テスト担当者", StringValue("田中")),
    ("테스터", StringValue("김철수"))
  ]))
  
  Span::add_event(unicode_span, "数据处理中", Some([
    ("处理数据量", IntValue(1000)),
    ("処理データ量", IntValue(1000)),
    ("처리 데이터량", IntValue(1000))
  ]))
  
  // 5. Create Unicode metrics
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "多语言仪表")
  let unicode_counter = Meter::create_counter(meter, "操作计数器", Some("操作计数器描述"), Some("次"))
  
  Counter::add(unicode_counter, 1.0, Some([
    ("操作类型", StringValue("集成测试")),
    ("ユーザー", StringValue("田中")),
    ("사용자", StringValue("김철수")),
    ("ステータス", StringValue("成功")),
    ("상태", StringValue("성공"))
  ]))
  
  // 6. Create Unicode log records
  let unicode_log_attrs = Attributes::new()
  Attributes::set(unicode_log_attrs, "日志级别", StringValue("信息"))
  Attributes::set(unicode_log_attrs, "ログレベル", StringValue("情報"))
  Attributes::set(unicode_log_attrs, "로그 레벨", StringValue("정보"))
  
  let unicode_log = LogRecord::new_with_context(
    Info,
    Some("多语言集成测试日志：集成测试操作已成功完成 🎉"),
    Some(unicode_log_attrs),
    Some(1234567890L),
    Some(1234567891L),
    Some("unicode_integration_trace"),
    Some("unicode_integration_span"),
    Some(Context::root())
  )
  
  // 7. Create Unicode baggage
  let unicode_baggage = Baggage::new()
  let final_baggage = Baggage::set_entry(unicode_baggage, "用户会话", "用户123_中文会话")
  let final_baggage_ja = Baggage::set_entry(final_baggage, "ユーザーセッション", "ユーザー456_日本語セッション")
  let final_baggage_ko = Baggage::set_entry(final_baggage_ja, "사용자 세션", "사용자789_한국어 세션")
  
  // 8. Complete the span
  Span::set_status(unicode_span, Ok, Some("操作成功完成"))
  Span::add_event(unicode_span, "测试完成", Some([
    ("完成时间", StringValue("2023-01-01 01:00:00")),
    ("結果", StringValue("成功")),
    ("결과", StringValue("성공"))
  ]))
  Span::end(unicode_span)
  
  // Verify all components work correctly with Unicode
  
  // Verify span
  assert_eq(Span::name(unicode_span), "多语言集成测试 🌍")
  assert_eq(Span::status(unicode_span), Ok)
  
  let operation_type = Span::get_attribute(unicode_span, "操作类型")
  match operation_type {
    Some(StringValue(value)) => assert_eq(value, "集成测试")
    None => assert_true(false)
  }
  
  // Verify log
  assert_eq(LogRecord::severity_number(unicode_log), Info)
  match LogRecord::body(unicode_log) {
    Some(body) => assert_eq(body, "多语言集成测试日志：集成测试操作已成功完成 🎉")
    None => assert_true(false)
  }
  
  // Verify baggage
  let user_session = Baggage::get_entry(final_baggage_ko, "用户会话")
  match user_session {
    Some(value) => assert_eq(value, "用户123_中文会话")
    None => assert_true(false)
  }
  
  let user_session_ko = Baggage::get_entry(final_baggage_ko, "사용자 세션")
  match user_session_ko {
    Some(value) => assert_eq(value, "사용자789_한국어 세션")
    None => assert_true(false)
  }
  
  // Verify metrics
  let metric_data = Meter::collect(meter)
  let metrics = MetricData::metrics(metric_data)
  assert_true(metrics.length() >= 1)
  
  // Verify resource
  let service_name = Resource::get_attribute(unicode_resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "多语言服务")
    None => assert_true(false)
  }
  
  let service_name_ja = Resource::get_attribute(unicode_resource, "サービス名")
  match service_name_ja {
    Some(StringValue(name)) => assert_eq(name, "多言語サービス")
    None => assert_true(false)
  }
}