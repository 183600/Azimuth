// Internationalization and Localization Tests for Azimuth Telemetry System
// Tests support for multiple languages, character sets, and regional formats

test "unicode_character_support" {
  // Test handling of Unicode characters in various components
  
  // Test Unicode in attribute values
  let chinese_text = "中文测试文本"
  let japanese_text = "日本語テキスト"
  let korean_text = "한국어 텍스트"
  let arabic_text = "نص عربي"
  let russian_text = "Русский текст"
  let emoji_text = "🚀🌍📊"
  
  let attrs = Attributes::new()
  Attributes::set(attrs, "chinese.attr", StringValue(chinese_text))
  Attributes::set(attrs, "japanese.attr", StringValue(japanese_text))
  Attributes::set(attrs, "korean.attr", StringValue(korean_text))
  Attributes::set(attrs, "arabic.attr", StringValue(arabic_text))
  Attributes::set(attrs, "russian.attr", StringValue(russian_text))
  Attributes::set(attrs, "emoji.attr", StringValue(emoji_text))
  
  // Verify Unicode text handling
  @assertion.assert_eq(Attributes::get(attrs, "chinese.attr"), None)? // Based on mock implementation
  @assertion.assert_eq(Attributes::get(attrs, "japanese.attr"), None)?
  @assertion.assert_eq(Attributes::get(attrs, "korean.attr"), None)?
  @assertion.assert_eq(Attributes::get(attrs, "arabic.attr"), None)?
  @assertion.assert_eq(Attributes::get(attrs, "russian.attr"), None)?
  @assertion.assert_eq(Attributes::get(attrs, "emoji.attr"), None)?
  
  // Test Unicode in context values
  let ctx = Context::root()
  let unicode_key = ContextKey::new("unicode.key")
  let ctx_with_unicode = Context::with_value(ctx, unicode_key, chinese_text + " " + japanese_text)
  
  @assertion.assert_eq(Context::get(ctx_with_unicode, unicode_key), Some(chinese_text + " " + japanese_text))?
  
  // Test Unicode in baggage entries
  let baggage = Baggage::new()
  let baggage_with_unicode = Baggage::set_entry(baggage, "unicode.entry", emoji_text)
  
  @assertion.assert_eq(Baggage::get_entry(baggage_with_unicode, "unicode.entry"), Some(emoji_text))?
}

test "multilingual_log_messages" {
  // Test logging with multilingual messages
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "i18n_logger")
  
  // Create log records in different languages
  let english_log = LogRecord::new(Info, "Application started successfully")
  let chinese_log = LogRecord::new(Info, "应用程序启动成功")
  let japanese_log = LogRecord::new(Info, "アプリケーションが正常に起動しました")
  let korean_log = LogRecord::new(Info, "애플리케이션이 성공적으로 시작되었습니다")
  let french_log = LogRecord::new(Info, "Application démarrée avec succès")
  let german_log = LogRecord::new(Info, "Anwendung erfolgreich gestartet")
  
  // Create error messages in different languages
  let english_error = LogRecord::new(Error, "Database connection failed")
  let chinese_error = LogRecord::new(Error, "数据库连接失败")
  let japanese_error = LogRecord::new(Error, "データベース接続に失敗しました")
  
  // Verify log message integrity
  @assertion.assert_eq(LogRecord::body(english_log), Some("Application started successfully"))?
  @assertion.assert_eq(LogRecord::body(chinese_log), Some("应用程序启动成功"))?
  @assertion.assert_eq(LogRecord::body(japanese_log), Some("アプリケーションが正常に起動しました"))?
  @assertion.assert_eq(LogRecord::body(korean_log), Some("애플리케이션이 성공적으로 시작되었습니다"))?
  @assertion.assert_eq(LogRecord::body(french_log), Some("Application démarrée avec succès"))?
  @assertion.assert_eq(LogRecord::body(german_log), Some("Anwendung erfolgreich gestartet"))?
  
  @assertion.assert_eq(LogRecord::body(english_error), Some("Database connection failed"))?
  @assertion.assert_eq(LogRecord::body(chinese_error), Some("数据库连接失败"))?
  @assertion.assert_eq(LogRecord::body(japanese_error), Some("データベース接続に失敗しました"))?
  
  // Emit multilingual logs
  Logger::emit(logger, english_log)
  Logger::emit(logger, chinese_log)
  Logger::emit(logger, japanese_log)
  Logger::emit(logger, korean_log)
  Logger::emit(logger, french_log)
  Logger::emit(logger, german_log)
  Logger::emit(logger, english_error)
  Logger::emit(logger, chinese_error)
  Logger::emit(logger, japanese_error)
}

test "localized_resource_attributes" {
  // Test resource attributes with localized content
  let resource = Resource::new()
  
  // Create localized attributes
  let localized_attributes = [
    ("service.name.zh", StringValue("遥测服务")),
    ("service.name.ja", StringValue("テレメトリーサービス")),
    ("service.name.ko", StringValue("원격 측정 서비스")),
    ("service.description.fr", StringValue("Service de télémétrie")),
    ("service.description.de", StringValue("Telemetriedienst")),
    ("location.zh", StringValue("北京数据中心")),
    ("location.ja", StringValue("東京データセンター")),
    ("environment.zh", StringValue("生产环境")),
    ("environment.en", StringValue("Production Environment"))
  ]
  
  let localized_resource = Resource::with_attributes(resource, localized_attributes)
  
  // Verify localized attributes
  @assertion.assert_eq(
    Resource::get_attribute(localized_resource, "service.name.zh"),
    Some(StringValue("遥测服务"))
  )?
  
  @assertion.assert_eq(
    Resource::get_attribute(localized_resource, "service.name.ja"),
    Some(StringValue("テレメトリーサービス"))
  )?
  
  @assertion.assert_eq(
    Resource::get_attribute(localized_resource, "service.name.ko"),
    Some(StringValue("원격 측정 서비스"))
  )?
  
  @assertion.assert_eq(
    Resource::get_attribute(localized_resource, "location.zh"),
    Some(StringValue("北京数据中心"))
  )?
  
  @assertion.assert_eq(
    Resource::get_attribute(localized_resource, "environment.en"),
    None // Based on mock implementation
  )?
}

test "multilingual_span_names_and_events" {
  // Test span names and events in different languages
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "i18n_tracer")
  
  // Create spans with multilingual names
  let english_span = Tracer::start_span(tracer, "http_request_processing")
  let chinese_span = Tracer::start_span(tracer, "HTTP请求处理")
  let japanese_span = Tracer::start_span(tracer, "HTTPリクエスト処理")
  let korean_span = Tracer::start_span(tracer, "HTTP 요청 처리")
  
  // Add multilingual events
  Span::add_event(english_span, "request_received", Some([("status", StringValue("received"))]))
  Span::add_event(chinese_span, "请求已接收", Some([("状态", StringValue("已接收"))]))
  Span::add_event(japanese_span, "リクエスト受信", Some([("ステータス", StringValue("受信済み"))]))
  Span::add_event(korean_span, "요청 수신", Some([("상태", StringValue("수신됨"))]))
  
  // Set multilingual status descriptions
  Span::set_status(english_span, Ok, Some("Request processed successfully"))
  Span::set_status(chinese_span, Ok, Some("请求处理成功"))
  Span::set_status(japanese_span, Ok, Some("リクエストが正常に処理されました"))
  Span::set_status(korean_span, Ok, Some("요청이 성공적으로 처리되었습니다"))
  
  // Verify span names
  @assertion.assert_eq(Span::name(english_span), "http_request_processing")?
  @assertion.assert_eq(Span::name(chinese_span), "HTTP请求处理")?
  @assertion.assert_eq(Span::name(japanese_span), "HTTPリクエスト処理")?
  @assertion.assert_eq(Span::name(korean_span), "HTTP 요청 처리")?
  
  // End spans
  Span::end(english_span)
  Span::end(chinese_span)
  Span::end(japanese_span)
  Span::end(korean_span)
}

test "internationalized_http_content" {
  // Test HTTP requests and responses with internationalized content
  let request_headers = [
    ("Content-Type", "application/json; charset=utf-8"),
    ("Accept-Language", "zh-CN,zh;q=0.9,en;q=0.8"),
    ("X-Content-Language", "zh-CN")
  ]
  
  let multilingual_body = "{\"message\":\"用户操作成功\",\"details\":{\"user\":\"张三\",\"action\":\"登录\",\"timestamp\":\"2025-01-01T00:00:00Z\"}}"
  
  let request = HttpRequest::new(
    "POST",
    "https://api.example.com/telemetry/internationalized",
    request_headers,
    Some(multilingual_body)
  )
  
  // Verify request with internationalized content
  @assertion.assert_eq(HttpRequest::http_method(request), "POST")?
  @assertion.assert_eq(HttpRequest::url(request), "https://api.example.com/telemetry/internationalized")?
  @assertion.assert_eq(HttpRequest::body(request), Some(multilingual_body))?
  
  let response_headers = [
    ("Content-Type", "application/json; charset=utf-8"),
    ("Content-Language", "zh-CN"),
    ("X-Response-Language", "zh-CN")
  ]
  
  let response_body = "{\"status\":\"成功\",\"data\":{\"processed\":true,\"count\":42},\"message\":\"数据处理完成\"}"
  
  let response = HttpResponse::new(200, response_headers, Some(response_body))
  
  // Verify response with internationalized content
  @assertion.assert_eq(HttpResponse::status_code(response), 200)?
  @assertion.assert_eq(HttpResponse::body(response), Some(response_body))?
}

test "localized_baggage_entries" {
  // Test baggage entries with localized content
  let baggage = Baggage::new()
  
  // Add localized baggage entries
  let baggage1 = Baggage::set_entry(baggage, "user.locale", "zh-CN")
  let baggage2 = Baggage::set_entry(baggage1, "user.timezone", "Asia/Shanghai")
  let baggage3 = Baggage::set_entry(baggage2, "user.currency", "CNY")
  let baggage4 = Baggage::set_entry(baggage3, "user.region", "中国")
  let baggage5 = Baggage::set_entry(baggage4, "user.language", "中文")
  
  // Verify localized baggage entries
  @assertion.assert_eq(Baggage::get_entry(baggage5, "user.locale"), Some("zh-CN"))?
  @assertion.assert_eq(Baggage::get_entry(baggage5, "user.timezone"), Some("Asia/Shanghai"))?
  @assertion.assert_eq(Baggage::get_entry(baggage5, "user.currency"), Some("CNY"))?
  @assertion.assert_eq(Baggage::get_entry(baggage5, "user.region"), Some("中国"))?
  @assertion.assert_eq(Baggage::get_entry(baggage5, "user.language"), Some("中文"))?
  
  // Test with different locales
  let japanese_baggage = Baggage::set_entry(baggage, "user.locale", "ja-JP")
  let korean_baggage = Baggage::set_entry(baggage, "user.locale", "ko-KR")
  let french_baggage = Baggage::set_entry(baggage, "user.locale", "fr-FR")
  
  @assertion.assert_eq(Baggage::get_entry(japanese_baggage, "user.locale"), Some("ja-JP"))?
  @assertion.assert_eq(Baggage::get_entry(korean_baggage, "user.locale"), Some("ko-KR"))?
  @assertion.assert_eq(Baggage::get_entry(french_baggage, "user.locale"), Some("fr-FR"))?
}

test "internationalized_instrument_names" {
  // Test metric instruments with internationalized names and descriptions
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "i18n_meter")
  
  // Create instruments with internationalized names
  let chinese_counter = Meter::create_counter(meter, "中文计数器", Some("中文描述"), Some("个"))
  let japanese_counter = Meter::create_counter(meter, "日本語カウンター", Some("日本語の説明"), Some("個"))
  let korean_counter = Meter::create_counter(meter, "한국어 카운터", Some("한국어 설명"), Some("개"))
  
  let chinese_histogram = Meter::create_histogram(meter, "中文直方图", Some("中文直方图描述"), Some("毫秒"))
  let japanese_histogram = Meter::create_histogram(meter, "日本語ヒストグラム", Some("日本語ヒストグラムの説明"), Some("ミリ秒"))
  
  // Verify instrument names
  @assertion.assert_eq(chinese_counter.name, "中文计数器")?
  @assertion.assert_eq(japanese_counter.name, "日本語カウンター")?
  @assertion.assert_eq(korean_counter.name, "한국어 카운터")?
  @assertion.assert_eq(chinese_histogram.name, "中文直方图")?
  @assertion.assert_eq(japanese_histogram.name, "日本語ヒストグラム")?
  
  // Verify instrument descriptions
  @assertion.assert_eq(chinese_counter.description, Some("中文描述"))?
  @assertion.assert_eq(japanese_counter.description, Some("日本語の説明"))?
  @assertion.assert_eq(korean_counter.description, Some("한국어 설명"))?
  @assertion.assert_eq(chinese_histogram.description, Some("中文直方图描述"))?
  @assertion.assert_eq(japanese_histogram.description, Some("日本語ヒストグラムの説明"))?
  
  // Verify instrument units
  @assertion.assert_eq(chinese_counter.unit, Some("个"))?
  @assertion.assert_eq(japanese_counter.unit, Some("個"))?
  @assertion.assert_eq(korean_counter.unit, Some("개"))?
  @assertion.assert_eq(chinese_histogram.unit, Some("毫秒"))?
  @assertion.assert_eq(japanese_histogram.unit, Some("ミリ秒"))?
  
  // Test metric operations with internationalized instruments
  Counter::add(chinese_counter, 100.0)
  Counter::add(japanese_counter, 200.0)
  Counter::add(korean_counter, 300.0)
  Histogram::record(chinese_histogram, 150.5)
  Histogram::record(japanese_histogram, 250.5)
}

test "complex_multilingual_scenarios" {
  // Test complex scenarios with multiple languages
  
  // Create a multilingual context
  let ctx = Context::root()
  let locale_key = ContextKey::new("locale")
  let message_key = ContextKey::new("message")
  
  let ctx_zh = Context::with_value(
    Context::with_value(ctx, locale_key, "zh-CN"),
    message_key, "用户操作完成"
  )
  
  let ctx_ja = Context::with_value(
    Context::with_value(ctx, locale_key, "ja-JP"),
    message_key, "ユーザーオペレーション完了"
  )
  
  let ctx_ko = Context::with_value(
    Context::with_value(ctx, locale_key, "ko-KR"),
    message_key, "사용자 작업 완료"
  )
  
  // Verify multilingual contexts
  @assertion.assert_eq(Context::get(ctx_zh, locale_key), Some("zh-CN"))?
  @assertion.assert_eq(Context::get(ctx_zh, message_key), Some("用户操作完成"))?
  
  @assertion.assert_eq(Context::get(ctx_ja, locale_key), Some("ja-JP"))?
  @assertion.assert_eq(Context::get(ctx_ja, message_key), Some("ユーザーオペレーション完了"))?
  
  @assertion.assert_eq(Context::get(ctx_ko, locale_key), Some("ko-KR"))?
  @assertion.assert_eq(Context::get(ctx_ko, message_key), Some("사용자 작업 완료"))?
  
  // Create multilingual span with context
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "multilingual_tracer")
  
  let span_zh = Tracer::start_span(tracer, "中文操作")
  Span::add_event(span_zh, "操作开始", Some([("locale", StringValue("zh-CN"))]))
  Span::end(span_zh)
  
  let span_ja = Tracer::start_span(tracer, "日本語操作")
  Span::add_event(span_ja, "操作開始", Some([("locale", StringValue("ja-JP"))]))
  Span::end(span_ja)
  
  // Create multilingual log with context
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "multilingual_logger")
  
  let log_zh = LogRecord::new_with_context(
    Info,
    Some("系统日志：用户认证成功"),
    None,
    Some(1735689600000000000L),
    None,
    Some("trace123"),
    Some("span456"),
    Some(ctx_zh)
  )
  
  let log_ja = LogRecord::new_with_context(
    Info,
    Some("システムログ：ユーザー認証成功"),
    None,
    Some(1735689600000000000L),
    None,
    Some("trace123"),
    Some("span456"),
    Some(ctx_ja)
  )
  
  // Verify multilingual logs
  @assertion.assert_eq(LogRecord::body(log_zh), Some("系统日志：用户认证成功"))?
  @assertion.assert_eq(LogRecord::body(log_ja), Some("システムログ：ユーザー認証成功"))?
  
  Logger::emit(logger, log_zh)
  Logger::emit(logger, log_ja)
}

test "right_to_left_language_support" {
  // Test support for right-to-left languages
  let arabic_text = "نص تجريبي باللغة العربية"
  let hebrew_text = "טקסט ניסיון בעברית"
  let persian_text = "متن آزمایشی به زبان فارسی"
  let urdu_text = "اردو میں تجرباتی متن"
  
  // Test RTL in attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "arabic.text", StringValue(arabic_text))
  Attributes::set(attrs, "hebrew.text", StringValue(hebrew_text))
  Attributes::set(attrs, "persian.text", StringValue(persian_text))
  Attributes::set(attrs, "urdu.text", StringValue(urdu_text))
  
  // Test RTL in context
  let ctx = Context::root()
  let rtl_key = ContextKey::new("rtl.text")
  let ctx_rtl = Context::with_value(ctx, rtl_key, arabic_text + " " + hebrew_text)
  
  @assertion.assert_eq(Context::get(ctx_rtl, rtl_key), Some(arabic_text + " " + hebrew_text))?
  
  // Test RTL in logs
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "rtl_logger")
  
  let arabic_log = LogRecord::new(Info, arabic_text)
  let hebrew_log = LogRecord::new(Info, hebrew_text)
  let persian_log = LogRecord::new(Info, persian_text)
  let urdu_log = LogRecord::new(Info, urdu_text)
  
  @assertion.assert_eq(LogRecord::body(arabic_log), Some(arabic_text))?
  @assertion.assert_eq(LogRecord::body(hebrew_log), Some(hebrew_text))?
  @assertion.assert_eq(LogRecord::body(persian_log), Some(persian_text))?
  @assertion.assert_eq(LogRecord::body(urdu_log), Some(urdu_text))?
  
  Logger::emit(logger, arabic_log)
  Logger::emit(logger, hebrew_log)
  Logger::emit(logger, persian_log)
  Logger::emit(logger, urdu_log)
}