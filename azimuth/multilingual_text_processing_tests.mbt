// Multilingual Text Processing Tests
// 测试多语言文本处理和国际化的功能

test "unicode_character_handling" {
  // Unicode字符处理测试
  
  let tracer = azimuth.Tracer.new("国际化测试服务")
  let span = azimuth.Tracer.start_span(tracer, "多语言操作")
  
  // 测试各种Unicode字符
  let chinese_text = "中文测试文本"
  let japanese_text = "日本語テキスト"
  let korean_text = "한국어 텍스트"
  let arabic_text = "نص عربي"
  let russian_text = "Русский текст"
  let emoji_text = "🌟🚀💻📊"
  let mixed_text = "中文 English 日本語 🌟 العربية"
  
  // 设置多语言属性
  azimuth.Span.set_attribute_string(span, "chinese_message", chinese_text)
  azimuth.Span.set_attribute_string(span, "japanese_message", japanese_text)
  azimuth.Span.set_attribute_string(span, "korean_message", korean_text)
  azimuth.Span.set_attribute_string(span, "arabic_message", arabic_text)
  azimuth.Span.set_attribute_string(span, "russian_message", russian_text)
  azimuth.Span.set_attribute_string(span, "emoji_message", emoji_text)
  azimuth.Span.set_attribute_string(span, "mixed_message", mixed_text)
  
  // 验证Unicode字符长度计算
  let chinese_len = @unsafe.String.length(chinese_text)
  let japanese_len = @unsafe.String.length(japanese_text)
  let emoji_len = @unsafe.String.length(emoji_text)
  
  @assertion.assert_equal(chinese_len, 6)
  @assertion.assert_equal(japanese_len, 7)
  @assertion.assert_equal(emoji_len, 4)
  
  // 验证属性存储和检索
  let attributes = azimuth.Span.get_attributes(span)
  
  match azimuth.Attributes.get_string(attributes, "chinese_message") {
    Some(v) => @assertion.assert_equal(v, chinese_text)
    _ => @assertion.assert_true(false)
  }
  
  match azimuth.Attributes.get_string(attributes, "mixed_message") {
    Some(v) => @assertion.assert_equal(v, mixed_text)
    _ => @assertion.assert_true(false)
  }
}

test "multilingual_error_messages" {
  // 多语言错误消息测试
  
  let tracer = azimuth.Tracer.new("多语言错误处理服务")
  let span = azimuth.Tracer.start_span(tracer, "多语言错误处理")
  
  // 创建不同语言的错误消息
  let error_messages = [
    ("zh", "操作失败：数据库连接超时"),
    ("en", "Operation failed: Database connection timeout"),
    ("ja", "操作失敗：データベース接続タイムアウト"),
    ("ko", "작업 실패: 데이터베이스 연결 시간 초과"),
    ("ar", "فشلت العملية: انتهت مهلة الاتصال بقاعدة البيانات"),
    ("ru", "Операция не удалась: тайм-аут подключения к базе данных")
  ]
  
  // 添加多语言错误事件
  for (lang, message) in error_messages {
    azimuth.Span.add_event_with_attributes(span, "error_occurred", [
      ("language", azimuth.AttributeValue.StringValue(lang)),
      ("error_message", azimuth.AttributeValue.StringValue(message)),
      ("error_code", azimuth.AttributeValue.IntValue(500))
    ])
  }
  
  // 验证多语言错误事件
  let events = azimuth.Span.get_events(span)
  @assertion.assert_equal(@unsafe.Array.length(events), 6)
  
  // 验证特定语言的错误消息
  for i = 0; i < @unsafe.Array.length(events); i + 1 {
    let event = @unsafe.Array.get(events, i)
    let event_attrs = azimuth.Event.get_attributes(event)
    let (expected_lang, expected_message) = @unsafe.Array.get(error_messages, i)
    
    match azimuth.Attributes.get_string(event_attrs, "language") {
      Some(v) => @assertion.assert_equal(v, expected_lang)
      _ => @assertion.assert_true(false)
    }
    
    match azimuth.Attributes.get_string(event_attrs, "error_message") {
      Some(v) => @assertion.assert_equal(v, expected_message)
      _ => @assertion.assert_true(false)
    }
  }
}

test "localized_logging_messages" {
  // 本地化日志消息测试
  
  let logger_provider = azimuth.LoggerProvider.new()
  let logger = azimuth.LoggerProvider.get_logger(logger_provider, "多语言日志服务", "1.0.0")
  
  // 创建不同语言的日志记录器
  let log_messages = [
    ("zh", "用户登录成功", "INFO"),
    ("en", "User login successful", "INFO"),
    ("ja", "ユーザーログイン成功", "INFO"),
    ("ko", "사용자 로그인 성공", "INFO"),
    ("zh", "数据库连接失败", "ERROR"),
    ("en", "Database connection failed", "ERROR"),
    ("ja", "データベース接続失敗", "ERROR"),
    ("ko", "데이터베이스 연결 실패", "ERROR")
  ]
  
  // 记录多语言日志
  for (lang, message, level) in log_messages {
    let log_record = azimuth.Logger.emit_log_with_attributes(logger, [
      ("message", azimuth.AttributeValue.StringValue(message)),
      ("language", azimuth.AttributeValue.StringValue(lang)),
      ("level", azimuth.AttributeValue.StringValue(level)),
      ("timestamp", azimuth.AttributeValue.IntValue(@unsafe.Time.now()))
    ])
    
    @assertion.assert_true(log_record)
  }
  
  // 验证日志记录数量
  let logs = azimuth.Logger.get_logs(logger)
  @assertion.assert_equal(@unsafe.Array.length(logs), 8)
  
  // 验证中文日志消息
  let chinese_logs = @unsafe.Array.filter(logs, fn(log) {
    let attrs = azimuth.LogRecord.get_attributes(log)
    match azimuth.Attributes.get_string(attrs, "language") {
      Some(v) => v == "zh"
      None => false
    }
  })
  
  @assertion.assert_equal(@unsafe.Array.length(chinese_logs), 2)
}

test "multilingual_baggage_propagation" {
  // 多语言Baggage传播测试
  
  let parent_context = azimuth.Context.new()
  
  // 添加多语言Baggage项
  let multilingual_baggage = [
    ("user_locale", "zh-CN"),
    ("preferred_language", "中文"),
    ("service_name", "用户服务"),
    ("operation_name", "データ処理"), // 日文
    ("error_message", "Operación fallida"), // 西班牙文
    ("custom_field", "Пользовательское поле") // 俄文
  ]
  
  for (key, value) in multilingual_baggage {
    azimuth.Context.set_baggage(parent_context, key, value)
  }
  
  // 创建子上下文并验证Baggage传播
  let child_context = azimuth.Context.copy(parent_context)
  azimuth.Context.set_baggage(child_context, "child_locale", "en-US")
  
  // 验证父Baggage在子上下文中存在
  for (key, expected_value) in multilingual_baggage {
    match azimuth.Context.get_baggage(child_context, key) {
      Some(actual_value) => @assertion.assert_equal(actual_value, expected_value)
      None => @assertion.assert_true(false)
    }
  }
  
  // 验证子上下文特定的Baggage
  match azimuth.Context.get_baggage(child_context, "child_locale") {
    Some(v) => @assertion.assert_equal(v, "en-US")
    None => @assertion.assert_true(false)
  }
}

test "internationalization_time_formatting" {
  // 国际化时间格式测试
  
  let tracer = azimuth.Tracer.new("国际化时间服务")
  let span = azimuth.Tracer.start_span(tracer, "多语言时间处理")
  
  // 获取当前时间戳
  let timestamp = @unsafe.Time.now()
  
  // 创建不同地区的时间格式
  let time_formats = [
    ("zh-CN", "2024-01-15 14:30:25 CST"),
    ("en-US", "01/15/2024 02:30:25 PM EST"),
    ("ja-JP", "2024/01/15 14:30:25 JST"),
    ("ko-KR", "2024. 01. 15. 오후 2:30:25 KST"),
    ("ar-SA", "١٥/٠١/٢٠٢٤ ٠٢:٣٠:٢٥ م AST"),
    ("ru-RU", "15.01.2024 14:30:25 MSK")
  ]
  
  // 添加多语言时间格式事件
  for (locale, formatted_time) in time_formats {
    azimuth.Span.add_event_with_attributes(span, "time_formatted", [
      ("locale", azimuth.AttributeValue.StringValue(locale)),
      ("formatted_time", azimuth.AttributeValue.StringValue(formatted_time)),
      ("timestamp", azimuth.AttributeValue.IntValue(timestamp))
    ])
  }
  
  // 验证时间格式事件
  let events = azimuth.Span.get_events(span)
  @assertion.assert_equal(@unsafe.Array.length(events), 6)
  
  // 验证特定地区的时间格式
  for i = 0; i < @unsafe.Array.length(events); i + 1 {
    let event = @unsafe.Array.get(events, i)
    let event_attrs = azimuth.Event.get_attributes(event)
    let (expected_locale, expected_time) = @unsafe.Array.get(time_formats, i)
    
    match azimuth.Attributes.get_string(event_attrs, "locale") {
      Some(v) => @assertion.assert_equal(v, expected_locale)
      None => @assertion.assert_true(false)
    }
    
    match azimuth.Attributes.get_string(event_attrs, "formatted_time") {
      Some(v) => @assertion.assert_equal(v, expected_time)
      None => @assertion.assert_true(false)
    }
  }
}

test "multilingual_resource_attributes" {
  // 多语言资源属性测试
  
  // 创建包含多语言信息的资源
  let resource = azimuth.Resource.new([
    ("service.name", azimuth.AttributeValue.StringValue("多语言遥测服务")),
    ("service.description", azimuth.AttributeValue.StringValue("Multilingual Telemetry Service")),
    ("service.version", azimuth.AttributeValue.StringValue("1.0.0")),
    ("service.owner", azimuth.AttributeValue.StringValue("开发团队")),
    ("service.owner_en", azimuth.AttributeValue.StringValue("Development Team")),
    ("deployment.region", azimuth.AttributeValue.StringValue("亚太地区")),
    ("deployment.region_en", azimuth.AttributeValue.StringValue("Asia Pacific")),
    ("support.contact", azimuth.AttributeValue.StringValue("技术支持团队")),
    ("support.contact_en", azimuth.AttributeValue.StringValue("Technical Support Team"))
  ])
  
  // 验证多语言资源属性
  let attributes = azimuth.Resource.get_attributes(resource)
  
  // 验证中文属性
  match azimuth.Attributes.get_string(attributes, "service.name") {
    Some(v) => @assertion.assert_equal(v, "多语言遥测服务")
    None => @assertion.assert_true(false)
  }
  
  match azimuth.Attributes.get_string(attributes, "service.owner") {
    Some(v) => @assertion.assert_equal(v, "开发团队")
    None => @assertion.assert_true(false)
  }
  
  // 验证英文属性
  match azimuth.Attributes.get_string(attributes, "service.description") {
    Some(v) => @assertion.assert_equal(v, "Multilingual Telemetry Service")
    None => @assertion.assert_true(false)
  }
  
  match azimuth.Attributes.get_string(attributes, "service.owner_en") {
    Some(v) => @assertion.assert_equal(v, "Development Team")
    None => @assertion.assert_true(false)
  }
  
  // 验证属性总数
  @assertion.assert_equal(azimuth.Attributes.len(attributes), 9)
}

test "text_encoding_conversion" {
  // 文本编码转换测试
  
  let tracer = azimuth.Tracer.new("编码转换服务")
  let span = azimuth.Tracer.start_span(tracer, "多语言编码处理")
  
  // 测试不同编码的文本
  let utf8_texts = [
    "简体中文：你好世界",
    "繁體中文：你好世界",
    "日本語：こんにちは世界",
    "한국어: 안녕하세요 세계",
    "العربية: مرحبا بالعالم",
    "Русский: Привет мир",
    "English: Hello World",
    "Español: Hola Mundo",
    "Français: Bonjour le monde",
    "Deutsch: Hallo Welt",
    "🌍🌎🌏🌐🌑🌒🌓🌔🌕🌖🌗🌘"
  ]
  
  // 处理每个文本并记录
  for text in utf8_texts {
    // 模拟文本处理操作
    let processed_text = text // 在实际应用中这里可能有编码转换
    
    azimuth.Span.add_event_with_attributes(span, "text_processed", [
      ("original_text", azimuth.AttributeValue.StringValue(text)),
      ("processed_text", azimuth.AttributeValue.StringValue(processed_text)),
      ("text_length", azimuth.AttributeValue.IntValue(@unsafe.String.length(text))),
      ("encoding", azimuth.AttributeValue.StringValue("UTF-8"))
    ])
  }
  
  // 验证文本处理事件
  let events = azimuth.Span.get_events(span)
  @assertion.assert_equal(@unsafe.Array.length(events), 12)
  
  // 验证特定文本的处理
  let emoji_event = @unsafe.Array.get(events, 11) // 最后一个emoji事件
  let emoji_attrs = azimuth.Event.get_attributes(emoji_event)
  
  match azimuth.Attributes.get_string(emoji_attrs, "original_text") {
    Some(v) => @assertion.assert_equal(v, "🌍🌎🌏🌐🌑🌒🌓🌔🌕🌖🌗🌘")
    None => @assertion.assert_true(false)
  }
  
  match azimuth.Attributes.get_int(emoji_attrs, "text_length") {
    Some(v) => @assertion.assert_equal(v, 12)
    None => @assertion.assert_true(false)
  }
}