// Internationalization Support Tests
// 验证遥测系统的国际化支持能力

test "unicode attribute handling" {
  // 测试Unicode属性处理
  let attributes = azimuth::Attributes::new()
  
  // 设置各种语言的Unicode属性
  azimuth::Attributes::set(attributes, "chinese.name", azimuth::StringValue("张三"))
  azimuth::Attributes::set(attributes, "japanese.name", azimuth::StringValue("田中太郎"))
  azimuth::Attributes::set(attributes, "korean.name", azimuth::StringValue("김철수"))
  azimuth::Attributes::set(attributes, "arabic.name", azimuth::StringValue("أحمد محمد"))
  azimuth::Attributes::set(attributes, "russian.name", azimuth::StringValue("Иван Петров"))
  azimuth::Attributes::set(attributes, "hebrew.name", azimuth::StringValue("דוד כהן"))
  azimuth::Attributes::set(attributes, "thai.name", azimuth::StringValue("สมชาย ใจดี"))
  
  // 测试表情符号
  azimuth::Attributes::set(attributes, "emoji.test", azimuth::StringValue("😀🎉🔥🚀💯"))
  
  // 测试特殊Unicode字符
  azimuth::Attributes::set(attributes, "special.chars", azimuth::StringValue("™®©€£¥§¶†‡•…‰‹›""''–—"))
  
  // 验证属性正确存储和检索
  let chinese_name = azimuth::Attributes::get(attributes, "chinese.name")
  let japanese_name = azimuth::Attributes::get(attributes, "japanese.name")
  let korean_name = azimuth::Attributes::get(attributes, "korean.name")
  let arabic_name = azimuth::Attributes::get(attributes, "arabic.name")
  let russian_name = azimuth::Attributes::get(attributes, "russian.name")
  let hebrew_name = azimuth::Attributes::get(attributes, "hebrew.name")
  let thai_name = azimuth::Attributes::get(attributes, "thai.name")
  let emoji_test = azimuth::Attributes::get(attributes, "emoji.test")
  let special_chars = azimuth::Attributes::get(attributes, "special.chars")
  
  assert_eq(chinese_name, Some(azimuth::StringValue("张三")))
  assert_eq(japanese_name, Some(azimuth::StringValue("田中太郎")))
  assert_eq(korean_name, Some(azimuth::StringValue("김철수")))
  assert_eq(arabic_name, Some(azimuth::StringValue("أحمد محمد")))
  assert_eq(russian_name, Some(azimuth::StringValue("Иван Петров")))
  assert_eq(hebrew_name, Some(azimuth::StringValue("דוד כהן")))
  assert_eq(thai_name, Some(azimuth::StringValue("สมชาย ใจดี")))
  assert_eq(emoji_test, Some(azimuth::StringValue("😀🎉🔥🚀💯")))
  assert_eq(special_chars, Some(azimuth::StringValue("™®©€£¥§¶†‡•…‰‹›""''–—")))
}

test "localized log messages" {
  // 测试本地化日志消息
  let provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(provider, "internationalization-test-logger")
  
  // 创建不同语言的日志记录
  let chinese_log = azimuth::Logger::create_log_record(logger)
  azimuth::LogRecord::set_body(chinese_log, "用户登录成功，用户ID: 12345")
  azimuth::LogRecord::set_severity(chinese_log, azimuth::INFO)
  azimuth::LogRecord::set_attribute(chinese_log, "locale", azimuth::StringValue("zh-CN"))
  
  let japanese_log = azimuth::Logger::create_log_record(logger)
  azimuth::LogRecord::set_body(japanese_log, "ユーザーログイン成功、ユーザーID: 12345")
  azimuth::LogRecord::set_severity(japanese_log, azimuth::INFO)
  azimuth::LogRecord::set_attribute(japanese_log, "locale", azimuth::StringValue("ja-JP"))
  
  let korean_log = azimuth::Logger::create_log_record(logger)
  azimuth::LogRecord::set_body(korean_log, "사용자 로그인 성공, 사용자 ID: 12345")
  azimuth::LogRecord::set_severity(korean_log, azimuth::INFO)
  azimuth::LogRecord::set_attribute(korean_log, "locale", azimuth::StringValue("ko-KR"))
  
  let english_log = azimuth::Logger::create_log_record(logger)
  azimuth::LogRecord::set_body(english_log, "User login successful, user ID: 12345")
  azimuth::LogRecord::set_severity(english_log, azimuth::INFO)
  azimuth::LogRecord::set_attribute(english_log, "locale", azimuth::StringValue("en-US"))
  
  // 验证本地化消息正确存储
  assert_eq(azimuth::LogRecord::body(chinese_log), "用户登录成功，用户ID: 12345")
  assert_eq(azimuth::LogRecord::body(japanese_log), "ユーザーログイン成功、ユーザーID: 12345")
  assert_eq(azimuth::LogRecord::body(korean_log), "사용자 로그인 성공, 사용자 ID: 12345")
  assert_eq(azimuth::LogRecord::body(english_log), "User login successful, user ID: 12345")
  
  // 验证locale属性
  let chinese_locale = azimuth::LogRecord::get_attribute(chinese_log, "locale")
  let japanese_locale = azimuth::LogRecord::get_attribute(japanese_log, "locale")
  let korean_locale = azimuth::LogRecord::get_attribute(korean_log, "locale")
  let english_locale = azimuth::LogRecord::get_attribute(english_log, "locale")
  
  assert_eq(chinese_locale, Some(azimuth::StringValue("zh-CN")))
  assert_eq(japanese_locale, Some(azimuth::StringValue("ja-JP")))
  assert_eq(korean_locale, Some(azimuth::StringValue("ko-KR")))
  assert_eq(english_locale, Some(azimuth::StringValue("en-US")))
}

test "localized span names and descriptions" {
  // 测试本地化span名称和描述
  let span_ctx = azimuth::SpanContext::new("i18n-trace", "i18n-span", true, "")
  
  // 创建不同语言的span
  let chinese_span = azimuth::Span::new("用户认证", azimuth::Server, span_ctx)
  azimuth::Span::set_attribute(chinese_span, "span.description", azimuth::StringValue("验证用户身份和权限"))
  azimuth::Span::set_attribute(chinese_span, "locale", azimuth::StringValue("zh-CN"))
  
  let japanese_span = azimuth::Span::new("ユーザー認証", azimuth::Server, span_ctx)
  azimuth::Span::set_attribute(japanese_span, "span.description", azimuth::StringValue("ユーザーの身元と権限を検証"))
  azimuth::Span::set_attribute(japanese_span, "locale", azimuth::StringValue("ja-JP"))
  
  let spanish_span = azimuth::Span::new("Autenticación de usuario", azimuth::Server, span_ctx)
  azimuth::Span::set_attribute(spanish_span, "span.description", azimuth::StringValue("Verificar identidad y permisos del usuario"))
  azimuth::Span::set_attribute(spanish_span, "locale", azimuth::StringValue("es-ES"))
  
  // 验证本地化span名称和描述
  assert_eq(azimuth::Span::name(chinese_span), "用户认证")
  assert_eq(azimuth::Span::name(japanese_span), "ユーザー認証")
  assert_eq(azimuth::Span::name(spanish_span), "Autenticación de usuario")
  
  let chinese_desc = azimuth::Span::get_attribute(chinese_span, "span.description")
  let japanese_desc = azimuth::Span::get_attribute(japanese_span, "span.description")
  let spanish_desc = azimuth::Span::get_attribute(spanish_span, "span.description")
  
  assert_eq(chinese_desc, Some(azimuth::StringValue("验证用户身份和权限")))
  assert_eq(japanese_desc, Some(azimuth::StringValue("ユーザーの身元と権限を検証")))
  assert_eq(spanish_desc, Some(azimuth::StringValue("Verificar identidad y permisos del usuario")))
}

test "right-to-left text handling" {
  // 测试从右到左的文本处理
  let attributes = azimuth::Attributes::new()
  
  // 阿拉伯语（从右到左）
  azimuth::Attributes::set(attributes, "arabic.message", azimuth::StringValue("مرحبا بالعالم"))
  azimuth::Attributes::set(attributes, "hebrew.message", azimuth::StringValue("שלום עולם"))
  
  // 混合方向文本
  azimuth::Attributes::set(attributes, "mixed.direction", azimuth::StringValue("Hello مرحبا World"))
  
  // 验证RTL文本正确存储
  let arabic_message = azimuth::Attributes::get(attributes, "arabic.message")
  let hebrew_message = azimuth::Attributes::get(attributes, "hebrew.message")
  let mixed_direction = azimuth::Attributes::get(attributes, "mixed.direction")
  
  assert_eq(arabic_message, Some(azimuth::StringValue("مرحبا بالعالم")))
  assert_eq(hebrew_message, Some(azimuth::StringValue("שלום עולם")))
  assert_eq(mixed_direction, Some(azimuth::StringValue("Hello مرحبا World")))
  
  // 测试RTL文本在日志中的处理
  let provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(provider, "rtl-test-logger")
  
  let rtl_log = azimuth::Logger::create_log_record(logger)
  azimuth::LogRecord::set_body(rtl_log, "خطأ: فشل الاتصال بقاعدة البيانات")
  azimuth::LogRecord::set_severity(rtl_log, azimuth::ERROR)
  azimuth::LogRecord::set_attribute(rtl_log, "text.direction", azimuth::StringValue("rtl"))
  
  assert_eq(azimuth::LogRecord::body(rtl_log), "خطأ: فشل الاتصال بقاعدة البيانات")
}

test "locale-specific formatting" {
  // 测试特定区域设置的格式化
  let attributes = azimuth::Attributes::new()
  
  // 日期和时间格式
  azimuth::Attributes::set(attributes, "date.us", azimuth::StringValue("12/31/2023"))
  azimuth::Attributes::set(attributes, "date.eu", azimuth::StringValue("31/12/2023"))
  azimuth::Attributes::set(attributes, "date.iso", azimuth::StringValue("2023-12-31"))
  
  // 数字格式
  azimuth::Attributes::set(attributes, "number.us", azimuth::StringValue("1,234.56"))
  azimuth::Attributes::set(attributes, "number.eu", azimuth::StringValue("1.234,56"))
  azimuth::Attributes::set(attributes, "number.in", azimuth::StringValue("1,23,456.78"))
  
  // 货币格式
  azimuth::Attributes::set(attributes, "currency.us", azimuth::StringValue("$1,234.56"))
  azimuth::Attributes::set(attributes, "currency.eu", azimuth::StringValue("€1.234,56"))
  azimuth::Attributes::set(attributes, "currency.jp", azimuth::StringValue("¥1,235"))
  
  // 验证格式化数据正确存储
  let date_us = azimuth::Attributes::get(attributes, "date.us")
  let date_eu = azimuth::Attributes::get(attributes, "date.eu")
  let date_iso = azimuth::Attributes::get(attributes, "date.iso")
  
  let number_us = azimuth::Attributes::get(attributes, "number.us")
  let number_eu = azimuth::Attributes::get(attributes, "number.eu")
  let number_in = azimuth::Attributes::get(attributes, "number.in")
  
  let currency_us = azimuth::Attributes::get(attributes, "currency.us")
  let currency_eu = azimuth::Attributes::get(attributes, "currency.eu")
  let currency_jp = azimuth::Attributes::get(attributes, "currency.jp")
  
  assert_eq(date_us, Some(azimuth::StringValue("12/31/2023")))
  assert_eq(date_eu, Some(azimuth::StringValue("31/12/2023")))
  assert_eq(date_iso, Some(azimuth::StringValue("2023-12-31")))
  
  assert_eq(number_us, Some(azimuth::StringValue("1,234.56")))
  assert_eq(number_eu, Some(azimuth::StringValue("1.234,56")))
  assert_eq(number_in, Some(azimuth::StringValue("1,23,456.78")))
  
  assert_eq(currency_us, Some(azimuth::StringValue("$1,234.56")))
  assert_eq(currency_eu, Some(azimuth::StringValue("€1.234,56")))
  assert_eq(currency_jp, Some(azimuth::StringValue("¥1,235")))
}

test "internationalization in context propagation" {
  // 测试国际化在上下文传播中的处理
  let baggage = azimuth::Baggage::new()
  
  // 设置多语言baggage条目
  azimuth::Baggage::set_entry(baggage, "user.name.zh", "张三")
  azimuth::Baggage::set_entry(baggage, "user.name.ja", "田中太郎")
  azimuth::Baggage::set_entry(baggage, "user.name.en", "John Doe")
  azimuth::Baggage::set_entry(baggage, "error.message.ar", "فشل في المصادقة")
  azimuth::Baggage::set_entry(baggage, "error.message.es", "Fallo en la autenticación")
  
  // 创建传播载体
  let carrier = azimuth::TextMapCarrier::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  // 注入多语言baggage
  azimuth::W3CBaggagePropagator::inject(baggage_propagator, baggage, carrier)
  
  // 提取baggage
  let extracted_baggage = azimuth::W3CBaggagePropagator::extract(baggage_propagator, carrier)
  
  // 验证多语言baggage条目正确传播
  let user_name_zh = azimuth::Baggage::get_entry(extracted_baggage, "user.name.zh")
  let user_name_ja = azimuth::Baggage::get_entry(extracted_baggage, "user.name.ja")
  let user_name_en = azimuth::Baggage::get_entry(extracted_baggage, "user.name.en")
  let error_message_ar = azimuth::Baggage::get_entry(extracted_baggage, "error.message.ar")
  let error_message_es = azimuth::Baggage::get_entry(extracted_baggage, "error.message.es")
  
  assert_eq(user_name_zh, Some("张三"))
  assert_eq(user_name_ja, Some("田中太郎"))
  assert_eq(user_name_en, Some("John Doe"))
  assert_eq(error_message_ar, Some("فشل في المصادقة"))
  assert_eq(error_message_es, Some("Fallo en la autenticación"))
}