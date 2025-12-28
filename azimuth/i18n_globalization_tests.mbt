// Internationalization and Globalization Tests for Azimuth Telemetry System
// Focus on multi-language support, locale-specific formatting, and cultural conventions

test "多语言错误消息测试" {
  let attrs = Attributes::new()
  
  // 测试中文错误消息
  Attributes::set(attrs, "error.message.zh", StringValue("用户验证失败"))
  Attributes::set(attrs, "error.code.zh", StringValue("AUTH_FAILED"))
  Attributes::set(attrs, "error.description.zh", StringValue("提供的用户凭据无效，请检查用户名和密码"))
  
  // 测试英文错误消息
  Attributes::set(attrs, "error.message.en", StringValue("User authentication failed"))
  Attributes::set(attrs, "error.code.en", StringValue("AUTH_FAILED"))
  Attributes::set(attrs, "error.description.en", StringValue("Invalid user credentials provided, please check username and password"))
  
  // 测试日文错误消息
  Attributes::set(attrs, "error.message.ja", StringValue("ユーザー認証に失敗しました"))
  Attributes::set(attrs, "error.code.ja", StringValue("AUTH_FAILED"))
  Attributes::set(attrs, "error.description.ja", StringValue("無効なユーザー資格情報が提供されました。ユーザー名とパスワードを確認してください"))
  
  // 验证中文错误消息
  let zh_message = Attributes::get(attrs, "error.message.zh")
  match zh_message {
    Some(StringValue(value)) => assert_eq(value, "用户验证失败")
    _ => assert_true(false)
  }
  
  // 验证英文错误消息
  let en_message = Attributes::get(attrs, "error.message.en")
  match en_message {
    Some(StringValue(value)) => assert_eq(value, "User authentication failed")
    _ => assert_true(false)
  }
  
  // 验证日文错误消息
  let ja_message = Attributes::get(attrs, "error.message.ja")
  match ja_message {
    Some(StringValue(value)) => assert_eq(value, "ユーザー認証に失敗しました")
    _ => assert_true(false)
  }
}

test "本地化时间格式测试" {
  let timestamp = 1735689600000000000L // 2025年1月1日的时间戳
  
  // 创建不同语言的日志记录
  let zh_log = LogRecord::new_with_context(
    Info,
    Some("系统启动完成 - 2025年01月01日 00:00:00"),
    None,
    Some(timestamp),
    None,
    None,
    None,
    None
  )
  
  let en_log = LogRecord::new_with_context(
    Info,
    Some("System startup completed - 01/01/2025 00:00:00"),
    None,
    Some(timestamp),
    None,
    None,
    None,
    None
  )
  
  let ja_log = LogRecord::new_with_context(
    Info,
    Some("システム起動完了 - 2025年01月01日 00:00:00"),
    None,
    Some(timestamp),
    None,
    None,
    None,
    None
  )
  
  // 验证不同语言的日志消息
  match LogRecord::body(zh_log) {
    Some(message) => assert_true(message.contains("2025年01月01日"))
    None => assert_true(false)
  }
  
  match LogRecord::body(en_log) {
    Some(message) => assert_true(message.contains("01/01/2025"))
    None => assert_true(false)
  }
  
  match LogRecord::body(ja_log) {
    Some(message) => assert_true(message.contains("2025年01月01日"))
    None => assert_true(false)
  }
}

test "多语言资源属性测试" {
  let resource_attrs = [
    // 中文资源属性
    ("service.name.zh", StringValue("方位角遥测系统")),
    ("service.description.zh", StringValue("为MoonBit提供OpenTelemetry风格的遥测功能")),
    ("service.version.zh", StringValue("版本 1.0.0")),
    
    // 英文资源属性
    ("service.name.en", StringValue("Azimuth Telemetry System")),
    ("service.description.en", StringValue("OpenTelemetry-style telemetry functionality for MoonBit")),
    ("service.version.en", StringValue("Version 1.0.0")),
    
    // 日文资源属性
    ("service.name.ja", StringValue("アジマスティレメトリーシステム")),
    ("service.description.ja", StringValue("MoonBit向けのOpenTelemetryスタイルのテレメトリー機能")),
    ("service.version.ja", StringValue("バージョン 1.0.0")),
    
    // 法文资源属性
    ("service.name.fr", StringValue("Système de Télémétrie Azimuth")),
    ("service.description.fr", StringValue("Fonctionnalité de télémétrie style OpenTelemetry pour MoonBit")),
    ("service.version.fr", StringValue("Version 1.0.0"))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), resource_attrs)
  
  // 验证中文服务名称
  let zh_name = Resource::get_attribute(resource, "service.name.zh")
  match zh_name {
    Some(StringValue(value)) => assert_eq(value, "方位角遥测系统")
    _ => assert_true(false)
  }
  
  // 验证英文服务描述
  let en_description = Resource::get_attribute(resource, "service.description.en")
  match en_description {
    Some(StringValue(value)) => assert_eq(value, "OpenTelemetry-style telemetry functionality for MoonBit")
    _ => assert_true(false)
  }
  
  // 验证日文版本信息
  let ja_version = Resource::get_attribute(resource, "service.version.ja")
  match ja_version {
    Some(StringValue(value)) => assert_eq(value, "バージョン 1.0.0")
    _ => assert_true(false)
  }
  
  // 验证法文服务名称
  let fr_name = Resource::get_attribute(resource, "service.name.fr")
  match fr_name {
    Some(StringValue(value)) => assert_eq(value, "Système de Télémétrie Azimuth")
    _ => assert_true(false)
  }
}

test "本地化数字和货币格式测试" {
  let attrs = Attributes::new()
  
  // 测试不同地区的数字格式
  Attributes::set(attrs, "metric.value.us", StringValue("1,234.56")) // 美国格式
  Attributes::set(attrs, "metric.value.eu", StringValue("1.234,56")) // 欧洲格式
  Attributes::set(attrs, "metric.value.cn", StringValue("1,234.56")) // 中国格式
  Attributes::set(attrs, "metric.value.in", StringValue("1,23,456.78")) // 印度格式
  
  // 测试货币格式
  Attributes::set(attrs, "price.usd", StringValue("$1,234.56"))
  Attributes::set(attrs, "price.eur", StringValue("1.234,56 €"))
  Attributes::set(attrs, "price.cny", StringValue("¥1,234.56"))
  Attributes::set(attrs, "price.jpy", StringValue("¥1,235")) // 日元没有小数
  
  // 验证美国数字格式
  let us_value = Attributes::get(attrs, "metric.value.us")
  match us_value {
    Some(StringValue(value)) => assert_eq(value, "1,234.56")
    _ => assert_true(false)
  }
  
  // 验证欧洲数字格式
  let eu_value = Attributes::get(attrs, "metric.value.eu")
  match eu_value {
    Some(StringValue(value)) => assert_eq(value, "1.234,56")
    _ => assert_true(false)
  }
  
  // 验证中国货币格式
  let cny_price = Attributes::get(attrs, "price.cny")
  match cny_price {
    Some(StringValue(value)) => assert_eq(value, "¥1,234.56")
    _ => assert_true(false)
  }
  
  // 验证日元格式
  let jpy_price = Attributes::get(attrs, "price.jpy")
  match jpy_price {
    Some(StringValue(value)) => assert_eq(value, "¥1,235")
    _ => assert_true(false)
  }
}

test "多语言Span名称和描述测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "i18n.test.tracer")
  
  // 创建多语言Span
  let zh_span = Tracer::start_span(tracer, "用户认证流程")
  let en_span = Tracer::start_span(tracer, "user_authentication_flow")
  let ja_span = Tracer::start_span(tracer, "ユーザー認証フロー")
  let fr_span = Tracer::start_span(tracer, "flux_d_authentification_utilisateur")
  
  // 验证Span名称
  assert_eq(Span::name(zh_span), "用户认证流程")
  assert_eq(Span::name(en_span), "user_authentication_flow")
  assert_eq(Span::name(ja_span), "ユーザー認証フロー")
  assert_eq(Span::name(fr_span), "flux_d_authentification_utilisateur")
  
  // 为Span添加多语言事件
  Span::add_event(zh_span, "用户登录成功", Some([("用户ID", StringValue("12345"))]))
  Span::add_event(en_span, "user_login_success", Some([("user_id", StringValue("12345"))]))
  Span::add_event(ja_span, "ユーザーログイン成功", Some([("ユーザーID", StringValue("12345"))]))
  Span::add_event(fr_span, "succès_connexion_utilisateur", Some([("id_utilisateur", StringValue("12345"))]))
  
  // 结束Span
  Span::end(zh_span)
  Span::end(en_span)
  Span::end(ja_span)
  Span::end(fr_span)
}

test "国际化Baggage条目测试" {
  let baggage = Baggage::new()
  
  // 设置多语言Baggage条目
  let baggage1 = Baggage::set_entry(baggage, "locale.zh", "zh-CN")
  let baggage2 = Baggage::set_entry(baggage1, "locale.en", "en-US")
  let baggage3 = Baggage::set_entry(baggage2, "locale.ja", "ja-JP")
  let baggage4 = Baggage::set_entry(baggage3, "locale.fr", "fr-FR")
  let baggage5 = Baggage::set_entry(baggage4, "timezone", "Asia/Shanghai")
  
  // 验证各地区设置
  let zh_locale = Baggage::get_entry(baggage5, "locale.zh")
  match zh_locale {
    Some(value) => assert_eq(value, "zh-CN")
    None => assert_true(false) // 简化实现可能返回None
  }
  
  let en_locale = Baggage::get_entry(baggage5, "locale.en")
  match en_locale {
    Some(value) => assert_eq(value, "en-US")
    None => assert_true(false) // 简化实现可能返回None
  }
  
  let ja_locale = Baggage::get_entry(baggage5, "locale.ja")
  match ja_locale {
    Some(value) => assert_eq(value, "ja-JP")
    None => assert_true(false) // 简化实现可能返回None
  }
  
  let fr_locale = Baggage::get_entry(baggage5, "locale.fr")
  match fr_locale {
    Some(value) => assert_eq(value, "fr-FR")
    None => assert_true(false) // 简化实现可能返回None
  }
  
  let timezone = Baggage::get_entry(baggage5, "timezone")
  match timezone {
    Some(value) => assert_eq(value, "Asia/Shanghai")
    None => assert_true(false) // 简化实现可能返回None
  }
}

test "多语言Metric名称和单位测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "i18n.metrics")
  
  // 创建多语言Counter
  let zh_counter = Meter::create_counter(meter, "请求计数", Some("总请求数"), Some("个"))
  let en_counter = Meter::create_counter(meter, "request_count", Some("Total number of requests"), Some("requests"))
  let ja_counter = Meter::create_counter(meter, "リクエスト数", Some("総リクエスト数"), Some("件"))
  
  // 验证Counter属性
  assert_eq(zh_counter.name, "请求计数")
  assert_eq(zh_counter.description, Some("总请求数"))
  assert_eq(zh_counter.unit, Some("个"))
  
  assert_eq(en_counter.name, "request_count")
  assert_eq(en_counter.description, Some("Total number of requests"))
  assert_eq(en_counter.unit, Some("requests"))
  
  assert_eq(ja_counter.name, "リクエスト数")
  assert_eq(ja_counter.description, Some("総リクエスト数"))
  assert_eq(ja_counter.unit, Some("件"))
  
  // 创建多语言Histogram
  let zh_histogram = Meter::create_histogram(meter, "响应时间", Some("响应时间分布"), Some("毫秒"))
  let en_histogram = Meter::create_histogram(meter, "response_time", Some("Response time distribution"), Some("milliseconds"))
  let ja_histogram = Meter::create_histogram(meter, "応答時間", Some("応答時間分布"), Some("ミリ秒"))
  
  // 验证Histogram属性
  assert_eq(zh_histogram.name, "响应时间")
  assert_eq(zh_histogram.description, Some("响应时间分布"))
  assert_eq(zh_histogram.unit, Some("毫秒"))
  
  assert_eq(en_histogram.name, "response_time")
  assert_eq(en_histogram.description, Some("Response time distribution"))
  assert_eq(en_histogram.unit, Some("milliseconds"))
  
  assert_eq(ja_histogram.name, "応答時間")
  assert_eq(ja_histogram.description, Some("応答時間分布"))
  assert_eq(ja_histogram.unit, Some("ミリ秒"))
}

test "本地化HTTP请求和响应测试" {
  // 创建多语言HTTP请求
  let zh_headers = [
    ("Accept-Language", "zh-CN,zh;q=0.9"),
    ("Content-Type", "application/json; charset=utf-8"),
    ("User-Agent", "方位角遥测系统/1.0.0")
  ]
  
  let en_headers = [
    ("Accept-Language", "en-US,en;q=0.9"),
    ("Content-Type", "application/json; charset=utf-8"),
    ("User-Agent", "Azimuth-Telemetry/1.0.0")
  ]
  
  let ja_headers = [
    ("Accept-Language", "ja-JP,ja;q=0.9"),
    ("Content-Type", "application/json; charset=utf-8"),
    ("User-Agent", "アジマスティレメトリーシステム/1.0.0")
  ]
  
  let zh_request = HttpRequest::new("POST", "https://api.example.com/zh/endpoint", zh_headers, Some("{\"操作\":\"查询\"}"))
  let en_request = HttpRequest::new("POST", "https://api.example.com/en/endpoint", en_headers, Some("{\"operation\":\"query\"}"))
  let ja_request = HttpRequest::new("POST", "https://api.example.com/ja/endpoint", ja_headers, Some("{\"操作\":\"クエリ\"}"))
  
  // 验证请求属性
  assert_eq(HttpRequest::url(zh_request), "https://api.example.com/zh/endpoint")
  assert_eq(HttpRequest::url(en_request), "https://api.example.com/en/endpoint")
  assert_eq(HttpRequest::url(ja_request), "https://api.example.com/ja/endpoint")
  
  // 创建多语言HTTP响应
  let zh_response_headers = [("Content-Language", "zh-CN")]
  let en_response_headers = [("Content-Language", "en-US")]
  let ja_response_headers = [("Content-Language", "ja-JP")]
  
  let zh_response = HttpResponse::new(200, zh_response_headers, Some("{\"消息\":\"操作成功\"}"))
  let en_response = HttpResponse::new(200, en_response_headers, Some("{\"message\":\"Operation successful\"}"))
  let ja_response = HttpResponse::new(200, ja_response_headers, Some("{\"メッセージ\":\"操作が成功しました\"}"))
  
  // 验证响应状态码
  assert_eq(HttpResponse::status_code(zh_response), 200)
  assert_eq(HttpResponse::status_code(en_response), 200)
  assert_eq(HttpResponse::status_code(ja_response), 200)
}

test "Unicode和特殊字符支持测试" {
  let attrs = Attributes::new()
  
  // 测试各种Unicode字符
  Attributes::set(attrs, "chinese.text", StringValue("中文测试文本"))
  Attributes::set(attrs, "japanese.text", StringValue("日本語テキスト"))
  Attributes::set(attrs, "korean.text", StringValue("한국어 텍스트"))
  Attributes::set(attrs, "arabic.text", StringValue("نص عربي"))
  Attributes::set(attrs, "russian.text", StringValue("Русский текст"))
  Attributes::set(attrs, "emoji.text", StringValue("测试🚀emoji和特殊字符✨"))
  Attributes::set(attrs, "math.symbols", StringValue("∑∏∫∆∇∂"))
  Attributes::set(attrs, "currency.symbols", StringValue("¥€£$₹₽₩"))
  
  // 验证中文文本
  let chinese_result = Attributes::get(attrs, "chinese.text")
  match chinese_result {
    Some(StringValue(value)) => assert_eq(value, "中文测试文本")
    _ => assert_true(false)
  }
  
  // 验证日文文本
  let japanese_result = Attributes::get(attrs, "japanese.text")
  match japanese_result {
    Some(StringValue(value)) => assert_eq(value, "日本語テキスト")
    _ => assert_true(false)
  }
  
  // 验证韩文文本
  let korean_result = Attributes::get(attrs, "korean.text")
  match korean_result {
    Some(StringValue(value)) => assert_eq(value, "한국어 텍스트")
    _ => assert_true(false)
  }
  
  // 验证阿拉伯文文本
  let arabic_result = Attributes::get(attrs, "arabic.text")
  match arabic_result {
    Some(StringValue(value)) => assert_eq(value, "نص عربي")
    _ => assert_true(false)
  }
  
  // 验证俄文文本
  let russian_result = Attributes::get(attrs, "russian.text")
  match russian_result {
    Some(StringValue(value)) => assert_eq(value, "Русский текст")
    _ => assert_true(false)
  }
  
  // 验证emoji文本
  let emoji_result = Attributes::get(attrs, "emoji.text")
  match emoji_result {
    Some(StringValue(value)) => assert_eq(value, "测试🚀emoji和特殊字符✨")
    _ => assert_true(false)
  }
  
  // 验证数学符号
  let math_result = Attributes::get(attrs, "math.symbols")
  match math_result {
    Some(StringValue(value)) => assert_eq(value, "∑∏∫∆∇∂")
    _ => assert_true(false)
  }
  
  // 验证货币符号
  let currency_result = Attributes::get(attrs, "currency.symbols")
  match currency_result {
    Some(StringValue(value)) => assert_eq(value, "¥€£$₹₽₩")
    _ => assert_true(false)
  }
}

test "文化特定的日期和时间格式测试" {
  let attrs = Attributes::new()
  
  // 测试不同文化的日期格式
  Attributes::set(attrs, "date.us", StringValue("12/31/2024")) // 月/日/年
  Attributes::set(attrs, "date.uk", StringValue("31/12/2024")) // 日/月/年
  Attributes::set(attrs, "date.cn", StringValue("2024年12月31日")) // 年月日
  Attributes::set(attrs, "date.jp", StringValue("2024年12月31日")) // 年月日
  Attributes::set(attrs, "date.iso", StringValue("2024-12-31")) // ISO格式
  
  // 测试时间格式
  Attributes::set(attrs, "time.us", StringValue("2:30:45 PM")) // 12小时制
  Attributes::set(attrs, "time.eu", StringValue("14:30:45")) // 24小时制
  Attributes::set(attrs, "time.cn", StringValue("14时30分45秒")) // 中文格式
  Attributes::set(attrs, "time.jp", StringValue("14時30分45秒")) // 日文格式
  
  // 验证美国日期格式
  let us_date = Attributes::get(attrs, "date.us")
  match us_date {
    Some(StringValue(value)) => assert_eq(value, "12/31/2024")
    _ => assert_true(false)
  }
  
  // 验证英国日期格式
  let uk_date = Attributes::get(attrs, "date.uk")
  match uk_date {
    Some(StringValue(value)) => assert_eq(value, "31/12/2024")
    _ => assert_true(false)
  }
  
  // 验证中国日期格式
  let cn_date = Attributes::get(attrs, "date.cn")
  match cn_date {
    Some(StringValue(value)) => assert_eq(value, "2024年12月31日")
    _ => assert_true(false)
  }
  
  // 验证美国时间格式
  let us_time = Attributes::get(attrs, "time.us")
  match us_time {
    Some(StringValue(value)) => assert_eq(value, "2:30:45 PM")
    _ => assert_true(false)
  }
  
  // 验证欧洲时间格式
  let eu_time = Attributes::get(attrs, "time.eu")
  match eu_time {
    Some(StringValue(value)) => assert_eq(value, "14:30:45")
    _ => assert_true(false)
  }
  
  // 验证中国时间格式
  let cn_time = Attributes::get(attrs, "time.cn")
  match cn_time {
    Some(StringValue(value)) => assert_eq(value, "14时30分45秒")
    _ => assert_true(false)
  }
}