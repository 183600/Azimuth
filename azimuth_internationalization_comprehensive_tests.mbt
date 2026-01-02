// Azimuth 国际化支持综合测试
// 测试多语言环境下的遥测系统

test "多语言错误消息处理" {
  // 测试多语言错误消息的处理
  
  // 设置不同语言环境
  I18n::set_locale("en-US")
  let english_error = ErrorMessage::get("validation.failed")
  assert_eq(english_error, "Validation failed")
  
  I18n::set_locale("zh-CN")
  let chinese_error = ErrorMessage::get("validation.failed")
  assert_eq(chinese_error, "验证失败")
  
  I18n::set_locale("ja-JP")
  let japanese_error = ErrorMessage::get("validation.failed")
  assert_eq(japanese_error, "検証に失敗しました")
  
  I18n::set_locale("es-ES")
  let spanish_error = ErrorMessage::get("validation.failed")
  assert_eq(spanish_error, "Validación fallida")
  
  I18n::set_locale("fr-FR")
  let french_error = ErrorMessage::get("validation.failed")
  assert_eq(french_error, "La validation a échoué")
  
  // 测试带参数的错误消息
  I18n::set_locale("en-US")
  let english_param_error = ErrorMessage::format("parameter.required", ["username"])
  assert_eq(english_param_error, "Parameter 'username' is required")
  
  I18n::set_locale("zh-CN")
  let chinese_param_error = ErrorMessage::format("parameter.required", ["username"])
  assert_eq(chinese_param_error, "参数 'username' 是必需的")
  
  I18n::set_locale("ja-JP")
  let japanese_param_error = ErrorMessage::format("parameter.required", ["username"])
  assert_eq(japanese_param_error, "パラメータ 'username' は必須です")
}

test "多语言日志记录" {
  // 测试多语言日志记录
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "i18n.test")
  
  // 设置不同语言环境并记录日志
  I18n::set_locale("en-US")
  let english_log = LogRecord::new(Info, "User authentication successful")
  LogRecord::add_attribute(english_log, "locale", "en-US")
  LogRecord::add_attribute(english_log, "message", "User logged in successfully")
  LogRecord::add_attribute(english_log, "user.id", "user-12345")
  
  I18n::set_locale("zh-CN")
  let chinese_log = LogRecord::new(Info, "用户认证成功")
  LogRecord::add_attribute(chinese_log, "locale", "zh-CN")
  LogRecord::add_attribute(chinese_log, "message", "用户登录成功")
  LogRecord::add_attribute(chinese_log, "user.id", "user-67890")
  
  I18n::set_locale("ja-JP")
  let japanese_log = LogRecord::new(Info, "ユーザー認証成功")
  LogRecord::add_attribute(japanese_log, "locale", "ja-JP")
  LogRecord::add_attribute(japanese_log, "message", "ユーザーログイン成功")
  LogRecord::add_attribute(japanese_log, "user.id", "user-11111")
  
  // 发射日志
  Logger::emit(logger, english_log)
  Logger::emit(logger, chinese_log)
  Logger::emit(logger, japanese_log)
  
  // 验证日志内容
  assert_eq(LogRecord::body(english_log), Some("User authentication successful"))
  assert_eq(LogRecord::body(chinese_log), Some("用户认证成功"))
  assert_eq(LogRecord::body(japanese_log), Some("ユーザー認証成功"))
  
  // 测试多语言错误日志
  I18n::set_locale("en-US")
  let english_error_log = LogRecord::new(Error, "Database connection failed")
  LogRecord::add_attribute(english_error_log, "error.code", "DB_CONN_001")
  LogRecord::add_attribute(english_error_log, "error.message", "Unable to connect to database server")
  
  I18n::set_locale("zh-CN")
  let chinese_error_log = LogRecord::new(Error, "数据库连接失败")
  LogRecord::add_attribute(chinese_error_log, "error.code", "DB_CONN_001")
  LogRecord::add_attribute(chinese_error_log, "error.message", "无法连接到数据库服务器")
  
  Logger::emit(logger, english_error_log)
  Logger::emit(logger, chinese_error_log)
}

test "多语言Span属性和事件" {
  // 测试多语言Span属性和事件
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "i18n.span.test")
  
  // 创建英文Span
  I18n::set_locale("en-US")
  let english_span = Tracer::start_span(tracer, "user.authentication.process")
  Span::set_attribute(english_span, "locale", "en-US")
  Span::set_attribute(english_span, "service.name", "auth-service")
  Span::set_attribute(english_span, "operation.name", "User Authentication")
  Span::set_attribute(english_span, "description", "Process user login with email and password")
  
  // 添加英文事件
  Span::add_event(english_span, "validation.started", [
    ("field", "email"),
    ("message", "Validating email format")
  ])
  
  Span::add_event(english_span, "validation.completed", [
    ("field", "email"),
    ("result", "success"),
    ("message", "Email format is valid")
  ])
  
  Span::add_event(english_span, "authentication.completed", [
    ("result", "success"),
    ("message", "User authenticated successfully")
  ])
  
  // 创建中文Span
  I18n::set_locale("zh-CN")
  let chinese_span = Tracer::start_span(tracer, "用户认证处理")
  Span::set_attribute(chinese_span, "locale", "zh-CN")
  Span::set_attribute(chinese_span, "service.name", "auth-service")
  Span::set_attribute(chinese_span, "operation.name", "用户认证")
  Span::set_attribute(chinese_span, "description", "使用邮箱和密码处理用户登录")
  
  // 添加中文事件
  Span::add_event(chinese_span, "验证开始", [
    ("field", "email"),
    ("message", "验证邮箱格式")
  ])
  
  Span::add_event(chinese_span, "验证完成", [
    ("field", "email"),
    ("result", "成功"),
    ("message", "邮箱格式有效")
  ])
  
  Span::add_event(chinese_span, "认证完成", [
    ("result", "成功"),
    ("message", "用户认证成功")
  ])
  
  // 创建日文Span
  I18n::set_locale("ja-JP")
  let japanese_span = Tracer::start_span(tracer, "ユーザー認証処理")
  Span::set_attribute(japanese_span, "locale", "ja-JP")
  Span::set_attribute(japanese_span, "service.name", "auth-service")
  Span::set_attribute(japanese_span, "operation.name", "ユーザー認証")
  Span::set_attribute(japanese_span, "description", "メールとパスワードでユーザーログインを処理")
  
  // 添加日文事件
  Span::add_event(japanese_span, "検証開始", [
    ("field", "email"),
    ("message", "メール形式を検証")
  ])
  
  Span::add_event(japanese_span, "検証完了", [
    ("field", "email"),
    ("result", "成功"),
    ("message", "メール形式が有効です")
  ])
  
  Span::add_event(japanese_span, "認証完了", [
    ("result", "成功"),
    ("message", "ユーザー認証が成功しました")
  ])
  
  // 验证Span属性
  assert_eq(Span::name(english_span), "user.authentication.process")
  assert_eq(Span::name(chinese_span), "用户认证处理")
  assert_eq(Span::name(japanese_span), "ユーザー認証処理")
  
  Span::end(english_span)
  Span::end(chinese_span)
  Span::end(japanese_span)
}

test "多语言度量标签" {
  // 测试多语言度量标签
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "i18n.metrics.test")
  
  // 创建多语言计数器
  let request_counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("count"))
  
  // 记录不同语言的请求
  I18n::set_locale("en-US")
  Counter::add_with_attributes(request_counter, 100.0, [
    ("locale", "en-US"),
    ("endpoint", "/api/users"),
    ("method", "GET"),
    ("status", "200"),
    ("region", "us-west-2")
  ])
  
  I18n::set_locale("zh-CN")
  Counter::add_with_attributes(request_counter, 50.0, [
    ("locale", "zh-CN"),
    ("endpoint", "/api/users"),
    ("method", "GET"),
    ("status", "200"),
    ("region", "ap-northeast-1")
  ])
  
  I18n::set_locale("ja-JP")
  Counter::add_with_attributes(request_counter, 30.0, [
    ("locale", "ja-JP"),
    ("endpoint", "/api/users"),
    ("method", "GET"),
    ("status", "200"),
    ("region", "ap-northeast-1")
  ])
  
  // 创建多语言直方图
  let response_time_histogram = Meter::create_histogram(meter, "http.request.duration", Some("HTTP request duration"), Some("ms"))
  
  // 记录不同语言的响应时间
  I18n::set_locale("en-US")
  Histogram::record_with_attributes(response_time_histogram, 125.5, [
    ("locale", "en-US"),
    ("endpoint", "/api/users"),
    ("method", "GET"),
    ("region", "us-west-2")
  ])
  
  I18n::set_locale("zh-CN")
  Histogram::record_with_attributes(response_time_histogram, 245.8, [
    ("locale", "zh-CN"),
    ("endpoint", "/api/users"),
    ("method", "GET"),
    ("region", "ap-northeast-1")
  ])
  
  I18n::set_locale("ja-JP")
  Histogram::record_with_attributes(response_time_histogram, 185.3, [
    ("locale", "ja-JP"),
    ("endpoint", "/api/users"),
    ("method", "GET"),
    ("region", "ap-northeast-1")
  ])
  
  // 验证度量属性
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(response_time_histogram.name, "http.request.duration")
}

test "时区和日期时间格式化" {
  // 测试不同时区和日期时间格式化
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "timezone.test")
  
  // 设置UTC时区
  TimeZone::set_timezone("UTC")
  let utc_span = Tracer::start_span(tracer, "utc.operation")
  Span::set_attribute(utc_span, "timezone", "UTC")
  Span::set_attribute(utc_span, "timestamp", DateTime::format_now("yyyy-MM-dd HH:mm:ss z"))
  
  // 设置美国东部时区
  TimeZone::set_timezone("America/New_York")
  let est_span = Tracer::start_span(tracer, "est.operation")
  Span::set_attribute(est_span, "timezone", "America/New_York")
  Span::set_attribute(est_span, "timestamp", DateTime::format_now("yyyy-MM-dd HH:mm:ss z"))
  
  // 设置中国时区
  TimeZone::set_timezone("Asia/Shanghai")
  let cst_span = Tracer::start_span(tracer, "cst.operation")
  Span::set_attribute(cst_span, "timezone", "Asia/Shanghai")
  Span::set_attribute(cst_span, "timestamp", DateTime::format_now("yyyy-MM-dd HH:mm:ss z"))
  
  // 设置日本时区
  TimeZone::set_timezone("Asia/Tokyo")
  let jst_span = Tracer::start_span(tracer, "jst.operation")
  Span::set_attribute(jst_span, "timezone", "Asia/Tokyo")
  Span::set_attribute(jst_span, "timestamp", DateTime::format_now("yyyy-MM-dd HH:mm:ss z"))
  
  // 测试不同日期格式
  I18n::set_locale("en-US")
  let us_date = DateTime::format_now("MM/dd/yyyy HH:mm:ss a")
  
  I18n::set_locale("zh-CN")
  let cn_date = DateTime::format_now("yyyy年MM月dd日 HH:mm:ss")
  
  I18n::set_locale("ja-JP")
  let jp_date = DateTime::format_now("yyyy年MM月dd日 HH:mm:ss")
  
  I18n::set_locale("de-DE")
  let de_date = DateTime::format_now("dd.MM.yyyy HH:mm:ss")
  
  // 验证日期格式
  assert_true(us_date.contains("/"))
  assert_true(cn_date.contains("年") && cn_date.contains("月") && cn_date.contains("日"))
  assert_true(jp_date.contains("年") && jp_date.contains("月") && jp_date.contains("日"))
  assert_true(de_date.contains("."))
  
  Span::end(utc_span)
  Span::end(est_span)
  Span::end(cst_span)
  Span::end(jst_span)
}

test "多语言资源包加载" {
  // 测试多语言资源包的加载和切换
  
  // 加载英文资源包
  let en_bundle = ResourceBundle::load("en-US")
  assert_true(ResourceBundle::is_loaded(en_bundle))
  
  // 加载中文资源包
  let zh_bundle = ResourceBundle::load("zh-CN")
  assert_true(ResourceBundle::is_loaded(zh_bundle))
  
  // 加载日文资源包
  let ja_bundle = ResourceBundle::load("ja-JP")
  assert_true(ResourceBundle::is_loaded(ja_bundle))
  
  // 测试资源包切换
  I18n::set_resource_bundle(en_bundle)
  let en_message = ResourceBundle::get_string(en_bundle, "telemetry.span.created")
  assert_eq(en_message, "Span created")
  
  I18n::set_resource_bundle(zh_bundle)
  let zh_message = ResourceBundle::get_string(zh_bundle, "telemetry.span.created")
  assert_eq(zh_message, "Span已创建")
  
  I18n::set_resource_bundle(ja_bundle)
  let ja_message = ResourceBundle::get_string(ja_bundle, "telemetry.span.created")
  assert_eq(ja_message, "Spanが作成されました")
  
  // 测试资源包回退机制
  let fallback_bundle = ResourceBundle::load("fr-FR")
  I18n::set_resource_bundle(fallback_bundle)
  
  // 尝试获取不存在的键，应该回退到默认语言
  let fallback_message = ResourceBundle::get_string(fallback_bundle, "telemetry.span.created")
  assert_eq(fallback_message, "Span created")  // 回退到英文
  
  // 测试资源包热重载
  let reloaded_bundle = ResourceBundle::reload("en-US")
  assert_true(ResourceBundle::is_loaded(reloaded_bundle))
  
  // 验证重载后的资源
  let reloaded_message = ResourceBundle::get_string(reloaded_bundle, "telemetry.span.created")
  assert_eq(reloaded_message, "Span created")
}

test "多语言数字和货币格式化" {
  // 测试多语言数字和货币格式化
  
  // 设置不同语言环境并格式化数字
  I18n::set_locale("en-US")
  let us_number = NumberFormat::format(1234567.89, "decimal")
  let us_currency = NumberFormat::format(1234567.89, "currency")
  let us_percent = NumberFormat::format(0.8547, "percent")
  
  assert_eq(us_number, "1,234,567.89")
  assert_eq(us_currency, "$1,234,567.89")
  assert_eq(us_percent, "85.47%")
  
  I18n::set_locale("zh-CN")
  let cn_number = NumberFormat::format(1234567.89, "decimal")
  let cn_currency = NumberFormat::format(1234567.89, "currency")
  let cn_percent = NumberFormat::format(0.8547, "percent")
  
  assert_eq(cn_number, "1,234,567.89")
  assert_eq(cn_currency, "¥1,234,567.89")
  assert_eq(cn_percent, "85.47%")
  
  I18n::set_locale("ja-JP")
  let ja_number = NumberFormat::format(1234567.89, "decimal")
  let ja_currency = NumberFormat::format(1234567.89, "currency")
  let ja_percent = NumberFormat::format(0.8547, "percent")
  
  assert_eq(ja_number, "1,234,567.89")
  assert_eq(ja_currency, "¥1,234,568")
  assert_eq(ja_percent, "85.47%")
  
  I18n::set_locale("de-DE")
  let de_number = NumberFormat::format(1234567.89, "decimal")
  let de_currency = NumberFormat::format(1234567.89, "currency")
  let de_percent = NumberFormat::format(0.8547, "percent")
  
  assert_eq(de_number, "1.234.567,89")
  assert_eq(de_currency, "1.234.567,89 €")
  assert_eq(de_percent, "85,47 %")
  
  // 在遥测数据中使用格式化数字
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "formatting.test")
  
  let revenue_counter = Meter::create_counter(meter, "revenue.total", Some("Total revenue"), Some("currency"))
  
  I18n::set_locale("en-US")
  Counter::add_with_attributes(revenue_counter, 1234567.89, [
    ("locale", "en-US"),
    ("currency", "USD"),
    ("formatted_amount", "$1,234,567.89")
  ])
  
  I18n::set_locale("zh-CN")
  Counter::add_with_attributes(revenue_counter, 1234567.89, [
    ("locale", "zh-CN"),
    ("currency", "CNY"),
    ("formatted_amount", "¥1,234,567.89")
  ])
}

test "多语言文本方向和布局" {
  // 测试多语言文本方向和布局支持
  
  // 测试从左到右的语言
  I18n::set_locale("en-US")
  let ltr_text = TextDirection::process("Hello World")
  assert_eq(TextDirection::get_direction(ltr_text), "LTR")
  
  I18n::set_locale("zh-CN")
  let ltr_text_cn = TextDirection::process("你好世界")
  assert_eq(TextDirection::get_direction(ltr_text_cn), "LTR")
  
  // 测试从右到左的语言
  I18n::set_locale("ar-SA")
  let rtl_text = TextDirection::process("مرحبا بالعالم")
  assert_eq(TextDirection::get_direction(rtl_text), "RTL")
  
  I18n::set_locale("he-IL")
  let rtl_text_he = TextDirection::process("שלום עולם")
  assert_eq(TextDirection::get_direction(rtl_text_he), "RTL")
  
  // 测试混合方向文本
  I18n::set_locale("en-US")
  let mixed_text = TextDirection::process("Hello مرحبا World")
  assert_eq(TextDirection::get_direction(mixed_text), "LTR")
  
  // 在Span中使用文本方向信息
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "text.direction.test")
  
  I18n::set_locale("en-US")
  let ltr_span = Tracer::start_span(tracer, "LTR Text Processing")
  Span::set_attribute(ltr_span, "text.direction", "LTR")
  Span::set_attribute(ltr_span, "text.sample", "Hello World")
  
  I18n::set_locale("ar-SA")
  let rtl_span = Tracer::start_span(tracer, "RTL Text Processing")
  Span::set_attribute(rtl_span, "text.direction", "RTL")
  Span::set_attribute(rtl_span, "text.sample", "مرحبا بالعالم")
  
  // 验证文本方向属性
  assert_eq(Span::name(ltr_span), "LTR Text Processing")
  assert_eq(Span::name(rtl_span), "RTL Text Processing")
  
  Span::end(ltr_span)
  Span::end(rtl_span)
}

test "多语言字符编码处理" {
  // 测试多语言字符编码处理
  
  // 测试不同语言的字符串
  let english_text = "Hello World"
  let chinese_text = "你好世界"
  let japanese_text = "こんにちは世界"
  let arabic_text = "مرحبا بالعالم"
  let russian_text = "Привет мир"
  let emoji_text = "Hello 🌍 World 🚀"
  
  // 测试UTF-8编码
  let english_encoded = Encoding::encode_utf8(english_text)
  let chinese_encoded = Encoding::encode_utf8(chinese_text)
  let japanese_encoded = Encoding::encode_utf8(japanese_text)
  let arabic_encoded = Encoding::encode_utf8(arabic_text)
  let russian_encoded = Encoding::encode_utf8(russian_text)
  let emoji_encoded = Encoding::encode_utf8(emoji_text)
  
  // 验证编码和解码
  let english_decoded = Encoding::decode_utf8(english_encoded)
  let chinese_decoded = Encoding::decode_utf8(chinese_encoded)
  let japanese_decoded = Encoding::decode_utf8(japanese_encoded)
  let arabic_decoded = Encoding::decode_utf8(arabic_encoded)
  let russian_decoded = Encoding::decode_utf8(russian_encoded)
  let emoji_decoded = Encoding::decode_utf8(emoji_encoded)
  
  assert_eq(english_decoded, english_text)
  assert_eq(chinese_decoded, chinese_text)
  assert_eq(japanese_decoded, japanese_text)
  assert_eq(arabic_decoded, arabic_text)
  assert_eq(russian_decoded, russian_text)
  assert_eq(emoji_decoded, emoji_text)
  
  // 在遥测数据中使用多语言文本
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "encoding.test")
  
  let span = Tracer::start_span(tracer, "Multilingual Processing")
  
  Span::set_attribute(span, "english.text", english_text)
  Span::set_attribute(span, "chinese.text", chinese_text)
  Span::set_attribute(span, "japanese.text", japanese_text)
  Span::set_attribute(span, "arabic.text", arabic_text)
  Span::set_attribute(span, "russian.text", russian_text)
  Span::set_attribute(span, "emoji.text", emoji_text)
  
  // 添加多语言事件
  Span::add_event(span, "multilingual.processing", [
    ("english", english_text),
    ("chinese", chinese_text),
    ("japanese", japanese_text),
    ("arabic", arabic_text),
    ("russian", russian_text),
    ("emoji", emoji_text)
  ])
  
  Span::end(span)
  
  // 验证字符长度计算
  assert_eq(StringUtils::length(english_text), 11)
  assert_eq(StringUtils::length(chinese_text), 4)
  assert_eq(StringUtils::length(japanese_text), 7)
  assert_eq(StringUtils::length(arabic_text), 11)
  assert_eq(StringUtils::length(russian_text), 11)
  assert_eq(StringUtils::length(emoji_text), 15)
}