// 多语言国际化支持测试
// 测试遥测系统的多语言支持、本地化和全球化功能

pub test "多语言基础本地化测试" {
  // 创建本地化管理器
  let localization_manager = azimuth::LocalizationManager::new()
  
  // 测试支持的语言列表
  let supported_languages = azimuth::LocalizationManager::get_supported_languages(localization_manager)
  assert_true(supported_languages.length() >= 5)
  assert_true(supported_languages.contains("zh-CN"))  // 简体中文
  assert_true(supported_languages.contains("en-US"))  // 美式英语
  assert_true(supported_languages.contains("ja-JP"))  // 日语
  assert_true(supported_languages.contains("ko-KR"))  // 韩语
  assert_true(supported_languages.contains("es-ES"))  // 西班牙语
  
  // 设置当前语言为中文
  azimuth::LocalizationManager::set_current_language(localization_manager, "zh-CN")
  let current_lang = azimuth::LocalizationManager::get_current_language(localization_manager)
  assert_eq(current_lang, "zh-CN")
  
  // 测试基础本地化字符串
  let service_name_key = "service.name"
  let localized_service_name = azimuth::LocalizationManager::get_localized_string(
    localization_manager, 
    service_name_key
  )
  assert_eq(localized_service_name, "服务名称")
  
  // 测试英文本地化
  azimuth::LocalizationManager::set_current_language(localization_manager, "en-US")
  let english_service_name = azimuth::LocalizationManager::get_localized_string(
    localization_manager, 
    service_name_key
  )
  assert_eq(english_service_name, "Service Name")
  
  // 测试日语本地化
  azimuth::LocalizationManager::set_current_language(localization_manager, "ja-JP")
  let japanese_service_name = azimuth::LocalizationManager::get_localized_string(
    localization_manager, 
    service_name_key
  )
  assert_eq(japanese_service_name, "サービス名")
}

pub test "遥测属性多语言支持测试" {
  let localization_manager = azimuth::LocalizationManager::new()
  
  // 创建多语言遥测属性
  let multilingual_attrs = azimuth::Attributes::new()
  
  // 设置不同语言的属性值
  azimuth::LocalizationManager::set_current_language(localization_manager, "zh-CN")
  azimuth::Attributes::set(multilingual_attrs, "error.message", azimuth::StringValue("操作失败"))
  azimuth::Attributes::set(multilingual_attrs, "user.action", azimuth::StringValue("用户登录"))
  
  azimuth::LocalizationManager::set_current_language(localization_manager, "en-US")
  azimuth::Attributes::set(multilingual_attrs, "error.message", azimuth::StringValue("Operation failed"))
  azimuth::Attributes::set(multilingual_attrs, "user.action", azimuth::StringValue("User login"))
  
  azimuth::LocalizationManager::set_current_language(localization_manager, "ja-JP")
  azimuth::Attributes::set(multilingual_attrs, "error.message", azimuth::StringValue("操作が失敗しました"))
  azimuth::Attributes::set(multilingual_attrs, "user.action", azimuth::StringValue("ユーザーログイン"))
  
  // 测试不同语言下的属性获取
  azimuth::LocalizationManager::set_current_language(localization_manager, "zh-CN")
  let zh_error_msg = azimuth::Attributes::get(multilingual_attrs, "error.message")
  let zh_user_action = azimuth::Attributes::get(multilingual_attrs, "user.action")
  assert_eq(zh_error_msg, Some(azimuth::StringValue("操作失败")))
  assert_eq(zh_user_action, Some(azimuth::StringValue("用户登录")))
  
  azimuth::LocalizationManager::set_current_language(localization_manager, "en-US")
  let en_error_msg = azimuth::Attributes::get(multilingual_attrs, "error.message")
  let en_user_action = azimuth::Attributes::get(multilingual_attrs, "user.action")
  assert_eq(en_error_msg, Some(azimuth::StringValue("Operation failed")))
  assert_eq(en_user_action, Some(azimuth::StringValue("User login")))
  
  azimuth::LocalizationManager::set_current_language(localization_manager, "ja-JP")
  let ja_error_msg = azimuth::Attributes::get(multilingual_attrs, "error.message")
  let ja_user_action = azimuth::Attributes::get(multilingual_attrs, "user.action")
  assert_eq(ja_error_msg, Some(azimuth::StringValue("操作が失敗しました")))
  assert_eq(ja_user_action, Some(azimuth::StringValue("ユーザーログイン")))
  
  // 测试多语言属性的序列化和反序列化
  let serialized_attrs = azimuth::LocalizationManager::serialize_multilingual_attributes(
    localization_manager, 
    multilingual_attrs
  )
  let deserialized_attrs = azimuth::LocalizationManager::deserialize_multilingual_attributes(
    localization_manager, 
    serialized_attrs
  )
  
  // 验证序列化后的多语言属性完整性
  azimuth::LocalizationManager::set_current_language(localization_manager, "zh-CN")
  let restored_zh_error = azimuth::Attributes::get(deserialized_attrs, "error.message")
  assert_eq(restored_zh_error, Some(azimuth::StringValue("操作失败")))
  
  azimuth::LocalizationManager::set_current_language(localization_manager, "en-US")
  let restored_en_error = azimuth::Attributes::get(deserialized_attrs, "error.message")
  assert_eq(restored_en_error, Some(azimuth::StringValue("Operation failed")))
}

pub test "多语言日志消息本地化测试" {
  let localization_manager = azimuth::LocalizationManager::new()
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "multilingual-logger")
  
  // 创建多语言日志模板
  let log_templates = [
    ("user.login.success", "用户 {username} 登录成功"),
    ("user.login.failed", "用户 {username} 登录失败: {reason}"),
    ("operation.completed", "操作 {operation} 已完成，耗时 {duration}ms"),
    ("error.occurred", "发生错误: {error_type} - {error_message}")
  ]
  
  // 注册多语言日志模板
  azimuth::LocalizationManager::set_current_language(localization_manager, "zh-CN")
  for (template_key, template_value) in log_templates {
    azimuth::LocalizationManager::register_log_template(localization_manager, template_key, template_value)
  }
  
  // 注册英文日志模板
  azimuth::LocalizationManager::set_current_language(localization_manager, "en-US")
  let english_templates = [
    ("user.login.success", "User {username} logged in successfully"),
    ("user.login.failed", "User {username} login failed: {reason}"),
    ("operation.completed", "Operation {operation} completed in {duration}ms"),
    ("error.occurred", "Error occurred: {error_type} - {error_message}")
  ]
  
  for (template_key, template_value) in english_templates {
    azimuth::LocalizationManager::register_log_template(localization_manager, template_key, template_value)
  }
  
  // 测试中文日志生成
  azimuth::LocalizationManager::set_current_language(localization_manager, "zh-CN")
  let zh_log_params = [("username", "张三"), ("reason", "密码错误")]
  let zh_log_message = azimuth::LocalizationManager::format_localized_log_message(
    localization_manager, 
    "user.login.failed", 
    zh_log_params
  )
  assert_eq(zh_log_message, "用户 张三 登录失败: 密码错误")
  
  // 创建中文日志记录
  let zh_log_record = azimuth::LogRecord::new(azimuth::Error, zh_log_message)
  azimuth::Logger::emit(logger, zh_log_record)
  
  // 测试英文日志生成
  azimuth::LocalizationManager::set_current_language(localization_manager, "en-US")
  let en_log_params = [("username", "john.doe"), ("reason", "invalid password")]
  let en_log_message = azimuth::LocalizationManager::format_localized_log_message(
    localization_manager, 
    "user.login.failed", 
    en_log_params
  )
  assert_eq(en_log_message, "User john.doe login failed: invalid password")
  
  // 创建英文日志记录
  let en_log_record = azimuth::LogRecord::new(azimuth::Error, en_log_message)
  azimuth::Logger::emit(logger, en_log_record)
  
  // 测试参数化日志的完整性
  let operation_params = [("operation", "数据处理"), ("duration", "150")]
  let operation_log = azimuth::LocalizationManager::format_localized_log_message(
    localization_manager, 
    "operation.completed", 
    operation_params
  )
  assert_eq(operation_log, "Operation 数据处理 completed in 150ms")
}

pub test "多语言度量和指标本地化测试" {
  let localization_manager = azimuth::LocalizationManager::new()
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "multilingual-meter")
  
  // 创建多语言度量描述
  let metric_descriptions = [
    ("request.count", "请求数量"),
    ("response.time", "响应时间"),
    ("error.rate", "错误率"),
    ("memory.usage", "内存使用量"),
    ("cpu.utilization", "CPU利用率")
  ]
  
  // 注册中文度量描述
  azimuth::LocalizationManager::set_current_language(localization_manager, "zh-CN")
  for (metric_key, description) in metric_descriptions {
    azimuth::LocalizationManager::register_metric_description(localization_manager, metric_key, description)
  }
  
  // 注册英文度量描述
  azimuth::LocalizationManager::set_current_language(localization_manager, "en-US")
  let english_descriptions = [
    ("request.count", "Number of requests"),
    ("response.time", "Response time"),
    ("error.rate", "Error rate"),
    ("memory.usage", "Memory usage"),
    ("cpu.utilization", "CPU utilization")
  ]
  
  for (metric_key, description) in english_descriptions {
    azimuth::LocalizationManager::register_metric_description(localization_manager, metric_key, description)
  }
  
  // 测试中文度量创建
  azimuth::LocalizationManager::set_current_language(localization_manager, "zh-CN")
  let zh_counter = azimuth::Meter::create_counter_with_localized_description(
    meter, 
    "request.count", 
    localization_manager
  )
  assert_eq(zh_counter.description, Some("请求数量"))
  
  let zh_histogram = azimuth::Meter::create_histogram_with_localized_description(
    meter, 
    "response.time", 
    Some("响应时间分布"),
    Some("ms"),
    localization_manager
  )
  assert_eq(zh_histogram.description, Some("响应时间分布"))
  
  // 测试英文度量创建
  azimuth::LocalizationManager::set_current_language(localization_manager, "en-US")
  let en_counter = azimuth::Meter::create_counter_with_localized_description(
    meter, 
    "request.count", 
    localization_manager
  )
  assert_eq(en_counter.description, Some("Number of requests"))
  
  let en_histogram = azimuth::Meter::create_histogram_with_localized_description(
    meter, 
    "response.time", 
    Some("Response time distribution"),
    Some("ms"),
    localization_manager
  )
  assert_eq(en_histogram.description, Some("Response time distribution"))
  
  // 测试度量数据的本地化展示
  let metric_data = azimuth::MetricData::new("response.time", 150.5)
  let localized_metric_display = azimuth::LocalizationManager::format_metric_display(
    localization_manager, 
    metric_data, 
    "zh-CN"
  )
  assert_true(localized_metric_display.contains("响应时间"))
  assert_true(localized_metric_display.contains("150.5"))
  
  let english_metric_display = azimuth::LocalizationManager::format_metric_display(
    localization_manager, 
    metric_data, 
    "en-US"
  )
  assert_true(english_metric_display.contains("Response time"))
  assert_true(english_metric_display.contains("150.5"))
}

pub test "多语言追踪和Span本地化测试" {
  let localization_manager = azimuth::LocalizationManager::new()
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "multilingual-tracer")
  
  // 创建多语言Span名称和描述
  let span_names = [
    ("user.authentication", "用户认证"),
    ("data.processing", "数据处理"),
    ("api.request", "API请求"),
    ("database.query", "数据库查询"),
    ("cache.operation", "缓存操作")
  ]
  
  // 注册中文Span名称
  azimuth::LocalizationManager::set_current_language(localization_manager, "zh-CN")
  for (span_key, localized_name) in span_names {
    azimuth::LocalizationManager::register_span_name(localization_manager, span_key, localized_name)
  }
  
  // 注册英文Span名称
  azimuth::LocalizationManager::set_current_language(localization_manager, "en-US")
  let english_span_names = [
    ("user.authentication", "User Authentication"),
    ("data.processing", "Data Processing"),
    ("api.request", "API Request"),
    ("database.query", "Database Query"),
    ("cache.operation", "Cache Operation")
  ]
  
  for (span_key, localized_name) in english_span_names {
    azimuth::LocalizationManager::register_span_name(localization_manager, span_key, localized_name)
  }
  
  // 测试中文Span创建
  azimuth::LocalizationManager::set_current_language(localization_manager, "zh-CN")
  let zh_span = azimuth::Tracer::start_span_with_localized_name(
    tracer, 
    "user.authentication", 
    localization_manager
  )
  assert_eq(azimuth::Span::name(zh_span), "用户认证")
  
  // 添加本地化事件
  let zh_event_params = [("username", "张三"), ("method", "密码认证")]
  azimuth::Span::add_localized_event(zh_span, "认证开始", zh_event_params, localization_manager)
  azimuth::Span::add_localized_event(zh_span, "认证成功", zh_event_params, localization_manager)
  
  // 测试英文Span创建
  azimuth::LocalizationManager::set_current_language(localization_manager, "en-US")
  let en_span = azimuth::Tracer::start_span_with_localized_name(
    tracer, 
    "user.authentication", 
    localization_manager
  )
  assert_eq(azimuth::Span::name(en_span), "User Authentication")
  
  // 添加本地化事件
  let en_event_params = [("username", "john.doe"), ("method", "password")]
  azimuth::Span::add_localized_event(en_span, "Authentication started", en_event_params, localization_manager)
  azimuth::Span::add_localized_event(en_span, "Authentication successful", en_event_params, localization_manager)
  
  // 测试Span状态的本地化
  azimuth::Span::set_localized_status(zh_span, azimuth::Ok, "认证完成", localization_manager)
  azimuth::Span::set_localized_status(en_span, azimuth::Ok, "Authentication completed", localization_manager)
  
  // 测试分布式追踪的多语言支持
  let trace_context = azimuth::SpanContext::new("trace-123", "span-456", true, "")
  let distributed_span = azimuth::Span::new_with_localized_context(
    "data.processing", 
    azimuth::Internal, 
    trace_context, 
    localization_manager
  )
  assert_eq(azimuth::Span::name(distributed_span), "数据处理")
}

pub test "多语言时区和日期格式测试" {
  let localization_manager = azimuth::LocalizationManager::new()
  
  // 测试不同地区的时区支持
  let timezones = [
    ("Asia/Shanghai", "UTC+8"),
    ("America/New_York", "UTC-5"),
    ("Europe/London", "UTC+0"),
    ("Asia/Tokyo", "UTC+9"),
    ("Australia/Sydney", "UTC+10")
  ]
  
  for (timezone_name, timezone_offset) in timezones {
    azimuth::LocalizationManager::register_timezone(localization_manager, timezone_name, timezone_offset)
  }
  
  // 创建测试时间戳
  let test_timestamp = 1735689600000000000L  // 2025-01-01 00:00:00 UTC
  
  // 测试不同时区的时间格式化
  let shanghai_time = azimuth::LocalizationManager::format_timestamp_with_timezone(
    localization_manager, 
    test_timestamp, 
    "Asia/Shanghai", 
    "zh-CN"
  )
  assert_true(shanghai_time.contains("2025"))
  assert_true(shanghai_time.contains("08:00"))  // UTC+8
  
  let new_york_time = azimuth::LocalizationManager::format_timestamp_with_timezone(
    localization_manager, 
    test_timestamp, 
    "America/New_York", 
    "en-US"
  )
  assert_true(new_york_time.contains("2024"))  // 前一天
  assert_true(new_york_time.contains("19:00"))  // UTC-5
  
  // 测试不同地区的日期格式
  let chinese_date_format = azimuth::LocalizationManager::format_date_with_locale(
    localization_manager, 
    test_timestamp, 
    "zh-CN", 
    "Asia/Shanghai"
  )
  assert_true(chinese_date_format.contains("2025年"))
  assert_true(chinese_date_format.contains("1月"))
  assert_true(chinese_date_format.contains("1日"))
  
  let english_date_format = azimuth::LocalizationManager::format_date_with_locale(
    localization_manager, 
    test_timestamp, 
    "en-US", 
    "America/New_York"
  )
  assert_true(english_date_format.contains("January"))
  assert_true(english_date_format.contains("1,"))
  assert_true(english_date_format.contains("2024"))
  
  let japanese_date_format = azimuth::LocalizationManager::format_date_with_locale(
    localization_manager, 
    test_timestamp, 
    "ja-JP", 
    "Asia/Tokyo"
  )
  assert_true(japanese_date_format.contains("2025年"))
  assert_true(japanese_date_format.contains("1月"))
  assert_true(japanese_date_format.contains("1日"))
  
  // 测试相对时间格式化
  let current_time = test_timestamp + 3600000000000L  // 1小时后
  let relative_time_zh = azimuth::LocalizationManager::format_relative_time(
    localization_manager, 
    test_timestamp, 
    current_time, 
    "zh-CN"
  )
  assert_true(relative_time_zh.contains("1小时前"))
  
  let relative_time_en = azimuth::LocalizationManager::format_relative_time(
    localization_manager, 
    test_timestamp, 
    current_time, 
    "en-US"
  )
  assert_true(relative_time_en.contains("1 hour ago"))
}

pub test "多语言数字和货币格式测试" {
  let localization_manager = azimuth::LocalizationManager::new()
  
  // 测试不同地区的数字格式
  let test_number = 1234567.89
  
  let chinese_number_format = azimuth::LocalizationManager::format_number_with_locale(
    localization_manager, 
    test_number, 
    "zh-CN"
  )
  assert_true(chinese_number_format.contains("1,234,567.89"))
  
  let english_number_format = azimuth::LocalizationManager::format_number_with_locale(
    localization_manager, 
    test_number, 
    "en-US"
  )
  assert_true(english_number_format.contains("1,234,567.89"))
  
  let german_number_format = azimuth::LocalizationManager::format_number_with_locale(
    localization_manager, 
    test_number, 
    "de-DE"
  )
  assert_true(german_number_format.contains("1.234.567,89"))  // 德语使用点作为千位分隔符，逗号作为小数点
  
  let french_number_format = azimuth::LocalizationManager::format_number_with_locale(
    localization_manager, 
    test_number, 
    "fr-FR"
  )
  assert_true(french_number_format.contains("1 234 567,89"))  // 法语使用空格作为千位分隔符
  
  // 测试货币格式
  let currency_amount = 1234.56
  
  let chinese_currency = azimuth::LocalizationManager::format_currency_with_locale(
    localization_manager, 
    currency_amount, 
    "zh-CN", 
    "CNY"
  )
  assert_true(chinese_currency.contains("¥"))
  assert_true(chinese_currency.contains("1,234.56"))
  
  let usd_currency = azimuth::LocalizationManager::format_currency_with_locale(
    localization_manager, 
    currency_amount, 
    "en-US", 
    "USD"
  )
  assert_true(usd_currency.contains("$"))
  assert_true(usd_currency.contains("1,234.56"))
  
  let euro_currency = azimuth::LocalizationManager::format_currency_with_locale(
    localization_manager, 
    currency_amount, 
    "de-DE", 
    "EUR"
  )
  assert_true(euro_currency.contains("€"))
  assert_true(euro_currency.contains("1.234,56"))
  
  // 测试百分比格式
  let percentage_value = 0.7543
  
  let chinese_percentage = azimuth::LocalizationManager::format_percentage_with_locale(
    localization_manager, 
    percentage_value, 
    "zh-CN"
  )
  assert_true(chinese_percentage.contains("75.43%"))
  
  let english_percentage = azimuth::LocalizationManager::format_percentage_with_locale(
    localization_manager, 
    percentage_value, 
    "en-US"
  )
  assert_true(english_percentage.contains("75.43%"))
  
  // 测试度量单位的本地化
  let measurement_value = 1500.75
  
  let chinese_measurement = azimuth::LocalizationManager::format_measurement_with_locale(
    localization_manager, 
    measurement_value, 
    "zh-CN", 
    "meter"
  )
  assert_true(chinese_measurement.contains("1,500.75"))
  assert_true(chinese_measurement.contains("米"))
  
  let english_measurement = azimuth::LocalizationManager::format_measurement_with_locale(
    localization_manager, 
    measurement_value, 
    "en-US", 
    "meter"
  )
  assert_true(english_measurement.contains("1,500.75"))
  assert_true(english_measurement.contains("meters"))
}

pub test "多语言错误消息和异常处理测试" {
  let localization_manager = azimuth::LocalizationManager::new()
  
  // 创建多语言错误消息模板
  let error_templates = [
    ("network.timeout", "网络连接超时"),
    ("database.connection.failed", "数据库连接失败"),
    ("authentication.invalid.credentials", "认证凭据无效"),
    ("resource.not.found", "资源未找到"),
    ("permission.denied", "权限不足")
  ]
  
  // 注册中文错误消息
  azimuth::LocalizationManager::set_current_language(localization_manager, "zh-CN")
  for (error_key, error_message) in error_templates {
    azimuth::LocalizationManager::register_error_message(localization_manager, error_key, error_message)
  }
  
  // 注册英文错误消息
  azimuth::LocalizationManager::set_current_language(localization_manager, "en-US")
  let english_error_templates = [
    ("network.timeout", "Network connection timeout"),
    ("database.connection.failed", "Database connection failed"),
    ("authentication.invalid.credentials", "Invalid authentication credentials"),
    ("resource.not.found", "Resource not found"),
    ("permission.denied", "Permission denied")
  ]
  
  for (error_key, error_message) in english_error_templates {
    azimuth::LocalizationManager::register_error_message(localization_manager, error_key, error_message)
  }
  
  // 测试中文错误消息生成
  azimuth::LocalizationManager::set_current_language(localization_manager, "zh-CN")
  let zh_error_params = [("timeout", "30秒"), ("server", "api.example.com")]
  let zh_network_error = azimuth::LocalizationManager::format_error_message(
    localization_manager, 
    "network.timeout", 
    zh_error_params
  )
  assert_eq(zh_network_error, "网络连接超时")
  
  // 创建本地化异常
  let zh_exception = azimuth::LocalizationManager::create_localized_exception(
    localization_manager, 
    "authentication.invalid.credentials", 
    [("username", "张三"), ("attempt", "3")]
  )
  assert_eq(zh_exception.message, "认证凭据无效")
  assert_eq(zh_exception.language, "zh-CN")
  
  // 测试英文错误消息生成
  azimuth::LocalizationManager::set_current_language(localization_manager, "en-US")
  let en_error_params = [("timeout", "30s"), ("server", "api.example.com")]
  let en_network_error = azimuth::LocalizationManager::format_error_message(
    localization_manager, 
    "network.timeout", 
    en_error_params
  )
  assert_eq(en_network_error, "Network connection timeout")
  
  // 创建英文本地化异常
  let en_exception = azimuth::LocalizationManager::create_localized_exception(
    localization_manager, 
    "authentication.invalid.credentials", 
    [("username", "john.doe"), ("attempt", "3")]
  )
  assert_eq(en_exception.message, "Invalid authentication credentials")
  assert_eq(en_exception.language, "en-US")
  
  // 测试错误代码的多语言映射
  let error_code_mapping = [
    ("E001", "network.timeout"),
    ("E002", "database.connection.failed"),
    ("E003", "authentication.invalid.credentials"),
    ("E004", "resource.not.found"),
    ("E005", "permission.denied")
  ]
  
  for (error_code, error_key) in error_code_mapping {
    azimuth::LocalizationManager::register_error_code_mapping(localization_manager, error_code, error_key)
  }
  
  // 测试通过错误代码获取本地化消息
  let localized_error_by_code = azimuth::LocalizationManager::get_localized_error_by_code(
    localization_manager, 
    "E003", 
    "zh-CN"
  )
  assert_eq(localized_error_by_code, "认证凭据无效")
  
  let english_error_by_code = azimuth::LocalizationManager::get_localized_error_by_code(
    localization_manager, 
    "E003", 
    "en-US"
  )
  assert_eq(english_error_by_code, "Invalid authentication credentials")
  
  // 测试错误堆栈跟踪的本地化
  let error_stack = [
    "at function authenticate()",
    "at function login()",
    "at function main()"
  ]
  
  let localized_stack_trace = azimuth::LocalizationManager::localize_stack_trace(
    localization_manager, 
    error_stack, 
    "zh-CN"
  )
  assert_true(localized_stack_trace.contains("函数"))
  
  let english_stack_trace = azimuth::LocalizationManager::localize_stack_trace(
    localization_manager, 
    error_stack, 
    "en-US"
  )
  assert_true(english_stack_trace.contains("function"))
}

pub test "多语言配置和动态切换测试" {
  let localization_manager = azimuth::LocalizationManager::new()
  
  // 测试配置文件加载
  let zh_config_content = "{\n    \"language\": \"zh-CN\",\n    \"region\": \"CN\",\n    \"timezone\": \"Asia/Shanghai\",\n    \"date_format\": \"YYYY年MM月DD日\",\n    \"number_format\": \"#,##0.##\",\n    \"currency\": \"CNY\"\n  }"
  
  let en_config_content = "{\n    \"language\": \"en-US\",\n    \"region\": \"US\",\n    \"timezone\": \"America/New_York\",\n    \"date_format\": \"MM/DD/YYYY\",\n    \"number_format\": \"#,##0.##\",\n    \"currency\": \"USD\"\n  }"
  
  // 加载中文配置
  azimuth::LocalizationManager::load_configuration_from_string(localization_manager, zh_config_content)
  assert_eq(azimuth::LocalizationManager::get_current_language(localization_manager), "zh-CN")
  assert_eq(azimuth::LocalizationManager::get_current_region(localization_manager), "CN")
  assert_eq(azimuth::LocalizationManager::get_current_timezone(localization_manager), "Asia/Shanghai")
  
  // 测试动态语言切换
  let test_text = "Hello World"
  let zh_translation = azimuth::LocalizationManager::translate(localization_manager, test_text, "zh-CN")
  let en_translation = azimuth::LocalizationManager::translate(localization_manager, test_text, "en-US")
  
  // 验证翻译结果
  assert_true(zh_translation != test_text)  // 应该有翻译
  // 注意：在实际实现中，这里应该返回具体的翻译结果
  
  // 测试运行时语言切换
  let runtime_switcher = azimuth::LocalizationManager::create_runtime_switcher(localization_manager)
  
  // 初始状态为中文
  azimuth::RuntimeLanguageSwitcher::set_current_language(runtime_switcher, "zh-CN")
  let current_runtime_lang = azimuth::RuntimeLanguageSwitcher::get_current_language(runtime_switcher)
  assert_eq(current_runtime_lang, "zh-CN")
  
  // 动态切换到英文
  azimuth::RuntimeLanguageSwitcher::set_current_language(runtime_switcher, "en-US")
  let switched_runtime_lang = azimuth::RuntimeLanguageSwitcher::get_current_language(runtime_switcher)
  assert_eq(switched_runtime_lang, "en-US")
  
  // 测试语言切换事件
  let language_change_events = []
  azimuth::RuntimeLanguageSwitcher::on_language_change(runtime_switcher, fn(old_lang, new_lang) {
    language_change_events.push((old_lang, new_lang))
  })
  
  // 触发语言切换
  azimuth::RuntimeLanguageSwitcher::set_current_language(runtime_switcher, "ja-JP")
  
  // 验证事件被触发
  assert_true(language_change_events.length() >= 1)
  assert_eq(language_change_events[0], ("en-US", "ja-JP"))
  
  // 测试回退语言机制
  azimuth::LocalizationManager::set_fallback_language(localization_manager, "en-US")
  let fallback_lang = azimuth::LocalizationManager::get_fallback_language(localization_manager)
  assert_eq(fallback_lang, "en-US")
  
  // 测试不存在的语言回退
  let non_existent_translation = azimuth::LocalizationManager::translate_with_fallback(
    localization_manager, 
    "non.existent.key", 
    "ko-KR"  // 韩语可能不存在
  )
  // 应该回退到英语或返回原文本
  
  // 测试语言偏好检测
  let browser_preferences = "zh-CN,zh;q=0.9,en;q=0.8"
  let detected_language = azimuth::LocalizationManager::detect_preferred_language(
    localization_manager, 
    browser_preferences
  )
  assert_eq(detected_language, "zh-CN")
  
  let other_preferences = "fr-FR,fr;q=0.9,en;q=0.8"
  let detected_other_language = azimuth::LocalizationManager::detect_preferred_language(
    localization_manager, 
    other_preferences
  )
  // 应该检测到支持的语言或回退到默认语言
}