// Azimuth Telemetry System - Configuration Management and Internationalization Test Suite
// 测试配置管理和国际化功能

test "遥测系统配置管理测试" {
  // 测试遥测系统的各种配置选项
  
  // 模拟配置对象
  let telemetry_config = Attributes::new()
  
  // 服务配置
  Attributes::set(telemetry_config, "service.name", StringValue("azimuth-service"))
  Attributes::set(telemetry_config, "service.version", StringValue("1.2.3"))
  Attributes::set(telemetry_config, "service.instance.id", StringValue("instance-abc123"))
  Attributes::set(telemetry_config, "service.namespace", StringValue("production"))
  
  // 采集器配置
  Attributes::set(telemetry_config, "exporter.type", StringValue("otlp"))
  Attributes::set(telemetry_config, "exporter.endpoint", StringValue("https://otel-collector:4317"))
  Attributes::set(telemetry_config, "exporter.timeout", IntValue(5000))
  Attributes::set(telemetry_config, "exporter.retry.max_attempts", IntValue(3))
  
  // 采样配置
  Attributes::set(telemetry_config, "sampler.type", StringValue("probability"))
  Attributes::set(telemetry_config, "sampler.probability", FloatValue(0.1))
  Attributes::set(telemetry_config, "sampler.parent_based", BoolValue(true))
  
  // 批处理配置
  Attributes::set(telemetry_config, "batcher.max_export_batch_size", IntValue(512))
  Attributes::set(telemetry_config, "batcher.max_export_timeout_millis", IntValue(30000))
  Attributes::set(telemetry_config, "batcher.max_queue_size", IntValue(2048))
  
  // 资源限制配置
  Attributes::set(telemetry_config, "limits.span_count", IntValue(1000))
  Attributes::set(telemetry_config, "limits.attribute_count", IntValue(128))
  Attributes::set(telemetry_config, "limits.event_count", IntValue(128))
  Attributes::set(telemetry_config, "limits.link_count", IntValue(128))
  
  // 验证配置读取
  let service_name = Attributes::get(telemetry_config, "service.name")
  let service_version = Attributes::get(telemetry_config, "service.version")
  let exporter_type = Attributes::get(telemetry_config, "exporter.type")
  let sampler_probability = Attributes::get(telemetry_config, "sampler.probability")
  
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "azimuth-service")
    _ => assert_true(false)
  }
  
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "1.2.3")
    _ => assert_true(false)
  }
  
  match exporter_type {
    Some(StringValue(exporter)) => assert_eq(exporter, "otlp")
    _ => assert_true(false)
  }
  
  match sampler_probability {
    Some(FloatValue(probability)) => assert_eq(probability, 0.1)
    _ => assert_true(false)
  }
  
  // 测试默认配置
  let default_config = Attributes::new()
  Attributes::set(default_config, "service.name", StringValue("unknown-service"))
  Attributes::set(default_config, "exporter.type", StringValue("console"))
  Attributes::set(default_config, "sampler.type", StringValue("always_on"))
  
  let default_service_name = Attributes::get(default_config, "service.name")
  let default_exporter = Attributes::get(default_config, "exporter.type")
  let default_sampler = Attributes::get(default_config, "sampler.type")
  
  match default_service_name {
    Some(StringValue(name)) => assert_eq(name, "unknown-service")
    _ => assert_true(false)
  }
  
  match default_exporter {
    Some(StringValue(exporter)) => assert_eq(exporter, "console")
    _ => assert_true(false)
  }
  
  match default_sampler {
    Some(StringValue(sampler)) => assert_eq(sampler, "always_on")
    _ => assert_true(false)
  }
}

test "环境变量配置测试" {
  // 模拟从环境变量读取配置
  
  // 模拟环境变量配置
  let env_config = Attributes::new()
  
  // 服务环境变量
  Attributes::set(env_config, "OTEL_SERVICE_NAME", StringValue("web-api"))
  Attributes::set(env_config, "OTEL_SERVICE_VERSION", StringValue("2.1.0"))
  Attributes::set(env_config, "OTEL_RESOURCE_ATTRIBUTES", StringValue("service.namespace=staging,deployment.environment=testing"))
  
  // 导出器环境变量
  Attributes::set(env_config, "OTEL_EXPORTER_OTLP_ENDPOINT", StringValue("http://localhost:4318"))
  Attributes::set(env_config, "OTEL_EXPORTER_OTLP_HEADERS", StringValue("authorization=Bearer token123"))
  Attributes::set(env_config, "OTEL_EXPORTER_OTLP_TIMEOUT", StringValue("10000"))
  
  // 采样环境变量
  Attributes::set(env_config, "OTEL_TRACES_SAMPLER", StringValue("traceidratio"))
  Attributes::set(env_config, "OTEL_TRACES_SAMPLER_ARG", StringValue("0.25"))
  
  // 批处理环境变量
  Attributes::set(env_config, "OTEL_BSP_MAX_EXPORT_BATCH_SIZE", StringValue("256"))
  Attributes::set(env_config, "OTEL_BSP_SCHEDULE_DELAY", StringValue("5000"))
  
  // 验证环境变量配置解析
  let env_service_name = Attributes::get(env_config, "OTEL_SERVICE_NAME")
  let env_endpoint = Attributes::get(env_config, "OTEL_EXPORTER_OTLP_ENDPOINT")
  let env_sampler = Attributes::get(env_config, "OTEL_TRACES_SAMPLER")
  let env_sampler_arg = Attributes::get(env_config, "OTEL_TRACES_SAMPLER_ARG")
  
  match env_service_name {
    Some(StringValue(name)) => assert_eq(name, "web-api")
    _ => assert_true(false)
  }
  
  match env_endpoint {
    Some(StringValue(endpoint)) => assert_eq(endpoint, "http://localhost:4318")
    _ => assert_true(false)
  }
  
  match env_sampler {
    Some(StringValue(sampler)) => assert_eq(sampler, "traceidratio")
    _ => assert_true(false)
  }
  
  match env_sampler_arg {
    Some(StringValue(arg)) => assert_eq(arg, "0.25")
    _ => assert_true(false)
  }
  
  // 测试资源属性解析
  let resource_attrs_str = Attributes::get(env_config, "OTEL_RESOURCE_ATTRIBUTES")
  match resource_attrs_str {
    Some(StringValue(attrs_str)) => {
      // 在真实实现中会解析 "service.namespace=staging,deployment.environment=testing"
      assert_true(attrs_str.contains("service.namespace"))
      assert_true(attrs_str.contains("deployment.environment"))
    }
    _ => assert_true(false)
  }
}

test "多语言国际化消息测试" {
  // 测试遥测系统的国际化消息支持
  
  // 英文消息
  let en_messages = Attributes::new()
  Attributes::set(en_messages, "span.name.request", StringValue("HTTP Request"))
  Attributes::set(en_messages, "span.name.database", StringValue("Database Query"))
  Attributes::set(en_messages, "log.info.connection", StringValue("Database connection established"))
  Attributes::set(en_messages, "log.error.timeout", StringValue("Database connection timeout"))
  Attributes::set(en_messages, "metric.name.requests", StringValue("http_requests_total"))
  Attributes::set(en_messages, "metric.help.requests", StringValue("Total number of HTTP requests"))
  
  // 中文消息
  let zh_messages = Attributes::new()
  Attributes::set(zh_messages, "span.name.request", StringValue("HTTP请求"))
  Attributes::set(zh_messages, "span.name.database", StringValue("数据库查询"))
  Attributes::set(zh_messages, "log.info.connection", StringValue("数据库连接已建立"))
  Attributes::set(zh_messages, "log.error.timeout", StringValue("数据库连接超时"))
  Attributes::set(zh_messages, "metric.name.requests", StringValue("http_请求总数"))
  Attributes::set(zh_messages, "metric.help.requests", StringValue("HTTP请求的总数"))
  
  // 日文消息
  let ja_messages = Attributes::new()
  Attributes::set(ja_messages, "span.name.request", StringValue("HTTPリクエスト"))
  Attributes::set(ja_messages, "span.name.database", StringValue("データベースクエリ"))
  Attributes::set(ja_messages, "log.info.connection", StringValue("データベース接続が確立されました"))
  Attributes::set(ja_messages, "log.error.timeout", StringValue("データベース接続タイムアウト"))
  Attributes::set(ja_messages, "metric.name.requests", StringValue("http_リクエスト合計"))
  Attributes::set(ja_messages, "metric.help.requests", StringValue("HTTPリクエストの総数"))
  
  // 验证英文消息
  let en_request_span = Attributes::get(en_messages, "span.name.request")
  let en_connection_log = Attributes::get(en_messages, "log.info.connection")
  
  match en_request_span {
    Some(StringValue(message)) => assert_eq(message, "HTTP Request")
    _ => assert_true(false)
  }
  
  match en_connection_log {
    Some(StringValue(message)) => assert_eq(message, "Database connection established")
    _ => assert_true(false)
  }
  
  // 验证中文消息
  let zh_request_span = Attributes::get(zh_messages, "span.name.request")
  let zh_timeout_log = Attributes::get(zh_messages, "log.error.timeout")
  
  match zh_request_span {
    Some(StringValue(message)) => assert_eq(message, "HTTP请求")
    _ => assert_true(false)
  }
  
  match zh_timeout_log {
    Some(StringValue(message)) => assert_eq(message, "数据库连接超时")
    _ => assert_true(false)
  }
  
  // 验证日文消息
  let ja_request_span = Attributes::get(ja_messages, "span.name.request")
  let ja_database_span = Attributes::get(ja_messages, "span.name.database")
  
  match ja_request_span {
    Some(StringValue(message)) => assert_eq(message, "HTTPリクエスト")
    _ => assert_true(false)
  }
  
  match ja_database_span {
    Some(StringValue(message)) => assert_eq(message, "データベースクエリ")
    _ => assert_true(false)
  }
}

test "本地化错误消息测试" {
  // 测试错误消息的本地化
  
  // 英文错误消息
  let en_errors = Attributes::new()
  Attributes::set(en_errors, "error.connection.failed", StringValue("Failed to connect to database"))
  Attributes::set(en_errors, "error.timeout.exceeded", StringValue("Request timeout exceeded"))
  Attributes::set(en_errors, "error.authentication.failed", StringValue("Authentication failed"))
  Attributes::set(en_errors, "error.resource.not_found", StringValue("Resource not found"))
  Attributes::set(en_errors, "error.rate.limit.exceeded", StringValue("Rate limit exceeded"))
  
  // 中文错误消息
  let zh_errors = Attributes::new()
  Attributes::set(zh_errors, "error.connection.failed", StringValue("数据库连接失败"))
  Attributes::set(zh_errors, "error.timeout.exceeded", StringValue("请求超时"))
  Attributes::set(zh_errors, "error.authentication.failed", StringValue("身份验证失败"))
  Attributes::set(zh_errors, "error.resource.not_found", StringValue("资源未找到"))
  Attributes::set(zh_errors, "error.rate.limit.exceeded", StringValue("超过速率限制"))
  
  // 西班牙语错误消息
  let es_errors = Attributes::new()
  Attributes::set(es_errors, "error.connection.failed", StringValue("Error al conectar a la base de datos"))
  Attributes::set(es_errors, "error.timeout.exceeded", StringValue("Tiempo de espera agotado"))
  Attributes::set(es_errors, "error.authentication.failed", StringValue("Autenticación fallida"))
  Attributes::set(es_errors, "error.resource.not_found", StringValue("Recurso no encontrado"))
  Attributes::set(es_errors, "error.rate.limit.exceeded", StringValue("Límite de velocidad excedido"))
  
  // 测试不同语言的错误日志记录
  let logger_en = LoggerProvider::get_logger(LoggerProvider::default(), "error-logger-en")
  let logger_zh = LoggerProvider::get_logger(LoggerProvider::default(), "error-logger-zh")
  let logger_es = LoggerProvider::get_logger(LoggerProvider::default(), "error-logger-es")
  
  // 英文错误日志
  let en_connection_error = Attributes::get(en_errors, "error.connection.failed")
  match en_connection_error {
    Some(StringValue(message)) => {
      let log_record = LogRecord::new(Error, message)
      Logger::emit(logger_en, log_record)
      assert_eq(LogRecord::body(log_record), Some("Failed to connect to database"))
    }
    _ => assert_true(false)
  }
  
  // 中文错误日志
  let zh_timeout_error = Attributes::get(zh_errors, "error.timeout.exceeded")
  match zh_timeout_error {
    Some(StringValue(message)) => {
      let log_record = LogRecord::new(Error, message)
      Logger::emit(logger_zh, log_record)
      assert_eq(LogRecord::body(log_record), Some("请求超时"))
    }
    _ => assert_true(false)
  }
  
  // 西班牙语错误日志
  let es_auth_error = Attributes::get(es_errors, "error.authentication.failed")
  match es_auth_error {
    Some(StringValue(message)) => {
      let log_record = LogRecord::new(Error, message)
      Logger::emit(logger_es, log_record)
      assert_eq(LogRecord::body(log_record), Some("Autenticación fallida"))
    }
    _ => assert_true(false)
  }
}

test "本地化属性和标签测试" {
  // 测试属性和标签的国际化
  
  // 英文属性名称
  let en_attributes = Attributes::new()
  Attributes::set(en_attributes, "attr.user.id", StringValue("user.id"))
  Attributes::set(en_attributes, "attr.request.method", StringValue("http.method"))
  Attributes::set(en_attributes, "attr.response.status", StringValue("http.status_code"))
  Attributes::set(en_attributes, "attr.service.name", StringValue("service.name"))
  Attributes::set(en_attributes, "attr.operation.name", StringValue("operation.name"))
  
  // 中文属性名称
  let zh_attributes = Attributes::new()
  Attributes::set(zh_attributes, "attr.user.id", StringValue("用户.标识"))
  Attributes::set(zh_attributes, "attr.request.method", StringValue("HTTP.方法"))
  Attributes::set(zh_attributes, "attr.response.status", StringValue("响应.状态码"))
  Attributes::set(zh_attributes, "attr.service.name", StringValue("服务.名称"))
  Attributes::set(zh_attributes, "attr.operation.name", StringValue("操作.名称"))
  
  // 法文属性名称
  let fr_attributes = Attributes::new()
  Attributes::set(fr_attributes, "attr.user.id", StringValue("utilisateur.id"))
  Attributes::set(fr_attributes, "attr.request.method", StringValue("requete.methode"))
  Attributes::set(fr_attributes, "attr.response.status", StringValue("reponse.code_statut"))
  Attributes::set(fr_attributes, "attr.service.name", StringValue("service.nom"))
  Attributes::set(fr_attributes, "attr.operation.name", StringValue("operation.nom"))
  
  // 测试使用本地化属性创建Span
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "i18n-tracer")
  
  // 英文Span
  let en_span = Tracer::start_span(tracer, "localized.span.en")
  let en_user_id_attr = Attributes::get(en_attributes, "attr.user.id")
  match en_user_id_attr {
    Some(StringValue(attr_name)) => {
      let span_attrs = Attributes::new()
      Attributes::set(span_attrs, attr_name, StringValue("user-123"))
      Span::add_event(en_span, "user.identified", Some(span_attrs.values))
    }
    _ => assert_true(false)
  }
  
  // 中文Span
  let zh_span = Tracer::start_span(tracer, "本地化Span")
  let zh_method_attr = Attributes::get(zh_attributes, "attr.request.method")
  match zh_method_attr {
    Some(StringValue(attr_name)) => {
      let span_attrs = Attributes::new()
      Attributes::set(span_attrs, attr_name, StringValue("GET"))
      Span::add_event(zh_span, "请求.开始", Some(span_attrs.values))
    }
    _ => assert_true(false)
  }
  
  // 法文Span
  let fr_span = Tracer::start_span(tracer, "span.localise.fr")
  let fr_service_attr = Attributes::get(fr_attributes, "attr.service.name")
  match fr_service_attr {
    Some(StringValue(attr_name)) => {
      let span_attrs = Attributes::new()
      Attributes::set(span_attrs, attr_name, StringValue("service-principal"))
      Span::add_event(fr_span, "service.identifie", Some(span_attrs.values))
    }
    _ => assert_true(false)
  }
  
  // 验证Span名称
  assert_eq(Span::name(en_span), "localized.span.en")
  assert_eq(Span::name(zh_span), "本地化Span")
  assert_eq(Span::name(fr_span), "span.localise.fr")
}

test "时区和时间格式国际化测试" {
  // 测试时间格式和时区的国际化
  
  let clock = Clock::system()
  let base_timestamp = Clock::now_unix_nanos(clock)
  
  // 不同时区的时间格式配置
  let timezone_configs = Attributes::new()
  
  // UTC时区
  Attributes::set(timezone_configs, "timezone.utc", StringValue("UTC"))
  Attributes::set(timezone_configs, "format.utc", StringValue("2025-01-01T12:00:00.000Z"))
  
  // 美国东部时间
  Attributes::set(timezone_configs, "timezone.est", StringValue("America/New_York"))
  Attributes::set(timezone_configs, "format.est", StringValue("2025-01-01T07:00:00.000-05:00"))
  
  // 中国时间
  Attributes::set(timezone_configs, "timezone.cst", StringValue("Asia/Shanghai"))
  Attributes::set(timezone_configs, "format.cst", StringValue("2025-01-01T20:00:00.000+08:00"))
  
  // 日本时间
  Attributes::set(timezone_configs, "timezone.jst", StringValue("Asia/Tokyo"))
  Attributes::set(timezone_configs, "format.jst", StringValue("2025-01-01T21:00:00.000+09:00"))
  
  // 德国时间
  Attributes::set(timezone_configs, "timezone.cet", StringValue("Europe/Berlin"))
  Attributes::set(timezone_configs, "format.cet", StringValue("2025-01-01T13:00:00.000+01:00"))
  
  // 测试不同时区的日志记录
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "timezone-logger")
  
  // UTC时间日志
  let utc_format = Attributes::get(timezone_configs, "format.utc")
  match utc_format {
    Some(StringValue(format)) => {
      let utc_log = LogRecord::new_with_context(
        Info,
        Some("Event at " + format),
        Some(Attributes::new()),
        Some(base_timestamp),
        None,
        None,
        None,
        None
      )
      Logger::emit(logger, utc_log)
      assert_eq(LogRecord::body(utc_log), Some("Event at 2025-01-01T12:00:00.000Z"))
    }
    _ => assert_true(false)
  }
  
  // 中国时间日志
  let cst_format = Attributes::get(timezone_configs, "format.cst")
  match cst_format {
    Some(StringValue(format)) => {
      let cst_log = LogRecord::new_with_context(
        Info,
        Some("事件发生时间: " + format),
        Some(Attributes::new()),
        Some(base_timestamp),
        None,
        None,
        None,
        None
      )
      Logger::emit(logger, cst_log)
      assert_eq(LogRecord::body(cst_log), Some("事件发生时间: 2025-01-01T20:00:00.000+08:00"))
    }
    _ => assert_true(false)
  }
  
  // 日本时间日志
  let jst_format = Attributes::get(timezone_configs, "format.jst")
  match jst_format {
    Some(StringValue(format)) => {
      let jst_log = LogRecord::new_with_context(
        Info,
        Some("イベント時刻: " + format),
        Some(Attributes::new()),
        Some(base_timestamp),
        None,
        None,
        None,
        None
      )
      Logger::emit(logger, jst_log)
      assert_eq(LogRecord::body(jst_log), Some("イベント時刻: 2025-01-01T21:00:00.000+09:00"))
    }
    _ => assert_true(false)
  }
  
  // 验证时区配置
  let utc_timezone = Attributes::get(timezone_configs, "timezone.utc")
  let cst_timezone = Attributes::get(timezone_configs, "timezone.cst")
  let jst_timezone = Attributes::get(timezone_configs, "timezone.jst")
  
  match utc_timezone {
    Some(StringValue(timezone)) => assert_eq(timezone, "UTC")
    _ => assert_true(false)
  }
  
  match cst_timezone {
    Some(StringValue(timezone)) => assert_eq(timezone, "Asia/Shanghai")
    _ => assert_true(false)
  }
  
  match jst_timezone {
    Some(StringValue(timezone)) => assert_eq(timezone, "Asia/Tokyo")
    _ => assert_true(false)
  }
}

test "配置验证和错误处理测试" {
  // 测试配置验证和错误处理的国际化
  
  // 配置验证规则
  let validation_rules = Attributes::new()
  Attributes::set(validation_rules, "rule.service.name.required", StringValue("Service name is required"))
  Attributes::set(validation_rules, "rule.service.name.invalid", StringValue("Service name contains invalid characters"))
  Attributes::set(validation_rules, "rule.exporter.endpoint.invalid", StringValue("Invalid exporter endpoint URL"))
  Attributes::set(validation_rules, "rule.sampler.probability.range", StringValue("Sampler probability must be between 0.0 and 1.0"))
  Attributes::set(validation_rules, "rule.timeout.positive", StringValue("Timeout value must be positive"))
  
  // 中文验证规则
  let zh_validation_rules = Attributes::new()
  Attributes::set(zh_validation_rules, "rule.service.name.required", StringValue("服务名称是必需的"))
  Attributes::set(zh_validation_rules, "rule.service.name.invalid", StringValue("服务名称包含无效字符"))
  Attributes::set(zh_validation_rules, "rule.exporter.endpoint.invalid", StringValue("无效的导出器端点URL"))
  Attributes::set(zh_validation_rules, "rule.sampler.probability.range", StringValue("采样器概率必须在0.0和1.0之间"))
  Attributes::set(zh_validation_rules, "rule.timeout.positive", StringValue("超时值必须为正数"))
  
  // 测试配置验证场景
  let test_configs = [
    ("", "http://localhost:4318", 0.5, 5000),    // 缺少服务名称
    ("invalid$name!", "http://localhost:4318", 0.5, 5000), // 无效服务名称
    ("valid-service", "invalid-url", 0.5, 5000), // 无效导出器端点
    ("valid-service", "http://localhost:4318", -0.1, 5000), // 无效采样概率
    ("valid-service", "http://localhost:4318", 0.5, -1000)   // 无效超时值
  ]
  
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "config-validator")
  
  for i = 0; i < test_configs.length(); i++ {
    let config = test_configs[i]
    let service_name = config.0
    let endpoint = config.1
    let probability = config.2
    let timeout = config.3
    
    // 验证配置并记录错误
    if service_name == "" {
      let error_msg = Attributes::get(validation_rules, "rule.service.name.required")
      match error_msg {
        Some(StringValue(msg)) => {
          let error_log = LogRecord::new(Error, "Config validation failed: " + msg)
          Logger::emit(logger, error_log)
        }
        _ => assert_true(false)
      }
    }
    
    if service_name.contains("$") || service_name.contains("!") {
      let error_msg = Attributes::get(zh_validation_rules, "rule.service.name.invalid")
      match error_msg {
        Some(StringValue(msg)) => {
          let error_log = LogRecord::new(Error, "配置验证失败: " + msg)
          Logger::emit(logger, error_log)
        }
        _ => assert_true(false)
      }
    }
    
    if probability < 0.0 || probability > 1.0 {
      let error_msg = Attributes::get(validation_rules, "rule.sampler.probability.range")
      match error_msg {
        Some(StringValue(msg)) => {
          let error_log = LogRecord::new(Error, "Config validation failed: " + msg)
          Logger::emit(logger, error_log)
        }
        _ => assert_true(false)
      }
    }
    
    if timeout <= 0 {
      let error_msg = Attributes::get(zh_validation_rules, "rule.timeout.positive")
      match error_msg {
        Some(StringValue(msg)) => {
          let error_log = LogRecord::new(Error, "配置验证失败: " + msg)
          Logger::emit(logger, error_log)
        }
        _ => assert_true(false)
      }
    }
  }
  
  // 验证验证规则加载
  let en_required_rule = Attributes::get(validation_rules, "rule.service.name.required")
  let zh_required_rule = Attributes::get(zh_validation_rules, "rule.service.name.required")
  
  match en_required_rule {
    Some(StringValue(rule)) => assert_eq(rule, "Service name is required")
    _ => assert_true(false)
  }
  
  match zh_required_rule {
    Some(StringValue(rule)) => assert_eq(rule, "服务名称是必需的")
    _ => assert_true(false)
  }
}