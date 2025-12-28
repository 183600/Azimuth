// 国际化配置测试用例
// 测试遥测系统的国际化和本地化支持

test "internationalization_attribute_values" {
  // 测试多语言属性值
  let base_resource = Resource::new()
  
  // 添加中文属性
  let chinese_attrs = [
    ("service.name", StringValue("用户服务")),
    ("service.description", StringValue("处理用户认证和授权的核心服务")),
    ("service.owner", StringValue("开发团队A")),
    ("region", StringValue("亚太地区"))
  ]
  let resource_with_chinese = Resource::with_attributes(base_resource, chinese_attrs)
  
  // 添加英文属性
  let english_attrs = [
    ("service.name.en", StringValue("User Service")),
    ("service.description.en", StringValue("Core service handling user authentication and authorization")),
    ("service.owner.en", StringValue("Development Team A")),
    ("region.en", StringValue("Asia-Pacific"))
  ]
  let resource_with_english = Resource::with_attributes(resource_with_chinese, english_attrs)
  
  // 验证中文属性
  let service_name_cn = Resource::get_attribute(resource_with_english, "service.name")
  let service_desc_cn = Resource::get_attribute(resource_with_english, "service.description")
  let service_owner_cn = Resource::get_attribute(resource_with_english, "service.owner")
  let region_cn = Resource::get_attribute(resource_with_english, "region")
  
  assert_eq(service_name_cn, Some(StringValue("用户服务")))
  assert_eq(service_desc_cn, Some(StringValue("处理用户认证和授权的核心服务")))
  assert_eq(service_owner_cn, Some(StringValue("开发团队A")))
  assert_eq(region_cn, Some(StringValue("亚太地区")))
  
  // 验证英文属性
  let service_name_en = Resource::get_attribute(resource_with_english, "service.name.en")
  let service_desc_en = Resource::get_attribute(resource_with_english, "service.description.en")
  let service_owner_en = Resource::get_attribute(resource_with_english, "service.owner.en")
  let region_en = Resource::get_attribute(resource_with_english, "region.en")
  
  assert_eq(service_name_en, Some(StringValue("User Service")))
  assert_eq(service_desc_en, Some(StringValue("Core service handling user authentication and authorization")))
  assert_eq(service_owner_en, Some(StringValue("Development Team A")))
  assert_eq(region_en, Some(StringValue("Asia-Pacific")))
}

test "internationalization_log_messages" {
  // 测试多语言日志消息
  let clock = Clock::system()
  let timestamp = Clock::now_unix_nanos(clock)
  
  // 创建中文日志记录
  let chinese_log = LogRecord::new_with_context(
    Info,
    Some("用户登录成功"),
    Some(Attributes::{ values: [
      ("user.id", StringValue("用户123")),
      ("login.method", StringValue("密码认证")),
      ("location", StringValue("北京"))
    ]}),
    Some(timestamp),
    None,
    Some("trace_001"),
    Some("span_001"),
    None
  )
  
  // 创建英文日志记录
  let english_log = LogRecord::new_with_context(
    Info,
    Some("User login successful"),
    Some(Attributes::{ values: [
      ("user.id", StringValue("user123")),
      ("login.method", StringValue("password_auth")),
      ("location", StringValue("Beijing"))
    ]}),
    Some(timestamp + 1000000L),
    None,
    Some("trace_001"),
    Some("span_001"),
    None
  )
  
  // 验证中文日志
  assert_eq(LogRecord::body(chinese_log), Some("用户登录成功"))
  match chinese_log.attributes {
    Some(attrs) => {
      let user_id = Attributes::get(attrs, "user.id")
      let login_method = Attributes::get(attrs, "login.method")
      let location = Attributes::get(attrs, "location")
      
      assert_eq(user_id, Some(StringValue("用户123")))
      assert_eq(login_method, Some(StringValue("密码认证")))
      assert_eq(location, Some(StringValue("北京")))
    }
    None => assert_false(true, "Expected attributes in Chinese log")
  }
  
  // 验证英文日志
  assert_eq(LogRecord::body(english_log), Some("User login successful"))
  match english_log.attributes {
    Some(attrs) => {
      let user_id = Attributes::get(attrs, "user.id")
      let login_method = Attributes::get(attrs, "login.method")
      let location = Attributes::get(attrs, "location")
      
      assert_eq(user_id, Some(StringValue("user123")))
      assert_eq(login_method, Some(StringValue("password_auth")))
      assert_eq(location, Some(StringValue("Beijing")))
    }
    None => assert_false(true, "Expected attributes in English log")
  }
}

test "internationalization_span_names" {
  // 测试多语言span名称
  let trace_id = "internationalization_trace"
  
  // 创建中文span
  let chinese_span_ctx = SpanContext::new(trace_id, "chinese_span", true, "")
  let chinese_span = Span::new("用户认证流程", Server, chinese_span_ctx)
  
  // 创建英文span
  let english_span_ctx = SpanContext::new(trace_id, "english_span", true, "")
  let english_span = Span::new("User Authentication Flow", Server, english_span_ctx)
  
  // 验证span名称
  assert_eq(Span::name(chinese_span), "用户认证流程")
  assert_eq(Span::name(english_span), "User Authentication Flow")
  
  // 验证span类型
  assert_eq(Span::kind(chinese_span), Server)
  assert_eq(Span::kind(english_span), Server)
  
  // 验证span上下文
  assert_eq(SpanContext::trace_id(chinese_span_ctx), trace_id)
  assert_eq(SpanContext::trace_id(english_span_ctx), trace_id)
  assert_eq(SpanContext::span_id(chinese_span_ctx), "chinese_span")
  assert_eq(SpanContext::span_id(english_span_ctx), "english_span")
}

test "internationalization_error_messages" {
  // 测试多语言错误消息
  let clock = Clock::system()
  let timestamp = Clock::now_unix_nanos(clock)
  
  // 创建中文错误日志
  let chinese_error = LogRecord::new_with_context(
    Error,
    Some("数据库连接失败"),
    Some(Attributes::{ values: [
      ("error.code", StringValue("DB_CONN_001")),
      ("error.message", StringValue("无法连接到数据库服务器")),
      ("retry.count", IntValue(3)),
      ("error.severity", StringValue("严重"))
    ]}),
    Some(timestamp),
    None,
    Some("trace_error_001"),
    Some("span_error_001"),
    None
  )
  
  // 创建英文错误日志
  let english_error = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(Attributes::{ values: [
      ("error.code", StringValue("DB_CONN_001")),
      ("error.message", StringValue("Unable to connect to database server")),
      ("retry.count", IntValue(3)),
      ("error.severity", StringValue("Critical"))
    ]}),
    Some(timestamp + 1000000L),
    None,
    Some("trace_error_001"),
    Some("span_error_001"),
    None
  )
  
  // 验证中文错误
  assert_eq(LogRecord::severity_number(chinese_error), Error)
  assert_eq(LogRecord::body(chinese_error), Some("数据库连接失败"))
  match chinese_error.attributes {
    Some(attrs) => {
      let error_code = Attributes::get(attrs, "error.code")
      let error_message = Attributes::get(attrs, "error.message")
      let retry_count = Attributes::get(attrs, "retry.count")
      let error_severity = Attributes::get(attrs, "error.severity")
      
      assert_eq(error_code, Some(StringValue("DB_CONN_001")))
      assert_eq(error_message, Some(StringValue("无法连接到数据库服务器")))
      assert_eq(retry_count, Some(IntValue(3)))
      assert_eq(error_severity, Some(StringValue("严重")))
    }
    None => assert_false(true, "Expected attributes in Chinese error log")
  }
  
  // 验证英文错误
  assert_eq(LogRecord::severity_number(english_error), Error)
  assert_eq(LogRecord::body(english_error), Some("Database connection failed"))
  match english_error.attributes {
    Some(attrs) => {
      let error_code = Attributes::get(attrs, "error.code")
      let error_message = Attributes::get(attrs, "error.message")
      let retry_count = Attributes::get(attrs, "retry.count")
      let error_severity = Attributes::get(attrs, "error.severity")
      
      assert_eq(error_code, Some(StringValue("DB_CONN_001")))
      assert_eq(error_message, Some(StringValue("Unable to connect to database server")))
      assert_eq(retry_count, Some(IntValue(3)))
      assert_eq(error_severity, Some(StringValue("Critical")))
    }
    None => assert_false(true, "Expected attributes in English error log")
  }
}

test "internationalization_metrics_names" {
  // 测试多语言指标名称
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "国际化仪表板")
  
  // 创建中文指标
  let chinese_counter = Meter::create_counter(meter, "请求总数")
  let chinese_histogram = Meter::create_histogram(meter, "响应时间")
  let chinese_gauge = Meter::create_gauge(meter, "活跃用户数")
  
  // 创建英文指标
  let english_meter = MeterProvider::get_meter(provider, "Internationalization Dashboard")
  let english_counter = Meter::create_counter(english_meter, "request.total")
  let english_histogram = Meter::create_histogram(english_meter, "response.duration")
  let english_gauge = Meter::create_gauge(english_meter, "active.users")
  
  // 验证中文指标名称
  assert_eq(chinese_counter.name, "请求总数")
  assert_eq(chinese_histogram.name, "响应时间")
  assert_eq(chinese_gauge.name, "活跃用户数")
  
  // 验证英文指标名称
  assert_eq(english_counter.name, "request.total")
  assert_eq(english_histogram.name, "response.duration")
  assert_eq(english_gauge.name, "active.users")
  
  // 记录一些指标值
  Counter::add(chinese_counter, 100.0)
  Histogram::record(chinese_histogram, 250.0)
  
  Counter::add(english_counter, 150.0)
  Histogram::record(english_histogram, 300.0)
  
  // 验证instrument类型
  let chinese_counter_instrument = Counter(chinese_counter.name, chinese_counter.description, chinese_counter.unit)
  let english_counter_instrument = Counter(english_counter.name, english_counter.description, english_counter.unit)
  
  assert_eq(Instrument::name(chinese_counter_instrument), "请求总数")
  assert_eq(Instrument::name(english_counter_instrument), "request.total")
}

test "internationalization_baggage_entries" {
  // 测试多语言baggage条目
  let baggage = Baggage::new()
  
  // 添加中文baggage条目
  let baggage_with_cn = Baggage::set_entry(baggage, "用户角色", "管理员")
  let baggage_with_cn2 = Baggage::set_entry(baggage_with_cn, "请求来源", "移动应用")
  
  // 添加英文baggage条目
  let baggage_with_en = Baggage::set_entry(baggage_with_cn2, "user.role", "administrator")
  let baggage_final = Baggage::set_entry(baggage_with_en, "request.source", "mobile_app")
  
  // 验证中文baggage条目
  let user_role_cn = Baggage::get_entry(baggage_final, "用户角色")
  let request_source_cn = Baggage::get_entry(baggage_final, "请求来源")
  
  assert_eq(user_role_cn, Some("管理员"))
  assert_eq(request_source_cn, Some("移动应用"))
  
  // 验证英文baggage条目
  let user_role_en = Baggage::get_entry(baggage_final, "user.role")
  let request_source_en = Baggage::get_entry(baggage_final, "request.source")
  
  assert_eq(user_role_en, Some("administrator"))
  assert_eq(request_source_en, Some("mobile_app"))
  
  // 验证不存在的条目
  let missing_entry = Baggage::get_entry(baggage_final, "不存在的条目")
  assert_eq(missing_entry, None)
}