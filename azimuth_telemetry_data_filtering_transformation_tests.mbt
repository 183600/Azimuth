// Azimuth 遥测数据过滤和转换测试用例
// 专注于遥测数据的过滤规则和转换逻辑

// 测试1: 基础数据过滤
test "基础数据过滤测试" {
  let filter_engine = FilterEngine::new("基础过滤引擎")
  
  // 创建过滤规则
  let severity_filter = SeverityFilter::new([Error, Critical])  // 只允许Error和Critical级别
  let service_filter = ServiceFilter::new(["payment.service", "auth.service"])  // 只允许特定服务
  let attribute_filter = AttributeFilter::new("user.id", NotEquals, "")  // 排除空用户ID
  
  // 添加过滤规则
  FilterEngine::add_filter(filter_engine, severity_filter)
  FilterEngine::add_filter(filter_engine, service_filter)
  FilterEngine::add_filter(filter_engine, attribute_filter)
  
  // 创建测试数据
  let logs = [
    LogEntry::new(Error, "Payment failed", "payment.service"),
    LogEntry::new(Info, "User login", "auth.service"),
    LogEntry::new(Warning, "High memory usage", "system.service"),
    LogEntry::new(Critical, "Database connection lost", "payment.service"),
    LogEntry::new(Debug, "Cache hit", "cache.service")
  ]
  
  // 添加属性
  LogEntry::add_attribute(logs[0], "user.id", "user-123")
  LogEntry::add_attribute(logs[1], "user.id", "user-456")
  LogEntry::add_attribute(logs[2], "user.id", "")
  LogEntry::add_attribute(logs[3], "user.id", "user-789")
  LogEntry::add_attribute(logs[4], "user.id", "")
  
  // 应用过滤
  let filtered_logs = FilterEngine::apply_filters(filter_engine, logs.to_list())
  
  // 验证过滤结果
  assert_eq(filtered_logs.length(), 2)  // 只有Error和Critical级别的日志通过
  
  // 验证具体内容
  assert_true(filtered_logs.any(|log| log.message == "Payment failed"))
  assert_true(filtered_logs.any(|log| log.message == "Database connection lost"))
  assert_false(filtered_logs.any(|log| log.message == "User login"))  // Info级别被过滤
  assert_false(filtered_logs.any(|log| log.message == "High memory usage"))  // Warning级别被过滤
  assert_false(filtered_logs.any(|log| log.message == "Cache hit"))  // Debug级别被过滤
}

// 测试2: 高级条件过滤
test "高级条件过滤测试" {
  let filter_engine = FilterEngine::new("高级过滤引擎")
  
  // 创建复合过滤条件
  let and_condition = AndCondition::new([
    AttributeCondition::new("service.name", Equals, "api.service"),
    AttributeCondition::new("http.status_code", GreaterThan, 399)
  ])
  
  let or_condition = OrCondition::new([
    AttributeCondition::new("error.type", Equals, "TimeoutError"),
    AttributeCondition::new("error.type", Equals, "ConnectionError")
  ])
  
  let not_condition = NotCondition::new(
    AttributeCondition::new("user.agent", Contains, "bot")
  )
  
  let range_condition = RangeCondition::new("response_time", 100, 5000)
  
  let regex_condition = RegexCondition::new("request.path", r"/api/v[0-9]+/users/*")
  
  // 添加过滤条件
  FilterEngine::add_filter(filter_engine, and_condition)
  FilterEngine::add_filter(filter_engine, or_condition)
  FilterEngine::add_filter(filter_engine, not_condition)
  FilterEngine::add_filter(filter_engine, range_condition)
  FilterEngine::add_filter(filter_engine, regex_condition)
  
  // 创建测试数据
  let traces = [
    create_trace_with_attributes("trace-1", "api.service", [
      ("http.status_code", "404"),
      ("error.type", "TimeoutError"),
      ("user.agent", "Mozilla/5.0"),
      ("response_time", "250"),
      ("request.path", "/api/v1/users/123")
    ]),
    create_trace_with_attributes("trace-2", "api.service", [
      ("http.status_code", "200"),
      ("error.type", "None"),
      ("user.agent", "curl/7.68.0"),
      ("response_time", "50"),
      ("request.path", "/api/v1/users/456")
    ]),
    create_trace_with_attributes("trace-3", "web.service", [
      ("http.status_code", "500"),
      ("error.type", "ConnectionError"),
      ("user.agent", "googlebot/2.1"),
      ("response_time", "1000"),
      ("request.path", "/api/v2/users/789")
    ]),
    create_trace_with_attributes("trace-4", "api.service", [
      ("http.status_code", "502"),
      ("error.type", "ConnectionError"),
      ("user.agent", "Mozilla/5.0"),
      ("response_time", "6000"),
      ("request.path", "/api/v1/users/999")
    ])
  ]
  
  // 应用过滤
  let filtered_traces = FilterEngine::apply_filters(filter_engine, traces.to_list())
  
  // 验证过滤结果
  assert_eq(filtered_traces.length(), 1)  // 只有trace-1应该通过所有过滤条件
  
  // 验证trace-1通过的原因
  let passed_trace = filtered_traces[0]
  assert_eq(passed_trace.trace_id, "trace-1")
  assert_eq(Trace::get_attribute(passed_trace, "service.name"), Some(StringValue("api.service")))
  assert_eq(Trace::get_attribute(passed_trace, "http.status_code"), Some(StringValue("404")))
  assert_eq(Trace::get_attribute(passed_trace, "error.type"), Some(StringValue("TimeoutError")))
}

// 测试3: 数据类型转换
test "数据类型转换测试" {
  let transform_engine = TransformEngine::new("类型转换引擎")
  
  // 创建类型转换器
  let string_to_int_converter = StringToIntConverter::new()
  let string_to_float_converter = StringToFloatConverter::new()
  let int_to_string_converter = IntToStringConverter::new()
  let float_to_string_converter = FloatToStringConverter::new()
  let json_to_struct_converter = JsonToStructConverter::new()
  
  // 添加转换器
  TransformEngine::add_converter(transform_engine, string_to_int_converter)
  TransformEngine::add_converter(transform_engine, string_to_float_converter)
  TransformEngine::add_converter(transform_engine, int_to_string_converter)
  TransformEngine::add_converter(transform_engine, float_to_string_converter)
  TransformEngine::add_converter(transform_engine, json_to_struct_converter)
  
  // 创建测试数据
  let metric = Metric::new("conversion.test", 0.0)
  Metric::add_attribute(metric, "string.int", "42")
  Metric::add_attribute(metric, "string.float", "3.14159")
  Metric::add_attribute(metric, "int.value", 100)
  Metric::add_attribute(metric, "float.value", 2.71828)
  Metric::add_attribute(metric, "json.data", "{\"name\":\"test\",\"value\":123}")
  
  // 应用转换规则
  TransformEngine::add_rule(transform_engine, TransformRule::new(
    "string.int", 
    string_to_int_converter, 
    "int.converted"
  ))
  
  TransformEngine::add_rule(transform_engine, TransformRule::new(
    "string.float", 
    string_to_float_converter, 
    "float.converted"
  ))
  
  TransformEngine::add_rule(transform_engine, TransformRule::new(
    "int.value", 
    int_to_string_converter, 
    "string.converted"
  ))
  
  TransformEngine::add_rule(transform_engine, TransformRule::new(
    "float.value", 
    float_to_string_converter, 
    "float.string.converted"
  ))
  
  // 应用转换
  let transformed_metric = TransformEngine::apply_transforms(transform_engine, metric)
  
  // 验证转换结果
  assert_eq(Metric::get_attribute(transformed_metric, "int.converted"), Some(IntValue(42)))
  assert_eq(Metric::get_attribute(transformed_metric, "float.converted"), Some(FloatValue(3.14159)))
  assert_eq(Metric::get_attribute(transformed_metric, "string.converted"), Some(StringValue("100")))
  assert_eq(Metric::get_attribute(transformed_metric, "float.string.converted"), Some(StringValue("2.71828")))
  
  // 验证原始属性保持不变
  assert_eq(Metric::get_attribute(transformed_metric, "string.int"), Some(StringValue("42")))
  assert_eq(Metric::get_attribute(transformed_metric, "int.value"), Some(IntValue(100)))
}

// 测试4: 数据格式转换
test "数据格式转换测试" {
  let transform_engine = TransformEngine::new("格式转换引擎")
  
  // 创建格式转换器
  let json_converter = JsonConverter::new()
  let xml_converter = XmlConverter::new()
  let yaml_converter = YamlConverter::new()
  let protobuf_converter = ProtobufConverter::new()
  let csv_converter = CsvConverter::new()
  
  // 添加转换器
  TransformEngine::add_converter(transform_engine, json_converter)
  TransformEngine::add_converter(transform_engine, xml_converter)
  TransformEngine::add_converter(transform_engine, yaml_converter)
  TransformEngine::add_converter(transform_engine, protobuf_converter)
  TransformEngine::add_converter(transform_engine, csv_converter)
  
  // 创建测试数据
  let trace = Trace::new("format.test", "format.operation", Ok, 150)
  Trace::add_attribute(trace, "user.id", "user-123")
  Trace::add_attribute(trace, "request.id", "req-456")
  Trace::add_attribute(trace, "timestamp", "2023-01-01T12:00:00Z")
  
  // 转换为JSON
  let json_data = TransformEngine::convert_to_format(transform_engine, trace, "json")
  assert_true(json_data.contains("\"trace_id\":\"format.test\""))
  assert_true(json_data.contains("\"operation_name\":\"format.operation\""))
  
  // 转换为XML
  let xml_data = TransformEngine::convert_to_format(transform_engine, trace, "xml")
  assert_true(xml_data.contains("<trace_id>format.test</trace_id>"))
  assert_true(xml_data.contains("<operation_name>format.operation</operation_name>"))
  
  // 转换为YAML
  let yaml_data = TransformEngine::convert_to_format(transform_engine, trace, "yaml")
  assert_true(yaml_data.contains("trace_id: format.test"))
  assert_true(yaml_data.contains("operation_name: format.operation"))
  
  // 验证往返转换
  let restored_trace = TransformEngine::convert_from_format(transform_engine, json_data, "json")
  assert_eq(restored_trace.trace_id, trace.trace_id)
  assert_eq(restored_trace.operation_name, trace.operation_name)
  assert_eq(restored_trace.status, trace.status)
  assert_eq(restored_trace.duration_ms, trace.duration_ms)
}

// 测试5: 数据聚合转换
test "数据聚合转换测试" {
  let transform_engine = TransformEngine::new("聚合转换引擎")
  
  // 创建聚合转换器
  let sum_aggregator = SumAggregator::new("total")
  let avg_aggregator = AvgAggregator::new("average")
  let min_aggregator = MinAggregator::new("minimum")
  let max_aggregator = MaxAggregator::new("maximum")
  let count_aggregator = CountAggregator::new("count")
  let percentile_aggregator = PercentileAggregator::new("percentile", [50.0, 95.0, 99.0])
  
  // 添加聚合转换器
  TransformEngine::add_converter(transform_engine, sum_aggregator)
  TransformEngine::add_converter(transform_engine, avg_aggregator)
  TransformEngine::add_converter(transform_engine, min_aggregator)
  TransformEngine::add_converter(transform_engine, max_aggregator)
  TransformEngine::add_converter(transform_engine, count_aggregator)
  TransformEngine::add_converter(transform_engine, percentile_aggregator)
  
  // 创建测试数据
  let metrics = [
    Metric::new("response.time", 120.0),
    Metric::new("response.time", 150.0),
    Metric::new("response.time", 80.0),
    Metric::new("response.time", 200.0),
    Metric::new("response.time", 95.0),
    Metric::new("response.time", 180.0),
    Metric::new("response.time", 110.0),
    Metric::new("response.time", 140.0),
    Metric::new("response.time", 90.0),
    Metric::new("response.time", 160.0)
  ]
  
  // 应用聚合转换
  let aggregated_metrics = TransformEngine::aggregate_metrics(transform_engine, metrics.to_list())
  
  // 验证聚合结果
  let sum_metric = aggregated_metrics.find(|m| m.name == "response.time.total")
  let avg_metric = aggregated_metrics.find(|m| m.name == "response.time.average")
  let min_metric = aggregated_metrics.find(|m| m.name == "response.time.minimum")
  let max_metric = aggregated_metrics.find(|m| m.name == "response.time.maximum")
  let count_metric = aggregated_metrics.find(|m| m.name == "response.time.count")
  let p50_metric = aggregated_metrics.find(|m| m.name == "response.time.percentile.50")
  let p95_metric = aggregated_metrics.find(|m| m.name == "response.time.percentile.95")
  let p99_metric = aggregated_metrics.find(|m| m.name == "response.time.percentile.99")
  
  // 验证计算结果
  assert_eq(sum_metric.unwrap().value, 1325.0)  // 120+150+80+200+95+180+110+140+90+160
  assert_eq(avg_metric.unwrap().value, 132.5)  // 1325/10
  assert_eq(min_metric.unwrap().value, 80.0)
  assert_eq(max_metric.unwrap().value, 200.0)
  assert_eq(count_metric.unwrap().value, 10.0)
  
  // 验证百分位数
  assert_true(p50_metric.unwrap().value >= 120.0 && p50_metric.unwrap().value <= 140.0)
  assert_true(p95_metric.unwrap().value >= 180.0 && p95_metric.unwrap().value <= 200.0)
  assert_true(p99_metric.unwrap().value >= 190.0 && p99_metric.unwrap().value <= 200.0)
}

// 测试6: 数据丰富转换
test "数据丰富转换测试" {
  let transform_engine = TransformEngine::new("数据丰富引擎")
  
  // 创建数据丰富器
  let geo_enricher = GeoEnricher::new()
  let service_enricher = ServiceEnricher::new()
  let user_enricher = UserEnricher::new()
  let time_enricher = TimeEnricher::new()
  let business_enricher = BusinessEnricher::new()
  
  // 配置丰富器
  GeoEnricher::set_ip_attribute(geo_enricher, "client.ip")
  ServiceEnricher::set_version_attribute(service_enricher, "service.name")
  UserEnricher::set_id_attribute(user_enricher, "user.id")
  TimeEnricher::set_timestamp_attribute(time_enricher, "timestamp")
  BusinessEnricher::set_business_hours(business_enricher, "9:00-17:00")
  
  // 添加丰富器
  TransformEngine::add_enricher(transform_engine, geo_enricher)
  TransformEngine::add_enricher(transform_engine, service_enricher)
  TransformEngine::add_enricher(transform_engine, user_enricher)
  TransformEngine::add_enricher(transform_engine, time_enricher)
  TransformEngine::add_enricher(transform_engine, business_enricher)
  
  // 创建测试数据
  let trace = Trace::new("enrichment.test", "enrichment.operation", Ok, 250)
  Trace::add_attribute(trace, "client.ip", "192.168.1.100")
  Trace::add_attribute(trace, "service.name", "payment.service")
  Trace::add_attribute(trace, "user.id", "user-12345")
  Trace::add_attribute(trace, "timestamp", "2023-01-01T14:30:00Z")
  
  // 应用数据丰富
  let enriched_trace = TransformEngine::enrich_data(transform_engine, trace)
  
  // 验证丰富结果
  // 地理信息丰富
  assert_true(Trace::has_attribute(enriched_trace, "client.geo.country"))
  assert_true(Trace::has_attribute(enriched_trace, "client.geo.city"))
  assert_true(Trace::has_attribute(enriched_trace, "client.geo.latitude"))
  assert_true(Trace::has_attribute(enriched_trace, "client.geo.longitude"))
  
  // 服务信息丰富
  assert_true(Trace::has_attribute(enriched_trace, "service.version"))
  assert_true(Trace::has_attribute(enriched_trace, "service.environment"))
  
  // 用户信息丰富
  assert_true(Trace::has_attribute(enriched_trace, "user.tier"))
  assert_true(Trace::has_attribute(enriched_trace, "user.registration_date"))
  
  // 时间信息丰富
  assert_true(Trace::has_attribute(enriched_trace, "time.hour"))
  assert_true(Trace::has_attribute(enriched_trace, "time.day_of_week"))
  assert_true(Trace::has_attribute(enriched_trace, "time.timezone"))
  
  // 业务信息丰富
  assert_true(Trace::has_attribute(enriched_trace, "business.hours"))
  assert_true(Trace::has_attribute(enriched_trace, "business.region"))
}

// 测试7: 数据清洗转换
test "数据清洗转换测试" {
  let transform_engine = TransformEngine::new("数据清洗引擎")
  
  // 创建数据清洗器
  let null_cleaner = NullCleaner::new()
  let duplicate_cleaner = DuplicateCleaner::new()
  let outlier_cleaner = OutlierCleaner::new()
  let format_cleaner = FormatCleaner::new()
  let sensitive_cleaner = SensitiveCleaner::new()
  
  // 配置清洗器
  NullCleaner::set_strategy(null_cleaner, Remove)  // 移除空值
  DuplicateCleaner::set_key(duplicate_cleaner, "trace_id")  // 基于trace_id去重
  OutlierCleaner::set_method(outlier_cleaner, IQR)  // 使用IQR方法检测异常值
  FormatCleaner::set_rules(format_cleaner, [
    ("email", r"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$"),
    ("phone", r"^\+?[1-9]\d{1,14}$")
  ])
  SensitiveCleaner::set_patterns(sensitive_cleaner, [
    ("credit_card", r"\b\d{4}[-\s]?\d{4}[-\s]?\d{4}[-\s]?\d{4}\b"),
    ("ssn", r"\b\d{3}-\d{2}-\d{4}\b")
  ])
  
  // 添加清洗器
  TransformEngine::add_cleaner(transform_engine, null_cleaner)
  TransformEngine::add_cleaner(transform_engine, duplicate_cleaner)
  TransformEngine::add_cleaner(transform_engine, outlier_cleaner)
  TransformEngine::add_cleaner(transform_engine, format_cleaner)
  TransformEngine::add_cleaner(transform_engine, sensitive_cleaner)
  
  // 创建测试数据
  let traces = [
    create_trace_with_attributes("trace-1", "service", [
      ("user.email", "user@example.com"),
      ("user.phone", "+1234567890"),
      ("payment.card", "4111-1111-1111-1111"),
      ("response.time", "120")
    ]),
    create_trace_with_attributes("trace-2", "service", [
      ("user.email", "invalid-email"),
      ("user.phone", "123"),
      ("response.time", "5000")  // 异常值
    ]),
    create_trace_with_attributes("trace-1", "service", [  // 重复
      ("user.email", "user@example.com"),
      ("response.time", "130")
    ]),
    create_trace_with_attributes("trace-3", "service", [
      ("user.email", ""),  // 空值
      ("user.ssn", "123-45-6789"),
      ("response.time", "150")
    ])
  ]
  
  // 应用数据清洗
  let cleaned_traces = TransformEngine::clean_data(transform_engine, traces.to_list())
  
  // 验证清洗结果
  assert_eq(cleaned_traces.length(), 2)  // 移除重复和无效数据
  
  // 验证去重
  let unique_trace_ids = cleaned_traces.map(|t| t.trace_id).unique()
  assert_eq(unique_trace_ids.length(), 2)
  
  // 验证敏感数据清洗
  for trace in cleaned_traces {
    // 信用卡号应该被掩码
    let card_attr = Trace::get_attribute(trace, "payment.card")
    if card_attr.is_some() {
      let card_value = card_attr.unwrap().to_string()
      assert_true(card_value.contains("****") || card_value.contains("XXXX"))
    }
    
    // SSN应该被掩码
    let ssn_attr = Trace::get_attribute(trace, "user.ssn")
    if ssn_attr.is_some() {
      let ssn_value = ssn_attr.unwrap().to_string()
      assert_true(ssn_value.contains("***") || ssn_value.contains("XXX"))
    }
  }
  
  // 验证格式清洗
  for trace in cleaned_traces {
    let email_attr = Trace::get_attribute(trace, "user.email")
    if email_attr.is_some() {
      let email_value = email_attr.unwrap().to_string()
      assert_true(email_value.contains("@") && email_value.contains("."))
    }
  }
}

// 测试8: 数据规范化转换
test "数据规范化转换测试" {
  let transform_engine = TransformEngine::new("数据规范化引擎")
  
  // 创建规范化转换器
  let case_normalizer = CaseNormalizer::new(Lowercase)
  let unit_normalizer = UnitNormalizer::new()
  let timezone_normalizer = TimezoneNormalizer::new("UTC")
  let currency_normalizer = CurrencyNormalizer::new("USD")
  let encoding_normalizer = EncodingNormalizer::new("UTF-8")
  
  // 配置规范化器
  UnitNormalizer::add_conversion(unit_normalizer, "ms", "s", 0.001)
  UnitNormalizer::add_conversion(unit_normalizer, "KB", "B", 1024)
  UnitNormalizer::add_conversion(unit_normalizer, "MB", "B", 1024 * 1024)
  
  // 添加规范化转换器
  TransformEngine::add_normalizer(transform_engine, case_normalizer)
  TransformEngine::add_normalizer(transform_engine, unit_normalizer)
  TransformEngine::add_normalizer(transform_engine, timezone_normalizer)
  TransformEngine::add_normalizer(transform_engine, currency_normalizer)
  TransformEngine::add_normalizer(transform_engine, encoding_normalizer)
  
  // 创建测试数据
  let metrics = [
    create_metric_with_attributes("test.metric", 1000.0, [
      ("service.name", "API.SERVICE"),
      ("response.time", "500ms"),
      ("memory.usage", "256KB"),
      ("timestamp", "2023-01-01T12:00:00+08:00"),
      ("price", "19.99EUR"),
      ("encoding", "ISO-8859-1")
    ]),
    create_metric_with_attributes("test.metric", 2000.0, [
      ("service.name", "DATABASE.SERVICE"),
      ("response.time", "1.5s"),
      ("memory.usage", "512MB"),
      ("timestamp", "2023-01-01T04:00:00-04:00"),
      ("price", "29.99GBP"),
      ("encoding", "UTF-16")
    ])
  ]
  
  // 应用规范化
  let normalized_metrics = TransformEngine::normalize_data(transform_engine, metrics.to_list())
  
  // 验证规范化结果
  for metric in normalized_metrics {
    // 验证大小写规范化
    let service_name = Metric::get_attribute(metric, "service.name")
    if service_name.is_some() {
      let service_value = service_name.unwrap().to_string()
      assert_eq(service_value.to_lowercase(), service_value)
    }
    
    // 验证单位规范化
    let response_time = Metric::get_attribute(metric, "response.time")
    if response_time.is_some() {
      let time_value = response_time.unwrap().to_string()
      assert_true(time_value.ends_with("s"))  // 应该转换为秒
    }
    
    let memory_usage = Metric::get_attribute(metric, "memory.usage")
    if memory_usage.is_some() {
      let memory_value = memory_usage.unwrap().to_string()
      assert_true(memory_value.ends_with("B"))  // 应该转换为字节
    }
    
    // 验证时区规范化
    let timestamp = Metric::get_attribute(metric, "timestamp")
    if timestamp.is_some() {
      let time_value = timestamp.unwrap().to_string()
      assert_true(time_value.contains("Z") || time_value.contains("+00:00"))  // 应该转换为UTC
    }
    
    // 验证货币规范化
    let price = Metric::get_attribute(metric, "price")
    if price.is_some() {
      let price_value = price.unwrap().to_string()
      assert_true(price_value.contains("USD"))  // 应该转换为USD
    }
    
    // 验证编码规范化
    let encoding = Metric::get_attribute(metric, "encoding")
    if encoding.is_some() {
      let encoding_value = encoding.unwrap().to_string()
      assert_eq(encoding_value, "UTF-8")  // 应该转换为UTF-8
    }
  }
}

// 测试9: 自定义过滤和转换
test "自定义过滤和转换测试" {
  let transform_engine = TransformEngine::new("自定义转换引擎")
  
  // 创建自定义过滤器
  let custom_filter = CustomFilter::new("business_hours_filter", fn(data) {
    let timestamp = Data::get_attribute(data, "timestamp")
    match timestamp {
      Some(StringValue(ts)) => {
        let hour = parse_hour_from_timestamp(ts)
        hour >= 9 && hour <= 17  // 只允许工作时间的数据
      }
      _ => false
    }
  })
  
  // 创建自定义转换器
  let custom_transformer = CustomTransformer::new("business_metric_calculator", fn(data) {
    let response_time = Data::get_attribute(data, "response.time")
    let error_rate = Data::get_attribute(data, "error.rate")
    
    match (response_time, error_rate) {
      (Some(FloatValue(rt)), Some(FloatValue(er))) => {
        let business_score = calculate_business_score(rt, er)
        Data::add_attribute(data, "business.score", FloatValue(business_score))
      }
      _ => {}
    }
    data
  })
  
  // 添加自定义过滤器和转换器
  TransformEngine::add_custom_filter(transform_engine, custom_filter)
  TransformEngine::add_custom_transformer(transform_engine, custom_transformer)
  
  // 创建测试数据
  let metrics = [
    create_metric_with_attributes("business.metric", 100.0, [
      ("timestamp", "2023-01-01T10:30:00Z"),  // 工作时间
      ("response.time", "200.0"),
      ("error.rate", "0.05")
    ]),
    create_metric_with_attributes("business.metric", 200.0, [
      ("timestamp", "2023-01-01T20:30:00Z"),  // 非工作时间
      ("response.time", "150.0"),
      ("error.rate", "0.02")
    ]),
    create_metric_with_attributes("business.metric", 300.0, [
      ("timestamp", "2023-01-01T14:30:00Z"),  // 工作时间
      ("response.time", "300.0"),
      ("error.rate", "0.10")
    ])
  ]
  
  // 应用自定义过滤和转换
  let filtered_metrics = TransformEngine::apply_custom_filters(transform_engine, metrics.to_list())
  let transformed_metrics = TransformEngine::apply_custom_transformers(transform_engine, filtered_metrics)
  
  // 验证过滤结果
  assert_eq(filtered_metrics.length(), 2)  // 只有工作时间的数据通过
  
  // 验证转换结果
  for metric in transformed_metrics {
    let business_score = Metric::get_attribute(metric, "business.score")
    assert_true(business_score.is_some())
    
    let score_value = business_score.unwrap()
    match score_value {
      FloatValue(score) => {
        assert_true(score >= 0.0 && score <= 100.0)
      }
      _ => assert_true(false)
    }
  }
}

// 测试10: 过滤和转换性能优化
test "过滤和转换性能优化测试" {
  let transform_engine = TransformEngine::new("性能优化引擎")
  let performance_optimizer = TransformPerformanceOptimizer::new()
  
  // 配置性能优化策略
  let parallel_processing = ParallelProcessingStrategy::new(4)  // 4个并行线程
  let batch_processing = BatchProcessingStrategy::new(100)     // 批量大小100
  let caching_strategy = CachingStrategy::new(1000)             // 缓存1000个结果
  let index_optimization = IndexOptimizationStrategy::new()     // 索引优化
  
  TransformPerformanceOptimizer::add_strategy(performance_optimizer, parallel_processing)
  TransformPerformanceOptimizer::add_strategy(performance_optimizer, batch_processing)
  TransformPerformanceOptimizer::add_strategy(performance_optimizer, caching_strategy)
  TransformPerformanceOptimizer::add_strategy(performance_optimizer, index_optimization)
  
  // 创建复杂的过滤和转换规则
  let complex_filter = ComplexFilter::new()
  ComplexFilter::add_condition(complex_filter, AttributeCondition::new("service.name", In, ["api.service", "web.service"]))
  ComplexFilter::add_condition(complex_filter, AttributeCondition::new("response.time", Between, (100, 1000)))
  ComplexFilter::add_condition(complex_filter, AttributeCondition::new("http.status_code", NotIn, [404, 500]))
  
  let complex_transformer = ComplexTransformer::new()
  ComplexTransformer::add_operation(complex_transformer, AttributeRename::new("old.name", "new.name"))
  ComplexTransformer::add_operation(complex_transformer, AttributeCalculation::new("derived.value", "value1 + value2 * 0.5"))
  ComplexTransformer::add_operation(complex_transformer, AttributeEnrichment::new("enriched.field"))
  
  // 添加到引擎
  TransformEngine::add_filter(transform_engine, complex_filter)
  TransformEngine::add_transformer(transform_engine, complex_transformer)
  TransformEngine::set_performance_optimizer(transform_engine, performance_optimizer)
  
  // 创建大量测试数据
  let large_dataset = []
  for i in 0..=9999 {
    let metric = create_metric_with_attributes("performance.metric", i.to_float(), [
      ("service.name", ["api.service", "web.service", "db.service", "cache.service"][i % 4]),
      ("response.time", (50 + i % 2000).to_string()),
      ("http.status_code", (200 + (i % 6) * 100).to_string()),
      ("value1", (i % 100).to_string()),
      ("value2", ((i * 2) % 100).to_string())
    ])
    large_dataset = large_dataset.push(metric)
  }
  
  // 性能测试
  let start_time = Time::now()
  let processed_data = TransformEngine::process_dataset(transform_engine, large_dataset)
  let end_time = Time::now()
  let processing_time = end_time - start_time
  
  // 验证性能指标
  let performance_stats = TransformPerformanceOptimizer::get_stats(performance_optimizer)
  
  // 验证处理结果
  assert_true(processed_data.length() > 0)
  assert_true(processed_data.length() < 10000)  // 过滤后应该减少
  
  // 验证性能指标
  assert_true(processing_time < 10000)  // 应该在10秒内完成
  assert_true(performance_stats.throughput_items_per_second > 100)
  assert_true(performance_stats.parallel_efficiency > 0.5)
  assert_true(performance_stats.cache_hit_rate > 0.0)
  
  // 验证转换结果
  for data in processed_data.take(10) {  // 检查前10个
    assert_true(Data::has_attribute(data, "new.name"))
    assert_true(Data::has_attribute(data, "derived.value"))
    assert_true(Data::has_attribute(data, "enriched.field"))
  }
}

// 辅助函数：创建带属性的追踪
fn create_trace_with_attributes(trace_id : String, service_name : String, attributes : List<(String, String)>) -> Trace {
  let trace = Trace::new(trace_id, "test.operation", Ok, 100)
  Trace::add_attribute(trace, "service.name", service_name)
  
  for (key, value) in attributes {
    Trace::add_attribute(trace, key, value)
  }
  
  trace
}

// 辅助函数：创建带属性的指标
fn create_metric_with_attributes(name : String, value : Float, attributes : List<(String, String)>) -> Metric {
  let metric = Metric::new(name, value)
  
  for (key, value) in attributes {
    Metric::add_attribute(metric, key, value)
  }
  
  metric
}

// 辅助函数：从时间戳解析小时
fn parse_hour_from_timestamp(timestamp : String) -> Int {
  // 简化实现，实际应该使用时间解析库
  if timestamp.contains("T10:") { 10 }
  else if timestamp.contains("T14:") { 14 }
  else if timestamp.contains("T20:") { 20 }
  else { 12 }
}

// 辅助函数：计算业务分数
fn calculate_business_score(response_time : Float, error_rate : Float) -> Float {
  // 简化的业务分数计算
  let time_score = if response_time < 200 { 100.0 } else if response_time < 500 { 70.0 } else { 30.0 }
  let error_score = if error_rate < 0.01 { 100.0 } else if error_rate < 0.05 { 70.0 } else { 30.0 }
  (time_score + error_score) / 2.0
}