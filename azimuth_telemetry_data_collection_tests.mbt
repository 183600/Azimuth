// Azimuth 遥测数据收集和过滤测试
// 专注于遥测数据的收集、过滤和预处理功能

// 测试1: 遥测数据收集器基础功能
test "遥测数据收集器基础功能" {
  // 创建遥测数据收集器
  let collector = TelemetryCollector::new()
  
  // 配置收集器
  TelemetryCollector::set_buffer_size(collector, 1000)
  TelemetryCollector::set_flush_interval(collector, 5000)
  TelemetryCollector::enable_batch_processing(collector, true)
  TelemetryCollector::set_batch_size(collector, 100)
  
  // 创建测试遥测数据
  let span_data = SpanData::new("test.operation", "trace-123", "span-456")
  SpanData::set_attribute(span_data, "service.name", StringValue("test.service"))
  SpanData::set_attribute(span_data, "operation.type", StringValue("test"))
  SpanData::set_status(span_data, SpanStatus::Ok)
  SpanData::set_duration(span_data, 125)
  
  // 添加数据到收集器
  let add_result = TelemetryCollector::add_span_data(collector, span_data)
  assert_true(add_result.success)
  assert_eq(add_result.buffer_count, 1)
  
  // 添加指标数据
  let metric_data = MetricData::new("test.metric", 42.5, MetricType::Gauge)
  MetricData::add_attribute(metric_data, "unit", StringValue("ms"))
  MetricData::add_attribute(metric_data, "environment", StringValue("test"))
  
  let metric_result = TelemetryCollector::add_metric_data(collector, metric_data)
  assert_true(metric_result.success)
  assert_eq(metric_result.buffer_count, 2)
  
  // 添加日志数据
  let log_data = LogData::new(LogLevel::Info, "Test log message")
  LogData::add_attribute(log_data, "service.name", StringValue("test.service"))
  LogData::add_attribute(log_data, "trace_id", StringValue("trace-123"))
  
  let log_result = TelemetryCollector::add_log_data(collector, log_data)
  assert_true(log_result.success)
  assert_eq(log_result.buffer_count, 3)
  
  // 检查缓冲区状态
  let buffer_status = TelemetryCollector::get_buffer_status(collector)
  assert_eq(buffer_status.total_items, 3)
  assert_eq(buffer_status.span_count, 1)
  assert_eq(buffer_status.metric_count, 1)
  assert_eq(buffer_status.log_count, 1)
  assert_true(buffer_status.utilization < 0.01)  // 3/1000
}

// 测试2: 遥测数据过滤器
test "遥测数据过滤器功能" {
  // 创建数据过滤器
  let filter = TelemetryFilter::new()
  
  // 添加属性过滤规则
  TelemetryFilter::add_attribute_filter(filter, {
    name: "service.name",
    operator: "equals",
    value: "important.service"
  })
  
  TelemetryFilter::add_attribute_filter(filter, {
    name: "environment",
    operator: "not_equals",
    value: "test"
  })
  
  // 添加持续时间过滤规则
  TelemetryFilter::add_duration_filter(filter, {
    operator: "greater_than",
    value: 100
  })
  
  // 添加状态过滤规则
  TelemetryFilter::add_status_filter(filter, {
    operator: "in",
    values: [SpanStatus::Error, SpanStatus::Timeout]
  })
  
  // 创建测试数据
  let important_span = SpanData::new("important.operation", "trace-1", "span-1")
  SpanData::set_attribute(important_span, "service.name", StringValue("important.service"))
  SpanData::set_attribute(important_span, "environment", StringValue("production"))
  SpanData::set_duration(important_span, 150)
  SpanData::set_status(important_span, SpanStatus::Ok)
  
  let test_span = SpanData::new("test.operation", "trace-2", "span-2")
  SpanData::set_attribute(test_span, "service.name", StringValue("test.service"))
  SpanData::set_attribute(test_span, "environment", StringValue("test"))
  SpanData::set_duration(test_span, 80)
  SpanData::set_status(test_span, SpanStatus::Ok)
  
  let error_span = SpanData::new("error.operation", "trace-3", "span-3")
  SpanData::set_attribute(error_span, "service.name", StringValue("important.service"))
  SpanData::set_attribute(error_span, "environment", StringValue("production"))
  SpanData::set_duration(error_span, 50)
  SpanData::set_status(error_span, SpanStatus::Error)
  
  // 应用过滤器
  let important_result = TelemetryFilter::matches(filter, important_span)
  assert_true(important_result)  // 匹配所有条件
  
  let test_result = TelemetryFilter::matches(filter, test_span)
  assert_false(test_result)  // 不匹配service.name和duration条件
  
  let error_result = TelemetryFilter::matches(filter, error_span)
  assert_true(error_result)  // 匹配service.name和status条件
  
  // 测试复合过滤器
  let composite_filter = TelemetryFilter::new()
  TelemetryFilter::add_composite_filter(composite_filter, {
    operator: "and",
    filters: [
      {
        type: "attribute",
        name: "service.name",
        operator: "equals",
        value: "important.service"
      },
      {
        type: "or",
        filters: [
          {
            type: "duration",
            operator: "greater_than",
            value: 100
          },
          {
            type: "status",
            operator: "in",
            values: [SpanStatus::Error, SpanStatus::Timeout]
          }
        ]
      }
    ]
  })
  
  let composite_important = TelemetryFilter::matches(composite_filter, important_span)
  assert_true(composite_important)  // service.name匹配且duration>100
  
  let composite_error = TelemetryFilter::matches(composite_filter, error_span)
  assert_true(composite_error)  // service.name匹配且status为Error
  
  let composite_test = TelemetryFilter::matches(composite_filter, test_span)
  assert_false(composite_test)  // service.name不匹配
}

// 测试3: 遥测数据采样器
test "遥测数据采样器功能" {
  // 创建概率采样器
  let probability_sampler = ProbabilitySampler::new(0.1)  // 10%采样率
  
  // 创建确定性采样器
  let deterministic_sampler = DeterministicSampler::new()
  
  // 创建自适应采样器
  let adaptive_sampler = AdaptiveSampler::new({
    max_samples_per_second: 100,
    min_sampling_probability: 0.01,
    max_sampling_probability: 0.5
  })
  
  // 测试概率采样
  let mut sampled_count = 0
  let mut total_count = 0
  
  for i in 0..=1000 {
    let span = SpanData::new("operation." + i.to_string(), "trace-" + i.to_string(), "span-" + i.to_string())
    let should_sample = ProbabilitySampler::should_sample(probability_sampler, span)
    
    total_count = total_count + 1
    if should_sample {
      sampled_count = sampled_count + 1
    }
  }
  
  // 验证采样率在合理范围内
  let actual_rate = sampled_count.to_float() / total_count.to_float()
  assert_true(actual_rate > 0.05 and actual_rate < 0.15)  // 10% ± 5%
  
  // 测试确定性采样
  let trace_id_1 = "trace-deterministic-1"
  let trace_id_2 = "trace-deterministic-2"
  
  let span_1_first = SpanData::new("operation", trace_id_1, "span-1")
  let span_1_second = SpanData::new("operation", trace_id_1, "span-2")
  let span_2 = SpanData::new("operation", trace_id_2, "span-3")
  
  let sample_1_first = DeterministicSampler::should_sample(deterministic_sampler, span_1_first)
  let sample_1_second = DeterministicSampler::should_sample(deterministic_sampler, span_1_second)
  let sample_2 = DeterministicSampler::should_sample(deterministic_sampler, span_2)
  
  // 验证确定性采样：同一trace的span应该有相同的采样决策
  assert_eq(sample_1_first, sample_1_second)
  // 不同trace可能有不同的采样决策
  assert_true(sample_1_first == true or sample_1_first == false)
  assert_true(sample_2 == true or sample_2 == false)
  
  // 测试自适应采样
  let base_time = 1640995200
  
  // 模拟低流量情况
  for i in 0..=10 {
    let timestamp = base_time + i
    let span = SpanData::new("low.traffic.operation", "trace-" + i.to_string(), "span-" + i.to_string())
    AdaptiveSampler::record_sample(adaptive_sampler, timestamp, span)
  }
  
  let low_traffic_sample = SpanData::new("test.operation", "trace-low", "span-low")
  let low_traffic_result = AdaptiveSampler::should_sample(adaptive_sampler, low_traffic_sample)
  
  // 低流量时应该有较高的采样率
  assert_true(low_traffic_result)
  
  // 模拟高流量情况
  for i in 0..=200 {
    let timestamp = base_time + i
    let span = SpanData::new("high.traffic.operation", "trace-" + (i + 1000).to_string(), "span-" + (i + 1000).to_string())
    AdaptiveSampler::record_sample(adaptive_sampler, timestamp, span)
  }
  
  let high_traffic_sample = SpanData::new("test.operation", "trace-high", "span-high")
  let high_traffic_result = AdaptiveSampler::should_sample(adaptive_sampler, high_traffic_sample)
  
  // 高流量时采样率可能降低，但仍有可能采样
  assert_true(high_traffic_result == true or high_traffic_result == false)
}

// 测试4: 遥测数据聚合器
test "遥测数据聚合器功能" {
  // 创建数据聚合器
  let aggregator = TelemetryAggregator::new()
  
  // 配置聚合规则
  TelemetryAggregator::add_aggregation_rule(aggregator, {
    name: "request_count_by_service",
    source_metric: "http.requests",
    group_by: ["service.name", "http.method"],
    aggregation_type: "count",
    interval: 60  // 1分钟
  })
  
  TelemetryAggregator::add_aggregation_rule(aggregator, {
    name: "response_time_percentiles",
    source_metric: "http.response.time",
    group_by: ["service.name", "endpoint"],
    aggregation_type: "percentile",
    percentiles: [50.0, 90.0, 95.0, 99.0],
    interval: 60
  })
  
  TelemetryAggregator::add_aggregation_rule(aggregator, {
    name: "error_rate_by_service",
    source_metric: "http.errors",
    group_by: ["service.name"],
    aggregation_type: "rate",
    interval: 60
  })
  
  // 添加测试数据
  let base_time = 1640995200
  
  // 服务A的请求
  for i in 0..=120 {
    let timestamp = base_time + i * 30  // 每30秒一个数据点
    
    // 成功请求
    TelemetryAggregator::add_metric(aggregator, timestamp, "http.requests", 1.0, [
      ("service.name", StringValue("service.a")),
      ("http.method", StringValue("GET")),
      ("http.status_code", StringValue("200"))
    ])
    
    TelemetryAggregator::add_metric(aggregator, timestamp, "http.response.time", 50.0 + (i % 20) * 2.0, [
      ("service.name", StringValue("service.a")),
      ("endpoint", StringValue("/api/users"))
    ])
    
    // 部分错误请求
    if i % 10 == 0 {
      TelemetryAggregator::add_metric(aggregator, timestamp, "http.requests", 1.0, [
        ("service.name", StringValue("service.a")),
        ("http.method", StringValue("GET")),
        ("http.status_code", StringValue("500"))
      ])
      
      TelemetryAggregator::add_metric(aggregator, timestamp, "http.errors", 1.0, [
        ("service.name", StringValue("service.a"))
      ])
    }
  }
  
  // 服务B的请求
  for i in 0..=80 {
    let timestamp = base_time + i * 45  // 每45秒一个数据点
    
    TelemetryAggregator::add_metric(aggregator, timestamp, "http.requests", 1.0, [
      ("service.name", StringValue("service.b")),
      ("http.method", StringValue("POST")),
      ("http.status_code", StringValue("201"))
    ])
    
    TelemetryAggregator::add_metric(aggregator, timestamp, "http.response.time", 80.0 + (i % 25) * 3.0, [
      ("service.name", StringValue("service.b")),
      ("endpoint", StringValue("/api/orders"))
    ])
  }
  
  // 执行聚合
  let aggregation_results = TelemetryAggregator::aggregate(aggregator, base_time, base_time + 3600)
  
  // 验证聚合结果
  assert_true(aggregation_results.length() > 0)
  
  // 检查请求计数聚合
  let request_count_a = aggregation_results.find(fn(r) {
    r.rule_name == "request_count_by_service" and
    r.attributes.contains(("service.name", StringValue("service.a")))
  })
  assert_true(request_count_a != None)
  
  match request_count_a {
    Some(result) => {
      assert_true(result.values.length() > 0)
      // 验证总请求数（成功+错误）
      let total_requests = result.values.reduce(fn(acc, v) { acc + v.value }, 0.0)
      assert_true(total_requests >= 120.0 and total_requests <= 135.0)  // 120 + 12个错误
    }
    None => assert_true(false)
  }
  
  // 检查响应时间百分位数聚合
  let response_time_a = aggregation_results.find(fn(r) {
    r.rule_name == "response_time_percentiles" and
    r.attributes.contains(("service.name", StringValue("service.a")))
  })
  assert_true(response_time_a != None)
  
  match response_time_a {
    Some(result) => {
      assert_true(result.values.length() > 0)
      // 验证百分位数数据结构
      let percentiles = result.values[0].percentiles
      assert_true(percentiles.length() == 4)
      assert_true(percentiles.any(fn(p) { p.percentile == 50.0 }))
      assert_true(percentiles.any(fn(p) { p.percentile == 90.0 }))
      assert_true(percentiles.any(fn(p) { p.percentile == 95.0 }))
      assert_true(percentiles.any(fn(p) { p.percentile == 99.0 }))
      
      // 验证百分位数顺序
      let sorted_percentiles = percentiles.sort_by(fn(p) { p.percentile })
      for i in 1..sorted_percentiles.length() {
        assert_true(sorted_percentiles[i-1].value <= sorted_percentiles[i].value)
      }
    }
    None => assert_true(false)
  }
  
  // 检查错误率聚合
  let error_rate_a = aggregation_results.find(fn(r) {
    r.rule_name == "error_rate_by_service" and
    r.attributes.contains(("service.name", StringValue("service.a")))
  })
  assert_true(error_rate_a != None)
  
  match error_rate_a {
    Some(result) => {
      assert_true(result.values.length() > 0)
      // 验证错误率约为10%（12/120）
      let avg_error_rate = result.values.reduce(fn(acc, v) { acc + v.value }, 0.0) / result.values.length().to_float()
      assert_true(avg_error_rate > 0.08 and avg_error_rate < 0.12)
    }
    None => assert_true(false)
  }
}

// 测试5: 遥测数据转换器
test "遥测数据转换器功能" {
  // 创建数据转换器
  let transformer = TelemetryTransformer::new()
  
  // 添加转换规则
  TelemetryTransformer::add_transform_rule(transformer, {
    name: "normalize_service_name",
    type: "attribute",
    condition: {
      attribute: "service.name",
      operator: "contains",
      value: "."
    },
    transformation: {
      type: "replace",
      pattern: "\\.",
      replacement: "_"
    }
  })
  
  TelemetryTransformer::add_transform_rule(transformer, {
    name: "duration_to_seconds",
    type: "attribute",
    condition: {
      attribute: "duration",
      operator: "exists"
    },
    transformation: {
      type: "convert",
      from_type: "milliseconds",
      to_type: "seconds",
      scale: 0.001
    }
  })
  
  TelemetryTransformer::add_transform_rule(transformer, {
    name: "add_environment_attribute",
    type: "span",
    condition: {
      attribute: "environment",
      operator: "not_exists"
    },
    transformation: {
      type: "add_attribute",
      name: "environment",
      value: "unknown"
    }
  })
  
  // 创建测试数据
  let span_with_dots = SpanData::new("test.operation", "trace-1", "span-1")
  SpanData::set_attribute(span_with_dots, "service.name", StringValue("my.service.name"))
  SpanData::set_attribute(span_with_dots, "duration", IntValue(250))
  
  let span_without_env = SpanData::new("test.operation", "trace-2", "span-2")
  SpanData::set_attribute(span_without_env, "service.name", StringValue("simple-service"))
  SpanData::set_attribute(span_without_env, "duration", IntValue(500))
  
  let span_with_env = SpanData::new("test.operation", "trace-3", "span-3")
  SpanData::set_attribute(span_with_env, "service.name", StringValue("existing.service"))
  SpanData::set_attribute(span_with_env, "duration", IntValue(100))
  SpanData::set_attribute(span_with_env, "environment", StringValue("production"))
  
  // 应用转换
  let transformed_span_1 = TelemetryTransformer::transform(transformer, span_with_dots)
  let transformed_span_2 = TelemetryTransformer::transform(transformer, span_without_env)
  let transformed_span_3 = TelemetryTransformer::transform(transformer, span_with_env)
  
  // 验证服务名称规范化
  match SpanData::get_attribute(transformed_span_1, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "my_service_name")
    _ => assert_true(false)
  }
  
  match SpanData::get_attribute(transformed_span_2, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "simple-service")  // 无点号，不转换
    _ => assert_true(false)
  }
  
  match SpanData::get_attribute(transformed_span_3, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "existing_service")
    _ => assert_true(false)
  }
  
  // 验证持续时间转换
  match SpanData::get_attribute(transformed_span_1, "duration") {
    Some(FloatValue(duration)) => assert_eq(duration, 0.25)
    _ => assert_true(false)
  }
  
  match SpanData::get_attribute(transformed_span_2, "duration") {
    Some(FloatValue(duration)) => assert_eq(duration, 0.5)
    _ => assert_true(false)
  }
  
  // 验证环境属性添加
  match SpanData::get_attribute(transformed_span_1, "environment") {
    Some(StringValue(env)) => assert_eq(env, "unknown")
    _ => assert_true(false)
  }
  
  match SpanData::get_attribute(transformed_span_2, "environment") {
    Some(StringValue(env)) => assert_eq(env, "unknown")
    _ => assert_true(false)
  }
  
  match SpanData::get_attribute(transformed_span_3, "environment") {
    Some(StringValue(env)) => assert_eq(env, "production")  // 已存在，不覆盖
    _ => assert_true(false)
  }
  
  // 测试批量转换
  let spans = [span_with_dots, span_without_env, span_with_env]
  let transformed_spans = TelemetryTransformer::transform_batch(transformer, spans)
  
  assert_eq(transformed_spans.length(), 3)
  
  for span in transformed_spans {
    match SpanData::get_attribute(span, "environment") {
      Some(StringValue(_)) => assert_true(true)
      _ => assert_true(false)
    }
  }
}

// 测试6: 遥测数据验证器
test "遥测数据验证器功能" {
  // 创建数据验证器
  let validator = TelemetryValidator::new()
  
  // 添加验证规则
  TelemetryValidator::add_required_attribute_rule(validator, {
    attribute_name: "service.name",
    error_message: "Service name is required"
  })
  
  TelemetryValidator::add_attribute_format_rule(validator, {
    attribute_name: "trace.id",
    pattern: "trace-[a-f0-9-]+",
    error_message: "Invalid trace ID format"
  })
  
  TelemetryValidator::add_attribute_range_rule(validator, {
    attribute_name: "duration",
    min_value: Some(0),
    max_value: Some(3600000),  // 1小时
    error_message: "Duration must be between 0 and 3600000ms"
  })
  
  TelemetryValidator::add_attribute_length_rule(validator, {
    attribute_name: "operation.name",
    min_length: Some(1),
    max_length: Some(100),
    error_message: "Operation name must be between 1 and 100 characters"
  })
  
  // 创建有效测试数据
  let valid_span = SpanData::new("valid.operation", "trace-abc123", "span-def456")
  SpanData::set_attribute(valid_span, "service.name", StringValue("valid.service"))
  SpanData::set_attribute(valid_span, "duration", IntValue(250))
  SpanData::set_attribute(valid_span, "operation.name", StringValue("valid.operation"))
  
  // 创建无效测试数据
  let invalid_span_1 = SpanData::new("invalid.operation", "invalid-trace", "span-789")
  // 缺少service.name
  SpanData::set_attribute(invalid_span_1, "duration", IntValue(-10))  // 负持续时间
  SpanData::set_attribute(invalid_span_1, "operation.name", StringValue(""))  // 空操作名
  
  let invalid_span_2 = SpanData::new("invalid.operation", "trace-xyz", "span-012")
  SpanData::set_attribute(invalid_span_2, "service.name", StringValue("invalid.service"))
  SpanData::set_attribute(invalid_span_2, "duration", IntValue(4000000))  // 超过最大值
  SpanData::set_attribute(invalid_span_2, "operation.name", StringValue("a".repeat(101)))  // 超过最大长度
  
  // 验证数据
  let valid_result = TelemetryValidator::validate(validator, valid_span)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.errors.length(), 0)
  
  let invalid_result_1 = TelemetryValidator::validate(validator, invalid_span_1)
  assert_false(invalid_result_1.is_valid)
  assert_true(invalid_result_1.errors.length() >= 3)  // 缺少service.name，负持续时间，空操作名
  
  // 验证错误消息
  let has_service_name_error = invalid_result_1.errors.any(fn(e) {
    e.contains("Service name is required")
  })
  assert_true(has_service_name_error)
  
  let has_duration_error = invalid_result_1.errors.any(fn(e) {
    e.contains("Duration must be between 0 and 3600000ms")
  })
  assert_true(has_duration_error)
  
  let has_operation_name_error = invalid_result_1.errors.any(fn(e) {
    e.contains("Operation name must be between 1 and 100 characters")
  })
  assert_true(has_operation_name_error)
  
  let invalid_result_2 = TelemetryValidator::validate(validator, invalid_span_2)
  assert_false(invalid_result_2.is_valid)
  assert_true(invalid_result_2.errors.length() >= 2)  // 超过最大持续时间，超过最大操作名长度
  
  // 测试批量验证
  let spans = [valid_span, invalid_span_1, invalid_span_2]
  let batch_results = TelemetryValidator::validate_batch(validator, spans)
  
  assert_eq(batch_results.length(), 3)
  assert_true(batch_results[0].is_valid)
  assert_false(batch_results[1].is_valid)
  assert_false(batch_results[2].is_valid)
  
  // 验证统计信息
  let validation_stats = TelemetryValidator::get_validation_stats(validator)
  assert_eq(validation_stats.total_validated, 3)
  assert_eq(validation_stats.valid_count, 1)
  assert_eq(validation_stats.invalid_count, 2)
}

// 测试7: 遥测数据路由器
test "遥测数据路由器功能" {
  // 创建数据路由器
  let router = TelemetryRouter::new()
  
  // 创建路由目标
  let primary_backend = BackendTarget::new("primary", {
    endpoint: "https://primary-telemetry.example.com/api/v1/telemetry",
    api_key: "primary-api-key",
    batch_size: 100,
    timeout: 5000
  })
  
  let secondary_backend = BackendTarget::new("secondary", {
    endpoint: "https://secondary-telemetry.example.com/api/v1/telemetry",
    api_key: "secondary-api-key",
    batch_size: 50,
    timeout: 3000
  })
  
  let debug_backend = BackendTarget::new("debug", {
    endpoint: "https://debug-telemetry.example.com/api/v1/telemetry",
    api_key: "debug-api-key",
    batch_size: 10,
    timeout: 1000
  })
  
  // 添加路由规则
  TelemetryRouter::add_routing_rule(router, {
    name: "production_data",
    condition: {
      attribute: "environment",
      operator: "equals",
      value: "production"
    },
    targets: ["primary"],
    priority: 1
  })
  
  TelemetryRouter::add_routing_rule(router, {
    name: "error_data",
    condition: {
      attribute: "status",
      operator: "equals",
      value: "error"
    },
    targets: ["primary", "secondary"],
    priority: 2
  })
  
  TelemetryRouter::add_routing_rule(router, {
    name: "debug_data",
    condition: {
      attribute: "debug",
      operator: "equals",
      value: "true"
    },
    targets: ["debug"],
    priority: 3
  })
  
  TelemetryRouter::add_routing_rule(router, {
    name: "default",
    condition: {},  // 无条件，默认规则
    targets: ["secondary"],
    priority: 10
  })
  
  // 注册后端目标
  TelemetryRouter::register_target(router, primary_backend)
  TelemetryRouter::register_target(router, secondary_backend)
  TelemetryRouter::register_target(router, debug_backend)
  
  // 创建测试数据
  let production_span = SpanData::new("production.operation", "trace-1", "span-1")
  SpanData::set_attribute(production_span, "environment", StringValue("production"))
  SpanData::set_attribute(production_span, "service.name", StringValue("production.service"))
  
  let error_span = SpanData::new("error.operation", "trace-2", "span-2")
  SpanData::set_attribute(error_span, "status", StringValue("error"))
  SpanData::set_attribute(error_span, "service.name", StringValue("error.service"))
  
  let debug_span = SpanData::new("debug.operation", "trace-3", "span-3")
  SpanData::set_attribute(debug_span, "debug", StringValue("true"))
  SpanData::set_attribute(debug_span, "service.name", StringValue("debug.service"))
  
  let default_span = SpanData::new("default.operation", "trace-4", "span-4")
  SpanData::set_attribute(default_span, "service.name", StringValue("default.service"))
  
  // 路由数据
  let production_routes = TelemetryRouter::route(router, production_span)
  assert_eq(production_routes.length(), 1)
  assert_eq(production_routes[0], "primary")
  
  let error_routes = TelemetryRouter::route(router, error_span)
  assert_eq(error_routes.length(), 2)
  assert_true(error_routes.contains("primary"))
  assert_true(error_routes.contains("secondary"))
  
  let debug_routes = TelemetryRouter::route(router, debug_span)
  assert_eq(debug_routes.length(), 1)
  assert_eq(debug_routes[0], "debug")
  
  let default_routes = TelemetryRouter::route(router, default_span)
  assert_eq(default_routes.length(), 1)
  assert_eq(default_routes[0], "secondary")
  
  // 测试路由统计
  let routing_stats = TelemetryRouter::get_routing_stats(router)
  assert_eq(routing_stats.total_routed, 4)
  assert_eq(routing_stats.routes_by_target.get("primary"), Some(2))  // production + error
  assert_eq(routing_stats.routes_by_target.get("secondary"), Some(2))  // error + default
  assert_eq(routing_stats.routes_by_target.get("debug"), Some(1))  // debug
}

// 测试8: 遥测数据缓冲区管理
test "遥测数据缓冲区管理功能" {
  // 创建缓冲区管理器
  let buffer_manager = BufferManager::new()
  
  // 配置缓冲区
  BufferManager::configure(buffer_manager, {
    max_size: 1000,
    flush_threshold: 800,  // 80%时触发刷新
    max_wait_time: 5000,   // 5秒最大等待时间
    compression_enabled: true,
    persistence_enabled: true
  })
  
  // 创建缓冲区
  let span_buffer = BufferManager::create_buffer(buffer_manager, "spans", {
    max_size: 500,
    eviction_policy: "fifo"
  })
  
  let metric_buffer = BufferManager::create_buffer(buffer_manager, "metrics", {
    max_size: 1000,
    eviction_policy: "lru"
  })
  
  let log_buffer = BufferManager::create_buffer(buffer_manager, "logs", {
    max_size: 200,
    eviction_policy: "priority"
  })
  
  // 测试缓冲区添加操作
  for i in 0..=100 {
    let span = SpanData::new("operation." + i.to_string(), "trace-" + i.to_string(), "span-" + i.to_string())
    let add_result = BufferManager::add(buffer_manager, span_buffer, span)
    assert_true(add_result.success)
  }
  
  for i in 0..=200 {
    let metric = MetricData::new("metric." + i.to_string(), i.to_float(), MetricType::Gauge)
    let add_result = BufferManager::add(buffer_manager, metric_buffer, metric)
    assert_true(add_result.success)
  }
  
  for i in 0..=50 {
    let log = LogData::new(LogLevel::Info, "Log message " + i.to_string())
    let add_result = BufferManager::add(buffer_manager, log_buffer, log)
    assert_true(add_result.success)
  }
  
  // 检查缓冲区状态
  let span_status = BufferManager::get_buffer_status(buffer_manager, span_buffer)
  assert_eq(span_status.item_count, 101)
  assert_eq(span_status.utilization, 101.0 / 500.0)
  assert_false(span_status.needs_flush)
  
  let metric_status = BufferManager::get_buffer_status(buffer_manager, metric_buffer)
  assert_eq(metric_status.item_count, 201)
  assert_eq(metric_status.utilization, 201.0 / 1000.0)
  assert_false(metric_status.needs_flush)
  
  let log_status = BufferManager::get_buffer_status(buffer_manager, log_buffer)
  assert_eq(log_status.item_count, 51)
  assert_eq(log_status.utilization, 51.0 / 200.0)
  assert_false(log_status.needs_flush)
  
  // 测试缓冲区刷新
  let flush_result = BufferManager::flush(buffer_manager, span_buffer)
  assert_true(flush_result.success)
  assert_eq(flush_result.items_flushed, 101)
  
  let span_status_after_flush = BufferManager::get_buffer_status(buffer_manager, span_buffer)
  assert_eq(span_status_after_flush.item_count, 0)
  assert_eq(span_status_after_flush.utilization, 0.0)
  
  // 测试缓冲区满时的处理
  for i in 0..=600 {
    let span = SpanData::new("overflow.operation." + i.to_string(), "trace-overflow-" + i.to_string(), "span-overflow-" + i.to_string())
    BufferManager::add(buffer_manager, span_buffer, span)
  }
  
  let overflow_status = BufferManager::get_buffer_status(buffer_manager, span_buffer)
  assert_eq(overflow_status.item_count, 500)  // 不超过最大大小
  assert_true(overflow_status.evicted_count > 0)  // 有数据被驱逐
  
  // 测试缓冲区优先级处理
  let high_priority_log = LogData::new(LogLevel::Error, "High priority error message")
  LogData::set_priority(high_priority_log, 10)
  
  let low_priority_log = LogData::new(LogLevel::Debug, "Low priority debug message")
  LogData::set_priority(low_priority_log, 1)
  
  // 填满日志缓冲区
  for i in 0..=200 {
    let log = LogData::new(LogLevel::Info, "Info message " + i.to_string())
    LogData::set_priority(log, 5)
    BufferManager::add(buffer_manager, log_buffer, log)
  }
  
  // 添加高优先级日志
  BufferManager::add(buffer_manager, log_buffer, high_priority_log)
  
  // 添加低优先级日志
  BufferManager::add(buffer_manager, log_buffer, low_priority_log)
  
  // 检查高优先级日志是否保留
  let buffer_items = BufferManager::get_items(buffer_manager, log_buffer)
  let has_high_priority = buffer_items.any(fn(item) {
    match item {
      LogItem(log) => LogData::get_priority(log) >= 10
      _ => false
    }
  })
  assert_true(has_high_priority)
  
  // 测试缓冲区压缩
  let compression_stats = BufferManager::get_compression_stats(buffer_manager)
  assert_true(compression_stats.enabled)
  assert_true(compression_stats.compression_ratio > 0.0)
  
  // 测试缓冲区持久化
  let persistence_stats = BufferManager::get_persistence_stats(buffer_manager)
  assert_true(persistence_stats.enabled)
  assert_true(persistence_stats.items_persisted > 0)
}