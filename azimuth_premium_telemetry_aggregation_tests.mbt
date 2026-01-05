// Azimuth Premium Telemetry Aggregation Tests
// 高级遥测数据聚合测试用例 - 测试复杂的遥测数据聚合场景

// Test 1: 多维度指标聚合
test "multi-dimensional metrics aggregation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "aggregation_test")
  
  // 创建多维计数器
  let counter = Meter::create_counter(meter, "requests_total", Some("Total requests"), Some("count"))
  
  // 添加不同维度的指标数据
  let http_attrs = Attributes::new()
  Attributes::set(http_attrs, "method", StringValue("GET"))
  Attributes::set(http_attrs, "status", StringValue("200"))
  Attributes::set(http_attrs, "service", StringValue("auth"))
  Counter::add(counter, 100.0, Some(http_attrs))
  
  let post_attrs = Attributes::new()
  Attributes::set(post_attrs, "method", StringValue("POST"))
  Attributes::set(post_attrs, "status", StringValue("201"))
  Attributes::set(post_attrs, "service", StringValue("auth"))
  Counter::add(counter, 50.0, Some(post_attrs))
  
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "method", StringValue("GET"))
  Attributes::set(error_attrs, "status", StringValue("500"))
  Attributes::set(error_attrs, "service", StringValue("payment"))
  Counter::add(counter, 10.0, Some(error_attrs))
  
  // 测试聚合函数
  let aggregation_result = MetricsAggregator::aggregate_by_dimension(counter, "service")
  assert_true(aggregation_result.length() >= 2)
  
  // 验证按服务分组的聚合结果
  let auth_total = MetricsAggregator::get_dimension_value(aggregation_result, "auth")
  match auth_total {
    Some(value) => assert_eq(value, 150.0)
    None => assert_true(false)
  }
  
  let payment_total = MetricsAggregator::get_dimension_value(aggregation_result, "payment")
  match payment_total {
    Some(value) => assert_eq(value, 10.0)
    None => assert_true(false)
  }
}

// Test 2: 时间窗口数据聚合
test "time window data aggregation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "time_window_test")
  
  // 创建直方图用于时间窗口聚合
  let histogram = Meter::create_histogram(
    meter, 
    "response_time", 
    Some("Response time histogram"), 
    Some("ms")
  )
  
  // 模拟不同时间点的数据
  let base_time = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  // 第一个小时的数据
  Histogram::record_with_timestamp(histogram, 100.0, Some(Attributes::new()), Some(base_time))
  Histogram::record_with_timestamp(histogram, 150.0, Some(Attributes::new()), Some(base_time + 1800000L))
  Histogram::record_with_timestamp(histogram, 200.0, Some(Attributes::new()), Some(base_time + 3600000L))
  
  // 第二个小时的数据
  Histogram::record_with_timestamp(histogram, 120.0, Some(Attributes::new()), Some(base_time + 7200000L))
  Histogram::record_with_timestamp(histogram, 180.0, Some(Attributes::new()), Some(base_time + 9000000L))
  Histogram::record_with_timestamp(histogram, 220.0, Some(Attributes::new()), Some(base_time + 10800000L))
  
  // 测试按小时聚合
  let hourly_aggregation = TimeWindowAggregator::aggregate_by_hour(histogram, base_time, base_time + 10800000L)
  assert_eq(hourly_aggregation.length(), 2)
  
  // 验证第一小时的平均值
  let first_hour_avg = TimeWindowAggregator::get_hourly_average(hourly_aggregation, 0)
  match first_hour_avg {
    Some(avg) => assert_eq(avg, 150.0)
    None => assert_true(false)
  }
  
  // 验证第二小时的最大值
  let second_hour_max = TimeWindowAggregator::get_hourly_maximum(hourly_aggregation, 1)
  match second_hour_max {
    Some(max) => assert_eq(max, 220.0)
    None => assert_true(false)
  }
}

// Test 3: 跨服务遥测数据关联聚合
test "cross-service telemetry correlation aggregation" {
  // 创建多个服务的跨度数据
  let trace_id = "trace_12345"
  
  // 服务A的跨度
  let span_a_ctx = SpanContext::new(trace_id, "span_a", true, "")
  let span_a = Span::new("service_a_operation", Server, span_a_ctx)
  Span::add_event(span_a, "start_processing", Some([("service", StringValue("service_a"))]))
  Span::set_status(span_a, Ok, Some("Completed successfully"))
  Span::end(span_a)
  
  // 服务B的跨度
  let span_b_ctx = SpanContext::new(trace_id, "span_b", true, "")
  let span_b = Span::new("service_b_operation", Client, span_b_ctx)
  Span::add_event(span_b, "api_call", Some([("service", StringValue("service_b"))]))
  Span::set_status(span_b, Ok, Some("API call successful"))
  Span::end(span_b)
  
  // 服务C的跨度
  let span_c_ctx = SpanContext::new(trace_id, "span_c", true, "")
  let span_c = Span::new("service_c_operation", Internal, span_c_ctx)
  Span::add_event(span_c, "data_processing", Some([("service", StringValue("service_c"))]))
  Span::set_status(span_c, Error, Some("Processing error"))
  Span::end(span_c)
  
  // 测试跨服务关联聚合
  let trace_aggregation = CrossServiceAggregator::aggregate_by_trace(trace_id)
  assert_eq(trace_aggregation.span_count, 3)
  assert_eq(trace_aggregation.service_count, 3)
  
  // 验证服务分布
  let service_distribution = CrossServiceAggregator::get_service_distribution(trace_aggregation)
  assert_true(service_distribution.contains("service_a"))
  assert_true(service_distribution.contains("service_b"))
  assert_true(service_distribution.contains("service_c"))
  
  // 验证错误率计算
  let error_rate = CrossServiceAggregator::calculate_error_rate(trace_aggregation)
  assert_eq(error_rate, 0.3333333333333333) // 1/3 的跨度有错误
}

// Test 4: 异常检测和模式识别聚合
test "anomaly detection and pattern recognition aggregation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "anomaly_detection_test")
  
  // 创建异常检测指标
  let error_counter = Meter::create_counter(meter, "errors_total", Some("Total errors"), Some("count"))
  let latency_histogram = Meter::create_histogram(meter, "latency", Some("Latency"), Some("ms"))
  
  // 正常模式数据
  for i in 1..=100 {
    Histogram::record(latency_histogram, 50.0 + (i % 20).to_float())
  }
  
  // 异常模式数据 - 突然增加的延迟
  for i in 1..=10 {
    Histogram::record(latency_histogram, 500.0 + (i * 10).to_float())
    Counter::add(error_counter, 1.0)
  }
  
  // 恢复正常模式
  for i in 1..=50 {
    Histogram::record(latency_histogram, 45.0 + (i % 15).to_float())
  }
  
  // 测试异常检测
  let anomalies = AnomalyDetector::detect_latency_anomalies(latency_histogram)
  assert_true(anomalies.length() > 0)
  
  // 验证异常时间窗口
  let anomaly_window = AnomalyDetector::get_anomaly_window(anomalies, 0)
  match anomaly_window {
    Some(window) => {
      assert_true(window.start_time > 0L)
      assert_true(window.end_time > window.start_time)
      assert_true(window.severity > 0.5)
    }
    None => assert_true(false)
  }
  
  // 测试模式识别
  let patterns = PatternRecognizer::recognize_error_patterns(error_counter)
  assert_true(patterns.length() > 0)
  
  // 验证错误突发模式
  let burst_pattern = PatternRecognizer::get_burst_pattern(patterns, 0)
  match burst_pattern {
    Some(pattern) => {
      assert_eq(pattern.pattern_type, "burst")
      assert_true(pattern.duration > 0L)
      assert_true(pattern.intensity > 0.0)
    }
    None => assert_true(false)
  }
}

// Test 5: 实时流数据聚合
test "real-time stream data aggregation" {
  // 创建实时流处理器
  let stream_processor = StreamProcessor::new()
  
  // 配置流聚合规则
  let aggregation_rules = [
    ("http_requests", "sum", "1m"), // 1分钟窗口的请求总和
    ("response_time", "avg", "5m"), // 5分钟窗口的平均响应时间
    ("error_rate", "rate", "1m")    // 1分钟窗口的错误率
  ]
  StreamProcessor::configure_aggregation_rules(stream_processor, aggregation_rules)
  
  // 模拟实时数据流
  let base_time = Time::now()
  
  // 第一批数据
  StreamProcessor::process_metric(stream_processor, "http_requests", 100.0, base_time)
  StreamProcessor::process_metric(stream_processor, "response_time", 50.0, base_time)
  StreamProcessor::process_metric(stream_processor, "error_rate", 0.01, base_time)
  
  // 第二批数据
  StreamProcessor::process_metric(stream_processor, "http_requests", 150.0, base_time + 30000L)
  StreamProcessor::process_metric(stream_processor, "response_time", 75.0, base_time + 30000L)
  StreamProcessor::process_metric(stream_processor, "error_rate", 0.02, base_time + 30000L)
  
  // 第三批数据
  StreamProcessor::process_metric(stream_processor, "http_requests", 120.0, base_time + 60000L)
  StreamProcessor::process_metric(stream_processor, "response_time", 60.0, base_time + 60000L)
  StreamProcessor::process_metric(stream_processor, "error_rate", 0.015, base_time + 60000L)
  
  // 测试实时聚合结果
  let aggregated_results = StreamProcessor::get_aggregated_results(stream_processor, base_time, base_time + 60000L)
  assert_true(aggregated_results.length() >= 3)
  
  // 验证请求总和
  let request_sum = StreamProcessor::get_aggregated_value(aggregated_results, "http_requests", "sum")
  match request_sum {
    Some(sum) => assert_eq(sum, 370.0)
    None => assert_true(false)
  }
  
  // 验证平均响应时间
  let avg_response_time = StreamProcessor::get_aggregated_value(aggregated_results, "response_time", "avg")
  match avg_response_time {
    Some(avg) => assert_eq(avg, 61.666666666666664) // (50+75+60)/3
    None => assert_true(false)
  }
  
  // 验证错误率
  let error_rate = StreamProcessor::get_aggregated_value(aggregated_results, "error_rate", "rate")
  match error_rate {
    Some(rate) => assert_eq(rate, 0.015) // 平均错误率
    None => assert_true(false)
  }
}

// Test 6: 自定义聚合函数测试
test "custom aggregation functions" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "custom_aggregation_test")
  
  // 创建测试指标
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("unit"))
  
  // 添加测试数据
  let test_values = [10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0]
  for value in test_values {
    Histogram::record(histogram, value)
  }
  
  // 测试百分位数聚合
  let p50 = CustomAggregator::percentile(histogram, 50.0)
  match p50 {
    Some(value) => assert_eq(value, 55.0)
    None => assert_true(false)
  }
  
  let p95 = CustomAggregator::percentile(histogram, 95.0)
  match p95 {
    Some(value) => assert_eq(value, 95.0)
    None => assert_true(false)
  }
  
  let p99 = CustomAggregator::percentile(histogram, 99.0)
  match p99 {
    Some(value) => assert_eq(value, 99.0)
    None => assert_true(false)
  }
  
  // 测试标准差聚合
  let std_dev = CustomAggregator::standard_deviation(histogram)
  match std_dev {
    Some(value) => assert_true(value > 0.0)
    None => assert_true(false)
  }
  
  // 测试变异系数聚合
  let coefficient_of_variation = CustomAggregator::coefficient_of_variation(histogram)
  match coefficient_of_variation {
    Some(value) => assert_true(value > 0.0)
    None => assert_true(false)
  }
  
  // 测试自定义加权平均
  let weights = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
  let weighted_avg = CustomAggregator::weighted_average(histogram, weights)
  match weighted_avg {
    Some(value) => assert_true(value > 50.0) // 加权平均应该大于简单平均
    None => assert_true(false)
  }
}

// Test 7: 内存优化的大数据集聚合
test "memory-optimized large dataset aggregation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "large_dataset_test")
  
  // 创建内存优化的聚合器
  let memory_optimized_aggregator = MemoryOptimizedAggregator::new(1000) // 1000个数据点的缓冲区
  
  // 添加大量数据点（超过缓冲区大小）
  for i in 1..=5000 {
    let value = (i % 100).to_float() + Math::random() * 10.0
    MemoryOptimizedAggregator::add_data_point(memory_optimized_aggregator, value)
  }
  
  // 测试内存优化的聚合函数
  let average = MemoryOptimizedAggregator::calculate_average(memory_optimized_aggregator)
  match average {
    Some(avg) => {
      assert_true(avg >= 0.0)
      assert_true(avg <= 110.0)
    }
    None => assert_true(false)
  }
  
  let minimum = MemoryOptimizedAggregator::calculate_minimum(memory_optimized_aggregator)
  match minimum {
    Some(min) => {
      assert_true(min >= 0.0)
      assert_true(min < 10.0)
    }
    None => assert_true(false)
  }
  
  let maximum = MemoryOptimizedAggregator::calculate_maximum(memory_optimized_aggregator)
  match maximum {
    Some(max) => {
      assert_true(max >= 99.0)
      assert_true(max < 110.0)
    }
    None => assert_true(false)
  }
  
  // 测试分块聚合
  let chunk_results = MemoryOptimizedAggregator::aggregate_by_chunks(memory_optimized_aggregator, 1000)
  assert_true(chunk_results.length() > 0)
  
  // 验证内存使用情况
  let memory_usage = MemoryOptimizedAggregator::get_memory_usage(memory_optimized_aggregator)
  assert_true(memory_usage.buffer_size <= 1000)
  assert_true(memory_usage.total_data_points == 5000)
}