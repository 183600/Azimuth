// Azimuth Telemetry Aggregation Analysis Tests
// 遥测数据聚合分析测试 - 验证遥测数据的聚合、分析和统计功能

// 测试1: 度量数据聚合
test "度量数据聚合测试" {
  // 创建度量数据点
  let metric_points = [
    MetricPoint { timestamp: 1609459200000L, value: 10.5, attributes: [("service", StringValue("service-a")), ("version", StringValue("1.0.0"))] },
    MetricPoint { timestamp: 1609459260000L, value: 15.2, attributes: [("service", StringValue("service-a")), ("version", StringValue("1.0.0"))] },
    MetricPoint { timestamp: 1609459320000L, value: 12.8, attributes: [("service", StringValue("service-b")), ("version", StringValue("1.0.0"))] },
    MetricPoint { timestamp: 1609459380000L, value: 18.3, attributes: [("service", StringValue("service-b")), ("version", StringValue("1.1.0"))] },
    MetricPoint { timestamp: 1609459440000L, value: 14.7, attributes: [("service", StringValue("service-a")), ("version", StringValue("1.1.0"))] }
  ]
  
  // 按服务名称聚合
  let service_aggregation = aggregate_metrics_by_attribute(metric_points, "service")
  
  // 验证聚合结果
  assert_eq(service_aggregation.length(), 2) // 两个服务
  
  // 验证service-a的聚合
  let service_a_data = find_aggregation_by_key(service_aggregation, "service-a")
  match service_a_data {
    Some(data) => {
      assert_eq(data.count, 3)
      assert_eq(data.sum, 10.5 + 15.2 + 14.7)
      assert_eq(data.min, 10.5)
      assert_eq(data.max, 15.2)
      assert_eq(data.average, data.sum / 3.0)
    }
    None => assert_true(false)
  }
  
  // 验证service-b的聚合
  let service_b_data = find_aggregation_by_key(service_aggregation, "service-b")
  match service_b_data {
    Some(data) => {
      assert_eq(data.count, 2)
      assert_eq(data.sum, 12.8 + 18.3)
      assert_eq(data.min, 12.8)
      assert_eq(data.max, 18.3)
      assert_eq(data.average, data.sum / 2.0)
    }
    None => assert_true(false)
  }
  
  // 按版本聚合
  let version_aggregation = aggregate_metrics_by_attribute(metric_points, "version")
  
  // 验证聚合结果
  assert_eq(version_aggregation.length(), 2) // 两个版本
  
  // 验证版本1.0.0的聚合
  let version_1_0_0_data = find_aggregation_by_key(version_aggregation, "1.0.0")
  match version_1_0_0_data {
    Some(data) => {
      assert_eq(data.count, 3)
      assert_eq(data.sum, 10.5 + 15.2 + 12.8)
      assert_eq(data.min, 10.5)
      assert_eq(data.max, 15.2)
    }
    None => assert_true(false)
  }
  
  // 验证版本1.1.0的聚合
  let version_1_1_0_data = find_aggregation_by_key(version_aggregation, "1.1.0")
  match version_1_1_0_data {
    Some(data) => {
      assert_eq(data.count, 2)
      assert_eq(data.sum, 18.3 + 14.7)
      assert_eq(data.min, 14.7)
      assert_eq(data.max, 18.3)
    }
    None => assert_true(false)
  }
}

// 测试2: 时间序列数据分析
test "时间序列数据分析测试" {
  // 创建时间序列数据点
  let time_series_points = [
    TimeSeriesPoint { timestamp: 1609459200000L, value: 100.0 },
    TimeSeriesPoint { timestamp: 1609459260000L, value: 110.0 },
    TimeSeriesPoint { timestamp: 1609459320000L, value: 105.0 },
    TimeSeriesPoint { timestamp: 1609459380000L, value: 120.0 },
    TimeSeriesPoint { timestamp: 1609459440000L, value: 115.0 },
    TimeSeriesPoint { timestamp: 1609459500000L, value: 130.0 },
    TimeSeriesPoint { timestamp: 1609459560000L, value: 125.0 },
    TimeSeriesPoint { timestamp: 1609459620000L, value: 140.0 }
  ]
  
  // 计算基本统计信息
  let stats = calculate_time_series_statistics(time_series_points)
  
  // 验证统计结果
  assert_eq(stats.count, 8)
  assert_eq(stats.sum, 100.0 + 110.0 + 105.0 + 120.0 + 115.0 + 130.0 + 125.0 + 140.0)
  assert_eq(stats.min, 100.0)
  assert_eq(stats.max, 140.0)
  assert_eq(stats.average, stats.sum / 8.0)
  
  // 计算趋势
  let trend = calculate_time_series_trend(time_series_points)
  match trend {
    Increasing => assert_true(true) // 值总体上在增加
    Decreasing => assert_true(false)
    Stable => assert_true(false)
  }
  
  // 计算移动平均（窗口大小为3）
  let moving_averages = calculate_moving_average(time_series_points, 3)
  
  // 验证移动平均结果
  assert_eq(moving_averages.length(), 6) // 8-3+1 = 6
  
  // 验证第一个移动平均：(100+110+105)/3 = 105.0
  assert_eq(moving_averages[0], 105.0)
  
  // 验证最后一个移动平均：(130+125+140)/3 = 131.666...
  assert_true(abs(moving_averages[5] - 131.6666666667) < 0.0001)
  
  // 计算变化率
  let change_rates = calculate_change_rates(time_series_points)
  
  // 验证变化率结果
  assert_eq(change_rates.length(), 7) // 8-1 = 7
  
  // 验证第一个变化率：(110-100)/100 = 0.1
  assert_true(abs(change_rates[0] - 0.1) < 0.0001)
  
  // 验证第二个变化率：(105-110)/110 = -0.04545...
  assert_true(abs(change_rates[1] - (-0.0454545455)) < 0.0001)
}

// 测试3: 分布式追踪数据分析
test "分布式追踪数据分析测试" {
  // 创建分布式追踪数据
  let trace_data = [
    SpanData {
      trace_id: "trace-001",
      span_id: "span-001",
      parent_span_id: None,
      operation_name: "GET /api/users",
      start_time: 1609459200000L,
      end_time: 1609459200150L,
      status: "OK",
      service_name: "api-gateway",
      attributes: [("http.method", StringValue("GET")), ("http.status_code", IntValue(200))]
    },
    SpanData {
      trace_id: "trace-001",
      span_id: "span-002",
      parent_span_id: Some("span-001"),
      operation_name: "database.query",
      start_time: 1609459200050L,
      end_time: 1609459200120L,
      status: "OK",
      service_name: "user-service",
      attributes: [("db.type", StringValue("postgresql")), ("db.statement", StringValue("SELECT * FROM users"))]
    },
    SpanData {
      trace_id: "trace-001",
      span_id: "span-003",
      parent_span_id: Some("span-001"),
      operation_name: "cache.get",
      start_time: 1609459200125L,
      end_time: 1609459200140L,
      status: "OK",
      service_name: "cache-service",
      attributes: [("cache.key", StringValue("user:123")), ("cache.hit", BoolValue(true))]
    },
    SpanData {
      trace_id: "trace-002",
      span_id: "span-004",
      parent_span_id: None,
      operation_name: "POST /api/orders",
      start_time: 1609459210000L,
      end_time: 1609459210300L,
      status: "ERROR",
      service_name: "api-gateway",
      attributes: [("http.method", StringValue("POST")), ("http.status_code", IntValue(500))]
    },
    SpanData {
      trace_id: "trace-002",
      span_id: "span-005",
      parent_span_id: Some("span-004"),
      operation_name: "database.insert",
      start_time: 1609459210100L,
      end_time: 1609459210280L,
      status: "ERROR",
      service_name: "order-service",
      attributes: [("db.type", StringValue("postgresql")), ("error.message", StringValue("Connection timeout"))]
    }
  ]
  
  // 按追踪ID分组
  let traces_by_id = group_spans_by_trace_id(trace_data)
  
  // 验证分组结果
  assert_eq(traces_by_id.length(), 2) // 两个追踪
  
  // 验证trace-001
  let trace_001 = find_trace_by_id(traces_by_id, "trace-001")
  match trace_001 {
    Some(spans) => {
      assert_eq(spans.length(), 3)
      
      // 验证根跨度
      let root_span = find_root_span(spans)
      match root_span {
        Some(span) => {
          assert_eq(span.span_id, "span-001")
          assert_eq(span.operation_name, "GET /api/users")
          assert_eq(span.service_name, "api-gateway")
          assert_eq(span.parent_span_id, None)
        }
        None => assert_true(false)
      }
      
      // 验证追踪持续时间
      let trace_duration = calculate_trace_duration(spans)
      assert_eq(trace_duration, 150) // 1609459200150 - 1609459200000
      
      // 验证服务调用次数
      let service_counts = count_service_calls(spans)
      assert_eq(service_counts.length(), 3) // 三个服务
      
      let api_gateway_count = find_service_call_count(service_counts, "api-gateway")
      match api_gateway_count {
        Some(count) => assert_eq(count, 1)
        None => assert_true(false)
      }
      
      let user_service_count = find_service_call_count(service_counts, "user-service")
      match user_service_count {
        Some(count) => assert_eq(count, 1)
        None => assert_true(false)
      }
      
      let cache_service_count = find_service_call_count(service_counts, "cache-service")
      match cache_service_count {
        Some(count) => assert_eq(count, 1)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证trace-002
  let trace_002 = find_trace_by_id(traces_by_id, "trace-002")
  match trace_002 {
    Some(spans) => {
      assert_eq(spans.length(), 2)
      
      // 验证错误状态
      let error_spans = filter_spans_by_status(spans, "ERROR")
      assert_eq(error_spans.length(), 2) // 两个错误跨度
      
      // 验证错误率
      let error_rate = calculate_error_rate(spans)
      assert_eq(error_rate, 1.0) // 100%错误率
    }
    None => assert_true(false)
  }
  
  // 计算整体统计
  let overall_stats = calculate_overall_trace_statistics(trace_data)
  
  // 验证整体统计
  assert_eq(overall_stats.total_traces, 2)
  assert_eq(overall_stats.total_spans, 5)
  assert_eq(overall_stats.successful_traces, 1)
  assert_eq(overall_stats.failed_traces, 1)
  assert_eq(overall_stats.success_rate, 0.5) // 50%
  
  // 验证平均追踪持续时间
  let average_trace_duration = overall_stats.average_trace_duration
  assert_eq(average_trace_duration, (150 + 300) / 2) // (trace-001: 150ms, trace-002: 300ms)
}

// 测试4: 日志数据分析
test "日志数据分析测试" {
  // 创建日志数据
  let log_data = [
    LogData {
      timestamp: 1609459200000L,
      level: "INFO",
      message: "User login successful",
      service_name: "auth-service",
      trace_id: Some("trace-001"),
      span_id: Some("span-001"),
      attributes: [("user.id", StringValue("user123")), ("ip.address", StringValue("192.168.1.1"))]
    },
    LogData {
      timestamp: 1609459200050L,
      level: "INFO",
      message: "Database connection established",
      service_name: "user-service",
      trace_id: Some("trace-001"),
      span_id: Some("span-002"),
      attributes: [("db.connection", StringValue("conn-12345"))]
    },
    LogData {
      timestamp: 1609459200100L,
      level: "WARN",
      message: "Cache miss for user profile",
      service_name: "cache-service",
      trace_id: Some("trace-001"),
      span_id: Some("span-003"),
      attributes: [("cache.key", StringValue("user:123")), ("cache.ttl", IntValue(3600))]
    },
    LogData {
      timestamp: 1609459210000L,
      level: "ERROR",
      message: "Database connection timeout",
      service_name: "order-service",
      trace_id: Some("trace-002"),
      span_id: Some("span-004"),
      attributes: [("db.connection", StringValue("conn-67890")), ("timeout", IntValue(5000))]
    },
    LogData {
      timestamp: 1609459210050L,
      level: "ERROR",
      message: "Order processing failed",
      service_name: "order-service",
      trace_id: Some("trace-002"),
      span_id: Some("span-005"),
      attributes: [("order.id", StringValue("order-456")), ("error.code", StringValue("DB_TIMEOUT"))]
    }
  ]
  
  // 按日志级别分组
  let logs_by_level = group_logs_by_level(log_data)
  
  // 验证分组结果
  assert_eq(logs_by_level.length(), 3) // INFO, WARN, ERROR
  
  // 验证INFO级别
  let info_logs = find_logs_by_level(logs_by_level, "INFO")
  match info_logs {
    Some(logs) => assert_eq(logs.length(), 2)
    None => assert_true(false)
  }
  
  // 验证WARN级别
  let warn_logs = find_logs_by_level(logs_by_level, "WARN")
  match warn_logs {
    Some(logs) => assert_eq(logs.length(), 1)
    None => assert_true(false)
  }
  
  // 验证ERROR级别
  let error_logs = find_logs_by_level(logs_by_level, "ERROR")
  match error_logs {
    Some(logs) => assert_eq(logs.length(), 2)
    None => assert_true(false)
  }
  
  // 按服务分组
  let logs_by_service = group_logs_by_service(log_data)
  
  // 验证分组结果
  assert_eq(logs_by_service.length(), 3) // auth-service, user-service, cache-service, order-service
  
  // 验证order-service的日志
  let order_service_logs = find_logs_by_service(logs_by_service, "order-service")
  match order_service_logs {
    Some(logs) => {
      assert_eq(logs.length(), 2)
      
      // 验证都是ERROR级别
      for log in logs {
        assert_eq(log.level, "ERROR")
      }
    }
    None => assert_true(false)
  }
  
  // 按追踪ID分组
  let logs_by_trace = group_logs_by_trace_id(log_data)
  
  // 验证分组结果
  assert_eq(logs_by_trace.length(), 2) // trace-001, trace-002
  
  // 验证trace-001的日志
  let trace_001_logs = find_logs_by_trace_id(logs_by_trace, "trace-001")
  match trace_001_logs {
    Some(logs) => {
      assert_eq(logs.length(), 3)
      
      // 验证日志时间顺序
      assert_true(logs[0].timestamp <= logs[1].timestamp)
      assert_true(logs[1].timestamp <= logs[2].timestamp)
    }
    None => assert_true(false)
  }
  
  // 计算错误率
  let error_rate = calculate_log_error_rate(log_data)
  assert_eq(error_rate, 0.4) // 2/5 = 40%
  
  // 识别错误模式
  let error_patterns = identify_error_patterns(log_data)
  
  // 验证错误模式
  assert_eq(error_patterns.length(), 2) // 两种错误模式
  
  let db_timeout_pattern = find_error_pattern(error_patterns, "Database connection timeout")
  match db_timeout_pattern {
    Some(pattern) => {
      assert_eq(pattern.count, 1)
      assert_eq(pattern.service_name, "order-service")
    }
    None => assert_true(false)
  }
  
  let order_processing_failed_pattern = find_error_pattern(error_patterns, "Order processing failed")
  match order_processing_failed_pattern {
    Some(pattern) => {
      assert_eq(pattern.count, 1)
      assert_eq(pattern.service_name, "order-service")
    }
    None => assert_true(false)
  }
}

// 测试5: 多维度数据分析
test "多维度数据分析测试" {
  // 创建多维度数据
  let multi_dimensional_data = [
    DimensionalDataPoint {
      timestamp: 1609459200000L,
      metrics: [
        ("response_time", 150.0),
        ("cpu_usage", 45.2),
        ("memory_usage", 67.8),
        ("throughput", 1200.0)
      ],
      dimensions: [
        ("service", "api-gateway"),
        ("region", "us-west-1"),
        ("version", "1.0.0")
      ]
    },
    DimensionalDataPoint {
      timestamp: 1609459260000L,
      metrics: [
        ("response_time", 120.0),
        ("cpu_usage", 50.1),
        ("memory_usage", 70.2),
        ("throughput", 1350.0)
      ],
      dimensions: [
        ("service", "api-gateway"),
        ("region", "us-west-1"),
        ("version", "1.0.0")
      ]
    },
    DimensionalDataPoint {
      timestamp: 1609459320000L,
      metrics: [
        ("response_time", 200.0),
        ("cpu_usage", 65.3),
        ("memory_usage", 75.4),
        ("throughput", 980.0)
      ],
      dimensions: [
        ("service", "user-service"),
        ("region", "us-east-1"),
        ("version", "1.1.0")
      ]
    },
    DimensionalDataPoint {
      timestamp: 1609459380000L,
      metrics: [
        ("response_time", 180.0),
        ("cpu_usage", 60.5),
        ("memory_usage", 72.1),
        ("throughput", 1100.0)
      ],
      dimensions: [
        ("service", "user-service"),
        ("region", "us-east-1"),
        ("version", "1.1.0")
      ]
    }
  ]
  
  // 按服务和区域分组
  let grouped_by_service_region = group_dimensional_data(multi_dimensional_data, ["service", "region"])
  
  // 验证分组结果
  assert_eq(grouped_by_service_region.length(), 2) // 两种组合
  
  // 验证api-gateway/us-west-1的聚合
  let api_gateway_west = find_dimensional_group(grouped_by_service_region, [("service", "api-gateway"), ("region", "us-west-1")])
  match api_gateway_west {
    Some(group) => {
      assert_eq(group.count, 2)
      
      // 验证response_time的平均值
      let response_time_avg = calculate_metric_average(group, "response_time")
      assert_eq(response_time_avg, (150.0 + 120.0) / 2.0)
      
      // 验证cpu_usage的平均值
      let cpu_usage_avg = calculate_metric_average(group, "cpu_usage")
      assert_eq(cpu_usage_avg, (45.2 + 50.1) / 2.0)
      
      // 验证throughput的平均值
      let throughput_avg = calculate_metric_average(group, "throughput")
      assert_eq(throughput_avg, (1200.0 + 1350.0) / 2.0)
    }
    None => assert_true(false)
  }
  
  // 验证user-service/us-east-1的聚合
  let user_service_east = find_dimensional_group(grouped_by_service_region, [("service", "user-service"), ("region", "us-east-1")])
  match user_service_east {
    Some(group) => {
      assert_eq(group.count, 2)
      
      // 验证response_time的平均值
      let response_time_avg = calculate_metric_average(group, "response_time")
      assert_eq(response_time_avg, (200.0 + 180.0) / 2.0)
      
      // 验证cpu_usage的平均值
      let cpu_usage_avg = calculate_metric_average(group, "cpu_usage")
      assert_eq(cpu_usage_avg, (65.3 + 60.5) / 2.0)
      
      // 验证throughput的平均值
      let throughput_avg = calculate_metric_average(group, "throughput")
      assert_eq(throughput_avg, (980.0 + 1100.0) / 2.0)
    }
    None => assert_true(false)
  }
  
  // 按版本分组
  let grouped_by_version = group_dimensional_data(multi_dimensional_data, ["version"])
  
  // 验证分组结果
  assert_eq(grouped_by_version.length(), 2) // 两个版本
  
  // 验证版本1.0.0的聚合
  let version_1_0_0 = find_dimensional_group(grouped_by_version, [("version", "1.0.0")])
  match version_1_0_0 {
    Some(group) => {
      assert_eq(group.count, 2)
      
      // 验证响应时间更小（性能更好）
      let response_time_avg = calculate_metric_average(group, "response_time")
      assert_true(response_time_avg < 150.0)
      
      // 验证吞吐量更高
      let throughput_avg = calculate_metric_average(group, "throughput")
      assert_true(throughput_avg > 1200.0)
    }
    None => assert_true(false)
  }
  
  // 验证版本1.1.0的聚合
  let version_1_1_0 = find_dimensional_group(grouped_by_version, [("version", "1.1.0")])
  match version_1_1_0 {
    Some(group) => {
      assert_eq(group.count, 2)
      
      // 验证响应时间更大（性能较差）
      let response_time_avg = calculate_metric_average(group, "response_time")
      assert_true(response_time_avg > 180.0)
      
      // 验证吞吐量较低
      let throughput_avg = calculate_metric_average(group, "throughput")
      assert_true(throughput_avg < 1100.0)
    }
    None => assert_true(false)
  }
  
  // 计算相关性分析
  let correlations = calculate_metric_correlations(multi_dimensional_data, ["response_time", "cpu_usage", "memory_usage", "throughput"])
  
  // 验证相关性结果
  assert_eq(correlations.length(), 6) // 4个指标两两组合: 4*3/2 = 6
  
  // 验证response_time和throughput的负相关
  let response_time_throughput_corr = find_correlation(correlations, "response_time", "throughput")
  match response_time_throughput_corr {
    Some(corr) => assert_true(corr < 0.0) // 负相关
    None => assert_true(false)
  }
  
  // 验证cpu_usage和memory_usage的正相关
  let cpu_memory_corr = find_correlation(correlations, "cpu_usage", "memory_usage")
  match cpu_memory_corr {
    Some(corr) => assert_true(corr > 0.0) // 正相关
    None => assert_true(false)
  }
}

// 类型定义
type MetricPoint {
  timestamp: Int64
  value: Double
  attributes: Array[(String, AttributeValue)]
}

type AggregationResult {
  key: String
  count: Int
  sum: Double
  min: Double
  max: Double
  average: Double
}

type TimeSeriesPoint {
  timestamp: Int64
  value: Double
}

type TimeSeriesStatistics {
  count: Int
  sum: Double
  min: Double
  max: Double
  average: Double
}

type Trend {
  Increasing
  Decreasing
  Stable
}

type SpanData {
  trace_id: String
  span_id: String
  parent_span_id: Option[String]
  operation_name: String
  start_time: Int64
  end_time: Int64
  status: String
  service_name: String
  attributes: Array[(String, AttributeValue)]
}

type TraceStatistics {
  total_traces: Int
  total_spans: Int
  successful_traces: Int
  failed_traces: Int
  success_rate: Double
  average_trace_duration: Int64
}

type LogData {
  timestamp: Int64
  level: String
  message: String
  service_name: String
  trace_id: Option[String]
  span_id: Option[String]
  attributes: Array[(String, AttributeValue)]
}

type ErrorPattern {
  message: String
  count: Int
  service_name: String
}

type DimensionalDataPoint {
  timestamp: Int64
  metrics: Array[(String, Double)]
  dimensions: Array[(String, String)]
}

type DimensionalGroup {
  dimensions: Array[(String, String)]
  count: Int
  data_points: Array[DimensionalDataPoint]
}

type AttributeValue {
  StringValue(String)
  IntValue(Int)
  DoubleValue(Double)
  BoolValue(Bool)
}

// 辅助函数实现
fn aggregate_metrics_by_attribute(points: Array[MetricPoint], attribute: String) -> Array[AggregationResult] {
  let mut groups = []
  
  // 按属性值分组
  for point in points {
    let mut attribute_value = ""
    for (key, value) in point.attributes {
      if key == attribute {
        match value {
          StringValue(s) => attribute_value = s
          _ => ()
        }
      }
    }
    
    // 查找或创建分组
    let mut group_found = false
    let mut updated_groups = []
    
    for group in groups {
      if group.key == attribute_value {
        group_found = true
        let new_count = group.count + 1
        let new_sum = group.sum + point.value
        let new_min = if point.value < group.min { point.value } else { group.min }
        let new_max = if point.value > group.max { point.value } else { group.max }
        let new_average = new_sum / new_count.to_double()
        
        updated_groups = updated_groups + [AggregationResult {
          key: group.key,
          count: new_count,
          sum: new_sum,
          min: new_min,
          max: new_max,
          average: new_average
        }]
      } else {
        updated_groups = updated_groups + [group]
      }
    }
    
    if !group_found {
      updated_groups = updated_groups + [AggregationResult {
        key: attribute_value,
        count: 1,
        sum: point.value,
        min: point.value,
        max: point.value,
        average: point.value
      }]
    }
    
    groups = updated_groups
  }
  
  groups
}

fn find_aggregation_by_key(aggregations: Array[AggregationResult], key: String) -> Option[AggregationResult] {
  for agg in aggregations {
    if agg.key == key {
      return Some(agg)
    }
  }
  None
}

fn calculate_time_series_statistics(points: Array[TimeSeriesPoint]) -> TimeSeriesStatistics {
  if points.length() == 0 {
    return TimeSeriesStatistics { count: 0, sum: 0.0, min: 0.0, max: 0.0, average: 0.0 }
  }
  
  let mut sum = 0.0
  let mut min = points[0].value
  let mut max = points[0].value
  
  for point in points {
    sum = sum + point.value
    if point.value < min { min = point.value }
    if point.value > max { max = point.value }
  }
  
  TimeSeriesStatistics {
    count: points.length(),
    sum: sum,
    min: min,
    max: max,
    average: sum / points.length().to_double()
  }
}

fn calculate_time_series_trend(points: Array[TimeSeriesPoint]) -> Trend {
  if points.length() < 2 {
    return Stable
  }
  
  let first_half_avg = calculate_average_of_range(points, 0, points.length() / 2)
  let second_half_avg = calculate_average_of_range(points, points.length() / 2, points.length())
  
  if second_half_avg > first_half_avg * 1.05 {
    Increasing
  } else if second_half_avg < first_half_avg * 0.95 {
    Decreasing
  } else {
    Stable
  }
}

fn calculate_average_of_range(points: Array[TimeSeriesPoint], start: Int, end: Int) -> Double {
  if start >= end {
    return 0.0
  }
  
  let mut sum = 0.0
  let mut count = 0
  
  let mut i = start
  while i < end && i < points.length() {
    sum = sum + points[i].value
    count = count + 1
    i = i + 1
  }
  
  if count == 0 { 0.0 } else { sum / count.to_double() }
}

fn calculate_moving_average(points: Array[TimeSeriesPoint], window_size: Int) -> Array[Double] {
  if points.length() < window_size || window_size <= 0 {
    return []
  }
  
  let mut result = []
  let mut i = 0
  
  while i <= points.length() - window_size {
    let mut sum = 0.0
    let mut j = 0
    
    while j < window_size {
      sum = sum + points[i + j].value
      j = j + 1
    }
    
    result = result + [sum / window_size.to_double()]
    i = i + 1
  }
  
  result
}

fn calculate_change_rates(points: Array[TimeSeriesPoint]) -> Array[Double] {
  if points.length() < 2 {
    return []
  }
  
  let mut result = []
  let mut i = 0
  
  while i < points.length() - 1 {
    let current = points[i].value
    let next = points[i + 1].value
    
    if current != 0.0 {
      result = result + [(next - current) / current]
    } else {
      result = result + [0.0]
    }
    
    i = i + 1
  }
  
  result
}

fn group_spans_by_trace_id(spans: Array[SpanData]) -> Array[(String, Array[SpanData])] {
  let mut groups = []
  
  for span in spans {
    let mut group_found = false
    let mut updated_groups = []
    
    for (trace_id, span_list) in groups {
      if trace_id == span.trace_id {
        group_found = true
        updated_groups = updated_groups + [(trace_id, span_list + [span])]
      } else {
        updated_groups = updated_groups + [(trace_id, span_list)]
      }
    }
    
    if !group_found {
      updated_groups = updated_groups + [(span.trace_id, [span])]
    }
    
    groups = updated_groups
  }
  
  groups
}

fn find_trace_by_id(traces: Array[(String, Array[SpanData])], trace_id: String) -> Option[Array[SpanData]] {
  for (id, spans) in traces {
    if id == trace_id {
      return Some(spans)
    }
  }
  None
}

fn find_root_span(spans: Array[SpanData]) -> Option[SpanData] {
  for span in spans {
    match span.parent_span_id {
      None => return Some(span)
      Some(_) => () // 不是根跨度
    }
  }
  None
}

fn calculate_trace_duration(spans: Array[SpanData]) -> Int64 {
  if spans.length() == 0 {
    return 0L
  }
  
  let mut min_start = spans[0].start_time
  let mut max_end = spans[0].end_time
  
  for span in spans {
    if span.start_time < min_start { min_start = span.start_time }
    if span.end_time > max_end { max_end = span.end_time }
  }
  
  max_end - min_start
}

fn count_service_calls(spans: Array[SpanData]) -> Array[(String, Int)] {
  let mut counts = []
  
  for span in spans {
    let mut service_found = false
    let mut updated_counts = []
    
    for (service, count) in counts {
      if service == span.service_name {
        service_found = true
        updated_counts = updated_counts + [(service, count + 1)]
      } else {
        updated_counts = updated_counts + [(service, count)]
      }
    }
    
    if !service_found {
      updated_counts = updated_counts + [(span.service_name, 1)]
    }
    
    counts = updated_counts
  }
  
  counts
}

fn find_service_call_count(counts: Array[(String, Int)], service: String) -> Option[Int] {
  for (s, count) in counts {
    if s == service {
      return Some(count)
    }
  }
  None
}

fn filter_spans_by_status(spans: Array[SpanData], status: String) -> Array[SpanData] {
  let mut result = []
  
  for span in spans {
    if span.status == status {
      result = result + [span]
    }
  }
  
  result
}

fn calculate_error_rate(spans: Array[SpanData]) -> Double {
  if spans.length() == 0 {
    return 0.0
  }
  
  let error_spans = filter_spans_by_status(spans, "ERROR")
  error_spans.length().to_double() / spans.length().to_double()
}

fn calculate_overall_trace_statistics(spans: Array[SpanData]) -> TraceStatistics {
  let traces = group_spans_by_trace_id(spans)
  
  let mut successful_traces = 0
  let mut failed_traces = 0
  let mut total_duration = 0L
  
  for (_, span_list) in traces {
    let error_rate = calculate_error_rate(span_list)
    let duration = calculate_trace_duration(span_list)
    
    if error_rate == 0.0 {
      successful_traces = successful_traces + 1
    } else {
      failed_traces = failed_traces + 1
    }
    
    total_duration = total_duration + duration
  }
  
  let total_traces = traces.length()
  let success_rate = if total_traces > 0 {
    successful_traces.to_double() / total_traces.to_double()
  } else {
    0.0
  }
  
  let average_trace_duration = if total_traces > 0 {
    total_duration / total_traces.to_int64()
  } else {
    0L
  }
  
  TraceStatistics {
    total_traces: total_traces,
    total_spans: spans.length(),
    successful_traces: successful_traces,
    failed_traces: failed_traces,
    success_rate: success_rate,
    average_trace_duration: average_trace_duration
  }
}

fn group_logs_by_level(logs: Array[LogData]) -> Array[(String, Array[LogData])] {
  let mut groups = []
  
  for log in logs {
    let mut group_found = false
    let mut updated_groups = []
    
    for (level, log_list) in groups {
      if level == log.level {
        group_found = true
        updated_groups = updated_groups + [(level, log_list + [log])]
      } else {
        updated_groups = updated_groups + [(level, log_list)]
      }
    }
    
    if !group_found {
      updated_groups = updated_groups + [(log.level, [log])]
    }
    
    groups = updated_groups
  }
  
  groups
}

fn find_logs_by_level(groups: Array[(String, Array[LogData])], level: String) -> Option[Array[LogData]] {
  for (l, logs) in groups {
    if l == level {
      return Some(logs)
    }
  }
  None
}

fn group_logs_by_service(logs: Array[LogData]) -> Array[(String, Array[LogData])] {
  let mut groups = []
  
  for log in logs {
    let mut group_found = false
    let mut updated_groups = []
    
    for (service, log_list) in groups {
      if service == log.service_name {
        group_found = true
        updated_groups = updated_groups + [(service, log_list + [log])]
      } else {
        updated_groups = updated_groups + [(service, log_list)]
      }
    }
    
    if !group_found {
      updated_groups = updated_groups + [(log.service_name, [log])]
    }
    
    groups = updated_groups
  }
  
  groups
}

fn find_logs_by_service(groups: Array[(String, Array[LogData])], service: String) -> Option[Array[LogData]] {
  for (s, logs) in groups {
    if s == service {
      return Some(logs)
    }
  }
  None
}

fn group_logs_by_trace_id(logs: Array[LogData]) -> Array[(String, Array[LogData])] {
  let mut groups = []
  
  for log in logs {
    match log.trace_id {
      Some(trace_id) => {
        let mut group_found = false
        let mut updated_groups = []
        
        for (id, log_list) in groups {
          if id == trace_id {
            group_found = true
            updated_groups = updated_groups + [(id, log_list + [log])]
          } else {
            updated_groups = updated_groups + [(id, log_list)]
          }
        }
        
        if !group_found {
          updated_groups = updated_groups + [(trace_id, [log])]
        }
        
        groups = updated_groups
      }
      None => () // 没有追踪ID的日志忽略
    }
  }
  
  groups
}

fn find_logs_by_trace_id(groups: Array[(String, Array[LogData])], trace_id: String) -> Option[Array[LogData]] {
  for (id, logs) in groups {
    if id == trace_id {
      return Some(logs)
    }
  }
  None
}

fn calculate_log_error_rate(logs: Array[LogData]) -> Double {
  if logs.length() == 0 {
    return 0.0
  }
  
  let mut error_count = 0
  
  for log in logs {
    if log.level == "ERROR" {
      error_count = error_count + 1
    }
  }
  
  error_count.to_double() / logs.length().to_double()
}

fn identify_error_patterns(logs: Array[LogData]) -> Array[ErrorPattern] {
  let mut patterns = []
  
  for log in logs {
    if log.level == "ERROR" {
      let mut pattern_found = false
      let mut updated_patterns = []
      
      for pattern in patterns {
        if pattern.message == log.message && pattern.service_name == log.service_name {
          pattern_found = true
          updated_patterns = updated_patterns + [ErrorPattern {
            message: pattern.message,
            count: pattern.count + 1,
            service_name: pattern.service_name
          }]
        } else {
          updated_patterns = updated_patterns + [pattern]
        }
      }
      
      if !pattern_found {
        updated_patterns = updated_patterns + [ErrorPattern {
          message: log.message,
          count: 1,
          service_name: log.service_name
        }]
      }
      
      patterns = updated_patterns
    }
  }
  
  patterns
}

fn find_error_pattern(patterns: Array[ErrorPattern], message: String) -> Option[ErrorPattern] {
  for pattern in patterns {
    if pattern.message == message {
      return Some(pattern)
    }
  }
  None
}

fn group_dimensional_data(data: Array[DimensionalDataPoint], dimensions: Array[String]) -> Array[(Array[(String, String)], DimensionalGroup)] {
  let mut groups = []
  
  for point in data {
    // 提取当前点的维度值
    let mut current_dims = []
    for dim in dimensions {
      let mut dim_value = ""
      for (d, v) in point.dimensions {
        if d == dim {
          dim_value = v
        }
      }
      current_dims = current_dims + [(dim, dim_value)]
    }
    
    // 查找或创建分组
    let mut group_found = false
    let mut updated_groups = []
    
    for (group_dims, group) in groups {
      if dimensions_match(group_dims, current_dims) {
        group_found = true
        updated_groups = updated_groups + [(group_dims, DimensionalGroup {
          dimensions: group.dimensions,
          count: group.count + 1,
          data_points: group.data_points + [point]
        })]
      } else {
        updated_groups = updated_groups + [(group_dims, group)]
      }
    }
    
    if !group_found {
      updated_groups = updated_groups + [(current_dims, DimensionalGroup {
        dimensions: current_dims,
        count: 1,
        data_points: [point]
      })]
    }
    
    groups = updated_groups
  }
  
  groups
}

fn dimensions_match(dims1: Array[(String, String)], dims2: Array[(String, String)]) -> Bool {
  if dims1.length() != dims2.length() {
    return false
  }
  
  for (d1, v1) in dims1 {
    let mut found = false
    for (d2, v2) in dims2 {
      if d1 == d2 && v1 == v2 {
        found = true
        break
      }
    }
    if !found {
      return false
    }
  }
  
  true
}

fn find_dimensional_group(groups: Array[(Array[(String, String)], DimensionalGroup)], dimensions: Array[(String, String)]) -> Option[DimensionalGroup] {
  for (group_dims, group) in groups {
    if dimensions_match(group_dims, dimensions) {
      return Some(group)
    }
  }
  None
}

fn calculate_metric_average(group: DimensionalGroup, metric: String) -> Double {
  if group.count == 0 {
    return 0.0
  }
  
  let mut sum = 0.0
  let mut count = 0
  
  for point in group.data_points {
    for (m, value) in point.metrics {
      if m == metric {
        sum = sum + value
        count = count + 1
      }
    }
  }
  
  if count == 0 { 0.0 } else { sum / count.to_double() }
}

fn calculate_metric_correlations(data: Array[DimensionalDataPoint], metrics: Array[String]) -> Array[(String, String, Double)] {
  let mut correlations = []
  
  // 计算所有指标对之间的相关性
  let mut i = 0
  while i < metrics.length() {
    let mut j = i + 1
    while j < metrics.length() {
      let metric1 = metrics[i]
      let metric2 = metrics[j]
      
      let correlation = calculate_correlation(data, metric1, metric2)
      correlations = correlations + [(metric1, metric2, correlation)]
      
      j = j + 1
    }
    i = i + 1
  }
  
  correlations
}

fn calculate_correlation(data: Array[DimensionalDataPoint], metric1: String, metric2: String) -> Double {
  if data.length() < 2 {
    return 0.0
  }
  
  // 提取两个指标的值
  let mut values1 = []
  let mut values2 = []
  
  for point in data {
    let mut v1 = 0.0
    let mut v2 = 0.0
    let mut found1 = false
    let mut found2 = false
    
    for (m, value) in point.metrics {
      if m == metric1 {
        v1 = value
        found1 = true
      } else if m == metric2 {
        v2 = value
        found2 = true
      }
    }
    
    if found1 && found2 {
      values1 = values1 + [v1]
      values2 = values2 + [v2]
    }
  }
  
  if values1.length() < 2 {
    return 0.0
  }
  
  // 计算平均值
  let mut sum1 = 0.0
  let mut sum2 = 0.0
  
  for i in 0..values1.length() {
    sum1 = sum1 + values1[i]
    sum2 = sum2 + values2[i]
  }
  
  let avg1 = sum1 / values1.length().to_double()
  let avg2 = sum2 / values2.length().to_double()
  
  // 计算协方差和方差
  let mut covariance = 0.0
  let mut variance1 = 0.0
  let mut variance2 = 0.0
  
  for i in 0..values1.length() {
    let diff1 = values1[i] - avg1
    let diff2 = values2[i] - avg2
    
    covariance = covariance + diff1 * diff2
    variance1 = variance1 + diff1 * diff1
    variance2 = variance2 + diff2 * diff2
  }
  
  if variance1 == 0.0 || variance2 == 0.0 {
    return 0.0
  }
  
  covariance / (sqrt(variance1) * sqrt(variance2))
}

fn find_correlation(correlations: Array[(String, String, Double)], metric1: String, metric2: String) -> Option[Double] {
  for (m1, m2, corr) in correlations {
    if (m1 == metric1 && m2 == metric2) || (m1 == metric2 && m2 == metric1) {
      return Some(corr)
    }
  }
  None
}

fn abs(x: Double) -> Double {
  if x < 0.0 { -x } else { x }
}

fn sqrt(x: Double) -> Double {
  // 简化实现，实际应该使用数学库
  if x == 0.0 { 0.0 } else { x / 2.0 }
}