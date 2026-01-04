// Azimuth 高级遥测功能测试用例
// 专注于遥测系统的高级功能和MoonBit语言特性

// 测试1: 自适应采样策略
test "自适应采样策略测试" {
  // 定义采样策略枚举
  enum SamplingStrategy {
    AlwaysOn
    AlwaysOff
    TraceIdRatio(Float)
    Adaptive
  }
  
  // 定义自适应采样配置
  type AdaptiveSamplingConfig = {
    min_sampling_rate: Float,
    max_sampling_rate: Float,
    error_rate_threshold: Float,
    latency_threshold: Float,
    throughput_threshold: Float
  }
  
  // 创建自适应采样配置
  let config = {
    min_sampling_rate: 0.01,
    max_sampling_rate: 0.5,
    error_rate_threshold: 0.05,
    latency_threshold: 0.5,
    throughput_threshold: 1000.0
  }
  
  // 定义采样决策函数
  let should_sample = fn(strategy: SamplingStrategy, config: AdaptiveSamplingConfig, 
                          error_rate: Float, avg_latency: Float, throughput: Float) {
    match strategy {
      SamplingStrategy::AlwaysOn => true
      SamplingStrategy::AlwaysOff => false
      SamplingStrategy::TraceIdRatio(ratio) => {
        // 简化：使用固定比例采样
        ratio > 0.5
      }
      SamplingStrategy::Adaptive => {
        // 基于错误率、延迟和吞吐量的自适应采样
        let error_factor = if error_rate > config.error_rate_threshold { 1.5 } else { 1.0 }
        let latency_factor = if avg_latency > config.latency_threshold { 1.3 } else { 1.0 }
        let throughput_factor = if throughput > config.throughput_threshold { 0.8 } else { 1.0 }
        
        let adjusted_rate = config.max_sampling_rate * error_factor * latency_factor * throughput_factor
        adjusted_rate.clamp(config.min_sampling_rate, config.max_sampling_rate) > 0.25
      }
    }
  }
  
  // 测试不同采样策略
  assert_true(should_sample(SamplingStrategy::AlwaysOn, config, 0.0, 0.0, 0.0))
  assert_false(should_sample(SamplingStrategy::AlwaysOff, config, 0.0, 0.0, 0.0))
  assert_false(should_sample(SamplingStrategy::TraceIdRatio(0.3), config, 0.0, 0.0, 0.0))
  assert_true(should_sample(SamplingStrategy::TraceIdRatio(0.7), config, 0.0, 0.0, 0.0))
  
  // 测试自适应采样
  // 正常情况下不应该采样
  assert_false(should_sample(SamplingStrategy::Adaptive, config, 0.01, 0.1, 500.0))
  
  // 高错误率应该增加采样
  assert_true(should_sample(SamplingStrategy::Adaptive, config, 0.1, 0.1, 500.0))
  
  // 高延迟应该增加采样
  assert_true(should_sample(SamplingStrategy::Adaptive, config, 0.01, 1.0, 500.0))
  
  // 高吞吐量应该减少采样
  assert_false(should_sample(SamplingStrategy::Adaptive, config, 0.1, 1.0, 2000.0))
}

// 测试2: 智能聚合算法
test "智能聚合算法测试" {
  // 定义聚合类型
  enum AggregationType {
    Sum
    Average
    Min
    Max
    Histogram
    Percentile
  }
  
  // 定义时间窗口
  type TimeWindow = {
    start_time: Int,
    end_time: Int,
    duration: Int
  }
  
  // 定义数据点
  type DataPoint = {
    timestamp: Int,
    value: Float,
    attributes: Array[(String, String)]
  }
  
  // 创建测试数据点
  let data_points = [
    { timestamp: 1640995200, value: 10.5, attributes: [("endpoint", "/api/users")] },
    { timestamp: 1640995210, value: 12.3, attributes: [("endpoint", "/api/users")] },
    { timestamp: 1640995220, value: 8.7, attributes: [("endpoint", "/api/users")] },
    { timestamp: 1640995230, value: 15.2, attributes: [("endpoint", "/api/orders")] },
    { timestamp: 1640995240, value: 9.8, attributes: [("endpoint", "/api/orders")] },
    { timestamp: 1640995250, value: 11.1, attributes: [("endpoint", "/api/users")] }
  ]
  
  // 定义聚合函数
  let aggregate = fn(points: Array[DataPoint], aggregation_type: AggregationType, 
                     window: Option[TimeWindow]) {
    let filtered_points = match window {
      Some(w) => {
        points.filter(fn(p) { 
          p.timestamp >= w.start_time and p.timestamp <= w.end_time 
        })
      }
      None => points
    }
    
    if filtered_points.length() == 0 {
      0.0
    } else {
      match aggregation_type {
        AggregationType::Sum => {
          filtered_points.reduce(fn(acc, p) { acc + p.value }, 0.0)
        }
        AggregationType::Average => {
          let sum = filtered_points.reduce(fn(acc, p) { acc + p.value }, 0.0)
          sum / filtered_points.length().to_float()
        }
        AggregationType::Min => {
          filtered_points.reduce(fn(acc, p) { if p.value < acc { p.value } else { acc } }, 
                                filtered_points[0].value)
        }
        AggregationType::Max => {
          filtered_points.reduce(fn(acc, p) { if p.value > acc { p.value } else { acc } }, 
                                filtered_points[0].value)
        }
        AggregationType::Histogram => {
          // 简化的直方图实现
          let sum = filtered_points.reduce(fn(acc, p) { acc + p.value }, 0.0)
          let avg = sum / filtered_points.length().to_float()
          avg
        }
        AggregationType::Percentile => {
          // 简化的百分位数实现（返回中位数）
          let sorted = filtered_points.sort(fn(a, b) { a.value <= b.value })
          let mid = sorted.length() / 2
          if sorted.length() % 2 == 0 {
            (sorted[mid - 1].value + sorted[mid].value) / 2.0
          } else {
            sorted[mid].value
          }
        }
      }
    }
  }
  
  // 测试聚合函数
  let sum_result = aggregate(data_points, AggregationType::Sum, None)
  assert_eq(sum_result, 67.6)
  
  let avg_result = aggregate(data_points, AggregationType::Average, None)
  assert_eq(avg_result, 11.266666666666666)
  
  let min_result = aggregate(data_points, AggregationType::Min, None)
  assert_eq(min_result, 8.7)
  
  let max_result = aggregate(data_points, AggregationType::Max, None)
  assert_eq(max_result, 15.2)
  
  // 测试时间窗口聚合
  let time_window = {
    start_time: 1640995200,
    end_time: 1640995230,
    duration: 30
  }
  
  let window_sum = aggregate(data_points, AggregationType::Sum, Some(time_window))
  assert_eq(window_sum, 46.7)
  
  // 测试属性过滤聚合
  let filter_by_attribute = fn(points: Array[DataPoint], key: String, value: String) {
    points.filter(fn(p) { 
      p.attributes.any(fn(attr) { attr.0 == key and attr.1 == value }) 
    })
  }
  
  let users_endpoint_data = filter_by_attribute(data_points, "endpoint", "/api/users")
  let users_avg = aggregate(users_endpoint_data, AggregationType::Average, None)
  assert_eq(users_avg, 10.65)
  
  let orders_endpoint_data = filter_by_attribute(data_points, "endpoint", "/api/orders")
  let orders_avg = aggregate(orders_endpoint_data, AggregationType::Average, None)
  assert_eq(orders_avg, 12.5)
}

// 测试3: 异常检测与告警
test "异常检测与告警测试" {
  // 定义异常类型
  enum AnomalyType {
    StatisticalOutlier
    SuddenSpike
    SuddenDrop
    PatternDeviation
    ThresholdBreach
  }
  
  // 定义告警级别
  enum AlertLevel {
    Info
    Warning
    Critical
  }
  
  // 定义异常检测结果
  type AnomalyDetectionResult = {
    anomaly_type: AnomalyType,
    alert_level: AlertLevel,
    confidence: Float,
    message: String,
    detected_at: Int
  }
  
  // 定义时间序列数据点
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float
  }
  
  // 创建测试时间序列数据
  let time_series_data = [
    { timestamp: 1640995200, value: 10.0 },
    { timestamp: 1640995260, value: 10.5 },
    { timestamp: 1640995320, value: 9.8 },
    { timestamp: 1640995380, value: 11.2 },
    { timestamp: 1640995440, value: 10.3 },
    { timestamp: 1640995500, value: 10.7 },
    { timestamp: 1640995560, value: 50.0 },  // 异常峰值
    { timestamp: 1640995620, value: 10.2 },
    { timestamp: 1640995680, value: 9.9 },
    { timestamp: 1640995740, value: 10.4 }
  ]
  
  // 定义异常检测函数
  let detect_anomalies = fn(data: Array[TimeSeriesPoint], threshold_multiplier: Float) {
    let mut results = []
    
    if data.length() < 3 {
      results
    } else {
      // 计算均值和标准差
      let sum = data.reduce(fn(acc, p) { acc + p.value }, 0.0)
      let mean = sum / data.length().to_float()
      
      let variance = data.reduce(fn(acc, p) { 
        let diff = p.value - mean
        acc + diff * diff 
      }, 0.0) / data.length().to_float()
      
      let std_dev = variance.sqrt()
      let threshold = mean + (std_dev * threshold_multiplier)
      
      // 检测异常点
      for point in data {
        if point.value > threshold {
          let confidence = (point.value - threshold) / threshold
          let alert_level = if confidence > 1.0 { 
            AlertLevel::Critical 
          } else if confidence > 0.5 { 
            AlertLevel::Warning 
          } else { 
            AlertLevel::Info 
          }
          
          let result = {
            anomaly_type: AnomalyType::SuddenSpike,
            alert_level,
            confidence,
            message: "检测到异常峰值: " + point.value.to_string() + 
                    " (阈值: " + threshold.to_string() + ")",
            detected_at: point.timestamp
          }
          
          results = results.push(result)
        }
      }
      
      results
    }
  }
  
  // 检测异常
  let anomalies = detect_anomalies(time_series_data, 2.0)
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 1)
  assert_eq(anomalies[0].anomaly_type, AnomalyType::SuddenSpike)
  assert_eq(anomalies[0].alert_level, AlertLevel::Critical)
  assert_eq(anomalies[0].detected_at, 1640995560)
  assert_true(anomalies[0].message.contains("50"))
  
  // 测试模式偏差检测
  let detect_pattern_deviation = fn(data: Array[TimeSeriesPoint], expected_pattern: Array[Float]) {
    if data.length() != expected_pattern.length() {
      None
    } else {
      let deviations = []
      for i in 0..data.length() {
        let expected = expected_pattern[i]
        let actual = data[i].value
        let deviation = (actual - expected).abs() / expected
        deviations = deviations.push(deviation)
      }
      
      let avg_deviation = deviations.reduce(fn(acc, d) { acc + d }, 0.0) / deviations.length().to_float()
      
      if avg_deviation > 0.5 {
        Some({
          anomaly_type: AnomalyType::PatternDeviation,
          alert_level: if avg_deviation > 1.0 { AlertLevel::Critical } else { AlertLevel::Warning },
          confidence: avg_deviation,
          message: "模式偏差: 平均偏差 " + avg_deviation.to_string(),
          detected_at: data[data.length() - 1].timestamp
        })
      } else {
        None
      }
    }
  }
  
  // 测试模式偏差
  let expected_pattern = [10.0, 10.5, 9.8, 11.2, 10.3]
  let actual_pattern = [
    { timestamp: 1640995200, value: 10.0 },
    { timestamp: 1640995260, value: 10.5 },
    { timestamp: 1640995320, value: 9.8 },
    { timestamp: 1640995380, value: 11.2 },
    { timestamp: 1640995440, value: 20.0 }  // 偏差较大
  ]
  
  let pattern_result = detect_pattern_deviation(actual_pattern, expected_pattern)
  
  match pattern_result {
    Some(result) => {
      assert_eq(result.anomaly_type, AnomalyType::PatternDeviation)
      assert_eq(result.alert_level, AlertLevel::Warning)
      assert_true(result.message.contains("模式偏差"))
    }
    None => assert_true(false)
  }
}

// 测试4: 遥测数据压缩传输
test "遥测数据压缩传输测试" {
  // 定义遥测数据记录
  type TelemetryRecord = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)]
  }
  
  // 创建测试遥测记录
  let records = [
    {
      trace_id: "trace-001",
      span_id: "span-001",
      parent_span_id: None,
      operation_name: "GET /api/users",
      start_time: 1640995200000,
      end_time: 1640995200250,
      status: "ok",
      attributes: [
        ("http.method", "GET"),
        ("http.status_code", "200"),
        ("user.id", "user-123")
      ]
    },
    {
      trace_id: "trace-001",
      span_id: "span-002",
      parent_span_id: Some("span-001"),
      operation_name: "database.query",
      start_time: 1640995200050,
      end_time: 1640995200200,
      status: "ok",
      attributes: [
        ("db.statement", "SELECT * FROM users"),
        ("db.type", "postgresql"),
        ("db.rows", "10")
      ]
    },
    {
      trace_id: "trace-002",
      span_id: "span-003",
      parent_span_id: None,
      operation_name: "POST /api/orders",
      start_time: 1640995300000,
      end_time: 1640995300500,
      status: "error",
      attributes: [
        ("http.method", "POST"),
        ("http.status_code", "500"),
        ("error.type", "ValidationError")
      ]
    }
  ]
  
  // 定义序列化函数
  let serialize_record = fn(record: TelemetryRecord) {
    let parent_id = match record.parent_span_id {
      Some(id) => id
      None => ""
    }
    
    let attrs = record.attributes.map(fn(attr) { 
      attr.0 + "=" + attr.1 
    }).join(",")
    
    record.trace_id + "|" + 
    record.span_id + "|" + 
    parent_id + "|" + 
    record.operation_name + "|" + 
    record.start_time.to_string() + "|" + 
    record.end_time.to_string() + "|" + 
    record.status + "|" + 
    attrs
  }
  
  // 定义反序列化函数
  let deserialize_record = fn(serialized: String) {
    let parts = serialized.split("|")
    if parts.length() != 8 {
      None
    } else {
      let trace_id = parts[0]
      let span_id = parts[1]
      let parent_span_id = if parts[2] == "" { None } else { Some(parts[2]) }
      let operation_name = parts[3]
      let start_time = parts[4].to_int()
      let end_time = parts[5].to_int()
      let status = parts[6]
      
      let attrs_str = parts[7]
      let attrs = if attrs_str == "" {
        []
      } else {
        attrs_str.split(",").map(fn(attr_str) {
          let attr_parts = attr_str.split("=")
          if attr_parts.length() == 2 {
            (attr_parts[0], attr_parts[1])
          } else {
            ("", "")
          }
        })
      }
      
      Some({
        trace_id,
        span_id,
        parent_span_id,
        operation_name,
        start_time,
        end_time,
        status,
        attributes: attrs
      })
    }
  }
  
  // 定义压缩函数（简化：重复字符串替换）
  let compress = fn(data: String) {
    // 简单的压缩：替换常见的重复模式
    let compressed = data
      .replace("trace-", "t:")
      .replace("span-", "s:")
      .replace("user-", "u:")
      .replace("database.query", "db.q")
      .replace("postgresql", "pg")
    
    compressed
  }
  
  // 定义解压缩函数
  let decompress = fn(compressed_data: String) {
    let decompressed = compressed_data
      .replace("t:", "trace-")
      .replace("s:", "span-")
      .replace("u:", "user-")
      .replace("db.q", "database.query")
      .replace("pg", "postgresql")
    
    decompressed
  }
  
  // 测试序列化
  let serialized_records = records.map(serialize_record)
  assert_eq(serialized_records.length(), 3)
  assert_true(serialized_records[0].contains("trace-001"))
  assert_true(serialized_records[0].contains("GET /api/users"))
  
  // 测试压缩
  let compressed_records = serialized_records.map(compress)
  assert_eq(compressed_records.length(), 3)
  assert_true(compressed_records[0].contains("t:001"))
  assert_true(compressed_records[0].contains("GET /api/users"))  // 未压缩部分
  
  // 计算压缩率
  let original_size = serialized_records.reduce(fn(acc, s) { acc + s.length() }, 0)
  let compressed_size = compressed_records.reduce(fn(acc, s) { acc + s.length() }, 0)
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  
  assert_true(compression_ratio < 1.0)  // 压缩后应该更小
  
  // 测试解压缩和反序列化
  let decompressed_records = compressed_records.map(decompress)
  let deserialized_records = decompressed_records.map(deserialize_record)
  
  // 验证反序列化结果
  assert_eq(deserialized_records.length(), 3)
  
  match deserialized_records[0] {
    Some(record) => {
      assert_eq(record.trace_id, "trace-001")
      assert_eq(record.span_id, "span-001")
      assert_eq(record.operation_name, "GET /api/users")
      assert_eq(record.status, "ok")
      assert_eq(record.parent_span_id, None)
      assert_true(record.attributes.any(fn(attr) { attr.0 == "http.method" and attr.1 == "GET" }))
    }
    None => assert_true(false)
  }
  
  match deserialized_records[1] {
    Some(record) => {
      assert_eq(record.trace_id, "trace-001")
      assert_eq(record.span_id, "span-002")
      assert_eq(record.operation_name, "database.query")
      assert_eq(record.parent_span_id, Some("span-001"))
      assert_true(record.attributes.any(fn(attr) { attr.0 == "db.type" and attr.1 == "postgresql" }))
    }
    None => assert_true(false)
  }
}

// 测试5: 多维度指标分析
test "多维度指标分析测试" {
  // 定义指标维度
  type MetricDimension = {
    name: String,
    value: String
  }
  
  // 定义多维度指标
  type MultiDimensionalMetric = {
    name: String,
    value: Float,
    dimensions: Array[MetricDimension],
    timestamp: Int
  }
  
  // 创建测试指标数据
  let metrics = [
    {
      name: "http.request.duration",
      value: 0.125,
      dimensions: [
        { name: "service", value: "user-service" },
        { name: "endpoint", value: "/api/users" },
        { name: "method", value: "GET" },
        { name: "status", value: "200" }
      ],
      timestamp: 1640995200
    },
    {
      name: "http.request.duration",
      value: 0.250,
      dimensions: [
        { name: "service", value: "user-service" },
        { name: "endpoint", value: "/api/users" },
        { name: "method", value: "POST" },
        { name: "status", value: "201" }
      ],
      timestamp: 1640995205
    },
    {
      name: "http.request.duration",
      value: 0.350,
      dimensions: [
        { name: "service", value: "order-service" },
        { name: "endpoint", value: "/api/orders" },
        { name: "method", value: "GET" },
        { name: "status", value: "200" }
      ],
      timestamp: 1640995210
    },
    {
      name: "http.request.duration",
      value: 0.075,
      dimensions: [
        { name: "service", value: "user-service" },
        { name: "endpoint", value: "/api/users" },
        { name: "method", value: "GET" },
        { name: "status", value: "200" }
      ],
      timestamp: 1640995215
    },
    {
      name: "http.request.duration",
      value: 0.500,
      dimensions: [
        { name: "service", value: "order-service" },
        { name: "endpoint", value: "/api/orders" },
        { name: "method", value: "POST" },
        { name: "status", value: "500" }
      ],
      timestamp: 1640995220
    }
  ]
  
  // 定义按维度过滤函数
  let filter_by_dimension = fn(metrics: Array[MultiDimensionalMetric], 
                               dimension_name: String, dimension_value: String) {
    metrics.filter(fn(metric) {
      metric.dimensions.any(fn(dim) { 
        dim.name == dimension_name and dim.value == dimension_value 
      })
    })
  }
  
  // 定义按多个维度过滤函数
  let filter_by_dimensions = fn(metrics: Array[MultiDimensionalMetric], 
                                filters: Array[(String, String)]) {
    metrics.filter(fn(metric) {
      filters.all(fn(filter) {
        metric.dimensions.any(fn(dim) { 
          dim.name == filter.0 and dim.value == filter.1 
        })
      })
    })
  }
  
  // 定义按维度分组聚合函数
  let aggregate_by_dimension = fn(metrics: Array[MultiDimensionalMetric], 
                                 dimension_name: String) {
    let mut groups = []
    
    // 获取所有唯一的维度值
    let unique_values = []
    for metric in metrics {
      for dim in metric.dimensions {
        if dim.name == dimension_name and not(unique_values.contains(dim.value)) {
          unique_values = unique_values.push(dim.value)
        }
      }
    }
    
    // 按每个维度值分组并计算聚合值
    for value in unique_values {
      let group_metrics = filter_by_dimension(metrics, dimension_name, value)
      let sum = group_metrics.reduce(fn(acc, m) { acc + m.value }, 0.0)
      let avg = sum / group_metrics.length().to_float()
      let count = group_metrics.length()
      
      let group = {
        dimension_name,
        dimension_value: value,
        count,
        sum,
        avg,
        min: group_metrics.reduce(fn(acc, m) { if m.value < acc { m.value } else { acc } }, 
                                 group_metrics[0].value),
        max: group_metrics.reduce(fn(acc, m) { if m.value > acc { m.value } else { acc } }, 
                                 group_metrics[0].value)
      }
      
      groups = groups.push(group)
    }
    
    groups
  }
  
  // 测试按单个维度过滤
  let user_service_metrics = filter_by_dimension(metrics, "service", "user-service")
  assert_eq(user_service_metrics.length(), 3)
  
  let get_requests = filter_by_dimension(metrics, "method", "GET")
  assert_eq(get_requests.length(), 3)
  
  // 测试按多个维度过滤
  let user_get_requests = filter_by_dimensions(metrics, [
    ("service", "user-service"),
    ("method", "GET"),
    ("status", "200")
  ])
  assert_eq(user_get_requests.length(), 2)
  
  // 测试按服务分组聚合
  let service_aggregations = aggregate_by_dimension(metrics, "service")
  assert_eq(service_aggregations.length(), 2)
  
  let user_service_agg = service_aggregations.find(fn(g) { g.dimension_value == "user-service" })
  match user_service_agg {
    Some(agg) => {
      assert_eq(agg.count, 3)
      assert_eq(agg.sum, 0.125 + 0.250 + 0.075)
      assert_eq(agg.avg, (0.125 + 0.250 + 0.075) / 3.0)
      assert_eq(agg.min, 0.075)
      assert_eq(agg.max, 0.250)
    }
    None => assert_true(false)
  }
  
  let order_service_agg = service_aggregations.find(fn(g) { g.dimension_value == "order-service" })
  match order_service_agg {
    Some(agg) => {
      assert_eq(agg.count, 2)
      assert_eq(agg.sum, 0.350 + 0.500)
      assert_eq(agg.avg, (0.350 + 0.500) / 2.0)
      assert_eq(agg.min, 0.350)
      assert_eq(agg.max, 0.500)
    }
    None => assert_true(false)
  }
  
  // 测试按状态码分组聚合
  let status_aggregations = aggregate_by_dimension(metrics, "status")
  assert_eq(status_aggregations.length(), 3)
  
  let success_status_agg = status_aggregations.find(fn(g) { g.dimension_value == "200" })
  match success_status_agg {
    Some(agg) => {
      assert_eq(agg.count, 3)
      assert_eq(agg.avg, (0.125 + 0.350 + 0.075) / 3.0)
    }
    None => assert_true(false)
  }
  
  let error_status_agg = status_aggregations.find(fn(g) { g.dimension_value == "500" })
  match error_status_agg {
    Some(agg) => {
      assert_eq(agg.count, 1)
      assert_eq(agg.avg, 0.500)
      assert_eq(agg.min, 0.500)
      assert_eq(agg.max, 0.500)
    }
    None => assert_true(false)
  }
}

// 测试6: 智能缓存策略
test "智能缓存策略测试" {
  // 定义缓存项
  type CacheItem = {
    key: String,
    value: String,
    access_count: Int,
    last_access_time: Int,
    creation_time: Int,
    size: Int
  }
  
  // 定义缓存策略
  enum CacheStrategy {
    LRU  // 最近最少使用
    LFU  // 最少使用频率
    TTL  // 生存时间
    SizeBased  // 基于大小
  }
  
  // 定义缓存配置
  type CacheConfig = {
    max_size: Int,
    max_items: Int,
    ttl_seconds: Int,
    strategy: CacheStrategy
  }
  
  // 创建缓存配置
  let config = {
    max_size: 1024,
    max_items: 100,
    ttl_seconds: 3600,
    strategy: CacheStrategy::LRU
  }
  
  // 创建测试缓存项
  let cache_items = [
    {
      key: "trace-001",
      value: "span data for trace 001",
      access_count: 10,
      last_access_time: 1640995200,
      creation_time: 1640995000,
      size: 25
    },
    {
      key: "trace-002",
      value: "span data for trace 002",
      access_count: 5,
      last_access_time: 1640995100,
      creation_time: 1640995050,
      size: 25
    },
    {
      key: "trace-003",
      value: "span data for trace 003",
      access_count: 20,
      last_access_time: 1640995150,
      creation_time: 1640994950,
      size: 25
    },
    {
      key: "trace-004",
      value: "large span data for trace 004 with more content",
      access_count: 2,
      last_access_time: 1640995250,
      creation_time: 1640995100,
      size: 50
    }
  ]
  
  // 定义缓存淘汰策略函数
  let evict_items = fn(items: Array[CacheItem], config: CacheConfig, current_size: Int) {
    if current_size <= config.max_size and items.length() <= config.max_items {
      items
    } else {
      match config.strategy {
        CacheStrategy::LRU => {
          // 按最后访问时间排序，移除最久未访问的
          let sorted = items.sort(fn(a, b) { a.last_access_time <= b.last_access_time })
          let items_to_remove = items.length() - config.max_items + 1
          sorted.drop(items_to_remove)
        }
        CacheStrategy::LFU => {
          // 按访问次数排序，移除访问次数最少的
          let sorted = items.sort(fn(a, b) { a.access_count <= b.access_count })
          let items_to_remove = items.length() - config.max_items + 1
          sorted.drop(items_to_remove)
        }
        CacheStrategy::TTL => {
          // 移除过期的项
          let current_time = 1640995300
          items.filter(fn(item) { 
            current_time - item.creation_time <= config.ttl_seconds 
          })
        }
        CacheStrategy::SizeBased => {
          // 按大小排序，移除最大的项
          let sorted = items.sort(fn(a, b) { a.size >= b.size })
          let mut remaining = sorted
          let mut total_size = remaining.reduce(fn(acc, item) { acc + item.size }, 0)
          
          while total_size > config.max_size and remaining.length() > 0 {
            total_size = total_size - remaining[0].size
            remaining = remaining.drop(1)
          }
          
          remaining
        }
      }
    }
  }
  
  // 测试LRU策略
  let lru_config = { config | strategy: CacheStrategy::LRU }
  let lru_result = evict_items(cache_items, lru_config, 125)
  assert_eq(lru_result.length(), 3)  // 应该移除一个项
  assert_eq(lru_result[0].key, "trace-003")  // trace-002应该被移除（最久未访问）
  
  // 测试LFU策略
  let lfu_config = { config | strategy: CacheStrategy::LFU }
  let lfu_result = evict_items(cache_items, lfu_config, 125)
  assert_eq(lfu_result.length(), 3)  // 应该移除一个项
  assert_eq(lfu_result[0].key, "trace-003")  // trace-004应该被移除（访问次数最少）
  
  // 测试TTL策略
  let ttl_config = { config | ttl_seconds: 200 }  // 设置较短的TTL
  let ttl_result = evict_items(cache_items, ttl_config, 125)
  // 当前时间是1640995300，TTL是200秒，所以创建时间早于1640995100的项应该被移除
  assert_eq(ttl_result.length(), 2)  // trace-001和trace-003应该被移除
  
  // 测试SizeBased策略
  let size_config = { config | max_size: 75 }  // 设置较小的最大大小
  let size_result = evict_items(cache_items, size_config, 125)
  assert_eq(size_result.length(), 2)  // 应该移除一些项直到总大小不超过75
  // trace-004（大小50）应该被移除，因为它最大
  
  // 定义缓存访问函数
  let access_cache = fn(items: Array[CacheItem], key: String, current_time: Int) {
    let mut found = None
    let mut updated_items = []
    
    for item in items {
      if item.key == key {
        found = Some(item.value)
        updated_items = updated_items.push({
          item | 
          access_count: item.access_count + 1,
          last_access_time: current_time
        })
      } else {
        updated_items = updated_items.push(item)
      }
    }
    
    (found, updated_items)
  }
  
  // 测试缓存访问
  let (found_value, updated_cache) = access_cache(cache_items, "trace-002", 1640995300)
  assert_eq(found_value, Some("span data for trace 002"))
  
  // 验证访问计数和最后访问时间已更新
  let updated_item = updated_cache.find(fn(item) { item.key == "trace-002" })
  match updated_item {
    Some(item) => {
      assert_eq(item.access_count, 6)  // 从5增加到6
      assert_eq(item.last_access_time, 1640995300)  // 更新为当前时间
    }
    None => assert_true(false)
  }
}

// 测试7: 动态配置热更新
test "动态配置热更新测试" {
  // 定义配置值类型
  enum ConfigValue {
    StringValue(String)
    IntValue(Int)
    FloatValue(Float)
    BoolValue(Bool)
    ArrayStringValue(Array[String])
  }
  
  // 定义配置变更监听器
  type ConfigChangeListener = {
    key: String,
    old_value: Option[ConfigValue],
    new_value: ConfigValue,
    timestamp: Int
  }
  
  // 定义配置管理器
  type ConfigManager = {
    configs: Array[(String, ConfigValue)],
    listeners: Array[ConfigChangeListener],
    last_updated: Int
  }
  
  // 创建初始配置管理器
  let initial_configs = [
    ("telemetry.sampling_rate", ConfigValue::FloatValue(0.1)),
    ("telemetry.batch_size", ConfigValue::IntValue(512)),
    ("telemetry.export_timeout", ConfigValue::IntValue(30000)),
    ("telemetry.enabled_endpoints", ConfigValue::ArrayStringValue(["/api/users", "/api/orders"])),
    ("telemetry.debug_mode", ConfigValue::BoolValue(false))
  ]
  
  let initial_manager = {
    configs: initial_configs,
    listeners: [],
    last_updated: 1640995200
  }
  
  // 定义配置获取函数
  let get_config = fn(manager: ConfigManager, key: String) {
    manager.configs.find(fn(config) { config.0 == key })
  }
  
  // 定义配置更新函数
  let update_config = fn(manager: ConfigManager, key: String, new_value: ConfigValue, current_time: Int) {
    let old_config = get_config(manager, key)
    let old_value = match old_config {
      Some(config) => Some(config.1)
      None => None
    }
    
    // 创建变更监听器
    let listener = {
      key,
      old_value,
      new_value,
      timestamp: current_time
    }
    
    // 更新配置
    let mut updated_configs = []
    let mut found = false
    
    for config in manager.configs {
      if config.0 == key {
        updated_configs = updated_configs.push((key, new_value))
        found = true
      } else {
        updated_configs = updated_configs.push(config)
      }
    }
    
    if not(found) {
      updated_configs = updated_configs.push((key, new_value))
    }
    
    // 添加监听器
    let updated_listeners = manager.listeners.push(listener)
    
    {
      configs: updated_configs,
      listeners: updated_listeners,
      last_updated: current_time
    }
  }
  
  // 定义批量配置更新函数
  let batch_update_configs = fn(manager: ConfigManager, updates: Array[(String, ConfigValue)], current_time: Int) {
    let mut updated_manager = manager
    
    for update in updates {
      updated_manager = update_config(updated_manager, update.0, update.1, current_time)
    }
    
    updated_manager
  }
  
  // 测试配置获取
  let sampling_rate_config = get_config(initial_manager, "telemetry.sampling_rate")
  match sampling_rate_config {
    Some(config) => {
      match config.1 {
        ConfigValue::FloatValue(rate) => assert_eq(rate, 0.1)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let debug_mode_config = get_config(initial_manager, "telemetry.debug_mode")
  match debug_mode_config {
    Some(config) => {
      match config.1 {
        ConfigValue::BoolValue(debug) => assert_eq(debug, false)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试配置更新
  let updated_manager = update_config(initial_manager, "telemetry.sampling_rate", 
                                     ConfigValue::FloatValue(0.5), 1640995300)
  
  // 验证配置已更新
  let updated_sampling_rate = get_config(updated_manager, "telemetry.sampling_rate")
  match updated_sampling_rate {
    Some(config) => {
      match config.1 {
        ConfigValue::FloatValue(rate) => assert_eq(rate, 0.5)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证监听器已添加
  assert_eq(updated_manager.listeners.length(), 1)
  assert_eq(updated_manager.listeners[0].key, "telemetry.sampling_rate")
  assert_eq(updated_manager.last_updated, 1640995300)
  
  // 验证监听器记录的变更
  match updated_manager.listeners[0].old_value {
    Some(ConfigValue::FloatValue(old_rate)) => assert_eq(old_rate, 0.1)
    _ => assert_true(false)
  }
  
  match updated_manager.listeners[0].new_value {
    ConfigValue::FloatValue(new_rate) => assert_eq(new_rate, 0.5)
    _ => assert_true(false)
  }
  
  // 测试批量更新
  let batch_updates = [
    ("telemetry.batch_size", ConfigValue::IntValue(1024)),
    ("telemetry.export_timeout", ConfigValue::IntValue(60000)),
    ("telemetry.debug_mode", ConfigValue::BoolValue(true))
  ]
  
  let batch_updated_manager = batch_update_configs(updated_manager, batch_updates, 1640995400)
  
  // 验证批量更新结果
  let updated_batch_size = get_config(batch_updated_manager, "telemetry.batch_size")
  match updated_batch_size {
    Some(config) => {
      match config.1 {
        ConfigValue::IntValue(size) => assert_eq(size, 1024)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let updated_debug_mode = get_config(batch_updated_manager, "telemetry.debug_mode")
  match updated_debug_mode {
    Some(config) => {
      match config.1 {
        ConfigValue::BoolValue(debug) => assert_eq(debug, true)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证监听器数量
  assert_eq(batch_updated_manager.listeners.length(), 4)  // 之前1个 + 批量更新3个
  assert_eq(batch_updated_manager.last_updated, 1640995400)
  
  // 测试配置变更历史
  let get_config_history = fn(manager: ConfigManager, key: String) {
    manager.listeners.filter(fn(listener) { listener.key == key })
  }
  
  let sampling_rate_history = get_config_history(batch_updated_manager, "telemetry.sampling_rate")
  assert_eq(sampling_rate_history.length(), 1)
  
  let debug_mode_history = get_config_history(batch_updated_manager, "telemetry.debug_mode")
  assert_eq(debug_mode_history.length(), 1)
  
  match debug_mode_history[0].old_value {
    Some(ConfigValue::BoolValue(old_debug)) => assert_eq(old_debug, false)
    _ => assert_true(false)
  }
  
  match debug_mode_history[0].new_value {
    ConfigValue::BoolValue(new_debug) => assert_eq(new_debug, true)
    _ => assert_true(false)
  }
}

// 测试8: 跨服务上下文传播
test "跨服务上下文传播测试" {
  // 定义上下文键
  type ContextKey = String
  
  // 定义上下文值
  enum ContextValue {
    StringValue(String)
    IntValue(Int)
    FloatValue(Float)
    BoolValue(Bool)
    BytesValue(Array[Byte])
  }
  
  // 定义传播上下文
  type PropagationContext = {
    trace_id: String,
    span_id: String,
    baggage: Array[(ContextKey, ContextValue)],
    sampling_decision: Bool
  }
  
  // 定义传播载体
  type Carrier = {
    headers: Array[(String, String)],
    metadata: Array[(String, String)]
  }
  
  // 定义传播器
  type Propagator = {
    inject: fn(context: PropagationContext, carrier: Carrier) -> Carrier
    extract: fn(carrier: Carrier) -> Option[PropagationContext]
  }
  
  // 创建W3C Trace Context传播器
  let w3c_trace_propagator = {
    inject: fn(context: PropagationContext, carrier: Carrier) {
      let traceparent = "00-" + context.trace_id + "-" + context.span_id + 
                       (if context.sampling_decision { "01" } else { "00" })
      
      let updated_headers = carrier.headers.push(("traceparent", traceparent))
      
      // 注入baggage
      let mut updated_headers_with_baggage = updated_headers
      for (key, value) in context.baggage {
        let value_str = match value {
          ContextValue::StringValue(s) => s
          ContextValue::IntValue(i) => i.to_string()
          ContextValue::FloatValue(f) => f.to_string()
          ContextValue::BoolValue(b) => if b { "true" } else { "false" }
          ContextValue::BytesValue(bytes) => "base64:" + bytes.to_string()  // 简化
        }
        updated_headers_with_baggage = updated_headers_with_baggage.push(("baggage", key + "=" + value_str))
      }
      
      {
        headers: updated_headers_with_baggage,
        metadata: carrier.metadata
      }
    },
    
    extract: fn(carrier: Carrier) {
      // 提取traceparent
      let traceparent_header = carrier.headers.find(fn(header) { header.0 == "traceparent" })
      
      match traceparent_header {
        Some(header) => {
          let parts = header.1.split("-")
          if parts.length() >= 4 {
            let trace_id = parts[1]
            let span_id = parts[2]
            let flags = parts[3]
            let sampling_decision = flags.length() >= 2 and flags[1] == '1'
            
            // 提取baggage
            let baggage_headers = carrier.headers.filter(fn(header) { header.0 == "baggage" })
            let mut baggage = []
            
            for baggage_header in baggage_headers {
              let parts = baggage_header.1.split("=")
              if parts.length() >= 2 {
                let key = parts[0]
                let value_str = parts[1]
                
                // 简化：假设所有值都是字符串
                let value = ContextValue::StringValue(value_str)
                baggage = baggage.push((key, value))
              }
            }
            
            Some({
              trace_id,
              span_id,
              baggage,
              sampling_decision
            })
          } else {
            None
          }
        }
        None => None
      }
    }
  }
  
  // 创建测试上下文
  let test_context = {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b9c7c989f97918e1",
    baggage: [
      ("user.id", ContextValue::StringValue("user-123")),
      ("request.id", ContextValue::StringValue("req-456")),
      ("service.version", ContextValue::StringValue("1.2.3"))
    ],
    sampling_decision: true
  }
  
  // 创建测试载体
  let test_carrier = {
    headers: [
      ("host", "api.example.com"),
      ("user-agent", "Mozilla/5.0")
    ],
    metadata: [
      ("source.service", "user-service"),
      ("target.service", "order-service")
    ]
  }
  
  // 测试上下文注入
  let injected_carrier = w3c_trace_propagator.inject(test_context, test_carrier)
  
  // 验证注入结果
  assert_eq(injected_carrier.headers.length(), 6)  // 原来2个 + traceparent + 3个baggage
  
  let traceparent_header = injected_carrier.headers.find(fn(header) { header.0 == "traceparent" })
  match traceparent_header {
    Some(header) => {
      assert_true(header.1.contains("0af7651916cd43dd8448eb211c80319c"))
      assert_true(header.1.contains("b9c7c989f97918e1"))
      assert_true(header.1.ends_with("01"))
    }
    None => assert_true(false)
  }
  
  let baggage_headers = injected_carrier.headers.filter(fn(header) { header.0 == "baggage" })
  assert_eq(baggage_headers.length(), 3)
  
  // 测试上下文提取
  let extracted_context = w3c_trace_propagator.extract(injected_carrier)
  
  match extracted_context {
    Some(context) => {
      assert_eq(context.trace_id, "0af7651916cd43dd8448eb211c80319c")
      assert_eq(context.span_id, "b9c7c989f97918e1")
      assert_eq(context.sampling_decision, true)
      assert_eq(context.baggage.length(), 3)
      
      // 验证baggage项
      let user_id_baggage = context.baggage.find(fn(item) { item.0 == "user.id" })
      match user_id_baggage {
        Some(baggage) => {
          match baggage.1 {
            ContextValue::StringValue(user_id) => assert_eq(user_id, "user-123")
            _ => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试复合传播器
  let composite_propagator = {
    inject: fn(context: PropagationContext, carrier: Carrier) {
      // 使用W3C Trace Context传播器
      w3c_trace_propagator.inject(context, carrier)
    },
    
    extract: fn(carrier: Carrier) {
      // 使用W3C Trace Context传播器
      w3c_trace_propagator.extract(carrier)
    }
  }
  
  // 测试跨服务传播
  let service_a_carrier = {
    headers: [],
    metadata: []
  }
  
  // 服务A注入上下文
  let carrier_from_a = composite_propagator.inject(test_context, service_a_carrier)
  
  // 模拟HTTP调用：将载体传递给服务B
  let http_request = {
    method: "GET",
    url: "https://order-service.example.com/api/orders",
    headers: carrier_from_a.headers
  }
  
  // 服务B提取上下文
  let service_b_carrier = {
    headers: http_request.headers,
    metadata: []
  }
  
  let context_at_b = composite_propagator.extract(service_b_carrier)
  
  match context_at_b {
    Some(context) => {
      assert_eq(context.trace_id, test_context.trace_id)
      assert_eq(context.span_id, test_context.span_id)
      assert_eq(context.sampling_decision, test_context.sampling_decision)
      
      // 服务B可以访问baggage中的信息
      let user_id_baggage = context.baggage.find(fn(item) { item.0 == "user.id" })
      match user_id_baggage {
        Some(baggage) => {
          match baggage.1 {
            ContextValue::StringValue(user_id) => assert_eq(user_id, "user-123")
            _ => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试上下文修改和重新传播
  match context_at_b {
    Some(context) => {
      // 服务B添加自己的baggage项
      let updated_baggage = context.baggage.push(("service.b.operation", ContextValue::StringValue("get_orders")))
      
      let updated_context = {
        trace_id: context.trace_id,
        span_id: "new-span-id-at-service-b",  // 创建新的span ID
        baggage: updated_baggage,
        sampling_decision: context.sampling_decision
      }
      
      // 服务C提取更新后的上下文
      let carrier_from_b = composite_propagator.inject(updated_context, service_b_carrier)
      let context_at_c = composite_propagator.extract(carrier_from_b)
      
      match context_at_c {
        Some(context_c) => {
          assert_eq(context_c.trace_id, test_context.trace_id)  // trace ID保持不变
          assert_eq(context_c.span_id, "new-span-id-at-service-b")  // span ID已更新
          assert_eq(context_c.baggage.length(), 4)  // 原来的3个 + 新增的1个
          
          let operation_baggage = context_c.baggage.find(fn(item) { item.0 == "service.b.operation" })
          match operation_baggage {
            Some(baggage) => {
              match baggage.1 {
                ContextValue::StringValue(operation) => assert_eq(operation, "get_orders")
                _ => assert_true(false)
              }
            }
            None => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}