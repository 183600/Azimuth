// Azimuth Metric Aggregation Test Suite
// 指标聚合测试套件 - 测试遥测指标的聚合、统计和分析功能

// Test 1: Counter指标聚合
test "counter metric aggregation operations" {
  // 创建MeterProvider和Meter
  let provider = @azimuth.MeterProvider::default()
  let meter = @azimuth.MeterProvider::get_meter(provider, "aggregation-test-meter")
  
  // 创建Counter指标
  let request_counter = @azimuth.Meter::create_counter(
    meter,
    "http.requests.total",
    Some("Total number of HTTP requests"),
    Some("requests")
  )
  
  // 创建不同属性的Counter记录
  let api_attributes = @azimuth.Attributes::new()
  @azimuth.Attributes::set(api_attributes, "http.method", @azimuth.StringValue("GET"))
  @azimuth.Attributes::set(api_attributes, "http.status_code", @azimuth.IntValue(200))
  @azimuth.Attributes::set(api_attributes, "service.name", @azimuth.StringValue("api-service"))
  
  let error_attributes = @azimuth.Attributes::new()
  @azimuth.Attributes::set(error_attributes, "http.method", @azimuth.StringValue("POST"))
  @azimuth.Attributes::set(error_attributes, "http.status_code", @azimuth.IntValue(500))
  @azimuth.Attributes::set(error_attributes, "service.name", @azimuth.StringValue("api-service"))
  
  // 记录Counter值
  for i = 0; i < 100; i = i + 1 {
    @azimuth.Counter::add(request_counter, 1.0, Some(api_attributes))
  }
  
  for i = 0; i < 25; i = i + 1 {
    @azimuth.Counter::add(request_counter, 1.0, Some(error_attributes))
  }
  
  // 创建指标聚合器
  let aggregator = @azimuth.MetricAggregator::new(
    @azimuth.AggregationConfig {
      aggregation_type: @azimuth.Sum,
      time_window_ms: 60000,  // 1分钟窗口
      group_by_attributes: ["http.method", "http.status_code"]
    }
  )
  
  // 聚合Counter指标
  let aggregation_result = @azimuth.MetricAggregator::aggregate_counter(aggregator, request_counter)
  
  // 验证聚合结果
  assert_eq(aggregation_result.data_points.length(), 2)
  
  // 验证GET 200请求的聚合
  let get_200_point = aggregation_result.data_points.find(fn(point) {
    match @azimuth.Attributes::get(point.attributes, "http.method") {
      Some(@azimuth.StringValue(method)) => method == "GET"
      _ => false
    } and match @azimuth.Attributes::get(point.attributes, "http.status_code") {
      Some(@azimuth.IntValue(status)) => status == 200
      _ => false
    }
  })
  
  match get_200_point {
    Some(point) => assert_eq(point.value, 100.0)
    None => assert_true(false)
  }
  
  // 验证POST 500请求的聚合
  let post_500_point = aggregation_result.data_points.find(fn(point) {
    match @azimuth.Attributes::get(point.attributes, "http.method") {
      Some(@azimuth.StringValue(method)) => method == "POST"
      _ => false
    } and match @azimuth.Attributes::get(point.attributes, "http.status_code") {
      Some(@azimuth.IntValue(status)) => status == 500
      _ => false
    }
  })
  
  match post_500_point {
    Some(point) => assert_eq(point.value, 25.0)
    None => assert_true(false)
  }
  
  // 验证总和
  let total_sum = aggregation_result.data_points.reduce(fn(acc, point) { acc + point.value }, 0.0)
  assert_eq(total_sum, 125.0)
}

// Test 2: Histogram指标聚合
test "histogram metric aggregation operations" {
  // 创建MeterProvider和Meter
  let provider = @azimuth.MeterProvider::default()
  let meter = @azimuth.MeterProvider::get_meter(provider, "histogram-test-meter")
  
  // 创建Histogram指标
  let response_time_histogram = @azimuth.Meter::create_histogram(
    meter,
    "http.response.time",
    Some("HTTP response time distribution"),
    Some("ms")
  )
  
  // 创建响应时间属性
  let fast_response_attrs = @azimuth.Attributes::new()
  @azimuth.Attributes::set(fast_response_attrs, "endpoint", @azimuth.StringValue("/api/users"))
  @azimuth.Attributes::set(fast_response_attrs, "service.name", @azimuth.StringValue("user-service"))
  
  let slow_response_attrs = @azimuth.Attributes::new()
  @azimuth.Attributes::set(slow_response_attrs, "endpoint", @azimuth.StringValue("/api/reports"))
  @azimuth.Attributes::set(slow_response_attrs, "service.name", @azimuth.StringValue("report-service"))
  
  // 记录Histogram值 - 快速响应
  let fast_response_times = [10, 15, 20, 25, 30, 35, 40, 45, 50]
  for time in fast_response_times {
    @azimuth.Histogram::record(response_time_histogram, time.to_double(), Some(fast_response_attrs))
  }
  
  // 记录Histogram值 - 慢速响应
  let slow_response_times = [100, 150, 200, 250, 300, 350, 400, 450, 500]
  for time in slow_response_times {
    @azimuth.Histogram::record(response_time_histogram, time.to_double(), Some(slow_response_attrs))
  }
  
  // 创建Histogram聚合器
  let histogram_aggregator = @azimuth.MetricAggregator::new(
    @azimuth.AggregationConfig {
      aggregation_type: @azimuth.Histogram,
      time_window_ms: 60000,
      group_by_attributes: ["endpoint"],
      bucket_boundaries: [0, 50, 100, 200, 300, 400, 500, 1000]
    }
  )
  
  // 聚合Histogram指标
  let histogram_result = @azimuth.MetricAggregator::aggregate_histogram(histogram_aggregator, response_time_histogram)
  
  // 验证聚合结果
  assert_eq(histogram_result.data_points.length(), 2)
  
  // 验证快速响应的聚合结果
  let fast_endpoint_point = histogram_result.data_points.find(fn(point) {
    match @azimuth.Attributes::get(point.attributes, "endpoint") {
      Some(@azimuth.StringValue(endpoint)) => endpoint == "/api/users"
      _ => false
    }
  })
  
  match fast_endpoint_point {
    Some(point) => {
      // 验证桶计数
      assert_eq(point.bucket_counts.length(), 8)
      assert_eq(point.bucket_counts[0], 0)  // 0-50ms桶应该包含一些值
      assert_eq(point.bucket_counts[1], 9)  // 0-50ms桶应该包含所有9个值
      assert_eq(point.bucket_counts[2], 9)  // 50-100ms桶应该包含所有9个值
      assert_eq(point.sum, 270.0)  // 所有快速响应时间的总和
      assert_eq(point.count, 9)    // 总计数
    }
    None => assert_true(false)
  }
  
  // 验证慢速响应的聚合结果
  let slow_endpoint_point = histogram_result.data_points.find(fn(point) {
    match @azimuth.Attributes::get(point.attributes, "endpoint") {
      Some(@azimuth.StringValue(endpoint)) => endpoint == "/api/reports"
      _ => false
    }
  })
  
  match slow_endpoint_point {
    Some(point) => {
      // 验证桶计数
      assert_eq(point.bucket_counts.length(), 8)
      assert_eq(point.bucket_counts[0], 0)  // 0-50ms桶
      assert_eq(point.bucket_counts[1], 0)  // 50-100ms桶
      assert_eq(point.bucket_counts[2], 2)  // 100-200ms桶应该包含100和150
      assert_eq(point.bucket_counts[3], 2)  // 200-300ms桶应该包含200和250
      assert_eq(point.bucket_counts[4], 2)  // 300-400ms桶应该包含300和350
      assert_eq(point.bucket_counts[5], 2)  // 400-500ms桶应该包含400和450
      assert_eq(point.bucket_counts[6], 1)  // 500-1000ms桶应该包含500
      assert_eq(point.sum, 2700.0)  // 所有慢速响应时间的总和
      assert_eq(point.count, 9)     // 总计数
    }
    None => assert_true(false)
  }
}

// Test 3: Gauge指标聚合
test "gauge metric aggregation operations" {
  // 创建MeterProvider和Meter
  let provider = @azimuth.MeterProvider::default()
  let meter = @azimuth.MeterProvider::get_meter(provider, "gauge-test-meter")
  
  // 创建Gauge指标
  let memory_usage_gauge = @azimuth.Meter::create_gauge(
    meter,
    "process.memory.usage",
    Some("Process memory usage"),
    Some("bytes")
  )
  
  // 创建不同服务的内存使用属性
  let api_service_attrs = @azimuth.Attributes::new()
  @azimuth.Attributes::set(api_service_attrs, "service.name", @azimuth.StringValue("api-service"))
  @azimuth.Attributes::set(api_service_attrs, "instance.id", @azimuth.StringValue("api-001"))
  
  let db_service_attrs = @azimuth.Attributes::new()
  @azimuth.Attributes::set(db_service_attrs, "service.name", @azimuth.StringValue("db-service"))
  @azimuth.Attributes::set(db_service_attrs, "instance.id", @azimuth.StringValue("db-001"))
  
  // 模拟内存使用变化
  let api_memory_values = [1048576, 2097152, 1572864, 2621440, 1310720]  // 1MB, 2MB, 1.5MB, 2.5MB, 1.25MB
  for value in api_memory_values {
    @azimuth.Gauge::record(memory_usage_gauge, value.to_double(), Some(api_service_attrs))
  }
  
  let db_memory_values = [5242880, 6291456, 7340032, 8388608, 9437184]  // 5MB, 6MB, 7MB, 8MB, 9MB
  for value in db_memory_values {
    @azimuth.Gauge::record(memory_usage_gauge, value.to_double(), Some(db_service_attrs))
  }
  
  // 创建Gauge聚合器
  let gauge_aggregator = @azimuth.MetricAggregator::new(
    @azimuth.AggregationConfig {
      aggregation_type: @azimuth.LastValue,
      time_window_ms: 60000,
      group_by_attributes: ["service.name"]
    }
  )
  
  // 聚合Gauge指标
  let gauge_result = @azimuth.MetricAggregator::aggregate_gauge(gauge_aggregator, memory_usage_gauge)
  
  // 验证聚合结果
  assert_eq(gauge_result.data_points.length(), 2)
  
  // 验证API服务的最新内存使用
  let api_service_point = gauge_result.data_points.find(fn(point) {
    match @azimuth.Attributes::get(point.attributes, "service.name") {
      Some(@azimuth.StringValue(service)) => service == "api-service"
      _ => false
    }
  })
  
  match api_service_point {
    Some(point) => assert_eq(point.value, 1310720.0)  // 最后一个值：1.25MB
    None => assert_true(false)
  }
  
  // 验证DB服务的最新内存使用
  let db_service_point = gauge_result.data_points.find(fn(point) {
    match @azimuth.Attributes::get(point.attributes, "service.name") {
      Some(@azimuth.StringValue(service)) => service == "db-service"
      _ => false
    }
  })
  
  match db_service_point {
    Some(point) => assert_eq(point.value, 9437184.0)  // 最后一个值：9MB
    None => assert_true(false)
  }
}

// Test 4: 多指标时间序列聚合
test "multi-metric time series aggregation" {
  // 创建时间序列数据点
  let timestamp_base = 1640995200000  // 基准时间戳
  
  let cpu_metrics = [
    (timestamp_base, 25.5),
    (timestamp_base + 60000, 30.2),
    (timestamp_base + 120000, 28.7),
    (timestamp_base + 180000, 35.1),
    (timestamp_base + 240000, 32.8)
  ]
  
  let memory_metrics = [
    (timestamp_base, 4096.0),
    (timestamp_base + 60000, 4352.0),
    (timestamp_base + 120000, 4608.0),
    (timestamp_base + 180000, 4864.0),
    (timestamp_base + 240000, 5120.0)
  ]
  
  let request_rate_metrics = [
    (timestamp_base, 125.0),
    (timestamp_base + 60000, 150.0),
    (timestamp_base + 120000, 175.0),
    (timestamp_base + 180000, 200.0),
    (timestamp_base + 240000, 180.0)
  ]
  
  // 创建多指标聚合器
  let multi_metric_aggregator = @azimuth.MultiMetricAggregator::new(
    @azimuth.MultiMetricConfig {
      metrics: ["cpu.usage", "memory.usage", "request.rate"],
      aggregation_functions: ["avg", "min", "max", "trend"],
      time_window_ms: 300000,  // 5分钟窗口
      downsample_interval_ms: 60000  // 1分钟下采样
    }
  )
  
  // 添加时间序列数据
  for (timestamp, value) in cpu_metrics {
    let data_point = @azimuth.TimeSeriesDataPoint {
      timestamp,
      metric_name: "cpu.usage",
      value,
      attributes: [
        ("service.name", @azimuth.StringValue("api-service")),
        ("instance.id", @azimuth.StringValue("instance-001"))
      ]
    }
    @azimuth.MultiMetricAggregator::add_data_point(multi_metric_aggregator, data_point)
  }
  
  for (timestamp, value) in memory_metrics {
    let data_point = @azimuth.TimeSeriesDataPoint {
      timestamp,
      metric_name: "memory.usage",
      value,
      attributes: [
        ("service.name", @azimuth.StringValue("api-service")),
        ("instance.id", @azimuth.StringValue("instance-001"))
      ]
    }
    @azimuth.MultiMetricAggregator::add_data_point(multi_metric_aggregator, data_point)
  }
  
  for (timestamp, value) in request_rate_metrics {
    let data_point = @azimuth.TimeSeriesDataPoint {
      timestamp,
      metric_name: "request.rate",
      value,
      attributes: [
        ("service.name", @azimuth.StringValue("api-service")),
        ("instance.id", @azimuth.StringValue("instance-001"))
      ]
    }
    @azimuth.MultiMetricAggregator::add_data_point(multi_metric_aggregator, data_point)
  }
  
  // 执行多指标聚合
  let aggregation_result = @azimuth.MultiMetricAggregator::aggregate(multi_metric_aggregator)
  
  // 验证聚合结果
  assert_eq(aggregation_result.metrics.length(), 3)
  
  // 验证CPU使用率聚合
  let cpu_aggregation = aggregation_result.metrics.find(fn(metric) { metric.name == "cpu.usage" })
  match cpu_aggregation {
    Some(cpu_metric) => {
      // 验证平均值
      match @azimuth.AggregatedMetric::get_function_value(cpu_metric, "avg") {
        Some(@azimuth.FloatValue(avg)) => {
          let expected_avg = (25.5 + 30.2 + 28.7 + 35.1 + 32.8) / 5.0
          assert_eq(avg, expected_avg)
        }
        _ => assert_true(false)
      }
      
      // 验证最小值
      match @azimuth.AggregatedMetric::get_function_value(cpu_metric, "min") {
        Some(@azimuth.FloatValue(min)) => assert_eq(min, 25.5)
        _ => assert_true(false)
      }
      
      // 验证最大值
      match @azimuth.AggregatedMetric::get_function_value(cpu_metric, "max") {
        Some(@azimuth.FloatValue(max)) => assert_eq(max, 35.1)
        _ => assert_true(false)
      }
      
      // 验证趋势
      match @azimuth.AggregatedMetric::get_function_value(cpu_metric, "trend") {
        Some(@azimuth.StringValue(trend)) => assert_eq(trend, "increasing")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证内存使用聚合
  let memory_aggregation = aggregation_result.metrics.find(fn(metric) { metric.name == "memory.usage" })
  match memory_aggregation {
    Some(memory_metric) => {
      // 验证平均值
      match @azimuth.AggregatedMetric::get_function_value(memory_metric, "avg") {
        Some(@azimuth.FloatValue(avg)) => {
          let expected_avg = (4096.0 + 4352.0 + 4608.0 + 4864.0 + 5120.0) / 5.0
          assert_eq(avg, expected_avg)
        }
        _ => assert_true(false)
      }
      
      // 验证趋势
      match @azimuth.AggregatedMetric::get_function_value(memory_metric, "trend") {
        Some(@azimuth.StringValue(trend)) => assert_eq(trend, "increasing")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证请求率聚合
  let request_rate_aggregation = aggregation_result.metrics.find(fn(metric) { metric.name == "request.rate" })
  match request_rate_aggregation {
    Some(request_rate_metric) => {
      // 验证平均值
      match @azimuth.AggregatedMetric::get_function_value(request_rate_metric, "avg") {
        Some(@azimuth.FloatValue(avg)) => {
          let expected_avg = (125.0 + 150.0 + 175.0 + 200.0 + 180.0) / 5.0
          assert_eq(avg, expected_avg)
        }
        _ => assert_true(false)
      }
      
      // 验证趋势
      match @azimuth.AggregatedMetric::get_function_value(request_rate_metric, "trend") {
        Some(@azimuth.StringValue(trend)) => assert_eq(trend, "fluctuating")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 5: 百分位数计算
test "percentile calculations for metrics" {
  // 创建响应时间数据
  let response_times = [
    10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105,
    110, 115, 120, 125, 130, 135, 140, 145, 150, 155, 160, 165, 170, 175, 180, 185, 190, 195, 200,
    250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000
  ]
  
  // 创建百分位数计算器
  let percentile_calculator = @azimuth.PercentileCalculator::new(
    @azimuth.PercentileConfig {
      percentiles: [50.0, 90.0, 95.0, 99.0],
      algorithm: "tdigest"  // 使用t-digest算法
    }
  )
  
  // 添加响应时间数据
  for time in response_times {
    @azimuth.PercentileCalculator::add_value(percentile_calculator, time.to_double())
  }
  
  // 计算百分位数
  let percentile_result = @azimuth.PercentileCalculator::calculate(percentile_calculator)
  
  // 验证百分位数结果
  assert_eq(percentile_result.percentiles.length(), 4)
  
  // 验证P50 (中位数)
  let p50 = percentile_result.percentiles.find(fn(p) { p.percentile == 50.0 })
  match p50 {
    Some(percentile) => {
      // 中位数应该在100-110之间
      assert_true(percentile.value >= 100.0)
      assert_true(percentile.value <= 110.0)
    }
    None => assert_true(false)
  }
  
  // 验证P90
  let p90 = percentile_result.percentiles.find(fn(p) { p.percentile == 90.0 })
  match p90 {
    Some(percentile) => {
      // P90应该在800-900之间
      assert_true(percentile.value >= 800.0)
      assert_true(percentile.value <= 900.0)
    }
    None => assert_true(false)
  }
  
  // 验证P95
  let p95 = percentile_result.percentiles.find(fn(p) { p.percentile == 95.0 })
  match p95 {
    Some(percentile) => {
      // P95应该在900-950之间
      assert_true(percentile.value >= 900.0)
      assert_true(percentile.value <= 950.0)
    }
    None => assert_true(false)
  }
  
  // 验证P99
  let p99 = percentile_result.percentiles.find(fn(p) { p.percentile == 99.0 })
  match p99 {
    Some(percentile) => {
      // P99应该在950-1000之间
      assert_true(percentile.value >= 950.0)
      assert_true(percentile.value <= 1000.0)
    }
    None => assert_true(false)
  }
  
  // 验证统计摘要
  assert_eq(percentile_result.count, response_times.length())
  assert_eq(percentile_result.sum, response_times.reduce(fn(acc, val) { acc + val.to_double() }, 0.0))
  assert_eq(percentile_result.min, 10.0)
  assert_eq(percentile_result.max, 1000.0)
  
  // 验证平均值
  let expected_avg = percentile_result.sum / percentile_result.count.to_double()
  assert_eq(percentile_result.average, expected_avg)
}

// Test 6: 指标基线比较
test "metric baseline comparison" {
  // 创建基线数据 (上周的数据)
  let baseline_cpu_data = [20.5, 22.3, 25.1, 23.7, 21.8, 24.2, 26.5]
  let baseline_memory_data = [3072.0, 3328.0, 3584.0, 3840.0, 4096.0, 4352.0, 4608.0]
  let baseline_request_data = [100.0, 110.0, 105.0, 115.0, 120.0, 125.0, 130.0]
  
  // 创建当前数据 (本周的数据)
  let current_cpu_data = [28.5, 32.3, 35.1, 33.7, 31.8, 34.2, 36.5]
  let current_memory_data = [3584.0, 3840.0, 4096.0, 4352.0, 4608.0, 4864.0, 5120.0]
  let current_request_data = [150.0, 160.0, 155.0, 165.0, 170.0, 175.0, 180.0]
  
  // 创建基线比较器
  let baseline_comparator = @azimuth.BaselineComparator::new(
    @azimuth.BaselineConfig {
      comparison_metrics: ["cpu.usage", "memory.usage", "request.rate"],
      baseline_period_days: 7,
      current_period_days: 7,
      threshold_percent: 20.0  // 20%的阈值
    }
  )
  
  // 添加基线数据
  for value in baseline_cpu_data {
    @azimuth.BaselineComparator::add_baseline_value(baseline_comparator, "cpu.usage", value)
  }
  
  for value in baseline_memory_data {
    @azimuth.BaselineComparator::add_baseline_value(baseline_comparator, "memory.usage", value)
  }
  
  for value in baseline_request_data {
    @azimuth.BaselineComparator::add_baseline_value(baseline_comparator, "request.rate", value)
  }
  
  // 添加当前数据
  for value in current_cpu_data {
    @azimuth.BaselineComparator::add_current_value(baseline_comparator, "cpu.usage", value)
  }
  
  for value in current_memory_data {
    @azimuth.BaselineComparator::add_current_value(baseline_comparator, "memory.usage", value)
  }
  
  for value in current_request_data {
    @azimuth.BaselineComparator::add_current_value(baseline_comparator, "request.rate", value)
  }
  
  // 执行基线比较
  let comparison_result = @azimuth.BaselineComparator::compare(baseline_comparator)
  
  // 验证比较结果
  assert_eq(comparison_result.comparisons.length(), 3)
  
  // 验证CPU使用率比较
  let cpu_comparison = comparison_result.comparisons.find(fn(comp) { comp.metric_name == "cpu.usage" })
  match cpu_comparison {
    Some(comparison) => {
      // 计算基线和当前的平均值
      let baseline_avg = baseline_cpu_data.reduce(fn(acc, val) { acc + val }, 0.0) / baseline_cpu_data.length().to_double()
      let current_avg = current_cpu_data.reduce(fn(acc, val) { acc + val }, 0.0) / current_cpu_data.length().to_double()
      
      assert_eq(comparison.baseline_average, baseline_avg)
      assert_eq(comparison.current_average, current_avg)
      
      // 验证变化百分比
      let expected_change_percent = ((current_avg - baseline_avg) / baseline_avg) * 100.0
      assert_eq(comparison.change_percent, expected_change_percent)
      
      // CPU使用率应该增加了超过20%
      assert_true(comparison.change_percent > 20.0)
      assert_eq(comparison.status, "alert")  // 超过阈值，应该是alert状态
    }
    None => assert_true(false)
  }
  
  // 验证内存使用比较
  let memory_comparison = comparison_result.comparisons.find(fn(comp) { comp.metric_name == "memory.usage" })
  match memory_comparison {
    Some(comparison) => {
      // 内存使用率应该增加了大约16.7%
      assert_true(comparison.change_percent > 15.0)
      assert_true(comparison.change_percent < 20.0)
      assert_eq(comparison.status, "warning")  // 低于阈值，应该是warning状态
    }
    None => assert_true(false)
  }
  
  // 验证请求率比较
  let request_comparison = comparison_result.comparisons.find(fn(comp) { comp.metric_name == "request.rate" })
  match request_comparison {
    Some(comparison) => {
      // 请求率应该增加了大约46%
      assert_true(comparison.change_percent > 40.0)
      assert_eq(comparison.status, "alert")  // 超过阈值，应该是alert状态
    }
    None => assert_true(false)
  }
  
  // 验证整体比较摘要
  assert_eq(comparison_result.metrics_with_alert, 2)
  assert_eq(comparison_result.metrics_with_warning, 1)
  assert_eq(comparison_result.metrics_with_normal, 0)
  assert_true(comparison_result.overall_status == "alert")
}