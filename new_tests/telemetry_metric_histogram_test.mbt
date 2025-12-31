// 遥测直方图指标测试用例

test "histogram_metric_creation_and_basic_operations" {
  // 测试直方图指标的创建和基本操作
  
  // 创建默认直方图
  let histogram = azimuth::telemetry::api::metrics::Histogram::new("request.duration", "Request duration in seconds")
  assert_eq(histogram.get_name(), "request.duration")
  assert_eq(histogram.get_description(), "Request duration in seconds")
  assert_eq(histogram.get_count(), 0)
  assert_eq(histogram.get_sum(), 0.0)
  
  // 记录观测值
  histogram.record(1.5)
  histogram.record(2.3)
  histogram.record(0.8)
  histogram.record(4.1)
  histogram.record(2.9)
  
  assert_eq(histogram.get_count(), 5)
  assert_eq(histogram.get_sum(), 11.6)
  assert_eq(histogram.get_min(), 0.8)
  assert_eq(histogram.get_max(), 4.1)
  assert_eq(histogram.get_average(), 2.32)
}

test "histogram_with_custom_buckets" {
  // 测试自定义桶边界的直方图
  
  // 创建自定义桶边界
  let custom_buckets = [0.1, 0.5, 1.0, 2.0, 5.0, 10.0]
  let histogram = azimuth::telemetry::api::metrics::Histogram::with_buckets(
    "response.size", 
    "Response size in bytes", 
    custom_buckets
  )
  
  // 记录不同大小的观测值
  histogram.record(0.05)  // 小于第一个桶
  histogram.record(0.3)   // 在第一个桶
  histogram.record(0.8)   // 在第二个桶
  histogram.record(1.5)   // 在第三个桶
  histogram.record(3.7)   // 在第四个桶
  histogram.record(8.2)   // 在第五个桶
  histogram.record(15.0)  // 在溢出桶
  
  assert_eq(histogram.get_count(), 7)
  assert_eq(histogram.get_sum(), 29.55)
  
  // 验证桶计数
  let bucket_counts = histogram.get_bucket_counts()
  assert_eq(bucket_counts.length(), custom_buckets.length() + 1)  // 包括溢出桶
  assert_eq(bucket_counts[0], 1)  // <= 0.1
  assert_eq(bucket_counts[1], 1)  // <= 0.5
  assert_eq(bucket_counts[2], 1)  // <= 1.0
  assert_eq(bucket_counts[3], 1)  // <= 2.0
  assert_eq(bucket_counts[4], 1)  // <= 5.0
  assert_eq(bucket_counts[5], 1)  // <= 10.0
  assert_eq(bucket_counts[6], 1)  // > 10.0
}

test "histogram_percentile_calculations" {
  // 测试直方图百分位数计算
  
  let histogram = azimuth::telemetry::api::metrics::Histogram::new("latency", "Request latency")
  
  // 记录一组已知的观测值
  let observations = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
  for obs in observations {
    histogram.record(obs)
  }
  
  // 测试百分位数
  assert_eq(histogram.get_percentile(0.0), 1.0)    // 最小值
  assert_eq(histogram.get_percentile(0.5), 5.5)    // 中位数
  assert_eq(histogram.get_percentile(0.9), 9.1)    // 90%分位数
  assert_eq(histogram.get_percentile(0.95), 9.55)  // 95%分位数
  assert_eq(histogram.get_percentile(0.99), 9.91)  // 99%分位数
  assert_eq(histogram.get_percentile(1.0), 10.0)   // 最大值
}

test "histogram_exponential_buckets" {
  // 测试指数桶边界
  
  // 创建指数桶边界
  let histogram = azimuth::telemetry::api::metrics::Histogram::with_exponential_buckets(
    "exponential.metric",
    "Exponential bucket test",
    1.0,  // 起始值
    2.0,  // 倍数
    10    // 桶数量
  )
  
  // 记录跨越多个桶的观测值
  histogram.record(0.5)   // 小于起始值
  histogram.record(1.0)   // 第一个桶
  histogram.record(2.5)   // 第二个桶
  histogram.record(7.0)   // 第三个桶
  histogram.record(20.0)  // 第五个桶
  histogram.record(100.0) // 溢出桶
  
  assert_eq(histogram.get_count(), 6)
  assert_eq(histogram.get_sum(), 131.0)
  
  // 验证指数桶边界
  let bucket_bounds = histogram.get_bucket_bounds()
  assert_eq(bucket_bounds[0], 1.0)
  assert_eq(bucket_bounds[1], 2.0)
  assert_eq(bucket_bounds[2], 4.0)
  assert_eq(bucket_bounds[3], 8.0)
  assert_eq(bucket_bounds[4], 16.0)
  assert_eq(bucket_bounds[5], 32.0)
  assert_eq(bucket_bounds[6], 64.0)
  assert_eq(bucket_bounds[7], 128.0)
  assert_eq(bucket_bounds[8], 256.0)
  assert_eq(bucket_bounds[9], 512.0)
}

test "histogram_with_attributes" {
  // 测试带属性的直方图
  
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("service.name", azimuth::telemetry::api::common::AttributeValue::String("api-service"))
    .with("endpoint", azimuth::telemetry::api::common::AttributeValue::String("/users"))
    .with("method", azimuth::telemetry::api::common::AttributeValue::String("GET"))
  
  let histogram = azimuth::telemetry::api::metrics::Histogram::with_attributes(
    "request.duration.by.endpoint",
    "Request duration by endpoint",
    attributes
  )
  
  // 记录观测值
  histogram.record(1.2)
  histogram.record(2.5)
  histogram.record(0.9)
  
  assert_eq(histogram.get_count(), 3)
  assert_eq(histogram.get_sum(), 4.6)
  
  // 验证属性
  let histogram_attrs = histogram.get_attributes()
  assert_eq(histogram_attrs.size(), 3)
  assert_eq(histogram_attrs.get("service.name").unwrap().to_string(), "api-service")
  assert_eq(histogram_attrs.get("endpoint").unwrap().to_string(), "/users")
  assert_eq(histogram_attrs.get("method").unwrap().to_string(), "GET")
}

test "histogram_reset_and_clear" {
  // 测试直方图重置和清空
  
  let histogram = azimuth::telemetry::api::metrics::Histogram::new("test.metric", "Test metric")
  
  // 记录一些观测值
  histogram.record(1.0)
  histogram.record(2.0)
  histogram.record(3.0)
  
  assert_eq(histogram.get_count(), 3)
  assert_eq(histogram.get_sum(), 6.0)
  
  // 重置直方图
  histogram.reset()
  
  // 验证重置后的状态
  assert_eq(histogram.get_count(), 0)
  assert_eq(histogram.get_sum(), 0.0)
  assert_eq(histogram.get_min(), 0.0)
  assert_eq(histogram.get_max(), 0.0)
  
  // 重新记录观测值
  histogram.record(5.0)
  histogram.record(7.0)
  
  assert_eq(histogram.get_count(), 2)
  assert_eq(histogram.get_sum(), 12.0)
  assert_eq(histogram.get_min(), 5.0)
  assert_eq(histogram.get_max(), 7.0)
}

test "histogram_concurrent_operations" {
  // 测试直方图的并发操作
  
  let histogram = azimuth::telemetry::api::metrics::Histogram::new("concurrent.metric", "Concurrent test")
  
  // 模拟并发记录操作
  for i in range(0, 100) {
    histogram.record(i.to_float())
  }
  
  assert_eq(histogram.get_count(), 100)
  assert_eq(histogram.get_sum(), 4950.0)  // 0 + 1 + 2 + ... + 99
  assert_eq(histogram.get_min(), 0.0)
  assert_eq(histogram.get_max(), 99.0)
  assert_eq(histogram.get_average(), 49.5)
}

test "histogram_boundary_conditions" {
  // 测试直方图的边界条件
  
  let histogram = azimuth::telemetry::api::metrics::Histogram::new("boundary.test", "Boundary test")
  
  // 测试极小值
  histogram.record(0.0)
  histogram.record(-1.0)  // 负值
  
  // 测试极大值
  histogram.record(1.7976931348623157e+308)  // Float64最大值
  
  // 测试特殊值
  histogram.record(1.0 / 0.0)  // 正无穷
  // histogram.record(-1.0 / 0.0)  // 负无穷 - 可能不支持
  // histogram.record(0.0 / 0.0)   // NaN - 可能不支持
  
  assert_eq(histogram.get_count(), 3)  // 或者更多，取决于特殊值的处理
  
  // 验证最小值和最大值
  let min_val = histogram.get_min()
  let max_val = histogram.get_max()
  
  assert_eq(min_val <= 0.0, true)
  assert_eq(max_val > 0.0, true)
  
  // 测试空直方图的统计操作
  let empty_histogram = azimuth::telemetry::api::metrics::Histogram::new("empty.test", "Empty test")
  assert_eq(empty_histogram.get_count(), 0)
  assert_eq(empty_histogram.get_sum(), 0.0)
  assert_eq(empty_histogram.get_average(), 0.0)
  assert_eq(empty_histogram.get_min(), 0.0)
  assert_eq(empty_histogram.get_max(), 0.0)
}

test "histogram_serialization_and_export" {
  // 测试直方图的序列化和导出
  
  let histogram = azimuth::telemetry::api::metrics::Histogram::new("export.test", "Export test")
  
  // 记录观测值
  histogram.record(1.5)
  histogram.record(2.3)
  histogram.record(0.8)
  
  // 导出为JSON
  let json_export = histogram.to_json()
  assert_eq(json_export.length() > 0, true)
  assert_eq(json_export.contains("export.test"), true)
  assert_eq(json_export.contains("count"), true)
  assert_eq(json_export.contains("sum"), true)
  
  // 导出为Prometheus格式
  let prometheus_export = histogram.to_prometheus()
  assert_eq(prometheus_export.length() > 0, true)
  assert_eq(prometheus_export.contains("export.test"), true)
  assert_eq(prometheus_export.contains(" histogram "), true)
  
  // 从JSON恢复
  let restored_histogram = azimuth::telemetry::api::metrics::Histogram::from_json(json_export)
  assert_eq(restored_histogram.get_name(), histogram.get_name())
  assert_eq(restored_histogram.get_description(), histogram.get_description())
  assert_eq(restored_histogram.get_count(), histogram.get_count())
  assert_eq(restored_histogram.get_sum(), histogram.get_sum())
}