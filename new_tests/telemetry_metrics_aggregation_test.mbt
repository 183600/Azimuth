// 指标聚合测试用例

test "counter_instrument_basic_operations" {
  // 测试计数器仪器的基本操作
  
  // 创建Meter和计数器
  let meter = azimuth::telemetry::api::metrics::MeterProvider::global().get_meter("test-meter", "1.0.0")
  let counter = meter.create_counter("http.requests.total", "Total number of HTTP requests", "requests")
  
  // 添加属性
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("http.method", azimuth::telemetry::api::common::AttributeValue::String("GET"))
    .with("http.status_code", azimuth::telemetry::api::common::AttributeValue::Int64(200))
  
  // 记录测量值
  counter.add(1, attributes)
  counter.add(3, attributes)
  counter.add(2, attributes)
  
  // 验证聚合结果
  let measurement = counter.collect(attributes)
  assert_eq(measurement.value, 6)
  assert_eq(measurement.attributes.size(), 2)
  
  // 测试不同属性集
  let error_attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("http.method", azimuth::telemetry::api::common::AttributeValue::String("GET"))
    .with("http.status_code", azimuth::telemetry::api::common::AttributeValue::Int64(500))
  
  counter.add(1, error_attributes)
  
  let error_measurement = counter.collect(error_attributes)
  assert_eq(error_measurement.value, 1)
  assert_eq(error_measurement.attributes.get("http.status_code").unwrap().to_string(), "500")
}

test "histogram_instrument_basic_operations" {
  // 测试直方图仪器的基本操作
  
  let meter = azimuth::telemetry::api::metrics::MeterProvider::global().get_meter("test-meter", "1.0.0")
  let histogram = meter.create_histogram("http.request.duration", "HTTP request duration", "ms")
  
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("http.method", azimuth::telemetry::api::common::AttributeValue::String("POST"))
    .with("http.route", azimuth::telemetry::api::common::AttributeValue::String("/api/users"))
  
  // 记录不同的响应时间
  histogram.record(50.0, attributes)   // 50ms
  histogram.record(120.0, attributes)  // 120ms
  histogram.record(85.0, attributes)   // 85ms
  histogram.record(200.0, attributes)  // 200ms
  histogram.record(45.0, attributes)   // 45ms
  
  // 收集聚合结果
  let measurement = histogram.collect(attributes)
  
  // 验证统计信息
  assert_eq(measurement.count, 5)
  assert_eq(measurement.sum, 500.0)
  assert_eq(measurement.min, 45.0)
  assert_eq(measurement.max, 200.0)
  assert_eq(measurement.mean, 100.0)
  
  // 验证分位数
  assert_eq(measurement.percentile_50, 85.0)  // 中位数
  assert_eq(measurement.percentile_95, 200.0) // 95%分位数
  assert_eq(measurement.percentile_99, 200.0) // 99%分位数
}

test "up_down_counter_instrument_operations" {
  // 测试上下计数器仪器操作
  
  let meter = azimuth::telemetry::api::metrics::MeterProvider::global().get_meter("test-meter", "1.0.0")
  let up_down_counter = meter.create_up_down_counter("active.connections", "Number of active connections", "connections")
  
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("connection.type", azimuth::telemetry::api::common::AttributeValue::String("websocket"))
    .with("service.name", azimuth::telemetry::api::common::AttributeValue::String("chat-service"))
  
  // 增加连接数
  up_down_counter.add(10, attributes)
  let measurement1 = up_down_counter.collect(attributes)
  assert_eq(measurement1.value, 10)
  
  // 减少连接数
  up_down_counter.add(-3, attributes)
  let measurement2 = up_down_counter.collect(attributes)
  assert_eq(measurement2.value, 7)
  
  // 增加更多连接
  up_down_counter.add(5, attributes)
  let measurement3 = up_down_counter.collect(attributes)
  assert_eq(measurement3.value, 12)
  
  // 测试负值
  up_down_counter.add(-15, attributes)
  let measurement4 = up_down_counter.collect(attributes)
  assert_eq(measurement4.value, -3)
}

test "gauge_instrument_operations" {
  // 测试仪表仪器操作
  
  let meter = azimuth::telemetry::api::metrics::MeterProvider::global().get_meter("test-meter", "1.0.0")
  let gauge = meter.create_gauge("memory.usage", "Memory usage", "bytes")
  
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("memory.type", azimuth::telemetry::api::common::AttributeValue::String("heap"))
    .with("process.name", azimuth::telemetry::api::common::AttributeValue::String("worker-process"))
  
  // 设置不同的内存使用值
  gauge.record(1024 * 1024 * 100, attributes)  // 100MB
  let measurement1 = gauge.collect(attributes)
  assert_eq(measurement1.value, 1024 * 1024 * 100)
  
  gauge.record(1024 * 1024 * 150, attributes)  // 150MB
  let measurement2 = gauge.collect(attributes)
  assert_eq(measurement2.value, 1024 * 1024 * 150)
  
  gauge.record(1024 * 1024 * 80, attributes)   // 80MB
  let measurement3 = gauge.collect(attributes)
  assert_eq(measurement3.value, 1024 * 1024 * 80)
}

test "metric_aggregation_with_multiple_attributes" {
  // 测试多属性指标的聚合
  
  let meter = azimuth::telemetry::api::metrics::MeterProvider::global().get_meter("test-meter", "1.0.0")
  let counter = meter.create_counter("api.calls.total", "Total API calls", "calls")
  
  // 记录不同属性组合的测量值
  let get_success_attrs = azimuth::telemetry::api::common::Attributes::empty()
    .with("method", azimuth::telemetry::api::common::AttributeValue::String("GET"))
    .with("status", azimuth::telemetry::api::common::AttributeValue::String("success"))
    .with("endpoint", azimuth::telemetry::api::common::AttributeValue::String("/api/users"))
  
  let get_error_attrs = azimuth::telemetry::api::common::Attributes::empty()
    .with("method", azimuth::telemetry::api::common::AttributeValue::String("GET"))
    .with("status", azimuth::telemetry::api::common::AttributeValue::String("error"))
    .with("endpoint", azimuth::telemetry::api::common::AttributeValue::String("/api/users"))
  
  let post_success_attrs = azimuth::telemetry::api::common::Attributes::empty()
    .with("method", azimuth::telemetry::api::common::AttributeValue::String("POST"))
    .with("status", azimuth::telemetry::api::common::AttributeValue::String("success"))
    .with("endpoint", azimuth::telemetry::api::common::AttributeValue::String("/api/orders"))
  
  // 记录测量值
  counter.add(15, get_success_attrs)
  counter.add(3, get_error_attrs)
  counter.add(7, post_success_attrs)
  counter.add(5, get_success_attrs)  // 再次记录GET成功
  counter.add(2, post_success_attrs) // 再次记录POST成功
  
  // 收集并验证结果
  let get_success_measurement = counter.collect(get_success_attrs)
  let get_error_measurement = counter.collect(get_error_attrs)
  let post_success_measurement = counter.collect(post_success_attrs)
  
  assert_eq(get_success_measurement.value, 20)
  assert_eq(get_error_measurement.value, 3)
  assert_eq(post_success_measurement.value, 9)
  
  // 验证属性
  assert_eq(get_success_measurement.attributes.get("method").unwrap().to_string(), "GET")
  assert_eq(get_success_measurement.attributes.get("status").unwrap().to_string(), "success")
  assert_eq(get_error_measurement.attributes.get("status").unwrap().to_string(), "error")
}

test "time_window_aggregation" {
  // 测试时间窗口聚合
  
  let meter = azimuth::telemetry::api::metrics::MeterProvider::global().get_meter("test-meter", "1.0.0")
  let histogram = meter.create_histogram("response.time", "Response time", "ms")
  
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("service", azimuth::telemetry::api::common::AttributeValue::String("api-gateway"))
  
  // 模拟不同时间点的测量
  let base_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  
  // 第一个时间窗口（0-10秒）
  histogram.record_with_timestamp(100.0, attributes, base_time + 1_000_000_000)  // 1秒后
  histogram.record_with_timestamp(150.0, attributes, base_time + 2_000_000_000)  // 2秒后
  histogram.record_with_timestamp(80.0, attributes, base_time + 3_000_000_000)   // 3秒后
  
  // 第二个时间窗口（10-20秒）
  histogram.record_with_timestamp(200.0, attributes, base_time + 11_000_000_000) // 11秒后
  histogram.record_with_timestamp(120.0, attributes, base_time + 12_000_000_000) // 12秒后
  histogram.record_with_timestamp(180.0, attributes, base_time + 13_000_000_000) // 13秒后
  
  // 收集第一个时间窗口的数据
  let window1_start = base_time
  let window1_end = base_time + 10_000_000_000
  let window1_measurement = histogram.collect_with_time_range(attributes, window1_start, window1_end)
  
  assert_eq(window1_measurement.count, 3)
  assert_eq(window1_measurement.sum, 330.0)
  assert_eq(window1_measurement.mean, 110.0)
  
  // 收集第二个时间窗口的数据
  let window2_start = base_time + 10_000_000_000
  let window2_end = base_time + 20_000_000_000
  let window2_measurement = histogram.collect_with_time_range(attributes, window2_start, window2_end)
  
  assert_eq(window2_measurement.count, 3)
  assert_eq(window2_measurement.sum, 500.0)
  assert_eq(window2_measurement.mean, 166.66666666666666)
}

test "metric_exemplar_support" {
  // 测试指标示例支持
  
  let meter = azimuth::telemetry::api::metrics::MeterProvider::global().get_meter("test-meter", "1.0.0")
  let histogram = meter.create_histogram_with_exemplars("latency", "Request latency", "ms")
  
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("operation", azimuth::telemetry::api::common::AttributeValue::String("database.query"))
  
  // 创建示例上下文
  let exemplar_ctx = azimuth::telemetry::api::context::Context::empty()
    .with_value(azimuth::telemetry::api::context::create_key("trace_id"), "trace-123-abc")
    .with_value(azimuth::telemetry::api::context::create_key("span_id"), "span-456-def")
  
  // 记录带示例的测量
  histogram.record_with_exemplar(150.0, attributes, exemplar_ctx)
  histogram.record_with_exemplar(200.0, attributes, exemplar_ctx)
  histogram.record_with_exemplar(75.0, attributes, exemplar_ctx)
  
  // 收集测量和示例
  let measurement = histogram.collect(attributes)
  
  assert_eq(measurement.count, 3)
  assert_eq(measurement.exemplars.length(), 3)
  
  // 验证示例
  let first_exemplar = measurement.exemplars[0]
  assert_eq(first_exemplar.value, 150.0)
  assert_eq(first_exemplar.trace_id, "trace-123-abc")
  assert_eq(first_exemplar.span_id, "span-456-def")
  
  let second_exemplar = measurement.exemplars[1]
  assert_eq(second_exemplar.value, 200.0)
  assert_eq(second_exemplar.trace_id, "trace-123-abc")
  
  let third_exemplar = measurement.exemplars[2]
  assert_eq(third_exemplar.value, 75.0)
  assert_eq(third_exemplar.trace_id, "trace-123-abc")
}

test "metric_cardinality_limits" {
  // 测试指标基数限制
  
  let meter = azimuth::telemetry::api::metrics::MeterProvider::global().get_meter("test-meter", "1.0.0")
  let counter = meter.create_counter("high.cardinality.metric", "High cardinality metric", "operations")
  
  // 设置基数限制为5
  let limited_counter = counter.with_cardinality_limit(5)
  
  // 记录超过限制的不同属性组合
  for i = 0; i < 10; i = i + 1 {
    let attributes = azimuth::telemetry::api::common::Attributes::empty()
      .with("user.id", azimuth::telemetry::api::common::AttributeValue::String("user" + i.to_string()))
      .with("operation", azimuth::telemetry::api::common::AttributeValue::String("read"))
    
    limited_counter.add(1, attributes)
  }
  
  // 收集所有测量
  let all_measurements = limited_counter.collect_all()
  
  // 验证基数限制生效
  assert_eq(all_measurements.length() <= 5, true)
  
  // 验证总计数仍然正确
  let total_count = 0
  for measurement in all_measurements {
    total_count = total_count + measurement.value
  }
  assert_eq(total_count, 10)
}

test "metric_aggregation_temporality" {
  // 测试指标聚合时间性
  
  let meter = azimuth::telemetry::api::metrics::MeterProvider::global().get_meter("test-meter", "1.0.0")
  let counter = meter.create_counter("cumulative.counter", "Cumulative counter", "events")
  
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("event.type", azimuth::telemetry::api::common::AttributeValue::String("user.login"))
  
  // 累积时间性
  let cumulative_counter = counter.with_temporality(azimuth::telemetry::api::metrics::Temporality::Cumulative)
  
  cumulative_counter.add(5, attributes)
  let measurement1 = cumulative_counter.collect(attributes)
  assert_eq(measurement1.temporality, azimuth::telemetry::api::metrics::Temporality::Cumulative)
  assert_eq(measurement1.value, 5)
  
  cumulative_counter.add(3, attributes)
  let measurement2 = cumulative_counter.collect(attributes)
  assert_eq(measurement2.value, 8)  // 累积值
  
  // 增量时间性
  let delta_counter = counter.with_temporality(azimuth::telemetry::api::metrics::Temporality::Delta)
  
  delta_counter.add(4, attributes)
  let delta_measurement1 = delta_counter.collect(attributes)
  assert_eq(delta_measurement1.temporality, azimuth::telemetry::api::metrics::Temporality::Delta)
  assert_eq(delta_measurement1.value, 4)
  
  delta_counter.add(2, attributes)
  let delta_measurement2 = delta_counter.collect(attributes)
  assert_eq(delta_measurement2.value, 2)  // 增量值
}

test "metric_memory_efficiency" {
  // 测试指标内存效率
  
  let meter = azimuth::telemetry::api::metrics::MeterProvider::global().get_meter("test-meter", "1.0.0")
  let histogram = meter.create_histogram("memory.efficient.metric", "Memory efficient metric", "bytes")
  
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("component", azimuth::telemetry::api::common::AttributeValue::String("cache"))
  
  // 记录大量测量值
  for i = 0; i < 1000; i = i + 1 {
    let value = (i % 100).to_float() + 50.0  // 50-149的值
    histogram.record(value, attributes)
  }
  
  // 收集聚合结果
  let measurement = histogram.collect(attributes)
  
  // 验证聚合正确性
  assert_eq(measurement.count, 1000)
  assert_eq(measurement.min, 50.0)
  assert_eq(measurement.max, 149.0)
  
  // 验证内存使用合理（通过检查聚合后的数据点数量）
  assert_eq(measurement.buckets.length() <= 50, true)  // 应该有合理的桶数量
}

test "metric_export_preparation" {
  // 测试指标导出准备
  
  let meter = azimuth::telemetry::api::metrics::MeterProvider::global().get_meter("test-meter", "1.0.0")
  
  let counter = meter.create_counter("export.test.counter", "Export test counter", "units")
  let histogram = meter.create_histogram("export.test.histogram", "Export test histogram", "ms")
  
  let counter_attrs = azimuth::telemetry::api::common::Attributes::empty()
    .with("service", azimuth::telemetry::api::common::AttributeValue::String("auth-service"))
  
  let histogram_attrs = azimuth::telemetry::api::common::Attributes::empty()
    .with("service", azimuth::telemetry::api::common::AttributeValue::String("user-service"))
  
  // 记录测量值
  counter.add(10, counter_attrs)
  counter.add(5, counter_attrs)
  
  histogram.record(100.0, histogram_attrs)
  histogram.record(150.0, histogram_attrs)
  histogram.record(75.0, histogram_attrs)
  
  // 准备导出数据
  let resource = azimuth::telemetry::api::common::Resource::builder()
    .with("service.name", azimuth::telemetry::api::common::AttributeValue::String("test-service"))
    .build()
  
  let scope = azimuth::telemetry::api::common::InstrumentationScope::new("test-meter", "1.0.0")
  
  let export_metrics = azimuth::telemetry::sdk::metrics::prepare_for_export([
    azimuth::telemetry::sdk::metrics::MetricData::Counter {
      name: "export.test.counter",
      description: "Export test counter",
      unit: "units",
      data: counter.collect_all()
    },
    azimuth::telemetry::sdk::metrics::MetricData::Histogram {
      name: "export.test.histogram",
      description: "Export test histogram",
      unit: "ms",
      data: histogram.collect_all()
    }
  ], resource, scope)
  
  // 验证导出数据结构
  assert_eq(export_metrics.resource_metrics.length(), 1)
  assert_eq(export_metrics.resource_metrics[0].scope_metrics.length(), 1)
  assert_eq(export_metrics.resource_metrics[0].scope_metrics[0].metrics.length(), 2)
  
  // 验证资源信息
  let exported_resource = export_metrics.resource_metrics[0].resource
  assert_eq(exported_resource.attributes.get("service.name").unwrap().to_string(), "test-service")
  
  // 验证仪器范围
  let exported_scope = export_metrics.resource_metrics[0].scope_metrics[0].scope
  assert_eq(exported_scope.name, "test-meter")
  assert_eq(exported_scope.version, "1.0.0")
}