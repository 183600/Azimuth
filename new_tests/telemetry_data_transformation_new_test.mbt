// 遥测数据转换测试用例

test "telemetry_data_format_conversion" {
  // 测试遥测数据格式转换
  
  // 创建原始遥测数据
  let original_data = azimuth::telemetry::api::common::TelemetryData::new()
    .with_trace_id("trace-12345")
    .with_span_id("span-67890")
    .with_timestamp(1640995200000)
    .with_service_name("test-service")
    .with_operation_name("user.login")
    .with_duration(150000)  // 150ms
    .with_status("SUCCESS")
    .with_attributes({
      "user.id": "user123",
      "http.method": "POST",
      "http.status_code": "200",
      "user.agent": "Mozilla/5.0"
    })
  
  // 转换为OpenTelemetry格式
  let otel_format = azimuth::telemetry::api::transform::DataTransformer::to_opentelemetry(original_data)
  assert_eq(otel_format.trace_id, "trace-12345")
  assert_eq(otel_format.span_id, "span-67890")
  assert_eq(otel_format.timestamp_unix_nano, 1640995200000000000)  // 转换为纳秒
  assert_eq(otel_format.service_name, "test-service")
  assert_eq(otel_format.name, "user.login")
  assert_eq(otel_format.duration_nano, 150000000)  // 转换为纳秒
  assert_eq(otel_format.status.code, 1)  // OK status code
  
  // 转换为Prometheus格式
  let prometheus_format = azimuth::telemetry::api::transform::DataTransformer::to_prometheus(original_data)
  assert_eq(prometheus_format.metric_name, "test_service_user_login_duration")
  assert_eq(prometheus_format.metric_value, 0.15)  // 转换为秒
  assert_eq(prometheus_format.labels.contains("service=\"test-service\""), true)
  assert_eq(prometheus_format.labels.contains("operation=\"user.login\""), true)
  assert_eq(prometheus_format.labels.contains("status=\"SUCCESS\""), true)
  
  // 转换为Jaeger格式
  let jaeger_format = azimuth::telemetry::api::transform::DataTransformer::to_jaeger(original_data)
  assert_eq(jaeger_format.traceId, "trace12345")  // Jaeger使用不同的格式
  assert_eq(jaeger_format.spanId, "span67890")
  assert_eq(jaeger_format.operationName, "user.login")
  assert_eq(jaeger_format.startTime, 1640995200000)
  assert_eq(jaeger_format.duration, 150000)
  assert_eq(jaeger_format.tags.length(), 4)  // 4个属性
}

test "telemetry_data_unit_conversion" {
  // 测试遥测数据单位转换
  
  // 创建带单位的遥测指标
  let time_metric = azimuth::telemetry::api::metrics::Metric::new("response_time", 150.0, "milliseconds")
  let memory_metric = azimuth::telemetry::api::metrics::Metric::new("memory_usage", 1024.0, "kilobytes")
  let cpu_metric = azimuth::telemetry::api::metrics::Metric::new("cpu_usage", 75.5, "percent")
  let bytes_metric = azimuth::telemetry::api::metrics::Metric::new("network_throughput", 1048576.0, "bytes_per_second")
  
  // 创建单位转换器
  let unit_converter = azimuth::telemetry::api::transform::UnitConverter::new()
  
  // 时间单位转换
  let time_seconds = unit_converter.convert(time_metric, "seconds")
  assert_eq(time_seconds.value, 0.15)
  assert_eq(time_seconds.unit, "seconds")
  
  let time_microseconds = unit_converter.convert(time_metric, "microseconds")
  assert_eq(time_microseconds.value, 150000.0)
  assert_eq(time_microseconds.unit, "microseconds")
  
  // 内存单位转换
  let memory_megabytes = unit_converter.convert(memory_metric, "megabytes")
  assert_eq(memory_megabytes.value, 1.0)
  assert_eq(memory_megabytes.unit, "megabytes")
  
  let memory_bytes = unit_converter.convert(memory_metric, "bytes")
  assert_eq(memory_bytes.value, 1048576.0)
  assert_eq(memory_bytes.unit, "bytes")
  
  // 百分比转换为小数
  let cpu_decimal = unit_converter.convert(cpu_metric, "decimal")
  assert_eq(cpu_decimal.value, 0.755)
  assert_eq(cpu_decimal.unit, "decimal")
  
  // 网络吞吐量单位转换
  let throughput_mbps = unit_converter.convert(bytes_metric, "megabits_per_second")
  assert_eq(throughput_mbps.value, 8.0)  // 1048576 bytes/s = 8 megabits/s
  assert_eq(throughput_mbps.unit, "megabits_per_second")
}

test "telemetry_data_aggregation_transformation" {
  // 测试遥测数据聚合转换
  
  // 创建原始数据点
  let data_points = [
    azimuth::telemetry::api::metrics::DataPoint::new(1640995200000, 100.0),
    azimuth::telemetry::api::metrics::DataPoint::new(1640995260000, 120.0),
    azimuth::telemetry::api::metrics::DataPoint::new(1640995320000, 90.0),
    azimuth::telemetry::api::metrics::DataPoint::new(1640995380000, 110.0),
    azimuth::telemetry::api::metrics::DataPoint::new(1640995440000, 130.0),
    azimuth::telemetry::api::metrics::DataPoint::new(1640995500000, 95.0),
    azimuth::telemetry::api::metrics::DataPoint::new(1640995560000, 105.0),
    azimuth::telemetry::api::metrics::DataPoint::new(1640995620000, 115.0)
  ]
  
  // 创建时间序列数据
  let time_series = azimuth::telemetry::api::metrics::TimeSeries::new("cpu_usage", data_points)
  
  // 创建聚合转换器
  let aggregation_transformer = azimuth::telemetry::api::transform::AggregationTransformer::new()
  
  // 按分钟聚合（原始数据已经是每分钟）
  let minute_aggregated = aggregation_transformer.aggregate_by_interval(time_series, 60000)  // 60秒
  assert_eq(minute_aggregated.data_points.length(), 8)
  assert_eq(minute_aggregated.data_points[0].value, 100.0)
  assert_eq(minute_aggregated.data_points[7].value, 115.0)
  
  // 按5分钟聚合
  let five_min_aggregated = aggregation_transformer.aggregate_by_interval(time_series, 300000)  // 300秒
  assert_eq(five_min_aggregated.data_points.length(), 2)
  
  // 验证第一个5分钟窗口的平均值
  let first_window_avg = (100.0 + 120.0 + 90.0 + 110.0 + 130.0) / 5.0
  assert_eq(five_min_aggregated.data_points[0].value, first_window_avg)
  
  // 验证第二个5分钟窗口的平均值
  let second_window_avg = (95.0 + 105.0 + 115.0) / 3.0
  assert_eq(five_min_aggregated.data_points[1].value, second_window_avg)
  
  // 计算移动平均
  let moving_average_3 = aggregation_transformer.moving_average(time_series, 3)
  assert_eq(moving_average_3.data_points.length(), 6)  // 少2个点
  
  // 验证移动平均计算
  assert_eq(moving_average_3.data_points[0].value, (100.0 + 120.0 + 90.0) / 3.0)
  assert_eq(moving_average_3.data_points[1].value, (120.0 + 90.0 + 110.0) / 3.0)
  assert_eq(moving_average_3.data_points[5].value, (95.0 + 105.0 + 115.0) / 3.0)
}

test "telemetry_data_normalization_and_standardization" {
  // 测试遥测数据标准化和规范化
  
  // 创建来自不同服务的遥测数据
  let service_a_data = azimuth::telemetry::api::trace::TraceData::new()
    .with_service_name("user-service")
    .with_operation_name("authenticate")
    .with_duration(150)  // ms
    .with_status("success")
    .with_attributes({
      "user_id": "123",
      "method": "POST",
      "endpoint": "/api/auth"
    })
  
  let service_b_data = azimuth::telemetry::api::trace::TraceData::new()
    .with_service_name("payment-service")
    .with_operation_name("process_payment")
    .with_duration(2500)  // ms
    .with_status("completed")
    .with_attributes({
      "user_id": "123",
      "payment_method": "credit_card",
      "amount": "99.99"
    })
  
  let service_c_data = azimuth::telemetry::api::trace::TraceData::new()
    .with_service_name("notification-service")
    .with_operation_name("send_email")
    .with_duration(800)  // ms
    .with_status("sent")
    .with_attributes({
      "user_id": "123",
      "notification_type": "email",
      "template": "welcome"
    })
  
  // 创建标准化器
  let normalizer = azimuth::telemetry::api::transform::DataNormalizer::new()
    .with_standard_attributes(["user_id", "trace_id", "request_id"])
    .with_status_mapping({
      "success": "SUCCESS",
      "completed": "SUCCESS",
      "sent": "SUCCESS",
      "failed": "ERROR",
      "error": "ERROR"
    })
    .with_duration_unit("milliseconds")
  
  // 标准化数据
  let normalized_a = normalizer.normalize(service_a_data)
  let normalized_b = normalizer.normalize(service_b_data)
  let normalized_c = normalizer.normalize(service_c_data)
  
  // 验证状态标准化
  assert_eq(normalized_a.status, "SUCCESS")
  assert_eq(normalized_b.status, "SUCCESS")
  assert_eq(normalized_c.status, "SUCCESS")
  
  // 验证标准属性存在
  assert_eq(normalized_a.attributes.contains("user_id"), true)
  assert_eq(normalized_b.attributes.contains("user_id"), true)
  assert_eq(normalized_c.attributes.contains("user_id"), true)
  
  // 验证持续时间单位一致
  assert_eq(normalized_a.duration_unit, "milliseconds")
  assert_eq(normalized_b.duration_unit, "milliseconds")
  assert_eq(normalized_c.duration_unit, "milliseconds")
}

test "telemetry_data_enrichment_and_annotation" {
  // 测试遥测数据丰富和注释
  
  // 创建基础遥测数据
  let base_data = azimuth::telemetry::api::trace::TraceData::new()
    .with_trace_id("trace-enrich-123")
    .with_span_id("span-enrich-456")
    .with_service_name("api-service")
    .with_operation_name("get_user_profile")
    .with_duration(75)
    .with_status("success")
    .with_attributes({
      "user_id": "user789",
      "request_id": "req-456"
    })
  
  // 创建数据丰富器
  let enricher = azimuth::telemetry::api::transform::DataEnricher::new()
    .with_static_enrichment({
      "environment": "production",
      "region": "us-west-2",
      "datacenter": "dc-01"
    })
    .with_dynamic_enrichment(|data| {
      // 根据数据内容动态添加属性
      if data.operation_name.contains("user") {
        {"domain": "user_management"}
      } else {
        {}
      }
    })
    .with_geo_enrichment(true)  // 启用地理位置丰富
    .with_service_topology_enrichment(true)  // 启用服务拓扑丰富
  
  // 丰富数据
  let enriched_data = enricher.enrich(base_data)
  
  // 验证静态丰富
  assert_eq(enriched_data.attributes.get("environment").unwrap(), "production")
  assert_eq(enriched_data.attributes.get("region").unwrap(), "us-west-2")
  assert_eq(enriched_data.attributes.get("datacenter").unwrap(), "dc-01")
  
  // 验证动态丰富
  assert_eq(enriched_data.attributes.get("domain").unwrap(), "user_management")
  
  // 验证地理位置丰富
  assert_eq(enriched_data.attributes.contains("country"), true)
  assert_eq(enriched_data.attributes.contains("city"), true)
  
  // 验证服务拓扑丰富
  assert_eq(enriched_data.attributes.contains("upstream_services"), true)
  assert_eq(enriched_data.attributes.contains("downstream_services"), true)
  
  // 验证原始数据保持不变
  assert_eq(enriched_data.trace_id, "trace-enrich-123")
  assert_eq(enriched_data.span_id, "span-enrich-456")
  assert_eq(enriched_data.service_name, "api-service")
  assert_eq(enriched_data.operation_name, "get_user_profile")
  assert_eq(enriched_data.duration, 75)
  assert_eq(enriched_data.status, "success")
  assert_eq(enriched_data.attributes.get("user_id").unwrap(), "user789")
  assert_eq(enriched_data.attributes.get("request_id").unwrap(), "req-456")
}

test "telemetry_data_sampling_transformation" {
  // 测试遥测数据采样转换
  
  // 创建大量遥测数据
  let mut trace_data = []
  for i = 0; i < 1000; i = i + 1 {
    let trace = azimuth::telemetry::api::trace::TraceData::new()
      .with_trace_id("trace-" + i.to_string())
      .with_span_id("span-" + i.to_string())
      .with_service_name("high-traffic-service")
      .with_operation_name("process_request")
      .with_duration(50 + (i % 200))  // 50-249ms
      .with_status(if i % 10 == 0 { "error" } else { "success" })
    trace_data = trace_data.push(trace)
  }
  
  // 创建采样转换器
  let sampling_transformer = azimuth::telemetry::api::transform::SamplingTransformer::new()
  
  // 概率采样（10%）
  let prob_sampled = sampling_transformer.probability_sampling(trace_data, 0.1)
  assert_eq(prob_sampled.length() > 80, true)  // 大约100个，允许一些误差
  assert_eq(prob_sampled.length() < 120, true)
  
  // 验证错误追踪的优先采样
  let error_count_in_original = trace_data.filter(|t| t.status == "error").length()
  let error_count_in_sampled = prob_sampled.filter(|t| t.status == "error").length()
  
  // 错误追踪应该有更高的采样概率
  let error_sampling_rate = error_count_in_sampled.to_float() / error_count_in_original.to_float()
  let overall_sampling_rate = prob_sampled.length().to_float() / trace_data.length().to_float()
  assert_eq(error_sampling_rate > overall_sampling_rate, true)
  
  // 基于持续时间的采样（保留慢请求）
  let duration_sampled = sampling_transformer.duration_sampling(trace_data, 100)  // 保留超过100ms的请求
  let expected_duration_count = trace_data.filter(|t| t.duration > 100).length()
  assert_eq(duration_sampled.length(), expected_duration_count)
  
  // 验证所有采样数据的持续时间都超过阈值
  assert_eq(duration_sampled.all(|t| t.duration > 100), true)
  
  // 基于属性的采样（保留特定用户的追踪）
  let attribute_sampled = sampling_transformer.attribute_sampling(trace_data, "user_id", "premium_user")
  // 由于我们的测试数据没有user_id属性，这应该返回空列表
  assert_eq(attribute_sampled.length(), 0)
  
  // 添加一些带特定属性的追踪
  let mut trace_data_with_premium = trace_data
  for i = 0; i < 50; i = i + 1 {
    let premium_trace = azimuth::telemetry::api::trace::TraceData::new()
      .with_trace_id("premium-trace-" + i.to_string())
      .with_service_name("premium-service")
      .with_attributes({"user_type": "premium_user"})
    trace_data_with_premium = trace_data_with_premium.push(premium_trace)
  }
  
  let premium_sampled = sampling_transformer.attribute_sampling(trace_data_with_premium, "user_type", "premium_user")
  assert_eq(premium_sampled.length(), 50)  // 所有高级用户追踪都应该被保留
}