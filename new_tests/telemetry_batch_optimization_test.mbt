// Azimuth Telemetry - Batch Processing Optimization Test
// 测试遥测数据批处理优化功能

test "batch_span_processing_optimization" {
  // 测试Span批处理优化
  let spans = [
    trace::Span::{
      name: "operation_1",
      context: trace::SpanContext::{
        trace_id: [1_byte; 16],
        span_id: [1_byte; 8],
        trace_flags: 1_byte,
        trace_state: ""
      },
      kind: trace::Internal,
      parent_span_id: None,
      start_time_unix_nanos: 1000000L,
      end_time_unix_nanos: Some(2000000L),
      status: trace::Ok,
      status_description: None,
      attributes: [("operation.type", common::AttributeValue::string("batch_test"))],
      events: [],
      links: []
    },
    trace::Span::{
      name: "operation_2",
      context: trace::SpanContext::{
        trace_id: [2_byte; 16],
        span_id: [2_byte; 8],
        trace_flags: 1_byte,
        trace_state: ""
      },
      kind: trace::Internal,
      parent_span_id: None,
      start_time_unix_nanos: 3000000L,
      end_time_unix_nanos: Some(4000000L),
      status: trace::Ok,
      status_description: None,
      attributes: [("operation.type", common::AttributeValue::string("batch_test"))],
      events: [],
      links: []
    },
    trace::Span::{
      name: "operation_3",
      context: trace::SpanContext::{
        trace_id: [3_byte; 16],
        span_id: [3_byte; 8],
        trace_flags: 1_byte,
        trace_state: ""
      },
      kind: trace::Internal,
      parent_span_id: None,
      start_time_unix_nanos: 5000000L,
      end_time_unix_nanos: Some(6000000L),
      status: trace::Ok,
      status_description: None,
      attributes: [("operation.type", common::AttributeValue::string("batch_test"))],
      events: [],
      links: []
    }
  ]
  
  // 模拟批处理优化：按时间排序
  let sorted_spans = spans.sort_by(fn(a, b) {
    if a.start_time_unix_nanos < b.start_time_unix_nanos { -1 }
    else if a.start_time_unix_nanos > b.start_time_unix_nanos { 1 }
    else { 0 }
  })
  
  // 验证排序结果
  assert_eq(sorted_spans.length(), 3)
  assert_eq(sorted_spans[0].name, "operation_1")
  assert_eq(sorted_spans[1].name, "operation_2")
  assert_eq(sorted_spans[2].name, "operation_3")
  
  // 模拟批处理优化：属性去重
  let common_attributes = [("operation.type", common::AttributeValue::string("batch_test"))]
  let optimized_spans = sorted_spans.map(fn(span) {
    { span | attributes: common_attributes }
  })
  
  // 验证属性优化
  for span in optimized_spans {
    assert_eq(span.attributes.length(), 1)
    assert_eq(span.attributes[0].0, "operation.type")
  }
}

test "batch_metrics_aggregation_optimization" {
  // 测试Metrics批处理聚合优化
  let measurements = [
    metrics::Measurement::{ value: 10.0, attributes: [("service", common::AttributeValue::string("api"))] },
    metrics::Measurement::{ value: 15.0, attributes: [("service", common::AttributeValue::string("api"))] },
    metrics::Measurement::{ value: 20.0, attributes: [("service", common::AttributeValue::string("worker"))] },
    metrics::Measurement::{ value: 25.0, attributes: [("service", common::AttributeValue::string("api"))] },
    metrics::Measurement::{ value: 30.0, attributes: [("service", common::AttributeValue::string("worker"))] }
  ]
  
  // 按属性分组
  let grouped_measurements = measurements.reduce(@{}, fn(acc, measurement) {
    let service_key = match measurement.attributes.find(fn((key, _)) { key == "service" }) {
      Some((_, common::StringValue(service))) => service
      _ => "unknown"
    }
    
    let current_values = acc.get_or(service_key, [])
    { acc | service_key: current_values + [measurement.value] }
  })
  
  // 验证分组结果
  assert_eq(grouped_measurements.size(), 2)
  
  let api_values = grouped_measurements.get("api").unwrap()
  let worker_values = grouped_measurements.get("worker").unwrap()
  
  assert_eq(api_values.length(), 3)
  assert_eq(worker_values.length(), 2)
  
  // 计算聚合统计
  let api_sum = api_values.reduce(0.0, fn(acc, val) { acc + val })
  let api_avg = api_sum / api_values.length().to_double()
  let api_max = api_values.reduce(0.0, fn(acc, val) { if val > acc { val } else { acc } })
  
  assert_eq(api_sum, 50.0) // 10 + 15 + 25
  assert_eq(api_avg, 16.666666666666668) // 50 / 3
  assert_eq(api_max, 25.0)
}

test "batch_compression_optimization" {
  // 测试批处理数据压缩优化
  let large_attributes = [
    for i = 0; i < 100; i = i + 1;
    ("attribute_" + i.to_string(), common::AttributeValue::string("value_" + i.to_string()))
  ]
  
  // 模拟压缩前的数据大小（简化计算）
  let uncompressed_size = large_attributes.reduce(0, fn(acc, (key, value)) {
    match value {
      common::StringValue(str_val) => acc + key.length() + str_val.length()
      _ => acc
    }
  })
  
  // 模拟压缩优化：去除重复前缀
  let compressed_attributes = large_attributes.map(fn((key, value)) {
    match value {
      common::StringValue(str_val) =>
        if str_val.starts_with("value_") {
          ("attr_" + key.split("_")[1], common::AttributeValue::string(str_val.split("_")[1]))
        } else {
          (key, value)
        }
      _ => (key, value)
    }
  })
  
  // 计算压缩后的数据大小
  let compressed_size = compressed_attributes.reduce(0, fn(acc, (key, value)) {
    match value {
      common::StringValue(str_val) => acc + key.length() + str_val.length()
      _ => acc
    }
  })
  
  // 验证压缩效果
  assert_true(compressed_size < uncompressed_size)
  assert_eq(compressed_attributes.length(), 100)
  
  // 验证压缩后的数据仍然可以正确解析
  let sample_compressed = compressed_attributes[0]
  match sample_compressed {
    ("attr_0", common::AttributeValue::StringValue("0")) => assert_true(true)
    _ => assert_false(true, "Compression failed")
  }
}

test "batch_time_window_optimization" {
  // 测试时间窗口批处理优化
  let time_ordered_data = [
    (1000000L, "event_1"),
    (1500000L, "event_2"), 
    (2500000L, "event_3"),
    (3000000L, "event_4"),
    (4500000L, "event_5"),
    (5000000L, "event_6")
  ]
  
  // 定义时间窗口大小（1秒 = 1,000,000纳秒）
  let window_size = 2000000L
  
  // 按时间窗口分组
  let windows = time_ordered_data.reduce(@{}, fn(acc, (timestamp, event)) {
    let window_id = (timestamp / window_size).to_string()
    let current_events = acc.get_or(window_id, [])
    { acc | window_id: current_events + [(timestamp, event)] }
  })
  
  // 验证时间窗口分组
  assert_eq(windows.size(), 3) // 应该有3个时间窗口
  
  // 验证第一个窗口 (0-2秒)
  let window_0 = windows.get("0").unwrap()
  assert_eq(window_0.length(), 3) // event_1, event_2, event_3
  
  // 验证第二个窗口 (2-4秒)
  let window_1 = windows.get("1").unwrap()
  assert_eq(window_1.length(), 2) // event_4, event_5
  
  // 验证第三个窗口 (4-6秒)
  let window_2 = windows.get("2").unwrap()
  assert_eq(window_2.length(), 1) // event_6
  
  // 验证每个窗口内的时间顺序
  for window in windows.values() {
    let sorted_window = window.sort_by(fn(a, b) {
      if a.0 < b.0 { -1 } else if a.0 > b.0 { 1 } else { 0 }
    })
    assert_eq(window, sorted_window)
  }
}

test "batch_memory_optimization" {
  // 测试批处理内存优化
  let resource = common::Resource::default("test_service")
  
  // 创建大量相似的Span，模拟内存优化场景
  let base_attributes = [
    ("service.name", common::AttributeValue::string(resource.service_name)),
    ("service.version", common::AttributeValue::string(resource.service_version.unwrap_or("1.0.0"))),
    ("telemetry.sdk.name", common::AttributeValue::string(resource.telemetry_sdk_name)),
    ("telemetry.sdk.version", common::AttributeValue::string(resource.telemetry_sdk_version))
  ]
  
  let spans = [
    for i = 0; i < 50; i = i + 1;
    trace::Span::{
      name: "operation_" + i.to_string(),
      context: trace::SpanContext::{
        trace_id: [i.to_byte(); 16],
        span_id: [i.to_byte; 8],
        trace_flags: 1_byte,
        trace_state: ""
      },
      kind: trace::Internal,
      parent_span_id: None,
      start_time_unix_nanos: (i * 1000000).to_int64(),
      end_time_unix_nanos: Some(((i + 1) * 1000000).to_int64()),
      status: trace::Ok,
      status_description: None,
      attributes: base_attributes + [("operation.id", common::AttributeValue::int(i.to_int64()))],
      events: [],
      links: []
    }
  ]
  
  // 模拟内存优化：共享公共属性
  let shared_attributes = base_attributes
  let optimized_spans = spans.map(fn(span) {
    let unique_attrs = span.attributes.filter(fn((key, _)) { 
      !shared_attributes.any(fn((shared_key, _)) { shared_key == key })
    })
    { span | attributes: shared_attributes + unique_attrs }
  })
  
  // 验证优化结果
  assert_eq(optimized_spans.length(), 50)
  
  // 验证每个Span都有正确的唯一属性
  for i in 0..50 {
    let span = optimized_spans[i]
    assert_eq(span.name, "operation_" + i.to_string())
    
    let operation_id_attr = span.attributes.find(fn((key, _)) { key == "operation.id" })
    assert_some(operation_id_attr)
    
    match operation_id_attr.unwrap() {
      (_, common::IntValue(id)) => assert_eq(id, i.to_int64())
      _ => assert_false(true, "Expected IntValue")
    }
  }
  
  // 验证公共属性的一致性
  let first_span = optimized_spans[0]
  let common_attrs_count = first_span.attributes.filter(fn((key, _)) {
    shared_attributes.any(fn((shared_key, _)) { shared_key == key })
  }).length()
  
  assert_eq(common_attrs_count, 4) // 四个公共属性
}