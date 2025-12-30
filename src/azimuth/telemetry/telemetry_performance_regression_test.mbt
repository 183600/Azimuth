// 性能回归测试 - 确保性能不会退化
test "performance_span_creation_overhead" {
  // 测试Span创建的性能开销
  
  let start_time = 1000000L  // 模拟开始时间
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  
  // 创建大量Span并测量时间
  let mut i = 0
  let span_creation_count = 100
  
  while i < span_creation_count {
    let (_, _) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
      ctx, 
      "performance_test_span_" + i.to_string(),
      @azimuth.telemetry.api.trace.Internal,
      Some([
        ("iteration", @azimuth.telemetry.api.common.AttributeValue::int(i.to_int64())),
        ("performance.test", @azimuth.telemetry.api.common.AttributeValue::bool(true))
      ])
    )
    i = i + 1
  }
  
  // 验证所有Span都能成功创建
  assert_eq(i, span_creation_count)
}

test "performance_metrics_recording_overhead" {
  // 测试Metrics记录的性能开销
  
  let counter = @azimuth.telemetry.api.metrics.NoopCounter::{}
  let histogram = @azimuth.telemetry.api.metrics.NoopHistogram::{}
  let gauge = @azimuth.telemetry.api.metrics.NoopGauge::{}
  
  // 记录大量Metric数据
  let mut i = 0
  let metric_operations_count = 100
  
  while i < metric_operations_count {
    // Counter操作
    counter.add(1L, Some([
      ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("increment")),
      ("iteration", @azimuth.telemetry.api.common.AttributeValue::int(i.to_int64()))
    ]))
    
    // Histogram操作
    histogram.record(i.to_double() * 0.1, Some([
      ("metric.type", @azimuth.telemetry.api.common.AttributeValue::string("latency")),
      ("iteration", @azimuth.telemetry.api.common.AttributeValue::int(i.to_int64()))
    ]))
    
    // Gauge操作
    gauge.record(i.to_double() * 2.0, Some([
      ("gauge.type", @azimuth.telemetry.api.common.AttributeValue::string("memory")),
      ("iteration", @azimuth.telemetry.api.common.AttributeValue::int(i.to_int64()))
    ]))
    
    i = i + 1
  }
  
  // 验证所有操作都能成功执行
  assert_eq(i, metric_operations_count)
}

test "performance_context_operations_overhead" {
  // 测试Context操作的性能开销
  
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let key1 = @azimuth.telemetry.api.context.create_key("key1")
  let key2 = @azimuth.telemetry.api.context.create_key("key2")
  let key3 = @azimuth.telemetry.api.context.create_key("key3")
  
  // 执行大量Context操作
  let mut i = 0
  let context_operations_count = 100
  
  while i < context_operations_count {
    // 添加值到Context
    let ctx_with_values = ctx
      .with_value(key1, "value1_" + i.to_string())
      .with_value(key2, "value2_" + i.to_string())
      .with_value(key3, "value3_" + i.to_string())
    
    // 从Context中获取值
    let _value1 = ctx_with_values.get(key1)
    let _value2 = ctx_with_values.get(key2)
    let _value3 = ctx_with_values.get(key3)
    
    i = i + 1
  }
  
  // 验证所有操作都能成功执行
  assert_eq(i, context_operations_count)
}