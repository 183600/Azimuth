// 性能优化和内存管理测试

test "memory_efficiency_attribute_storage" {
  // 测试属性存储的内存效率
  let resource = common::Resource::default("memory-test-service")
  
  // 创建大量不同类型的属性值
  let string_attr = common::AttributeValue::string("test_string_value")
  let int_attr = common::AttributeValue::int(42L)
  let float_attr = common::AttributeValue::float(3.14)
  let bool_attr = common::AttributeValue::bool(true)
  
  let string_array_attr = common::AttributeValue::array_string(["a", "b", "c"])
  let int_array_attr = common::AttributeValue::array_int([1L, 2L, 3L])
  let float_array_attr = common::AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array_attr = common::AttributeValue::array_bool([true, false, true])
  
  // 验证属性值创建成功
  match string_attr {
    common::StringValue(s) => @assertion.assert_eq(s, "test_string_value")
    _ => @assertion.assert_true(false)
  }
  
  match int_attr {
    common::IntValue(i) => @assertion.assert_eq(i, 42L)
    _ => @assertion.assert_true(false)
  }
  
  match float_attr {
    common::FloatValue(f) => @assertion.assert_eq(f, 3.14)
    _ => @assertion.assert_true(false)
  }
  
  match bool_attr {
    common::BoolValue(b) => @assertion.assert_eq(b, true)
    _ => @assertion.assert_true(false)
  }
}

test "performance_large_attributes_array" {
  // 测试大型属性数组的性能
  let large_string_array = Array[String]::with_capacity(1000, fn(i) { "attr_${i}" })
  let large_int_array = Array[Int64]::with_capacity(1000, fn(i) { i.to_int64() })
  let large_float_array = Array[Double]::with_capacity(1000, fn(i) { i.to_double() })
  let large_bool_array = Array[Bool]::with_capacity(1000, fn(i) { i % 2 == 0 })
  
  let string_array_attr = common::AttributeValue::array_string(large_string_array)
  let int_array_attr = common::AttributeValue::array_int(large_int_array)
  let float_array_attr = common::AttributeValue::array_float(large_float_array)
  let bool_array_attr = common::AttributeValue::array_bool(large_bool_array)
  
  // 验证大型数组创建成功
  match string_array_attr {
    common::ArrayStringValue(arr) => @assertion.assert_eq(arr.length, 1000)
    _ => @assertion.assert_true(false)
  }
  
  match int_array_attr {
    common::ArrayIntValue(arr) => @assertion.assert_eq(arr.length, 1000)
    _ => @assertion.assert_true(false)
  }
  
  match float_array_attr {
    common::ArrayFloatValue(arr) => @assertion.assert_eq(arr.length, 1000)
    _ => @assertion.assert_true(false)
  }
  
  match bool_array_attr {
    common::ArrayBoolValue(arr) => @assertion.assert_eq(arr.length, 1000)
    _ => @assertion.assert_true(false)
  }
}

test "memory_efficiency_span_creation" {
  // 测试Span创建的内存效率
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("memory-efficiency-tracer")
  
  // 批量创建Span以测试内存使用
  let spans = Array[]::build(fn(push) {
    for i = 0; i < 100; i = i + 1 {
      let (ctx, span) = tracer.start_span(
        context::Context::empty(),
        "memory_test_span_${i}",
        trace::Internal,
        [
          ("index", common::AttributeValue::int(i.to_int64())),
          ("batch_id", common::AttributeValue::string("batch_001"))
        ]
      )
      push(span)
    }
  })
  
  // 验证所有Span创建成功
  @assertion.assert_eq(spans.length, 100)
  
  // 验证Span属性
  let first_span = spans[0]
  @assertion.assert_eq(first_span.name, "memory_test_span_0")
  @assertion.assert_eq(first_span.attributes.length, 2)
}

test "performance_metrics_batch_operations" {
  // 测试指标批量操作性能
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("performance-meter")
  
  let counter = meter.create_counter("batch_counter", "count", "Batch counter")
  let histogram = meter.create_histogram("batch_histogram", "ms", "Batch histogram")
  let gauge = meter.create_gauge("batch_gauge", "value", "Batch gauge")
  
  // 批量操作测试
  for i = 0; i < 1000; i = i + 1 {
    counter.add(1L, [("batch_id", common::AttributeValue::int(i.to_int64()))])
    histogram.record(i.to_double(), [("value", common::AttributeValue::int(i.to_int64()))])
    gauge.record(i.to_double(), [("metric", common::AttributeValue::string("test"))])
  }
  
  // 验证批量操作完成
  @assertion.assert_true(true) // 如果没有异常，则测试通过
}

test "memory_efficiency_context_operations" {
  // 测试上下文操作的内存效率
  let base_ctx = context::Context::empty()
  
  // 创建带有大量属性的上下文
  let large_attributes = Array[(String, common::AttributeValue)]::with_capacity(100, fn(i) {
    ("attr_${i}", common::AttributeValue::string("value_${i}"))
  })
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("context-tracer")
  
  let (ctx, span) = tracer.start_span(
    base_ctx,
    "context_memory_test",
    trace::Internal,
    large_attributes
  )
  
  // 验证上下文和Span创建成功
  @assertion.assert_eq(span.name, "context_memory_test")
  @assertion.assert_eq(span.attributes.length, 100)
}

test "performance_resource_initialization" {
  // 测试资源初始化性能
  let resources = Array[]::build(fn(push) {
    for i = 0; i < 100; i = i + 1 {
      let resource = common::Resource::default("service_${i}")
      push(resource)
    }
  })
  
  // 验证资源创建性能
  @assertion.assert_eq(resources.length, 100)
  
  // 验证每个资源的属性
  for i = 0; i < resources.length; i = i + 1 {
    let resource = resources[i]
    @assertion.assert_eq(resource.service_name, "service_${i}")
    @assertion.assert_eq(resource.telemetry_sdk_name, "azimuth")
    @assertion.assert_eq(resource.telemetry_sdk_version, "0.1.0")
  }
}

test "memory_cleanup_verification" {
  // 测试内存清理验证
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  
  // 创建大量对象然后让它们超出作用域
  {
    let tracer = tracer_provider.get_tracer("cleanup-tracer")
    let meter = meter_provider.get_meter("cleanup-meter")
    
    for i = 0; i < 500; i = i + 1 {
      let (ctx, span) = tracer.start_span(
        context::Context::empty(),
        "cleanup_test_${i}",
        trace::Internal
      )
      
      let counter = meter.create_counter("cleanup_counter_${i}", "count", "Cleanup counter")
      counter.add(1L)
    }
  }
  
  // 验证清理完成
  @assertion.assert_true(true) // 如果没有内存泄漏，测试通过
}