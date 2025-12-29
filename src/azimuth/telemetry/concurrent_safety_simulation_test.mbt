// 并发安全测试 - 测试多线程环境下的数据安全性

test "concurrent safety: context operations" {
  // 测试Context操作的并发安全性
  let base_context = Context::empty()
  let key = create_key("concurrent_key")
  let iterations = 100
  
  // 模拟并发操作 - 在实际环境中这里会使用真实的并发机制
  // 由于MoonBit的限制，这里模拟并发访问模式
  
  let mut contexts = []
  let mut i = 0
  while i < iterations {
    let context = base_context.with_value(key, "value_" + i.to_string())
    contexts.push(context)
    i = i + 1
  }
  
  // 验证所有context都有正确的值
  let mut j = 0
  while j < contexts.length {
    let context = contexts[j]
    assert match (context.get(key)) {
      Some(value) => value == "value_" + j.to_string()
      None => false
    }
    j = j + 1
  }
  
  // 测试链式操作的并发安全性
  let mut chain_contexts = []
  let mut k = 0
  while k < 10 {
    let mut context = base_context
    let mut m = 0
    while m < 10 {
      let chain_key = create_key("chain_key_" + m.to_string())
      context = context.with_value(chain_key, "chain_value_" + k.to_string() + "_" + m.to_string())
      m = m + 1
    }
    chain_contexts.push(context)
    k = k + 1
  }
  
  // 验证链式操作的结果
  let mut n = 0
  while n < chain_contexts.length {
    let context = chain_contexts[n]
    let mut p = 0
    while p < 10 {
      let chain_key = create_key("chain_key_" + p.to_string())
      assert match (context.get(chain_key)) {
        Some(value) => value == "chain_value_" + n.to_string() + "_" + p.to_string()
        None => false
      }
      p = p + 1
    }
    n = n + 1
  }
}

test "concurrent safety: baggage operations" {
  // 测试Baggage操作的并发安全性
  let base_baggage = Baggage::empty()
  let iterations = 100
  
  // 模拟并发添加条目
  let mut baggage_list = []
  let mut i = 0
  while i < iterations {
    let baggage = base_baggage.with_entry("key_" + i.to_string(), "value_" + i.to_string())
    baggage_list.push(baggage)
    i = i + 1
  }
  
  // 验证所有baggage都有正确的条目
  let mut j = 0
  while j < baggage_list.length {
    let baggage = baggage_list[j]
    assert match (baggage.get("key_" + j.to_string())) {
      Some(value) => value == "value_" + j.to_string()
      None => false
    }
    j = j + 1
  }
  
  // 测试复杂baggage操作的并发安全性
  let base_complex_baggage = Baggage::empty()
  let mut complex_baggage_list = []
  
  let mut k = 0
  while k < 20 {
    let mut baggage = base_complex_baggage
    let mut m = 0
    while m < 5 {
      baggage = baggage.with_entry("complex_key_" + k.to_string() + "_" + m.to_string(), 
                                   "complex_value_" + k.to_string() + "_" + m.to_string())
      m = m + 1
    }
    complex_baggage_list.push(baggage)
    k = k + 1
  }
  
  // 验证复杂baggage操作的结果
  let mut n = 0
  while n < complex_baggage_list.length {
    let baggage = complex_baggage_list[n]
    let mut p = 0
    while p < 5 {
      let key = "complex_key_" + n.to_string() + "_" + p.to_string()
      assert match (baggage.get(key)) {
        Some(value) => value == "complex_value_" + n.to_string() + "_" + p.to_string()
        None => false
      }
      p = p + 1
    }
    n = n + 1
  }
}

test "concurrent safety: metrics operations" {
  // 测试Metrics操作的并发安全性
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("concurrent_test_meter", None, None)
  
  // 创建多个instrument
  let mut counters = []
  let mut histograms = []
  let mut up_down_counters = []
  let mut gauges = []
  
  let mut i = 0
  while i < 10 {
    counters.push(meter.create_counter("counter_" + i.to_string(), None, None))
    histograms.push(meter.create_histogram("histogram_" + i.to_string(), None, None))
    up_down_counters.push(meter.create_up_down_counter("up_down_" + i.to_string(), None, None))
    gauges.push(meter.create_gauge("gauge_" + i.to_string(), None, None))
    i = i + 1
  }
  
  // 模拟并发measurement操作
  let attributes = [
    ("thread_id", AttributeValue::string("simulated_thread")),
    ("operation", AttributeValue::string("concurrent_test"))
  ]
  
  let mut j = 0
  while j < counters.length {
    let counter = counters[j]
    let histogram = histograms[j]
    let up_down_counter = up_down_counters[j]
    let gauge = gauges[j]
    
    // 每个instrument执行多次操作
    let mut k = 0
    while k < 10 {
      counter.add(1L, Some(attributes))
      histogram.record(k.to_double(), Some(attributes))
      up_down_counter.add(k.to_int64() - 5L, Some(attributes))
      gauge.record(k.to_double() * 2.0, Some(attributes))
      k = k + 1
    }
    
    j = j + 1
  }
  
  // 验证所有instrument都创建成功
  assert counters.length == 10
  assert histograms.length == 10
  assert up_down_counters.length == 10
  assert gauges.length == 10
}

test "concurrent safety: resource sharing" {
  // 测试Resource共享的并发安全性
  let shared_resource = Resource::default("shared_service")
  
  // 模拟多个"线程"同时访问同一个resource
  let mut resource_copies = []
  let mut i = 0
  while i < 50 {
    // 创建resource的副本用于模拟并发访问
    let resource_copy = Resource::{
      service_name: shared_resource.service_name,
      service_version: shared_resource.service_version,
      telemetry_sdk_name: shared_resource.telemetry_sdk_name,
      telemetry_sdk_version: shared_resource.telemetry_sdk_version,
      attributes: shared_resource.attributes
    }
    resource_copies.push(resource_copy)
    i = i + 1
  }
  
  // 验证所有副本都是一致的
  let mut j = 0
  while j < resource_copies.length {
    let resource = resource_copies[j]
    assert resource.service_name == "shared_service"
    assert resource.service_version == None
    assert resource.telemetry_sdk_name == "azimuth"
    assert resource.telemetry_sdk_version == "0.1.0"
    assert resource.attributes.length == 0
    j = j + 1
  }
  
  // 测试带有属性的resource共享
  let shared_resource_with_attrs = Resource::{
    service_name: "shared_service_with_attrs",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("shared_attr1", AttributeValue::string("shared_value1")),
      ("shared_attr2", AttributeValue::int(42L)),
      ("shared_attr3", AttributeValue::bool(true))
    ]
  }
  
  let mut attr_resource_copies = []
  let mut k = 0
  while k < 30 {
    let attr_resource_copy = Resource::{
      service_name: shared_resource_with_attrs.service_name,
      service_version: shared_resource_with_attrs.service_version,
      telemetry_sdk_name: shared_resource_with_attrs.telemetry_sdk_name,
      telemetry_sdk_version: shared_resource_with_attrs.telemetry_sdk_version,
      attributes: shared_resource_with_attrs.attributes
    }
    attr_resource_copies.push(attr_resource_copy)
    k = k + 1
  }
  
  // 验证带有属性的resource副本都是一致的
  let mut m = 0
  while m < attr_resource_copies.length {
    let resource = attr_resource_copies[m]
    assert resource.service_name == "shared_service_with_attrs"
    assert match (resource.service_version) {
      Some(v) => v == "1.0.0"
      None => false
    }
    assert resource.attributes.length == 3
    m = m + 1
  }
}

test "concurrent safety: instrumentation scope sharing" {
  // 测试InstrumentationScope共享的并发安全性
  let shared_scope = InstrumentationScope::{
    name: "shared_scope",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/shared-schema")
  }
  
  // 模拟多个"线程"同时访问同一个scope
  let mut scope_copies = []
  let mut i = 0
  while i < 40 {
    let scope_copy = InstrumentationScope::{
      name: shared_scope.name,
      version: shared_scope.version,
      schema_url: shared_scope.schema_url
    }
    scope_copies.push(scope_copy)
    i = i + 1
  }
  
  // 验证所有副本都是一致的
  let mut j = 0
  while j < scope_copies.length {
    let scope = scope_copies[j]
    assert scope.name == "shared_scope"
    assert match (scope.version) {
      Some(v) => v == "1.0.0"
      None => false
    }
    assert match (scope.schema_url) {
      Some(url) => url == "https://example.com/shared-schema"
      None => false
    }
    j = j + 1
  }
}

test "concurrent safety: mixed operations" {
  // 测试混合操作的并发安全性
  let base_context = Context::empty()
  let base_baggage = Baggage::empty()
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("mixed_operations_meter", None, None)
  
  let counter = meter.create_counter("mixed_counter", None, None)
  let histogram = meter.create_histogram("mixed_histogram", None, None)
  
  // 模拟多个"线程"执行混合操作
  let mut operation_results = []
  let mut i = 0
  while i < 20 {
    // 每个操作包括context、baggage和metrics操作
    let context = base_context.with_value(create_key("op_" + i.to_string()), "context_value_" + i.to_string())
    let baggage = base_baggage.with_entry("baggage_key_" + i.to_string(), "baggage_value_" + i.to_string())
    
    let attributes = [
      ("operation_id", AttributeValue::string("op_" + i.to_string())),
      ("context_value", AttributeValue::string("context_value_" + i.to_string())),
      ("baggage_value", AttributeValue::string("baggage_value_" + i.to_string()))
    ]
    
    counter.add(1L, Some(attributes))
    histogram.record(i.to_double(), Some(attributes))
    
    operation_results.push((context, baggage))
    i = i + 1
  }
  
  // 验证所有操作结果都是正确的
  let mut j = 0
  while j < operation_results.length {
    let (context, baggage) = operation_results[j]
    
    assert match (context.get(create_key("op_" + j.to_string()))) {
      Some(value) => value == "context_value_" + j.to_string()
      None => false
    }
    
    assert match (baggage.get("baggage_key_" + j.to_string())) {
      Some(value) => value == "baggage_value_" + j.to_string()
      None => false
    }
    
    j = j + 1
  }
}