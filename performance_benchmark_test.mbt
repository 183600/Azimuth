// 性能基准测试
// 测试遥测系统的性能特征

use azimuth.telemetry.api.trace
use azimuth.telemetry.api.context
use azimuth.telemetry.api.common

test "span_creation_performance" {
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("perf-tracer", "1.0.0")
  let ctx = context::Context::current()
  
  // 测试大量span创建的性能
  let start_time = @sys.monotonic_time()
  
  for i = 0; i < 1000; i = i + 1 {
    let (_, _) = tracer.start_span(
      ctx, 
      "perf-span-" + i.to_string(), 
      trace::Internal,
      [("iteration", common::AttributeValue::int(i.to_int64()))]
    )
  }
  
  let end_time = @sys.monotonic_time()
  let duration_ms = (end_time - start_time) / 1000000L
  
  // 验证性能在合理范围内（1000个span创建应该在100ms内完成）
  assert_eq(duration_ms < 100L, true, "Span creation should be fast")
}

test "attribute_operations_performance" {
  // 创建大量属性进行性能测试
  let start_time = @sys.monotonic_time()
  
  let mut attributes : Array[(String, common::AttributeValue)] = []
  for i = 0; i < 1000; i = i + 1 {
    attributes = attributes.push(("attr-" + i.to_string(), common::AttributeValue::int(i.to_int64())))
  }
  
  let end_time = @sys.monotonic_time()
  let duration_ms = (end_time - start_time) / 1000000L
  
  // 验证属性操作性能
  assert_eq(attributes.length(), 1000)
  assert_eq(duration_ms < 50L, true, "Attribute operations should be efficient")
}

test "context_switching_performance" {
  let base_ctx = context::Context::current()
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("perf-tracer", "1.0.0")
  
  let start_time = @sys.monotonic_time()
  
  // 测试上下文切换性能
  let mut current_ctx = base_ctx
  for i = 0; i < 100; i = i + 1 {
    let (new_ctx, _) = tracer.start_span(
      current_ctx,
      "context-switch-" + i.to_string(),
      trace::Internal
    )
    current_ctx = new_ctx
  }
  
  let end_time = @sys.monotonic_time()
  let duration_ms = (end_time - start_time) / 1000000L
  
  // 验证上下文切换性能
  assert_eq(duration_ms < 20L, true, "Context switching should be efficient")
}

test "memory_allocation_pattern" {
  // 测试内存分配模式
  let initial_memory = @sys.get_memory_usage()
  
  // 创建大量临时对象
  for i = 0; i < 100; i = i + 1 {
    let resource = common::Resource::default("service-" + i.to_string())
    let _ = resource.service_name.length()
    
    let attributes = [
      ("test1", common::AttributeValue::string("value1")),
      ("test2", common::AttributeValue::int(i.to_int64())),
      ("test3", common::AttributeValue::bool(i % 2 == 0))
    ]
    let _ = attributes.length()
  }
  
  // 强制垃圾回收（如果可能）
  @sys.gc_collect()
  
  let final_memory = @sys.get_memory_usage()
  let memory_increase = final_memory - initial_memory
  
  // 验证内存增长在合理范围内
  assert_eq(memory_increase < 1000000L, true, "Memory usage should be reasonable")
}