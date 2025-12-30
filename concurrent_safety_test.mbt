// 并发安全性测试
// 测试遥测系统在并发环境下的安全性

use azimuth.telemetry.api.trace
use azimuth.telemetry.api.context
use azimuth.telemetry.api.common
use @lib.concurrent

test "concurrent_span_creation" {
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("concurrent-tracer", "1.0.0")
  let base_ctx = context::Context::current()
  
  // 创建多个并发任务
  let tasks = [
    concurrent.spawn(fn() {
      for i = 0; i < 100; i = i + 1 {
        let (_, _) = tracer.start_span(
          base_ctx,
          "concurrent-span-1-" + i.to_string(),
          trace::Internal,
          [("thread", common::AttributeValue::string("thread-1"))]
        )
      }
      "thread-1-complete"
    }),
    
    concurrent.spawn(fn() {
      for i = 0; i < 100; i = i + 1 {
        let (_, _) = tracer.start_span(
          base_ctx,
          "concurrent-span-2-" + i.to_string(),
          trace::Server,
          [("thread", common::AttributeValue::string("thread-2"))]
        )
      }
      "thread-2-complete"
    }),
    
    concurrent.spawn(fn() {
      for i = 0; i < 100; i = i + 1 {
        let (_, _) = tracer.start_span(
          base_ctx,
          "concurrent-span-3-" + i.to_string(),
          trace::Client,
          [("thread", common::AttributeValue::string("thread-3"))]
        )
      }
      "thread-3-complete"
    })
  ]
  
  // 等待所有任务完成
  let results = concurrent.join_all(tasks)
  assert_eq(results.length(), 3)
  assert_eq(results[0], "thread-1-complete")
  assert_eq(results[1], "thread-2-complete")
  assert_eq(results[2], "thread-3-complete")
}

test "concurrent_attribute_operations" {
  // 测试并发属性操作
  let shared_attrs = concurrent.atomic_ref([])
  
  let tasks = [
    concurrent.spawn(fn() {
      for i = 0; i < 50; i = i + 1 {
        let current = shared_attrs.get()
        let new_attr = ("thread-1-" + i.to_string(), common::AttributeValue::int(i.to_int64()))
        shared_attrs.set(current.push(new_attr))
      }
      "attrs-1-complete"
    }),
    
    concurrent.spawn(fn() {
      for i = 0; i < 50; i = i + 1 {
        let current = shared_attrs.get()
        let new_attr = ("thread-2-" + i.to_string(), common::AttributeValue::string("value-" + i.to_string()))
        shared_attrs.set(current.push(new_attr))
      }
      "attrs-2-complete"
    })
  ]
  
  let results = concurrent.join_all(tasks)
  let final_attrs = shared_attrs.get()
  
  assert_eq(results.length(), 2)
  assert_eq(results[0], "attrs-1-complete")
  assert_eq(results[1], "attrs-2-complete")
  assert_eq(final_attrs.length(), 100)
}

test "concurrent_context_propagation" {
  // 测试并发上下文传播
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("context-test", "1.0.0")
  let root_ctx = context::Context::current()
  
  let contexts = concurrent.atomic_ref([root_ctx])
  
  let tasks = [
    concurrent.spawn(fn() {
      let current_ctxs = contexts.get()
      let parent_ctx = current_ctxs[current_ctxs.length() - 1]
      
      let (new_ctx, _) = tracer.start_span(
        parent_ctx,
        "context-propagation-1",
        trace::Internal
      )
      
      let updated_ctxs = contexts.get()
      contexts.set(updated_ctxs.push(new_ctx))
      "context-1-complete"
    }),
    
    concurrent.spawn(fn() {
      let current_ctxs = contexts.get()
      let parent_ctx = current_ctxs[current_ctxs.length() - 1]
      
      let (new_ctx, _) = tracer.start_span(
        parent_ctx,
        "context-propagation-2",
        trace::Server
      )
      
      let updated_ctxs = contexts.get()
      contexts.set(updated_ctxs.push(new_ctx))
      "context-2-complete"
    })
  ]
  
  let results = concurrent.join_all(tasks)
  let final_ctxs = contexts.get()
  
  assert_eq(results.length(), 2)
  assert_eq(results[0], "context-1-complete")
  assert_eq(results[1], "context-2-complete")
  assert_eq(final_ctxs.length(), 3)  // root + 2 new contexts
}

test "concurrent_resource_sharing" {
  // 测试并发资源共享
  let resource = common::Resource::default("shared-service")
  let resource_ref = concurrent.atomic_ref(resource)
  
  let tasks = [
    concurrent.spawn(fn() {
      let current_resource = resource_ref.get()
      let updated_resource = { current_resource |
        service_version: Some("1.0.0"),
        attributes: current_resource.attributes.push(("thread", common::AttributeValue::string("thread-1")))
      }
      resource_ref.set(updated_resource)
      "resource-1-complete"
    }),
    
    concurrent.spawn(fn() {
      let current_resource = resource_ref.get()
      let updated_resource = { current_resource |
        attributes: current_resource.attributes.push(("thread", common::AttributeValue::string("thread-2")))
      }
      resource_ref.set(updated_resource)
      "resource-2-complete"
    })
  ]
  
  let results = concurrent.join_all(tasks)
  let final_resource = resource_ref.get()
  
  assert_eq(results.length(), 2)
  assert_eq(results[0], "resource-1-complete")
  assert_eq(results[1], "resource-2-complete")
  assert_eq(final_resource.service_name, "shared-service")
  assert_eq(final_resource.attributes.length(), 2)
}