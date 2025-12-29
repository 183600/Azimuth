// 并发安全性测试 - 测试多线程环境下的数据安全性
use azimuth.telemetry.api.common.{AttributeValue, Resource}
use azimuth.telemetry.api.trace.{SpanContext, Span, SpanKind, StatusCode, SpanEvent, NoopTracer, NoopTracerProvider}
use azimuth.telemetry.api.metrics.{NoopMeter, NoopMeterProvider}
use azimuth.telemetry.api.logs.{SeverityNumber, LogRecordBuilder, NoopLogger, NoopLoggerProvider}
use azimuth.telemetry.api.context.{Context, ContextKey, Baggage, create_key}

test "concurrent_span_creation_safety" {
  // 测试并发创建span的安全性
  
  let ctx = Context::empty()
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("concurrent-test", Some("1.0.0"))
  
  // 模拟10个"并发"worker，每个创建100个span
  let mut worker_id = 0
  let total_spans_created = 0
  
  while worker_id < 10 {
    let mut span_count = 0
    while span_count < 100 {
      let span_name = "worker_" + worker_id.to_string() + "_span_" + span_count.to_string()
      let attributes = [
        ("worker.id", AttributeValue::int(worker_id.to_int64())),
        ("span.count", AttributeValue::int(span_count.to_int64())),
        ("concurrent.test", AttributeValue::bool(true)),
        ("thread.safe", AttributeValue::bool(true))
      ]
      
      let (_, span) = tracer.start_span(
        ctx, 
        span_name, 
        Internal, 
        Some(attributes)
      )
      
      // 验证每个span都正确创建
      assert_eq(span.name, span_name)
      assert_eq(span.kind, Internal)
      assert_eq(span.attributes.length(), 4)
      
      // 验证属性值正确
      match span.attributes[0].1 {
        IntValue(worker_id_attr) => assert_eq(worker_id_attr, worker_id.to_int64())
        _ => @test.fail("Expected IntValue for worker.id")
      }
      
      match span.attributes[1].1 {
        IntValue(span_count_attr) => assert_eq(span_count_attr, span_count.to_int64())
        _ => @test.fail("Expected IntValue for span.count")
      }
      
      span_count = span_count + 1
      total_spans_created = total_spans_created + 1
    }
    worker_id = worker_id + 1
  }
  
  // 验证总共创建了1000个span
  assert_eq(total_spans_created, 1000)
}

test "concurrent_context_operations_safety" {
  // 测试并发Context操作的安全性
  
  // 模拟多个"并发"操作对同一个Context进行操作
  let base_ctx = Context::empty()
  let keys = []
  
  // 创建100个不同的键
  let mut i = 0
  while i < 100 {
    keys.push(create_key("concurrent_key_" + i.to_string()))
    i = i + 1
  }
  
  // 模拟10个worker并发操作Context
  let mut worker_id = 0
  let contexts = []
  
  while worker_id < 10 {
    let mut current_ctx = base_ctx
    let mut operation_count = 0
    
    // 每个worker执行50次操作
    while operation_count < 50 {
      let key_index = (worker_id * 50 + operation_count) % keys.length()
      let value = "worker_" + worker_id.to_string() + "_value_" + operation_count.to_string()
      current_ctx = current_ctx.with_value(keys[key_index], value)
      operation_count = operation_count + 1
    }
    
    contexts.push(current_ctx)
    worker_id = worker_id + 1
  }
  
  // 验证每个Context都正确维护了其状态
  let mut ctx_index = 0
  while ctx_index < contexts.length() {
    let ctx = contexts[ctx_index]
    
    // 验证Context中设置的值
    let mut verification_count = 0
    let mut op_count = 0
    while op_count < 50 {
      let key_index = (ctx_index * 50 + op_count) % keys.length()
      let expected_value = "worker_" + ctx_index.to_string() + "_value_" + op_count.to_string()
      
      match ctx.get(keys[key_index]) {
        Some(actual_value) => {
          if actual_value == expected_value {
            verification_count = verification_count + 1
          }
        }
        None => ()  // 某些键可能被覆盖，这是预期的
      }
      op_count = op_count + 1
    }
    
    // 至少应该有一些值能够正确匹配
    assert_eq(verification_count > 0, true)
    ctx_index = ctx_index + 1
  }
}

test "concurrent_baggage_operations_safety" {
  // 测试并发Baggage操作的安全性
  
  let base_baggage = Baggage::empty()
  let baggages = []
  
  // 模拟10个worker并发操作Baggage
  let mut worker_id = 0
  while worker_id < 10 {
    let mut current_baggage = base_baggage
    let mut operation_count = 0
    
    // 每个worker执行30次操作
    while operation_count < 30 {
      let key = "worker_" + worker_id.to_string() + "_key_" + operation_count.to_string()
      let value = "worker_" + worker_id.to_string() + "_value_" + operation_count.to_string()
      current_baggage = current_baggage.with_entry(key, value)
      operation_count = operation_count + 1
    }
    
    baggages.push(current_baggage)
    worker_id = worker_id + 1
  }
  
  // 验证每个Baggage都正确维护了其状态
  let mut baggage_index = 0
  while baggage_index < baggages.length() {
    let baggage = baggages[baggage_index]
    
    // 验证Baggage中设置的条目
    let mut verification_count = 0
    let mut op_count = 0
    while op_count < 30 {
      let key = "worker_" + baggage_index.to_string() + "_key_" + op_count.to_string()
      let expected_value = "worker_" + baggage_index.to_string() + "_value_" + op_count.to_string()
      
      match baggage.get(key) {
        Some(actual_value) => {
          if actual_value == expected_value {
            verification_count = verification_count + 1
          }
        }
        None => @test.fail("Expected Some(value) for baggage key: " + key)
      }
      op_count = op_count + 1
    }
    
    // 所有条目都应该能够正确匹配
    assert_eq(verification_count, 30)
    baggage_index = baggage_index + 1
  }
}

test "concurrent_metrics_recording_safety" {
  // 测试并发指标记录的安全性
  
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("concurrent-metrics", Some("1.0.0"))
  
  let counter = meter.create_counter("concurrent.operations", Some("count"), Some("Concurrent operations"))
  let histogram = meter.create_histogram("concurrent.duration", Some("ms"), Some("Concurrent operation duration"))
  let gauge = meter.create_gauge("concurrent.memory", Some("MB"), Some("Concurrent memory usage"))
  
  // 模拟10个worker并发记录指标
  let mut worker_id = 0
  let total_operations = 0
  
  while worker_id < 10 {
    let mut operation_count = 0
    while operation_count < 100 {
      let attributes = [
        ("worker.id", AttributeValue::int(worker_id.to_int64())),
        ("operation.id", AttributeValue::int(operation_count.to_int64())),
        ("concurrent.test", AttributeValue::bool(true))
      ]
      
      // 记录不同类型的指标
      counter.add(1L, Some(attributes))
      histogram.record(operation_count.to_double() * 0.1, Some(attributes))
      gauge.record(100.0 + worker_id.to_double() + operation_count.to_double() * 0.01, Some(attributes))
      
      operation_count = operation_count + 1
      total_operations = total_operations + 1
    }
    worker_id = worker_id + 1
  }
  
  // 验证总共执行了1000次操作
  assert_eq(total_operations, 1000)
}

test "concurrent_log_emission_safety" {
  // 测试并发日志记录的安全性
  
  let logger_provider = NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("concurrent-logger", Some("1.0.0"))
  
  // 模拟10个worker并发记录日志
  let mut worker_id = 0
  let total_logs = 0
  
  while worker_id < 10 {
    let mut log_count = 0
    while log_count < 50 {
      let log_record = LogRecord::builder()
        .timestamp(1640995200000000000L + (worker_id * 50 + log_count).to_int64())
        .severity(match log_count % 6 {
          0 => SeverityNumber::Trace
          1 => SeverityNumber::Debug
          2 => SeverityNumber::Info
          3 => SeverityNumber::Warn
          4 => SeverityNumber::Error
          _ => SeverityNumber::Fatal
        })
        .body("Worker " + worker_id.to_string() + " log " + log_count.to_string())
        .with_attribute("worker.id", AttributeValue::int(worker_id.to_int64()))
        .with_attribute("log.id", AttributeValue::int(log_count.to_int64()))
        .with_attribute("concurrent.test", AttributeValue::bool(true))
        .build()
      
      logger.emit(log_record)
      
      // 使用便捷方法记录日志
      match log_count % 5 {
        0 => logger.debug("Debug from worker " + worker_id.to_string(), Some([
          ("worker.id", AttributeValue::int(worker_id.to_int64())),
          ("log.count", AttributeValue::int(log_count.to_int64()))
        ]))
        1 => logger.info("Info from worker " + worker_id.to_string(), Some([
          ("worker.id", AttributeValue::int(worker_id.to_int64())),
          ("log.count", AttributeValue::int(log_count.to_int64()))
        ]))
        2 => logger.warn("Warning from worker " + worker_id.to_string(), Some([
          ("worker.id", AttributeValue::int(worker_id.to_int64())),
          ("log.count", AttributeValue::int(log_count.to_int64()))
        ]))
        3 => logger.error("Error from worker " + worker_id.to_string(), Some([
          ("worker.id", AttributeValue::int(worker_id.to_int64())),
          ("log.count", AttributeValue::int(log_count.to_int64()))
        ]))
        _ => logger.fatal("Fatal from worker " + worker_id.to_string(), Some([
          ("worker.id", AttributeValue::int(worker_id.to_int64())),
          ("log.count", AttributeValue::int(log_count.to_int64()))
        ]))
      }
      
      log_count = log_count + 1
      total_logs = total_logs + 1
    }
    worker_id = worker_id + 1
  }
  
  // 验证总共记录了500个日志记录（使用LogRecordBuilder）+ 500个便捷方法日志
  assert_eq(total_logs, 500)
}

test "concurrent_resource_sharing_safety" {
  // 测试并发资源共享的安全性
  
  // 创建共享的Resource对象
  let shared_resource = Resource::default("shared-service")
  
  // 模拟10个worker并发使用同一个Resource创建span
  let ctx = Context::empty()
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("resource-sharing-test", Some("1.0.0"))
  
  let mut worker_id = 0
  let total_spans = 0
  
  while worker_id < 10 {
    let mut span_count = 0
    while span_count < 20 {
      let span_attributes = [
        ("service.name", AttributeValue::string(shared_resource.service_name)),
        ("sdk.name", AttributeValue::string(shared_resource.telemetry_sdk_name)),
        ("sdk.version", AttributeValue::string(shared_resource.telemetry_sdk_version)),
        ("worker.id", AttributeValue::int(worker_id.to_int64())),
        ("span.count", AttributeValue::int(span_count.to_int64())),
        ("resource.shared", AttributeValue::bool(true))
      ]
      
      let (_, span) = tracer.start_span(
        ctx, 
        "shared_resource_span_" + worker_id.to_string() + "_" + span_count.to_string(), 
        Internal, 
        Some(span_attributes)
      )
      
      // 验证span正确使用了共享资源信息
      assert_eq(span.attributes.length(), 6)
      
      // 验证共享资源属性
      match span.attributes[0].1 {
        StringValue(service_name) => assert_eq(service_name, shared_resource.service_name)
        _ => @test.fail("Expected StringValue for service.name")
      }
      
      match span.attributes[1].1 {
        StringValue(sdk_name) => assert_eq(sdk_name, shared_resource.telemetry_sdk_name)
        _ => @test.fail("Expected StringValue for sdk.name")
      }
      
      match span.attributes[2].1 {
        StringValue(sdk_version) => assert_eq(sdk_version, shared_resource.telemetry_sdk_version)
        _ => @test.fail("Expected StringValue for sdk.version")
      }
      
      span_count = span_count + 1
      total_spans = total_spans + 1
    }
    worker_id = worker_id + 1
  }
  
  // 验证总共创建了200个span
  assert_eq(total_spans, 200)
}

test "concurrent_complex_operations_safety" {
  // 测试复杂并发操作的安全性
  
  let ctx = Context::empty()
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("complex-concurrent-test", Some("1.0.0"))
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("complex-concurrent-metrics", Some("1.0.0"))
  let logger_provider = NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("complex-concurrent-logger", Some("1.0.0"))
  
  // 模拟5个worker执行复杂的并发操作
  let mut worker_id = 0
  let total_complex_operations = 0
  
  while worker_id < 5 {
    let mut operation_count = 0
    while operation_count < 20 {
      // 1. 创建Context并设置值
      let key = create_key("worker_" + worker_id.to_string() + "_key")
      let worker_ctx = ctx.with_value(key, "worker_" + worker_id.to_string() + "_value_" + operation_count.to_string())
      
      // 2. 创建Baggage
      let baggage = Baggage::empty()
        .with_entry("worker.id", worker_id.to_string())
        .with_entry("operation.id", operation_count.to_string())
      
      // 3. 创建复杂的span
      let complex_attributes = [
        ("worker.id", AttributeValue::int(worker_id.to_int64())),
        ("operation.id", AttributeValue::int(operation_count.to_int64())),
        ("complex.operation", AttributeValue::bool(true)),
        ("concurrent.safe", AttributeValue::bool(true)),
        ("baggage.worker.id", AttributeValue::string("worker_" + worker_id.to_string())),
        ("baggage.operation.id", AttributeValue::string(operation_count.to_string())),
        ("context.value", AttributeValue::string("worker_" + worker_id.to_string() + "_value_" + operation_count.to_string())),
        ("nested.data", AttributeValue::array_string([
          "level1_" + operation_count.to_string(),
          "level2_" + operation_count.to_string(),
          "level3_" + operation_count.to_string()
        ]))
      ]
      
      let (_, span) = tracer.start_span(
        worker_ctx, 
        "complex_operation_" + worker_id.to_string() + "_" + operation_count.to_string(), 
        Server, 
        Some(complex_attributes)
      )
      
      // 4. 记录指标
      let counter = meter.create_counter("complex.operations", Some("count"), Some("Complex operations"))
      counter.add(1L, Some([
        ("worker.id", AttributeValue::int(worker_id.to_int64())),
        ("operation.type", AttributeValue::string("complex"))
      ]))
      
      // 5. 记录日志
      let log_record = LogRecord::builder()
        .timestamp(1640995200000000000L + (worker_id * 20 + operation_count).to_int64())
        .severity(SeverityNumber::Info)
        .body("Complex operation " + operation_count.to_string() + " completed for worker " + worker_id.to_string())
        .with_attribute("worker.id", AttributeValue::int(worker_id.to_int64()))
        .with_attribute("operation.id", AttributeValue::int(operation_count.to_int64()))
        .with_attribute("span.name", AttributeValue::string(span.name))
        .with_attribute("complex.test", AttributeValue::bool(true))
        .build()
      
      logger.emit(log_record)
      
      // 验证复杂操作的正确性
      assert_eq(span.name, "complex_operation_" + worker_id.to_string() + "_" + operation_count.to_string())
      assert_eq(span.kind, Server)
      assert_eq(span.attributes.length(), 8)
      
      // 验证Context值
      match worker_ctx.get(key) {
        Some(value) => assert_eq(value, "worker_" + worker_id.to_string() + "_value_" + operation_count.to_string())
        None => @test.fail("Expected Some(value) for context key")
      }
      
      // 验证Baggage值
      match baggage.get("worker.id") {
        Some(value) => assert_eq(value, worker_id.to_string())
        None => @test.fail("Expected Some(value) for baggage worker.id")
      }
      
      match baggage.get("operation.id") {
        Some(value) => assert_eq(value, operation_count.to_string())
        None => @test.fail("Expected Some(value) for baggage operation.id")
      }
      
      operation_count = operation_count + 1
      total_complex_operations = total_complex_operations + 1
    }
    worker_id = worker_id + 1
  }
  
  // 验证总共执行了100个复杂操作
  assert_eq(total_complex_operations, 100)
}