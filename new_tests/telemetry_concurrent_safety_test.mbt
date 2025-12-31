// 并发安全测试用例

test "concurrent_span_creation_safety" {
  // 并发Span创建安全测试
  
  let tracer = azimuth::telemetry::api::trace::TracerProvider::global().get_tracer("concurrent-safe-tracer")
  let ctx = azimuth::telemetry::api::context::Context::empty()
  
  // 创建多个并发任务，每个创建多个Span
  let num_tasks = 20
  let spans_per_task = 100
  
  let task_group = azimuth::telemetry::sdk::platform::async::TaskGroup::new()
  let span_counter = azimuth::telemetry::sdk::platform::atomic::AtomicInt::new(0)
  
  for task_id = 0; task_id < num_tasks; task_id = task_id + 1 {
    task_group.spawn(async {
      for i = 0; i < spans_per_task; i = i + 1 {
        let (_, span) = tracer.start_span(
          ctx,
          "concurrent.safe.span." + task_id.to_string() + "." + i.to_string(),
          azimuth::telemetry::api::trace::SpanKind::Server
        )
        
        // 设置属性
        span.set_attribute("task_id", azimuth::telemetry::api::common::AttributeValue::Int64(task_id))
        span.set_attribute("iteration", azimuth::telemetry::api::common::AttributeValue::Int64(i))
        
        // 添加事件
        span.add_event(
          "test.event",
          azimuth::telemetry::api::common::Attributes::empty()
            .with("event_data", azimuth::telemetry::api::common::AttributeValue::String("test_data"))
        )
        
        // 结束Span
        span.end()
        
        // 原子递增计数器
        span_counter.fetch_add(1)
      }
    })
  }
  
  // 等待所有任务完成
  task_group.wait_all()
  
  // 验证所有Span都被创建
  let total_spans = span_counter.load()
  assert_eq(total_spans, num_tasks * spans_per_task)
}

test "concurrent_metric_recording_safety" {
  // 并发指标记录安全测试
  
  let meter = azimuth::telemetry::api::metrics::MeterProvider::global().get_meter("concurrent-safe-meter")
  
  let counter = meter.create_counter("concurrent.counter", "Concurrent counter", "operations")
  let histogram = meter.create_histogram("concurrent.histogram", "Concurrent histogram", "ms")
  
  let attrs = azimuth::telemetry::api::common::Attributes::empty()
    .with("operation", azimuth::telemetry::api::common::AttributeValue::String("concurrent_test"))
  
  // 创建多个并发任务记录指标
  let num_tasks = 15
  let records_per_task = 1000
  
  let task_group = azimuth::telemetry::sdk::platform::async::TaskGroup::new()
  let counter_total = azimuth::telemetry::sdk::platform::atomic::AtomicInt::new(0)
  
  for task_id = 0; task_id < num_tasks; task_id = task_id + 1 {
    task_group.spawn(async {
      for i = 0; i < records_per_task; i = i + 1 {
        // 记录计数器
        counter.add(1, attrs.with("task_id", azimuth::telemetry::api::common::AttributeValue::Int64(task_id)))
        
        // 记录直方图
        histogram.record(
          (task_id * 100 + i).to_float(),
          attrs.with("task_id", azimuth::telemetry::api::common::AttributeValue::Int64(task_id))
        )
        
        counter_total.fetch_add(1)
      }
    })
  }
  
  task_group.wait_all()
  
  // 验证所有记录都被处理
  let total_records = counter_total.load()
  assert_eq(total_records, num_tasks * records_per_task * 2)  // 每个任务记录counter和histogram
  
  // 验证最终聚合结果
  let final_counter_measurement = counter.collect(attrs)
  assert_eq(final_counter_measurement.value, num_tasks * records_per_task)
  
  let final_histogram_measurement = histogram.collect(attrs)
  assert_eq(final_histogram_measurement.count, num_tasks * records_per_task)
}

test "concurrent_logging_safety" {
  // 并发日志记录安全测试
  
  let logger = azimuth::telemetry::api::logs::LoggerProvider::global().get_logger("concurrent-safe-logger")
  
  let num_tasks = 10
  let logs_per_task = 500
  
  let task_group = azimuth::telemetry::sdk::platform::async::TaskGroup::new()
  let log_counter = azimuth::telemetry::sdk::platform::atomic::AtomicInt::new(0)
  
  for task_id = 0; task_id < num_tasks; task_id = task_id + 1 {
    task_group.spawn(async {
      for i = 0; i < logs_per_task; i = i + 1 {
        let attributes = azimuth::telemetry::api::common::Attributes::empty()
          .with("task_id", azimuth::telemetry::api::common::AttributeValue::Int64(task_id))
          .with("iteration", azimuth::telemetry::api::common::AttributeValue::Int64(i))
          .with("operation", azimuth::telemetry::api::common::AttributeValue::String("concurrent_logging"))
        
        let log_record = azimuth::telemetry::api::logs::LogRecord::new(
          timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
          observed_timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
          severity_number=azimuth::telemetry::api::logs::SeverityNumber::Info,
          severity_text="INFO",
          body="Concurrent log message from task " + task_id.to_string() + " iteration " + i.to_string(),
          attributes=attributes,
          trace_id=Some("4bf92f3577b34da6a3ce929d0e0e4736"),
          span_id=Some("00f067aa0ba902b7"),
          trace_flags=Some(1)
        )
        
        logger.emit(log_record)
        log_counter.fetch_add(1)
      }
    })
  }
  
  task_group.wait_all()
  
  // 验证所有日志都被记录
  let total_logs = log_counter.load()
  assert_eq(total_logs, num_tasks * logs_per_task)
}

test "concurrent_context_propagation_safety" {
  // 并发上下文传播安全测试
  
  let propagator = azimuth::telemetry::api::propagation::W3CTraceContextPropagator::new()
  
  let num_tasks = 12
  let operations_per_task = 200
  
  let task_group = azimuth::telemetry::sdk::platform::async::TaskGroup::new()
  let operation_counter = azimuth::telemetry::sdk::platform::atomic::AtomicInt::new(0)
  
  for task_id = 0; task_id < num_tasks; task_id = task_id + 1 {
    task_group.spawn(async {
      // 创建唯一的上下文
      let trace_id = "4bf92f3577b34da6a3ce929d0e0e" + task_id.to_string().pad_left(4, '0')
      let span_id = "00f067aa0ba90" + task_id.to_string().pad_left(4, '0')
      
      let span_context = azimuth::telemetry::api::trace::SpanContext::new(trace_id, span_id, true, false)
      let ctx = azimuth::telemetry::api::context::Context::empty()
        .with_value(azimuth::telemetry::api::context::create_key("span_context"), span_context)
        .with_value(azimuth::telemetry::api::context::create_key("task_id"), task_id.to_string())
      
      // 并发进行注入和提取操作
      for i = 0; i < operations_per_task; i = i + 1 {
        // 注入上下文
        let carrier = azimuth::telemetry::api::propagation::TextMapCarrier::new()
        propagator.inject(ctx, carrier)
        
        // 提取上下文
        let extracted_ctx = propagator.extract(carrier)
        
        // 验证提取的上下文
        let extracted_span_context = extracted_ctx.get(azimuth::telemetry::api::context::create_key("span_context"))
        assert_eq(extracted_span_context.is_some(), true)
        
        let extracted_task_id = extracted_ctx.get(azimuth::telemetry::api::context::create_key("task_id"))
        assert_eq(extracted_task_id.unwrap(), task_id.to_string())
        
        operation_counter.fetch_add(1)
      }
    })
  }
  
  task_group.wait_all()
  
  // 验证所有操作都完成
  let total_operations = operation_counter.load()
  assert_eq(total_operations, num_tasks * operations_per_task)
}

test "concurrent_attribute_operations_safety" {
  // 并发属性操作安全测试
  
  let num_tasks = 8
  let operations_per_task = 100
  
  let task_group = azimuth::telemetry::sdk::platform::async::TaskGroup::new()
  let operation_counter = azimuth::telemetry::sdk::platform::atomic::AtomicInt::new(0)
  
  // 创建共享的属性集合（如果支持）
  let shared_attrs = azimuth::telemetry::sdk::platform::concurrent::ConcurrentAttributes::new()
  
  for task_id = 0; task_id < num_tasks; task_id = task_id + 1 {
    task_group.spawn(async {
      for i = 0; i < operations_per_task; i = i + 1 {
        // 创建独立的属性集合（避免共享状态竞争）
        let attrs = azimuth::telemetry::api::common::Attributes::empty()
          .with("task_id", azimuth::telemetry::api::common::AttributeValue::Int64(task_id))
          .with("iteration", azimuth::telemetry::api::common::AttributeValue::Int64(i))
          .with("operation", azimuth::telemetry::api::common::AttributeValue::String("concurrent_test"))
          .with("timestamp", azimuth::telemetry::api::common::AttributeValue::Int64(
            azimuth::telemetry::sdk::platform::time::current_unix_nanos()
          ))
        
        // 测试属性查找
        let task_id_value = attrs.get("task_id")
        let iteration_value = attrs.get("iteration")
        let operation_value = attrs.get("operation")
        
        // 验证属性值
        assert_eq(task_id_value.unwrap().to_string(), task_id.to_string())
        assert_eq(iteration_value.unwrap().to_string(), i.to_string())
        assert_eq(operation_value.unwrap().to_string(), "concurrent_test")
        
        // 测试属性合并
        let additional_attrs = azimuth::telemetry::api::common::Attributes::empty()
          .with("additional_field", azimuth::telemetry::api::common::AttributeValue::String("additional_value"))
        
        let merged_attrs = attrs.merge(additional_attrs)
        assert_eq(merged_attrs.size(), 5)
        assert_eq(merged_attrs.get("additional_field").unwrap().to_string(), "additional_value")
        
        operation_counter.fetch_add(1)
      }
    })
  }
  
  task_group.wait_all()
  
  // 验证所有操作都完成
  let total_operations = operation_counter.load()
  assert_eq(total_operations, num_tasks * operations_per_task)
}

test "concurrent_sampling_safety" {
  // 并发采样安全测试
  
  let sampler = azimuth::telemetry::sdk::trace::TraceIdRatioBasedSampler::new(0.1)
  
  let num_tasks = 16
  let samples_per_task = 1000
  
  let task_group = azimuth::telemetry::sdk::platform::async::TaskGroup::new()
  let sample_counter = azimuth::telemetry::sdk::platform::atomic::AtomicInt::new(0)
  let sampled_counter = azimuth::telemetry::sdk::platform::atomic::AtomicInt::new(0)
  
  for task_id = 0; task_id < num_tasks; task_id = task_id + 1 {
    task_group.spawn(async {
      for i = 0; i < samples_per_task; i = i + 1 {
        let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736" + task_id.to_string() + i.to_string()
        let span_name = "concurrent.sampling.span"
        let span_kind = azimuth::telemetry::api::trace::SpanKind::Server
        let attributes = azimuth::telemetry::api::common::Attributes::empty()
          .with("task_id", azimuth::telemetry::api::common::AttributeValue::Int64(task_id))
        let parent_context = None
        
        let decision = sampler.should_sample(
          parent_context,
          trace_id,
          span_name,
          span_kind,
          attributes
        )
        
        sample_counter.fetch_add(1)
        
        if decision.is_sampled() {
          sampled_counter.fetch_add(1)
        }
      }
    })
  }
  
  task_group.wait_all()
  
  // 验证采样统计
  let total_samples = sample_counter.load()
  let total_sampled = sampled_counter.load()
  
  assert_eq(total_samples, num_tasks * samples_per_task)
  
  // 验证采样率大约为10%（允许一些误差）
  let actual_sampling_rate = total_sampled.to_float() / total_samples.to_float()
  assert_eq(actual_sampling_rate >= 0.05 and actual_sampling_rate <= 0.15, true)
}

test "concurrent_resource_operations_safety" {
  // 并发资源操作安全测试
  
  let num_tasks = 6
  let operations_per_task = 50
  
  let task_group = azimuth::telemetry::sdk::platform::async::TaskGroup::new()
  let operation_counter = azimuth::telemetry::sdk::platform::atomic::AtomicInt::new(0)
  
  for task_id = 0; task_id < num_tasks; task_id = task_id + 1 {
    task_group.spawn(async {
      for i = 0; i < operations_per_task; i = i + 1 {
        // 创建资源
        let resource = azimuth::telemetry::api::common::Resource::builder()
          .with("service.name", azimuth::telemetry::api::common::AttributeValue::String("service-" + task_id.to_string()))
          .with("service.instance", azimuth::telemetry::api::common::AttributeValue::String("instance-" + i.to_string()))
          .with("task_id", azimuth::telemetry::api::common::AttributeValue::Int64(task_id))
          .build()
        
        // 测试资源属性访问
        let service_name = resource.attributes.get("service.name")
        let service_instance = resource.attributes.get("service.instance")
        let task_id_attr = resource.attributes.get("task_id")
        
        assert_eq(service_name.unwrap().to_string(), "service-" + task_id.to_string())
        assert_eq(service_instance.unwrap().to_string(), "instance-" + i.to_string())
        assert_eq(task_id_attr.unwrap().to_string(), task_id.to_string())
        
        // 测试资源合并
        let additional_resource = azimuth::telemetry::api::common::Resource::builder()
          .with("environment", azimuth::telemetry::api::common::AttributeValue::String("test"))
          .build()
        
        let merged_resource = resource.merge(additional_resource)
        assert_eq(merged_resource.attributes.size(), 4)
        assert_eq(merged_resource.attributes.get("environment").unwrap().to_string(), "test")
        
        // 测试资源序列化
        let serialized = resource.to_json()
        assert_eq(serialized.length() > 0, true)
        
        let deserialized = azimuth::telemetry::api::common::Resource::from_json(serialized)
        assert_eq(deserialized.attributes.size(), 3)
        
        operation_counter.fetch_add(1)
      }
    })
  }
  
  task_group.wait_all()
  
  // 验证所有操作都完成
  let total_operations = operation_counter.load()
  assert_eq(total_operations, num_tasks * operations_per_task)
}

test "concurrent_exporter_safety" {
  // 并发导出器安全测试
  
  // 创建内存导出器用于测试
  let memory_exporter = azimuth::telemetry::sdk::exporter::InMemorySpanExporter::new()
  
  let tracer_provider = azimuth::telemetry::sdk::trace::SdkTracerProvider::builder()
    .with_span_processor(azimuth::telemetry::sdk::trace::BatchSpanProcessor::new(
      exporter=memory_exporter,
      max_queue_size=1000,
      scheduled_delay_millis=100,
      max_export_batch_size=50
    ))
    .build()
  
  let tracer = tracer_provider.get_tracer("concurrent-exporter-tracer")
  let ctx = azimuth::telemetry::api::context::Context::empty()
  
  let num_tasks = 10
  let spans_per_task = 100
  
  let task_group = azimuth::telemetry::sdk::platform::async::TaskGroup::new()
  let span_counter = azimuth::telemetry::sdk::platform::atomic::AtomicInt::new(0)
  
  for task_id = 0; task_id < num_tasks; task_id = task_id + 1 {
    task_group.spawn(async {
      for i = 0; i < spans_per_task; i = i + 1 {
        let (_, span) = tracer.start_span(
          ctx,
          "concurrent.exporter.span." + task_id.to_string() + "." + i.to_string(),
          azimuth::telemetry::api::trace::SpanKind::Server
        )
        
        span.set_attribute("task_id", azimuth::telemetry::api::common::AttributeValue::Int64(task_id))
        span.end()
        
        span_counter.fetch_add(1)
      }
    })
  }
  
  task_group.wait_all()
  
  // 等待导出完成
  azimuth::telemetry::sdk::platform::time::sleep(1000)  // 1秒
  
  // 强制刷新
  tracer_provider.force_flush()
  
  // 验证导出的Span数量
  let exported_spans = memory_exporter.get_exported_spans()
  assert_eq(exported_spans.length(), num_tasks * spans_per_task)
  
  // 验证所有Span都有正确的属性
  for span in exported_spans {
    assert_eq(span.attributes.get("task_id").is_some(), true)
  }
  
  // 关闭提供者
  tracer_provider.shutdown()
}

test "race_condition_detection" {
  // 竞态条件检测测试
  
  let shared_counter = azimuth::telemetry::sdk::platform::atomic::AtomicInt::new(0)
  let num_tasks = 20
  let increments_per_task = 1000
  
  let task_group = azimuth::telemetry::sdk::platform::async::TaskGroup::new()
  
  for task_id = 0; task_id < num_tasks; task_id = task_id + 1 {
    task_group.spawn(async {
      for i = 0; i < increments_per_task; i = i + 1 {
        // 使用原子操作避免竞态条件
        shared_counter.fetch_add(1)
        
        // 模拟一些工作
        azimuth::telemetry::sdk::platform::time::sleep(1)  // 1微秒
      }
    })
  }
  
  task_group.wait_all()
  
  // 验证没有竞态条件（最终值应该是准确的）
  let final_value = shared_counter.load()
  let expected_value = num_tasks * increments_per_task
  
  assert_eq(final_value, expected_value)
}

test "deadlock_prevention" {
  // 死锁预防测试
  
  let resource1 = azimuth::telemetry::sdk::platform::concurrent::Mutex::new("resource1")
  let resource2 = azimuth::telemetry::sdk::platform::concurrent::Mutex::new("resource2")
  
  let num_tasks = 10
  let task_group = azimuth::telemetry::sdk::platform::async::TaskGroup::new()
  
  for task_id = 0; task_id < num_tasks; task_id = task_id + 1 {
    task_group.spawn(async {
      if task_id % 2 == 0 {
        // 偶数任务：先获取resource1，再获取resource2
        let guard1 = resource1.lock()
        azimuth::telemetry::sdk::platform::time::sleep(10)  // 10微秒
        let guard2 = resource2.lock()
        
        // 执行操作
        assert_eq(*guard1, "resource1")
        assert_eq(*guard2, "resource2")
      } else {
        // 奇数任务：先获取resource2，再获取resource1
        let guard1 = resource2.lock()
        azimuth::telemetry::sdk::platform::time::sleep(10)  // 10微秒
        let guard2 = resource1.lock()
        
        // 执行操作
        assert_eq(*guard1, "resource2")
        assert_eq(*guard2, "resource1")
      }
    })
  }
  
  // 设置超时以检测死锁
  let timeout_task = azimuth::telemetry::sdk::platform::async::spawn(async {
    azimuth::telemetry::sdk::platform::time::sleep(5000)  // 5秒超时
    panic("Deadlock detected or operations taking too long")
  })
  
  // 等待所有任务完成
  task_group.wait_all()
  
  // 取消超时任务
  timeout_task.cancel()
  
  // 如果到达这里，说明没有死锁
  assert_eq(true, true)
}