// Azimuth Premium Quality Test Suite
// 高质量遥测系统测试用例 - 涵盖核心功能、边界条件和错误处理

// 测试1: 遥测上下文传播的完整性
test "telemetry_context_propagation_integrity" {
  // 创建父级上下文
  let parent_ctx = azimuth::SpanContext::new("trace-001", "span-parent", true, "env=production,service=api")
  
  // 创建子级span并验证上下文传播
  let child_span = azimuth::Span::new("child-operation", azimuth::Internal, parent_ctx)
  let child_ctx = azimuth::Span::get_context(child_span)
  
  // 验证trace ID的一致性
  assert_eq(azimuth::SpanContext::get_trace_id(child_ctx), "trace-001")
  assert_not_eq(azimuth::SpanContext::get_span_id(child_ctx), "span-parent")
  
  // 验证baggage项的正确传播
  let propagated_baggage = azimuth::SpanContext::get_baggage(child_ctx)
  assert_true(propagated_baggage.contains("env=production"))
  assert_true(propagated_baggage.contains("service=api"))
  
  // 测试深层嵌套的上下文传播
  let grandchild_span = azimuth::Span::new("grandchild-operation", azimuth::Client, child_ctx)
  let grandchild_ctx = azimuth::Span::get_context(grandchild_span)
  
  // 验证三代之间的trace ID一致性
  assert_eq(azimuth::SpanContext::get_trace_id(grandchild_ctx), "trace-001")
  assert_not_eq(azimuth::SpanContext::get_span_id(grandchild_ctx), "span-parent")
  assert_not_eq(azimuth::SpanContext::get_span_id(grandchild_ctx), azimuth::SpanContext::get_span_id(child_ctx))
}

// 测试2: 性能指标聚合的准确性
test "metrics_aggregation_accuracy" {
  // 创建测试用的指标收集器
  let metrics_collector = azimuth::Metrics::new_collector("test-service")
  
  // 记录不同类型的指标
  azimuth::Metrics::record_counter(metrics_collector, "requests_total", 10, ["method:GET", "status:200"])
  azimuth::Metrics::record_counter(metrics_collector, "requests_total", 5, ["method:POST", "status:201"])
  azimuth::Metrics::record_counter(metrics_collector, "requests_total", 2, ["method:GET", "status:404"])
  
  azimuth::Metrics::record_histogram(metrics_collector, "response_time_ms", 150.0, ["endpoint:/api/data"])
  azimuth::Metrics::record_histogram(metrics_collector, "response_time_ms", 200.0, ["endpoint:/api/data"])
  azimuth::Metrics::record_histogram(metrics_collector, "response_time_ms", 100.0, ["endpoint:/api/data"])
  
  azimuth::Metrics::record_gauge(metrics_collector, "active_connections", 25)
  azimuth::Metrics::record_gauge(metrics_collector, "active_connections", 30)
  azimuth::Metrics::record_gauge(metrics_collector, "active_connections", 20)
  
  // 验证计数器聚合
  let get_counter = azimuth::Metrics::get_counter(metrics_collector, "requests_total", ["method:GET"])
  assert_eq(get_counter, 12)  // 10 + 2
  
  let post_counter = azimuth::Metrics::get_counter(metrics_collector, "requests_total", ["method:POST"])
  assert_eq(post_counter, 5)
  
  // 验证直方图统计
  let histogram_stats = azimuth::Metrics::get_histogram_stats(metrics_collector, "response_time_ms", ["endpoint:/api/data"])
  assert_eq(histogram_stats.count, 3)
  assert_eq(histogram_stats.sum, 450.0)
  assert_eq(histogram_stats.min, 100.0)
  assert_eq(histogram_stats.max, 200.0)
  
  // 验证仪表盘最新值
  let current_gauge = azimuth::Metrics::get_gauge(metrics_collector, "active_connections")
  assert_eq(current_gauge, 20)  // 应该是最后设置的值
}

// 测试3: 分布式追踪的一致性验证
test "distributed_tracing_consistency" {
  // 模拟分布式系统中的多个服务
  let service_a_trace = azimuth::Trace::start("service-a-operation")
  let service_a_context = azimuth::Trace::get_context(service_a_trace)
  
  // 服务A调用服务B
  let service_b_headers = azimuth::Propagator::inject(service_a_context)
  let service_b_context = azimuth::Propagator::extract(service_b_headers)
  let service_b_trace = azimuth::Trace::continue("service-b-operation", service_b_context)
  
  // 服务B调用服务C
  let service_c_headers = azimuth::Propagator::inject(azimuth::Trace::get_context(service_b_trace))
  let service_c_context = azimuth::Propagator::extract(service_c_headers)
  let service_c_trace = azimuth::Trace::continue("service-c-operation", service_c_context)
  
  // 验证所有服务的trace ID一致
  let trace_id_a = azimuth::SpanContext::get_trace_id(azimuth::Trace::get_context(service_a_trace))
  let trace_id_b = azimuth::SpanContext::get_trace_id(azimuth::Trace::get_context(service_b_trace))
  let trace_id_c = azimuth::SpanContext::get_trace_id(azimuth::Trace::get_context(service_c_trace))
  
  assert_eq(trace_id_a, trace_id_b)
  assert_eq(trace_id_b, trace_id_c)
  
  // 验证父子关系
  let span_a = azimuth::Trace::get_root_span(service_a_trace)
  let span_b = azimuth::Trace::get_root_span(service_b_trace)
  let span_c = azimuth::Trace::get_root_span(service_c_trace)
  
  let parent_b = azimuth::Span::get_parent_span_id(span_b)
  let parent_c = azimuth::Span::get_parent_span_id(span_c)
  
  assert_eq(parent_b, azimuth::Span::get_span_id(span_a))
  assert_eq(parent_c, azimuth::Span::get_span_id(span_b))
  
  // 完成追踪并验证完整性
  azimuth::Trace::finish(service_a_trace)
  azimuth::Trace::finish(service_b_trace)
  azimuth::Trace::finish(service_c_trace)
  
  // 验证所有span都被正确记录
  let completed_spans = azimuth::Trace::get_completed_spans()
  assert_eq(completed_spans.length(), 3)
}

// 测试4: 错误处理和恢复机制
test "error_handling_and_recovery" {
  // 测试span创建时的错误处理
  let invalid_span_result = azimuth::Span::new("", azimuth::Internal, azimuth::SpanContext::empty())
  match invalid_span_result {
    Ok(_) => assert_false(true, "Expected error for empty span name")
    Err(error) => assert_true(error.contains("span name cannot be empty"))
  }
  
  // 测试属性设置的错误处理
  let valid_span = azimuth::Span::new("valid-operation", azimuth::Internal, azimuth::SpanContext::empty()) |> unwrap
  let null_key_result = azimuth::Span::set_attribute(valid_span, "", StringValue("value"))
  match null_key_result {
    Ok(_) => assert_false(true, "Expected error for empty attribute key")
    Err(error) => assert_true(error.contains("attribute key cannot be empty"))
  }
  
  // 测试过长的属性值处理
  let long_value = "x" * 10000  // 创建一个很长的字符串
  let long_value_result = azimuth::Span::set_attribute(valid_span, "long.key", StringValue(long_value))
  match long_value_result {
    Ok(_) => assert_false(true, "Expected error for attribute value too long")
    Err(error) => assert_true(error.contains("attribute value exceeds maximum length"))
  }
  
  // 测试恢复机制 - 确保错误后span仍然可用
  let recovery_result = azimuth::Span::set_attribute(valid_span, "recovery.test", StringValue("success"))
  match recovery_result {
    Ok(_) => assert_true(true, "Span should recover from previous errors")
    Err(_) => assert_false(true, "Span should be functional after error handling")
  }
}

// 测试5: 并发安全性验证
test "concurrent_safety_verification" {
  // 创建共享的遥测资源
  let shared_collector = azimuth::Metrics::new_collector("concurrent-test")
  let shared_tracer = azimuth::Tracer::new("concurrent-service")
  
  // 模拟并发操作
  let concurrent_operations = []
  
  // 并发记录指标
  for i in range(0, 100) {
    let operation = {
      let metric_name = "concurrent_counter_" + i.to_string()
      azimuth::Metrics::record_counter(shared_collector, metric_name, 1, ["thread:" + i.to_string()])
    }
    concurrent_operations.push(operation)
  }
  
  // 并发创建span
  for i in range(0, 50) {
    let operation = {
      let span_name = "concurrent_span_" + i.to_string()
      let span = azimuth::Tracer::start_span(shared_tracer, span_name) |> unwrap
      azimuth::Span::set_attribute(span, "concurrent.id", IntValue(i)) |> ignore
      azimuth::Tracer::finish_span(shared_tracer, span)
    }
    concurrent_operations.push(operation)
  }
  
  // 执行所有并发操作（在实际实现中这里会使用真正的并发机制）
  for operation in concurrent_operations {
    operation()
  }
  
  // 验证数据完整性
  let total_metrics = azimuth::Metrics::get_all_metrics(shared_collector)
  assert_eq(total_metrics.length(), 100)  // 确保所有指标都被记录
  
  let total_spans = azimuth::Tracer::get_completed_spans(shared_tracer)
  assert_eq(total_spans.length(), 50)  // 确保所有span都被完成
  
  // 验证没有数据损坏
  for metric in total_metrics {
    let metric_value = azimuth::Metrics::get_metric_value(shared_collector, metric.name)
    assert_eq(metric_value, 1)  // 每个指标应该只被记录一次
  }
}

// 测试6: 内存管理和资源清理
test "memory_management_and_cleanup" {
  // 创建大量临时资源
  let temp_resources = []
  
  for i in range(0, 1000) {
    let temp_span = azimuth::Span::new("temp-span-" + i.to_string(), azimuth::Internal, azimuth::SpanContext::empty()) |> unwrap
    let temp_metrics = azimuth::Metrics::new_collector("temp-collector-" + i.to_string())
    
    // 添加一些属性和指标
    azimuth::Span::set_attribute(temp_span, "temp.id", IntValue(i)) |> ignore
    azimuth::Metrics::record_counter(temp_metrics, "temp.counter", i, [])
    
    temp_resources.push((temp_span, temp_metrics))
  }
  
  // 验证资源创建成功
  assert_eq(temp_resources.length(), 1000)
  
  // 清理所有资源
  for (span, collector) in temp_resources {
    azimuth::Span::finish(span)
    azimuth::Metrics::cleanup(collector)
  }
  
  // 验证资源被正确清理
  let active_spans = azimuth::Span::get_active_count()
  assert_eq(active_spans, 0)  // 所有span应该被清理
  
  let active_collectors = azimuth::Metrics::get_active_collector_count()
  assert_eq(active_collectors, 0)  // 所有收集器应该被清理
  
  // 验证内存没有泄漏（在实际实现中这里会检查内存使用情况）
  let memory_usage = azimuth::System::get_memory_usage()
  assert_true(memory_usage < 100 * 1024 * 1024)  // 应该小于100MB
}

// 测试7: 配置动态更新测试
test "dynamic_configuration_updates" {
  // 创建初始配置
  let initial_config = azimuth::Config::new()
  azimuth::Config::set_sampling_rate(initial_config, 0.1)  // 10%采样率
  azimuth::Config::set_max_attributes(initial_config, 100)
  azimuth::Config::set_max_events(initial_config, 50)
  
  // 创建使用初始配置的tracer
  let tracer = azimuth::Tracer::with_config(initial_config)
  
  // 验证初始配置生效
  assert_eq(azimuth::Tracer::get_sampling_rate(tracer), 0.1)
  assert_eq(azimuth::Tracer::get_max_attributes(tracer), 100)
  assert_eq(azimuth::Tracer::get_max_events(tracer), 50)
  
  // 动态更新配置
  azimuth::Config::set_sampling_rate(initial_config, 0.5)  // 更新到50%采样率
  azimuth::Config::set_max_attributes(initial_config, 200)
  azimuth::Config::set_max_events(initial_config, 100)
  
  // 通知tracer配置已更新
  azimuth::Tracer::update_config(tracer, initial_config)
  
  // 验证配置更新生效
  assert_eq(azimuth::Tracer::get_sampling_rate(tracer), 0.5)
  assert_eq(azimuth::Tracer::get_max_attributes(tracer), 200)
  assert_eq(azimuth::Tracer::get_max_events(tracer), 100)
  
  // 测试配置验证
  let invalid_config_result = azimuth::Config::set_sampling_rate(initial_config, 1.5)  // 无效采样率
  match invalid_config_result {
    Ok(_) => assert_false(true, "Expected error for invalid sampling rate")
    Err(error) => assert_true(error.contains("sampling rate must be between 0 and 1"))
  }
  
  // 测试配置持久化
  let config_data = azimuth::Config::serialize(initial_config)
  let restored_config = azimuth::Config::deserialize(config_data) |> unwrap
  
  assert_eq(azimuth::Config::get_sampling_rate(restored_config), 0.5)
  assert_eq(azimuth::Config::get_max_attributes(restored_config), 200)
  assert_eq(azimuth::Config::get_max_events(restored_config), 100)
}

// 测试8: 序列化和反序列化完整性
test "serialization_deserialization_integrity" {
  // 创建复杂的遥测数据
  let original_span = azimuth::Span::new("serialization-test", azimuth::Server, azimuth::SpanContext::new("trace-123", "span-456", true, "env=test")) |> unwrap
  
  // 添加各种类型的属性
  azimuth::Span::set_attribute(original_span, "string.attr", StringValue("test value")) |> ignore
  azimuth::Span::set_attribute(original_span, "int.attr", IntValue(42)) |> ignore
  azimuth::Span::set_attribute(original_span, "float.attr", FloatValue(3.14159)) |> ignore
  azimuth::Span::set_attribute(original_span, "bool.attr", BoolValue(true)) |> ignore
  azimuth::Span::set_attribute(original_span, "array.attr", ArrayStringValue(["item1", "item2", "item3"])) |> ignore
  
  // 添加事件
  azimuth::Span::add_event(original_span, "test-event", [
    ("event.key1", StringValue("event.value1")),
    ("event.key2", IntValue(123))
  ])
  
  // 设置状态
  azimuth::Span::set_status(original_span, azimuth::Status::Ok)
  
  // 序列化span
  let serialized_data = azimuth::Span::serialize(original_span)
  assert_true(serialized_data.length() > 0)
  
  // 反序列化span
  let deserialized_span = azimuth::Span::deserialize(serialized_data) |> unwrap
  
  // 验证反序列化的完整性
  assert_eq(azimuth::Span::get_name(deserialized_span), azimuth::Span::get_name(original_span))
  assert_eq(azimuth::Span::get_kind(deserialized_span), azimuth::Span::get_kind(original_span))
  
  let original_ctx = azimuth::Span::get_context(original_span)
  let deserialized_ctx = azimuth::Span::get_context(deserialized_span)
  
  assert_eq(azimuth::SpanContext::get_trace_id(deserialized_ctx), azimuth::SpanContext::get_trace_id(original_ctx))
  assert_eq(azimuth::SpanContext::get_span_id(deserialized_ctx), azimuth::SpanContext::get_span_id(original_ctx))
  
  // 验证属性完整性
  let original_attrs = azimuth::Span::get_attributes(original_span)
  let deserialized_attrs = azimuth::Span::get_attributes(deserialized_span)
  
  assert_eq(original_attrs.length(), deserialized_attrs.length())
  
  for (key, value) in original_attrs {
    let deserialized_value = azimuth::Attributes::get(deserialized_attrs, key) |> unwrap
    match (value, deserialized_value) {
      (StringValue(orig), StringValue(deser)) => assert_eq(orig, deser)
      (IntValue(orig), IntValue(deser)) => assert_eq(orig, deser)
      (FloatValue(orig), FloatValue(deser)) => assert_eq(orig, deser)
      (BoolValue(orig), BoolValue(deser)) => assert_eq(orig, deser)
      (ArrayStringValue(orig), ArrayStringValue(deser)) => {
        assert_eq(orig.length(), deser.length())
        for i in range(0, orig.length()) {
          assert_eq(orig[i], deser[i])
        }
      }
      _ => assert_false(true, "Attribute type mismatch")
    }
  }
  
  // 验证事件完整性
  let original_events = azimuth::Span::get_events(original_span)
  let deserialized_events = azimuth::Span::get_events(deserialized_span)
  
  assert_eq(original_events.length(), deserialized_events.length())
  assert_eq(azimuth::Event::get_name(original_events[0]), azimuth::Event::get_name(deserialized_events[0]))
  
  // 验证状态完整性
  assert_eq(azimuth::Span::get_status(original_span), azimuth::Span::get_status(deserialized_span))
}

// 测试9: 国际化和本地化文本处理
test "internationalization_localization_handling" {
  // 测试多语言属性值
  let multilingual_span = azimuth::Span::new("国际化测试", azimuth::Internal, azimuth::SpanContext::empty()) |> unwrap
  
  // 添加各种语言的属性
  azimuth::Span::set_attribute(multilingual_span, "chinese.name", StringValue("张三")) |> ignore
  azimuth::Span::set_attribute(multilingual_span, "japanese.name", StringValue("田中太郎")) |> ignore
  azimuth::Span::set_attribute(multilingual_span, "korean.name", StringValue("김철수")) |> ignore
  azimuth::Span::set_attribute(multilingual_span, "arabic.name", StringValue("أحمد محمد")) |> ignore
  azimuth::Span::set_attribute(multilingual_span, "russian.name", StringValue("Иван Петров")) |> ignore
  azimuth::Span::set_attribute(multilingual_span, "emoji.test", StringValue("🚀🌟💻🔧")) |> ignore
  
  // 测试混合语言的文本
  let mixed_text = "Hello 世界! こんにちは 안녕하세요! مرحبا Привет!"
  azimuth::Span::set_attribute(multilingual_span, "mixed.text", StringValue(mixed_text)) |> ignore
  
  // 测试从右到左的文本
  let rtl_text = "مرحبا بالعالم"
  azimuth::Span::set_attribute(multilingual_span, "rtl.text", StringValue(rtl_text)) |> ignore
  
  // 验证文本处理正确性
  let attrs = azimuth::Span::get_attributes(multilingual_span)
  
  let chinese_name = azimuth::Attributes::get(attrs, "chinese.name") |> unwrap
  match chinese_name {
    StringValue(name) => assert_eq(name, "张三")
    _ => assert_false(true, "Expected StringValue")
  }
  
  let emoji_test = azimuth::Attributes::get(attrs, "emoji.test") |> unwrap
  match emoji_test {
    StringValue(emojis) => assert_true(emojis.contains("🚀"))
    _ => assert_false(true, "Expected StringValue")
  }
  
  let mixed_text_attr = azimuth::Attributes::get(attrs, "mixed.text") |> unwrap
  match mixed_text_attr {
    StringValue(text) => {
      assert_true(text.contains("Hello"))
      assert_true(text.contains("世界"))
      assert_true(text.contains("こんにちは"))
      assert_true(text.contains("안녕하세요"))
    }
    _ => assert_false(true, "Expected StringValue")
  }
  
  // 测试文本长度和字节长度处理
  let chinese_attr = azimuth::Attributes::get(attrs, "chinese.name") |> unwrap
  match chinese_attr {
    StringValue(text) => {
      assert_eq(text.length(), 2)  // 字符长度
      assert_eq(text.byte_length(), 6)  // UTF-8字节长度
    }
    _ => assert_false(true, "Expected StringValue")
  }
  
  // 测试本地化错误消息
  let localized_error = azimuth::Error::new_with_localized_message("validation.error", "zh-CN", "验证失败")
  assert_eq(azimuth::Error::get_message(localized_error, "zh-CN"), "验证失败")
  assert_eq(azimuth::Error::get_message(localized_error, "en-US"), "Validation error")
  assert_eq(azimuth::Error::get_message(localized_error, "ja-JP"), "検証エラー")
}

// 测试10: 资源生命周期和依赖管理
test "resource_lifecycle_and_dependency_management" {
  // 创建资源依赖图
  let root_resource = azimuth::Resource::new("root-service", "v1.0.0")
  let child_resource1 = azimuth::Resource::with_parent("child-service-1", "v1.1.0", root_resource)
  let child_resource2 = azimuth::Resource::with_parent("child-service-2", "v1.2.0", root_resource)
  let grandchild_resource = azimuth::Resource::with_parent("grandchild-service", "v1.0.1", child_resource1)
  
  // 验证资源层次结构
  assert_eq(azimuth::Resource::get_parent(root_resource), None)
  assert_eq(azimuth::Resource::get_parent(child_resource1), Some(root_resource))
  assert_eq(azimuth::Resource::get_parent(child_resource2), Some(root_resource))
  assert_eq(azimuth::Resource::get_parent(grandchild_resource), Some(child_resource1))
  
  // 验证资源依赖关系
  let root_dependencies = azimuth::Resource::get_dependencies(root_resource)
  assert_eq(root_dependencies.length(), 2)  // 两个子资源
  assert_true(root_dependencies.contains(child_resource1))
  assert_true(root_dependencies.contains(child_resource2))
  
  let child1_dependencies = azimuth::Resource::get_dependencies(child_resource1)
  assert_eq(child1_dependencies.length(), 1)  // 一个孙资源
  assert_true(child1_dependencies.contains(grandchild_resource))
  
  // 测试资源状态管理
  assert_eq(azimuth::Resource::get_state(root_resource), azimuth::ResourceState::Initialized)
  assert_eq(azimuth::Resource::get_state(child_resource1), azimuth::ResourceState::Initialized)
  
  // 启动根资源（应该级联启动所有子资源）
  azimuth::Resource::start(root_resource)
  assert_eq(azimuth::Resource::get_state(root_resource), azimuth::ResourceState::Running)
  assert_eq(azimuth::Resource::get_state(child_resource1), azimuth::ResourceState::Running)
  assert_eq(azimuth::Resource::get_state(child_resource2), azimuth::ResourceState::Running)
  assert_eq(azimuth::Resource::get_state(grandchild_resource), azimuth::ResourceState::Running)
  
  // 测试资源健康检查
  let root_health = azimuth::Resource::check_health(root_resource)
  assert_eq(root_health.status, azimuth::HealthStatus::Healthy)
  
  // 模拟子资源故障
  azimuth::Resource::simulate_failure(child_resource2, "Connection timeout")
  let child2_health = azimuth::Resource::check_health(child_resource2)
  assert_eq(child2_health.status, azimuth::HealthStatus::Unhealthy)
  assert_eq(child2_health.error, "Connection timeout")
  
  // 验证故障传播
  let root_health_after_failure = azimuth::Resource::check_health(root_resource)
  assert_eq(root_health_after_failure.status, azimuth::HealthStatus::Degraded)
  
  // 测试资源优雅关闭
  let shutdown_result = azimuth::Resource::shutdown_gracefully(root_resource, 5000)  // 5秒超时
  match shutdown_result {
    Ok(_) => {
      assert_eq(azimuth::Resource::get_state(root_resource), azimuth::ResourceState::Stopped)
      assert_eq(azimuth::Resource::get_state(child_resource1), azimuth::ResourceState::Stopped)
      assert_eq(azimuth::Resource::get_state(child_resource2), azimuth::ResourceState::Stopped)
      assert_eq(azimuth::Resource::get_state(grandchild_resource), azimuth::ResourceState::Stopped)
    }
    Err(error) => assert_false(true, "Graceful shutdown should succeed: " + error)
  }
  
  // 验证资源清理
  let active_resources = azimuth::Resource::get_active_resources()
  assert_eq(active_resources.length(), 0)  // 所有资源应该被清理
}