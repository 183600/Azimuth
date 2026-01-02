// Azimuth Error Boundary and Recovery Tests
// 测试错误边界和恢复机制

test "span operation error handling" {
  // 创建可能会失败的span操作
  let span = @azimuth.Span::builder("error-prone-operation")
    .with_error_handler(fn(error) {
      // 记录错误但不中断操作
      @azimuth.Logger::get_default().error("Span operation failed: " + error.message)
    })
    .start()
  
  // 模拟错误条件
  span.set_attribute("test.error.simulation", "true")
  
  // 执行可能失败的操作
  let result = @azimuth.ErrorBoundary::execute(fn() {
    // 模拟可能失败的操作
    if span.get_attribute("test.error.simulation") == Some("true") {
      @azimuth.TelemetryError::operation_failed("Simulated operation failure")
    } else {
      "Operation succeeded"
    }
  })
  
  // 验证错误处理
  match result {
    Ok(value) => assert_eq(value, "Operation succeeded"),
    Err(error) => {
      // 验证错误被正确记录
      assert_eq(error.message, "Simulated operation failure")
      assert_eq(span.get_status().code, @azimuth.StatusCode::ERROR)
    }
  }
  
  span.end()
}

test "metric collection error recovery" {
  // 创建带有错误恢复的度量收集器
  let collector = @azimuth.MetricCollector::builder()
    .with_error_recovery_strategy(@azimuth.ErrorRecoveryStrategy::RETRY)
    .with_max_retry_attempts(3)
    .build()
  
  // 模拟收集过程中的错误
  let error_count = @azimuth.AtomicInt::new(0)
  
  let collection_result = collector.collect_with_retry(fn() {
    let current_attempt = error_count.increment()
    if current_attempt < 3 {
      // 前两次尝试失败
      @azimuth.TelemetryError::collection_failed("Collection attempt " + current_attempt.to_string())
    } else {
      // 第三次尝试成功
      @azimuth.MetricData::new("test.metric", 42.0)
    }
  })
  
  // 验证重试机制
  assert_true(collection_result.is_ok())
  assert_eq(error_count.get(), 3)  // 应该尝试了3次
  
  match collection_result {
    Ok(metric_data) => {
      assert_eq(metric_data.name, "test.metric")
      assert_eq(metric_data.value, 42.0)
    }
    Err(_) => assert_true(false)
  }
}

test "log processing error isolation" {
  // 创建带有错误隔离的日志处理器
  let processor = @azimuth.LogProcessor::builder()
    .with_error_isolation(true)
    .build()
  
  // 添加正常日志记录
  processor.process(@azimuth.LogRecord::info("Normal log message"))
  
  // 添加会导致错误的日志记录
  let problematic_record = @azimuth.LogRecord::error("Problematic log")
    .with_attribute("error.simulation", "true")
  
  let process_result = processor.process(problematic_record)
  
  // 验证错误隔离
  match process_result {
    Ok(_) => assert_true(false),  // 应该失败
    Err(error) => {
      assert_eq(error.message, "Log processing failed")
    }
  }
  
  // 验证处理器仍然可以处理正常日志
  let normal_result = processor.process(@azimuth.LogRecord::info("Another normal log"))
  assert_true(normal_result.is_ok())
}

test "trace context corruption recovery" {
  // 创建可能损坏的追踪上下文
  let valid_trace_id = @azimuth.TraceId::generate()
  let valid_span_id = @azimuth.SpanId::generate()
  
  // 创建有效的上下文
  let valid_context = @azimuth.TraceContext::new(
    valid_trace_id,
    valid_span_id,
    @azimuth.TraceFlags::SAMPLED
  )
  
  // 模拟上下文损坏
  let corrupted_serialized = "invalid-context-data"
  
  // 尝试从损坏的数据恢复上下文
  let recovery_result = @azimuth.TraceContext::recover_from_corrupted(corrupted_serialized)
  
  // 验证恢复机制
  match recovery_result {
    Ok(recovered_context) => {
      // 验证恢复的上下文有有效的ID
      assert_true(recovered_context.trace_id.is_valid())
      assert_true(recovered_context.span_id.is_valid())
    }
    Err(error) => {
      // 如果无法恢复，应该创建新的上下文
      assert_eq(error.message, "Unable to recover from corrupted context")
    }
  }
  
  // 验证原始有效上下文不受影响
  assert_eq(valid_context.trace_id, valid_trace_id)
  assert_eq(valid_context.span_id, valid_span_id)
}

test "resource exhaustion handling" {
  // 创建资源限制配置
  let resource_limits = @azimuth.ResourceLimits::builder()
    .with_max_spans(100)
    .with_max_metrics(1000)
    .with_max_logs(500)
    .build()
  
  // 创建带有资源限制的遥测管理器
  let telemetry_manager = @azimuth.TelemetryManager::with_limits(resource_limits)
  
  // 添加span直到达到限制
  for i = 0; i < 150; i = i + 1 {
    let span_result = telemetry_manager.create_span("span-" + i.to_string())
    
    if i < 100 {
      // 前100个应该成功
      assert_true(span_result.is_ok())
    } else {
      // 超过限制的应该失败
      assert_true(span_result.is_err())
      match span_result {
        Err(error) => {
          assert_eq(error.code, @azimuth.ErrorCode::RESOURCE_EXHAUSTED)
        }
        _ => assert_true(false)
      }
    }
  }
  
  // 验证资源清理后可以继续创建
  telemetry_manager.clear_spans()
  
  let new_span_result = telemetry_manager.create_span("new-span-after-clear")
  assert_true(new_span_result.is_ok())
}

test "circuit breaker pattern for external services" {
  // 创建熔断器
  let circuit_breaker = @azimuth.CircuitBreaker::builder()
    .with_failure_threshold(3)
    .with_timeout_millis(5000)
    .with_success_threshold(2)
    .build()
  
  // 模拟外部服务调用
  let call_count = @azimuth.AtomicInt::new(0)
  
  // 前几次调用失败
  for i = 0; i < 3; i = i + 1 {
    let result = circuit_breaker.call(fn() {
      call_count.increment()
      @azimuth.TelemetryError::service_unavailable("External service down")
    })
    
    assert_true(result.is_err())
  }
  
  // 验证熔断器已打开
  assert_eq(circuit_breaker.get_state(), @azimuth.CircuitBreakerState::OPEN)
  
  // 熔断器打开时的调用应该直接失败，不尝试实际调用
  let call_count_before = call_count.get()
  let result_when_open = circuit_breaker.call(fn() {
    call_count.increment()
    "Success"
  })
  
  assert_true(result_when_open.is_err())
  assert_eq(call_count.get(), call_count_before)  // 调用次数不应增加
  
  // 等待超时后熔断器进入半开状态
  @azimuth.Time::sleep(6000)  // 超过5秒超时
  
  // 半开状态下的成功调用应该关闭熔断器
  for i = 0; i < 2; i = i + 1 {
    let result = circuit_breaker.call(fn() {
      "Success after recovery"
    })
    
    assert_true(result.is_ok())
  }
  
  // 验证熔断器已关闭
  assert_eq(circuit_breaker.get_state(), @azimuth.CircuitBreakerState::CLOSED)
}

test "graceful degradation under high load" {
  // 创建带有优雅降级的遥测系统
  let telemetry_system = @azimuth.TelemetrySystem::builder()
    .with_degradation_strategy(@azimuth.DegradationStrategy::SAMPLING_ADJUSTMENT)
    .with_load_threshold(1000)  // 每秒1000个操作
    .build()
  
  // 模拟正常负载
  for i = 0; i < 500; i = i + 1 {
    let span = telemetry_system.create_span("normal-load-" + i.to_string())
    assert_true(span.is_ok())
  }
  
  // 验证采样率未调整
  assert_eq(telemetry_system.get_sampling_rate(), 1.0)  // 100%采样
  
  // 模拟高负载
  for i = 0; i < 2000; i = i + 1 {
    let span = telemetry_system.create_span("high-load-" + i.to_string())
    // 在高负载下，一些操作可能会被拒绝或降级
  }
  
  // 验证采样率已调整
  assert_true(telemetry_system.get_sampling_rate() < 1.0)  // 采样率降低
  
  // 验证系统仍然可用
  let critical_span = telemetry_system.create_span("critical-operation")
  assert_true(critical_span.is_ok())
}