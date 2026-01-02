// Azimuth Resource Management Tests
// 测试资源管理和清理功能

test "span resource lifecycle management" {
  // 创建span资源
  let span = @azimuth.Span::builder("test-operation")
    .with_attribute("service.name", "test-service")
    .start()
  
  // 验证span已创建
  assert_true(span.is_recording())
  assert_eq(span.name, "test-operation")
  
  // 结束span
  span.end()
  
  // 验证span已结束
  assert_false(span.is_recording())
  assert_true(span.has_ended())
}

test "span automatic cleanup on scope exit" {
  // 创建作用域管理的span
  let tracer = @azimuth.Tracer::get_default()
  let scope = tracer.start_active_span("scoped-operation")
  
  // 验证span在作用域内活跃
  assert_true(scope.span().is_recording())
  
  // 模拟作用域退出（在实际实现中会自动调用）
  scope.close()
  
  // 验证span已结束
  assert_false(scope.span().is_recording())
}

test "metric instrument resource management" {
  // 创建度量工具
  let meter = @azimuth.Meter::get_default()
  
  // 创建计数器
  let counter = meter.create_counter("test_counter")
    .with_unit("requests")
    .with_description("Test request counter")
  
  // 使用计数器
  counter.add(1, @azimuth.Attributes::empty())
  counter.add(5, @azimuth.Attributes::empty())
  
  // 验证计数器资源
  assert_eq(counter.name, "test_counter")
  assert_eq(counter.unit, "requests")
  
  // 清理计数器
  counter.clear()
  
  // 验证计数器已清理
  assert_eq(counter.get_value(), 0)
}

test "batch processor resource cleanup" {
  // 创建批处理器
  let batch_processor = @azimuth.BatchSpanProcessor::builder()
    .with_max_queue_size(100)
    .with_max_export_batch_size(10)
    .with_export_timeout_millis(5000)
    .build()
  
  // 添加span到批处理器
  for i = 0; i < 5; i = i + 1 {
    let span = @azimuth.Span::builder("batch-test-" + i.to_string()).start()
    batch_processor.on_span_end(span)
  }
  
  // 验证队列中有span
  assert_true(batch_processor.queue_size() > 0)
  
  // 强制刷新并关闭处理器
  batch_processor.force_flush()
  batch_processor.shutdown()
  
  // 验证处理器已关闭
  assert_true(batch_processor.is_shutdown())
  assert_eq(batch_processor.queue_size(), 0)
}

test "logger resource management" {
  // 创建日志记录器
  let logger_provider = @azimuth.LoggerProvider::builder().build()
  let logger = logger_provider.get_logger("test-logger")
  
  // 记录日志
  logger.emit_log(@azimuth.LogRecord::info("Test log message"))
  logger.emit_log(@azimuth.LogRecord::error("Error message"))
  
  // 验证日志记录器资源
  assert_eq(logger.name, "test-logger")
  
  // 关闭日志提供者
  logger_provider.shutdown()
  
  // 验证提供者已关闭
  assert_true(logger_provider.is_shutdown())
}

test "memory pool for telemetry objects" {
  // 创建内存池
  let pool = @azimuth.TelemetryObjectPool::new(10)  // 最大10个对象
  
  // 从池中获取对象
  let obj1 = pool.acquire()
  let obj2 = pool.acquire()
  
  // 验证对象获取成功
  assert_true(obj1.is_some())
  assert_true(obj2.is_some())
  
  // 使用对象
  match obj1 {
    Some(obj) => {
      obj.set_data("test-data-1")
      assert_eq(obj.get_data(), "test-data-1")
    }
    None => assert_true(false)
  }
  
  // 归还对象到池中
  match obj1 {
    Some(obj) => {
      pool.release(obj)
    }
    None => assert_true(false)
  }
  
  // 验证对象已归还
  assert_eq(pool.available_count(), 1)
}

test "resource cleanup under error conditions" {
  // 创建可能失败的资源
  let resource = @azimuth.FragileResource::new()
  
  // 模拟错误条件
  resource.simulate_error()
  
  // 尝试清理资源
  let cleanup_result = resource.cleanup()
  
  // 验证清理即使出错也能执行
  assert_true(cleanup_result.is_ok())
  
  // 验证资源状态
  assert_true(resource.is_cleaned())
}

test "concurrent resource access management" {
  // 创建共享资源
  let shared_resource = @azimuth.SharedResource::new()
  
  // 模拟并发访问
  let tasks = []
  for i = 0; i < 5; i = i + 1 {
    let task = @azimuth.Task::spawn(fn() {
      // 获取资源锁
      let lock = shared_resource.acquire_lock()
      
      // 使用资源
      shared_resource.increment_counter()
      
      // 释放锁
      lock.release()
    })
    tasks.push(task)
  }
  
  // 等待所有任务完成
  for task in tasks {
    task.wait()
  }
  
  // 验证资源状态
  assert_eq(shared_resource.get_counter(), 5)
}