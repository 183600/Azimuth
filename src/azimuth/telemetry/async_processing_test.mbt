// 高级异步处理测试用例
// 测试异步流处理、异步回调机制、异步错误恢复、异步性能优化等高级异步功能

test "async_stream_processing" {
  // 测试异步流处理
  
  enum StreamState {
    Active
    Paused
    Completed
    Error
    Closed
  }
  
  struct StreamItem {
    id: String
    data: String
    timestamp: Int64
    processed: Bool
  }
  
  struct AsyncStream {
    stream_id: String
    state: StreamState
    buffer: Array<StreamItem>
    buffer_size: Int
    processed_count: Int
    error_count: Int
    last_activity: Int64
  }
  
  struct StreamProcessor {
    streams: Array<AsyncStream>
    max_concurrent_streams: Int
    processing_rate: Int  // items per second
    backlog_threshold: Int
  }
  
  // 创建流处理器
  let create_stream_processor = fn(max_streams: Int, rate: Int) -> StreamProcessor {
    StreamProcessor{
      streams: Array::empty(),
      max_concurrent_streams: max_streams,
      processing_rate: rate,
      backlog_threshold: 1000
    }
  }
  
  // 创建异步流
  let create_async_stream = fn(stream_id: String, buffer_size: Int) -> AsyncStream {
    AsyncStream{
      stream_id: stream_id,
      state: Active,
      buffer: Array::empty(),
      buffer_size: buffer_size,
      processed_count: 0,
      error_count: 0,
      last_activity: 1640995200000000L
    }
  }
  
  // 添加流项目
  let add_stream_item = fn(stream: AsyncStream, item: StreamItem) -> AsyncStream {
    if stream.buffer.length() >= stream.buffer_size {
      // 缓冲区满，丢弃最旧的项目
      let mut new_buffer = stream.buffer.to_array()
      if new_buffer.length() > 0 {
        new_buffer.remove_at(0)
      }
      new_buffer.push(item)
      
      { stream |
        buffer: new_buffer,
        last_activity: item.timestamp
      }
    } else {
      let mut new_buffer = stream.buffer.to_array()
      new_buffer.push(item)
      
      { stream |
        buffer: new_buffer,
        last_activity: item.timestamp
      }
    }
  }
  
  // 处理流项目
  let process_stream_items = fn(processor: StreamProcessor, current_time: Int64) -> StreamProcessor {
    let mut updated_streams = Array::empty<AsyncStream>()
    
    let mut i = 0
    while i < processor.streams.length() {
      let stream = processor.streams[i]
      
      if stream.state == Active && stream.buffer.length() > 0 {
        // 计算这次可以处理的项目数
        let time_since_last = (current_time - stream.last_activity) / 1000000000L
        let items_to_process = @min(
          stream.buffer.length(),
          (processor.processing_rate * time_since_last.to_int()).max(1)
        )
        
        let mut processed_items = Array::empty<StreamItem>()
        let mut remaining_items = Array::empty<StreamItem>()
        
        let mut j = 0
        while j < stream.buffer.length() {
          let item = stream.buffer[j]
          if j < items_to_process {
            processed_items.push({ item | processed: true })
          } else {
            remaining_items.push(item)
          }
          j = j + 1
        }
        
        let updated_stream = { stream |
          buffer: remaining_items,
          processed_count: stream.processed_count + processed_items.length(),
          last_activity: current_time
        }
        
        updated_streams.push(updated_stream)
      } else {
        updated_streams.push(stream)
      }
      
      i = i + 1
    }
    
    { processor | streams: updated_streams }
  }
  
  // 测试流处理
  let processor = create_stream_processor(3, 10)  // 最多3个流，每秒10个项目
  
  // 创建测试流
  let stream1 = create_async_stream("stream-1", 100)
  let stream2 = create_async_stream("stream-2", 50)
  let stream3 = create_async_stream("stream-3", 200)
  
  let processor_with_streams = { processor |
    streams: [stream1, stream2, stream3]
  }
  
  // 添加测试项目
  let current_time = 1640995200000000L
  
  let mut test_processor = processor_with_streams
  let mut stream_index = 0
  let mut item_index = 0
  
  // 为每个流添加项目
  while stream_index < 3 {
    let mut stream = test_processor.streams[stream_index]
    let mut i = 0
    while i < 20 {
      let item = StreamItem{
        id: "item-" + stream_index.to_string() + "-" + i.to_string(),
        data: "data-" + i.to_string(),
        timestamp: current_time + i.to_int64() * 1000000L,
        processed: false
      }
      stream = add_stream_item(stream, item)
      i = i + 1
    }
    
    let mut updated_streams = test_processor.streams.to_array()
    updated_streams[stream_index] = stream
    test_processor = { test_processor | streams: updated_streams }
    
    stream_index = stream_index + 1
  }
  
  // 验证项目添加
  assert_eq(test_processor.streams[0].buffer.length(), 20)
  assert_eq(test_processor.streams[1].buffer.length(), 20)
  assert_eq(test_processor.streams[2].buffer.length(), 20)
  
  // 处理流项目（模拟2秒后）
  let processed_processor = process_stream_items(test_processor, current_time + 2000000000L)
  
  // 验证处理结果
  assert_eq(processed_processor.streams[0].processed_count > 0, true)
  assert_eq(processed_processor.streams[1].processed_count > 0, true)
  assert_eq(processed_processor.streams[2].processed_count > 0, true)
  
  // 验证剩余项目
  let total_remaining = processed_processor.streams.fold(0, fn(acc, stream) { 
    acc + stream.buffer.length() 
  })
  assert_eq(total_remaining < 60, true)  // 应该处理了一些项目
}

test "async_callback_mechanisms" {
  // 测试异步回调机制
  
  enum CallbackType {
    OnSuccess
    OnError
    OnProgress
    OnComplete
    OnTimeout
  }
  
  struct AsyncCallback {
    callback_id: String
    callback_type: CallbackType
    function: String  // 简化的函数引用
    timeout_ms: Int
    retry_count: Int
    max_retries: Int
  }
  
  struct AsyncOperation {
    operation_id: String
    status: String
    progress: Float
    result: Option<String>
    error: Option<String>
    callbacks: Array<AsyncCallback>
    created_time: Int64
    updated_time: Int64
  }
  
  struct CallbackManager {
    operations: Array<AsyncOperation>
    pending_callbacks: Array<(String, CallbackType, String)>  // (operation_id, callback_type, result)
    executed_callbacks: Array<String>
    callback_timeout_ms: Int
  }
  
  // 创建回调管理器
  let create_callback_manager = fn(timeout_ms: Int) -> CallbackManager {
    CallbackManager{
      operations: Array::empty(),
      pending_callbacks: Array::empty(),
      executed_callbacks: Array::empty(),
      callback_timeout_ms: timeout_ms
    }
  }
  
  // 创建异步操作
  let create_async_operation = fn(operation_id: String, callbacks: Array<AsyncCallback>) -> AsyncOperation {
    let current_time = 1640995200000000L
    AsyncOperation{
      operation_id: operation_id,
      status: "pending",
      progress: 0.0,
      result: None,
      error: None,
      callbacks: callbacks,
      created_time: current_time,
      updated_time: current_time
    }
  }
  
  // 添加回调
  let add_callback = fn(operation: AsyncOperation, callback_type: CallbackType, function: String) -> AsyncOperation {
    let callback = AsyncCallback{
      callback_id: "callback-" + operation.callbacks.length().to_string(),
      callback_type: callback_type,
      function: function,
      timeout_ms: 5000,
      retry_count: 0,
      max_retries: 3
    }
    
    let mut new_callbacks = operation.callbacks.to_array()
    new_callbacks.push(callback)
    
    { operation | callbacks: new_callbacks }
  }
  
  // 触发回调
  let trigger_callbacks = fn(
    manager: CallbackManager,
    operation_id: String,
    callback_type: CallbackType,
    result: String,
    current_time: Int64
  ) -> CallbackManager {
    let mut updated_manager = manager
    
    // 查找操作
    let operation_index = manager.operations.index_of(fn(op) { op.operation_id == operation_id })
    
    match operation_index {
      Some(index) => {
        let operation = manager.operations[index]
        
        // 查找匹配的回调
        let mut i = 0
        while i < operation.callbacks.length() {
          let callback = operation.callbacks[i]
          
          if callback.callback_type == callback_type {
            // 添加到待执行队列
            updated_manager.pending_callbacks.push((operation_id, callback_type, result))
          }
          
          i = i + 1
        }
        
        // 更新操作状态
        let mut updated_operations = manager.operations.to_array()
        let updated_operation = match callback_type {
          OnSuccess => { operation |
            status: "completed",
            result: Some(result),
            updated_time: current_time
          }
          OnError => { operation |
            status: "failed",
            error: Some(result),
            updated_time: current_time
          }
          OnProgress => { operation |
            progress: result.to_float(),
            updated_time: current_time
          }
          OnComplete => { operation |
            status: "completed",
            updated_time: current_time
          }
          OnTimeout => { operation |
            status: "timeout",
            error: Some("Operation timeout"),
            updated_time: current_time
          }
        }
        
        updated_operations[index] = updated_operation
        updated_manager = { updated_manager | operations: updated_operations }
      }
      None => {}
    }
    
    updated_manager
  }
  
  // 执行待处理的回调
  let execute_pending_callbacks = fn(manager: CallbackManager, current_time: Int64) -> CallbackManager {
    let mut updated_manager = manager
    let mut executed_this_round = Array::empty<String>()
    
    let mut i = 0
    while i < updated_manager.pending_callbacks.length() {
      let (operation_id, callback_type, result) = updated_manager.pending_callbacks[i]
      let callback_id = operation_id + "-" + callback_type.to_string()
      
      // 模拟回调执行
      let execution_success = true  // 简化：总是成功
      
      if execution_success {
        executed_this_round.push(callback_id)
      }
      
      i = i + 1
    }
    
    // 移除已执行的回调
    let mut remaining_callbacks = Array::empty<(String, CallbackType, String)>()
    let mut j = 0
    while j < updated_manager.pending_callbacks.length() {
      let pending = updated_manager.pending_callbacks[j]
      let callback_id = pending.0 + "-" + pending.1.to_string()
      
      if !executed_this_round.contains(callback_id) {
        remaining_callbacks.push(pending)
      }
      
      j = j + 1
    }
    
    let mut new_executed = updated_manager.executed_callbacks.to_array()
    let mut k = 0
    while k < executed_this_round.length() {
      new_executed.push(executed_this_round[k])
      k = k + 1
    }
    
    { updated_manager |
      pending_callbacks: remaining_callbacks,
      executed_callbacks: new_executed
    }
  }
  
  // 测试回调机制
  let callback_manager = create_callback_manager(10000)
  
  // 创建测试操作
  let test_operation = create_async_operation("op-001", Array::empty())
  let operation_with_callbacks = add_callback(test_operation, OnSuccess, "handle_success")
  let operation_with_more_callbacks = add_callback(operation_with_callbacks, OnError, "handle_error")
  let final_operation = add_callback(operation_with_more_callbacks, OnProgress, "handle_progress")
  
  let manager_with_operation = { callback_manager |
    operations: [final_operation]
  }
  
  // 触发进度回调
  let current_time = 1640995200000000L
  let manager_after_progress = trigger_callbacks(
    manager_with_operation,
    "op-001",
    OnProgress,
    "50.0",
    current_time
  )
  
  assert_eq(manager_after_progress.pending_callbacks.length(), 1)
  assert_eq(manager_after_progress.operations[0].progress, 50.0)
  
  // 执行回调
  let manager_after_execution = execute_pending_callbacks(manager_after_progress, current_time + 1000000L)
  
  assert_eq(manager_after_execution.executed_callbacks.length(), 1)
  assert_eq(manager_after_execution.pending_callbacks.length(), 0)
  
  // 触发成功回调
  let manager_after_success = trigger_callbacks(
    manager_after_execution,
    "op-001",
    OnSuccess,
    "operation completed successfully",
    current_time + 2000000L
  )
  
  assert_eq(manager_after_success.pending_callbacks.length(), 1)
  assert_eq(manager_after_success.operations[0].status, "completed")
  assert_eq(manager_after_success.operations[0].result, Some("operation completed successfully"))
  
  // 执行成功回调
  let final_manager = execute_pending_callbacks(manager_after_success, current_time + 3000000L)
  
  assert_eq(final_manager.executed_callbacks.length(), 2)
  assert_eq(final_manager.pending_callbacks.length(), 0)
}

test "async_error_recovery" {
  // 测试异步错误恢复
  
  enum ErrorType {
    NetworkTimeout
    ConnectionLost
    ResourceExhausted
    DataCorruption
    ServiceUnavailable
  }
  
  enum RecoveryStrategy {
    Retry
    Fallback
    CircuitBreaker
    GracefulDegradation
    FailFast
  }
  
  struct AsyncError {
    error_id: String
    error_type: ErrorType
    severity: String  // "low", "medium", "high", "critical"
    message: String
    timestamp: Int64
    retry_count: Int
    max_retries: Int
    recovery_strategy: RecoveryStrategy
  }
  
  struct ErrorRecoveryManager {
    active_errors: Array<AsyncError>
    recovered_errors: Array<AsyncError>
    failed_recoveries: Array<AsyncError>
    circuit_breaker_state: String  // "closed", "open", "half_open"
    failure_threshold: Int
    recovery_timeout_ms: Int
  }
  
  // 创建错误恢复管理器
  let create_recovery_manager = fn(threshold: Int, timeout_ms: Int) -> ErrorRecoveryManager {
    ErrorRecoveryManager{
      active_errors: Array::empty(),
      recovered_errors: Array::empty(),
      failed_recoveries: Array::empty(),
      circuit_breaker_state: "closed",
      failure_threshold: threshold,
      recovery_timeout_ms: timeout_ms
    }
  }
  
  // 创建异步错误
  let create_async_error = fn(
    error_id: String,
    error_type: ErrorType,
    severity: String,
    message: String,
    strategy: RecoveryStrategy,
    max_retries: Int
  ) -> AsyncError {
    AsyncError{
      error_id: error_id,
      error_type: error_type,
      severity: severity,
      message: message,
      timestamp: 1640995200000000L,
      retry_count: 0,
      max_retries: max_retries,
      recovery_strategy: strategy
    }
  }
  
  // 尝试错误恢复
  let attempt_error_recovery = fn(
    manager: ErrorRecoveryManager,
    error_id: String,
    current_time: Int64
  ) -> ErrorRecoveryManager {
    let mut updated_manager = manager
    
    // 查找错误
    let error_index = manager.active_errors.index_of(fn(err) { err.error_id == error_id })
    
    match error_index {
      Some(index) => {
        let error = manager.active_errors[index]
        
        // 检查熔断器状态
        if manager.circuit_breaker_state == "open" {
          // 熔断器打开，不尝试恢复
          return updated_manager
        }
        
        // 根据恢复策略处理错误
        let recovery_success = match error.recovery_strategy {
          Retry => {
            // 重试策略：有概率成功
            error.retry_count < error.max_retries && (error.retry_count % 2 == 0)
          }
          Fallback => {
            // 回退策略：总是成功
            true
          }
          CircuitBreaker => {
            // 熔断策略：检查失败次数
            manager.active_errors.length() < manager.failure_threshold
          }
          GracefulDegradation => {
            // 优雅降级：部分功能可用
            error.severity != "critical"
          }
          FailFast => {
            // 快速失败：不尝试恢复
            false
          }
        }
        
        if recovery_success {
          // 恢复成功
          let recovered_error = { error |
            timestamp: current_time,
            retry_count: error.retry_count + 1
          }
          
          let mut new_recovered = manager.recovered_errors.to_array()
          new_recovered.push(recovered_error)
          
          let mut new_active = manager.active_errors.to_array()
          new_active.remove_at(index)
          
          updated_manager = { updated_manager |
            recovered_errors: new_recovered,
            active_errors: new_active
          }
        } else {
          // 恢复失败
          let failed_error = { error |
            timestamp: current_time,
            retry_count: error.retry_count + 1
          }
          
          if failed_error.retry_count >= failed_error.max_retries {
            // 超过最大重试次数，标记为恢复失败
            let mut new_failed = manager.failed_recoveries.to_array()
            new_failed.push(failed_error)
            
            let mut new_active = manager.active_errors.to_array()
            new_active.remove_at(index)
            
            updated_manager = { updated_manager |
              failed_recoveries: new_failed,
              active_errors: new_active
            }
            
            // 检查是否需要打开熔断器
            if manager.failed_recoveries.length() >= manager.failure_threshold {
              updated_manager = { updated_manager | circuit_breaker_state: "open" }
            }
          } else {
            // 更新重试次数，保持活跃状态
            let mut new_active = manager.active_errors.to_array()
            new_active[index] = failed_error
            
            updated_manager = { updated_manager | active_errors: new_active }
          }
        }
      }
      None => {}
    }
    
    updated_manager
  }
  
  // 测试错误恢复
  let recovery_manager = create_recovery_manager(3, 5000)
  
  // 创建测试错误
  let test_errors = [
    create_async_error(
      "error-001",
      NetworkTimeout,
      "medium",
      "Network timeout occurred",
      Retry,
      3
    ),
    create_async_error(
      "error-002",
      ConnectionLost,
      "high",
      "Connection was lost",
      Fallback,
      1
    ),
    create_async_error(
      "error-003",
      ResourceExhausted,
      "critical",
      "Memory exhausted",
      GracefulDegradation,
      2
    ),
    create_async_error(
      "error-004",
      ServiceUnavailable,
      "medium",
      "Service is temporarily unavailable",
      FailFast,
      1
    )
  ]
  
  let manager_with_errors = { recovery_manager | active_errors: test_errors }
  
  // 尝试恢复错误
  let current_time = 1640995200000000L
  
  // 第一次恢复尝试
  let manager_after_first_recovery = attempt_error_recovery(
    manager_with_errors,
    "error-001",
    current_time
  )
  
  // Retry策略，第一次重试应该成功（偶数次重试成功）
  assert_eq(manager_after_first_recovery.recovered_errors.length(), 1)
  assert_eq(manager_after_first_recovery.active_errors.length(), 3)
  
  // 第二次恢复尝试
  let manager_after_second_recovery = attempt_error_recovery(
    manager_after_first_recovery,
    "error-002",
    current_time + 1000000L
  )
  
  // Fallback策略，应该总是成功
  assert_eq(manager_after_second_recovery.recovered_errors.length(), 2)
  assert_eq(manager_after_second_recovery.active_errors.length(), 2)
  
  // 第三次恢复尝试
  let manager_after_third_recovery = attempt_error_recovery(
    manager_after_second_recovery,
    "error-003",
    current_time + 2000000L
  )
  
  // GracefulDegradation策略，非critical错误应该成功
  assert_eq(manager_after_third_recovery.recovered_errors.length(), 3)
  assert_eq(manager_after_third_recovery.active_errors.length(), 1)
  
  // 第四次恢复尝试
  let manager_after_fourth_recovery = attempt_error_recovery(
    manager_after_third_recovery,
    "error-004",
    current_time + 3000000L
  )
  
  // FailFast策略，应该总是失败
  assert_eq(manager_after_fourth_recovery.recovered_errors.length(), 3)
  assert_eq(manager_after_fourth_recovery.failed_recoveries.length(), 1)
  assert_eq(manager_after_fourth_recovery.active_errors.length(), 0)
  
  // 验证熔断器状态
  assert_eq(manager_after_fourth_recovery.circuit_breaker_state, "closed")  // 失败数未达到阈值
}

test "async_performance_optimization" {
  // 测试异步性能优化
  
  struct PerformanceMetrics {
    operation_id: String
    start_time: Int64
    end_time: Int64
    duration_ms: Int64
    cpu_usage: Float
    memory_usage_mb: Float
    throughput: Float  // operations per second
    latency_p50: Float
    latency_p95: Float
    latency_p99: Float
  }
  
  struct OptimizationConfig {
    enable_batching: Bool
    batch_size: Int
    enable_compression: Bool
    enable_caching: Bool
    cache_size: Int
    max_concurrent_operations: Int
    timeout_ms: Int
  }
  
  struct AsyncOptimizer {
    config: OptimizationConfig
    metrics_history: Array<PerformanceMetrics>
    optimization_suggestions: Array<String>
    current_performance_score: Float
  }
  
  // 创建异步优化器
  let create_async_optimizer = fn(config: OptimizationConfig) -> AsyncOptimizer {
    AsyncOptimizer{
      config: config,
      metrics_history: Array::empty(),
      optimization_suggestions: Array::empty(),
      current_performance_score: 0.0
    }
  }
  
  // 模拟异步操作性能
  let simulate_async_operation = fn(
    operation_id: String,
    config: OptimizationConfig,
    base_latency_ms: Int64,
    current_time: Int64
  ) -> PerformanceMetrics {
    let mut actual_latency = base_latency_ms
    
    // 批处理优化
    if config.enable_batching {
      actual_latency = actual_latency / config.batch_size.to_int64()
    }
    
    // 压缩优化
    if config.enable_compression {
      actual_latency = actual_latency + 50  // 压缩开销
    }
    
    // 缓存优化
    let cache_hit = config.enable_caching && (operation_id.to_int() % 3 == 0)
    if cache_hit {
      actual_latency = actual_latency / 4  // 缓存命中大幅减少延迟
    }
    
    let end_time = current_time + actual_latency * 1000000L
    let duration_ms = actual_latency
    
    // 模拟性能指标
    let throughput = if duration_ms > 0 {
      1000.0 / duration_ms.to_float()
    } else {
      0.0
    }
    
    PerformanceMetrics{
      operation_id: operation_id,
      start_time: current_time,
      end_time: end_time,
      duration_ms: duration_ms,
      cpu_usage: 0.3 + (config.max_concurrent_operations.to_float() * 0.1),
      memory_usage_mb: 50.0 + (config.cache_size.to_float() * 0.5),
      throughput: throughput,
      latency_p50: duration_ms.to_float() * 0.8,
      latency_p95: duration_ms.to_float() * 1.2,
      latency_p99: duration_ms.to_float() * 1.5
    }
  }
  
  // 分析性能并生成优化建议
  let analyze_performance = fn(optimizer: AsyncOptimizer) -> AsyncOptimizer {
    if optimizer.metrics_history.length() < 5 {
      return optimizer  // 数据不足
    }
    
    let recent_metrics = optimizer.metrics_history.slice(
      optimizer.metrics_history.length() - 5,
      optimizer.metrics_history.length()
    )
    
    let avg_latency = recent_metrics.fold(0.0, fn(acc, m) { acc + m.latency_p95 }) / recent_metrics.length().to_float()
    let avg_throughput = recent_metrics.fold(0.0, fn(acc, m) { acc + m.throughput }) / recent_metrics.length().to_float()
    let avg_cpu = recent_metrics.fold(0.0, fn(acc, m) { acc + m.cpu_usage }) / recent_metrics.length().to_float()
    let avg_memory = recent_metrics.fold(0.0, fn(acc, m) { acc + m.memory_usage_mb }) / recent_metrics.length().to_float()
    
    let mut suggestions = Array::empty<String>()
    
    // 延迟优化建议
    if avg_latency > 100.0 {
      if !optimizer.config.enable_batching {
        suggestions.push("Enable batching to reduce latency")
      }
      if !optimizer.config.enable_caching {
        suggestions.push("Enable caching to improve response time")
      }
      if optimizer.config.batch_size < 10 {
        suggestions.push("Increase batch size for better throughput")
      }
    }
    
    // 吞吐量优化建议
    if avg_throughput < 10.0 {
      if optimizer.config.max_concurrent_operations < 20 {
        suggestions.push("Increase max concurrent operations")
      }
      if !optimizer.config.enable_compression {
        suggestions.push("Enable compression to reduce network overhead")
      }
    }
    
    // 资源使用优化建议
    if avg_cpu > 0.8 {
      suggestions.push("Reduce max concurrent operations to lower CPU usage")
    }
    
    if avg_memory > 200.0 {
      if optimizer.config.cache_size > 1000 {
        suggestions.push("Reduce cache size to lower memory usage")
      }
    }
    
    // 计算性能评分
    let latency_score = if avg_latency < 50.0 { 1.0 } else if avg_latency < 100.0 { 0.8 } else if avg_latency < 200.0 { 0.6 } else { 0.3 }
    let throughput_score = if avg_throughput > 50.0 { 1.0 } else if avg_throughput > 20.0 { 0.8 } else if avg_throughput > 10.0 { 0.6 } else { 0.3 }
    let resource_score = if avg_cpu < 0.5 && avg_memory < 100.0 { 1.0 } else if avg_cpu < 0.8 && avg_memory < 200.0 { 0.8 } else { 0.5 }
    
    let performance_score = (latency_score + throughput_score + resource_score) / 3.0
    
    { optimizer |
      optimization_suggestions: suggestions,
      current_performance_score: performance_score
    }
  }
  
  // 测试性能优化
  let optimization_config = OptimizationConfig{
    enable_batching: true,
    batch_size: 5,
    enable_compression: false,
    enable_caching: true,
    cache_size: 500,
    max_concurrent_operations: 10,
    timeout_ms: 5000
  }
  
  let optimizer = create_async_optimizer(optimization_config)
  
  // 模拟多个异步操作
  let current_time = 1640995200000000L
  let mut updated_optimizer = optimizer
  
  let mut i = 0
  while i < 10 {
    let operation_id = "op-" + i.to_string()
    let base_latency = 200 + (i * 20)  // 递增的基础延迟
    
    let metrics = simulate_async_operation(
      operation_id,
      optimization_config,
      base_latency,
      current_time + i.to_int64() * 100000000L
    )
    
    let mut new_metrics = updated_optimizer.metrics_history.to_array()
    new_metrics.push(metrics)
    
    updated_optimizer = { updated_optimizer | metrics_history: new_metrics }
    
    i = i + 1
  }
  
  // 分析性能
  let analyzed_optimizer = analyze_performance(updated_optimizer)
  
  // 验证性能指标
  assert_eq(analyzed_optimizer.metrics_history.length(), 10)
  
  let avg_latency = analyzed_optimizer.metrics_history.fold(0.0, fn(acc, m) { acc + m.latency_p95 }) / 10.0
  let avg_throughput = analyzed_optimizer.metrics_history.fold(0.0, fn(acc, m) { acc + m.throughput }) / 10.0
  
  assert_eq(avg_latency > 0.0, true)
  assert_eq(avg_throughput > 0.0, true)
  
  // 验证性能评分
  assert_eq(analyzed_optimizer.current_performance_score >= 0.0, true)
  assert_eq(analyzed_optimizer.current_performance_score <= 1.0, true)
  
  // 验证优化建议
  if analyzed_optimizer.optimization_suggestions.length() > 0 {
    let mut j = 0
    while j < analyzed_optimizer.optimization_suggestions.length() {
      let suggestion = analyzed_optimizer.optimization_suggestions[j]
      assert_eq(suggestion.length() > 0, true)
      j = j + 1
    }
  }
  
  // 测试不同配置的性能比较
  let high_performance_config = OptimizationConfig{
    enable_batching: true,
    batch_size: 20,
    enable_compression: true,
    enable_caching: true,
    cache_size: 1000,
    max_concurrent_operations: 50,
    timeout_ms: 10000
  }
  
  let high_performance_optimizer = create_async_optimizer(high_performance_config)
  let mut high_perf_updated = high_performance_optimizer
  
  let mut k = 0
  while k < 10 {
    let operation_id = "high-perf-op-" + k.to_string()
    let metrics = simulate_async_operation(
      operation_id,
      high_performance_config,
      200,  // 相同的基础延迟
      current_time + k.to_int64() * 100000000L
    )
    
    let mut new_metrics = high_perf_updated.metrics_history.to_array()
    new_metrics.push(metrics)
    high_perf_updated = { high_perf_updated | metrics_history: new_metrics }
    
    k = k + 1
  }
  
  let high_perf_analyzed = analyze_performance(high_perf_updated)
  
  // 高性能配置应该有更好的性能评分
  assert_eq(high_perf_analyzed.current_performance_score >= analyzed_optimizer.current_performance_score, true)
}

test "async_backpressure_handling" {
  // 测试异步背压处理
  
  enum BackpressureStrategy {
    DropOldest
    DropNewest
    Buffer
    Throttle
    Reject
  }
  
  struct BackpressureConfig {
    strategy: BackpressureStrategy
    max_queue_size: Int
    buffer_size: Int
    throttle_rate: Int  // operations per second
    rejection_threshold: Float  // 0.0 - 1.0
  }
  
  struct AsyncQueue {
    queue_id: String
    items: Array<String>
    max_size: Int
    dropped_items: Int
    rejected_items: Int
    processed_items: Int
    last_processed_time: Int64
  }
  
  struct BackpressureHandler {
    queues: Array<AsyncQueue>
    config: BackpressureConfig
    total_dropped: Int
    total_rejected: Int
    system_load: Float
  }
  
  // 创建背压处理器
  let create_backpressure_handler = fn(config: BackpressureConfig) -> BackpressureHandler {
    BackpressureHandler{
      queues: Array::empty(),
      config: config,
      total_dropped: 0,
      total_rejected: 0,
      system_load: 0.0
    }
  }
  
  // 创建异步队列
  let create_async_queue = fn(queue_id: String, max_size: Int) -> AsyncQueue {
    AsyncQueue{
      queue_id: queue_id,
      items: Array::empty(),
      max_size: max_size,
      dropped_items: 0,
      rejected_items: 0,
      processed_items: 0,
      last_processed_time: 1640995200000000L
    }
  }
  
  // 处理背压
  let handle_backpressure = fn(
    handler: BackpressureHandler,
    queue_id: String,
    new_items: Array<String>,
    current_time: Int64
  ) -> BackpressureHandler {
    let mut updated_handler = handler
    
    // 查找队列
    let queue_index = handler.queues.index_of(fn(q) { q.queue_id == queue_id })
    
    match queue_index {
      Some(index) => {
        let queue = handler.queues[index]
        let mut updated_queue = queue
        let available_space = queue.max_size - queue.items.length()
        let items_to_add = new_items.length()
        
        match handler.config.strategy {
          DropOldest => {
            if items_to_add > available_space {
              // 丢弃最旧的项目
              let items_to_drop = items_to_add - available_space
              let mut new_items_list = queue.items.to_array()
              
              let mut i = 0
              while i < items_to_drop {
                if new_items_list.length() > 0 {
                  new_items_list.remove_at(0)
                  updated_queue.dropped_items = updated_queue.dropped_items + 1
                }
                i = i + 1
              }
              
              // 添加新项目
              let mut j = 0
              while j < new_items.length() {
                new_items_list.push(new_items[j])
                j = j + 1
              }
              
              updated_queue.items = new_items_list
              updated_handler.total_dropped = updated_handler.total_dropped + items_to_drop
            } else {
              // 有足够空间，直接添加
              let mut new_items_list = queue.items.to_array()
              let mut j = 0
              while j < new_items.length() {
                new_items_list.push(new_items[j])
                j = j + 1
              }
              updated_queue.items = new_items_list
            }
          }
          DropNewest => {
            // 只添加能容纳的项目，丢弃最新的
            let items_to_accept = @min(items_to_add, available_space + queue.items.length())
            let items_to_drop = items_to_add - items_to_accept
            
            let mut new_items_list = queue.items.to_array()
            let mut j = 0
            while j < items_to_accept {
              new_items_list.push(new_items[j])
              j = j + 1
            }
            
            updated_queue.items = new_items_list
            updated_queue.dropped_items = updated_queue.dropped_items + items_to_drop
            updated_handler.total_dropped = updated_handler.total_dropped + items_to_drop
          }
          Buffer => {
            // 使用缓冲区
            let buffer_space = handler.config.buffer_size
            let total_space = queue.max_size + buffer_space
            
            if items_to_add <= total_space - queue.items.length() {
              // 可以缓冲
              let mut new_items_list = queue.items.to_array()
              let mut j = 0
              while j < new_items.length() {
                new_items_list.push(new_items[j])
                j = j + 1
              }
              updated_queue.items = new_items_list
            } else {
              // 缓冲区也满了，丢弃新项目
              updated_queue.rejected_items = updated_queue.rejected_items + items_to_add
              updated_handler.total_rejected = updated_handler.total_rejected + items_to_add
            }
          }
          Throttle => {
            // 限流处理
            let time_since_last = (current_time - queue.last_processed_time) / 1000000000L
            let allowed_items = (handler.config.throttle_rate * time_since_last.to_int()).max(0)
            
            let items_to_process = @min(items_to_add, allowed_items)
            let items_to_reject = items_to_add - items_to_process
            
            // 处理允许的项目
            let mut new_items_list = queue.items.to_array()
            let mut j = 0
            while j < items_to_process {
              new_items_list.push(new_items[j])
              j = j + 1
            }
            
            updated_queue.items = new_items_list
            updated_queue.rejected_items = updated_queue.rejected_items + items_to_reject
            updated_handler.total_rejected = updated_handler.total_rejected + items_to_reject
          }
          Reject => {
            // 直接拒绝
            let system_load_factor = handler.system_load
            if system_load_factor > handler.config.rejection_threshold {
              updated_queue.rejected_items = updated_queue.rejected_items + items_to_add
              updated_handler.total_rejected = updated_handler.total_rejected + items_to_add
            } else {
              // 系统负载较低，允许添加
              let mut new_items_list = queue.items.to_array()
              let mut j = 0
              while j < new_items.length() && new_items_list.length() < queue.max_size {
                new_items_list.push(new_items[j])
                j = j + 1
              }
              
              let items_rejected = new_items.length() - j
              updated_queue.items = new_items_list
              updated_queue.rejected_items = updated_queue.rejected_items + items_rejected
              updated_handler.total_rejected = updated_handler.total_rejected + items_rejected
            }
          }
        }
        
        let mut updated_queues = handler.queues.to_array()
        updated_queues[index] = updated_queue
        updated_handler = { updated_handler | queues: updated_queues }
      }
      None => {}
    }
    
    updated_handler
  }
  
  // 测试背压处理
  let backpressure_config = BackpressureConfig{
    strategy: DropOldest,
    max_queue_size: 10,
    buffer_size: 5,
    throttle_rate: 5,
    rejection_threshold: 0.8
  }
  
  let backpressure_handler = create_backpressure_handler(backpressure_config)
  
  // 创建测试队列
  let test_queue = create_async_queue("queue-1", 10)
  let handler_with_queue = { backpressure_handler | queues: [test_queue] }
  
  // 添加项目（超过队列容量）
  let new_items = [
    "item-1", "item-2", "item-3", "item-4", "item-5",
    "item-6", "item-7", "item-8", "item-9", "item-10",
    "item-11", "item-12", "item-13", "item-14", "item-15"
  ]
  
  let current_time = 1640995200000000L
  let handler_after_backpressure = handle_backpressure(
    handler_with_queue,
    "queue-1",
    new_items,
    current_time
  )
  
  // 验证背压处理结果
  let updated_queue = handler_after_backpressure.queues[0]
  
  // DropOldest策略：应该保留最新的10个项目
  assert_eq(updated_queue.items.length(), 10)
  assert_eq(updated_queue.items[0], "item-6")  // 最旧的被丢弃
  assert_eq(updated_queue.items[9], "item-15") // 最新的被保留
  assert_eq(updated_queue.dropped_items, 5)    // 5个项目被丢弃
  assert_eq(handler_after_backpressure.total_dropped, 5)
  
  // 测试不同策略
  let drop_newest_config = { backpressure_config | strategy = DropNewest }
  let drop_newest_handler = create_backpressure_handler(drop_newest_config)
  let drop_newest_with_queue = { drop_newest_handler | queues: [create_async_queue("queue-2", 10)] }
  
  let drop_newest_result = handle_backpressure(
    drop_newest_with_queue,
    "queue-2",
    new_items,
    current_time
  )
  
  let drop_newest_queue = drop_newest_result.queues[0]
  
  // DropNewest策略：应该保留最旧的10个项目
  assert_eq(drop_newest_queue.items.length(), 10)
  assert_eq(drop_newest_queue.items[0], "item-1")   // 最旧的被保留
  assert_eq(drop_newest_queue.items[9], "item-10")  // 最新的被丢弃
  assert_eq(drop_newest_queue.dropped_items, 5)     // 5个项目被丢弃
  
  // 测试限流策略
  let throttle_config = { backpressure_config | strategy = Throttle }
  let throttle_handler = create_backpressure_handler(throttle_config)
  let throttle_with_queue = { throttle_handler | queues: [create_async_queue("queue-3", 10)] }
  
  let throttle_result = handle_backpressure(
    throttle_with_queue,
    "queue-3",
    new_items,
    current_time + 2000000000L  // 2秒后
  )
  
  let throttle_queue = throttle_result.queues[0]
  
  // Throttle策略：根据时间间隔限流
  assert_eq(throttle_queue.items.length() <= 10, true)
  assert_eq(throttle_queue.rejected_items > 0, true)
  assert_eq(throttle_result.total_rejected > 0, true)
}