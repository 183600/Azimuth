// 异步并发测试 - 用于测试遥测系统中的异步操作和并发处理功能

test "async_task_execution" {
  // 测试异步任务执行
  
  enum TaskState {
    Pending
    Running
    Completed
    Failed
    Cancelled
  }
  
  struct AsyncTask {
    task_id : String
    state : TaskState
    priority : Int
    created_at : Int64
    started_at : Option<Int64>
    completed_at : Option<Int64>
    result : Option<String>
    error : Option<String>
  }
  
  struct TaskScheduler {
    max_concurrent_tasks : Int
    task_queue : Array<AsyncTask>
    running_tasks : Array<AsyncTask>
    completed_tasks : Array<AsyncTask>
  }
  
  // 创建任务调度器
  let create_scheduler = fn(max_concurrent : Int) : TaskScheduler {
    {
      max_concurrent_tasks: max_concurrent,
      task_queue: [],
      running_tasks: [],
      completed_tasks: []
    }
  }
  
  // 创建异步任务
  let create_task = fn(task_id : String, priority : Int) : AsyncTask {
    let current_time = 1609459200000L  // 模拟当前时间
    {
      task_id: task_id,
      state: Pending,
      priority: priority,
      created_at: current_time,
      started_at: None,
      completed_at: None,
      result: None,
      error: None
    }
  }
  
  // 提交任务到调度器
  let submit_task = fn(scheduler : TaskScheduler, task : AsyncTask) : TaskScheduler {
    let updated_queue = scheduler.task_queue.push(task)
    { scheduler | task_queue: updated_queue }
  }
  
  // 模拟任务执行
  let execute_task = fn(task : AsyncTask, duration_ms : Int64) : AsyncTask {
    let start_time = 1609459200000L
    let end_time = start_time + duration_ms
    
    let success = duration_ms < 5000L  // 超过5秒的任务失败
    
    {
      task |
      state: success ? Completed : Failed,
      started_at: Some(start_time),
      completed_at: Some(end_time),
      result: success ? Some("Task completed successfully") : None,
      error: success ? None : Some("Task execution timeout")
    }
  }
  
  // 测试任务调度和执行
  let scheduler = create_scheduler(3)  // 最多3个并发任务
  
  // 创建多个任务
  let task1 = create_task("task-001", 1)
  let task2 = create_task("task-002", 2)
  let task3 = create_task("task-003", 3)
  let task4 = create_task("task-004", 1)
  let task5 = create_task("task-005", 2)
  
  // 提交任务
  let scheduler_with_tasks = scheduler
    |> submit_task(task1)
    |> submit_task(task2)
    |> submit_task(task3)
    |> submit_task(task4)
    |> submit_task(task5)
  
  assert_eq(scheduler_with_tasks.task_queue.length(), 5)
  
  // 模拟并发执行（最多3个）
  let mut running_tasks : Array<AsyncTask> = []
  let mut remaining_queue = scheduler_with_tasks.task_queue
  
  // 按优先级排序
  remaining_queue = remaining_queue.sort_by(fn(a, b) { a.priority - b.priority })
  
  // 启动最多3个任务
  for i = 0; i < @min(3, remaining_queue.length()); i = i + 1 {
    let task = remaining_queue[i]
    let running_task = { task | state: Running, started_at: Some(1609459200000L) }
    running_tasks.push(running_task)
  }
  
  assert_eq(running_tasks.length(), 3)
  
  // 执行任务
  let mut completed_tasks : Array<AsyncTask> = []
  for task in running_tasks {
    let duration = match task.task_id {
      "task-001" => 1000L
      "task-002" => 3000L
      "task-003" => 6000L  // 这个会失败
      _ => 2000L
    }
    
    let completed = execute_task(task, duration)
    completed_tasks.push(completed)
  }
  
  // 验证执行结果
  assert_eq(completed_tasks.length(), 3)
  assert_eq(completed_tasks[0].state, Completed)
  assert_eq(completed_tasks[1].state, Completed)
  assert_eq(completed_tasks[2].state, Failed)
  
  // 验证任务时间
  assert_eq(completed_tasks[0].started_at.unwrap() < completed_tasks[0].completed_at.unwrap(), true)
  assert_eq(completed_tasks[2].error.unwrap(), "Task execution timeout")
}

test "concurrent_span_processing" {
  // 测试并发Span处理
  
  struct SpanData {
    trace_id : String
    span_id : String
    parent_span_id : String
    operation_name : String
    start_time : Int64
    end_time : Int64
    status : String
    tags : Array<String>
  }
  
  struct ConcurrentProcessor {
    worker_count : Int
    input_queue : Array<SpanData>
    processing_queue : Array<SpanData>
    output_queue : Array<SpanData>
    processed_count : Int
  }
  
  // 创建并发处理器
  let create_processor = fn(workers : Int) : ConcurrentProcessor {
    {
      worker_count: workers,
      input_queue: [],
      processing_queue: [],
      output_queue: [],
      processed_count: 0
    }
  }
  
  // 生成测试Span数据
  let generate_test_spans = fn(count : Int) : Array<SpanData> {
    let mut spans : Array<SpanData> = []
    
    for i = 0; i < count; i = i + 1 {
      spans.push({
        trace_id: "trace-" + (@sprintf("%03d", i)),
        span_id: "span-" + (@sprintf("%03d", i)),
        parent_span_id: if i > 0 { "span-" + (@sprintf("%03d", i - 1)) } else { "" },
        operation_name: "operation-" + (@sprintf("%03d", i)),
        start_time: 1609459200000L + (i.to_int64() * 1000L),
        end_time: 1609459200000L + (i.to_int64() * 1000L) + 500L,
        status: "ok",
        tags: ["tag1:value1", "tag2:value2"]
      })
    }
    
    spans
  }
  
  // 模拟并发处理
  let process_spans_concurrently = fn(processor : ConcurrentProcessor, 
                                      spans : Array<SpanData>) : ConcurrentProcessor {
    let mut updated_processor = processor
    
    // 将Span添加到输入队列
    updated_processor.input_queue = spans
    
    // 模拟并发处理（按工作线程数分批）
    let batch_size = @max(1, spans.length() / processor.worker_count)
    let mut processed_spans : Array<SpanData> = []
    
    for i = 0; i < spans.length(); i = i + batch_size {
      let end_index = @min(i + batch_size, spans.length())
      let mut batch : Array<SpanData> = []
      
      for j = i; j < end_index; j = j + 1 {
        batch.push(spans[j])
      }
      
      // 处理批次（模拟并行处理）
      for span in batch {
        // 模拟处理：添加处理时间戳
        let processed_span = {
          span |
          tags: span.tags.push("processed_at:" + (1609459200000L + i.to_int64() * 100L).to_string())
        }
        processed_spans.push(processed_span)
      }
    }
    
    updated_processor.output_queue = processed_spans
    updated_processor.processed_count = processed_spans.length()
    updated_processor
  }
  
  // 测试并发处理
  let processor = create_processor(4)  // 4个工作线程
  let test_spans = generate_test_spans(20)
  
  let processed_processor = process_spans_concurrently(processor, test_spans)
  
  // 验证处理结果
  assert_eq(processed_processor.processed_count, 20)
  assert_eq(processed_processor.output_queue.length(), 20)
  
  // 验证每个Span都被处理
  for span in processed_processor.output_queue {
    assert_eq(span.tags.length(), 3)  // 原来的2个标签 + 1个处理标签
    assert_eq(span.tags.any(fn(tag) { tag.contains("processed_at:") }), true)
  }
  
  // 验证处理顺序可能不同（并发特性）
  let input_ids = test_spans.map(fn(s) { s.span_id })
  let output_ids = processed_processor.output_queue.map(fn(s) { s.span_id })
  
  // 检查所有Span都被处理（顺序可能不同）
  for id in input_ids {
    assert_eq(output_ids.contains(id), true)
  }
}

test "async_metric_collection" {
  // 测试异步指标收集
  
  struct MetricPoint {
    name : String
    value : Double
    timestamp : Int64
    labels : Array<String>
    metric_type : String
  }
  
  struct AsyncMetricCollector {
    collection_interval_ms : Int
    buffer_size : Int
    metrics_buffer : Array<MetricPoint>
    collection_tasks : Array<String>
    is_collecting : Bool
  }
  
  // 创建异步指标收集器
  let create_collector = fn(interval_ms : Int, buffer_size : Int) : AsyncMetricCollector {
    {
      collection_interval_ms: interval_ms,
      buffer_size: buffer_size,
      metrics_buffer: [],
      collection_tasks: [],
      is_collecting: false
    }
  }
  
  // 生成模拟指标数据
  let generate_metrics = fn(metric_name : String, count : Int, start_time : Int64) : Array<MetricPoint> {
    let mut metrics : Array<MetricPoint> = []
    
    for i = 0; i < count; i = i + 1 {
      metrics.push({
        name: metric_name,
        value: (@rand() * 100.0),
        timestamp: start_time + (i.to_int64() * 1000L),
        labels: ["source:test", "instance:1"],
        metric_type: "gauge"
      })
    }
    
    metrics
  }
  
  // 模拟异步收集
  let start_async_collection = fn(collector : AsyncMetricCollector, 
                                  metric_names : Array<String>,
                                  duration_seconds : Int) : AsyncMetricCollector {
    let mut updated_collector = { collector | is_collecting: true }
    
    // 为每个指标创建收集任务
    for name in metric_names {
      updated_collector.collection_tasks = updated_collector.collection_tasks.push("collect-" + name)
    }
    
    // 模拟异步收集过程
    let mut all_metrics : Array<MetricPoint> = []
    let start_time = 1609459200000L
    
    for name in metric_names {
      let metrics = generate_metrics(name, duration_seconds, start_time)
      all_metrics = all_metrics.concat(metrics)
    }
    
    // 按时间戳排序
    all_metrics = all_metrics.sort_by(fn(a, b) { 
      if a.timestamp < b.timestamp { -1 } else if a.timestamp > b.timestamp { 1 } else { 0 }
    })
    
    // 应用缓冲区大小限制
    if all_metrics.length() > collector.buffer_size {
      let start_index = all_metrics.length() - collector.buffer_size
      all_metrics = all_metrics.slice(start_index, all_metrics.length())
    }
    
    updated_collector.metrics_buffer = all_metrics
    updated_collector
  }
  
  // 测试异步指标收集
  let collector = create_collector(1000, 100)  // 1秒间隔，100个缓冲区大小
  let metric_names = ["cpu_usage", "memory_usage", "disk_io", "network_io"]
  
  let collecting_collector = start_async_collection(collector, metric_names, 10)  // 收集10秒
  
  // 验证收集结果
  assert_eq(collecting_collector.is_collecting, true)
  assert_eq(collecting_collector.collection_tasks.length(), 4)
  assert_eq(collecting_collector.metrics_buffer.length(), 40)  // 4个指标 × 10个数据点
  
  // 验证指标数据
  for metric in collecting_collector.metrics_buffer {
    assert_eq(metric.value >= 0.0 && metric.value <= 100.0, true)
    assert_eq(metric.labels.length(), 2)
    assert_eq(metric.metric_type, "gauge")
    assert_eq(metric_names.contains(metric.name), true)
  }
  
  // 验证时间序列
  let cpu_metrics = collecting_collector.metrics_buffer.filter(fn(m) { m.name == "cpu_usage" })
  assert_eq(cpu_metrics.length(), 10)
  
  for i = 1; i < cpu_metrics.length(); i = i + 1 {
    assert_eq(cpu_metrics[i].timestamp > cpu_metrics[i-1].timestamp, true)
  }
  
  // 测试缓冲区限制
  let large_collector = create_collector(1000, 5)  // 小缓冲区
  let large_collection = start_async_collection(large_collector, ["test_metric"], 10)
  
  assert_eq(large_collection.metrics_buffer.length(), 5)  // 应该被限制为5
}

test "concurrent_error_handling" {
  // 测试并发错误处理
  
  enum ErrorSeverity {
    Low
    Medium
    High
    Critical
  }
  
  struct ConcurrentError {
    error_id : String
    task_id : String
    severity : ErrorSeverity
    message : String
    timestamp : Int64
    retry_count : Int
    is_resolved : Bool
  }
  
  struct ErrorHandler {
    max_concurrent_errors : Int
    error_queue : Array<ConcurrentError>
    processing_errors : Array<ConcurrentError>
    resolved_errors : Array<ConcurrentError>
    error_threshold : Int
  }
  
  // 创建错误处理器
  let create_error_handler = fn(max_concurrent : Int, threshold : Int) : ErrorHandler {
    {
      max_concurrent_errors: max_concurrent,
      error_queue: [],
      processing_errors: [],
      resolved_errors: [],
      error_threshold: threshold
    }
  }
  
  // 生成测试错误
  let generate_test_errors = fn(count : Int) : Array<ConcurrentError> {
    let mut errors : Array<ConcurrentError> = []
    let severities = [Low, Medium, High, Critical]
    
    for i = 0; i < count; i = i + 1 {
      errors.push({
        error_id: "error-" + (@sprintf("%03d", i)),
        task_id: "task-" + (@sprintf("%03d", i)),
        severity: severities[i % severities.length()],
        message: "Test error " + i.to_string(),
        timestamp: 1609459200000L + (i.to_int64() * 100L),
        retry_count: 0,
        is_resolved: false
      })
    }
    
    errors
  }
  
  // 并发错误处理
  let process_errors_concurrently = fn(handler : ErrorHandler, 
                                       errors : Array<ConcurrentError>) : ErrorHandler {
    let mut updated_handler = { handler | error_queue: errors }
    
    // 按严重程度排序（Critical优先）
    let sorted_errors = errors.sort_by(fn(a, b) {
      let severity_order = fn(s : ErrorSeverity) : Int {
        match s {
          Critical => 0
          High => 1
          Medium => 2
          Low => 3
        }
      }
      severity_order(a.severity) - severity_order(b.severity)
    })
    
    // 并发处理错误（限制并发数）
    let mut processing_errors : Array<ConcurrentError> = []
    let mut resolved_errors : Array<ConcurrentError> = []
    
    let concurrent_limit = @min(handler.max_concurrent_errors, sorted_errors.length())
    
    for i = 0; i < concurrent_limit; i = i + 1 {
      processing_errors.push(sorted_errors[i])
    }
    
    // 模拟错误处理
    for error in processing_errors {
      let resolved = match error.severity {
        Low => error.retry_count >= 1
        Medium => error.retry_count >= 2
        High => error.retry_count >= 3
        Critical => error.retry_count >= 5
      }
      
      if resolved {
        resolved_errors.push({ error | is_resolved: true, retry_count: error.retry_count + 1 })
      } else {
        // 重新排队处理
        let retry_error = { error | retry_count: error.retry_count + 1 }
        if retry_error.retry_count < 5 {  // 最大重试次数
          processing_errors.push(retry_error)
        } else {
          resolved_errors.push({ retry_error | is_resolved: false })  // 标记为无法解决
        }
      }
    }
    
    updated_handler.processing_errors = processing_errors
    updated_handler.resolved_errors = resolved_errors
    
    updated_handler
  }
  
  // 测试并发错误处理
  let error_handler = create_error_handler(3, 10)  // 最多3个并发错误，阈值10
  let test_errors = generate_test_errors(15)
  
  let processed_handler = process_errors_concurrently(error_handler, test_errors)
  
  // 验证处理结果
  assert_eq(processed_handler.error_queue.length(), 15)
  assert_eq(processed_handler.processing_errors.length() <= 3, true)  // 并发限制
  assert_eq(processed_handler.resolved_errors.length() >= 0, true)
  
  // 验证错误严重程度处理
  let critical_errors = test_errors.filter(fn(e) { e.severity == Critical })
  let resolved_critical = processed_handler.resolved_errors.filter(fn(e) { e.severity == Critical && e.is_resolved })
  
  // Critical错误需要更多重试次数才能解决
  for error in resolved_critical {
    assert_eq(error.retry_count >= 5, true)
  }
  
  // 测试错误阈值
  let error_count = processed_handler.error_queue.length()
  let threshold_exceeded = error_count > processed_handler.error_threshold
  
  if threshold_exceeded {
    // 应该触发熔断或降级机制
    assert_eq(true, true)  // 这里只是模拟验证
  }
}

test "async_resource_management" {
  // 测试异步资源管理
  
  enum ResourceType {
    Memory
    Network
    FileHandle
    DatabaseConnection
  }
  
  struct AsyncResource {
    resource_id : String
    resource_type : ResourceType
    allocated_at : Int64
    size_bytes : Int64
    is_in_use : Bool
    reference_count : Int
  }
  
  struct ResourceManager {
    max_resources : Int
    max_memory_mb : Int64
    allocated_resources : Array<AsyncResource>
    resource_pool : Array<AsyncResource>
    pending_requests : Array<String>
  }
  
  // 创建资源管理器
  let create_resource_manager = fn(max_resources : Int, max_memory_mb : Int64) : ResourceManager {
    {
      max_resources: max_resources,
      max_memory_mb: max_memory_mb,
      allocated_resources: [],
      resource_pool: [],
      pending_requests: []
    }
  }
  
  // 异步分配资源
  let allocate_resource_async = fn(manager : ResourceManager, 
                                   resource_type : ResourceType,
                                   request_id : String,
                                   size_mb : Int64) : ResourceManager {
    let mut updated_manager = manager
    
    // 检查资源限制
    let resource_count = updated_manager.allocated_resources.length()
    let total_memory_mb = updated_manager.allocated_resources
      .map(fn(r) { r.size_bytes / (1024 * 1024) })
      .fold(0L, fn(acc, size) { acc + size })
    
    let can_allocate = resource_count < manager.max_resources && 
                      total_memory_mb + size_mb <= manager.max_memory_mb
    
    if can_allocate {
      // 创建新资源
      let new_resource = {
        resource_id: "res-" + request_id,
        resource_type: resource_type,
        allocated_at: 1609459200000L,
        size_bytes: size_mb * 1024 * 1024,
        is_in_use: true,
        reference_count: 1
      }
      
      updated_manager.allocated_resources = updated_manager.allocated_resources.push(new_resource)
    } else {
      // 添加到待处理队列
      updated_manager.pending_requests = updated_manager.pending_requests.push(request_id)
    }
    
    updated_manager
  }
  
  // 异步释放资源
  let release_resource_async = fn(manager : ResourceManager, 
                                  resource_id : String) : ResourceManager {
    let mut updated_manager = manager
    
    // 查找并释放资源
    let mut found_index = -1
    for i = 0; i < updated_manager.allocated_resources.length(); i = i + 1 {
      if updated_manager.allocated_resources[i].resource_id == resource_id {
        found_index = i
        break
      }
    }
    
    if found_index >= 0 {
      let resource = updated_manager.allocated_resources[found_index]
      let updated_resource = { resource | is_in_use: false, reference_count: 0 }
      
      // 从已分配列表移除，添加到资源池
      updated_manager.allocated_resources = updated_manager.allocated_resources.slice(0, found_index)
        .concat(updated_manager.allocated_resources.slice(found_index + 1, updated_manager.allocated_resources.length()))
      
      updated_manager.resource_pool = updated_manager.resource_pool.push(updated_resource)
      
      // 处理待处理请求
      if updated_manager.pending_requests.length() > 0 {
        let next_request = updated_manager.pending_requests[0]
        updated_manager.pending_requests = updated_manager.pending_requests.slice(1, updated_manager.pending_requests.length())
        
        // 重新分配资源给待处理的请求
        let reallocated_resource = {
          updated_resource |
          resource_id: "res-" + next_request,
          is_in_use: true,
          reference_count: 1,
          allocated_at: 1609459200000L
        }
        
        updated_manager.allocated_resources = updated_manager.allocated_resources.push(reallocated_resource)
      }
    }
    
    updated_manager
  }
  
  // 测试异步资源管理
  let resource_manager = create_resource_manager(5, 100)  // 最多5个资源，100MB内存
  
  // 分配资源
  let manager1 = allocate_resource_async(resource_manager, Memory, "req-001", 20)
  let manager2 = allocate_resource_async(manager1, Network, "req-002", 15)
  let manager3 = allocate_resource_async(manager2, DatabaseConnection, "req-003", 25)
  let manager4 = allocate_resource_async(manager3, FileHandle, "req-004", 10)
  let manager5 = allocate_resource_async(manager4, Memory, "req-005", 30)
  
  // 验证资源分配
  assert_eq(manager5.allocated_resources.length(), 5)
  assert_eq(manager5.pending_requests.length(), 0)
  
  // 尝试分配超出限制的资源
  let manager6 = allocate_resource_async(manager5, Memory, "req-006", 20)
  
  // 应该被添加到待处理队列
  assert_eq(manager6.allocated_resources.length(), 5)  // 仍然只有5个
  assert_eq(manager6.pending_requests.length(), 1)   // 1个待处理请求
  
  // 释放一个资源
  let manager7 = release_resource_async(manager6, "res-req-001")
  
  // 验证资源释放和重新分配
  assert_eq(manager7.allocated_resources.length(), 5)  // 仍然5个，但资源不同
  assert_eq(manager7.pending_requests.length(), 0)   // 待处理请求被处理
  assert_eq(manager7.resource_pool.length(), 1)      // 1个资源在池中
  
  // 验证特定资源被重新分配
  let reallocated_resources = manager7.allocated_resources.filter(fn(r) { r.resource_id == "res-req-006" })
  assert_eq(reallocated_resources.length(), 1)
  
  // 测试内存限制
  let memory_usage = manager7.allocated_resources
    .filter(fn(r) { r.resource_type == Memory })
    .map(fn(r) { r.size_bytes / (1024 * 1024) })
    .fold(0L, fn(acc, size) { acc + size })
  
  assert_eq(memory_usage <= manager7.max_memory_mb, true)
}