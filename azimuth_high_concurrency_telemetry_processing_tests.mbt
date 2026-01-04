// Azimuth 高并发遥测处理测试用例
// 包含高并发环境下的遥测数据处理、缓冲和批量操作

// 测试1: 并发Span生成和处理
test "并发Span生成和处理" {
  // 定义并发工作器
  type Worker = {
    id: Int,
    spans_generated: Int,
    spans_processed: Int,
    active_spans: Array[String]
  }
  
  // 定义Span池
  type SpanPool = {
    capacity: Int,
    pending_spans: Array[TelemetrySpan],
    processing_spans: Array[TelemetrySpan>,
    completed_spans: Array[TelemetrySpan>
  }
  
  // 创建工作器
  let create_worker = fn(id: Int) {
    {
      id,
      spans_generated: 0,
      spans_processed: 0,
      active_spans: []
    }
  }
  
  // 创建Span池
  let create_span_pool = fn(capacity: Int) {
    {
      capacity,
      pending_spans: [],
      processing_spans: [],
      completed_spans: []
    }
  }
  
  // 工作器生成Span
  let worker_generate_spans = fn(worker: Worker, count: Int) {
    let mut updated_worker = worker
    let mut new_spans = []
    
    for i in 0..count {
      let span_id = "span-" + worker.id.to_string() + "-" + i.to_string()
      let span = {
        trace_id: "trace-concurrent-" + worker.id.to_string(),
        span_id,
        parent_span_id: None,
        operation_name: "operation_" + i.to_string(),
        start_time: Time::now(),
        end_time: Time::now() + 100,
        status: "ok",
        attributes: [
          ("worker.id", worker.id.to_string()),
          ("span.index", i.to_string())
        ]
      }
      
      new_spans = new_spans.push(span)
      updated_worker.active_spans = updated_worker.active_spans.push(span_id)
      updated_worker.spans_generated = updated_worker.spans_generated + 1
    }
    
    (updated_worker, new_spans)
  }
  
  // 添加Span到池中
  let add_spans_to_pool = fn(pool: SpanPool, spans: Array[TelemetrySpan>) {
    let mut updated_pool = pool
    
    for span in spans {
      if updated_pool.pending_spans.length() < updated_pool.capacity {
        updated_pool.pending_spans = updated_pool.pending_spans.push(span)
      }
    }
    
    updated_pool
  }
  
  // 处理池中的Span
  let process_spans_from_pool = fn(pool: SpanPool, batch_size: Int) {
    let mut updated_pool = pool
    let mut processed_count = 0
    
    // 从待处理队列移动到处理队列
    let mut spans_to_process = []
    while updated_pool.pending_spans.length() > 0 && processed_count < batch_size {
      let span = updated_pool.pending_spans[0]
      updated_pool.pending_spans = updated_pool.pending_spans.slice(1)
      spans_to_process = spans_to_process.push(span)
      processed_count = processed_count + 1
    }
    
    // 模拟处理过程
    updated_pool.processing_spans = updated_pool.processing_spans + spans_to_process
    
    // 完成处理
    let mut completed_spans = []
    while updated_pool.processing_spans.length() > 0 {
      let span = updated_pool.processing_spans[0]
      updated_pool.processing_spans = updated_pool.processing_spans.slice(1)
      
      // 模拟处理时间
      let processed_span = {
        trace_id: span.trace_id,
        span_id: span.span_id,
        parent_span_id: span.parent_span_id,
        operation_name: span.operation_name,
        start_time: span.start_time,
        end_time: span.end_time,
        status: "processed",
        attributes: span.attributes + [("processed_at", Time::now().to_string())]
      }
      
      completed_spans = completed_spans.push(processed_span)
    }
    
    updated_pool.completed_spans = updated_pool.completed_spans + completed_spans
    (updated_pool, completed_spans.length())
  }
  
  // 模拟并发工作器操作
  let simulate_concurrent_workers = fn(worker_count: Int, spans_per_worker: Int, pool_capacity: Int) {
    let mut workers = []
    let mut pool = create_span_pool(pool_capacity)
    
    // 创建工作器
    for i in 0..worker_count {
      workers = workers.push(create_worker(i))
    }
    
    // 并发生成Span
    let mut all_spans = []
    for i in 0..workers.length() {
      let (worker, spans) = worker_generate_spans(workers[i], spans_per_worker)
      workers[i] = worker
      all_spans = all_spans + spans
    }
    
    // 添加所有Span到池
    pool = add_spans_to_pool(pool, all_spans)
    
    // 批量处理Span
    let mut total_processed = 0
    let batch_size = 10
    
    while pool.pending_spans.length() > 0 {
      let (updated_pool, processed_count) = process_spans_from_pool(pool, batch_size)
      pool = updated_pool
      total_processed = total_processed + processed_count
    }
    
    (workers, pool, total_processed)
  }
  
  // 测试并发Span生成和处理
  let (workers, pool, total_processed) = simulate_concurrent_workers(5, 20, 100)
  
  // 验证工作器状态
  assert_eq(workers.length(), 5)
  
  for worker in workers {
    assert_eq(worker.spans_generated, 20)
    assert_eq(worker.active_spans.length(), 20)
  }
  
  // 验证池状态
  assert_eq(pool.pending_spans.length(), 0)
  assert_eq(pool.processing_spans.length(), 0)
  assert_eq(pool.completed_spans.length(), total_processed)
  
  // 验证所有Span都被处理
  let expected_total_spans = 5 * 20  // 5个工作器，每个生成20个Span
  assert_eq(total_processed, expected_total_spans)
  assert_eq(pool.completed_spans.length(), expected_total_spans)
  
  // 验证处理的Span状态
  for span in pool.completed_spans {
    assert_eq(span.status, "processed")
    assert_true(span.attributes.any(fn(attr) { attr.0 == "processed_at" }))
  }
  
  // 测试高并发场景
  let (high_workers, high_pool, high_processed) = simulate_concurrent_workers(10, 50, 500)
  
  assert_eq(high_workers.length(), 10)
  assert_eq(high_processed, 500)  // 10个工作器，每个生成50个Span
  assert_eq(high_pool.completed_spans.length(), 500)
}

// 测试2: 并发缓冲区管理
test "并发缓冲区管理" {
  // 定义缓冲区状态
  enum BufferState {
    Empty
    Filling
    Full
    Flushing
    Error
  }
  
  // 定义并发缓冲区
  type ConcurrentBuffer = {
    id: String,
    capacity: Int,
    items: Array[TelemetrySpan>,
    state: BufferState,
    last_flush_time: Int,
    flush_count: Int
  }
  
  // 创建并发缓冲区
  let create_buffer = fn(id: String, capacity: Int) {
    {
      id,
      capacity,
      items: [],
      state: BufferState::Empty,
      last_flush_time: Time::now(),
      flush_count: 0
    }
  }
  
  // 添加项目到缓冲区
  let add_to_buffer = fn(buffer: ConcurrentBuffer, item: TelemetrySpan) {
    let mut updated_buffer = buffer
    
    match buffer.state {
      BufferState::Empty | BufferState::Filling => {
        if buffer.items.length() < buffer.capacity {
          updated_buffer.items = updated_buffer.items.push(item)
          updated_buffer.state = if updated_buffer.items.length() == updated_buffer.capacity {
            BufferState::Full
          } else {
            BufferState::Filling
          }
        } else {
          updated_buffer.state = BufferState::Full
        }
      }
      _ => {
        // 不在可填充状态下，不添加项目
      }
    }
    
    updated_buffer
  }
  
  // 批量添加项目到缓冲区
  let batch_add_to_buffer = fn(buffer: ConcurrentBuffer, items: Array[TelemetrySpan>) {
    let mut updated_buffer = buffer
    
    for item in items {
      updated_buffer = add_to_buffer(updated_buffer, item)
    }
    
    updated_buffer
  }
  
  // 刷新缓冲区
  let flush_buffer = fn(buffer: ConcurrentBuffer) {
    let mut updated_buffer = buffer
    
    match buffer.state {
      BufferState::Full | BufferState::Filling => {
        updated_buffer.state = BufferState::Flushing
        
        // 模拟刷新操作
        let flushed_items = updated_buffer.items
        updated_buffer.items = []
        updated_buffer.state = BufferState::Empty
        updated_buffer.last_flush_time = Time::now()
        updated_buffer.flush_count = updated_buffer.flush_count + 1
        
        (updated_buffer, flushed_items)
      }
      BufferState::Empty => {
        (updated_buffer, [])
      }
      _ => {
        (updated_buffer, [])
      }
    }
  }
  
  // 创建缓冲区管理器
  type BufferManager = {
    buffers: Array[ConcurrentBuffer>,
    total_capacity: Int,
    total_items: Int,
    total_flushes: Int
  }
  
  // 创建缓冲区管理器
  let create_buffer_manager = fn(buffer_count: Int, buffer_capacity: Int) {
    let mut buffers = []
    for i in 0..buffer_count {
      buffers = buffers.push(create_buffer("buffer-" + i.to_string(), buffer_capacity))
    }
    
    {
      buffers,
      total_capacity: buffer_count * buffer_capacity,
      total_items: 0,
      total_flushes: 0
    }
  }
  
  // 添加项目到最佳缓冲区
  let add_to_best_buffer = fn(manager: BufferManager, item: TelemetrySpan) {
    let mut updated_manager = manager
    let mut best_buffer_index = -1
    let mut min_items = 999999
    
    // 找到项目最少的缓冲区
    for i in 0..manager.buffers.length() {
      let buffer = manager.buffers[i]
      if buffer.state != BufferState::Full && buffer.state != BufferState::Flushing {
        if buffer.items.length() < min_items {
          min_items = buffer.items.length()
          best_buffer_index = i
        }
      }
    }
    
    // 添加到最佳缓冲区
    if best_buffer_index >= 0 {
      let updated_buffer = add_to_buffer(manager.buffers[best_buffer_index], item)
      updated_manager.buffers[best_buffer_index] = updated_buffer
      updated_manager.total_items = updated_manager.total_items + 1
    }
    
    updated_manager
  }
  
  // 批量添加项目到管理器
  let batch_add_to_manager = fn(manager: BufferManager, items: Array[TelemetrySpan>) {
    let mut updated_manager = manager
    
    for item in items {
      updated_manager = add_to_best_buffer(updated_manager, item)
    }
    
    updated_manager
  }
  
  // 刷新所有已满的缓冲区
  let flush_full_buffers = fn(manager: BufferManager) {
    let mut updated_manager = manager
    let mut all_flushed_items = []
    
    for i in 0..manager.buffers.length() {
      let buffer = updated_manager.buffers[i]
      
      if buffer.state == BufferState::Full {
        let (updated_buffer, flushed_items) = flush_buffer(buffer)
        updated_manager.buffers[i] = updated_buffer
        updated_manager.total_flushes = updated_manager.total_flushes + 1
        all_flushed_items = all_flushed_items + flushed_items
      }
    }
    
    (updated_manager, all_flushed_items)
  }
  
  // 刷新所有缓冲区
  let flush_all_buffers = fn(manager: BufferManager) {
    let mut updated_manager = manager
    let mut all_flushed_items = []
    
    for i in 0..manager.buffers.length() {
      let buffer = updated_manager.buffers[i]
      let (updated_buffer, flushed_items) = flush_buffer(buffer)
      updated_manager.buffers[i] = updated_buffer
      
      if flushed_items.length() > 0 {
        updated_manager.total_flushes = updated_manager.total_flushes + 1
      }
      
      all_flushed_items = all_flushed_items + flushed_items
    }
    
    (updated_manager, all_flushed_items)
  }
  
  // 生成测试Span
  let generate_test_spans = fn(count: Int, worker_id: Int) {
    let mut spans = []
    
    for i in 0..count {
      let span = {
        trace_id: "trace-buffer-" + worker_id.to_string(),
        span_id: "span-" + worker_id.to_string() + "-" + i.to_string(),
        parent_span_id: None,
        operation_name: "buffer_operation_" + i.to_string(),
        start_time: Time::now(),
        end_time: Time::now() + 50,
        status: "ok",
        attributes: [
          ("worker.id", worker_id.to_string()),
          ("buffer.test", "true")
        ]
      }
      
      spans = spans.push(span)
    }
    
    spans
  }
  
  // 测试并发缓冲区管理
  let manager = create_buffer_manager(3, 10)  // 3个缓冲区，每个容量10
  
  // 生成测试数据
  let test_spans = generate_test_spans(25, 1)  // 25个Span，超过单个缓冲区容量
  
  // 批量添加到管理器
  let updated_manager = batch_add_to_manager(manager, test_spans)
  
  // 验证项目分布
  let mut total_items = 0
  for buffer in updated_manager.buffers {
    total_items = total_items + buffer.items.length()
  }
  
  assert_eq(total_items, 25)
  assert_eq(updated_manager.total_items, 25)
  
  // 刷新已满的缓冲区
  let (manager_after_flush, flushed_items) = flush_full_buffers(updated_manager)
  
  // 验证刷新结果
  assert_true(flushed_items.length() > 0)
  assert_true(manager_after_flush.total_flushes > 0)
  
  // 刷新所有缓冲区
  let (final_manager, all_flushed_items) = flush_all_buffers(manager_after_flush)
  
  // 验证所有项目都被刷新
  assert_eq(all_flushed_items.length(), 25)
  
  // 验证所有缓冲区都是空的
  for buffer in final_manager.buffers {
    assert_eq(buffer.state, BufferState::Empty)
    assert_eq(buffer.items.length(), 0)
  }
  
  // 测试高并发场景
  let high_manager = create_buffer_manager(5, 20)  // 5个缓冲区，每个容量20
  
  // 生成大量测试数据
  let high_test_spans = generate_test_spans(100, 2)  // 100个Span
  
  // 批量添加到高并发管理器
  let high_updated_manager = batch_add_to_manager(high_manager, high_test_spans)
  
  // 验证项目分布
  let mut high_total_items = 0
  for buffer in high_updated_manager.buffers {
    high_total_items = high_total_items + buffer.items.length()
  }
  
  assert_eq(high_total_items, 100)
  
  // 多次刷新操作
  let mut current_manager = high_updated_manager
  let mut total_flushed_overall = 0
  
  // 执行3轮刷新
  for round in 0..3 {
    let (next_manager, round_flushed) = flush_full_buffers(current_manager)
    current_manager = next_manager
    total_flushed_overall = total_flushed_overall + round_flushed.length()
    
    // 在轮次之间添加新数据
    if round < 2 {
      let additional_spans = generate_test_spans(20, 3 + round)
      current_manager = batch_add_to_manager(current_manager, additional_spans)
    }
  }
  
  // 最后刷新所有缓冲区
  let (final_high_manager, final_flushed) = flush_all_buffers(current_manager)
  total_flushed_overall = total_flushed_overall + final_flushed.length()
  
  // 验证高并发场景结果
  assert_eq(total_flushed_overall, 160)  // 100 + 20 + 20 + 20
  assert_eq(final_high_manager.total_flushes > 0, true)
}

// 测试3: 并发度量聚合
test "并发度量聚合" {
  // 定义度量点
  type MetricPoint = {
    name: String,
    value: Float,
    timestamp: Int,
    attributes: Array<(String, String)>
  }
  
  // 定义聚合器
  type MetricAggregator = {
    name: String,
    points: Array[MetricPoint],
    count: Int,
    sum: Float,
    min: Float,
    max: Float,
    last_update: Int
  }
  
  // 创建聚合器
  let create_aggregator = fn(name: String) {
    {
      name,
      points: [],
      count: 0,
      sum: 0.0,
      min: 0.0,
      max: 0.0,
      last_update: Time::now()
    }
  }
  
  // 添加度量点到聚合器
  let add_point = fn(aggregator: MetricAggregator, point: MetricPoint) {
    let mut updated_aggregator = aggregator
    
    updated_aggregator.points = updated_aggregator.points.push(point)
    updated_aggregator.count = updated_aggregator.count + 1
    updated_aggregator.sum = updated_aggregator.sum + point.value
    updated_aggregator.last_update = Time::now()
    
    if updated_aggregator.count == 1 {
      updated_aggregator.min = point.value
      updated_aggregator.max = point.value
    } else {
      updated_aggregator.min = if point.value < updated_aggregator.min { 
        point.value 
      } else { 
        updated_aggregator.min 
      }
      updated_aggregator.max = if point.value > updated_aggregator.max { 
        point.value 
      } else { 
        updated_aggregator.max 
      }
    }
    
    updated_aggregator
  }
  
  // 批量添加度量点
  let batch_add_points = fn(aggregator: MetricAggregator, points: Array[MetricPoint>) {
    let mut updated_aggregator = aggregator
    
    for point in points {
      updated_aggregator = add_point(updated_aggregator, point)
    }
    
    updated_aggregator
  }
  
  // 计算平均值
  let calculate_average = fn(aggregator: MetricAggregator) {
    if aggregator.count > 0 {
      aggregator.sum / (aggregator.count as Float)
    } else {
      0.0
    }
  }
  
  // 定义聚合管理器
  type AggregationManager = {
    aggregators: Array[MetricAggregator>,
    total_points: Int,
    last_aggregation: Int
  }
  
  // 创建聚合管理器
  let create_aggregation_manager = fn(metric_names: Array<String>) {
    let mut aggregators = []
    
    for name in metric_names {
      aggregators = aggregators.push(create_aggregator(name))
    }
    
    {
      aggregators,
      total_points: 0,
      last_aggregation: Time::now()
    }
  }
  
  // 添加度量点到管理器
  let add_point_to_manager = fn(manager: AggregationManager, point: MetricPoint) {
    let mut updated_manager = manager
    
    // 找到对应的聚合器
    let mut aggregator_index = -1
    for i in 0..manager.aggregators.length() {
      if manager.aggregators[i].name == point.name {
        aggregator_index = i
        break
      }
    }
    
    // 如果找到聚合器，添加点
    if aggregator_index >= 0 {
      let updated_aggregator = add_point(manager.aggregators[aggregator_index], point)
      updated_manager.aggregators[aggregator_index] = updated_aggregator
      updated_manager.total_points = updated_manager.total_points + 1
    } else {
      // 创建新的聚合器
      let new_aggregator = add_point(create_aggregator(point.name), point)
      updated_manager.aggregators = updated_manager.aggregators.push(new_aggregator)
      updated_manager.total_points = updated_manager.total_points + 1
    }
    
    updated_manager
  }
  
  // 批量添加度量点到管理器
  let batch_add_points_to_manager = fn(manager: AggregationManager, points: Array[MetricPoint>) {
    let mut updated_manager = manager
    
    for point in points {
      updated_manager = add_point_to_manager(updated_manager, point)
    }
    
    updated_manager
  }
  
  // 获取聚合结果
  let get_aggregation_result = fn(manager: AggregationManager, metric_name: String) {
    for aggregator in manager.aggregators {
      if aggregator.name == metric_name {
        return {
          name: aggregator.name,
          count: aggregator.count,
          sum: aggregator.sum,
          min: aggregator.min,
          max: aggregator.max,
          avg: calculate_average(aggregator)
        }
      }
    }
    
    // 如果没找到，返回空结果
    {
      name: metric_name,
      count: 0,
      sum: 0.0,
      min: 0.0,
      max: 0.0,
      avg: 0.0
    }
  }
  
  // 生成测试度量点
  let generate_metric_points = fn(metric_name: String, count: Int, worker_id: Int) {
    let mut points = []
    
    for i in 0..count {
      let point = {
        name: metric_name,
        value: (worker_id * 100 + i * 10) as Float,
        timestamp: Time::now() + i,
        attributes: [
          ("worker.id", worker_id.to_string()),
          ("metric.index", i.to_string())
        ]
      }
      
      points = points.push(point)
    }
    
    points
  }
  
  // 测试并发度量聚合
  let manager = create_aggregation_manager(["request.duration", "response.size"])
  
  // 生成测试数据
  let duration_points = generate_metric_points("request.duration", 20, 1)
  let size_points = generate_metric_points("response.size", 15, 1)
  
  // 批量添加到管理器
  let updated_manager = batch_add_points_to_manager(manager, duration_points + size_points)
  
  // 验证聚合结果
  assert_eq(updated_manager.total_points, 35)
  
  let duration_result = get_aggregation_result(updated_manager, "request.duration")
  assert_eq(duration_result.name, "request.duration")
  assert_eq(duration_result.count, 20)
  assert_eq(duration_result.min, 100.0)
  assert_eq(duration_result.max, 290.0)  // 100 + 19 * 10
  assert_eq(duration_result.sum, 3900.0)  // sum of 100, 110, 120, ..., 290
  
  let size_result = get_aggregation_result(updated_manager, "response.size")
  assert_eq(size_result.name, "response.size")
  assert_eq(size_result.count, 15)
  assert_eq(size_result.min, 100.0)
  assert_eq(size_result.max, 240.0)  // 100 + 14 * 10
  assert_eq(size_result.sum, 2550.0)  // sum of 100, 110, 120, ..., 240
  
  // 测试高并发场景
  let high_manager = create_aggregation_manager([
    "cpu.usage", 
    "memory.usage", 
    "network.io", 
    "disk.io"
  ])
  
  // 模拟多个工作器并发生成度量
  let mut current_manager = high_manager
  
  for worker_id in 0..5 {
    let cpu_points = generate_metric_points("cpu.usage", 10, worker_id)
    let memory_points = generate_metric_points("memory.usage", 10, worker_id)
    let network_points = generate_metric_points("network.io", 5, worker_id)
    let disk_points = generate_metric_points("disk.io", 5, worker_id)
    
    let all_points = cpu_points + memory_points + network_points + disk_points
    current_manager = batch_add_points_to_manager(current_manager, all_points)
  }
  
  // 验证高并发聚合结果
  assert_eq(current_manager.total_points, 150)  // 5 workers * (10 + 10 + 5 + 5) points
  
  let cpu_result = get_aggregation_result(current_manager, "cpu.usage")
  assert_eq(cpu_result.count, 50)  // 5 workers * 10 points
  
  let memory_result = get_aggregation_result(current_manager, "memory.usage")
  assert_eq(memory_result.count, 50)  // 5 workers * 10 points
  
  let network_result = get_aggregation_result(current_manager, "network.io")
  assert_eq(network_result.count, 25)  // 5 workers * 5 points
  
  let disk_result = get_aggregation_result(current_manager, "disk.io")
  assert_eq(disk_result.count, 25)  // 5 workers * 5 points
  
  // 验证聚合器数量
  assert_eq(current_manager.aggregators.length(), 4)
  
  // 验证每个聚合器的点数总和等于总点数
  let mut total_from_aggregators = 0
  for aggregator in current_manager.aggregators {
    total_from_aggregators = total_from_aggregators + aggregator.count
  }
  
  assert_eq(total_from_aggregators, current_manager.total_points)
}

// 测试4: 并发采样策略
test "并发采样策略" {
  // 定义采样决策
  type SamplingDecision = {
    trace_id: String,
    sampled: Bool,
    reason: String
  }
  
  // 定义采样器
  type Sampler = {
    name: String,
    sample_rate: Float,
    sampled_count: Int,
    total_count: Int
  }
  
  // 创建采样器
  let create_sampler = fn(name: String, sample_rate: Float) {
    {
      name,
      sample_rate,
      sampled_count: 0,
      total_count: 0
    }
  }
  
  // 基于Trace ID的采样决策
  let make_sampling_decision = fn(sampler: Sampler, trace_id: String) {
    // 简化的采样算法：基于trace_id的哈希值
    let hash = trace_id.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
    let normalized = (hash % 100) as Float / 100.0
    
    let sampled = normalized <= sampler.sample_rate
    let reason = if sampled {
      "trace_id_hash_within_rate"
    } else {
      "trace_id_hash_exceeds_rate"
    }
    
    {
      trace_id,
      sampled,
      reason
    }
  }
  
  // 更新采样器统计
  let update_sampler_stats = fn(sampler: Sampler, decision: SamplingDecision) {
    let mut updated_sampler = sampler
    updated_sampler.total_count = updated_sampler.total_count + 1
    
    if decision.sampled {
      updated_sampler.sampled_count = updated_sampler.sampled_count + 1
    }
    
    updated_sampler
  }
  
  // 定义采样管理器
  type SamplingManager = {
    samplers: Array[Sampler>,
    total_decisions: Int,
    total_sampled: Int
  }
  
  // 创建采样管理器
  let create_sampling_manager = fn(sampler_configs: Array<(String, Float)>) {
    let mut samplers = []
    
    for (name, rate) in sampler_configs {
      samplers = samplers.push(create_sampler(name, rate))
    }
    
    {
      samplers,
      total_decisions: 0,
      total_sampled: 0
    }
  }
  
  // 使用指定采样器做决策
  let make_decision_with_sampler = fn(manager: SamplingManager, sampler_name: String, trace_id: String) {
    let mut updated_manager = manager
    let sampler_index = manager.samplers.find_index(fn(s) { s.name == sampler_name })
    
    match sampler_index {
      Some(index) => {
        let sampler = manager.samplers[index]
        let decision = make_sampling_decision(sampler, trace_id)
        let updated_sampler = update_sampler_stats(sampler, decision)
        
        updated_manager.samplers[index] = updated_sampler
        updated_manager.total_decisions = updated_manager.total_decisions + 1
        
        if decision.sampled {
          updated_manager.total_sampled = updated_manager.total_sampled + 1
        }
        
        (updated_manager, decision)
      }
      None => {
        // 默认不采样
        (updated_manager, {
          trace_id,
          sampled: false,
          reason: "sampler_not_found"
        })
      }
    }
  }
  
  // 批量采样决策
  let batch_sampling_decisions = fn(manager: SamplingManager, sampler_name: String, trace_ids: Array<String>) {
    let mut updated_manager = manager
    let mut decisions = []
    
    for trace_id in trace_ids {
      let (next_manager, decision) = make_decision_with_sampler(updated_manager, sampler_name, trace_id)
      updated_manager = next_manager
      decisions = decisions.push(decision)
    }
    
    (updated_manager, decisions)
  }
  
  // 生成测试Trace ID
  let generate_trace_ids = fn(count: Int, prefix: String) {
    let mut trace_ids = []
    
    for i in 0..count {
      let trace_id = prefix + "-" + i.to_string()
      trace_ids = trace_ids.push(trace_id)
    }
    
    trace_ids
  }
  
  // 测试并发采样策略
  let manager = create_sampling_manager([
    ("default", 0.5),
    ("high_rate", 0.8),
    ("low_rate", 0.2)
  ])
  
  // 生成测试Trace ID
  let trace_ids = generate_trace_ids(100, "concurrent-test")
  
  // 使用不同采样器进行采样
  let (manager_after_default, default_decisions) = batch_sampling_decisions(manager, "default", trace_ids)
  let (manager_after_high, high_decisions) = batch_sampling_decisions(manager_after_default, "high_rate", trace_ids)
  let (final_manager, low_decisions) = batch_sampling_decisions(manager_after_high, "low_rate", trace_ids)
  
  // 验证采样决策数量
  assert_eq(default_decisions.length(), 100)
  assert_eq(high_decisions.length(), 100)
  assert_eq(low_decisions.length(), 100)
  
  // 验证采样率
  let default_sampler = final_manager.samplers.find(fn(s) { s.name == "default" }).unwrap()
  let high_sampler = final_manager.samplers.find(fn(s) { s.name == "high_rate" }).unwrap()
  let low_sampler = final_manager.samplers.find(fn(s) { s.name == "low_rate" }).unwrap()
  
  assert_eq(default_sampler.total_count, 100)
  assert_eq(high_sampler.total_count, 100)
  assert_eq(low_sampler.total_count, 100)
  
  // 验证采样数量在预期范围内（允许一定偏差）
  let default_sampled_rate = (default_sampler.sampled_count as Float) / (default_sampler.total_count as Float)
  let high_sampled_rate = (high_sampler.sampled_count as Float) / (high_sampler.total_count as Float)
  let low_sampled_rate = (low_sampler.sampled_count as Float) / (low_sampler.total_count as Float)
  
  assert_true(default_sampled_rate > 0.3 && default_sampled_rate < 0.7)  // 50% ± 20%
  assert_true(high_sampled_rate > 0.6 && high_sampled_rate < 1.0)     // 80% ± 20%
  assert_true(low_sampled_rate > 0.0 && low_sampled_rate < 0.4)       // 20% ± 20%
  
  // 验证总统计
  assert_eq(final_manager.total_decisions, 300)  // 3 samplers * 100 decisions
  
  // 测试高并发场景
  let high_manager = create_sampling_manager([
    ("production", 0.1),
    ("staging", 0.5),
    ("development", 1.0)
  ])
  
  // 生成大量Trace ID
  let high_trace_ids = generate_trace_ids(1000, "high-concurrency")
  
  // 并发采样
  let mut current_manager = high_manager
  
  for i in 0..10 {
    let batch_trace_ids = high_trace_ids.slice(i * 100, (i + 1) * 100)
    let (next_manager, _) = batch_sampling_decisions(current_manager, "production", batch_trace_ids)
    current_manager = next_manager
  }
  
  for i in 0..10 {
    let batch_trace_ids = high_trace_ids.slice(i * 100, (i + 1) * 100)
    let (next_manager, _) = batch_sampling_decisions(current_manager, "staging", batch_trace_ids)
    current_manager = next_manager
  }
  
  for i in 0..10 {
    let batch_trace_ids = high_trace_ids.slice(i * 100, (i + 1) * 100)
    let (next_manager, _) = batch_sampling_decisions(current_manager, "development", batch_trace_ids)
    current_manager = next_manager
  }
  
  // 验证高并发采样结果
  let prod_sampler = current_manager.samplers.find(fn(s) { s.name == "production" }).unwrap()
  let staging_sampler = current_manager.samplers.find(fn(s) { s.name == "staging" }).unwrap()
  let dev_sampler = current_manager.samplers.find(fn(s) { s.name == "development" }).unwrap()
  
  assert_eq(prod_sampler.total_count, 1000)
  assert_eq(staging_sampler.total_count, 1000)
  assert_eq(dev_sampler.total_count, 1000)
  
  // 开发环境应该100%采样
  assert_eq(dev_sampler.sampled_count, 1000)
  
  // 生产环境应该约10%采样
  let prod_sampled_rate = (prod_sampler.sampled_count as Float) / (prod_sampler.total_count as Float)
  assert_true(prod_sampled_rate > 0.05 && prod_sampled_rate < 0.15)
  
  // 验证总统计
  assert_eq(current_manager.total_decisions, 3000)  // 3 samplers * 1000 decisions
}