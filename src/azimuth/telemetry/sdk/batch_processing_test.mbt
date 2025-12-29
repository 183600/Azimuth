// 高级批处理机制测试用例
// 测试动态批处理、智能批处理策略、批处理优化等高级批处理功能

test "dynamic_batch_size_adjustment" {
  // 测试动态批处理大小调整
  
  struct SystemMetrics {
    cpu_usage: Float
    memory_usage: Float
    network_throughput: Int
    queue_depth: Int
    processing_latency_ms: Int
  }
  
  struct DynamicBatchConfig {
    min_batch_size: Int
    max_batch_size: Int
    target_latency_ms: Int
    adjustment_factor: Float
    evaluation_window_seconds: Int
  }
  
  struct DynamicBatchProcessor {
    current_batch_size: Int
    config: DynamicBatchConfig
    recent_metrics: Array<SystemMetrics>
    last_adjustment_time: Int64
    performance_history: Array<(Int64, Int, Int)>  // (timestamp, batch_size, latency_ms)
  }
  
  // 创建动态批处理配置
  let batch_config = DynamicBatchConfig{
    min_batch_size: 10,
    max_batch_size: 1000,
    target_latency_ms: 100,
    adjustment_factor: 0.2,
    evaluation_window_seconds: 30
  }
  
  // 创建动态批处理器
  let batch_processor = DynamicBatchProcessor{
    current_batch_size: 100,
    config: batch_config,
    recent_metrics: Array::empty(),
    last_adjustment_time: 1640995200000000L,
    performance_history: Array::empty()
  }
  
  // 计算新的批处理大小
  let calculate_optimal_batch_size = fn(processor: DynamicBatchProcessor, current_metrics: SystemMetrics) -> Int {
    let current_latency = current_metrics.processing_latency_ms
    let target_latency = processor.config.target_latency_ms
    let current_size = processor.current_batch_size
    
    let new_size = if current_latency > target_latency * 1.2 {
      // 延迟过高，减小批处理大小
      let reduction = (current_latency - target_latency).to_float() / target_latency.to_float()
      let adjusted_size = current_size.to_float() * (1.0 - reduction * processor.config.adjustment_factor)
      adjusted_size.max(processor.config.min_batch_size.to_float()).to_int()
    } else if current_latency < target_latency * 0.8 && current_metrics.cpu_usage < 0.8 {
      // 延迟较低且CPU有余量，增大批处理大小
      let increase = (target_latency - current_latency).to_float() / target_latency.to_float()
      let adjusted_size = current_size.to_float() * (1.0 + increase * processor.config.adjustment_factor)
      adjusted_size.min(processor.config.max_batch_size.to_float()).to_int()
    } else {
      // 延迟在目标范围内，保持当前大小
      current_size
    }
    
    // 考虑系统资源使用情况
    let resource_factor = if current_metrics.memory_usage > 0.9 || current_metrics.cpu_usage > 0.9 {
      0.7  // 高资源使用，减小批处理
    } else if current_metrics.memory_usage < 0.5 && current_metrics.cpu_usage < 0.5 {
      1.3  // 低资源使用，可以增大批处理
    } else {
      1.0  // 正常资源使用
    }
    
    let final_size = (new_size.to_float() * resource_factor).to_int()
    final_size.max(processor.config.min_batch_size).min(processor.config.max_batch_size)
  }
  
  // 测试不同系统负载场景
  let test_scenarios = [
    SystemMetrics{
      cpu_usage: 0.3,
      memory_usage: 0.4,
      network_throughput: 1000,
      queue_depth: 50,
      processing_latency_ms: 50  // 低延迟
    },
    SystemMetrics{
      cpu_usage: 0.6,
      memory_usage: 0.7,
      network_throughput: 2000,
      queue_depth: 100,
      processing_latency_ms: 100  // 目标延迟
    },
    SystemMetrics{
      cpu_usage: 0.8,
      memory_usage: 0.8,
      network_throughput: 3000,
      queue_depth: 200,
      processing_latency_ms: 150  // 高延迟
    },
    SystemMetrics{
      cpu_usage: 0.95,
      memory_usage: 0.9,
      network_throughput: 4000,
      queue_depth: 500,
      processing_latency_ms: 200  // 极高延迟
    }
  ]
  
  let mut current_size = batch_processor.current_batch_size
  
  let mut i = 0
  while i < test_scenarios.length() {
    let scenario = test_scenarios[i]
    let new_size = calculate_optimal_batch_size(batch_processor, scenario)
    
    // 验证批处理大小在合理范围内
    assert_eq(new_size >= batch_config.min_batch_size, true)
    assert_eq(new_size <= batch_config.max_batch_size, true)
    
    // 验证批处理大小调整趋势
    match i {
      0 => {
        // 低延迟场景，批处理大小应该增加
        assert_eq(new_size > current_size, true)
      }
      1 => {
        // 目标延迟场景，批处理大小应该相对稳定
        let size_change = abs(new_size - current_size)
        assert_eq(size_change <= current_size / 10, true)  // 变化不超过10%
      }
      2 => {
        // 高延迟场景，批处理大小应该减少
        assert_eq(new_size < current_size, true)
      }
      3 => {
        // 极高延迟+高资源使用，批处理大小应该大幅减少
        assert_eq(new_size < current_size, true)
        assert_eq(new_size <= batch_config.min_batch_size * 2, true)
      }
      _ => {}
    }
    
    current_size = new_size
    i = i + 1
  }
}

test "intelligent_batching_strategies" {
  // 测试智能批处理策略
  
  enum BatchingStrategy {
    TimeBased(Int)        // 基于时间的批处理（毫秒）
    SizeBased(Int)        // 基于大小的批处理（项目数）
    Hybrid(Int, Int)      // 混合策略（时间，大小）
    Adaptive              // 自适应策略
    Priority              // 基于优先级的批处理
  }
  
  struct BatchItem {
    id: String
    size_bytes: Int
    priority: Int
    creation_time: Int64
    urgency_score: Float
  }
  
  struct IntelligentBatcher {
    strategy: BatchingStrategy
    current_batch: Array<BatchItem>
    batch_start_time: Int64
    total_items_processed: Int
    average_batch_size: Float
  }
  
  // 创建智能批处理器
  let create_intelligent_batcher = fn(strategy: BatchingStrategy) -> IntelligentBatcher {
    IntelligentBatcher{
      strategy: strategy,
      current_batch: Array::empty(),
      batch_start_time: 1640995200000000L,
      total_items_processed: 0,
      average_batch_size: 0.0
    }
  }
  
  // 判断是否应该触发批处理
  let should_flush_batch = fn(batcher: IntelligentBatcher, current_time: Int64, item: BatchItem) -> Bool {
    match batcher.strategy {
      TimeBased(max_wait_ms) => {
        let elapsed_ms = (current_time - batcher.batch_start_time) / 1000000L
        elapsed_ms >= max_wait_ms.to_int64()
      }
      SizeBased(max_items) => {
        batcher.current_batch.length() >= max_items
      }
      Hybrid(max_wait_ms, max_items) => {
        let elapsed_ms = (current_time - batcher.batch_start_time) / 1000000L
        batcher.current_batch.length() >= max_items || elapsed_ms >= max_wait_ms.to_int64()
      }
      Adaptive => {
        // 自适应策略：基于项目特性和系统状态
        let batch_size = batcher.current_batch.length()
        let elapsed_ms = (current_time - batcher.batch_start_time) / 1000000L
        
        // 计算批处理的"价值"
        let batch_value = batcher.current_batch.fold(0.0, fn(acc, batch_item) {
          acc + batch_item.urgency_score
        })
        
        // 如果有高优先级项目或批处理价值高，立即处理
        if item.priority >= 8 || batch_value > 5.0 {
          true
        } else if batch_size >= 50 {
          // 达到最小批处理大小
          true
        } else if elapsed_ms >= 1000 {
          // 超过最大等待时间
          true
        } else {
          false
        }
      }
      Priority => {
        // 基于优先级的批处理：高优先级项目立即触发
        item.priority >= 7 || batcher.current_batch.length() >= 20
      }
    }
  }
  
  // 添加项目到批处理
  let add_to_batch = fn(batcher: IntelligentBatcher, item: BatchItem, current_time: Int64) -> (IntelligentBatcher, Bool) {
    let should_flush = should_flush_batch(batcher, current_time, item)
    
    if should_flush && batcher.current_batch.length() > 0 {
      // 先处理当前批次，再添加新项目
      let new_batcher = { batcher |
        current_batch: [item],
        batch_start_time: current_time,
        total_items_processed: batcher.total_items_processed + batcher.current_batch.length()
      }
      (new_batcher, true)  // 触发了批处理
    } else {
      // 添加到当前批次
      let mut new_batch = batcher.current_batch.to_array()
      new_batch.push(item)
      
      let new_batcher = { batcher |
        current_batch: new_batch
      }
      (new_batcher, false)  // 未触发批处理
    }
  }
  
  // 测试不同批处理策略
  let strategies = [
    TimeBased(1000),
    SizeBased(10),
    Hybrid(500, 5),
    Adaptive,
    Priority
  ]
  
  let test_items = [
    BatchItem{
      id: "item-1",
      size_bytes: 1024,
      priority: 5,
      creation_time: 1640995200000000L,
      urgency_score: 2.0
    },
    BatchItem{
      id: "item-2",
      size_bytes: 2048,
      priority: 9,
      creation_time: 1640995200001000L,
      urgency_score: 8.0
    },
    BatchItem{
      id: "item-3",
      size_bytes: 512,
      priority: 3,
      creation_time: 1640995200002000L,
      urgency_score: 1.0
    }
  ]
  
  let mut i = 0
  while i < strategies.length() {
    let strategy = strategies[i]
    let batcher = create_intelligent_batcher(strategy)
    
    let mut current_batcher = batcher
    let mut flush_count = 0
    
    let mut j = 0
    while j < test_items.length() {
      let (updated_batcher, flushed) = add_to_batch(current_batcher, test_items[j], test_items[j].creation_time)
      if flushed {
        flush_count = flush_count + 1
      }
      current_batcher = updated_batcher
      j = j + 1
    }
    
    // 验证不同策略的行为
    match strategy {
      TimeBased(_) => {
        // 时间策略：由于时间间隔短，可能不会触发批处理
        assert_eq(flush_count <= 1, true)
      }
      SizeBatch(size) => {
        // 大小策略：根据项目数量触发
        if test_items.length() >= size {
          assert_eq(flush_count >= 1, true)
        }
      }
      Hybrid(_, size) => {
        // 混合策略：根据时间或大小触发
        assert_eq(flush_count >= 1, true)
      }
      Adaptive => {
        // 自适应策略：高优先级项目应该触发批处理
        assert_eq(flush_count >= 1, true)
      }
      Priority => {
        // 优先级策略：高优先级项目应该触发批处理
        assert_eq(flush_count >= 1, true)
      }
    }
    
    i = i + 1
  }
}

test "batch_processing_optimization" {
  // 测试批处理优化技术
  
  struct OptimizationConfig {
    enable_compression: Bool
    enable_deduplication: Bool
    enable_reordering: Bool
    max_batch_memory_mb: Int
    compression_threshold: Int
  }
  
  struct BatchOptimizer {
    config: OptimizationConfig
    compression_ratio: Float
    deduplication_rate: Float
    optimization_overhead_ns: Int64
  }
  
  // 创建批处理优化器
  let optimizer_config = OptimizationConfig{
    enable_compression: true,
    enable_deduplication: true,
    enable_reordering: true,
    max_batch_memory_mb: 10,
    compression_threshold: 1024
  }
  
  let batch_optimizer = BatchOptimizer{
    config: optimizer_config,
    compression_ratio: 0.3,
    deduplication_rate: 0.15,
    optimization_overhead_ns: 1000000L  // 1ms
  }
  
  // 模拟批处理项目
  struct ProcessItem {
    id: String
    data: String
    timestamp: Int64
    size_bytes: Int
  }
  
  // 压缩优化
  let compress_batch = fn(items: Array<ProcessItem>, config: OptimizationConfig) -> (Array<ProcessItem>, Float) {
    if !config.enable_compression {
      return (items, 1.0)
    }
    
    let total_size = items.fold(0, fn(acc, item) { acc + item.size_bytes })
    
    // 模拟压缩：只有总大小超过阈值才压缩
    if total_size >= config.compression_threshold {
      let compressed_size = (total_size.to_float() * 0.3).to_int()  // 假设压缩到30%
      let compression_ratio = compressed_size.to_float() / total_size.to_float()
      
      // 返回压缩后的项目和压缩比
      (items, compression_ratio)
    } else {
      (items, 1.0)
    }
  }
  
  // 去重优化
  let deduplicate_batch = fn(items: Array<ProcessItem], config: OptimizationConfig) -> (Array<ProcessItem>, Float) {
    if !config.enable_deduplication {
      return (items, 1.0)
    }
    
    let mut unique_items = Array::empty<ProcessItem>()
    let mut seen_ids = Array::empty<String>()
    
    let mut i = 0
    while i < items.length() {
      let item = items[i]
      if !seen_ids.contains(item.id) {
        unique_items.push(item)
        seen_ids.push(item.id)
      }
      i = i + 1
    }
    
    let deduplication_rate = if items.length() > 0 {
      (items.length() - unique_items.length()).to_float() / items.length().to_float()
    } else {
      0.0
    }
    
    (unique_items, deduplication_rate)
  }
  
  // 重排序优化
  let reorder_batch = fn(items: Array<ProcessItem>, config: OptimizationConfig) -> Array<ProcessItem> {
    if !config.enable_reordering {
      return items
    }
    
    // 按时间戳排序（优化处理顺序）
    let mut sorted_items = items.to_array()
    sorted_items.sort_by(fn(a, b) { (a.timestamp - b.timestamp).to_int() })
    
    sorted_items
  }
  
  // 综合优化
  let optimize_batch = fn(
    items: Array<ProcessItem>, 
    optimizer: BatchOptimizer
  ) -> (Array<ProcessItem>, Float, Float, Int64) {
    let start_time = 1640995200000000L
    
    // 步骤1：去重
    let (deduplicated_items, dedup_rate) = deduplicate_batch(items, optimizer.config)
    
    // 步骤2：重排序
    let reordered_items = reorder_batch(deduplicated_items, optimizer.config)
    
    // 步骤3：压缩
    let (compressed_items, compression_ratio) = compress_batch(reordered_items, optimizer.config)
    
    let end_time = start_time + optimizer.optimization_overhead_ns
    
    (compressed_items, dedup_rate, compression_ratio, end_time - start_time)
  }
  
  // 创建测试数据
  let test_batch = [
    ProcessItem{
      id: "item-1",
      data: "x" * 500,
      timestamp: 1640995200000000L,
      size_bytes: 500
    },
    ProcessItem{
      id: "item-2",
      data: "x" * 300,
      timestamp: 1640995200001000L,
      size_bytes: 300
    },
    ProcessItem{
      id: "item-1",  // 重复项目
      data: "x" * 500,
      timestamp: 1640995200002000L,
      size_bytes: 500
    },
    ProcessItem{
      id: "item-3",
      data: "x" * 800,
      timestamp: 1640995200000000L,  // 时间戳较早
      size_bytes: 800
    }
  ]
  
  // 执行优化
  let (optimized_batch, dedup_rate, compression_ratio, optimization_time) = 
    optimize_batch(test_batch, batch_optimizer)
  
  // 验证去重效果
  assert_eq(dedup_rate > 0.0, true)
  assert_eq(optimized_batch.length(), test_batch.length() - 1)  // 应该去除一个重复项
  
  // 验证压缩效果
  let total_original_size = test_batch.fold(0, fn(acc, item) { acc + item.size_bytes })
  let total_optimized_size = optimized_batch.fold(0, fn(acc, item) { acc + item.size_bytes })
  
  if total_original_size >= optimizer_config.compression_threshold {
    assert_eq(compression_ratio < 1.0, true)  // 应该有压缩效果
  }
  
  // 验证重排序效果
  let mut is_sorted = true
  let mut i = 1
  while i < optimized_batch.length() {
    if optimized_batch[i].timestamp < optimized_batch[i-1].timestamp {
      is_sorted = false
      break
    }
    i = i + 1
  }
  assert_eq(is_sorted, true)
  
  // 验证优化开销
  assert_eq(optimization_time > 0L, true)
  assert_eq(optimization_time <= batch_optimizer.optimization_overhead_ns * 2, true)
  
  // 测试内存限制
  let memory_usage_mb = total_optimized_size / (1024 * 1024)
  assert_eq(memory_usage_mb <= optimizer_config.max_batch_memory_mb, true)
}

test "batch_processing_fault_tolerance" {
  // 测试批处理的容错机制
  
  enum BatchState {
    Pending
    Processing
    Completed
    Failed
    Retry
  }
  
  struct BatchItem {
    id: String
    data: String
    retry_count: Int
    max_retries: Int
    state: BatchState
  }
  
  struct Batch {
    items: Array<BatchItem>
    batch_id: String
    creation_time: Int64
    state: BatchState
    failure_reason: Option<String>
  }
  
  struct FaultTolerantProcessor {
    max_retry_attempts: Int
    retry_delay_ms: Int
    partial_failure_handling: Bool
    dead_letter_queue: Array<BatchItem>
  }
  
  // 创建容错处理器
  let fault_processor = FaultTolerantProcessor{
    max_retry_attempts: 3,
    retry_delay_ms: 1000,
    partial_failure_handling: true,
    dead_letter_queue: Array::empty()
  }
  
  // 处理批处理（模拟成功/失败）
  let process_batch = fn(batch: Batch, success_rate: Float) -> (Batch, Array<BatchItem]) {
    let mut processed_items = Array::empty<BatchItem>()
    let mut failed_items = Array::empty<BatchItem>()
    
    let mut i = 0
    while i < batch.items.length() {
      let item = batch.items[i]
      
      // 模拟处理成功/失败
      let random_value = (item.id.to_int() % 100).to_float() / 100.0
      let success = random_value < success_rate
      
      if success {
        let processed_item = { item | state: Completed }
        processed_items.push(processed_item)
      } else {
        let failed_item = { item | state: Failed }
        failed_items.push(failed_item)
      }
      
      i = i + 1
    }
    
    let batch_state = if failed_items.length() == 0 {
      Completed
    } else if processed_items.length() == 0 {
      Failed
    } else {
      Failed  // 部分失败仍标记为失败
    }
    
    let updated_batch = { batch |
      items: processed_items.concat(failed_items),
      state: batch_state,
      failure_reason: if failed_items.length() > 0 { Some("processing_failed") } else { None }
    }
    
    (updated_batch, failed_items)
  }
  
  // 重试失败的批处理
  let retry_failed_batch = fn(
    processor: FaultTolerantProcessor, 
    batch: Batch, 
    failed_items: Array<BatchItem>,
    current_time: Int64
  ) -> (FaultTolerantProcessor, Batch) {
    let mut retry_items = Array::empty<BatchItem>()
    let mut dead_letter_items = Array::empty<BatchItem>()
    
    let mut i = 0
    while i < failed_items.length() {
      let item = failed_items[i]
      
      if item.retry_count < processor.max_retry_attempts {
        let retry_item = { item |
          retry_count: item.retry_count + 1,
          state: Retry
        }
        retry_items.push(retry_item)
      } else {
        // 超过最大重试次数，放入死信队列
        dead_letter_items.push({ item | state: Failed })
      }
      
      i = i + 1
    }
    
    // 更新死信队列
    let mut new_dead_letter = processor.dead_letter_queue.to_array()
    let mut j = 0
    while j < dead_letter_items.length() {
      new_dead_letter.push(dead_letter_items[j])
      j = j + 1
    }
    
    // 创建重试批次
    let retry_batch = Batch{
      items: retry_items,
      batch_id: batch.batch_id + "-retry-" + current_time.to_string(),
      creation_time: current_time,
      state: Pending,
      failure_reason: None
    }
    
    let updated_processor = { processor | dead_letter_queue: new_dead_letter }
    
    (updated_processor, retry_batch)
  }
  
  // 创建测试批次
  let test_batch = Batch{
    items: [
      BatchItem{
        id: "item-1",
        data: "data-1",
        retry_count: 0,
        max_retries: 3,
        state: Pending
      },
      BatchItem{
        id: "item-2",
        data: "data-2",
        retry_count: 0,
        max_retries: 3,
        state: Pending
      },
      BatchItem{
        id: "item-3",
        data: "data-3",
        retry_count: 0,
        max_retries: 3,
        state: Pending
      }
    ],
    batch_id: "batch-1",
    creation_time: 1640995200000000L,
    state: Pending,
    failure_reason: None
  }
  
  // 场景1：完全成功
  let (success_batch, failed_items_1) = process_batch(test_batch, 1.0)  // 100%成功率
  assert_eq(success_batch.state, Completed)
  assert_eq(failed_items_1.length(), 0)
  
  // 场景2：部分失败
  let (partial_batch, failed_items_2) = process_batch(test_batch, 0.6)  // 60%成功率
  assert_eq(partial_batch.state, Failed)
  assert_eq(failed_items_2.length() > 0, true)
  assert_eq(failed_items_2.length() < test_batch.items.length(), true)
  
  // 场景3：重试机制
  let current_time = 1640995200000000L
  let (processor_after_retry, retry_batch) = retry_failed_batch(
    fault_processor, 
    partial_batch, 
    failed_items_2, 
    current_time
  )
  
  assert_eq(retry_batch.state, Pending)
  assert_eq(retry_batch.items.length(), failed_items_2.length())
  
  // 验证重试次数增加
  let mut i = 0
  while i < retry_batch.items.length() {
    assert_eq(retry_batch.items[i].retry_count, 1)
    assert_eq(retry_batch.items[i].state, Retry)
    i = i + 1
  }
  
  // 场景4：超过最大重试次数
  let max_retry_item = BatchItem{
    id: "item-max-retry",
    data: "data-max",
    retry_count: 3,
    max_retries: 3,
    state: Failed
  }
  
  let (processor_final, _) = retry_failed_batch(
    processor_after_retry,
    test_batch,
    [max_retry_item],
    current_time + 1000000L
  )
  
  assert_eq(processor_final.dead_letter_queue.length(), 1)
  assert_eq(processor_final.dead_letter_queue[0].id, "item-max-retry")
  assert_eq(processor_final.dead_letter_queue[0].state, Failed)
  
  // 场景5：部分失败处理
  if fault_processor.partial_failure_handling {
    // 部分失败时，成功的项目应该被保留
    let (partial_success_batch, _) = process_batch(test_batch, 0.8)  // 80%成功率
    let successful_items = partial_success_batch.items.filter(fn(item) { item.state == Completed })
    
    assert_eq(successful_items.length() > 0, true)
    assert_eq(successful_items.length() < test_batch.items.length(), true)
  }
}

test "batch_processing_performance_monitoring" {
  // 测试批处理性能监控
  
  struct PerformanceMetrics {
    batch_id: String
    batch_size: Int
    processing_time_ns: Int64
    throughput_items_per_second: Float
    memory_usage_mb: Float
    cpu_usage_percent: Float
    error_rate: Float
  }
  
  struct PerformanceMonitor {
    metrics_history: Array<PerformanceMetrics>
    alert_thresholds: AlertThresholds
    current_alerts: Array<String>
  }
  
  struct AlertThresholds {
    max_processing_time_ms: Int
    min_throughput: Float
    max_memory_usage_mb: Float
    max_cpu_usage_percent: Float
    max_error_rate: Float
  }
  
  // 创建性能监控器
  let alert_thresholds = AlertThresholds{
    max_processing_time_ms: 1000,
    min_throughput: 100.0,
    max_memory_usage_mb: 100.0,
    max_cpu_usage_percent: 80.0,
    max_error_rate: 0.05
  }
  
  let performance_monitor = PerformanceMonitor{
    metrics_history: Array::empty(),
    alert_thresholds: alert_thresholds,
    current_alerts: Array::empty()
  }
  
  // 计算性能指标
  let calculate_metrics = fn(
    batch_id: String,
    batch_size: Int,
    processing_time_ns: Int64,
    memory_usage_mb: Float,
    cpu_usage_percent: Float,
    error_count: Int
  ) -> PerformanceMetrics {
    let processing_time_ms = processing_time_ns / 1000000L
    let throughput = if processing_time_ms > 0 {
      batch_size.to_float() / (processing_time_ms.to_float() / 1000.0)
    } else {
      0.0
    }
    
    let error_rate = if batch_size > 0 {
      error_count.to_float() / batch_size.to_float()
    } else {
      0.0
    }
    
    PerformanceMetrics{
      batch_id: batch_id,
      batch_size: batch_size,
      processing_time_ns: processing_time_ns,
      throughput_items_per_second: throughput,
      memory_usage_mb: memory_usage_mb,
      cpu_usage_percent: cpu_usage_percent,
      error_rate: error_rate
    }
  }
  
  // 检查性能告警
  let check_performance_alerts = fn(
    monitor: PerformanceMonitor,
    metrics: PerformanceMetrics
  ) -> Array<String> {
    let mut alerts = Array::empty<String>()
    
    let processing_time_ms = metrics.processing_time_ns / 1000000L
    
    if processing_time_ms > monitor.alert_thresholds.max_processing_time_ms.to_int64() {
      alerts.push("High processing time: " + processing_time_ms.to_string() + "ms")
    }
    
    if metrics.throughput_items_per_second < monitor.alert_thresholds.min_throughput {
      alerts.push("Low throughput: " + metrics.throughput_items_per_second.to_string() + " items/sec")
    }
    
    if metrics.memory_usage_mb > monitor.alert_thresholds.max_memory_usage_mb {
      alerts.push("High memory usage: " + metrics.memory_usage_mb.to_string() + "MB")
    }
    
    if metrics.cpu_usage_percent > monitor.alert_thresholds.max_cpu_usage_percent {
      alerts.push("High CPU usage: " + metrics.cpu_usage_percent.to_string() + "%")
    }
    
    if metrics.error_rate > monitor.alert_thresholds.max_error_rate {
      alerts.push("High error rate: " + (metrics.error_rate * 100.0).to_string() + "%")
    }
    
    alerts
  }
  
  // 测试不同性能场景
  let test_scenarios = [
    (
      "batch-normal",
      100,    // batch_size
      500000000L,  // 500ms processing time
      50.0,   // 50MB memory
      30.0,   // 30% CPU
      2       // 2 errors
    ),
    (
      "batch-slow",
      200,    // batch_size
      1500000000L,  // 1500ms processing time (slow)
      80.0,   // 80MB memory
      70.0,   // 70% CPU
      5       // 5 errors
    ),
    (
      "batch-failing",
      50,     // batch_size
      200000000L,   // 200ms processing time
      120.0,  // 120MB memory (high)
      90.0,   // 90% CPU (high)
      10      // 10 errors (high error rate)
    ),
    (
      "batch-optimal",
      500,    // batch_size
      200000000L,   // 200ms processing time (fast)
      30.0,   // 30MB memory (low)
      20.0,   // 20% CPU (low)
      1       // 1 error (low error rate)
    )
  ]
  
  let mut i = 0
  while i < test_scenarios.length() {
    let (batch_id, batch_size, processing_time, memory, cpu, errors) = test_scenarios[i]
    
    let metrics = calculate_metrics(
      batch_id,
      batch_size,
      processing_time,
      memory,
      cpu,
      errors
    )
    
    let alerts = check_performance_alerts(performance_monitor, metrics)
    
    // 验证性能指标计算
    assert_eq(metrics.batch_id, batch_id)
    assert_eq(metrics.batch_size, batch_size)
    assert_eq(metrics.processing_time_ns, processing_time)
    assert_eq(metrics.memory_usage_mb, memory)
    assert_eq(metrics.cpu_usage_percent, cpu)
    
    let expected_error_rate = errors.to_float() / batch_size.to_float()
    assert_eq(abs(metrics.error_rate - expected_error_rate) < 0.001, true)
    
    // 验证吞吐量计算
    let expected_throughput = batch_size.to_float() / (processing_time.to_float() / 1000000000.0)
    assert_eq(abs(metrics.throughput_items_per_second - expected_throughput) < 1.0, true)
    
    // 验证告警触发
    match batch_id {
      "batch-normal" => {
        // 正常场景，应该没有告警
        assert_eq(alerts.length(), 0)
      }
      "batch-slow" => {
        // 慢处理场景，应该有处理时间告警
        assert_eq(alerts.length() >= 1, true)
        assert_eq(alerts.any(fn(alert) { alert.contains("High processing time") }), true)
      }
      "batch-failing" => {
        // 失败场景，应该有多个告警
        assert_eq(alerts.length() >= 3, true)
        assert_eq(alerts.any(fn(alert) { alert.contains("High memory usage") }), true)
        assert_eq(alerts.any(fn(alert) { alert.contains("High CPU usage") }), true)
        assert_eq(alerts.any(fn(alert) { alert.contains("High error rate") }), true)
      }
      "batch-optimal" => {
        // 最优场景，应该没有告警
        assert_eq(alerts.length(), 0)
      }
      _ => {}
    }
    
    i = i + 1
  }
  
  // 测试性能趋势分析
  let trend_scenarios = [
    ("trend-1", 100, 500000000L, 50.0, 30.0, 2),
    ("trend-2", 100, 600000000L, 55.0, 35.0, 3),
    ("trend-3", 100, 700000000L, 60.0, 40.0, 4),
    ("trend-4", 100, 800000000L, 65.0, 45.0, 5)
  ]
  
  let mut trend_metrics = Array::empty<PerformanceMetrics>()
  let mut j = 0
  while j < trend_scenarios.length() {
    let (batch_id, batch_size, processing_time, memory, cpu, errors) = trend_scenarios[j]
    
    let metrics = calculate_metrics(batch_id, batch_size, processing_time, memory, cpu, errors)
    trend_metrics.push(metrics)
    
    j = j + 1
  }
  
  // 分析性能趋势
  let analyze_performance_trend = fn(metrics: Array<PerformanceMetrics>) -> String {
    if metrics.length() < 2 {
      return "insufficient_data"
    }
    
    let first = metrics[0]
    let last = metrics[metrics.length() - 1]
    
    let processing_time_trend = if last.processing_time_ns > first.processing_time_ns {
      "degrading"
    } else if last.processing_time_ns < first.processing_time_ns {
      "improving"
    } else {
      "stable"
    }
    
    let memory_trend = if last.memory_usage_mb > first.memory_usage_mb * 1.2 {
      "increasing"
    } else if last.memory_usage_mb < first.memory_usage_mb * 0.8 {
      "decreasing"
    } else {
      "stable"
    }
    
    let error_trend = if last.error_rate > first.error_rate * 1.5 {
      "worsening"
    } else if last.error_rate < first.error_rate * 0.5 {
      "improving"
    } else {
      "stable"
    }
    
    "processing_time:" + processing_time_trend + ",memory:" + memory_trend + ",error_rate:" + error_trend
  }
  
  let trend_analysis = analyze_performance_trend(trend_metrics)
  assert_eq(trend_analysis.contains("processing_time:degrading"), true)
  assert_eq(trend_analysis.contains("memory:increasing"), true)
  assert_eq(trend_analysis.contains("error_rate:worsening"), true)
}