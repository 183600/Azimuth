// Azimuth 资源利用率优化测试用例
// 专注于测试内存和CPU使用优化以及性能监控

// 测试1: 内存池管理优化
test "内存池管理优化" {
  // 定义内存块
  type MemoryBlock = {
    id: Int,
    size: Int,
    allocated: Bool,
    data: Array[Byte]
  }
  
  // 定义内存池
  type MemoryPool = {
    blocks: Array[MemoryBlock],
    total_size: Int,
    allocated_size: Int,
    free_size: Int,
    allocation_count: Int
  }
  
  // 创建内存池管理器
  let memory_pool_manager = fn(pool_size: Int, block_sizes: Array[Int>) {
    let mut blocks = []
    let mut current_id = 0
    let mut total_allocated = 0
    
    // 预分配不同大小的内存块
    for size in block_sizes {
      for i in 0..(pool_size / size) {
        blocks = blocks.push({
          id: current_id,
          size: size,
          allocated: false,
          data: Array::new(size, 0)  // 初始化为零
        })
        current_id = current_id + 1
      }
    }
    
    fn(allocate_size: Int) -> (Option[MemoryBlock], MemoryPool) {
      // 查找合适的空闲块（首次适应算法）
      let mut selected_block_index = -1
      let mut selected_block_size = 0
      
      for i in 0..blocks.length() {
        let block = blocks[i]
        if not block.allocated && block.size >= allocate_size {
          if selected_block_index == -1 || block.size < selected_block_size {
            selected_block_index = i
            selected_block_size = block.size
          }
        }
      }
      
      if selected_block_index >= 0 {
        // 分配内存块
        let mut updated_blocks = blocks.copy()
        let mut block = updated_blocks[selected_block_index]
        block = { ..block, allocated: true }
        updated_blocks[selected_block_index] = block
        
        let allocated_block = updated_blocks[selected_block_index]
        total_allocated = total_allocated + allocated_block.size
        
        let pool = MemoryPool({
          blocks: updated_blocks,
          total_size: pool_size,
          allocated_size: total_allocated,
          free_size: pool_size - total_allocated,
          allocation_count: total_allocated  // 简化计数
        })
        
        (Some(allocated_block), pool)
      } else {
        // 没有合适的块
        let pool = MemoryPool({
          blocks: blocks,
          total_size: pool_size,
          allocated_size: total_allocated,
          free_size: pool_size - total_allocated,
          allocation_count: total_allocated
        })
        
        (None, pool)
      }
    }
  }
  
  // 创建内存池（1MB，包含不同大小的块）
  let pool_size = 1024 * 1024  // 1MB
  let block_sizes = [64, 128, 256, 512, 1024]  // 不同大小的块（字节）
  let pool_manager = memory_pool_manager(pool_size, block_sizes)
  
  // 分配内存块
  let (block1, pool1) = pool_manager(100)   // 需要100字节，分配128字节块
  let (block2, pool2) = pool_manager(200)   // 需要200字节，分配256字节块
  let (block3, pool3) = pool_manager(400)   // 需要400字节，分配512字节块
  let (block4, pool4) = pool_manager(1200)  // 需要1200字节，分配失败（没有足够大的块）
  
  // 验证分配结果
  assert_true(block1.is_some())
  assert_true(block2.is_some())
  assert_true(block3.is_some())
  assert_false(block4.is_some())  // 分配失败
  
  // 验证分配的块大小
  assert_eq(block1.unwrap().size, 128)
  assert_eq(block2.unwrap().size, 256)
  assert_eq(block3.unwrap().size, 512)
  
  // 验证内存池状态
  assert_eq(pool1.total_size, pool_size)
  assert_eq(pool1.allocated_size, 128)
  assert_eq(pool1.free_size, pool_size - 128)
  
  assert_eq(pool2.allocated_size, 128 + 256)
  assert_eq(pool2.free_size, pool_size - (128 + 256))
  
  assert_eq(pool3.allocated_size, 128 + 256 + 512)
  assert_eq(pool3.free_size, pool_size - (128 + 256 + 512))
  
  // 计算内存利用率
  let memory_utilization = (pool3.allocated_size as Float) / (pool3.total_size as Float) * 100.0
  assert_true(memory_utilization > 0.0 && memory_utilization < 100.0)
  
  // 计算内存碎片率（内部碎片）
  let internal_fragmentation = (pool3.allocated_size as Float - (100 + 200 + 400) as Float) / (pool3.allocated_size as Float) * 100.0
  assert_true(internal_fragmentation > 0.0)  // 由于块大小不匹配，会有内部碎片
}

// 测试2: CPU缓存优化
test "CPU缓存优化" {
  // 定义缓存行
  type CacheLine = {
    data: Array[Int],
    tag: Int,
    valid: Bool,
    last_access: Int
  }
  
  // 定义CPU缓存
  type CPUCache = {
    lines: Array[CacheLine>,
    line_size: Int,
    associativity: Int,
    total_sets: Int,
    hits: Int,
    misses: Int
  }
  
  // 创建CPU缓存模拟器
  let cpu_cache_simulator = fn(line_size: Int, associativity: Int, total_sets: Int) {
    let mut cache = CPUCache({
      lines: Array::new(total_sets * associativity, CacheLine({
        data: Array::new(line_size / 4, 0),  // 假设Int是4字节
        tag: 0,
        valid: false,
        last_access: 0
      })),
      line_size: line_size,
      associativity: associativity,
      total_sets: total_sets,
      hits: 0,
      misses: 0
    })
    
    let mut access_counter = 0
    
    fn(address: Int) -> (Int, CPUCache) {
      access_counter = access_counter + 1
      
      // 计算缓存索引和标签
      let block_address = address / line_size
      let set_index = block_address % total_sets
      let tag = block_address / total_sets
      
      // 查找缓存行
      let set_start = set_index * associativity
      let set_end = set_start + associativity
      
      let mut hit_index = -1
      let mut lru_index = set_start
      let mut lru_time = access_counter
      
      // 搜索集合中的缓存行
      for i in set_start..set_end {
        let line = cache.lines[i]
        if line.valid && line.tag == tag {
          hit_index = i
          break
        }
        
        if line.valid && line.last_access < lru_time {
          lru_time = line.last_access
          lru_index = i
        } else if not line.valid {
          lru_index = i
          break
        }
      }
      
      if hit_index >= 0 {
        // 缓存命中
        let mut updated_lines = cache.lines.copy()
        let mut hit_line = updated_lines[hit_index]
        hit_line = { ..hit_line, last_access: access_counter }
        updated_lines[hit_index] = hit_line
        
        cache = { ..cache, lines: updated_lines, hits: cache.hits + 1 }
        (1, cache)  // 返回命中状态1
      } else {
        // 缓存未命中，需要加载新数据
        let mut updated_lines = cache.lines.copy()
        let mut replace_line = updated_lines[lru_index]
        replace_line = {
          data: Array::new(line_size / 4, address % 100),  // 模拟数据加载
          tag: tag,
          valid: true,
          last_access: access_counter
        }
        updated_lines[lru_index] = replace_line
        
        cache = { ..cache, lines: updated_lines, misses: cache.misses + 1 }
        (0, cache)  // 返回命中状态0
      }
    }
  }
  
  // 创建缓存（64字节行，4路组相联，16组）
  let cache_sim = cpu_cache_simulator(64, 4, 16)
  
  // 测试访问模式
  let sequential_addresses = [0, 64, 128, 192, 256, 320, 384, 448]  // 顺序访问
  let random_addresses = [1024, 2048, 512, 1536, 768, 1280, 1792, 2560]  // 随机访问
  let repeating_addresses = [0, 64, 0, 64, 0, 64, 0, 64]  // 重复访问相同位置
  
  // 测试顺序访问
  let mut sequential_cache = CPUCache({
    lines: Array::new(64, CacheLine({ data: [], tag: 0, valid: false, last_access: 0 })),
    line_size: 64,
    associativity: 4,
    total_sets: 16,
    hits: 0,
    misses: 0
  })
  let mut sequential_results = []
  
  for address in sequential_addresses {
    let (result, updated_cache) = cache_sim(address)
    sequential_cache = updated_cache
    sequential_results = sequential_results.push(result)
  }
  
  // 测试随机访问
  let mut random_cache = CPUCache({
    lines: Array::new(64, CacheLine({ data: [], tag: 0, valid: false, last_access: 0 })),
    line_size: 64,
    associativity: 4,
    total_sets: 16,
    hits: 0,
    misses: 0
  })
  let mut random_results = []
  
  for address in random_addresses {
    let (result, updated_cache) = cache_sim(address)
    random_cache = updated_cache
    random_results = random_results.push(result)
  }
  
  // 测试重复访问
  let mut repeating_cache = CPUCache({
    lines: Array::new(64, CacheLine({ data: [], tag: 0, valid: false, last_access: 0 })),
    line_size: 64,
    associativity: 4,
    total_sets: 16,
    hits: 0,
    misses: 0
  })
  let mut repeating_results = []
  
  for address in repeating_addresses {
    let (result, updated_cache) = cache_sim(address)
    repeating_cache = updated_cache
    repeating_results = repeating_results.push(result)
  }
  
  // 计算命中率
  let sequential_hit_rate = (sequential_results.reduce(fn(acc, r) { acc + r }, 0) as Float) / (sequential_results.length() as Float) * 100.0
  let random_hit_rate = (random_results.reduce(fn(acc, r) { acc + r }, 0) as Float) / (random_results.length() as Float) * 100.0
  let repeating_hit_rate = (repeating_results.reduce(fn(acc, r) { acc + r }, 0) as Float) / (repeating_results.length() as Float) * 100.0
  
  // 验证缓存性能
  assert_true(sequential_hit_rate >= 0.0)  // 顺序访问可能有一些命中
  assert_true(random_hit_rate >= 0.0)     // 随机访问命中率较低
  assert_true(repeating_hit_rate > 50.0)  // 重复访问应该有高命中率
  
  // 重复访问的命中率应该最高
  assert_true(repeating_hit_rate > sequential_hit_rate)
  assert_true(repeating_hit_rate > random_hit_rate)
}

// 测试3: 数据结构内存布局优化
test "数据结构内存布局优化" {
  // 定义紧凑数据结构
  type CompactTelemetryPoint = {
    timestamp: Int,      // 8字节
    metric_id: Int16,    // 2字节
    value: Float32,      // 4字节
    flags: Byte,         // 1字节
    // 总计: 15字节，但可能填充到16字节
  }
  
  // 定义非紧凑数据结构
  type UncompactTelemetryPoint = {
    timestamp: Int,        // 8字节
    metric_name: String,   // 动态大小
    value: Float,          // 8字节
    labels: Array[(String, String)>,  // 动态大小
    data_type: String,     // 动态大小
    metadata: Array[(String, String)>,  // 动态大小
  }
  
  // 创建内存布局分析器
  let memory_layout_analyzer = fn() {
    fn(compact_data: Array[CompactTelemetryPoint>, uncompact_data: Array[UncompactTelemetryPoint>) {
      // 计算紧凑数据结构的内存使用
      let compact_size_per_item = 16  // 假设填充到16字节
      let compact_total_size = compact_data.length() * compact_size_per_item
      let compact_memory_efficiency = (compact_data.length() * 15) as Float / (compact_total_size as Float) * 100.0
      
      // 估算非紧凑数据结构的内存使用
      let uncompact_size_per_item = uncompact_data.reduce(fn(acc, item) {
        let base_size = 8 + 8  // timestamp + value
        let name_size = item.metric_name.length() * 2  // 假设每个字符2字节
        let type_size = item.data_type.length() * 2
        let labels_size = item.labels.reduce(fn(sum, label) { sum + (label.0.length() + label.1.length()) * 2 }, 0)
        let metadata_size = item.metadata.reduce(fn(sum, meta) { sum + (meta.0.length() + meta.1.length()) * 2 }, 0)
        acc + base_size + name_size + type_size + labels_size + metadata_size
      }, 0) / uncompact_data.length()
      
      let uncompact_total_size = uncompact_data.length() * uncompact_size_per_item
      
      // 计算内存压缩比
      let compression_ratio = (uncompact_total_size as Float) / (compact_total_size as Float)
      
      // 计算缓存友好性（基于数据大小）
      let compact_cache_friendly = compact_size_per_item <= 64  // 小于缓存行
      let uncompact_cache_friendly = uncompact_size_per_item <= 64
      
      {
        compact_data: {
          count: compact_data.length(),
          size_per_item: compact_size_per_item,
          total_size: compact_total_size,
          memory_efficiency: compact_memory_efficiency,
          cache_friendly: compact_cache_friendly
        },
        uncompact_data: {
          count: uncompact_data.length(),
          estimated_size_per_item: uncompact_size_per_item,
          estimated_total_size: uncompact_total_size,
          cache_friendly: uncompact_cache_friendly
        },
        compression_ratio: compression_ratio,
        memory_savings_percent: (1.0 - 1.0 / compression_ratio) * 100.0
      }
    }
  }
  
  let analyzer = memory_layout_analyzer()
  
  // 创建紧凑数据
  let compact_data = [
    { timestamp: 1735689600000, metric_id: 1001, value: 45.2, flags: 0x01 },
    { timestamp: 1735689660000, metric_id: 1002, value: 78.5, flags: 0x01 },
    { timestamp: 1735689720000, metric_id: 1003, value: 1250.0, flags: 0x02 },
    { timestamp: 1735689780000, metric_id: 1004, value: 150.0, flags: 0x03 },
    { timestamp: 1735689840000, metric_id: 1005, value: 5.0, flags: 0x02 }
  ]
  
  // 创建非紧凑数据
  let uncompact_data = [
    {
      timestamp: 1735689600000,
      metric_name: "cpu_usage_percentage",
      value: 45.2,
      labels: [("host", "server1"), ("region", "us-west")],
      data_type: "gauge",
      metadata: [("unit", "percent"), ("interval", "60s")]
    },
    {
      timestamp: 1735689660000,
      metric_name: "memory_usage_bytes",
      value: 78.5,
      labels: [("host", "server1"), ("region", "us-west")],
      data_type: "gauge",
      metadata: [("unit", "bytes"), ("total", "16GB")]
    },
    {
      timestamp: 1735689720000,
      metric_name: "http_requests_total",
      value: 1250.0,
      labels: [("host", "server1"), ("endpoint", "/api/users")],
      data_type: "counter",
      metadata: [("method", "GET"), ("status", "200")]
    },
    {
      timestamp: 1735689780000,
      metric_name: "http_request_duration_seconds",
      value: 150.0,
      labels: [("host", "server1"), ("endpoint", "/api/users")],
      data_type: "histogram",
      metadata: [("quantile", "0.95"), ("unit", "milliseconds")]
    },
    {
      timestamp: 1735689840000,
      metric_name: "error_count_total",
      value: 5.0,
      labels: [("host", "server1"), ("error_type", "timeout")],
      data_type: "counter",
      metadata: [("severity", "warning"), ("service", "api")]
    }
  ]
  
  // 分析内存布局
  let analysis_result = analyzer(compact_data, uncompact_data)
  
  // 验证分析结果
  assert_eq(analysis_result.compact_data.count, 5)
  assert_eq(analysis_result.compact_data.size_per_item, 16)
  assert_eq(analysis_result.compact_data.total_size, 80)  // 5 * 16
  assert_true(analysis_result.compact_data.cache_friendly)
  
  assert_eq(analysis_result.uncompact_data.count, 5)
  assert_true(analysis_result.uncompact_data.estimated_size_per_item > 16)  // 非紧凑结构更大
  assert_false(analysis_result.uncompact_data.cache_friendly)  // 可能不适合单个缓存行
  
  // 验证内存压缩效果
  assert_true(analysis_result.compression_ratio > 1.0)  // 紧凑结构更小
  assert_true(analysis_result.memory_savings_percent > 0.0)  // 有内存节省
  
  // 验证内存效率
  assert_true(analysis_result.compact_data.memory_efficiency > 90.0)  // 高内存效率
}

// 测试4: CPU利用率优化
test "CPU利用率优化" {
  // 定义任务类型
  enum TaskType {
    ComputeIntensive
    IIntensive
    Mixed
  }
  
  // 定义任务
  type Task = {
    id: Int,
    task_type: TaskType,
    duration_ms: Int,
    cpu_usage_percent: Float,
    priority: Int
  }
  
  // 定义CPU核心
  type CPUCore = {
    id: Int,
    current_load: Float,
    tasks: Array[Task>,
    max_capacity: Float
  }
  
  // 创建CPU调度器
  let cpu_scheduler = fn(num_cores: Int) {
    let mut cores = Array::new(num_cores, CPUCore({
      id: 0,
      current_load: 0.0,
      tasks: [],
      max_capacity: 100.0
    }))
    
    // 初始化核心ID
    for i in 0..num_cores {
      let mut core = cores[i]
      core = { ..core, id: i }
      cores[i] = core
    }
    
    fn(task: Task) -> (Bool, Array[CPUCore>) {
      // 找到负载最低的核心
      let mut selected_core_index = 0
      let mut min_load = cores[0].current_load
      
      for i in 1..cores.length() {
        if cores[i].current_load < min_load {
          min_load = cores[i].current_load
          selected_core_index = i
        }
      }
      
      // 检查是否有足够容量
      let selected_core = cores[selected_core_index]
      let can_schedule = (selected_core.current_load + task.cpu_usage_percent) <= selected_core.max_capacity
      
      if can_schedule {
        // 调度任务到选定核心
        let mut updated_cores = cores.copy()
        let mut core = updated_cores[selected_core_index]
        core = {
          current_load: core.current_load + task.cpu_usage_percent,
          tasks: core.tasks.push(task)
        }
        updated_cores[selected_core_index] = core
        
        (true, updated_cores)
      } else {
        // 无法调度
        (false, cores)
      }
    }
  }
  
  // 创建CPU性能分析器
  let cpu_performance_analyzer = fn(cores: Array[CPUCore>) {
    let total_cores = cores.length()
    let total_load = cores.reduce(fn(acc, core) { acc + core.current_load }, 0.0)
    let average_utilization = total_load / (total_cores as Float)
    
    let max_load = cores.reduce(fn(acc, core) { 
      if core.current_load > acc { core.current_load } else { acc } 
    }, 0.0)
    
    let min_load = cores.reduce(fn(acc, core) { 
      if core.current_load < acc { core.current_load } else { acc } 
    }, 100.0)
    
    let load_balance = if max_load > 0.0 {
      1.0 - ((max_load - min_load) / max_load)
    } else {
      1.0
    }
    
    // 计算任务分布
    let total_tasks = cores.reduce(fn(acc, core) { acc + core.tasks.length() }, 0)
    let tasks_per_core = if total_cores > 0 {
      total_tasks as Float / (total_cores as Float)
    } else {
      0.0
    }
    
    // 计算CPU效率（考虑负载均衡和利用率）
    let utilization_efficiency = if average_utilization > 80.0 {
      100.0 - (average_utilization - 80.0)  // 利用率超过80%时效率下降
    } else {
      average_utilization
    }
    
    let cpu_efficiency = (utilization_efficiency + (load_balance * 100.0)) / 2.0
    
    {
      total_cores: total_cores,
      average_utilization: average_utilization,
      max_load: max_load,
      min_load: min_load,
      load_balance: load_balance,
      total_tasks: total_tasks,
      tasks_per_core: tasks_per_core,
      cpu_efficiency: cpu_efficiency,
      is_well_balanced: load_balance > 0.8,
      is_optimally_utilized: average_utilization >= 70.0 && average_utilization <= 85.0
    }
  }
  
  // 创建调度器（4核心）
  let scheduler = cpu_scheduler(4)
  
  // 创建不同类型的任务
  let tasks = [
    { id: 1, task_type: TaskType::ComputeIntensive, duration_ms: 100, cpu_usage_percent: 25.0, priority: 1 },
    { id: 2, task_type: TaskType::IIntensive, duration_ms: 200, cpu_usage_percent: 10.0, priority: 2 },
    { id: 3, task_type: TaskType::Mixed, duration_ms: 150, cpu_usage_percent: 15.0, priority: 1 },
    { id: 4, task_type: TaskType::ComputeIntensive, duration_ms: 120, cpu_usage_percent: 30.0, priority: 2 },
    { id: 5, task_type: TaskType::IIntensive, duration_ms: 180, cpu_usage_percent: 5.0, priority: 3 },
    { id: 6, task_type: TaskType::Mixed, duration_ms: 90, cpu_usage_percent: 20.0, priority: 1 },
    { id: 7, task_type: TaskType::ComputeIntensive, duration_ms: 110, cpu_usage_percent: 35.0, priority: 2 },
    { id: 8, task_type: TaskType::Mixed, duration_ms: 160, cpu_usage_percent: 12.0, priority: 3 }
  ]
  
  // 调度任务
  let mut current_cores = Array::new(4, CPUCore({
    id: 0,
    current_load: 0.0,
    tasks: [],
    max_capacity: 100.0
  }))
  
  let mut scheduled_tasks = []
  let mut unscheduled_tasks = []
  
  for task in tasks {
    let (scheduled, updated_cores) = scheduler(task)
    current_cores = updated_cores
    
    if scheduled {
      scheduled_tasks = scheduled_tasks.push(task.id)
    } else {
      unscheduled_tasks = unscheduled_tasks.push(task.id)
    }
  }
  
  // 分析CPU性能
  let performance_analysis = cpu_performance_analyzer(current_cores)
  
  // 验证调度结果
  assert_eq(scheduled_tasks.length(), 8)  // 所有任务都应该被调度
  assert_eq(unscheduled_tasks.length(), 0)
  
  // 验证CPU利用率
  assert_true(performance_analysis.average_utilization > 0.0)
  assert_true(performance_analysis.average_utilization <= 100.0)
  
  // 验证负载均衡
  assert_true(performance_analysis.load_balance >= 0.0 && performance_analysis.load_balance <= 1.0)
  
  // 验证任务分布
  assert_eq(performance_analysis.total_tasks, 8)
  assert_eq(performance_analysis.total_cores, 4)
  assert_eq(performance_analysis.tasks_per_core, 2.0)  // 8个任务分布在4个核心上
  
  // 验证CPU效率
  assert_true(performance_analysis.cpu_efficiency >= 0.0 && performance_analysis.cpu_efficiency <= 100.0)
  
  // 验证负载分布（每个核心应该有2个任务）
  for core in current_cores {
    assert_eq(core.tasks.length(), 2)
    assert_true(core.current_load > 0.0)
    assert_true(core.current_load <= core.max_capacity)
  }
}

// 测试5: 内存与CPU协同优化
test "内存与CPU协同优化" {
  // 定义资源使用情况
  type ResourceUsage = {
    timestamp: Int,
    memory_usage_mb: Float,
    cpu_usage_percent: Float,
    active_tasks: Int,
    cache_hit_rate: Float,
    memory_fragmentation_percent: Float
  }
  
  // 创建资源优化器
  let resource_optimizer = fn() {
    let mut optimization_history = []
    
    fn(current_usage: ResourceUsage) -> (String, ResourceUsage) {
      // 分析当前资源使用情况
      let memory_pressure = current_usage.memory_usage_mb / 4096.0 * 100.0  // 假设4GB总内存
      let cpu_pressure = current_usage.cpu_usage_percent
      let cache_efficiency = current_usage.cache_hit_rate
      let fragmentation = current_usage.memory_fragmentation_percent
      
      // 决定优化策略
      let optimization_action = 
        if memory_pressure > 80.0 && fragmentation > 30.0 {
          "memory_compaction"  // 内存压力大且碎片严重，进行内存压缩
        } else if cpu_pressure > 85.0 && cache_efficiency < 70.0 {
          "cache_optimization"  // CPU压力大且缓存效率低，优化缓存
        } else if memory_pressure > 70.0 && cpu_pressure < 50.0 {
          "memory_cleanup"  // 内存压力大但CPU空闲，进行内存清理
        } else if cpu_pressure > 70.0 && memory_pressure < 50.0 {
          "cpu_optimization"  // CPU压力大但内存充足，优化CPU使用
        } else if memory_pressure < 30.0 && cpu_pressure < 30.0 {
          "resource_consolidation"  // 资源使用率低，整合资源
        } else {
          "no_action"  // 资源使用正常，无需优化
        }
      
      // 模拟优化效果
      let optimized_usage = match optimization_action {
        "memory_compaction" => {
          {
            ..current_usage,
            memory_fragmentation_percent: fragmentation * 0.2,  // 碎片减少80%
            memory_usage_mb: current_usage.memory_usage_mb * 0.9,  // 内存使用减少10%
            cache_hit_rate: cache_efficiency * 1.1  // 缓存命中率提高10%
          }
        }
        "cache_optimization" => {
          {
            ..current_usage,
            cache_hit_rate: if cache_efficiency * 1.3 > 95.0 { 95.0 } else { cache_efficiency * 1.3 },  // 缓存命中率提高30%
            cpu_usage_percent: cpu_pressure * 0.8  // CPU使用减少20%
          }
        }
        "memory_cleanup" => {
          {
            ..current_usage,
            memory_usage_mb: current_usage.memory_usage_mb * 0.7,  // 内存使用减少30%
            active_tasks: (current_usage.active_tasks as Float * 0.8) as Int  // 活跃任务减少20%
          }
        }
        "cpu_optimization" => {
          {
            ..current_usage,
            cpu_usage_percent: cpu_pressure * 0.7,  // CPU使用减少30%
            cache_hit_rate: cache_efficiency * 1.15  // 缓存命中率提高15%
          }
        }
        "resource_consolidation" => {
          {
            ..current_usage,
            memory_usage_mb: current_usage.memory_usage_mb * 1.2,  // 内存使用增加20%（合并缓存）
            cpu_usage_percent: cpu_pressure * 1.1,  // CPU使用增加10%（合并处理）
            active_tasks: (current_usage.active_tasks as Float * 0.9) as Int  // 活跃任务减少10%
          }
        }
        _ => current_usage  // 无变化
      }
      
      // 记录优化历史
      optimization_history = optimization_history.push({
        timestamp: current_usage.timestamp,
        action: optimization_action,
        before_memory: current_usage.memory_usage_mb,
        after_memory: optimized_usage.memory_usage_mb,
        before_cpu: current_usage.cpu_usage_percent,
        after_cpu: optimized_usage.cpu_usage_percent,
        before_cache: current_usage.cache_hit_rate,
        after_cache: optimized_usage.cache_hit_rate
      })
      
      (optimization_action, optimized_usage)
    }
    
    fn get_optimization_history() {
      optimization_history
    }
  }
  
  let optimizer = resource_optimizer()
  
  // 创建不同场景的资源使用情况
  let scenarios = [
    // 场景1：内存压力大，碎片严重
    {
      timestamp: 1000,
      memory_usage_mb: 3500.0,  // 3.5GB，超过80%
      cpu_usage_percent: 45.0,
      active_tasks: 120,
      cache_hit_rate: 75.0,
      memory_fragmentation_percent: 40.0
    },
    // 场景2：CPU压力大，缓存效率低
    {
      timestamp: 2000,
      memory_usage_mb: 2000.0,  // 2GB，约50%
      cpu_usage_percent: 90.0,  // 90%，超过85%
      active_tasks: 80,
      cache_hit_rate: 60.0,     // 60%，低于70%
      memory_fragmentation_percent: 15.0
    },
    // 场景3：内存压力大，CPU空闲
    {
      timestamp: 3000,
      memory_usage_mb: 3000.0,  // 3GB，超过70%
      cpu_usage_percent: 30.0,  // 30%，低于50%
      active_tasks: 100,
      cache_hit_rate: 85.0,
      memory_fragmentation_percent: 20.0
    },
    // 场景4：资源使用正常
    {
      timestamp: 4000,
      memory_usage_mb: 1500.0,  // 1.5GB，约37%
      cpu_usage_percent: 60.0,  // 60%，适中
      active_tasks: 60,
      cache_hit_rate: 80.0,
      memory_fragmentation_percent: 10.0
    },
    // 场景5：资源使用率低
    {
      timestamp: 5000,
      memory_usage_mb: 800.0,   // 800MB，低于30%
      cpu_usage_percent: 25.0,  // 25%，低于30%
      active_tasks: 30,
      cache_hit_rate: 90.0,
      memory_fragmentation_percent: 5.0
    }
  ]
  
  // 应用优化策略
  let mut optimization_results = []
  let mut current_usage = scenarios[0]
  
  for scenario in scenarios {
    let (action, optimized_usage) = optimizer(current_usage)
    optimization_results = optimization_results.push({
      scenario: scenario,
      action_taken: action,
      optimized_usage: optimized_usage
    })
    current_usage = optimized_usage
  }
  
  // 验证优化结果
  assert_eq(optimization_results.length(), 5)
  
  // 验证场景1：内存压缩
  assert_eq(optimization_results[0].action_taken, "memory_compaction")
  assert_true(optimization_results[0].optimized_usage.memory_fragmentation_percent < scenarios[0].memory_fragmentation_percent)
  assert_true(optimization_results[0].optimized_usage.memory_usage_mb < scenarios[0].memory_usage_mb)
  
  // 验证场景2：缓存优化
  assert_eq(optimization_results[1].action_taken, "cache_optimization")
  assert_true(optimization_results[1].optimized_usage.cache_hit_rate > scenarios[1].cache_hit_rate)
  assert_true(optimization_results[1].optimized_usage.cpu_usage_percent < scenarios[1].cpu_usage_percent)
  
  // 验证场景3：内存清理
  assert_eq(optimization_results[2].action_taken, "memory_cleanup")
  assert_true(optimization_results[2].optimized_usage.memory_usage_mb < scenarios[2].memory_usage_mb)
  assert_true(optimization_results[2].optimized_usage.active_tasks < scenarios[2].active_tasks)
  
  // 验证场景4：无需优化
  assert_eq(optimization_results[3].action_taken, "no_action")
  
  // 验证场景5：资源整合
  assert_eq(optimization_results[4].action_taken, "resource_consolidation")
  assert_true(optimization_results[4].optimized_usage.memory_usage_mb > scenarios[4].memory_usage_mb)
  assert_true(optimization_results[4].optimized_usage.cpu_usage_percent > scenarios[4].cpu_usage_percent)
  assert_true(optimization_results[4].optimized_usage.active_tasks < scenarios[4].active_tasks)
  
  // 验证优化历史
  let history = optimizer.get_optimization_history()
  assert_eq(history.length(), 5)
  
  // 验证总体优化效果
  let initial_memory = scenarios[0].memory_usage_mb
  let final_memory = optimization_results[4].optimized_usage.memory_usage_mb
  let initial_cpu = scenarios[0].cpu_usage_percent
  let final_cpu = optimization_results[4].optimized_usage.cpu_usage_percent
  
  // 资源使用应该更加平衡
  let initial_imbalance = (initial_memory / 4096.0 * 100.0) - initial_cpu
  let final_imbalance = (final_memory / 4096.0 * 100.0) - final_cpu
  
  assert_true(final_imbalance.abs() <= initial_imbalance.abs())
}