// Azimuth 遥测系统性能基准测试用例
// 专注于系统性能评估、负载测试、资源使用情况等

// 测试1: 遥测数据收集性能基准
test "遥测数据收集性能基准测试" {
  // 定义性能指标
  type PerformanceMetrics = {
    operation_name: String
    total_operations: Int
    total_time_ms: Int
    avg_time_ms: Float
    min_time_ms: Int
    max_time_ms: Int
    throughput_ops_per_sec: Float
    memory_usage_mb: Float
    cpu_usage_percent: Float
  }
  
  // 定义基准测试配置
  type BenchmarkConfig = {
    operation_count: Int
    concurrent_threads: Int
    data_size_bytes: Int
    batch_size: Int
    warmup_iterations: Int
  }
  
  // 模拟数据点生成
  let generate_test_data = fn(count: Int, size_bytes: Int) -> Array[String] {
    let mut data = []
    let base_size = size_bytes / 20 // 假设每个字符约20字节
    
    for i in 0..count {
      let payload = "data-".repeat(base_size) + i.to_string()
      data = data.push(payload)
    }
    
    data
  }
  
  // 模拟数据收集操作
  let simulate_data_collection = fn(data: Array[String], batch_size: Int) -> Int {
    let start_time = 1640995200000 // 模拟时间戳
    
    // 模拟处理时间基于数据量
    let processing_time = (data.length() / batch_size) * 5 // 每批次5毫秒
    let end_time = start_time + processing_time
    
    end_time - start_time
  }
  
  // 运行基准测试
  let run_benchmark = fn(config: BenchmarkConfig, operation: (Array[String], Int) -> Int) -> PerformanceMetrics {
    // 生成测试数据
    let test_data = generate_test_data(config.operation_count, config.data_size_bytes)
    
    // 预热
    let warmup_data = test_data.slice(0, config.warmup_iterations)
    let _ = operation(warmup_data, config.batch_size)
    
    // 实际测试
    let mut execution_times = []
    
    for i in 0..config.operation_count {
      let batch_start = i * config.batch_size
      let batch_end = if (i + 1) * config.batch_size > test_data.length() {
        test_data.length()
      } else {
        (i + 1) * config.batch_size
      }
      
      let batch_data = test_data.slice(batch_start, batch_end)
      let execution_time = operation(batch_data, config.batch_size)
      execution_times = execution_times.push(execution_time)
    }
    
    // 计算性能指标
    let total_operations = config.operation_count
    let total_time = execution_times.reduce(fn(acc, time) { acc + time }, 0)
    let avg_time = (total_time as Float) / (execution_times.length() as Float)
    let min_time = execution_times.reduce(fn(acc, time) { if time < acc { time } else { acc } }, 999999)
    let max_time = execution_times.reduce(fn(acc, time) { if time > acc { time } else { acc } }, 0)
    let throughput = (total_operations as Float) / ((total_time as Float) / 1000.0)
    
    // 模拟资源使用情况
    let memory_usage = (config.data_size_bytes as Float) / (1024.0 * 1024.0) * 1.5 // 1.5倍数据大小
    let cpu_usage = if config.concurrent_threads > 1 {
      80.0 + (config.concurrent_threads as Float) * 5.0
    } else {
      60.0
    }
    
    {
      operation_name: "data_collection",
      total_operations,
      total_time_ms: total_time,
      avg_time_ms: avg_time,
      min_time_ms: min_time,
      max_time_ms: max_time,
      throughput_ops_per_sec: throughput,
      memory_usage_mb: memory_usage,
      cpu_usage_percent: cpu_usage
    }
  }
  
  // 创建基准测试配置
  let small_config = {
    operation_count: 100,
    concurrent_threads: 1,
    data_size_bytes: 1024,      // 1KB
    batch_size: 10,
    warmup_iterations: 10
  }
  
  let medium_config = {
    operation_count: 1000,
    concurrent_threads: 2,
    data_size_bytes: 10240,     // 10KB
    batch_size: 50,
    warmup_iterations: 50
  }
  
  let large_config = {
    operation_count: 10000,
    concurrent_threads: 4,
    data_size_bytes: 102400,    // 100KB
    batch_size: 100,
    warmup_iterations: 100
  }
  
  // 运行基准测试
  let small_metrics = run_benchmark(small_config, simulate_data_collection)
  let medium_metrics = run_benchmark(medium_config, simulate_data_collection)
  let large_metrics = run_benchmark(large_config, simulate_data_collection)
  
  // 验证基准测试结果
  
  // 小规模测试
  assert_eq(small_metrics.total_operations, 100)
  assert_true(small_metrics.avg_time_ms > 0)
  assert_true(small_metrics.throughput_ops_per_sec > 0)
  assert_eq(small_metrics.memory_usage_mb, (1024.0 * 1.5) / (1024.0 * 1024.0)) // 约1.5KB
  
  // 中等规模测试
  assert_eq(medium_metrics.total_operations, 1000)
  assert_true(medium_metrics.avg_time_ms > small_metrics.avg_time_ms) // 更多数据应该有更长的平均时间
  assert_true(medium_metrics.memory_usage_mb > small_metrics.memory_usage_mb) // 更多内存使用
  
  // 大规模测试
  assert_eq(large_metrics.total_operations, 10000)
  assert_true(large_metrics.avg_time_ms > medium_metrics.avg_time_ms)
  assert_true(large_metrics.memory_usage_mb > medium_metrics.memory_usage_mb)
  assert_true(large_metrics.cpu_usage_percent > medium_metrics.cpu_usage_percent) // 更多并发线程
  
  // 验证吞吐量随数据量的变化
  assert_true(small_metrics.throughput_ops_per_sec > medium_metrics.throughput_ops_per_sec)
  assert_true(medium_metrics.throughput_ops_per_sec > large_metrics.throughput_ops_per_sec)
}

// 测试2: 内存使用效率基准测试
test "内存使用效率基准测试" {
  // 定义内存使用记录
  type MemoryRecord = {
    timestamp: Int
    allocated_mb: Float
    used_mb: Float
    peak_mb: Float
    gc_count: Int
    gc_time_ms: Int
  }
  
  // 定义内存压力测试配置
  type MemoryStressConfig = {
    initial_objects: Int
    growth_rate: Float
    max_objects: Int
    object_size_bytes: Int
    gc_threshold_mb: Float
  }
  
  // 模拟内存分配
  let simulate_memory_allocation = fn(object_count: Int, object_size: Int) -> Float {
    // 简化的内存分配模拟
    let base_overhead = 50.0 // MB 基础开销
    let object_memory = (object_count as Float) * (object_size as Float) / (1024.0 * 1024.0)
    base_overhead + object_memory
  }
  
  // 模拟垃圾回收
  let simulate_garbage_collection = fn(current_memory: Float, threshold: Float) -> (Float, Int) {
    if current_memory > threshold {
      // 模拟GC回收30%内存
      let reclaimed = current_memory * 0.3
      let gc_time = ((current_memory - threshold) * 10.0) as Int // 模拟GC时间
      (current_memory - reclaimed, gc_time)
    } else {
      (current_memory, 0)
    }
  }
  
  // 运行内存压力测试
  let run_memory_stress_test = fn(config: MemoryStressConfig) -> Array[MemoryRecord] {
    let mut records = []
    let mut current_objects = config.initial_objects
    let mut total_gc_count = 0
    let mut total_gc_time = 0
    let mut peak_memory = 0.0
    
    while current_objects < config.max_objects {
      // 分配新对象
      let allocated_memory = simulate_memory_allocation(current_objects, config.object_size_bytes)
      
      // 检查是否需要GC
      let (memory_after_gc, gc_time) = simulate_garbage_collection(allocated_memory, config.gc_threshold_mb)
      
      if gc_time > 0 {
        total_gc_count = total_gc_count + 1
        total_gc_time = total_gc_time + gc_time
      }
      
      // 更新峰值内存
      if memory_after_gc > peak_memory {
        peak_memory = memory_after_gc
      }
      
      // 记录内存状态
      let record = {
        timestamp: 1640995200 + (current_objects * 100),
        allocated_mb: allocated_memory,
        used_mb: memory_after_gc,
        peak_mb: peak_memory,
        gc_count: total_gc_count,
        gc_time_ms: total_gc_time
      }
      
      records = records.push(record)
      
      // 增加对象数量
      current_objects = (current_objects as Float * config.growth_rate) as Int
    }
    
    records
  }
  
  // 创建内存压力测试配置
  let stress_config = {
    initial_objects: 1000,
    growth_rate: 1.2,  // 每次增长20%
    max_objects: 10000,
    object_size_bytes: 1024, // 1KB对象
    gc_threshold_mb: 100.0   // 100MB GC阈值
  }
  
  // 运行内存压力测试
  let memory_records = run_memory_stress_test(stress_config)
  
  // 验证内存压力测试结果
  assert_true(memory_records.length() > 0)
  
  // 验证内存增长趋势
  let first_record = memory_records[0]
  let last_record = memory_records[memory_records.length() - 1]
  
  assert_true(last_record.allocated_mb > first_record.allocated_mb)
  assert_true(last_record.peak_mb >= first_record.peak_mb)
  
  // 验证GC活动
  assert_true(last_record.gc_count >= 0)
  assert_true(last_record.gc_time_ms >= 0)
  
  // 验证内存效率指标
  let memory_efficiency = (last_record.used_mb / last_record.allocated_mb) * 100.0
  assert_true(memory_efficiency > 0 && memory_efficiency <= 100.0)
  
  // 计算平均GC时间
  let avg_gc_time = if last_record.gc_count > 0 {
    (last_record.gc_time_ms as Float) / (last_record.gc_count as Float)
  } else {
    0.0
  }
  
  assert_true(avg_gc_time >= 0)
}

// 测试3: 并发处理性能基准测试
test "并发处理性能基准测试" {
  // 定义并发测试结果
  type ConcurrencyResult = {
    thread_count: Int
    total_operations: Int
    total_time_ms: Int
    throughput_ops_per_sec: Float
    avg_latency_ms: Float
    p95_latency_ms: Float
    p99_latency_ms: Float
    error_rate_percent: Float
  }
  
  // 定义并发测试配置
  type ConcurrencyConfig = {
    thread_counts: Array[Int]
    operations_per_thread: Int
    test_duration_ms: Int
    think_time_ms: Int
  }
  
  // 模拟并发操作
  let simulate_concurrent_operations = fn(thread_count: Int, operations_per_thread: Int, think_time: Int) -> ConcurrencyResult {
    let total_operations = thread_count * operations_per_thread
    
    // 模拟并发执行时间：随着线程数增加，由于资源竞争，执行时间非线性增长
    let base_time = operations_per_thread * 10 // 每个操作10ms基础时间
    let contention_factor = if thread_count == 1 {
      1.0
    } else if thread_count <= 4 {
      1.2
    } else if thread_count <= 8 {
      1.5
    } else {
      2.0
    }
    
    let total_time = (base_time as Float * contention_factor) as Int
    
    // 计算延迟分布
    let base_latency = 10.0
    let latency_variance = if thread_count > 1 {
      (thread_count as Float) * 2.0
    } else {
      0.0
    }
    
    let avg_latency = base_latency + latency_variance
    let p95_latency = avg_latency * 1.8
    let p99_latency = avg_latency * 2.5
    
    // 计算错误率：高并发时错误率增加
    let error_rate = if thread_count <= 2 {
      0.0
    } else if thread_count <= 4 {
      0.1
    } else if thread_count <= 8 {
      0.5
    } else {
      2.0
    }
    
    let throughput = (total_operations as Float) / ((total_time as Float) / 1000.0)
    
    {
      thread_count,
      total_operations,
      total_time_ms: total_time,
      throughput_ops_per_sec: throughput,
      avg_latency_ms: avg_latency,
      p95_latency_ms: p95_latency,
      p99_latency_ms: p99_latency,
      error_rate_percent: error_rate
    }
  }
  
  // 运行并发基准测试
  let run_concurrency_benchmark = fn(config: ConcurrencyConfig) -> Array[ConcurrencyResult] {
    let mut results = []
    
    for thread_count in config.thread_counts {
      let result = simulate_concurrent_operations(
        thread_count, 
        config.operations_per_thread, 
        config.think_time_ms
      )
      results = results.push(result)
    }
    
    results
  }
  
  // 创建并发测试配置
  let concurrency_config = {
    thread_counts: [1, 2, 4, 8, 16],
    operations_per_thread: 100,
    test_duration_ms: 10000,
    think_time_ms: 5
  }
  
  // 运行并发基准测试
  let concurrency_results = run_concurrency_benchmark(concurrency_config)
  
  // 验证并发测试结果
  assert_eq(concurrency_results.length(), 5)
  
  // 验证单线程基准
  let single_thread = concurrency_results[0]
  assert_eq(single_thread.thread_count, 1)
  assert_eq(single_thread.total_operations, 100)
  assert_eq(single_thread.error_rate_percent, 0.0)
  
  // 验证多线程扩展性
  let two_thread = concurrency_results[1]
  let four_thread = concurrency_results[2]
  let eight_thread = concurrency_results[3]
  let sixteen_thread = concurrency_results[4]
  
  // 验证操作总数随线程数增加
  assert_eq(two_thread.total_operations, 200)
  assert_eq(four_thread.total_operations, 400)
  assert_eq(eight_thread.total_operations, 800)
  assert_eq(sixteen_thread.total_operations, 1600)
  
  // 验证吞吐量变化（理想情况应该线性增长，但实际由于资源竞争可能不是）
  assert_true(two_thread.throughput_ops_per_sec > single_thread.throughput_ops_per_sec)
  
  // 验证延迟随并发增加
  assert_true(two_thread.avg_latency_ms >= single_thread.avg_latency_ms)
  assert_true(four_thread.avg_latency_ms >= two_thread.avg_latency_ms)
  assert_true(eight_thread.avg_latency_ms >= four_thread.avg_latency_ms)
  assert_true(sixteen_thread.avg_latency_ms >= eight_thread.avg_latency_ms)
  
  // 验证错误率随并发增加
  assert_true(two_thread.error_rate_percent >= single_thread.error_rate_percent)
  assert_true(four_thread.error_rate_percent >= two_thread.error_rate_percent)
  assert_true(eight_thread.error_rate_percent >= four_thread.error_rate_percent)
  assert_true(sixteen_thread.error_rate_percent >= eight_thread.error_rate_percent)
  
  // 验证P95和P99延迟
  for result in concurrency_results {
    assert_true(result.p95_latency_ms >= result.avg_latency_ms)
    assert_true(result.p99_latency_ms >= result.p95_latency_ms)
  }
}

// 测试4: 数据序列化与反序列化性能基准测试
test "数据序列化与反序列化性能基准测试" {
  // 定义序列化格式
  enum SerializationFormat {
    JSON
    Binary
    Protobuf
    MessagePack
  }
  
  // 定义序列化性能结果
  type SerializationResult = {
    format: SerializationFormat
    data_size_bytes: Int
    serialized_size_bytes: Int
    serialization_time_ms: Int
    deserialization_time_ms: Int
    compression_ratio: Float
    throughput_ops_per_sec: Float
  }
  
  // 定义测试数据结构
  type TestData = {
    id: String
    timestamp: Int
    value: Float
    tags: Map[String, String]
    metadata: Array[String]
  }
  
  // 创建测试数据
  let create_test_data = fn(count: Int) -> Array[TestData] {
    let mut data = []
    
    for i in 0..count {
      let test_item = {
        id: "item-" + i.to_string(),
        timestamp: 1640995200 + i,
        value: (i as Float) * 1.5,
        tags: Map::from([
          ("service", "test-service"),
          ("environment", "test"),
          ("index", i.to_string())
        ]),
        metadata: ["meta1", "meta2", "meta3"]
      }
      data = data.push(test_item)
    }
    
    data
  }
  
  // 模拟序列化操作
  let simulate_serialization = fn(data: Array[TestData], format: SerializationFormat) -> (Int, Int) {
    let data_size = data.length() * 100 // 假设每个对象100字节
    
    // 不同格式的序列化时间和压缩比
    let (time_per_item, compression_ratio) = match format {
      SerializationFormat::JSON => (2, 0.7),      // 2ms/item, 70%原始大小
      SerializationFormat::Binary => (1, 0.5),    // 1ms/item, 50%原始大小
      SerializationFormat::Protobuf => (1, 0.4),  // 1ms/item, 40%原始大小
      SerializationFormat::MessagePack => (1, 0.45) // 1ms/item, 45%原始大小
    }
    
    let total_time = data.length() * time_per_item
    let serialized_size = (data_size as Float * compression_ratio) as Int
    
    (total_time, serialized_size)
  }
  
  // 模拟反序列化操作
  let simulate_deserialization = fn(serialized_size: Int, format: SerializationFormat) -> Int {
    // 不同格式的反序列化时间
    let time_per_byte = match format {
      SerializationFormat::JSON => 0.01,      // 0.01ms/byte
      SerializationFormat::Binary => 0.005,   // 0.005ms/byte
      SerializationFormat::Protobuf => 0.006, // 0.006ms/byte
      SerializationFormat::MessagePack => 0.007 // 0.007ms/byte
    }
    
    (serialized_size as Float * time_per_byte) as Int
  }
  
  // 运行序列化基准测试
  let run_serialization_benchmark = fn(data_count: Int) -> Array[SerializationResult] {
    let test_data = create_test_data(data_count)
    let formats = [
      SerializationFormat::JSON,
      SerializationFormat::Binary,
      SerializationFormat::Protobuf,
      SerializationFormat::MessagePack
    ]
    
    let mut results = []
    
    for format in formats {
      let (serialization_time, serialized_size) = simulate_serialization(test_data, format)
      let deserialization_time = simulate_deserialization(serialized_size, format)
      
      let data_size = test_data.length() * 100
      let total_time = serialization_time + deserialization_time
      let throughput = (test_data.length() as Float) / ((total_time as Float) / 1000.0)
      let compression_ratio = (serialized_size as Float) / (data_size as Float)
      
      let result = {
        format,
        data_size_bytes: data_size,
        serialized_size_bytes: serialized_size,
        serialization_time_ms: serialization_time,
        deserialization_time_ms: deserialization_time,
        compression_ratio,
        throughput_ops_per_sec: throughput
      }
      
      results = results.push(result)
    }
    
    results
  }
  
  // 运行基准测试
  let serialization_results = run_serialization_benchmark(1000)
  
  // 验证序列化基准测试结果
  assert_eq(serialization_results.length(), 4)
  
  // 验证每种格式的结果
  for result in serialization_results {
    assert_true(result.serialization_time_ms > 0)
    assert_true(result.deserialization_time_ms > 0)
    assert_true(result.serialized_size_bytes > 0)
    assert_true(result.compression_ratio > 0 && result.compression_ratio <= 1.0)
    assert_true(result.throughput_ops_per_sec > 0)
  }
  
  // 查找JSON结果
  let json_result = serialization_results.find(fn(r) { 
    match r.format {
      SerializationFormat::JSON => true,
      _ => false
    }
  })
  
  // 查找Protobuf结果
  let protobuf_result = serialization_results.find(fn(r) { 
    match r.format {
      SerializationFormat::Protobuf => true,
      _ => false
    }
  })
  
  // 验证Protobuf比JSON更紧凑
  match (json_result, protobuf_result) {
    (Some(json), Some(protobuf)) => {
      assert_true(protobuf.compression_ratio < json.compression_ratio)
      assert_true(protobuf.serialized_size_bytes < json.serialized_size_bytes)
    },
    _ => assert_true(false)
  }
  
  // 验证二进制格式通常比文本格式更快
  let binary_result = serialization_results.find(fn(r) { 
    match r.format {
      SerializationFormat::Binary => true,
      _ => false
    }
  })
  
  match (json_result, binary_result) {
    (Some(json), Some(binary)) => {
      assert_true(binary.serialization_time_ms <= json.serialization_time_ms)
    },
    _ => assert_true(false)
  }
}

// 测试5: 系统资源使用监控基准测试
test "系统资源使用监控基准测试" {
  // 定义资源使用记录
  type ResourceUsageRecord = {
    timestamp: Int
    cpu_percent: Float
    memory_mb: Float
    disk_io_mb_per_sec: Float
    network_io_mb_per_sec: Float
    open_file_descriptors: Int
    thread_count: Int
  }
  
  // 定义资源基准结果
  type ResourceBenchmarkResult = {
    test_name: String
    duration_ms: Int
    avg_cpu_percent: Float
    peak_cpu_percent: Float
    avg_memory_mb: Float
    peak_memory_mb: Float
    total_disk_io_mb: Float
    total_network_io_mb: Float
    resource_efficiency_score: Float
  }
  
  // 模拟资源使用监控
  let simulate_resource_monitoring = fn(duration_ms: Int, operation_type: String) -> Array[ResourceUsageRecord] {
    let mut records = []
    let interval_ms = 1000 // 每秒记录一次
    let record_count = duration_ms / interval_ms
    
    // 基础资源使用量
    let (base_cpu, base_memory, base_disk_io, base_network_io) = match operation_type {
      "data_processing" => (60.0, 512.0, 20.0, 10.0),
      "network_io" => (30.0, 256.0, 5.0, 50.0),
      "compute_intensive" => (90.0, 1024.0, 10.0, 5.0),
      "memory_intensive" => (40.0, 2048.0, 30.0, 15.0),
      _ => (20.0, 128.0, 1.0, 1.0)
    }
    
    for i in 0..record_count {
      // 模拟资源使用的波动
      let time_factor = (i as Float) / (record_count as Float)
      let cpu_variation = 10.0 * (time_factor * 3.14).sin()
      let memory_variation = 50.0 * (time_factor * 2.0).sin()
      
      let cpu = (base_cpu + cpu_variation).max(0.0).min(100.0)
      let memory = (base_memory + memory_variation).max(0.0)
      let disk_io = base_disk_io * (1.0 + 0.2 * (time_factor * 4.0).sin())
      let network_io = base_network_io * (1.0 + 0.3 * (time_factor * 3.0).sin())
      
      let record = {
        timestamp: 1640995200 + (i * interval_ms),
        cpu_percent: cpu,
        memory_mb: memory,
        disk_io_mb_per_sec: disk_io,
        network_io_mb_per_sec: network_io,
        open_file_descriptors: 100 + i,
        thread_count: 8 + (i % 4)
      }
      
      records = records.push(record)
    }
    
    records
  }
  
  // 计算资源基准结果
  let calculate_resource_benchmark = fn(records: Array[ResourceUsageRecord], test_name: String) -> ResourceBenchmarkResult {
    if records.length() == 0 {
      return {
        test_name,
        duration_ms: 0,
        avg_cpu_percent: 0.0,
        peak_cpu_percent: 0.0,
        avg_memory_mb: 0.0,
        peak_memory_mb: 0.0,
        total_disk_io_mb: 0.0,
        total_network_io_mb: 0.0,
        resource_efficiency_score: 0.0
      }
    }
    
    let first_record = records[0]
    let last_record = records[records.length() - 1]
    let duration_ms = last_record.timestamp - first_record.timestamp
    
    // 计算CPU指标
    let cpu_values = records.map(fn(r) { r.cpu_percent })
    let avg_cpu = cpu_values.reduce(fn(acc, cpu) { acc + cpu }, 0.0) / (cpu_values.length() as Float)
    let peak_cpu = cpu_values.reduce(fn(acc, cpu) { if cpu > acc { cpu } else { acc } }, 0.0)
    
    // 计算内存指标
    let memory_values = records.map(fn(r) { r.memory_mb })
    let avg_memory = memory_values.reduce(fn(acc, mem) { acc + mem }, 0.0) / (memory_values.length() as Float)
    let peak_memory = memory_values.reduce(fn(acc, mem) { if mem > acc { mem } else { acc } }, 0.0)
    
    // 计算总I/O
    let total_disk_io = records.reduce(fn(acc, r) { acc + r.disk_io_mb_per_sec }, 0.0)
    let total_network_io = records.reduce(fn(acc, r) { acc + r.network_io_mb_per_sec }, 0.0)
    
    // 计算资源效率分数（越低越好，表示资源使用越高效）
    let cpu_score = avg_cpu / 100.0
    let memory_score = (avg_memory / 4096.0).min(1.0) // 假设4GB为最大内存
    let io_score = ((total_disk_io + total_network_io) / 1000.0).min(1.0) // 假设1GB为最大I/O
    let resource_efficiency_score = 100.0 * (1.0 - ((cpu_score + memory_score + io_score) / 3.0))
    
    {
      test_name,
      duration_ms,
      avg_cpu_percent: avg_cpu,
      peak_cpu_percent: peak_cpu,
      avg_memory_mb: avg_memory,
      peak_memory_mb: peak_memory,
      total_disk_io_mb: total_disk_io,
      total_network_io_mb: total_network_io,
      resource_efficiency_score
    }
  }
  
  // 运行不同类型的资源基准测试
  let data_processing_records = simulate_resource_monitoring(10000, "data_processing")
  let network_io_records = simulate_resource_monitoring(10000, "network_io")
  let compute_intensive_records = simulate_resource_monitoring(10000, "compute_intensive")
  let memory_intensive_records = simulate_resource_monitoring(10000, "memory_intensive")
  
  let data_processing_result = calculate_resource_benchmark(data_processing_records, "data_processing")
  let network_io_result = calculate_resource_benchmark(network_io_records, "network_io")
  let compute_intensive_result = calculate_resource_benchmark(compute_intensive_records, "compute_intensive")
  let memory_intensive_result = calculate_resource_benchmark(memory_intensive_records, "memory_intensive")
  
  // 验证数据处理测试结果
  assert_eq(data_processing_result.test_name, "data_processing")
  assert_eq(data_processing_result.duration_ms, 10000)
  assert_true(data_processing_result.avg_cpu_percent > 50.0)
  assert_true(data_processing_result.peak_cpu_percent >= data_processing_result.avg_cpu_percent)
  assert_true(data_processing_result.avg_memory_mb > 400.0)
  
  // 验证网络I/O测试结果
  assert_eq(network_io_result.test_name, "network_io")
  assert_true(network_io_result.avg_cpu_percent < data_processing_result.avg_cpu_percent)
  assert_true(network_io_result.total_network_io_mb > data_processing_result.total_network_io_mb)
  
  // 验证计算密集型测试结果
  assert_eq(compute_intensive_result.test_name, "compute_intensive")
  assert_true(compute_intensive_result.avg_cpu_percent > data_processing_result.avg_cpu_percent)
  assert_true(compute_intensive_result.peak_cpu_percent > 80.0)
  
  // 验证内存密集型测试结果
  assert_eq(memory_intensive_result.test_name, "memory_intensive")
  assert_true(memory_intensive_result.avg_memory_mb > compute_intensive_result.avg_memory_mb)
  assert_true(memory_intensive_result.peak_memory_mb > 1500.0)
  
  // 验证资源效率分数在合理范围内
  let results = [
    data_processing_result,
    network_io_result,
    compute_intensive_result,
    memory_intensive_result
  ]
  
  for result in results {
    assert_true(result.resource_efficiency_score >= 0.0 && result.resource_efficiency_score <= 100.0)
  }
  
  // 验证网络I/O测试的效率分数应该较高（CPU使用率低）
  assert_true(network_io_result.resource_efficiency_score > compute_intensive_result.resource_efficiency_score)
}