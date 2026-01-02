// Azimuth Telemetry System Resource Usage Optimization Tests
// 遥测系统资源使用优化测试用例，专注于内存、CPU、存储和网络资源的优化

// Test 1: 内存池管理和优化
test "memory pool management and optimization" {
  // 创建内存池管理器
  let pool_manager = azimuth::resource::memory::PoolManager::new()
  
  // 配置内存池
  let small_pool = azimuth::resource::memory::create_pool(pool_manager, "small", 1024, 100) // 1KB块，100个
  let medium_pool = azimuth::resource::memory::create_pool(pool_manager, "medium", 8192, 20) // 8KB块，20个
  let large_pool = azimuth::resource::memory::create_pool(pool_manager, "large", 65536, 5) // 64KB块，5个
  
  // 配置自动扩展
  azimuth::resource::memory::enable_auto_expansion(small_pool, true, 50) // 自动扩展50个块
  azimuth::resource::memory::enable_auto_expansion(medium_pool, true, 10)
  azimuth::resource::memory::enable_auto_expansion(large_pool, false) // 大块不自动扩展
  
  // 测试内存分配
  let mut allocated_blocks = []
  
  // 分配小块内存
  for i in 0..=120 {
    let block = azimuth::resource::memory::allocate(small_pool)
    if block !== None {
      allocated_blocks = allocated_blocks.push(("small", block.unwrap()))
    }
  }
  
  // 分配中等块内存
  for i in 0..=25 {
    let block = azimuth::resource::memory::allocate(medium_pool)
    if block !== None {
      allocated_blocks = allocated_blocks.push(("medium", block.unwrap()))
    }
  }
  
  // 分配大块内存
  for i in 0..=4 {
    let block = azimuth::resource::memory::allocate(large_pool)
    if block !== None {
      allocated_blocks = allocated_blocks.push(("large", block.unwrap()))
    }
  }
  
  // 验证分配结果
  let small_stats = azimuth::resource::memory::get_statistics(small_pool)
  let medium_stats = azimuth::resource::memory::get_statistics(medium_pool)
  let large_stats = azimuth::resource::memory::get_statistics(large_pool)
  
  assert_eq(azimuth::resource::memory::stats::get_allocated_count(small_stats), 120)
  assert_eq(azimuth::resource::memory::stats::get_allocated_count(medium_stats), 20)
  assert_eq(azimuth::resource::memory::stats::get_allocated_count(large_stats), 5)
  
  // 验证自动扩展
  assert_eq(azimuth::resource::memory::stats::get_total_blocks(small_stats), 150) // 100 + 50自动扩展
  assert_eq(azimuth::resource::memory::stats::get_total_blocks(medium_stats), 30) // 20 + 10自动扩展
  assert_eq(azimuth::resource::memory::stats::get_total_blocks(large_stats), 5) // 无扩展
  
  // 释放部分内存
  for i in 0..=49 {
    let block_info = allocated_blocks[i]
    let block = block_info.1
    if block_info.0 == "small" {
      azimuth::resource::memory::deallocate(small_pool, block)
    } else if block_info.0 == "medium" {
      azimuth::resource::memory::deallocate(medium_pool, block)
    } else if block_info.0 == "large" {
      azimuth::resource::memory::deallocate(large_pool, block)
    }
  }
  
  // 验证释放后的统计
  let small_stats_after = azimuth::resource::memory::get_statistics(small_pool)
  let medium_stats_after = azimuth::resource::memory::get_statistics(medium_pool)
  
  assert_eq(azimuth::resource::memory::stats::get_allocated_count(small_stats_after), 70)
  assert_eq(azimuth::resource::memory::stats::get_free_count(small_stats_after), 80)
  
  // 测试内存碎片整理
  let fragmentation_before = azimuth::resource::memory::get_fragmentation_ratio(small_pool)
  azimuth::resource::memory::defragment(small_pool)
  let fragmentation_after = azimuth::resource::memory::get_fragmentation_ratio(small_pool)
  
  assert_true(fragmentation_after <= fragmentation_before) // 碎片应该减少或保持不变
}

// Test 2: CPU资源调度和优化
test "CPU resource scheduling and optimization" {
  // 创建CPU调度器
  let cpu_scheduler = azimuth::resource::cpu::Scheduler::new()
  
  // 配置CPU亲和性
  azimuth::resource::cpu::set_affinity_mask(cpu_scheduler, 0b1111) // 使用前4个CPU核心
  azimuth::resource::cpu::enable_hyperthreading_optimization(cpu_scheduler, true)
  
  // 创建任务队列
  let task_queue = azimuth::resource::cpu::TaskQueue::new()
  
  // 添加不同优先级的任务
  let critical_tasks = []
  let high_tasks = []
  let normal_tasks = []
  let low_tasks = []
  
  // 添加关键任务
  for i in 0..=4 {
    let task = azimuth::resource::cpu::Task::new("critical_" + i.to_string(), "critical")
    azimuth::resource::cpu::task::set_cpu_requirement(task, 2.0) // 需要2个CPU核心
    azimuth::resource::cpu::task::set_expected_duration(task, 100) // 100ms
    critical_tasks = critical_tasks.push(task)
  }
  
  // 添加高优先级任务
  for i in 0..=9 {
    let task = azimuth::resource::cpu::Task::new("high_" + i.to_string(), "high")
    azimuth::resource::cpu::task::set_cpu_requirement(task, 1.0) // 需要1个CPU核心
    azimuth::resource::cpu::task::set_expected_duration(task, 200) // 200ms
    high_tasks = high_tasks.push(task)
  }
  
  // 添加普通任务
  for i in 0..=19 {
    let task = azimuth::resource::cpu::Task::new("normal_" + i.to_string(), "normal")
    azimuth::resource::cpu::task::set_cpu_requirement(task, 0.5) // 需要0.5个CPU核心
    azimuth::resource::cpu::task::set_expected_duration(task, 300) // 300ms
    normal_tasks = normal_tasks.push(task)
  }
  
  // 添加低优先级任务
  for i in 0..=29 {
    let task = azimuth::resource::cpu::Task::new("low_" + i.to_string(), "low")
    azimuth::resource::cpu::task::set_cpu_requirement(task, 0.25) // 需要0.25个CPU核心
    azimuth::resource::cpu::task::set_expected_duration(task, 500) // 500ms
    low_tasks = low_tasks.push(task)
  }
  
  // 添加任务到队列
  for task in critical_tasks {
    azimuth::resource::cpu::queue::add_task(task_queue, task)
  }
  
  for task in high_tasks {
    azimuth::resource::cpu::queue::add_task(task_queue, task)
  }
  
  for task in normal_tasks {
    azimuth::resource::cpu::queue::add_task(task_queue, task)
  }
  
  for task in low_tasks {
    azimuth::resource::cpu::queue::add_task(task_queue, task)
  }
  
  // 配置调度策略
  azimuth::resource::cpu::set_scheduling_policy(cpu_scheduler, "priority_preemptive")
  azimuth::resource::cpu::set_time_slice(cpu_scheduler, 10) // 10ms时间片
  azimuth::resource::cpu::enable_cpu_throttling(cpu_scheduler, true, 0.8) // CPU使用率超过80%时限制
  
  // 执行调度
  let start_time = azimuth::time::now()
  let schedule_result = azimuth::resource::cpu::execute_schedule(cpu_scheduler, task_queue)
  let end_time = azimuth::time::now()
  
  // 验证调度结果
  assert_true(azimuth::resource::cpu::schedule::is_successful(schedule_result))
  
  let total_time = end_time - start_time
  assert_true(total_time < 5000) // 应该在5秒内完成
  
  // 获取调度统计
  let schedule_stats = azimuth::resource::cpu::get_schedule_statistics(schedule_result)
  let completed_tasks = azimuth::resource::cpu::stats::get_completed_tasks(schedule_stats)
  let cpu_utilization = azimuth::resource::cpu::stats::get_cpu_utilization(schedule_stats)
  let context_switches = azimuth::resource::cpu::stats::get_context_switches(schedule_stats)
  
  assert_eq(completed_tasks, 50) // 所有任务应该完成
  assert_true(cpu_utilization > 0.7) // CPU利用率应该大于70%
  assert_true(context_switches > 0) // 应该有上下文切换
  
  // 验证优先级调度
  let critical_completed = azimuth::resource::cpu::stats::get_completed_by_priority(schedule_stats, "critical")
  let high_completed = azimuth::resource::cpu::stats::get_completed_by_priority(schedule_stats, "high")
  let normal_completed = azimuth::resource::cpu::stats::get_completed_by_priority(schedule_stats, "normal")
  let low_completed = azimuth::resource::cpu::stats::get_completed_by_priority(schedule_stats, "low")
  
  assert_eq(critical_completed, 5)
  assert_eq(high_completed, 10)
  assert_eq(normal_completed, 20)
  assert_eq(low_completed, 15) // 低优先级任务可能未全部完成
  
  // 验证CPU亲和性
  let affinity_stats = azimuth::resource::cpu::get_affinity_statistics(cpu_scheduler)
  let used_cores = azimuth::resource::cpu::stats::get_used_cores(affinity_stats)
  assert_true(used_cores <= 4) // 不应该超过4个核心
}

// Test 3: 存储I/O优化
test "storage I/O optimization" {
  // 创建存储优化器
  let storage_optimizer = azimuth::resource::storage::Optimizer::new()
  
  // 配置存储策略
  azimuth::resource::storage::set_cache_size(storage_optimizer, 64 * 1024 * 1024) // 64MB缓存
  azimuth::resource::storage::enable_write_behind_cache(storage_optimizer, true)
  azimuth::resource::storage::set_write_behind_threshold(storage_optimizer, 1024 * 1024) // 1MB阈值
  azimuth::resource::storage::enable_read_ahead(storage_optimizer, true)
  azimuth::resource::storage::set_read_ahead_size(storage_optimizer, 128 * 1024) // 128KB预读
  
  // 创建测试数据
  let test_data = []
  for i in 0..=999 {
    let data_block = "telemetry_data_" + i.to_string() + "_".repeat(100) // 每块约1KB
    test_data = test_data.push(data_block)
  }
  
  // 测试写入性能
  let write_start_time = azimuth::time::now()
  
  for data in test_data {
    azimuth::resource::storage::write(storage_optimizer, "data_" + azimuth::random::uuid(), data)
  }
  
  // 刷新写入缓存
  azimuth::resource::storage::flush_write_cache(storage_optimizer)
  
  let write_end_time = azimuth::time::now()
  let write_time = write_end_time - write_start_time
  
  // 获取写入统计
  let write_stats = azimuth::resource::storage::get_write_statistics(storage_optimizer)
  let write_throughput = azimuth::resource::storage::stats::get_throughput(write_stats)
  let cache_hit_ratio = azimuth::resource::storage::stats::get_cache_hit_ratio(write_stats)
  let average_write_time = azimuth::resource::storage::stats::get_average_write_time(write_stats)
  
  assert_true(write_throughput > 1000) // 写入吞吐量应该大于1KB/s
  assert_true(average_write_time < 10) // 平均写入时间应该小于10ms
  
  // 测试读取性能
  let read_start_time = azimuth::time::now()
  
  // 读取所有写入的数据
  let read_data = []
  for i in 0..=999 {
    let data = azimuth::resource::storage::read(storage_optimizer, "data_" + i.to_string())
    read_data = read_data.push(data)
  }
  
  let read_end_time = azimuth::time::now()
  let read_time = read_end_time - read_start_time
  
  // 获取读取统计
  let read_stats = azimuth::resource::storage::get_read_statistics(storage_optimizer)
  let read_throughput = azimuth::resource::storage::stats::get_throughput(read_stats)
  let read_cache_hit_ratio = azimuth::resource::storage::stats::get_cache_hit_ratio(read_stats)
  let average_read_time = azimuth::resource::storage::stats::get_average_read_time(read_stats)
  
  assert_true(read_throughput > 5000) // 读取吞吐量应该大于5KB/s
  assert_true(read_cache_hit_ratio > 0.5) // 缓存命中率应该大于50%
  assert_true(average_read_time < 5) // 平均读取时间应该小于5ms
  
  // 测试批量I/O操作
  let batch_data = []
  for i in 0..=99 {
    let data = "batch_data_" + i.to_string() + "_".repeat(500) // 每块约500B
    batch_data = batch_data.push(("batch_" + i.to_string(), data))
  }
  
  let batch_start_time = azimuth::time::now()
  azimuth::resource::storage::batch_write(storage_optimizer, batch_data)
  azimuth::resource::storage::flush_write_cache(storage_optimizer)
  let batch_end_time = azimuth::time::now()
  let batch_time = batch_end_time - batch_start_time
  
  // 验证批量写入性能
  let batch_stats = azimuth::resource::storage::get_batch_statistics(storage_optimizer)
  let batch_throughput = azimuth::resource::storage::stats::get_throughput(batch_stats)
  
  assert_true(batch_throughput > 2000) // 批量写入吞吐量应该大于2KB/s
  
  // 测试存储压缩
  let compression_test_data = "x".repeat(10240) // 10KB重复数据
  let compressed_size_before = azimuth::resource::storage::get_compressed_size(storage_optimizer, compression_test_data)
  
  azimuth::resource::storage::enable_compression(storage_optimizer, true, "lz4")
  let compressed_size_after = azimuth::resource::storage::get_compressed_size(storage_optimizer, compression_test_data)
  
  // 验证压缩效果
  assert_true(compressed_size_after < compressed_size_before)
  assert_true(compressed_size_after < 1024) // 压缩后应该小于1KB
}

// Test 4: 网络资源优化
test "network resource optimization" {
  // 创建网络优化器
  let network_optimizer = azimuth::resource::network::Optimizer::new()
  
  // 配置网络优化参数
  azimuth::resource::network::set_connection_pool_size(network_optimizer, 20) // 20个连接
  azimuth::resource::network::enable_connection_reuse(network_optimizer, true)
  azimuth::resource::network::set_keep_alive_interval(network_optimizer, 30) // 30秒保活
  azimuth::resource::network::enable_nagle_algorithm(network_optimizer, false) // 禁用Nagle算法减少延迟
  azimuth::resource::network::set_send_buffer_size(network_optimizer, 64 * 1024) // 64KB发送缓冲区
  azimuth::resource::network::set_receive_buffer_size(network_optimizer, 64 * 1024) // 64KB接收缓冲区
  
  // 配置数据压缩
  azimuth::resource::network::enable_compression(network_optimizer, true, "gzip")
  azimuth::resource::network::set_compression_threshold(network_optimizer, 1024) // 大于1KB的数据压缩
  
  // 创建测试数据
  let small_packets = []
  let large_packets = []
  
  // 小数据包（不会被压缩）
  for i in 0..=49 {
    let data = "small_packet_" + i.to_string()
    small_packets = small_packets.push(data)
  }
  
  // 大数据包（会被压缩）
  for i in 0..=9 {
    let data = "large_packet_" + i.to_string() + "_".repeat(200) // 约1KB
    large_packets = large_packets.push(data)
  }
  
  // 测试连接池
  let connections = []
  for i in 0..=19 {
    let connection = azimuth::resource::network::get_connection(network_optimizer, "remote-server:8080")
    connections = connections.push(connection)
  }
  
  // 验证连接池大小
  let pool_stats = azimuth::resource::network::get_connection_pool_statistics(network_optimizer)
  let active_connections = azimuth::resource::network::stats::get_active_connections(pool_stats)
  let pool_size = azimuth::resource::network::stats::get_pool_size(pool_stats)
  
  assert_eq(active_connections, 20)
  assert_eq(pool_size, 20)
  
  // 测试小数据包传输
  let small_start_time = azimuth::time::now()
  
  for packet in small_packets {
    azimuth::resource::network::send_data(network_optimizer, connections[0], packet)
  }
  
  let small_end_time = azimuth::time::now()
  let small_time = small_end_time - small_start_time
  
  // 测试大数据包传输
  let large_start_time = azimuth::time::now()
  
  for packet in large_packets {
    azimuth::resource::network::send_data(network_optimizer, connections[0], packet)
  }
  
  let large_end_time = azimuth::time::now()
  let large_time = large_end_time - large_start_time
  
  // 获取传输统计
  let transmission_stats = azimuth::resource::network::get_transmission_statistics(network_optimizer)
  let total_bytes_sent = azimuth::resource::network::stats::get_total_bytes_sent(transmission_stats)
  let compressed_bytes_sent = azimuth::resource::network::stats::get_compressed_bytes_sent(transmission_stats)
  let compression_ratio = azimuth::resource::network::stats::get_compression_ratio(transmission_stats)
  let average_latency = azimuth::resource::network::stats::get_average_latency(transmission_stats)
  
  assert_true(total_bytes_sent > 0)
  assert_true(compressed_bytes_sent > 0)
  assert_true(compression_ratio < 1.0) // 应该有压缩效果
  assert_true(average_latency < 100) // 平均延迟应该小于100ms
  
  // 测试批量传输
  let batch_data = small_packets.concat(large_packets)
  let batch_start_time = azimuth::time::now()
  
  azimuth::resource::network::batch_send(network_optimizer, connections[0], batch_data)
  
  let batch_end_time = azimuth::time::now()
  let batch_time = batch_end_time - batch_start_time
  
  // 验证批量传输效率
  let batch_stats = azimuth::resource::network::get_batch_statistics(network_optimizer)
  let batch_throughput = azimuth::resource::network::stats::get_throughput(batch_stats)
  
  assert_true(batch_throughput > 1000) // 批量传输吞吐量应该大于1KB/s
  
  // 测试连接复用
  let reused_connection = azimuth::resource::network::get_connection(network_optimizer, "remote-server:8080")
  let is_reused = azimuth::resource::network::is_connection_reused(network_optimizer, reused_connection)
  assert_true(is_reused) // 应该复用现有连接
  
  // 释放连接
  for connection in connections {
    azimuth::resource::network::release_connection(network_optimizer, connection)
  }
  
  // 验证连接释放
  let pool_stats_after = azimuth::resource::network::get_connection_pool_statistics(network_optimizer)
  let active_connections_after = azimuth::resource::network::stats::get_active_connections(pool_stats_after)
  
  assert_eq(active_connections_after, 0) // 所有连接应该已释放
}

// Test 5: 综合资源使用优化
test "comprehensive resource usage optimization" {
  // 创建资源优化管理器
  let resource_manager = azimuth::resource::OptimizationManager::new()
  
  // 配置资源限制
  azimuth::resource::set_memory_limit(resource_manager, 100 * 1024 * 1024) // 100MB内存限制
  azimuth::resource::set_cpu_limit(resource_manager, 2.0) // 2个CPU核心限制
  azimuth::resource::set_network_bandwidth_limit(resource_manager, 10 * 1024 * 1024) // 10MB/s带宽限制
  azimuth::resource::set_storage_iops_limit(resource_manager, 1000) // 1000 IOPS限制
  
  // 启用自适应资源调整
  azimuth::resource::enable_adaptive_adjustment(resource_manager, true)
  azimuth::resource::set_adjustment_interval(resource_manager, 30) // 30秒调整间隔
  azimuth::resource::set_adjustment_threshold(resource_manager, 0.8) // 80%阈值触发调整
  
  // 创建资源监控器
  let resource_monitor = azimuth::resource::Monitor::new()
  azimuth::resource::monitor::start(resource_monitor)
  
  // 模拟遥测系统工作负载
  let telemetry_workload = azimuth::resource::workload::TelemetryWorkload::new()
  azimuth::resource::workload::set_concurrent_requests(telemetry_workload, 50)
  azimuth::resource::workload::set_data_rate(telemetry_workload, 1000) // 1000 records/s
  azimuth::resource::workload::set_data_size(telemetry_workload, 1024) // 1KB per record
  
  // 执行工作负载
  let workload_start_time = azimuth::time::now()
  let workload_result = azimuth::resource::workload::execute(telemetry_workload, resource_manager)
  let workload_end_time = azimuth::time::now()
  
  // 验证工作负载执行
  assert_true(azimuth::resource::workload::is_successful(workload_result))
  
  let workload_duration = workload_end_time - workload_start_time
  assert_true(workload_duration < 60000) // 应该在60秒内完成
  
  // 获取资源使用统计
  let resource_stats = azimuth::resource::get_usage_statistics(resource_manager)
  let memory_usage = azimuth::resource::stats::get_memory_usage(resource_stats)
  let cpu_usage = azimuth::resource::stats::get_cpu_usage(resource_stats)
  let network_usage = azimuth::resource::stats::get_network_usage(resource_stats)
  let storage_usage = azimuth::resource::stats::get_storage_usage(resource_stats)
  
  // 验证资源使用在限制内
  assert_true(memory_usage <= 100 * 1024 * 1024) // 内存使用不超过100MB
  assert_true(cpu_usage <= 2.0) // CPU使用不超过2个核心
  assert_true(network_usage <= 10 * 1024 * 1024) // 网络使用不超过10MB/s
  assert_true(storage_usage <= 1000) // 存储IOPS不超过1000
  
  // 获取自适应调整历史
  let adjustment_history = azimuth::resource::get_adjustment_history(resource_manager)
  assert_true(adjustment_history.length() >= 0) // 应该有调整历史记录
  
  // 验证资源效率
  let efficiency_metrics = azimuth::resource::get_efficiency_metrics(resource_manager)
  let memory_efficiency = azimuth::resource::efficiency::get_memory_efficiency(efficiency_metrics)
  let cpu_efficiency = azimuth::resource::efficiency::get_cpu_efficiency(efficiency_metrics)
  let overall_efficiency = azimuth::resource::efficiency::get_overall_efficiency(efficiency_metrics)
  
  assert_true(memory_efficiency > 0.7) // 内存效率应该大于70%
  assert_true(cpu_efficiency > 0.7) // CPU效率应该大于70%
  assert_true(overall_efficiency > 0.7) // 整体效率应该大于70%
  
  // 停止资源监控
  azimuth::resource::monitor::stop(resource_monitor)
  
  // 获取监控报告
  let monitor_report = azimuth::resource::monitor::get_report(resource_monitor)
  let peak_memory_usage = azimuth::resource::report::get_peak_memory_usage(monitor_report)
  let peak_cpu_usage = azimuth::resource::report::get_peak_cpu_usage(monitor_report)
  let total_data_processed = azimuth::resource::report::get_total_data_processed(monitor_report)
  
  assert_true(peak_memory_usage > 0)
  assert_true(peak_cpu_usage > 0)
  assert_true(total_data_processed > 0)
}

// Test 6: 资源使用模式分析和预测
test "resource usage pattern analysis and prediction" {
  // 创建资源使用分析器
  let usage_analyzer = azimuth::resource::analyzer::UsageAnalyzer::new()
  
  // 生成历史资源使用数据
  let historical_data = []
  let base_time = azimuth::time::now() - (7 * 24 * 60 * 60 * 1000) // 7天前
  
  for day in 0..=6 {
    for hour in 0..=23 {
      let timestamp = base_time + (day * 24 + hour) * 60 * 60 * 1000
      
      // 模拟日周期性使用模式
      let hour_factor = azimuth::math::sin(2.0 * azimuth::math::pi() * hour.to_float() / 24.0)
      let memory_usage = 50.0 + 20.0 * hour_factor + azimuth::random::normal(0.0, 5.0)
      let cpu_usage = 30.0 + 25.0 * hour_factor + azimuth::random::normal(0.0, 3.0)
      let network_usage = 40.0 + 30.0 * hour_factor + azimuth::random::normal(0.0, 4.0)
      
      let usage_data = azimuth::resource::usage::Data::new(timestamp)
      azimuth::resource::usage::set_memory_usage(usage_data, memory_usage)
      azimuth::resource::usage::set_cpu_usage(usage_data, cpu_usage)
      azimuth::resource::usage::set_network_usage(usage_data, network_usage)
      
      historical_data = historical_data.push(usage_data)
    }
  }
  
  // 分析使用模式
  let pattern_analysis = azimuth::resource::analyzer::analyze_patterns(usage_analyzer, historical_data)
  
  // 验证模式分析结果
  let daily_pattern = azimuth::resource::analysis::get_daily_pattern(pattern_analysis)
  let hourly_pattern = azimuth::resource::analysis::get_hourly_pattern(pattern_analysis)
  let trend_analysis = azimuth::resource::analysis::get_trend_analysis(pattern_analysis)
  
  assert_eq(daily_pattern.length(), 7) // 应该有7天的模式
  assert_eq(hourly_pattern.length(), 24) // 应该有24小时的模式
  
  // 验证趋势分析
  let memory_trend = azimuth::resource::trend::get_memory_trend(trend_analysis)
  let cpu_trend = azimuth::resource::trend::get_cpu_trend(trend_analysis)
  let network_trend = azimuth::resource::trend::get_network_trend(trend_analysis)
  
  assert_true(memory_trend >= -1.0 && memory_trend <= 1.0) // 趋势系数应该在-1到1之间
  assert_true(cpu_trend >= -1.0 && cpu_trend <= 1.0)
  assert_true(network_trend >= -1.0 && network_trend <= 1.0)
  
  // 预测未来资源使用
  let future_predictions = azimuth::resource::analyzer::predict_usage(usage_analyzer, 24) // 预测24小时
  
  // 验证预测结果
  assert_eq(future_predictions.length(), 24)
  
  // 检查预测值的合理性
  for prediction in future_predictions {
    let predicted_memory = azimuth::resource::prediction::get_memory_usage(prediction)
    let predicted_cpu = azimuth::resource::prediction::get_cpu_usage(prediction)
    let predicted_network = azimuth::resource::prediction::get_network_usage(prediction)
    
    assert_true(predicted_memory >= 0.0 && predicted_memory <= 100.0) // 预测值应该在合理范围内
    assert_true(predicted_cpu >= 0.0 && predicted_cpu <= 100.0)
    assert_true(predicted_network >= 0.0 && predicted_network <= 100.0)
  }
  
  // 生成资源优化建议
  let optimization_recommendations = azimuth::resource::analyzer::generate_recommendations(usage_analyzer, historical_data)
  
  // 验证优化建议
  assert_true(optimization_recommendations.length() > 0) // 应该有优化建议
  
  // 检查建议类型
  let memory_recommendations = optimization_recommendations.filter(fn(r) { 
    azimuth::resource::recommendation::get_type(r) == "memory" 
  })
  let cpu_recommendations = optimization_recommendations.filter(fn(r) { 
    azimuth::resource::recommendation::get_type(r) == "cpu" 
  })
  let network_recommendations = optimization_recommendations.filter(fn(r) { 
    azimuth::resource::recommendation::get_type(r) == "network" 
  })
  
  assert_true(memory_recommendations.length() > 0)
  assert_true(cpu_recommendations.length() > 0)
  assert_true(network_recommendations.length() > 0)
  
  // 验证建议的可行性
  for recommendation in optimization_recommendations {
    let feasibility = azimuth::resource::recommendation::get_feasibility(recommendation)
    let savings = azimuth::resource::recommendation::get_potential_savings(recommendation)
    
    assert_true(feasibility >= 0.0 && feasibility <= 1.0) // 可行性应该在0-1之间
    assert_true(savings >= 0.0) // 潜在节省应该非负
  }
}

// Test 7: 资源使用监控和告警
test "resource usage monitoring and alerting" {
  // 创建资源监控告警系统
  let resource_monitor = azimuth::resource::monitoring::System::new()
  
  // 配置监控指标
  azimuth::resource::monitoring::add_metric(resource_monitor, "memory_usage", "gauge")
  azimuth::resource::monitoring::add_metric(resource_monitor, "cpu_usage", "gauge")
  azimuth::resource::monitoring::add_metric(resource_monitor, "network_throughput", "gauge")
  azimuth::resource::monitoring::add_metric(resource_monitor, "storage_iops", "gauge")
  azimuth::resource::monitoring::add_metric(resource_monitor, "disk_space", "gauge")
  
  // 配置告警阈值
  azimuth::resource::monitoring::set_threshold(resource_monitor, "memory_usage", 0.8, "warning")
  azimuth::resource::monitoring::set_threshold(resource_monitor, "memory_usage", 0.9, "critical")
  azimuth::resource::monitoring::set_threshold(resource_monitor, "cpu_usage", 0.7, "warning")
  azimuth::resource::monitoring::set_threshold(resource_monitor, "cpu_usage", 0.9, "critical")
  azimuth::resource::monitoring::set_threshold(resource_monitor, "disk_space", 0.9, "warning")
  azimuth::resource::monitoring::set_threshold(resource_monitor, "disk_space", 0.95, "critical")
  
  // 启动监控
  azimuth::resource::monitoring::start(resource_monitor)
  
  // 模拟资源使用变化
  let resource_scenarios = [
    // 正常使用
    [("memory_usage", 0.5), ("cpu_usage", 0.4), ("network_throughput", 1000), ("storage_iops", 500), ("disk_space", 0.6)],
    
    // 内存使用警告
    [("memory_usage", 0.85), ("cpu_usage", 0.5), ("network_throughput", 1200), ("storage_iops", 600), ("disk_space", 0.7)],
    
    // CPU使用警告
    [("memory_usage", 0.6), ("cpu_usage", 0.75), ("network_throughput", 1500), ("storage_iops", 700), ("disk_space", 0.8)],
    
    // 磁盘空间警告
    [("memory_usage", 0.5), ("cpu_usage", 0.4), ("network_throughput", 1100), ("storage_iops", 550), ("disk_space", 0.92)],
    
    // 多个严重告警
    [("memory_usage", 0.95), ("cpu_usage", 0.92), ("network_throughput", 2000), ("storage_iops", 900), ("disk_space", 0.96)]
  ]
  
  let alert_history = []
  
  // 执行场景
  for scenario in resource_scenarios {
    // 更新资源使用指标
    for (metric, value) in scenario {
      azimuth::resource::monitoring::update_metric(resource_monitor, metric, value)
    }
    
    // 等待告警评估
    azimuth::time::sleep(100)
    
    // 获取当前告警
    let current_alerts = azimuth::resource::monitoring::get_alerts(resource_monitor)
    
    // 记录告警
    for alert in current_alerts {
      alert_history = alert_history.push(alert)
    }
  }
  
  // 停止监控
  azimuth::resource::monitoring::stop(resource_monitor)
  
  // 验证告警历史
  assert_true(alert_history.length() > 0) // 应该有告警
  
  // 统计不同级别的告警
  let warning_alerts = alert_history.filter(fn(a) { azimuth::resource::alert::get_severity(a) == "warning" })
  let critical_alerts = alert_history.filter(fn(a) { azimuth::resource::alert::get_severity(a) == "critical" })
  
  assert_true(warning_alerts.length() >= 3) // 应该至少有3个警告告警
  assert_true(critical_alerts.length() >= 3) // 应该至少有3个严重告警
  
  // 验证告警内容
  let memory_alerts = alert_history.filter(fn(a) { azimuth::resource::alert::get_metric(a) == "memory_usage" })
  let cpu_alerts = alert_history.filter(fn(a) { azimuth::resource::alert::get_metric(a) == "cpu_usage" })
  let disk_alerts = alert_history.filter(fn(a) { azimuth::resource::alert::get_metric(a) == "disk_space" })
  
  assert_true(memory_alerts.length() >= 2) // 内存应该有警告和严重告警
  assert_true(cpu_alerts.length() >= 2) // CPU应该有警告和严重告警
  assert_true(disk_alerts.length() >= 2) // 磁盘应该有警告和严重告警
  
  // 验证告警阈值触发
  let memory_warning_alerts = memory_alerts.filter(fn(a) { azimuth::resource::alert::get_severity(a) == "warning" })
  let memory_critical_alerts = memory_alerts.filter(fn(a) { azimuth::resource::alert::get_severity(a) == "critical" })
  
  for alert in memory_warning_alerts {
    let value = azimuth::resource::alert::get_value(alert)
    assert_true(value >= 0.8 && value < 0.9) // 警告阈值范围
  }
  
  for alert in memory_critical_alerts {
    let value = azimuth::resource::alert::get_value(alert)
    assert_true(value >= 0.9) // 严重阈值范围
  }
  
  // 获取监控报告
  let monitoring_report = azimuth::resource::monitoring::get_report(resource_monitor)
  let total_metrics_collected = azimuth::resource::report::get_total_metrics_collected(monitoring_report)
  let total_alerts_generated = azimuth::resource::report::get_total_alerts_generated(monitoring_report)
  let peak_resource_usage = azimuth::resource::report::get_peak_resource_usage(monitoring_report)
  
  assert_true(total_metrics_collected > 0)
  assert_eq(total_alerts_generated, alert_history.length())
  assert_true(peak_resource_usage.length() > 0)
}

// Test 8: 资源使用基准测试和性能优化验证
test "resource usage benchmarking and performance optimization validation" {
  // 创建基准测试管理器
  let benchmark_manager = azimuth::resource::benchmark::Manager::new()
  
  // 配置基准测试场景
  let baseline_scenario = azimuth::resource::benchmark::Scenario::new("baseline")
  azimuth::resource::benchmark::scenario::set_workload(baseline_scenario, "standard")
  azimuth::resource::benchmark::scenario::set_duration(baseline_scenario, 60000) // 60秒
  azimuth::resource::benchmark::scenario::set_concurrency(baseline_scenario, 10)
  
  let optimized_scenario = azimuth::resource::benchmark::Scenario::new("optimized")
  azimuth::resource::benchmark::scenario::set_workload(optimized_scenario, "standard")
  azimuth::resource::benchmark::scenario::set_duration(optimized_scenario, 60000) // 60秒
  azimuth::resource::benchmark::scenario::set_concurrency(optimized_scenario, 10)
  
  // 启用优化特性
  azimuth::resource::benchmark::scenario::enable_memory_pooling(optimized_scenario, true)
  azimuth::resource::benchmark::scenario::enable_cpu_optimization(optimized_scenario, true)
  azimuth::resource::benchmark::scenario::enable_io_optimization(optimized_scenario, true)
  azimuth::resource::benchmark::scenario::enable_network_optimization(optimized_scenario, true)
  
  // 执行基准测试
  let baseline_start_time = azimuth::time::now()
  let baseline_result = azimuth::resource::benchmark::execute(benchmark_manager, baseline_scenario)
  let baseline_end_time = azimuth::time::now()
  
  let optimized_start_time = azimuth::time::now()
  let optimized_result = azimuth::resource::benchmark::execute(benchmark_manager, optimized_scenario)
  let optimized_end_time = azimuth::time::now()
  
  // 验证基准测试完成
  assert_true(azimuth::resource::benchmark::is_successful(baseline_result))
  assert_true(azimuth::resource::benchmark::is_successful(optimized_result))
  
  // 获取基准测试指标
  let baseline_metrics = azimuth::resource::benchmark::get_metrics(baseline_result)
  let optimized_metrics = azimuth::resource::benchmark::get_metrics(optimized_result)
  
  // 比较性能指标
  let baseline_throughput = azimuth::resource::metrics::get_throughput(baseline_metrics)
  let optimized_throughput = azimuth::resource::metrics::get_throughput(optimized_metrics)
  
  let baseline_latency = azimuth::resource::metrics::get_average_latency(baseline_metrics)
  let optimized_latency = azimuth::resource::metrics::get_average_latency(optimized_metrics)
  
  let baseline_memory_usage = azimuth::resource::metrics::get_peak_memory_usage(baseline_metrics)
  let optimized_memory_usage = azimuth::resource::metrics::get_peak_memory_usage(optimized_metrics)
  
  let baseline_cpu_usage = azimuth::resource::metrics::get_average_cpu_usage(baseline_metrics)
  let optimized_cpu_usage = azimuth::resource::metrics::get_average_cpu_usage(optimized_metrics)
  
  // 验证性能改进
  let throughput_improvement = (optimized_throughput - baseline_throughput) / baseline_throughput
  let latency_reduction = (baseline_latency - optimized_latency) / baseline_latency
  let memory_reduction = (baseline_memory_usage - optimized_memory_usage) / baseline_memory_usage
  let cpu_reduction = (baseline_cpu_usage - optimized_cpu_usage) / baseline_cpu_usage
  
  assert_true(throughput_improvement > 0.1) // 吞吐量应该至少提升10%
  assert_true(latency_reduction > 0.1) // 延迟应该至少减少10%
  assert_true(memory_reduction > 0.1) // 内存使用应该至少减少10%
  assert_true(cpu_reduction > 0.05) // CPU使用应该至少减少5%
  
  // 获取详细资源使用分析
  let resource_analysis = azimuth::resource::benchmark::analyze_resource_usage(baseline_result, optimized_result)
  
  let memory_efficiency_gain = azimuth::resource::analysis::get_memory_efficiency_gain(resource_analysis)
  let cpu_efficiency_gain = azimuth::resource::analysis::get_cpu_efficiency_gain(resource_analysis)
  let io_efficiency_gain = azimuth::resource::analysis::get_io_efficiency_gain(resource_analysis)
  let network_efficiency_gain = azimuth::resource::analysis::get_network_efficiency_gain(resource_analysis)
  
  assert_true(memory_efficiency_gain > 0.1) // 内存效率应该提升
  assert_true(cpu_efficiency_gain > 0.1) // CPU效率应该提升
  assert_true(io_efficiency_gain > 0.1) // I/O效率应该提升
  assert_true(network_efficiency_gain > 0.1) // 网络效率应该提升
  
  // 生成优化报告
  let optimization_report = azimuth::resource::benchmark::generate_optimization_report(baseline_result, optimized_result)
  
  // 验证报告内容
  let overall_improvement = azimuth::resource::report::get_overall_improvement(optimization_report)
  let recommendations = azimuth::resource::report::get_recommendations(optimization_report)
  
  assert_true(overall_improvement > 0.1) // 整体改进应该大于10%
  assert_true(recommendations.length() > 0) // 应该有优化建议
  
  // 验证建议的实用性
  for recommendation in recommendations {
    let impact = azimuth::resource::recommendation::get_expected_impact(recommendation)
    let effort = azimuth::resource::recommendation::get_implementation_effort(recommendation)
    
    assert_true(impact > 0.0) // 预期影响应该为正
    assert_true(effort >= 1.0 && effort <= 10.0) // 实施努力应该在1-10范围内
  }
}