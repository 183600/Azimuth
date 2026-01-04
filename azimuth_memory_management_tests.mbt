// Azimuth Memory Management Test Suite
// 内存管理测试套件 - 专注于内存分配、垃圾回收和内存泄漏检测

// 测试1: 内存分配和释放
test "内存分配和释放测试" {
  // 创建内存管理器
  let memory_manager = MemoryManager::new()
  MemoryManager::set_initial_heap_size(memory_manager, 64 * 1024 * 1024) // 64MB初始堆
  MemoryManager::set_max_heap_size(memory_manager, 512 * 1024 * 1024) // 512MB最大堆
  MemoryManager::set_gc_threshold(memory_manager, 0.8) // 80%使用率触发GC
  
  // 初始化内存管理器
  let init_result = MemoryManager::initialize(memory_manager)
  assert_true(init_result.success)
  
  // 测试基本内存分配
  let alloc_sizes = [1024, 4096, 16384, 65536, 262144] // 1KB, 4KB, 16KB, 64KB, 256KB
  let allocated_blocks = []
  
  for size in alloc_sizes {
    let alloc_start = Clock::now_unix_nanos(Clock::system())
    let alloc_result = MemoryManager::allocate(memory_manager, size)
    let alloc_end = Clock::now_unix_nanos(Clock::system())
    
    assert_true(alloc_result.success)
    assert_true(alloc_result.pointer != 0)
    assert_eq(alloc_result.size, size)
    
    // 验证分配时间
    let alloc_time = alloc_end - alloc_start
    assert_true(alloc_time < 10000000) // 小于10ms
    
    allocated_blocks.push(alloc_result)
  }
  
  // 验证内存使用情况
  let memory_stats = MemoryManager::get_stats(memory_manager)
  assert_true(memory_stats.total_allocated > 0)
  assert_true(memory_stats.used_heap > 0)
  assert_true(memory_stats.free_heap > 0)
  assert_true(memory_stats.allocation_count >= alloc_sizes.length())
  
  // 测试内存写入和读取
  for block in allocated_blocks {
    let test_data = "azimuth-telemetry-test-data"
    let write_result = MemoryManager::write_memory(memory_manager, block.pointer, test_data)
    assert_true(write_result.success)
    
    let read_result = MemoryManager::read_memory(memory_manager, block.pointer, test_data.length())
    assert_true(read_result.success)
    assert_eq(read_result.data, test_data)
  }
  
  // 测试内存释放
  let free_start = Clock::now_unix_nanos(Clock::system())
  
  for block in allocated_blocks {
    let free_result = MemoryManager::deallocate(memory_manager, block.pointer)
    assert_true(free_result.success)
  }
  
  let free_end = Clock::now_unix_nanos(Clock::system())
  
  // 验证释放时间
  let free_time = free_end - free_start
  assert_true(free_time < 5000000) // 小于5ms
  
  // 验证释放后的内存状态
  let after_free_stats = MemoryManager::get_stats(memory_manager)
  assert_true(after_free_stats.deallocation_count >= alloc_sizes.length())
  
  // 测试大内存分配
  let large_sizes = [1024 * 1024, 2 * 1024 * 1024, 4 * 1024 * 1024] // 1MB, 2MB, 4MB
  let large_blocks = []
  
  for size in large_sizes {
    let alloc_result = MemoryManager::allocate(memory_manager, size)
    
    // 大内存分配可能失败，取决于可用内存
    if alloc_result.success {
      large_blocks.push(alloc_result)
      
      // 验证大内存块
      let memory_info = MemoryManager::get_memory_info(memory_manager, alloc_result.pointer)
      assert_true(memory_info.size == size)
      assert_true(memory_info.is_allocated)
    }
  }
  
  // 清理大内存块
  for block in large_blocks {
    MemoryManager::deallocate(memory_manager, block.pointer)
  }
  
  // 测试内存对齐
  let alignment_sizes = [8, 16, 32, 64, 128, 256, 512]
  
  for alignment in alignment_sizes {
    let aligned_alloc_result = MemoryManager::allocate_aligned(memory_manager, 4096, alignment)
    assert_true(aligned_alloc_result.success)
    
    let pointer = aligned_alloc_result.pointer
    assert_eq(pointer % alignment, 0) // 验证对齐
    
    MemoryManager::deallocate(memory_manager, pointer)
  }
  
  // 测试内存池
  let pool_config = MemoryPoolConfig::new()
  MemoryPoolConfig::set_block_size(pool_config, 4096) // 4KB块
  MemoryPoolConfig::set_initial_blocks(pool_config, 10)
  MemoryPoolConfig::set_max_blocks(pool_config, 100)
  
  let memory_pool = MemoryManager::create_pool(memory_manager, pool_config)
  assert_true(memory_pool.is_some())
  
  let pool = memory_pool.unwrap()
  
  // 从池中分配
  let pool_blocks = []
  for i in 1..=15 {
    let pool_alloc_result = MemoryPool::allocate(pool)
    if pool_alloc_result.success {
      pool_blocks.push(pool_alloc_result.pointer)
    }
  }
  
  // 验证池分配
  assert_true(pool_blocks.length() >= 10) // 至少初始块数量
  
  // 归还到池中
  for pointer in pool_blocks {
    let pool_free_result = MemoryPool::deallocate(pool, pointer)
    assert_true(pool_free_result.success)
  }
  
  // 验证池状态
  let pool_stats = MemoryPool::get_stats(pool)
  assert_true(pool_stats.available_blocks >= 10)
  assert_eq(pool_stats.used_blocks, 0)
  
  // 关闭内存管理器
  let shutdown_result = MemoryManager::shutdown(memory_manager)
  assert_true(shutdown_result.success)
}

// 测试2: 垃圾回收机制
test "垃圾回收机制测试" {
  // 创建垃圾回收器
  let gc = GarbageCollector::new()
  GarbageCollector::set_gc_type(gc, GcType::Generational)
  GarbageCollector::set_young_generation_size(gc, 16 * 1024 * 1024) // 16MB年轻代
  GarbageCollector::set_old_generation_size(gc, 64 * 1024 * 1024) // 64MB老年代
  GarbageCollector::set_gc_threshold(gc, 0.7) // 70%使用率触发GC
  
  // 初始化垃圾回收器
  let init_result = GarbageCollector::initialize(gc)
  assert_true(init_result.success)
  
  // 创建对象引用跟踪
  let object_tracker = ObjectTracker::new()
  
  // 测试年轻代GC
  let young_objects = []
  for i in 1..=1000 {
    let obj = GarbageCollector::allocate_object(gc, 1024) // 1KB对象
    object_tracker.add_reference(obj, "young-obj-" + i.to_string())
    young_objects.push(obj)
  }
  
  // 验证年轻代使用情况
  let young_gen_stats = GarbageCollector::get_generation_stats(gc, Generation::Young)
  assert_true(young_gen_stats.used_objects >= 1000)
  assert_true(young_gen_stats.used_memory >= 1000 * 1024)
  
  // 触发年轻代GC
  let gc_start = Clock::now_unix_nanos(Clock::system())
  let young_gc_result = GarbageCollector::collect_young(gc)
  let gc_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(young_gc_result.success)
  
  // 验证GC时间
  let gc_time = gc_end - gc_start
  assert_true(gc_time < 100000000) // 小于100ms
  
  // 验证GC后的统计
  let after_young_gc_stats = GarbageCollector::get_generation_stats(gc, Generation::Young)
  assert_true(after_young_gc_stats.gc_count > young_gen_stats.gc_count)
  assert_true(after_young_gc_stats.collected_objects >= 0)
  
  // 释放一半对象的引用，制造垃圾
  for i in 0..=499 {
    object_tracker.remove_reference(young_objects[i])
  }
  
  // 再次触发年轻代GC
  let second_young_gc_result = GarbageCollector::collect_young(gc)
  assert_true(second_young_gc_result.success)
  
  // 验证垃圾被回收
  let after_second_gc_stats = GarbageCollector::get_generation_stats(gc, Generation::Young)
  assert_true(after_second_gc_stats.collected_objects >= 400) // 至少回收400个对象
  
  // 测试老年代GC
  let old_objects = []
  for i in 1..=100 {
    let obj = GarbageCollector::allocate_object(gc, 10 * 1024) // 10KB对象
    object_tracker.add_reference(obj, "old-obj-" + i.to_string())
    old_objects.push(obj)
    
    // 模拟对象晋升到老年代
    GarbageCollector::promote_to_old(gc, obj)
  }
  
  // 验证老年代使用情况
  let old_gen_stats = GarbageCollector::get_generation_stats(gc, Generation::Old)
  assert_true(old_gen_stats.used_objects >= 100)
  
  // 释放部分老年代对象的引用
  for i in 0..=49 {
    object_tracker.remove_reference(old_objects[i])
  }
  
  // 触发老年代GC
  let old_gc_start = Clock::now_unix_nanos(Clock::system())
  let old_gc_result = GarbageCollector::collect_old(gc)
  let old_gc_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(old_gc_result.success)
  
  // 验证老年代GC时间（通常比年轻代慢）
  let old_gc_time = old_gc_end - old_gc_start
  assert_true(old_gc_time < 500000000) // 小于500ms
  
  // 验证老年代GC结果
  let after_old_gc_stats = GarbageCollector::get_generation_stats(gc, Generation::Old)
  assert_true(after_old_gc_stats.collected_objects >= 40) // 至少回收40个对象
  
  // 测试Full GC
  let full_gc_start = Clock::now_unix_nanos(Clock::system())
  let full_gc_result = GarbageCollector::collect_full(gc)
  let full_gc_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(full_gc_result.success)
  
  // 验证Full GC时间（通常最慢）
  let full_gc_time = full_gc_end - full_gc_start
  assert_true(full_gc_time < 1000000000) // 小于1秒
  
  // 验证Full GC统计
  let gc_summary = GarbageCollector::get_gc_summary(gc)
  assert_true(gc_summary.total_gc_count > 0)
  assert_true(gc_summary.total_gc_time > 0)
  assert_true(gc_summary.average_gc_time > 0)
  assert_true(gc_summary.total_collected_objects > 0)
  assert_true(gc_summary.total_collected_memory > 0)
  
  // 测试并发GC
  let concurrent_gc = GarbageCollector::new()
  GarbageCollector::set_gc_type(concurrent_gc, GcType::Concurrent)
  GarbageCollector::initialize(concurrent_gc)
  
  // 在并发GC中分配对象
  let concurrent_objects = []
  for i in 1..=500 {
    let obj = GarbageCollector::allocate_object(concurrent_gc, 2048) // 2KB对象
    object_tracker.add_reference(obj, "concurrent-obj-" + i.to_string())
    concurrent_objects.push(obj)
  }
  
  // 启动并发GC
  let concurrent_gc_result = GarbageCollector::start_concurrent_gc(concurrent_gc)
  assert_true(concurrent_gc_result.success)
  
  // 在GC运行时继续分配对象
  let mut additional_objects = []
  for i in 1..=100 {
    let obj = GarbageCollector::allocate_object(concurrent_gc, 1024) // 1KB对象
    additional_objects.push(obj)
  }
  
  // 等待并发GC完成
  let concurrent_gc_complete = GarbageCollector::wait_for_gc(concurrent_gc, 10000) // 10秒超时
  assert_true(concurrent_gc_complete)
  
  // 验证并发GC结果
  let concurrent_gc_stats = GarbageCollector::get_gc_summary(concurrent_gc)
  assert_true(concurrent_gc_stats.total_gc_count > 0)
  
  // 测试GC调优参数
  let tuned_gc = GarbageCollector::new()
  GarbageCollector::set_gc_type(tuned_gc, GcType::Generational)
  GarbageCollector::set_young_generation_size(tuned_gc, 32 * 1024 * 1024) // 增大年轻代
  GarbageCollector::set_gc_threshold(tuned_gc, 0.6) // 降低GC阈值
  GarbageCollector::set_max_pause_time(tuned_gc, 50) // 50ms最大暂停时间
  
  GarbageCollector::initialize(tuned_gc)
  
  // 分配对象测试调优效果
  let tuned_objects = []
  for i in 1..=2000 {
    let obj = GarbageCollector::allocate_object(tuned_gc, 512) // 512B对象
    tuned_objects.push(obj)
  }
  
  // 触发GC
  let tuned_gc_result = GarbageCollector::collect_young(tuned_gc)
  assert_true(tuned_gc_result.success)
  
  // 验证调优后的GC性能
  let tuned_gc_stats = GarbageCollector::get_gc_summary(tuned_gc)
  assert_true(tuned_gc_stats.average_gc_time <= 50000000) // 平均GC时间应小于50ms
  
  // 清理资源
  object_tracker.clear()
  GarbageCollector::shutdown(gc)
  GarbageCollector::shutdown(concurrent_gc)
  GarbageCollector::shutdown(tuned_gc)
}

// 测试3: 内存泄漏检测
test "内存泄漏检测测试" {
  // 创建内存泄漏检测器
  let leak_detector = MemoryLeakDetector::new()
  MemoryLeakDetector::set_tracking_enabled(leak_detector, true)
  MemoryLeakDetector::set_stack_trace_depth(leak_detector, 10) // 10层堆栈跟踪
  MemoryLeakDetector::set_leak_threshold(leak_detector, 100) // 100次分配后检查泄漏
  
  // 初始化泄漏检测器
  let init_result = MemoryLeakDetector::initialize(leak_detector)
  assert_true(init_result.success)
  
  // 测试正常内存使用（无泄漏）
  let normal_blocks = []
  for i in 1..=50 {
    let block = MemoryLeakDetector::tracked_allocate(leak_detector, 1024)
    normal_blocks.push(block)
    
    // 正常释放
    MemoryLeakDetector::tracked_deallocate(leak_detector, block)
  }
  
  // 检查泄漏
  let leak_check_result1 = MemoryLeakDetector::check_leaks(leak_detector)
  assert_true(leak_check_result1.success)
  assert_eq(leak_check_result1.leaked_blocks.length(), 0) // 应该没有泄漏
  
  // 测试内存泄漏
  let leaked_blocks = []
  for i in 1..=20 {
    let block = MemoryLeakDetector::tracked_allocate(leak_detector, 2048)
    leaked_blocks.push(block)
    
    // 故意不释放，制造泄漏
    if i % 5 == 0 {
      // 每5个释放1个，留下80%的泄漏
      MemoryLeakDetector::tracked_deallocate(leak_detector, block)
    }
  }
  
  // 检查泄漏
  let leak_check_result2 = MemoryLeakDetector::check_leaks(leak_detector)
  assert_true(leak_check_result2.success)
  assert_true(leak_check_result2.leaked_blocks.length() >= 15) // 应该有泄漏
  
  // 验证泄漏报告
  let leak_report = leak_check_result2.leak_report
  assert_true(leak_report.total_leaked_blocks >= 15)
  assert_true(leak_report.total_leaked_memory >= 15 * 2048)
  
  // 检查泄漏详情
  for leaked_block in leak_check_result2.leaked_blocks {
    assert_true(leaked_block.size == 2048)
    assert_true(leaked_block.allocation_time > 0)
    assert_true(leaked_block.stack_trace.length() > 0)
  }
  
  // 测试循环引用泄漏
  let object_a = MemoryLeakDetector::tracked_allocate_object(leak_detector, "ObjectA")
  let object_b = MemoryLeakDetector::tracked_allocate_object(leak_detector, "ObjectB")
  
  // 创建循环引用
  MemoryLeakDetector::add_reference(leak_detector, object_a, object_b)
  MemoryLeakDetector::add_reference(leak_detector, object_b, object_a)
  
  // 释放根引用，但循环引用仍然存在
  MemoryLeakDetector::remove_root_reference(leak_detector, object_a)
  MemoryLeakDetector::remove_root_reference(leak_detector, object_b)
  
  // 检查循环引用泄漏
  let cycle_leak_result = MemoryLeakDetector::check_cyclic_leaks(leak_detector)
  assert_true(cycle_leak_result.success)
  assert_true(cycle_leak_result.cycles.length() > 0)
  
  // 验证循环引用报告
  let cycle_report = cycle_leak_result.cycle_report
  assert_true(cycle_report.total_cycles > 0)
  assert_true(cycle_report.total_objects_in_cycles >= 2)
  
  // 测试内存增长检测
  let growth_tracker = MemoryGrowthTracker::new()
  MemoryGrowthTracker::set_check_interval(growth_tracker, 1000) // 每次分配检查
  MemoryGrowthTracker::set_growth_threshold(growth_tracker, 10.0) // 10倍增长阈值
  
  // 模拟内存增长
  let growing_blocks = []
  for round in 1..=5 {
    let round_size = 1000 * round * 1024 // 每轮增加1MB*轮次
    
    for i in 1..=10 {
      let block = MemoryLeakDetector::tracked_allocate(leak_detector, round_size)
      growing_blocks.push(block)
      
      MemoryGrowthTracker::record_allocation(growth_tracker, block, round_size)
    }
    
    let growth_check = MemoryGrowthTracker::check_growth(growth_tracker)
    if round >= 3 {
      // 在第3轮后应该检测到显著增长
      assert_true(growth_check.memory_growth_factor > 1.0)
    }
  }
  
  // 测试内存碎片检测
  let fragmentation_detector = MemoryFragmentationDetector::new()
  MemoryFragmentationDetector::set_analysis_threshold(fragmentation_detector, 100) // 100次分配后分析
  
  // 创建碎片化内存模式
  let fragmented_blocks = []
  for i in 1..=200 {
    let size = if i % 3 == 0 { 1024 } else if i % 3 == 1 { 4096 } else { 8192 }
    let block = MemoryLeakDetector::tracked_allocate(leak_detector, size)
    fragmented_blocks.push((block, size))
  }
  
  // 随机释放一些块，制造碎片
  for i in 0..=fragmented_blocks.length() - 1 {
    if i % 4 == 0 {
      MemoryLeakDetector::tracked_deallocate(leak_detector, fragmented_blocks[i].0)
    }
  }
  
  // 分析碎片化
  let fragmentation_result = MemoryFragmentationDetector::analyze_fragmentation(fragmentation_detector)
  assert_true(fragmentation_result.success)
  
  // 验证碎片化报告
  let frag_report = fragmentation_result.fragmentation_report
  assert_true(frag_report.total_allocated_blocks > 0)
  assert_true(frag_report.fragmentation_ratio >= 0.0)
  assert_true(frag_report.fragmentation_ratio <= 1.0)
  
  // 测试内存使用模式分析
  let pattern_analyzer = MemoryUsagePatternAnalyzer::new()
  
  // 记录不同的内存使用模式
  for i in 1..=1000 {
    let pattern_type = match i % 4 {
      0 => "burst_allocation",
      1 => "steady_allocation",
      2 => "frequent_small_alloc",
      3 => "occasional_large_alloc"
    }
    
    let size = match pattern_type {
      "burst_allocation" => 1024 * (i % 10 + 1),
      "steady_allocation" => 4096,
      "frequent_small_alloc" => 64,
      "occasional_large_alloc" => 1024 * 1024
    }
    
    let block = MemoryLeakDetector::tracked_allocate(leak_detector, size)
    pattern_analyzer.record_allocation(pattern_type, size)
    
    // 某些模式立即释放
    if pattern_type == "frequent_small_alloc" || (pattern_type == "burst_allocation" && i % 3 == 0) {
      MemoryLeakDetector::tracked_deallocate(leak_detector, block)
    }
  }
  
  // 分析内存使用模式
  let pattern_analysis = pattern_analyzer.analyze_patterns()
  assert_true(pattern_analysis.success)
  
  // 验证模式分析结果
  let analysis_report = pattern_analysis.analysis_report
  assert_true(analysis_report.patterns.length() > 0)
  
  // 检查是否有异常模式
  for pattern in analysis_report.patterns {
    assert_true(pattern.name.length() > 0)
    assert_true(pattern.allocation_count > 0)
    assert_true(pattern.average_size > 0)
    assert_true(pattern.total_allocated > 0)
  }
  
  // 清理泄漏的内存
  for block in leaked_blocks {
    MemoryLeakDetector::tracked_deallocate(leak_detector, block)
  }
  
  for (block, _) in fragmented_blocks {
    MemoryLeakDetector::tracked_deallocate(leak_detector, block)
  }
  
  for block in growing_blocks {
    MemoryLeakDetector::tracked_deallocate(leak_detector, block)
  }
  
  // 最终泄漏检查
  let final_leak_check = MemoryLeakDetector::check_leaks(leak_detector)
  assert_true(final_leak_check.success)
  assert_eq(final_leak_check.leaked_blocks.length(), 0) // 清理后应该没有泄漏
  
  // 关闭检测器
  MemoryLeakDetector::shutdown(leak_detector)
}

// 测试4: 内存池管理
test "内存池管理测试" {
  // 创建内存池管理器
  let pool_manager = MemoryPoolManager::new()
  MemoryPoolManager::set_default_alignment(pool_manager, 16) // 16字节对齐
  MemoryPoolManager::set_max_pools(pool_manager, 10) // 最多10个池
  
  // 初始化池管理器
  let init_result = MemoryPoolManager::initialize(pool_manager)
  assert_true(init_result.success)
  
  // 创建不同大小的内存池
  let pool_sizes = [64, 256, 1024, 4096, 16384] // 64B, 256B, 1KB, 4KB, 16KB
  let pools = []
  
  for size in pool_sizes {
    let pool_config = MemoryPoolConfig::new()
    MemoryPoolConfig::set_block_size(pool_config, size)
    MemoryPoolConfig::set_initial_blocks(pool_config, 20)
    MemoryPoolConfig::set_max_blocks(pool_config, 100)
    MemoryPoolConfig::set_growth_factor(pool_config, 1.5) // 1.5倍增长
    
    let pool = MemoryPoolManager::create_pool(pool_manager, size, pool_config)
    assert_true(pool.is_some())
    
    pools.push(pool.unwrap())
  }
  
  // 测试从不同大小的池中分配
  let allocated_blocks = []
  for i in 0..=pools.length() - 1 {
    let pool = pools[i]
    let pool_size = pool_sizes[i]
    
    let pool_blocks = []
    for j in 1..=25 {
      let alloc_result = MemoryPool::allocate(pool)
      if alloc_result.success {
        pool_blocks.push(alloc_result.pointer)
        
        // 验证分配的块大小
        let block_info = MemoryPool::get_block_info(pool, alloc_result.pointer)
        assert_eq(block_info.size, pool_size)
      }
    }
    
    allocated_blocks.push((pool, pool_blocks))
  }
  
  // 验证池分配统计
  for (pool, blocks) in allocated_blocks {
    let pool_stats = MemoryPool::get_stats(pool)
    assert_true(pool_stats.used_blocks == blocks.length())
    assert_true(pool_stats.allocation_count >= blocks.length())
  }
  
  // 测试内存池释放
  for (pool, blocks) in allocated_blocks {
    for pointer in blocks {
      let free_result = MemoryPool::deallocate(pool, pointer)
      assert_true(free_result.success)
    }
    
    // 验证释放后的状态
    let pool_stats = MemoryPool::get_stats(pool)
    assert_eq(pool_stats.used_blocks, 0)
    assert_true(pool_stats.deallocation_count >= blocks.length())
  }
  
  // 测试内存池自动扩展
  let expand_pool = pools[2] // 使用1KB的池
  let expand_blocks = []
  
  // 分配超过初始数量的块
  for i in 1..=25 {
    let alloc_result = MemoryPool::allocate(expand_pool)
    if alloc_result.success {
      expand_blocks.push(alloc_result.pointer)
    }
  }
  
  // 验证池已扩展
  let expand_stats = MemoryPool::get_stats(expand_pool)
  assert_true(expand_stats.total_blocks > 20) // 应该超过初始数量
  assert_eq(expand_stats.used_blocks, expand_blocks.length())
  
  // 测试内存池收缩
  // 释放大部分块
  for i in 0..=expand_blocks.length() - 6 {
    MemoryPool::deallocate(expand_pool, expand_blocks[i])
  }
  
  // 手动触发收缩
  let shrink_result = MemoryPool::shrink(expand_pool)
  assert_true(shrink_result.success)
  
  // 验证收缩结果
  let shrink_stats = MemoryPool::get_stats(expand_pool)
  assert_true(shrink_stats.total_blocks < expand_stats.total_blocks) // 总块数应该减少
  
  // 清理剩余块
  for i in expand_blocks.length() - 6..=expand_blocks.length() - 1 {
    MemoryPool::deallocate(expand_pool, expand_blocks[i])
  }
  
  // 测试线程安全内存池
  let thread_safe_pool = pools[4] // 使用16KB的池
  let thread_safe_config = MemoryPoolConfig::new()
  MemoryPoolConfig::set_thread_safe(thread_safe_config, true)
  
  let ts_pool = MemoryPoolManager::create_pool(pool_manager, 32768, thread_safe_config) // 32KB线程安全池
  assert_true(ts_pool.is_some())
  
  let ts_pool_instance = ts_pool.unwrap()
  
  // 并发分配测试
  let concurrent_results = []
  for i in 1..=10 {
    let result = Task::spawn(fn() {
      let thread_blocks = []
      for j in 1..=10 {
        let alloc_result = MemoryPool::allocate(ts_pool_instance)
        if alloc_result.success {
          thread_blocks.push(alloc_result.pointer)
        }
        
        // 模拟工作
        Thread::sleep(1)
      }
      
      // 释放块
      for pointer in thread_blocks {
        MemoryPool::deallocate(ts_pool_instance, pointer)
      }
      
      thread_blocks.length()
    })
    concurrent_results.push(result)
  }
  
  // 等待所有线程完成
  let mut total_allocated = 0
  for result in concurrent_results {
    let allocated = Task::await(result)
    total_allocated = total_allocated + allocated
  }
  
  assert_eq(total_allocated, 100) // 10个线程 × 10个块
  
  // 验证线程安全池状态
  let ts_stats = MemoryPool::get_stats(ts_pool_instance)
  assert_eq(ts_stats.used_blocks, 0) // 所有块应该已释放
  assert_true(ts_stats.allocation_count >= 100)
  
  // 测试内存池性能
  let perf_pool = pools[1] // 使用256B的池
  let perf_iterations = 10000
  
  // 预热
  for i in 1..=100 {
    let alloc_result = MemoryPool::allocate(perf_pool)
    if alloc_result.success {
      MemoryPool::deallocate(perf_pool, alloc_result.pointer)
    }
  }
  
  // 性能测试
  let perf_start = Clock::now_unix_nanos(Clock::system())
  
  let perf_blocks = []
  for i in 1..=perf_iterations {
    let alloc_result = MemoryPool::allocate(perf_pool)
    if alloc_result.success {
      perf_blocks.push(alloc_result.pointer)
    }
  }
  
  for pointer in perf_blocks {
    MemoryPool::deallocate(perf_pool, pointer)
  }
  
  let perf_end = Clock::now_unix_nanos(Clock::system())
  let perf_duration = perf_end - perf_start
  
  // 验证性能
  let avg_operation_time = perf_duration.to_float() / (perf_iterations * 2) // 分配+释放
  assert_true(avg_operation_time < 1000) // 平均每次操作小于1微秒
  
  // 测试内存池监控
  let pool_monitor = MemoryPoolMonitor::new()
  MemoryPoolMonitor::add_pool(pool_monitor, perf_pool)
  MemoryPoolMonitor::set_monitoring_interval(pool_monitor, 100) // 100ms间隔
  
  // 启动监控
  let monitor_start = MemoryPoolMonitor::start(pool_monitor)
  assert_true(monitor_start.success)
  
  // 执行一些分配操作
  let monitor_blocks = []
  for i in 1..=50 {
    let alloc_result = MemoryPool::allocate(perf_pool)
    if alloc_result.success {
      monitor_blocks.push(alloc_result.pointer)
    }
  }
  
  // 等待监控数据收集
  Thread::sleep(200)
  
  // 释放块
  for pointer in monitor_blocks {
    MemoryPool::deallocate(perf_pool, pointer)
  }
  
  // 获取监控数据
  let monitor_data = MemoryPoolMonitor::get_monitoring_data(pool_monitor, perf_pool)
  assert_true(monitor_data.success)
  
  // 验证监控数据
  let monitoring_report = monitor_data.monitoring_report
  assert_true(monitoring_report.utilization_samples.length() > 0)
  assert_true(monitoring_report.peak_utilization >= 0.0)
  assert_true(monitoring_report.peak_utilization <= 1.0)
  assert_true(monitoring_report.average_utilization >= 0.0)
  
  // 停止监控
  MemoryPoolMonitor::stop(pool_monitor)
  
  // 清理所有池
  for pool in pools {
    MemoryPoolManager::destroy_pool(pool_manager, pool)
  }
  
  MemoryPoolManager::destroy_pool(pool_manager, ts_pool_instance)
  
  // 关闭池管理器
  MemoryPoolManager::shutdown(pool_manager)
}

// 测试5: 内存映射和虚拟内存
test "内存映射和虚拟内存测试" {
  // 创建虚拟内存管理器
  let vm_manager = VirtualMemoryManager::new()
  VirtualMemoryManager::set_page_size(vm_manager, 4096) // 4KB页大小
  VirtualMemoryManager::set_max_mapped_regions(vm_manager, 100) // 最多100个映射区域
  
  // 初始化虚拟内存管理器
  let init_result = VirtualMemoryManager::initialize(vm_manager)
  assert_true(init_result.success)
  
  // 创建测试文件
  let test_file = "/tmp/azimuth_mmap_test.dat"
  let file_size = 1024 * 1024 // 1MB
  
  // 创建文件并写入数据
  let file_handle = FileSystem::open_file(test_file, "w+")
  assert_true(file_handle.is_ok())
  
  let test_data = "azimuth-telemetry-memory-mapping-test-"
  let mut written_bytes = 0
  
  while written_bytes < file_size {
    let chunk = test_data + written_bytes.to_string()
    let write_result = FileSystem::write_file(file_handle.unwrap(), chunk)
    if write_result.success {
      written_bytes = written_bytes + chunk.length()
    } else {
      break
    }
  }
  
  FileSystem::close_file(file_handle.unwrap())
  
  // 测试内存映射
  let mmap_config = MemoryMapConfig::new()
  MemoryMapConfig::set_protection(mmap_config, MemoryProtection::ReadWrite)
  MemoryMapConfig::set_flags(mmap_config, MemoryMapFlags::Shared)
  
  let mmap_start = Clock::now_unix_nanos(Clock::system())
  let mmap_result = VirtualMemoryManager::map_file(vm_manager, test_file, 0, file_size, mmap_config)
  let mmap_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(mmap_result.success)
  
  // 验证映射时间
  let mmap_time = mmap_end - mmap_start
  assert_true(mmap_time < 100000000) // 小于100ms
  
  let mapped_region = mmap_result.mapped_region
  assert_true(mapped_region.address != 0)
  assert_eq(mapped_region.size, file_size)
  
  // 测试内存映射读取
  let read_start = Clock::now_unix_nanos(Clock::system())
  let read_data = VirtualMemoryManager::read_memory(vm_manager, mapped_region, 0, 1024) // 读取前1KB
  let read_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(read_data.length() == 1024)
  
  // 验证读取时间
  let read_time = read_end - read_start
  assert_true(read_time < 10000000) // 小于10ms
  
  // 验证读取内容
  assert_true(read_data.starts_with(test_data))
  
  // 测试内存映射写入
  let write_data = "MODIFIED-DATA-"
  let write_offset = 512
  
  let write_start = Clock::now_unix_nanos(Clock::system())
  let write_result = VirtualMemoryManager::write_memory(vm_manager, mapped_region, write_offset, write_data)
  let write_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(write_result.success)
  
  // 验证写入时间
  let write_time = write_end - write_start
  assert_true(write_time < 10000000) // 小于10ms
  
  // 验证写入内容
  let verify_data = VirtualMemoryManager::read_memory(vm_manager, mapped_region, write_offset, write_data.length())
  assert_eq(verify_data, write_data)
  
  // 测试内存映射同步
  let sync_start = Clock::now_unix_nanos(Clock::system())
  let sync_result = VirtualMemoryManager::sync(vm_manager, mapped_region)
  let sync_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(sync_result.success)
  
  // 验证同步时间
  let sync_time = sync_end - sync_start
  assert_true(sync_time < 50000000) // 小于50ms
  
  // 验证文件内容已更新
  let updated_file = FileSystem::read_file(test_file)
  assert_true(updated_file.contains(write_data))
  
  // 测试匿名内存映射
  let anon_size = 512 * 1024 // 512KB
  let anon_mmap_result = VirtualMemoryManager::map_anonymous(vm_manager, anon_size, MemoryProtection::ReadWrite)
  
  assert_true(anon_mmap_result.success)
  
  let anon_region = anon_mmap_result.mapped_region
  assert_true(anon_region.address != 0)
  assert_eq(anon_region.size, anon_size)
  
  // 测试匿名内存映射操作
  let anon_test_data = "anonymous-memory-test-data"
  let anon_write_result = VirtualMemoryManager::write_memory(vm_manager, anon_region, 0, anon_test_data)
  assert_true(anon_write_result.success)
  
  let anon_read_result = VirtualMemoryManager::read_memory(vm_manager, anon_region, 0, anon_test_data.length())
  assert_eq(anon_read_result, anon_test_data)
  
  // 测试内存保护
  let protect_region = VirtualMemoryManager::map_anonymous(vm_manager, 4096, MemoryProtection::ReadWrite)
  assert_true(protect_region.success)
  
  // 设置只读保护
  let protect_result = VirtualMemoryManager::protect_memory(vm_manager, protect_region.mapped_region, MemoryProtection::ReadOnly)
  assert_true(protect_result.success)
  
  // 尝试写入只读区域（应该失败）
  let readonly_write_result = VirtualMemoryManager::try_write_memory(vm_manager, protect_region.mapped_region, 0, "test")
  assert_false(readonly_write_result.success)
  
  // 测试内存映射性能
  let perf_file = "/tmp/azimuth_mmap_perf.dat"
  let perf_file_size = 10 * 1024 * 1024 // 10MB
  
  // 创建性能测试文件
  let perf_file_handle = FileSystem::open_file(perf_file, "w+")
  if perf_file_handle.is_ok() {
    let large_data = "x".repeat(1024) // 1KB数据
    for i in 1..=perf_file_size / 1024 {
      FileSystem::write_file(perf_file_handle.unwrap(), large_data)
    }
    FileSystem::close_file(perf_file_handle.unwrap())
  }
  
  // 映射大文件
  let perf_mmap_result = VirtualMemoryManager::map_file(vm_manager, perf_file, 0, perf_file_size, mmap_config)
  assert_true(perf_mmap_result.success)
  
  let perf_region = perf_mmap_result.mapped_region
  
  // 性能测试：随机访问
  let random_access_start = Clock::now_unix_nanos(Clock::system())
  
  for i in 1..=1000 {
    let offset = (Random::next_int() % (perf_file_size - 1024))
    let read_result = VirtualMemoryManager::read_memory(vm_manager, perf_region, offset, 1024)
    assert_true(read_result.success)
  }
  
  let random_access_end = Clock::now_unix_nanos(Clock::system())
  let random_access_time = random_access_end - random_access_start
  
  // 验证随机访问性能
  let avg_random_access_time = random_access_time.to_float() / 1000.0
  assert_true(avg_random_access_time < 100000) // 平均每次访问小于100微秒
  
  // 性能测试：顺序访问
  let sequential_access_start = Clock::now_unix_nanos(Clock::system())
  
  let chunk_size = 64 * 1024 // 64KB块
  for offset in range(0, perf_file_size, chunk_size) {
    let read_result = VirtualMemoryManager::read_memory(vm_manager, perf_region, offset, chunk_size)
    assert_true(read_result.success)
  }
  
  let sequential_access_end = Clock::now_unix_nanos(Clock::system())
  let sequential_access_time = sequential_access_end - sequential_access_start
  
  // 验证顺序访问性能
  let throughput = perf_file_size.to_float() / (sequential_access_time.to_float() / 1000000000.0) // MB/s
  assert_true(throughput > 100.0) // 至少100MB/s
  
  // 测试内存映射解除映射
  let unmap_result = VirtualMemoryManager::unmap(vm_manager, mapped_region)
  assert_true(unmap_result.success)
  
  let anon_unmap_result = VirtualMemoryManager::unmap(vm_manager, anon_region)
  assert_true(anon_unmap_result.success)
  
  let protect_unmap_result = VirtualMemoryManager::unmap(vm_manager, protect_region.mapped_region)
  assert_true(protect_unmap_result.success)
  
  let perf_unmap_result = VirtualMemoryManager::unmap(vm_manager, perf_region)
  assert_true(perf_unmap_result.success)
  
  // 验证虚拟内存统计
  let vm_stats = VirtualMemoryManager::get_stats(vm_manager)
  assert_true(vm_stats.total_mapped_regions >= 4)
  assert_true(vm_stats.current_mapped_regions == 0) // 所有区域已解除映射
  assert_true(vm_stats.total_mapped_memory >= file_size + anon_size + 4096 + perf_file_size)
  
  // 清理测试文件
  FileSystem::delete_file(test_file)
  FileSystem::delete_file(perf_file)
  
  // 关闭虚拟内存管理器
  let shutdown_result = VirtualMemoryManager::shutdown(vm_manager)
  assert_true(shutdown_result.success)
}

// 测试6: 内存优化和压缩
test "内存优化和压缩测试" {
  // 创建内存优化器
  let memory_optimizer = MemoryOptimizer::new()
  MemoryOptimizer::set_compression_enabled(memory_optimizer, true)
  MemoryOptimizer::set_compression_algorithm(memory_optimizer, CompressionAlgorithm::LZ4)
  MemoryOptimizer::set_compression_threshold(memory_optimizer, 1024) // 1KB以上压缩
  
  // 初始化内存优化器
  let init_result = MemoryOptimizer::initialize(memory_optimizer)
  assert_true(init_result.success)
  
  // 测试内存压缩
  let original_data = "azimuth-telemetry-compression-test-".repeat(100) // 约4KB数据
  let original_bytes = original_data.to_bytes()
  
  let compress_start = Clock::now_unix_nanos(Clock::system())
  let compress_result = MemoryOptimizer::compress(memory_optimizer, original_bytes)
  let compress_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(compress_result.success)
  
  // 验证压缩效果
  let compressed_data = compress_result.compressed_data
  let compression_ratio = compressed_data.length().to_float() / original_bytes.length().to_float()
  assert_true(compression_ratio < 0.8) // 压缩率应该小于80%
  
  // 验证压缩时间
  let compress_time = compress_end - compress_start
  assert_true(compress_time < 10000000) // 小于10ms
  
  // 测试内存解压
  let decompress_start = Clock::now_unix_nanos(Clock::system())
  let decompress_result = MemoryOptimizer::decompress(memory_optimizer, compressed_data)
  let decompress_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(decompress_result.success)
  assert_eq(decompress_result.decompressed_data, original_bytes)
  
  // 验证解压时间
  let decompress_time = decompress_end - decompress_start
  assert_true(decompress_time < 10000000) // 小于10ms
  
  // 测试内存去重
  let deduplicator = MemoryDeduplicator::new()
  MemoryDeduplicator::set_chunk_size(deduplicator, 1024) // 1KB块
  MemoryDeduplicator::set_hash_algorithm(deduplicator, HashAlgorithm::SHA256)
  
  // 创建重复数据
  let duplicate_chunk = "duplicate-chunk-".repeat(50) // 约1KB重复块
  let unique_chunk = "unique-chunk-".repeat(50) + Random::next_int(1000).to_string()
  
  let test_chunks = [
    duplicate_chunk,
    unique_chunk,
    duplicate_chunk, // 重复
    unique_chunk + "-modified", // 相似但不完全相同
    duplicate_chunk  // 重复
  ]
  
  // 添加到去重器
  let deduplicated_refs = []
  for chunk in test_chunks {
    let dedup_result = MemoryDeduplicator::add_chunk(deduplicator, chunk.to_bytes())
    assert_true(dedup_result.success)
    deduplicated_refs.push(dedup_result.reference)
  }
  
  // 验证去重效果
  let dedup_stats = MemoryDeduplicator::get_stats(deduplicator)
  assert_true(dedup_stats.total_chunks >= 3) // 至少3个唯一块
  assert_true(dedup_stats.duplicate_chunks >= 2) // 至少2个重复块
  assert_true(dedup_stats.space_saved > 0)
  
  // 测试内存池优化
  let pool_optimizer = MemoryPoolOptimizer::new()
  MemoryPoolOptimizer::set_auto_optimization(pool_optimizer, true)
  MemoryPoolOptimizer::set_optimization_interval(pool_optimizer, 5000) // 5秒间隔
  MemoryPoolOptimizer::set_fragmentation_threshold(pool_optimizer, 0.3) // 30%碎片率阈值
  
  // 创建内存池
  let pool_config = MemoryPoolConfig::new()
  pool_config.set_block_size(4096)
  pool_config.set_initial_blocks(50)
  pool_config.set_max_blocks(200)
  
  let pool = MemoryPool::new(pool_config)
  MemoryPool::initialize(pool)
  
  // 模拟碎片化使用模式
  let allocated_blocks = []
  for i in 1..=100 {
    let alloc_result = MemoryPool::allocate(pool)
    if alloc_result.success {
      allocated_blocks.push(alloc_result.pointer)
      
      // 随机释放一些块，制造碎片
      if i % 3 == 0 && allocated_blocks.length() > 5 {
        let random_index = Random::next_int() % allocated_blocks.length()
        let block_to_free = allocated_blocks[random_index]
        MemoryPool::deallocate(pool, block_to_free)
        allocated_blocks.remove(random_index)
      }
    }
  }
  
  // 检查碎片化状态
  let fragmentation_check = MemoryPool::check_fragmentation(pool)
  assert_true(fragmentation_check.success)
  
  let fragmentation_ratio = fragmentation_check.fragmentation_ratio
  if fragmentation_ratio > 0.3 {
    // 如果碎片化严重，执行优化
    let optimize_start = Clock::now_unix_nanos(Clock::system())
    let optimize_result = MemoryPoolOptimizer::optimize_pool(pool_optimizer, pool)
    let optimize_end = Clock::now_unix_nanos(Clock::system())
    
    if optimize_result.success {
      // 验证优化效果
      let post_optimize_check = MemoryPool::check_fragmentation(pool)
      assert_true(post_optimize_check.fragmentation_ratio < fragmentation_ratio)
      
      // 验证优化时间
      let optimize_time = optimize_end - optimize_start
      assert_true(optimize_time < 100000000) // 小于100ms
    }
  }
  
  // 清理剩余块
  for pointer in allocated_blocks {
    MemoryPool::deallocate(pool, pointer)
  }
  
  // 测试内存预取
  let prefetcher = MemoryPrefetcher::new()
  MemoryPrefetcher::set_prefetch_distance(prefetcher, 2) // 预取2个块
  MemoryPrefetcher::set_access_pattern_detection(prefetcher, true)
  
  // 模拟顺序访问模式
  let sequential_blocks = []
  for i in 1..=20 {
    let alloc_result = MemoryPool::allocate(pool)
    if alloc_result.success {
      sequential_blocks.push(alloc_result.pointer)
      
      // 记录访问模式
      MemoryPrefetcher::record_access(prefetcher, alloc_result.pointer)
    }
  }
  
  // 检测访问模式
  let pattern_result = MemoryPrefetcher::detect_pattern(prefetcher)
  assert_true(pattern_result.success)
  
  let access_pattern = pattern_result.pattern
  assert_true(access_pattern.pattern_type == "sequential") // 应该检测到顺序模式
  
  // 测试内存缓存优化
  let cache_optimizer = MemoryCacheOptimizer::new()
  MemoryCacheOptimizer::set_cache_size(cache_optimizer, 64 * 1024 * 1024) // 64MB缓存
  MemoryCacheOptimizer::set_cache_line_size(cache_optimizer, 64) // 64字节缓存行
  MemoryCacheOptimizer::set_associativity(cache_optimizer, 8) // 8路组相联
  
  // 模拟缓存访问
  let cache_addresses = []
  for i in 1..=1000 {
    let address = (i * 64) % (64 * 1024 * 1024) // 64字节对齐的地址
    cache_addresses.push(address)
    
    // 记录缓存访问
    MemoryCacheOptimizer::record_access(cache_optimizer, address)
  }
  
  // 分析缓存性能
  let cache_analysis = MemoryCacheOptimizer::analyze_performance(cache_optimizer)
  assert_true(cache_analysis.success)
  
  let cache_report = cache_analysis.performance_report
  assert_true(cache_report.hit_rate >= 0.0)
  assert_true(cache_report.hit_rate <= 1.0)
  assert_true(cache_report.miss_rate >= 0.0)
  assert_true(cache_report.miss_rate <= 1.0)
  assert_true(cache_report.hit_rate + cache_report.miss_rate == 1.0)
  
  // 测试内存对齐优化
  let alignment_optimizer = MemoryAlignmentOptimizer::new()
  MemoryAlignmentOptimizer::set_default_alignment(alignment_optimizer, 16) // 16字节默认对齐
  
  // 测试不同对齐要求的分配
  let alignment_tests = [8, 16, 32, 64, 128]
  let aligned_blocks = []
  
  for alignment in alignment_tests {
    let alloc_result = MemoryAlignmentOptimizer::allocate_aligned(alignment_optimizer, 1024, alignment)
    assert_true(alloc_result.success)
    
    let pointer = alloc_result.pointer
    assert_eq(pointer % alignment, 0) // 验证对齐
    
    aligned_blocks.push((pointer, alignment))
  }
  
  // 测试内存布局优化
  let layout_optimizer = MemoryLayoutOptimizer::new()
  MemoryLayoutOptimizer::set_struct_packing(layout_optimizer, true)
  MemoryLayoutOptimizer::set_field_reordering(layout_optimizer, true)
  
  // 定义测试结构体布局
  let struct_fields = [
    ("field1", 8),   // 8字节
    ("field2", 4),   // 4字节
    ("field3", 16),  // 16字节
    ("field4", 1),   // 1字节
    ("field5", 8)    // 8字节
  ]
  
  let layout_result = MemoryLayoutOptimizer::optimize_struct_layout(layout_optimizer, struct_fields)
  assert_true(layout_result.success)
  
  let optimized_layout = layout_result.optimized_layout
  assert_true(optimized_layout.total_size > 0)
  assert_true(optimized_layout.alignment > 0)
  assert_eq(optimized_layout.fields.length(), struct_fields.length())
  
  // 验证字段重排序减少了填充
  let original_layout_size = MemoryLayoutOptimizer::calculate_original_size(struct_fields)
  assert_true(optimized_layout.total_size <= original_layout_size)
  
  // 清理资源
  for (pointer, _) in aligned_blocks {
    MemoryAlignmentOptimizer::deallocate_aligned(alignment_optimizer, pointer)
  }
  
  MemoryPool::destroy(pool)
  
  // 关闭优化器
  MemoryOptimizer::shutdown(memory_optimizer)
  MemoryDeduplicator::shutdown(deduplicator)
  MemoryPoolOptimizer::shutdown(pool_optimizer)
  MemoryPrefetcher::shutdown(prefetcher)
  MemoryCacheOptimizer::shutdown(cache_optimizer)
  MemoryAlignmentOptimizer::shutdown(alignment_optimizer)
  MemoryLayoutOptimizer::shutdown(layout_optimizer)
}