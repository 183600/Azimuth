// Azimuth 性能优化增强测试
// 专注于性能优化、资源管理、缓存策略和算法优化

// 测试1: 缓存策略和实现
test "缓存策略和实现测试" {
  // 创建缓存管理器
  let cache_manager = CacheManager::new()
  
  // 配置LRU缓存
  let lru_cache = CacheManager::create_cache(cache_manager, {
    name: "lru_cache",
    type: "lru",
    max_size: 100,
    ttl_ms: 60000,  // 1分钟TTL
    eviction_policy: "least_recently_used"
  })
  
  // 测试LRU缓存基本操作
  CacheManager::put(lru_cache, "key1", "value1")
  CacheManager::put(lru_cache, "key2", "value2")
  CacheManager::put(lru_cache, "key3", "value3")
  
  // 验证缓存获取
  assert_eq(CacheManager::get(lru_cache, "key1"), Some("value1"))
  assert_eq(CacheManager::get(lru_cache, "key2"), Some("value2"))
  assert_eq(CacheManager::get(lru_cache, "key3"), Some("value3"))
  assert_eq(CacheManager::get(lru_cache, "nonexistent"), None)
  
  // 验证缓存大小
  assert_eq(CacheManager::size(lru_cache), 3)
  
  // 测试LRU淘汰策略
  // 访问key1，使其成为最近使用
  CacheManager::get(lru_cache, "key1")
  
  // 添加更多键，触发淘汰
  for i in 0..=98 {
    CacheManager::put(lru_cache, "key" + (i + 4).to_string(), "value" + (i + 4).to_string())
  }
  
  // 验证缓存大小不超过最大值
  assert_eq(CacheManager::size(lru_cache), 100)
  
  // key1应该仍在缓存中（最近访问过）
  assert_eq(CacheManager::get(lru_cache, "key1"), Some("value1"))
  
  // key2应该已被淘汰（最久未访问）
  assert_eq(CacheManager::get(lru_cache, "key2"), None)
  
  // 配置LFU缓存
  let lfu_cache = CacheManager::create_cache(cache_manager, {
    name: "lfu_cache",
    type: "lfu",
    max_size: 50,
    ttl_ms: 120000,  // 2分钟TTL
    eviction_policy: "least_frequently_used"
  })
  
  // 测试LFU缓存淘汰策略
  CacheManager::put(lfu_cache, "freq1", "value1")
  CacheManager::put(lfu_cache, "freq2", "value2")
  CacheManager::put(lfu_cache, "freq3", "value3")
  
  // 频繁访问freq1
  for i in 0..=10 {
    CacheManager::get(lfu_cache, "freq1")
  }
  
  // 访问freq2几次
  for i in 0..=3 {
    CacheManager::get(lfu_cache, "freq2")
  }
  
  // 访问freq3一次
  CacheManager::get(lfu_cache, "freq3")
  
  // 添加更多键，触发淘汰
  for i in 0..=48 {
    CacheManager::put(lfu_cache, "freq" + (i + 4).to_string(), "value" + (i + 4).to_string())
  }
  
  // 验证缓存大小
  assert_eq(CacheManager::size(lfu_cache), 50)
  
  // freq1应该仍在缓存中（访问频率最高）
  assert_eq(CacheManager::get(lfu_cache, "freq1"), Some("value1"))
  
  // freq3应该已被淘汰（访问频率最低）
  assert_eq(CacheManager::get(lfu_cache, "freq3"), None)
  
  // 配置TTL缓存
  let ttl_cache = CacheManager::create_cache(cache_manager, {
    name: "ttl_cache",
    type: "ttl",
    max_size: 200,
    ttl_ms: 100,  // 100ms TTL
    eviction_policy: "time_based"
  })
  
  // 测试TTL淘汰
  CacheManager::put(ttl_cache, "ttl_key1", "ttl_value1")
  assert_eq(CacheManager::get(ttl_cache, "ttl_key1"), Some("ttl_value1"))
  
  // 等待TTL过期（在测试中模拟）
  CacheManager::simulate_time_passage(ttl_cache, 150)  // 150ms
  
  // 验证TTL过期后键被淘汰
  assert_eq(CacheManager::get(ttl_cache, "ttl_key1"), None)
  
  // 测试缓存统计
  let lru_stats = CacheManager::get_statistics(lru_cache)
  assert_true(lru_stats.hits > 0)
  assert_true(lru_stats.misses > 0)
  assert_true(lru_stats.evictions > 0)
  assert_true(lru_stats.hit_rate > 0)
  
  // 测试批量操作
  let batch_cache = CacheManager::create_cache(cache_manager, {
    name: "batch_cache",
    type: "lru",
    max_size: 100,
    ttl_ms: 60000,
    eviction_policy: "least_recently_used"
  })
  
  // 批量插入
  let batch_data = []
  for i in 0..=50 {
    batch_data.push(("batch_key" + i.to_string(), "batch_value" + i.to_string()))
  }
  
  CacheManager::put_batch(batch_cache, batch_data)
  assert_eq(CacheManager::size(batch_cache), 51)
  
  // 批量获取
  let batch_keys = []
  for i in 0..=25 {
    batch_keys.push("batch_key" + i.to_string())
  }
  
  let batch_results = CacheManager::get_batch(batch_cache, batch_keys)
  assert_eq(batch_results.length(), 26)
  
  for (key, value) in batch_results {
    assert_true(key.starts_with("batch_key"))
    assert_eq(value, "batch_value" + key.substring(10))
  }
  
  // 测试分布式缓存
  let distributed_cache = CacheManager::create_distributed_cache(cache_manager, {
    name: "distributed_cache",
    nodes: [
      { host: "cache1.example.com", port: 11211 },
      { host: "cache2.example.com", port: 11211 },
      { host: "cache3.example.com", port: 11211 }
    ],
    distribution_algorithm: "consistent_hashing",
    replication_factor: 2,
    max_size_per_node: 1000
  })
  
  // 测试分布式缓存操作
  CacheManager::put(distributed_cache, "dist_key1", "dist_value1")
  CacheManager::put(distributed_cache, "dist_key2", "dist_value2")
  CacheManager::put(distributed_cache, "dist_key3", "dist_value3")
  
  assert_eq(CacheManager::get(distributed_cache, "dist_key1"), Some("dist_value1"))
  assert_eq(CacheManager::get(distributed_cache, "dist_key2"), Some("dist_value2"))
  assert_eq(CacheManager::get(distributed_cache, "dist_key3"), Some("dist_value3"))
  
  // 验证分布式缓存统计
  let dist_stats = CacheManager::get_distributed_statistics(distributed_cache)
  assert_eq(dist_stats.total_nodes, 3)
  assert_true(dist_stats.total_keys > 0)
  assert_true(dist_stats.replication_count > 0)
}

// 测试2: 内存管理和优化
test "内存管理和优化测试" {
  // 创建内存管理器
  let memory_manager = MemoryManager::new()
  
  // 配置内存池
  MemoryManager::configure_memory_pool(memory_manager, {
    pool_name: "default_pool",
    initial_size_mb: 100,
    max_size_mb: 1000,
    allocation_strategy: "bump_allocator",
    gc_strategy: "generational",
    gc_threshold: 0.8,  // 80%内存使用率触发GC
    compaction_threshold: 0.6  // 60%碎片率触发压缩
  })
  
  // 测试内存分配
  let allocation1 = MemoryManager::allocate(memory_manager, 1024)  // 1KB
  let allocation2 = MemoryManager::allocate(memory_manager, 10240) // 10KB
  let allocation3 = MemoryManager::allocate(memory_manager, 102400) // 100KB
  
  // 验证分配结果
  assert_true(allocation1.address > 0)
  assert_eq(allocation1.size, 1024)
  
  assert_true(allocation2.address > 0)
  assert_eq(allocation2.size, 10240)
  
  assert_true(allocation3.address > 0)
  assert_eq(allocation3.size, 102400)
  
  // 获取内存使用统计
  let memory_stats = MemoryManager::get_memory_statistics(memory_manager)
  assert_true(memory_stats.total_allocated_mb > 0)
  assert_true(memory_stats.total_allocated_mb <= memory_stats.max_size_mb)
  assert_true(memory_stats.allocation_count >= 3)
  
  // 测试内存释放
  MemoryManager::deallocate(memory_manager, allocation2)
  
  // 验证内存释放后的统计
  let updated_stats = MemoryManager::get_memory_statistics(memory_manager)
  assert_true(updated_stats.total_allocated_mb < memory_stats.total_allocated_mb)
  assert_eq(updated_stats.allocation_count, memory_stats.allocation_count - 1)
  
  // 测试内存压缩
  // 创建一些碎片
  let fragment_allocations = []
  for i in 0..=50 {
    let alloc = MemoryManager::allocate(memory_manager, 1024)  // 1KB
    fragment_allocations.push(alloc)
  }
  
  // 释放一些分配，创建碎片
  for i in 0..=25 {
    if i % 2 == 0 {
      MemoryManager::deallocate(memory_manager, fragment_allocations[i])
    }
  }
  
  // 获取压缩前的碎片率
  let before_compaction = MemoryManager::get_memory_statistics(memory_manager)
  let fragmentation_before = before_compaction.fragmentation_ratio
  
  // 执行内存压缩
  MemoryManager::compact(memory_manager)
  
  // 获取压缩后的碎片率
  let after_compaction = MemoryManager::get_memory_statistics(memory_manager)
  let fragmentation_after = after_compaction.fragmentation_ratio
  
  // 验证压缩效果
  assert_true(fragmentation_after <= fragmentation_before)
  
  // 释放剩余的碎片分配
  for i in 0..=50 {
    if i % 2 == 1 {
      MemoryManager::deallocate(memory_manager, fragment_allocations[i])
    }
  }
  
  // 测试垃圾回收
  // 创建一些对象引用
  let object_refs = []
  for i in 0..=100 {
    let obj_ref = MemoryManager::create_object(memory_manager, {
      type: "test_object",
      size: 512,  // 512字节
      data: "object_data_" + i.to_string()
    })
    object_refs.push(obj_ref)
  }
  
  // 释放一半引用，创建垃圾
  for i in 0..=50 {
    object_refs[i] = None
  }
  
  // 获取GC前的内存统计
  let before_gc = MemoryManager::get_memory_statistics(memory_manager)
  
  // 执行垃圾回收
  MemoryManager::garbage_collect(memory_manager)
  
  // 获取GC后的内存统计
  let after_gc = MemoryManager::get_memory_statistics(memory_manager)
  
  // 验证GC效果
  assert_true(after_gc.total_allocated_mb <= before_gc.total_allocated_mb)
  assert_true(after_gc.gc_count > before_gc.gc_count)
  
  // 测试内存泄漏检测
  let leak_detector = MemoryLeakDetector::new()
  
  // 创建可能泄漏的分配
  let potential_leaks = []
  for i in 0..=50 {
    let alloc = MemoryManager::allocate(memory_manager, 2048)  // 2KB
    potential_leaks.push(alloc)
    
    // 故意不释放一些分配，模拟泄漏
    if i % 3 != 0 {
      MemoryManager::deallocate(memory_manager, alloc)
    }
  }
  
  // 执行泄漏检测
  let leak_results = MemoryLeakDetector::detect_leaks(leak_detector, memory_manager)
  
  // 验证泄漏检测结果
  assert_true(leak_results.leaked_allocations > 0)
  assert_true(leak_results.leaked_memory_mb > 0)
  
  // 修复泄漏
  for alloc in potential_leaks {
    // 检查分配是否仍然有效
    if MemoryManager::is_allocation_valid(memory_manager, alloc) {
      MemoryManager::deallocate(memory_manager, alloc)
    }
  }
  
  // 再次检测泄漏
  let fixed_leak_results = MemoryLeakDetector::detect_leaks(leak_detector, memory_manager)
  
  // 验证泄漏已修复
  assert_eq(fixed_leak_results.leaked_allocations, 0)
  assert_eq(fixed_leak_results.leaked_memory_mb, 0)
  
  // 测试智能指针
  let smart_ptr_manager = SmartPointerManager::new()
  
  // 创建共享指针
  let shared_data = "important_data"
  let shared_ptr1 = SmartPointerManager::create_shared_ptr(smart_ptr_manager, shared_data)
  let shared_ptr2 = SmartPointerManager::copy_shared_ptr(smart_ptr_manager, shared_ptr1)
  
  // 验证共享指针引用计数
  assert_eq(SmartPointerManager::ref_count(smart_ptr_manager, shared_ptr1), 2)
  assert_eq(SmartPointerManager::ref_count(smart_ptr_manager, shared_ptr2), 2)
  
  // 访问共享指针数据
  assert_eq(SmartPointerManager::deref(smart_ptr_manager, shared_ptr1), shared_data)
  assert_eq(SmartPointerManager::deref(smart_ptr_manager, shared_ptr2), shared_data)
  
  // 释放一个引用
  SmartPointerManager::release_shared_ptr(smart_ptr_manager, shared_ptr2)
  
  // 验证引用计数减少
  assert_eq(SmartPointerManager::ref_count(smart_ptr_manager, shared_ptr1), 1)
  
  // 创建唯一指针
  let unique_data = { value: 42, name: "test_object" }
  let unique_ptr = SmartPointerManager::create_unique_ptr(smart_ptr_manager, unique_data)
  
  // 验证唯一指针
  assert_eq(SmartPointerManager::deref_unique(smart_ptr_manager, unique_ptr).value, 42)
  assert_eq(SmartPointerManager::deref_unique(smart_ptr_manager, unique_ptr).name, "test_object")
  
  // 移动唯一指针
  let moved_ptr = SmartPointerManager::move_unique_ptr(smart_ptr_manager, unique_ptr)
  
  // 验证移动后的指针
  assert_eq(SmartPointerManager::deref_unique(smart_ptr_manager, moved_ptr).value, 42)
  assert_eq(SmartPointerManager::deref_unique(smart_ptr_manager, moved_ptr).name, "test_object")
  
  // 原指针应该已失效
  assert_false(SmartPointerManager::is_unique_ptr_valid(smart_ptr_manager, unique_ptr))
  
  // 释放唯一指针
  SmartPointerManager::release_unique_ptr(smart_ptr_manager, moved_ptr)
  
  // 获取智能指针统计
  let ptr_stats = SmartPointerManager::get_statistics(smart_ptr_manager)
  assert_true(ptr_stats.shared_ptr_count >= 0)
  assert_true(ptr_stats.unique_ptr_count >= 0)
  assert_true(ptr_stats.total_allocations > 0)
}

// 测试3: 算法优化和数据结构
test "算法优化和数据结构测试" {
  // 测试高效排序算法
  let large_array = []
  
  // 生成大型随机数组
  for i in 0..=10000 {
    large_array.push(Random::next_int(100000))
  }
  
  // 测试快速排序
  let quick_sort_array = large_array.copy()
  let quick_sort_start = Time::now()
  AlgorithmOptimizer::quick_sort(quick_sort_array)
  let quick_sort_time = Time::now() - quick_sort_start
  
  // 验证排序结果
  assert_true(is_sorted(quick_sort_array))
  
  // 测试归并排序
  let merge_sort_array = large_array.copy()
  let merge_sort_start = Time::now()
  AlgorithmOptimizer::merge_sort(merge_sort_array)
  let merge_sort_time = Time::now() - merge_sort_start
  
  // 验证排序结果
  assert_true(is_sorted(merge_sort_array))
  
  // 测试堆排序
  let heap_sort_array = large_array.copy()
  let heap_sort_start = Time::now()
  AlgorithmOptimizer::heap_sort(heap_sort_array)
  let heap_sort_time = Time::now() - heap_sort_start
  
  // 验证排序结果
  assert_true(is_sorted(heap_sort_array))
  
  // 测试基数排序（适用于整数）
  let radix_sort_array = large_array.copy()
  let radix_sort_start = Time::now()
  AlgorithmOptimizer::radix_sort(radix_sort_array)
  let radix_sort_time = Time::now() - radix_sort_start
  
  // 验证排序结果
  assert_true(is_sorted(radix_sort_array))
  
  // 比较排序性能
  let sort_times = [
    ("quick_sort", quick_sort_time),
    ("merge_sort", merge_sort_time),
    ("heap_sort", heap_sort_time),
    ("radix_sort", radix_sort_time)
  ]
  
  // 验证所有排序算法都在合理时间内完成
  for (algorithm, time) in sort_times {
    assert_true(time < 10000)  // 所有排序应在10秒内完成
  }
  
  // 测试搜索算法
  let sorted_array = quick_sort_array  // 使用已排序的数组
  
  // 测试二分搜索
  let search_targets = [0, 100, 1000, 10000, 50000, 99999]
  
  for target in search_targets {
    let binary_search_start = Time::now()
    let binary_result = AlgorithmOptimizer::binary_search(sorted_array, target)
    let binary_search_time = Time::now() - binary_search_start
    
    // 验证二分搜索结果
    if binary_result != None {
      let index = binary_result.unwrap()
      assert_eq(sorted_array[index], target)
    }
    
    // 二分搜索应该非常快
    assert_true(binary_search_time < 10)  // 小于10毫秒
  }
  
  // 测试插值搜索（适用于均匀分布）
  let interpolation_search_start = Time::now()
  let interpolation_result = AlgorithmOptimizer::interpolation_search(sorted_array, 50000)
  let interpolation_search_time = Time::now() - interpolation_search_start
  
  // 验证插值搜索结果
  if interpolation_result != None {
    let index = interpolation_result.unwrap()
    assert_eq(sorted_array[index], 50000)
  }
  
  // 插值搜索应该比线性搜索快
  assert_true(interpolation_search_time < 100)  // 小于100毫秒
  
  // 测试高效数据结构
  // 跳表（Skip List）
  let skip_list = SkipList::new()
  
  // 插入元素
  for i in 0..=1000 {
    SkipList::insert(skip_list, i, "value_" + i.to_string())
  }
  
  // 验证跳表查找
  for i in 0..=1000 {
    let result = SkipList::search(skip_list, i)
    assert_eq(result, Some("value_" + i.to_string()))
  }
  
  // 验证跳表范围查找
  let range_result = SkipList::range_search(skip_list, 100, 200)
  assert_eq(range_result.length(), 101)
  
  for (key, value) in range_result {
    assert_true(key >= 100 and key <= 200)
    assert_eq(value, "value_" + key.to_string())
  }
  
  // 布隆过滤器（Bloom Filter）
  let bloom_filter = BloomFilter::new(10000, 0.01)  // 10000个元素，1%假阳性率
  
  // 添加元素
  for i in 0..=5000 {
    BloomFilter::add(bloom_filter, "element_" + i.to_string())
  }
  
  // 测试布隆过滤器
  let mut true_positives = 0
  let mut true_negatives = 0
  let mut false_positives = 0
  let mut false_negatives = 0
  
  // 测试已添加的元素（应该全部为真）
  for i in 0..=5000 {
    if BloomFilter::contains(bloom_filter, "element_" + i.to_string()) {
      true_positives = true_positives + 1
    } else {
      false_negatives = false_negatives + 1
    }
  }
  
  // 测试未添加的元素（可能为假阳性）
  for i in 5001..=6000 {
    if BloomFilter::contains(bloom_filter, "element_" + i.to_string()) {
      false_positives = false_positives + 1
    } else {
      true_negatives = true_negatives + 1
    }
  }
  
  // 验证布隆过滤器特性
  assert_eq(false_negatives, 0)  // 不应该有假阴性
  assert_eq(true_positives, 5001)  // 所有已添加元素应该被检测到
  
  // 假阳性率应该接近预期
  let false_positive_rate = false_positives.to_float() / (false_positives + true_negatives).to_float()
  assert_true(false_positive_rate < 0.02)  // 假阳性率应小于2%（略高于理论值1%）
  
  // 线段树（Segment Tree）
  let segment_tree = SegmentTree::new([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  
  // 测试线段树范围查询
  let sum_1_5 = SegmentTree::range_query(segment_tree, 0, 4)  // 索引0到4的和
  assert_eq(sum_1_5, 15)  // 1+2+3+4+5 = 15
  
  let sum_6_10 = SegmentTree::range_query(segment_tree, 5, 9)  // 索引5到9的和
  assert_eq(sum_6_10, 40)  // 6+7+8+9+10 = 40
  
  // 测试线段树更新
  SegmentTree::update(segment_tree, 2, 10)  // 将索引2的值从3更新为10
  
  let updated_sum_1_5 = SegmentTree::range_query(segment_tree, 0, 4)
  assert_eq(updated_sum_1_5, 22)  // 1+2+10+4+5 = 22
  
  // 测试字典树（Trie）
  let trie = Trie::new()
  
  // 插入单词
  let words = ["apple", "app", "application", "apply", "banana", "band", "bandage"]
  for word in words {
    Trie::insert(trie, word)
  }
  
  // 测试单词查找
  assert_true(Trie::search(trie, "apple"))
  assert_true(Trie::search(trie, "app"))
  assert_true(Trie::search(trie, "application"))
  assert_true(Trie::search(trie, "apply"))
  assert_true(Trie::search(trie, "banana"))
  assert_true(Trie::search(trie, "band"))
  assert_true(Trie::search(trie, "bandage"))
  
  // 测试不存在的单词
  assert_false(Trie::search(trie, "apples"))
  assert_false(Trie::search(trie, "applicant"))
  assert_false(Trie::search(trie, "bank"))
  
  // 测试前缀查找
  let app_prefixes = Trie::prefix_search(trie, "app")
  assert_eq(app_prefixes.length(), 4)  // app, apple, application, apply
  
  let ban_prefixes = Trie::prefix_search(trie, "ban")
  assert_eq(ban_prefixes.length(), 3)  // banana, band, bandage
  
  // 测试并查集（Union-Find）
  let union_find = UnionFind::new(10)
  
  // 初始时每个元素都是独立的集合
  for i in 0..=9 {
    assert_eq(UnionFind::find(union_find, i), i)
  }
  
  // 合并集合
  UnionFind::union(union_find, 0, 1)
  UnionFind::union(union_find, 2, 3)
  UnionFind::union(union_find, 4, 5)
  UnionFind::union(union_find, 6, 7)
  UnionFind::union(union_find, 8, 9)
  
  // 验证合并结果
  assert_eq(UnionFind::find(union_find, 0), UnionFind::find(union_find, 1))
  assert_eq(UnionFind::find(union_find, 2), UnionFind::find(union_find, 3))
  assert_eq(UnionFind::find(union_find, 4), UnionFind::find(union_find, 5))
  assert_eq(UnionFind::find(union_find, 6), UnionFind::find(union_find, 7))
  assert_eq(UnionFind::find(union_find, 8), UnionFind::find(union_find, 9))
  
  // 跨集合合并
  UnionFind::union(union_find, 1, 2)
  UnionFind::union(union_find, 5, 6)
  
  // 验证跨集合合并结果
  assert_eq(UnionFind::find(union_find, 0), UnionFind::find(union_find, 3))
  assert_eq(UnionFind::find(union_find, 4), UnionFind::find(union_find, 7))
  
  // 获取并查集统计
  let uf_stats = UnionFind::get_statistics(union_find)
  assert_eq(uf_stats.total_elements, 10)
  assert_eq(uf_stats.disjoint_sets, 3)
  assert_true(uf_stats.max_set_size > 0)
}

// 测试4: 并发性能优化
test "并发性能优化测试" {
  // 创建并发性能测试器
  let concurrency_profiler = ConcurrencyProfiler::new()
  
  // 配置并发测试参数
  ConcurrencyProfiler::configure_test(concurrency_profiler, {
    thread_counts: [1, 2, 4, 8, 16],
    operations_per_thread: 10000,
    workload_types: ["cpu_intensive", "io_bound", "mixed"],
    measurement_duration_ms: 10000
  })
  
  // CPU密集型工作负载
  let cpu_intensive_work = fn() {
    let mut result = 1
    for i in 1..=10000 {
      result = (result * i) % 1000000
    }
    result
  }
  
  // IO模拟工作负载
  let io_bound_work = fn() {
    let start_time = Time::now()
    while Time::now() - start_time < 1 {  // 1ms模拟IO
      // 模拟等待
    }
    "io_completed"
  }
  
  // 混合工作负载
  let mixed_work = fn() {
    // 一部分CPU工作
    let mut result = 1
    for i in 1..=1000 {
      result = (result * i) % 100000
    }
    
    // 一部分IO工作
    let start_time = Time::now()
    while Time::now() - start_time < 1 {  // 1ms模拟IO
      // 模拟等待
    }
    
    result
  }
  
  // 测试不同线程数的性能
  let cpu_performance_results = []
  let io_performance_results = []
  let mixed_performance_results = []
  
  for thread_count in [1, 2, 4, 8, 16] {
    // CPU密集型测试
    let cpu_start_time = Time::now()
    let cpu_futures = []
    
    for i in 0..=thread_count {
      let future = ConcurrentTaskManager::submit_task(
        ConcurrentTaskManager::new(),
        cpu_intensive_work
      )
      cpu_futures.push(future)
    }
    
    // 等待所有任务完成
    for future in cpu_futures {
      ConcurrentTaskManager::get_task_result(future)
    }
    
    let cpu_end_time = Time::now()
    let cpu_duration = cpu_end_time - cpu_start_time
    
    cpu_performance_results.push((thread_count, cpu_duration))
    
    // IO密集型测试
    let io_start_time = Time::now()
    let io_futures = []
    
    for i in 0..=thread_count {
      let future = ConcurrentTaskManager::submit_task(
        ConcurrentTaskManager::new(),
        io_bound_work
      )
      io_futures.push(future)
    }
    
    // 等待所有任务完成
    for future in io_futures {
      ConcurrentTaskManager::get_task_result(future)
    }
    
    let io_end_time = Time::now()
    let io_duration = io_end_time - io_start_time
    
    io_performance_results.push((thread_count, io_duration))
    
    // 混合工作负载测试
    let mixed_start_time = Time::now()
    let mixed_futures = []
    
    for i in 0..=thread_count {
      let future = ConcurrentTaskManager::submit_task(
        ConcurrentTaskManager::new(),
        mixed_work
      )
      mixed_futures.push(future)
    }
    
    // 等待所有任务完成
    for future in mixed_futures {
      ConcurrentTaskManager::get_task_result(future)
    }
    
    let mixed_end_time = Time::now()
    let mixed_duration = mixed_end_time - mixed_start_time
    
    mixed_performance_results.push((thread_count, mixed_duration))
  }
  
  // 分析CPU密集型性能
  // CPU密集型任务应该在线程数等于CPU核心数时达到最佳性能
  let cpu_best_performance = cpu_performance_results.reduce(fn(best, current) {
    if current.1 < best.1 { current } else { best }
  }, (1, cpu_performance_results[0].1))
  
  // 分析IO密集型性能
  // IO密集型任务应该随着线程数增加而提高性能（在一定范围内）
  let io_best_performance = io_performance_results.reduce(fn(best, current) {
    if current.1 < best.1 { current } else { best }
  }, (1, io_performance_results[0].1))
  
  // 分析混合工作负载性能
  let mixed_best_performance = mixed_performance_results.reduce(fn(best, current) {
    if current.1 < best.1 { current } else { best }
  }, (1, mixed_performance_results[0].1))
  
  // 验证性能结果
  assert_true(cpu_best_performance.1 > 0)
  assert_true(io_best_performance.1 > 0)
  assert_true(mixed_best_performance.1 > 0)
  
  // 测试无锁数据结构性能
  let lock_free_queue = LockFreeQueue::new()
  let standard_queue = ConcurrentQueue::new()
  
  // 测试无锁队列性能
  let lock_free_start = Time::now()
  let lock_free_futures = []
  
  // 生产者任务
  for i in 0..=8 {
    let task_id = i
    let future = ConcurrentTaskManager::submit_task(
      ConcurrentTaskManager::new(),
      fn() {
        for j in 0..=1000 {
          LockFreeQueue::enqueue(lock_free_queue, "item_" + task_id.to_string() + "_" + j.to_string())
        }
      }
    )
    lock_free_futures.push(future)
  }
  
  // 消费者任务
  for i in 0..=8 {
    let future = ConcurrentTaskManager::submit_task(
      ConcurrentTaskManager::new(),
      fn() {
        let mut consumed = 0
        while consumed < 1000 {
          let item = LockFreeQueue::dequeue(lock_free_queue)
          if item != None {
            consumed = consumed + 1
          }
        }
      }
    )
    lock_free_futures.push(future)
  }
  
  // 等待所有任务完成
  for future in lock_free_futures {
    ConcurrentTaskManager::get_task_result(future)
  }
  
  let lock_free_end = Time::now()
  let lock_free_duration = lock_free_end - lock_free_start
  
  // 测试标准队列性能
  let standard_start = Time::now()
  let standard_futures = []
  
  // 生产者任务
  for i in 0..=8 {
    let task_id = i
    let future = ConcurrentTaskManager::submit_task(
      ConcurrentTaskManager::new(),
      fn() {
        for j in 0..=1000 {
          ConcurrentQueue::enqueue(standard_queue, "item_" + task_id.to_string() + "_" + j.to_string())
        }
      }
    )
    standard_futures.push(future)
  }
  
  // 消费者任务
  for i in 0..=8 {
    let future = ConcurrentTaskManager::submit_task(
      ConcurrentTaskManager::new(),
      fn() {
        let mut consumed = 0
        while consumed < 1000 {
          let item = ConcurrentQueue::dequeue(standard_queue)
          if item != None {
            consumed = consumed + 1
          }
        }
      }
    )
    standard_futures.push(future)
  }
  
  // 等待所有任务完成
  for future in standard_futures {
    ConcurrentTaskManager::get_task_result(future)
  }
  
  let standard_end = Time::now()
  let standard_duration = standard_end - standard_start
  
  // 无锁队列应该比标准队列更快（在高并发场景下）
  // 注意：这个断言可能在某些环境下不成立，因为无锁结构的性能取决于具体实现
  // assert_true(lock_free_duration < standard_duration)
  
  // 测试工作窃取算法
  let work_stealing_pool = WorkStealingPool::new(4)  // 4个工作线程
  
  // 创建大量小任务
  let work_stealing_futures = []
  for i in 0..=10000 {
    let task_id = i
    let future = WorkStealingPool::submit(work_stealing_pool, fn() {
      // 小任务
      let mut result = 1
      for j in 1..=100 {
        result = (result * j) % 1000
      }
      result
    })
    work_stealing_futures.push(future)
  }
  
  // 等待所有任务完成
  let work_stealing_results = []
  for future in work_stealing_futures {
    let result = WorkStealingPool::get_result(work_stealing_pool, future)
    work_stealing_results.push(result)
  }
  
  // 验证结果
  assert_eq(work_stealing_results.length(), 10001)
  
  // 获取工作窃取统计
  let ws_stats = WorkStealingPool::get_statistics(work_stealing_pool)
  assert_eq(ws_stats.worker_threads, 4)
  assert_true(ws_stats.total_tasks > 0)
  assert_true(ws_stats.stolen_tasks > 0)  // 应该有任务被窃取
  
  // 测试线程局部存储性能
  let thread_local_storage = ThreadLocalStorage::new()
  
  // 测试线程局部存储访问
  let tls_futures = []
  for i in 0..=10 {
    let thread_id = i
    let future = ConcurrentTaskManager::submit_task(
      ConcurrentTaskManager::new(),
      fn() {
        // 设置线程局部变量
        ThreadLocalStorage::set(thread_local_storage, "thread_id", thread_id)
        ThreadLocalStorage::set(thread_local_storage, "counter", 0)
        
        // 多次访问线程局部变量
        for j in 0..=1000 {
          let current_counter = ThreadLocalStorage::get(thread_local_storage, "counter").unwrap()
          ThreadLocalStorage::set(thread_local_storage, "counter", current_counter + 1)
        }
        
        // 获取最终值
        let final_counter = ThreadLocalStorage::get(thread_local_storage, "counter").unwrap()
        let stored_thread_id = ThreadLocalStorage::get(thread_local_storage, "thread_id").unwrap()
        
        (stored_thread_id, final_counter)
      }
    )
    tls_futures.push(future)
  }
  
  // 等待所有任务完成
  let tls_results = []
  for future in tls_futures {
    let result = ConcurrentTaskManager::get_task_result(future)
    tls_results.push(result)
  }
  
  // 验证线程局部存储结果
  assert_eq(tls_results.length(), 11)
  
  for (thread_id, counter) in tls_results {
    assert_eq(counter, 1001)  // 每个线程应该递增1000次，从0开始到1001
  }
  
  // 获取并发性能分析报告
  let performance_report = ConcurrencyProfiler::generate_report(concurrency_profiler, {
    include_cpu_analysis: true,
    include_io_analysis: true,
    include_contention_analysis: true,
    include_scalability_analysis: true
  })
  
  // 验证性能报告
  assert_true(performance_report.cpu_analysis.length() > 0)
  assert_true(performance_report.io_analysis.length() > 0)
  assert_true(performance_report.contention_analysis.length() > 0)
  assert_true(performance_report.scalability_analysis.length() > 0)
}

// 测试5: 资源池化和对象重用
test "资源池化和对象重用测试" {
  // 创建对象池管理器
  let object_pool_manager = ObjectPoolManager::new()
  
  // 配置数据库连接池
  let db_connection_pool = ObjectPoolManager::create_pool(object_pool_manager, {
    name: "db_connection_pool",
    factory: fn() {
      {
        connection_id: "conn_" + Random::next_int(10000).to_string(),
        is_active: true,
        created_at: Time::now(),
        last_used: Time::now(),
        usage_count: 0
      }
    },
    validator: fn(conn) { conn.is_active },
    resetter: fn(conn) {
      { conn | last_used: Time::now(), usage_count: conn.usage_count + 1 }
    },
    min_size: 5,
    max_size: 20,
    max_idle_time_ms: 30000,  // 30秒
    max_lifetime_ms: 300000,  // 5分钟
    validation_interval_ms: 10000  // 10秒
  })
  
  // 测试连接池基本操作
  let conn1 = ObjectPoolManager::acquire(db_connection_pool)
  let conn2 = ObjectPoolManager::acquire(db_connection_pool)
  let conn3 = ObjectPoolManager::acquire(db_connection_pool)
  
  // 验证连接获取
  assert_true(conn1.connection_id.starts_with("conn_"))
  assert_true(conn2.connection_id.starts_with("conn_"))
  assert_true(conn3.connection_id.starts_with("conn_"))
  assert_not_eq(conn1.connection_id, conn2.connection_id)
  assert_not_eq(conn2.connection_id, conn3.connection_id)
  assert_not_eq(conn1.connection_id, conn3.connection_id)
  
  // 获取连接池统计
  let pool_stats = ObjectPoolManager::get_statistics(db_connection_pool)
  assert_true(pool_stats.active_objects >= 3)
  assert_true(pool_stats.idle_objects <= pool_stats.max_size - pool_stats.active_objects)
  
  // 释放连接回池
  ObjectPoolManager::release(db_connection_pool, conn1)
  ObjectPoolManager::release(db_connection_pool, conn2)
  
  // 验证连接释放后的统计
  let updated_stats = ObjectPoolManager::get_statistics(db_connection_pool)
  assert_true(updated_stats.active_objects <= pool_stats.active_objects - 2)
  assert_true(updated_stats.idle_objects >= pool_stats.idle_objects + 2)
  
  // 重新获取连接，应该重用已释放的连接
  let conn4 = ObjectPoolManager::acquire(db_connection_pool)
  let conn5 = ObjectPoolManager::acquire(db_connection_pool)
  
  // 验证连接重用
  assert_true(conn4.connection_id == conn1.connection_id or conn4.connection_id == conn2.connection_id)
  assert_true(conn5.connection_id == conn1.connection_id or conn5.connection_id == conn2.connection_id)
  assert_not_eq(conn4.connection_id, conn5.connection_id)
  
  // 验证使用计数增加
  assert_true(conn4.usage_count > 0)
  assert_true(conn5.usage_count > 0)
  
  // 释放剩余连接
  ObjectPoolManager::release(db_connection_pool, conn3)
  ObjectPoolManager::release(db_connection_pool, conn4)
  ObjectPoolManager::release(db_connection_pool, conn5)
  
  // 配置线程池
  let thread_pool = ObjectPoolManager::create_pool(object_pool_manager, {
    name: "thread_pool",
    factory: fn() {
      {
        thread_id: "thread_" + Random::next_int(10000).to_string(),
        is_active: true,
        task_count: 0,
        created_at: Time::now()
      }
    },
    validator: fn(thread) { thread.is_active },
    resetter: fn(thread) {
      { thread | task_count: 0 }
    },
    min_size: 2,
    max_size: 10,
    max_idle_time_ms: 60000,  // 1分钟
    max_lifetime_ms: 600000,  // 10分钟
    validation_interval_ms: 30000  // 30秒
  })
  
  // 测试线程池并发使用
  let pool_futures = []
  for i in 0..=20 {
    let task_id = i
    let future = ConcurrentTaskManager::submit_task(
      ConcurrentTaskManager::new(),
      fn() {
        let thread = ObjectPoolManager::acquire(thread_pool)
        
        // 使用线程执行任务
        let start_time = Time::now()
        while Time::now() - start_time < 10 {  // 10ms任务
          // 模拟工作
        }
        
        let result = "task_" + task_id.to_string() + "_completed_by_" + thread.thread_id
        
        ObjectPoolManager::release(thread_pool, thread)
        result
      }
    )
    pool_futures.push(future)
  }
  
  // 等待所有任务完成
  let pool_results = []
  for future in pool_futures {
    let result = ConcurrentTaskManager::get_task_result(future)
    pool_results.push(result)
  }
  
  // 验证结果
  assert_eq(pool_results.length(), 21)
  
  for result in pool_results {
    assert_true(result.contains("task_"))
    assert_true(result.contains("_completed_by_"))
    assert_true(result.contains("thread_"))
  }
  
  // 获取线程池统计
  let thread_pool_stats = ObjectPoolManager::get_statistics(thread_pool)
  assert_true(thread_pool_stats.total_acquisitions >= 21)
  assert_true(thread_pool_stats.total_releases >= 21)
  assert_true(thread_pool_stats.active_objects <= thread_pool_stats.max_size)
  
  // 配置内存缓冲区池
  let buffer_pool = ObjectPoolManager::create_pool(object_pool_manager, {
    name: "buffer_pool",
    factory: fn() {
      {
        buffer: Array::create(1024, 0),  // 1KB缓冲区
        size: 1024,
        in_use: false
      }
    },
    validator: fn(buffer) { buffer.size == 1024 },
    resetter: fn(buffer) {
      // 清零缓冲区
      { buffer | buffer: Array::create(1024, 0), in_use: false }
    },
    min_size: 10,
    max_size: 100,
    max_idle_time_ms: 120000,  // 2分钟
    max_lifetime_ms: 1800000,  // 30分钟
    validation_interval_ms: 60000  // 1分钟
  })
  
  // 测试缓冲区池性能
  let pool_start_time = Time::now()
  
  // 使用池化缓冲区
  for i in 0..=1000 {
    let buffer = ObjectPoolManager::acquire(buffer_pool)
    
    // 使用缓冲区
    for j in 0..=buffer.size - 1 {
      buffer.buffer[j] = (i + j) % 256
    }
    
    ObjectPoolManager::release(buffer_pool, buffer)
  }
  
  let pool_duration = Time::now() - pool_start_time
  
  // 测试直接分配性能
  let direct_start_time = Time::now()
  
  // 直接分配缓冲区
  for i in 0..=1000 {
    let buffer = Array::create(1024, 0)
    
    // 使用缓冲区
    for j in 0..=1023 {
      buffer[j] = (i + j) % 256
    }
    // 无需显式释放，垃圾回收器会处理
  }
  
  let direct_duration = Time::now() - direct_start_time
  
  // 对象池应该在高频分配/释放场景下表现更好
  // 注意：这个断言可能在某些环境下不成立，因为GC优化等因素
  // assert_true(pool_duration < direct_duration)
  
  // 获取缓冲区池统计
  let buffer_pool_stats = ObjectPoolManager::get_statistics(buffer_pool)
  assert_true(buffer_pool_stats.total_acquisitions >= 1000)
  assert_true(buffer_pool_stats.total_releases >= 1000)
  assert_true(buffer_pool_stats.hit_rate > 0.5)  // 至少50%的命中率
  
  // 测试对象池预热
  let warmup_pool = ObjectPoolManager::create_pool(object_pool_manager, {
    name: "warmup_pool",
    factory: fn() {
      {
        id: "obj_" + Random::next_int(10000).to_string(),
        data: "initialized_data",
        ready: true
      }
    },
    validator: fn(obj) { obj.ready },
    resetter: fn(obj) { obj },
    min_size: 5,
    max_size: 15,
    max_idle_time_ms: 60000,
    max_lifetime_ms: 600000,
    validation_interval_ms: 30000
  })
  
  // 预热对象池
  ObjectPoolManager::warmup(warmup_pool)
  
  // 验证预热后的状态
  let warmup_stats = ObjectPoolManager::get_statistics(warmup_pool)
  assert_eq(warmup_stats.idle_objects, 5)  // 应该有5个空闲对象
  
  // 快速获取对象，应该无需创建新对象
  let warmup_obj1 = ObjectPoolManager::acquire(warmup_pool)
  let warmup_obj2 = ObjectPoolManager::acquire(warmup_pool)
  let warmup_obj3 = ObjectPoolManager::acquire(warmup_pool)
  
  // 验证获取的对象已初始化
  assert_true(warmup_obj1.ready)
  assert_true(warmup_obj2.ready)
  assert_true(warmup_obj3.ready)
  assert_eq(warmup_obj1.data, "initialized_data")
  assert_eq(warmup_obj2.data, "initialized_data")
  assert_eq(warmup_obj3.data, "initialized_data")
  
  // 释放对象
  ObjectPoolManager::release(warmup_pool, warmup_obj1)
  ObjectPoolManager::release(warmup_pool, warmup_obj2)
  ObjectPoolManager::release(warmup_pool, warmup_obj3)
  
  // 获取对象池管理器全局统计
  let global_stats = ObjectPoolManager::get_global_statistics(object_pool_manager)
  
  // 验证全局统计
  assert_true(global_stats.total_pools >= 4)
  assert_true(global_stats.total_objects > 0)
  assert_true(global_stats.total_acquisitions > 0)
  assert_true(global_stats.total_releases > 0)
  assert_true(global_stats.avg_hit_rate > 0)
}