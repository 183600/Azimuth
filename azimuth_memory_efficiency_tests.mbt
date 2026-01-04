// Memory Efficiency Test Suite for Azimuth
// 内存效率测试套件 - 测试各种内存优化和高效内存使用技术

// Test 1: 内存池管理
test "memory pool management" {
  // 对象池实现
  let create_object_pool = fn(create_fn, reset_fn, initial_size) {
    let pool = ref []
    
    // 初始化池
    for i in 0..initial_size {
      let obj = create_fn()
      pool.push(obj)
    }
    
    fn() {
      // 获取对象
      if (!pool).is_empty() {
        (!pool).pop()
      } else {
        create_fn()
      }
    },
    
    fn(obj) {
      // 归还对象
      reset_fn(obj)
      pool.push(obj)
    },
    
    fn() {
      // 获取池大小
      (!pool).length()
    }
  }
  
  // 示例：缓冲区对象池
  let create_buffer = fn() { { data: [], size: 0, capacity: 1024 } }
  let reset_buffer = fn(buf) { buf.data = []; buf.size = 0; buf.capacity = 1024 }
  
  let (get_buffer, return_buffer, pool_size) = create_object_pool(create_buffer, reset_buffer, 3)
  
  // 初始池大小
  assert_eq(pool_size(), 3)
  
  // 获取缓冲区
  let buf1 = get_buffer()
  assert_eq(pool_size(), 2)
  
  let buf2 = get_buffer()
  assert_eq(pool_size(), 1)
  
  let buf3 = get_buffer()
  assert_eq(pool_size(), 0)
  
  // 池为空，创建新对象
  let buf4 = get_buffer()
  assert_eq(pool_size(), 0)
  
  // 归还缓冲区
  return_buffer(buf1)
  assert_eq(pool_size(), 1)
  
  return_buffer(buf2)
  assert_eq(pool_size(), 2)
  
  return_buffer(buf3)
  assert_eq(pool_size(), 3)
  
  return_buffer(buf4)
  assert_eq(pool_size(), 4)
  
  // 验证重置
  buf1.data.push("test")
  buf1.size = 4
  return_buffer(buf1)
  
  let buf5 = get_buffer()
  assert_eq(buf5.size, 0)
  assert_eq(buf5.data.length(), 0)
}

// Test 2: 内存分配策略
test "memory allocation strategies" {
  // 预分配策略
  let preallocate_array = fn(expected_size) {
    let mut arr = []
    arr.reserve(expected_size)
    arr
  }
  
  // 动态增长策略
  let dynamic_array = fn() { [] }
  
  // 测试预分配效率
  let prealloc = preallocate_array(1000)
  let dynamic = dynamic_array()
  
  // 填充数组
  for i in 0..1000 {
    prealloc.push(i)
    dynamic.push(i)
  }
  
  assert_eq(prealloc.length(), 1000)
  assert_eq(dynamic.length(), 1000)
  
  // 内存重用策略
  let reuse_buffer = fn(initial_capacity) {
    let buffer = ref []
    let capacity = ref initial_capacity
    
    fn() {
      // 清空但保留容量
      (!buffer).clear()
      !buffer
    },
    
    fn() {
      // 获取当前容量
      !capacity
    },
    
    fn(new_capacity) {
      // 调整容量
      if new_capacity > !capacity {
        (!buffer).reserve(new_capacity - !capacity)
        capacity := new_capacity
      }
    }
  }
  
  let (get_reusable_buffer, get_capacity, resize) = reuse_buffer(100)
  
  assert_eq(get_capacity(), 100)
  
  let buf1 = get_reusable_buffer()
  assert_eq(buf1.length(), 0)
  
  // 填充缓冲区
  for i in 0..50 {
    buf1.push(i)
  }
  
  // 重用缓冲区
  let buf2 = get_reusable_buffer()
  assert_eq(buf2.length(), 0)
  assert_eq(get_capacity(), 100)  // 容量保留
  
  // 扩展容量
  resize(200)
  assert_eq(get_capacity(), 200)
}

// Test 3: 内存压缩技术
test "memory compression techniques" {
  // 字符串压缩
  let compress_string = fn(str) {
    // 简单的行程编码压缩
    if str.length() == 0 {
      ""
    } else {
      let mut result = ""
      let mut current_char = str[0]
      let mut count = 1
      
      for i in 1..str.length() {
        if str[i] == current_char {
          count = count + 1
        } else {
          result = result + current_char.to_string() + count.to_string()
          current_char = str[i]
          count = 1
        }
      }
      
      result = result + current_char.to_string() + count.to_string()
      result
    }
  }
  
  let decompress_string = fn(compressed) {
    let mut result = ""
    let mut i = 0
    
    while i < compressed.length() {
      let char = compressed[i]
      i = i + 1
      
      let mut count_str = ""
      while i < compressed.length() && is_digit(compressed[i]) {
        count_str = count_str + compressed[i].to_string()
        i = i + 1
      }
      
      let count = count_str.to_int().unwrap_or(1)
      for j in 0..count {
        result = result + char.to_string()
      }
    }
    
    result
  }
  
  // 测试压缩
  let original = "AAAABBBCCDAAA"
  let compressed = compress_string(original)
  let decompressed = decompress_string(compressed)
  
  assert_eq(compressed, "A4B3C2D1A3")
  assert_eq(decompressed, original)
  
  // 数组压缩
  let compress_array = fn(arr) {
    if arr.is_empty() {
      []
    } else {
      let mut result = []
      let mut current = arr[0]
      let mut count = 1
      
      for i in 1..arr.length() {
        if arr[i] == current {
          count = count + 1
        } else {
          result.push((current, count))
          current = arr[i]
          count = 1
        }
      }
      
      result.push((current, count))
      result
    }
  }
  
  let decompress_array = fn(compressed) {
    let mut result = []
    for (value, count) in compressed {
      for i in 0..count {
        result.push(value)
      }
    }
    result
  }
  
  let original_array = [1, 1, 1, 2, 2, 3, 3, 3, 3, 4]
  let compressed_array = compress_array(original_array)
  let decompressed_array = decompress_array(compressed_array)
  
  assert_eq(compressed_array, [(1, 3), (2, 2), (3, 4), (4, 1)])
  assert_eq(decompressed_array, original_array)
}

// Test 4: 内存泄漏检测
test "memory leak detection" {
  // 资源跟踪器
  let create_resource_tracker = fn() {
    let active_resources = ref 0
    let total_created = ref 0
    let total_destroyed = ref 0
    
    fn() {
      // 创建资源
      active_resources := !active_resources + 1
      total_created := !total_created + 1
      !active_resources
    },
    
    fn() {
      // 销毁资源
      if !active_resources > 0 {
        active_resources := !active_resources - 1
        total_destroyed := !total_destroyed + 1
      }
      !active_resources
    },
    
    fn() {
      // 获取统计信息
      {
        active: !active_resources,
        created: !total_created,
        destroyed: !total_destroyed,
        leaked: !active_resources
      }
    }
  }
  
  let (create_resource, destroy_resource, get_stats) = create_resource_tracker()
  
  // 创建和销毁资源
  let active1 = create_resource()
  assert_eq(active1, 1)
  
  let active2 = create_resource()
  assert_eq(active2, 2)
  
  let active3 = create_resource()
  assert_eq(active3, 3)
  
  // 销毁资源
  let active4 = destroy_resource()
  assert_eq(active4, 2)
  
  let active5 = destroy_resource()
  assert_eq(active5, 1)
  
  // 检查统计信息
  let stats = get_stats()
  assert_eq(stats.active, 1)
  assert_eq(stats.created, 3)
  assert_eq(stats.destroyed, 2)
  assert_eq(stats.leaked, 1)
  
  // 清理剩余资源
  destroy_resource()
  
  let final_stats = get_stats()
  assert_eq(final_stats.active, 0)
  assert_eq(final_stats.created, 3)
  assert_eq(final_stats.destroyed, 3)
  assert_eq(final_stats.leaked, 0)
  
  // 内存使用监控
  let memory_monitor = fn() {
    let allocations = ref 0
    let deallocations = ref 0
    let total_allocated = ref 0
    let total_deallocated = ref 0
    
    fn(size) {
      // 分配内存
      allocations := !allocations + 1
      total_allocated := !total_allocated + size
    },
    
    fn(size) {
      // 释放内存
      deallocations := !deallocations + 1
      total_deallocated := !total_deallocated + size
    },
    
    fn() {
      // 获取内存统计
      {
        allocations: !allocations,
        deallocations: !deallocations,
        total_allocated: !total_allocated,
        total_deallocated: !total_deallocated,
        current_usage: !total_allocated - !total_deallocated
      }
    }
  }
  
  let (allocate, deallocate, get_memory_stats) = memory_monitor()
  
  // 模拟内存分配和释放
  allocate(100)
  allocate(200)
  allocate(150)
  
  deallocate(100)
  deallocate(50)  // 部分释放
  
  let mem_stats = get_memory_stats()
  assert_eq(mem_stats.allocations, 3)
  assert_eq(mem_stats.deallocations, 2)
  assert_eq(mem_stats.total_allocated, 450)
  assert_eq(mem_stats.total_deallocated, 150)
  assert_eq(mem_stats.current_usage, 300)
}

// Test 5: 内存映射和虚拟内存
test "memory_mapping_and_virtual_memory" {
  // 模拟内存映射文件
  let create_memory_map = fn(file_size, page_size) {
    let pages = ref {}
    let current_size = ref 0
    
    fn(offset, length) {
      // 映射内存区域
      let start_page = offset / page_size
      let end_page = (offset + length - 1) / page_size
      
      let mut mapped_pages = []
      for page in start_page..=end_page {
        if (!pages).contains(page) {
          // 模拟加载页面
          pages.set(page, { loaded: true, dirty: false })
        }
        mapped_pages.push(page)
      }
      
      mapped_pages
    },
    
    fn(offset, length) {
      // 取消映射内存区域
      let start_page = offset / page_size
      let end_page = (offset + length - 1) / page_size
      
      for page in start_page..=end_page {
        if (!pages).contains(page) {
          pages.delete(page)
        }
      }
    },
    
    fn() {
      // 获取已加载页面数
      (!pages).size()
    },
    
    fn() {
      // 获取总大小
      !current_size
    }
  }
  
  let (map_region, unmap_region, loaded_pages, total_size) = create_memory_map(10000, 1024)
  
  // 映射内存区域
  let mapped1 = map_region(0, 2048)
  assert_eq(loaded_pages(), 2)
  
  let mapped2 = map_region(2048, 1024)
  assert_eq(loaded_pages(), 3)
  
  // 取消映射
  unmap_region(0, 1024)
  assert_eq(loaded_pages(), 2)
  
  // 虚拟内存管理
  let virtual_memory_manager = fn(total_pages) {
    let physical_pages = ref []
    let page_table = ref {}
    let next_physical = ref 0
    
    fn(virtual_page) {
      // 分配物理页面
      if (!page_table).contains(virtual_page) {
        if !next_physical < total_pages {
          let physical_page = !next_physical
          next_physical := !next_physical + 1
          physical_pages.push(physical_page)
          page_table.set(virtual_page, physical_page)
          Some(physical_page)
        } else {
          None  // 物理内存不足
        }
      } else {
        (!page_table).get(virtual_page)
      }
    },
    
    fn(virtual_page) {
      // 释放物理页面
      match (!page_table).get(virtual_page) {
        Some(physical_page) => {
          page_table.delete(virtual_page)
          // 从物理页面列表中移除
          let mut new_physical = []
          for p in !physical_pages {
            if p != physical_page {
              new_physical.push(p)
            }
          }
          physical_pages := new_physical
        }
        None => {}
      }
    },
    
    fn() {
      // 获取物理页面使用情况
      {
        total: total_pages,
        used: (!physical_pages).length(),
        free: total_pages - (!physical_pages).length()
      }
    }
  }
  
  let (allocate_page, free_page, get_usage) = virtual_memory_manager(10)
  
  // 分配页面
  assert_eq(allocate_page(0), Some(0))
  assert_eq(allocate_page(1), Some(1))
  assert_eq(allocate_page(2), Some(2))
  
  let usage1 = get_usage()
  assert_eq(usage1.used, 3)
  assert_eq(usage1.free, 7)
  
  // 释放页面
  free_page(1)
  
  let usage2 = get_usage()
  assert_eq(usage2.used, 2)
  assert_eq(usage2.free, 8)
}

// Test 6: 内存对齐和优化
test "memory_alignment_and_optimization" {
  // 内存对齐计算
  let align_up = fn(size, alignment) {
    (size + alignment - 1) & ~(alignment - 1)
  }
  
  let align_down = fn(size, alignment) {
    size & ~(alignment - 1)
  }
  
  let is_aligned = fn(size, alignment) {
    size % alignment == 0
  }
  
  // 测试对齐
  assert_eq(align_up(10, 8), 16)
  assert_eq(align_up(16, 8), 16)
  assert_eq(align_up(17, 8), 24)
  
  assert_eq(align_down(10, 8), 8)
  assert_eq(align_down(16, 8), 16)
  assert_eq(align_down(17, 8), 16)
  
  assert_true(is_aligned(16, 8))
  assert_false(is_aligned(10, 8))
  
  // 结构体布局优化
  let optimized_layout = fn() {
    // 模拟内存布局
    let fields = [
      { name: "a", size: 4, alignment: 4 },
      { name: "b", size: 1, alignment: 1 },
      { name: "c", size: 8, alignment: 8 },
      { name: "d", size: 2, alignment: 2 }
    ]
    
    // 优化前的布局（按声明顺序）
    let mut offset = 0
    let mut original_layout = []
    
    for field in fields {
      let aligned_offset = align_up(offset, field.alignment)
      original_layout.push({
        name: field.name,
        offset: aligned_offset,
        size: field.size
      })
      offset = aligned_offset + field.size
    }
    
    let original_size = align_up(offset, 8)  // 结构体对齐到最大字段对齐
    
    // 优化后的布局（按对齐大小排序）
    let sorted_fields = fields.sort_by(fn(a, b) { b.alignment - a.alignment })
    offset = 0
    let mut optimized_layout_result = []
    
    for field in sorted_fields {
      let aligned_offset = align_up(offset, field.alignment)
      optimized_layout_result.push({
        name: field.name,
        offset: aligned_offset,
        size: field.size
      })
      offset = aligned_offset + field.size
    }
    
    let optimized_size = align_up(offset, 8)
    
    {
      original: { layout: original_layout, size: original_size },
      optimized: { layout: optimized_layout_result, size: optimized_size }
    }
  }
  
  let layout_result = optimized_layout()
  assert_eq(layout_result.original.size, 24)
  assert_eq(layout_result.optimized.size, 16)
  
  // 内存池对齐
  let aligned_memory_pool = fn(block_size, alignment, num_blocks) {
    let aligned_size = align_up(block_size, alignment)
    let pool_size = aligned_size * num_blocks
    let free_blocks = ref []
    
    // 初始化空闲块列表
    for i in 0..num_blocks {
      free_blocks.push(i * aligned_size)
    }
    
    fn() {
      // 分配对齐块
      match (!free_blocks).pop() {
        Some(offset) => Some(offset)
        None => None
      }
    },
    
    fn(offset) {
      // 释放对齐块
      free_blocks.push(offset)
    },
    
    fn() {
      // 获取池信息
      {
        block_size: aligned_size,
        alignment: alignment,
        total_blocks: num_blocks,
        free_blocks: (!free_blocks).length()
      }
    }
  }
  
  let (aligned_alloc, aligned_free, pool_info) = aligned_memory_pool(100, 16, 10)
  
  let info1 = pool_info()
  assert_eq(info1.free_blocks, 10)
  
  let block1 = aligned_alloc()
  let block2 = aligned_alloc()
  
  let info2 = pool_info()
  assert_eq(info2.free_blocks, 8)
  
  aligned_free(block1.unwrap())
  aligned_free(block2.unwrap())
  
  let info3 = pool_info()
  assert_eq(info3.free_blocks, 10)
}

// Test 7: 内存缓存优化
test "memory_cache_optimization" {
  // 缓存行模拟
  let cache_line_size = 64
  
  let simulate_cache_access = fn(data_size, access_pattern) {
    let cache_lines = ref {}
    let cache_hits = ref 0
    let cache_misses = ref 0
    
    for address in access_pattern {
      let cache_line = address / cache_line_size
      
      if (!cache_lines).contains(cache_line) {
        // 缓存未命中
        cache_misses := !cache_misses + 1
        cache_lines.set(cache_line, true)
        
        // 模拟缓存容量限制
        if (!cache_lines).size() > 16 {
          // 简单LRU：移除第一个
          let first_key = (!cache_lines).keys()[0]
          cache_lines.delete(first_key)
        }
      } else {
        // 缓存命中
        cache_hits := !cache_hits + 1
      }
    }
    
    {
      hits: !cache_hits,
      misses: !cache_misses,
      total: !cache_hits + !cache_misses,
      hit_rate: (!cache_hits).to_float() / ((!cache_hits + !cache_misses).to_float())
    }
  }
  
  // 顺序访问模式
  let sequential_access = []
  for i in 0..100 {
    sequential_access.push(i * 4)  // 每次4字节
  }
  
  let sequential_result = simulate_cache_access(400, sequential_access)
  assert_eq(sequential_result.total, 100)
  assert_eq(sequential_result.hits, 75)  // 每个缓存行16次访问，1次未命中，15次命中
  
  // 随机访问模式
  let random_access = [0, 256, 512, 768, 1024, 1280, 1536, 1792]
  let random_result = simulate_cache_access(2048, random_access)
  assert_eq(random_result.total, 8)
  assert_eq(random_result.hits, 0)  // 每个访问都在不同的缓存行
  
  // 数据预取优化
  let prefetch_optimized_access = fn() {
    let cache_lines = ref {}
    let cache_hits = ref 0
    let cache_misses = ref 0
    
    let access_with_prefetch = fn(address) {
      let cache_line = address / cache_line_size
      let next_cache_line = (address + 64) / cache_line_size
      
      // 预取下一个缓存行
      if !(!cache_lines).contains(next_cache_line) {
        cache_lines.set(next_cache_line, true)
      }
      
      if (!cache_lines).contains(cache_line) {
        cache_misses := !cache_misses + 1
        cache_lines.set(cache_line, true)
      } else {
        cache_hits := !cache_hits + 1
      }
    }
    
    // 顺序访问并预取
    for i in 0..100 {
      access_with_prefetch(i * 4)
    }
    
    {
      hits: !cache_hits,
      misses: !cache_misses,
      hit_rate: (!cache_hits).to_float() / ((!cache_hits + !cache_misses).to_float())
    }
  }
  
  let prefetch_result = prefetch_optimized_access()
  assert_eq(prefetch_result.hits, 99)  // 预取减少了缓存未命中
}

// Test 8: 内存回收和垃圾回收
test "memory_gc_and_reclamation" {
  // 引用计数垃圾回收
  let create_ref_counted = fn(initial_value) {
    let value = ref initial_value
    let count = ref 1
    
    fn() {
      // 增加引用计数
      count := !count + 1
    },
    
    fn() {
      // 减少引用计数
      count := !count - 1
      if !count == 0 {
        // 释放资源
        true
      } else {
        false
      }
    },
    
    fn() {
      // 获取引用计数
      !count
    },
    
    fn() {
      // 获取值
      !value
    },
    
    fn(new_value) {
      // 设置值
      value := new_value
    }
  }
  
  let (increment, decrement, get_count, get_value, set_value) = create_ref_counted(42)
  
  assert_eq(get_count(), 1)
  assert_eq(get_value(), 42)
  
  increment()
  assert_eq(get_count(), 2)
  
  let should_free1 = decrement()
  assert_false(should_free1)
  assert_eq(get_count(), 1)
  
  let should_free2 = decrement()
  assert_true(should_free2)  // 引用计数为0，应该释放
  
  // 标记-清除垃圾回收
  let create_mark_sweep_gc = fn() {
    let objects = ref {}
    let roots = ref []
    let marked = ref {}
    
    fn(id, obj) {
      // 分配对象
      objects.set(id, { data: obj, marked: false })
    },
    
    fn(id) {
      // 添加根引用
      roots.push(id)
    },
    
    fn() {
      // 标记阶段
      marked := {}
      
      let mark = fn(id) {
        if (!marked).contains(id) && (!objects).contains(id) {
          marked.set(id, true)
          
          // 标记引用的对象（简化版，实际需要遍历对象引用）
          let obj = (!objects).get(id).unwrap()
          // 这里应该遍历obj的引用并递归标记
        }
      }
      
      // 从根开始标记
      for root in !roots {
        mark(root)
      }
      
      // 清除阶段
      let mut freed = []
      for (id, _) in !objects {
        if (!marked).contains(id) {
          freed.push(id)
          objects.delete(id)
        }
      }
      
      freed
    },
    
    fn() {
      // 获取统计信息
      {
        total_objects: (!objects).size(),
        root_count: (!roots).length(),
        marked_count: (!marked).size()
      }
    }
  }
  
  let (allocate, add_root, collect, get_gc_stats) = create_mark_sweep_gc()
  
  // 分配对象
  allocate("obj1", { value: 42 })
  allocate("obj2", { value: "hello" })
  allocate("obj3", { value: [1, 2, 3] })
  
  // 添加根引用
  add_root("obj1")
  add_root("obj2")
  
  // 垃圾回收
  let freed = collect()
  
  // obj3应该被回收（没有根引用）
  assert_eq(freed.length(), 1)
  assert_true(freed.contains("obj3"))
  
  let stats = get_gc_stats()
  assert_eq(stats.total_objects, 2)
  assert_eq(stats.root_count, 2)
}

// Test 9: 内存压力和限制
test "memory_pressure_and_limits" {
  // 内存限制管理器
  let create_memory_limiter = fn(limit) {
    let used = ref 0
    let allocations = ref []
    
    fn(size) {
      // 尝试分配内存
      if !used + size <= limit {
        used := !used + size
        allocations.push(size)
        true
      } else {
        false
      }
    },
    
    fn(size) {
      // 释放内存
      if !allocations.is_empty() {
        let removed = (!allocations).pop()
        used := !used - removed
        true
      } else {
        false
      }
    },
    
    fn() {
      // 获取使用情况
      {
        limit: limit,
        used: !used,
        available: limit - !used,
        allocation_count: (!allocations).length()
      }
    }
  }
  
  let (allocate_mem, free_mem, get_mem_status) = create_memory_limiter(1000)
  
  // 分配内存
  assert_true(allocate_mem(300))
  assert_true(allocate_mem(400))
  assert_true(allocate_mem(200))
  
  let status1 = get_mem_status()
  assert_eq(status1.used, 900)
  assert_eq(status1.available, 100)
  
  // 尝试超出限制
  assert_false(allocate_mem(200))  // 900 + 200 > 1000
  
  // 释放内存
  assert_true(free_mem(0))  // 释放最后一次分配
  
  let status2 = get_mem_status()
  assert_eq(status2.used, 700)
  assert_eq(status2.available, 300)
  
  // 内存压力检测
  let memory_pressure_detector = fn(warning_threshold, critical_threshold) {
    let memory_samples = ref []
    
    fn(sample) {
      // 添加内存样本
      memory_samples.push(sample)
      
      // 保持最近的样本
      if (!memory_samples).length() > 10 {
        memory_samples := (!memory_samples).slice(1, (!memory_samples).length())
      }
    },
    
    fn() {
      // 检测压力水平
      if (!memory_samples).is_empty() {
        let latest = (!memory_samples)[(!memory_samples).length() - 1]
        let usage_ratio = latest.used.to_float() / latest.limit.to_float()
        
        if usage_ratio >= critical_threshold {
          "critical"
        } else if usage_ratio >= warning_threshold {
          "warning"
        } else {
          "normal"
        }
      } else {
        "unknown"
      }
    }
  }
  
  let (add_sample, check_pressure) = memory_pressure_detector(0.8, 0.95)
  
  add_sample({ limit: 1000, used: 500 })
  assert_eq(check_pressure(), "normal")
  
  add_sample({ limit: 1000, used: 850 })
  assert_eq(check_pressure(), "warning")
  
  add_sample({ limit: 1000, used: 960 })
  assert_eq(check_pressure(), "critical")
}

// Test 10: 内存分析工具
test "memory_profiling_tools" {
  // 内存分析器
  let create_memory_profiler = fn() {
    let allocation_records = ref []
    let deallocation_records = ref []
    let current_allocations = ref {}
    
    fn(id, size, type_name) {
      // 记录分配
      let record = {
        id: id,
        size: size,
        type: type_name,
        timestamp: current_time(),
        stack_trace: get_stack_trace()  // 模拟函数
      }
      
      allocation_records.push(record)
      current_allocations.set(id, record)
    },
    
    fn(id) {
      // 记录释放
      match (!current_allocations).get(id) {
        Some(record) => {
          deallocation_records.push({
            id: id,
            timestamp: current_time(),
            lifetime: current_time() - record.timestamp
          })
          current_allocations.delete(id)
        }
        None => {}
      }
    },
    
    fn() {
      // 生成分析报告
      let total_allocated = (!allocation_records).reduce(fn(acc, record) { acc + record.size }, 0)
      let total_deallocated = (!deallocation_records).reduce(fn(acc, record) { acc + 1 }, 0)
      let current_usage = (!current_allocations).values().reduce(fn(acc, record) { acc + record.size }, 0)
      
      // 按类型分组
      let mut type_usage = {}
      for record in (!current_allocations).values() {
        let current = type_usage.get(record.type).unwrap_or(0)
        type_usage.set(record.type, current + record.size)
      }
      
      {
        total_allocated: total_allocated,
        total_deallocated: total_deallocated,
        current_usage: current_usage,
        allocation_count: (!allocation_records).length(),
        deallocation_count: (!deallocation_records).length(),
        leak_count: (!current_allocations).size(),
        type_distribution: type_usage
      }
    }
  }
  
  let (record_allocation, record_deallocation, generate_report) = create_memory_profiler()
  
  // 模拟分配和释放
  record_allocation("obj1", 100, "String")
  record_allocation("obj2", 200, "Array")
  record_allocation("obj3", 150, "Object")
  
  record_deallocation("obj2")
  
  // 生成报告
  let report = generate_report()
  
  assert_eq(report.allocation_count, 3)
  assert_eq(report.deallocation_count, 1)
  assert_eq(report.leak_count, 2)
  assert_eq(report.current_usage, 250)  // 100 + 150
  
  assert_eq(report.type_distribution.get("String").unwrap_or(0), 100)
  assert_eq(report.type_distribution.get("Object").unwrap_or(0), 150)
  assert_eq(report.type_distribution.get("Array").unwrap_or(0), 0)  // 已释放
  
  // 内存热点检测
  let detect_memory_hotspots = fn(allocation_records) {
    let mut hotspots = []
    
    // 按大小排序
    let sorted_by_size = allocation_records.sort_by(fn(a, b) { b.size - a.size })
    
    // 取前10%作为热点
    let top_count = (sorted_by_size.length() * 0.1).to_int()
    for i in 0..top_count {
      hotspots.push(sorted_by_size[i])
    }
    
    hotspots
  }
  
  let mock_records = [
    { id: "1", size: 1000, type: "LargeBuffer" },
    { id: "2", size: 50, type: "SmallObject" },
    { id: "3", size: 2000, type: "LargeBuffer" },
    { id: "4", size: 100, type: "MediumObject" },
    { id: "5", size: 5000, type: "HugeBuffer" }
  ]
  
  let hotspots = detect_memory_hotspots(mock_records)
  assert_eq(hotspots.length(), 1)
  assert_eq(hotspots[0].size, 5000)
}