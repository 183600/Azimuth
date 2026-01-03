// Azimuth Memory Leak Detection and Resource Cleanup Tests
// 内存泄漏检测和资源清理测试套件

// Test 1: 内存分配和释放跟踪
test "memory allocation and deallocation tracking" {
  // 内存跟踪器配置
  let memory_tracker = {
    "allocations": [],
    "deallocations": [],
    "total_allocated": 0,
    "total_deallocated": 0,
    "current_usage": 0,
    "peak_usage": 0
  }
  
  // 模拟内存分配
  let allocate_memory = fn(size, tracker) {
    let allocation_id = @lib.string.random(8)
    let timestamp = @lib.time.now()
    
    // 更新跟踪器
    tracker["allocations"] = tracker["allocations"].push({
      "id": allocation_id,
      "size": size,
      "timestamp": timestamp,
      "stack_trace": ["function_a", "function_b", "allocate_memory"]
    })
    
    tracker["total_allocated"] = tracker["total_allocated"] + size
    tracker["current_usage"] = tracker["current_usage"] + size
    
    if tracker["current_usage"] > tracker["peak_usage"] {
      tracker["peak_usage"] = tracker["current_usage"]
    }
    
    {
      "success": true,
      "allocation_id": allocation_id,
      "size": size,
      "timestamp": timestamp
    }
  }
  
  // 模拟内存释放
  let deallocate_memory = fn(allocation_id, tracker) {
    // 查找分配记录
    let allocation = tracker["allocations"].filter_fn(alloc => alloc["id"] == allocation_id)[0]
    
    if allocation {
      let timestamp = @lib.time.now()
      
      // 更新跟踪器
      tracker["deallocations"] = tracker["deallocations"].push({
        "allocation_id": allocation_id,
        "size": allocation["size"],
        "timestamp": timestamp,
        "lifetime": timestamp - allocation["timestamp"]
      })
      
      tracker["total_deallocated"] = tracker["total_deallocated"] + allocation["size"]
      tracker["current_usage"] = tracker["current_usage"] - allocation["size"]
      
      // 从分配列表中移除
      tracker["allocations"] = tracker["allocations"].filter_fn(alloc => alloc["id"] != allocation_id)
      
      {
        "success": true,
        "allocation_id": allocation_id,
        "size": allocation["size"],
        "lifetime": timestamp - allocation["timestamp"]
      }
    } else {
      {
        "success": false,
        "error": "Allocation not found"
      }
    }
  }
  
  // 测试内存分配和释放
  let allocation_sizes = [1024, 2048, 4096, 8192, 16384]
  let allocation_ids = []
  
  // 分配内存
  for size in allocation_sizes {
    let result = allocate_memory(size, memory_tracker)
    allocation_ids = allocation_ids.push(result["allocation_id"])
  }
  
  // 验证分配结果
  assert_eq(memory_tracker["allocations"].length(), allocation_sizes.length(), "分配记录数量应该正确")
  assert_eq(memory_tracker["total_allocated"], allocation_sizes.reduce(fn(acc, size) { acc + size }, 0), "总分配量应该正确")
  assert_eq(memory_tracker["current_usage"], memory_tracker["total_allocated"], "当前使用量应该等于总分配量")
  assert_eq(memory_tracker["total_deallocated"], 0, "初始释放量应该为0")
  
  // 释放部分内存
  let deallocation_results = []
  for i in 0..3 {  // 释放前4个分配
    let result = deallocate_memory(allocation_ids[i], memory_tracker)
    deallocation_results = deallocation_results.push(result)
  }
  
  // 验证释放结果
  let successful_deallocations = deallocation_results.filter_fn(result => result["success"])
  assert_eq(successful_deallocations.length(), 4, "应该有4个成功的释放")
  
  // 验证内存跟踪状态
  assert_eq(memory_tracker["deallocations"].length(), 4, "释放记录数量应该正确")
  assert_eq(memory_tracker["allocations"].length(), 1, "应该还有1个未释放的分配")
  assert_true(memory_tracker["current_usage"] < memory_tracker["total_allocated"], "当前使用量应该小于总分配量")
  assert_true(memory_tracker["total_deallocated"] > 0, "总释放量应该大于0")
  
  // 释放剩余内存
  let final_deallocation = deallocate_memory(allocation_ids[4], memory_tracker)
  assert_true(final_deallocation["success"], "最后的释放应该成功")
  
  // 验证完全释放后的状态
  assert_eq(memory_tracker["allocations"].length(), 0, "所有分配都应该被释放")
  assert_eq(memory_tracker["current_usage"], 0, "当前使用量应该为0")
  assert_eq(memory_tracker["total_allocated"], memory_tracker["total_deallocated"], "总分配量应该等于总释放量")
}

// Test 2: 内存泄漏检测算法
test "memory leak detection algorithm" {
  // 内存泄漏检测器配置
  let leak_detector = {
    "allocation_threshold": 100,      // 分配数量阈值
    "size_threshold": 10485760,       // 10MB大小阈值
    "age_threshold": 60000,           // 60秒时间阈值
    "growth_rate_threshold": 0.5,     // 50%增长率阈值
    "detected_leaks": []
  }
  
  // 模拟内存分配历史
  let allocation_history = []
  let current_time = @lib.time.now()
  
  // 创建正常分配（会被释放）
  for i in 0..50 {
    allocation_history = allocation_history.push({
      "id": "normal_" + i.to_string(),
      "size": 1024,
      "timestamp": current_time - i * 1000,
      "released": true,
      "release_time": current_time - i * 1000 + 500
    })
  }
  
  // 创建泄漏分配（不会被释放）
  for i in 0..20 {
    allocation_history = allocation_history.push({
      "id": "leak_" + i.to_string(),
      "size": 2048,
      "timestamp": current_time - i * 2000,
      "released": false,
      "release_time": nil
    })
  }
  
  // 创建大量小分配
  for i in 0..150 {
    allocation_history = allocation_history.push({
      "id": "small_" + i.to_string(),
      "size": 512,
      "timestamp": current_time - i * 100,
      "released": i < 100,  // 前100个被释放，后50个泄漏
      "release_time": if i < 100 { Some(current_time - i * 100 + 50) } else { nil }
    })
  }
  
  // 内存泄漏检测函数
  let detect_memory_leaks = fn(history, detector) {
    let leaks = []
    let current_allocations = history.filter_fn(alloc => not(alloc["released"]))
    
    // 检测1：未释放分配数量超过阈值
    if current_allocations.length() > detector["allocation_threshold"] {
      leaks = leaks.push({
        "type": "count_threshold",
        "description": "Too many unreleased allocations",
        "count": current_allocations.length(),
        "threshold": detector["allocation_threshold"]
      })
    }
    
    // 检测2：未释放分配总大小超过阈值
    let total_unreleased_size = current_allocations.reduce(fn(acc, alloc) { acc + alloc["size"] }, 0)
    if total_unreleased_size > detector["size_threshold"] {
      leaks = leaks.push({
        "type": "size_threshold",
        "description": "Too much unreleased memory",
        "size": total_unreleased_size,
        "threshold": detector["size_threshold"]
      })
    }
    
    // 检测3：长期存在的分配
    let old_allocations = current_allocations.filter_fn(alloc => {
      current_time - alloc["timestamp"] > detector["age_threshold"]
    })
    
    if old_allocations.length() > 0 {
      leaks = leaks.push({
        "type": "age_threshold",
        "description": "Long-lived allocations detected",
        "count": old_allocations.length(),
        "age_threshold": detector["age_threshold"]
      })
    }
    
    // 检测4：内存增长率异常
    let time_windows = [10000, 30000, 60000]  // 10秒、30秒、60秒窗口
    for window in time_windows {
      let window_start = current_time - window
      let window_allocations = history.filter_fn(alloc => {
        alloc["timestamp"] >= window_start && not(alloc["released"])
      })
      
      if window_allocations.length() > 0 {
        let window_size = window_allocations.reduce(fn(acc, alloc) { acc + alloc["size"] }, 0)
        let growth_rate = window_size.to_decimal() / detector["size_threshold"].to_decimal()
        
        if growth_rate > detector["growth_rate_threshold"] {
          leaks = leaks.push({
            "type": "growth_rate",
            "description": "High memory growth rate detected",
            "window": window,
            "growth_rate": growth_rate,
            "threshold": detector["growth_rate_threshold"]
          })
        }
      }
    }
    
    leaks
  }
  
  // 执行泄漏检测
  let detected_leaks = detect_memory_leaks(allocation_history, leak_detector)
  
  // 验证泄漏检测结果
  assert_true(detected_leaks.length() > 0, "应该检测到内存泄漏")
  
  // 验证特定类型的泄漏
  let count_leaks = detected_leaks.filter_fn(leak => leak["type"] == "count_threshold")
  let size_leaks = detected_leaks.filter_fn(leak => leak["type"] == "size_threshold")
  let age_leaks = detected_leaks.filter_fn(leak => leak["type"] == "age_threshold")
  
  assert_true(count_leaks.length() > 0, "应该检测到数量阈值泄漏")
  assert_true(size_leaks.length() > 0, "应该检测到大小阈值泄漏")
  assert_true(age_leaks.length() > 0, "应该检测到长期分配泄漏")
  
  // 验证泄漏详情
  for leak in detected_leaks {
    assert_true(leak["description"].length() > 0, "泄漏描述不应为空")
    
    match leak["type"] {
      "count_threshold" => {
        assert_true(leak["count"] > leak["threshold"], "检测到的数量应该超过阈值")
      }
      "size_threshold" => {
        assert_true(leak["size"] > leak["threshold"], "检测到的大小应该超过阈值")
      }
      "age_threshold" => {
        assert_true(leak["count"] > 0, "应该有长期存在的分配")
      }
      "growth_rate" => {
        assert_true(leak["growth_rate"] > leak["threshold"], "增长率应该超过阈值")
      }
      _ => {}
    }
  }
}

// Test 3: 资源生命周期管理
test "resource lifecycle management" {
  // 资源类型定义
  let resource_types = [
    {"name": "file_handle", "cleanup_function": "close_file", "auto_cleanup": true},
    {"name": "database_connection", "cleanup_function": "close_connection", "auto_cleanup": true},
    {"name": "network_socket", "cleanup_function": "close_socket", "auto_cleanup": true},
    {"name": "memory_buffer", "cleanup_function": "free_buffer", "auto_cleanup": false},
    {"name": "timer", "cleanup_function": "cancel_timer", "auto_cleanup": true}
  ]
  
  // 资源管理器
  let resource_manager = {
    "active_resources": [],
    "cleanup_queue": [],
    "cleanup_statistics": {
      "total_cleaned": 0,
      "failed_cleanups": 0,
      "auto_cleanups": 0,
      "manual_cleanups": 0
    }
  }
  
  // 创建资源
  let create_resource = fn(resource_type, resource_id, manager) {
    let timestamp = @lib.time.now()
    let resource_config = resource_types.filter_fn(rt => rt["name"] == resource_type)[0]
    
    let resource = {
      "id": resource_id,
      "type": resource_type,
      "created_at": timestamp,
      "last_accessed": timestamp,
      "cleanup_function": resource_config["cleanup_function"],
      "auto_cleanup": resource_config["auto_cleanup"],
      "cleanup_count": 0,
      "state": "active"
    }
    
    manager["active_resources"] = manager["active_resources"].push(resource)
    
    {
      "success": true,
      "resource_id": resource_id,
      "resource_type": resource_type
    }
  }
  
  // 访问资源
  let access_resource = fn(resource_id, manager) {
    let resource_index = manager["active_resources"].index_of_fn(resource => resource["id"] == resource_id)
    
    match resource_index {
      Some(index) => {
        let resource = manager["active_resources"][index]
        manager["active_resources"][index]["last_accessed"] = @lib.time.now()
        
        {
          "success": true,
          "resource": resource
        }
      }
      None => {
        {
          "success": false,
          "error": "Resource not found"
        }
      }
    }
  }
  
  // 清理资源
  let cleanup_resource = fn(resource_id, manager, manual) {
    let resource_index = manager["active_resources"].index_of_fn(resource => resource["id"] == resource_id)
    
    match resource_index {
      Some(index) => {
        let resource = manager["active_resources"][index]
        
        // 模拟清理过程
        let cleanup_success = @lib.random.float() > 0.1  // 90%成功率
        
        if cleanup_success {
          // 更新统计
          manager["cleanup_statistics"]["total_cleaned"] = manager["cleanup_statistics"]["total_cleaned"] + 1
          
          if manual {
            manager["cleanup_statistics"]["manual_cleanups"] = manager["cleanup_statistics"]["manual_cleanups"] + 1
          } else {
            manager["cleanup_statistics"]["auto_cleanups"] = manager["cleanup_statistics"]["auto_cleanups"] + 1
          }
          
          // 从活动资源列表中移除
          manager["active_resources"] = manager["active_resources"].slice(0, index) + 
                                     manager["active_resources"].slice(index + 1, manager["active_resources"].length())
          
          {
            "success": true,
            "resource_id": resource_id,
            "cleanup_type": if manual { "manual" } else { "auto" }
          }
        } else {
          manager["cleanup_statistics"]["failed_cleanups"] = manager["cleanup_statistics"]["failed_cleanups"] + 1
          
          {
            "success": false,
            "resource_id": resource_id,
            "error": "Cleanup failed"
          }
        }
      }
      None => {
        {
          "success": false,
          "error": "Resource not found"
        }
      }
    }
  }
  
  // 自动清理过期资源
  let auto_cleanup_expired = fn(manager, max_age) {
    let current_time = @lib.time.now()
    let expired_resources = []
    
    for i in 0..manager["active_resources"].length() {
      let resource = manager["active_resources"][i]
      
      if resource["auto_cleanup"] && (current_time - resource["last_accessed"]) > max_age {
        expired_resources = expired_resources.push(resource["id"])
      }
    }
    
    let cleanup_results = []
    for resource_id in expired_resources {
      let result = cleanup_resource(resource_id, manager, false)
      cleanup_results = cleanup_results.push(result)
    }
    
    cleanup_results
  }
  
  // 创建测试资源
  let test_resources = [
    {"type": "file_handle", "id": "file_001"},
    {"type": "database_connection", "id": "db_001"},
    {"type": "network_socket", "id": "socket_001"},
    {"type": "memory_buffer", "id": "buffer_001"},
    {"type": "timer", "id": "timer_001"}
  ]
  
  let creation_results = []
  for resource in test_resources {
    let result = create_resource(resource["type"], resource["id"], resource_manager)
    creation_results = creation_results.push(result)
  }
  
  // 验证资源创建
  assert_eq(resource_manager["active_resources"].length(), test_resources.length(), "活动资源数量应该正确")
  assert_true(creation_results.filter_fn(r => r["success"]).length() == creation_results.length(), "所有创建应该成功")
  
  // 访问部分资源
  let access_results = []
  for i in 0..3 {
    let result = access_resource(test_resources[i]["id"], resource_manager)
    access_results = access_results.push(result)
  }
  
  // 验证资源访问
  assert_true(access_results.filter_fn(r => r["success"]).length() == access_results.length(), "所有访问应该成功")
  
  // 手动清理部分资源
  let manual_cleanup_results = []
  for i in 0..2 {
    let result = cleanup_resource(test_resources[i]["id"], resource_manager, true)
    manual_cleanup_results = manual_cleanup_results.push(result)
  }
  
  // 验证手动清理
  assert_true(manual_cleanup_results.filter_fn(r => r["success"]).length() > 0, "应该有成功的手动清理")
  assert_eq(resource_manager["cleanup_statistics"]["manual_cleanups"], manual_cleanup_results.filter_fn(r => r["success"]).length(), 
             "手动清理统计应该正确")
  
  // 模拟时间流逝并执行自动清理
  @lib.time.sleep(100)  // 等待100ms
  
  // 更新资源的最后访问时间（模拟部分资源被访问）
  let current_time = @lib.time.now()
  for i in 0..resource_manager["active_resources"].length() {
    if i % 2 == 0 {
      resource_manager["active_resources"][i]["last_accessed"] = current_time - 1000  // 1秒前访问
    } else {
      resource_manager["active_resources"][i]["last_accessed"] = current_time - 100000  // 100秒前访问
    }
  }
  
  // 执行自动清理（清理50秒未访问的资源）
  let auto_cleanup_results = auto_cleanup_expired(resource_manager, 50000)
  
  // 验证自动清理
  assert_true(auto_cleanup_results.length() > 0, "应该有自动清理的资源")
  assert_eq(resource_manager["cleanup_statistics"]["auto_cleanups"], auto_cleanup_results.filter_fn(r => r["success"]).length(), 
             "自动清理统计应该正确")
  
  // 验证清理统计
  assert_true(resource_manager["cleanup_statistics"]["total_cleaned"] > 0, "总清理数应该大于0")
  assert_eq(resource_manager["cleanup_statistics"]["total_cleaned"], 
             resource_manager["cleanup_statistics"]["manual_cleanups"] + resource_manager["cleanup_statistics"]["auto_cleanups"],
             "总清理数应该等于手动清理数加自动清理数")
}

// Test 4: 垃圾回收性能测试
test "garbage collection performance testing" {
  // 垃圾回收配置
  let gc_config = {
    "generation_threshold": 1000,    // 代数阈值
    "memory_threshold": 10485760,    // 10MB内存阈值
    "time_threshold": 5000,          // 5秒时间阈值
    "force_gc_interval": 30000       // 30秒强制GC间隔
  }
  
  // GC统计
  let gc_statistics = {
    "total_collections": 0,
    "young_gc_count": 0,
    "old_gc_count": 0,
    "full_gc_count": 0,
    "total_time": 0.0,
    "memory_recovered": 0,
    "objects_collected": 0
  }
  
  // 模拟对象分配
  let allocate_objects = fn(count, size_range, generation) {
    let objects = []
    for i in 0..count {
      let size = @lib.random.int(size_range[1] - size_range[0]) + size_range[0]
      objects = objects.push({
        "id": "obj_" + i.to_string(),
        "size": size,
        "generation": generation,
        "created_at": @lib.time.now(),
        "last_accessed": @lib.time.now(),
        "references": @lib.random.int(5),  // 0-4个引用
        "reachable": @lib.random.float() > 0.3  // 70%可达
      })
    }
    objects
  }
  
  // 模拟垃圾回收
  let simulate_garbage_collection = fn(objects, gc_type, stats) {
    let start_time = @lib.time.now()
    
    // 标记可达对象
    let reachable_objects = objects.filter_fn(obj => obj["reachable"])
    let unreachable_objects = objects.filter_fn(obj => not(obj["reachable"]))
    
    // 计算回收的内存
    let memory_recovered = unreachable_objects.reduce(fn(acc, obj) { acc + obj["size"] }, 0)
    
    // 更新统计
    stats["total_collections"] = stats["total_collections"] + 1
    stats["total_time"] = stats["total_time"] + (@lib.time.now() - start_time)
    stats["memory_recovered"] = stats["memory_recovered"] + memory_recovered
    stats["objects_collected"] = stats["objects_collected"] + unreachable_objects.length()
    
    match gc_type {
      "young" => {
        stats["young_gc_count"] = stats["young_gc_count"] + 1
      }
      "old" => {
        stats["old_gc_count"] = stats["old_gc_count"] + 1
      }
      "full" => {
        stats["full_gc_count"] = stats["full_gc_count"] + 1
      }
      _ => {}
    }
    
    // 模拟GC时间（根据对象数量）
    let gc_time = objects.length() * 0.1  // 每个对象0.1ms
    @lib.time.sleep(gc_time.to_int())
    
    {
      "gc_type": gc_type,
      "objects_before": objects.length(),
      "objects_after": reachable_objects.length(),
      "memory_recovered": memory_recovered,
      "execution_time": gc_time
    }
  }
  
  // 创建不同代的对象
  let young_objects = allocate_objects(500, (64, 256), 0)  // 年轻代
  let old_objects = allocate_objects(200, (256, 1024), 1)   // 老年代
  
  // 执行年轻代GC
  let young_gc_result = simulate_garbage_collection(young_objects, "young", gc_statistics)
  
  // 验证年轻代GC
  assert_eq(young_gc_result["gc_type"], "young", "GC类型应该正确")
  assert_true(young_gc_result["objects_after"] <= young_gc_result["objects_before"], "GC后对象数应该减少或保持不变")
  assert_true(young_gc_result["memory_recovered"] >= 0, "回收的内存应该非负")
  assert_eq(gc_statistics["young_gc_count"], 1, "年轻代GC计数应该正确")
  
  // 执行老年代GC
  let old_gc_result = simulate_garbage_collection(old_objects, "old", gc_statistics)
  
  // 验证老年代GC
  assert_eq(old_gc_result["gc_type"], "old", "GC类型应该正确")
  assert_true(old_gc_result["memory_recovered"] >= 0, "回收的内存应该非负")
  assert_eq(gc_statistics["old_gc_count"], 1, "老年代GC计数应该正确")
  
  // 创建更多对象触发Full GC
  let many_objects = allocate_objects(1000, (128, 512), 0)
  let full_gc_result = simulate_garbage_collection(many_objects, "full", gc_statistics)
  
  // 验证Full GC
  assert_eq(full_gc_result["gc_type"], "full", "GC类型应该正确")
  assert_eq(gc_statistics["full_gc_count"], 1, "Full GC计数应该正确")
  
  // 验证总体GC统计
  assert_eq(gc_statistics["total_collections"], 3, "总GC次数应该正确")
  assert_eq(gc_statistics["young_gc_count"] + gc_statistics["old_gc_count"] + gc_statistics["full_gc_count"], 
             gc_statistics["total_collections"], "各类型GC次数总和应该等于总GC次数")
  assert_true(gc_statistics["total_time"] > 0, "总GC时间应该大于0")
  assert_true(gc_statistics["memory_recovered"] > 0, "总回收内存应该大于0")
  assert_true(gc_statistics["objects_collected"] > 0, "总回收对象数应该大于0")
  
  // 计算GC性能指标
  let avg_gc_time = gc_statistics["total_time"] / gc_statistics["total_collections"].to_decimal()
  let memory_efficiency = gc_statistics["memory_recovered"].to_decimal() / gc_statistics["total_time"]
  
  assert_true(avg_gc_time > 0, "平均GC时间应该大于0")
  assert_true(memory_efficiency > 0, "内存效率应该大于0")
}

// Test 5: 循环引用检测
test "circular reference detection" {
  // 对象引用图
  let object_graph = {
    "nodes": {},
    "edges": [],
    "detected_cycles": []
  }
  
  // 创建对象节点
  let create_object_node = fn(object_id, size) {
    object_graph["nodes"][object_id] = {
      "id": object_id,
      "size": size,
      "references": [],
      "referenced_by": [],
      "visited": false,
      "in_cycle": false
    }
  }
  
  // 添加引用关系
  let add_reference = fn(from_id, to_id) {
    // 更新引用
    let from_node = object_graph["nodes"][from_id]
    let to_node = object_graph["nodes"][to_id]
    
    if from_node && to_node {
      from_node["references"] = from_node["references"].push(to_id)
      to_node["referenced_by"] = to_node["referenced_by"].push(from_id)
      
      object_graph["edges"] = object_graph["edges"].push((from_id, to_id))
    }
  }
  
  // 深度优先搜索检测循环引用
  let detect_cycles_dfs = fn(node_id, path, graph) {
    let node = graph["nodes"][node_id]
    
    if not(node) {
      return false
    }
    
    if node["visited"] {
      // 检查是否在当前路径中（循环引用）
      if path.contains(node_id) {
        let cycle_start = path.index_of(node_id)
        let cycle = path.slice(cycle_start, path.length()) + [node_id]
        graph["detected_cycles"] = graph["detected_cycles"].push(cycle)
        return true
      }
      return false
    }
    
    node["visited"] = true
    let current_path = path + [node_id]
    let found_cycle = false
    
    // 递归检查所有引用
    for ref_id in node["references"] {
      if detect_cycles_dfs(ref_id, current_path, graph) {
        found_cycle = true
      }
    }
    
    node["visited"] = false
    found_cycle
  }
  
  // 检测所有循环引用
  let detect_all_cycles = fn(graph) {
    // 重置访问标记
    for node_id in graph["nodes"].keys() {
      graph["nodes"][node_id]["visited"] = false
    }
    
    graph["detected_cycles"] = []
    
    // 对每个节点进行DFS
    for node_id in graph["nodes"].keys() {
      detect_cycles_dfs(node_id, [], graph)
    }
    
    graph["detected_cycles"]
  }
  
  // 创建测试对象图
  create_object_node("A", 1024)
  create_object_node("B", 2048)
  create_object_node("C", 1536)
  create_object_node("D", 3072)
  create_object_node("E", 512)
  
  // 创建正常引用关系
  add_reference("A", "B")
  add_reference("B", "C")
  add_reference("C", "D")
  
  // 创建循环引用
  add_reference("D", "A")  // A -> B -> C -> D -> A (循环)
  add_reference("E", "E")  // E -> E (自循环)
  
  // 检测循环引用
  let detected_cycles = detect_all_cycles(object_graph)
  
  // 验证循环引用检测结果
  assert_true(detected_cycles.length() >= 2, "应该检测到至少2个循环引用")
  
  // 验证具体循环
  let cycle_a = detected_cycles.filter_fn(cycle => cycle.contains("A") && cycle.length() > 3)
  let cycle_e = detected_cycles.filter_fn(cycle => cycle.contains("E"))
  
  assert_true(cycle_a.length() > 0, "应该检测到A-B-C-D-A循环")
  assert_true(cycle_e.length() > 0, "应该检测到E自循环")
  
  // 验证循环中的节点标记
  for cycle in detected_cycles {
    for node_id in cycle {
      let node = object_graph["nodes"][node_id]
      if node {
        node["in_cycle"] = true
      }
    }
  }
  
  assert_true(object_graph["nodes"]["A"]["in_cycle"], "节点A应该在循环中")
  assert_true(object_graph["nodes"]["B"]["in_cycle"], "节点B应该在循环中")
  assert_true(object_graph["nodes"]["C"]["in_cycle"], "节点C应该在循环中")
  assert_true(object_graph["nodes"]["D"]["in_cycle"], "节点D应该在循环中")
  assert_true(object_graph["nodes"]["E"]["in_cycle"], "节点E应该在循环中")
  
  // 计算循环引用占用的内存
  let cycle_memory = 0
  for node_id in object_graph["nodes"].keys() {
    let node = object_graph["nodes"][node_id]
    if node["in_cycle"] {
      cycle_memory = cycle_memory + node["size"]
    }
  }
  
  let total_memory = object_graph["nodes"].reduce(fn(acc, node) { acc + node["size"] }, 0)
  let cycle_memory_ratio = cycle_memory.to_decimal() / total_memory.to_decimal()
  
  assert_true(cycle_memory > 0, "循环引用应该占用内存")
  assert_true(cycle_memory_ratio > 0.5, "循环引用应该占用大部分内存")
}

// Test 6: 资源池管理
test "resource pool management" {
  // 资源池配置
  let pool_config = {
    "min_size": 2,
    "max_size": 10,
    "creation_timeout": 1000,    // 1秒
    "acquisition_timeout": 500,  // 500ms
    "idle_timeout": 30000,       // 30秒
    "max_lifetime": 300000       // 5分钟
  }
  
  // 资源池
  let resource_pool = {
    "available_resources": [],
    "active_resources": [],
    "creation_queue": [],
    "statistics": {
      "total_created": 0,
      "total_destroyed": 0,
      "peak_size": 0,
      "acquisition_count": 0,
      "release_count": 0
    }
  }
  
  // 创建资源
  let create_resource = fn(pool) {
    let start_time = @lib.time.now()
    
    // 模拟资源创建延迟
    let creation_delay = @lib.random.int(200) + 50  // 50-250ms
    @lib.time.sleep(creation_delay)
    
    let end_time = @lib.time.now()
    
    if end_time - start_time > pool_config["creation_timeout"] {
      return {"success": false, "error": "Creation timeout"}
    }
    
    let resource = {
      "id": "resource_" + @lib.string.random(8),
      "created_at": end_time,
      "last_acquired": nil,
      "acquisition_count": 0,
      "state": "available"
    }
    
    pool["statistics"]["total_created"] = pool["statistics"]["total_created"] + 1
    
    // 更新峰值大小
    let current_size = pool["available_resources"].length() + pool["active_resources"].length()
    if current_size > pool["statistics"]["peak_size"] {
      pool["statistics"]["peak_size"] = current_size
    }
    
    {"success": true, "resource": resource}
  }
  
  // 获取资源
  let acquire_resource = fn(pool) {
    let start_time = @lib.time.now()
    
    // 尝试从可用资源中获取
    if pool["available_resources"].length() > 0 {
      let resource = pool["available_resources"][0]
      pool["available_resources"] = pool["available_resources"].slice(1, pool["available_resources"].length())
      
      resource["last_acquired"] = start_time
      resource["acquisition_count"] = resource["acquisition_count"] + 1
      resource["state"] = "active"
      
      pool["active_resources"] = pool["active_resources"].push(resource)
      pool["statistics"]["acquisition_count"] = pool["statistics"]["acquisition_count"] + 1
      
      return {"success": true, "resource": resource}
    }
    
    // 如果没有可用资源且未达到最大大小，创建新资源
    let current_size = pool["available_resources"].length() + pool["active_resources"].length()
    if current_size < pool_config["max_size"] {
      let create_result = create_resource(pool)
      if create_result["success"] {
        let resource = create_result["resource"]
        resource["last_acquired"] = start_time
        resource["acquisition_count"] = resource["acquisition_count"] + 1
        resource["state"] = "active"
        
        pool["active_resources"] = pool["active_resources"].push(resource)
        pool["statistics"]["acquisition_count"] = pool["statistics"]["acquisition_count"] + 1
        
        return {"success": true, "resource": resource}
      }
    }
    
    // 检查超时
    if @lib.time.now() - start_time > pool_config["acquisition_timeout"] {
      return {"success": false, "error": "Acquisition timeout"}
    }
    
    {"success": false, "error": "No available resources"}
  }
  
  // 释放资源
  let release_resource = fn(resource_id, pool) {
    let resource_index = pool["active_resources"].index_of_fn(resource => resource["id"] == resource_id)
    
    match resource_index {
      Some(index) => {
        let resource = pool["active_resources"][index]
        
        // 检查资源是否过期
        let current_time = @lib.time.now()
        let is_expired = (current_time - resource["created_at"]) > pool_config["max_lifetime"]
        
        if is_expired {
          // 销毁过期资源
          pool["active_resources"] = pool["active_resources"].slice(0, index) + 
                                     pool["active_resources"].slice(index + 1, pool["active_resources"].length())
          pool["statistics"]["total_destroyed"] = pool["statistics"]["total_destroyed"] + 1
          
          return {"success": true, "action": "destroyed", "reason": "expired"}
        } else {
          // 返回资源到池中
          resource["state"] = "available"
          pool["active_resources"] = pool["active_resources"].slice(0, index) + 
                                     pool["active_resources"].slice(index + 1, pool["active_resources"].length())
          pool["available_resources"] = pool["available_resources"].push(resource)
          pool["statistics"]["release_count"] = pool["statistics"]["release_count"] + 1
          
          return {"success": true, "action": "returned"}
        }
      }
      None => {
        {"success": false, "error": "Resource not found"}
      }
    }
  }
  
  // 初始化资源池（创建最小数量的资源）
  let init_results = []
  for i in 0..pool_config["min_size"] {
    let result = create_resource(resource_pool)
    if result["success"] {
      resource_pool["available_resources"] = resource_pool["available_resources"].push(result["resource"])
    }
    init_results = init_results.push(result)
  }
  
  // 验证初始化
  assert_eq(resource_pool["available_resources"].length(), pool_config["min_size"], "应该创建最小数量的资源")
  assert_eq(resource_pool["statistics"]["total_created"], pool_config["min_size"], "创建统计应该正确")
  
  // 测试资源获取
  let acquisition_results = []
  let acquired_resources = []
  
  for i in 0..5 {
    let result = acquire_resource(resource_pool)
    acquisition_results = acquisition_results.push(result)
    if result["success"] {
      acquired_resources = acquired_resources.push(result["resource"]["id"])
    }
  }
  
  // 验证资源获取
  assert_true(acquisition_results.filter_fn(r => r["success"]).length() > 0, "应该有成功的资源获取")
  assert_eq(resource_pool["statistics"]["acquisition_count"], acquired_resources.length(), "获取统计应该正确")
  
  // 测试资源释放
  let release_results = []
  for i in 0..3 {
    let result = release_resource(acquired_resources[i], resource_pool)
    release_results = release_results.push(result)
  }
  
  // 验证资源释放
  assert_true(release_results.filter_fn(r => r["success"]).length() > 0, "应该有成功的资源释放")
  assert_eq(resource_pool["statistics"]["release_count"], release_results.filter_fn(r => r["success"]).length(), 
             "释放统计应该正确")
  
  // 验证池大小限制
  let total_resources = resource_pool["available_resources"].length() + resource_pool["active_resources"].length()
  assert_true(total_resources <= pool_config["max_size"], "池大小不应该超过最大限制")
  
  // 验证统计信息
  assert_true(resource_pool["statistics"]["peak_size"] >= pool_config["min_size"], "峰值大小应该至少为最小大小")
  assert_eq(resource_pool["statistics"]["acquisition_count"], resource_pool["statistics"]["release_count"] + resource_pool["active_resources"].length(),
             "获取次数应该等于释放次数加活动资源数")
}

// Test 7: 内存压力测试
test "memory pressure testing" {
  // 内存压力测试配置
  let pressure_test_config = {
    "max_memory": 52428800,    // 50MB
    "allocation_rate": 100,     // 每次分配100个对象
    "allocation_size_range": (1024, 8192),  // 1KB-8KB
    "test_duration": 10000,     // 10秒
    "gc_trigger_threshold": 0.8  // 80%内存使用率触发GC
  }
  
  // 内存压力测试状态
  let pressure_test_state = {
    "current_memory_usage": 0,
    "peak_memory_usage": 0,
    "allocation_count": 0,
    "deallocation_count": 0,
    "gc_count": 0,
    "oom_count": 0,
    "allocations": []
  }
  
  // 模拟内存分配
  let allocate_under_pressure = fn(state, config) {
    let allocations = []
    
    for i in 0..config["allocation_rate"] {
      let size = @lib.random.int(config["allocation_size_range"][1] - config["allocation_size_range"][0]) + 
                  config["allocation_size_range"][0]
      
      // 检查内存限制
      if state["current_memory_usage"] + size > config["max_memory"] {
        state["oom_count"] = state["oom_count"] + 1
        break
      }
      
      let allocation = {
        "id": "alloc_" + state["allocation_count"].to_string(),
        "size": size,
        "timestamp": @lib.time.now(),
        "lifetime": 0
      }
      
      allocations = allocations.push(allocation)
      state["allocations"] = state["allocations"].push(allocation)
      state["current_memory_usage"] = state["current_memory_usage"] + size
      state["allocation_count"] = state["allocation_count"] + 1
      
      // 更新峰值内存使用
      if state["current_memory_usage"] > state["peak_memory_usage"] {
        state["peak_memory_usage"] = state["current_memory_usage"]
      }
    }
    
    allocations
  }
  
  // 模拟内存释放
  let deallocate_under_pressure = fn(state, deallocation_rate) {
    if state["allocations"].length() == 0 {
      return
    }
    
    let deallocation_count = @lib.min(deallocation_rate, state["allocations"].length())
    let current_time = @lib.time.now()
    
    for i in 0..deallocation_count {
      if state["allocations"].length() == 0 {
        break
      }
      
      // 随机选择一个分配进行释放
      let index = @lib.random.int(state["allocations"].length())
      let allocation = state["allocations"][index]
      
      state["current_memory_usage"] = state["current_memory_usage"] - allocation["size"]
      state["deallocation_count"] = state["deallocation_count"] + 1
      
      // 从分配列表中移除
      state["allocations"] = state["allocations"].slice(0, index) + 
                            state["allocations"].slice(index + 1, state["allocations"].length())
    }
  }
  
  // 模拟垃圾回收
  let simulate_gc_under_pressure = fn(state) {
    let memory_usage_ratio = state["current_memory_usage"].to_decimal() / pressure_test_config["max_memory"].to_decimal()
    
    if memory_usage_ratio > pressure_test_config["gc_trigger_threshold"] {
      // 模拟GC过程
      let gc_start = @lib.time.now()
      
      // 随机释放一些分配（模拟GC回收）
      let gc_target = state["allocations"].length() / 4  // 释放25%
      deallocate_under_pressure(state, gc_target)
      
      let gc_end = @lib.time.now()
      state["gc_count"] = state["gc_count"] + 1
      
      return {
        "triggered": true,
        "memory_before": state["current_memory_usage"] + (gc_target * 4096),  // 估算
        "memory_after": state["current_memory_usage"],
        "duration": gc_end - gc_start
      }
    }
    
    {"triggered": false}
  }
  
  // 执行内存压力测试
  let test_start_time = @lib.time.now()
  let gc_results = []
  
  while @lib.time.now() - test_start_time < pressure_test_config["test_duration"] {
    // 分配内存
    allocate_under_pressure(pressure_test_state, pressure_test_config)
    
    // 随机释放一些内存
    if pressure_test_state["allocations"].length() > 50 {
      deallocate_under_pressure(pressure_test_state, @lib.random.int(20) + 10)
    }
    
    // 检查是否需要GC
    let gc_result = simulate_gc_under_pressure(pressure_test_state)
    if gc_result["triggered"] {
      gc_results = gc_results.push(gc_result)
    }
    
    // 短暂休眠
    @lib.time.sleep(100)
  }
  
  // 验证压力测试结果
  assert_true(pressure_test_state["allocation_count"] > 0, "应该有内存分配")
  assert_true(pressure_test_state["deallocation_count"] > 0, "应该有内存释放")
  assert_true(pressure_test_state["peak_memory_usage"] > 0, "应该有峰值内存使用")
  assert_true(pressure_test_state["peak_memory_usage"] <= pressure_test_config["max_memory"], 
             "峰值内存使用不应该超过限制")
  
  // 验证GC触发
  assert_true(gc_results.length() > 0, "应该触发GC")
  
  for gc_result in gc_results {
    assert_true(gc_result["memory_before"] >= gc_result["memory_after"], "GC后内存应该减少")
    assert_true(gc_result["duration"] > 0, "GC应该有执行时间")
  }
  
  // 计算内存使用效率
  let total_allocated = pressure_test_state["allocation_count"] * 4096  // 估算平均大小
  let memory_efficiency = pressure_test_state["peak_memory_usage"].to_decimal() / total_allocated.to_decimal()
  
  assert_true(memory_efficiency > 0 && memory_efficiency < 1.0, "内存效率应该在合理范围内")
  
  // 验证OOM情况
  if pressure_test_state["oom_count"] > 0 {
    assert_true(pressure_test_state["oom_count"] < pressure_test_state["allocation_count"] / 10, 
               "OOM次数不应该过多")
  }
}

// Test 8: 异常情况下的资源清理
test "resource cleanup under exceptional conditions" {
  // 异常情况模拟
  let exception_scenarios = [
    {"type": "allocation_failure", "frequency": 0.1},
    {"type": "cleanup_failure", "frequency": 0.05},
    {"type": "timeout_error", "frequency": 0.08},
    {"type": "corruption_error", "frequency": 0.03}
  ]
  
  // 资源清理测试状态
  let cleanup_test_state = {
    "resources_created": 0,
    "resources_cleaned": 0,
    "cleanup_failures": 0,
    "exception_handled": 0,
    "fallback_cleanup": 0
  }
  
  // 模拟资源创建（可能失败）
  let create_resource_with_exceptions = fn(state) {
    state["resources_created"] = state["resources_created"] + 1
    
    // 检查是否触发分配失败异常
    let allocation_failure = exception_scenarios.filter_fn(s => s["type"] == "allocation_failure")[0]
    if @lib.random.float() < allocation_failure["frequency"] {
      state["exception_handled"] = state["exception_handled"] + 1
      return {"success": false, "error": "allocation_failure"}
    }
    
    let resource = {
      "id": "resource_" + state["resources_created"].to_string(),
      "created_at": @lib.time.now(),
      "cleanup_attempts": 0,
      "cleanup_success": false
    }
    
    {"success": true, "resource": resource}
  }
  
  // 模拟资源清理（可能失败）
  let cleanup_resource_with_exceptions = fn(resource, state) {
    resource["cleanup_attempts"] = resource["cleanup_attempts"] + 1
    
    // 检查是否触发清理失败异常
    let cleanup_failure = exception_scenarios.filter_fn(s => s["type"] == "cleanup_failure")[0]
    if @lib.random.float() < cleanup_failure["frequency"] {
      state["cleanup_failures"] = state["cleanup_failures"] + 1
      state["exception_handled"] = state["exception_handled"] + 1
      
      // 尝试备用清理
      if resource["cleanup_attempts"] < 3 {
        state["fallback_cleanup"] = state["fallback_cleanup"] + 1
        return {"success": false, "retry": true}
      } else {
        return {"success": false, "retry": false, "error": "cleanup_failed"}
      }
    }
    
    // 检查是否触发超时异常
    let timeout_error = exception_scenarios.filter_fn(s => s["type"] == "timeout_error")[0]
    if @lib.random.float() < timeout_error["frequency"] {
      state["exception_handled"] = state["exception_handled"] + 1
      
      // 超时情况下尝试强制清理
      state["fallback_cleanup"] = state["fallback_cleanup"] + 1
      resource["cleanup_success"] = true
      state["resources_cleaned"] = state["resources_cleaned"] + 1
      
      return {"success": true, "forced": true}
    }
    
    // 正常清理
    resource["cleanup_success"] = true
    state["resources_cleaned"] = state["resources_cleaned"] + 1
    
    {"success": true}
  }
  
  // 执行异常情况下的清理测试
  let test_resources = []
  let creation_results = []
  
  // 创建资源（可能失败）
  for i in 0..20 {
    let result = create_resource_with_exceptions(cleanup_test_state)
    creation_results = creation_results.push(result)
    if result["success"] {
      test_resources = test_resources.push(result["resource"])
    }
  }
  
  // 清理资源（可能失败）
  let cleanup_results = []
  for resource in test_resources {
    let mut cleanup_success = false
    let mut retry_count = 0
    
    while not(cleanup_success) && retry_count < 3 {
      let result = cleanup_resource_with_exceptions(resource, cleanup_test_state)
      cleanup_results = cleanup_results.push(result)
      
      if result["success"] {
        cleanup_success = true
      } else if result["retry"] {
        retry_count = retry_count + 1
      } else {
        break
      }
    }
  }
  
  // 验证异常处理结果
  assert_true(cleanup_test_state["resources_created"] > 0, "应该尝试创建资源")
  assert_true(cleanup_test_state["exception_handled"] > 0, "应该处理异常")
  
  let successful_creations = creation_results.filter_fn(r => r["success"])
  assert_eq(successful_creations.length(), test_resources.length(), "成功创建的资源应该等于测试资源数")
  
  // 验证重试机制
  let retry_cleanups = cleanup_results.filter_fn(r => not(r["success"]) && r["retry"])
  assert_true(retry_cleanups.length() > 0, "应该有重试的清理")
  
  // 验证备用清理
  assert_true(cleanup_test_state["fallback_cleanup"] > 0, "应该有备用清理")
  
  // 验证最终清理状态
  let successful_cleanups = cleanup_results.filter_fn(r => r["success"])
  assert_true(successful_cleanups.length() > 0, "应该有成功的清理")
  
  // 验证资源清理率
  let cleanup_rate = cleanup_test_state["resources_cleaned"].to_decimal() / test_resources.length().to_decimal()
  assert_true(cleanup_rate > 0.8, "资源清理率应该超过80%")
  
  // 验证异常处理覆盖率
  let exception_coverage = cleanup_test_state["exception_handled"].to_decimal() / cleanup_test_state["resources_created"].to_decimal()
  assert_true(exception_coverage > 0.1, "异常处理覆盖率应该合理")
}

// Test 9: 内存使用模式分析
test "memory usage pattern analysis" {
  // 内存使用模式类型
  let memory_patterns = [
    {"name": "steady_state", "description": "稳定的内存使用模式"},
    {"name": "gradual_growth", "description": "逐渐增长的内存使用模式"},
    {"name": "spiky", "description": "尖峰状的内存使用模式"},
    {"name": "leaky", "description": "内存泄漏模式"},
    {"name": "cyclic", "description": "周期性内存使用模式"}
  ]
  
  // 内存使用数据生成器
  let generate_memory_data = fn(pattern, duration, interval) {
    let data = []
    let mut base_memory = 10485760  // 10MB基础内存
    
    for i in 0..(duration / interval) {
      let timestamp = i * interval
      let memory_usage = match pattern {
        "steady_state" => {
          base_memory + @lib.random.int(1048576) - 524288  // ±0.5MB
        }
        "gradual_growth" => {
          base_memory + i * 10240  // 每次增加10KB
        }
        "spiky" => {
          if i % 10 == 0 {
            base_memory + @lib.random.int(10485760)  // 随机尖峰
          } else {
            base_memory
          }
        }
        "leaky" => {
          base_memory + i * 2048  // 每次泄漏2KB
        }
        "cyclic" => {
          let cycle_position = i % 20
          base_memory + @lib.sin(cycle_position.to_decimal() * 0.314) * 5242880  // 正弦波动
        }
        _ => base_memory
      }
      
      data = data.push({
        "timestamp": timestamp,
        "memory_usage": memory_usage,
        "allocations": @lib.random.int(100),
        "deallocations": @lib.random.int(80)
      })
    }
    
    data
  }
  
  // 内存模式分析器
  let analyze_memory_pattern = fn(data) {
    if data.length() == 0 {
      return {"pattern": "unknown", "confidence": 0.0}
    }
    
    let memory_values = data.map_fn(d => d["memory_usage"])
    let timestamps = data.map_fn(d => d["timestamp"])
    
    // 计算基本统计
    let min_memory = memory_values.reduce(fn(acc, val) { if val < acc { val } else { acc } }, memory_values[0])
    let max_memory = memory_values.reduce(fn(acc, val) { if val > acc { val } else { acc } }, memory_values[0])
    let avg_memory = memory_values.reduce(fn(acc, val) { acc + val }, 0) / memory_values.length()
    
    // 计算趋势
    let first_half = memory_values.slice(0, memory_values.length() / 2)
    let second_half = memory_values.slice(memory_values.length() / 2, memory_values.length())
    
    let first_avg = first_half.reduce(fn(acc, val) { acc + val }, 0) / first_half.length()
    let second_avg = second_half.reduce(fn(acc, val) { acc + val }, 0) / second_half.length()
    
    let trend = (second_avg - first_avg).to_decimal() / first_avg.to_decimal()
    
    // 计算方差
    let variance = memory_values
      .map_fn(val => (val - avg_memory) * (val - avg_memory))
      .reduce(fn(acc, val) { acc + val }, 0) / memory_values.length()
    let std_dev = @lib.sqrt(variance)
    
    // 计算变异系数
    let coefficient_of_variation = std_dev.to_decimal() / avg_memory.to_decimal()
    
    // 检测尖峰
    let spike_threshold = avg_memory + std_dev * 2
    let spikes = memory_values.filter_fn(val => val > spike_threshold).length()
    let spike_ratio = spikes.to_decimal() / memory_values.length()
    
    // 模式识别
    let pattern = if coefficient_of_variation < 0.1 {
      "steady_state"
    } else if trend > 0.1 && spikes < 5 {
      "gradual_growth"
    } else if spike_ratio > 0.1 {
      "spiky"
    } else if trend > 0.05 && coefficient_of_variation > 0.2 {
      "leaky"
    } else if coefficient_of_variation > 0.15 && coefficient_of_variation < 0.3 {
      "cyclic"
    } else {
      "unknown"
    }
    
    // 计算置信度
    let confidence = match pattern {
      "steady_state" => 1.0 - coefficient_of_variation
      "gradual_growth" => @lib.abs(trend)
      "spiky" => spike_ratio
      "leaky" => trend * (1.0 - coefficient_of_variation)
      "cyclic" => coefficient_of_variation * (1.0 - spike_ratio)
      _ => 0.5
    }
    
    {
      "pattern": pattern,
      "confidence": @lib.max(0.0, @lib.min(1.0, confidence)),
      "statistics": {
        "min_memory": min_memory,
        "max_memory": max_memory,
        "avg_memory": avg_memory,
        "trend": trend,
        "coefficient_of_variation": coefficient_of_variation,
        "spike_ratio": spike_ratio
      }
    }
  }
  
  // 测试不同模式的分析
  let pattern_results = []
  
  for pattern_config in memory_patterns {
    let pattern_name = pattern_config["name"]
    let test_data = generate_memory_data(pattern_name, 60000, 1000)  // 60秒，每秒1个点
    let analysis = analyze_memory_pattern(test_data)
    
    pattern_results = analysis
    pattern_results = pattern_results.push({
      "expected_pattern": pattern_name,
      "detected_pattern": analysis["pattern"],
      "confidence": analysis["confidence"]
    })
  }
  
  // 验证模式识别结果
  assert_eq(pattern_results.length(), memory_patterns.length(), "应该分析所有模式")
  
  // 验证特定模式的识别
  let steady_state_result = pattern_results.filter_fn(r => r["expected_pattern"] == "steady_state")[0]
  assert_eq(steady_state_result["detected_pattern"], "steady_state", "稳定状态模式应该被正确识别")
  assert_true(steady_state_result["confidence"] > 0.8, "稳定状态模式识别置信度应该高")
  
  let gradual_growth_result = pattern_results.filter_fn(r => r["expected_pattern"] == "gradual_growth")[0]
  assert_eq(gradual_growth_result["detected_pattern"], "gradual_growth", "逐渐增长模式应该被正确识别")
  assert_true(gradual_growth_result["confidence"] > 0.5, "逐渐增长模式识别置信度应该合理")
  
  let spiky_result = pattern_results.filter_fn(r => r["expected_pattern"] == "spiky")[0]
  assert_eq(spiky_result["detected_pattern"], "spiky", "尖峰模式应该被正确识别")
  assert_true(spiky_result["confidence"] > 0.5, "尖峰模式识别置信度应该合理")
  
  // 验证统计信息
  for result in pattern_results {
    assert_true(result["confidence"] >= 0.0 && result["confidence"] <= 1.0, "置信度应该在0-1范围内")
  }
}

// Test 10: 资源清理性能优化
test "resource cleanup performance optimization" {
  // 清理优化配置
  let optimization_config = {
    "batch_size": 50,           // 批处理大小
    "parallel_cleanup": true,    // 并行清理
    "lazy_cleanup": true,        // 延迟清理
    "cleanup_cache_size": 100,   // 清理缓存大小
    "priority_cleanup": true     // 优先级清理
  }
  
  // 清理性能测试状态
  let performance_test_state = {
    "resources": [],
    "cleanup_times": [],
    "optimization_applied": [],
    "baseline_time": 0.0,
    "optimized_time": 0.0
  }
  
  // 创建测试资源
  let create_test_resources = fn(count) {
    let resources = []
    for i in 0..count {
      resources = resources.push({
        "id": "resource_" + i.to_string(),
        "priority": @lib.random.int(5),  // 0-4优先级
        "size": @lib.random.int(4096) + 1024,  // 1KB-5KB
        "created_at": @lib.time.now(),
        "cleanup_complexity": @lib.random.int(3) + 1  // 1-4复杂度
      })
    }
    resources
  }
  
  // 基准清理（无优化）
  let baseline_cleanup = fn(resources) {
    let start_time = @lib.time.now()
    
    for resource in resources {
      // 模拟清理时间（基于复杂度）
      let cleanup_time = resource["cleanup_complexity"] * 10  // 10-40ms
      @lib.time.sleep(cleanup_time)
    }
    
    let end_time = @lib.time.now()
    end_time - start_time
  }
  
  // 批处理清理
  let batch_cleanup = fn(resources, batch_size) {
    let start_time = @lib.time.now()
    
    for i in 0..resources.length() {
      if i % batch_size == 0 || i == resources.length() - 1 {
        let batch_end = if i + batch_size < resources.length() {
          i + batch_size
        } else {
          resources.length()
        }
        
        let batch = resources.slice(i, batch_end)
        
        // 批处理清理（减少开销）
        for resource in batch {
          let cleanup_time = resource["cleanup_complexity"] * 8  // 批处理优化减少20%时间
          @lib.time.sleep(cleanup_time)
        }
      }
    }
    
    let end_time = @lib.time.now()
    end_time - start_time
  }
  
  // 优先级清理
  let priority_cleanup = fn(resources) {
    let start_time = @lib.time.now()
    
    // 按优先级排序（高优先级先清理）
    let sorted_resources = resources.sort(fn(a, b) { b["priority"] - a["priority"] })
    
    for resource in sorted_resources {
      // 高优先级资源清理更快
      let cleanup_time = resource["cleanup_complexity"] * 8 - resource["priority"] * 2
      @lib.time.sleep(@lib.max(5, cleanup_time))
    }
    
    let end_time = @lib.time.now()
    end_time - start_time
  }
  
  // 延迟清理
  let lazy_cleanup = fn(resources) {
    let start_time = @lib.time.now()
    
    // 模拟延迟清理：只清理高优先级或大资源
    let important_resources = resources.filter_fn(resource => {
      resource["priority"] >= 3 || resource["size"] > 3072
    })
    
    for resource in important_resources {
      let cleanup_time = resource["cleanup_complexity"] * 10
      @lib.time.sleep(cleanup_time)
    }
    
    // 其余资源标记为延迟清理
    let lazy_count = resources.length() - important_resources.length()
    
    let end_time = @lib.time.now()
    (end_time - start_time, lazy_count)
  }
  
  // 创建测试资源
  let test_resources = create_test_resources(100)
  
  // 执行基准清理
  let baseline_time = baseline_cleanup(test_resources)
  performance_test_state["baseline_time"] = baseline_time
  
  // 执行批处理清理
  let batch_time = batch_cleanup(test_resources, optimization_config["batch_size"])
  performance_test_state["cleanup_times"] = performance_test_state["cleanup_times"].push({
    "optimization": "batch_cleanup",
    "time": batch_time,
    "improvement": (baseline_time - batch_time) / baseline_time * 100.0
  })
  performance_test_state["optimization_applied"] = performance_test_state["optimization_applied"].push("batch_cleanup")
  
  // 执行优先级清理
  let priority_time = priority_cleanup(test_resources)
  performance_test_state["cleanup_times"] = performance_test_state["cleanup_times"].push({
    "optimization": "priority_cleanup",
    "time": priority_time,
    "improvement": (baseline_time - priority_time) / baseline_time * 100.0
  })
  performance_test_state["optimization_applied"] = performance_test_state["optimization_applied"].push("priority_cleanup")
  
  // 执行延迟清理
  let (lazy_time, lazy_count) = lazy_cleanup(test_resources)
  performance_test_state["cleanup_times"] = performance_test_state["cleanup_times"].push({
    "optimization": "lazy_cleanup",
    "time": lazy_time,
    "improvement": (baseline_time - lazy_time) / baseline_time * 100.0,
    "resources_deferred": lazy_count
  })
  performance_test_state["optimization_applied"] = performance_test_state["optimization_applied"].push("lazy_cleanup")
  
  // 验证性能优化结果
  assert_true(baseline_time > 0, "基准清理时间应该大于0")
  
  // 验证各种优化策略
  for cleanup_result in performance_test_state["cleanup_times"] {
    assert_true(cleanup_result["time"] > 0, "优化清理时间应该大于0")
    
    if cleanup_result["optimization"] != "lazy_cleanup" {
      assert_true(cleanup_result["improvement"] >= 0, "优化应该有改进或至少不降低性能")
    }
  }
  
  // 验证批处理优化
  let batch_result = performance_test_state["cleanup_times"].filter_fn(r => r["optimization"] == "batch_cleanup")[0]
  assert_true(batch_result["improvement"] > 0, "批处理应该有性能改进")
  
  // 验证优先级优化
  let priority_result = performance_test_state["cleanup_times"].filter_fn(r => r["optimization"] == "priority_cleanup")[0]
  assert_true(priority_result["improvement"] > 0, "优先级清理应该有性能改进")
  
  // 验证延迟清理
  let lazy_result = performance_test_state["cleanup_times"].filter_fn(r => r["optimization"] == "lazy_cleanup")[0]
  assert_true(lazy_result["time"] < baseline_time, "延迟清理应该更快")
  assert_true(lazy_result["resources_deferred"] > 0, "应该有延迟清理的资源")
  
  // 计算最佳优化策略
  let best_optimization = performance_test_state["cleanup_times"].reduce(fn(best, current) {
    if current["improvement"] > best["improvement"] {
      current
    } else {
      best
    }
  }, performance_test_state["cleanup_times"][0])
  
  assert_true(best_optimization["improvement"] > 0, "最佳优化应该有性能改进")
  
  // 验证优化策略应用
  assert_eq(performance_test_state["optimization_applied"].length(), 3, "应该应用3种优化策略")
}