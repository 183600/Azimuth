// Azimuth Memory Leak Protection Test Suite
// 内存泄漏防护测试用例

// 测试1: 资源生命周期管理
test "资源生命周期管理测试" {
  // 资源管理器
  struct ResourceManager {
    active_resources : Array[String]
    max_resources : Int
  }
  
  fn ResourceManager::new(max_resources : Int) -> ResourceManager {
    ResourceManager::{ active_resources: [], max_resources }
  }
  
  fn ResourceManager::allocate_resource(manager : ResourceManager, resource_id : String) -> ResourceManager {
    if manager.active_resources.length() >= manager.max_resources {
      panic("Maximum resource limit reached")
    }
    
    ResourceManager::{ 
      active_resources: manager.active_resources.push(resource_id), 
      max_resources: manager.max_resources 
    }
  }
  
  fn ResourceManager::deallocate_resource(manager : ResourceManager, resource_id : String) -> ResourceManager {
    let filtered_resources = manager.active_resources.filter(fn(id) { id != resource_id })
    ResourceManager::{ active_resources: filtered_resources, max_resources: manager.max_resources }
  }
  
  fn ResourceManager::get_active_count(manager : ResourceManager) -> Int {
    manager.active_resources.length()
  }
  
  fn ResourceManager::is_resource_active(manager : ResourceManager, resource_id : String) -> Bool {
    manager.active_resources.contains(resource_id)
  }
  
  // 自动资源管理
  struct AutoResource[T] {
    id : String
    data : T
    manager : ResourceManager
  }
  
  fn AutoResource::new[T](manager : ResourceManager, id : String, data : T) -> AutoResource[T] {
    let updated_manager = ResourceManager::allocate_resource(manager, id)
    AutoResource::{ id, data, manager: updated_manager }
  }
  
  fn AutoResource::dispose[T](resource : AutoResource[T]) -> ResourceManager {
    ResourceManager::deallocate_resource(resource.manager, resource.id)
  }
  
  // 测试资源管理器
  let manager = ResourceManager::new(10)
  assert_eq(ResourceManager::get_active_count(manager), 0)
  
  let manager1 = ResourceManager::allocate_resource(manager, "resource-1")
  assert_eq(ResourceManager::get_active_count(manager1), 1)
  assert_true(ResourceManager::is_resource_active(manager1, "resource-1"))
  
  let manager2 = ResourceManager::allocate_resource(manager1, "resource-2")
  assert_eq(ResourceManager::get_active_count(manager2), 2)
  
  let manager3 = ResourceManager::deallocate_resource(manager2, "resource-1")
  assert_eq(ResourceManager::get_active_count(manager3), 1)
  assert_false(ResourceManager::is_resource_active(manager3, "resource-1"))
  assert_true(ResourceManager::is_resource_active(manager3, "resource-2"))
  
  // 测试自动资源管理
  let auto_resource = AutoResource::new(manager3, "auto-resource-1", "test-data")
  assert_eq(ResourceManager::get_active_count(auto_resource.manager), 2)
  assert_true(ResourceManager::is_resource_active(auto_resource.manager, "auto-resource-1"))
  
  let restored_manager = AutoResource::dispose(auto_resource)
  assert_eq(ResourceManager::get_active_count(restored_manager), 1)
  assert_false(ResourceManager::is_resource_active(restored_manager, "auto-resource-1"))
  
  // 测试资源限制
  let limited_manager = ResourceManager::new(2)
  let limited1 = ResourceManager::allocate_resource(limited_manager, "limited-1")
  let limited2 = ResourceManager::allocate_resource(limited1, "limited-2")
  
  // 尝试分配超过限制的资源应该失败
  try {
    ResourceManager::allocate_resource(limited2, "limited-3")
    assert_true(false)  // 不应该到达这里
  } catch {
    assert_true(true)   // 应该抛出异常
  }
}

// 测试2: 内存池管理
test "内存池管理测试" {
  // 内存池块
  struct MemoryBlock {
    id : Int
    size : Int
    allocated : Bool
    data : Array[Byte]
  }
  
  // 内存池
  struct MemoryPool {
    blocks : Array[MemoryBlock]
    next_id : Int
    total_size : Int
    allocated_size : Int
  }
  
  fn MemoryPool::new(block_size : Int, block_count : Int) -> MemoryPool {
    let mut blocks = []
    for i in 0..block_count {
      let block = MemoryBlock::{
        id: i,
        size: block_size,
        allocated: false,
        data: Array::filled(block_size, 0)
      }
      blocks = blocks.push(block)
    }
    
    MemoryPool::{ 
      blocks, 
      next_id: block_count, 
      total_size: block_size * block_count, 
      allocated_size: 0 
    }
  }
  
  fn MemoryPool::allocate(pool : MemoryPool, size : Int) -> (MemoryPool, Option[Int]) {
    // 查找合适大小的未分配块
    for i in 0..pool.blocks.length() {
      let block = pool.blocks[i]
      if !block.allocated && block.size >= size {
        // 分配这个块
        let allocated_block = MemoryBlock::{
          id: block.id,
          size: size,
          allocated: true,
          data: block.data
        }
        
        let mut updated_blocks = pool.blocks
        updated_blocks[i] = allocated_block
        
        let updated_pool = MemoryPool::{
          blocks: updated_blocks,
          next_id: pool.next_id,
          total_size: pool.total_size,
          allocated_size: pool.allocated_size + size
        }
        
        return (updated_pool, Some(block.id))
      }
    }
    
    // 没有合适的块
    (pool, None)
  }
  
  fn MemoryPool::deallocate(pool : MemoryPool, block_id : Int) -> MemoryPool {
    for i in 0..pool.blocks.length() {
      let block = pool.blocks[i]
      if block.id == block_id && block.allocated {
        // 释放这个块
        let deallocated_block = MemoryBlock::{
          id: block.id,
          size: block.size,
          allocated: false,
          data: Array::filled(block.size, 0)  // 清零数据
        }
        
        let mut updated_blocks = pool.blocks
        updated_blocks[i] = deallocated_block
        
        return MemoryPool::{
          blocks: updated_blocks,
          next_id: pool.next_id,
          total_size: pool.total_size,
          allocated_size: pool.allocated_size - block.size
        }
      }
    }
    
    // 没有找到指定的块
    pool
  }
  
  fn MemoryPool::get_usage_ratio(pool : MemoryPool) -> Double {
    if pool.total_size == 0 {
      0.0
    } else {
      pool.allocated_size.to_double() / pool.total_size.to_double()
    }
  }
  
  fn MemoryPool::get_fragmentation_ratio(pool : MemoryPool) -> Double {
    let mut allocated_blocks = 0
    let mut total_blocks = 0
    
    for block in pool.blocks {
      total_blocks = total_blocks + 1
      if block.allocated {
        allocated_blocks = allocated_blocks + 1
      }
    }
    
    if total_blocks == 0 {
      0.0
    } else {
      // 简单的碎片化度量：已分配块的比例
      allocated_blocks.to_double() / total_blocks.to_double()
    }
  }
  
  // 测试内存池
  let pool = MemoryPool::new(1024, 10)  // 10个块，每个1024字节
  assert_eq(pool.total_size, 10240)
  assert_eq(pool.allocated_size, 0)
  assert_eq(MemoryPool::get_usage_ratio(pool), 0.0)
  
  // 分配块
  let (pool1, block1_id) = MemoryPool::allocate(pool, 512)
  assert_true(block1_id.is_some())
  assert_eq(pool1.allocated_size, 512)
  assert_eq(MemoryPool::get_usage_ratio(pool1), 512.0 / 10240.0)
  
  let (pool2, block2_id) = MemoryPool::allocate(pool1, 256)
  assert_true(block2_id.is_some())
  assert_eq(pool2.allocated_size, 768)
  
  let (pool3, block3_id) = MemoryPool::allocate(pool2, 2048)  // 超过块大小，应该失败
  assert_true(block3_id.is_none())
  assert_eq(pool3.allocated_size, 768)  // 不应该改变
  
  // 释放块
  let pool4 = MemoryPool::deallocate(pool3, block1_id.unwrap())
  assert_eq(pool4.allocated_size, 256)
  
  // 重新分配应该成功
  let (pool5, block4_id) = MemoryPool::allocate(pool4, 1024)
  assert_true(block4_id.is_some())
  assert_eq(pool5.allocated_size, 1280)
  
  // 测试碎片化
  let fragmentation = MemoryPool::get_fragmentation_ratio(pool5)
  assert_true(fragmentation >= 0.0 && fragmentation <= 1.0)
}

// 测试3: 循环引用检测
test "循环引用检测测试" {
  // 引用计数节点
  struct RefCountNode {
    id : String
    ref_count : Int
    references : Array[String]  // 引用的节点ID
  }
  
  // 引用计数管理器
  struct RefCountManager {
    nodes : Array[RefCountNode]
  }
  
  fn RefCountManager::new() -> RefCountManager {
    RefCountManager::{ nodes: [] }
  }
  
  fn RefCountManager::create_node(manager : RefCountManager, id : String) -> RefCountManager {
    let node = RefCountNode::{ id, ref_count: 1, references: [] }
    RefCountManager::{ nodes: manager.nodes.push(node) }
  }
  
  fn RefCountManager::add_reference(manager : RefCountManager, from_id : String, to_id : String) -> RefCountManager {
    let mut updated_nodes = manager.nodes
    
    // 增加目标节点的引用计数
    for i in 0..updated_nodes.length() {
      if updated_nodes[i].id == to_id {
        let node = updated_nodes[i]
        updated_nodes[i] = RefCountNode::{
          id: node.id,
          ref_count: node.ref_count + 1,
          references: node.references
        }
        break
      }
    }
    
    // 添加引用关系
    for i in 0..updated_nodes.length() {
      if updated_nodes[i].id == from_id {
        let node = updated_nodes[i]
        updated_nodes[i] = RefCountNode::{
          id: node.id,
          ref_count: node.ref_count,
          references: node.references.push(to_id)
        }
        break
      }
    }
    
    updated_nodes
  }
  
  fn RefCountManager::remove_reference(manager : RefCountManager, from_id : String, to_id : String) -> RefCountManager {
    let mut updated_nodes = manager.nodes
    
    // 减少目标节点的引用计数
    for i in 0..updated_nodes.length() {
      if updated_nodes[i].id == to_id {
        let node = updated_nodes[i]
        updated_nodes[i] = RefCountNode::{
          id: node.id,
          ref_count: node.ref_count - 1,
          references: node.references
        }
        break
      }
    }
    
    // 移除引用关系
    for i in 0..updated_nodes.length() {
      if updated_nodes[i].id == from_id {
        let node = updated_nodes[i]
        updated_nodes[i] = RefCountNode::{
          id: node.id,
          ref_count: node.ref_count,
          references: node.references.filter(fn(id) { id != to_id })
        }
        break
      }
    }
    
    updated_nodes
  }
  
  fn RefCountManager::detect_cycles(manager : RefCountManager) -> Array[Array[String]] {
    let mut cycles = []
    
    for node in manager.nodes {
      let mut visited = []
      let mut path = []
      let mut current = node.id
      
      // 深度优先搜索检测循环
      fn dfs(manager : RefCountManager, current : String, visited : Array[String], path : Array[String]) -> Option[Array[String]] {
        if visited.contains(current) {
          // 找到循环，提取循环路径
          let cycle_start = path.index_of(current).unwrap()
          return Some(path.slice(cycle_start, path.length()))
        }
        
        // 获取当前节点
        let current_node = manager.nodes.find(fn(n) { n.id == current })
        match current_node {
          None => None
          Some(node) => {
            let new_visited = visited.push(current)
            let new_path = path.push(current)
            
            for ref_id in node.references {
              match dfs(manager, ref_id, new_visited, new_path) {
                Some(cycle) => return Some(cycle)
                None => ()
              }
            }
            
            None
          }
        }
      }
      
      match dfs(manager, current, visited, path) {
        Some(cycle) => {
          if !cycles.contains(cycle) {
            cycles = cycles.push(cycle)
          }
        }
        None => ()
      }
    }
    
    cycles
  }
  
  fn RefCountManager::collect_garbage(manager : RefCountManager) -> RefCountManager {
    let mut collected_nodes = []
    
    for node in manager.nodes {
      if node.ref_count > 0 {
        collected_nodes = collected_nodes.push(node)
      }
    }
    
    RefCountManager::{ nodes: collected_nodes }
  }
  
  // 测试引用计数管理
  let manager = RefCountManager::new()
  let manager1 = RefCountManager::create_node(manager, "node-a")
  let manager2 = RefCountManager::create_node(manager1, "node-b")
  let manager3 = RefCountManager::create_node(manager2, "node-c")
  
  // 创建引用关系：a -> b -> c
  let manager4 = RefCountManager::add_reference(manager3, "node-a", "node-b")
  let manager5 = RefCountManager::add_reference(manager4, "node-b", "node-c")
  
  // 验证引用计数
  let node_a = manager5.nodes.find(fn(n) { n.id == "node-a" }).unwrap()
  let node_b = manager5.nodes.find(fn(n) { n.id == "node-b" }).unwrap()
  let node_c = manager5.nodes.find(fn(n) { n.id == "node-c" }).unwrap()
  
  assert_eq(node_a.ref_count, 1)  // 初始引用
  assert_eq(node_b.ref_count, 2)  // 初始引用 + a的引用
  assert_eq(node_c.ref_count, 2)  // 初始引用 + b的引用
  
  // 测试循环检测（无循环）
  let cycles1 = RefCountManager::detect_cycles(manager5)
  assert_eq(cycles1.length(), 0)
  
  // 创建循环引用：c -> a
  let manager6 = RefCountManager::add_reference(manager5, "node-c", "node-a")
  
  // 验证循环引用的引用计数
  let node_a_cycle = manager6.nodes.find(fn(n) { n.id == "node-a" }).unwrap()
  let node_c_cycle = manager6.nodes.find(fn(n) { n.id == "node-c" }).unwrap()
  
  assert_eq(node_a_cycle.ref_count, 2)  // 初始引用 + c的引用
  assert_eq(node_c_cycle.ref_count, 2)  // 保持不变
  
  // 测试循环检测（有循环）
  let cycles2 = RefCountManager::detect_cycles(manager6)
  assert_eq(cycles2.length(), 1)
  assert_true(cycles2[0].contains("node-a"))
  assert_true(cycles2[0].contains("node-b"))
  assert_true(cycles2[0].contains("node-c"))
  
  // 测试垃圾回收（循环引用不会被回收）
  let manager7 = RefCountManager::collect_garbage(manager6)
  assert_eq(manager7.nodes.length(), 3)  // 所有节点都应该保留，因为有循环引用
  
  // 移除循环引用
  let manager8 = RefCountManager::remove_reference(manager7, "node-c", "node-a")
  
  // 验证引用计数减少
  let node_a_no_cycle = manager8.nodes.find(fn(n) { n.id == "node-a" }).unwrap()
  assert_eq(node_a_no_cycle.ref_count, 1)
  
  // 测试循环检测（无循环）
  let cycles3 = RefCountManager::detect_cycles(manager8)
  assert_eq(cycles3.length(), 0)
}

// 测试4: 内存使用监控
test "内存使用监控测试" {
  // 内存使用统计
  struct MemoryStats {
    total_allocated : Int
    peak_usage : Int
    current_usage : Int
    allocation_count : Int
    deallocation_count : Int
    leak_count : Int
  }
  
  fn MemoryStats::new() -> MemoryStats {
    MemoryStats::{
      total_allocated: 0,
      peak_usage: 0,
      current_usage: 0,
      allocation_count: 0,
      deallocation_count: 0,
      leak_count: 0
    }
  }
  
  fn MemoryStats::allocate(stats : MemoryStats, size : Int) -> MemoryStats {
    let new_current = stats.current_usage + size
    let new_peak = if new_current > stats.peak_usage { new_current } else { stats.peak_usage }
    
    MemoryStats::{
      total_allocated: stats.total_allocated + size,
      peak_usage: new_peak,
      current_usage: new_current,
      allocation_count: stats.allocation_count + 1,
      deallocation_count: stats.deallocation_count,
      leak_count: stats.leak_count
    }
  }
  
  fn MemoryStats::deallocate(stats : MemoryStats, size : Int) -> MemoryStats {
    MemoryStats::{
      total_allocated: stats.total_allocated,
      peak_usage: stats.peak_usage,
      current_usage: stats.current_usage - size,
      allocation_count: stats.allocation_count,
      deallocation_count: stats.deallocation_count + 1,
      leak_count: stats.leak_count
    }
  }
  
  fn MemoryStats::detect_leak(stats : MemoryStats) -> MemoryStats {
    if stats.current_usage > 0 {
      MemoryStats::{ leak_count: stats.leak_count + 1, ..stats }
    } else {
      stats
    }
  }
  
  fn MemoryStats::get_leak_ratio(stats : MemoryStats) -> Double {
    if stats.total_allocated == 0 {
      0.0
    } else {
      stats.current_usage.to_double() / stats.total_allocated.to_double()
    }
  }
  
  // 内存监控器
  struct MemoryMonitor {
    stats : MemoryStats
    threshold : Double  // 泄漏阈值
  }
  
  fn MemoryMonitor::new(threshold : Double) -> MemoryMonitor {
    MemoryMonitor::{ stats: MemoryStats::new(), threshold }
  }
  
  fn MemoryMonitor::allocate(monitor : MemoryMonitor, size : Int) -> MemoryMonitor {
    let new_stats = MemoryStats::allocate(monitor.stats, size)
    MemoryMonitor::{ stats: new_stats, threshold: monitor.threshold }
  }
  
  fn MemoryMonitor::deallocate(monitor : MemoryMonitor, size : Int) -> MemoryMonitor {
    let new_stats = MemoryStats::deallocate(monitor.stats, size)
    MemoryMonitor::{ stats: new_stats, threshold: monitor.threshold }
  }
  
  fn MemoryMonitor::check_leak(monitor : MemoryMonitor) -> (MemoryMonitor, Bool) {
    let leak_ratio = MemoryStats::get_leak_ratio(monitor.stats)
    let has_leak = leak_ratio > monitor.threshold
    
    let new_stats = if has_leak {
      MemoryStats::detect_leak(monitor.stats)
    } else {
      monitor.stats
    }
    
    (MemoryMonitor::{ stats: new_stats, threshold: monitor.threshold }, has_leak)
  }
  
  // 测试内存监控
  let monitor = MemoryMonitor::new(0.1)  // 10%泄漏阈值
  assert_eq(monitor.stats.current_usage, 0)
  assert_eq(monitor.stats.allocation_count, 0)
  
  // 分配内存
  let monitor1 = MemoryMonitor::allocate(monitor, 1024)
  assert_eq(monitor1.stats.current_usage, 1024)
  assert_eq(monitor1.stats.allocation_count, 1)
  assert_eq(monitor1.stats.peak_usage, 1024)
  
  let monitor2 = MemoryMonitor::allocate(monitor1, 2048)
  assert_eq(monitor2.stats.current_usage, 3072)
  assert_eq(monitor2.stats.allocation_count, 2)
  assert_eq(monitor2.stats.peak_usage, 3072)
  
  // 释放部分内存
  let monitor3 = MemoryMonitor::deallocate(monitor2, 1024)
  assert_eq(monitor3.stats.current_usage, 2048)
  assert_eq(monitor3.stats.deallocation_count, 1)
  assert_eq(monitor3.stats.peak_usage, 3072)  // 峰值保持不变
  
  // 检查泄漏（当前使用/总分配 = 2048/3072 ≈ 0.67 > 0.1）
  let (monitor4, has_leak) = MemoryMonitor::check_leak(monitor3)
  assert_true(has_leak)
  assert_eq(monitor4.stats.leak_count, 1)
  
  // 释放所有内存
  let monitor5 = MemoryMonitor::deallocate(monitor4, 2048)
  assert_eq(monitor5.stats.current_usage, 0)
  assert_eq(monitor5.stats.deallocation_count, 2)
  
  // 再次检查泄漏（当前使用/总分配 = 0/3072 = 0 < 0.1）
  let (monitor6, has_leak2) = MemoryMonitor::check_leak(monitor5)
  assert_false(has_leak2)
  assert_eq(monitor6.stats.leak_count, 1)  // 泄漏计数不会减少
  
  // 测试泄漏比率
  let leak_ratio = MemoryStats::get_leak_ratio(monitor6.stats)
  assert_eq(leak_ratio, 0.0)
  
  // 创建新的监控器进行更多测试
  let monitor7 = MemoryMonitor::new(0.5)  // 50%泄漏阈值
  let monitor8 = MemoryMonitor::allocate(monitor7, 1000)
  let monitor9 = MemoryMonitor::allocate(monitor8, 2000)
  let monitor10 = MemoryMonitor::deallocate(monitor9, 1500)
  
  // 检查泄漏（当前使用/总分配 = 1500/3000 = 0.5，等于阈值）
  let (_, has_leak3) = MemoryMonitor::check_leak(monitor10)
  assert_false(has_leak3)  // 等于阈值不算泄漏
  
  // 再释放一点
  let monitor11 = MemoryMonitor::deallocate(monitor10, 100)
  
  // 检查泄漏（当前使用/总分配 = 1400/3000 ≈ 0.47 < 0.5）
  let (_, has_leak4) = MemoryMonitor::check_leak(monitor11)
  assert_false(has_leak4)
}

// 测试5: 自动内存清理
test "自动内存清理测试" {
  // 清理策略
  enum CleanupStrategy {
    Immediate
    Threshold(Double)  // 使用率阈值
    Periodic(Int)      // 清理间隔（操作次数）
    Manual
  }
  
  // 清理项
  struct CleanupItem {
    id : String
    size : Int
    last_accessed : Int64
    priority : Int  // 优先级，数值越小优先级越高
  }
  
  // 自动清理器
  struct AutoCleaner {
    items : Array[CleanupItem]
    strategy : CleanupStrategy
    operation_count : Int
    total_size : Int
    max_size : Int
  }
  
  fn AutoCleaner::new(strategy : CleanupStrategy, max_size : Int) -> AutoCleaner {
    AutoCleaner::{
      items: [],
      strategy,
      operation_count: 0,
      total_size: 0,
      max_size
    }
  }
  
  fn AutoCleaner::add_item(cleaner : AutoCleaner, id : String, size : Int, priority : Int) -> AutoCleaner {
    let current_time = 1640995200000000000L  // 模拟当前时间
    let item = CleanupItem::{ id, size, last_accessed: current_time, priority }
    
    let new_cleaner = AutoCleaner::{
      items: cleaner.items.push(item),
      strategy: cleaner.strategy,
      operation_count: cleaner.operation_count + 1,
      total_size: cleaner.total_size + size,
      max_size: cleaner.max_size
    }
    
    // 根据策略决定是否立即清理
    match cleaner.strategy {
      CleanupStrategy::Immediate => AutoCleaner::cleanup(new_cleaner)
      CleanupStrategy::Threshold(threshold) => {
        if new_cleaner.total_size.to_double() / new_cleaner.max_size.to_double() > threshold {
          AutoCleaner::cleanup(new_cleaner)
        } else {
          new_cleaner
        }
      }
      CleanupStrategy::Periodic(interval) => {
        if new_cleaner.operation_count % interval == 0 {
          AutoCleaner::cleanup(new_cleaner)
        } else {
          new_cleaner
        }
      }
      CleanupStrategy::Manual => new_cleaner
    }
  }
  
  fn AutoCleaner::cleanup(cleaner : AutoCleaner) -> AutoCleaner {
    // 按优先级排序（优先级高的先清理）
    let sorted_items = cleaner.items.sort_by(fn(a, b) { 
      if a.priority < b.priority { 1 } 
      else if a.priority > b.priority { -1 } 
      else { 0 } 
    })
    
    let mut remaining_items = []
    let mut current_size = 0
    let target_size = cleaner.max_size * 7 / 10  // 清理到70%
    
    // 保留高优先级的项目，直到达到目标大小
    for item in sorted_items {
      if current_size + item.size <= target_size {
        remaining_items = remaining_items.push(item)
        current_size = current_size + item.size
      }
    }
    
    AutoCleaner::{
      items: remaining_items,
      strategy: cleaner.strategy,
      operation_count: cleaner.operation_count,
      total_size: current_size,
      max_size: cleaner.max_size
    }
  }
  
  fn AutoCleaner::manual_cleanup(cleaner : AutoCleaner) -> AutoCleaner {
    AutoCleaner::cleanup(cleaner)
  }
  
  fn AutoCleaner::get_usage_ratio(cleaner : AutoCleaner) -> Double {
    cleaner.total_size.to_double() / cleaner.max_size.to_double()
  }
  
  // 测试立即清理策略
  let immediate_cleaner = AutoCleaner::new(CleanupStrategy::Immediate, 10000)
  let cleaner1 = AutoCleaner::add_item(immediate_cleaner, "item-1", 3000, 1)
  
  // 立即清理应该保留高优先级的项目
  assert_true(cleaner1.items.length() <= 1)
  assert_true(cleaner1.total_size <= 7000)  // 应该清理到70%以下
  
  // 测试阈值清理策略
  let threshold_cleaner = AutoCleaner::new(CleanupStrategy::Threshold(0.8), 10000)
  let cleaner2 = AutoCleaner::add_item(threshold_cleaner, "item-1", 3000, 1);
  let cleaner3 = AutoCleaner::add_item(cleaner2, "item-2", 3000, 2);
  let cleaner4 = AutoCleaner::add_item(cleaner3, "item-3", 3000, 3)
  
  // 使用率 = 9000/10000 = 0.9 > 0.8，应该触发清理
  assert_true(cleaner4.total_size <= 7000)
  
  // 测试周期清理策略
  let periodic_cleaner = AutoCleaner::new(CleanupStrategy::Periodic(3), 10000)
  let cleaner5 = AutoCleaner::add_item(periodic_cleaner, "item-1", 2000, 1);
  let cleaner6 = AutoCleaner::add_item(cleaner5, "item-2", 2000, 2);
  let cleaner7 = AutoCleaner::add_item(cleaner6, "item-3", 2000, 3)  // 第3个操作，应该触发清理
  
  assert_true(cleaner7.total_size <= 7000)
  
  // 测试手动清理策略
  let manual_cleaner = AutoCleaner::new(CleanupStrategy::Manual, 10000)
  let cleaner8 = AutoCleaner::add_item(manual_cleaner, "item-1", 4000, 1);
  let cleaner9 = AutoCleaner::add_item(cleaner8, "item-2", 4000, 2);
  let cleaner10 = AutoCleaner::add_item(cleaner9, "item-3", 4000, 3)
  
  // 手动策略不会自动清理
  assert_eq(cleaner10.total_size, 12000)
  assert_eq(cleaner10.items.length(), 3)
  
  // 手动触发清理
  let cleaner11 = AutoCleaner::manual_cleanup(cleaner10)
  assert_true(cleaner11.total_size <= 7000)
  assert_true(cleaner11.items.length() <= 2)
  
  // 验证清理保留了高优先级的项目
  let highest_priority = cleaner11.items.map(fn(item) { item.priority }).min()
  assert_eq(highest_priority, 1)  // 应该保留优先级为1的项目
}