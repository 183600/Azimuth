// 资源限制测试用例
test "memory_usage_limits" {
  // 测试内存使用限制
  
  // 1. 定义内存状态
  enum MemoryState {
    Normal
    Warning
    Critical
    Exhausted
  }
  
  // 2. 定义内存监控器
  struct MemoryMonitor {
    total_memory_mb : Int
    used_memory_mb : Int
    available_memory_mb : Int
    threshold_warning_percent : Double
    threshold_critical_percent : Double
    current_state : MemoryState
    allocation_history : Array[AllocationRecord]
  }
  
  // 3. 定义分配记录
  struct AllocationRecord {
    timestamp : Int64
    size_mb : Int
    allocation_type : String
    success : Bool
    rejected_reason : String?
  }
  
  // 4. 定义内存分配器
  struct MemoryAllocator {
    monitor : MemoryMonitor
    max_allocation_mb : Int
    enable_rejection : Bool
    enable_gc : Bool
    gc_threshold_percent : Double
  }
  
  // 5. 创建内存监控器
  let memory_monitor = MemoryMonitor::{
    total_memory_mb: 8192,  // 8GB
    used_memory_mb: 4096,   // 4GB已使用
    available_memory_mb: 4096,  // 4GB可用
    threshold_warning_percent: 70.0,  // 70%警告
    threshold_critical_percent: 90.0, // 90%严重
    current_state: Normal,
    allocation_history: []
  }
  
  // 6. 创建内存分配器
  let memory_allocator = MemoryAllocator::{
    monitor: memory_monitor,
    max_allocation_mb: 1024,  // 单次最大分配1GB
    enable_rejection: true,
    enable_gc: true,
    gc_threshold_percent: 80.0  // 80%时触发GC
  }
  
  // 7. 实现内存分配函数
  fn allocate_memory(allocator : MemoryAllocator, size_mb : Int, allocation_type : String) -> (MemoryAllocator, Bool, String?) {
    let mut updated_allocator = allocator
    let mut success = false
    let mut rejection_reason = None
    
    // 检查单次分配限制
    if size_mb > allocator.max_allocation_mb {
      rejection_reason = Some("Allocation size exceeds maximum limit")
      success = false
    } else if size_mb > allocator.monitor.available_memory_mb {
      rejection_reason = Some("Insufficient available memory")
      success = false
    } else {
      // 模拟内存分配
      updated_allocator.monitor.used_memory_mb = updated_allocator.monitor.used_memory_mb + size_mb
      updated_allocator.monitor.available_memory_mb = updated_allocator.monitor.available_memory_mb - size_mb
      success = true
      
      // 更新内存状态
      let usage_percent = (updated_allocator.monitor.used_memory_mb.to_double() / updated_allocator.monitor.total_memory_mb.to_double()) * 100.0
      
      if usage_percent >= updated_allocator.monitor.threshold_critical_percent {
        updated_allocator.monitor.current_state = Critical
      } else if usage_percent >= updated_allocator.monitor.threshold_warning_percent {
        updated_allocator.monitor.current_state = Warning
      } else {
        updated_allocator.monitor.current_state = Normal
      }
      
      // 检查是否需要触发GC
      if allocator.enable_gc and usage_percent >= allocator.gc_threshold_percent {
        // 模拟垃圾回收
        let freed_memory = (updated_allocator.monitor.used_memory_mb * 10) / 100  // 释放10%
        updated_allocator.monitor.used_memory_mb = updated_allocator.monitor.used_memory_mb - freed_memory
        updated_allocator.monitor.available_memory_mb = updated_allocator.monitor.available_memory_mb + freed_memory
      }
    }
    
    // 记录分配历史
    let record = AllocationRecord::{
      timestamp: 1640995200000000000L,
      size_mb: size_mb,
      allocation_type: allocation_type,
      success: success,
      rejected_reason: rejection_reason
    }
    
    updated_allocator.monitor.allocation_history.push(record)
    
    (updated_allocator, success, rejection_reason)
  }
  
  // 8. 测试内存分配场景
  
  // 场景1: 正常内存分配
  let (allocator1, success1, reason1) = allocate_memory(memory_allocator, 512, "telemetry_buffer")
  assert_eq(success1, true)
  assert_eq(reason1, None)
  assert_eq(allocator1.monitor.used_memory_mb, 4608)  // 4096 + 512
  assert_eq(allocator1.monitor.current_state, Normal)
  
  // 场景2: 大内存分配
  let (allocator2, success2, reason2) = allocate_memory(allocator1, 2048, "large_batch_processing")
  assert_eq(success2, true)
  assert_eq(reason2, None)
  assert_eq(allocator2.monitor.used_memory_mb, 6656)  // 4608 + 2048
  assert_eq(allocator2.monitor.current_state, Warning)  // 超过70%
  
  // 场景3: 超出单次分配限制
  let (allocator3, success3, reason3) = allocate_memory(allocator2, 2048, "oversized_allocation")
  assert_eq(success3, false)
  assert_eq(reason3, Some("Allocation size exceeds maximum limit"))
  
  // 场景4: 内存不足
  let (allocator4, success4, reason4) = allocate_memory(allocator3, 2000, "memory_intensive_operation")
  assert_eq(success4, false)
  assert_eq(reason4, Some("Insufficient available memory"))
  
  // 场景5: 触发垃圾回收
  let (allocator5, success5, reason5) = allocate_memory(allocator4, 1000, "gc_trigger_allocation")
  assert_eq(success5, true)
  // 由于GC触发，实际使用内存会减少
  assert_eq(allocator5.monitor.current_state, Warning or allocator5.monitor.current_state == Normal)
}

test "cpu_usage_throttling" {
  // 测试CPU使用限制和节流
  
  // 1. 定义CPU状态
  enum CPUState {
    Idle
    Normal
    Busy
    Overloaded
  }
  
  // 2. 定义CPU监控器
  struct CPUMonitor {
    core_count : Int
    current_usage_percent : Double
    average_usage_1min : Double
    average_usage_5min : Double
    threshold_warning_percent : Double
    threshold_throttle_percent : Double
    current_state : CPUState
    usage_history : Array[CPUUsageRecord]
  }
  
  // 3. 定义CPU使用记录
  struct CPUUsageRecord {
    timestamp : Int64
    usage_percent : Double
    process_count : Int
    throttled : Bool
  }
  
  // 4. 定义任务调度器
  struct TaskScheduler {
    monitor : CPUMonitor
    max_concurrent_tasks : Int
    current_tasks : Int
    throttling_enabled : Bool
    adaptive_throttling : Bool
  }
  
  // 5. 定义任务
  struct Task {
    id : String
    priority : Int
    cpu_intensity : Double  // 0.0 - 1.0
    duration_ms : Int
    created_at : Int64
    started_at : Int64?
    completed_at : Int64?
  }
  
  // 6. 创建CPU监控器
  let cpu_monitor = CPUMonitor::{
    core_count: 8,
    current_usage_percent: 45.5,
    average_usage_1min: 50.2,
    average_usage_5min: 48.7,
    threshold_warning_percent: 70.0,
    threshold_throttle_percent: 85.0,
    current_state: Normal,
    usage_history: []
  }
  
  // 7. 创建任务调度器
  let task_scheduler = TaskScheduler::{
    monitor: cpu_monitor,
    max_concurrent_tasks: 16,
    current_tasks: 8,
    throttling_enabled: true,
    adaptive_throttling: true
  }
  
  // 8. 实现任务调度函数
  fn schedule_task(scheduler : TaskScheduler, task : Task) -> (TaskScheduler, Bool, String?) {
    let mut updated_scheduler = scheduler
    let mut scheduled = false
    let mut rejection_reason = None
    
    // 检查并发任务限制
    if scheduler.current_tasks >= scheduler.max_concurrent_tasks {
      rejection_reason = Some("Maximum concurrent tasks reached")
      scheduled = false
    } else {
      // 检查CPU使用率
      let projected_usage = scheduler.monitor.current_usage_percent + (task.cpu_intensity * 100.0)
      
      if projected_usage >= scheduler.monitor.threshold_throttle_percent {
        if scheduler.throttling_enabled {
          // 启用节流，拒绝高CPU强度任务
          if task.cpu_intensity > 0.5 {
            rejection_reason = Some("Task throttled due to high CPU intensity")
            scheduled = false
          } else {
            // 允许低CPU强度任务
            scheduled = true
            updated_scheduler.current_tasks = updated_scheduler.current_tasks + 1
          }
        } else {
          scheduled = true
          updated_scheduler.current_tasks = updated_scheduler.current_tasks + 1
        }
      } else {
        scheduled = true
        updated_scheduler.current_tasks = updated_scheduler.current_tasks + 1
      }
    }
    
    // 更新CPU状态
    let usage_percent = updated_scheduler.monitor.current_usage_percent
    if usage_percent >= updated_scheduler.monitor.threshold_throttle_percent {
      updated_scheduler.monitor.current_state = Overloaded
    } else if usage_percent >= updated_scheduler.monitor.threshold_warning_percent {
      updated_scheduler.monitor.current_state = Busy
    } else if usage_percent > 10.0 {
      updated_scheduler.monitor.current_state = Normal
    } else {
      updated_scheduler.monitor.current_state = Idle
    }
    
    // 记录CPU使用情况
    let record = CPUUsageRecord::{
      timestamp: 1640995200000000000L,
      usage_percent: usage_percent,
      process_count: updated_scheduler.current_tasks,
      throttled: not scheduled
    }
    
    updated_scheduler.monitor.usage_history.push(record)
    
    (updated_scheduler, scheduled, rejection_reason)
  }
  
  // 9. 实现任务完成函数
  fn complete_task(scheduler : TaskScheduler, task_id : String, actual_cpu_usage : Double) -> TaskScheduler {
    let mut updated_scheduler = scheduler
    
    // 减少当前任务数
    if updated_scheduler.current_tasks > 0 {
      updated_scheduler.current_tasks = updated_scheduler.current_tasks - 1
    }
    
    // 更新CPU使用率
    updated_scheduler.monitor.current_usage_percent = actual_cpu_usage
    
    // 更新CPU状态
    let usage_percent = actual_cpu_usage
    if usage_percent >= updated_scheduler.monitor.threshold_throttle_percent {
      updated_scheduler.monitor.current_state = Overloaded
    } else if usage_percent >= updated_scheduler.monitor.threshold_warning_percent {
      updated_scheduler.monitor.current_state = Busy
    } else if usage_percent > 10.0 {
      updated_scheduler.monitor.current_state = Normal
    } else {
      updated_scheduler.monitor.current_state = Idle
    }
    
    updated_scheduler
  }
  
  // 10. 测试CPU节流场景
  
  // 创建测试任务
  let low_cpu_task = Task::{
    id: "task_001",
    priority: 1,
    cpu_intensity: 0.2,  // 低CPU强度
    duration_ms: 1000,
    created_at: 1640995200000000000L,
    started_at: None,
    completed_at: None
  }
  
  let medium_cpu_task = Task::{
    id: "task_002",
    priority: 2,
    cpu_intensity: 0.6,  // 中等CPU强度
    duration_ms: 2000,
    created_at: 1640995200000000000L,
    started_at: None,
    completed_at: None
  }
  
  let high_cpu_task = Task::{
    id: "task_003",
    priority: 3,
    cpu_intensity: 0.9,  // 高CPU强度
    duration_ms: 5000,
    created_at: 1640995200000000000L,
    started_at: None,
    completed_at: None
  }
  
  // 场景1: 调度低CPU强度任务
  let (scheduler1, scheduled1, reason1) = schedule_task(task_scheduler, low_cpu_task)
  assert_eq(scheduled1, true)
  assert_eq(reason1, None)
  assert_eq(scheduler1.current_tasks, 9)
  
  // 场景2: 调度中等CPU强度任务
  let (scheduler2, scheduled2, reason2) = schedule_task(scheduler1, medium_cpu_task)
  assert_eq(scheduled2, true)
  assert_eq(reason2, None)
  assert_eq(scheduler2.current_tasks, 10)
  
  // 场景3: 模拟高CPU使用情况
  let mut high_load_scheduler = scheduler2
  high_load_scheduler.monitor.current_usage_percent = 90.0  // 90% CPU使用率
  high_load_scheduler.monitor.current_state = Overloaded
  
  // 场景4: 在高负载下调度高CPU强度任务（应该被拒绝）
  let (scheduler3, scheduled3, reason3) = schedule_task(high_load_scheduler, high_cpu_task)
  assert_eq(scheduled3, false)
  assert_eq(reason3, Some("Task throttled due to high CPU intensity"))
  
  // 场景5: 在高负载下调度低CPU强度任务（应该被允许）
  let (scheduler4, scheduled4, reason4) = schedule_task(scheduler3, low_cpu_task)
  assert_eq(scheduled4, true)
  assert_eq(reason4, None)
  
  // 场景6: 任务完成，CPU使用率下降
  let scheduler5 = complete_task(scheduler4, "task_001", 65.0)
  assert_eq(scheduler5.current_tasks, 9)
  assert_eq(scheduler5.monitor.current_usage_percent, 65.0)
  assert_eq(scheduler5.monitor.current_state, Normal)
}

test "disk_space_management" {
  // 测试磁盘空间管理
  
  // 1. 定义磁盘状态
  enum DiskState {
    Healthy
    Warning
    Critical
    Full
  }
  
  // 2. 定义磁盘监控器
  struct DiskMonitor {
    total_space_gb : Int
    used_space_gb : Int
    available_space_gb : Int
    threshold_warning_percent : Double
    threshold_critical_percent : Double
    current_state : DiskState
    cleanup_policy : CleanupPolicy
  }
  
  // 3. 定义清理策略
  struct CleanupPolicy {
    auto_cleanup_enabled : Bool
    retention_days : Int
    cleanup_order : Array[String]  // 按优先级清理的数据类型
    min_free_space_gb : Int
  }
  
  // 4. 定义数据文件
  struct DataFile {
    id : String
    file_type : String
    size_gb : Double
    created_at : Int64
    last_accessed : Int64
    access_count : Int
    priority : Int
  }
  
  // 5. 定义存储管理器
  struct StorageManager {
    monitor : DiskMonitor
    files : Array[DataFile]
    write_cache_enabled : Bool
    compression_enabled : Bool
  }
  
  // 6. 创建磁盘监控器
  let cleanup_policy = CleanupPolicy::{
    auto_cleanup_enabled: true,
    retention_days: 30,
    cleanup_order: ["logs", "metrics", "traces"],
    min_free_space_gb: 10
  }
  
  let disk_monitor = DiskMonitor::{
    total_space_gb: 1000,
    used_space_gb: 800,
    available_space_gb: 200,
    threshold_warning_percent: 80.0,
    threshold_critical_percent: 95.0,
    current_state: Warning,
    cleanup_policy: cleanup_policy
  }
  
  // 7. 创建测试文件
  let test_files = [
    DataFile::{
      id: "file_001",
      file_type: "traces",
      size_gb: 50.5,
      created_at: 1640995200000000000L,
      last_accessed: 1640995200000000000L,
      access_count: 100,
      priority: 1
    },
    DataFile::{
      id: "file_002",
      file_type: "metrics",
      size_gb: 25.3,
      created_at: 1640995200000000000L,
      last_accessed: 1640995200000000000L,
      access_count: 50,
      priority: 2
    },
    DataFile::{
      id: "file_003",
      file_type: "logs",
      size_gb: 15.7,
      created_at: 1640995200000000000L,
      last_accessed: 1640995200000000000L,
      access_count: 25,
      priority: 3
    },
    DataFile::{
      id: "file_004",
      file_type: "archives",
      size_gb: 100.0,
      created_at: 1640995200000000000L - (86400000000000L * 60),  // 60天前
      last_accessed: 1640995200000000000L - (86400000000000L * 30), // 30天前
      access_count: 5,
      priority: 4
    },
    DataFile::{
      id: "file_005",
      file_type: "temp",
      size_gb: 5.2,
      created_at: 1640995200000000000L,
      last_accessed: 1640995200000000000L,
      access_count: 1,
      priority: 5
    }
  ]
  
  // 8. 创建存储管理器
  let storage_manager = StorageManager::{
    monitor: disk_monitor,
    files: test_files,
    write_cache_enabled: true,
    compression_enabled: true
  }
  
  // 9. 实现文件写入函数
  fn write_file(manager : StorageManager, file_size_gb : Double, file_type : String) -> (StorageManager, Bool, String?) {
    let mut updated_manager = manager
    let mut success = false
    let mut error_message = None
    
    // 检查可用空间
    if file_size_gb > updated_manager.monitor.available_space_gb.to_double() {
      // 尝试清理空间
      if updated_manager.monitor.cleanup_policy.auto_cleanup_enabled {
        let (cleaned_manager, freed_space) = cleanup_disk_space(updated_manager, file_size_gb)
        updated_manager = cleaned_manager
        
        if freed_space >= file_size_gb {
          success = true
        } else {
          error_message = Some("Insufficient disk space even after cleanup")
        }
      } else {
        error_message = Some("Insufficient disk space")
      }
    } else {
      success = true
    }
    
    if success {
      // 创建新文件
      let new_file = DataFile::{
        id: "file_" + (manager.files.length() + 1).to_string(),
        file_type: file_type,
        size_gb: file_size_gb,
        created_at: 1640995200000000000L,
        last_accessed: 1640995200000000000L,
        access_count: 1,
        priority: 1
      }
      
      updated_manager.files.push(new_file)
      
      // 更新磁盘使用情况
      updated_manager.monitor.used_space_gb = (updated_manager.monitor.used_space_gb.to_double() + file_size_gb).to_int()
      updated_manager.monitor.available_space_gb = (updated_manager.monitor.available_space_gb.to_double() - file_size_gb).to_int()
      
      // 更新磁盘状态
      let usage_percent = (updated_manager.monitor.used_space_gb.to_double() / updated_manager.monitor.total_space_gb.to_double()) * 100.0
      
      if usage_percent >= updated_manager.monitor.threshold_critical_percent {
        updated_manager.monitor.current_state = Critical
      } else if usage_percent >= updated_manager.monitor.threshold_warning_percent {
        updated_manager.monitor.current_state = Warning
      } else {
        updated_manager.monitor.current_state = Healthy
      }
    }
    
    (updated_manager, success, error_message)
  }
  
  // 10. 实现磁盘清理函数
  fn cleanup_disk_space(manager : StorageManager, required_space_gb : Double) -> (StorageManager, Double) {
    let mut updated_manager = manager
    let mut freed_space = 0.0
    let current_time = 1640995200000000000L
    let retention_days_ns = manager.monitor.cleanup_policy.retention_days * 86400000000000L
    
    // 按清理优先级排序文件
    let sorted_files = sort_files_by_cleanup_priority(manager.files, manager.monitor.cleanup_policy.cleanup_order)
    
    // 清理过期文件
    let mut i = 0
    while i < sorted_files.length() and freed_space < required_space_gb {
      let file = sorted_files[i]
      
      // 检查文件是否过期
      if current_time - file.created_at > retention_days_ns or file.file_type == "temp" {
        // 删除文件
        updated_manager.monitor.used_space_gb = (updated_manager.monitor.used_space_gb.to_double() - file.size_gb).to_int()
        updated_manager.monitor.available_space_gb = (updated_manager.monitor.available_space_gb.to_double() + file.size_gb).to_int()
        freed_space = freed_space + file.size_gb
        
        // 从文件列表中移除
        let mut j = 0
        while j < updated_manager.files.length() {
          if updated_manager.files[j].id == file.id {
            let mut new_files = []
            let mut k = 0
            while k < updated_manager.files.length() {
              if k != j {
                new_files.push(updated_manager.files[k])
              }
              k = k + 1
            }
            updated_manager.files = new_files
            break
          }
          j = j + 1
        }
      }
      
      i = i + 1
    }
    
    (updated_manager, freed_space)
  }
  
  // 11. 实现文件排序函数
  fn sort_files_by_cleanup_priority(files : Array[DataFile], cleanup_order : Array[String]) -> Array[DataFile] {
    let sorted = []
    let mut processed = []
    
    // 按清理顺序添加文件
    let mut i = 0
    while i < cleanup_order.length() {
      let file_type = cleanup_order[i]
      
      let mut j = 0
      while j < files.length() {
        if files[j].file_type == file_type {
          sorted.push(files[j])
          processed.push(j)
        }
        j = j + 1
      }
      
      i = i + 1
    }
    
    // 添加剩余文件
    let mut k = 0
    while k < files.length() {
      let mut already_processed = false
      let mut l = 0
      while l < processed.length() {
        if processed[l] == k {
          already_processed = true
          break
        }
        l = l + 1
      }
      
      if not already_processed {
        sorted.push(files[k])
      }
      
      k = k + 1
    }
    
    sorted
  }
  
  // 12. 测试磁盘空间管理
  
  // 场景1: 正常文件写入
  let (manager1, success1, error1) = write_file(storage_manager, 50.0, "traces")
  assert_eq(success1, true)
  assert_eq(error1, None)
  assert_eq(manager1.files.length(), 6)
  
  // 场景2: 磁盘空间不足，触发清理
  let mut full_manager = manager1
  full_manager.monitor.available_space_gb = 5  // 只剩5GB空间
  full_manager.monitor.current_state = Critical
  
  let (manager2, success2, error2) = write_file(full_manager, 20.0, "logs")
  assert_eq(success2, true)  // 清理后应该成功
  assert_eq(error2, None)
  
  // 场景3: 磁盘完全满，无法写入
  let mut completely_full_manager = manager2
  completely_full_manager.monitor.available_space_gb = 0
  completely_full_manager.monitor.current_state = Full
  
  let (manager3, success3, error3) = write_file(completely_full_manager, 10.0, "metrics")
  assert_eq(success3, false)
  assert_eq(error3 != None, true)
  
  // 场景4: 禁用自动清理
  let mut no_cleanup_manager = storage_manager
  no_cleanup_manager.monitor.cleanup_policy.auto_cleanup_enabled = false
  no_cleanup_manager.monitor.available_space_gb = 5
  
  let (manager4, success4, error4) = write_file(no_cleanup_manager, 10.0, "metrics")
  assert_eq(success4, false)
  assert_eq(error4, Some("Insufficient disk space"))
}

test "connection_pool_limits" {
  // 测试连接池限制
  
  // 1. 定义连接状态
  enum ConnectionState {
    Idle
    Active
    Busy
    Closed
    Error
  }
  
  // 2. 定义数据库连接
  struct DatabaseConnection {
    id : String
    state : ConnectionState
    created_at : Int64
    last_used : Int64
    usage_count : Int
    max_lifetime_ms : Int
    idle_timeout_ms : Int
  }
  
  // 3. 定义连接池配置
  struct PoolConfig {
    min_connections : Int
    max_connections : Int
    max_idle_time_ms : Int
    max_lifetime_ms : Int
    connection_timeout_ms : Int
    validation_enabled : Bool
  }
  
  // 4. 定义连接池
  struct ConnectionPool {
    config : PoolConfig
    connections : Array[DatabaseConnection]
    active_connections : Int
    total_created : Int
    total_destroyed : Int
    wait_queue : Array[String]  // 等待连接的请求ID
  }
  
  // 5. 创建连接池配置
  let pool_config = PoolConfig::{
    min_connections: 5,
    max_connections: 20,
    max_idle_time_ms: 300000,  // 5分钟
    max_lifetime_ms: 3600000,  // 1小时
    connection_timeout_ms: 5000,
    validation_enabled: true
  }
  
  // 6. 创建初始连接池
  let initial_connections = []
  let mut i = 0
  while i < pool_config.min_connections {
    initial_connections.push(DatabaseConnection::{
      id: "conn_" + (i + 1).to_string(),
      state: Idle,
      created_at: 1640995200000000000L,
      last_used: 1640995200000000000L,
      usage_count: 0,
      max_lifetime_ms: pool_config.max_lifetime_ms,
      idle_timeout_ms: pool_config.max_idle_time_ms
    })
    i = i + 1
  }
  
  let connection_pool = ConnectionPool::{
    config: pool_config,
    connections: initial_connections,
    active_connections: 0,
    total_created: pool_config.min_connections,
    total_destroyed: 0,
    wait_queue: []
  }
  
  // 7. 实现连接获取函数
  fn get_connection(pool : ConnectionPool, request_id : String) -> (ConnectionPool, DatabaseConnection?, Bool) {
    let mut updated_pool = pool
    let mut connection = None
    let mut from_wait_queue = false
    
    // 查找空闲连接
    let mut i = 0
    while i < updated_pool.connections.length() {
      if updated_pool.connections[i].state == Idle {
        // 验证连接
        if validate_connection(updated_pool.connections[i]) {
          updated_pool.connections[i].state = Active
          updated_pool.connections[i].last_used = 1640995200000000000L
          updated_pool.connections[i].usage_count = updated_pool.connections[i].usage_count + 1
          updated_pool.active_connections = updated_pool.active_connections + 1
          connection = Some(updated_pool.connections[i])
          break
        } else {
          // 连接无效，销毁并创建新连接
          updated_pool.connections[i].state = Closed
          updated_pool.total_destroyed = updated_pool.total_destroyed + 1
        }
      }
      i = i + 1
    }
    
    // 如果没有空闲连接，检查是否可以创建新连接
    if connection == None and updated_pool.connections.length() < updated_pool.config.max_connections {
      let new_connection = DatabaseConnection::{
        id: "conn_" + (updated_pool.total_created + 1).to_string(),
        state: Active,
        created_at: 1640995200000000000L,
        last_used: 1640995200000000000L,
        usage_count: 1,
        max_lifetime_ms: updated_pool.config.max_lifetime_ms,
        idle_timeout_ms: updated_pool.config.max_idle_time_ms
      }
      
      updated_pool.connections.push(new_connection)
      updated_pool.active_connections = updated_pool.active_connections + 1
      updated_pool.total_created = updated_pool.total_created + 1
      connection = Some(new_connection)
    }
    
    // 如果仍然没有连接，加入等待队列
    if connection == None {
      updated_pool.wait_queue.push(request_id)
      from_wait_queue = true
    }
    
    (updated_pool, connection, from_wait_queue)
  }
  
  // 8. 实现连接释放函数
  fn release_connection(pool : ConnectionPool, connection_id : String) -> (ConnectionPool, Bool, String?) {
    let mut updated_pool = pool
    let mut released = false
    let mut error_message = None
    
    // 查找连接
    let mut i = 0
    while i < updated_pool.connections.length() {
      if updated_pool.connections[i].id == connection_id {
        if updated_pool.connections[i].state == Active {
          updated_pool.connections[i].state = Idle
          updated_pool.connections[i].last_used = 1640995200000000000L
          updated_pool.active_connections = updated_pool.active_connections - 1
          released = true
          
          // 检查等待队列
          if updated_pool.wait_queue.length() > 0 {
            let waiting_request = updated_pool.wait_queue[0]
            updated_pool.connections[i].state = Active
            updated_pool.connections[i].usage_count = updated_pool.connections[i].usage_count + 1
            updated_pool.active_connections = updated_pool.active_connections + 1
            
            // 移除等待的请求
            let mut new_wait_queue = []
            let mut j = 1
            while j < updated_pool.wait_queue.length() {
              new_wait_queue.push(updated_pool.wait_queue[j])
              j = j + 1
            }
            updated_pool.wait_queue = new_wait_queue
          }
        } else {
          error_message = Some("Connection is not active")
        }
        break
      }
      i = i + 1
    }
    
    if not released and error_message == None {
      error_message = Some("Connection not found")
    }
    
    (updated_pool, released, error_message)
  }
  
  // 9. 实现连接验证函数
  fn validate_connection(connection : DatabaseConnection) -> Bool {
    let current_time = 1640995200000000000L
    
    // 检查连接是否过期
    if current_time - connection.created_at > connection.max_lifetime_ms * 1000000L {
      return false
    }
    
    // 检查连接是否空闲超时
    if connection.state == Idle and current_time - connection.last_used > connection.idle_timeout_ms * 1000000L {
      return false
    }
    
    true
  }
  
  // 10. 测试连接池限制
  
  // 场景1: 获取连接
  let (pool1, conn1, queued1) = get_connection(connection_pool, "req_001")
  assert_eq(conn1 != None, true)
  assert_eq(queued1, false)
  assert_eq(pool1.active_connections, 1)
  
  // 场景2: 获取多个连接
  let (pool2, conn2, queued2) = get_connection(pool1, "req_002")
  let (pool3, conn3, queued3) = get_connection(pool2, "req_003")
  let (pool4, conn4, queued4) = get_connection(pool3, "req_004")
  
  assert_eq(conn2 != None, true)
  assert_eq(conn3 != None, true)
  assert_eq(conn4 != None, true)
  assert_eq(pool4.active_connections, 3)
  
  // 场景3: 释放连接
  match conn1 {
    Some(c) => {
      let (pool5, released5, error5) = release_connection(pool4, c.id)
      assert_eq(released5, true)
      assert_eq(error5, None)
      assert_eq(pool5.active_connections, 2)
    }
    None => {}
  }
  
  // 场景4: 连接池满，请求进入等待队列
  let mut full_pool = pool4
  // 创建最大数量的连接
  let mut i = pool4.active_connections
  while i < pool4.config.max_connections {
    let (temp_pool, temp_conn, _) = get_connection(full_pool, "req_fill_" + i.to_string())
    full_pool = temp_pool
    i = i + 1
  }
  
  // 尝试获取超出限制的连接
  let (pool6, conn6, queued6) = get_connection(full_pool, "req_overflow")
  assert_eq(conn6, None)
  assert_eq(queued6, true)
  assert_eq(pool6.wait_queue.length(), 1)
  
  // 场景5: 释放连接后，等待队列中的请求被处理
  match conn2 {
    Some(c) => {
      let (pool7, released7, error7) = release_connection(pool6, c.id)
      assert_eq(released7, true)
      assert_eq(pool7.active_connections, pool6.config.max_connections)  // 连接立即被等待队列使用
      assert_eq(pool7.wait_queue.length(), 0)
    }
    None => {}
  }
}

// 辅助函数
fn billing_item_to_string(item : BillingItem) -> String {
  match item {
    SpanIngestion => "Span Ingestion"
    MetricIngestion => "Metric Ingestion"
    LogIngestion => "Log Ingestion"
    DataStorage => "Data Storage"
    DataTransfer => "Data Transfer"
    APIRequests => "API Requests"
    SupportHours => "Support Hours"
  }
}

fn unit_for_billing_item(item : BillingItem) -> String {
  match item {
    SpanIngestion => "spans"
    MetricIngestion => "metrics"
    LogIngestion => "logs"
    DataStorage => "GB"
    DataTransfer => "GB"
    APIRequests => "requests"
    SupportHours => "hours"
  }
}