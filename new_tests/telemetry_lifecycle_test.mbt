// 遥测数据生命周期管理测试用例

test "telemetry_data_creation_tracking" {
  // 测试遥测数据创建与追踪
  
  // 定义遥测数据项
  type TelemetryDataItem = {
    item_id: String,
    data_type: String,
    creation_timestamp: Int64,
    created_by: String,
    initial_attributes: Array[(String, String)],
    data_size_bytes: Int,
    priority: String
  }
  
  // 模拟创建不同类型的遥测数据
  let telemetry_items = [
    TelemetryDataItem {
      item_id: "trace-001",
      data_type: "trace",
      creation_timestamp: 1640995200000L,
      created_by: "user-service",
      initial_attributes: [
        ("service.name", "user-service"),
        ("operation.name", "get_user"),
        ("user.id", "12345")
      ],
      data_size_bytes: 512,
      priority: "high"
    },
    TelemetryDataItem {
      item_id: "metric-001",
      data_type: "metric",
      creation_timestamp: 1640995201000L,
      created_by: "order-service",
      initial_attributes: [
        ("service.name", "order-service"),
        ("metric.name", "order_count"),
        ("metric.type", "counter")
      ],
      data_size_bytes: 256,
      priority: "medium"
    },
    TelemetryDataItem {
      item_id: "log-001",
      data_type: "log",
      creation_timestamp: 1640995202000L,
      created_by: "payment-service",
      initial_attributes: [
        ("service.name", "payment-service"),
        ("log.level", "info"),
        ("message", "Payment processed successfully")
      ],
      data_size_bytes: 1024,
      priority: "low"
    },
    TelemetryDataItem {
      item_id: "trace-002",
      data_type: "trace",
      creation_timestamp: 1640995203000L,
      created_by: "notification-service",
      initial_attributes: [
        ("service.name", "notification-service"),
        ("operation.name", "send_email"),
        ("notification.type", "email")
      ],
      data_size_bytes: 768,
      priority: "medium"
    }
  ]
  
  // 验证遥测数据项
  assert_eq(telemetry_items.length(), 4)
  assert_eq(telemetry_items[0].data_type, "trace")
  assert_eq(telemetry_items[1].data_type, "metric")
  assert_eq(telemetry_items[2].data_type, "log")
  assert_eq(telemetry_items[3].priority, "medium")
  
  // 验证时间戳递增
  assert_eq(telemetry_items[0].creation_timestamp < telemetry_items[1].creation_timestamp, true)
  assert_eq(telemetry_items[1].creation_timestamp < telemetry_items[2].creation_timestamp, true)
  assert_eq(telemetry_items[2].creation_timestamp < telemetry_items[3].creation_timestamp, true)
  
  // 按数据类型分组
  let mut items_by_type = {}
  let mut i = 0
  while i < telemetry_items.length() {
    let item = telemetry_items[i]
    let data_type = item.data_type
    let current_items = items_by_type.get(data_type) |> unwrap_or([])
    items_by_type[data_type] = [..current_items, item]
    i = i + 1
  }
  
  // 验证分组结果
  assert_eq(items_by_type["trace"].length(), 2)
  assert_eq(items_by_type["metric"].length(), 1)
  assert_eq(items_by_type["log"].length(), 1)
  
  // 计算创建统计
  let mut total_data_size = 0
  let mut high_priority_count = 0
  let mut unique_services = []
  
  i = 0
  while i < telemetry_items.length() {
    let item = telemetry_items[i]
    total_data_size = total_data_size + item.data_size_bytes
    
    if item.priority == "high" {
      high_priority_count = high_priority_count + 1
    }
    
    // 统计唯一服务
    let service_name = item.created_by
    let mut already_seen = false
    let mut j = 0
    while j < unique_services.length() {
      if unique_services[j] == service_name {
        already_seen = true
        break
      }
      j = j + 1
    }
    
    if not already_seen {
      unique_services.push(service_name)
    }
    
    i = i + 1
  }
  
  // 验证统计结果
  assert_eq(total_data_size, 2560)  // 512 + 256 + 1024 + 768
  assert_eq(high_priority_count, 1)
  assert_eq(unique_services.length(), 4)
  assert_eq(unique_services.contains("user-service"), true)
  assert_eq(unique_services.contains("payment-service"), true)
  
  // 数据创建追踪器
  type CreationTracker = {
    total_items_created: Int,
    items_by_type: Array[(String, Int)],
    items_by_service: Array[(String, Int)],
    creation_rate_per_minute: Double,
    average_data_size: Double
  }
  
  // 创建追踪器
  let creation_tracker = CreationTracker {
    total_items_created: telemetry_items.length(),
    items_by_type: [
      ("trace", items_by_type["trace"].length()),
      ("metric", items_by_type["metric"].length()),
      ("log", items_by_type["log"].length())
    ],
    items_by_service: [
      ("user-service", 1),
      ("order-service", 1),
      ("payment-service", 1),
      ("notification-service", 1)
    ],
    creation_rate_per_minute: telemetry_items.length().to_double(),  // 假设在1分钟内创建
    average_data_size: total_data_size.to_double() / telemetry_items.length().to_double()
  }
  
  // 验证追踪器
  assert_eq(creation_tracker.total_items_created, 4)
  assert_eq(creation_tracker.creation_rate_per_minute, 4.0)
  assert_eq(creation_tracker.average_data_size, 640.0)  // 2560 / 4
  
  // 生成创建追踪报告
  let creation_report = "Telemetry Data Creation Report:\n"
    + "Total Items Created: " + creation_tracker.total_items_created.to_string() + "\n"
    + "Creation Rate: " + creation_tracker.creation_rate_per_minute.to_string() + " items/minute\n"
    + "Average Data Size: " + creation_tracker.average_data_size.to_string() + " bytes\n"
    + "Items by Type:\n"
    + "  - Traces: " + creation_tracker.items_by_type[0].1.to_string() + "\n"
    + "  - Metrics: " + creation_tracker.items_by_type[1].1.to_string() + "\n"
    + "  - Logs: " + creation_tracker.items_by_type[2].1.to_string() + "\n"
    + "Items by Service:\n"
    + "  - user-service: " + creation_tracker.items_by_service[0].1.to_string() + "\n"
    + "  - order-service: " + creation_tracker.items_by_service[1].1.to_string() + "\n"
    + "  - payment-service: " + creation_tracker.items_by_service[2].1.to_string() + "\n"
    + "  - notification-service: " + creation_tracker.items_by_service[3].1.to_string() + "\n"
    + "High Priority Items: " + high_priority_count.to_string()
  
  // 验证报告内容
  assert_eq(creation_report.contains("Total Items Created: 4"), true)
  assert_eq(creation_report.contains("Creation Rate: 4.0 items/minute"), true)
  assert_eq(creation_report.contains("Average Data Size: 640.0 bytes"), true)
  assert_eq(creation_report.contains("Traces: 2"), true)
  assert_eq(creation_report.contains("Metrics: 1"), true)
  assert_eq(creation_report.contains("Logs: 1"), true)
  assert_eq(creation_report.contains("High Priority Items: 1"), true)
}

test "telemetry_data_updates_and_versioning" {
  // 测试遥测数据更新与版本控制
  
  // 定义数据更新记录
  type DataUpdate = {
    update_id: String,
    item_id: String,
    update_timestamp: Int64,
    update_type: String,
    old_value: String,
    new_value: String,
    updated_by: String,
    reason: String
  }
  
  // 定义版本化的遥测数据
  type VersionedTelemetryData = {
    item_id: String,
    current_version: Int,
    creation_timestamp: Int64,
    last_updated_timestamp: Int64,
    updates: Array[DataUpdate],
    current_attributes: Array[(String, String)],
    is_active: Bool
  }
  
  // 创建初始版本化数据
  let mut versioned_data = VersionedTelemetryData {
    item_id: "trace-001",
    current_version: 1,
    creation_timestamp: 1640995200000L,
    last_updated_timestamp: 1640995200000L,
    updates: [],
    current_attributes: [
      ("service.name", "user-service"),
      ("operation.name", "get_user"),
      ("user.id", "12345"),
      ("http.method", "GET"),
      ("http.status_code", "200")
    ],
    is_active: true
  }
  
  // 验证初始状态
  assert_eq(versioned_data.current_version, 1)
  assert_eq(versioned_data.updates.length(), 0)
  assert_eq(versioned_data.current_attributes.length(), 5)
  assert_eq(versioned_data.is_active, true)
  
  // 模拟数据更新1: 添加新的属性
  let update_1 = DataUpdate {
    update_id: "upd-001",
    item_id: "trace-001",
    update_timestamp: 1640995300000L,
    update_type: "attribute_add",
    old_value: "",
    new_value: "response_time_ms:150",
    updated_by: "telemetry-processor",
    reason: "Add performance metric"
  }
  
  versioned_data.updates.push(update_1)
  versioned_data.current_attributes.push(("response_time_ms", "150"))
  versioned_data.current_version = versioned_data.current_version + 1
  versioned_data.last_updated_timestamp = update_1.update_timestamp
  
  // 验证第一次更新
  assert_eq(versioned_data.current_version, 2)
  assert_eq(versioned_data.updates.length(), 1)
  assert_eq(versioned_data.current_attributes.length(), 6)
  assert_eq(versioned_data.last_updated_timestamp, 1640995300000L)
  
  // 模拟数据更新2: 修改现有属性
  let update_2 = DataUpdate {
    update_id: "upd-002",
    item_id: "trace-001",
    update_timestamp: 1640995400000L,
    update_type: "attribute_modify",
    old_value: "http.status_code:200",
    new_value: "http.status_code:201",
    updated_by: "api-gateway",
    reason: "Status code corrected"
  }
  
  // 查找并修改属性
  let mut i = 0
  while i < versioned_data.current_attributes.length() {
    if versioned_data.current_attributes[i].0 == "http.status_code" {
      versioned_data.current_attributes[i] = ("http.status_code", "201")
      break
    }
    i = i + 1
  }
  
  versioned_data.updates.push(update_2)
  versioned_data.current_version = versioned_data.current_version + 1
  versioned_data.last_updated_timestamp = update_2.update_timestamp
  
  // 验证第二次更新
  assert_eq(versioned_data.current_version, 3)
  assert_eq(versioned_data.updates.length(), 2)
  
  // 验证属性修改
  let mut status_code_found = false
  i = 0
  while i < versioned_data.current_attributes.length() {
    if versioned_data.current_attributes[i].0 == "http.status_code" {
      assert_eq(versioned_data.current_attributes[i].1, "201")
      status_code_found = true
      break
    }
    i = i + 1
  }
  assert_eq(status_code_found, true)
  
  // 模拟数据更新3: 删除属性
  let update_3 = DataUpdate {
    update_id: "upd-003",
    item_id: "trace-001",
    update_timestamp: 1640995500000L,
    update_type: "attribute_delete",
    old_value: "user.id:12345",
    new_value: "",
    updated_by: "privacy-filter",
    reason: "Remove sensitive information"
  }
  
  // 删除敏感属性
  let mut new_attributes = []
  i = 0
  while i < versioned_data.current_attributes.length() {
    if versioned_data.current_attributes[i].0 != "user.id" {
      new_attributes.push(versioned_data.current_attributes[i])
    }
    i = i + 1
  }
  
  versioned_data.current_attributes = new_attributes
  versioned_data.updates.push(update_3)
  versioned_data.current_version = versioned_data.current_version + 1
  versioned_data.last_updated_timestamp = update_3.update_timestamp
  
  // 验证第三次更新
  assert_eq(versioned_data.current_version, 4)
  assert_eq(versioned_data.updates.length(), 3)
  assert_eq(versioned_data.current_attributes.length(), 5)  // 从6减少到5
  
  // 验证敏感属性已删除
  let mut user_id_found = false
  i = 0
  while i < versioned_data.current_attributes.length() {
    if versioned_data.current_attributes[i].0 == "user.id" {
      user_id_found = true
      break
    }
    i = i + 1
  }
  assert_eq(user_id_found, false)
  
  // 模拟数据更新4: 状态变更
  let update_4 = DataUpdate {
    update_id: "upd-004",
    item_id: "trace-001",
    update_timestamp: 1640995600000L,
    update_type: "status_change",
    old_value: "active:true",
    new_value: "active:false",
    updated_by: "lifecycle-manager",
    reason: "Data retention policy - archive old data"
  }
  
  versioned_data.is_active = false
  versioned_data.updates.push(update_4)
  versioned_data.current_version = versioned_data.current_version + 1
  versioned_data.last_updated_timestamp = update_4.update_timestamp
  
  // 验证第四次更新
  assert_eq(versioned_data.current_version, 5)
  assert_eq(versioned_data.updates.length(), 4)
  assert_eq(versioned_data.is_active, false)
  
  // 分析更新模式
  type UpdateAnalysis = {
    total_updates: Int,
    updates_by_type: Array[(String, Int)],
    updates_by_updater: Array[(String, Int)],
    average_update_interval_ms: Int64,
    most_active_updater: String
  }
  
  // 统计更新类型
  let mut updates_by_type = {}
  let mut updates_by_updater = {}
  
  i = 0
  while i < versioned_data.updates.length() {
    let update = versioned_data.updates[i]
    
    // 统计更新类型
    let update_type = update.update_type
    let current_type_count = updates_by_type.get(update_type) |> unwrap_or(0)
    updates_by_type[update_type] = current_type_count + 1
    
    // 统计更新者
    let updater = update.updated_by
    let current_updater_count = updates_by_updater.get(updater) |> unwrap_or(0)
    updates_by_updater[updater] = current_updater_count + 1
    
    i = i + 1
  }
  
  // 计算平均更新间隔
  let total_time_span = versioned_data.last_updated_timestamp - versioned_data.creation_timestamp
  let average_interval = if versioned_data.updates.length() > 0 {
    total_time_span / versioned_data.updates.length().to_int64()
  } else { 0L }
  
  // 找出最活跃的更新者
  let mut most_active_updater = ""
  let mut max_updates = 0
  let updater_keys = ["telemetry-processor", "api-gateway", "privacy-filter", "lifecycle-manager"]
  
  i = 0
  while i < updater_keys.length() {
    let updater = updater_keys[i]
    let update_count = updates_by_updater.get(updater) |> unwrap_or(0)
    if update_count > max_updates {
      max_updates = update_count
      most_active_updater = updater
    }
    i = i + 1
  }
  
  // 创建更新分析
  let update_analysis = UpdateAnalysis {
    total_updates: versioned_data.updates.length(),
    updates_by_type: [
      ("attribute_add", updates_by_type["attribute_add"]),
      ("attribute_modify", updates_by_type["attribute_modify"]),
      ("attribute_delete", updates_by_type["attribute_delete"]),
      ("status_change", updates_by_type["status_change"])
    ],
    updates_by_updater: [
      ("telemetry-processor", updates_by_updater["telemetry-processor"]),
      ("api-gateway", updates_by_updater["api-gateway"]),
      ("privacy-filter", updates_by_updater["privacy-filter"]),
      ("lifecycle-manager", updates_by_updater["lifecycle-manager"])
    ],
    average_update_interval_ms: average_interval,
    most_active_updater: most_active_updater
  }
  
  // 验证分析结果
  assert_eq(update_analysis.total_updates, 4)
  assert_eq(update_analysis.average_update_interval_ms, 100000L)  // 400000 / 4
  assert_eq(update_analysis.updates_by_type.length(), 4)
  assert_eq(update_analysis.updates_by_updater.length(), 4)
  
  // 生成版本控制报告
  let versioning_report = "Telemetry Data Versioning Report:\n"
    + "Item ID: " + versioned_data.item_id + "\n"
    + "Current Version: " + versioned_data.current_version.to_string() + "\n"
    + "Total Updates: " + update_analysis.total_updates.to_string() + "\n"
    + "Average Update Interval: " + update_analysis.average_update_interval_ms.to_string() + "ms\n"
    + "Current Status: " + (if versioned_data.is_active { "Active" } else { "Archived" }) + "\n"
    + "Updates by Type:\n"
    + "  - Attribute Add: " + update_analysis.updates_by_type[0].1.to_string() + "\n"
    + "  - Attribute Modify: " + update_analysis.updates_by_type[1].1.to_string() + "\n"
    + "  - Attribute Delete: " + update_analysis.updates_by_type[2].1.to_string() + "\n"
    + "  - Status Change: " + update_analysis.updates_by_type[3].1.to_string() + "\n"
    + "Current Attributes: " + versioned_data.current_attributes.length().to_string() + "\n"
    + "Most Active Updater: " + update_analysis.most_active_updater
  
  // 验证报告内容
  assert_eq(versioning_report.contains("Item ID: trace-001"), true)
  assert_eq(versioning_report.contains("Current Version: 5"), true)
  assert_eq(versioning_report.contains("Total Updates: 4"), true)
  assert_eq(versioning_report.contains("Current Status: Archived"), true)
  assert_eq(versioning_report.contains("Current Attributes: 5"), true)
}

test "telemetry_data_retention_and_cleanup" {
  // 测试遥测数据保留与清理
  
  // 定义保留策略
  type RetentionPolicy = {
    policy_name: String,
    data_type: String,
    retention_days: Int,
    max_versions: Int,
    cleanup_criteria: Array[String]
  }
  
  // 定义数据项状态
  type DataItemStatus = {
    item_id: String,
    data_type: String,
    creation_date: String,
    age_days: Int,
    version_count: Int,
    last_accessed_date: String,
    access_frequency: String,
    storage_size_mb: Double,
    is_active: Bool
  }
  
  // 模拟不同的保留策略
  let retention_policies = [
    RetentionPolicy {
      policy_name: "trace_retention",
      data_type: "trace",
      retention_days: 30,
      max_versions: 10,
      cleanup_criteria: ["age_exceeded", "version_limit", "inactive"]
    },
    RetentionPolicy {
      policy_name: "metric_retention",
      data_type: "metric",
      retention_days: 90,
      max_versions: 50,
      cleanup_criteria: ["age_exceeded", "version_limit"]
    },
    RetentionPolicy {
      policy_name: "log_retention",
      data_type: "log",
      retention_days: 7,
      max_versions: 5,
      cleanup_criteria: ["age_exceeded", "storage_pressure", "inactive"]
    }
  ]
  
  // 验证保留策略
  assert_eq(retention_policies.length(), 3)
  assert_eq(retention_policies[0].retention_days, 30)
  assert_eq(retention_policies[1].max_versions, 50)
  assert_eq(retention_policies[2].cleanup_criteria.length(), 3)
  
  // 模拟数据项状态
  let data_items = [
    DataItemStatus {
      item_id: "trace-001",
      data_type: "trace",
      creation_date: "2023-12-01",
      age_days: 30,
      version_count: 5,
      last_accessed_date: "2023-12-28",
      access_frequency: "daily",
      storage_size_mb: 2.5,
      is_active: true
    },
    DataItemStatus {
      item_id: "trace-002",
      data_type: "trace",
      creation_date: "2023-11-01",
      age_days: 60,
      version_count: 12,
      last_accessed_date: "2023-11-15",
      access_frequency: "weekly",
      storage_size_mb: 3.8,
      is_active: false
    },
    DataItemStatus {
      item_id: "metric-001",
      data_type: "metric",
      creation_date: "2023-10-01",
      age_days: 91,
      version_count: 45,
      last_accessed_date: "2023-12-25",
      access_frequency: "daily",
      storage_size_mb: 1.2,
      is_active: true
    },
    DataItemStatus {
      item_id: "log-001",
      data_type: "log",
      creation_date: "2023-12-25",
      age_days: 6,
      version_count: 3,
      last_accessed_date: "2023-12-30",
      access_frequency: "hourly",
      storage_size_mb: 0.8,
      is_active: true
    },
    DataItemStatus {
      item_id: "log-002",
      data_type: "log",
      creation_date: "2023-12-20",
      age_days: 11,
      version_count: 6,
      last_accessed_date: "2023-12-25",
      access_frequency: "rarely",
      storage_size_mb: 1.5,
      is_active: false
    }
  ]
  
  // 验证数据项
  assert_eq(data_items.length(), 5)
  assert_eq(data_items[0].data_type, "trace")
  assert_eq(data_items[2].data_type, "metric")
  assert_eq(data_items[3].data_type, "log")
  
  // 清理决策函数
  let should_cleanup = fn(item: DataItemStatus, policy: RetentionPolicy) -> (Bool, String) {
    let mut cleanup_reasons = []
    
    // 检查年龄限制
    if item.age_days > policy.retention_days {
      cleanup_reasons.push("age_exceeded")
    }
    
    // 检查版本限制
    if item.version_count > policy.max_versions {
      cleanup_reasons.push("version_limit")
    }
    
    // 检查非活跃状态
    if not item.is_active {
      cleanup_reasons.push("inactive")
    }
    
    // 检查存储压力（假设超过2MB的数据在压力下会被清理）
    if item.storage_size_mb > 2.0 and policy.cleanup_criteria.contains("storage_pressure") {
      cleanup_reasons.push("storage_pressure")
    }
    
    if cleanup_reasons.length() > 0 {
      (true, cleanup_reasons.join(","))
    } else {
      (false, "")
    }
  }
  
  // 对每个数据项应用清理策略
  let mut cleanup_decisions = []
  let mut i = 0
  while i < data_items.length() {
    let item = data_items[i]
    
    // 找到对应的策略
    let mut policy = retention_policies[0]  // 默认策略
    let mut j = 0
    while j < retention_policies.length() {
      if retention_policies[j].data_type == item.data_type {
        policy = retention_policies[j]
        break
      }
      j = j + 1
    }
    
    let (should_cleanup_item, reason) = should_cleanup(item, policy)
    cleanup_decisions.push((item.item_id, should_cleanup_item, reason))
    
    i = i + 1
  }
  
  // 验证清理决策
  assert_eq(cleanup_decisions.length(), 5)
  
  // 预期结果：
  // trace-001: 30天，活跃，版本5 -> 不清理
  // trace-002: 60天>30，非活跃，版本12>10 -> 清理
  // metric-001: 91天>90，版本45<50 -> 清理（年龄）
  // log-001: 6天<7，版本3<5 -> 不清理
  // log-002: 11天>7，非活跃，版本6>5 -> 清理
  
  assert_eq(cleanup_decisions[0].1, false)  // trace-001 不清理
  assert_eq(cleanup_decisions[1].1, true)   // trace-002 清理
  assert_eq(cleanup_decisions[2].1, true)   // metric-001 清理
  assert_eq(cleanup_decisions[3].1, false)  // log-001 不清理
  assert_eq(cleanup_decisions[4].1, true)   // log-002 清理
  
  // 验证清理原因
  assert_eq(cleanup_decisions[1].2.contains("age_exceeded"), true)
  assert_eq(cleanup_decisions[1].2.contains("version_limit"), true)
  assert_eq(cleanup_decisions[1].2.contains("inactive"), true)
  assert_eq(cleanup_decisions[2].2, "age_exceeded")
  assert_eq(cleanup_decisions[4].2.contains("age_exceeded"), true)
  assert_eq(cleanup_decisions[4].2.contains("version_limit"), true)
  
  // 计算清理统计
  type CleanupStatistics = {
    total_items: Int,
    items_to_cleanup: Int,
    items_to_keep: Int,
    storage_freed_mb: Double,
    cleanup_by_type: Array[(String, Int)]
  }
  
  let mut items_to_cleanup_count = 0
  let mut storage_freed = 0.0
  let mut cleanup_by_type = {}
  
  i = 0
  while i < cleanup_decisions.length() {
    let decision = cleanup_decisions[i]
    let item = data_items[i]
    
    if decision.1 {  // 需要清理
      items_to_cleanup_count = items_to_cleanup_count + 1
      storage_freed = storage_freed + item.storage_size_mb
      
      let data_type = item.data_type
      let current_count = cleanup_by_type.get(data_type) |> unwrap_or(0)
      cleanup_by_type[data_type] = current_count + 1
    }
    
    i = i + 1
  }
  
  // 创建清理统计
  let cleanup_stats = CleanupStatistics {
    total_items: data_items.length(),
    items_to_cleanup: items_to_cleanup_count,
    items_to_keep: data_items.length() - items_to_cleanup_count,
    storage_freed_mb: storage_freed,
    cleanup_by_type: [
      ("trace", cleanup_by_type["trace"]),
      ("metric", cleanup_by_type["metric"]),
      ("log", cleanup_by_type["log"])
    ]
  }
  
  // 验证清理统计
  assert_eq(cleanup_stats.total_items, 5)
  assert_eq(cleanup_stats.items_to_cleanup, 3)
  assert_eq(cleanup_stats.items_to_keep, 2)
  assert_eq(cleanup_stats.storage_freed_mb, 6.5)  // 3.8 + 1.2 + 1.5
  assert_eq(cleanup_stats.cleanup_by_type[0].1, 1)  // trace: 1个
  assert_eq(cleanup_stats.cleanup_by_type[1].1, 1)  // metric: 1个
  assert_eq(cleanup_stats.cleanup_by_type[2].1, 1)  // log: 1个
  
  // 生成保留与清理报告
  let retention_report = "Telemetry Data Retention and Cleanup Report:\n"
    + "Total Data Items: " + cleanup_stats.total_items.to_string() + "\n"
    + "Items to Cleanup: " + cleanup_stats.items_to_cleanup.to_string() + "\n"
    + "Items to Keep: " + cleanup_stats.items_to_keep.to_string() + "\n"
    + "Storage to be Freed: " + cleanup_stats.storage_freed_mb.to_string() + " MB\n"
    + "Cleanup by Data Type:\n"
    + "  - Traces: " + cleanup_stats.cleanup_by_type[0].1.to_string() + " items\n"
    + "  - Metrics: " + cleanup_stats.cleanup_by_type[1].1.to_string() + " items\n"
    + "  - Logs: " + cleanup_stats.cleanup_by_type[2].1.to_string() + " items\n"
    + "Detailed Cleanup Decisions:\n"
    + "  - " + cleanup_decisions[0].0 + ": KEEP (active, within retention)\n"
    + "  - " + cleanup_decisions[1].0 + ": CLEANUP (" + cleanup_decisions[1].2 + ")\n"
    + "  - " + cleanup_decisions[2].0 + ": CLEANUP (" + cleanup_decisions[2].2 + ")\n"
    + "  - " + cleanup_decisions[3].0 + ": KEEP (recent, low version count)\n"
    + "  - " + cleanup_decisions[4].0 + ": CLEANUP (" + cleanup_decisions[4].2 + ")"
  
  // 验证报告内容
  assert_eq(retention_report.contains("Total Data Items: 5"), true)
  assert_eq(retention_report.contains("Items to Cleanup: 3"), true)
  assert_eq(retention_report.contains("Items to Keep: 2"), true)
  assert_eq(retention_report.contains("Storage to be Freed: 6.5 MB"), true)
  assert_eq(retention_report.contains("trace-001: KEEP"), true)
  assert_eq(retention_report.contains("trace-002: CLEANUP"), true)
  assert_eq(retention_report.contains("metric-001: CLEANUP"), true)
  assert_eq(retention_report.contains("log-001: KEEP"), true)
  assert_eq(retention_report.contains("log-002: CLEANUP"), true)
}