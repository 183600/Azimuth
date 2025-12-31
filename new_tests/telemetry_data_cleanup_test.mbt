// 遥测数据清理测试用例

test "telemetry_automated_cleanup" {
  // 测试遥测数据自动清理
  
  // 自动清理配置
  let cleanup_policies = {
    "temporary": 1,        // 临时数据1天
    "debug": 7,           // 调试数据7天
    "normal": 30,         // 普通数据30天
    "important": 90,      // 重要数据90天
    "critical": 365       // 关键数据1年
  }
  
  let cleanup_schedules = {
    "hourly": 3600,       // 每小时清理
    "daily": 86400,       // 每日清理
    "weekly": 604800      // 每周清理
  }
  
  let cleanup_strategies = ["immediate", "batch", "gradual"]
  let max_cleanup_batch_size = 10000
  
  // 验证配置
  assert_eq(cleanup_policies.size(), 5)
  assert_eq(cleanup_schedules.size(), 3)
  assert_eq(cleanup_strategies.length(), 3)
  
  // 清理记录
  type CleanupRecord = {
    record_id: String,
    data_type: String,
    category: String,
    created_time: Int,
    size_bytes: Int,
    access_count: Int,
    last_access_time: Int,
    cleanup_eligible: Bool,
    cleanup_reason: String
  }
  
  // 清理操作结果
  type CleanupResult = {
    cleanup_id: String,
    strategy: String,
    records_processed: Int,
    records_deleted: Int,
    space_freed_bytes: Int,
    cleanup_time_ms: Int,
    success: Bool
  }
  
  // 清理统计
  type CleanupStats = {
    total_records: Int,
    eligible_records: Int,
    cleaned_records: Int,
    space_freed_total: Int,
    average_cleanup_time: Int,
    cleanup_efficiency: Double
  }
  
  // 判断是否有资格清理
  let is_cleanup_eligible = fn(record: CleanupRecord, current_time: Int) -> (Bool, String) {
    let age_days = (current_time - record.created_time) / 86400
    let retention_days = cleanup_policies[record.category]
    let days_since_access = (current_time - record.last_access_time) / 86400
    
    if age_days >= retention_days {
      (true, "Retention period exceeded")
    } else if record.access_count == 0 and days_since_access >= 7 {
      (true, "Unused data")
    } else if record.size_bytes > 100 * 1024 * 1024 and age_days >= retention_days / 2 {
      (true, "Large old data")
    } else {
      (false, "")
    }
  }
  
  // 模拟清理操作
  let execute_cleanup = fn(records: Array[CleanupRecord], strategy: String, current_time: Int) -> CleanupResult {
    let eligible_records = records.filter(fn(r) { r.cleanup_eligible })
    let mut records_to_delete = []
    
    match strategy {
      "immediate" => {
        // 立即删除所有符合条件的记录
        records_to_delete = eligible_records
      }
      "batch" => {
        // 批量删除，限制批次大小
        let mut i = 0
        while i < min(max_cleanup_batch_size, eligible_records.length()) {
          records_to_delete.push(eligible_records[i])
          i = i + 1
        }
      }
      "gradual" => {
        // 渐进式删除，只删除最老的记录
        let sorted_records = eligible_records.sort_by(fn(a, b) { a.created_time - b.created_time })
        let gradual_size = eligible_records.length() / 4  // 删除25%
        let mut i = 0
        while i < min(gradual_size, sorted_records.length()) {
          records_to_delete.push(sorted_records[i])
          i = i + 1
        }
      }
      _ => ()
    }
    
    // 计算清理时间和空间
    let base_cleanup_time = records_to_delete.length() * 10  // 每条记录10ms
    let strategy_factor = match strategy {
      "immediate" => 1.5,
      "batch" => 1.0,
      "gradual" => 0.7,
      _ => 1.0
    }
    
    let cleanup_time = (base_cleanup_time.to_double() * strategy_factor).to_int()
    let space_freed = records_to_delete.fold(0, fn(acc, r) { acc + r.size_bytes })
    
    // 模拟成功率
    let success_probability = match strategy {
      "immediate" => 0.95,
      "batch" => 0.98,
      "gradual" => 0.99,
      _ => 0.95
    }
    
    let random_value = (current_time % 100)
    let success = random_value < (success_probability * 100.0).to_int()
    
    CleanupResult {
      cleanup_id: "cleanup_" + strategy + "_" + current_time.to_string(),
      strategy: strategy,
      records_processed: records.length(),
      records_deleted: if success { records_to_delete.length() } else { 0 },
      space_freed_bytes: if success { space_freed } else { 0 },
      cleanup_time_ms: cleanup_time,
      success: success
    }
  }
  
  // 生成清理记录
  let generate_cleanup_records = fn(count: Int, base_time: Int) -> Array[CleanupRecord] {
    let categories = ["temporary", "debug", "normal", "important", "critical"]
    let data_types = ["logs", "metrics", "traces", "events"]
    let mut records = []
    
    let mut i = 0
    while i < count {
      let category = categories[i % categories.length()]
      let data_type = data_types[i % data_types.length()]
      let created_time = base_time - (i * 86400)  // 每天一条记录
      let size = 1024 + (i % 100) * 512  // 1KB-52KB
      let access_count = max(0, 50 - i / 10)  // 越老访问次数越少
      let last_access = created_time + (access_count * 3600)
      
      let record = CleanupRecord {
        record_id: "cleanup_" + i.to_string(),
        data_type: data_type,
        category: category,
        created_time: created_time,
        size_bytes: size,
        access_count: access_count,
        last_access_time: last_access,
        cleanup_eligible: false,
        cleanup_reason: ""
      }
      
      records.push(record)
      i = i + 1
    }
    
    records
  }
  
  // 生成测试记录
  let current_time = 1640995200
  let cleanup_records = generate_cleanup_records(365, current_time)  // 1年的数据
  
  // 验证记录生成
  assert_eq(cleanup_records.length(), 365)
  
  // 标记符合条件的记录
  let mut eligible_records = []
  let mut updated_records = []
  
  let mut i = 0
  while i < cleanup_records.length() {
    let record = cleanup_records[i]
    let (eligible, reason) = is_cleanup_eligible(record, current_time)
    
    let updated_record = CleanupRecord {
      record_id: record.record_id,
      data_type: record.data_type,
      category: record.category,
      created_time: record.created_time,
      size_bytes: record.size_bytes,
      access_count: record.access_count,
      last_access_time: record.last_access_time,
      cleanup_eligible: eligible,
      cleanup_reason: reason
    }
    
    if eligible {
      eligible_records.push(updated_record)
    }
    
    updated_records.push(updated_record)
    i = i + 1
  }
  
  // 验证符合条件的记录
  assert_eq(eligible_records.length() > 0, true)
  
  // 执行不同策略的清理测试
  let mut cleanup_results = []
  
  let mut j = 0
  while j < cleanup_strategies.length() {
    let strategy = cleanup_strategies[j]
    let result = execute_cleanup(updated_records, strategy, current_time)
    cleanup_results.push(result)
    j = j + 1
  }
  
  // 验证清理结果
  assert_eq(cleanup_results.length(), 3)
  
  // 统计清理效果
  let mut total_deleted = 0
  let mut total_space_freed = 0
  let mut total_cleanup_time = 0
  let mut successful_cleanups = 0
  
  let mut k = 0
  while k < cleanup_results.length() {
    let result = cleanup_results[k]
    
    if result.success {
      successful_cleanups = successful_cleanups + 1
      total_deleted = total_deleted + result.records_deleted
      total_space_freed = total_space_freed + result.space_freed_bytes
      total_cleanup_time = total_cleanup_time + result.cleanup_time_ms
    }
    
    k = k + 1
  }
  
  // 验证清理统计
  assert_eq(successful_cleanups > 0, true)
  assert_eq(total_deleted > 0, true)
  assert_eq(total_space_freed > 0, true)
  assert_eq(total_cleanup_time > 0, true)
  
  // 验证不同策略的效果
  let immediate_result = cleanup_results.filter(fn(r) { r.strategy == "immediate" })[0]
  let batch_result = cleanup_results.filter(fn(r) { r.strategy == "batch" })[0]
  let gradual_result = cleanup_results.filter(fn(r) { r.strategy == "gradual" })[0]
  
  // 立即清理应该删除最多记录
  assert_eq(immediate_result.records_deleted >= batch_result.records_deleted, true)
  assert_eq(batch_result.records_deleted >= gradual_result.records_deleted, true)
  
  // 渐进式清理应该最快
  assert_eq(gradual_result.cleanup_time_ms <= batch_result.cleanup_time_ms, true)
  assert_eq(batch_result.cleanup_time_ms <= immediate_result.cleanup_time_ms, true)
  
  // 验证不同类别的清理情况
  let temporary_records = eligible_records.filter(fn(r) { r.category == "temporary" })
  let critical_records = eligible_records.filter(fn(r) { r.category == "critical" })
  
  // 临时数据应该更容易被清理
  assert_eq(temporary_records.length() >= critical_records.length(), true)
}

test "telemetry_smart_cleanup" {
  // 测试遥测数据智能清理
  
  // 智能清理配置
  let data_value_scores = {
    "high": 0.8,        // 高价值数据
    "medium": 0.5,      // 中等价值数据
    "low": 0.2          // 低价值数据
  }
  
  let access_patterns = {
    "hot": 0.9,         // 热点数据
    "warm": 0.6,        // 温数据
    "cold": 0.3         // 冷数据
  }
  
  let storage_pressure = {
    "low": 0.2,         // 低存储压力
    "medium": 0.5,      // 中等存储压力
    "high": 0.8         // 高存储压力
  }
  
  // 验证配置
  assert_eq(data_value_scores.size(), 3)
  assert_eq(access_patterns.size(), 3)
  assert_eq(storage_pressure.size(), 3)
  
  // 智能清理记录
  type SmartCleanupRecord = {
    record_id: String,
    data_type: String,
    value_score: Double,
    access_pattern: String,
    access_frequency: Double,
    age_days: Int,
    size_mb: Double,
    cleanup_priority: Double,
    recommended_action: String
  }
  
  // 智能清理结果
  type SmartCleanupResult {
    records_analyzed: Int,
    records_cleaned: Int,
    space_freed_mb: Double,
    value_preserved_score: Double,
    cleanup_efficiency: Double
  }
  
  // 计算数据价值分数
  let calculate_value_score = fn(data_type: String, error_rate: Double, business_impact: Double) -> Double {
    let base_value = match data_type {
      "error_logs" => data_value_scores["high"],
      "security_events" => data_value_scores["high"],
      "performance_metrics" => data_value_scores["medium"],
      "debug_logs" => data_value_scores["low"],
      "access_logs" => data_value_scores["medium"],
      _ => data_value_scores["low"]
    }
    
    let quality_factor = max(0.5, 1.0 - error_rate)
    let impact_factor = min(1.0, business_impact)
    
    base_value * quality_factor * impact_factor
  }
  
  // 计算访问模式
  let calculate_access_pattern = fn(recent_access: Int, total_access: Int, age_days: Int) -> String {
    let access_rate = if age_days > 0 { total_access.to_double() / age_days.to_double() } else { 0.0 }
    let recent_ratio = if total_access > 0 { recent_access.to_double() / total_access.to_double() } else { 0.0 }
    
    if access_rate > 1.0 and recent_ratio > 0.7 {
      "hot"
    } else if access_rate > 0.1 and recent_ratio > 0.3 {
      "warm"
    } else {
      "cold"
    }
  }
  
  // 计算清理优先级
  let calculate_cleanup_priority = fn(value_score: Double, access_pattern: String, age_days: Int, size_mb: Double, storage_pressure_level: Double) -> Double {
    let pattern_score = access_patterns[access_pattern]
    let age_factor = min(1.0, age_days.to_double() / 365.0)
    let size_factor = min(1.0, size_mb / 1000.0)  // GB级别
    
    // 优先级 = (1 - 价值) * (1 - 访问热度) * 年龄因子 * 大小因子 * 存储压力
    let priority = (1.0 - value_score) * (1.0 - pattern_score) * age_factor * size_factor * storage_pressure_level
    priority
  }
  
  // 智能清理决策
  let make_cleanup_decision = fn(priority: Double, storage_pressure: String) -> String {
    let pressure_level = storage_pressure[storage_pressure]
    
    if priority > 0.8 * pressure_level {
      "delete_immediately"
    } else if priority > 0.6 * pressure_level {
      "archive_then_delete"
    } else if priority > 0.4 * pressure_level {
      "compress_and_keep"
    } else {
      "keep"
    }
  }
  
  // 生成智能清理记录
  let generate_smart_cleanup_records = fn(count: Int, base_time: Int) -> Array[SmartCleanupRecord] {
    let data_types = ["error_logs", "security_events", "performance_metrics", "debug_logs", "access_logs"]
    let mut records = []
    
    let mut i = 0
    while i < count {
      let data_type = data_types[i % data_types.length()]
      let created_time = base_time - (i * 86400)  // 每天一条记录
      let age_days = (base_time - created_time) / 86400
      
      let recent_access = max(0, 50 - i)
      let total_access = recent_access + (i % 100)
      let error_rate = (i % 20).to_double() / 100.0
      let business_impact = (i % 10).to_double() / 10.0
      let size_mb = 10.0 + (i % 100).to_double()  // 10MB-110MB
      
      let value_score = calculate_value_score(data_type, error_rate, business_impact)
      let access_pattern = calculate_access_pattern(recent_access, total_access, age_days)
      let access_frequency = if age_days > 0 { total_access.to_double() / age_days.to_double() } else { 0.0 }
      
      let storage_pressure_level = storage_pressure["medium"]  // 默认中等存储压力
      let cleanup_priority = calculate_cleanup_priority(value_score, access_pattern, age_days, size_mb, storage_pressure_level)
      let recommended_action = make_cleanup_decision(cleanup_priority, "medium")
      
      let record = SmartCleanupRecord {
        record_id: "smart_" + i.to_string(),
        data_type: data_type,
        value_score: value_score,
        access_pattern: access_pattern,
        access_frequency: access_frequency,
        age_days: age_days,
        size_mb: size_mb,
        cleanup_priority: cleanup_priority,
        recommended_action: recommended_action
      }
      
      records.push(record)
      i = i + 1
    }
    
    records
  }
  
  // 生成测试记录
  let current_time = 1640995200
  let smart_records = generate_smart_cleanup_records(200, current_time)
  
  // 验证记录生成
  assert_eq(smart_records.length(), 200)
  
  // 执行智能清理分析
  let mut records_to_delete = []
  let mut records_to_archive = []
  let mut records_to_compress = []
  let mut records_to_keep = []
  
  let mut total_value_preserved = 0.0
  let mut total_size = 0.0
  let mut space_freed = 0.0
  
  let mut i = 0
  while i < smart_records.length() {
    let record = smart_records[i]
    total_size = total_size + record.size_mb
    total_value_preserved = total_value_preserved + record.value_score * record.size_mb
    
    match record.recommended_action {
      "delete_immediately" => {
        records_to_delete.push(record)
        space_freed = space_freed + record.size_mb
      }
      "archive_then_delete" => {
        records_to_archive.push(record)
        space_freed = space_freed + record.size_mb * 0.8  // 归档后删除节省80%空间
      }
      "compress_and_keep" => {
        records_to_compress.push(record)
        space_freed = space_freed + record.size_mb * 0.3  // 压缩节省30%空间
      }
      "keep" => {
        records_to_keep.push(record)
      }
      _ => ()
    }
    
    i = i + 1
  }
  
  // 计算智能清理结果
  let value_preserved_score = if total_size > 0 {
    (total_value_preserved - space_freed * 0.1) / total_size  // 假设被清理的数据价值较低
  } else { 0.0 }
  
  let cleanup_efficiency = if total_size > 0 {
    (space_freed / total_size) * 100.0
  } else { 0.0 }
  
  let smart_result = SmartCleanupResult {
    records_analyzed: smart_records.length(),
    records_cleaned: records_to_delete.length() + records_to_archive.length(),
    space_freed_mb: space_freed,
    value_preserved_score: value_preserved_score,
    cleanup_efficiency: cleanup_efficiency
  }
  
  // 验证智能清理结果
  assert_eq(smart_result.records_analyzed, 200)
  assert_eq(smart_result.records_cleaned > 0, true)
  assert_eq(smart_result.space_freed_mb > 0.0, true)
  assert_eq(smart_result.value_preserved_score >= 0.0, true)
  assert_eq(smart_result.value_preserved_score <= 1.0, true)
  assert_eq(smart_result.cleanup_efficiency > 0.0, true)
  
  // 验证高价值数据保护
  let high_value_records = smart_records.filter(fn(r) { r.value_score > 0.7 })
  let high_value_deleted = high_value_records.filter(fn(r) { r.recommended_action == "delete_immediately" }).length()
  
  if high_value_records.length() > 0 {
    let high_value_deletion_rate = high_value_deleted.to_double() / high_value_records.length().to_double()
    assert_eq(high_value_deletion_rate < 0.3, true)  // 高价值数据删除率应该低于30%
  }
  
  // 验证热点数据保护
  let hot_records = smart_records.filter(fn(r) { r.access_pattern == "hot" })
  let hot_deleted = hot_records.filter(fn(r) { r.recommended_action == "delete_immediately" }).length()
  
  if hot_records.length() > 0 {
    let hot_deletion_rate = hot_deleted.to_double() / hot_records.length().to_double()
    assert_eq(hot_deletion_rate < 0.2, true)  // 热点数据删除率应该低于20%
  }
  
  // 验证清理优先级排序
  let sorted_by_priority = smart_records.sort_by(fn(a, b) { 
    if a.cleanup_priority > b.cleanup_priority { -1 } else if a.cleanup_priority < b.cleanup_priority { 1 } else { 0 }
  })
  
  // 最高优先级的记录应该被建议删除
  if sorted_by_priority.length() > 0 {
    let highest_priority = sorted_by_priority[0]
    assert_eq(highest_priority.recommended_action == "delete_immediately" or 
              highest_priority.recommended_action == "archive_then_delete", true)
  }
  
  // 验证不同数据类型的处理差异
  let error_logs = smart_records.filter(fn(r) { r.data_type == "error_logs" })
  let debug_logs = smart_records.filter(fn(r) { r.data_type == "debug_logs" })
  
  if error_logs.length() > 0 and debug_logs.length() > 0 {
    let error_avg_priority = error_logs.fold(0.0, fn(acc, r) { acc + r.cleanup_priority }) / error_logs.length().to_double()
    let debug_avg_priority = debug_logs.fold(0.0, fn(acc, r) { acc + r.cleanup_priority }) / debug_logs.length().to_double()
    
    // 错误日志应该比调试日志有更低的清理优先级
    assert_eq(error_avg_priority < debug_avg_priority, true)
  }
}

test "telemetry_compliance_cleanup" {
  // 测试遥测数据合规清理
  
  // 合规清理配置
  let compliance_requirements = {
    "gdpr": { 
      "right_to_be_forgotten": true,
      "data_minimization": true,
      "retention_limits": 2555,  // 7年
      "anonymization_before_delete": true
    },
    "hipaa": {
      "minimum_necessary": true,
      "retention_requirements": 2555,  // 7年
      "secure_disposal": true,
      "audit_trail": true
    },
    "sox": {
      "retention_period": 2555,  // 7年
      "immutable_records": true,
      "tamper_evidence": true,
      "sign_off_required": true
    },
    "pci": {
      "card_data_protection": true,
      "retention_period": 365,   // 1年
      "secure_deletion": true,
      "compliance_validation": true
    }
  }
  
  let data_sensitivity = {
    "pii": { "gdpr": true, "hipaa": false, "sox": false, "pci": false },
    "phi": { "gdpr": true, "hipaa": true, "sox": false, "pci": false },
    "financial": { "gdpr": false, "hipaa": false, "sox": true, "pci": true },
    "authentication": { "gdpr": true, "hipaa": false, "sox": true, "pci": true }
  }
  
  // 验证配置
  assert_eq(compliance_requirements.size(), 4)
  assert_eq(data_sensitivity.size(), 4)
  
  // 合规清理记录
  type ComplianceCleanupRecord {
    record_id: String,
    data_type: String,
    sensitivity: String,
    created_time: Int,
    applicable_regulations: Array[String],
    compliance_status: String,
    cleanup_constraints: Array[String],
    approved_for_deletion: Bool
  }
  
  // 合规清理结果
  type ComplianceCleanupResult {
    total_records: Int,
    compliant_deletions: Int,
    non_compliant_deletions: Int,
    blocked_deletions: Int,
    compliance_score: Double
  }
  
  // 检查合规约束
  let check_compliance_constraints = fn(record: ComplianceCleanupRecord, current_time: Int) -> Array[String] {
    let mut constraints = []
    
    let mut i = 0
    while i < record.applicable_regulations.length() {
      let regulation = record.applicable_regulations[i]
      let requirement = compliance_requirements[regulation]
      let age_days = (current_time - record.created_time) / 86400
      
      match regulation {
        "gdpr" => {
          if age_days < requirement["retention_limits"] {
            constraints.push("GDPR: Retention period not exceeded")
          }
          if requirement["anonymization_before_delete"] and not record.cleanup_constraints.contains("anonymized") {
            constraints.push("GDPR: Data must be anonymized before deletion")
          }
        }
        "hipaa" => {
          if age_days < requirement["retention_requirements"] {
            constraints.push("HIPAA: Retention requirements not met")
          }
          if requirement["secure_disposal"] and not record.cleanup_constraints.contains("secure_disposal") {
            constraints.push("HIPAA: Secure disposal method required")
          }
        }
        "sox" => {
          if age_days < requirement["retention_period"] {
            constraints.push("SOX: Retention period not met")
          }
          if requirement["immutable_records"] {
            constraints.push("SOX: Records are immutable")
          }
        }
        "pci" => {
          if age_days < requirement["retention_period"] {
            constraints.push("PCI: Retention period not met")
          }
          if requirement["secure_deletion"] and not record.cleanup_constraints.contains("secure_deletion") {
            constraints.push("PCI: Secure deletion required")
          }
        }
        _ => ()
      }
      
      i = i + 1
    }
    
    constraints
  }
  
  // 获取适用的法规
  let get_applicable_regulations = fn(sensitivity: String) -> Array[String] {
    let mut regulations = []
    let regulation_entries = ["gdpr", "hipaa", "sox", "pci"]
    
    let mut i = 0
    while i < regulation_entries.length() {
      let regulation = regulation_entries[i]
      if data_sensitivity[sensitivity][regulation] {
        regulations.push(regulation)
      }
      i = i + 1
    }
    
    regulations
  }
  
  // 生成合规清理记录
  let generate_compliance_cleanup_records = fn(count: Int, base_time: Int) -> Array[ComplianceCleanupRecord] {
    let sensitivities = ["pii", "phi", "financial", "authentication"]
    let data_types = ["user_data", "health_records", "transactions", "auth_logs"]
    let mut records = []
    
    let mut i = 0
    while i < count {
      let sensitivity = sensitivities[i % sensitivities.length()]
      let data_type = data_types[i % data_types.length()]
      let created_time = base_time - (i * 86400)  // 每天一条记录
      let regulations = get_applicable_regulations(sensitivity)
      
      // 应用一些清理约束
      let mut cleanup_constraints = []
      if regulations.contains("gdpr") and i % 3 == 0 { cleanup_constraints.push("anonymized") }
      if regulations.contains("hipaa") and i % 2 == 0 { cleanup_constraints.push("secure_disposal") }
      if regulations.contains("pci") and i % 4 == 0 { cleanup_constraints.push("secure_deletion") }
      
      let record = ComplianceCleanupRecord {
        record_id: "compliance_cleanup_" + i.to_string(),
        data_type: data_type,
        sensitivity: sensitivity,
        created_time: created_time,
        applicable_regulations: regulations,
        compliance_status: "pending_review",
        cleanup_constraints: cleanup_constraints,
        approved_for_deletion: false
      }
      
      records.push(record)
      i = i + 1
    }
    
    records
  }
  
  // 生成测试记录
  let current_time = 1640995200
  let compliance_records = generate_compliance_cleanup_records(150, current_time)
  
  // 验证记录生成
  assert_eq(compliance_records.length(), 150)
  
  // 执行合规清理检查
  let mut updated_records = []
  let mut compliant_deletions = 0
  let mut non_compliant_deletions = 0
  let mut blocked_deletions = 0
  
  let mut i = 0
  while i < compliance_records.length() {
    let record = compliance_records[i]
    let constraints = check_compliance_constraints(record, current_time)
    
    // 决定是否可以删除
    let can_delete = constraints.length() == 0
    let approved = can_delete and (record.record_id.length() % 10) < 8  // 80%批准率
    
    if approved {
      if constraints.length() == 0 {
        compliant_deletions = compliant_deletions + 1
      } else {
        non_compliant_deletions = non_compliant_deletions + 1
      }
    } else if constraints.length() > 0 {
      blocked_deletions = blocked_deletions + 1
    }
    
    let updated_record = ComplianceCleanupRecord {
      record_id: record.record_id,
      data_type: record.data_type,
      sensitivity: record.sensitivity,
      created_time: record.created_time,
      applicable_regulations: record.applicable_regulations,
      compliance_status: if approved { "approved_for_deletion" } else { "blocked" },
      cleanup_constraints: record.cleanup_constraints,
      approved_for_deletion: approved
    }
    
    updated_records.push(updated_record)
    i = i + 1
  }
  
  // 计算合规分数
  let total_deletion_attempts = compliant_deletions + non_compliant_deletions
  let compliance_score = if total_deletion_attempts > 0 {
    compliant_deletions.to_double() / total_deletion_attempts.to_double()
  } else { 1.0 }
  
  let cleanup_result = ComplianceCleanupResult {
    total_records: updated_records.length(),
    compliant_deletions: compliant_deletions,
    non_compliant_deletions: non_compliant_deletions,
    blocked_deletions: blocked_deletions,
    compliance_score: compliance_score
  }
  
  // 验证合规清理结果
  assert_eq(cleanup_result.total_records, 150)
  assert_eq(cleanup_result.compliant_deletions + cleanup_result.non_compliant_deletions + cleanup_result.blocked_deletions, 150)
  assert_eq(cleanup_result.compliance_score >= 0.0, true)
  assert_eq(cleanup_result.compliance_score <= 1.0, true)
  
  // 验证合规分数
  assert_eq(cleanup_result.compliance_score > 0.8, true)  // 合规分数应该较高
  
  // 验证敏感数据的特殊处理
  let pii_records = updated_records.filter(fn(r) { r.sensitivity == "pii" })
  let pii_blocked = pii_records.filter(fn(r) { r.compliance_status == "blocked" }).length()
  
  if pii_records.length() > 0 {
    let pii_blocking_rate = pii_blocked.to_double() / pii_records.length().to_double()
    assert_eq(pii_blocking_rate > 0.3, true)  // PII数据应该有较高的阻止率
  }
  
  // 验证不同法规的约束效果
  let gdpr_records = updated_records.filter(fn(r) { r.applicable_regulations.contains("gdpr") })
  let sox_records = updated_records.filter(fn(r) { r.applicable_regulations.contains("sox") })
  
  if gdpr_records.length() > 0 and sox_records.length() > 0 {
    let gdpr_blocked = gdpr_records.filter(fn(r) { r.compliance_status == "blocked" }).length()
    let sox_blocked = sox_records.filter(fn(r) { r.compliance_status == "blocked" }).length()
    
    let gdpr_blocking_rate = gdpr_blocked.to_double() / gdpr_records.length().to_double()
    let sox_blocking_rate = sox_blocked.to_double() / sox_records.length().to_double()
    
    // SOX记录由于不可变性应该有更高的阻止率
    assert_eq(sox_blocking_rate >= gdpr_blocking_rate, true)
  }
  
  // 验证时间对合规决策的影响
  let old_records = updated_records.filter(fn(r) { (current_time - r.created_time) / 86400 > 365 })
  let recent_records = updated_records.filter(fn(r) { (current_time - r.created_time) / 86400 <= 365 })
  
  if old_records.length() > 0 and recent_records.length() > 0 {
    let old_approved = old_records.filter(fn(r) { r.approved_for_deletion }).length()
    let recent_approved = recent_records.filter(fn(r) { r.approved_for_deletion }).length()
    
    let old_approval_rate = old_approved.to_double() / old_records.length().to_double()
    let recent_approval_rate = recent_approved.to_double() / recent_records.length().to_double()
    
    // 老记录应该有更高的批准率
    assert_eq(old_approval_rate >= recent_approval_rate, true)
  }
}