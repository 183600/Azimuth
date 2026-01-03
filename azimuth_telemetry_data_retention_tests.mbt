// Azimuth 遥测数据保留策略测试用例
// 专注于测试数据生命周期管理和保留策略

// 测试1: 基于时间的数据保留策略
test "基于时间的数据保留策略" {
  // 定义数据保留策略
  type RetentionPolicy = {
    name: String,
    retention_days: Int,
    auto_cleanup: Bool,
    archive_before_delete: Bool
  }
  
  // 定义遥测数据记录
  type TelemetryRecord = {
    id: String,
    timestamp: Int,
    data_type: String,
    size_bytes: Int,
    metadata: Array[(String, String)]
  }
  
  // 创建数据保留管理器
  let retention_manager = fn(policies: Array[RetentionPolicy]) {
    fn(records: Array[TelemetryRecord], current_time: Int) {
      let mut to_delete = []
      let mut to_archive = []
      let mut to_keep = []
      
      for record in records {
        let record_age_days = (current_time - record.timestamp) / (24 * 60 * 60 * 1000)
        
        // 查找适用的保留策略
        let applicable_policy = policies.find(fn(policy) {
          // 简化策略匹配：基于数据类型
          match record.data_type {
            "metric" => policy.name == "metrics_retention"
            "log" => policy.name == "logs_retention"
            "trace" => policy.name == "traces_retention"
            _ => false
          }
        })
        
        match applicable_policy {
          Some(policy) => {
            if record_age_days > policy.retention_days {
              if policy.archive_before_delete {
                to_archive = to_archive.push(record)
              }
              to_delete = to_delete.push(record)
            } else {
              to_keep = to_keep.push(record)
            }
          }
          None => {
            // 没有找到策略，默认保留
            to_keep = to_keep.push(record)
          }
        }
      }
      
      {
        records_to_delete: to_delete,
        records_to_archive: to_archive,
        records_to_keep: to_keep,
        total_processed: records.length()
      }
    }
  }
  
  // 定义保留策略
  let policies = [
    { name: "metrics_retention", retention_days: 30, auto_cleanup: true, archive_before_delete: true },
    { name: "logs_retention", retention_days: 90, auto_cleanup: true, archive_before_delete: false },
    { name: "traces_retention", retention_days: 7, auto_cleanup: true, archive_before_delete: true }
  ]
  
  let manager = retention_manager(policies)
  
  // 创建测试数据（假设当前时间为2025年1月3日）
  let current_time = 1735881600000  // 2025-01-03的时间戳
  let day_in_ms = 24 * 60 * 60 * 1000
  
  let test_records = [
    // 指标数据
    { id: "metric1", timestamp: current_time - (10 * day_in_ms), data_type: "metric", size_bytes: 1024, metadata: [] },  // 10天前
    { id: "metric2", timestamp: current_time - (35 * day_in_ms), data_type: "metric", size_bytes: 2048, metadata: [] },  // 35天前
    { id: "metric3", timestamp: current_time - (60 * day_in_ms), data_type: "metric", size_bytes: 1536, metadata: [] },  // 60天前
    
    // 日志数据
    { id: "log1", timestamp: current_time - (30 * day_in_ms), data_type: "log", size_bytes: 512, metadata: [] },        // 30天前
    { id: "log2", timestamp: current_time - (95 * day_in_ms), data_type: "log", size_bytes: 768, metadata: [] },        // 95天前
    
    // 追踪数据
    { id: "trace1", timestamp: current_time - (5 * day_in_ms), data_type: "trace", size_bytes: 3072, metadata: [] },     // 5天前
    { id: "trace2", timestamp: current_time - (10 * day_in_ms), data_type: "trace", size_bytes: 4096, metadata: [] }    // 10天前
  ]
  
  // 应用保留策略
  let result = manager(test_records, current_time)
  
  // 验证结果
  assert_eq(result.total_processed, 7)
  
  // 验证删除的记录
  assert_eq(result.records_to_delete.length(), 4)  // metric2, metric3, log2, trace2
  
  // 验证归档的记录
  assert_eq(result.records_to_archive.length(), 3)  // metric2, metric3, trace2 (这些设置了archive_before_delete)
  
  // 验证保留的记录
  assert_eq(result.records_to_keep.length(), 3)  // metric1, log1, trace1
  
  // 验证具体记录
  let deleted_ids = result.records_to_delete.map(fn(r) { r.id })
  assert_true(deleted_ids.contains("metric2"))
  assert_true(deleted_ids.contains("metric3"))
  assert_true(deleted_ids.contains("log2"))
  assert_true(deleted_ids.contains("trace2"))
  
  let archived_ids = result.records_to_archive.map(fn(r) { r.id })
  assert_true(archived_ids.contains("metric2"))
  assert_true(archived_ids.contains("metric3"))
  assert_true(archived_ids.contains("trace2"))
  assert_false(archived_ids.contains("log2"))  // 日志不归档
}

// 测试2: 基于存储空间的数据保留策略
test "基于存储空间的数据保留策略" {
  // 定义存储配额
  type StorageQuota = {
    max_size_mb: Int,
    warning_threshold_percent: Float,
    cleanup_strategy: String  // "oldest_first", "lowest_priority", "random"
  }
  
  // 创建存储空间管理器
  let storage_manager = fn(quota: StorageQuota) {
    fn(records: Array[TelemetryRecord>) {
      let total_size = records.reduce(fn(acc, record) { acc + record.size_bytes }, 0)
      let total_size_mb = total_size / (1024 * 1024)
      let usage_percent = (total_size_mb as Float) / (quota.max_size_mb as Float) * 100.0
      
      let mut records_to_delete = []
      let mut final_records = records
      
      if usage_percent > 100.0 {
        // 超出配额，需要清理
        let bytes_to_free = total_size - (quota.max_size_mb * 1024 * 1024)
        let mut freed_bytes = 0
        
        // 根据策略排序记录
        let sorted_records = match quota.cleanup_strategy {
          "oldest_first" => final_records.sort(fn(a, b) { if a.timestamp < b.timestamp { -1 } else if a.timestamp > b.timestamp { 1 } else { 0 } })
          "lowest_priority" => final_records.sort(fn(a, b) { 
            let priority_a = match a.data_type { "trace" => 1, "metric" => 2, "log" => 3, _ => 4 }
            let priority_b = match b.data_type { "trace" => 1, "metric" => 2, "log" => 3, _ => 4 }
            if priority_a < priority_b { -1 } else if priority_a > priority_b { 1 } else { 0 }
          })
          _ => final_records  // 随机或默认顺序
        }
        
        // 删除记录直到释放足够空间
        for record in sorted_records {
          if freed_bytes >= bytes_to_free {
            break
          }
          records_to_delete = records_to_delete.push(record)
          freed_bytes = freed_bytes + record.size_bytes
        }
        
        // 从最终记录中移除已删除的记录
        let deleted_ids = records_to_delete.map(fn(r) { r.id })
        final_records = final_records.filter(fn(r) { not deleted_ids.contains(r.id) })
      }
      
      {
        original_size_mb: total_size_mb,
        usage_percent: usage_percent,
        records_to_delete: records_to_delete,
        remaining_records: final_records,
        freed_bytes: records_to_delete.reduce(fn(acc, r) { acc + r.size_bytes }, 0),
        is_over_quota: usage_percent > 100.0,
        is_near_warning: usage_percent > quota.warning_threshold_percent
      }
    }
  }
  
  // 创建存储配额
  let quota = {
    max_size_mb: 10,  // 10MB配额
    warning_threshold_percent: 80.0,
    cleanup_strategy: "oldest_first"
  }
  
  let manager = storage_manager(quota)
  
  // 创建测试数据（总大小超过10MB）
  let test_records = [
    { id: "record1", timestamp: 1000, data_type: "trace", size_bytes: 4 * 1024 * 1024, metadata: [] },    // 4MB
    { id: "record2", timestamp: 2000, data_type: "metric", size_bytes: 3 * 1024 * 1024, metadata: [] },  // 3MB
    { id: "record3", timestamp: 3000, data_type: "log", size_bytes: 2 * 1024 * 1024, metadata: [] },     // 2MB
    { id: "record4", timestamp: 4000, data_type: "trace", size_bytes: 2 * 1024 * 1024, metadata: [] },    // 2MB
    { id: "record5", timestamp: 5000, data_type: "metric", size_bytes: 1 * 1024 * 1024, metadata: [] }    // 1MB
  ]
  
  // 总大小：12MB，超出配额
  let result = manager(test_records)
  
  // 验证结果
  assert_eq(result.original_size_mb, 12)  // 12MB总大小
  assert_true(result.is_over_quota)       // 超出配额
  assert_true(result.usage_percent > 100.0)
  
  // 验证删除了记录
  assert_true(result.records_to_delete.length() > 0)
  
  // 验证释放了足够空间
  let remaining_size = result.remaining_records.reduce(fn(acc, r) { acc + r.size_bytes }, 0)
  let remaining_size_mb = remaining_size / (1024 * 1024)
  assert_true(remaining_size_mb <= quota.max_size_mb)
  
  // 验证按时间戳排序（最旧的先删除）
  let deleted_timestamps = result.records_to_delete.map(fn(r) { r.timestamp })
  assert_true(deleted_timestamps.is_sorted(fn(a, b) { a <= b }))
}

// 测试3: 分层数据保留策略
test "分层数据保留策略" {
  // 定义数据层
  enum DataLayer {
    Hot    // 热数据：频繁访问，快速存储
    Warm   // 温数据：偶尔访问，中等性能存储
    Cold   // 冷数据：很少访问，低成本存储
    Archive // 归档：长期保存，极低成本存储
  }
  
  // 定义分层保留规则
  type TierRule = {
    layer: DataLayer,
    max_age_days: Int,
    max_size_mb: Int,
    access_frequency_threshold: Int  // 每天访问次数
  }
  
  // 创建分层保留管理器
  let tiered_retention_manager = fn(rules: Array[TierRule]) {
    fn(records: Array[TelemetryRecord], current_time: Int, access_stats: Array[(String, Int)>) {
      let mut tiered_records = []
      
      for record in records {
        let record_age_days = (current_time - record.timestamp) / (24 * 60 * 60 * 1000)
        
        // 获取访问统计
        let access_count = match access_stats.find(fn(stat) { stat.0 == record.id }) {
          Some((_, count)) => count
          None => 0
        }
        
        // 确定数据层
        let determined_layer = 
          if record_age_days <= 7 && access_count >= 10 {
            DataLayer::Hot
          } else if record_age_days <= 30 && access_count >= 5 {
            DataLayer::Warm
          } else if record_age_days <= 365 && access_count >= 1 {
            DataLayer::Cold
          } else {
            DataLayer::Archive
          }
        
        tiered_records = tiered_records.push({
          record: record,
          layer: determined_layer,
          age_days: record_age_days,
          access_count: access_count
        })
      }
      
      // 按层分组
      let hot_records = tiered_records.filter(fn(tr) { match tr.layer { DataLayer::Hot => true, _ => false } })
      let warm_records = tiered_records.filter(fn(tr) { match tr.layer { DataLayer::Warm => true, _ => false } })
      let cold_records = tiered_records.filter(fn(tr) { match tr.layer { DataLayer::Cold => true, _ => false } })
      let archive_records = tiered_records.filter(fn(tr) { match tr.layer { DataLayer::Archive => true, _ => false } })
      
      {
        tiered_records: tiered_records,
        hot_records: hot_records,
        warm_records: warm_records,
        cold_records: cold_records,
        archive_records: archive_records,
        layer_distribution: [
          ("hot", hot_records.length()),
          ("warm", warm_records.length()),
          ("cold", cold_records.length()),
          ("archive", archive_records.length())
        ]
      }
    }
  }
  
  let manager = tiered_retention_manager([])
  
  // 创建测试数据
  let current_time = 1735881600000  // 2025-01-03
  let day_in_ms = 24 * 60 * 60 * 1000
  
  let test_records = [
    { id: "record1", timestamp: current_time - (2 * day_in_ms), data_type: "trace", size_bytes: 1024, metadata: [] },    // 2天前
    { id: "record2", timestamp: current_time - (5 * day_in_ms), data_type: "metric", size_bytes: 2048, metadata: [] },  // 5天前
    { id: "record3", timestamp: current_time - (10 * day_in_ms), data_type: "log", size_bytes: 1536, metadata: [] },    // 10天前
    { id: "record4", timestamp: current_time - (30 * day_in_ms), data_type: "trace", size_bytes: 3072, metadata: [] },  // 30天前
    { id: "record5", timestamp: current_time - (60 * day_in_ms), data_type: "metric", size_bytes: 4096, metadata: [] },  // 60天前
    { id: "record6", timestamp: current_time - (400 * day_in_ms), data_type: "log", size_bytes: 512, metadata: [] }      // 400天前
  ]
  
  // 访问统计（记录ID，每天访问次数）
  let access_stats = [
    ("record1", 15),  // 高频访问
    ("record2", 8),   // 中频访问
    ("record3", 3),   // 低频访问
    ("record4", 1),   // 很少访问
    ("record5", 0),   // 无访问
    ("record6", 0)    // 无访问
  ]
  
  // 应用分层策略
  let result = manager(test_records, current_time, access_stats)
  
  // 验证分层结果
  assert_eq(result.tiered_records.length(), 6)
  
  // 验证热数据层
  assert_eq(result.hot_records.length(), 2)  // record1, record2
  let hot_ids = result.hot_records.map(fn(tr) { tr.record.id })
  assert_true(hot_ids.contains("record1"))
  assert_true(hot_ids.contains("record2"))
  
  // 验证温数据层
  assert_eq(result.warm_records.length(), 1)  // record3
  assert_eq(result.warm_records[0].record.id, "record3")
  
  // 验证冷数据层
  assert_eq(result.cold_records.length(), 2)  // record4, record5
  let cold_ids = result.cold_records.map(fn(tr) { tr.record.id })
  assert_true(cold_ids.contains("record4"))
  assert_true(cold_ids.contains("record5"))
  
  // 验证归档层
  assert_eq(result.archive_records.length(), 1)  // record6
  assert_eq(result.archive_records[0].record.id, "record6")
  
  // 验证层分布
  let distribution = Map::from_array(result.layer_distribution)
  assert_eq(Map::get(distribution, "hot"), Some(2))
  assert_eq(Map::get(distribution, "warm"), Some(1))
  assert_eq(Map::get(distribution, "cold"), Some(2))
  assert_eq(Map::get(distribution, "archive"), Some(1))
}

// 测试4: 智能数据保留策略
test "智能数据保留策略" {
  // 定义数据重要性评分
  type ImportanceScore = {
    record_id: String,
    score: Float,
    factors: Array[(String, Float)]
  }
  
  // 创建智能保留管理器
  let intelligent_retention_manager = fn() {
    fn(records: Array[TelemetryRecord], access_patterns: Array[(String, Array[Int])>, business_value: Array[(String, Float)>) {
      let mut scored_records = []
      
      for record in records {
        let mut score = 0.0
        let mut factors = []
        
        // 因子1：访问频率（0-30分）
        let access_pattern = match access_patterns.find(fn(p) { p.0 == record.id }) {
          Some((_, pattern)) => pattern
          None => []
        }
        
        let recent_accesses = if access_pattern.length() >= 7 {
          access_pattern.slice(access_pattern.length() - 7, access_pattern.length())
        } else {
          access_pattern
        }
        
        let access_frequency = recent_accesses.reduce(fn(acc, count) { acc + count }, 0)
        let access_score = (access_frequency as Float) / 7.0 * 30.0  // 最多30分
        score = score + access_score
        factors = factors.push(("access_frequency", access_score))
        
        // 因子2：业务价值（0-40分）
        let business_score = match business_value.find(fn(bv) { bv.0 == record.data_type }) {
          Some((_, value)) => value * 40.0
          None => 10.0  // 默认值
        }
        score = score + business_score
        factors = factors.push(("business_value", business_score))
        
        // 因子3：数据新鲜度（0-20分）
        let current_time = 1735881600000  // 2025-01-03
        let age_days = (current_time - record.timestamp) / (24 * 60 * 60 * 1000)
        let freshness_score = if age_days <= 1 {
          20.0
        } else if age_days <= 7 {
          15.0
        } else if age_days <= 30 {
          10.0
        } else if age_days <= 90 {
          5.0
        } else {
          0.0
        }
        score = score + freshness_score
        factors = factors.push(("freshness", freshness_score))
        
        // 因子4：数据大小（负分，0-10分）
        let size_penalty = if record.size_bytes > 10 * 1024 * 1024 {  // > 10MB
          10.0
        } else if record.size_bytes > 5 * 1024 * 1024 {  // > 5MB
          5.0
        } else if record.size_bytes > 1 * 1024 * 1024 {  // > 1MB
          2.0
        } else {
          0.0
        }
        score = score - size_penalty
        factors = factors.push(("size_penalty", size_penalty))
        
        scored_records = scored_records.push({
          record: record,
          importance_score: score,
          factors: factors
        })
      }
      
      // 按重要性评分排序
      let sorted_records = scored_records.sort(fn(a, b) {
        if a.importance_score > b.importance_score { -1 }
        else if a.importance_score < b.importance_score { 1 }
        else { 0 }
      })
      
      // 模拟保留决策（保留前80%的数据）
      let keep_count = (sorted_records.length() as Float * 0.8) as Int
      let records_to_keep = sorted_records.slice(0, keep_count)
      let records_to_delete = sorted_records.slice(keep_count, sorted_records.length())
      
      {
        all_scored_records: sorted_records,
        records_to_keep: records_to_keep,
        records_to_delete: records_to_delete,
        retention_rate: (keep_count as Float) / (sorted_records.length() as Float),
        average_importance_score: sorted_records.reduce(fn(acc, sr) { acc + sr.importance_score }, 0.0) / (sorted_records.length() as Float)
      }
    }
  }
  
  let manager = intelligent_retention_manager()
  
  // 创建测试数据
  let current_time = 1735881600000  // 2025-01-03
  let day_in_ms = 24 * 60 * 60 * 1000
  
  let test_records = [
    { id: "record1", timestamp: current_time - (1 * day_in_ms), data_type: "trace", size_bytes: 512, metadata: [] },     // 新，小，高价值
    { id: "record2", timestamp: current_time - (5 * day_in_ms), data_type: "log", size_bytes: 2048, metadata: [] },     // 较新，中等，低价值
    { id: "record3", timestamp: current_time - (10 * day_in_ms), data_type: "metric", size_bytes: 1024, metadata: [] }, // 较旧，小，中价值
    { id: "record4", timestamp: current_time - (30 * day_in_ms), data_type: "trace", size_bytes: 15 * 1024 * 1024, metadata: [] }, // 旧，大，高价值
    { id: "record5", timestamp: current_time - (60 * day_in_ms), data_type: "log", size_bytes: 8 * 1024 * 1024, metadata: [] }   // 很旧，大，低价值
  ]
  
  // 访问模式（记录ID，每天访问次数数组）
  let access_patterns = [
    ("record1", [10, 15, 12, 8, 20, 18, 25]),  // 高频访问
    ("record2", [2, 3, 1, 4, 2, 3, 1]),        // 中低频访问
    ("record3", [5, 6, 4, 8, 7, 5, 6]),        // 中频访问
    ("record4", [15, 20, 18, 25, 22, 30, 28]), // 高频访问
    ("record5", [0, 1, 0, 1, 0, 0, 1])         // 极低频访问
  ]
  
  // 业务价值（数据类型，价值权重0-1）
  let business_value = [
    ("trace", 0.9),    // 高价值
    ("metric", 0.7),   // 中高价值
    ("log", 0.3)       // 低价值
  ]
  
  // 应用智能保留策略
  let result = manager(test_records, access_patterns, business_value)
  
  // 验证结果
  assert_eq(result.all_scored_records.length(), 5)
  assert_eq(result.records_to_keep.length(), 4)  // 保留80% = 4个记录
  assert_eq(result.records_to_delete.length(), 1)  // 删除20% = 1个记录
  
  // 验证保留的记录有更高的重要性评分
  let kept_scores = result.records_to_keep.map(fn(r) { r.importance_score })
  let deleted_scores = result.records_to_delete.map(fn(r) { r.importance_score })
  
  let min_kept_score = kept_scores.reduce(fn(acc, score) { if score < acc { score } else { acc } }, kept_scores[0])
  let max_deleted_score = deleted_scores.reduce(fn(acc, score) { if score > acc { score } else { acc } }, deleted_scores[0])
  
  assert_true(min_kept_score >= max_deleted_score)
  
  // 验证评分因素
  let record1_scored = result.all_scored_records.find(fn(sr) { sr.record.id == "record1" })!
  assert_true(record1_scored.importance_score > 0)
  
  let access_factor = record1_scored.factors.find(fn(f) { f.0 == "access_frequency" })!
  assert_true(access_factor.1 > 20.0)  // 高频访问应该有高分
  
  let business_factor = record1_scored.factors.find(fn(f) { f.0 == "business_value" })!
  assert_eq(business_factor.1.round(), 36.0)  // trace类型：0.9 * 40 = 36分
}