// Azimuth 遥测数据资源生命周期管理测试用例
// 专注于遥测系统中的资源生命周期管理和优化机制

// 测试1: 遥测数据存储生命周期管理
test "遥测数据存储生命周期管理" {
  // 模拟不同生命周期的遥测数据
  let telemetry_data = [
    { 
      id: "data1", 
      created_at: 1640995200, 
      last_accessed: 1640995200,
      data_type: "metrics", 
      retention_policy: "30d", 
      size: 1024,
      access_count: 10,
      importance: "high"
    },
    { 
      id: "data2", 
      created_at: 1640995100, 
      last_accessed: 1640995150,
      data_type: "traces", 
      retention_policy: "7d", 
      size: 2048,
      access_count: 5,
      importance: "medium"
    },
    { 
      id: "data3", 
      created_at: 1640995000, 
      last_accessed: 1640995000,
      data_type: "logs", 
      retention_policy: "90d", 
      size: 512,
      access_count: 2,
      importance: "low"
    },
    { 
      id: "data4", 
      created_at: 1640994900, 
      last_accessed: 1640995200,
      data_type: "metrics", 
      retention_policy: "30d", 
      size: 1536,
      access_count: 25,
      importance: "high"
    },
    { 
      id: "data5", 
      created_at: 1640994800, 
      last_accessed: 1640994800,
      data_type: "profiles", 
      retention_policy: "1d", 
      size: 4096,
      access_count: 1,
      importance: "low"
    }
  ]
  
  // 当前时间
  let current_time = 1640995200
  
  // 生命周期管理配置
  let lifecycle_config = {
    retention_policies: {
      "1d": 86400,      // 1天
      "7d": 604800,     // 7天
      "30d": 2592000,   // 30天
      "90d": 7776000    // 90天
    },
    access_based_extension: {
      "high": 1.5,      // 高重要性数据延长50%
      "medium": 1.2,    // 中等重要性数据延长20%
      "low": 1.0        // 低重要性数据不延长
    },
    hot_storage_threshold: 7 * 86400,  // 7天内数据为热数据
    cold_storage_threshold: 30 * 86400 // 30天后数据为冷数据
  }
  
  // 生命周期管理算法
  let mut lifecycle_results = []
  
  for data in telemetry_data {
    let age = current_time - data.created_at
    let days_since_access = (current_time - data.last_accessed) / 86400
    let retention_seconds = lifecycle_config.retention_policies.get(data.retention_policy).unwrap()
    
    // 基于重要性的保留期延长
    let importance_multiplier = lifecycle_config.access_based_extension.get(data.importance).unwrap()
    let extended_retention = (retention_seconds.to_float() * importance_multiplier).to_int()
    
    // 确定数据状态
    let data_status = 
      if age >= extended_retention { "expired" }
      else if age >= lifecycle_config.cold_storage_threshold { "cold" }
      else if age >= lifecycle_config.hot_storage_threshold { "warm" }
      else { "hot" }
    
    // 计算剩余保留时间
    let remaining_retention = if age < extended_retention { 
      extended_retention - age 
    } else { 
      0 
    }
    
    // 计算访问频率
    let access_frequency = if age > 0 { 
      data.access_count.to_float() / (age / 86400).to_float() 
    } else { 
      data.access_count.to_float() 
    }
    
    // 确定推荐操作
    let recommended_action = 
      if data_status == "expired" { "delete" }
      else if data_status == "cold" && access_frequency < 0.1 { "archive" }
      else if data_status == "warm" && access_frequency < 0.5 { "compress" }
      else { "keep" }
    
    lifecycle_results = lifecycle_results.push({
      id: data.id,
      data_type: data.data_type,
      age: age,
      days_since_access: days_since_access,
      data_status: data_status,
      remaining_retention: remaining_retention,
      access_frequency: access_frequency,
      recommended_action: recommended_action,
      size: data.size,
      importance: data.importance
    })
  }
  
  // 验证生命周期管理结果
  assert_eq(lifecycle_results.length(), 5)
  
  // 验证过期数据
  let expired_data = lifecycle_results.filter(fn(r) { r.data_status == "expired" })
  assert_eq(expired_data.length(), 1) // 应该有1个过期数据
  assert_eq(expired_data[0].id, "data5") // data5应该过期（1d策略）
  assert_eq(expired_data[0].recommended_action, "delete")
  
  // 验证冷数据
  let cold_data = lifecycle_results.filter(fn(r) { r.data_status == "cold" })
  assert_eq(cold_data.length(), 1) // 应该有1个冷数据
  assert_eq(cold_data[0].id, "data3") // data3应该是冷数据
  assert_eq(cold_data[0].recommended_action, "archive")
  
  // 验证热数据
  let hot_data = lifecycle_results.filter(fn(r) { r.data_status == "hot" })
  assert_eq(hot_data.length(), 2) // 应该有2个热数据
  assert_true(hot_data.map(fn(r) { r.id }).contains("data1"))
  assert_true(hot_data.map(fn(r) { r.id }).contains("data2"))
  
  // 验证温数据
  let warm_data = lifecycle_results.filter(fn(r) { r.data_status == "warm" })
  assert_eq(warm_data.length(), 1) // 应该有1个温数据
  assert_eq(warm_data[0].id, "data4") // data4应该是温数据
  
  // 验证访问频率计算
  let data1_result = lifecycle_results.filter(fn(r) { r.id == "data1" })[0]
  assert_eq(data1_result.access_frequency, 10.0) // 10次访问/1天
  
  let data4_result = lifecycle_results.filter(fn(r) { r.id == "data4" })[0]
  assert_true(data4_result.access_frequency > 1.0) // 高频访问数据
}

// 测试2: 遥测数据分层存储管理
test "遥测数据分层存储管理" {
  // 模拟分层存储的遥测数据
  let tiered_storage_data = [
    { 
      id: "data1", 
      size: 1024, 
      access_frequency: 10.0, 
      last_access: 1640995200,
      current_tier: "hot",
      data_type: "metrics",
      query_latency: 10
    },
    { 
      id: "data2", 
      size: 2048, 
      access_frequency: 2.0, 
      last_access: 1640995000,
      current_tier: "warm",
      data_type: "traces",
      query_latency: 50
    },
    { 
      id: "data3", 
      size: 512, 
      access_frequency: 0.1, 
      last_access: 1640994800,
      current_tier: "cold",
      data_type: "logs",
      query_latency: 200
    },
    { 
      id: "data4", 
      size: 4096, 
      access_frequency: 8.0, 
      last_access: 1640995190,
      current_tier: "hot",
      data_type: "profiles",
      query_latency: 15
    },
    { 
      id: "data5", 
      size: 1536, 
      access_frequency: 0.5, 
      last_access: 1640994900,
      current_tier: "warm",
      data_type: "metrics",
      query_latency: 80
    }
  ]
  
  // 分层存储配置
  let tier_config = {
    "hot": {
      min_access_frequency: 5.0,
      max_size: 10000,
      cost_per_gb: 0.10,
      max_query_latency: 20
    },
    "warm": {
      min_access_frequency: 0.5,
      max_size: 50000,
      cost_per_gb: 0.05,
      max_query_latency: 100
    },
    "cold": {
      min_access_frequency: 0.0,
      max_size: 100000,
      cost_per_gb: 0.01,
      max_query_latency: 500
    }
  }
  
  // 分层存储管理算法
  let mut tier_management_results = []
  
  for data in tiered_storage_data {
    // 确定推荐层级
    let mut recommended_tier = "cold"
    for (tier_name, tier_config) in tier_config {
      if data.access_frequency >= tier_config.min_access_frequency && 
         data.query_latency <= tier_config.max_query_latency {
        recommended_tier = tier_name
        break
      }
    }
    
    // 计算层级迁移成本
    let current_tier_config = tier_config.get(data.current_tier).unwrap()
    let recommended_tier_config = tier_config.get(recommended_tier).unwrap()
    
    let migration_cost = (data.size.to_float() / 1024.0 / 1024.0 / 1024.0) * 
                        (recommended_tier_config.cost_per_gb - current_tier_config.cost_per_gb)
    
    // 确定迁移动作
    let migration_action = 
      if recommended_tier == data.current_tier { "keep" }
      else if recommended_tier == "hot" && data.current_tier == "cold" { "promote_fast" }
      else if recommended_tier == "hot" && data.current_tier == "warm" { "promote" }
      else if recommended_tier == "warm" && data.current_tier == "hot" { "demote" }
      else if recommended_tier == "cold" && data.current_tier == "warm" { "demote" }
      else if recommended_tier == "cold" && data.current_tier == "hot" { "demote_fast" }
      else { "migrate" }
    
    // 计算性能影响
    let current_latency = data.query_latency
    let expected_latency = match recommended_tier {
      "hot" => 15,
      "warm" => 60,
      "cold" => 250,
      _ => data.query_latency
    }
    
    let performance_impact = ((expected_latency - current_latency).to_float() / current_latency.to_float()) * 100.0
    
    tier_management_results = tier_management_results.push({
      id: data.id,
      current_tier: data.current_tier,
      recommended_tier: recommended_tier,
      migration_action: migration_action,
      migration_cost: migration_cost,
      performance_impact: performance_impact,
      access_frequency: data.access_frequency,
      query_latency: data.query_latency,
      size: data.size
    })
  }
  
  // 验证分层存储管理结果
  assert_eq(tier_management_results.length(), 5)
  
  // 验证需要保持层级的数据
  let keep_data = tier_management_results.filter(fn(r) { r.migration_action == "keep" })
  assert_eq(keep_data.length(), 2) // 应该有2个数据保持当前层级
  assert_true(keep_data.map(fn(r) { r.id }).contains("data1"))
  assert_true(keep_data.map(fn(r) { r.id }).contains("data4"))
  
  // 验证需要降级的数据
  let demote_data = tier_management_results.filter(fn(r) { r.migration_action == "demote" })
  assert_eq(demote_data.length(), 1) // 应该有1个数据需要降级
  assert_eq(demote_data[0].id, "data2") // data2应该从warm降级到cold
  
  // 验证需要升级的数据
  let promote_data = tier_management_results.filter(fn(r) { r.migration_action == "promote" })
  assert_eq(promote_data.length(), 1) // 应该有1个数据需要升级
  assert_eq(promote_data[0].id, "data5") // data5应该从warm升级到hot
  
  // 验证性能影响计算
  let data2_result = tier_management_results.filter(fn(r) { r.id == "data2" })[0]
  assert_true(data2_result.performance_impact > 0) // 降级到cold应该增加延迟
  
  let data5_result = tier_management_results.filter(fn(r) { r.id == "data5" })[0]
  assert_true(data5_result.performance_impact < 0) // 升级到hot应该减少延迟
}

// 测试3: 遥测数据资源池管理
test "遥测数据资源池管理" {
  // 模拟资源池中的遥测数据
  let resource_pool_data = [
    { 
      id: "res1", 
      pool_type: "memory", 
      allocated_size: 2048, 
      used_size: 1024,
      max_size: 4096,
      priority: "high",
      last_cleanup: 1640995000
    },
    { 
      id: "res2", 
      pool_type: "disk", 
      allocated_size: 10240, 
      used_size: 8192,
      max_size: 20480,
      priority: "medium",
      last_cleanup: 1640995100
    },
    { 
      id: "res3", 
      pool_type: "memory", 
      allocated_size: 1024, 
      used_size: 512,
      max_size: 2048,
      priority: "low",
      last_cleanup: 1640994900
    },
    { 
      id: "res4", 
      pool_type: "network", 
      allocated_size: 512, 
      used_size: 256,
      max_size: 1024,
      priority: "high",
      last_cleanup: 1640995200
    },
    { 
      id: "res5", 
      pool_type: "disk", 
      allocated_size: 5120, 
      used_size: 4096,
      max_size: 10240,
      priority: "medium",
      last_cleanup: 1640995050
    }
  ]
  
  // 当前时间
  let current_time = 1640995200
  
  // 资源池管理配置
  let pool_config = {
    cleanup_threshold: 0.8,        // 使用率超过80%时触发清理
    priority_weights: {
      "high": 1.0,
      "medium": 0.7,
      "low": 0.4
    },
    cleanup_intervals: {
      "memory": 300,    // 5分钟
      "disk": 1800,     // 30分钟
      "network": 60     // 1分钟
    },
    reallocation_threshold: 0.9    // 使用率超过90%时触发重新分配
  }
  
  // 资源池管理算法
  let mut pool_management_results = []
  
  for resource in resource_pool_data {
    // 计算使用率
    let usage_rate = resource.used_size.to_float() / resource.max_size.to_float()
    
    // 确定是否需要清理
    let needs_cleanup = usage_rate >= pool_config.cleanup_threshold
    
    // 确定是否需要重新分配
    let needs_reallocation = usage_rate >= pool_config.reallocation_threshold
    
    // 计算距上次清理时间
    let time_since_cleanup = current_time - resource.last_cleanup
    let cleanup_interval = pool_config.cleanup_intervals.get(resource.pool_type).unwrap()
    let cleanup_overdue = time_since_cleanup >= cleanup_interval
    
    // 计算资源优先级得分
    let priority_weight = pool_config.priority_weights.get(resource.priority).unwrap()
    let priority_score = usage_rate * priority_weight
    
    // 确定推荐操作
    let recommended_action = 
      if needs_reallocation { "reallocate" }
      else if needs_cleanup && cleanup_overdue { "cleanup" }
      else if needs_cleanup { "schedule_cleanup" }
      else if cleanup_overdue { "maintenance" }
      else { "monitor" }
    
    // 计算可释放空间
    let releasable_size = if needs_cleanup {
      (resource.used_size * 0.3).to_int() // 释放30%已使用空间
    } else {
      0
    }
    
    // 计算扩展需求
    let expansion_needed = if needs_reallocation {
      (resource.max_size * 0.5).to_int() // 扩展50%最大空间
    } else {
      0
    }
    
    pool_management_results = pool_management_results.push({
      id: resource.id,
      pool_type: resource.pool_type,
      usage_rate: usage_rate,
      needs_cleanup: needs_cleanup,
      needs_reallocation: needs_reallocation,
      cleanup_overdue: cleanup_overdue,
      recommended_action: recommended_action,
      priority_score: priority_score,
      releasable_size: releasable_size,
      expansion_needed: expansion_needed,
      current_size: resource.used_size,
      max_size: resource.max_size
    })
  }
  
  // 验证资源池管理结果
  assert_eq(pool_management_results.length(), 5)
  
  // 验证需要重新分配的资源
  let reallocation_needed = pool_management_results.filter(fn(r) { r.needs_reallocation })
  assert_eq(reallocation_needed.length(), 1) // 应该有1个资源需要重新分配
  assert_eq(reallocation_needed[0].id, "res2") // res2使用率应该超过90%
  assert_eq(reallocation_needed[0].recommended_action, "reallocate")
  
  // 验证需要清理的资源
  let cleanup_needed = pool_management_results.filter(fn(r) { r.needs_cleanup })
  assert_eq(cleanup_needed.length(), 2) // 应该有2个资源需要清理
  assert_true(cleanup_needed.map(fn(r) { r.id }).contains("res2"))
  assert_true(cleanup_needed.map(fn(r) { r.id }).contains("res5"))
  
  // 验证清理过期的资源
  let cleanup_overdue = pool_management_results.filter(fn(r) { r.cleanup_overdue })
  assert_eq(cleanup_overdue.length(), 2) // 应该有2个资源清理过期
  assert_true(cleanup_overdue.map(fn(r) { r.id }).contains("res3"))
  assert_true(cleanup_overdue.map(fn(r) { r.id }).contains("res5"))
  
  // 验证高优先级资源
  let high_priority_resources = pool_management_results.filter(fn(r) { 
    resource_pool_data.filter(fn(res) { res.id == r.id })[0].priority == "high" 
  })
  assert_eq(high_priority_resources.length(), 2) // 应该有2个高优先级资源
  
  // 验证优先级得分计算
  let res1_result = pool_management_results.filter(fn(r) { r.id == "res1" })[0]
  assert_eq(res1_result.priority_score, 0.25) // 0.5使用率 * 1.0权重 = 0.5
  
  let res3_result = pool_management_results.filter(fn(r) { r.id == "res3" })[0]
  assert_eq(res3_result.priority_score, 0.1) // 0.25使用率 * 0.4权重 = 0.1
}

// 测试4: 遥测数据自动清理策略
test "遥测数据自动清理策略" {
  // 模拟需要清理的遥测数据
  let cleanup_data = [
    { 
      id: "data1", 
      created_at: 1640995200, 
      last_accessed: 1640995100,
      size: 1024,
      access_count: 5,
      cleanup_policy: "lru",
      retention_days: 7
    },
    { 
      id: "data2", 
      created_at: 1640995000, 
      last_accessed: 1640995000,
      size: 2048,
      access_count: 1,
      cleanup_policy: "lru",
      retention_days: 7
    },
    { 
      id: "data3", 
      created_at: 1640994800, 
      last_accessed: 1640995100,
      size: 512,
      access_count: 10,
      cleanup_policy: "lfu",
      retention_days: 7
    },
    { 
      id: "data4", 
      created_at: 1640994700, 
      last_accessed: 1640994700,
      size: 4096,
      access_count: 2,
      cleanup_policy: "ttl",
      retention_days: 3
    },
    { 
      id: "data5", 
      created_at: 1640994900, 
      last_accessed: 1640995200,
      size: 1536,
      access_count: 8,
      cleanup_policy: "lru",
      retention_days: 7
    }
  ]
  
  // 当前时间
  let current_time = 1640995200
  
  // 清理策略配置
  let cleanup_config = {
    storage_threshold: 0.8,        // 存储使用率阈值
    cleanup_percentage: 0.2,       // 每次清理20%的数据
    min_access_count: 2,           // 最小访问次数
    priority_exempt: true,         // 高优先级数据豁免
    cleanup_batch_size: 2          // 每批清理数据量
  }
  
  // 自动清理算法
  let mut cleanup_results = []
  
  // 计算数据年龄
  let mut aged_data = []
  for data in cleanup_data {
    let age = current_time - data.created_at
    let days_since_access = (current_time - data.last_accessed) / 86400
    let retention_expired = age > (data.retention_days * 86400)
    
    aged_data = aged_data.push({
      id: data.id,
      age: age,
      days_since_access: days_since_access,
      retention_expired: retention_expired,
      size: data.size,
      access_count: data.access_count,
      cleanup_policy: data.cleanup_policy,
      last_accessed: data.last_accessed,
      created_at: data.created_at
    })
  }
  
  // 按不同清理策略排序
  let mut lru_candidates = aged_data.filter(fn(d) { d.cleanup_policy == "lru" })
  let mut lfu_candidates = aged_data.filter(fn(d) { d.cleanup_policy == "lfu" })
  let mut ttl_candidates = aged_data.filter(fn(d) { d.cleanup_policy == "ttl" })
  
  // LRU策略：按最近访问时间排序
  lru_candidates = lru_candidates.sort_by(fn(a, b) {
    if a.last_accessed < b.last_accessed { -1 }
    else if a.last_accessed > b.last_accessed { 1 }
    else { 0 }
  })
  
  // LFU策略：按访问频率排序
  lfu_candidates = lfu_candidates.sort_by(fn(a, b) {
    if a.access_count < b.access_count { -1 }
    else if a.access_count > b.access_count { 1 }
    else { 0 }
  })
  
  // TTL策略：按创建时间排序
  ttl_candidates = ttl_candidates.sort_by(fn(a, b) {
    if a.created_at < b.created_at { -1 }
    else if a.created_at > b.created_at { 1 }
    else { 0 }
  })
  
  // 合并候选数据
  let mut all_candidates = []
  all_candidates = all_candidates.concat(lru_candidates)
  all_candidates = all_candidates.concat(lfu_candidates)
  all_candidates = all_candidates.concat(ttl_candidates)
  
  // 优先清理过期数据
  let expired_data = all_candidates.filter(fn(d) { d.retention_expired })
  let active_data = all_candidates.filter(fn(d) { not d.retention_expired })
  
  // 选择清理目标
  let mut cleanup_targets = []
  
  // 首先清理过期数据
  for data in expired_data {
    cleanup_targets = cleanup_targets.push({
      id: data.id,
      cleanup_reason: "retention_expired",
      size: data.size,
      priority: "high"
    })
  }
  
  // 然后清理低频访问数据
  let low_access_data = active_data.filter(fn(d) { d.access_count < cleanup_config.min_access_count })
  for data in low_access_data {
    if cleanup_targets.length() < cleanup_config.cleanup_batch_size {
      cleanup_targets = cleanup_targets.push({
        id: data.id,
        cleanup_reason: "low_access_count",
        size: data.size,
        priority: "medium"
      })
    }
  }
  
  // 最后清理最旧的数据
  if cleanup_targets.length() < cleanup_config.cleanup_batch_size {
    let remaining_slots = cleanup_config.cleanup_batch_size - cleanup_targets.length()
    for i in 0..remaining_slots.min(active_data.length()) {
      let data = active_data[i]
      cleanup_targets = cleanup_targets.push({
        id: data.id,
        cleanup_reason: "oldest_data",
        size: data.size,
        priority: "low"
      })
    }
  }
  
  // 计算清理效果
  let total_cleaned_size = cleanup_targets.reduce(fn(acc, target) { acc + target.size }, 0)
  let total_data_size = cleanup_data.reduce(fn(acc, data) { acc + data.size }, 0)
  let cleanup_percentage = total_cleaned_size.to_float() / total_data_size.to_float() * 100.0
  
  // 验证清理结果
  assert_eq(cleanup_targets.length(), cleanup_config.cleanup_batch_size) // 应该清理指定数量的数据
  
  // 验证过期数据被优先清理
  let expired_cleaned = cleanup_targets.filter(fn(t) { t.cleanup_reason == "retention_expired" })
  assert_eq(expired_cleaned.length(), 1) // 应该有1个过期数据被清理
  assert_eq(expired_cleaned[0].id, "data4") // data4应该过期
  
  // 验证低访问数据被清理
  let low_access_cleaned = cleanup_targets.filter(fn(t) { t.cleanup_reason == "low_access_count" })
  assert_eq(low_access_cleaned.length(), 1) // 应该有1个低访问数据被清理
  assert_eq(low_access_cleaned[0].id, "data2") // data2访问次数最少
  
  // 验证最旧数据被清理
  let oldest_cleaned = cleanup_targets.filter(fn(t) { t.cleanup_reason == "oldest_data" })
  assert_eq(oldest_cleaned.length(), 0) // 由于过期和低访问数据已占满批次，不需要清理最旧数据
  
  // 验证清理百分比
  assert_true(cleanup_percentage > 0.0) // 应该有数据被清理
  assert_true(cleanup_percentage < 50.0) // 清理比例应该合理
}

// 测试5: 遥测数据资源优化调度
test "遥测数据资源优化调度" {
  // 模拟需要优化调度的遥测数据资源
  let resource_data = [
    { 
      id: "res1", 
      type: "metrics", 
      priority: "high", 
      cpu_usage: 0.8,
      memory_usage: 0.6,
      io_usage: 0.4,
      network_usage: 0.3,
      processing_time: 100,
      queue_size: 50
    },
    { 
      id: "res2", 
      type: "traces", 
      priority: "medium", 
      cpu_usage: 0.4,
      memory_usage: 0.7,
      io_usage: 0.8,
      network_usage: 0.6,
      processing_time: 200,
      queue_size: 30
    },
    { 
      id: "res3", 
      type: "logs", 
      priority: "low", 
      cpu_usage: 0.2,
      memory_usage: 0.3,
      io_usage: 0.5,
      network_usage: 0.4,
      processing_time: 50,
      queue_size: 20
    },
    { 
      id: "res4", 
      type: "profiles", 
      priority: "high", 
      cpu_usage: 0.9,
      memory_usage: 0.8,
      io_usage: 0.3,
      network_usage: 0.2,
      processing_time: 150,
      queue_size: 80
    },
    { 
      id: "res5", 
      type: "events", 
      priority: "medium", 
      cpu_usage: 0.5,
      memory_usage: 0.4,
      io_usage: 0.6,
      network_usage: 0.7,
      processing_time: 80,
      queue_size: 40
    }
  ]
  
  // 资源调度配置
  let scheduling_config = {
    resource_thresholds: {
      "cpu": 0.8,
      "memory": 0.8,
      "io": 0.8,
      "network": 0.8
    },
    priority_weights: {
      "high": 1.0,
      "medium": 0.7,
      "low": 0.4
    },
    queue_threshold: 50,           // 队列阈值
    load_balance_threshold: 0.2    // 负载均衡阈值
  }
  
  // 资源优化调度算法
  let mut scheduling_results = []
  
  // 计算资源负载得分
  for resource in resource_data {
    // 检查资源瓶颈
    let resource_bottlenecks = []
    
    if resource.cpu_usage >= scheduling_config.resource_thresholds.get("cpu").unwrap() {
      resource_bottlenecks = resource_bottlenecks.push("cpu")
    }
    if resource.memory_usage >= scheduling_config.resource_thresholds.get("memory").unwrap() {
      resource_bottlenecks = resource_bottlenecks.push("memory")
    }
    if resource.io_usage >= scheduling_config.resource_thresholds.get("io").unwrap() {
      resource_bottlenecks = resource_bottlenecks.push("io")
    }
    if resource.network_usage >= scheduling_config.resource_thresholds.get("network").unwrap() {
      resource_bottlenecks = resource_bottlenecks.push("network")
    }
    
    // 计算综合负载得分
    let load_score = (resource.cpu_usage + resource.memory_usage + 
                     resource.io_usage + resource.network_usage) / 4.0
    
    // 计算优先级权重
    let priority_weight = scheduling_config.priority_weights.get(resource.priority).unwrap()
    
    // 计算调度优先级得分
    let scheduling_priority = load_score * priority_weight
    
    // 确定调度动作
    let scheduling_action = 
      if resource_bottlenecks.length() >= 2 { "scale_up" }
      else if resource_bottlenecks.length() == 1 { "optimize" }
      else if resource.queue_size > scheduling_config.queue_threshold { "increase_throughput" }
      else if load_score < 0.3 { "scale_down" }
      else { "maintain" }
    
    // 计算资源调整建议
    let resource_adjustments = {}
    
    if resource.cpu_usage >= scheduling_config.resource_thresholds.get("cpu").unwrap() {
      resource_adjustments = resource_adjustments.set("cpu", "increase")
    } else if resource.cpu_usage < 0.3 {
      resource_adjustments = resource_adjustments.set("cpu", "decrease")
    }
    
    if resource.memory_usage >= scheduling_config.resource_thresholds.get("memory").unwrap() {
      resource_adjustments = resource_adjustments.set("memory", "increase")
    } else if resource.memory_usage < 0.3 {
      resource_adjustments = resource_adjustments.set("memory", "decrease")
    }
    
    // 计算队列处理建议
    let queue_processing_needed = resource.queue_size > scheduling_config.queue_threshold
    let suggested_workers = if queue_processing_needed {
      (resource.queue_size / 10).max(1)
    } else {
      0
    }
    
    scheduling_results = scheduling_results.push({
      id: resource.id,
      type: resource.type,
      priority: resource.priority,
      load_score: load_score,
      scheduling_priority: scheduling_priority,
      resource_bottlenecks: resource_bottlenecks,
      scheduling_action: scheduling_action,
      resource_adjustments: resource_adjustments,
      queue_processing_needed: queue_processing_needed,
      suggested_workers: suggested_workers,
      processing_time: resource.processing_time,
      queue_size: resource.queue_size
    })
  }
  
  // 验证资源优化调度结果
  assert_eq(scheduling_results.length(), 5)
  
  // 验证需要扩展的资源
  let scale_up_resources = scheduling_results.filter(fn(r) { r.scheduling_action == "scale_up" })
  assert_eq(scale_up_resources.length(), 2) // 应该有2个资源需要扩展
  assert_true(scale_up_resources.map(fn(r) { r.id }).contains("res1"))
  assert_true(scale_up_resources.map(fn(r) { r.id }).contains("res4"))
  
  // 验证需要优化的资源
  let optimize_resources = scheduling_results.filter(fn(r) { r.scheduling_action == "optimize" })
  assert_eq(optimize_resources.length(), 1) // 应该有1个资源需要优化
  assert_eq(optimize_resources[0].id, "res2") // res2应该有io瓶颈
  
  // 验证需要增加吞吐量的资源
  let throughput_resources = scheduling_results.filter(fn(r) { r.scheduling_action == "increase_throughput" })
  assert_eq(throughput_resources.length(), 1) // 应该有1个资源需要增加吞吐量
  assert_eq(throughput_resources[0].id, "res4") // res4队列大小超过阈值
  
  // 验证需要缩减的资源
  let scale_down_resources = scheduling_results.filter(fn(r) { r.scheduling_action == "scale_down" })
  assert_eq(scale_down_resources.length(), 1) // 应该有1个资源需要缩减
  assert_eq(scale_down_resources[0].id, "res3") // res3负载较低
  
  // 验证资源瓶颈识别
  let res1_result = scheduling_results.filter(fn(r) { r.id == "res1" })[0]
  assert_true(res1_result.resource_bottlenecks.contains("cpu")) // res1应该有CPU瓶颈
  
  let res2_result = scheduling_results.filter(fn(r) { r.id == "res2" })[0]
  assert_true(res2_result.resource_bottlenecks.contains("io")) // res2应该有IO瓶颈
  
  let res4_result = scheduling_results.filter(fn(r) { r.id == "res4" })[0]
  assert_true(res4_result.resource_bottlenecks.contains("cpu")) // res4应该有CPU瓶颈
  assert_true(res4_result.resource_bottlenecks.contains("memory")) // res4应该有内存瓶颈
  
  // 验证调度优先级排序
  let sorted_by_priority = scheduling_results.sort_by(fn(a, b) {
    if a.scheduling_priority > b.scheduling_priority { -1 }
    else if a.scheduling_priority < b.scheduling_priority { 1 }
    else { 0 }
  })
  
  // 高优先级资源应该排在前面
  assert_eq(sorted_by_priority[0].priority, "high")
  assert_eq(sorted_by_priority[1].priority, "high")
  
  // 验证工作线程建议
  let res4_result = scheduling_results.filter(fn(r) { r.id == "res4" })[0]
  assert_true(res4_result.suggested_workers > 0) // res4应该需要额外工作线程
  assert_eq(res4_result.suggested_workers, 8) // 80/10 = 8个工作线程
}