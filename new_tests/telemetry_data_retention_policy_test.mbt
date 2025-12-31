// 遥测数据保留策略测试
// 测试遥测数据的保留、归档和清理策略

test "data_retention_time_based_policy" {
  // 基于时间的数据保留策略测试
  let retention_policies = [
    ("metrics", 7),      // 指标保留7天
    ("traces", 30),      // 链路追踪保留30天
    ("logs", 90),        // 日志保留90天
    ("events", 365)      // 事件保留1年
  ]
  
  let current_time = 1640995200000L  // 2022-01-01 00:00:00 UTC
  let ms_per_day = 86400000L
  
  let generate_test_data = fn(data_type : String, days_back : Int) -> Array[(String, Int64, Int)] {
    let mut data = []
    for i = 0; i < days_back; i = i + 1 {
      let timestamp = current_time - @int.to_int64(i) * ms_per_day
      let data_size = 1024 + i * 100  // 数据大小随时间变化
      data = data.push((data_type, timestamp, data_size))
    }
    data
  }
  
  let apply_retention_policy = fn(data : Array[(String, Int64, Int)], policies : Array[(String, Int)], current_timestamp : Int64) -> Array[(String, Int64, Int)] {
    data.filter(fn((data_type, timestamp, _)) {
      let retention_days = policies.filter(fn(p, _) { p == data_type })[0].1
      let cutoff_time = current_timestamp - @int.to_int64(retention_days) * ms_per_day
      timestamp >= cutoff_time
    })
  }
  
  // 生成测试数据
  let all_data = []
  for (data_type, retention_days) in retention_policies {
    let type_data = generate_test_data(data_type, retention_days + 30)  // 生成比保留期更多的数据
    all_data.extend(type_data)
  }
  
  // 应用保留策略
  let retained_data = apply_retention_policy(all_data, retention_policies, current_time)
  
  // 验证保留策略
  for (data_type, retention_days) in retention_policies {
    let type_retained = retained_data.filter(fn(dt, _, _) { dt == data_type })
    let cutoff_time = current_time - @int.to_int64(retention_days) * ms_per_day
    
    // 验证保留的数据都在保留期内
    for (_, timestamp, _) in type_retained {
      @assertion.assert_true(timestamp >= cutoff_time)?
    }
    
    // 验证保留的数据数量正确
    @assertion.assert_eq(type_retained.length, retention_days)?
  }
}

test "data_retention_size_based_policy" {
  // 基于大小的数据保留策略测试
  let storage_limits = {
    "max_total_size_mb": 10240,  // 10GB总限制
    "max_metrics_size_mb": 2048, // 2GB指标限制
    "max_traces_size_mb": 4096,  // 4GB链路追踪限制
    "max_logs_size_mb": 4096     // 4GB日志限制
  }
  
  let generate_data_by_size = fn(data_type : String, count : Int, avg_size_kb : Int) -> Array[(String, Int, Int64)] {
    let mut data = []
    for i = 0; i < count; i = i + 1 {
      let size_kb = avg_size_kb + (i % 20 - 10)  // 大小有变化
      let timestamp = 1640995200000L + @int.to_int64(i * 60000)  // 每分钟一个数据点
      data = data.push((data_type, size_kb, timestamp))
    }
    data
  }
  
  let apply_size_based_retention = fn(data : Array[(String, Int, Int64)], limits : { "max_total_size_mb": Int, "max_metrics_size_mb": Int, "max_traces_size_mb": Int, "max_logs_size_mb": Int }) -> Array[(String, Int, Int64)] {
    let mut total_size_kb = 0
    let mut metrics_size_kb = 0
    let mut traces_size_kb = 0
    let mut logs_size_kb = 0
    
    // 按时间排序（最新的在前）
    let sorted_data = data.sort(fn(a, b) { @int.compare(b.2, a.2) })
    
    let mut retained_data = []
    
    for (data_type, size_kb, timestamp) in sorted_data {
      let would_exceed_total = total_size_kb + size_kb > limits.max_total_size_mb * 1024
      let would_exceed_type = match data_type {
        "metrics" => metrics_size_kb + size_kb > limits.max_metrics_size_mb * 1024
        "traces" => traces_size_kb + size_kb > limits.max_traces_size_mb * 1024
        "logs" => logs_size_kb + size_kb > limits.max_logs_size_mb * 1024
        _ => false
      }
      
      if !would_exceed_total && !would_exceed_type {
        retained_data = retained_data.push((data_type, size_kb, timestamp))
        total_size_kb = total_size_kb + size_kb
        
        match data_type {
          "metrics" => metrics_size_kb = metrics_size_kb + size_kb
          "traces" => traces_size_kb = traces_size_kb + size_kb
          "logs" => logs_size_kb = logs_size_kb + size_kb
          _ => {}
        }
      }
    }
    
    retained_data
  }
  
  // 生成测试数据
  let metrics_data = generate_data_by_size("metrics", 5000, 10)   // 平均10KB
  let traces_data = generate_data_by_size("traces", 3000, 50)     // 平均50KB
  let logs_data = generate_data_by_size("logs", 2000, 30)         // 平均30KB
  
  let all_data = metrics_data.extend(traces_data).extend(logs_data)
  
  // 应用基于大小的保留策略
  let retained_data = apply_size_based_retention(all_data, storage_limits)
  
  // 验证大小限制
  let total_size_kb = retained_data.map(fn(_, size, _) { size }).reduce(fn(acc, size) { acc + size }, 0)
  let metrics_size_kb = retained_data.filter(fn(dt, _, _) { dt == "metrics" }).map(fn(_, size, _) { size }).reduce(fn(acc, size) { acc + size }, 0)
  let traces_size_kb = retained_data.filter(fn(dt, _, _) { dt == "traces" }).map(fn(_, size, _) { size }).reduce(fn(acc, size) { acc + size }, 0)
  let logs_size_kb = retained_data.filter(fn(dt, _, _) { dt == "logs" }).map(fn(_, size, _) { size }).reduce(fn(acc, size) { acc + size }, 0)
  
  @assertion.assert_true(total_size_kb <= storage_limits.max_total_size_mb * 1024)?
  @assertion.assert_true(metrics_size_kb <= storage_limits.max_metrics_size_mb * 1024)?
  @assertion.assert_true(traces_size_kb <= storage_limits.max_traces_size_mb * 1024)?
  @assertion.assert_true(logs_size_kb <= storage_limits.max_logs_size_mb * 1024)?
  
  // 验证保留的数据是最新的（按时间排序）
  let sorted_retained = retained_data.sort(fn(a, b) { @int.compare(b.2, a.2) })
  for i = 1; i < sorted_retained.length; i = i + 1 {
    @assertion.assert_true(sorted_retained[i-1].2 >= sorted_retained[i].2)?
  }
}

test "data_retention_tiered_storage" {
  // 分层存储数据保留策略测试
  let storage_tiers = [
    ("hot", 7, 100),      // 热存储：7天，100GB
    ("warm", 30, 500),    // 温存储：30天，500GB
    ("cold", 365, 2000),  // 冷存储：1年，2TB
    ("archive", -1, 10000) // 归档：永久，10TB
  ]
  
  let current_time = 1640995200000L
  let ms_per_day = 86400000L
  
  let determine_storage_tier = fn(timestamp : Int64, current_timestamp : Int64, tiers : Array[(String, Int, Int)]) -> String {
    let age_days = @float.to_int(@float.from_int64(current_timestamp - timestamp) / @float.from_int64(ms_per_day))
    
    for (tier_name, retention_days, _) in tiers {
      if retention_days == -1 {
        return tier_name  // 归档层
      } else if age_days <= retention_days {
        return tier_name
      }
    }
    
    "archive"  // 默认归档
  }
  
  let generate_tiered_data = fn(count : Int, days_span : Int) -> Array[(String, Int64, Int)] {
    let mut data = []
    for i = 0; i < count; i = i + 1 {
      let days_back = (i * days_span) / count
      let timestamp = current_time - @int.to_int64(days_back) * ms_per_day
      let size_mb = 50 + (i % 100)  // 50-149MB
      data = data.push(("", timestamp, size_mb))
    }
    data
  }
  
  let assign_storage_tiers = fn(data : Array[(String, Int64, Int)], tiers : Array[(String, Int, Int)], current_timestamp : Int64) -> Array[(String, Int64, Int, String)] {
    data.map(fn(_, timestamp, size) {
      let tier = determine_storage_tier(timestamp, current_timestamp, tiers)
      ("", timestamp, size, tier)
    })
  }
  
  let test_data = generate_tiered_data(1000, 400)  // 1000个数据点，跨越400天
  let tiered_data = assign_storage_tiers(test_data, storage_tiers, current_time)
  
  // 验证分层存储分配
  let tier_counts = storage_tiers.map(fn(tier_name, _, _) {
    let count = tiered_data.filter_fn((_, _, _, assigned_tier) { assigned_tier == tier_name }).length
    (tier_name, count)
  })
  
  // 验证每个层都有数据
  for (tier_name, count) in tier_counts {
    match tier_name {
      "hot" => @assertion.assert_true(count > 0)?
      "warm" => @assertion.assert_true(count > 0)?
      "cold" => @assertion.assert_true(count > 0)?
      "archive" => @assertion.assert_true(count > 0)?
      _ => @assertion.assert_fail("Unknown tier")?
    }
  }
  
  // 验证数据按时间正确分层
  let hot_data = tiered_data.filter_fn((_, _, _, tier) { tier == "hot" })
  let warm_data = tiered_data.filter_fn((_, _, _, tier) { tier == "warm" })
  let cold_data = tiered_data.filter_fn((_, _, _, tier) { tier == "cold" })
  
  for (_, timestamp, _, _) in hot_data {
    let age_days = @float.to_int(@float.from_int64(current_time - timestamp) / @float.from_int64(ms_per_day))
    @assertion.assert_true(age_days <= 7)?
  }
  
  for (_, timestamp, _, _) in warm_data {
    let age_days = @float.to_int(@float.from_int64(current_time - timestamp) / @float.from_int64(ms_per_day))
    @assertion.assert_true(age_days > 7 && age_days <= 30)?
  }
  
  for (_, timestamp, _, _) in cold_data {
    let age_days = @float.to_int(@float.from_int64(current_time - timestamp) / @float.from_int64(ms_per_day))
    @assertion.assert_true(age_days > 30 && age_days <= 365)?
  }
}

test "data_retention_compliance_requirements" {
  // 合规性要求的保留策略测试
  let compliance_requirements = [
    ("GDPR", 30, "personal_data"),           // GDPR：个人数据30天
    ("HIPAA", 365, "health_data"),           // HIPAA：健康数据1年
    ("SOX", 2555, "financial_data"),         // SOX：财务数据7年
    ("PCI", 365, "payment_data")             // PCI：支付数据1年
  ]
  
  let data_types = [
    ("user_profile", "personal_data"),
    ("medical_record", "health_data"),
    ("transaction_log", "financial_data"),
    ("credit_card", "payment_data"),
    ("system_metric", "system_data")
  ]
  
  let check_compliance_retention = fn(data_type : String, data_category : String, age_days : Int, requirements : Array[(String, Int, String)]) -> (Bool, String?) {
    let applicable_requirements = requirements.filter_fn((_, _, category) { category == data_category })
    
    if applicable_requirements.length == 0 {
      (true, None)  // 无特定要求
    } else {
      let (regulation, retention_days, _) = applicable_requirements[0]
      if age_days <= retention_days {
        (true, None)
      } else {
        (false, Some("Data exceeds retention period for " + regulation))
      }
    }
  }
  
  let test_cases = [
    ("user_profile", 15),    // 15天前
    ("user_profile", 45),    // 45天前
    ("medical_record", 200), // 200天前
    ("medical_record", 400), // 400天前
    ("transaction_log", 1000), // 1000天前
    ("transaction_log", 3000), // 3000天前
    ("credit_card", 200),    // 200天前
    ("credit_card", 400),    // 400天前
    ("system_metric", 5000)  // 5000天前（无合规要求）
  ]
  
  let compliance_results = []
  for (data_type_name, age_days) in test_cases {
    let data_category = data_types.filter_fn((name, _) { name == data_type_name })[0].1
    let (is_compliant, error_msg) = check_compliance_retention(data_type_name, data_category, age_days, compliance_requirements)
    compliance_results = compliance_results.push((data_type_name, age_days, is_compliant, error_msg))
  }
  
  // 验证合规性检查结果
  @assertion.assert_true(compliance_results.filter_fn((_, _, compliant, _) { compliant }).length >= 5)?
  
  // 验证特定违规情况
  let user_profile_violations = compliance_results.filter_fn((name, _, compliant, _) { name == "user_profile" && !compliant })
  @assertion.assert_eq(user_profile_violations.length, 1)?  // 45天应该违规
  
  let medical_record_violations = compliance_results.filter_fn((name, _, compliant, _) { name == "medical_record" && !compliant })
  @assertion.assert_eq(medical_record_violations.length, 1)?  // 400天应该违规
  
  let transaction_log_violations = compliance_results.filter_fn((name, _, compliant, _) { name == "transaction_log" && !compliant })
  @assertion.assert_eq(transaction_log_violations.length, 0)?  // 1000和3000天都应该合规（SOX要求7年=2555天）
  
  // 验证无合规要求的数据总是合规的
  let system_metric_results = compliance_results.filter_fn((name, _, compliant, _) { name == "system_metric" })
  @assertion.assert_eq(system_metric_results.length, 1)?
  @assertion.assert_true(system_metric_results[0].2)?  // 应该合规
}

test "data_retention_cost_optimization" {
  // 成本优化的保留策略测试
  let storage_costs = [
    ("ssd_hot", 0.23),     // SSD热存储：$0.23/GB/月
    ("hdd_warm", 0.10),    // HDD温存储：$0.10/GB/月
    ("cold_glacier", 0.04), // 冷存储：$0.04/GB/月
    ("archive_deep", 0.01)  // 深度归档：$0.01/GB/月
  ]
  
  let data_volume_by_tier = [
    ("ssd_hot", 100),      // 100GB
    ("hdd_warm", 500),     // 500GB
    ("cold_glacier", 2000), // 2TB
    ("archive_deep", 5000)  // 5TB
  ]
  
  let calculate_monthly_cost = fn(volumes : Array[(String, Int)], costs : Array[(String, Double)]) -> Double {
    let mut total_cost = 0.0
    
    for (tier_name, volume_gb) in volumes {
      let cost_per_gb = costs.filter_fn((name, _) { name == tier_name })[0].1
      total_cost = total_cost + @float.from_int(volume_gb) * cost_per_gb
    }
    
    total_cost
  }
  
  let optimize_for_budget = fn(budget_usd : Double, volumes : Array[(String, Int)], costs : Array[(String, Double)]) -> Array[(String, Int)] {
    let mut remaining_budget = budget_usd
    let mut optimized_volumes = []
    
    // 按成本效益排序（从最便宜的存储开始）
    let sorted_costs = costs.sort(fn(a, b) { @float.compare(a.1, b.1) })
    
    for (tier_name, cost_per_gb) in sorted_costs {
      let current_volume = volumes.filter_fn((name, _) { name == tier_name })[0].1
      let max_affordable_volume = @float.to_int(remaining_budget / cost_per_gb)
      let optimized_volume = @min(current_volume, max_affordable_volume)
      
      optimized_volumes = optimized_volumes.push((tier_name, optimized_volume))
      remaining_budget = remaining_budget - @float.from_int(optimized_volume) * cost_per_gb
    }
    
    optimized_volumes
  }
  
  let current_cost = calculate_monthly_cost(data_volume_by_tier, storage_costs)
  
  // 测试不同预算下的优化
  let budget_scenarios = [100.0, 500.0, 1000.0, 2000.0]
  
  for budget in budget_scenarios {
    let optimized_volumes = optimize_for_budget(budget, data_volume_by_tier, storage_costs)
    let optimized_cost = calculate_monthly_cost(optimized_volumes, storage_costs)
    
    // 验证优化后的成本不超过预算
    @assertion.assert_true(optimized_cost <= budget)?
    
    // 验证总存储量不增加
    let original_total_gb = data_volume_by_tier.map_fn((_, volume) { volume }).reduce(fn(acc, vol) { acc + vol }, 0)
    let optimized_total_gb = optimized_volumes.map_fn((_, volume) { volume }).reduce(fn(acc, vol) { acc + vol }, 0)
    @assertion.assert_true(optimized_total_gb <= original_total_gb)?
  }
  
  // 验证原成本
  @assertion.assert_true(current_cost > 0.0)?
  
  // 验证成本计算
  let expected_cost = 100.0 * 0.23 + 500.0 * 0.10 + 2000.0 * 0.04 + 5000.0 * 0.01
  @assertion.assert_true(@float.abs(current_cost - expected_cost) < 0.01)?
}

test "data_retention_automated_cleanup" {
  // 自动清理策略测试
  let cleanup_rules = [
    ("error_logs", 14, "ERROR"),       // 错误日志保留14天
    ("debug_logs", 3, "DEBUG"),        // 调试日志保留3天
    ("trace_data", 30, "TRACE"),       // 链路追踪数据保留30天
    ("metrics_data", 90, "METRIC")     // 指标数据保留90天
  ]
  
  let current_time = 1640995200000L
  let ms_per_day = 86400000L
  
  let generate_cleanup_candidates = fn(rule_name : String, data_type : String, retention_days : Int, count : Int) -> Array[(String, String, Int64, Int)] {
    let mut candidates = []
    for i = 0; i < count; i = i + 1 {
      let age_days = i + (count / 2)  // 生成不同年龄的数据
      let timestamp = current_time - @int.to_int64(age_days) * ms_per_day
      let size_mb = 10 + (i % 20)
      candidates = candidates.push((rule_name, data_type, timestamp, size_mb))
    }
    candidates
  }
  
  let execute_cleanup = fn(candidates : Array[(String, String, Int64, Int)], rules : Array[(String, Int, String)], current_timestamp : Int64) -> { "deleted": Array[(String, String, Int64, Int)], "retained": Array[(String, String, Int64, Int)], "freed_space_mb": Int } {
    let mut deleted = []
    let mut retained = []
    let mut freed_space_mb = 0
    
    for (rule_name, data_type, timestamp, size_mb) in candidates {
      let rule = rules.filter_fn((name, _, _) { name == rule_name })[0]
      let (_, retention_days, _) = rule
      let age_days = @float.to_int(@float.from_int64(current_timestamp - timestamp) / @float.from_int64(ms_per_day))
      
      if age_days > retention_days {
        deleted = deleted.push((rule_name, data_type, timestamp, size_mb))
        freed_space_mb = freed_space_mb + size_mb
      } else {
        retained = retained.push((rule_name, data_type, timestamp, size_mb))
      }
    }
    
    { "deleted": deleted, "retained": retained, "freed_space_mb": freed_space_mb }
  }
  
  // 生成清理候选数据
  let all_candidates = []
  for (rule_name, retention_days, data_type) in cleanup_rules {
    let candidates = generate_cleanup_candidates(rule_name, data_type, retention_days, 50)
    all_candidates.extend(candidates)
  }
  
  // 执行清理
  let cleanup_result = execute_cleanup(all_candidates, cleanup_rules, current_time)
  
  // 验证清理结果
  @assertion.assert_true(cleanup_result.deleted.length > 0)?
  @assertion.assert_true(cleanup_result.retained.length > 0)?
  @assertion.assert_true(cleanup_result.freed_space_mb > 0)?
  
  // 验证删除的数据确实超过了保留期
  for (rule_name, _, timestamp, _) in cleanup_result.deleted {
    let rule = cleanup_rules.filter_fn((name, _, _) { name == rule_name })[0]
    let (_, retention_days, _) = rule
    let age_days = @float.to_int(@float.from_int64(current_time - timestamp) / @float.from_int64(ms_per_day))
    @assertion.assert_true(age_days > retention_days)?
  }
  
  // 验证保留的数据在保留期内
  for (rule_name, _, timestamp, _) in cleanup_result.retained {
    let rule = cleanup_rules.filter_fn((name, _, _) { name == rule_name })[0]
    let (_, retention_days, _) = rule
    let age_days = @float.to_int(@float.from_int64(current_time - timestamp) / @float.from_int64(ms_per_day))
    @assertion.assert_true(age_days <= retention_days)?
  }
  
  // 验证释放的空间计算正确
  let calculated_freed_space = cleanup_result.deleted.map_fn((_, _, _, size) { size }).reduce(fn(acc, size) { acc + size }, 0)
  @assertion.assert_eq(cleanup_result.freed_space_mb, calculated_freed_space)?
}