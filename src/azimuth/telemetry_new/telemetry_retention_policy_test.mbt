// 遥测数据保留策略测试用例

test "retention_policy_configuration" {
  // 测试保留策略配置
  
  let retention_policies = [
    {
      "policy_id": "policy_001",
      "policy_name": "Production Traces",
      "data_type": "traces",
      "retention_days": 30,
      "storage_tier": "hot",
      "conditions": {"environment": "production", "service": "*"},
      "actions": ["compress_after_7d", "archive_after_30d"]
    },
    {
      "policy_id": "policy_002",
      "policy_name": "Development Logs",
      "data_type": "logs", 
      "retention_days": 7,
      "storage_tier": "cold",
      "conditions": {"environment": "development", "service": "*"},
      "actions": ["compress_after_1d", "delete_after_7d"]
    },
    {
      "policy_id": "policy_003",
      "policy_name": "Critical Metrics",
      "data_type": "metrics",
      "retention_days": 365,
      "storage_tier": "hot",
      "conditions": {"metric_type": "critical", "service": "*"},
      "actions": ["aggregate_after_90d", "archive_after_365d"]
    }
  ]
  
  // 验证保留策略
  assert_eq(retention_policies.length(), 3)
  
  // 验证策略配置
  let traces_policy = retention_policies[0]
  assert_eq(traces_policy["policy_id"], "policy_001")
  assert_eq(traces_policy["data_type"], "traces")
  assert_eq(traces_policy["retention_days"], 30)
  assert_eq(traces_policy["storage_tier"], "hot")
  
  // 验证策略条件匹配
  let test_data_items = [
    {
      "item_id": "item_001",
      "data_type": "traces",
      "timestamp": 1641018000000L,
      "attributes": {"environment": "production", "service": "api-gateway"},
      "size": 1024
    },
    {
      "item_id": "item_002",
      "data_type": "logs",
      "timestamp": 1641018000000L,
      "attributes": {"environment": "development", "service": "user-service"},
      "size": 512
    },
    {
      "item_id": "item_003",
      "data_type": "metrics",
      "timestamp": 1641018000000L,
      "attributes": {"metric_type": "critical", "service": "order-service"},
      "size": 256
    },
    {
      "item_id": "item_004",
      "data_type": "traces",
      "timestamp": 1641018000000L,
      "attributes": {"environment": "staging", "service": "payment-service"},
      "size": 768
    }
  ]
  
  // 验证测试数据
  assert_eq(test_data_items.length(), 4)
  
  // 测试策略匹配
  let mut policy_matches = []
  
  for item in test_data_items {
    let item_id = item["item_id"]
    let data_type = item["data_type"]
    let attributes = item["attributes"]
    
    // 查找匹配的策略
    let mut matched_policies = []
    for policy in retention_policies {
      if policy["data_type"] == data_type {
        let conditions = policy["conditions"]
        let mut matches = true
        
        // 检查条件匹配
        for condition_key in conditions.keys() {
          let condition_value = conditions[condition_key]
          let item_value = attributes[condition_key]
          
          // 简单的通配符匹配
          if condition_value != "*" and condition_value != item_value {
            matches = false
            break
          }
        }
        
        if matches {
          matched_policies.push(policy["policy_id"])
        }
      }
    }
    
    policy_matches.push((item_id, data_type, matched_policies))
  }
  
  // 验证策略匹配结果
  assert_eq(policy_matches.length(), 4)
  
  // 验证生产追踪数据匹配
  assert_eq(policy_matches[0].0, "item_001")
  assert_eq(policy_matches[0].1, "traces")
  assert_eq(policy_matches[0].2.length(), 1)
  assert_eq(policy_matches[0].2[0], "policy_001")
  
  // 验证开发日志数据匹配
  assert_eq(policy_matches[1].0, "item_002")
  assert_eq(policy_matches[1].1, "logs")
  assert_eq(policy_matches[1].2.length(), 1)
  assert_eq(policy_matches[1].2[0], "policy_002")
  
  // 验证关键指标数据匹配
  assert_eq(policy_matches[2].0, "item_003")
  assert_eq(policy_matches[2].1, "metrics")
  assert_eq(policy_matches[2].2.length(), 1)
  assert_eq(policy_matches[2].2[0], "policy_003")
  
  // 验证暂存追踪数据不匹配
  assert_eq(policy_matches[3].0, "item_004")
  assert_eq(policy_matches[3].1, "traces")
  assert_eq(policy_matches[3].2.length(), 0)  // 没有匹配的策略
}

test "retention_aging_process" {
  // 测试保留老化过程
  
  let current_time = 1643673600000L  // 2022-02-01 00:00:00
  
  let aging_data = [
    {
      "item_id": "item_001",
      "data_type": "traces",
      "created_at": 1640860800000L,  // 2021-12-30 (32天前)
      "policy_id": "policy_001",
      "retention_days": 30,
      "current_tier": "hot"
    },
    {
      "item_id": "item_002",
      "data_type": "logs", 
      "created_at": 1640947200000L,  // 2021-12-31 (31天前)
      "policy_id": "policy_002",
      "retention_days": 7,
      "current_tier": "cold"
    },
    {
      "item_id": "item_003",
      "data_type": "metrics",
      "created_at": 1642000000000L,  // 2022-01-12 (20天前)
      "policy_id": "policy_003", 
      "retention_days": 365,
      "current_tier": "hot"
    },
    {
      "item_id": "item_004",
      "data_type": "traces",
      "created_at": 1643500800000L,  // 2022-01-30 (2天前)
      "policy_id": "policy_001",
      "retention_days": 30,
      "current_tier": "hot"
    }
  ]
  
  // 验证老化数据
  assert_eq(aging_data.length(), 4)
  
  // 计算数据年龄和处理动作
  let mut aging_actions = []
  
  for item in aging_data {
    let item_id = item["item_id"]
    let created_at = item["created_at"]
    let retention_days = item["retention_days"]
    let current_tier = item["current_tier"]
    
    // 计算年龄（天）
    let age_days = (current_time - created_at) / (24 * 60 * 60 * 1000)
    
    // 确定处理动作
    let mut action = "keep"
    let mut new_tier = current_tier
    
    if age_days >= retention_days {
      action = "delete"
    } else if age_days >= retention_days * 0.9 {
      action = "archive"
      new_tier = "cold"
    } else if age_days >= 7 and current_tier == "hot" {
      action = "compress"
      new_tier = "warm"
    }
    
    aging_actions.push((
      item_id,
      age_days,
      retention_days,
      action,
      current_tier,
      new_tier
    ))
  }
  
  // 验证老化动作
  assert_eq(aging_actions.length(), 4)
  
  // 验证过期数据删除
  let expired_items = []
  for action in aging_actions {
    if action.3 == "delete" {
      expired_items.push(action)
    }
  }
  assert_eq(expired_items.length(), 1)
  assert_eq(expired_items[0].0, "item_002")  // 31天前的日志，保留期7天
  
  // 验证需要归档的数据
  let archive_items = []
  for action in aging_actions {
    if action.3 == "archive" {
      archive_items.push(action)
    }
  }
  assert_eq(archive_items.length(), 1)
  assert_eq(archive_items[0].0, "item_001")  // 32天前的追踪，保留期30天
  
  // 验证需要压缩的数据
  let compress_items = []
  for action in aging_actions {
    if action.3 == "compress" {
      compress_items.push(action)
    }
  }
  assert_eq(compress_items.length(), 1)
  assert_eq(compress_items[0].0, "item_003")  // 20天前的指标，需要压缩
  
  // 验证保持不变的数据
  let keep_items = []
  for action in aging_actions {
    if action.3 == "keep" {
      keep_items.push(action)
    }
  }
  assert_eq(keep_items.length(), 1)
  assert_eq(keep_items[0].0, "item_004")  // 2天前的追踪，保持不变
}

test "storage_tier_management" {
  // 测试存储层管理
  
  let storage_tiers = [
    {
      "tier_name": "hot",
      "description": "Frequent access data",
      "storage_type": "ssd",
      "cost_per_gb": 0.10,
      "max_capacity_gb": 1000,
      "current_usage_gb": 750,
      "performance": {"read_ms": 1, "write_ms": 1}
    },
    {
      "tier_name": "warm",
      "description": "Occasional access data",
      "storage_type": "hdd",
      "cost_per_gb": 0.05,
      "max_capacity_gb": 5000,
      "current_usage_gb": 2000,
      "performance": {"read_ms": 10, "write_ms": 5}
    },
    {
      "tier_name": "cold",
      "description": "Rare access data",
      "storage_type": "tape",
      "cost_per_gb": 0.01,
      "max_capacity_gb": 10000,
      "current_usage_gb": 3000,
      "performance": {"read_ms": 1000, "write_ms": 100}
    }
  ]
  
  // 验证存储层
  assert_eq(storage_tiers.length(), 3)
  
  // 分析存储使用情况
  let mut storage_analysis = []
  
  for tier in storage_tiers {
    let tier_name = tier["tier_name"]
    let cost_per_gb = tier["cost_per_gb"]
    let max_capacity = tier["max_capacity_gb"]
    let current_usage = tier["current_usage_gb"]
    
    // 计算使用率
    let usage_percentage = current_usage.to_double() / max_capacity.to_double() * 100.0
    
    // 计算月度成本
    let monthly_cost = current_usage.to_double() * cost_per_gb
    
    // 确定存储状态
    let mut status = "healthy"
    if usage_percentage > 90.0 {
      status = "critical"
    } else if usage_percentage > 80.0 {
      status = "warning"
    }
    
    storage_analysis.push((
      tier_name,
      usage_percentage,
      monthly_cost,
      status
    ))
  }
  
  // 验证存储分析
  assert_eq(storage_analysis.length(), 3)
  
  // 验证热存储状态
  let hot_analysis = storage_analysis[0]
  assert_eq(hot_analysis.0, "hot")
  assert_eq(hot_analysis.1, 75.0)  // 750/1000 * 100
  assert_eq(hot_analysis.2, 75.0)  // 750 * 0.10
  assert_eq(hot_analysis.3, "healthy")
  
  // 验证温存储状态
  let warm_analysis = storage_analysis[1]
  assert_eq(warm_analysis.0, "warm")
  assert_eq(warm_analysis.1, 40.0)  // 2000/5000 * 100
  assert_eq(warm_analysis.2, 100.0)  // 2000 * 0.05
  assert_eq(warm_analysis.3, "healthy")
  
  // 验证冷存储状态
  let cold_analysis = storage_analysis[2]
  assert_eq(cold_analysis.0, "cold")
  assert_eq(cold_analysis.1, 30.0)  // 3000/10000 * 100
  assert_eq(cold_analysis.2, 30.0)  // 3000 * 0.01
  assert_eq(cold_analysis.3, "healthy")
  
  // 测试数据迁移策略
  let migration_rules = [
    {
      "from_tier": "hot",
      "to_tier": "warm",
      "condition": "age_days >= 7",
      "priority": 1
    },
    {
      "from_tier": "warm", 
      "to_tier": "cold",
      "condition": "age_days >= 30",
      "priority": 2
    },
    {
      "from_tier": "hot",
      "to_tier": "cold",
      "condition": "age_days >= 90",
      "priority": 3
    }
  ]
  
  // 验证迁移规则
  assert_eq(migration_rules.length(), 3)
  
  // 模拟数据迁移
  let data_items_for_migration = [
    {
      "item_id": "item_001",
      "current_tier": "hot",
      "age_days": 10,
      "size_gb": 5
    },
    {
      "item_id": "item_002",
      "current_tier": "hot", 
      "age_days": 35,
      "size_gb": 3
    },
    {
      "item_id": "item_003",
      "current_tier": "warm",
      "age_days": 45,
      "size_gb": 8
    }
  ]
  
  // 处理迁移
  let mut migration_results = []
  
  for item in data_items_for_migration {
    let item_id = item["item_id"]
    let current_tier = item["current_tier"]
    let age_days = item["age_days"]
    let size_gb = item["size_gb"]
    
    // 查找适用的迁移规则
    let mut applicable_rules = []
    for rule in migration_rules {
      if rule["from_tier"] == current_tier {
        // 简化条件检查
        if rule["condition"] == "age_days >= 7" and age_days >= 7 {
          applicable_rules.push(rule)
        } else if rule["condition"] == "age_days >= 30" and age_days >= 30 {
          applicable_rules.push(rule)
        } else if rule["condition"] == "age_days >= 90" and age_days >= 90 {
          applicable_rules.push(rule)
        }
      }
    }
    
    // 选择优先级最高的规则
    let mut selected_rule = {}
    if applicable_rules.length() > 0 {
      selected_rule = applicable_rules[0]
      for rule in applicable_rules {
        if rule["priority"] < selected_rule["priority"] {
          selected_rule = rule
        }
      }
    }
    
    let mut target_tier = current_tier
    let mut will_migrate = false
    
    if selected_rule.keys().length() > 0 {
      target_tier = selected_rule["to_tier"]
      will_migrate = true
    }
    
    migration_results.push((
      item_id,
      current_tier,
      target_tier,
      age_days,
      size_gb,
      will_migrate
    ))
  }
  
  // 验证迁移结果
  assert_eq(migration_results.length(), 3)
  
  // 验证第一个项目迁移（热到温）
  assert_eq(migration_results[0].0, "item_001")
  assert_eq(migration_results[0].1, "hot")
  assert_eq(migration_results[0].2, "warm")
  assert_eq(migration_results[0].5, true)
  
  // 验证第二个项目迁移（热到冷，35天符合>=30天的条件）
  assert_eq(migration_results[1].0, "item_002")
  assert_eq(migration_results[1].1, "hot")
  assert_eq(migration_results[1].2, "cold")
  assert_eq(migration_results[1].5, true)
  
  // 验证第三个项目迁移（温到冷）
  assert_eq(migration_results[2].0, "item_003")
  assert_eq(migration_results[2].1, "warm")
  assert_eq(migration_results[2].2, "cold")
  assert_eq(migration_results[2].5, true)
}

test "retention_compliance_monitoring" {
  // 测试保留合规性监控
  
  let compliance_requirements = [
    {
      "requirement_id": "req_001",
      "regulation": "GDPR",
      "data_type": "personal_data",
      "max_retention_days": 2555,  // 7年
      "purpose": "user privacy protection"
    },
    {
      "requirement_id": "req_002",
      "regulation": "SOX",
      "data_type": "financial_audit",
      "min_retention_days": 2555,  // 7年
      "purpose": "financial compliance"
    },
    {
      "requirement_id": "req_003", 
      "regulation": "HIPAA",
      "data_type": "health_records",
      "max_retention_days": 2190,  // 6年
      "purpose": "patient data protection"
    }
  ]
  
  // 验证合规要求
  assert_eq(compliance_requirements.length(), 3)
  
  // 创建合规监控数据
  let compliance_data = [
    {
      "dataset_id": "dataset_001",
      "data_type": "personal_data",
      "regulation": "GDPR",
      "retention_days": 2000,
      "created_at": 1577836800000L,  // 2020-01-01
      "record_count": 10000
    },
    {
      "dataset_id": "dataset_002",
      "data_type": "personal_data", 
      "regulation": "GDPR",
      "retention_days": 3000,
      "created_at": 1577836800000L,  // 2020-01-01
      "record_count": 15000
    },
    {
      "dataset_id": "dataset_003",
      "data_type": "financial_audit",
      "regulation": "SOX",
      "retention_days": 2555,
      "created_at": 1609459200000L,  // 2021-01-01
      "record_count": 5000
    },
    {
      "dataset_id": "dataset_004",
      "data_type": "health_records",
      "regulation": "HIPAA",
      "retention_days": 2500,
      "created_at": 1609459200000L,  // 2021-01-01
      "record_count": 8000
    }
  ]
  
  // 验证合规数据
  assert_eq(compliance_data.length(), 4)
  
  // 执行合规性检查
  let mut compliance_results = []
  
  for dataset in compliance_data {
    let dataset_id = dataset["dataset_id"]
    let data_type = dataset["data_type"]
    let regulation = dataset["regulation"]
    let retention_days = dataset["retention_days"]
    let record_count = dataset["record_count"]
    
    // 查找对应的合规要求
    let mut requirement = {}
    for req in compliance_requirements {
      if req["data_type"] == data_type and req["regulation"] == regulation {
        requirement = req
        break
      }
    }
    
    if requirement.keys().length() > 0 {
      let max_retention = requirement["max_retention_days"]
      let min_retention = requirement["min_retention_days"]
      
      // 检查合规性
      let mut is_compliant = true
      let mut violation_type = ""
      
      if max_retention > 0 and retention_days > max_retention {
        is_compliant = false
        violation_type = "exceeds_max_retention"
      } else if min_retention > 0 and retention_days < min_retention {
        is_compliant = false
        violation_type = "below_min_retention"
      }
      
      // 计算风险等级
      let mut risk_level = "low"
      if not is_compliant {
        if regulation == "GDPR" or regulation == "HIPAA" {
          risk_level = "high"
        } else {
          risk_level = "medium"
        }
      }
      
      // 计算潜在罚款（模拟）
      let mut potential_fine = 0
      if not is_compliant {
        match regulation {
          "GDPR" => potential_fine = record_count * 10  // 每条记录10欧元
          "SOX" => potential_fine = 1000000  // 固定罚款
          "HIPAA" => potential_fine = record_count * 50  // 每条记录50美元
          _ => potential_fine = 500000
        }
      }
      
      compliance_results.push((
        dataset_id,
        data_type,
        regulation,
        retention_days,
        is_compliant,
        violation_type,
        risk_level,
        potential_fine,
        record_count
      ))
    }
  }
  
  // 验证合规检查结果
  assert_eq(compliance_results.length(), 4)
  
  // 验证合规数据集
  let mut compliant_datasets = []
  let mut non_compliant_datasets = []
  
  for result in compliance_results {
    if result.4 {
      compliant_datasets.push(result)
    } else {
      non_compliant_datasets.push(result)
    }
  }
  
  assert_eq(compliant_datasets.length(), 2)
  assert_eq(non_compliant_datasets.length(), 2)
  
  // 验证非合规数据集详情
  assert_eq(non_compliant_datasets[0].0, "dataset_002")  // GDPR个人数据，3000天 > 2555天
  assert_eq(non_compliant_datasets[0].4, false)
  assert_eq(non_compliant_datasets[0].5, "exceeds_max_retention")
  assert_eq(non_compliant_datasets[0].6, "high")
  assert_eq(non_compliant_datasets[0].7, 150000)  // 15000 * 10
  
  assert_eq(non_compliant_datasets[1].0, "dataset_004")  // HIPAA健康记录，2500天 > 2190天
  assert_eq(non_compliant_datasets[1].4, false)
  assert_eq(non_compliant_datasets[1].5, "exceeds_max_retention")
  assert_eq(non_compliant_datasets[1].6, "high")
  assert_eq(non_compliant_datasets[1].7, 400000)  // 8000 * 50
  
  // 计算总体合规性指标
  let total_datasets = compliance_results.length()
  let compliant_count = compliant_datasets.length()
  let compliance_rate = compliant_count.to_double() / total_datasets.to_double() * 100.0
  
  assert_eq(compliance_rate, 50.0)  // 2/4 * 100
  
  // 计算总潜在罚款
  let mut total_potential_fine = 0
  for result in non_compliant_datasets {
    total_potential_fine = total_potential_fine + result.7
  }
  
  assert_eq(total_potential_fine, 550000)  // 150000 + 400000
  
  // 生成合规报告摘要
  let compliance_summary = {
    "total_datasets": total_datasets,
    "compliant_datasets": compliant_count,
    "compliance_rate": compliance_rate,
    "high_risk_violations": 2,
    "total_potential_fine": total_potential_fine,
    "requires_immediate_action": total_potential_fine > 100000
  }
  
  // 验证合规摘要
  assert_eq(compliance_summary["compliance_rate"], 50.0)
  assert_eq(compliance_summary["high_risk_violations"], 2)
  assert_eq(compliance_summary["total_potential_fine"], 550000)
  assert_eq(compliance_summary["requires_immediate_action"], true)
}