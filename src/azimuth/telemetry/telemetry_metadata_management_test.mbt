// 遥测数据元数据管理测试用例
// 测试遥测数据元数据的创建、存储、查询和管理

test "telemetry_metadata_schema_validation" {
  // 测试元数据模式验证
  
  let metadata_schema = {
    "required_fields": ["data_id", "timestamp", "source", "type", "version"],
    "optional_fields": ["tags", "attributes", "correlation_id", "parent_span_id"],
    "field_types": {
      "data_id": "string",
      "timestamp": "datetime",
      "source": "string", 
      "type": "enum",
      "version": "string",
      "tags": "array<string>",
      "attributes": "map<string, any>",
      "correlation_id": "string",
      "parent_span_id": "string"
    },
    "enum_values": {
      "type": ["metric", "trace", "log", "event"]
    }
  }
  
  // 验证模式结构
  assert_eq(metadata_schema["required_fields"].length(), 5)
  assert_eq(metadata_schema["optional_fields"].length(), 4)
  assert_eq(metadata_schema["enum_values"]["type"].length(), 4)
  
  // 测试有效元数据
  let valid_metadata = {
    "data_id": "data_12345",
    "timestamp": "2024-01-01T10:00:00Z",
    "source": "payment-service",
    "type": "metric",
    "version": "1.0",
    "tags": ["production", "critical"],
    "attributes": {"region": "us-east-1", "instance": "i-12345"},
    "correlation_id": "corr_abc123"
  }
  
  // 验证必填字段
  let validation_result = {"valid": true, "errors": []}
  for field in metadata_schema["required_fields"] {
    if !valid_metadata.contains(field) {
      validation_result["valid"] = false
      validation_result["errors"].push("Missing required field: " + field)
    }
  }
  
  assert_eq(validation_result["valid"], true)
  assert_eq(validation_result["errors"].length(), 0)
  
  // 验证字段类型
  let expected_type = metadata_schema["field_types"]["type"]
  let actual_value = valid_metadata["type"]
  let valid_enum_values = metadata_schema["enum_values"]["type"]
  
  assert_eq(expected_type, "enum")
  assert_eq(valid_enum_values.contains(actual_value), true)
  
  // 测试无效元数据（缺少必填字段）
  let invalid_metadata = {
    "data_id": "data_67890",
    "source": "auth-service",
    "type": "trace"
    // 缺少 timestamp, version
  }
  
  let invalid_validation = {"valid": true, "errors": []}
  for field in metadata_schema["required_fields"] {
    if !invalid_metadata.contains(field) {
      invalid_validation["valid"] = false
      invalid_validation["errors"].push("Missing required field: " + field)
    }
  }
  
  assert_eq(invalid_validation["valid"], false)
  assert_eq(invalid_validation["errors"].length(), 2)
  assert_eq(invalid_validation["errors"].contains("Missing required field: timestamp"), true)
  assert_eq(invalid_validation["errors"].contains("Missing required field: version"), true)
}

test "telemetry_metadata_indexing_strategy" {
  // 测试元数据索引策略
  
  let indexing_config = {
    "primary_indexes": ["data_id", "timestamp"],
    "secondary_indexes": ["source", "type", "correlation_id"],
    "composite_indexes": [
      {"fields": ["source", "type"], "name": "source_type_idx"},
      {"fields": ["timestamp", "type"], "name": "timestamp_type_idx"},
      {"fields": ["correlation_id", "timestamp"], "name": "correlation_time_idx"}
    ],
    "full_text_search_fields": ["tags", "attributes.description"]
  }
  
  // 验证索引配置
  assert_eq(indexing_config["primary_indexes"].length(), 2)
  assert_eq(indexing_config["secondary_indexes"].length(), 3)
  assert_eq(indexing_config["composite_indexes"].length(), 3)
  
  // 模拟元数据查询场景
  let query_scenarios = [
    {"description": "按ID查询", "query_fields": ["data_id"], "expected_index_type": "primary"},
    {"description": "按时间范围查询", "query_fields": ["timestamp"], "expected_index_type": "primary"},
    {"description": "按服务源查询", "query_fields": ["source"], "expected_index_type": "secondary"},
    {"description": "按数据类型查询", "query_fields": ["type"], "expected_index_type": "secondary"},
    {"description": "按服务和类型查询", "query_fields": ["source", "type"], "expected_index_type": "composite"},
    {"description": "按关联ID查询", "query_fields": ["correlation_id"], "expected_index_type": "secondary"}
  ]
  
  // 分析索引使用情况
  let index_usage_analysis = {}
  for scenario in query_scenarios {
    let query_fields = scenario["query_fields"]
    let index_type = ""
    
    // 检查主索引
    let uses_primary = false
    for field in query_fields {
      if indexing_config["primary_indexes"].contains(field) {
        uses_primary = true
        break
      }
    }
    
    // 检查复合索引
    let uses_composite = false
    for composite_idx in indexing_config["composite_indexes"] {
      let composite_fields = composite_idx["fields"]
      let all_fields_covered = true
      for field in query_fields {
        if !composite_fields.contains(field) {
          all_fields_covered = false
          break
        }
      }
      if all_fields_covered && composite_fields.length() == query_fields.length() {
        uses_composite = true
        break
      }
    }
    
    // 确定索引类型
    if uses_primary && query_fields.length() == 1 {
      index_type = "primary"
    } else if uses_composite {
      index_type = "composite"
    } else {
      index_type = "secondary"
    }
    
    index_usage_analysis[scenario["description"]] = {
      "query_fields": query_fields,
      "detected_index_type": index_type,
      "expected_index_type": scenario["expected_index_type"],
      "optimal": index_type == scenario["expected_index_type"]
    }
  }
  
  // 验证索引分析结果
  assert_eq(index_usage_analysis.keys().length(), 6)
  
  let id_query_analysis = index_usage_analysis["按ID查询"]
  assert_eq(id_query_analysis["detected_index_type"], "primary")
  assert_eq(id_query_analysis["optimal"], true)
  
  let source_type_query = index_usage_analysis["按服务和类型查询"]
  assert_eq(source_type_query["detected_index_type"], "composite")
  assert_eq(source_type_query["optimal"], true)
  
  // 统计优化查询比例
  let optimal_queries = 0
  for analysis in index_usage_analysis.values() {
    if analysis["optimal"] {
      optimal_queries = optimal_queries + 1
    }
  }
  
  let optimization_rate = @int.to_float(optimal_queries) / @int.to_float(index_usage_analysis.keys().length()) * 100.0
  assert_eq(optimization_rate, 100.0)
}

test "telemetry_metadata_versioning" {
  // 测试元数据版本管理
  
  let metadata_versions = [
    {
      "version": "1.0",
      "release_date": "2023-01-01",
      "fields": ["data_id", "timestamp", "source", "type"],
      "deprecated_fields": [],
      "new_fields": [],
      "breaking_changes": false
    },
    {
      "version": "1.1", 
      "release_date": "2023-06-01",
      "fields": ["data_id", "timestamp", "source", "type", "tags"],
      "deprecated_fields": [],
      "new_fields": ["tags"],
      "breaking_changes": false
    },
    {
      "version": "2.0",
      "release_date": "2024-01-01", 
      "fields": ["data_id", "timestamp", "source", "type", "tags", "metadata_version"],
      "deprecated_fields": [],
      "new_fields": ["metadata_version"],
      "breaking_changes": false
    },
    {
      "version": "2.1",
      "release_date": "2024-06-01",
      "fields": ["data_id", "timestamp", "source", "type", "tags", "metadata_version", "attributes"],
      "deprecated_fields": [],
      "new_fields": ["attributes"],
      "breaking_changes": false
    }
  ]
  
  // 验证版本历史
  assert_eq(metadata_versions.length(), 4)
  assert_eq(metadata_versions[0]["version"], "1.0")
  assert_eq(metadata_versions[3]["version"], "2.1")
  
  // 分析版本演进
  let version_evolution = []
  for i in 1..metadata_versions.length() {
    let current_version = metadata_versions[i]
    let previous_version = metadata_versions[i-1]
    
    let field_changes = {
      "version": current_version["version"],
      "added_fields": current_version["new_fields"],
      "total_fields": current_version["fields"].length(),
      "breaking_changes": current_version["breaking_changes"]
    }
    
    version_evolution.push(field_changes)
  }
  
  // 验证版本演进分析
  assert_eq(version_evolution.length(), 3)
  assert_eq(version_evolution[0]["version"], "1.1")
  assert_eq(version_evolution[0]["added_fields"].length(), 1)
  assert_eq(version_evolution[0]["added_fields"][0], "tags")
  
  // 测试版本兼容性检查
  let compatibility_matrix = {}
  for i in 0..metadata_versions.length() {
    for j in i..metadata_versions.length() {
      let from_version = metadata_versions[i]["version"]
      let to_version = metadata_versions[j]["version"]
      
      let is_compatible = true
      for k in i+1..j {
        if metadata_versions[k]["breaking_changes"] {
          is_compatible = false
          break
        }
      }
      
      compatibility_matrix[from_version + "_to_" + to_version] = is_compatible
    }
  }
  
  // 验证兼容性矩阵
  assert_eq(compatibility_matrix["1.0_to_1.0"], true)
  assert_eq(compatibility_matrix["1.0_to_2.1"], true) // 无破坏性变更
  assert_eq(compatibility_matrix["2.0_to_2.1"], true)
  
  // 测试版本升级路径
  let upgrade_paths = []
  let current_version = "1.0"
  let target_version = "2.1"
  
  let current_index = metadata_versions.index_of(fn(v) { v["version"] == current_version })
  let target_index = metadata_versions.index_of(fn(v) { v["version"] == target_version })
  
  for i in current_index+1..target_index+1 {
    upgrade_paths.push(metadata_versions[i]["version"])
  }
  
  assert_eq(upgrade_paths.length(), 3)
  assert_eq(upgrade_paths[0], "1.1")
  assert_eq(upgrade_paths[1], "2.0")
  assert_eq(upgrade_paths[2], "2.1")
}

test "telemetry_metadata_search_capabilities" {
  // 测试元数据搜索能力
  
  let metadata_store = [
    {
      "data_id": "data_001",
      "timestamp": "2024-01-01T10:00:00Z",
      "source": "payment-service",
      "type": "metric",
      "tags": ["production", "critical", "finance"],
      "attributes": {"region": "us-east-1", "amount": 1500.50, "currency": "USD"},
      "correlation_id": "corr payment 001"
    },
    {
      "data_id": "data_002", 
      "timestamp": "2024-01-01T10:05:00Z",
      "source": "auth-service",
      "type": "trace",
      "tags": ["production", "security"],
      "attributes": {"user_id": "user_123", "ip": "192.168.1.100", "method": "OAuth"},
      "correlation_id": "corr auth 001"
    },
    {
      "data_id": "data_003",
      "timestamp": "2024-01-01T10:10:00Z", 
      "source": "payment-service",
      "type": "log",
      "tags": ["production", "error", "finance"],
      "attributes": {"error_code": "PAYMENT_FAILED", "amount": 2500.75, "currency": "EUR"},
      "correlation_id": "corr payment 002"
    },
    {
      "data_id": "data_004",
      "timestamp": "2024-01-01T10:15:00Z",
      "source": "notification-service", 
      "type": "event",
      "tags": ["production", "notification"],
      "attributes": {"recipient": "user@example.com", "channel": "email", "status": "sent"},
      "correlation_id": "corr notify 001"
    }
  ]
  
  // 验证元数据存储
  assert_eq(metadata_store.length(), 4)
  
  // 测试精确匹配搜索
  let exact_search_results = metadata_store.filter(fn(m) { m["source"] == "payment-service" })
  assert_eq(exact_search_results.length(), 2)
  assert_eq(exact_search_results[0]["data_id"], "data_001")
  assert_eq(exact_search_results[1]["data_id"], "data_003")
  
  // 测试类型过滤搜索
  let type_search_results = metadata_store.filter(fn(m) { m["type"] == "metric" })
  assert_eq(type_search_results.length(), 1)
  assert_eq(type_search_results[0]["data_id"], "data_001")
  
  // 测试标签搜索
  let tag_search_results = []
  for metadata in metadata_store {
    if metadata["tags"].contains("critical") {
      tag_search_results.push(metadata)
    }
  }
  assert_eq(tag_search_results.length(), 1)
  assert_eq(tag_search_results[0]["data_id"], "data_001")
  
  // 测试属性搜索
  let attribute_search_results = []
  for metadata in metadata_store {
    let attributes = metadata["attributes"]
    if attributes.contains("amount") && @float.to_float(attributes["amount"]) > 2000.0 {
      attribute_search_results.push(metadata)
    }
  }
  assert_eq(attribute_search_results.length(), 1)
  assert_eq(attribute_search_results[0]["data_id"], "data_003")
  
  // 测试全文搜索
  let full_text_search_query = "payment"
  let full_text_results = []
  for metadata in metadata_store {
    let searchable_text = metadata["source"] + " " + 
                         @string.join(metadata["tags"], " ") + " " +
                         metadata["correlation_id"]
    
    if searchable_text.contains(full_text_search_query) {
      full_text_results.push(metadata)
    }
  }
  assert_eq(full_text_results.length(), 3) // data_001, data_003, 和它们的correlation_id
  
  // 测试复合查询
  let composite_results = []
  for metadata in metadata_store {
    let source_match = metadata["source"] == "payment-service"
    let tag_match = metadata["tags"].contains("production")
    let has_amount_attribute = metadata["attributes"].contains("amount")
    
    if source_match && tag_match && has_amount_attribute {
      composite_results.push(metadata)
    }
  }
  assert_eq(composite_results.length(), 2)
  
  // 生成搜索性能报告
  let search_performance_report = "元数据搜索性能报告\n"
  search_performance_report = search_performance_report + "精确匹配搜索: " + exact_search_results.length().to_string() + " 结果\n"
  search_performance_report = search_performance_report + "类型过滤搜索: " + type_search_results.length().to_string() + " 结果\n"
  search_performance_report = search_performance_report + "标签搜索: " + tag_search_results.length().to_string() + " 结果\n"
  search_performance_report = search_performance_report + "属性搜索: " + attribute_search_results.length().to_string() + " 结果\n"
  search_performance_report = search_performance_report + "全文搜索: " + full_text_results.length().to_string() + " 结果\n"
  search_performance_report = search_performance_report + "复合查询: " + composite_results.length().to_string() + " 结果\n"
  
  // 验证搜索报告
  assert_eq(search_performance_report.has_prefix("元数据搜索性能报告"), true)
  assert_eq(search_performance_report.contains("精确匹配搜索: 2 结果"), true)
  assert_eq(search_performance_report.contains("复合查询: 2 结果"), true)
}

test "telemetry_metadata_enrichment" {
  // 测试元数据丰富化
  
  let base_metadata = {
    "data_id": "data_12345",
    "timestamp": "2024-01-01T10:00:00Z",
    "source": "api-gateway",
    "type": "trace"
  }
  
  // 定义丰富化规则
  let enrichment_rules = [
    {
      "name": "add_environment_info",
      "condition": fn(metadata) { true }, // 应用于所有元数据
      "action": fn(metadata) {
        metadata["environment"] = "production"
        metadata["datacenter"] = "us-east-1"
        metadata
      }
    },
    {
      "name": "add_service_info", 
      "condition": fn(metadata) { metadata.contains("source") },
      "action": fn(metadata) {
        let source = metadata["source"]
        metadata["service_owner"] = source + "-team"
        metadata["service_tier"] = "critical"
        metadata
      }
    },
    {
      "name": "add_temporal_info",
      "condition": fn(metadata) { metadata.contains("timestamp") },
      "action": fn(metadata) {
        let timestamp = metadata["timestamp"]
        metadata["hour_of_day"] = 10 // 从 timestamp 提取
        metadata["day_of_week"] = 1 // 假设是周一
        metadata
      }
    },
    {
      "name": "add_correlation_info",
      "condition": fn(metadata) { metadata["type"] == "trace" },
      "action": fn(metadata) {
        metadata["trace_id"] = "trace_" + metadata["data_id"]
        metadata["span_count"] = 5 // 模拟值
        metadata
      }
    }
  ]
  
  // 验证丰富化规则
  assert_eq(enrichment_rules.length(), 4)
  
  // 应用丰富化规则
  let enriched_metadata = base_metadata
  let applied_enrichments = []
  
  for rule in enrichment_rules {
    if rule["condition"](enriched_metadata) {
      enriched_metadata = rule["action"](enriched_metadata)
      applied_enrichments.push(rule["name"])
    }
  }
  
  // 验证丰富化结果
  assert_eq(applied_enrichments.length(), 4) // 所有规则都应该被应用
  assert_eq(enriched_metadata.contains("environment"), true)
  assert_eq(enriched_metadata["environment"], "production")
  assert_eq(enriched_metadata.contains("service_owner"), true)
  assert_eq(enriched_metadata["service_owner"], "api-gateway-team")
  assert_eq(enriched_metadata.contains("trace_id"), true)
  assert_eq(enriched_metadata["trace_id"], "trace_data_12345")
  
  // 验证原始数据保持不变
  assert_eq(enriched_metadata["data_id"], "data_12345")
  assert_eq(enriched_metadata["source"], "api-gateway")
  assert_eq(enriched_metadata["type"], "trace")
  
  // 测试条件性丰富化
  let log_metadata = {
    "data_id": "log_67890",
    "timestamp": "2024-01-01T11:00:00Z",
    "source": "auth-service",
    "type": "log"
  }
  
  let enriched_log_metadata = log_metadata
  let log_applied_enrichments = []
  
  for rule in enrichment_rules {
    if rule["condition"](enriched_log_metadata) {
      enriched_log_metadata = rule["action"](enriched_log_metadata)
      log_applied_enrichments.push(rule["name"])
    }
  }
  
  // 日志类型不应该应用 trace 相关的丰富化
  assert_eq(log_applied_enrichments.length(), 3)
  assert_eq(log_applied_enrichments.contains("add_correlation_info"), false)
  assert_eq(enriched_log_metadata.contains("trace_id"), false)
  
  // 生成丰富化报告
  let enrichment_report = "元数据丰富化报告\n"
  enrichment_report = enrichment_report + "原始字段数: " + base_metadata.keys().length().to_string() + "\n"
  enrichment_report = enrichment_report + "丰富化后字段数: " + enriched_metadata.keys().length().to_string() + "\n"
  enrichment_report = enrichment_report + "应用的丰富化规则: " + applied_enrichments.length().to_string() + "\n"
  enrichment_report = enrichment_report + "新增字段: environment, datacenter, service_owner, service_tier, hour_of_day, day_of_week, trace_id, span_count\n"
  
  // 验证丰富化报告
  assert_eq(enrichment_report.has_prefix("元数据丰富化报告"), true)
  assert_eq(enrichment_report.contains("原始字段数: 3"), true)
  assert_eq(enrichment_report.contains("丰富化后字段数: 11"), true)
}

test "telemetry_metadata_retention_policy" {
  // 测试元数据保留策略
  
  let retention_policies = {
    "active_metadata": {"retention_days": 90, "storage_tier": "hot"},
    "recent_metadata": {"retention_days": 365, "storage_tier": "warm"},
    "historical_metadata": {"retention_days": 2555, "storage_tier": "cold"}, // 7年
    "archived_metadata": {"retention_days": -1, "storage_tier": "archive"} // 永久保留
  }
  
  let metadata_records = [
    {"id": "meta_001", "created_at": 1703980800L, "access_frequency": "high", "category": "active"},
    {"id": "meta_002", "created_at": 1699193600L, "access_frequency": "medium", "category": "recent"},
    {"id": "meta_003", "created_at": 1672531200L, "access_frequency": "low", "category": "historical"},
    {"id": "meta_004", "created_at": 1609459200L, "access_frequency": "rare", "category": "archived"}
  ]
  
  let current_time = 1704067200L // 2024-01-01 00:00:00 UTC
  
  // 验证保留策略
  assert_eq(retention_policies.keys().length(), 4)
  assert_eq(retention_policies["archived_metadata"]["retention_days"], -1)
  
  // 应用保留策略
  let retention_decisions = []
  for record in metadata_records {
    let age_days = (current_time - record["created_at"]) / (24 * 60 * 60)
    let category = record["category"]
    let policy = retention_policies[category + "_metadata"]
    
    let should_retain = true
    let action = "keep"
    
    if policy["retention_days"] != -1 && age_days > policy["retention_days"] {
      should_retain = false
      action = "delete"
    }
    
    retention_decisions.push({
      "id": record["id"],
      "age_days": age_days,
      "category": category,
      "policy_retention_days": policy["retention_days"],
      "should_retain": should_retain,
      "action": action,
      "storage_tier": policy["storage_tier"]
    })
  }
  
  // 验证保留决策
  assert_eq(retention_decisions.length(), 4)
  
  // 验证活跃元数据（应该保留）
  let active_decision = retention_decisions.filter(fn(d) { d["id"] == "meta_001" })[0]
  assert_eq(active_decision["should_retain"], true)
  assert_eq(active_decision["action"], "keep")
  assert_eq(active_decision["storage_tier"], "hot")
  
  // 验证归档元数据（应该永久保留）
  let archived_decision = retention_decisions.filter(fn(d) { d["id"] == "meta_004" })[0]
  assert_eq(archived_decision["should_retain"], true)
  assert_eq(archived_decision["action"], "keep")
  assert_eq(archived_decision["storage_tier"], "archive")
  
  // 统计保留决策
  let keep_count = retention_decisions.filter(fn(d) { d["action"] == "keep" }).length()
  let delete_count = retention_decisions.filter(fn(d) { d["action"] == "delete" }).length()
  
  assert_eq(keep_count, 4)
  assert_eq(delete_count, 0)
  
  // 按存储层级分组
  let storage_tier_distribution = {}
  for decision in retention_decisions {
    let tier = decision["storage_tier"]
    if !storage_tier_distribution.contains(tier) {
      storage_tier_distribution[tier] = 0
    }
    storage_tier_distribution[tier] = storage_tier_distribution[tier] + 1
  }
  
  assert_eq(storage_tier_distribution["hot"], 1)
  assert_eq(storage_tier_distribution["warm"], 1)
  assert_eq(storage_tier_distribution["cold"], 1)
  assert_eq(storage_tier_distribution["archive"], 1)
  
  // 生成保留策略报告
  let retention_report = "元数据保留策略报告\n"
  retention_report = retention_report + "总记录数: " + retention_decisions.length().to_string() + "\n"
  retention_report = retention_report + "保留记录数: " + keep_count.to_string() + "\n"
  retention_report = retention_report + "删除记录数: " + delete_count.to_string() + "\n"
  retention_report = retention_report + "存储层级分布:\n"
  
  for tier in storage_tier_distribution.keys() {
    retention_report = retention_report + "- " + tier + ": " + storage_tier_distribution[tier].to_string() + " 记录\n"
  }
  
  // 验证保留报告
  assert_eq(retention_report.has_prefix("元数据保留策略报告"), true)
  assert_eq(retention_report.contains("总记录数: 4"), true)
  assert_eq(retention_report.contains("- archive: 1 记录"), true)
}

test "telemetry_metadata_access_control" {
  // 测试元数据访问控制
  
  let user_permissions = {
    "admin": ["read", "write", "delete", "manage"],
    "analyst": ["read"],
    "developer": ["read", "write"],
    "viewer": ["read"]
  }
  
  let metadata_sensitivity = {
    "public": ["source", "type", "timestamp"],
    "internal": ["source", "type", "timestamp", "tags", "attributes.environment"],
    "confidential": ["source", "type", "timestamp", "tags", "attributes", "correlation_id"],
    "restricted": ["*"] // 所有字段
  }
  
  let access_requests = [
    {"user_role": "admin", "metadata_sensitivity": "public", "requested_action": "read"},
    {"user_role": "analyst", "metadata_sensitivity": "confidential", "requested_action": "read"},
    {"user_role": "developer", "metadata_sensitivity": "internal", "requested_action": "write"},
    {"user_role": "viewer", "metadata_sensitivity": "restricted", "requested_action": "delete"},
    {"user_role": "analyst", "metadata_sensitivity": "public", "requested_action": "write"}
  ]
  
  // 验证权限配置
  assert_eq(user_permissions.keys().length(), 4)
  assert_eq(metadata_sensitivity.keys().length(), 4)
  
  // 处理访问请求
  let access_decisions = []
  for request in access_requests {
    let user_role = request["user_role"]
    let sensitivity = request["metadata_sensitivity"]
    let action = request["requested_action"]
    
    let user_perms = user_permissions[user_role]
    let has_permission = user_perms.contains(action)
    
    // 特殊规则：analyst不能进行写操作
    if user_role == "analyst" && action == "write" {
      has_permission = false
    }
    
    // 特殊规则：viewer不能进行删除操作
    if user_role == "viewer" && action == "delete" {
      has_permission = false
    }
    
    access_decisions.push({
      "user_role": user_role,
      "metadata_sensitivity": sensitivity,
      "requested_action": action,
      "allowed": has_permission,
      "reason": if has_permission { "Permission granted" } else { "Insufficient permissions" }
    })
  }
  
  // 验证访问决策
  assert_eq(access_decisions.length(), 5)
  
  // 验证具体决策
  let admin_read_decision = access_decisions.filter(fn(d) { d["user_role"] == "admin" && d["requested_action"] == "read" })[0]
  assert_eq(admin_read_decision["allowed"], true)
  
  let analyst_write_decision = access_decisions.filter(fn(d) { d["user_role"] == "analyst" && d["requested_action"] == "write" })[0]
  assert_eq(analyst_write_decision["allowed"], false)
  
  let viewer_delete_decision = access_decisions.filter(fn(d) { d["user_role"] == "viewer" && d["requested_action"] == "delete" })[0]
  assert_eq(viewer_delete_decision["allowed"], false)
  
  // 统计访问决策
  let allowed_count = access_decisions.filter(fn(d) { d["allowed"] }).length()
  let denied_count = access_decisions.filter(fn(d) { !d["allowed"] }).length()
  
  assert_eq(allowed_count, 3)
  assert_eq(denied_count, 2)
  
  // 按角色统计访问模式
  let role_access_patterns = {}
  for decision in access_decisions {
    let role = decision["user_role"]
    if !role_access_patterns.contains(role) {
      role_access_patterns[role] = {"allowed": 0, "denied": 0}
    }
    
    if decision["allowed"] {
      role_access_patterns[role]["allowed"] = role_access_patterns[role]["allowed"] + 1
    } else {
      role_access_patterns[role]["denied"] = role_access_patterns[role]["denied"] + 1
    }
  }
  
  // 验证角色访问模式
  assert_eq(role_access_patterns["admin"]["allowed"], 1)
  assert_eq(role_access_patterns["admin"]["denied"], 0)
  assert_eq(role_access_patterns["analyst"]["allowed"], 1)
  assert_eq(role_access_patterns["analyst"]["denied"], 1)
  
  // 生成访问控制报告
  let access_report = "元数据访问控制报告\n"
  access_report = access_report + "总访问请求: " + access_decisions.length().to_string() + "\n"
  access_report = access_report + "允许访问: " + allowed_count.to_string() + "\n"
  access_report = access_report + "拒绝访问: " + denied_count.to_string() + "\n"
  access_report = access_report + "访问成功率: " + (@int.to_float(allowed_count) / @int.to_float(access_decisions.length()) * 100.0).to_string() + "%\n"
  
  // 验证访问报告
  assert_eq(access_report.has_prefix("元数据访问控制报告"), true)
  assert_eq(access_report.contains("总访问请求: 5"), true)
  assert_eq(access_report.contains("访问成功率: 60.0%"), true)
}