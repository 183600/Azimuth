// Azimuth 遥测数据一致性验证测试用例
// 专注于数据一致性检查、完整性验证、跨服务数据同步等功能

// 测试1: 遥测数据完整性验证
test "遥测数据完整性验证功能" {
  // 定义数据完整性规则
  enum IntegrityRuleType {
    NotNull        // 非空检查
    RangeCheck     // 范围检查
    FormatCheck    // 格式检查
    Referential    // 引用完整性
    Uniqueness     // 唯一性检查
    Consistency    // 一致性检查
  }
  
  // 定义验证规则
  type IntegrityRule = {
    id: String
    name: String
    rule_type: IntegrityRuleType
    field: String
    parameters: Map[String, String]
    severity: String
    description: String
  }
  
  // 定义验证结果
  type ValidationResult = {
    rule_id: String
    passed: Bool
    violation_count: Int
    affected_records: Array[Int]
    error_message: String
    severity: String
  }
  
  // 定义数据记录
  type DataRecord = {
    id: String
    timestamp: Int
    trace_id: String
    span_id: String
    parent_span_id: Option[String]
    service_name: String
    operation_name: String
    duration_ms: Int
    status: String
    tags: Map[String, String]
  }
  
  // 非空检查
  let check_not_null = fn(records: Array[DataRecord], field: String) -> ValidationResult {
    let mut violations = []
    
    for i in 0..records.length() {
      let record = records[i]
      let is_null = match field {
        "id" => record.id == "",
        "trace_id" => record.trace_id == "",
        "span_id" => record.span_id == "",
        "service_name" => record.service_name == "",
        "operation_name" => record.operation_name == "",
        "status" => record.status == "",
        _ => false
      }
      
      if is_null {
        violations = violations.push(i)
      }
    }
    
    {
      rule_id: "not_null_" + field,
      passed: violations.length() == 0,
      violation_count: violations.length(),
      affected_records: violations,
      error_message: if violations.length() > 0 { "字段 " + field + " 不能为空" } else { "" },
      severity: "high"
    }
  }
  
  // 范围检查
  let check_range = fn(records: Array[DataRecord], field: String, min_value: Int, max_value: Int) -> ValidationResult {
    let mut violations = []
    
    for i in 0..records.length() {
      let record = records[i]
      let value = match field {
        "duration_ms" => record.duration_ms,
        "timestamp" => record.timestamp,
        _ => 0
      }
      
      if value < min_value || value > max_value {
        violations = violations.push(i)
      }
    }
    
    {
      rule_id: "range_" + field,
      passed: violations.length() == 0,
      violation_count: violations.length(),
      affected_records: violations,
      error_message: if violations.length() > 0 { 
        "字段 " + field + " 的值应在 " + min_value.to_string() + " 到 " + max_value.to_string() + " 范围内" 
      } else { "" },
      severity: "medium"
    }
  }
  
  // 格式检查
  let check_format = fn(records: Array[DataRecord], field: String, pattern: String) -> ValidationResult {
    let mut violations = []
    
    for i in 0..records.length() {
      let record = records[i]
      let value = match field {
        "id" => record.id,
        "trace_id" => record.trace_id,
        "span_id" => record.span_id,
        "status" => record.status,
        _ => ""
      }
      
      let is_valid = match pattern {
        "uuid" => value.length() == 36 && value[8] == '-' && value[13] == '-' && value[18] == '-' && value[23] == '-',
        "trace_id_format" => value.starts_with("trace-") && value.length() > 10,
        "span_id_format" => value.starts_with("span-") && value.length() > 8,
        "status_values" => value == "ok" || value == "error" || value == "timeout",
        _ => false
      }
      
      if not(is_valid) {
        violations = violations.push(i)
      }
    }
    
    {
      rule_id: "format_" + field,
      passed: violations.length() == 0,
      violation_count: violations.length(),
      affected_records: violations,
      error_message: if violations.length() > 0 { "字段 " + field + " 的格式不符合要求" } else { "" },
      severity: "medium"
    }
  }
  
  // 引用完整性检查
  let check_referential_integrity = fn(records: Array[DataRecord]) -> ValidationResult {
    let mut violations = []
    
    // 收集所有有效的span_id
    let mut valid_span_ids = []
    for record in records {
      valid_span_ids = valid_span_ids.push(record.span_id)
    }
    
    // 检查parent_span_id是否存在于span_id中
    for i in 0..records.length() {
      let record = records[i]
      
      match record.parent_span_id {
        Some(parent_id) => {
          if not(valid_span_ids.contains(parent_id)) {
            violations = violations.push(i)
          }
        },
        None => {}
      }
    }
    
    {
      rule_id: "referential_parent_span",
      passed: violations.length() == 0,
      violation_count: violations.length(),
      affected_records: violations,
      error_message: if violations.length() > 0 { "parent_span_id 引用了不存在的 span_id" } else { "" },
      severity: "high"
    }
  }
  
  // 唯一性检查
  let check_uniqueness = fn(records: Array[DataRecord], field: String) -> ValidationResult {
    let mut seen_values = []
    let mut violations = []
    
    for i in 0..records.length() {
      let record = records[i]
      let value = match field {
        "id" => record.id,
        "span_id" => record.span_id,
        _ => ""
      }
      
      if seen_values.contains(value) {
        violations = violations.push(i)
      } else {
        seen_values = seen_values.push(value)
      }
    }
    
    {
      rule_id: "unique_" + field,
      passed: violations.length() == 0,
      violation_count: violations.length(),
      affected_records: violations,
      error_message: if violations.length() > 0 { "字段 " + field + " 必须是唯一的" } else { "" },
      severity: "high"
    }
  }
  
  // 一致性检查
  let check_consistency = fn(records: Array[DataRecord]) -> ValidationResult {
    let mut violations = []
    
    for i in 0..records.length() {
      let record = records[i]
      
      // 检查时间一致性：子span的开始时间应该不早于父span
      match record.parent_span_id {
        Some(parent_id) => {
          let parent_record = records.find(fn(r) { r.span_id == parent_id })
          match parent_record {
            Some(parent) => {
              if record.timestamp < parent.timestamp {
                violations = violations.push(i)
              }
            },
            None => {}
          }
        },
        None => {}
      }
      
      // 检查状态一致性：如果duration为0，状态应该是ok
      if record.duration_ms == 0 && record.status != "ok" {
        violations = violations.push(i)
      }
    }
    
    {
      rule_id: "consistency_checks",
      passed: violations.length() == 0,
      violation_count: violations.length(),
      affected_records: violations,
      error_message: if violations.length() > 0 { "数据一致性检查失败" } else { "" },
      severity: "medium"
    }
  }
  
  // 运行完整性验证
  let run_integrity_validation = fn(records: Array[DataRecord], rules: Array[IntegrityRule]) -> Array[ValidationResult] {
    let mut results = []
    
    for rule in rules {
      let result = match rule.rule_type {
        IntegrityRuleType::NotNull => check_not_null(records, rule.field),
        IntegrityRuleType::RangeCheck => {
          let min_value = match Map::get(rule.parameters, "min") {
            Some(value) => value.to_int(),
            None => 0
          }
          let max_value = match Map::get(rule.parameters, "max") {
            Some(value) => value.to_int(),
            None => 999999
          }
          check_range(records, rule.field, min_value, max_value)
        },
        IntegrityRuleType::FormatCheck => {
          let pattern = match Map::get(rule.parameters, "pattern") {
            Some(value) => value,
            None => ""
          }
          check_format(records, rule.field, pattern)
        },
        IntegrityRuleType::Referential => check_referential_integrity(records),
        IntegrityRuleType::Uniqueness => check_uniqueness(records, rule.field),
        IntegrityRuleType::Consistency => check_consistency(records)
      }
      
      results = results.push(result)
    }
    
    results
  }
  
  // 创建测试数据
  let test_records = [
    {
      id: "record-001",
      timestamp: 1640995200000,
      trace_id: "trace-12345",
      span_id: "span-001",
      parent_span_id: None,
      service_name: "auth-service",
      operation_name: "login",
      duration_ms: 120,
      status: "ok",
      tags: Map::from([("user.id", "user123"), ("ip", "192.168.1.1")])
    },
    {
      id: "record-002",
      timestamp: 1640995200050,
      trace_id: "trace-12345",
      span_id: "span-002",
      parent_span_id: Some("span-001"),
      service_name: "auth-service",
      operation_name: "validate_credentials",
      duration_ms: 80,
      status: "ok",
      tags: Map::from([("user.id", "user123")])
    },
    {
      id: "record-003",
      timestamp: 1640995200100,
      trace_id: "trace-67890",
      span_id: "span-003",
      parent_span_id: None,
      service_name: "payment-service",
      operation_name: "process_payment",
      duration_ms: 500,
      status: "error",
      tags: Map::from([("payment.id", "pay123"), ("error.code", "CARD_DECLINED")])
    },
    {
      id: "", // 空ID，应该违反非空规则
      timestamp: 1640995200150,
      trace_id: "invalid_trace", // 无效的trace_id格式
      span_id: "span-001", // 重复的span_id，应该违反唯一性规则
      parent_span_id: Some("span-999"), // 不存在的父span，应该违反引用完整性
      service_name: "",
      operation_name: "test",
      duration_ms: -10, // 负数持续时间，应该违反范围检查
      status: "unknown", // 无效状态，应该违反格式检查
      tags: Map::from([])
    },
    {
      id: "record-005",
      timestamp: 1640995000000, // 早于父span的时间戳，应该违反一致性检查
      trace_id: "trace-12345",
      span_id: "span-005",
      parent_span_id: Some("span-001"),
      service_name: "user-service",
      operation_name: "get_profile",
      duration_ms: 0, // 持续时间为0但状态不是ok，应该违反一致性检查
      status: "timeout",
      tags: Map::from([("user.id", "user123")])
    }
  ]
  
  // 创建验证规则
  let integrity_rules = [
    {
      id: "rule-001",
      name: "ID不能为空",
      rule_type: IntegrityRuleType::NotNull,
      field: "id",
      parameters: Map::empty(),
      severity: "high",
      description: "记录ID不能为空"
    },
    {
      id: "rule-002",
      name: "持续时间范围检查",
      rule_type: IntegrityRuleType::RangeCheck,
      field: "duration_ms",
      parameters: Map::from([("min", "0"), ("max", "300000")]),
      severity: "medium",
      description: "持续时间必须在0到300000毫秒之间"
    },
    {
      id: "rule-003",
      name: "Trace ID格式检查",
      rule_type: IntegrityRuleType::FormatCheck,
      field: "trace_id",
      parameters: Map::from([("pattern", "trace_id_format")]),
      severity: "medium",
      description: "Trace ID必须符合格式要求"
    },
    {
      id: "rule-004",
      name: "Span ID唯一性检查",
      rule_type: IntegrityRuleType::Uniqueness,
      field: "span_id",
      parameters: Map::empty(),
      severity: "high",
      description: "Span ID必须唯一"
    },
    {
      id: "rule-005",
      name: "引用完整性检查",
      rule_type: IntegrityRuleType::Referential,
      field: "",
      parameters: Map::empty(),
      severity: "high",
      description: "Parent span ID必须引用存在的span"
    },
    {
      id: "rule-006",
      name: "状态格式检查",
      rule_type: IntegrityRuleType::FormatCheck,
      field: "status",
      parameters: Map::from([("pattern", "status_values")]),
      severity: "medium",
      description: "状态值必须是ok、error或timeout之一"
    },
    {
      id: "rule-007",
      name: "一致性检查",
      rule_type: IntegrityRuleType::Consistency,
      field: "",
      parameters: Map::empty(),
      severity: "medium",
      description: "检查数据一致性"
    }
  ]
  
  // 运行完整性验证
  let validation_results = run_integrity_validation(test_records, integrity_rules)
  
  // 验证结果
  assert_eq(validation_results.length(), 7)
  
  // 检查非空规则：应该失败（record-004的ID为空）
  let not_null_result = validation_results.find(fn(r) { r.rule_id == "not_null_id" })
  match not_null_result {
    Some(result) => {
      assert_false(result.passed)
      assert_eq(result.violation_count, 1)
      assert_eq(result.affected_records[0], 3) // record-004的索引
    },
    None => assert_true(false)
  }
  
  // 检查范围规则：应该失败（record-004的持续时间为负数）
  let range_result = validation_results.find(fn(r) { r.rule_id == "range_duration_ms" })
  match range_result {
    Some(result) => {
      assert_false(result.passed)
      assert_eq(result.violation_count, 1)
      assert_eq(result.affected_records[0], 3) // record-004的索引
    },
    None => assert_true(false)
  }
  
  // 检查格式规则：应该失败（record-004的trace_id和status格式无效）
  let trace_format_result = validation_results.find(fn(r) { r.rule_id == "format_trace_id" })
  match trace_format_result {
    Some(result) => {
      assert_false(result.passed)
      assert_eq(result.violation_count, 1)
      assert_eq(result.affected_records[0], 3) // record-004的索引
    },
    None => assert_true(false)
  }
  
  let status_format_result = validation_results.find(fn(r) { r.rule_id == "format_status" })
  match status_format_result {
    Some(result) => {
      assert_false(result.passed)
      assert_eq(result.violation_count, 1)
      assert_eq(result.affected_records[0], 3) // record-004的索引
    },
    None => assert_true(false)
  }
  
  // 检查唯一性规则：应该失败（span-001重复）
  let uniqueness_result = validation_results.find(fn(r) { r.rule_id == "unique_span_id" })
  match uniqueness_result {
    Some(result) => {
      assert_false(result.passed)
      assert_eq(result.violation_count, 1)
      assert_eq(result.affected_records[0], 3) // record-004的索引
    },
    None => assert_true(false)
  }
  
  // 检查引用完整性：应该失败（span-999不存在）
  let referential_result = validation_results.find(fn(r) { r.rule_id == "referential_parent_span" })
  match referential_result {
    Some(result) => {
      assert_false(result.passed)
      assert_eq(result.violation_count, 1)
      assert_eq(result.affected_records[0], 3) // record-004的索引
    },
    None => assert_true(false)
  }
  
  // 检查一致性规则：应该失败（record-005的时间戳早于父span，且持续时间为0但状态不是ok）
  let consistency_result = validation_results.find(fn(r) { r.rule_id == "consistency_checks" })
  match consistency_result {
    Some(result) => {
      assert_false(result.passed)
      assert_eq(result.violation_count, 2) // record-004和record-005
    },
    None => assert_true(false)
  }
}

// 测试2: 跨服务数据一致性验证
test "跨服务数据一致性验证功能" {
  // 定义服务数据记录
  type ServiceDataRecord = {
    service_name: String
    trace_id: String
    span_id: String
    parent_span_id: Option[String]
    operation_name: String
    timestamp: Int
    duration_ms: Int
    status: String
    tags: Map[String, String]
  }
  
  // 定义跨服务一致性检查结果
  type CrossServiceConsistencyResult = {
    check_type: String
    passed: Bool
    affected_traces: Array[String]
    affected_services: Array[String]
    description: String
    severity: String
  }
  
  // 检查追踪链完整性
  let check_trace_chain_completeness = fn(service_data: Array[ServiceDataRecord>) -> CrossServiceConsistencyResult {
    // 按trace_id分组
    let mut trace_groups = Map::empty()
    
    for record in service_data {
      let current_traces = match Map::get(trace_groups, record.trace_id) {
        Some(traces) => traces,
        None => []
      }
      
      trace_groups = Map::insert(trace_groups, record.trace_id, current_traces.push(record))
    }
    
    let mut incomplete_traces = []
    let mut affected_services = []
    
    for (trace_id, records) in trace_groups.to_array() {
      // 检查是否有根span（没有父span的span）
      let root_spans = records.filter(fn(r) { r.parent_span_id == None })
      
      if root_spans.length() == 0 {
        incomplete_traces = incomplete_traces.push(trace_id)
        
        // 收集涉及的服务
        for record in records {
          if not(affected_services.contains(record.service_name)) {
            affected_services = affected_services.push(record.service_name)
          }
        }
      }
      
      // 检查是否有断开的链
      let mut span_ids = []
      let mut parent_span_ids = []
      
      for record in records {
        span_ids = span_ids.push(record.span_id)
        
        match record.parent_span_id {
          Some(parent_id) => {
            parent_span_ids = parent_span_ids.push(parent_id)
          },
          None => {}
        }
      }
      
      // 检查是否有父span_id不在span_ids中
      for parent_id in parent_span_ids {
        if not(span_ids.contains(parent_id)) {
          if not(incomplete_traces.contains(trace_id)) {
            incomplete_traces = incomplete_traces.push(trace_id)
          }
          
          // 收集涉及的服务
          for record in records {
            if not(affected_services.contains(record.service_name)) {
              affected_services = affected_services.push(record.service_name)
            }
          }
        }
      }
    }
    
    {
      check_type: "trace_chain_completeness",
      passed: incomplete_traces.length() == 0,
      affected_traces: incomplete_traces,
      affected_services,
      description: if incomplete_traces.length() > 0 { "追踪链不完整，缺少根span或有断开的链" } else { "追踪链完整" },
      severity: "high"
    }
  }
  
  // 检查跨服务时间一致性
  let check_cross_service_time_consistency = fn(service_data: Array[ServiceDataRecord], max_time_diff_ms: Int) -> CrossServiceConsistencyResult {
    // 按trace_id分组
    let mut trace_groups = Map::empty()
    
    for record in service_data {
      let current_traces = match Map::get(trace_groups, record.trace_id) {
        Some(traces) => traces,
        None => []
      }
      
      trace_groups = Map::insert(trace_groups, record.trace_id, current_traces.push(record))
    }
    
    let mut inconsistent_traces = []
    let mut affected_services = []
    
    for (trace_id, records) in trace_groups.to_array() {
      // 按时间戳排序
      let sorted_records = records.sort_by(fn(a, b) { a.timestamp - b.timestamp })
      
      // 检查时间戳顺序是否合理
      let mut time_inconsistencies = 0
      
      for i in 1..sorted_records.length() {
        let current = sorted_records[i]
        let previous = sorted_records[i - 1]
        
        // 检查子span是否在父span之后开始
        match current.parent_span_id {
          Some(parent_id) => {
            let parent_record = sorted_records.find(fn(r) { r.span_id == parent_id })
            match parent_record {
              Some(parent) => {
                if current.timestamp < parent.timestamp {
                  time_inconsistencies = time_inconsistencies + 1
                }
              },
              None => {}
            }
          },
          None => {}
        }
        
        // 检查相邻span的时间差是否过大
        let time_diff = (current.timestamp - previous.timestamp).abs()
        if time_diff > max_time_diff_ms {
          time_inconsistencies = time_inconsistencies + 1
        }
      }
      
      if time_inconsistencies > 0 {
        inconsistent_traces = inconsistent_traces.push(trace_id)
        
        // 收集涉及的服务
        for record in records {
          if not(affected_services.contains(record.service_name)) {
            affected_services = affected_services.push(record.service_name)
          }
        }
      }
    }
    
    {
      check_type: "cross_service_time_consistency",
      passed: inconsistent_traces.length() == 0,
      affected_traces: inconsistent_traces,
      affected_services,
      description: if inconsistent_traces.length() > 0 { "跨服务时间戳不一致" } else { "跨服务时间戳一致" },
      severity: "medium"
    }
  }
  
  // 检查跨服务标签一致性
  let check_cross_service_tag_consistency = fn(service_data: Array[ServiceDataRecord], required_tags: Array[String]) -> CrossServiceConsistencyResult {
    // 按trace_id分组
    let mut trace_groups = Map::empty()
    
    for record in service_data {
      let current_traces = match Map::get(trace_groups, record.trace_id) {
        Some(traces) => traces,
        None => []
      }
      
      trace_groups = Map::insert(trace_groups, record.trace_id, current_traces.push(record))
    }
    
    let mut inconsistent_traces = []
    let mut affected_services = []
    
    for (trace_id, records) in trace_groups.to_array() {
      // 检查每个必需的标签
      let mut tag_inconsistencies = 0
      
      for tag in required_tags {
        // 收集同一trace中所有服务的该标签值
        let mut tag_values = Map::empty()
        
        for record in records {
          let tag_value = match Map::get(record.tags, tag) {
            Some(value) => value,
            None => ""
          }
          
          if tag_value != "" {
            let current_values = match Map::get(tag_values, tag_value) {
              Some(values) => values,
              None => []
            }
            
            tag_values = Map::insert(tag_values, tag_value, current_values.push(record.service_name))
          }
        }
        
        // 如果同一个trace中有不同的标签值，则认为不一致
        if tag_values.size() > 1 {
          tag_inconsistencies = tag_inconsistencies + 1
        }
      }
      
      if tag_inconsistencies > 0 {
        inconsistent_traces = inconsistent_traces.push(trace_id)
        
        // 收集涉及的服务
        for record in records {
          if not(affected_services.contains(record.service_name)) {
            affected_services = affected_services.push(record.service_name)
          }
        }
      }
    }
    
    {
      check_type: "cross_service_tag_consistency",
      passed: inconsistent_traces.length() == 0,
      affected_traces: inconsistent_traces,
      affected_services,
      description: if inconsistent_traces.length() > 0 { "跨服务标签不一致" } else { "跨服务标签一致" },
      severity: "low"
    }
  }
  
  // 运行跨服务一致性检查
  let run_cross_service_consistency_checks = fn(service_data: Array[ServiceDataRecord]) -> Array[CrossServiceConsistencyResult] {
    let mut results = []
    
    // 检查追踪链完整性
    let trace_chain_result = check_trace_chain_completeness(service_data)
    results = results.push(trace_chain_result)
    
    // 检查跨服务时间一致性
    let time_consistency_result = check_cross_service_time_consistency(service_data, 5000) // 5秒最大时间差
    results = results.push(time_consistency_result)
    
    // 检查跨服务标签一致性
    let tag_consistency_result = check_cross_service_tag_consistency(service_data, ["user.id", "request.id"])
    results = results.push(tag_consistency_result)
    
    results
  }
  
  // 创建测试数据
  let service_data_records = [
    // 正常的追踪链
    {
      service_name: "api-gateway",
      trace_id: "trace-001",
      span_id: "span-001",
      parent_span_id: None,
      operation_name: "handle_request",
      timestamp: 1640995200000,
      duration_ms: 100,
      status: "ok",
      tags: Map::from([("user.id", "user123"), ("request.id", "req001")])
    },
    {
      service_name: "auth-service",
      trace_id: "trace-001",
      span_id: "span-002",
      parent_span_id: Some("span-001"),
      operation_name: "authenticate",
      timestamp: 1640995200050,
      duration_ms: 80,
      status: "ok",
      tags: Map::from([("user.id", "user123"), ("request.id", "req001")])
    },
    {
      service_name: "user-service",
      trace_id: "trace-001",
      span_id: "span-003",
      parent_span_id: Some("span-001"),
      operation_name: "get_user",
      timestamp: 1640995200100,
      duration_ms: 60,
      status: "ok",
      tags: Map::from([("user.id", "user123"), ("request.id", "req001")])
    },
    
    // 不完整的追踪链 - 缺少根span
    {
      service_name: "auth-service",
      trace_id: "trace-002",
      span_id: "span-004",
      parent_span_id: Some("span-999"), // 不存在的父span
      operation_name: "authenticate",
      timestamp: 1640995202000,
      duration_ms: 90,
      status: "ok",
      tags: Map::from([("user.id", "user456"), ("request.id", "req002")])
    },
    {
      service_name: "user-service",
      trace_id: "trace-002",
      span_id: "span-005",
      parent_span_id: Some("span-004"),
      operation_name: "get_user",
      timestamp: 1640995202050,
      duration_ms: 70,
      status: "ok",
      tags: Map::from([("user.id", "user456"), ("request.id", "req002")])
    },
    
    // 时间不一致的追踪链
    {
      service_name: "api-gateway",
      trace_id: "trace-003",
      span_id: "span-006",
      parent_span_id: None,
      operation_name: "handle_request",
      timestamp: 1640995203000,
      duration_ms: 100,
      status: "ok",
      tags: Map::from([("user.id", "user789"), ("request.id", "req003")])
    },
    {
      service_name: "payment-service",
      trace_id: "trace-003",
      span_id: "span-007",
      parent_span_id: Some("span-006"),
      operation_name: "process_payment",
      timestamp: 1640995202500, // 早于父span的时间戳
      duration_ms: 200,
      status: "ok",
      tags: Map::from([("user.id", "user789"), ("request.id", "req003")])
    },
    
    // 标签不一致的追踪链
    {
      service_name: "api-gateway",
      trace_id: "trace-004",
      span_id: "span-008",
      parent_span_id: None,
      operation_name: "handle_request",
      timestamp: 1640995204000,
      duration_ms: 100,
      status: "ok",
      tags: Map::from([("user.id", "user001"), ("request.id", "req004")])
    },
    {
      service_name: "order-service",
      trace_id: "trace-004",
      span_id: "span-009",
      parent_span_id: Some("span-008"),
      operation_name: "create_order",
      timestamp: 1640995204050,
      duration_ms: 150,
      status: "ok",
      tags: Map::from([("user.id", "user002"), ("request.id", "req004")]) // 不同的user.id
    }
  ]
  
  // 运行跨服务一致性检查
  let consistency_results = run_cross_service_consistency_checks(service_data_records)
  
  // 验证结果
  assert_eq(consistency_results.length(), 3)
  
  // 检查追踪链完整性结果
  let trace_chain_result = consistency_results.find(fn(r) { r.check_type == "trace_chain_completeness" })
  match trace_chain_result {
    Some(result) => {
      assert_false(result.passed) // 应该失败，因为trace-002缺少根span
      assert_true(result.affected_traces.contains("trace-002"))
      assert_true(result.affected_services.contains("auth-service"))
      assert_true(result.affected_services.contains("user-service"))
    },
    None => assert_true(false)
  }
  
  // 检查跨服务时间一致性结果
  let time_consistency_result = consistency_results.find(fn(r) { r.check_type == "cross_service_time_consistency" })
  match time_consistency_result {
    Some(result) => {
      assert_false(result.passed) // 应该失败，因为trace-003有时间不一致
      assert_true(result.affected_traces.contains("trace-003"))
      assert_true(result.affected_services.contains("api-gateway"))
      assert_true(result.affected_services.contains("payment-service"))
    },
    None => assert_true(false)
  }
  
  // 检查跨服务标签一致性结果
  let tag_consistency_result = consistency_results.find(fn(r) { r.check_type == "cross_service_tag_consistency" })
  match tag_consistency_result {
    Some(result) => {
      assert_false(result.passed) // 应该失败，因为trace-004的user.id不一致
      assert_true(result.affected_traces.contains("trace-004"))
      assert_true(result.affected_services.contains("api-gateway"))
      assert_true(result.affected_services.contains("order-service"))
    },
    None => assert_true(false)
  }
}

// 测试3: 数据版本控制与一致性恢复
test "数据版本控制与一致性恢复功能" {
  // 定义数据版本
  type DataVersion = {
    version_id: String
    timestamp: Int
    description: String
    checksum: String
    is_consistent: Bool
  }
  
  // 定义数据变更记录
  type DataChange = {
    change_id: String
    record_id: String
    field: String
    old_value: String
    new_value: String
    change_timestamp: Int
    version_id: String
    change_reason: String
  }
  
  // 定义一致性恢复策略
  enum RecoveryStrategy {
    Rollback         // 回滚到上一个一致版本
    ForwardRecovery  // 前向恢复，应用后续变更
    ManualResolution // 手动解决
    Ignore           // 忽略不一致
  }
  
  // 定义恢复计划
  type RecoveryPlan = {
    inconsistency_id: String
    strategy: RecoveryStrategy
    target_version: Option[String]
    steps: Array[String]
    estimated_impact: String
  }
  
  // 计算数据校验和
  let calculate_checksum = fn(data: Array[Map[String, String]]) -> String {
    // 简化的校验和计算
    let concatenated = data.reduce(fn(acc, record) => {
      let record_str = record.to_array().reduce(fn(rec_acc, pair) => {
        let (key, value) = pair
        rec_acc + key + ":" + value + ";"
      }, "")
      acc + record_str
    }, "")
    
    // 简化的哈希：使用字符串长度和第一个/最后一个字符
    let length = concatenated.length()
    let first_char = if length > 0 { concatenated[0] } else { '0' }
    let last_char = if length > 0 { concatenated[length - 1] } else { '0' }
    
    "checksum-" + length.to_string() + "-" + first_char.to_string() + "-" + last_char.to_string()
  }
  
  // 检测数据不一致
  let detect_inconsistencies = fn(data: Array[Map[String, String]], expected_checksum: String) -> Array[String] {
    let actual_checksum = calculate_checksum(data)
    
    if actual_checksum != expected_checksum {
      // 简化的不一致检测：返回一些假设的不一致ID
      ["inconsistency-001", "inconsistency-002"]
    } else {
      []
    }
  }
  
  // 创建数据版本
  let create_data_version = fn(data: Array[Map[String, String]], description: String, expected_checksum: String) -> DataVersion {
    let checksum = calculate_checksum(data)
    let is_consistent = checksum == expected_checksum
    
    {
      version_id: "version-" + (1640995200).to_string(),
      timestamp: 1640995200,
      description,
      checksum,
      is_consistent
    }
  }
  
  // 记录数据变更
  let record_data_change = fn(record_id: String, field: String, old_value: String, new_value: String, version_id: String, reason: String) -> DataChange {
    {
      change_id: "change-" + (1640995200).to_string(),
      record_id,
      field,
      old_value,
      new_value,
      change_timestamp: 1640995200,
      version_id,
      change_reason: reason
    }
  }
  
  // 生成恢复计划
  let generate_recovery_plan = fn(inconsistency_id: String, available_versions: Array[DataVersion], changes: Array[DataChange]) -> RecoveryPlan {
    // 查找最近的一致版本
    let consistent_versions = available_versions.filter(fn(v) { v.is_consistent })
    
    let (strategy, target_version) = if consistent_versions.length() > 0 {
      // 找到最新的一致版本
      let latest_consistent = consistent_versions.reduce(fn(acc, v) => {
        if v.timestamp > acc.timestamp { v } else { acc }
      }, consistent_versions[0])
      
      (RecoveryStrategy::Rollback, Some(latest_consistent.version_id))
    } else {
      (RecoveryStrategy::ManualResolution, None)
    }
    
    // 生成恢复步骤
    let steps = match strategy {
      RecoveryStrategy::Rollback => {
        target_version.map(fn(version) => {
          // 查找需要回滚的变更
          let rollback_changes = changes.filter(fn(c) { c.version_id > version })
          
          [
            "1. 识别需要回滚的变更",
            "2. 备份当前数据状态",
            "3. 应用回滚操作到版本 " + version,
            "4. 验证数据一致性",
            "5. 更新元数据"
          ]
        }).unwrap_or([])
      },
      RecoveryStrategy::ForwardRecovery => {
        [
          "1. 识别不一致的数据",
          "2. 应用前向修复",
          "3. 验证修复结果",
          "4. 更新数据版本"
        ]
      },
      RecoveryStrategy::ManualResolution => {
        [
          "1. 标记不一致的数据",
          "2. 通知数据管理员",
          "3. 手动审查和修复",
          "4. 验证修复结果"
        ]
      },
      RecoveryStrategy::Ignore => {
        [
          "1. 记录不一致问题",
          "2. 监控后续影响",
          "3. 定期检查是否自动解决"
        ]
      }
    }
    
    // 估算影响
    let estimated_impact = match strategy {
      RecoveryStrategy::Rollback => "可能影响最近的数据变更",
      RecoveryStrategy::ForwardRecovery => "可能影响数据完整性",
      RecoveryStrategy::ManualResolution => "需要人工干预，解决时间不确定",
      RecoveryStrategy::Ignore => "可能导致后续数据分析错误"
    }
    
    {
      inconsistency_id,
      strategy,
      target_version,
      steps,
      estimated_impact
    }
  }
  
  // 执行恢复操作
  let execute_recovery = fn(data: Array[Map[String, String]], plan: RecoveryPlan) -> Array[Map[String, String]] {
    match plan.strategy {
      RecoveryStrategy::Rollback => {
        // 简化的回滚操作：返回一个模拟的"已回滚"数据集
        data.map(fn(record) => {
          let mut updated_record = record
          updated_record = Map::insert(updated_record, "recovery_status", "rolled_back")
          updated_record = Map::insert(updated_record, "recovery_timestamp", "1640995300")
          updated_record
        })
      },
      RecoveryStrategy::ForwardRecovery => {
        // 简化的前向恢复：标记并修复不一致的数据
        data.map(fn(record) => {
          let mut updated_record = record
          
          // 检查是否有不一致的字段
          let status = match Map::get(record, "status") {
            Some(s) => s,
            None => ""
          }
          
          if status == "inconsistent" {
            updated_record = Map::insert(updated_record, "status", "fixed")
            updated_record = Map::insert(updated_record, "recovery_status", "forward_recovered")
          }
          
          updated_record
        })
      },
      RecoveryStrategy::ManualResolution => {
        // 标记需要手动解决的数据
        data.map(fn(record) => {
          let mut updated_record = record
          updated_record = Map::insert(updated_record, "recovery_status", "requires_manual_resolution")
          updated_record
        })
      },
      RecoveryStrategy::Ignore => {
        // 不做任何更改，只添加标记
        data.map(fn(record) => {
          let mut updated_record = record
          updated_record = Map::insert(updated_record, "recovery_status", "inconsistency_ignored")
          updated_record
        })
      }
    }
  }
  
  // 创建测试数据
  let initial_data = [
    Map::from([
      ("id", "record-001"),
      ("trace_id", "trace-001"),
      ("service_name", "auth-service"),
      ("status", "ok"),
      ("timestamp", "1640995200000")
    ]),
    Map::from([
      ("id", "record-002"),
      ("trace_id", "trace-001"),
      ("service_name", "user-service"),
      ("status", "ok"),
      ("timestamp", "1640995200050")
    ])
  ]
  
  // 创建初始版本
  let initial_checksum = calculate_checksum(initial_data)
  let version1 = create_data_version(initial_data, "Initial version", initial_checksum)
  
  // 模拟数据变更
  let updated_data = [
    Map::from([
      ("id", "record-001"),
      ("trace_id", "trace-001"),
      ("service_name", "auth-service"),
      ("status", "ok"),
      ("timestamp", "1640995200000")
    ]),
    Map::from([
      ("id", "record-002"),
      ("trace_id", "trace-001"),
      ("service_name", "user-service"),
      ("status", "error"), // 状态变更
      ("timestamp", "1640995200050")
    ])
  ]
  
  let changes = [
    record_data_change("record-002", "status", "ok", "error", "version-1640995201", "Error occurred during processing")
  ]
  
  // 创建更新版本
  let updated_checksum = "checksum-invalid" // 模拟不一致的校验和
  let version2 = create_data_version(updated_data, "Updated with error", updated_checksum)
  
  // 检测不一致
  let inconsistencies = detect_inconsistencies(updated_data, initial_checksum)
  
  // 验证不一致检测
  assert_true(inconsistencies.length() > 0)
  
  // 生成恢复计划
  let available_versions = [version1, version2]
  let recovery_plan = generate_recovery_plan(inconsistencies[0], available_versions, changes)
  
  // 验证恢复计划
  assert_eq(recovery_plan.inconsistency_id, inconsistencies[0])
  assert_eq(recovery_plan.strategy, RecoveryStrategy::Rollback)
  assert_eq(recovery_plan.target_version, Some(version1.version_id))
  assert_true(recovery_plan.steps.length() > 0)
  
  // 执行恢复
  let recovered_data = execute_recovery(updated_data, recovery_plan)
  
  // 验证恢复结果
  assert_eq(recovered_data.length(), updated_data.length())
  
  for record in recovered_data {
    let recovery_status = match Map::get(record, "recovery_status") {
      Some(status) => status,
      None => ""
    }
    
    assert_eq(recovery_status, "rolled_back")
  }
  
  // 测试前向恢复策略
  let forward_recovery_plan = {
    inconsistency_id: inconsistencies[0],
    strategy: RecoveryStrategy::ForwardRecovery,
    target_version: None,
    steps: [],
    estimated_impact: ""
  }
  
  let forward_recovered_data = execute_recovery(updated_data, forward_recovery_plan)
  
  // 验证前向恢复结果
  let inconsistent_record = forward_recovered_data.find(fn(record) => {
    match Map::get(record, "status") {
      Some(status) => status == "error",
      None => false
    }
  })
  
  match inconsistent_record {
    Some(record) => {
      let recovery_status = match Map::get(record, "recovery_status") {
        Some(status) => status,
        None => ""
      }
      
      assert_eq(recovery_status, "forward_recovered")
      
      let status = match Map::get(record, "status") {
        Some(status) => status,
        None => ""
      }
      
      assert_eq(status, "fixed")
    },
    None => assert_true(false)
  }
}