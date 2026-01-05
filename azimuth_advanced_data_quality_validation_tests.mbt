// Azimuth 高级数据质量验证测试用例
// 专注于遥测数据的质量检查、验证和清洗功能

// 测试1: 遥测数据完整性验证
test "遥测数据完整性验证" {
  // 模拟不同完整性的遥测数据
  let telemetry_data = [
    { 
      timestamp: 1640995200, 
      trace_id: "trace-001", 
      span_id: "span-001", 
      service_name: "user-service", 
      operation_name: "get_user", 
      duration_ms: 120, 
      status: "ok",
      tags: [("user_id", "12345"), ("region", "us-west")],
      metrics: [("cpu", 45.2), ("memory", 512.3)],
      logs: [("User 12345 retrieved", "INFO")],
      completeness_score: 1.0
    },
    { 
      timestamp: 1640995205, 
      trace_id: "trace-002", 
      span_id: "span-002", 
      service_name: "order-service", 
      operation_name: "create_order", 
      duration_ms: 0,  // 缺失值
      status: "",
      tags: [],
      metrics: [("cpu", 0.0), ("memory", 0.0)],  // 缺失值
      logs: [],
      completeness_score: 0.6
    },
    { 
      timestamp: 0,  // 缺失值
      trace_id: "",  // 缺失值
      span_id: "span-003", 
      service_name: "payment-service", 
      operation_name: "process_payment", 
      duration_ms: 250, 
      status: "error",
      tags: [("payment_id", "pay-001")],
      metrics: [("cpu", 55.8)],
      logs: [("Payment failed", "ERROR")],
      completeness_score: 0.7
    },
    { 
      timestamp: 1640995215, 
      trace_id: "trace-004", 
      span_id: "span-004", 
      service_name: "inventory-service", 
      operation_name: "check_stock", 
      duration_ms: 80, 
      status: "ok",
      tags: [("product_id", "prod-123"), ("warehouse", "wh-01")],
      metrics: [("cpu", 35.5), ("memory", 256.7), ("disk_io", 120.3)],
      logs: [("Stock checked for prod-123", "INFO"), ("Available: 50 units", "INFO")],
      completeness_score: 1.0
    },
    { 
      timestamp: 1640995220, 
      trace_id: "trace-005", 
      span_id: "span-005", 
      service_name: "notification-service", 
      operation_name: "send_notification", 
      duration_ms: -50,  // 异常值
      status: "unknown",  // 异常值
      tags: [("channel", "email"), ("recipient", "user@example.com")],
      metrics: [("cpu", 999.9)],  // 异常值
      logs: [("Notification sent", "INFO")],
      completeness_score: 0.8
    }
  ]
  
  // 验证必需字段完整性
  let validate_required_fields = fn(data: Array<TelemetryData>) {
    let required_fields = ["timestamp", "trace_id", "span_id", "service_name", "operation_name"]
    let mut validation_results = []
    
    for record in data {
      let mut missing_fields = []
      let mut present_fields = []
      
      // 检查必需字段
      if record.timestamp == 0 {
        missing_fields = missing_fields.push("timestamp")
      } else {
        present_fields = present_fields.push("timestamp")
      }
      
      if record.trace_id == "" {
        missing_fields = missing_fields.push("trace_id")
      } else {
        present_fields = present_fields.push("trace_id")
      }
      
      if record.span_id == "" {
        missing_fields = missing_fields.push("span_id")
      } else {
        present_fields = present_fields.push("span_id")
      }
      
      if record.service_name == "" {
        missing_fields = missing_fields.push("service_name")
      } else {
        present_fields = present_fields.push("service_name")
      }
      
      if record.operation_name == "" {
        missing_fields = missing_fields.push("operation_name")
      } else {
        present_fields = present_fields.push("operation_name")
      }
      
      let completeness_score = present_fields.length().to_float() / required_fields.length().to_float()
      
      validation_results = validation_results.push({
        span_id: record.span_id,
        missing_fields: missing_fields,
        present_fields: present_fields,
        completeness_score: completeness_score,
        is_complete: missing_fields.length() == 0
      })
    }
    
    validation_results
  }
  
  // 验证数据类型一致性
  let validate_data_types = fn(data: Array<TelemetryData>) {
    let mut type_validation_results = []
    
    for record in data {
      let mut type_issues = []
      
      // 验证时间戳类型
      if record.timestamp != 0 and record.timestamp < 1000000000 {
        type_issues = type_issues.push({
          field: "timestamp",
          expected_type: "unix_timestamp",
          actual_value: record.timestamp.to_string(),
          issue: "timestamp appears to be in seconds, not milliseconds"
        })
      }
      
      // 验证持续时间类型
      if record.duration_ms < 0 {
        type_issues = type_issues.push({
          field: "duration_ms",
          expected_type: "non_negative_integer",
          actual_value: record.duration_ms.to_string(),
          issue: "duration cannot be negative"
        })
      }
      
      // 验证状态值
      let valid_statuses = ["ok", "error", "timeout", "cancelled"]
      if record.status != "" and not valid_statuses.contains(record.status) {
        type_issues = type_issues.push({
          field: "status",
          expected_type: "valid_status",
          actual_value: record.status,
          issue: "status must be one of: " + valid_statuses.join(", ")
        })
      }
      
      // 验证指标值
      for metric in record.metrics {
        if metric.1 < 0 or metric.1.is_nan() or metric.1.is_infinite() {
          type_issues = type_issues.push({
            field: "metric_" + metric.0,
            expected_type: "finite_number",
            actual_value: metric.1.to_string(),
            issue: "metric values must be finite and non-negative"
          })
        }
      }
      
      type_validation_results = type_validation_results.push({
        span_id: record.span_id,
        type_issues: type_issues,
        is_valid: type_issues.length() == 0
      })
    }
    
    type_validation_results
  }
  
  // 验证业务规则一致性
  let validate_business_rules = fn(data: Array<TelemetryData>) {
    let mut business_validation_results = []
    
    for record in data {
      let mut rule_violations = []
      
      // 规则1: 错误状态的持续时间应该相对较短
      if record.status == "error" and record.duration_ms > 10000 {
        rule_violations = rule_violations.push({
          rule: "error_duration_limit",
          description: "Error operations should complete within 10 seconds",
          actual_value: record.duration_ms.to_string(),
          severity: "warning"
        })
      }
      
      // 规则2: CPU使用率应该在合理范围内
      for metric in record.metrics {
        if metric.0 == "cpu" and (metric.1 > 100.0 or metric.1 < 0.0) {
          rule_violations = rule_violations.push({
            rule: "cpu_usage_range",
            description: "CPU usage should be between 0% and 100%",
            actual_value: metric.1.to_string(),
            severity: "error"
          })
        }
      }
      
      // 规则3: 内存使用率应该在合理范围内
      for metric in record.metrics {
        if metric.0 == "memory" and (metric.1 < 0.0 or metric.1 > 100000.0) {
          rule_violations = rule_violations.push({
            rule: "memory_usage_range",
            description: "Memory usage should be positive and reasonable",
            actual_value: metric.1.to_string(),
            severity: "warning"
          })
        }
      }
      
      // 规则4: 操作名称应该符合命名约定
      let valid_operation_patterns = ["get_", "create_", "update_", "delete_", "process_", "check_", "send_"]
      let operation_valid = valid_operation_patterns.any_fn(pattern) { 
        record.operation_name.starts_with(pattern)
      }
      
      if not operation_valid {
        rule_violations = rule_violations.push({
          rule: "operation_naming_convention",
          description: "Operation names should follow standard naming conventions",
          actual_value: record.operation_name,
          severity: "info"
        })
      }
      
      business_validation_results = business_validation_results.push({
        span_id: record.span_id,
        rule_violations: rule_violations,
        is_compliant: rule_violations.length() == 0
      })
    }
    
    business_validation_results
  }
  
  // 计算整体数据质量评分
  let calculate_data_quality_score = fn(
    field_validation: Array[FieldValidationResult>,
    type_validation: Array[TypeValidationResult>,
    business_validation: Array[BusinessValidationResult>
  ) {
    let mut total_score = 0.0
    let mut weight_sum = 0.0
    
    // 字段完整性权重: 40%
    let field_completeness = field_validation.map_fn(r) { r.completeness_score }.sum() / field_validation.length().to_float()
    total_score = total_score + (field_completeness * 0.4)
    weight_sum = weight_sum + 0.4
    
    // 类型一致性权重: 30%
    let type_consistency = type_validation.map_fn(r) { if r.is_valid { 1.0 } else { 0.0 } }.sum() / type_validation.length().to_float()
    total_score = total_score + (type_consistency * 0.3)
    weight_sum = weight_sum + 0.3
    
    // 业务规则一致性权重: 30%
    let business_compliance = business_validation.map_fn(r) { if r.is_compliant { 1.0 } else { 0.0 } }.sum() / business_validation.length().to_float()
    total_score = total_score + (business_compliance * 0.3)
    weight_sum = weight_sum + 0.3
    
    if weight_sum > 0.0 {
      total_score / weight_sum * 100.0
    } else {
      0.0
    }
  }
  
  // 执行数据质量验证
  let field_validation = validate_required_fields(telemetry_data)
  let type_validation = validate_data_types(telemetry_data)
  let business_validation = validate_business_rules(telemetry_data)
  let quality_score = calculate_data_quality_score(field_validation, type_validation, business_validation)
  
  // 验证字段完整性结果
  assert_eq(field_validation.length(), 5)
  
  // 验证第一个记录（完整）
  let record1_validation = field_validation[0]
  assert_eq(record1_validation.span_id, "span-001")
  assert_eq(record1_validation.missing_fields.length(), 0)
  assert_eq(record1_validation.present_fields.length(), 5)
  assert_eq(record1_validation.completeness_score, 1.0)
  assert_true(record1_validation.is_complete)
  
  // 验证第二个记录（部分缺失）
  let record2_validation = field_validation[1]
  assert_eq(record2_validation.span_id, "span-002")
  assert_eq(record2_validation.missing_fields.length(), 1)  // trace_id
  assert_eq(record2_validation.present_fields.length(), 4)
  assert_eq(record2_validation.completeness_score, 0.8)  // 4/5
  assert_false(record2_validation.is_complete)
  
  // 验证第三个记录（部分缺失）
  let record3_validation = field_validation[2]
  assert_eq(record3_validation.span_id, "span-003")
  assert_eq(record3_validation.missing_fields.length(), 2)  // timestamp, trace_id
  assert_eq(record3_validation.present_fields.length(), 3)
  assert_eq(record3_validation.completeness_score, 0.6)  // 3/5
  assert_false(record3_validation.is_complete)
  
  // 验证类型一致性结果
  assert_eq(type_validation.length(), 5)
  
  // 验证第五个记录的类型问题
  let record5_type_validation = type_validation[4]
  assert_eq(record5_type_validation.span_id, "span-005")
  assert_eq(record5_type_validation.type_issues.length(), 3)  // duration_ms, status, cpu
  assert_false(record5_type_validation.is_valid)
  
  // 验证业务规则一致性结果
  assert_eq(business_validation.length(), 5)
  
  // 验证第五个记录的业务规则问题
  let record5_business_validation = business_validation[4]
  assert_eq(record5_business_validation.span_id, "span-005")
  assert_eq(record5_business_validation.rule_violations.length(), 2)  // cpu_usage_range, operation_naming_convention
  assert_false(record5_business_validation.is_compliant)
  
  // 验证整体数据质量评分
  assert_true(quality_score > 0.0)
  assert_true(quality_score < 100.0)
}

// 测试2: 遥测数据一致性验证
test "遥测数据一致性验证" {
  // 模拟相关联的遥测数据
  let related_telemetry_data = [
    // 根span
    { 
      timestamp: 1640995200, 
      trace_id: "trace-001", 
      span_id: "span-001", 
      parent_span_id: "", 
      service_name: "api-gateway", 
      operation_name: "process_request", 
      duration_ms: 500, 
      status: "ok"
    },
    // 子span1
    { 
      timestamp: 1640995201, 
      trace_id: "trace-001", 
      span_id: "span-002", 
      parent_span_id: "span-001", 
      service_name: "user-service", 
      operation_name: "get_user", 
      duration_ms: 200, 
      status: "ok"
    },
    // 子span2
    { 
      timestamp: 1640995202, 
      trace_id: "trace-001", 
      span_id: "span-003", 
      parent_span_id: "span-001", 
      service_name: "order-service", 
      operation_name: "create_order", 
      duration_ms: 300, 
      status: "ok"
    },
    // 不一致的子span（时间戳早于父span）
    { 
      timestamp: 1640995195, 
      trace_id: "trace-002", 
      span_id: "span-004", 
      parent_span_id: "span-005", 
      service_name: "payment-service", 
      operation_name: "process_payment", 
      duration_ms: 250, 
      status: "ok"
    },
    // 父span
    { 
      timestamp: 1640995210, 
      trace_id: "trace-002", 
      span_id: "span-005", 
      parent_span_id: "", 
      service_name: "api-gateway", 
      operation_name: "process_request", 
      duration_ms: 400, 
      status: "ok"
    },
    // 孤儿span（父span不存在）
    { 
      timestamp: 1640995220, 
      trace_id: "trace-003", 
      span_id: "span-006", 
      parent_span_id: "span-999", 
      service_name: "inventory-service", 
      operation_name: "check_stock", 
      duration_ms: 150, 
      status: "ok"
    },
    // 循环引用（span-007的父是span-008，span-008的父是span-007）
    { 
      timestamp: 1640995230, 
      trace_id: "trace-004", 
      span_id: "span-007", 
      parent_span_id: "span-008", 
      service_name: "notification-service", 
      operation_name: "send_notification", 
      duration_ms: 100, 
      status: "ok"
    },
    { 
      timestamp: 1640995231, 
      trace_id: "trace-004", 
      span_id: "span-008", 
      parent_span_id: "span-007", 
      service_name: "email-service", 
      operation_name: "send_email", 
      duration_ms: 80, 
      status: "ok"
    }
  ]
  
  // 验证追踪层次结构一致性
  let validate_trace_hierarchy = fn(data: Array<RelatedTelemetryData>) {
    let mut hierarchy_issues = []
    
    // 按trace_id分组
    let trace_groups = data.group_by_fn(d) { d.trace_id }
    
    for (trace_id, trace_spans) in trace_groups {
      // 构建span映射
      let span_map = {}
      for span in trace_spans {
        span_map[span.span_id] = span
      }
      
      // 检查每个span的父span是否存在
      for span in trace_spans {
        if span.parent_span_id != "" and not span_map.has_key(span.parent_span_id) {
          hierarchy_issues = hierarchy_issues.push({
            issue_type: "orphan_span",
            trace_id: trace_id,
            span_id: span.span_id,
            parent_span_id: span.parent_span_id,
            description: "Span references non-existent parent span"
          })
        }
      }
      
      // 检查循环引用
      let visited_spans = []
      let recursion_stack = []
      
      let detect_cycle = fn(span_id: String) {
        if recursion_stack.contains(span_id) {
          return true  // 发现循环
        }
        
        if visited_spans.contains(span_id) {
          return false  // 已访问过，无循环
        }
        
        visited_spans = visited_spans.push(span_id)
        recursion_stack = recursion_stack.push(span_id)
        
        if span_map.has_key(span_id) {
          let span = span_map[span_id]
          if span.parent_span_id != "" {
            return detect_cycle(span.parent_span_id)
          }
        }
        
        recursion_stack = recursion_stack.slice(0, recursion_stack.length() - 1)
        return false
      }
      
      for span in trace_spans {
        visited_spans = []
        recursion_stack = []
        
        if detect_cycle(span.span_id) {
          hierarchy_issues = hierarchy_issues.push({
            issue_type: "circular_reference",
            trace_id: trace_id,
            span_id: span.span_id,
            parent_span_id: span.parent_span_id,
            description: "Circular reference detected in span hierarchy"
          })
        }
      }
      
      // 检查时间戳一致性
      let root_spans = trace_spans.filter_fn(s) { s.parent_span_id == "" }
      
      for root_span in root_spans {
        let check_timestamp_consistency = fn(span: RelatedTelemetryData, root_timestamp: Int) {
          if span.timestamp < root_timestamp {
            hierarchy_issues = hierarchy_issues.push({
              issue_type: "timestamp_inconsistency",
              trace_id: trace_id,
              span_id: span.span_id,
              parent_span_id: span.parent_span_id,
              description: "Span timestamp is earlier than root span timestamp"
            })
          }
          
          // 递归检查子span
          let child_spans = trace_spans.filter_fn(s) { s.parent_span_id == span.span_id }
          for child in child_spans {
            check_timestamp_consistency(child, root_timestamp)
          }
        }
        
        check_timestamp_consistency(root_span, root_span.timestamp)
      }
      
      // 检查持续时间一致性
      for span in trace_spans {
        if span.duration_ms < 0 {
          hierarchy_issues = hierarchy_issues.push({
            issue_type: "invalid_duration",
            trace_id: trace_id,
            span_id: span.span_id,
            parent_span_id: span.parent_span_id,
            description: "Span duration is negative"
          })
        }
        
        // 检查子span持续时间是否超过父span
        if span.parent_span_id != "" and span_map.has_key(span.parent_span_id) {
          let parent_span = span_map[span.parent_span_id]
          if span.duration_ms > parent_span.duration_ms {
            hierarchy_issues = hierarchy_issues.push({
              issue_type: "duration_inconsistency",
              trace_id: trace_id,
              span_id: span.span_id,
              parent_span_id: span.parent_span_id,
              description: "Child span duration exceeds parent span duration"
            })
          }
        }
      }
    }
    
    hierarchy_issues
  }
  
  // 验证服务调用一致性
  let validate_service_call_consistency = fn(data: Array[RelatedTelemetryData>) {
    let mut call_consistency_issues = []
    
    // 按服务分组
    let service_groups = data.group_by_fn(d) { d.service_name }
    
    for (service_name, service_spans) in service_groups {
      // 检查服务内操作名称的一致性
      let operation_names = service_spans.map_fn(s) { s.operation_name }.unique()
      
      // 检查操作名称是否符合命名约定
      for operation_name in operation_names {
        let valid_patterns = ["get_", "create_", "update_", "delete_", "process_", "check_", "send_"]
        let is_valid = valid_patterns.any_fn(pattern) { operation_name.starts_with(pattern) }
        
        if not is_valid {
          call_consistency_issues = call_consistency_issues.push({
            issue_type: "operation_naming_inconsistency",
            service_name: service_name,
            operation_name: operation_name,
            description: "Operation name doesn't follow standard naming convention"
          })
        }
      }
      
      // 检查持续时间的一致性（同一操作的持续时间不应差异过大）
      let operation_groups = service_spans.group_by_fn(s) { s.operation_name }
      
      for (operation_name, operation_spans) in operation_groups {
        if operation_spans.length() > 1 {
          let durations = operation_spans.map_fn(s) { s.duration_ms }
          let avg_duration = durations.sum() / durations.length()
          
          for span in operation_spans {
            let deviation = (span.duration_ms - avg_duration).abs()
            let deviation_percent = (deviation.to_float() / avg_duration.to_float()) * 100.0
            
            if deviation_percent > 200.0 {  // 偏差超过200%
              call_consistency_issues = call_consistency_issues.push({
                issue_type: "duration_variance",
                service_name: service_name,
                operation_name: operation_name,
                span_id: span.span_id,
                duration_ms: span.duration_ms,
                avg_duration_ms: avg_duration,
                deviation_percent: deviation_percent,
                description: "Span duration deviates significantly from average"
              })
            }
          }
        }
      }
    }
    
    call_consistency_issues
  }
  
  // 验证跨服务调用一致性
  let validate_cross_service_consistency = fn(data: Array[RelatedTelemetryData>) {
    let mut cross_service_issues = []
    
    // 构建服务调用图
    let service_calls = []
    
    for span in data {
      if span.parent_span_id != "" {
        // 查找父span
        let parent_span = data.find_fn(s) { s.span_id == span.parent_span_id }
        
        if parent_span.is_some() {
          let parent = parent_span.unwrap()
          
          // 如果是跨服务调用
          if parent.service_name != span.service_name {
            service_calls = service_calls.push({
              caller_service: parent.service_name,
              callee_service: span.service_name,
              caller_operation: parent.operation_name,
              callee_operation: span.operation_name,
              trace_id: span.trace_id,
              caller_span_id: parent.span_id,
              callee_span_id: span.span_id
            })
          }
        }
      }
    }
    
    // 检查服务调用的一致性
    let call_groups = service_calls.group_by_fn(call) { 
      call.caller_service + "->" + call.callee_service 
    }
    
    for (call_pair, calls) in call_groups {
      if calls.length() > 1 {
        // 检查相同服务调用的操作一致性
        let caller_operations = calls.map_fn(c) { c.caller_operation }.unique()
        let callee_operations = calls.map_fn(c) { c.callee_operation }.unique()
        
        // 如果同一服务调用对有多个不同的操作组合
        if caller_operations.length() > 1 or callee_operations.length() > 1 {
          cross_service_issues = cross_service_issues.push({
            issue_type: "inconsistent_service_call",
            call_pair: call_pair,
            caller_operations: caller_operations,
            callee_operations: callee_operations,
            description: "Same service call pair has inconsistent operations"
          })
        }
      }
    }
    
    cross_service_issues
  }
  
  // 执行一致性验证
  let hierarchy_issues = validate_trace_hierarchy(related_telemetry_data)
  let call_consistency_issues = validate_service_call_consistency(related_telemetry_data)
  let cross_service_issues = validate_cross_service_consistency(related_telemetry_data)
  
  // 验证追踪层次结构一致性
  assert_eq(hierarchy_issues.length(), 4)
  
  // 验证孤儿span问题
  let orphan_span_issues = hierarchy_issues.filter_fn(issue) { issue.issue_type == "orphan_span" }
  assert_eq(orphan_span_issues.length(), 1)
  assert_eq(orphan_span_issues[0].span_id, "span-006")
  assert_eq(orphan_span_issues[0].parent_span_id, "span-999")
  
  // 验证循环引用问题
  let circular_ref_issues = hierarchy_issues.filter_fn(issue) { issue.issue_type == "circular_reference" }
  assert_eq(circular_ref_issues.length(), 2)  // span-007和span-008都会检测到循环
  
  // 验证时间戳不一致问题
  let timestamp_issues = hierarchy_issues.filter_fn(issue) { issue.issue_type == "timestamp_inconsistency" }
  assert_eq(timestamp_issues.length(), 1)
  assert_eq(timestamp_issues[0].span_id, "span-004")
  
  // 验证服务调用一致性
  assert_eq(call_consistency_issues.length(), 0)  // 在这个测试数据中没有服务调用一致性问题
  
  // 验证跨服务调用一致性
  assert_eq(cross_service_issues.length(), 0)  // 在这个测试数据中没有跨服务调用一致性问题
}

// 测试3: 遥测数据时效性验证
test "遥测数据时效性验证" {
  // 模拟不同时效性的遥测数据
  let telemetry_time_data = [
    { 
      timestamp: 1640995200000,  // 2022-01-01 00:00:00 UTC
      ingestion_time: 1640995201000,  // 1秒后摄入
      trace_id: "trace-001", 
      span_id: "span-001", 
      service_name: "user-service",
      data_age_ms: 1000
    },
    { 
      timestamp: 1640995205000,  // 2022-01-01 00:00:05 UTC
      ingestion_time: 1640995215000,  // 10秒后摄入
      trace_id: "trace-002", 
      span_id: "span-002", 
      service_name: "order-service",
      data_age_ms: 10000
    },
    { 
      timestamp: 1640995100000,  // 2022-01-01 00:00:00 UTC (10分钟前)
      ingestion_time: 1640995200000,  // 现在才摄入
      trace_id: "trace-003", 
      span_id: "span-003", 
      service_name: "payment-service",
      data_age_ms: 600000  // 10分钟延迟
    },
    { 
      timestamp: 1640995199000,  // 2022-01-01 00:00:00 UTC (1秒前)
      ingestion_time: 1640995200000,  // 现在摄入
      trace_id: "trace-004", 
      span_id: "span-004", 
      service_name: "inventory-service",
      data_age_ms: 1000
    },
    { 
      timestamp: 1640995200000,  // 2022-01-01 00:00:00 UTC
      ingestion_time: 1640995300000,  // 100秒后摄入
      trace_id: "trace-005", 
      span_id: "span-005", 
      service_name: "notification-service",
      data_age_ms: 100000
    }
  ]
  
  // 验证数据延迟
  let validate_data_latency = fn(data: Array<TelemetryTimeData>, max_acceptable_latency_ms: Int) {
    let mut latency_issues = []
    
    for record in data {
      let latency_ms = record.ingestion_time - record.timestamp
      
      if latency_ms > max_acceptable_latency_ms {
        latency_issues = latency_issues.push({
          trace_id: record.trace_id,
          span_id: record.span_id,
          service_name: record.service_name,
          latency_ms: latency_ms,
          max_acceptable_latency_ms: max_acceptable_latency_ms,
          severity: if latency_ms > max_acceptable_latency_ms * 5 { "critical" } else { "warning" }
        })
      }
    }
    
    latency_issues
  }
  
  // 验证数据时序一致性
  let validate_temporal_consistency = fn(data: Array<TelemetryTimeData>) {
    let mut temporal_issues = []
    
    // 按trace_id分组
    let trace_groups = data.group_by_fn(d) { d.trace_id }
    
    for (trace_id, trace_records) in trace_groups {
      // 按时间戳排序
      let sorted_records = trace_records.sort_by_fn(a, b) { a.timestamp - b.timestamp }
      
      // 检查时间戳是否单调递增
      for i in 1..sorted_records.length() {
        if sorted_records[i].timestamp < sorted_records[i-1].timestamp {
          temporal_issues = temporal_issues.push({
            issue_type: "non_monotonic_timestamp",
            trace_id: trace_id,
            span_id: sorted_records[i].span_id,
            current_timestamp: sorted_records[i].timestamp,
            previous_timestamp: sorted_records[i-1].timestamp,
            description: "Timestamps are not in chronological order"
          })
        }
      }
      
      // 检查摄入时间是否单调递增
      let sorted_by_ingestion = trace_records.sort_by_fn(a, b) { a.ingestion_time - b.ingestion_time }
      
      for i in 1..sorted_by_ingestion.length() {
        if sorted_by_ingestion[i].ingestion_time < sorted_by_ingestion[i-1].ingestion_time {
          temporal_issues = temporal_issues.push({
            issue_type: "non_monotonic_ingestion",
            trace_id: trace_id,
            span_id: sorted_by_ingestion[i].span_id,
            current_ingestion_time: sorted_by_ingestion[i].ingestion_time,
            previous_ingestion_time: sorted_by_ingestion[i-1].ingestion_time,
            description: "Ingestion times are not in chronological order"
          })
        }
      }
    }
    
    temporal_issues
  }
  
  // 分析数据时效性分布
  let analyze_timeliness_distribution = fn(data: Array<TelemetryTimeData>) {
    let latencies = data.map_fn(d) { d.ingestion_time - d.timestamp }
    
    // 计算统计指标
    let min_latency = latencies.min()
    let max_latency = latencies.max()
    let avg_latency = latencies.sum() / latencies.length()
    
    // 计算百分位数
    let sorted_latencies = latencies.sort()
    let p50 = sorted_latencies[sorted_latencies.length() / 2]
    let p95 = sorted_latencies[(sorted_latencies.length() * 95 / 100).min(sorted_latencies.length() - 1)]
    let p99 = sorted_latencies[(sorted_latencies.length() * 99 / 100).min(sorted_latencies.length() - 1)]
    
    // 按延迟范围分类
    let latency_ranges = [
      { range: "< 1s", min_ms: 0, max_ms: 1000, count: 0 },
      { range: "1s - 10s", min_ms: 1000, max_ms: 10000, count: 0 },
      { range: "10s - 60s", min_ms: 10000, max_ms: 60000, count: 0 },
      { range: "> 60s", min_ms: 60000, max_ms: 999999999, count: 0 }
    ]
    
    let mut updated_ranges = []
    for range in latency_ranges {
      let count = latencies.filter_fn(l) { l >= range.min_ms and l < range.max_ms }.length()
      updated_ranges = updated_ranges.push({ range | count: count })
    }
    
    // 按服务分析延迟
    let service_groups = data.group_by_fn(d) { d.service_name }
    let mut service_latency_stats = []
    
    for (service_name, service_records) in service_groups {
      let service_latencies = service_records.map_fn(d) { d.ingestion_time - d.timestamp }
      let service_avg = service_latencies.sum() / service_latencies.length()
      
      service_latency_stats = service_latency_stats.push({
        service_name: service_name,
        record_count: service_records.length(),
        avg_latency_ms: service_avg,
        max_latency_ms: service_latencies.max()
      })
    }
    
    {
      min_latency_ms: min_latency,
      max_latency_ms: max_latency,
      avg_latency_ms: avg_latency,
      p50_latency_ms: p50,
      p95_latency_ms: p95,
      p99_latency_ms: p99,
      latency_distribution: updated_ranges,
      service_latency_stats: service_latency_stats
    }
  }
  
  // 生成时效性优化建议
  let generate_timeliness_recommendations = fn(
    latency_issues: Array<LatencyIssue>,
    temporal_issues: Array<TemporalIssue>,
    distribution: TimelinessDistribution
  ) {
    let mut recommendations = []
    
    // 基于延迟问题的建议
    if latency_issues.length() > 0 {
      let critical_issues = latency_issues.filter_fn(issue) { issue.severity == "critical" }
      let warning_issues = latency_issues.filter_fn(issue) { issue.severity == "warning" }
      
      if critical_issues.length() > 0 {
        recommendations = recommendations.push({
          category: "critical_latency",
          priority: "high",
          recommendation: "Investigate and fix critical data pipeline bottlenecks causing excessive delays",
          affected_services: critical_issues.map_fn(issue) { issue.service_name }.unique()
        })
      }
      
      if warning_issues.length() > 0 {
        recommendations = recommendations.push({
          category: "moderate_latency",
          priority: "medium",
          recommendation: "Optimize data collection and transmission processes to reduce moderate delays",
          affected_services: warning_issues.map_fn(issue) { issue.service_name }.unique()
        })
      }
    }
    
    // 基于时序问题的建议
    if temporal_issues.length() > 0 {
      let non_monotonic_timestamps = temporal_issues.filter_fn(issue) { issue.issue_type == "non_monotonic_timestamp" }
      let non_monotonic_ingestion = temporal_issues.filter_fn(issue) { issue.issue_type == "non_monotonic_ingestion" }
      
      if non_monotonic_timestamps.length() > 0 {
        recommendations = recommendations.push({
          category: "timestamp_consistency",
          priority: "high",
          recommendation: "Fix clock synchronization issues across services to ensure consistent timestamps",
          affected_traces: non_monotonic_timestamps.map_fn(issue) { issue.trace_id }.unique()
        })
      }
      
      if non_monotonic_ingestion.length() > 0 {
        recommendations = recommendations.push({
          category: "ingestion_order",
          priority: "medium",
          recommendation: "Review data ingestion pipeline to ensure proper ordering of records",
          affected_traces: non_monotonic_ingestion.map_fn(issue) { issue.trace_id }.unique()
        })
      }
    }
    
    // 基于分布统计的建议
    if distribution.p95_latency_ms > 30000 {  // P95超过30秒
      recommendations = recommendations.push({
        category: "overall_performance",
        priority: "high",
        recommendation: "Overall data timeliness is poor, consider major pipeline improvements",
        current_p95_ms: distribution.p95_latency_ms
      })
    }
    
    // 基于服务特定延迟的建议
    let high_latency_services = distribution.service_latency_stats.filter_fn(stat) { 
      stat.avg_latency_ms > 10000  // 平均延迟超过10秒
    }
    
    if high_latency_services.length() > 0 {
      recommendations = recommendations.push({
        category: "service_specific",
        priority: "medium",
        recommendation: "Focus optimization efforts on services with highest average latency",
        high_latency_services: high_latency_services.map_fn(stat) { 
          { service: stat.service_name, avg_latency: stat.avg_latency_ms }
        }
      })
    }
    
    recommendations
  }
  
  // 执行时效性验证
  let latency_issues = validate_data_latency(telemetry_time_data, 5000)  // 5秒最大可接受延迟
  let temporal_issues = validate_temporal_consistency(telemetry_time_data)
  let distribution = analyze_timeliness_distribution(telemetry_time_data)
  let recommendations = generate_timeliness_recommendations(latency_issues, temporal_issues, distribution)
  
  // 验证数据延迟问题
  assert_eq(latency_issues.length(), 3)
  
  // 验证延迟问题的严重程度
  let critical_issues = latency_issues.filter_fn(issue) { issue.severity == "critical" }
  let warning_issues = latency_issues.filter_fn(issue) { issue.severity == "warning" }
  assert_eq(critical_issues.length(), 1)  // trace-003 (10分钟延迟)
  assert_eq(warning_issues.length(), 2)   // trace-002 (10秒延迟) 和 trace-005 (100秒延迟)
  
  // 验证时序一致性
  assert_eq(temporal_issues.length(), 0)  // 在这个测试数据中没有时序一致性问题
  
  // 验证时效性分布
  assert_eq(distribution.min_latency_ms, 1000)
  assert_eq(distribution.max_latency_ms, 600000)
  assert_eq(distribution.avg_latency_ms, 142400)  // (1000+10000+600000+1000+100000)/5 = 142400
  
  // 验证延迟分布
  let under_1s = distribution.latency_distribution.find_fn(r) { r.range == "< 1s" }
  assert_eq(under_1s.unwrap().count, 2)
  
  let one_to_10s = distribution.latency_distribution.find_fn(r) { r.range == "1s - 10s" }
  assert_eq(one_to_10s.unwrap().count, 1)
  
  let ten_to_60s = distribution.latency_distribution.find_fn(r) { r.range == "10s - 60s" }
  assert_eq(ten_to_60s.unwrap().count, 1)
  
  let over_60s = distribution.latency_distribution.find_fn(r) { r.range == "> 60s" }
  assert_eq(over_60s.unwrap().count, 1)
  
  // 验证服务延迟统计
  assert_eq(distribution.service_latency_stats.length(), 5)
  
  let payment_service_stats = distribution.service_latency_stats.find_fn(stat) { 
    stat.service_name == "payment-service" 
  }
  assert_true(payment_service_stats.is_some())
  assert_eq(payment_service_stats.unwrap().avg_latency_ms, 600000)
  
  // 验证时效性优化建议
  assert_eq(recommendations.length(), 3)
  
  // 验证关键延迟建议
  let critical_latency_rec = recommendations.find_fn(rec) { rec.category == "critical_latency" }
  assert_true(critical_latency_rec.is_some())
  assert_eq(critical_latency_rec.unwrap().priority, "high")
  
  // 验证整体性能建议
  let overall_performance_rec = recommendations.find_fn(rec) { rec.category == "overall_performance" }
  assert_true(overall_performance_rec.is_some())
  assert_eq(overall_performance_rec.unwrap().priority, "high")
}