// 遥测跨服务传播综合测试用例

test "telemetry_trace_context_propagation" {
  // 测试遥测链路追踪上下文传播
  
  // 链路追踪上下文
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: String,
    trace_flags: Int,
    trace_state: Map[String, String],
    baggage: Map[String, String]
  }
  
  // 服务节点
  type ServiceNode = {
    service_name: String,
    service_version: String,
    node_id: String,
    inbound_context: TraceContext,
    outbound_context: TraceContext,
    operation_name: String,
    start_time: Int,
    end_time: Int
  }
  
  // 传播结果
  type PropagationResult = {
    source_service: String,
    target_service: String,
    context_preserved: Bool,
    trace_id_consistent: Bool,
    parent_span_correct: Bool,
    baggage_propagated: Bool,
    trace_state_preserved: Bool,
    propagation_time_ms: Int
  }
  
  // 创建初始链路上下文
  let initial_trace_context = TraceContext {
    trace_id: "trace_1234567890abcdef",
    span_id: "span_1111111111111111",
    parent_span_id: "",
    trace_flags: 1,  // 采样标志
    trace_state: {
      "rojo": "00f067aa0ba902b7",
      "congo": "t61rcWkgMzE"
    },
    baggage: {
      "user.id": "user_12345",
      "request.id": "req_67890",
      "session.id": "sess_abcde"
    }
  }
  
  // 验证初始上下文
  assert_eq(initial_trace_context.trace_id, "trace_1234567890abcdef")
  assert_eq(initial_trace_context.span_id, "span_1111111111111111")
  assert_eq(initial_trace_context.parent_span_id, "")
  assert_eq(initial_trace_context.trace_flags, 1)
  assert_eq(initial_trace_context.trace_state.size(), 2)
  assert_eq(initial_trace_context.baggage.size(), 3)
  
  // 定义服务调用链
  let service_call_chain = [
    { service_name: "api-gateway", operation: "http_request" },
    { service_name: "auth-service", operation: "authenticate" },
    { service_name: "user-service", operation: "get_user" },
    { service_name: "order-service", operation: "create_order" },
    { service_name: "notification-service", operation: "send_notification" }
  ]
  
  // 验证服务调用链
  assert_eq(service_call_chain.length(), 5)
  assert_eq(service_call_chain[0].service_name, "api-gateway")
  assert_eq(service_call_chain[4].service_name, "notification-service")
  
  // 上下文传播函数
  let propagate_context = fn(source_context: TraceContext, service_name: String, operation: String) -> ServiceNode {
    let current_time = 1640995200
    
    // 生成新的span ID
    let new_span_id = "span_" + (current_time + service_name.length()).to_string()
    
    // 创建新的上下文（保持trace_id，更新span_id，设置parent_span_id）
    let outbound_context = TraceContext {
      trace_id: source_context.trace_id,
      span_id: new_span_id,
      parent_span_id: source_context.span_id,
      trace_flags: source_context.trace_flags,
      trace_state: source_context.trace_state,
      baggage: source_context.baggage
    }
    
    ServiceNode {
      service_name: service_name,
      service_version: "1.0.0",
      node_id: service_name + "_node_001",
      inbound_context: source_context,
      outbound_context: outbound_context,
      operation_name: operation,
      start_time: current_time,
      end_time: current_time + 100
    }
  }
  
  // 验证传播结果函数
  let verify_propagation = fn(source: ServiceNode, target: ServiceNode) -> PropagationResult {
    let context_preserved = target.inbound_context.trace_id == source.outbound_context.trace_id
    let trace_id_consistent = source.outbound_context.trace_id == target.inbound_context.trace_id
    let parent_span_correct = target.inbound_context.parent_span_id == source.outbound_context.span_id
    let baggage_propagated = target.inbound_context.baggage.size() == source.outbound_context.baggage.size()
    let trace_state_preserved = target.inbound_context.trace_state.size() == source.outbound_context.trace_state.size()
    
    PropagationResult {
      source_service: source.service_name,
      target_service: target.service_name,
      context_preserved: context_preserved,
      trace_id_consistent: trace_id_consistent,
      parent_span_correct: parent_span_correct,
      baggage_propagated: baggage_propagated,
      trace_state_preserved: trace_state_preserved,
      propagation_time_ms: 5  // 模拟传播时间
    }
  }
  
  // 执行服务调用链和上下文传播
  let mut service_nodes = []
  let mut propagation_results = []
  
  // 第一个服务（API网关）
  let first_service = propagate_context(initial_trace_context, service_call_chain[0].service_name, service_call_chain[0].operation)
  service_nodes.push(first_service)
  
  // 后续服务和传播验证
  let mut i = 1
  while i < service_call_chain.length() {
    let current_service = propagate_context(service_nodes[i-1].outbound_context, service_call_chain[i].service_name, service_call_chain[i].operation)
    service_nodes.push(current_service)
    
    // 验证传播
    let propagation_result = verify_propagation(service_nodes[i-1], current_service)
    propagation_results.push(propagation_result)
    
    i = i + 1
  }
  
  // 验证服务节点
  assert_eq(service_nodes.length(), 5)
  assert_eq(propagation_results.length(), 4)  // 4次传播
  
  // 验证传播结果
  i = 0
  while i < propagation_results.length() {
    let result = propagation_results[i]
    
    assert_eq(result.context_preserved, true)
    assert_eq(result.trace_id_consistent, true)
    assert_eq(result.parent_span_correct, true)
    assert_eq(result.baggage_propagated, true)
    assert_eq(result.trace_state_preserved, true)
    assert_eq(result.propagation_time_ms > 0, true)
    
    i = i + 1
  }
  
  // 验证链路一致性
  i = 0
  while i < service_nodes.length() {
    let node = service_nodes[i]
    
    // 所有节点应该有相同的trace_id
    assert_eq(node.outbound_context.trace_id, initial_trace_context.trace_id)
    
    // 第一个节点的parent_span_id应该为空
    if i == 0 {
      assert_eq(node.outbound_context.parent_span_id, "")
    } else {
      // 其他节点的parent_span_id应该等于前一个节点的span_id
      assert_eq(node.outbound_context.parent_span_id, service_nodes[i-1].outbound_context.span_id)
    }
    
    i = i + 1
  }
}

test "telemetry_baggage_propagation_limits" {
  // 测试遥测行李(baggage)传播限制
  
  // 行李传播配置
  type BaggageConfig = {
    max_baggage_items: Int,
    max_key_length: Int,
    max_value_length: Int,
    max_total_size_bytes: Int,
    allowed_keys: Array[String],
    blocked_keys: Array[String]
  }
  
  // 行李项
  type BaggageItem = {
    key: String,
    value: String,
    metadata: Map[String, String]
  }
  
  // 行李传播结果
  type BaggagePropagationResult = {
    original_items: Int,
    propagated_items: Int,
    dropped_items: Int,
    truncated_items: Int,
    total_size_bytes: Int,
    propagation_successful: Bool,
    warnings: Array[String]
  }
  
  // 行李传播配置
  let baggage_config = BaggageConfig {
    max_baggage_items: 10,
    max_key_length: 50,
    max_value_length: 100,
    max_total_size_bytes: 1000,
    allowed_keys: ["user.id", "session.id", "request.id", "tenant.id", "correlation.id"],
    blocked_keys: ["password", "secret", "token", "key"]
  }
  
  // 验证配置
  assert_eq(baggage_config.max_baggage_items, 10)
  assert_eq(baggage_config.max_key_length, 50)
  assert_eq(baggage_config.allowed_keys.length(), 5)
  assert_eq(baggage_config.blocked_keys.length(), 4)
  
  // 创建测试行李项
  let test_baggage_items = [
    BaggageItem { key: "user.id", value: "user_12345", metadata: {} },
    BaggageItem { key: "session.id", value: "sess_abcdef123456", metadata: {} },
    BaggageItem { key: "request.id", value: "req_789012345678", metadata: {} },
    BaggageItem { key: "tenant.id", value: "tenant_001", metadata: {} },
    BaggageItem { key: "correlation.id", value: "corr_1234567890", metadata: {} },
    BaggageItem { key: "custom.data", value: "some_custom_value", metadata: {} },  // 不在允许列表中
    BaggageItem { key: "password", value: "secret_password", metadata: {} },  // 在阻止列表中
    BaggageItem { key: "very.long.key.name.that.exceeds.the.maximum.allowed.length.for.testing.purposes", value: "value", metadata: {} },  // 超长键名
    BaggageItem { key: "short.key", value: "very.long.value.that.exceeds.the.maximum.allowed.length.for.testing.purposes.and.should.be.truncated.according.to.the.configuration.rules", metadata: {} },  // 超长值
    BaggageItem { key: "extra.item.1", value: "extra_1", metadata: {} },
    BaggageItem { key: "extra.item.2", value: "extra_2", metadata: {} },
    BaggageItem { key: "extra.item.3", value: "extra_3", metadata: {} }  // 超过最大项目数
  ]
  
  // 验证测试行李项
  assert_eq(test_baggage_items.length(), 12)
  assert_eq(test_baggage_items[0].key, "user.id")
  assert_eq(test_baggage_items[6].key, "password")
  
  // 行李过滤和传播函数
  let propagate_baggage = fn(items: Array[BaggageItem], config: BaggageConfig) -> BaggagePropagationResult {
    let mut propagated_items = []
    let mut warnings = []
    let mut total_size = 0
    let mut dropped_count = 0
    let mut truncated_count = 0
    
    let mut i = 0
    while i < items.length() and propagated_items.length() < config.max_baggage_items {
      let item = items[i]
      let mut should_propagate = true
      let mut modified_item = item
      
      // 检查阻止的键
      let mut j = 0
      while j < config.blocked_keys.length() {
        if item.key.contains(config.blocked_keys[j]) {
          should_propagate = false
          warnings.push("阻止的行李键: " + item.key)
          dropped_count = dropped_count + 1
          break
        }
        j = j + 1
      }
      
      // 检查允许的键（如果有允许列表）
      if should_propagate and config.allowed_keys.length() > 0 {
        let mut key_allowed = false
        j = 0
        while j < config.allowed_keys.length() {
          if item.key == config.allowed_keys[j] {
            key_allowed = true
            break
          }
          j = j + 1
        }
        
        if not key_allowed {
          should_propagate = false
          warnings.push("不在允许列表中的行李键: " + item.key)
          dropped_count = dropped_count + 1
        }
      }
      
      // 检查键长度
      if should_propagate and item.key.length() > config.max_key_length {
        modified_item.key = item.key.substring(0, config.max_key_length)
        warnings.push("键名被截断: " + item.key + " -> " + modified_item.key)
        truncated_count = truncated_count + 1
      }
      
      // 检查值长度
      if should_propagate and item.value.length() > config.max_value_length {
        modified_item.value = item.value.substring(0, config.max_value_length)
        warnings.push("值被截断: " + item.key)
        truncated_count = truncated_count + 1
      }
      
      // 计算大小
      let item_size = modified_item.key.length() + modified_item.value.length()
      
      // 检查总大小限制
      if should_propagate and total_size + item_size > config.max_total_size_bytes {
        should_propagate = false
        warnings.push("超出总大小限制: " + item.key)
        dropped_count = dropped_count + 1
      }
      
      if should_propagate {
        propagated_items.push(modified_item)
        total_size = total_size + item_size
      }
      
      i = i + 1
    }
    
    // 如果有更多项目但因为数量限制被丢弃
    while i < items.length() {
      dropped_count = dropped_count + 1
      warnings.push("超出最大项目数限制: " + items[i].key)
      i = i + 1
    }
    
    let propagation_successful = propagated_items.length() > 0 and warnings.length() < items.length() / 2
    
    BaggagePropagationResult {
      original_items: items.length(),
      propagated_items: propagated_items.length(),
      dropped_items: dropped_count,
      truncated_items: truncated_count,
      total_size_bytes: total_size,
      propagation_successful: propagation_successful,
      warnings: warnings
    }
  }
  
  // 执行行李传播测试
  let result = propagate_baggage(test_baggage_items, baggage_config)
  
  // 验证传播结果
  assert_eq(result.original_items, 12)
  assert_eq(result.propagated_items <= baggage_config.max_baggage_items, true)
  assert_eq(result.dropped_items > 0, true)  // 应该有被丢弃的项目
  assert_eq(result.truncated_items > 0, true)  // 应该有被截断的项目
  assert_eq(result.total_size_bytes <= baggage_config.max_total_size_bytes, true)
  assert_eq(result.propagation_successful, true)
  assert_eq(result.warnings.length() > 0, true)
  
  // 验证具体的项目
  assert_eq(result.propagated_items >= 5, true)  // 至少允许列表中的项目应该被传播
  assert_eq(result.dropped_items >= 1, true)  // 至少password应该被丢弃
}

test "telemetry_cross_protocol_propagation" {
  // 测试遥测跨协议传播
  
  // 协议类型
  type Protocol = {
    protocol_name: String,
    version: String,
    header_format: String,
    metadata_encoding: String,
    compression_supported: Bool
  }
  
  // 协议传播适配器
  type ProtocolAdapter = {
    source_protocol: String,
    target_protocol: String,
    conversion_successful: Bool,
    data_loss: Bool,
    conversion_overhead_ms: Int,
    supported_fields: Array[String],
    lost_fields: Array[String]
  }
  
  // 跨协议传播结果
  type CrossProtocolResult = {
    source_protocol: String,
    target_protocol: String,
    trace_context_preserved: Bool,
    baggage_preserved: Bool,
    metadata_preserved: Bool,
    total_conversion_time_ms: Int,
    data_integrity_score: Double
  }
  
  // 支持的协议
  let protocols = [
    Protocol {
      protocol_name: "http",
      version: "1.1",
      header_format: "text",
      metadata_encoding: "url-encoded",
      compression_supported: false
    },
    Protocol {
      protocol_name: "http",
      version: "2.0",
      header_format: "binary",
      metadata_encoding: "hpack",
      compression_supported: true
    },
    Protocol {
      protocol_name: "grpc",
      version: "1.0",
      header_format: "binary",
      metadata_encoding: "protobuf",
      compression_supported: true
    },
    Protocol {
      protocol_name: "kafka",
      version: "2.8",
      header_format: "binary",
      metadata_encoding: "avro",
      compression_supported: true
    }
  ]
  
  // 验证协议定义
  assert_eq(protocols.length(), 4)
  assert_eq(protocols[0].protocol_name, "http")
  assert_eq(protocols[1].version, "2.0")
  assert_eq(protocols[2].metadata_encoding, "protobuf")
  
  // 协议适配器配置
  let protocol_adapters = [
    ProtocolAdapter {
      source_protocol: "http/1.1",
      target_protocol: "http/2.0",
      conversion_successful: true,
      data_loss: false,
      conversion_overhead_ms: 2,
      supported_fields: ["traceparent", "tracestate", "baggage"],
      lost_fields: []
    },
    ProtocolAdapter {
      source_protocol: "http/1.1",
      target_protocol: "grpc/1.0",
      conversion_successful: true,
      data_loss: false,
      conversion_overhead_ms: 5,
      supported_fields: ["trace-id", "span-id", "baggage"],
      lost_fields: []
    },
    ProtocolAdapter {
      source_protocol: "grpc/1.0",
      target_protocol: "kafka/2.8",
      conversion_successful: true,
      data_loss: true,
      conversion_overhead_ms: 8,
      supported_fields: ["trace-id", "span-id"],
      lost_fields: ["tracestate", "baggage"]
    },
    ProtocolAdapter {
      source_protocol: "kafka/2.8",
      target_protocol: "http/1.1",
      conversion_successful: true,
      data_loss: true,
      conversion_overhead_ms: 10,
      supported_fields: ["trace-id", "span-id"],
      lost_fields: ["tracestate"]
    }
  ]
  
  // 验证协议适配器
  assert_eq(protocol_adapters.length(), 4)
  assert_eq(protocol_adapters[0].source_protocol, "http/1.1")
  assert_eq(protocol_adapters[2].data_loss, true)
  
  // 跨协议传播测试数据
  let cross_protocol_data = {
    "traceparent": "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
    "tracestate": "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE",
    "baggage": "user.id=user123,session.id=sess456",
    "custom.metadata": "custom_value_123",
    "x-request-id": "req_789012"
  }
  
  // 验证测试数据
  assert_eq(cross_protocol_data.size(), 5)
  assert_eq(cross_protocol_data.contains_key("traceparent"), true)
  
  // 跨协议转换函数
  let convert_protocol = fn(data: Map[String, String], source: String, target: String) -> CrossProtocolResult {
    let mut conversion_time = 0
    let mut preserved_fields = []
    let mut lost_fields = []
    let mut data_integrity_score = 1.0
    
    // 查找适配器
    let mut adapter_found = false
    let mut i = 0
    
    while i < protocol_adapters.length() {
      let adapter = protocol_adapters[i]
      
      if adapter.source_protocol == source and adapter.target_protocol == target {
        adapter_found = true
        conversion_time = adapter.conversion_overhead_ms
        
        // 检查保留的字段
        let mut j = 0
        while j < adapter.supported_fields.length() {
          let field = adapter.supported_fields[j]
          
          // 映射字段名（不同协议可能有不同的字段名）
          let data_key = if field == "traceparent" { "traceparent" }
                        else if field == "trace-id" { "traceparent" }
                        else if field == "span-id" { "traceparent" }
                        else if field == "tracestate" { "tracestate" }
                        else if field == "baggage" { "baggage" }
                        else { field }
          
          if data.contains_key(data_key) {
            preserved_fields.push(data_key)
          }
          
          j = j + 1
        }
        
        // 检查丢失的字段
        j = 0
        while j < adapter.lost_fields.length() {
          let field = adapter.lost_fields[j]
          
          if data.contains_key(field) {
            lost_fields.push(field)
            data_integrity_score = data_integrity_score - 0.2
          }
          
          j = j + 1
        }
        
        break
      }
      
      i = i + 1
    }
    
    if not adapter_found {
      // 没有找到适配器，假设直接转换
      conversion_time = 15
      data_integrity_score = 0.5
      lost_fields = ["tracestate", "baggage", "custom.metadata"]
    }
    
    // 计算保留情况
    let trace_context_preserved = data.contains_key("traceparent") and preserved_fields.contains("traceparent")
    let baggage_preserved = data.contains_key("baggage") and preserved_fields.contains("baggage")
    let metadata_preserved = data.contains_key("custom.metadata") and not lost_fields.contains("custom.metadata")
    
    CrossProtocolResult {
      source_protocol: source,
      target_protocol: target,
      trace_context_preserved: trace_context_preserved,
      baggage_preserved: baggage_preserved,
      metadata_preserved: metadata_preserved,
      total_conversion_time_ms: conversion_time,
      data_integrity_score: data_integrity_score
    }
  }
  
  // 执行跨协议传播测试
  let test_conversions = [
    { from: "http/1.1", to: "http/2.0" },
    { from: "http/1.1", to: "grpc/1.0" },
    { from: "grpc/1.0", to: "kafka/2.8" },
    { from: "kafka/2.8", to: "http/1.1" },
    { from: "http/2.0", to: "kafka/2.8" }  // 没有直接适配器
  ]
  
  let mut conversion_results = []
  let mut i = 0
  
  while i < test_conversions.length() {
    let conversion = test_conversions[i]
    let result = convert_protocol(cross_protocol_data, conversion.from, conversion.to)
    conversion_results.push(result)
    i = i + 1
  }
  
  // 验证转换结果
  assert_eq(conversion_results.length(), 5)
  
  i = 0
  while i < conversion_results.length() {
    let result = conversion_results[i]
    let expected = test_conversions[i]
    
    assert_eq(result.source_protocol, expected.from)
    assert_eq(result.target_protocol, expected.to)
    assert_eq(result.total_conversion_time_ms > 0, true)
    assert_eq(result.data_integrity_score >= 0.0 and result.data_integrity_score <= 1.0, true)
    
    // 验证链路上下文应该总是被保留
    assert_eq(result.trace_context_preserved, true)
    
    // 验证特定转换的预期结果
    if expected.from == "http/1.1" and expected.to == "http/2.0" {
      assert_eq(result.baggage_preserved, true)
      assert_eq(result.data_integrity_score, 1.0)
    } else if expected.from == "grpc/1.0" and expected.to == "kafka/2.8" {
      assert_eq(result.baggage_preserved, false)  // 根据适配器配置
      assert_eq(result.data_integrity_score < 1.0, true)
    } else if expected.from == "http/2.0" and expected.to == "kafka/2.8" {
      assert_eq(result.data_integrity_score < 1.0, true)  // 没有直接适配器
    }
    
    i = i + 1
  }
}

test "telemetry_propagation_security_isolation" {
  // 测试遥测传播安全隔离
  
  // 安全级别
  type SecurityLevel = {
    level_name: String,
    trust_required: Bool,
    encryption_required: Bool,
    authentication_required: Bool,
    allowed_fields: Array[String],
    restricted_fields: Array[String]
  }
  
  // 隔离策略
  type IsolationPolicy = {
    source_level: String,
    target_level: String,
    propagation_allowed: Bool,
    field_filtering: Bool,
    data_sanitization: Bool,
    audit_required: Bool
  }
  
  // 安全传播结果
  type SecurityPropagationResult = {
    source_level: String,
    target_level: String,
    propagation_successful: Bool,
    sanitized_fields: Array[String],
    blocked_fields: Array[String],
    audit_log_entries: Int,
    security_violations: Int
  }
  
  // 定义安全级别
  let security_levels = {
    "public": SecurityLevel {
      level_name: "public",
      trust_required: false,
      encryption_required: false,
      authentication_required: false,
      allowed_fields: ["trace.id", "span.id", "service.name"],
      restricted_fields: ["user.id", "session.id", "auth.token"]
    },
    "internal": SecurityLevel {
      level_name: "internal",
      trust_required: true,
      encryption_required: false,
      authentication_required: true,
      allowed_fields: ["trace.id", "span.id", "service.name", "user.id", "session.id"],
      restricted_fields: ["auth.token", "api.key", "secret"]
    },
    "confidential": SecurityLevel {
      level_name: "confidential",
      trust_required: true,
      encryption_required: true,
      authentication_required: true,
      allowed_fields: ["trace.id", "span.id", "service.name", "user.id", "session.id", "auth.token"],
      restricted_fields: ["credit.card", "ssn", "password"]
    },
    "top-secret": SecurityLevel {
      level_name: "top-secret",
      trust_required: true,
      encryption_required: true,
      authentication_required: true,
      allowed_fields: ["trace.id", "span.id"],  // 最小化
      restricted_fields: ["user.id", "session.id", "auth.token", "all.pii"]
    }
  }
  
  // 验证安全级别
  assert_eq(security_levels.size(), 4)
  assert_eq(security_levels["public"].trust_required, false)
  assert_eq(security_levels["confidential"].encryption_required, true)
  assert_eq(security_levels["top-secret"].allowed_fields.length(), 2)
  
  // 隔离策略
  let isolation_policies = [
    IsolationPolicy {
      source_level: "public",
      target_level: "public",
      propagation_allowed: true,
      field_filtering: false,
      data_sanitization: false,
      audit_required: false
    },
    IsolationPolicy {
      source_level: "public",
      target_level: "internal",
      propagation_allowed: true,
      field_filtering: true,
      data_sanitization: false,
      audit_required: true
    },
    IsolationPolicy {
      source_level: "internal",
      target_level: "public",
      propagation_allowed: true,
      field_filtering: true,
      data_sanitization: true,
      audit_required: true
    },
    IsolationPolicy {
      source_level: "confidential",
      target_level: "public",
      propagation_allowed: false,
      field_filtering: true,
      data_sanitization: true,
      audit_required: true
    },
    IsolationPolicy {
      source_level: "top-secret",
      target_level: "internal",
      propagation_allowed: false,
      field_filtering: true,
      data_sanitization: true,
      audit_required: true
    }
  ]
  
  // 验证隔离策略
  assert_eq(isolation_policies.length(), 5)
  assert_eq(isolation_policies[0].propagation_allowed, true)
  assert_eq(isolation_policies[3].propagation_allowed, false)
  
  // 敏感测试数据
  let sensitive_telemetry_data = {
    "trace.id": "trace_123456789",
    "span.id": "span_987654321",
    "service.name": "secure-service",
    "user.id": "user_sensitive_123",
    "session.id": "session_confidential_456",
    "auth.token": "bearer_token_secret_789",
    "api.key": "api_key_secret_012",
    "credit.card": "4111-1111-1111-1111",
    "ssn": "123-45-6789",
    "password": "secret_password_345"
  }
  
  // 验证敏感数据
  assert_eq(sensitive_telemetry_data.size(), 10)
  assert_eq(sensitive_telemetry_data.contains_key("credit.card"), true)
  assert_eq(sensitive_telemetry_data.contains_key("auth.token"), true)
  
  // 安全传播函数
  let secure_propagate = fn(data: Map[String, String], source_level: String, target_level: String) -> SecurityPropagationResult {
    let mut propagated_data = {}
    let mut sanitized_fields = []
    let mut blocked_fields = []
    let mut audit_entries = 0
    let mut violations = 0
    
    // 查找隔离策略
    let mut policy = IsolationPolicy {
      source_level: source_level,
      target_level: target_level,
      propagation_allowed: false,
      field_filtering: true,
      data_sanitization: true,
      audit_required: true
    }
    
    let mut i = 0
    while i < isolation_policies.length() {
      if isolation_policies[i].source_level == source_level and 
         isolation_policies[i].target_level == target_level {
        policy = isolation_policies[i]
        break
      }
      i = i + 1
    }
    
    // 如果不允许传播
    if not policy.propagation_allowed {
      violations = violations + 1
      audit_entries = audit_entries + 1
      
      return SecurityPropagationResult {
        source_level: source_level,
        target_level: target_level,
        propagation_successful: false,
        sanitized_fields: [],
        blocked_fields: [],
        audit_log_entries: audit_entries,
        security_violations: violations
      }
    }
    
    // 获取目标安全级别配置
    let target_security = security_levels[target_level]
    
    // 字段过滤和清理
    let mut field_keys = ["trace.id", "span.id", "service.name", "user.id", "session.id", "auth.token", "api.key", "credit.card", "ssn", "password"]
    i = 0
    while i < field_keys.length() {
      let field = field_keys[i]
      
      if data.contains_key(field) {
        let mut field_allowed = false
        
        // 检查是否在允许列表中
        let mut j = 0
        while j < target_security.allowed_fields.length() {
          if field == target_security.allowed_fields[j] {
            field_allowed = true
            break
          }
          j = j + 1
        }
        
        // 检查是否在限制列表中
        let mut field_restricted = false
        j = 0
        while j < target_security.restricted_fields.length() {
          if field == target_security.restricted_fields[j] {
            field_restricted = true
            break
          }
          j = j + 1
        }
        
        if field_allowed and not field_restricted {
          // 字段允许传播
          let mut value = data[field]
          
          // 数据清理
          if policy.data_sanitization and (field.contains("token") or field.contains("key") or field.contains("password")) {
            value = "SANITIZED_" + field
            sanitized_fields.push(field)
            audit_entries = audit_entries + 1
          }
          
          propagated_data[field] = value
        } else if field_restricted {
          // 字段被阻止
          blocked_fields.push(field)
          audit_entries = audit_entries + 1
          
          if source_level == "confidential" or source_level == "top-secret" {
            violations = violations + 1
          }
        }
      }
      
      i = i + 1
    }
    
    // 审计日志
    if policy.audit_required {
      audit_entries = audit_entries + 1
    }
    
    let propagation_successful = propagated_data.size() > 0 and violations == 0
    
    SecurityPropagationResult {
      source_level: source_level,
      target_level: target_level,
      propagation_successful: propagation_successful,
      sanitized_fields: sanitized_fields,
      blocked_fields: blocked_fields,
      audit_log_entries: audit_entries,
      security_violations: violations
    }
  }
  
  // 执行安全传播测试
  let security_tests = [
    { from: "public", to: "public", expected_success: true },
    { from: "public", to: "internal", expected_success: true },
    { from: "internal", to: "public", expected_success: true },
    { from: "confidential", to: "public", expected_success: false },
    { from: "top-secret", to: "internal", expected_success: false }
  ]
  
  let mut security_results = []
  let mut i = 0
  
  while i < security_tests.length() {
    let test_case = security_tests[i]
    let result = secure_propagate(sensitive_telemetry_data, test_case.from, test_case.to)
    security_results.push(result)
    i = i + 1
  }
  
  // 验证安全传播结果
  assert_eq(security_results.length(), 5)
  
  i = 0
  while i < security_results.length() {
    let result = security_results[i]
    let expected = security_tests[i]
    
    assert_eq(result.source_level, expected.from)
    assert_eq(result.target_level, expected.to)
    assert_eq(result.propagation_successful, expected.expected_success)
    assert_eq(result.audit_log_entries >= 0, true)
    assert_eq(result.security_violations >= 0, true)
    
    if expected.expected_success {
      assert_eq(result.blocked_fields.length() >= 0, true)
    } else {
      assert_eq(result.security_violations > 0, true)
    }
    
    i = i + 1
  }
}