// Azimuth 遥测数据跨服务一致性测试用例
// 专注于遥测系统中的跨服务数据一致性和协调机制

// 测试1: 分布式追踪上下文传播一致性
test "分布式追踪上下文传播一致性" {
  // 模拟分布式追踪链路
  let trace_chain = [
    { 
      service: "api-gateway", 
      trace_id: "trace-123", 
      span_id: "span-001", 
      parent_span: "", 
      timestamp: 1640995200,
      baggage: { "user_id": "user-456", "request_id": "req-789" }
    },
    { 
      service: "auth-service", 
      trace_id: "trace-123", 
      span_id: "span-002", 
      parent_span: "span-001", 
      timestamp: 1640995260,
      baggage: { "user_id": "user-456", "request_id": "req-789", "auth_level": "high" }
    },
    { 
      service: "user-service", 
      trace_id: "trace-123", 
      span_id: "span-003", 
      parent_span: "span-002", 
      timestamp: 1640995320,
      baggage: { "user_id": "user-456", "request_id": "req-789", "auth_level": "high" }
    },
    { 
      service: "order-service", 
      trace_id: "trace-123", 
      span_id: "span-004", 
      parent_span: "span-003", 
      timestamp: 1640995380,
      baggage: { "user_id": "user-456", "request_id": "req-789", "auth_level": "high", "order_type": "premium" }
    },
    { 
      service: "payment-service", 
      trace_id: "trace-123", 
      span_id: "span-005", 
      parent_span: "span-004", 
      timestamp: 1640995440,
      baggage: { "user_id": "user-456", "request_id": "req-789", "auth_level": "high", "order_type": "premium" }
    }
  ]
  
  // 追踪一致性验证配置
  let trace_consistency_config = {
    require_trace_id_continuity: true,    // 要求trace_id连续
    require_parent_span_validity: true,   // 要求父span有效
    require_baggage_propagation: true,    // 要求baggage传播
    require_timestamp_ordering: true      // 要求时间戳顺序
  }
  
  // 追踪一致性验证算法
  let mut consistency_violations = []
  let mut propagated_baggage = {}
  
  // 按时间戳排序
  let sorted_chain = trace_chain.sort_by(fn(a, b) {
    if a.timestamp < b.timestamp { -1 }
    else if a.timestamp > b.timestamp { 1 }
    else { 0 }
  })
  
  // 验证trace_id连续性
  let trace_ids = sorted_chain.map(fn(s) { s.trace_id })
  let unique_trace_ids = trace_ids.to_set()
  
  if trace_consistency_config.require_trace_id_continuity && unique_trace_ids.size() > 1 {
    consistency_violations = consistency_violations.push({
      violation_type: "trace_id_inconsistency",
      description: "多个trace_id在同一条追踪链中",
      affected_spans: sorted_chain.map(fn(s) { s.span_id })
    })
  }
  
  // 验证父子关系
  let span_relations = {}
  for span in sorted_chain {
    span_relations = span_relations.set(span.span_id, span.parent_span)
  }
  
  for span in sorted_chain {
    if span.parent_span != "" && not span_relations.contains(span.parent_span) {
      consistency_violations = consistency_violations.push({
        violation_type: "invalid_parent_span",
        description: "span引用了不存在的父span",
        affected_spans: [span.span_id],
        parent_span: span.parent_span
      })
    }
  }
  
  // 验证时间戳顺序
  if trace_consistency_config.require_timestamp_ordering {
    for i in 1..sorted_chain.length() {
      let current = sorted_chain[i]
      let previous = sorted_chain[i-1]
      
      if current.parent_span != "" {
        let parent_span = sorted_chain.filter(fn(s) { s.span_id == current.parent_span })[0]
        if current.timestamp < parent_span.timestamp {
          consistency_violations = consistency_violations.push({
            violation_type: "timestamp_order_violation",
            description: "子span时间戳早于父span",
            affected_spans: [current.span_id],
            parent_span: current.parent_span
          })
        }
      }
    }
  }
  
  // 验证baggage传播
  if trace_consistency_config.require_baggage_propagation {
    // 收集所有baggage键
    let mut all_baggage_keys = {}
    for span in sorted_chain {
      for key in span.baggage.keys() {
        all_baggage_keys = all_baggage_keys.set(key, true)
      }
    }
    
    // 检查每个baggage键的传播一致性
    for key in all_baggage_keys.keys() {
      let mut values = {}
      let mut propagation_chain = []
      
      for span in sorted_chain {
        match span.baggage.get(key) {
          Some(value) => {
            values = values.set(value, true)
            propagation_chain = propagation_chain.push({
              span_id: span.span_id,
              service: span.service,
              value: value
            })
          }
          None => ()
        }
      }
      
      // 检查值是否一致
      if values.size() > 1 {
        consistency_violations = consistency_violations.push({
          violation_type: "baggage_value_inconsistency",
          description: "baggage键在不同服务中有不同值",
          baggage_key: key,
          propagation_chain: propagation_chain
        })
      }
    }
  }
  
  // 验证追踪一致性结果
  assert_eq(consistency_violations.length(), 0) // 不应该有一致性违规
  
  // 验证trace_id一致性
  assert_eq(unique_trace_ids.size(), 1) // 只有一个trace_id
  assert_true(unique_trace_ids.contains("trace-123"))
  
  // 验证父子关系
  let root_spans = sorted_chain.filter(fn(s) { s.parent_span == "" })
  assert_eq(root_spans.length(), 1) // 只有一个根span
  assert_eq(root_spans[0].span_id, "span-001")
  assert_eq(root_spans[0].service, "api-gateway")
  
  // 验证baggage传播
  let initial_baggage = sorted_chain[0].baggage
  let final_baggage = sorted_chain[sorted_chain.length() - 1].baggage
  
  // 初始baggage应该全部传播到最终span
  for key in initial_baggage.keys() {
    assert_true(final_baggage.contains(key))
    assert_eq(final_baggage.get(key).unwrap(), initial_baggage.get(key).unwrap())
  }
}

// 测试2: 跨服务指标一致性验证
test "跨服务指标一致性验证" {
  // 模拟跨服务指标数据
  let cross_service_metrics = [
    { 
      service: "api-gateway", 
      timestamp: 1640995200, 
      metric: "request_count", 
      value: 1000, 
      tags: { "endpoint": "/api/v1/users", "method": "GET" }
    },
    { 
      service: "auth-service", 
      timestamp: 1640995200, 
      metric: "request_count", 
      value: 800, 
      tags: { "endpoint": "/auth", "method": "POST" }
    },
    { 
      service: "user-service", 
      timestamp: 1640995200, 
      metric: "request_count", 
      value: 600, 
      tags: { "endpoint": "/users", "method": "GET" }
    },
    { 
      service: "order-service", 
      timestamp: 1640995200, 
      metric: "request_count", 
      value: 300, 
      tags: { "endpoint": "/orders", "method": "POST" }
    },
    { 
      service: "api-gateway", 
      timestamp: 1640995200, 
      metric: "error_count", 
      value: 50, 
      tags: { "endpoint": "/api/v1/users", "method": "GET" }
    },
    { 
      service: "auth-service", 
      timestamp: 1640995200, 
      metric: "error_count", 
      value: 40, 
      tags: { "endpoint": "/auth", "method": "POST" }
    },
    { 
      service: "user-service", 
      timestamp: 1640995200, 
      metric: "error_count", 
      value: 30, 
      tags: { "endpoint": "/users", "method": "GET" }
    },
    { 
      service: "order-service", 
      timestamp: 1640995200, 
      metric: "error_count", 
      value: 15, 
      tags: { "endpoint": "/orders", "method": "POST" }
    }
  ]
  
  // 指标一致性配置
  let metric_consistency_config = {
    error_rate_threshold: 0.1,        // 错误率阈值
    request_distribution_tolerance: 0.2, // 请求分布容差
    time_synchronization_tolerance: 5   // 时间同步容差（秒）
  }
  
  // 指标一致性验证算法
  let mut metric_consistency_results = []
  
  // 按服务和指标分组
  let mut service_metrics = {}
  for metric in cross_service_metrics {
    let service_key = metric.service + ":" + metric.metric
    let metrics_list = service_metrics.get(service_key).unwrap_or([])
    service_metrics = service_metrics.set(service_key, metrics_list.push(metric))
  }
  
  // 计算各服务的错误率
  let mut service_error_rates = {}
  for service in ["api-gateway", "auth-service", "user-service", "order-service"] {
    let request_count_key = service + ":request_count"
    let error_count_key = service + ":error_count"
    
    let request_count = service_metrics.get(request_count_key).unwrap_or([])[0].value
    let error_count = service_metrics.get(error_count_key).unwrap_or([])[0].value
    
    let error_rate = error_count.to_float() / request_count.to_float()
    service_error_rates = service_error_rates.set(service, error_rate)
  }
  
  // 验证错误率一致性
  let error_rates = service_error_rates.values()
  let avg_error_rate = error_rates.reduce(fn(acc, rate) { acc + rate }, 0.0) / error_rates.length().to_float()
  
  for (service, error_rate) in service_error_rates {
    let deviation = (error_rate - avg_error_rate).abs()
    let is_consistent = deviation <= metric_consistency_config.error_rate_threshold
    
    metric_consistency_results = metric_consistency_results.push({
      service: service,
      metric: "error_rate",
      value: error_rate,
      avg_value: avg_error_rate,
      deviation: deviation,
      is_consistent: is_consistent,
      threshold: metric_consistency_config.error_rate_threshold
    })
  }
  
  // 验证请求分布一致性
  let total_requests = cross_service_metrics
    .filter(fn(m) { m.metric == "request_count" })
    .reduce(fn(acc, m) { acc + m.value }, 0)
  
  for service in ["api-gateway", "auth-service", "user-service", "order-service"] {
    let service_requests = service_metrics.get(service + ":request_count").unwrap_or([])[0].value
    let expected_requests = total_requests / 4 // 平均分布
    let distribution_deviation = (service_requests - expected_requests).abs().to_float() / expected_requests.to_float()
    let is_distribution_consistent = distribution_deviation <= metric_consistency_config.request_distribution_tolerance
    
    metric_consistency_results = metric_consistency_results.push({
      service: service,
      metric: "request_distribution",
      value: service_requests.to_float(),
      avg_value: expected_requests.to_float(),
      deviation: distribution_deviation,
      is_consistent: is_distribution_consistent,
      threshold: metric_consistency_config.request_distribution_tolerance
    })
  }
  
  // 验证指标一致性结果
  assert_eq(metric_consistency_results.length(), 8) // 4个服务的错误率 + 4个服务的请求分布
  
  // 验证错误率一致性
  let error_rate_results = metric_consistency_results.filter(fn(r) { r.metric == "error_rate" })
  for result in error_rate_results {
    assert_true(result.is_consistent) // 所有服务的错误率应该一致
    assert_eq(result.value, 0.05) // 所有服务的错误率都是5%
  }
  
  // 验证请求分布一致性
  let distribution_results = metric_consistency_results.filter(fn(r) { r.metric == "request_distribution" })
  let consistent_distributions = distribution_results.filter(fn(r) { r.is_consistent })
  assert_true(consistent_distributions.length() >= 3) // 至少3个服务的请求分布应该一致
  
  // 验证总体一致性
  let consistent_metrics = metric_consistency_results.filter(fn(r) { r.is_consistent })
  let overall_consistency = consistent_metrics.length().to_float() / metric_consistency_results.length().to_float() * 100.0
  
  assert_true(overall_consistency >= 75.0) // 整体一致性应该至少75%
}

// 测试3: 跨服务日志关联一致性
test "跨服务日志关联一致性" {
  // 模拟跨服务日志数据
  let cross_service_logs = [
    { 
      service: "api-gateway", 
      timestamp: 1640995200, 
      level: "INFO", 
      message: "Request received", 
      trace_id: "trace-123", 
      span_id: "span-001",
      correlation_id: "corr-456"
    },
    { 
      service: "auth-service", 
      timestamp: 1640995260, 
      level: "INFO", 
      message: "Authentication started", 
      trace_id: "trace-123", 
      span_id: "span-002",
      correlation_id: "corr-456"
    },
    { 
      service: "auth-service", 
      timestamp: 1640995280, 
      level: "ERROR", 
      message: "Authentication failed", 
      trace_id: "trace-123", 
      span_id: "span-002",
      correlation_id: "corr-456"
    },
    { 
      service: "api-gateway", 
      timestamp: 1640995300, 
      level: "WARN", 
      message: "Authentication error response", 
      trace_id: "trace-123", 
      span_id: "span-001",
      correlation_id: "corr-456"
    },
    { 
      service: "user-service", 
      timestamp: 1640995400, 
      level: "INFO", 
      message: "User request processed", 
      trace_id: "trace-124", 
      span_id: "span-003",
      correlation_id: "corr-789"
    },
    { 
      service: "order-service", 
      timestamp: 1640995500, 
      level: "INFO", 
      message: "Order created", 
      trace_id: "trace-124", 
      span_id: "span-004",
      correlation_id: "corr-789"
    }
  ]
  
  // 日志关联一致性配置
  let log_correlation_config = {
    require_trace_id_consistency: true,    // 要求trace_id一致性
    require_correlation_id_consistency: true, // 要求correlation_id一致性
    require_temporal_consistency: true,    // 要求时间一致性
    max_log_gap: 300                       // 最大日志间隔（秒）
  }
  
  // 日志关联一致性验证算法
  let mut correlation_violations = []
  
  // 按trace_id分组
  let mut trace_groups = {}
  for log in cross_service_logs {
    let trace_logs = trace_groups.get(log.trace_id).unwrap_or([])
    trace_groups = trace_groups.set(log.trace_id, trace_logs.push(log))
  }
  
  // 验证每个trace组的关联一致性
  for (trace_id, logs) in trace_groups {
    // 按时间戳排序
    let sorted_logs = logs.sort_by(fn(a, b) {
      if a.timestamp < b.timestamp { -1 }
      else if a.timestamp > b.timestamp { 1 }
      else { 0 }
    })
    
    // 验证correlation_id一致性
    if log_correlation_config.require_correlation_id_consistency {
      let correlation_ids = sorted_logs.map(fn(l) { l.correlation_id })
      let unique_correlation_ids = correlation_ids.to_set()
      
      if unique_correlation_ids.size() > 1 {
        correlation_violations = correlation_violations.push({
          violation_type: "correlation_id_inconsistency",
          trace_id: trace_id,
          description: "同一trace中有多个correlation_id",
          correlation_ids: unique_correlation_ids.to_array()
        })
      }
    }
    
    // 验证时间一致性
    if log_correlation_config.require_temporal_consistency && sorted_logs.length() > 1 {
      for i in 1..sorted_logs.length() {
        let time_gap = sorted_logs[i].timestamp - sorted_logs[i-1].timestamp
        if time_gap > log_correlation_config.max_log_gap {
          correlation_violations = correlation_violations.push({
            violation_type: "temporal_inconsistency",
            trace_id: trace_id,
            description: "日志时间间隔过大",
            time_gap: time_gap,
            log1: {
              service: sorted_logs[i-1].service,
              timestamp: sorted_logs[i-1].timestamp
            },
            log2: {
              service: sorted_logs[i].service,
              timestamp: sorted_logs[i].timestamp
            }
          })
        }
      }
    }
    
    // 验证日志级别的逻辑一致性
    let error_logs = sorted_logs.filter(fn(l) { l.level == "ERROR" })
    if error_logs.length() > 0 {
      // 有ERROR日志后，后续应该有相关的警告或信息日志
      let first_error_index = sorted_logs.index(fn(l) { l.level == "ERROR" }).unwrap()
      let subsequent_logs = sorted_logs.slice(first_error_index + 1, sorted_logs.length())
      
      if subsequent_logs.length() == 0 {
        correlation_violations = correlation_violations.push({
          violation_type: "error_handling_inconsistency",
          trace_id: trace_id,
          description: "ERROR日志后缺少相关的处理日志",
          error_log: error_logs[0]
        })
      }
    }
  }
  
  // 验证日志关联一致性结果
  assert_eq(correlation_violations.length(), 0) // 不应该有关联违规
  
  // 验证trace分组
  assert_eq(trace_groups.size(), 2) // 应该有2个trace组
  assert_true(trace_groups.contains("trace-123"))
  assert_true(trace_groups.contains("trace-124"))
  
  // 验证trace-123的关联一致性
  let trace123_logs = trace_groups.get("trace-123").unwrap()
  assert_eq(trace123_logs.length(), 4) // 应该有4条日志
  
  // 验证correlation_id一致性
  let trace123_correlation_ids = trace123_logs.map(fn(l) { l.correlation_id }).to_set()
  assert_eq(trace123_correlation_ids.size(), 1) // 只有一个correlation_id
  assert_true(trace123_correlation_ids.contains("corr-456"))
  
  // 验证trace-124的关联一致性
  let trace124_logs = trace_groups.get("trace-124").unwrap()
  assert_eq(trace124_logs.length(), 2) // 应该有2条日志
  
  // 验证correlation_id一致性
  let trace124_correlation_ids = trace124_logs.map(fn(l) { l.correlation_id }).to_set()
  assert_eq(trace124_correlation_ids.size(), 1) // 只有一个correlation_id
  assert_true(trace124_correlation_ids.contains("corr-789"))
}

// 测试4: 跨服务配置一致性验证
test "跨服务配置一致性验证" {
  // 模拟跨服务配置数据
  let service_configurations = [
    {
      service: "api-gateway",
      config_version: "v1.2.3",
      features: {
        "rate_limiting": true,
        "circuit_breaker": true,
        "request_tracing": true,
        "metrics_collection": true
      },
      thresholds: {
        "max_request_size": 10485760,  // 10MB
        "max_response_time": 5000,     // 5秒
        "max_concurrent_requests": 1000
      },
      endpoints: [
        { path: "/api/v1/users", methods: ["GET", "POST"], auth_required: true },
        { path: "/api/v1/orders", methods: ["GET", "POST"], auth_required: true }
      ]
    },
    {
      service: "auth-service",
      config_version: "v1.2.3",
      features: {
        "rate_limiting": true,
        "circuit_breaker": true,
        "request_tracing": true,
        "metrics_collection": true
      },
      thresholds: {
        "max_request_size": 10485760,  // 10MB
        "max_response_time": 3000,     // 3秒
        "max_concurrent_requests": 500
      },
      endpoints: [
        { path: "/auth", methods: ["POST"], auth_required: false },
        { path: "/validate", methods: ["POST"], auth_required: false }
      ]
    },
    {
      service: "user-service",
      config_version: "v1.2.2",  // 版本不一致
      features: {
        "rate_limiting": true,
        "circuit_breaker": true,
        "request_tracing": true,
        "metrics_collection": false  // 特性不一致
      },
      thresholds: {
        "max_request_size": 10485760,  // 10MB
        "max_response_time": 5000,     // 5秒
        "max_concurrent_requests": 800
      },
      endpoints: [
        { path: "/users", methods: ["GET", "POST"], auth_required: true },
        { path: "/users/{id}", methods: ["GET", "PUT", "DELETE"], auth_required: true }
      ]
    },
    {
      service: "order-service",
      config_version: "v1.2.3",
      features: {
        "rate_limiting": true,
        "circuit_breaker": true,
        "request_tracing": true,
        "metrics_collection": true
      },
      thresholds: {
        "max_request_size": 5242880,   // 5MB - 阈值不一致
        "max_response_time": 5000,     // 5秒
        "max_concurrent_requests": 600
      },
      endpoints: [
        { path: "/orders", methods: ["GET", "POST"], auth_required: true },
        { path: "/orders/{id}", methods: ["GET", "PUT", "DELETE"], auth_required: true }
      ]
    }
  ]
  
  // 配置一致性验证算法
  let mut config_consistency_results = []
  
  // 提取全局配置基准
  let reference_config = service_configurations[0] // 使用api-gateway作为基准
  
  // 验证配置版本一致性
  let config_versions = service_configurations.map(fn(c) { c.config_version })
  let unique_versions = config_versions.to_set()
  
  config_consistency_results = config_consistency_results.push({
    consistency_type: "config_version",
    is_consistent: unique_versions.size() == 1,
    reference_version: reference_config.config_version,
    all_versions: config_versions,
    inconsistent_services: if unique_versions.size() > 1 {
      service_configurations.filter(fn(c) { c.config_version != reference_config.config_version }).map(fn(c) { c.service })
    } else {
      []
    }
  })
  
  // 验证功能特性一致性
  let reference_features = reference_config.features
  let mut feature_inconsistencies = []
  
  for service_config in service_configurations {
    if service_config.service != reference_config.service {
      let mut inconsistent_features = []
      
      for (feature_name, expected_value) in reference_features {
        match service_config.features.get(feature_name) {
          Some(actual_value) => {
            if actual_value != expected_value {
              inconsistent_features = inconsistent_features.push({
                feature: feature_name,
                expected: expected_value,
                actual: actual_value
              })
            }
          }
          None => {
            inconsistent_features = inconsistent_features.push({
              feature: feature_name,
              expected: expected_value,
              actual: "missing"
            })
          }
        }
      }
      
      if inconsistent_features.length() > 0 {
        feature_inconsistencies = feature_inconsistencies.push({
          service: service_config.service,
          inconsistencies: inconsistent_features
        })
      }
    }
  }
  
  config_consistency_results = config_consistency_results.push({
    consistency_type: "features",
    is_consistent: feature_inconsistencies.length() == 0,
    reference_features: reference_features,
    inconsistencies: feature_inconsistencies
  })
  
  // 验证阈值一致性（允许服务特定差异）
  let reference_thresholds = reference_config.thresholds
  let mut threshold_inconsistencies = []
  
  for service_config in service_configurations {
    if service_config.service != reference_config.service {
      let mut inconsistent_thresholds = []
      
      for (threshold_name, expected_value) in reference_thresholds {
        match service_config.thresholds.get(threshold_name) {
          Some(actual_value) => {
            // 允许某些阈值有合理差异
            let tolerance = match threshold_name {
              "max_response_time" => 2000,  // 2秒容差
              "max_concurrent_requests" => 200, // 200个请求容差
              _ => 0
            }
            
            let diff = (actual_value - expected_value).abs()
            if diff > tolerance {
              inconsistent_thresholds = inconsistent_thresholds.push({
                threshold: threshold_name,
                expected: expected_value,
                actual: actual_value,
                difference: diff,
                tolerance: tolerance
              })
            }
          }
          None => {
            inconsistent_thresholds = inconsistent_thresholds.push({
              threshold: threshold_name,
              expected: expected_value,
              actual: "missing",
              difference: 0,
              tolerance: 0
            })
          }
        }
      }
      
      if inconsistent_thresholds.length() > 0 {
        threshold_inconsistencies = threshold_inconsistencies.push({
          service: service_config.service,
          inconsistencies: inconsistent_thresholds
        })
      }
    }
  }
  
  config_consistency_results = config_consistency_results.push({
    consistency_type: "thresholds",
    is_consistent: threshold_inconsistencies.length() == 0,
    reference_thresholds: reference_thresholds,
    inconsistencies: threshold_inconsistencies
  })
  
  // 验证配置一致性结果
  assert_eq(config_consistency_results.length(), 3) // 版本、功能、阈值三种一致性检查
  
  // 验证配置版本一致性
  let version_consistency = config_consistency_results.filter(fn(r) { r.consistency_type == "config_version" })[0]
  assert_false(version_consistency.is_consistent) // 应该存在版本不一致
  assert_true(version_consistency.inconsistent_services.contains("user-service"))
  
  // 验证功能特性一致性
  let feature_consistency = config_consistency_results.filter(fn(r) { r.consistency_type == "features" })[0]
  assert_false(feature_consistency.is_consistent) // 应该存在功能不一致
  assert_true(feature_consistency.inconsistencies.length() > 0)
  assert_true(feature_consistency.inconsistencies[0].service == "user-service")
  assert_true(feature_consistency.inconsistencies[0].inconsistencies[0].feature == "metrics_collection")
  
  // 验证阈值一致性
  let threshold_consistency = config_consistency_results.filter(fn(r) { r.consistency_type == "thresholds" })[0]
  assert_false(threshold_consistency.is_consistent) // 应该存在阈值不一致
  assert_true(threshold_consistency.inconsistencies.length() > 0)
  assert_true(threshold_consistency.inconsistencies[0].service == "order-service")
  assert_true(threshold_consistency.inconsistencies[0].inconsistencies[0].threshold == "max_request_size")
}

// 测试5: 跨服务状态同步一致性
test "跨服务状态同步一致性" {
  // 模拟跨服务状态同步数据
  let service_states = [
    {
      service: "user-service",
      timestamp: 1640995200,
      states: {
        "user123": { status: "active", last_updated: 1640995100, version: 5 },
        "user456": { status: "inactive", last_updated: 1640995000, version: 3 },
        "user789": { status: "active", last_updated: 1640995150, version: 7 }
      }
    },
    {
      service: "order-service",
      timestamp: 1640995200,
      states: {
        "user123": { status: "active", last_updated: 1640995100, version: 5 },
        "user456": { status: "inactive", last_updated: 1640995000, version: 3 },
        "user789": { status: "active", last_updated: 1640995150, version: 6 } // 版本不一致
      }
    },
    {
      service: "payment-service",
      timestamp: 1640995200,
      states: {
        "user123": { status: "active", last_updated: 1640995100, version: 5 },
        "user456": { status: "suspended", last_updated: 1640995050, version: 4 }, // 状态和版本不一致
        "user789": { status: "active", last_updated: 1640995150, version: 7 }
      }
    },
    {
      service: "notification-service",
      timestamp: 1640995200,
      states: {
        "user123": { status: "active", last_updated: 1640995100, version: 5 },
        "user456": { status: "inactive", last_updated: 1640995000, version: 3 },
        "user789": { status: "active", last_updated: 1640995150, version: 7 }
      }
    }
  ]
  
  // 状态同步一致性配置
  let state_sync_config = {
    version_consistency_required: true,     // 要求版本一致性
    status_consistency_required: true,      // 要求状态一致性
    timestamp_consistency_required: true,   // 要求时间戳一致性
    max_version_drift: 2,                   // 最大版本漂移
    max_timestamp_drift: 60                 // 最大时间戳漂移（秒）
  }
  
  // 状态同步一致性验证算法
  let mut state_sync_results = []
  
  // 收集所有用户ID
  let mut all_user_ids = {}
  for service_state in service_states {
    for user_id in service_state.states.keys() {
      all_user_ids = all_user_ids.set(user_id, true)
    }
  }
  
  // 验证每个用户的状态一致性
  for user_id in all_user_ids.keys() {
    let mut user_states = []
    let mut service_versions = {}
    
    // 收集该用户在所有服务中的状态
    for service_state in service_states {
      match service_state.states.get(user_id) {
        Some(state) => {
          user_states = user_states.push({
            service: service_state.service,
            status: state.status,
            last_updated: state.last_updated,
            version: state.version
          })
          service_versions = service_versions.set(service_state.service, state.version)
        }
        None => ()
      }
    }
    
    // 验证版本一致性
    let versions = user_states.map(fn(s) { s.version })
    let max_version = versions.reduce(fn(acc, v) { if v > acc { v } else { acc }, 0)
    let min_version = versions.reduce(fn(acc, v) { if v < acc { v } else { acc }, max_version)
    let version_drift = max_version - min_version
    
    let version_consistent = if state_sync_config.version_consistency_required {
      version_drift <= state_sync_config.max_version_drift
    } else {
      true
    }
    
    // 验证状态一致性
    let statuses = user_states.map(fn(s) { s.status }).to_set()
    let status_consistent = if state_sync_config.status_consistency_required {
      statuses.size() == 1
    } else {
      true
    }
    
    // 验证时间戳一致性
    let timestamps = user_states.map(fn(s) { s.last_updated })
    let max_timestamp = timestamps.reduce(fn(acc, t) { if t > acc { t } else { acc }, 0)
    let min_timestamp = timestamps.reduce(fn(acc, t) { if t < acc { t } else { acc }, max_timestamp)
    let timestamp_drift = max_timestamp - min_timestamp
    
    let timestamp_consistent = if state_sync_config.timestamp_consistency_required {
      timestamp_drift <= state_sync_config.max_timestamp_drift
    } else {
      true
    }
    
    // 计算总体一致性
    let overall_consistent = version_consistent && status_consistent && timestamp_consistent
    
    // 识别不一致的服务
    let mut inconsistent_services = []
    
    if not version_consistent {
      let outdated_services = user_states.filter(fn(s) { 
        max_version - s.version > state_sync_config.max_version_drift 
      })
      for service in outdated_services {
        inconsistent_services = inconsistent_services.push({
          service: service.service,
          inconsistency_type: "version_drift",
          current_version: service.version,
          expected_version: max_version
        })
      }
    }
    
    if not status_consistent {
      let status_groups = {}
      for state in user_states {
        let group = status_groups.get(state.status).unwrap_or([])
        status_groups = status_groups.set(state.status, group.push(state.service))
      }
      
      for (status, services) in status_groups {
        if services.length() < user_states.length() {
          for service in services {
            inconsistent_services = inconsistent_services.push({
              service: service,
              inconsistency_type: "status_divergence",
              current_status: status,
              majority_status: if statuses.size() > 1 { "diverged" } else { status }
            })
          }
        }
      }
    }
    
    if not timestamp_consistent {
      let stale_services = user_states.filter(fn(s) { 
        max_timestamp - s.last_updated > state_sync_config.max_timestamp_drift 
      })
      for service in stale_services {
        inconsistent_services = inconsistent_services.push({
          service: service.service,
          inconsistency_type: "timestamp_drift",
          current_timestamp: service.last_updated,
          freshest_timestamp: max_timestamp
        })
      }
    }
    
    state_sync_results = state_sync_results.push({
      user_id: user_id,
      version_consistent: version_consistent,
      status_consistent: status_consistent,
      timestamp_consistent: timestamp_consistent,
      overall_consistent: overall_consistent,
      version_drift: version_drift,
      status_divergence: statuses.size(),
      timestamp_drift: timestamp_drift,
      inconsistent_services: inconsistent_services
    })
  }
  
  // 验证状态同步一致性结果
  assert_eq(state_sync_results.length(), 3) // 应该有3个用户的状态同步结果
  
  // 验证user123的一致性
  let user123_result = state_sync_results.filter(fn(r) { r.user_id == "user123" })[0]
  assert_true(user123_result.overall_consistent) // user123应该完全一致
  assert_eq(user123_result.version_drift, 0)
  assert_eq(user123_result.status_divergence, 1)
  assert_eq(user123_result.timestamp_drift, 0)
  assert_eq(user123_result.inconsistent_services.length(), 0)
  
  // 验证user456的一致性
  let user456_result = state_sync_results.filter(fn(r) { r.user_id == "user456" })[0]
  assert_false(user456_result.overall_consistent) // user456应该不一致
  assert_true(user456_result.version_drift > 0) // 版本漂移
  assert_true(user456_result.status_divergence > 1) // 状态分歧
  assert_true(user456_result.inconsistent_services.length() > 0)
  
  // 验证user789的一致性
  let user789_result = state_sync_results.filter(fn(r) { r.user_id == "user789" })[0]
  assert_false(user789_result.overall_consistent) // user789应该不一致
  assert_true(user789_result.version_drift > 0) // 版本漂移
  assert_eq(user789_result.status_divergence, 1) // 状态一致
  assert_true(user789_result.inconsistent_services.length() > 0)
  
  // 验证总体一致性统计
  let consistent_users = state_sync_results.filter(fn(r) { r.overall_consistent }).length()
  let overall_consistency_rate = consistent_users.to_float() / state_sync_results.length().to_float() * 100.0
  
  assert_eq(overall_consistency_rate, 33.33) // 只有1/3的用户状态完全一致
}