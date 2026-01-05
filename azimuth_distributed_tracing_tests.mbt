// Azimuth 分布式系统追踪测试用例
// 专注于分布式系统中的追踪和遥测数据协调功能

// 测试1: 分布式追踪链路传播
test "分布式追踪链路传播" {
  // 模拟分布式追踪数据
  let trace_spans = [
    { 
      trace_id: "trace-001", 
      span_id: "span-001", 
      parent_span_id: "", 
      service_name: "api-gateway", 
      operation_name: "process_request", 
      start_time: 1640995200000000, 
      end_time: 1640995200500000, 
      status: "ok",
      tags: [("http.method", "GET"), ("http.url", "/api/orders")]
    },
    { 
      trace_id: "trace-001", 
      span_id: "span-002", 
      parent_span_id: "span-001", 
      service_name: "order-service", 
      operation_name: "get_orders", 
      start_time: 1640995200100000, 
      end_time: 1640995200450000, 
      status: "ok",
      tags: [("db.query", "SELECT * FROM orders")]
    },
    { 
      trace_id: "trace-001", 
      span_id: "span-003", 
      parent_span_id: "span-002", 
      service_name: "database", 
      operation_name: "query", 
      start_time: 1640995200200000, 
      end_time: 1640995200400000, 
      status: "ok",
      tags: [("db.statement", "SELECT * FROM orders WHERE user_id = ?")]
    },
    { 
      trace_id: "trace-001", 
      span_id: "span-004", 
      parent_span_id: "span-001", 
      service_name: "user-service", 
      operation_name: "get_user", 
      start_time: 1640995200150000, 
      end_time: 1640995200350000, 
      status: "ok",
      tags: [("cache.hit", "false")]
    },
    { 
      trace_id: "trace-002", 
      span_id: "span-005", 
      parent_span_id: "", 
      service_name: "api-gateway", 
      operation_name: "process_request", 
      start_time: 1640995300000000, 
      end_time: 1640995300800000, 
      status: "error",
      tags: [("http.method", "POST"), ("http.url", "/api/payments")]
    },
    { 
      trace_id: "trace-002", 
      span_id: "span-006", 
      parent_span_id: "span-005", 
      service_name: "payment-service", 
      operation_name: "process_payment", 
      start_time: 1640995300100000, 
      end_time: 1640995300750000, 
      status: "error",
      tags: [("error", "payment_declined")]
    }
  ]
  
  // 构建追踪树
  let build_trace_tree = fn(spans: Array[TraceSpan>) {
    let traces = spans.map_fn(s) { s.trace_id }.unique()
    let mut trace_trees = []
    
    for trace_id in traces {
      let trace_spans = spans.filter_fn(s) { s.trace_id == trace_id }
      let root_spans = trace_spans.filter_fn(s) { s.parent_span_id == "" }
      
      for root_span in root_spans {
        let build_tree_recursive = fn(span: TraceSpan, all_spans: Array[TraceSpan>) {
          let child_spans = all_spans.filter_fn(s) { s.parent_span_id == span.span_id }
          let mut children = []
          
          for child in child_spans {
            children = children.push(build_tree_recursive(child, all_spans))
          }
          
          {
            span: span,
            children: children,
            duration: span.end_time - span.start_time,
            service_name: span.service_name,
            operation_name: span.operation_name,
            status: span.status
          }
        }
        
        trace_trees = trace_trees.push(build_tree_recursive(root_span, trace_spans))
      }
    }
    
    trace_trees
  }
  
  // 计算追踪统计
  let calculate_trace_stats = fn(trace_tree: TraceTreeNode) {
    let mut total_spans = 1
    let mut total_duration = trace_tree.duration
    let mut service_counts = []
    let mut status_counts = []
    let mut max_depth = 0
    
    // 初始化服务计数
    let services = ["api-gateway", "order-service", "database", "user-service", "payment-service"]
    for service in services {
      service_counts = service_counts.push({ service: service, count: 0 })
    }
    
    // 初始化状态计数
    let statuses = ["ok", "error"]
    for status in statuses {
      status_counts = status_counts.push({ status: status, count: 0 })
    }
    
    // 更新当前节点的计数
    for i in 0..service_counts.length() {
      if service_counts[i].service == trace_tree.service_name {
        service_counts[i] = { service_counts[i] | count: service_counts[i].count + 1 }
      }
    }
    
    for i in 0..status_counts.length() {
      if status_counts[i].status == trace_tree.status {
        status_counts[i] = { status_counts[i] | count: status_counts[i].count + 1 }
      }
    }
    
    // 递归处理子节点
    let process_children = fn(node: TraceTreeNode, depth: Int) {
      let mut spans = 1
      let mut duration = node.duration
      let mut max_d = depth
      
      for child in node.children {
        let child_result = process_children(child, depth + 1)
        spans = spans + child_result.spans
        duration = duration + child_result.duration
        
        if child_result.max_depth > max_d {
          max_d = child_result.max_depth
        }
      }
      
      {
        spans: spans,
        duration: duration,
        max_depth: max_d
      }
    }
    
    let children_stats = process_children(trace_tree, 0)
    total_spans = total_spans + children_stats.spans - 1  // 减去根节点重复计数
    total_duration = children_stats.duration
    max_depth = children_stats.max_depth
    
    // 统计所有子节点的服务和状态
    let count_all_nodes = fn(node: TraceTreeNode, services: Array[ServiceCount>, statuses: Array[StatusCount>) {
      let mut updated_services = services
      let mut updated_statuses = statuses
      
      // 更新当前节点
      for i in 0..updated_services.length() {
        if updated_services[i].service == node.service_name {
          updated_services[i] = { updated_services[i] | count: updated_services[i].count + 1 }
        }
      }
      
      for i in 0..updated_statuses.length() {
        if updated_statuses[i].status == node.status {
          updated_statuses[i] = { updated_statuses[i] | count: updated_statuses[i].count + 1 }
        }
      }
      
      // 递归处理子节点
      for child in node.children {
        let result = count_all_nodes(child, updated_services, updated_statuses)
        updated_services = result.services
        updated_statuses = result.statuses
      }
      
      {
        services: updated_services,
        statuses: updated_statuses
      }
    }
    
    let count_result = count_all_nodes(trace_tree, service_counts, status_counts)
    
    {
      trace_id: trace_tree.span.trace_id,
      total_spans: total_spans,
      total_duration: total_duration,
      max_depth: max_depth,
      service_counts: count_result.services,
      status_counts: count_result.statuses,
      root_service: trace_tree.service_name,
      root_operation: trace_tree.operation_name
    }
  }
  
  // 构建追踪树并计算统计
  let trace_trees = build_trace_tree(trace_spans)
  let trace_stats = trace_trees.map_fn(tree) { calculate_trace_stats(tree) }
  
  // 验证追踪树构建
  assert_eq(trace_trees.length(), 2)  // 两个不同的追踪ID
  
  // 验证第一个追踪树 (trace-001)
  let trace_001_tree = trace_trees[0]
  assert_eq(trace_001_tree.span.trace_id, "trace-001")
  assert_eq(trace_001_tree.span.service_name, "api-gateway")
  assert_eq(trace_001_tree.children.length(), 2)  // order-service和user-service
  
  // 验证第二个追踪树 (trace-002)
  let trace_002_tree = trace_trees[1]
  assert_eq(trace_002_tree.span.trace_id, "trace-002")
  assert_eq(trace_002_tree.span.service_name, "api-gateway")
  assert_eq(trace_002_tree.children.length(), 1)  // payment-service
  
  // 验证追踪统计
  assert_eq(trace_stats.length(), 2)
  
  // 验证trace-001的统计
  let trace_001_stats = trace_stats[0]
  assert_eq(trace_001_stats.trace_id, "trace-001")
  assert_eq(trace_001_stats.total_spans, 4)
  assert_eq(trace_001_stats.total_duration, 500000)  // 500ms
  assert_eq(trace_001_stats.max_depth, 2)  // 最深层次为2
  assert_eq(trace_001_stats.root_service, "api-gateway")
  assert_eq(trace_001_stats.root_operation, "process_request")
  
  // 验证服务计数
  let api_gateway_count = trace_001_stats.service_counts.find_fn(c) { c.service == "api-gateway" }
  assert_true(api_gateway_count.is_some())
  assert_eq(api_gateway_count.unwrap().count, 1)
  
  let order_service_count = trace_001_stats.service_counts.find_fn(c) { c.service == "order-service" }
  assert_true(order_service_count.is_some())
  assert_eq(order_service_count.unwrap().count, 1)
  
  let database_count = trace_001_stats.service_counts.find_fn(c) { c.service == "database" }
  assert_true(database_count.is_some())
  assert_eq(database_count.unwrap().count, 1)
  
  let user_service_count = trace_001_stats.service_counts.find_fn(c) { c.service == "user-service" }
  assert_true(user_service_count.is_some())
  assert_eq(user_service_count.unwrap().count, 1)
  
  // 验证状态计数
  let ok_count = trace_001_stats.status_counts.find_fn(c) { c.status == "ok" }
  assert_true(ok_count.is_some())
  assert_eq(ok_count.unwrap().count, 4)
  
  let error_count = trace_001_stats.status_counts.find_fn(c) { c.status == "error" }
  assert_true(error_count.is_some())
  assert_eq(error_count.unwrap().count, 0)
  
  // 验证trace-002的统计
  let trace_002_stats = trace_stats[1]
  assert_eq(trace_002_stats.trace_id, "trace-002")
  assert_eq(trace_002_stats.total_spans, 2)
  assert_eq(trace_002_stats.total_duration, 800000)  // 800ms
  assert_eq(trace_002_stats.max_depth, 1)  // 最深层次为1
  assert_eq(trace_002_stats.root_service, "api-gateway")
  assert_eq(trace_002_stats.root_operation, "process_request")
  
  // 验证服务计数
  let api_gateway_count_2 = trace_002_stats.service_counts.find_fn(c) { c.service == "api-gateway" }
  assert_true(api_gateway_count_2.is_some())
  assert_eq(api_gateway_count_2.unwrap().count, 1)
  
  let payment_service_count = trace_002_stats.service_counts.find_fn(c) { c.service == "payment-service" }
  assert_true(payment_service_count.is_some())
  assert_eq(payment_service_count.unwrap().count, 1)
  
  // 验证状态计数
  let ok_count_2 = trace_002_stats.status_counts.find_fn(c) { c.status == "ok" }
  assert_true(ok_count_2.is_some())
  assert_eq(ok_count_2.unwrap().count, 0)
  
  let error_count_2 = trace_002_stats.status_counts.find_fn(c) { c.status == "error" }
  assert_true(error_count_2.is_some())
  assert_eq(error_count_2.unwrap().count, 2)
}

// 测试2: 分布式系统性能分析
test "分布式系统性能分析" {
  // 模拟分布式系统性能数据
  let performance_data = [
    { 
      service: "api-gateway", 
      operation: "process_request", 
      timestamp: 1640995200, 
      duration_ms: 50, 
      status: "success",
      cpu_usage: 25.5,
      memory_usage: 512.3
    },
    { 
      service: "order-service", 
      operation: "get_orders", 
      timestamp: 1640995200, 
      duration_ms: 35, 
      status: "success",
      cpu_usage: 35.2,
      memory_usage: 768.7
    },
    { 
      service: "user-service", 
      operation: "get_user", 
      timestamp: 1640995200, 
      duration_ms: 20, 
      status: "success",
      cpu_usage: 15.8,
      memory_usage: 256.4
    },
    { 
      service: "database", 
      operation: "query", 
      timestamp: 1640995200, 
      duration_ms: 15, 
      status: "success",
      cpu_usage: 45.6,
      memory_usage: 1024.8
    },
    { 
      service: "api-gateway", 
      operation: "process_request", 
      timestamp: 1640995210, 
      duration_ms: 75, 
      status: "success",
      cpu_usage: 35.7,
      memory_usage: 518.9
    },
    { 
      service: "order-service", 
      operation: "get_orders", 
      timestamp: 1640995210, 
      duration_ms: 55, 
      status: "success",
      cpu_usage: 45.3,
      memory_usage: 775.2
    },
    { 
      service: "payment-service", 
      operation: "process_payment", 
      timestamp: 1640995210, 
      duration_ms: 120, 
      status: "error",
      cpu_usage: 65.8,
      memory_usage: 896.5
    },
    { 
      service: "api-gateway", 
      operation: "process_request", 
      timestamp: 1640995220, 
      duration_ms: 45, 
      status: "success",
      cpu_usage: 28.9,
      memory_usage: 515.6
    },
    { 
      service: "order-service", 
      operation: "get_orders", 
      timestamp: 1640995220, 
      duration_ms: 40, 
      status: "success",
      cpu_usage: 38.5,
      memory_usage: 770.1
    },
    { 
      service: "user-service", 
      operation: "get_user", 
      timestamp: 1640995220, 
      duration_ms: 25, 
      status: "success",
      cpu_usage: 18.2,
      memory_usage: 258.7
    }
  ]
  
  // 按服务分组分析性能
  let analyze_service_performance = fn(data: Array[PerformanceData>) {
    let services = ["api-gateway", "order-service", "user-service", "payment-service", "database"]
    let mut service_performance = []
    
    for service in services {
      let service_data = data.filter_fn(d) { d.service == service }
      
      if service_data.length() > 0 {
        let mut total_duration = 0
        let mut success_count = 0
        let mut total_cpu = 0.0
        let mut total_memory = 0.0
        let mut durations = []
        
        for d in service_data {
          total_duration = total_duration + d.duration_ms
          total_cpu = total_cpu + d.cpu_usage
          total_memory = total_memory + d.memory_usage
          durations = durations.push(d.duration_ms)
          
          if d.status == "success" {
            success_count = success_count + 1
          }
        }
        
        let avg_duration = total_duration / service_data.length()
        let success_rate = (success_count.to_float() / service_data.length().to_float()) * 100.0
        let avg_cpu = total_cpu / service_data.length().to_float()
        let avg_memory = total_memory / service_data.length().to_float()
        
        // 计算百分位数（简化版）
        let sorted_durations = durations.sort()
        let p50 = sorted_durations[sorted_durations.length() / 2]
        let p95 = sorted_durations[(sorted_durations.length() * 95 / 100).min(sorted_durations.length() - 1)]
        let p99 = sorted_durations[(sorted_durations.length() * 99 / 100).min(sorted_durations.length() - 1)]
        
        service_performance = service_performance.push({
          service: service,
          request_count: service_data.length(),
          avg_duration_ms: avg_duration,
          p50_duration_ms: p50,
          p95_duration_ms: p95,
          p99_duration_ms: p99,
          success_rate: success_rate,
          avg_cpu_usage: avg_cpu,
          avg_memory_usage: avg_memory
        })
      }
    }
    
    service_performance
  }
  
  // 识别性能瓶颈
  let identify_performance_bottlenecks = fn(service_performance: Array[ServicePerformance>) {
    let high_latency_threshold = 100  // ms
    let low_success_rate_threshold = 95.0  // %
    let high_cpu_threshold = 60.0  // %
    let high_memory_threshold = 800.0  // MB
    
    let mut bottlenecks = []
    
    for perf in service_performance {
      let mut issues = []
      
      if perf.p95_duration_ms > high_latency_threshold {
        issues = issues.push({
          type: "high_latency",
          severity: if perf.p95_duration_ms > 200 { "critical" } else { "warning" },
          value: perf.p95_duration_ms,
          threshold: high_latency_threshold
        })
      }
      
      if perf.success_rate < low_success_rate_threshold {
        issues = issues.push({
          type: "low_success_rate",
          severity: if perf.success_rate < 90.0 { "critical" } else { "warning" },
          value: perf.success_rate,
          threshold: low_success_rate_threshold
        })
      }
      
      if perf.avg_cpu_usage > high_cpu_threshold {
        issues = issues.push({
          type: "high_cpu_usage",
          severity: if perf.avg_cpu_usage > 80.0 { "critical" } else { "warning" },
          value: perf.avg_cpu_usage,
          threshold: high_cpu_threshold
        })
      }
      
      if perf.avg_memory_usage > high_memory_threshold {
        issues = issues.push({
          type: "high_memory_usage",
          severity: if perf.avg_memory_usage > 1000.0 { "critical" } else { "warning" },
          value: perf.avg_memory_usage,
          threshold: high_memory_threshold
        })
      }
      
      if issues.length() > 0 {
        bottlenecks = bottlenecks.push({
          service: perf.service,
          issues: issues,
          overall_severity: if issues.any_fn(i) { i.severity == "critical" } { "critical" } else { "warning" }
        })
      }
    }
    
    bottlenecks
  }
  
  // 计算系统整体性能指标
  let calculate_system_metrics = fn(data: Array[PerformanceData>) {
    let mut total_requests = 0
    let mut total_duration = 0
    let mut success_count = 0
    let mut total_cpu = 0.0
    let mut total_memory = 0.0
    
    for d in data {
      total_requests = total_requests + 1
      total_duration = total_duration + d.duration_ms
      total_cpu = total_cpu + d.cpu_usage
      total_memory = total_memory + d.memory_usage
      
      if d.status == "success" {
        success_count = success_count + 1
      }
    }
    
    let overall_avg_duration = total_duration / total_requests
    let overall_success_rate = (success_count.to_float() / total_requests.to_float()) * 100.0
    let overall_avg_cpu = total_cpu / total_requests.to_float()
    let overall_avg_memory = total_memory / total_requests.to_float()
    
    // 计算请求分布
    let services = ["api-gateway", "order-service", "user-service", "payment-service", "database"]
    let mut request_distribution = []
    
    for service in services {
      let service_requests = data.filter_fn(d) { d.service == service }.length()
      let percentage = (service_requests.to_float() / total_requests.to_float()) * 100.0
      
      request_distribution = request_distribution.push({
        service: service,
        request_count: service_requests,
        percentage: percentage
      })
    }
    
    {
      total_requests: total_requests,
      overall_avg_duration_ms: overall_avg_duration,
      overall_success_rate: overall_success_rate,
      overall_avg_cpu_usage: overall_avg_cpu,
      overall_avg_memory_usage: overall_avg_memory,
      request_distribution: request_distribution
    }
  }
  
  // 执行性能分析
  let service_performance = analyze_service_performance(performance_data)
  let bottlenecks = identify_performance_bottlenecks(service_performance)
  let system_metrics = calculate_system_metrics(performance_data)
  
  // 验证服务性能分析
  assert_eq(service_performance.length(), 5)
  
  // 验证api-gateway性能
  let api_gateway_perf = service_performance.find_fn(p) { p.service == "api-gateway" }
  assert_true(api_gateway_perf.is_some())
  assert_eq(api_gateway_perf.unwrap().request_count, 3)
  assert_eq(api_gateway_perf.unwrap().avg_duration_ms, 56)  // (50+75+45)/3 = 56.67，取整数
  assert_eq(api_gateway_perf.unwrap().p50_duration_ms, 50)
  assert_eq(api_gateway_perf.unwrap().p95_duration_ms, 75)
  assert_eq(api_gateway_perf.unwrap().p99_duration_ms, 75)
  assert_eq(api_gateway_perf.unwrap().success_rate, 100.0)
  assert_eq(api_gateway_perf.unwrap().avg_cpu_usage, 30.03)  // (25.5+35.7+28.9)/3 = 30.03
  assert_eq(api_gateway_perf.unwrap().avg_memory_usage, 515.6)  // (512.3+518.9+515.6)/3 = 515.6
  
  // 验证payment-service性能
  let payment_service_perf = service_performance.find_fn(p) { p.service == "payment-service" }
  assert_true(payment_service_perf.is_some())
  assert_eq(payment_service_perf.unwrap().request_count, 1)
  assert_eq(payment_service_perf.unwrap().avg_duration_ms, 120)
  assert_eq(payment_service_perf.unwrap().p50_duration_ms, 120)
  assert_eq(payment_service_perf.unwrap().p95_duration_ms, 120)
  assert_eq(payment_service_perf.unwrap().p99_duration_ms, 120)
  assert_eq(payment_service_perf.unwrap().success_rate, 0.0)
  assert_eq(payment_service_perf.unwrap().avg_cpu_usage, 65.8)
  assert_eq(payment_service_perf.unwrap().avg_memory_usage, 896.5)
  
  // 验证性能瓶颈识别
  assert_eq(bottlenecks.length(), 1)
  
  let bottleneck = bottlenecks[0]
  assert_eq(bottleneck.service, "payment-service")
  assert_eq(bottleneck.overall_severity, "critical")
  assert_eq(bottleneck.issues.length(), 3)  // high_latency, low_success_rate, high_cpu_usage
  
  // 验证瓶颈问题
  let high_latency_issue = bottleneck.issues.find_fn(i) { i.type == "high_latency" }
  assert_true(high_latency_issue.is_some())
  assert_eq(high_latency_issue.unwrap().severity, "warning")
  assert_eq(high_latency_issue.unwrap().value, 120)
  
  let low_success_rate_issue = bottleneck.issues.find_fn(i) { i.type == "low_success_rate" }
  assert_true(low_success_rate_issue.is_some())
  assert_eq(low_success_rate_issue.unwrap().severity, "critical")
  assert_eq(low_success_rate_issue.unwrap().value, 0.0)
  
  let high_cpu_issue = bottleneck.issues.find_fn(i) { i.type == "high_cpu_usage" }
  assert_true(high_cpu_issue.is_some())
  assert_eq(high_cpu_issue.unwrap().severity, "warning")
  assert_eq(high_cpu_issue.unwrap().value, 65.8)
  
  // 验证系统整体指标
  assert_eq(system_metrics.total_requests, 10)
  assert_eq(system_metrics.overall_avg_duration_ms, 52)  // (50+35+20+15+75+55+120+45+40+25)/10 = 52
  assert_eq(system_metrics.overall_success_rate, 90.0)  // 9/10 * 100 = 90%
  assert_eq(system_metrics.overall_avg_cpu_usage, 36.45)  // (25.5+35.2+15.8+45.6+35.7+45.3+65.8+28.9+38.5+18.2)/10 = 36.45
  assert_eq(system_metrics.overall_avg_memory_usage, 618.71)  // (512.3+768.7+256.4+1024.8+518.9+775.2+896.5+515.6+770.1+258.7)/10 = 618.71
  
  // 验证请求分布
  assert_eq(system_metrics.request_distribution.length(), 5)
  
  let api_gateway_dist = system_metrics.request_distribution.find_fn(d) { d.service == "api-gateway" }
  assert_true(api_gateway_dist.is_some())
  assert_eq(api_gateway_dist.unwrap().request_count, 3)
  assert_eq(api_gateway_dist.unwrap().percentage, 30.0)  // 3/10 * 100 = 30%
  
  let order_service_dist = system_metrics.request_distribution.find_fn(d) { d.service == "order-service" }
  assert_true(order_service_dist.is_some())
  assert_eq(order_service_dist.unwrap().request_count, 3)
  assert_eq(order_service_dist.unwrap().percentage, 30.0)  // 3/10 * 100 = 30%
}

// 测试3: 分布式系统依赖分析
test "分布式系统依赖分析" {
  // 模拟服务调用关系数据
  let service_dependencies = [
    { 
      caller_service: "api-gateway", 
      callee_service: "order-service", 
      timestamp: 1640995200, 
      call_count: 25, 
      error_count: 1, 
      avg_latency_ms: 45 
    },
    { 
      caller_service: "api-gateway", 
      callee_service: "user-service", 
      timestamp: 1640995200, 
      call_count: 30, 
      error_count: 0, 
      avg_latency_ms: 20 
    },
    { 
      caller_service: "order-service", 
      callee_service: "database", 
      timestamp: 1640995200, 
      call_count: 50, 
      error_count: 2, 
      avg_latency_ms: 15 
    },
    { 
      caller_service: "user-service", 
      callee_service: "database", 
      timestamp: 1640995200, 
      call_count: 35, 
      error_count: 0, 
      avg_latency_ms: 12 
    },
    { 
      caller_service: "api-gateway", 
      callee_service: "payment-service", 
      timestamp: 1640995210, 
      call_count: 15, 
      error_count: 3, 
      avg_latency_ms: 80 
    },
    { 
      caller_service: "payment-service", 
      callee_service: "external-payment-gateway", 
      timestamp: 1640995210, 
      call_count: 15, 
      error_count: 3, 
      avg_latency_ms: 75 
    },
    { 
      caller_service: "order-service", 
      callee_service: "inventory-service", 
      timestamp: 1640995220, 
      call_count: 20, 
      error_count: 1, 
      avg_latency_ms: 35 
    }
  ]
  
  // 构建服务依赖图
  let build_dependency_graph = fn(dependencies: Array[ServiceDependency>) {
    let services = ["api-gateway", "order-service", "user-service", "payment-service", "database", "inventory-service", "external-payment-gateway"]
    let mut nodes = []
    let mut edges = []
    
    // 创建节点
    for service in services {
      let inbound_deps = dependencies.filter_fn(d) { d.callee_service == service }
      let outbound_deps = dependencies.filter_fn(d) { d.caller_service == service }
      
      let mut total_inbound_calls = 0
      let mut total_inbound_errors = 0
      let mut total_inbound_latency = 0
      
      for dep in inbound_deps {
        total_inbound_calls = total_inbound_calls + dep.call_count
        total_inbound_errors = total_inbound_errors + dep.error_count
        total_inbound_latency = total_inbound_latency + (dep.avg_latency_ms * dep.call_count)
      }
      
      let mut total_outbound_calls = 0
      let mut total_outbound_errors = 0
      let mut total_outbound_latency = 0
      
      for dep in outbound_deps {
        total_outbound_calls = total_outbound_calls + dep.call_count
        total_outbound_errors = total_outbound_errors + dep.error_count
        total_outbound_latency = total_outbound_latency + (dep.avg_latency_ms * dep.call_count)
      }
      
      let inbound_error_rate = if total_inbound_calls > 0 {
        (total_inbound_errors.to_float() / total_inbound_calls.to_float()) * 100.0
      } else {
        0.0
      }
      
      let outbound_error_rate = if total_outbound_calls > 0 {
        (total_outbound_errors.to_float() / total_outbound_calls.to_float()) * 100.0
      } else {
        0.0
      }
      
      let avg_inbound_latency = if total_inbound_calls > 0 {
        total_inbound_latency / total_inbound_calls
      } else {
        0
      }
      
      let avg_outbound_latency = if total_outbound_calls > 0 {
        total_outbound_latency / total_outbound_calls
      } else {
        0
      }
      
      nodes = nodes.push({
        service: service,
        inbound_calls: total_inbound_calls,
        outbound_calls: total_outbound_calls,
        inbound_error_rate: inbound_error_rate,
        outbound_error_rate: outbound_error_rate,
        avg_inbound_latency: avg_inbound_latency,
        avg_outbound_latency: avg_outbound_latency,
        dependencies: inbound_deps.length() + outbound_deps.length()
      })
    }
    
    // 创建边
    for dep in dependencies {
      let error_rate = if dep.call_count > 0 {
        (dep.error_count.to_float() / dep.call_count.to_float()) * 100.0
      } else {
        0.0
      }
      
      edges = edges.push({
        from: dep.caller_service,
        to: dep.callee_service,
        call_count: dep.call_count,
        error_count: dep.error_count,
        error_rate: error_rate,
        avg_latency_ms: dep.avg_latency_ms
      })
    }
    
    {
      nodes: nodes,
      edges: edges
    }
  }
  
  // 识别关键路径
  let identify_critical_paths = fn(graph: DependencyGraph) {
    // 按调用量排序的边
    let sorted_edges = graph.edges.sort_by(fn(a, b) { b.call_count - a.call_count })
    
    // 取前5个最繁忙的连接
    let top_edges = sorted_edges.slice(0, 5.min(sorted_edges.length()))
    
    // 计算关键路径指标
    let mut critical_paths = []
    
    for edge in top_edges {
      let from_node = graph.nodes.find_fn(n) { n.service == edge.from }
      let to_node = graph.nodes.find_fn(n) { n.service == edge.to }
      
      if from_node.is_some() and to_node.is_some() {
        let from_dep_count = from_node.unwrap().dependencies
        let to_dep_count = to_node.unwrap().dependencies
        
        // 关键性评分：基于调用量、错误率和节点依赖数
        let criticality_score = 
          (edge.call_count.to_float() / 10.0) + 
          (edge.error_rate * 5.0) + 
          (from_dep_count.to_float() * 2.0) + 
          (to_dep_count.to_float() * 2.0)
        
        critical_paths = critical_paths.push({
          from: edge.from,
          to: edge.to,
          call_count: edge.call_count,
          error_rate: edge.error_rate,
          avg_latency_ms: edge.avg_latency_ms,
          criticality_score: criticality_score,
          from_dependencies: from_dep_count,
          to_dependencies: to_dep_count
        })
      }
    }
    
    // 按关键性评分排序
    critical_paths.sort_by(fn(a, b) { b.criticality_score - a.criticality_score })
  }
  
  // 识别潜在故障点
  let identify_potential_failure_points = fn(graph: DependencyGraph) {
    let high_error_rate_threshold = 10.0  // %
    let high_latency_threshold = 50  // ms
    let high_dependency_threshold = 5  // 依赖数
    
    let mut failure_points = []
    
    for node in graph.nodes {
      let mut issues = []
      
      if node.inbound_error_rate > high_error_rate_threshold {
        issues = issues.push({
          type: "high_inbound_error_rate",
          severity: if node.inbound_error_rate > 20.0 { "critical" } else { "warning" },
          value: node.inbound_error_rate
        })
      }
      
      if node.outbound_error_rate > high_error_rate_threshold {
        issues = issues.push({
          type: "high_outbound_error_rate",
          severity: if node.outbound_error_rate > 20.0 { "critical" } else { "warning" },
          value: node.outbound_error_rate
        })
      }
      
      if node.avg_inbound_latency > high_latency_threshold {
        issues = issues.push({
          type: "high_inbound_latency",
          severity: if node.avg_inbound_latency > 100 { "critical" } else { "warning" },
          value: node.avg_inbound_latency
        })
      }
      
      if node.avg_outbound_latency > high_latency_threshold {
        issues = issues.push({
          type: "high_outbound_latency",
          severity: if node.avg_outbound_latency > 100 { "critical" } else { "warning" },
          value: node.avg_outbound_latency
        })
      }
      
      if node.dependencies > high_dependency_threshold {
        issues = issues.push({
          type: "too_many_dependencies",
          severity: if node.dependencies > 10 { "critical" } else { "warning" },
          value: node.dependencies
        })
      }
      
      if issues.length() > 0 {
        failure_points = failure_points.push({
          service: node.service,
          issues: issues,
          total_calls: node.inbound_calls + node.outbound_calls,
          overall_severity: if issues.any_fn(i) { i.severity == "critical" } { "critical" } else { "warning" }
        })
      }
    }
    
    // 按总调用量排序（调用量高的服务优先处理）
    failure_points.sort_by(fn(a, b) { b.total_calls - a.total_calls })
  }
  
  // 构建依赖图并分析
  let dependency_graph = build_dependency_graph(service_dependencies)
  let critical_paths = identify_critical_paths(dependency_graph)
  let failure_points = identify_potential_failure_points(dependency_graph)
  
  // 验证依赖图构建
  assert_eq(dependency_graph.nodes.length(), 7)
  assert_eq(dependency_graph.edges.length(), 7)
  
  // 验证api-gateway节点
  let api_gateway_node = dependency_graph.nodes.find_fn(n) { n.service == "api-gateway" }
  assert_true(api_gateway_node.is_some())
  assert_eq(api_gateway_node.unwrap().inbound_calls, 0)
  assert_eq(api_gateway_node.unwrap().outbound_calls, 70)  // 25+30+15
  assert_eq(api_gateway_node.unwrap().inbound_error_rate, 0.0)
  assert_eq(api_gateway_node.unwrap().outbound_error_rate, 5.71)  // (1+0+3)/70 * 100 = 5.71%
  assert_eq(api_gateway_node.unwrap().avg_inbound_latency, 0)
  assert_eq(api_gateway_node.unwrap().avg_outbound_latency, 44)  // (25*45+30*20+15*80)/70 = 44.64，取整数
  assert_eq(api_gateway_node.unwrap().dependencies, 3)
  
  // 验证database节点
  let database_node = dependency_graph.nodes.find_fn(n) { n.service == "database" }
  assert_true(database_node.is_some())
  assert_eq(database_node.unwrap().inbound_calls, 85)  // 50+35
  assert_eq(database_node.unwrap().outbound_calls, 0)
  assert_eq(database_node.unwrap().inbound_error_rate, 2.35)  // (2+0)/85 * 100 = 2.35%
  assert_eq(database_node.unwrap().outbound_error_rate, 0.0)
  assert_eq(database_node.unwrap().avg_inbound_latency, 13)  // (50*15+35*12)/85 = 13.82，取整数
  assert_eq(database_node.unwrap().avg_outbound_latency, 0)
  assert_eq(database_node.unwrap().dependencies, 2)
  
  // 验证关键路径识别
  assert_eq(critical_paths.length(), 5)
  
  // 最关键的路径应该是order-service -> database（调用量最大）
  let most_critical_path = critical_paths[0]
  assert_eq(most_critical_path.from, "order-service")
  assert_eq(most_critical_path.to, "database")
  assert_eq(most_critical_path.call_count, 50)
  assert_eq(most_critical_path.error_rate, 4.0)  // 2/50 * 100 = 4%
  assert_eq(most_critical_path.avg_latency_ms, 15)
  assert_true(most_critical_path.criticality_score > 0.0)
  
  // 验证潜在故障点识别
  assert_eq(failure_points.length(), 2)
  
  // 第一个故障点应该是payment-service（错误率高）
  let payment_failure_point = failure_points[0]
  assert_eq(payment_failure_point.service, "payment-service")
  assert_eq(payment_failure_point.overall_severity, "critical")
  assert_eq(payment_failure_point.issues.length(), 2)  // high_outbound_error_rate, high_outbound_latency
  
  // 验证payment-service的问题
  let payment_outbound_error_issue = payment_failure_point.issues.find_fn(i) { i.type == "high_outbound_error_rate" }
  assert_true(payment_outbound_error_issue.is_some())
  assert_eq(payment_outbound_error_issue.unwrap().severity, "critical")
  assert_eq(payment_outbound_error_issue.unwrap().value, 20.0)  // 3/15 * 100 = 20%
  
  let payment_outbound_latency_issue = payment_failure_point.issues.find_fn(i) { i.type == "high_outbound_latency" }
  assert_true(payment_outbound_latency_issue.is_some())
  assert_eq(payment_outbound_latency_issue.unwrap().severity, "warning")
  assert_eq(payment_outbound_latency_issue.unwrap().value, 75)
}

// 测试4: 分布式系统容错分析
test "分布式系统容错分析" {
  // 模拟容错事件数据
  let fault_tolerance_events = [
    { 
      timestamp: 1640995200, 
      service: "order-service", 
      event_type: "circuit_breaker_open", 
      target_service: "payment-service", 
      reason: "high_error_rate",
      error_rate: 25.0
    },
    { 
      timestamp: 1640995205, 
      service: "order-service", 
      event_type: "retry_attempt", 
      target_service: "payment-service", 
      attempt: 1,
      result: "failed"
    },
    { 
      timestamp: 1640995210, 
      service: "order-service", 
      event_type: "retry_attempt", 
      target_service: "payment-service", 
      attempt: 2,
      result: "failed"
    },
    { 
      timestamp: 1640995215, 
      service: "order-service", 
      event_type: "fallback_activated", 
      target_service: "payment-service", 
      fallback_type: "cash_payment",
      result: "success"
    },
    { 
      timestamp: 1640995220, 
      service: "api-gateway", 
      event_type: "timeout", 
      target_service: "user-service", 
      timeout_ms: 5000,
      expected_ms: 2000
    },
    { 
      timestamp: 1640995225, 
      service: "user-service", 
      event_type: "bulkhead_isolation", 
      resource: "database_connection_pool",
      active_connections: 10,
      max_connections: 10
    },
    { 
      timestamp: 1640995230, 
      service: "payment-service", 
      event_type: "circuit_breaker_half_open", 
      target_service: "external-payment-gateway",
      test_request_result: "success"
    },
    { 
      timestamp: 1640995235, 
      service: "payment-service", 
      event_type: "circuit_breaker_closed", 
      target_service: "external-payment-gateway",
      recovery_time_ms: 35000
    }
  ]
  
  // 分析容错模式
  let analyze_fault_tolerance_patterns = fn(events: Array[FaultToleranceEvent>) {
    let mut circuit_breaker_events = []
    let mut retry_events = []
    let mut fallback_events = []
    let mut timeout_events = []
    let mut bulkhead_events = []
    
    for event in events {
      match event.event_type {
        "circuit_breaker_open" | "circuit_breaker_half_open" | "circuit_breaker_closed" => {
          circuit_breaker_events = circuit_breaker_events.push(event)
        }
        "retry_attempt" => {
          retry_events = retry_events.push(event)
        }
        "fallback_activated" => {
          fallback_events = fallback_events.push(event)
        }
        "timeout" => {
          timeout_events = timeout_events.push(event)
        }
        "bulkhead_isolation" => {
          bulkhead_events = bulkhead_events.push(event)
        }
        _ => {}
      }
    }
    
    // 分析熔断器模式
    let mut circuit_breaker_analysis = []
    let services_with_circuit_breaker = ["order-service", "payment-service"]
    
    for service in services_with_circuit_breaker {
      let service_events = circuit_breaker_events.filter_fn(e) { e.service == service }
      
      if service_events.length() > 0 {
        let open_events = service_events.filter_fn(e) { e.event_type == "circuit_breaker_open" }
        let half_open_events = service_events.filter_fn(e) { e.event_type == "circuit_breaker_half_open" }
        let closed_events = service_events.filter_fn(e) { e.event_type == "circuit_breaker_closed" }
        
        circuit_breaker_analysis = circuit_breaker_analysis.push({
          service: service,
          open_count: open_events.length(),
          half_open_count: half_open_events.length(),
          closed_count: closed_events.length(),
          total_events: service_events.length()
        })
      }
    }
    
    // 分析重试模式
    let mut retry_analysis = []
    let retry_groups = retry_events.group_by(fn(e) { e.target_service })
    
    for (target_service, group_events) in retry_groups {
      let failed_retries = group_events.filter_fn(e) { e.result == "failed" }
      let successful_retries = group_events.filter_fn(e) { e.result == "success" }
      
      let max_attempts = group_events.map_fn(e) { e.attempt }.max()
      
      retry_analysis = retry_analysis.push({
        target_service: target_service,
        total_retries: group_events.length(),
        failed_retries: failed_retries.length(),
        successful_retries: successful_retries.length(),
        max_attempts: max_attempts,
        success_rate: if group_events.length() > 0 {
          (successful_retries.length().to_float() / group_events.length().to_float()) * 100.0
        } else {
          0.0
        }
      })
    }
    
    // 分析超时模式
    let mut timeout_analysis = []
    
    for timeout_event in timeout_events {
      let timeout_ratio = timeout_event.timeout_ms.to_float() / timeout_event.expected_ms.to_float()
      
      timeout_analysis = timeout_analysis.push({
        service: timeout_event.service,
        target_service: timeout_event.target_service,
        timeout_ms: timeout_event.timeout_ms,
        expected_ms: timeout_event.expected_ms,
        timeout_ratio: timeout_ratio,
        severity: if timeout_ratio > 3.0 { "critical" } else if timeout_ratio > 2.0 { "warning" } else { "info" }
      })
    }
    
    {
      circuit_breaker_analysis: circuit_breaker_analysis,
      retry_analysis: retry_analysis,
      fallback_events_count: fallback_events.length(),
      timeout_analysis: timeout_analysis,
      bulkhead_events_count: bulkhead_events.length()
    }
  }
  
  // 计算容错效果指标
  let calculate_fault_tolerance_effectiveness = fn(events: Array[FaultToleranceEvent>) {
    let mut total_fault_events = 0
    let mut successful_recoveries = 0
    let mut circuit_breaker_activations = 0
    let mut fallback_activations = 0
    
    for event in events {
      match event.event_type {
        "circuit_breaker_open" => {
          total_fault_events = total_fault_events + 1
          circuit_breaker_activations = circuit_breaker_activations + 1
        }
        "retry_attempt" => {
          total_fault_events = total_fault_events + 1
        }
        "fallback_activated" => {
          total_fault_events = total_fault_events + 1
          fallback_activations = fallback_activations + 1
          
          if event.result == "success" {
            successful_recoveries = successful_recoveries + 1
          }
        }
        "circuit_breaker_closed" => {
          successful_recoveries = successful_recoveries + 1
        }
        _ => {}
      }
    }
    
    let recovery_rate = if total_fault_events > 0 {
      (successful_recoveries.to_float() / total_fault_events.to_float()) * 100.0
    } else {
      0.0
    }
    
    // 计算平均恢复时间
    let circuit_breaker_open_events = events.filter_fn(e) { e.event_type == "circuit_breaker_open" }
    let circuit_breaker_closed_events = events.filter_fn(e) { e.event_type == "circuit_breaker_closed" }
    
    let mut recovery_times = []
    
    for open_event in circuit_breaker_open_events {
      let service = open_event.service
      let target = open_event.target_service
      
      // 查找对应的关闭事件
      let closed_event = circuit_breaker_closed_events.find_fn(e) {
        e.service == service and e.target_service == target
      }
      
      if closed_event.is_some() and closed_event.unwrap().has_field("recovery_time_ms") {
        recovery_times = recovery_times.push(closed_event.unwrap().recovery_time_ms)
      }
    }
    
    let avg_recovery_time = if recovery_times.length() > 0 {
      let mut sum = 0
      for time in recovery_times {
        sum = sum + time
      }
      sum / recovery_times.length()
    } else {
      0
    }
    
    {
      total_fault_events: total_fault_events,
      successful_recoveries: successful_recoveries,
      recovery_rate: recovery_rate,
      circuit_breaker_activations: circuit_breaker_activations,
      fallback_activations: fallback_activations,
      avg_recovery_time_ms: avg_recovery_time
    }
  }
  
  // 生成容错改进建议
  let generate_fault_tolerance_recommendations = fn(analysis: FaultToleranceAnalysis, effectiveness: FaultToleranceEffectiveness) {
    let mut recommendations = []
    
    // 熔断器建议
    for cb_analysis in analysis.circuit_breaker_analysis {
      if cb_analysis.open_count > cb_analysis.closed_count {
        recommendations = recommendations.push({
          service: cb_analysis.service,
          issue: "frequent_circuit_breaker_openings",
          recommendation: "Consider increasing error rate threshold or improving downstream service reliability"
        })
      }
    }
    
    // 重试建议
    for retry_analysis in analysis.retry_analysis {
      if retry_analysis.success_rate < 50.0 {
        recommendations = recommendations.push({
          service: retry_analysis.target_service,
          issue: "low_retry_success_rate",
          recommendation: "Implement exponential backoff or reduce max retry attempts"
        })
      }
    }
    
    // 超时建议
    for timeout_analysis in analysis.timeout_analysis {
      if timeout_analysis.severity == "critical" {
        recommendations = recommendations.push({
          service: timeout_analysis.service,
          issue: "excessive_timeout",
          recommendation: "Optimize service performance or adjust timeout values"
        })
      }
    }
    
    // 整体容错效果建议
    if effectiveness.recovery_rate < 70.0 {
      recommendations = recommendations.push({
        service: "system",
        issue: "low_recovery_rate",
        recommendation: "Review and improve fault tolerance mechanisms across services"
      })
    }
    
    if effectiveness.avg_recovery_time > 60000 {  // 超过1分钟
      recommendations = recommendations.push({
        service: "system",
        issue: "slow_recovery",
        recommendation: "Implement faster health checks and proactive monitoring"
      })
    }
    
    recommendations
  }
  
  // 执行容错分析
  let fault_tolerance_analysis = analyze_fault_tolerance_patterns(fault_tolerance_events)
  let fault_tolerance_effectiveness = calculate_fault_tolerance_effectiveness(fault_tolerance_events)
  let recommendations = generate_fault_tolerance_recommendations(fault_tolerance_analysis, fault_tolerance_effectiveness)
  
  // 验证容错模式分析
  assert_eq(fault_tolerance_analysis.circuit_breaker_analysis.length(), 2)
  
  // 验证order-service的熔断器分析
  let order_service_cb = fault_tolerance_analysis.circuit_breaker_analysis.find_fn(a) { a.service == "order-service" }
  assert_true(order_service_cb.is_some())
  assert_eq(order_service_cb.unwrap().open_count, 1)
  assert_eq(order_service_cb.unwrap().half_open_count, 0)
  assert_eq(order_service_cb.unwrap().closed_count, 0)
  assert_eq(order_service_cb.unwrap().total_events, 1)
  
  // 验证payment-service的熔断器分析
  let payment_service_cb = fault_tolerance_analysis.circuit_breaker_analysis.find_fn(a) { a.service == "payment-service" }
  assert_true(payment_service_cb.is_some())
  assert_eq(payment_service_cb.unwrap().open_count, 0)
  assert_eq(payment_service_cb.unwrap().half_open_count, 1)
  assert_eq(payment_service_cb.unwrap().closed_count, 1)
  assert_eq(payment_service_cb.unwrap().total_events, 2)
  
  // 验证重试分析
  assert_eq(fault_tolerance_analysis.retry_analysis.length(), 1)
  
  let payment_retry_analysis = fault_tolerance_analysis.retry_analysis[0]
  assert_eq(payment_retry_analysis.target_service, "payment-service")
  assert_eq(payment_retry_analysis.total_retries, 2)
  assert_eq(payment_retry_analysis.failed_retries, 2)
  assert_eq(payment_retry_analysis.successful_retries, 0)
  assert_eq(payment_retry_analysis.max_attempts, 2)
  assert_eq(payment_retry_analysis.success_rate, 0.0)
  
  // 验证超时分析
  assert_eq(fault_tolerance_analysis.timeout_analysis.length(), 1)
  
  let user_service_timeout = fault_tolerance_analysis.timeout_analysis[0]
  assert_eq(user_service_timeout.service, "api-gateway")
  assert_eq(user_service_timeout.target_service, "user-service")
  assert_eq(user_service_timeout.timeout_ms, 5000)
  assert_eq(user_service_timeout.expected_ms, 2000)
  assert_eq(user_service_timeout.timeout_ratio, 2.5)
  assert_eq(user_service_timeout.severity, "warning")
  
  // 验证其他事件计数
  assert_eq(fault_tolerance_analysis.fallback_events_count, 1)
  assert_eq(fault_tolerance_analysis.bulkhead_events_count, 1)
  
  // 验证容错效果指标
  assert_eq(fault_tolerance_effectiveness.total_fault_events, 6)  // 1 + 2 + 1 + 1 + 1 (不包括恢复事件)
  assert_eq(fault_tolerance_effectiveness.successful_recoveries, 2)  // 1 fallback success + 1 circuit breaker closed
  assert_eq(fault_tolerance_effectiveness.recovery_rate, 33.33)  // 2/6 * 100 = 33.33%
  assert_eq(fault_tolerance_effectiveness.circuit_breaker_activations, 1)
  assert_eq(fault_tolerance_effectiveness.fallback_activations, 1)
  assert_eq(fault_tolerance_effectiveness.avg_recovery_time_ms, 35000)
  
  // 验证容错改进建议
  assert_eq(recommendations.length(), 4)
  
  // 验证重试建议
  let retry_recommendation = recommendations.find_fn(r) { r.issue == "low_retry_success_rate" }
  assert_true(retry_recommendation.is_some())
  assert_eq(retry_recommendation.unwrap().service, "payment-service")
  
  // 验证整体容错效果建议
  let recovery_rate_recommendation = recommendations.find_fn(r) { r.issue == "low_recovery_rate" }
  assert_true(recovery_rate_recommendation.is_some())
  assert_eq(recovery_rate_recommendation.unwrap().service, "system")
}