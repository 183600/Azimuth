// 遥测数据聚合测试用例

test "telemetry_metrics_aggregation" {
  // 测试遥测指标聚合功能
  
  // 聚合配置
  let aggregation_window_seconds = 60
  let max_aggregation_buckets = 1000
  let aggregation_flush_interval = 30
  
  // 验证聚合配置
  assert_eq(aggregation_window_seconds > 0, true)
  assert_eq(max_aggregation_buckets > 0, true)
  assert_eq(aggregation_flush_interval > 0, true)
  
  // 指标数据结构
  type MetricPoint = {
    metric_name: String,
    metric_value: Double,
    metric_type: String,  // counter, gauge, histogram
    timestamp: Int,
    tags: Map[String, String]
  }
  
  // 聚合桶结构
  type AggregationBucket = {
    window_start: Int,
    window_end: Int,
    metrics: Map[String, MetricAggregate],
    count: Int
  }
  
  // 聚合结果结构
  type MetricAggregate = {
    sum: Double,
    count: Int,
    min: Double,
    max: Double,
    avg: Double,
    last_value: Double
  }
  
  // 创建测试指标数据
  let metric_points = [
    // 计数器指标
    MetricPoint {
      metric_name: "http_requests_total",
      metric_value: 1.0,
      metric_type: "counter",
      timestamp: 1640995200,
      tags: { "method": "GET", "status": "200", "service": "api-gateway" }
    },
    MetricPoint {
      metric_name: "http_requests_total",
      metric_value: 1.0,
      metric_type: "counter",
      timestamp: 1640995210,
      tags: { "method": "GET", "status": "200", "service": "api-gateway" }
    },
    MetricPoint {
      metric_name: "http_requests_total",
      metric_value: 1.0,
      metric_type: "counter",
      timestamp: 1640995220,
      tags: { "method": "POST", "status": "201", "service": "api-gateway" }
    },
    // 仪表盘指标
    MetricPoint {
      metric_name: "memory_usage_bytes",
      metric_value: 1024.0 * 1024.0 * 100.0,  // 100MB
      metric_type: "gauge",
      timestamp: 1640995200,
      tags: { "service": "user-service", "instance": "pod-1" }
    },
    MetricPoint {
      metric_name: "memory_usage_bytes",
      metric_value: 1024.0 * 1024.0 * 120.0,  // 120MB
      metric_type: "gauge",
      timestamp: 1640995230,
      tags: { "service": "user-service", "instance": "pod-1" }
    },
    MetricPoint {
      metric_name: "memory_usage_bytes",
      metric_value: 1024.0 * 1024.0 * 90.0,   // 90MB
      metric_type: "gauge",
      timestamp: 1640995260,
      tags: { "service": "user-service", "instance": "pod-1" }
    },
    // 直方图指标
    MetricPoint {
      metric_name: "request_duration_seconds",
      metric_value: 0.1,
      metric_type: "histogram",
      timestamp: 1640995200,
      tags: { "endpoint": "/api/users", "service": "user-service" }
    },
    MetricPoint {
      metric_name: "request_duration_seconds",
      metric_value: 0.2,
      metric_type: "histogram",
      timestamp: 1640995210,
      tags: { "endpoint": "/api/users", "service": "user-service" }
    },
    MetricPoint {
      metric_name: "request_duration_seconds",
      metric_value: 0.15,
      metric_type: "histogram",
      timestamp: 1640995220,
      tags: { "endpoint": "/api/users", "service": "user-service" }
    },
    MetricPoint {
      metric_name: "request_duration_seconds",
      metric_value: 0.3,
      metric_type: "histogram",
      timestamp: 1640995230,
      tags: { "endpoint": "/api/orders", "service": "order-service" }
    }
  ]
  
  // 验证测试数据
  assert_eq(metric_points.length(), 10)
  assert_eq(metric_points[0].metric_name, "http_requests_total")
  assert_eq(metric_points[9].metric_type, "histogram")
  
  // 聚合处理
  let mut aggregation_buckets = []
  let mut current_bucket = AggregationBucket {
    window_start: 1640995200,
    window_end: 1640995200 + aggregation_window_seconds,
    metrics: {},
    count: 0
  }
  
  // 处理每个指标点
  let mut i = 0
  while i < metric_points.length() {
    let point = metric_points[i]
    
    // 检查是否需要新的聚合桶
    if point.timestamp >= current_bucket.window_end {
      // 保存当前桶
      aggregation_buckets.push(current_bucket)
      
      // 创建新桶
      current_bucket = AggregationBucket {
        window_start: point.timestamp,
        window_end: point.timestamp + aggregation_window_seconds,
        metrics: {},
        count: 0
      }
    }
    
    // 生成指标键
    let metric_key = point.metric_name + "|" + 
                    point.tags["service"] + "|" +
                    point.tags.get("method").or_else("") + "|" +
                    point.tags.get("status").or_else("")
    
    // 获取或创建聚合数据
    let aggregate = current_bucket.metrics.get(metric_key).or_else(
      MetricAggregate {
        sum: 0.0,
        count: 0,
        min: point.metric_value,
        max: point.metric_value,
        avg: 0.0,
        last_value: point.metric_value
      }
    )
    
    // 更新聚合数据
    let new_sum = aggregate.sum + point.metric_value
    let new_count = aggregate.count + 1
    let new_min = min(aggregate.min, point.metric_value)
    let new_max = max(aggregate.max, point.metric_value)
    let new_avg = new_sum / new_count.to_double()
    
    let updated_aggregate = MetricAggregate {
      sum: new_sum,
      count: new_count,
      min: new_min,
      max: new_max,
      avg: new_avg,
      last_value: point.metric_value
    }
    
    // 更新桶中的聚合数据
    current_bucket.metrics[metric_key] = updated_aggregate
    current_bucket.count = current_bucket.count + 1
    
    i = i + 1
  }
  
  // 添加最后一个桶
  aggregation_buckets.push(current_bucket)
  
  // 验证聚合结果
  assert_eq(aggregation_buckets.length(), 1)  // 所有数据在同一个时间窗口内
  assert_eq(aggregation_buckets[0].count, 10)
  
  // 验证HTTP请求聚合
  let http_requests_key = "http_requests_total|api-gateway|GET|200"
  let http_aggregate = aggregation_buckets[0].metrics[http_requests_key]
  assert_eq(http_aggregate.count, 2)
  assert_eq(http_aggregate.sum, 2.0)
  assert_eq(http_aggregate.avg, 1.0)
  
  // 验证内存使用聚合
  let memory_key = "memory_usage_bytes|user-service||"
  let memory_aggregate = aggregation_buckets[0].metrics[memory_key]
  assert_eq(memory_aggregate.count, 3)
  assert_eq(memory_aggregate.min, 1024.0 * 1024.0 * 90.0)
  assert_eq(memory_aggregate.max, 1024.0 * 1024.0 * 120.0)
  
  // 验证请求延迟聚合
  let duration_key = "request_duration_seconds|user-service||"
  let duration_aggregate = aggregation_buckets[0].metrics[duration_key]
  assert_eq(duration_aggregate.count, 3)
  assert_eq(duration_aggregate.min, 0.1)
  assert_eq(duration_aggregate.max, 0.2)
  assert_eq(duration_aggregate.avg > 0.14, true)
  assert_eq(duration_aggregate.avg < 0.16, true)
}

test "telemetry_log_aggregation" {
  // 测试遥测日志聚合功能
  
  // 日志聚合配置
  let log_aggregation_window = 300  // 5分钟窗口
  let max_log_entries_per_window = 10000
  let log_level_priorities = {
    "ERROR": 4,
    "WARN": 3,
    "INFO": 2,
    "DEBUG": 1
  }
  
  // 验证配置
  assert_eq(log_aggregation_window > 0, true)
  assert_eq(max_log_entries_per_window > 0, true)
  assert_eq(log_level_priorities["ERROR"] > log_level_priorities["DEBUG"], true)
  
  // 日志条目结构
  type LogEntry = {
    timestamp: Int,
    level: String,
    message: String,
    service: String,
    trace_id: String,
    span_id: String,
    attributes: Map[String, String]
  }
  
  // 日志聚合统计
  type LogAggregate = {
    total_count: Int,
    error_count: Int,
    warn_count: Int,
    info_count: Int,
    debug_count: Int,
    unique_services: Set[String],
    error_patterns: Map[String, Int]
  }
  
  // 创建测试日志数据
  let log_entries = [
    LogEntry {
      timestamp: 1640995200,
      level: "INFO",
      message: "Service started successfully",
      service: "user-service",
      trace_id: "trace-001",
      span_id: "span-001",
      attributes: { "version": "1.0.0", "port": "8080" }
    },
    LogEntry {
      timestamp: 1640995210,
      level: "INFO",
      message: "Database connection established",
      service: "user-service",
      trace_id: "trace-001",
      span_id: "span-002",
      attributes: { "db_host": "localhost", "db_port": "5432" }
    },
    LogEntry {
      timestamp: 1640995220,
      level: "WARN",
      message: "High memory usage detected",
      service: "user-service",
      trace_id: "trace-002",
      span_id: "span-003",
      attributes: { "memory_usage": "85%", "threshold": "80%" }
    },
    LogEntry {
      timestamp: 1640995230,
      level: "ERROR",
      message: "Database connection failed",
      service: "order-service",
      trace_id: "trace-003",
      span_id: "span-004",
      attributes: { "error_code": "DB_CONN_FAILED", "retry_count": "3" }
    },
    LogEntry {
      timestamp: 1640995240,
      level: "ERROR",
      message: "Database connection failed",
      service: "order-service",
      trace_id: "trace-004",
      span_id: "span-005",
      attributes: { "error_code": "DB_CONN_FAILED", "retry_count": "3" }
    },
    LogEntry {
      timestamp: 1640995250,
      level: "INFO",
      message: "Request processed",
      service: "api-gateway",
      trace_id: "trace-005",
      span_id: "span-006",
      attributes: { "method": "GET", "path": "/api/users", "status": "200" }
    },
    LogEntry {
      timestamp: 1640995260,
      level: "DEBUG",
      message: "Cache hit for user data",
      service: "api-gateway",
      trace_id: "trace-005",
      span_id: "span-007",
      attributes: { "cache_key": "user_123", "ttl": "300" }
    },
    LogEntry {
      timestamp: 1640995270,
      level: "WARN",
      message: "Rate limit approaching",
      service: "api-gateway",
      trace_id: "trace-006",
      span_id: "span-008",
      attributes: { "current_rate": "95", "limit": "100" }
    }
  ]
  
  // 验证测试数据
  assert_eq(log_entries.length(), 8)
  assert_eq(log_entries[3].level, "ERROR")
  assert_eq(log_entries[6].level, "DEBUG")
  
  // 日志聚合处理
  let mut log_aggregate = LogAggregate {
    total_count: 0,
    error_count: 0,
    warn_count: 0,
    info_count: 0,
    debug_count: 0,
    unique_services: {},
    error_patterns: {}
  }
  
  // 处理每个日志条目
  let mut i = 0
  while i < log_entries.length() {
    let entry = log_entries[i]
    
    // 更新总数
    log_aggregate.total_count = log_aggregate.total_count + 1
    
    // 按级别统计
    match entry.level {
      "ERROR" => log_aggregate.error_count = log_aggregate.error_count + 1
      "WARN" => log_aggregate.warn_count = log_aggregate.warn_count + 1
      "INFO" => log_aggregate.info_count = log_aggregate.info_count + 1
      "DEBUG" => log_aggregate.debug_count = log_aggregate.debug_count + 1
      _ => ()
    }
    
    // 统计唯一服务
    log_aggregate.unique_services.add(entry.service)
    
    // 统计错误模式
    if entry.level == "ERROR" {
      let pattern = entry.message + "|" + entry.attributes.get("error_code").or_else("")
      let current_count = log_aggregate.error_patterns.get(pattern).or_else(0)
      log_aggregate.error_patterns[pattern] = current_count + 1
    }
    
    i = i + 1
  }
  
  // 验证聚合结果
  assert_eq(log_aggregate.total_count, 8)
  assert_eq(log_aggregate.error_count, 2)
  assert_eq(log_aggregate.warn_count, 2)
  assert_eq(log_aggregate.info_count, 3)
  assert_eq(log_aggregate.debug_count, 1)
  
  // 验证服务统计
  assert_eq(log_aggregate.unique_services.contains("user-service"), true)
  assert_eq(log_aggregate.unique_services.contains("order-service"), true)
  assert_eq(log_aggregate.unique_services.contains("api-gateway"), true)
  assert_eq(log_aggregate.unique_services.size(), 3)
  
  // 验证错误模式统计
  let db_error_pattern = "Database connection failed|DB_CONN_FAILED"
  assert_eq(log_aggregate.error_patterns.contains(db_error_pattern), true)
  assert_eq(log_aggregate.error_patterns[db_error_pattern], 2)
  
  // 验证日志级别比例
  let error_rate = log_aggregate.error_count.to_double() / log_aggregate.total_count.to_double()
  assert_eq(error_rate, 0.25)  // 2/8 = 25%
  
  let warn_rate = log_aggregate.warn_count.to_double() / log_aggregate.total_count.to_double()
  assert_eq(warn_rate, 0.25)  // 2/8 = 25%
  
  let info_rate = log_aggregate.info_count.to_double() / log_aggregate.total_count.to_double()
  assert_eq(info_rate > 0.37, true)  // 3/8 ≈ 37.5%
  assert_eq(info_rate < 0.38, true)
}

test "telemetry_trace_aggregation" {
  // 测试遥测跟踪聚合功能
  
  // 跟踪聚合配置
  let trace_aggregation_timeout = 300  // 5分钟超时
  let max_trace_spans = 1000
  let slow_operation_threshold = 1.0  // 1秒
  
  // 验证配置
  assert_eq(trace_aggregation_timeout > 0, true)
  assert_eq(max_trace_spans > 0, true)
  assert_eq(slow_operation_threshold > 0, true)
  
  // 跟踪span结构
  type TraceSpan = {
    trace_id: String,
    span_id: String,
    parent_span_id: String,
    operation_name: String,
    start_time: Int,
    end_time: Int,
    duration_ms: Int,
    service: String,
    status: String,
    tags: Map[String, String]
  }
  
  // 跟踪聚合统计
  type TraceAggregate = {
    trace_id: String,
    total_spans: Int,
    total_duration_ms: Int,
    service_breakdown: Map[String, Int],
    slow_operations: Int,
    error_operations: Int,
    span_depth: Int
  }
  
  // 创建测试跟踪数据
  let trace_spans = [
    // 根span
    TraceSpan {
      trace_id: "trace-001",
      span_id: "span-root",
      parent_span_id: "",
      operation_name: "HTTP GET /api/orders",
      start_time: 1640995200000,  // 毫秒时间戳
      end_time: 1640995200500,
      duration_ms: 500,
      service: "api-gateway",
      status: "ok",
      tags: { "http.method": "GET", "http.status_code": "200" }
    },
    // 子span 1
    TraceSpan {
      trace_id: "trace-001",
      span_id: "span-001",
      parent_span_id: "span-root",
      operation_name: "authenticate_user",
      start_time: 1640995200100,
      end_time: 1640995200200,
      duration_ms: 100,
      service: "auth-service",
      status: "ok",
      tags: { "user_id": "12345", "auth_method": "jwt" }
    },
    // 子span 2
    TraceSpan {
      trace_id: "trace-001",
      span_id: "span-002",
      parent_span_id: "span-root",
      operation_name: "fetch_orders",
      start_time: 1640995200200,
      end_time: 1640995200400,
      duration_ms: 200,
      service: "order-service",
      status: "ok",
      tags: { "query": "limit=10", "result_count": "5" }
    },
    // 子span 3 (嵌套)
    TraceSpan {
      trace_id: "trace-001",
      span_id: "span-003",
      parent_span_id: "span-002",
      operation_name: "database_query",
      start_time: 1640995200250,
      end_time: 1640995200350,
      duration_ms: 100,
      service: "order-service",
      status: "ok",
      tags: { "table": "orders", "operation": "SELECT" }
    },
    // 慢操作span
    TraceSpan {
      trace_id: "trace-001",
      span_id: "span-004",
      parent_span_id: "span-002",
      operation_name: "external_api_call",
      start_time: 1640995200350,
      end_time: 1640995200450,
      duration_ms: 1200,  // 超过慢操作阈值
      service: "order-service",
      status: "ok",
      tags: { "api_endpoint": "https://payment-service.com", "timeout": "2000" }
    },
    // 错误span
    TraceSpan {
      trace_id: "trace-001",
      span_id: "span-005",
      parent_span_id: "span-root",
      operation_name: "log_request",
      start_time: 1640995200450,
      end_time: 1640995200480,
      duration_ms: 30,
      service: "logging-service",
      status: "error",
      tags: { "error_code": "LOG_WRITE_FAILED", "retry_count": "2" }
    }
  ]
  
  // 验证测试数据
  assert_eq(trace_spans.length(), 6)
  assert_eq(trace_spans[0].parent_span_id, "")
  assert_eq(trace_spans[4].duration_ms, 1200)
  assert_eq(trace_spans[5].status, "error")
  
  // 跟踪聚合处理
  let mut trace_aggregates = {}
  let current_trace_id = "trace-001"
  
  // 初始化跟踪聚合
  trace_aggregates[current_trace_id] = TraceAggregate {
    trace_id: current_trace_id,
    total_spans: 0,
    total_duration_ms: 0,
    service_breakdown: {},
    slow_operations: 0,
    error_operations: 0,
    span_depth: 0
  }
  
  // 处理每个span
  let mut i = 0
  while i < trace_spans.length() {
    let span = trace_spans[i]
    let aggregate = trace_aggregates[span.trace_id]
    
    // 更新基本统计
    let new_total_spans = aggregate.total_spans + 1
    let new_total_duration = aggregate.total_duration_ms + span.duration_ms
    
    // 更新服务分解
    let service_count = aggregate.service_breakdown.get(span.service).or_else(0)
    let new_service_breakdown = aggregate.service_breakdown
    new_service_breakdown[span.service] = service_count + 1
    
    // 更新慢操作统计
    let new_slow_operations = if span.duration_ms > (slow_operation_threshold * 1000.0).to_int() {
      aggregate.slow_operations + 1
    } else {
      aggregate.slow_operations
    }
    
    // 更新错误操作统计
    let new_error_operations = if span.status == "error" {
      aggregate.error_operations + 1
    } else {
      aggregate.error_operations
    }
    
    // 计算span深度（简化版）
    let mut span_depth = 0
    let mut j = 0
    while j < trace_spans.length() {
      if trace_spans[j].span_id == span.parent_span_id {
        span_depth = span_depth + 1
      }
      j = j + 1
    }
    let new_span_depth = max(aggregate.span_depth, span_depth)
    
    // 更新聚合数据
    trace_aggregates[span.trace_id] = TraceAggregate {
      trace_id: span.trace_id,
      total_spans: new_total_spans,
      total_duration_ms: new_total_duration,
      service_breakdown: new_service_breakdown,
      slow_operations: new_slow_operations,
      error_operations: new_error_operations,
      span_depth: new_span_depth
    }
    
    i = i + 1
  }
  
  // 验证聚合结果
  let aggregate = trace_aggregates[current_trace_id]
  assert_eq(aggregate.total_spans, 6)
  assert_eq(aggregate.total_duration_ms, 2130)  // 500+100+200+100+1200+30
  assert_eq(aggregate.slow_operations, 1)
  assert_eq(aggregate.error_operations, 1)
  
  // 验证服务分解
  assert_eq(aggregate.service_breakdown["api-gateway"], 1)
  assert_eq(aggregate.service_breakdown["auth-service"], 1)
  assert_eq(aggregate.service_breakdown["order-service"], 3)
  assert_eq(aggregate.service_breakdown["logging-service"], 1)
  
  // 验证性能指标
  let avg_span_duration = aggregate.total_duration_ms.to_double() / aggregate.total_spans.to_double()
  assert_eq(avg_span_duration > 355.0, true)  // 2130/6 ≈ 355
  assert_eq(avg_span_duration < 356.0, true)
  
  // 验证错误率
  let error_rate = aggregate.error_operations.to_double() / aggregate.total_spans.to_double()
  assert_eq(error_rate, 0.16666666666666666)  // 1/6 ≈ 16.67%
  
  // 验证慢操作率
  let slow_operation_rate = aggregate.slow_operations.to_double() / aggregate.total_spans.to_double()
  assert_eq(slow_operation_rate, 0.16666666666666666)  // 1/6 ≈ 16.67%
}