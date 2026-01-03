// Azimuth 并发安全测试用例
// 测试并发环境下的线程安全和数据一致性

test "并发指标收集安全性" {
  // 创建并发安全的指标收集器
  let metrics_collector = @azimuth.ConcurrentMetricsCollector {
    metrics: @azimuth.ConcurrentMap.new(),
    counters: @azimuth.AtomicCounter.new(0),
    gauges: @azimuth.AtomicReference.new(@azimuth.Map.new()),
    histograms: @azimuth.AtomicReference.new(@azimuth.Map.new()),
    lock: @azimuth.Mutex.new()
  }
  
  // 模拟多个线程并发更新指标
  let thread_count = 10
  let operations_per_thread = 100
  
  // 线程1: 更新计数器
  let counter_updates = (0..thread_count).map(fn(thread_id) {
    (0..operations_per_thread).map(fn(op_id) {
      @azimuth.CounterUpdate {
        name: "requests.total",
        value: 1,
        attributes: @azimuth.Attributes {
          values: [
            ("thread_id", @azimuth.StringValue(thread_id.to_string())),
            ("operation", @azimuth.StringValue("update"))
          ]
        },
        timestamp: @azimuth.Timestamp.now()
      }
    })
  }).flatten()
  
  // 线程2: 更新仪表盘
  let gauge_updates = (0..thread_count).map(fn(thread_id) {
    (0..operations_per_thread).map(fn(op_id) {
      @azimuth.GaugeUpdate {
        name: "memory.usage",
        value: thread_id.to_float() * 10.0 + op_id.to_float(),
        attributes: @azimuth.Attributes {
          values: [
            ("thread_id", @azimuth.StringValue(thread_id.to_string())),
            ("operation", @azimuth.StringValue("gauge"))
          ]
        },
        timestamp: @azimuth.Timestamp.now()
      }
    })
  }).flatten()
  
  // 线程3: 更新直方图
  let histogram_updates = (0..thread_count).map(fn(thread_id) {
    (0..operations_per_thread).map(fn(op_id) {
      @azimuth.HistogramUpdate {
        name: "response.time",
        value: (thread_id * 100 + op_id).to_float(),
        attributes: @azimuth.Attributes {
          values: [
            ("thread_id", @azimuth.StringValue(thread_id.to_string())),
            ("operation", @azimuth.StringValue("histogram"))
          ]
        },
        timestamp: @azimuth.Timestamp.now()
      }
    })
  }).flatten()
  
  // 验证更新数量
  assert_eq(counter_updates.length(), thread_count * operations_per_thread)
  assert_eq(gauge_updates.length(), thread_count * operations_per_thread)
  assert_eq(histogram_updates.length(), thread_count * operations_per_thread)
  
  // 模拟并发处理计数器更新
  let total_counter_value = counter_updates.reduce(fn(acc, update) {
    acc + update.value
  }, 0)
  assert_eq(total_counter_value, thread_count * operations_per_thread)
  
  // 模拟并发处理仪表盘更新（最后一个值应该保留）
  let gauge_groups = gauge_updates.group_by(fn(update) { update.name })
  let final_gauge_values = gauge_groups.map_fn((name, updates) => {
    let last_update = updates[updates.length() - 1]
    (name, last_update.value)
  })
  
  // 验证仪表盘最终值
  match final_gauge_values.find(fn(pair) { pair.0 == "memory.usage" }) {
    Some((_, value)) => {
      // 最后一个更新应该来自最后一个线程的最后一个操作
      let expected_value = (thread_count - 1).to_float() * 10.0 + (operations_per_thread - 1).to_float()
      assert_eq(value, expected_value)
    }
    None => assert_true(false)
  }
  
  // 模拟并发处理直方图更新
  let histogram_groups = histogram_updates.group_by(fn(update) { update.name })
  let histogram_data = histogram_groups.map_fn((name, updates) => {
    let values = updates.map(fn(update) { update.value })
    let count = values.length()
    let sum = values.reduce(fn(acc, value) { acc + value }, 0.0)
    let avg = sum / count.to_float()
    
    // 计算分位数
    let sorted_values = values.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
    let p50 = sorted_values[sorted_values.length() / 2]
    let p95 = sorted_values[(sorted_values.length() * 95 / 100).min(sorted_values.length() - 1)]
    let p99 = sorted_values[(sorted_values.length() * 99 / 100).min(sorted_values.length() - 1)]
    
    (name, @azimuth.HistogramData {
      count: count,
      sum: sum,
      avg: avg,
      p50: p50,
      p95: p95,
      p99: p99
    })
  })
  
  // 验证直方图统计
  match histogram_data.find_fn((name, _) { name == "response.time" }) {
    Some((_, data)) => {
      assert_eq(data.count, thread_count * operations_per_thread)
      assert_eq(data.avg, (thread_count * operations_per_thread - 1).to_float() / 2.0) // 平均值应该是中间值
      assert_eq(data.p50, (thread_count * operations_per_thread / 2).to_float())
    }
    None => assert_true(false)
  }
}

test "并发追踪上下文传播安全性" {
  // 创建线程安全的追踪上下文管理器
  let context_manager = @azimuth.ConcurrentContextManager {
    active_contexts: @azimuth.ConcurrentMap.new(),
    context_propagator: @azimuth.ThreadSafeContextPropagator.new(),
    lock: @azimuth.RwLock.new()
  }
  
  // 创建根追踪上下文
  let root_context = @azimuth.TraceContext {
    trace_id: @azimuth.TraceId("1234567890abcdef1234567890abcdef"),
    span_id: @azimuth.SpanId("1111111111111111"),
    sampled: true,
    baggage: @azimuth.Baggage {
      entries: [
        ("user.id", "user-123"),
        ("request.id", "req-456"),
        ("session.id", "sess-789")
      ]
    }
  }
  
  // 模拟并发创建子Span
  let concurrent_spans = (0..8).map(fn(thread_id) {
    let child_span_id = @azimuth.SpanId((2000000000000000 + thread_id).to_string())
    @azimuth.Span {
      context: @azimuth.TraceContext {
        trace_id: root_context.trace_id,
        span_id: child_span_id,
        sampled: root_context.sampleed,
        baggage: root_context.baggage
      },
      parent_span_id: Some(root_context.span_id),
      name: "concurrent.operation." + thread_id.to_string(),
      kind: @azimuth.SpanKind.Internal,
      start_time: @azimuth.Timestamp.now(),
      end_time: Some(@azimuth.Timestamp.now() + 1000),
      status: @azimuth.SpanStatus.Ok,
      attributes: @azimuth.Attributes {
        values: [
          ("thread.id", @azimuth.StringValue(thread_id.to_string())),
          ("worker.type", @azimuth.StringValue("concurrent"))
        ]
      },
      events: [],
      links: []
    }
  })
  
  // 验证并发Span创建
  assert_eq(concurrent_spans.length(), 8)
  
  // 验证所有Span共享相同的TraceId
  for span in concurrent_spans {
    assert_eq(span.context.trace_id, root_context.trace_id)
    assert_eq(span.context.sampled, root_context.sampleed)
    assert_eq(span.context.baggage.entries.length(), root_context.baggage.entries.length())
  }
  
  // 验证每个Span都有唯一的SpanId
  let span_ids = concurrent_spans.map(fn(span) { span.context.span_id })
  let unique_span_ids = span_ids.unique()
  assert_eq(unique_span_ids.length(), span_ids.length())
  
  // 验证父子关系
  for span in concurrent_spans {
    match span.parent_span_id {
      Some(id) => assert_eq(id, root_context.span_id)
      None => assert_true(false)
    }
  }
  
  // 模拟并发Baggage更新
  let updated_spans = concurrent_spans.map_fn(span => {
    let updated_baggage = @azimuth.Baggage {
      entries: span.context.baggage.entries + [
        ("thread.local.data", "value-" + span.attributes.values[0].1.to_string())
      ]
    }
    
    @azimuth.Span {
      context: @azimuth.TraceContext {
        trace_id: span.context.trace_id,
        span_id: span.context.span_id,
        sampled: span.context.sampleed,
        baggage: updated_baggage
      },
      parent_span_id: span.parent_span_id,
      name: span.name,
      kind: span.kind,
      start_time: span.start_time,
      end_time: span.end_time,
      status: span.status,
      attributes: span.attributes,
      events: span.events,
      links: span.links
    }
  })
  
  // 验证Baggage更新
  for span in updated_spans {
    assert_eq(span.context.baggage.entries.length(), 4) // 原来的3个 + 新增的1个
    
    // 验证原有的Baggage项仍然存在
    let user_id = span.context.baggage.entries.find_fn(entry => entry.0 == "user.id")
    let request_id = span.context.baggage.entries.find_fn(entry => entry.0 == "request.id")
    let session_id = span.context.baggage.entries.find_fn(entry => entry.0 == "session.id")
    let thread_data = span.context.baggage.entries.find_fn(entry => entry.0 == "thread.local.data")
    
    assert_true(user_id.is_some())
    assert_true(request_id.is_some())
    assert_true(session_id.is_some())
    assert_true(thread_data.is_some())
  }
  
  // 验证每个Span都有唯一的线程本地数据
  let thread_data_values = updated_spans.map(fn(span) {
    match span.context.baggage.entries.find_fn(entry => entry.0 == "thread.local.data") {
      Some((_, value)) => value
      None => ""
    }
  })
  let unique_thread_data = thread_data_values.unique()
  assert_eq(unique_thread_data.length(), thread_data_values.length())
}

test "并发日志记录安全性" {
  // 创建线程安全的日志记录器
  let logger = @azimuth.ConcurrentLogger {
    buffer: @azimuth.ConcurrentRingBuffer.new(10000),
    writers: @azimuth.AtomicReference.new(@azimuth.List.new()),
    min_level: @azimuth.LogSeverity.Info,
    lock: @azimuth.Mutex.new()
  }
  
  // 模拟并发日志记录
  let thread_count = 5
  let logs_per_thread = 200
  
  let concurrent_logs = (0..thread_count).map(fn(thread_id) {
    (0..logs_per_thread).map(fn(log_id) {
      let severity = match log_id % 5 {
        0 => @azimuth.LogSeverity.Trace
        1 => @azimuth.LogSeverity.Debug
        2 => @azimuth.LogSeverity.Info
        3 => @azimuth.LogSeverity.Warn
        4 => @azimuth.LogSeverity.Error
        _ => @azimuth.LogSeverity.Info
      }
      
      @azimuth.LogRecord {
        timestamp: @azimuth.Timestamp.now(),
        severity: severity,
        body: @azimuth.StringValue("Log message from thread " + thread_id.to_string() + " #" + log_id.to_string()),
        resource: @azimuth.Resource {
          attributes: @azimuth.Attributes {
            values: [
              ("service.name", @azimuth.StringValue("azimuth-service")),
              ("service.version", @azimuth.StringValue("1.0.0")),
              ("thread.id", @azimuth.StringValue(thread_id.to_string()))
            ]
          }
        },
        instrumentation_scope: @azimuth.InstrumentationScope {
          name: "azimuth.concurrent.logger",
          version: "1.0.0"
        },
        attributes: @azimuth.Attributes {
          values: [
            ("log.id", @azimuth.StringValue((thread_id * logs_per_thread + log_id).to_string())),
            ("thread.pool", @azimuth.StringValue("worker"))
          ]
        },
        trace_id: None,
        span_id: None,
        flags: 0
      }
    })
  }).flatten()
  
  // 验证日志数量
  assert_eq(concurrent_logs.length(), thread_count * logs_per_thread)
  
  // 按严重性分组日志
  let logs_by_severity = concurrent_logs.group_by(fn(log) { log.severity })
  
  // 验证每种严重性的日志数量
  let trace_logs = logs_by_severity.get(@azimuth.LogSeverity.Trace).unwrap_or(@azimuth.List.new())
  let debug_logs = logs_by_severity.get(@azimuth.LogSeverity.Debug).unwrap_or(@azimuth.List.new())
  let info_logs = logs_by_severity.get(@azimuth.LogSeverity.Info).unwrap_or(@azimuth.List.new())
  let warn_logs = logs_by_severity.get(@azimuth.LogSeverity.Warn).unwrap_or(@azimuth.List.new())
  let error_logs = logs_by_severity.get(@azimuth.LogSeverity.Error).unwrap_or(@azimuth.List.new())
  
  // 每种严重性应该有大约相同数量的日志
  assert_eq(trace_logs.length(), thread_count * logs_per_thread / 5)
  assert_eq(debug_logs.length(), thread_count * logs_per_thread / 5)
  assert_eq(info_logs.length(), thread_count * logs_per_thread / 5)
  assert_eq(warn_logs.length(), thread_count * logs_per_thread / 5)
  assert_eq(error_logs.length(), thread_count * logs_per_thread / 5)
  
  // 按线程ID分组日志
  let logs_by_thread = concurrent_logs.group_by(fn(log) {
    match log.resource.attributes.values.find_fn(attr => attr.0 == "thread.id") {
      Some((_, @azimuth.StringValue(thread_id))) => thread_id
      _ => "unknown"
    }
  })
  
  // 验证每个线程的日志数量
  for thread_id in (0..thread_count) {
    let thread_logs = logs_by_thread.get(thread_id.to_string()).unwrap_or(@azimuth.List.new())
    assert_eq(thread_logs.length(), logs_per_thread)
  }
  
  // 验证日志ID的唯一性
  let log_ids = concurrent_logs.map(fn(log) {
    match log.attributes.values.find_fn(attr => attr.0 == "log.id") {
      Some((_, @azimuth.StringValue(log_id))) => log_id
      _ => ""
    }
  })
  let unique_log_ids = log_ids.unique()
  assert_eq(unique_log_ids.length(), log_ids.length())
  
  // 模拟并发日志处理（按时间戳排序）
  let sorted_logs = concurrent_logs.sort(fn(a, b) {
    if a.timestamp < b.timestamp { -1 } 
    else if a.timestamp > b.timestamp { 1 } 
    else { 0 }
  })
  
  // 验证排序结果
  for i in (1..sorted_logs.length()) {
    assert_true(sorted_logs[i-1].timestamp <= sorted_logs[i].timestamp)
  }
  
  // 模拟并发日志过滤（按最小级别）
  let filtered_logs = concurrent_logs.filter(fn(log) {
    match log.severity {
      @azimuth.LogSeverity.Trace => false
      @azimuth.LogSeverity.Debug => false
      @azimuth.LogSeverity.Info => true
      @azimuth.LogSeverity.Warn => true
      @azimuth.LogSeverity.Error => true
      @azimuth.LogSeverity.Fatal => true
    }
  })
  
  // 验证过滤结果（应该排除Trace和Debug级别的日志）
  assert_eq(filtered_logs.length(), thread_count * logs_per_thread * 3 / 5)
}

test "并发资源管理安全性" {
  // 创建线程安全的资源池
  let resource_pool = @azimuth.ConcurrentResourcePool {
    resources: @azimuth.ConcurrentQueue.new(),
    allocated_resources: @azimuth.AtomicCounter.new(0),
    total_resources: @azimuth.AtomicCounter.new(0),
    max_resources: 20,
    min_resources: 5,
    condition: @azimuth.ConditionVariable.new(),
    mutex: @azimuth.Mutex.new()
  }
  
  // 初始化资源池
  let initial_resources = (0..10).map(fn(i) {
    @azimuth.PooledResource {
      id: "resource-" + i.to_string(),
      type: "database.connection",
      created_at: @azimuth.Timestamp.now(),
      last_used: @azimuth.Timestamp.now(),
      in_use: false,
      use_count: 0
    }
  })
  
  // 模拟并发资源分配
  let thread_count = 15
  let allocation_requests = (0..thread_count).map(fn(thread_id) {
    @azimuth.ResourceAllocationRequest {
      request_id: "req-" + thread_id.to_string(),
      resource_type: "database.connection",
      timeout_ms: 5000,
      thread_id: thread_id,
      timestamp: @azimuth.Timestamp.now()
    }
  })
  
  // 模拟资源分配过程
  let mut allocation_results = @azimuth.List.new()
  let mut available_resources = initial_resources
  
  for request in allocation_requests {
    if available_resources.length() > 0 {
      // 分配资源
      let resource = available_resources[0]
      available_resources = available_resources.slice(1, available_resources.length())
      
      let allocated_resource = @azimuth.PooledResource {
        id: resource.id,
        type: resource.type,
        created_at: resource.created_at,
        last_used: request.timestamp,
        in_use: true,
        use_count: resource.use_count + 1
      }
      
      allocation_results = allocation_results + [@azimuth.ResourceAllocationResult {
        request_id: request.request_id,
        success: true,
        resource: Some(allocated_resource),
        wait_time_ms: 0,
        timestamp: request.timestamp
      }]
    } else {
      // 资源不足
      allocation_results = allocation_results + [@azimuth.ResourceAllocationResult {
        request_id: request.request_id,
        success: false,
        resource: None,
        wait_time_ms: request.timeout_ms,
        timestamp: request.timestamp
      }]
    }
  }
  
  // 验证分配结果
  let successful_allocations = allocation_results.filter(fn(result) { result.success })
  let failed_allocations = allocation_results.filter(fn(result) { !result.success })
  
  assert_eq(successful_allocations.length(), 10) // 只有10个资源可用
  assert_eq(failed_allocations.length(), 5) // 5个请求失败
  
  // 验证分配的资源
  for allocation in successful_allocations {
    assert_true(allocation.resource.is_some())
    match allocation.resource {
      Some(resource) => {
        assert_true(resource.in_use)
        assert_eq(resource.use_count, 1) // 每个资源只使用一次
      }
      None => assert_true(false)
    }
  }
  
  // 模拟并发资源释放
  let release_requests = successful_allocations.map(fn(allocation) {
    match allocation.resource {
      Some(resource) => {
        @azimuth.ResourceReleaseRequest {
          resource_id: resource.id,
          thread_id: allocation.request_id.split("-")[1].to_int(),
          timestamp: @azimuth.Timestamp.now()
        }
      }
      None => {
        @azimuth.ResourceReleaseRequest {
          resource_id: "",
          thread_id: 0,
          timestamp: @azimuth.Timestamp.now()
        }
      }
    }
  })
  
  // 模拟资源释放过程
  let mut release_results = @azimuth.List.new()
  let mut released_resources = @azimuth.List.new()
  
  for request in release_requests {
    let resource = successful_allocations.find_fn(allocation => {
      match allocation.resource {
        Some(r) => r.id == request.resource_id
        None => false
      }
    })
    
    match resource {
      Some(allocation) => {
      match allocation.resource {
        Some(r) => {
          let released_resource = @azimuth.PooledResource {
            id: r.id,
            type: r.type,
            created_at: r.created_at,
            last_used: request.timestamp,
            in_use: false,
            use_count: r.use_count
          }
          
          released_resources = released_resources + [released_resource]
          release_results = release_results + [@azimuth.ResourceReleaseResult {
            request_id: request.resource_id,
            success: true,
            timestamp: request.timestamp
          }]
        }
        None => {
          release_results = release_results + [@azimuth.ResourceReleaseResult {
            request_id: request.resource_id,
            success: false,
            timestamp: request.timestamp
          }]
        }
      }
    }
    None => {
      release_results = release_results + [@azimuth.ResourceReleaseResult {
        request_id: request.resource_id,
        success: false,
        timestamp: request.timestamp
      }]
    }
  }
  
  // 验证释放结果
  let successful_releases = release_results.filter(fn(result) { result.success })
  assert_eq(successful_releases.length(), 10) // 所有分配的资源都应该成功释放
  assert_eq(released_resources.length(), 10)
  
  // 验证释放的资源状态
  for resource in released_resources {
    assert_false(resource.in_use)
    assert_eq(resource.use_count, 1) // 使用次数保持不变
  }
  
  // 验证资源池状态
  assert_eq(released_resources.length(), initial_resources.length()) // 所有资源都释放回池中
}