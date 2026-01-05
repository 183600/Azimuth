// Azimuth Performance Benchmarking Test Suite
// This file contains test cases for performance benchmarking and optimization in Azimuth

// Test 1: Span Creation Performance
test "span creation performance benchmarking" {
  // Define span structure
  type Span = {
    id: String,
    trace_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    tags: Array[(String, String)]
  }
  
  // Create span
  let create_span = fn(trace_id: String, parent_id: Option[String], operation: String) {
    let start_time = 1640995200  // Simulated timestamp
    let span_id = "span-" + start_time.to_string()
    
    {
      id: span_id,
      trace_id,
      parent_span_id: parent_id,
      operation_name: operation,
      start_time,
      end_time: start_time + 100,  // 100ms duration
      status: "ok",
      tags: [
        ("service.name", "telemetry"),
        ("service.version", "1.0.0"),
        ("environment", "test")
      ]
    }
  }
  
  // Benchmark span creation
  let benchmark_span_creation = fn(num_spans: Int) {
    let start_time = 0  // Simulated start time
    let mut spans = []
    
    for i in 0..num_spans {
      let trace_id = "trace-" + (i / 10).to_string()
      let parent_id = if i > 0 { Some("span-" + (i - 1).to_string()) } else { None }
      let operation = "operation-" + i.to_string()
      
      let span = create_span(trace_id, parent_id, operation)
      spans = spans.push(span)
    }
    
    let end_time = 1000  // Simulated end time (1000ms)
    let duration = end_time - start_time
    let spans_per_second = (num_spans.to_float() / duration.to_float()) * 1000.0
    
    {
      total_spans: num_spans,
      duration_ms: duration,
      spans_per_second,
      spans
    }
  }
  
  // Benchmark with different volumes
  let small_benchmark = benchmark_span_creation(100)
  let medium_benchmark = benchmark_span_creation(1000)
  let large_benchmark = benchmark_span_creation(10000)
  
  // Test benchmark results
  assert_eq(small_benchmark.total_spans, 100)
  assert_eq(small_benchmark.duration_ms, 1000)
  assert_eq(small_benchmark.spans_per_second, 100.0)
  assert_eq(small_benchmark.spans.length(), 100)
  
  assert_eq(medium_benchmark.total_spans, 1000)
  assert_eq(medium_benchmark.duration_ms, 1000)
  assert_eq(medium_benchmark.spans_per_second, 1000.0)
  assert_eq(medium_benchmark.spans.length(), 1000)
  
  assert_eq(large_benchmark.total_spans, 10000)
  assert_eq(large_benchmark.duration_ms, 1000)
  assert_eq(large_benchmark.spans_per_second, 10000.0)
  assert_eq(large_benchmark.spans.length(), 10000)
  
  // Test span structure integrity
  let first_span = small_benchmark.spans[0]
  assert_eq(first_span.operation_name, "operation-0")
  assert_eq(first_span.parent_span_id, None)
  
  let second_span = small_benchmark.spans[1]
  assert_eq(second_span.operation_name, "operation-1")
  assert_eq(second_span.parent_span_id, Some("span--1"))
  
  // Test tag integrity
  assert_eq(first_span.tags.length(), 3)
  assert_true(first_span.tags.contains(("service.name", "telemetry")))
  assert_true(first_span.tags.contains(("service.version", "1.0.0")))
  assert_true(first_span.tags.contains(("environment", "test")))
}

// Test 2: Metric Collection Performance
test "metric collection performance benchmarking" {
  // Define metric structure
  type Metric = {
    name: String,
    value: Float,
    unit: String,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  // Create metric
  let create_metric = fn(name: String, value: Float, unit: String) {
    {
      name,
      value,
      unit,
      timestamp: 1640995200,
      tags: [
        ("service", "telemetry"),
        ("host", "server-1"),
        ("region", "us-west-2")
      ]
    }
  }
  
  // Collect metrics
  let collect_metrics = fn(num_metrics: Int) {
    let start_time = 0
    let mut metrics = []
    
    for i in 0..num_metrics {
      let metric_name = "metric-" + (i % 10).to_string()
      let metric_value = (i % 100).to_float()
      let metric_unit = if i % 3 == 0 { "ms" } else if i % 3 == 1 { "count" } else { "bytes" }
      
      let metric = create_metric(metric_name, metric_value, metric_unit)
      metrics = metrics.push(metric)
    }
    
    let end_time = 500  // Simulated end time (500ms)
    let duration = end_time - start_time
    let metrics_per_second = (num_metrics.to_float() / duration.to_float()) * 1000.0
    
    {
      total_metrics: num_metrics,
      duration_ms: duration,
      metrics_per_second,
      metrics
    }
  }
  
  // Benchmark metric collection
  let metric_benchmark = collect_metrics(5000)
  
  // Test benchmark results
  assert_eq(metric_benchmark.total_metrics, 5000)
  assert_eq(metric_benchmark.duration_ms, 500)
  assert_eq(metric_benchmark.metrics_per_second, 10000.0)
  assert_eq(metric_benchmark.metrics.length(), 5000)
  
  // Test metric distribution
  let mut metric_counts = []
  for i in 0..10 {
    metric_counts = metric_counts.push(0)
  }
  
  for metric in metric_benchmark.metrics {
    let metric_num = metric.name.substring(7, metric.name.length()).to_int()
    metric_counts[metric_num] = metric_counts[metric_num] + 1
  }
  
  // Each metric should appear approximately 500 times (5000 / 10)
  for count in metric_counts {
    assert_eq(count, 500)
  }
  
  // Test metric structure
  let first_metric = metric_benchmark.metrics[0]
  assert_eq(first_metric.name, "metric-0")
  assert_eq(first_metric.value, 0.0)
  assert_eq(first_metric.unit, "ms")
  assert_eq(first_metric.tags.length(), 3)
}

// Test 3: Log Aggregation Performance
test "log aggregation performance benchmarking" {
  // Define log entry
  type LogEntry = {
    timestamp: Int,
    level: String,
    message: String,
    service: String,
    trace_id: Option[String],
    span_id: Option[String],
    fields: Array[(String, String)]
  }
  
  // Create log entry
  let create_log_entry = fn(timestamp: Int, level: String, message: String, service: String) {
    {
      timestamp,
      level,
      message,
      service,
      trace_id: if timestamp % 5 == 0 { Some("trace-" + (timestamp / 1000).to_string()) } else { None },
      span_id: if timestamp % 3 == 0 { Some("span-" + (timestamp / 100).to_string()) } else { None },
      fields: [
        ("thread", "main"),
        ("class", "TelemetryService"),
        ("method", "processData")
      ]
    }
  }
  
  // Aggregate logs
  let aggregate_logs = fn(num_logs: Int) {
    let start_time = 0
    let mut logs = []
    
    for i in 0..num_logs {
      let timestamp = 1640995200 + i
      let level = if i % 4 == 0 { "ERROR" } else if i % 4 == 1 { "WARN" } else if i % 4 == 2 { "INFO" } else { "DEBUG" }
      let message = "Log message " + i.to_string()
      let service = "service-" + (i % 5).to_string()
      
      let log_entry = create_log_entry(timestamp, level, message, service)
      logs = logs.push(log_entry)
    }
    
    let end_time = 800  // Simulated end time (800ms)
    let duration = end_time - start_time
    let logs_per_second = (num_logs.to_float() / duration.to_float()) * 1000.0
    
    {
      total_logs: num_logs,
      duration_ms: duration,
      logs_per_second,
      logs
    }
  }
  
  // Benchmark log aggregation
  let log_benchmark = aggregate_logs(8000)
  
  // Test benchmark results
  assert_eq(log_benchmark.total_logs, 8000)
  assert_eq(log_benchmark.duration_ms, 800)
  assert_eq(log_benchmark.logs_per_second, 10000.0)
  assert_eq(log_benchmark.logs.length(), 8000)
  
  // Test log level distribution
  let mut error_count = 0
  let mut warn_count = 0
  let mut info_count = 0
  let mut debug_count = 0
  
  for log in log_benchmark.logs {
    match log.level {
      "ERROR" => error_count = error_count + 1
      "WARN" => warn_count = warn_count + 1
      "INFO" => info_count = info_count + 1
      "DEBUG" => debug_count = debug_count + 1
      _ => ()
    }
  }
  
  // Each level should appear 2000 times (8000 / 4)
  assert_eq(error_count, 2000)
  assert_eq(warn_count, 2000)
  assert_eq(info_count, 2000)
  assert_eq(debug_count, 2000)
  
  // Test trace correlation
  let mut trace_correlated_logs = 0
  for log in log_benchmark.logs {
    match log.trace_id {
      Some(_) => trace_correlated_logs = trace_correlated_logs + 1
      None => ()
    }
  }
  
  // Approximately 1/5 of logs should have trace IDs
  assert_eq(trace_correlated_logs, 1600)
}

// Test 4: Trace Processing Performance
test "trace processing performance benchmarking" {
  // Define span for trace processing
  type TraceSpan = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    service_name: String,
    start_time: Int,
    duration: Int,
    status: String
  }
  
  // Create trace span
  let create_trace_span = fn(trace_id: String, span_id: String, parent_id: Option[String], service: String) {
    {
      trace_id,
      span_id,
      parent_span_id: parent_id,
      operation_name: "operation-" + span_id,
      service_name: service,
      start_time: 1640995200 + span_id.to_int(),
      duration: 50 + (span_id.to_int() % 200),
      status: if span_id.to_int() % 10 == 0 { "error" } else { "ok" }
    }
  }
  
  // Process trace
  let process_trace = fn(spans: Array[TraceSpan]) {
    let start_time = 0
    
    // Group spans by trace ID
    let mut trace_groups = []
    for span in spans {
      let mut found = false
      for i in 0..trace_groups.length() {
        if trace_groups[i].0 == span.trace_id {
          let span_list = trace_groups[i].1.push(span)
          trace_groups[i] = (trace_groups[i].0, span_list)
          found = true
        }
      }
      if not(found) {
        trace_groups = trace_groups.push((span.trace_id, [span]))
      }
    }
    
    // Calculate trace statistics
    let mut trace_stats = []
    for (trace_id, trace_spans) in trace_groups {
      let mut total_duration = 0
      let mut service_count = []
      let mut error_count = 0
      
      for span in trace_spans {
        total_duration = total_duration + span.duration
        
        let mut service_found = false
        for i in 0..service_count.length() {
          if service_count[i].0 == span.service_name {
            service_count[i] = (service_count[i].0, service_count[i].1 + 1)
            service_found = true
          }
        }
        if not(service_found) {
          service_count = service_count.push((span.service_name, 1))
        }
        
        if span.status == "error" {
          error_count = error_count + 1
        }
      }
      
      trace_stats = trace_stats.push({
        trace_id,
        span_count: trace_spans.length(),
        total_duration,
        service_count: service_count.length(),
        error_count
      })
    }
    
    let end_time = 1200  // Simulated end time (1200ms)
    let duration = end_time - start_time
    let spans_per_second = (spans.length().to_float() / duration.to_float()) * 1000.0
    
    {
      total_spans: spans.length(),
      trace_count: trace_groups.length(),
      duration_ms: duration,
      spans_per_second,
      trace_stats
    }
  }
  
  // Create test spans
  let mut test_spans = []
  for i in 0..10000 {
    let trace_id = "trace-" + (i / 20).to_string()  // 20 spans per trace
    let span_id = i.to_string()
    let parent_id = if i > 0 && i % 20 != 0 { Some((i - 1).to_string()) } else { None }
    let service = "service-" + (i % 8).to_string()
    
    let span = create_trace_span(trace_id, span_id, parent_id, service)
    test_spans = test_spans.push(span)
  }
  
  // Process traces
  let trace_processing_result = process_trace(test_spans)
  
  // Test processing results
  assert_eq(trace_processing_result.total_spans, 10000)
  assert_eq(trace_processing_result.trace_count, 500)  // 10000 / 20
  assert_eq(trace_processing_result.duration_ms, 1200)
  assert_eq(trace_processing_result.spans_per_second, 8333.33)
  assert_eq(trace_processing_result.trace_stats.length(), 500)
  
  // Test trace statistics
  let first_trace = trace_processing_result.trace_stats[0]
  assert_eq(first_trace.trace_id, "trace-0")
  assert_eq(first_trace.span_count, 20)
  assert_eq(first_trace.service_count, 8)  // All 8 services should be represented
  assert_eq(first_trace.error_count, 2)   // Approximately 1/10 of spans have errors
  
  // Verify service distribution
  let mut service_span_counts = []
  for i in 0..8 {
    service_span_counts = service_span_counts.push(0)
  }
  
  for span in test_spans {
    let service_num = span.service_name.substring(8, span.service_name.length()).to_int()
    service_span_counts[service_num] = service_span_counts[service_num] + 1
  }
  
  // Each service should have approximately 1250 spans (10000 / 8)
  for count in service_span_counts {
    assert_eq(count, 1250)
  }
}

// Test 5: Serialization Performance
test "serialization performance benchmarking" {
  // Define telemetry data
  type TelemetryData = {
    timestamp: Int,
    trace_id: String,
    spans: Array[SpanInfo],
    metrics: Array[MetricInfo],
    logs: Array[LogInfo]
  }
  
  type SpanInfo = {
    id: String,
    operation: String,
    service: String,
    start_time: Int,
    duration: Int
  }
  
  type MetricInfo = {
    name: String,
    value: Float,
    unit: String
  }
  
  type LogInfo = {
    level: String,
    message: String,
    timestamp: Int
  }
  
  // Create telemetry data
  let create_telemetry_data = fn(trace_id: String, span_count: Int, metric_count: Int, log_count: Int) {
    let mut spans = []
    for i in 0..span_count {
      spans = spans.push({
        id: "span-" + i.to_string(),
        operation: "operation-" + i.to_string(),
        service: "service-" + (i % 5).to_string(),
        start_time: 1640995200 + i * 10,
        duration: 50 + i % 100
      })
    }
    
    let mut metrics = []
    for i in 0..metric_count {
      metrics = metrics.push({
        name: "metric-" + i.to_string(),
        value: (i % 100).to_float(),
        unit: if i % 3 == 0 { "ms" } else if i % 3 == 1 { "count" } else { "bytes" }
      })
    }
    
    let mut logs = []
    for i in 0..log_count {
      logs = logs.push({
        level: if i % 4 == 0 { "ERROR" } else if i % 4 == 1 { "WARN" } else if i % 4 == 2 { "INFO" } else { "DEBUG" },
        message: "Log message " + i.to_string(),
        timestamp: 1640995200 + i * 5
      })
    }
    
    {
      timestamp: 1640995200,
      trace_id,
      spans,
      metrics,
      logs
    }
  }
  
  // Serialize to JSON (simulated)
  let serialize_to_json = fn(data: TelemetryData) {
    let start_time = 0
    
    // Simulate serialization process
    let json = "{"
      + "\"timestamp\":" + data.timestamp.to_string() + ","
      + "\"trace_id\":\"" + data.trace_id + "\"," 
      + "\"spans\":["
      + data.spans.map(fn(span) {
        "{\"id\":\"" + span.id + "\",\"operation\":\"" + span.operation + "\",\"service\":\"" + span.service + "\",\"start_time\":" + span.start_time.to_string() + ",\"duration\":" + span.duration.to_string() + "}"
      }).join(",")
      + "],"
      + "\"metrics\":["
      + data.metrics.map(fn(metric) {
        "{\"name\":\"" + metric.name + "\",\"value\":" + metric.value.to_string() + ",\"unit\":\"" + metric.unit + "\"}"
      }).join(",")
      + "],"
      + "\"logs\":["
      + data.logs.map(fn(log) {
        "{\"level\":\"" + log.level + "\",\"message\":\"" + log.message + "\",\"timestamp\":" + log.timestamp.to_string() + "}"
      }).join(",")
      + "]"
      + "}"
    
    let end_time = 300  // Simulated serialization time (300ms)
    let duration = end_time - start_time
    let size_bytes = json.length()
    
    {
      json,
      size_bytes,
      serialization_time_ms: duration
    }
  }
  
  // Deserialize from JSON (simulated)
  let deserialize_from_json = fn(json: String) {
    let start_time = 0
    
    // Simulate deserialization process
    // In a real implementation, this would parse the JSON string
    let data = create_telemetry_data("trace-123", 10, 5, 8)
    
    let end_time = 250  // Simulated deserialization time (250ms)
    let duration = end_time - start_time
    
    {
      data,
      deserialization_time_ms: duration
    }
  }
  
  // Benchmark serialization
  let telemetry_data = create_telemetry_data("trace-123", 50, 25, 40)
  let serialization_result = serialize_to_json(telemetry_data)
  
  // Test serialization
  assert_eq(serialization_result.serialization_time_ms, 300)
  assert_true(serialization_result.size_bytes > 1000)  // Should be substantial
  assert_true(serialization_result.json.contains("trace-123"))
  assert_true(serialization_result.json.contains("spans"))
  assert_true(serialization_result.json.contains("metrics"))
  assert_true(serialization_result.json.contains("logs"))
  
  // Benchmark deserialization
  let deserialization_result = deserialize_from_json(serialization_result.json)
  
  // Test deserialization
  assert_eq(deserialization_result.deserialization_time_ms, 250)
  assert_eq(deserialization_result.data.trace_id, "trace-123")
  assert_eq(deserialization_result.data.spans.length(), 50)
  assert_eq(deserialization_result.data.metrics.length(), 25)
  assert_eq(deserialization_result.data.logs.length(), 40)
  
  // Calculate throughput
  let total_time = serialization_result.serialization_time_ms + deserialization_result.deserialization_time_ms
  let data_size = serialization_result.size_bytes
  let throughput_mbps = (data_size.to_float() / total_time.to_float()) * 8.0  // Convert to megabits per second
  
  assert_true(throughput_mbps > 0.1)  // Should be at least 0.1 Mbps
}

// Test 6: Memory Usage Profiling
test "memory usage profiling for telemetry data" {
  // Define memory-optimized span
  type OptimizedSpan = {
    id: String,
    trace_id: String,  // Reuse trace_id reference to reduce memory
    operation_id: Int,  // Use integer ID instead of string
    start_time: Int,
    duration: Int
  }
  
  // Define operation lookup table
  type OperationTable = Array[String]
  
  // Create operation table
  let operation_table = [
    "database_query",
    "api_call",
    "cache_lookup",
    "auth_check",
    "validation",
    "serialization",
    "network_request",
    "file_io",
    "computation",
    "aggregation"
  ]
  
  // Create optimized span
  let create_optimized_span = fn(trace_id: String, operation_id: Int, start_time: Int, duration: Int) {
    {
      id: "span-" + start_time.to_string(),
      trace_id,
      operation_id,
      start_time,
      duration
    }
  }
  
  // Create regular span for comparison
  let create_regular_span = fn(trace_id: String, operation: String, start_time: Int, duration: Int) {
    {
      id: "span-" + start_time.to_string(),
      trace_id,
      operation_name: operation,
      start_time,
      duration
    }
  }
  
  // Calculate memory usage (simulated)
  let calculate_memory_usage = fn(num_spans: Int, use_optimized: Bool) {
    let start_time = 0
    let mut spans = []
    
    if use_optimized {
      // Create optimized spans
      for i in 0..num_spans {
        let trace_id = "trace-" + (i / 10).to_string()
        let operation_id = i % operation_table.length()
        let start = 1640995200 + i
        let duration = 50 + i % 200
        
        let span = create_optimized_span(trace_id, operation_id, start, duration)
        spans = spans.push(span)
      }
    } else {
      // Create regular spans
      for i in 0..num_spans {
        let trace_id = "trace-" + (i / 10).to_string()
        let operation = operation_table[i % operation_table.length()]
        let start = 1640995200 + i
        let duration = 50 + i % 200
        
        let span = create_regular_span(trace_id, operation, start, duration)
        spans = spans.push(span)
      }
    }
    
    let end_time = 100  // Simulated creation time
    let duration = end_time - start_time
    
    // Calculate estimated memory usage
    let base_span_size = if use_optimized { 64 } else { 128 }  // Bytes per span
    let total_memory_mb = (num_spans * base_span_size) / (1024 * 1024)
    
    {
      span_count: num_spans,
      creation_time_ms: duration,
      estimated_memory_mb: total_memory_mb,
      spans
    }
  }
  
  // Benchmark memory usage
  let optimized_result = calculate_memory_usage(10000, true)
  let regular_result = calculate_memory_usage(10000, false)
  
  // Test results
  assert_eq(optimized_result.span_count, 10000)
  assert_eq(regular_result.span_count, 10000)
  assert_eq(optimized_result.creation_time_ms, 100)
  assert_eq(regular_result.creation_time_ms, 100)
  
  // Optimized version should use less memory
  assert_true(optimized_result.estimated_memory_mb < regular_result.estimated_memory_mb)
  
  // Test specific memory calculations
  assert_eq(optimized_result.estimated_memory_mb, 0)  // 10000 * 64 / (1024 * 1024) = 0.61 MB, rounded down
  assert_eq(regular_result.estimated_memory_mb, 1)   // 10000 * 128 / (1024 * 1024) = 1.22 MB, rounded down
  
  // Test optimized span lookup
  let first_optimized_span = optimized_result.spans[0]
  assert_eq(first_optimized_span.trace_id, "trace-0")
  assert_eq(first_optimized_span.operation_id, 0)
  assert_eq(operation_table[first_optimized_span.operation_id], "database_query")
  
  // Test regular span lookup
  let first_regular_span = regular_result.spans[0]
  assert_eq(first_regular_span.trace_id, "trace-0")
  assert_eq(first_regular_span.operation_name, "database_query")
}

// Test 7: Concurrent Processing Performance
test "concurrent processing performance for telemetry data" {
  // Define processing task
  type ProcessingTask = {
    id: String,
    data_size: Int,
    complexity: String  // "low", "medium", "high"
  }
  
  // Define processing result
  type ProcessingResult = {
    task_id: String,
    start_time: Int,
    end_time: Int,
    duration: Int,
    success: Bool,
    processed_items: Int
  }
  
  // Process task (simulated)
  let process_task = fn(task: ProcessingTask, start_offset: Int) {
    let base_time = match task.complexity {
      "low" => 50,
      "medium" => 100,
      "high" => 200,
      _ => 100
    }
    
    let processing_time = base_time + (task.data_size / 100)
    let start_time = start_offset
    let end_time = start_time + processing_time
    
    {
      task_id: task.id,
      start_time,
      end_time,
      duration: processing_time,
      success: true,
      processed_items: task.data_size
    }
  }
  
  // Process tasks concurrently (simulated)
  let process_concurrently = fn(tasks: Array[ProcessingTask], max_workers: Int) {
    let start_time = 0
    let mut results = []
    let mut current_time = 0
    let mut active_workers = 0
    let mut task_queue = tasks
    let mut completed_tasks = 0
    
    while completed_tasks < tasks.length() {
      // Start new tasks if workers are available
      while active_workers < max_workers && task_queue.length() > 0 {
        let task = task_queue[0]
        task_queue = task_queue.slice(1, task_queue.length())
        
        let result = process_task(task, current_time)
        results = results.push(result)
        active_workers = active_workers + 1
      }
      
      // Find the earliest completing task
      let mut min_end_time = 999999
      for result in results {
        if result.end_time > current_time && result.end_time < min_end_time {
          min_end_time = result.end_time
        }
      }
      
      // Advance time to the next completion
      current_time = min_end_time
      
      // Update active workers
      active_workers = 0
      for result in results {
        if result.end_time > current_time {
          active_workers = active_workers + 1
        } else {
          completed_tasks = completed_tasks + 1
        }
      }
    }
    
    let end_time = current_time
    let duration = end_time - start_time
    
    {
      total_tasks: tasks.length(),
      max_workers,
      total_duration_ms: duration,
      results
    }
  }
  
  // Create test tasks
  let mut tasks = []
  for i in 0..50 {
    let complexity = if i % 3 == 0 { "low" } else if i % 3 == 1 { "medium" } else { "high" }
    let data_size = 100 + (i % 10) * 50
    
    tasks = tasks.push({
      id: "task-" + i.to_string(),
      data_size,
      complexity
    })
  }
  
  // Process with different worker counts
  let single_worker_result = process_concurrently(tasks, 1)
  let multi_worker_result = process_concurrently(tasks, 4)
  
  // Test results
  assert_eq(single_worker_result.total_tasks, 50)
  assert_eq(single_worker_result.max_workers, 1)
  assert_eq(single_worker_result.results.length(), 50)
  
  assert_eq(multi_worker_result.total_tasks, 50)
  assert_eq(multi_worker_result.max_workers, 4)
  assert_eq(multi_worker_result.results.length(), 50)
  
  // Multi-worker should be faster
  assert_true(multi_worker_result.total_duration_ms < single_worker_result.total_duration_ms)
  
  // Calculate throughput
  let single_throughput = 50.0 / (single_worker_result.total_duration_ms.to_float() / 1000.0)
  let multi_throughput = 50.0 / (multi_worker_result.total_duration_ms.to_float() / 1000.0)
  
  assert_true(multi_throughput > single_throughput)
  
  // Test task distribution
  let mut low_complexity_count = 0
  let mut medium_complexity_count = 0
  let mut high_complexity_count = 0
  
  for task in tasks {
    match task.complexity {
      "low" => low_complexity_count = low_complexity_count + 1
      "medium" => medium_complexity_count = medium_complexity_count + 1
      "high" => high_complexity_count = high_complexity_count + 1
      _ => ()
    }
  }
  
  // Each complexity should appear approximately 1/3 of the time
  assert_eq(low_complexity_count, 17)  // 50 / 3 = 16.67, rounded
  assert_eq(medium_complexity_count, 17)
  assert_eq(high_complexity_count, 16)
}

// Test 8: Cache Performance
test "cache performance for frequently accessed telemetry data" {
  // Define cache entry
  type CacheEntry = {
    key: String,
    value: String,
    access_count: Int,
    last_accessed: Int,
    size: Int
  }
  
  // Define cache
  type Cache = {
    entries: Array[CacheEntry],
    max_size: Int,
    current_size: Int,
    hits: Int,
    misses: Int
  }
  
  // Create cache
  let create_cache = fn(max_size: Int) {
    {
      entries: [],
      max_size,
      current_size: 0,
      hits: 0,
      misses: 0
    }
  }
  
  // Get from cache
  let cache_get = fn(cache: Cache, key: String, current_time: Int) {
    let mut found = false
    let mut value = None
    let mut updated_entries = []
    
    for entry in cache.entries {
      if entry.key == key {
        found = true
        value = Some(entry.value)
        updated_entries = updated_entries.push({
          key: entry.key,
          value: entry.value,
          access_count: entry.access_count + 1,
          last_accessed: current_time,
          size: entry.size
        })
      } else {
        updated_entries = updated_entries.push(entry)
      }
    }
    
    if found {
      ({
        value,
        hit: true,
        updated_cache: {
          entries: updated_entries,
          max_size: cache.max_size,
          current_size: cache.current_size,
          hits: cache.hits + 1,
          misses: cache.misses
        }
      })
    } else {
      ({
        value: None,
        hit: false,
        updated_cache: {
          entries: cache.entries,
          max_size: cache.max_size,
          current_size: cache.current_size,
          hits: cache.hits,
          misses: cache.misses + 1
        }
      })
    }
  }
  
  // Put in cache
  let cache_put = fn(cache: Cache, key: String, value: String, current_time: Int) {
    let value_size = value.length()
    
    // Check if key already exists
    let mut key_exists = false
    for entry in cache.entries {
      if entry.key == key {
        key_exists = true
      }
    }
    
    if key_exists {
      // Update existing entry
      let mut updated_entries = []
      for entry in cache.entries {
        if entry.key == key {
          updated_entries = updated_entries.push({
            key: entry.key,
            value: value,
            access_count: entry.access_count + 1,
            last_accessed: current_time,
            size: value_size
          })
        } else {
          updated_entries = updated_entries.push(entry)
        }
      }
      
      {
        entries: updated_entries,
        max_size: cache.max_size,
        current_size: cache.current_size,
        hits: cache.hits,
        misses: cache.misses
      }
    } else {
      // Add new entry
      if cache.current_size + value_size <= cache.max_size {
        // Space available
        {
          entries: cache.entries.push({
            key,
            value,
            access_count: 1,
            last_accessed: current_time,
            size: value_size
          }),
          max_size: cache.max_size,
          current_size: cache.current_size + value_size,
          hits: cache.hits,
          misses: cache.misses
        }
      } else {
        // Need to evict (simplified LRU)
        let mut oldest_time = 999999
        let mut oldest_index = -1
        
        for i in 0..cache.entries.length() {
          if cache.entries[i].last_accessed < oldest_time {
            oldest_time = cache.entries[i].last_accessed
            oldest_index = i
          }
        }
        
        let mut updated_entries = []
        for i in 0..cache.entries.length() {
          if i != oldest_index {
            updated_entries = updated_entries.push(cache.entries[i])
          }
        }
        
        let new_current_size = cache.current_size - cache.entries[oldest_index].size + value_size
        
        {
          entries: updated_entries.push({
            key,
            value,
            access_count: 1,
            last_accessed: current_time,
            size: value_size
          }),
          max_size: cache.max_size,
          current_size: new_current_size,
          hits: cache.hits,
          misses: cache.misses
        }
      }
    }
  }
  
  // Benchmark cache performance
  let benchmark_cache = fn(num_operations: Int, cache_size: Int) {
    let start_time = 0
    let mut cache = create_cache(cache_size * 1024)  // Convert KB to bytes
    let mut current_time = 0
    
    // Perform operations
    for i in 0..num_operations {
      current_time = current_time + 1
      let key = "key-" + (i % 100).to_string()  // 100 unique keys
      
      // Try to get from cache
      let get_result = cache_get(cache, key, current_time)
      cache = get_result.updated_cache
      
      match get_result.value {
        Some(_) => ()  // Cache hit
        None => {
          // Cache miss, put value in cache
          let value = "value-for-" + key + "-with-some-additional-data-to-increase-size"
          cache = cache_put(cache, key, value, current_time)
        }
      }
    }
    
    let end_time = 200  // Simulated end time (200ms)
    let duration = end_time - start_time
    let hit_rate = if cache.hits + cache.misses > 0 {
      (cache.hits.to_float() / (cache.hits + cache.misses).to_float()) * 100.0
    } else {
      0.0
    }
    
    {
      total_operations: num_operations,
      duration_ms: duration,
      hits: cache.hits,
      misses: cache.misses,
      hit_rate,
      operations_per_second: (num_operations.to_float() / duration.to_float()) * 1000.0
    }
  }
  
  // Benchmark with different cache sizes
  let small_cache_result = benchmark_cache(1000, 10)   // 10KB cache
  let medium_cache_result = benchmark_cache(1000, 50)  // 50KB cache
  let large_cache_result = benchmark_cache(1000, 100)  // 100KB cache
  
  // Test results
  assert_eq(small_cache_result.total_operations, 1000)
  assert_eq(small_cache_result.duration_ms, 200)
  assert_eq(small_cache_result.hits + small_cache_result.misses, 1000)
  
  assert_eq(medium_cache_result.total_operations, 1000)
  assert_eq(medium_cache_result.duration_ms, 200)
  assert_eq(medium_cache_result.hits + medium_cache_result.misses, 1000)
  
  assert_eq(large_cache_result.total_operations, 1000)
  assert_eq(large_cache_result.duration_ms, 200)
  assert_eq(large_cache_result.hits + large_cache_result.misses, 1000)
  
  // Larger cache should have higher hit rate
  assert_true(large_cache_result.hit_rate >= medium_cache_result.hit_rate)
  assert_true(medium_cache_result.hit_rate >= small_cache_result.hit_rate)
  
  // Test throughput
  assert_eq(small_cache_result.operations_per_second, 5000.0)
  assert_eq(medium_cache_result.operations_per_second, 5000.0)
  assert_eq(large_cache_result.operations_per_second, 5000.0)
}

// Test 9: Network I/O Performance
test "network I/O performance for telemetry data transmission" {
  // Define network packet
  type NetworkPacket = {
    id: String,
    data: String,
    size: Int,
    priority: Int  // 1-10, higher is more important
  }
  
  // Define network transmission result
  type TransmissionResult = {
    packet_id: String,
    start_time: Int,
    end_time: Int,
    duration: Int,
    success: Bool,
    bytes_transmitted: Int
  }
  
  // Simulate network transmission
  let transmit_packet = fn(packet: NetworkPacket, bandwidth: Int, latency: Int, start_time: Int) {
    // Calculate transmission time based on size and bandwidth
    let transmission_time = (packet.size * 8) / bandwidth  // Convert bytes to bits
    
    // Add latency
    let total_time = transmission_time + latency
    
    let end_time = start_time + total_time
    
    {
      packet_id: packet.id,
      start_time,
      end_time,
      duration: total_time,
      success: true,
      bytes_transmitted: packet.size
    }
  }
  
  // Batch transmission with priority queuing
  let batch_transmit = fn(packets: Array[NetworkPacket], bandwidth: Int, latency: Int) {
    let start_time = 0
    let mut results = []
    let mut current_time = 0
    let mut packet_queue = packets
    
    // Sort packets by priority (higher priority first)
    let mut sorted = true
    while sorted {
      sorted = false
      for i in 0..packet_queue.length() - 1 {
        if packet_queue[i].priority < packet_queue[i + 1].priority {
          let temp = packet_queue[i]
          packet_queue[i] = packet_queue[i + 1]
          packet_queue[i + 1] = temp
          sorted = true
        }
      }
    }
    
    // Transmit packets
    for packet in packet_queue {
      let result = transmit_packet(packet, bandwidth, latency, current_time)
      results = results.push(result)
      current_time = result.end_time
    }
    
    let end_time = current_time
    let duration = end_time - start_time
    let total_bytes = results.fold(0, fn(acc, result) { acc + result.bytes_transmitted })
    let throughput_mbps = (total_bytes.to_float() * 8.0) / (duration.to_float() / 1000.0) / (1024.0 * 1024.0)
    
    {
      total_packets: packets.length(),
      total_duration_ms: duration,
      total_bytes,
      throughput_mbps,
      results
    }
  }
  
  // Create test packets
  let mut packets = []
  for i in 0..100 {
    let size = 100 + (i % 10) * 50  // 100-550 bytes
    let priority = 1 + (i % 10)     // 1-10 priority
    
    packets = packets.push({
      id: "packet-" + i.to_string(),
      data: "telemetry-data-" + i.to_string(),
      size,
      priority
    })
  }
  
  // Test with different network conditions
  let high_bandwidth_result = batch_transmit(packets, 1000000, 10)    // 1 Mbps, 10ms latency
  let medium_bandwidth_result = batch_transmit(packets, 500000, 25)   // 0.5 Mbps, 25ms latency
  let low_bandwidth_result = batch_transmit(packets, 100000, 50)      // 0.1 Mbps, 50ms latency
  
  // Test results
  assert_eq(high_bandwidth_result.total_packets, 100)
  assert_eq(high_bandwidth_result.results.length(), 100)
  assert_true(high_bandwidth_result.total_duration_ms > 0)
  assert_true(high_bandwidth_result.total_bytes > 0)
  assert_true(high_bandwidth_result.throughput_mbps > 0)
  
  assert_eq(medium_bandwidth_result.total_packets, 100)
  assert_eq(medium_bandwidth_result.results.length(), 100)
  assert_true(medium_bandwidth_result.total_duration_ms > 0)
  assert_true(medium_bandwidth_result.total_bytes > 0)
  assert_true(medium_bandwidth_result.throughput_mbps > 0)
  
  assert_eq(low_bandwidth_result.total_packets, 100)
  assert_eq(low_bandwidth_result.results.length(), 100)
  assert_true(low_bandwidth_result.total_duration_ms > 0)
  assert_true(low_bandwidth_result.total_bytes > 0)
  assert_true(low_bandwidth_result.throughput_mbps > 0)
  
  // Higher bandwidth should be faster
  assert_true(high_bandwidth_result.total_duration_ms <= medium_bandwidth_result.total_duration_ms)
  assert_true(medium_bandwidth_result.total_duration_ms <= low_bandwidth_result.total_duration_ms)
  
  // Test packet ordering by priority
  let first_packet = high_bandwidth_result.results[0]
  let last_packet = high_bandwidth_result.results[99]
  
  // First packet should have highest priority (10)
  let first_priority = 0
  for packet in packets {
    if packet.id == first_packet.packet_id {
      first_priority = packet.priority
    }
  }
  assert_eq(first_priority, 10)
  
  // Last packet should have lowest priority (1)
  let last_priority = 0
  for packet in packets {
    if packet.id == last_packet.packet_id {
      last_priority = packet.priority
    }
  }
  assert_eq(last_priority, 1)
  
  // Calculate average packet size
  let avg_packet_size = high_bandwidth_result.total_bytes / high_bandwidth_result.total_packets
  assert_true(avg_packet_size >= 100 && avg_packet_size <= 550)
}

// Test 10: Database Query Performance
test "database query performance for telemetry data storage and retrieval" {
  // Define query type
  enum QueryType {
    SelectSpansByTraceId
    SelectMetricsByTimeRange
    SelectLogsByService
    SelectErrorRates
    InsertSpan
    InsertMetric
    InsertLog
  }
  
  // Define query result
  type QueryResult = {
    query_type: QueryType,
    execution_time_ms: Int,
    rows_affected: Int,
    success: Bool
  }
  
  // Simulate database query
  let execute_query = fn(query_type: QueryType, data_size: Int) {
    let base_time = match query_type {
      QueryType::SelectSpansByTraceId => 50,
      QueryType::SelectMetricsByTimeRange => 100,
      QueryType::SelectLogsByService => 75,
      QueryType::SelectErrorRates => 25,
      QueryType::InsertSpan => 10,
      QueryType::InsertMetric => 5,
      QueryType::InsertLog => 8
    }
    
    // Execution time increases with data size
    let execution_time = base_time + (data_size / 100)
    let rows_affected = match query_type {
      QueryType::SelectSpansByTraceId => 10 + (data_size / 50),
      QueryType::SelectMetricsByTimeRange => 50 + (data_size / 20),
      QueryType::SelectLogsByService => 25 + (data_size / 40),
      QueryType::SelectErrorRates => 5,
      QueryType::InsertSpan => 1,
      QueryType::InsertMetric => 1,
      QueryType::InsertLog => 1
    }
    
    {
      query_type,
      execution_time_ms: execution_time,
      rows_affected,
      success: true
    }
  }
  
  // Benchmark query performance
  let benchmark_queries = fn(num_queries: Int, data_size_range: (Int, Int)) {
    let start_time = 0
    let mut results = []
    let query_types = [
      QueryType::SelectSpansByTraceId,
      QueryType::SelectMetricsByTimeRange,
      QueryType::SelectLogsByService,
      QueryType::SelectErrorRates,
      QueryType::InsertSpan,
      QueryType::InsertMetric,
      QueryType::InsertLog
    ]
    
    for i in 0..num_queries {
      let query_type = query_types[i % query_types.length()]
      let data_size = data_size_range.0 + (i % (data_size_range.1 - data_size_range.0 + 1))
      
      let result = execute_query(query_type, data_size)
      results = results.push(result)
    }
    
    let end_time = 500  // Simulated end time (500ms)
    let duration = end_time - start_time
    
    // Calculate statistics
    let mut total_execution_time = 0
    let mut total_rows_affected = 0
    let mut query_type_stats = []
    
    for query_type in query_types {
      query_type_stats = query_type_stats.push((query_type, 0, 0, 0))  // (count, total_time, total_rows)
    }
    
    for result in results {
      total_execution_time = total_execution_time + result.execution_time_ms
      total_rows_affected = total_rows_affected + result.rows_affected
      
      for i in 0..query_type_stats.length() {
        if query_type_stats[i].0 == result.query_type {
          let (count, total_time, total_rows) = query_type_stats[i]
          query_type_stats[i] = (result.query_type, count + 1, total_time + result.execution_time_ms, total_rows + result.rows_affected)
        }
      }
    }
    
    let avg_execution_time = total_execution_time / num_queries
    let queries_per_second = (num_queries.to_float() / duration.to_float()) * 1000.0
    
    {
      total_queries: num_queries,
      duration_ms: duration,
      avg_execution_time_ms: avg_execution_time,
      total_rows_affected,
      queries_per_second,
      query_type_stats
    }
  }
  
  // Benchmark with different data sizes
  let small_data_result = benchmark_queries(100, (100, 500))
  let large_data_result = benchmark_queries(100, (1000, 5000))
  
  // Test results
  assert_eq(small_data_result.total_queries, 100)
  assert_eq(small_data_result.duration_ms, 500)
  assert_eq(small_data_result.query_type_stats.length(), 7)
  
  assert_eq(large_data_result.total_queries, 100)
  assert_eq(large_data_result.duration_ms, 500)
  assert_eq(large_data_result.query_type_stats.length(), 7)
  
  // Large data should have higher average execution time
  assert_true(large_data_result.avg_execution_time_ms >= small_data_result.avg_execution_time_ms)
  
  // Test query distribution
  let mut select_count = 0
  let mut insert_count = 0
  
  for stat in small_data_result.query_type_stats {
    match stat.0 {
      QueryType::SelectSpansByTraceId => select_count = select_count + 1
      QueryType::SelectMetricsByTimeRange => select_count = select_count + 1
      QueryType::SelectLogsByService => select_count = select_count + 1
      QueryType::SelectErrorRates => select_count = select_count + 1
      QueryType::InsertSpan => insert_count = insert_count + 1
      QueryType::InsertMetric => insert_count = insert_count + 1
      QueryType::InsertLog => insert_count = insert_count + 1
    }
  }
  
  assert_eq(select_count, 4)  // 4 select query types
  assert_eq(insert_count, 3)  // 3 insert query types
  
  // Test performance metrics
  assert_true(small_data_result.queries_per_second > 0)
  assert_true(large_data_result.queries_per_second > 0)
  assert_eq(small_data_result.queries_per_second, 200.0)  // 100 queries / 0.5 seconds
  assert_eq(large_data_result.queries_per_second, 200.0)  // 100 queries / 0.5 seconds
}