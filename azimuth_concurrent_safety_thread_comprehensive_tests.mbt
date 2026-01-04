// Azimuth Concurrent Safety Thread Comprehensive Tests
// This file contains comprehensive concurrent safety and thread safety tests

// Test 1: Thread-Safe Data Structures
test "thread-safe data structures for telemetry operations" {
  // Define thread-safe counter
  type AtomicCounter = {
    value: Int,
    lock_acquired: Bool
  }
  
  // Define thread-safe map entry
  type MapEntry = {
    key: String,
    value: String,
    version: Int,
    locked: Bool
  }
  
  // Define thread-safe map
  type ThreadSafeMap = {
    entries: Array[MapEntry],
    lock_count: Int,
    operations_count: Int
  }
  
  // Simulate atomic increment operation
  let atomic_increment = fn(counter: AtomicCounter) {
    if not(counter.lock_acquired) {
      // Simulate acquiring lock
      let locked_counter = { counter | lock_acquired: true }
      let new_value = locked_counter.value + 1
      // Simulate releasing lock
      { value: new_value, lock_acquired: false }
    } else {
      // Lock already acquired, would wait in real implementation
      counter
    }
  }
  
  // Simulate thread-safe map operations
  let thread_safe_put = fn(map: ThreadSafeMap, key: String, value: String) {
    let mut new_entries = map.entries
    let mut found = false
    
    // Check if key already exists
    for i in 0..new_entries.length() {
      if new_entries[i].key == key and not(new_entries[i].locked) {
        // Simulate acquiring lock on entry
        new_entries[i] = {
          key: new_entries[i].key,
          value: value,
          version: new_entries[i].version + 1,
          locked: false  // Simulate lock release
        }
        found = true
        break
      }
    }
    
    if not(found) {
      // Add new entry
      new_entries = new_entries.push({
        key: key,
        value: value,
        version: 1,
        locked: false
      })
    }
    
    {
      entries: new_entries,
      lock_count: map.lock_count + 1,
      operations_count: map.operations_count + 1
    }
  }
  
  let thread_safe_get = fn(map: ThreadSafeMap, key: String) {
    for entry in map.entries {
      if entry.key == key and not(entry.locked) {
        Some(entry.value)
      }
    }
    None
  }
  
  // Test atomic operations
  let counter = { value: 0, lock_acquired: false }
  let counter1 = atomic_increment(counter)
  let counter2 = atomic_increment(counter1)
  let counter3 = atomic_increment(counter2)
  
  assert_eq(counter1.value, 1)
  assert_eq(counter2.value, 2)
  assert_eq(counter3.value, 3)
  assert_false(counter1.lock_acquired)
  assert_false(counter2.lock_acquired)
  assert_false(counter3.lock_acquired)
  
  // Test thread-safe map operations
  let initial_map = {
    entries: [],
    lock_count: 0,
    operations_count: 0
  }
  
  let map1 = thread_safe_put(initial_map, "trace-1", "span-1")
  let map2 = thread_safe_put(map1, "trace-2", "span-2")
  let map3 = thread_safe_put(map2, "trace-1", "span-1-updated")
  
  assert_eq(map3.entries.length(), 2)
  assert_eq(thread_safe_get(map3, "trace-1"), Some("span-1-updated"))
  assert_eq(thread_safe_get(map3, "trace-2"), Some("span-2"))
  assert_eq(thread_safe_get(map3, "trace-3"), None)
  
  // Verify version tracking
  let trace1_entry = map3.entries.filter(fn(e) { e.key == "trace-1" })[0]
  let trace2_entry = map3.entries.filter(fn(e) { e.key == "trace-2" })[0]
  
  assert_eq(trace1_entry.version, 2)  # Updated once
  assert_eq(trace2_entry.version, 1)  # Never updated
  
  // Test concurrent simulation
  let simulate_concurrent_puts = fn(map: ThreadSafeMap, operations: Array<(String, String)>) {
    let mut result = map
    for operation in operations {
      result = thread_safe_put(result, operation[0], operation[1])
    }
    result
  }
  
  let concurrent_operations = [
    ("trace-3", "span-3"),
    ("trace-4", "span-4"),
    ("trace-1", "span-1-final"),
    ("trace-5", "span-5")
  ]
  
  let concurrent_result = simulate_concurrent_puts(map3, concurrent_operations)
  
  assert_eq(concurrent_result.entries.length(), 5)
  assert_eq(thread_safe_get(concurrent_result, "trace-1"), Some("span-1-final"))
  assert_eq(thread_safe_get(concurrent_result, "trace-3"), Some("span-3"))
  assert_eq(thread_safe_get(concurrent_result, "trace-4"), Some("span-4"))
  assert_eq(thread_safe_get(concurrent_result, "trace-5"), Some("span-5"))
  
  // Verify operation tracking
  assert_eq(concurrent_result.operations_count, 7)  # 3 initial + 4 concurrent
  assert_eq(concurrent_result.lock_count, 7)
}

// Test 2: Concurrent Telemetry Processing
test "concurrent telemetry processing safety" {
  // Define telemetry batch
  type TelemetryBatch = {
    batch_id: String,
    spans: Array[TelemetrySpan>,
    processing_status: String,
    worker_id: Option[Int]
  }
  
  // Define telemetry span
  type TelemetrySpan = {
    span_id: String,
    trace_id: String,
    service_name: String,
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String
  }
  
  // Define worker pool
  type WorkerPool = {
    workers: Array[Worker>,
    task_queue: Array<TelemetryBatch>,
    completed_tasks: Array<TelemetryBatch>,
    active_workers: Int
  }
  
  // Define worker
  type Worker = {
    worker_id: Int,
    status: String,
    current_task: Option<TelemetryBatch>,
    processed_count: Int
  }
  
  // Create worker pool
  let create_worker_pool = fn(worker_count: Int) {
    let mut workers = []
    for i in 0..worker_count {
      workers = workers.push({
        worker_id: i,
        status: "idle",
        current_task: None,
        processed_count: 0
      })
    }
    
    {
      workers,
      task_queue: [],
      completed_tasks: [],
      active_workers: 0
    }
  }
  
  // Simulate processing a telemetry batch
  let process_batch = fn(batch: TelemetryBatch, worker_id: Int) {
    // Simulate processing time
    let processed_spans = batch.spans.map(fn(span) {
      {
        span_id: span.span_id,
        trace_id: span.trace_id,
        service_name: span.service_name,
        operation_name: span.operation_name,
        start_time: span.start_time,
        end_time: span.end_time,
        status: "processed"
      }
    })
    
    {
      batch_id: batch.batch_id,
      spans: processed_spans,
      processing_status: "completed",
      worker_id: Some(worker_id)
    }
  }
  
  // Assign task to worker
  let assign_task = fn(pool: WorkerPool, batch: TelemetryBatch) {
    let mut updated_workers = pool.workers
    let mut assigned = false
    
    for i in 0..updated_workers.length() {
      if updated_workers[i].status == "idle" {
        updated_workers[i] = {
          worker_id: updated_workers[i].worker_id,
          status: "busy",
          current_task: Some(batch),
          processed_count: updated_workers[i].processed_count
        }
        assigned = true
        break
      }
    }
    
    if assigned {
      {
        workers: updated_workers,
        task_queue: pool.task_queue,
        completed_tasks: pool.completed_tasks,
        active_workers: pool.active_workers + 1
      }
    } else {
      // No available workers, add to queue
      {
        workers: updated_workers,
        task_queue: pool.task_queue.push(batch),
        completed_tasks: pool.completed_tasks,
        active_workers: pool.active_workers
      }
    }
  }
  
  // Complete worker task
  let complete_task = fn(pool: WorkerPool, worker_id: Int) {
    let mut updated_workers = pool.workers
    let mut completed_batch = None
    
    for i in 0..updated_workers.length() {
      if updated_workers[i].worker_id == worker_id {
        match updated_workers[i].current_task {
          Some(batch) => {
            completed_batch = Some(process_batch(batch, worker_id))
            updated_workers[i] = {
              worker_id: worker_id,
              status: "idle",
              current_task: None,
              processed_count: updated_workers[i].processed_count + 1
            }
          }
          None => {}
        }
        break
      }
    }
    
    match completed_batch {
      Some(batch) => {
        {
          workers: updated_workers,
          task_queue: pool.task_queue,
          completed_tasks: pool.completed_tasks.push(batch),
          active_workers: pool.active_workers - 1
        }
      }
      None => pool
    }
  }
  
  // Test concurrent processing
  let pool = create_worker_pool(3)
  
  // Create test batches
  let batch1 = {
    batch_id: "batch-1",
    spans: [
      { span_id: "span-1", trace_id: "trace-1", service_name: "service-1", operation_name: "op-1", start_time: 1640995200, end_time: 1640995210, status: "pending" },
      { span_id: "span-2", trace_id: "trace-1", service_name: "service-2", operation_name: "op-2", start_time: 1640995220, end_time: 1640995230, status: "pending" }
    ],
    processing_status: "pending",
    worker_id: None
  }
  
  let batch2 = {
    batch_id: "batch-2",
    spans: [
      { span_id: "span-3", trace_id: "trace-2", service_name: "service-3", operation_name: "op-3", start_time: 1640995240, end_time: 1640995250, status: "pending" }
    ],
    processing_status: "pending",
    worker_id: None
  }
  
  let batch3 = {
    batch_id: "batch-3",
    spans: [
      { span_id: "span-4", trace_id: "trace-3", service_name: "service-4", operation_name: "op-4", start_time: 1640995260, end_time: 1640995270, status: "pending" },
      { span_id: "span-5", trace_id: "trace-3", service_name: "service-5", operation_name: "op-5", start_time: 1640995280, end_time: 1640995290, status: "pending" }
    ],
    processing_status: "pending",
    worker_id: None
  }
  
  // Assign tasks
  let pool1 = assign_task(pool, batch1)
  let pool2 = assign_task(pool1, batch2)
  let pool3 = assign_task(pool2, batch3)
  
  // Verify task assignment
  assert_eq(pool3.active_workers, 3)
  assert_eq(pool3.task_queue.length(), 0)  # All tasks assigned immediately
  
  // Complete tasks
  let pool4 = complete_task(pool3, 0)  # Complete batch1 on worker 0
  let pool5 = complete_task(pool4, 1)  # Complete batch2 on worker 1
  let pool6 = complete_task(pool5, 2)  # Complete batch3 on worker 2
  
  // Verify task completion
  assert_eq(pool6.active_workers, 0)
  assert_eq(pool6.completed_tasks.length(), 3)
  
  // Verify processed batches
  let completed_batch1 = pool6.completed_tasks.filter(fn(b) { b.batch_id == "batch-1" })[0]
  let completed_batch2 = pool6.completed_tasks.filter(fn(b) { b.batch_id == "batch-2" })[0]
  let completed_batch3 = pool6.completed_tasks.filter(fn(b) { b.batch_id == "batch-3" })[0]
  
  assert_eq(completed_batch1.processing_status, "completed")
  assert_eq(completed_batch1.worker_id, Some(0))
  assert_eq(completed_batch1.spans.length(), 2)
  assert_eq(completed_batch1.spans[0].status, "processed")
  assert_eq(completed_batch1.spans[1].status, "processed")
  
  assert_eq(completed_batch2.processing_status, "completed")
  assert_eq(completed_batch2.worker_id, Some(1))
  assert_eq(completed_batch2.spans.length(), 1)
  assert_eq(completed_batch2.spans[0].status, "processed")
  
  assert_eq(completed_batch3.processing_status, "completed")
  assert_eq(completed_batch3.worker_id, Some(2))
  assert_eq(completed_batch3.spans.length(), 2)
  assert_eq(completed_batch3.spans[0].status, "processed")
  assert_eq(completed_batch3.spans[1].status, "processed")
  
  // Test worker statistics
  let worker0 = pool6.workers.filter(fn(w) { w.worker_id == 0 })[0]
  let worker1 = pool6.workers.filter(fn(w) { w.worker_id == 1 })[0]
  let worker2 = pool6.workers.filter(fn(w) { w.worker_id == 2 })[0]
  
  assert_eq(worker0.processed_count, 1)
  assert_eq(worker1.processed_count, 1)
  assert_eq(worker2.processed_count, 1)
  assert_eq(worker0.status, "idle")
  assert_eq(worker1.status, "idle")
  assert_eq(worker2.status, "idle")
  
  // Test queue behavior when all workers are busy
  let batch4 = {
    batch_id: "batch-4",
    spans: [
      { span_id: "span-6", trace_id: "trace-4", service_name: "service-6", operation_name: "op-6", start_time: 1640995300, end_time: 1640995310, status: "pending" }
    ],
    processing_status: "pending",
    worker_id: None
  }
  
  let batch5 = {
    batch_id: "batch-5",
    spans: [
      { span_id: "span-7", trace_id: "trace-5", service_name: "service-7", operation_name: "op-7", start_time: 1640995320, end_time: 1640995330, status: "pending" }
    ],
    processing_status: "pending",
    worker_id: None
  }
  
  // Assign tasks when workers are busy
  let busy_pool = {
    workers: [
      { worker_id: 0, status: "busy", current_task: Some(batch1), processed_count: 1 },
      { worker_id: 1, status: "busy", current_task: Some(batch2), processed_count: 1 },
      { worker_id: 2, status: "busy", current_task: Some(batch3), processed_count: 1 }
    ],
    task_queue: [],
    completed_tasks: [],
    active_workers: 3
  }
  
  let busy_pool1 = assign_task(busy_pool, batch4)
  let busy_pool2 = assign_task(busy_pool1, batch5)
  
  // Tasks should be queued
  assert_eq(busy_pool2.active_workers, 3)
  assert_eq(busy_pool2.task_queue.length(), 2)
  assert_eq(busy_pool2.task_queue[0].batch_id, "batch-4")
  assert_eq(busy_pool2.task_queue[1].batch_id, "batch-5")
}

// Test 3: Concurrent Trace Context Management
test "concurrent trace context management safety" {
  // Define trace context
  type TraceContext = {
    trace_id: String,
    span_stack: Array<String>,
    baggage: Array<(String, String)>,
    lock_count: Int,
    version: Int
  }
  
  // Define context manager
  type ContextManager = {
    contexts: Array<TraceContext>,
    lock_table: Array<(String, Bool)>,  # trace_id -> locked
    operations_count: Int
  }
  
  // Create context manager
  let create_context_manager = fn() {
    {
      contexts: [],
      lock_table: [],
      operations_count: 0
    }
  }
  
  // Simulate acquiring lock on trace context
  let acquire_context_lock = fn(manager: ContextManager, trace_id: String) {
    let mut updated_lock_table = manager.lock_table
    let mut lock_acquired = false
    
    // Check if trace_id is already locked
    let mut found = false
    for i in 0..updated_lock_table.length() {
      if updated_lock_table[i][0] == trace_id {
        if not(updated_lock_table[i][1]) {
          // Lock is available, acquire it
          updated_lock_table[i] = (trace_id, true)
          lock_acquired = true
        }
        found = true
        break
      }
    }
    
    if not(found) {
      // Add new lock entry
      updated_lock_table = updated_lock_table.push((trace_id, true))
      lock_acquired = true
    }
    
    {
      contexts: manager.contexts,
      lock_table: updated_lock_table,
      operations_count: manager.operations_count + 1
    }
  }
  
  // Simulate releasing lock on trace context
  let release_context_lock = fn(manager: ContextManager, trace_id: String) {
    let mut updated_lock_table = manager.lock_table
    
    for i in 0..updated_lock_table.length() {
      if updated_lock_table[i][0] == trace_id {
        updated_lock_table[i] = (trace_id, false)
        break
      }
    }
    
    {
      contexts: manager.contexts,
      lock_table: updated_lock_table,
      operations_count: manager.operations_count + 1
    }
  }
  
  // Create or update trace context
  let update_trace_context = fn(manager: ContextManager, trace_id: String, span_id: String, baggage_items: Array<(String, String)>) {
    // First acquire lock
    let locked_manager = acquire_context_lock(manager, trace_id)
    
    // Check if context exists
    let mut updated_contexts = locked_manager.contexts
    let mut found = false
    
    for i in 0..updated_contexts.length() {
      if updated_contexts[i].trace_id == trace_id {
        // Update existing context
        let existing_context = updated_contexts[i]
        updated_contexts[i] = {
          trace_id: existing_context.trace_id,
          span_stack: existing_context.span_stack.push(span_id),
          baggage: existing_context.baggage + baggage_items,
          lock_count: existing_context.lock_count + 1,
          version: existing_context.version + 1
        }
        found = true
        break
      }
    }
    
    if not(found) {
      // Create new context
      updated_contexts = updated_contexts.push({
        trace_id: trace_id,
        span_stack: [span_id],
        baggage: baggage_items,
        lock_count: 1,
        version: 1
      })
    }
    
    // Release lock
    let unlocked_manager = release_context_lock({
      contexts: updated_contexts,
      lock_table: locked_manager.lock_table,
      operations_count: locked_manager.operations_count
    }, trace_id)
    
    unlocked_manager
  }
  
  // Get trace context
  let get_trace_context = fn(manager: ContextManager, trace_id: String) {
    // First acquire lock
    let locked_manager = acquire_context_lock(manager, trace_id)
    
    // Find context
    let mut found_context = None
    for context in locked_manager.contexts {
      if context.trace_id == trace_id {
        found_context = Some(context)
        break
      }
    }
    
    // Release lock
    let unlocked_manager = release_context_lock(locked_manager, trace_id)
    
    (found_context, unlocked_manager)
  }
  
  // Test concurrent context management
  let manager = create_context_manager()
  
  // Update contexts concurrently
  let manager1 = update_trace_context(manager, "trace-1", "span-1", [("user.id", "user-1")])
  let manager2 = update_trace_context(manager1, "trace-2", "span-2", [("user.id", "user-2")])
  let manager3 = update_trace_context(manager2, "trace-1", "span-3", [("request.id", "req-1")])
  let manager4 = update_trace_context(manager3, "trace-3", "span-4", [("user.id", "user-3")])
  let manager5 = update_trace_context(manager4, "trace-2", "span-5", [("session.id", "sess-1")])
  
  // Get contexts
  let (context1_opt, final_manager) = get_trace_context(manager5, "trace-1")
  let (context2_opt, _) = get_trace_context(final_manager, "trace-2")
  let (context3_opt, _) = get_trace_context(final_manager, "trace-3")
  
  // Verify context1
  assert_true(context1_opt.length > 0)
  let context1 = context1_opt.get
  assert_eq(context1.trace_id, "trace-1")
  assert_eq(context1.span_stack.length(), 2)
  assert_eq(context1.span_stack[0], "span-1")
  assert_eq(context1.span_stack[1], "span-3")
  assert_eq(context1.baggage.length(), 2)
  assert_true(context1.baggage.contains(("user.id", "user-1")))
  assert_true(context1.baggage.contains(("request.id", "req-1")))
  assert_eq(context1.version, 2)
  
  // Verify context2
  assert_true(context2_opt.length > 0)
  let context2 = context2_opt.get
  assert_eq(context2.trace_id, "trace-2")
  assert_eq(context2.span_stack.length(), 2)
  assert_eq(context2.span_stack[0], "span-2")
  assert_eq(context2.span_stack[1], "span-5")
  assert_eq(context2.baggage.length(), 2)
  assert_true(context2.baggage.contains(("user.id", "user-2")))
  assert_true(context2.baggage.contains(("session.id", "sess-1")))
  assert_eq(context2.version, 2)
  
  // Verify context3
  assert_true(context3_opt.length > 0)
  let context3 = context3_opt.get
  assert_eq(context3.trace_id, "trace-3")
  assert_eq(context3.span_stack.length(), 1)
  assert_eq(context3.span_stack[0], "span-4")
  assert_eq(context3.baggage.length(), 1)
  assert_true(context3.baggage.contains(("user.id", "user-3")))
  assert_eq(context3.version, 1)
  
  // Test concurrent baggage operations
  let update_baggage = fn(manager: ContextManager, trace_id: String, baggage_items: Array<(String, String)>) {
    let (context_opt, locked_manager) = get_trace_context(manager, trace_id)
    
    match context_opt {
      Some(context) => {
        let updated_context = {
          trace_id: context.trace_id,
          span_stack: context.span_stack,
          baggage: context.baggage + baggage_items,
          lock_count: context.lock_count + 1,
          version: context.version + 1
        }
        
        let mut updated_contexts = locked_manager.contexts
        for i in 0..updated_contexts.length() {
          if updated_contexts[i].trace_id == trace_id {
            updated_contexts[i] = updated_context
            break
          }
        }
        
        {
          contexts: updated_contexts,
          lock_table: locked_manager.lock_table,
          operations_count: locked_manager.operations_count + 1
        }
      }
      None => manager
    }
  }
  
  let manager6 = update_baggage(manager5, "trace-1", [("operation.type", "query")])
  let (context1_updated_opt, _) = get_trace_context(manager6, "trace-1")
  
  assert_true(context1_updated_opt.length > 0)
  let context1_updated = context1_updated_opt.get
  assert_eq(context1_updated.baggage.length(), 3)
  assert_true(context1_updated.baggage.contains(("operation.type", "query")))
  assert_eq(context1_updated.version, 3)
  
  // Test lock contention simulation
  let simulate_lock_contention = fn(manager: ContextManager, trace_id: String, operations: Array<Array<(String, String)>>) {
    let mut result_manager = manager
    
    for baggage_items in operations {
      result_manager = update_baggage(result_manager, trace_id, baggage_items)
    }
    
    result_manager
  }
  
  let concurrent_baggage_operations = [
    [("thread.id", "thread-1")],
    [("operation.id", "op-1")],
    [("request.id", "req-concurrent-1")],
    [("session.id", "sess-concurrent-1")]
  ]
  
  let contention_manager = simulate_lock_contention(manager6, "trace-1", concurrent_baggage_operations)
  let (context1_contention_opt, _) = get_trace_context(contention_manager, "trace-1")
  
  assert_true(context1_contention_opt.length > 0)
  let context1_contention = context1_contention_opt.get
  assert_eq(context1_contention.baggage.length(), 7)  # 3 original + 4 concurrent
  assert_eq(context1_contention.version, 7)  # Each operation increments version
  
  // Verify all baggage items are present
  assert_true(context1_contention.baggage.contains(("user.id", "user-1")))
  assert_true(context1_contention.baggage.contains(("request.id", "req-1")))
  assert_true(context1_contention.baggage.contains(("operation.type", "query")))
  assert_true(context1_contention.baggage.contains(("thread.id", "thread-1")))
  assert_true(context1_contention.baggage.contains(("operation.id", "op-1")))
  assert_true(context1_contention.baggage.contains(("request.id", "req-concurrent-1")))
  assert_true(context1_contention.baggage.contains(("session.id", "sess-concurrent-1")))
}

// Test 4: Concurrent Metrics Aggregation
test "concurrent metrics aggregation safety" {
  // Define metric entry
  type MetricEntry = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array<(String, String)>
  }
  
  // Define aggregated metric
  type AggregatedMetric = {
    name: String,
    count: Int,
    sum: Float,
    min: Float,
    max: Float,
    avg: Float,
    last_updated: Int,
    tags: Array<(String, String)>
  }
  
  // Define metrics store
  type MetricsStore = {
    raw_metrics: Array<MetricEntry>,
    aggregated_metrics: Array<AggregatedMetric>,
    lock_count: Int,
    aggregation_count: Int
  }
  
  // Create metrics store
  let create_metrics_store = fn() {
    {
      raw_metrics: [],
      aggregated_metrics: [],
      lock_count: 0,
      aggregation_count: 0
    }
  }
  
  // Add metric to store
  let add_metric = fn(store: MetricsStore, metric: MetricEntry) {
    // Simulate lock acquisition
    let locked_store = { store | lock_count: store.lock_count + 1 }
    
    // Add raw metric
    let updated_raw_metrics = locked_store.raw_metrics.push(metric)
    
    // Update or create aggregated metric
    let mut updated_aggregated_metrics = locked_store.aggregated_metrics
    let mut found = false
    
    for i in 0..updated_aggregated_metrics.length() {
      if updated_aggregated_metrics[i].name == metric.name && 
         updated_aggregated_metrics[i].tags == metric.tags {
        // Update existing aggregation
        let existing = updated_aggregated_metrics[i]
        let new_count = existing.count + 1
        let new_sum = existing.sum + metric.value
        let new_min = if metric.value < existing.min { metric.value } else { existing.min }
        let new_max = if metric.value > existing.max { metric.value } else { existing.max }
        let new_avg = new_sum / new_count.to_float()
        
        updated_aggregated_metrics[i] = {
          name: existing.name,
          count: new_count,
          sum: new_sum,
          min: new_min,
          max: new_max,
          avg: new_avg,
          last_updated: metric.timestamp,
          tags: existing.tags
        }
        found = true
        break
      }
    }
    
    if not(found) {
      // Create new aggregation
      updated_aggregated_metrics = updated_aggregated_metrics.push({
        name: metric.name,
        count: 1,
        sum: metric.value,
        min: metric.value,
        max: metric.value,
        avg: metric.value,
        last_updated: metric.timestamp,
        tags: metric.tags
      })
    }
    
    // Simulate lock release
    {
      raw_metrics: updated_raw_metrics,
      aggregated_metrics: updated_aggregated_metrics,
      lock_count: locked_store.lock_count,
      aggregation_count: locked_store.aggregation_count + 1
    }
  }
  
  // Get aggregated metric
  let get_aggregated_metric = fn(store: MetricsStore, name: String, tags: Array<(String, String)>) {
    // Simulate lock acquisition
    let locked_store = { store | lock_count: store.lock_count + 1 }
    
    // Find metric
    let mut found_metric = None
    for metric in locked_store.aggregated_metrics {
      if metric.name == name && metric.tags == tags {
        found_metric = Some(metric)
        break
      }
    }
    
    // Simulate lock release
    (found_metric, locked_store)
  }
  
  // Test concurrent metrics aggregation
  let store = create_metrics_store()
  
  // Add metrics concurrently
  let store1 = add_metric(store, {
    name: "response_time",
    value: 100.0,
    timestamp: 1640995200,
    tags: [("service", "api-gateway"), ("endpoint", "/api/users")]
  })
  
  let store2 = add_metric(store1, {
    name: "response_time",
    value: 150.0,
    timestamp: 1640995210,
    tags: [("service", "api-gateway"), ("endpoint", "/api/users")]
  })
  
  let store3 = add_metric(store2, {
    name: "response_time",
    value: 120.0,
    timestamp: 1640995220,
    tags: [("service", "api-gateway"), ("endpoint", "/api/users")]
  })
  
  let store4 = add_metric(store3, {
    name: "error_rate",
    value: 0.1,
    timestamp: 1640995230,
    tags: [("service", "auth-service")]
  })
  
  let store5 = add_metric(store4, {
    name: "error_rate",
    value: 0.2,
    timestamp: 1640995240,
    tags: [("service", "auth-service")]
  })
  
  // Get aggregated metrics
  let (response_time_opt, final_store) = get_aggregated_metric(store5, "response_time", [
    ("service", "api-gateway"), ("endpoint", "/api/users")
  ])
  
  let (error_rate_opt, _) = get_aggregated_metric(final_store, "error_rate", [
    ("service", "auth-service")
  ])
  
  // Verify response_time aggregation
  assert_true(response_time_opt.length > 0)
  let response_time = response_time_opt.get
  assert_eq(response_time.name, "response_time")
  assert_eq(response_time.count, 3)
  assert_eq(response_time.sum, 370.0)  # 100 + 150 + 120
  assert_eq(response_time.min, 100.0)
  assert_eq(response_time.max, 150.0)
  assert_eq(response_time.avg, 370.0 / 3.0)
  assert_eq(response_time.last_updated, 1640995220)
  
  // Verify error_rate aggregation
  assert_true(error_rate_opt.length > 0)
  let error_rate = error_rate_opt.get
  assert_eq(error_rate.name, "error_rate")
  assert_eq(error_rate.count, 2)
  assert_eq(error_rate.sum, 0.3)  # 0.1 + 0.2
  assert_eq(error_rate.min, 0.1)
  assert_eq(error_rate.max, 0.2)
  assert_eq(error_rate.avg, 0.3 / 2.0)
  assert_eq(error_rate.last_updated, 1640995240)
  
  // Test concurrent metric updates
  let simulate_concurrent_updates = fn(store: MetricsStore, metrics: Array<MetricEntry>) {
    let mut result_store = store
    for metric in metrics {
      result_store = add_metric(result_store, metric)
    }
    result_store
  }
  
  let concurrent_metrics = [
    { name: "throughput", value: 1000.0, timestamp: 1640995250, tags: [("service", "payment-service")] },
    { name: "throughput", value: 1200.0, timestamp: 1640995260, tags: [("service", "payment-service")] },
    { name: "throughput", value: 1100.0, timestamp: 1640995270, tags: [("service", "payment-service")] },
    { name: "throughput", value: 1300.0, timestamp: 1640995280, tags: [("service", "payment-service")] },
    { name: "throughput", value: 1050.0, timestamp: 1640995290, tags: [("service", "payment-service")] }
  ]
  
  let concurrent_store = simulate_concurrent_updates(store5, concurrent_metrics)
  let (throughput_opt, _) = get_aggregated_metric(concurrent_store, "throughput", [
    ("service", "payment-service")
  ])
  
  assert_true(throughput_opt.length > 0)
  let throughput = throughput_opt.get
  assert_eq(throughput.name, "throughput")
  assert_eq(throughput.count, 5)
  assert_eq(throughput.sum, 5650.0)  # 1000 + 1200 + 1100 + 1300 + 1050
  assert_eq(throughput.min, 1000.0)
  assert_eq(throughput.max, 1300.0)
  assert_eq(throughput.avg, 5650.0 / 5.0)
  
  // Test store statistics
  assert_eq(final_store.raw_metrics.length(), 5)
  assert_eq(final_store.aggregated_metrics.length(), 3)
  assert_true(final_store.lock_count > 0)
  assert_eq(final_store.aggregation_count, 5)
  
  // Test metric reset
  let reset_metric = fn(store: MetricsStore, name: String, tags: Array<(String, String)>) {
    let mut updated_aggregated_metrics = []
    
    for metric in store.aggregated_metrics {
      if metric.name == name && metric.tags == tags {
        // Reset metric
        updated_aggregated_metrics = updated_aggregated_metrics.push({
          name: metric.name,
          count: 0,
          sum: 0.0,
          min: 0.0,
          max: 0.0,
          avg: 0.0,
          last_updated: metric.last_updated,
          tags: metric.tags
        })
      } else {
        updated_aggregated_metrics = updated_aggregated_metrics.push(metric)
      }
    }
    
    {
      raw_metrics: store.raw_metrics,
      aggregated_metrics: updated_aggregated_metrics,
      lock_count: store.lock_count + 1,
      aggregation_count: store.aggregation_count
    }
  }
  
  let reset_store = reset_metric(final_store, "response_time", [
    ("service", "api-gateway"), ("endpoint", "/api/users")
  ])
  
  let (reset_response_time_opt, _) = get_aggregated_metric(reset_store, "response_time", [
    ("service", "api-gateway"), ("endpoint", "/api/users")
  ])
  
  assert_true(reset_response_time_opt.length > 0)
  let reset_response_time = reset_response_time_opt.get
  assert_eq(reset_response_time.count, 0)
  assert_eq(reset_response_time.sum, 0.0)
  assert_eq(reset_response_time.avg, 0.0)
  
  // Other metrics should be unaffected
  let (unchanged_error_rate_opt, _) = get_aggregated_metric(reset_store, "error_rate", [
    ("service", "auth-service")
  ])
  
  assert_true(unchanged_error_rate_opt.length > 0)
  let unchanged_error_rate = unchanged_error_rate_opt.get
  assert_eq(unchanged_error_rate.count, 2)
}

// Test 5: Concurrent Resource Pool Management
test "concurrent resource pool management safety" {
  // Define resource
  type Resource = {
    resource_id: String,
    resource_type: String,
    in_use: Bool,
    last_used: Int,
    usage_count: Int
  }
  
  // Define resource pool
  type ResourcePool = {
    resources: Array<Resource>,
    max_size: Int,
    current_size: Int,
    active_count: Int,
    lock_count: Int
  }
  
  // Create resource pool
  let create_resource_pool = fn(max_size: Int, resource_type: String) {
    let mut resources = []
    for i in 0..max_size {
      resources = resources.push({
        resource_id: resource_type + "-" + i.to_string(),
        resource_type: resource_type,
        in_use: false,
        last_used: 0,
        usage_count: 0
      })
    }
    
    {
      resources,
      max_size,
      current_size: max_size,
      active_count: 0,
      lock_count: 0
    }
  }
  
  // Acquire resource from pool
  let acquire_resource = fn(pool: ResourcePool) {
    // Simulate lock acquisition
    let locked_pool = { pool | lock_count: pool.lock_count + 1 }
    
    // Find available resource
    let mut updated_resources = locked_pool.resources
    let mut acquired_resource = None
    
    for i in 0..updated_resources.length() {
      if not(updated_resources[i].in_use) {
        updated_resources[i] = {
          resource_id: updated_resources[i].resource_id,
          resource_type: updated_resources[i].resource_type,
          in_use: true,
          last_used: 1640995200,
          usage_count: updated_resources[i].usage_count + 1
        }
        acquired_resource = Some(updated_resources[i])
        break
      }
    }
    
    // Simulate lock release
    let unlocked_pool = {
      resources: updated_resources,
      max_size: locked_pool.max_size,
      current_size: locked_pool.current_size,
      active_count: locked_pool.active_count + (if acquired_resource.length > 0 { 1 } else { 0 }),
      lock_count: locked_pool.lock_count
    }
    
    (acquired_resource, unlocked_pool)
  }
  
  // Release resource back to pool
  let release_resource = fn(pool: ResourcePool, resource_id: String) {
    // Simulate lock acquisition
    let locked_pool = { pool | lock_count: pool.lock_count + 1 }
    
    // Find and release resource
    let mut updated_resources = locked_pool.resources
    let mut found = false
    
    for i in 0..updated_resources.length() {
      if updated_resources[i].resource_id == resource_id {
        updated_resources[i] = {
          resource_id: updated_resources[i].resource_id,
          resource_type: updated_resources[i].resource_type,
          in_use: false,
          last_used: 1640995300,
          usage_count: updated_resources[i].usage_count
        }
        found = true
        break
      }
    }
    
    // Simulate lock release
    {
      resources: updated_resources,
      max_size: locked_pool.max_size,
      current_size: locked_pool.current_size,
      active_count: if found { locked_pool.active_count - 1 } else { locked_pool.active_count },
      lock_count: locked_pool.lock_count
    }
  }
  
  // Test concurrent resource acquisition and release
  let pool = create_resource_pool(3, "database-connection")
  
  // Acquire resources
  let (resource1_opt, pool1) = acquire_resource(pool)
  let (resource2_opt, pool2) = acquire_resource(pool1)
  let (resource3_opt, pool3) = acquire_resource(pool2)
  
  // Verify resource acquisition
  assert_true(resource1_opt.length > 0)
  assert_true(resource2_opt.length > 0)
  assert_true(resource3_opt.length > 0)
  
  let resource1 = resource1_opt.get
  let resource2 = resource2_opt.get
  let resource3 = resource3_opt.get
  
  assert_eq(resource1.resource_id, "database-connection-0")
  assert_eq(resource2.resource_id, "database-connection-1")
  assert_eq(resource3.resource_id, "database-connection-2")
  
  assert_true(resource1.in_use)
  assert_true(resource2.in_use)
  assert_true(resource3.in_use)
  
  assert_eq(pool3.active_count, 3)
  
  // Try to acquire when pool is exhausted
  let (resource4_opt, pool4) = acquire_resource(pool3)
  assert_true(resource4_opt.length == 0)  # No available resources
  assert_eq(pool4.active_count, 3)  # No change in active count
  
  // Release resources
  let pool5 = release_resource(pool4, resource1.resource_id)
  let pool6 = release_resource(pool5, resource2.resource_id)
  
  // Verify resource release
  assert_eq(pool6.active_count, 1)
  
  // Acquire resources again
  let (resource5_opt, pool7) = acquire_resource(pool6)
  let (resource6_opt, pool8) = acquire_resource(pool7)
  
  assert_true(resource5_opt.length > 0)
  assert_true(resource6_opt.length > 0)
  
  let resource5 = resource5_opt.get
  let resource6 = resource6_opt.get
  
  // Should get the released resources (possibly in different order)
  assert_true(resource5.resource_id == "database-connection-0" || resource5.resource_id == "database-connection-1")
  assert_true(resource6.resource_id == "database-connection-0" || resource6.resource_id == "database-connection-1")
  assert_not_eq(resource5.resource_id, resource6.resource_id)
  
  assert_eq(pool8.active_count, 3)
  
  // Test resource usage statistics
  let get_resource_stats = fn(pool: ResourcePool) {
    let mut total_usage = 0
    let mut in_use_count = 0
    
    for resource in pool.resources {
      total_usage = total_usage + resource.usage_count
      if resource.in_use {
        in_use_count = in_use_count + 1
      }
    }
    
    {
      total_resources: pool.resources.length(),
      in_use_count,
      available_count: pool.resources.length() - in_use_count,
      total_usage,
      avg_usage_per_resource: total_usage.to_float() / pool.resources.length().to_float()
    }
  }
  
  let stats = get_resource_stats(pool8)
  assert_eq(stats.total_resources, 3)
  assert_eq(stats.in_use_count, 3)
  assert_eq(stats.available_count, 0)
  assert_eq(stats.total_usage, 5)  # 3 initial + 2 re-acquired
  assert_eq(stats.avg_usage_per_resource, 5.0 / 3.0)
  
  // Test resource pool expansion
  let expand_pool = fn(pool: ResourcePool, additional_size: Int, resource_type: String) {
    let current_size = pool.resources.length()
    let new_max_size = pool.max_size + additional_size
    
    let mut new_resources = pool.resources
    for i in 0..additional_size {
      new_resources = new_resources.push({
        resource_id: resource_type + "-" + (current_size + i).to_string(),
        resource_type: resource_type,
        in_use: false,
        last_used: 0,
        usage_count: 0
      })
    }
    
    {
      resources: new_resources,
      max_size: new_max_size,
      current_size: new_max_size,
      active_count: pool.active_count,
      lock_count: pool.lock_count + 1
    }
  }
  
  let expanded_pool = expand_pool(pool8, 2, "database-connection")
  assert_eq(expanded_pool.resources.length(), 5)
  assert_eq(expanded_pool.max_size, 5)
  assert_eq(expanded_pool.current_size, 5)
  assert_eq(expanded_pool.active_count, 3)  # Active count unchanged
  
  // Acquire from expanded pool
  let (resource7_opt, pool9) = acquire_resource(expanded_pool)
  let (resource8_opt, pool10) = acquire_resource(pool9)
  
  assert_true(resource7_opt.length > 0)
  assert_true(resource8_opt.length > 0)
  
  let resource7 = resource7_opt.get
  let resource8 = resource8_opt.get
  
  assert_eq(resource7.resource_id, "database-connection-3")
  assert_eq(resource8.resource_id, "database-connection-4")
  
  assert_eq(pool10.active_count, 5)
  
  // Test resource pool shrinking
  let shrink_pool = fn(pool: ResourcePool, remove_count: Int) {
    let mut available_resources = []
    let mut in_use_resources = []
    
    // Separate available and in-use resources
    for resource in pool.resources {
      if resource.in_use {
        in_use_resources = in_use_resources.push(resource)
      } else {
        available_resources = available_resources.push(resource)
      }
    }
    
    // Remove available resources (up to remove_count)
    let keep_count = if available_resources.length() > remove_count {
      available_resources.length() - remove_count
    } else {
      0
    }
    
    let kept_available = available_resources.slice(0, keep_count)
    let final_resources = in_use_resources + kept_available
    
    {
      resources: final_resources,
      max_size: pool.max_size - remove_count,
      current_size: final_resources.length(),
      active_count: pool.active_count,
      lock_count: pool.lock_count + 1
    }
  }
  
  // Release some resources first
  let pool11 = release_resource(pool10, resource5.resource_id)
  let pool12 = release_resource(pool11, resource6.resource_id)
  
  // Now shrink the pool
  let shrunk_pool = shrink_pool(pool12, 2)
  assert_eq(shrunk_pool.resources.length(), 3)
  assert_eq(shrunk_pool.max_size, 3)
  assert_eq(shrunk_pool.current_size, 3)
  assert_eq(shrunk_pool.active_count, 3)  # In-use resources preserved
  
  // Verify remaining resources are the in-use ones
  let remaining_resource_ids = shrunk_pool.resources.map(fn(r) { r.resource_id })
  assert_true(remaining_resource_ids.contains(resource3.resource_id))
  assert_true(remaining_resource_ids.contains(resource7.resource_id))
  assert_true(remaining_resource_ids.contains(resource8.resource_id))
}