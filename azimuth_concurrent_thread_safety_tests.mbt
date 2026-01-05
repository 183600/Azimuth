// Azimuth Concurrent Thread Safety Test Suite
// This file contains test cases for concurrent operations and thread safety in Azimuth

// Test 1: Atomic Operations for Counters
test "atomic operations for telemetry counters" {
  // Simulate atomic counter using mutex-like behavior
  type AtomicCounter = {
    value: Int,
    lock: Bool
  }
  
  // Create atomic counter
  let create_counter = fn(initial_value: Int) {
    {
      value: initial_value,
      lock: false
    }
  }
  
  // Atomic increment
  let atomic_increment = fn(counter: AtomicCounter) {
    if not(counter.lock) {
      // Lock acquired
      {
        value: counter.value + 1,
        lock: false
      }
    } else {
      // Lock not acquired, return original
      counter
    }
  }
  
  // Atomic add
  let atomic_add = fn(counter: AtomicCounter, delta: Int) {
    if not(counter.lock) {
      // Lock acquired
      {
        value: counter.value + delta,
        lock: false
      }
    } else {
      // Lock not acquired, return original
      counter
    }
  }
  
  // Test atomic operations
  let counter1 = create_counter(0)
  assert_eq(counter1.value, 0)
  
  let counter2 = atomic_increment(counter1)
  assert_eq(counter2.value, 1)
  
  let counter3 = atomic_add(counter2, 5)
  assert_eq(counter3.value, 6)
  
  // Test concurrent increments (simulated)
  let mut current_counter = create_counter(0)
  let mut results = []
  
  // Simulate 10 concurrent increments
  for i in 0..10 {
    current_counter = atomic_increment(current_counter)
    results = results.push(current_counter.value)
  }
  
  // Final value should be 10
  assert_eq(current_counter.value, 10)
  
  // Test intermediate values
  for i in 0..results.length() {
    assert_eq(results[i], i + 1)
  }
}

// Test 2: Thread-Safe Queue
test "thread-safe queue for telemetry events" {
  // Define thread-safe queue
  type ThreadSafeQueue = {
    items: Array[String],
    head: Int,
    tail: Int,
    size: Int,
    capacity: Int,
    lock: Bool
  }
  
  // Create thread-safe queue
  let create_queue = fn(capacity: Int) {
    {
      items: [],
      head: 0,
      tail: 0,
      size: 0,
      capacity,
      lock: false
    }
  }
  
  // Enqueue operation
  let enqueue = fn(queue: ThreadSafeQueue, item: String) {
    if not(queue.lock) && queue.size < queue.capacity {
      let updated_items = queue.items.push(item)
      {
        items: updated_items,
        head: queue.head,
        tail: queue.tail + 1,
        size: queue.size + 1,
        capacity: queue.capacity,
        lock: false
      }
    } else {
      // Queue full or locked
      queue
    }
  }
  
  // Dequeue operation
  let dequeue = fn(queue: ThreadSafeQueue) {
    if not(queue.lock) && queue.size > 0 {
      let item = queue.items[queue.head]
      let updated_items = queue.items.slice(1, queue.items.length())
      {
        item: Some(item),
        updated_queue: {
          items: updated_items,
          head: queue.head + 1,
          tail: queue.tail,
          size: queue.size - 1,
          capacity: queue.capacity,
          lock: false
        }
      }
    } else {
      // Queue empty or locked
      {
        item: None,
        updated_queue: queue
      }
    }
  }
  
  // Test queue operations
  let mut queue = create_queue(5)
  
  // Test empty queue
  assert_eq(queue.size, 0)
  let empty_result = dequeue(queue)
  assert_eq(empty_result.item, None)
  queue = empty_result.updated_queue
  
  // Enqueue items
  queue = enqueue(queue, "event-1")
  assert_eq(queue.size, 1)
  
  queue = enqueue(queue, "event-2")
  assert_eq(queue.size, 2)
  
  queue = enqueue(queue, "event-3")
  assert_eq(queue.size, 3)
  
  // Dequeue items
  let dequeue_result1 = dequeue(queue)
  assert_eq(dequeue_result1.item, Some("event-1"))
  assert_eq(dequeue_result1.updated_queue.size, 2)
  queue = dequeue_result1.updated_queue
  
  let dequeue_result2 = dequeue(queue)
  assert_eq(dequeue_result2.item, Some("event-2"))
  assert_eq(dequeue_result2.updated_queue.size, 1)
  queue = dequeue_result2.updated_queue
  
  // Enqueue more items
  queue = enqueue(queue, "event-4")
  queue = enqueue(queue, "event-5")
  
  // Test queue capacity
  assert_eq(queue.size, 3)
  
  // Fill to capacity
  queue = enqueue(queue, "event-6")
  queue = enqueue(queue, "event-7")
  
  assert_eq(queue.size, 5)
  
  // Try to enqueue beyond capacity
  let full_queue = enqueue(queue, "event-8")
  assert_eq(full_queue.size, 5)  // Should remain at capacity
}

// Test 3: Concurrent Span Processing
test "concurrent span processing with synchronization" {
  // Define span processor
  type SpanProcessor = {
    processed_spans: Array[String],
    processing_queue: Array[String],
    max_concurrent: Int,
    current_processing: Int,
    lock: Bool
  }
  
  // Create span processor
  let create_processor = fn(max_concurrent: Int) {
    {
      processed_spans: [],
      processing_queue: [],
      max_concurrent,
      current_processing: 0,
      lock: false
    }
  }
  
  // Add span to processing queue
  let add_span = fn(processor: SpanProcessor, span_id: String) {
    if not(processor.lock) {
      {
        processed_spans: processor.processed_spans,
        processing_queue: processor.processing_queue.push(span_id),
        max_concurrent: processor.max_concurrent,
        current_processing: processor.current_processing,
        lock: false
      }
    } else {
      processor
    }
  }
  
  // Process spans (simulate concurrent processing)
  let process_spans = fn(processor: SpanProcessor) {
    if not(processor.lock) && processor.current_processing < processor.max_concurrent && processor.processing_queue.length() > 0 {
      let span_to_process = processor.processing_queue[0]
      let remaining_queue = processor.processing_queue.slice(1, processor.processing_queue.length())
      
      {
        processed_spans: processor.processed_spans.push(span_to_process),
        processing_queue: remaining_queue,
        max_concurrent: processor.max_concurrent,
        current_processing: processor.current_processing + 1,
        lock: false
      }
    } else {
      processor
    }
  }
  
  // Complete span processing
  let complete_span = fn(processor: SpanProcessor) {
    if not(processor.lock) && processor.current_processing > 0 {
      {
        processed_spans: processor.processed_spans,
        processing_queue: processor.processing_queue,
        max_concurrent: processor.max_concurrent,
        current_processing: processor.current_processing - 1,
        lock: false
      }
    } else {
      processor
    }
  }
  
  // Test concurrent span processing
  let mut processor = create_processor(3)
  
  // Add spans to queue
  processor = add_span(processor, "span-1")
  processor = add_span(processor, "span-2")
  processor = add_span(processor, "span-3")
  processor = add_span(processor, "span-4")
  processor = add_span(processor, "span-5")
  
  assert_eq(processor.processing_queue.length(), 5)
  assert_eq(processor.current_processing, 0)
  
  // Process spans up to concurrency limit
  processor = process_spans(processor)
  assert_eq(processor.processing_queue.length(), 4)
  assert_eq(processor.current_processing, 1)
  assert_eq(processor.processed_spans.length(), 1)
  
  processor = process_spans(processor)
  assert_eq(processor.processing_queue.length(), 3)
  assert_eq(processor.current_processing, 2)
  assert_eq(processor.processed_spans.length(), 2)
  
  processor = process_spans(processor)
  assert_eq(processor.processing_queue.length(), 2)
  assert_eq(processor.current_processing, 3)
  assert_eq(processor.processed_spans.length(), 3)
  
  // Try to process beyond concurrency limit
  processor = process_spans(processor)
  assert_eq(processor.processing_queue.length(), 2)
  assert_eq(processor.current_processing, 3)  // Should remain at max
  assert_eq(processor.processed_spans.length(), 3)
  
  // Complete some spans
  processor = complete_span(processor)
  assert_eq(processor.current_processing, 2)
  
  processor = complete_span(processor)
  assert_eq(processor.current_processing, 1)
  
  // Process more spans
  processor = process_spans(processor)
  assert_eq(processor.processing_queue.length(), 1)
  assert_eq(processor.current_processing, 2)
  assert_eq(processor.processed_spans.length(), 4)
  
  processor = process_spans(processor)
  assert_eq(processor.processing_queue.length(), 0)
  assert_eq(processor.current_processing, 3)
  assert_eq(processor.processed_spans.length(), 5)
}

// Test 4: Reader-Writer Lock for Trace Data
test "reader-writer lock for trace data access" {
  // Define reader-writer lock state
  enum LockState {
    Unlocked
    ReadLocked(Int)  // Number of readers
    WriteLocked
  }
  
  // Define trace data with reader-writer lock
  type TraceData = {
    traces: Array[String],
    lock_state: LockState,
    waiting_readers: Int,
    waiting_writers: Int
  }
  
  // Create trace data
  let create_trace_data = fn() {
    {
      traces: [],
      lock_state: LockState::Unlocked,
      waiting_readers: 0,
      waiting_writers: 0
    }
  }
  
  // Acquire read lock
  let acquire_read_lock = fn(data: TraceData) {
    match data.lock_state {
      LockState::Unlocked => {
        {
          traces: data.traces,
          lock_state: LockState::ReadLocked(1),
          waiting_readers: data.waiting_readers,
          waiting_writers: data.waiting_writers
        }
      }
      LockState::ReadLocked(count) => {
        {
          traces: data.traces,
          lock_state: LockState::ReadLocked(count + 1),
          waiting_readers: data.waiting_readers,
          waiting_writers: data.waiting_writers
        }
      }
      LockState::WriteLocked => {
        // Must wait for writer to finish
        {
          traces: data.traces,
          lock_state: LockState::WriteLocked,
          waiting_readers: data.waiting_readers + 1,
          waiting_writers: data.waiting_writers
        }
      }
    }
  }
  
  // Release read lock
  let release_read_lock = fn(data: TraceData) {
    match data.lock_state {
      LockState::ReadLocked(1) => {
        // Last reader, unlock
        {
          traces: data.traces,
          lock_state: LockState::Unlocked,
          waiting_readers: data.waiting_readers,
          waiting_writers: data.waiting_writers
        }
      }
      LockState::ReadLocked(count) => {
        // More readers, decrement count
        {
          traces: data.traces,
          lock_state: LockState::ReadLocked(count - 1),
          waiting_readers: data.waiting_readers,
          waiting_writers: data.waiting_writers
        }
      }
      _ => data  // Invalid state
    }
  }
  
  // Acquire write lock
  let acquire_write_lock = fn(data: TraceData) {
    match data.lock_state {
      LockState::Unlocked => {
        {
          traces: data.traces,
          lock_state: LockState::WriteLocked,
          waiting_readers: data.waiting_readers,
          waiting_writers: data.waiting_writers
        }
      }
      _ => {
        // Must wait for lock to be released
        {
          traces: data.traces,
          lock_state: data.lock_state,
          waiting_readers: data.waiting_readers,
          waiting_writers: data.waiting_writers + 1
        }
      }
    }
  }
  
  // Release write lock
  let release_write_lock = fn(data: TraceData) {
    match data.lock_state {
      LockState::WriteLocked => {
        {
          traces: data.traces,
          lock_state: LockState::Unlocked,
          waiting_readers: data.waiting_readers,
          waiting_writers: data.waiting_writers
        }
      }
      _ => data  // Invalid state
    }
  }
  
  // Read traces (requires read lock)
  let read_traces = fn(data: TraceData) {
    match data.lock_state {
      LockState::ReadLocked(_) => {
        // Can read
        data.traces
      }
      _ => []  // Cannot read without proper lock
    }
  }
  
  // Write trace (requires write lock)
  let write_trace = fn(data: TraceData, trace: String) {
    match data.lock_state {
      LockState::WriteLocked => {
        {
          traces: data.traces.push(trace),
          lock_state: data.lock_state,
          waiting_readers: data.waiting_readers,
          waiting_writers: data.waiting_writers
        }
      }
      _ => data  // Cannot write without proper lock
    }
  }
  
  // Test reader-writer lock
  let mut trace_data = create_trace_data()
  
  // Test initial state
  assert_eq(trace_data.traces.length(), 0)
  assert_eq(trace_data.lock_state, LockState::Unlocked)
  
  // Acquire read locks
  trace_data = acquire_read_lock(trace_data)
  assert_eq(trace_data.lock_state, LockState::ReadLocked(1))
  
  trace_data = acquire_read_lock(trace_data)
  assert_eq(trace_data.lock_state, LockState::ReadLocked(2))
  
  trace_data = acquire_read_lock(trace_data)
  assert_eq(trace_data.lock_state, LockState::ReadLocked(3))
  
  // Read traces with read lock
  let traces1 = read_traces(trace_data)
  assert_eq(traces1.length(), 0)
  
  // Try to acquire write lock (should wait)
  trace_data = acquire_write_lock(trace_data)
  assert_eq(trace_data.lock_state, LockState::ReadLocked(3))
  assert_eq(trace_data.waiting_writers, 1)
  
  // Release read locks
  trace_data = release_read_lock(trace_data)
  assert_eq(trace_data.lock_state, LockState::ReadLocked(2))
  
  trace_data = release_read_lock(trace_data)
  assert_eq(trace_data.lock_state, LockState::ReadLocked(1))
  
  trace_data = release_read_lock(trace_data)
  assert_eq(trace_data.lock_state, LockState::Unlocked)
  
  // Now writer can acquire lock (in real implementation)
  trace_data = acquire_write_lock(trace_data)
  assert_eq(trace_data.lock_state, LockState::WriteLocked)
  
  // Write trace with write lock
  trace_data = write_trace(trace_data, "trace-1")
  assert_eq(trace_data.traces.length(), 1)
  assert_eq(trace_data.traces[0], "trace-1")
  
  trace_data = write_trace(trace_data, "trace-2")
  assert_eq(trace_data.traces.length(), 2)
  assert_eq(trace_data.traces[1], "trace-2")
  
  // Release write lock
  trace_data = release_write_lock(trace_data)
  assert_eq(trace_data.lock_state, LockState::Unlocked)
}

// Test 5: Concurrent Metric Aggregation
test "concurrent metric aggregation with synchronization" {
  // Define metric aggregator
  type MetricAggregator = {
    metrics: Array[(String, Float)],
    lock: Bool,
    pending_updates: Array[(String, Float)]
  }
  
  // Create metric aggregator
  let create_aggregator = fn() {
    {
      metrics: [],
      lock: false,
      pending_updates: []
    }
  }
  
  // Add metric update
  let add_metric_update = fn(aggregator: MetricAggregator, name: String, value: Float) {
    if not(aggregator.lock) {
      {
        metrics: aggregator.metrics,
        lock: aggregator.lock,
        pending_updates: aggregator.pending_updates.push((name, value))
      }
    } else {
      aggregator
    }
  }
  
  // Process pending updates
  let process_updates = fn(aggregator: MetricAggregator) {
    if not(aggregator.lock) {
      let mut updated_metrics = aggregator.metrics
      let updates = aggregator.pending_updates
      
      for (name, value) in updates {
        let mut found = false
        for i in 0..updated_metrics.length() {
          if updated_metrics[i].0 == name {
            updated_metrics[i] = (name, updated_metrics[i].1 + value)
            found = true
          }
        }
        if not(found) {
          updated_metrics = updated_metrics.push((name, value))
        }
      }
      
      {
        metrics: updated_metrics,
        lock: aggregator.lock,
        pending_updates: []
      }
    } else {
      aggregator
    }
  }
  
  // Get metric value
  let get_metric = fn(aggregator: MetricAggregator, name: String) {
    for metric in aggregator.metrics {
      if metric.0 == name {
        return Some(metric.1)
      }
    }
    None
  }
  
  // Test concurrent metric aggregation
  let mut aggregator = create_aggregator()
  
  // Add metric updates
  aggregator = add_metric_update(aggregator, "request.count", 1.0)
  aggregator = add_metric_update(aggregator, "request.duration", 100.0)
  aggregator = add_metric_update(aggregator, "error.count", 0.0)
  
  assert_eq(aggregator.pending_updates.length(), 3)
  assert_eq(aggregator.metrics.length(), 0)
  
  // Process updates
  aggregator = process_updates(aggregator)
  
  assert_eq(aggregator.pending_updates.length(), 0)
  assert_eq(aggregator.metrics.length(), 3)
  
  // Check metric values
  assert_eq(get_metric(aggregator, "request.count"), Some(1.0))
  assert_eq(get_metric(aggregator, "request.duration"), Some(100.0))
  assert_eq(get_metric(aggregator, "error.count"), Some(0.0))
  
  // Add more updates
  aggregator = add_metric_update(aggregator, "request.count", 5.0)
  aggregator = add_metric_update(aggregator, "request.duration", 250.0)
  aggregator = add_metric_update(aggregator, "error.count", 1.0)
  aggregator = add_metric_update(aggregator, "new.metric", 42.0)
  
  // Process updates again
  aggregator = process_updates(aggregator)
  
  // Check updated metric values
  assert_eq(get_metric(aggregator, "request.count"), Some(6.0))
  assert_eq(get_metric(aggregator, "request.duration"), Some(350.0))
  assert_eq(get_metric(aggregator, "error.count"), Some(1.0))
  assert_eq(get_metric(aggregator, "new.metric"), Some(42.0))
  
  // Test non-existent metric
  assert_eq(get_metric(aggregator, "non.existent"), None)
}

// Test 6: Concurrent Log Writing
test "concurrent log writing with synchronization" {
  // Define log entry
  type LogEntry = {
    timestamp: Int,
    level: String,
    message: String,
    thread_id: Int
  }
  
  // Define concurrent log writer
  type LogWriter = {
    entries: Array[LogEntry],
    lock: Bool,
    buffer_size: Int,
    flush_threshold: Int
  }
  
  // Create log writer
  let create_log_writer = fn(buffer_size: Int, flush_threshold: Int) {
    {
      entries: [],
      lock: false,
      buffer_size,
      flush_threshold
    }
  }
  
  // Write log entry
  let write_log = fn(writer: LogWriter, level: String, message: String, thread_id: Int) {
    if not(writer.lock) && writer.entries.length() < writer.buffer_size {
      let entry = {
        timestamp: 1640995200,
        level,
        message,
        thread_id
      }
      
      {
        entries: writer.entries.push(entry),
        lock: writer.lock,
        buffer_size: writer.buffer_size,
        flush_threshold: writer.flush_threshold
      }
    } else {
      writer
    }
  }
  
  // Flush logs (simulate writing to disk)
  let flush_logs = fn(writer: LogWriter) {
    if not(writer.lock) {
      // In a real implementation, this would write to disk
      // For simulation, we just clear the buffer
      {
        entries: [],
        lock: writer.lock,
        buffer_size: writer.buffer_size,
        flush_threshold: writer.flush_threshold
      }
    } else {
      writer
    }
  }
  
  // Check if flush is needed
  let should_flush = fn(writer: LogWriter) {
    writer.entries.length() >= writer.flush_threshold
  }
  
  // Test concurrent log writing
  let mut log_writer = create_log_writer(100, 10)
  
  // Write logs from different threads (simulated)
  log_writer = write_log(log_writer, "INFO", "Thread 1 starting", 1)
  log_writer = write_log(log_writer, "INFO", "Thread 2 starting", 2)
  log_writer = write_log(log_writer, "DEBUG", "Thread 1 processing", 1)
  log_writer = write_log(log_writer, "WARN", "Thread 2 slow operation", 2)
  log_writer = write_log(log_writer, "INFO", "Thread 3 starting", 3)
  log_writer = write_log(log_writer, "ERROR", "Thread 1 failed", 1)
  log_writer = write_log(log_writer, "INFO", "Thread 2 completed", 2)
  log_writer = write_log(log_writer, "DEBUG", "Thread 3 processing", 3)
  log_writer = write_log(log_writer, "INFO", "Thread 3 completed", 3)
  log_writer = write_log(log_writer, "INFO", "Thread 1 retrying", 1)
  
  // Check if flush is needed
  assert_true(should_flush(log_writer))
  assert_eq(log_writer.entries.length(), 10)
  
  // Flush logs
  log_writer = flush_logs(log_writer)
  
  assert_eq(log_writer.entries.length(), 0)
  
  // Write more logs
  log_writer = write_log(log_writer, "INFO", "Thread 1 succeeded", 1)
  log_writer = write_log(log_writer, "INFO", "All threads completed", 0)
  
  assert_eq(log_writer.entries.length(), 2)
  assert_false(should_flush(log_writer))
  
  // Test log entry structure
  log_writer = write_log(log_writer, "ERROR", "Critical error", 1)
  let last_entry = log_writer.entries[log_writer.entries.length() - 1]
  assert_eq(last_entry.level, "ERROR")
  assert_eq(last_entry.message, "Critical error")
  assert_eq(last_entry.thread_id, 1)
  assert_eq(last_entry.timestamp, 1640995200)
}

// Test 7: Concurrent Trace Context Propagation
test "concurrent trace context propagation" {
  // Define trace context
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    baggage: Array[(String, String)],
    lock: Bool
  }
  
  // Create trace context
  let create_context = fn(trace_id: String, span_id: String) {
    {
      trace_id,
      span_id,
      parent_span_id: None,
      baggage: [],
      lock: false
    }
  }
  
  // Create child context
  let create_child_context = fn(parent: TraceContext, span_id: String) {
    if not(parent.lock) {
      {
        trace_id: parent.trace_id,
        span_id,
        parent_span_id: Some(parent.span_id),
        baggage: parent.baggage,
        lock: false
      }
    } else {
      parent
    }
  }
  
  // Add baggage item
  let add_baggage = fn(context: TraceContext, key: String, value: String) {
    if not(context.lock) {
      {
        trace_id: context.trace_id,
        span_id: context.span_id,
        parent_span_id: context.parent_span_id,
        baggage: context.baggage.push((key, value)),
        lock: false
      }
    } else {
      context
    }
  }
  
  // Get baggage item
  let get_baggage = fn(context: TraceContext, key: String) {
    for (k, v) in context.baggage {
      if k == key {
        return Some(v)
      }
    }
    None
  }
  
  // Test concurrent trace context propagation
  let root_context = create_context("trace-123", "span-root")
  
  // Add baggage to root context
  let root_with_baggage = add_baggage(root_context, "user.id", "user-456")
  let root_with_more_baggage = add_baggage(root_with_baggage, "request.id", "req-789")
  
  assert_eq(get_baggage(root_with_more_baggage, "user.id"), Some("user-456"))
  assert_eq(get_baggage(root_with_more_baggage, "request.id"), Some("req-789"))
  
  // Create child contexts (simulating concurrent operations)
  let child1 = create_child_context(root_with_more_baggage, "span-child1")
  let child2 = create_child_context(root_with_more_baggage, "span-child2")
  let child3 = create_child_context(root_with_more_baggage, "span-child3")
  
  // Test child context inheritance
  assert_eq(child1.trace_id, "trace-123")
  assert_eq(child1.span_id, "span-child1")
  assert_eq(child1.parent_span_id, Some("span-root"))
  assert_eq(get_baggage(child1, "user.id"), Some("user-456"))
  assert_eq(get_baggage(child1, "request.id"), Some("req-789"))
  
  assert_eq(child2.trace_id, "trace-123")
  assert_eq(child2.span_id, "span-child2")
  assert_eq(child2.parent_span_id, Some("span-root"))
  assert_eq(get_baggage(child2, "user.id"), Some("user-456"))
  assert_eq(get_baggage(child2, "request.id"), Some("req-789"))
  
  // Add baggage to child context
  let child1_with_baggage = add_baggage(child1, "operation.type", "database")
  let child2_with_baggage = add_baggage(child2, "operation.type", "cache")
  
  // Test child-specific baggage
  assert_eq(get_baggage(child1_with_baggage, "operation.type"), Some("database"))
  assert_eq(get_baggage(child2_with_baggage, "operation.type"), Some("cache"))
  
  // Create grandchild context
  let grandchild = create_child_context(child1_with_baggage, "span-grandchild")
  
  // Test grandchild context inheritance
  assert_eq(grandchild.trace_id, "trace-123")
  assert_eq(grandchild.span_id, "span-grandchild")
  assert_eq(grandchild.parent_span_id, Some("span-child1"))
  assert_eq(get_baggage(grandchild, "user.id"), Some("user-456"))
  assert_eq(get_baggage(grandchild, "request.id"), Some("req-789"))
  assert_eq(get_baggage(grandchild, "operation.type"), Some("database"))
}

// Test 8: Concurrent Resource Pool
test "concurrent resource pool for database connections" {
  // Define resource
  type Resource = {
    id: String,
    in_use: Bool,
    created_at: Int
  }
  
  // Define resource pool
  type ResourcePool = {
    resources: Array[Resource],
    max_size: Int,
    current_size: Int,
    lock: Bool
  }
  
  // Create resource pool
  let create_pool = fn(max_size: Int) {
    {
      resources: [],
      max_size,
      current_size: 0,
      lock: false
    }
  }
  
  // Acquire resource
  let acquire_resource = fn(pool: ResourcePool) {
    if not(pool.lock) {
      // Find available resource
      for i in 0..pool.resources.length() {
        if not(pool.resources[i].in_use) {
          let mut updated_resources = pool.resources
          updated_resources[i] = {
            id: updated_resources[i].id,
            in_use: true,
            created_at: updated_resources[i].created_at
          }
          
          return {
            resource: Some(updated_resources[i]),
            updated_pool: {
              resources: updated_resources,
              max_size: pool.max_size,
              current_size: pool.current_size,
              lock: false
            }
          }
        }
      }
      
      // No available resource, create new one if under limit
      if pool.current_size < pool.max_size {
        let new_resource = {
          id: "resource-" + pool.current_size.to_string(),
          in_use: true,
          created_at: 1640995200
        }
        
        {
          resource: Some(new_resource),
          updated_pool: {
            resources: pool.resources.push(new_resource),
            max_size: pool.max_size,
            current_size: pool.current_size + 1,
            lock: false
          }
        }
      } else {
        // Pool at capacity
        {
          resource: None,
          updated_pool: pool
        }
      }
    } else {
      {
        resource: None,
        updated_pool: pool
      }
    }
  }
  
  // Release resource
  let release_resource = fn(pool: ResourcePool, resource_id: String) {
    if not(pool.lock) {
      let mut updated_resources = pool.resources
      
      for i in 0..updated_resources.length() {
        if updated_resources[i].id == resource_id {
          updated_resources[i] = {
            id: updated_resources[i].id,
            in_use: false,
            created_at: updated_resources[i].created_at
          }
        }
      }
      
      {
        updated_pool: {
          resources: updated_resources,
          max_size: pool.max_size,
          current_size: pool.current_size,
          lock: false
        }
      }
    } else {
      {
        updated_pool: pool
      }
    }
  }
  
  // Test concurrent resource pool
  let mut pool = create_pool(3)
  
  // Test empty pool
  assert_eq(pool.current_size, 0)
  assert_eq(pool.resources.length(), 0)
  
  // Acquire resources
  let acquire1 = acquire_resource(pool)
  assert_true(acquire1.resource.is_some())
  assert_eq(acquire1.updated_pool.current_size, 1)
  pool = acquire1.updated_pool
  
  let acquire2 = acquire_resource(pool)
  assert_true(acquire2.resource.is_some())
  assert_eq(acquire2.updated_pool.current_size, 2)
  pool = acquire2.updated_pool
  
  let acquire3 = acquire_resource(pool)
  assert_true(acquire3.resource.is_some())
  assert_eq(acquire3.updated_pool.current_size, 3)
  pool = acquire3.updated_pool
  
  // Pool should be at capacity
  let acquire4 = acquire_resource(pool)
  assert_false(acquire4.resource.is_some())
  assert_eq(acquire4.updated_pool.current_size, 3)
  
  // Release a resource
  match acquire1.resource {
    Some(resource) => {
      let release_result = release_resource(pool, resource.id)
      pool = release_result.updated_pool
    }
    None => ()
  }
  
  // Now we can acquire again
  let acquire5 = acquire_resource(pool)
  assert_true(acquire5.resource.is_some())
  assert_eq(acquire5.updated_pool.current_size, 3)
  pool = acquire5.updated_pool
  
  // Test resource reuse
  match acquire5.resource {
    Some(resource) => {
      assert_true(resource.in_use)
      assert_true(resource.id == "resource-0")  // Should reuse the first resource
    }
    None => assert_true(false)
  }
}

// Test 9: Concurrent Batch Processing
test "concurrent batch processing for telemetry data" {
  // Define batch processor
  type BatchProcessor = {
    batches: Array[Array[String]],
    processing_batches: Array[Array[String]],
    completed_batches: Array[Array[String]],
    max_concurrent: Int,
    batch_size: Int,
    lock: Bool
  }
  
  // Create batch processor
  let create_processor = fn(max_concurrent: Int, batch_size: Int) {
    {
      batches: [],
      processing_batches: [],
      completed_batches: [],
      max_concurrent,
      batch_size,
      lock: false
    }
  }
  
  // Add data to processor
  let add_data = fn(processor: BatchProcessor, data: Array[String]) {
    if not(processor.lock) {
      let mut batches = processor.batches
      let mut current_batch = []
      
      for i in 0..data.length() {
        current_batch = current_batch.push(data[i])
        
        if current_batch.length() >= processor.batch_size {
          batches = batches.push(current_batch)
          current_batch = []
        }
      }
      
      // Add remaining items
      if current_batch.length() > 0 {
        batches = batches.push(current_batch)
      }
      
      {
        batches,
        processing_batches: processor.processing_batches,
        completed_batches: processor.completed_batches,
        max_concurrent: processor.max_concurrent,
        batch_size: processor.batch_size,
        lock: false
      }
    } else {
      processor
    }
  }
  
  // Start processing batch
  let start_batch_processing = fn(processor: BatchProcessor) {
    if not(processor.lock) && 
       processor.processing_batches.length() < processor.max_concurrent && 
       processor.batches.length() > 0 {
      let batch = processor.batches[0]
      let remaining_batches = processor.batches.slice(1, processor.batches.length())
      
      {
        batches: remaining_batches,
        processing_batches: processor.processing_batches.push(batch),
        completed_batches: processor.completed_batches,
        max_concurrent: processor.max_concurrent,
        batch_size: processor.batch_size,
        lock: false
      }
    } else {
      processor
    }
  }
  
  // Complete batch processing
  let complete_batch_processing = fn(processor: BatchProcessor, batch_index: Int) {
    if not(processor.lock) && batch_index < processor.processing_batches.length() {
      let completed_batch = processor.processing_batches[batch_index]
      let mut remaining_processing = []
      
      for i in 0..processor.processing_batches.length() {
        if i != batch_index {
          remaining_processing = remaining_processing.push(processor.processing_batches[i])
        }
      }
      
      {
        batches: processor.batches,
        processing_batches: remaining_processing,
        completed_batches: processor.completed_batches.push(completed_batch),
        max_concurrent: processor.max_concurrent,
        batch_size: processor.batch_size,
        lock: false
      }
    } else {
      processor
    }
  }
  
  // Test concurrent batch processing
  let mut processor = create_processor(2, 3)
  
  // Add data
  let data = ["item-1", "item-2", "item-3", "item-4", "item-5", "item-6", "item-7", "item-8", "item-9", "item-10"]
  processor = add_data(processor, data)
  
  // Should create 4 batches: [1,2,3], [4,5,6], [7,8,9], [10]
  assert_eq(processor.batches.length(), 4)
  assert_eq(processor.batches[0], ["item-1", "item-2", "item-3"])
  assert_eq(processor.batches[1], ["item-4", "item-5", "item-6"])
  assert_eq(processor.batches[2], ["item-7", "item-8", "item-9"])
  assert_eq(processor.batches[3], ["item-10"])
  
  // Start processing batches
  processor = start_batch_processing(processor)
  assert_eq(processor.batches.length(), 3)
  assert_eq(processor.processing_batches.length(), 1)
  assert_eq(processor.processing_batches[0], ["item-1", "item-2", "item-3"])
  
  processor = start_batch_processing(processor)
  assert_eq(processor.batches.length(), 2)
  assert_eq(processor.processing_batches.length(), 2)
  assert_eq(processor.processing_batches[1], ["item-4", "item-5", "item-6"])
  
  // Try to start another batch (should be at limit)
  processor = start_batch_processing(processor)
  assert_eq(processor.batches.length(), 2)
  assert_eq(processor.processing_batches.length(), 2)
  
  // Complete first batch
  processor = complete_batch_processing(processor, 0)
  assert_eq(processor.processing_batches.length(), 1)
  assert_eq(processor.completed_batches.length(), 1)
  assert_eq(processor.completed_batches[0], ["item-1", "item-2", "item-3"])
  
  // Start another batch
  processor = start_batch_processing(processor)
  assert_eq(processor.batches.length(), 1)
  assert_eq(processor.processing_batches.length(), 2)
  assert_eq(processor.processing_batches[1], ["item-7", "item-8", "item-9"])
  
  // Complete second batch
  processor = complete_batch_processing(processor, 0)
  assert_eq(processor.processing_batches.length(), 1)
  assert_eq(processor.completed_batches.length(), 2)
  assert_eq(processor.completed_batches[1], ["item-4", "item-5", "item-6"])
  
  // Complete third batch
  processor = complete_batch_processing(processor, 0)
  assert_eq(processor.processing_batches.length(), 0)
  assert_eq(processor.completed_batches.length(), 3)
  assert_eq(processor.completed_batches[2], ["item-7", "item-8", "item-9"])
  
  // Start last batch
  processor = start_batch_processing(processor)
  assert_eq(processor.batches.length(), 0)
  assert_eq(processor.processing_batches.length(), 1)
  assert_eq(processor.processing_batches[0], ["item-10"])
  
  // Complete last batch
  processor = complete_batch_processing(processor, 0)
  assert_eq(processor.processing_batches.length(), 0)
  assert_eq(processor.completed_batches.length(), 4)
  assert_eq(processor.completed_batches[3], ["item-10"])
}

// Test 10: Concurrent Cache with Synchronization
test "concurrent cache with synchronization" {
  // Define cache entry
  type CacheEntry = {
    key: String,
    value: String,
    expiry_time: Int,
    access_count: Int
  }
  
  // Define concurrent cache
  type ConcurrentCache = {
    entries: Array[CacheEntry],
    lock: Bool,
    max_size: Int
  }
  
  // Create concurrent cache
  let create_cache = fn(max_size: Int) {
    {
      entries: [],
      lock: false,
      max_size
    }
  }
  
  // Get from cache
  let cache_get = fn(cache: ConcurrentCache, key: String, current_time: Int) {
    if not(cache.lock) {
      for i in 0..cache.entries.length() {
        if cache.entries[i].key == key && cache.entries[i].expiry_time > current_time {
          // Valid entry found
          let mut updated_entries = cache.entries
          updated_entries[i] = {
            key: updated_entries[i].key,
            value: updated_entries[i].value,
            expiry_time: updated_entries[i].expiry_time,
            access_count: updated_entries[i].access_count + 1
          }
          
          return {
            value: Some(updated_entries[i].value),
            updated_cache: {
              entries: updated_entries,
              lock: false,
              max_size: cache.max_size
            }
          }
        }
      }
      
      // Entry not found or expired
      {
        value: None,
        updated_cache: cache
      }
    } else {
      {
        value: None,
        updated_cache: cache
      }
    }
  }
  
  // Put in cache
  let cache_put = fn(cache: ConcurrentCache, key: String, value: String, ttl: Int, current_time: Int) {
    if not(cache.lock) {
      // Check if key already exists
      let mut key_exists = false
      for entry in cache.entries {
        if entry.key == key {
          key_exists = true
        }
      }
      
      if key_exists {
        // Update existing entry
        let mut updated_entries = cache.entries
        for i in 0..updated_entries.length() {
          if updated_entries[i].key == key {
            updated_entries[i] = {
              key,
              value,
              expiry_time: current_time + ttl,
              access_count: updated_entries[i].access_count + 1
            }
          }
        }
        
        {
          updated_cache: {
            entries: updated_entries,
            lock: false,
            max_size: cache.max_size
          }
        }
      } else {
        // Add new entry
        if cache.entries.length() < cache.max_size {
          // Space available
          {
            updated_cache: {
              entries: cache.entries.push({
                key,
                value,
                expiry_time: current_time + ttl,
                access_count: 1
              }),
              lock: false,
              max_size: cache.max_size
            }
          }
        } else {
          // Need to evict (simplified LRU)
          let mut min_access_count = 999999
          let mut evict_index = -1
          
          for i in 0..cache.entries.length() {
            if cache.entries[i].access_count < min_access_count {
              min_access_count = cache.entries[i].access_count
              evict_index = i
            }
          }
          
          let mut updated_entries = []
          for i in 0..cache.entries.length() {
            if i != evict_index {
              updated_entries = updated_entries.push(cache.entries[i])
            }
          }
          
          {
            updated_cache: {
              entries: updated_entries.push({
                key,
                value,
                expiry_time: current_time + ttl,
                access_count: 1
              }),
              lock: false,
              max_size: cache.max_size
            }
          }
        }
      }
    } else {
      {
        updated_cache: cache
      }
    }
  }
  
  // Test concurrent cache
  let mut cache = create_cache(3)
  
  // Test empty cache
  let empty_result = cache_get(cache, "key1", 1640995200)
  assert_eq(empty_result.value, None)
  cache = empty_result.updated_cache
  
  // Put entries in cache
  cache = cache_put(cache, "key1", "value1", 3600, 1640995200)
  cache = cache_put(cache, "key2", "value2", 3600, 1640995200)
  cache = cache_put(cache, "key3", "value3", 3600, 1640995200)
  
  assert_eq(cache.entries.length(), 3)
  
  // Get entries from cache
  let get1 = cache_get(cache, "key1", 1640995200)
  assert_eq(get1.value, Some("value1"))
  cache = get1.updated_cache
  
  let get2 = cache_get(cache, "key2", 1640995200)
  assert_eq(get2.value, Some("value2"))
  cache = get2.updated_cache
  
  // Update existing entry
  cache = cache_put(cache, "key1", "newvalue1", 3600, 1640995200)
  
  let get1_updated = cache_get(cache, "key1", 1640995200)
  assert_eq(get1_updated.value, Some("newvalue1"))
  cache = get1_updated.updated_cache
  
  // Test cache eviction
  cache = cache_put(cache, "key4", "value4", 3600, 1640995200)  // Should evict least used
  
  assert_eq(cache.entries.length(), 3)
  
  // Check that key3 was evicted (least accessed)
  let get3 = cache_get(cache, "key3", 1640995200)
  assert_eq(get3.value, None)
  
  // Check that key4 is in cache
  let get4 = cache_get(cache, "key4", 1640995200)
  assert_eq(get4.value, Some("value4"))
  
  // Test expiration
  let expired_result = cache_get(cache, "key1", 1640995200 + 7200)  // 2 hours later
  assert_eq(expired_result.value, None)
}