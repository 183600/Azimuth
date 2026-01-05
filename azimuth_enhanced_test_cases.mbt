// Azimuth Enhanced Test Cases
// This file contains additional MoonBit test cases focusing on advanced features

// Test 1: Recursive Functions and Algorithms
test "recursive functions and algorithms" {
  // Factorial using recursion
  let factorial = fn(n: Int) {
    if n <= 1 {
      1
    } else {
      n * factorial(n - 1)
    }
  }
  
  assert_eq(factorial(0), 1)
  assert_eq(factorial(1), 1)
  assert_eq(factorial(5), 120)
  assert_eq(factorial(7), 5040)
  
  // Fibonacci sequence
  let fibonacci = fn(n: Int) {
    if n <= 1 {
      n
    } else {
      fibonacci(n - 1) + fibonacci(n - 2)
    }
  }
  
  assert_eq(fibonacci(0), 0)
  assert_eq(fibonacci(1), 1)
  assert_eq(fibonacci(5), 5)
  assert_eq(fibonacci(7), 13)
  
  // Greatest common divisor
  let gcd = fn(a: Int, b: Int) {
    if b == 0 {
      a
    } else {
      gcd(b, a % b)
    }
  }
  
  assert_eq(gcd(48, 18), 6)
  assert_eq(gcd(17, 5), 1)
  assert_eq(gcd(100, 25), 25)
}

// Test 2: Advanced Pattern Matching
test "advanced pattern matching with guards" {
  enum TelemetryEvent {
    SpanCreated(String, Int)
    SpanEnded(String, Int, Int)
    MetricRecorded(String, Float, String)
    ErrorOccurred(String, String)
  }
  
  let process_event = fn(event: TelemetryEvent) {
    match event {
      TelemetryEvent::SpanCreated(name, timestamp) if timestamp > 1000 => 
        "Recent span: " + name
      TelemetryEvent::SpanCreated(name, _) => 
        "Old span: " + name
      TelemetryEvent::SpanEnded(name, start, end) if end - start > 1000 => 
        "Long span: " + name
      TelemetryEvent::SpanEnded(name, _, _) => 
        "Quick span: " + name
      TelemetryEvent::MetricRecorded(name, value, unit) if value > 100.0 => 
        "High metric: " + name + " (" + value.to_string() + unit + ")"
      TelemetryEvent::MetricRecorded(name, value, unit) => 
        "Normal metric: " + name + " (" + value.to_string() + unit + ")"
      TelemetryEvent::ErrorOccurred(message, severity) if severity == "critical" => 
        "CRITICAL: " + message
      TelemetryEvent::ErrorOccurred(message, _) => 
        "Error: " + message
    }
  }
  
  assert_eq(process_event(TelemetryEvent::SpanCreated("operation", 2000)), "Recent span: operation")
  assert_eq(process_event(TelemetryEvent::SpanCreated("old_op", 500)), "Old span: old_op")
  assert_eq(process_event(TelemetryEvent::SpanEnded("slow_op", 1000, 2500)), "Long span: slow_op")
  assert_eq(process_event(TelemetryEvent::SpanEnded("fast_op", 1000, 1200)), "Quick span: fast_op")
  assert_eq(process_event(TelemetryEvent::MetricRecorded("cpu", 85.5, "%")), "Normal metric: cpu (85.5%)")
  assert_eq(process_event(TelemetryEvent::MetricRecorded("memory", 150.0, "MB")), "High metric: memory (150.0MB)")
  assert_eq(process_event(TelemetryEvent::ErrorOccurred("system crash", "critical")), "CRITICAL: system crash")
  assert_eq(process_event(TelemetryEvent::ErrorOccurred("timeout", "warning")), "Error: timeout")
}

// Test 3: State Management and Immutability
test "state management and immutability patterns" {
  // Simulate a telemetry counter with immutable updates
  type Counter = {
    value: Int,
    name: String,
    updated_at: Int
  }
  
  let create_counter = fn(name: String) {
    {
      value: 0,
      name,
      updated_at: 1640995200
    }
  }
  
  let increment = fn(counter: Counter) {
    {
      value: counter.value + 1,
      name: counter.name,
      updated_at: 1640995201
    }
  }
  
  let add = fn(counter: Counter, amount: Int) {
    {
      value: counter.value + amount,
      name: counter.name,
      updated_at: 1640995202
    }
  }
  
  let counter = create_counter("requests")
  assert_eq(counter.value, 0)
  assert_eq(counter.name, "requests")
  
  let counter1 = increment(counter)
  assert_eq(counter1.value, 1)
  assert_eq(counter.value, 0)  // Original unchanged
  
  let counter2 = add(counter1, 5)
  assert_eq(counter2.value, 6)
  assert_eq(counter1.value, 1)  // Previous unchanged
  
  // Chain operations
  let counter3 = counter
    |> increment
    |> increment
    |> fn(c) { add(c, 10) }
  
  assert_eq(counter3.value, 12)
  assert_eq(counter.value, 0)  // Original still unchanged
}

// Test 4: Pipeline and Function Composition
test "pipeline operations and function composition" {
  // Define pipeline operator simulation
  let pipeline = fn(value, operations) {
    let mut result = value
    for op in operations {
      result = op(result)
    }
    result
  }
  
  // Define transformation functions
  let add = fn(x: Int) { x + 1 }
  let multiply = fn(x: Int) { x * 2 }
  let square = fn(x: Int) { x * x }
  let to_string = fn(x: Int) { x.to_string() }
  
  // Test pipeline with individual functions
  let operations = [add, multiply, square]
  let result = pipeline(5, operations)
  assert_eq(result, 144)  // ((5 + 1) * 2)^2 = 144
  
  // Test pipeline with different types
  let string_ops = [add, multiply, to_string]
  let string_result = pipeline(3, string_ops)
  assert_eq(string_result, "8")  // (3 + 1) * 2 = 8, then to string
  
  // Function composition
  let compose = fn(f, g) { fn(x) { f(g(x)) } }
  let add_then_multiply = compose(multiply, add)
  assert_eq(add_then_multiply(5), 12)  // (5 + 1) * 2 = 12
  
  let square_of_double = compose(square, multiply)
  assert_eq(square_of_double(4), 64)  // (4 * 2)^2 = 64
  
  // Multi-level composition
  let complex_composition = compose(to_string, compose(square, compose(multiply, add)))
  assert_eq(complex_composition(2), "36")  // ((2 + 1) * 2)^2 = 36, then to string
}

// Test 5: Generics and Type Parameters
test "generics and type parameter operations" {
  // Generic container type
  type Container[T] = {
    items: Array[T],
    count: Int
  }
  
  // Generic functions
  let create_container = fn() {
    {
      items: [],
      count: 0
    }
  }
  
  let add_item = fn(container: Container[T], item: T) {
    {
      items: container.items.push(item),
      count: container.count + 1
    }
  }
  
  let find_item = fn(container: Container[T], predicate: (T) -> Bool) {
    let mut found = None
    for item in container.items {
      if predicate(item) {
        found = Some(item)
      }
    }
    found
  }
  
  // Test with Int type
  let int_container = create_container()
    |> add_item(1)
    |> add_item(2)
    |> add_item(3)
    |> add_item(4)
    |> add_item(5)
  
  assert_eq(int_container.count, 5)
  
  let even_number = find_item(int_container, fn(x) { x % 2 == 0 })
  assert_eq(even_number, Some(2))
  
  let large_number = find_item(int_container, fn(x) { x > 10 })
  assert_eq(large_number, None)
  
  // Test with String type
  let string_container = create_container()
    |> add_item("trace-1")
    |> add_item("trace-2")
    |> add_item("span-1")
    |> add_item("span-2")
  
  assert_eq(string_container.count, 4)
  
  let trace_item = find_item(string_container, fn(s) { s.starts_with("trace") })
  assert_eq(trace_item, Some("trace-1"))
  
  let metric_item = find_item(string_container, fn(s) { s.starts_with("metric") })
  assert_eq(metric_item, None)
  
  // Test with custom type
  type Span = {
    id: String,
    duration: Int
  }
  
  let span_container = create_container()
    |> add_item({ id: "span-1", duration: 100 })
    |> add_item({ id: "span-2", duration: 200 })
    |> add_item({ id: "span-3", duration: 50 })
  
  assert_eq(span_container.count, 3)
  
  let slow_span = find_item(span_container, fn(s) { s.duration > 150 })
  match slow_span {
    Some(span) => {
      assert_eq(span.id, "span-2")
      assert_eq(span.duration, 200)
    }
    None => assert_true(false)
  }
}

// Test 6: Async Operations Simulation
test "async operations simulation" {
  // Simulate async operation with callback
  type AsyncResult[T] = {
    completed: Bool,
    value: Option[T],
    error: Option[String]
  }
  
  let create_pending = fn() {
    {
      completed: false,
      value: None,
      error: None
    }
  }
  
  let create_success = fn(value: T) {
    {
      completed: true,
      value: Some(value),
      error: None
    }
  }
  
  let create_error = fn(error: String) {
    {
      completed: true,
      value: None,
      error: Some(error)
    }
  }
  
  // Simulate async telemetry data fetch
  let fetch_telemetry_data = fn(trace_id: String) {
    if trace_id.starts_with("valid-") {
      create_success({
        trace_id: trace_id,
        span_count: 5,
        duration: 250,
        status: "success"
      })
    } else {
      create_error("Invalid trace ID: " + trace_id)
    }
  }
  
  // Test successful fetch
  let valid_result = fetch_telemetry_data("valid-123")
  assert_true(valid_result.completed)
  assert_false(valid_result.error.is_some())
  
  match valid_result.value {
    Some(data) => {
      assert_eq(data.trace_id, "valid-123")
      assert_eq(data.span_count, 5)
      assert_eq(data.duration, 250)
      assert_eq(data.status, "success")
    }
    None => assert_true(false)
  }
  
  // Test error case
  let invalid_result = fetch_telemetry_data("invalid-123")
  assert_true(invalid_result.completed)
  assert_true(invalid_result.value.is_none())
  
  match invalid_result.error {
    Some(error) => assert_eq(error, "Invalid trace ID: invalid-123")
    None => assert_true(false)
  }
  
  // Simulate async operation chaining
  let chain_operations = fn(trace_id: String) {
    let fetch_result = fetch_telemetry_data(trace_id)
    
    match fetch_result.completed {
      true => {
        match fetch_result.value {
          Some(data) => {
            if data.duration > 200 {
              create_success("Slow operation detected: " + data.trace_id)
            } else {
              create_success("Normal operation: " + data.trace_id)
            }
          }
          None => create_error("No data available")
        }
      }
      false => create_error("Operation not completed")
    }
  }
  
  let slow_operation = chain_operations("valid-456")
  assert_true(slow_operation.completed)
  match slow_operation.value {
    Some(message) => assert_eq(message, "Slow operation detected: valid-456")
    None => assert_true(false)
  }
  
  let normal_operation = chain_operations("valid-789")
  assert_true(normal_operation.completed)
  match normal_operation.value {
    Some(message) => assert_eq(message, "Normal operation: valid-789")
    None => assert_true(false)
  }
}

// Test 7: Memory Management and Resource Handling
test "memory management and resource handling" {
  // Simulate resource management
  type Resource[T] = {
    id: String,
    data: T,
    acquired: Bool,
    ref_count: Int
  }
  
  let create_resource = fn(id: String, data: T) {
    {
      id,
      data,
      acquired: false,
      ref_count: 0
    }
  }
  
  let acquire = fn(resource: Resource[T]) {
    {
      id: resource.id,
      data: resource.data,
      acquired: true,
      ref_count: resource.ref_count + 1
    }
  }
  
  let release = fn(resource: Resource[T]) {
    if resource.ref_count > 0 {
      {
        id: resource.id,
        data: resource.data,
        acquired: resource.ref_count > 1,
        ref_count: resource.ref_count - 1
      }
    } else {
      resource
    }
  }
  
  // Test resource lifecycle
  let file_resource = create_resource("file-1", "important data")
  assert_false(file_resource.acquired)
  assert_eq(file_resource.ref_count, 0)
  
  let acquired_once = acquire(file_resource)
  assert_true(acquired_once.acquired)
  assert_eq(acquired_once.ref_count, 1)
  
  let acquired_twice = acquire(acquired_once)
  assert_true(acquired_twice.acquired)
  assert_eq(acquired_twice.ref_count, 2)
  
  let released_once = release(acquired_twice)
  assert_true(released_once.acquired)  // Still acquired due to second reference
  assert_eq(released_once.ref_count, 1)
  
  let released_twice = release(released_once)
  assert_false(released_twice.acquired)  // No more references
  assert_eq(released_twice.ref_count, 0)
  
  // Test resource pool
  type ResourcePool[T] = {
    available: Array[Resource[T]],
    in_use: Array[Resource[T]]
  }
  
  let create_pool = fn() {
    {
      available: [],
      in_use: []
    }
  }
  
  let add_to_pool = fn(pool: ResourcePool[T], resource: Resource[T]) {
    {
      available: pool.available.push(resource),
      in_use: pool.in_use
    }
  }
  
  let checkout = fn(pool: ResourcePool[T]) {
    match pool.available.length() {
      0 => (pool, None)
      _ => {
        let resource = pool.available[0]
        let remaining = pool.available.slice(1)
        let acquired_resource = acquire(resource)
        (
          {
            available: remaining,
            in_use: pool.in_use.push(acquired_resource)
          },
          Some(acquired_resource)
        )
      }
    }
  }
  
  let checkin = fn(pool: ResourcePool[T], resource: Resource[T]) {
    let released_resource = release(resource)
    (
      {
        available: pool.available.push(released_resource),
        in_use: pool.in_use.filter(fn(r) { r.id != resource.id })
      },
      released_resource
    )
  }
  
  // Test pool operations
  let initial_pool = create_pool()
    |> add_to_pool(create_resource("res-1", "data1"))
    |> add_to_pool(create_resource("res-2", "data2"))
    |> add_to_pool(create_resource("res-3", "data3"))
  
  assert_eq(initial_pool.available.length(), 3)
  assert_eq(initial_pool.in_use.length(), 0)
  
  let (pool_after_checkout, checked_out) = checkout(initial_pool)
  assert_eq(pool_after_checkout.available.length(), 2)
  assert_eq(pool_after_checkout.in_use.length(), 1)
  
  match checked_out {
    Some(resource) => {
      assert_eq(resource.id, "res-1")
      assert_true(resource.acquired)
    }
    None => assert_true(false)
  }
  
  match checked_out {
    Some(resource) => {
      let (pool_after_checkin, _) = checkin(pool_after_checkout, resource)
      assert_eq(pool_after_checkin.available.length(), 3)
      assert_eq(pool_after_checkin.in_use.length(), 0)
    }
    None => assert_true(false)
  }
}

// Test 8: Advanced Data Structures
test "advanced data structures" {
  // Simulate a stack
  type Stack[T] = {
    items: Array[T],
    size: Int
  }
  
  let create_stack = fn() {
    {
      items: [],
      size: 0
    }
  }
  
  let push = fn(stack: Stack[T], item: T) {
    {
      items: stack.items.push(item),
      size: stack.size + 1
    }
  }
  
  let pop = fn(stack: Stack[T]) {
    match stack.size {
      0 => (stack, None)
      _ => {
        let top = stack.items[stack.size - 1]
        let remaining = stack.items.slice(0, stack.size - 1)
        (
          {
            items: remaining,
            size: stack.size - 1
          },
          Some(top)
        )
      }
    }
  }
  
  let peek = fn(stack: Stack[T]) {
    match stack.size {
      0 => None
      _ => Some(stack.items[stack.size - 1])
    }
  }
  
  // Test stack operations
  let empty_stack = create_stack()
  assert_eq(empty_stack.size, 0)
  assert_eq(peek(empty_stack), None)
  
  let stack_with_items = empty_stack
    |> push(10)
    |> push(20)
    |> push(30)
  
  assert_eq(stack_with_items.size, 3)
  assert_eq(peek(stack_with_items), Some(30))
  
  let (stack_after_pop, popped_item) = pop(stack_with_items)
  assert_eq(popped_item, Some(30))
  assert_eq(stack_after_pop.size, 2)
  assert_eq(peek(stack_after_pop), Some(20))
  
  // Simulate a queue
  type Queue[T] = {
    items: Array[T],
    front: Int,
    back: Int
  }
  
  let create_queue = fn() {
    {
      items: [],
      front: 0,
      back: 0
    }
  }
  
  let enqueue = fn(queue: Queue[T], item: T) {
    {
      items: queue.items.push(item),
      front: queue.front,
      back: queue.back + 1
    }
  }
  
  let dequeue = fn(queue: Queue[T]) {
    match queue.front >= queue.back {
      true => (queue, None)
      false => {
        let front_item = queue.items[queue.front]
        (
          {
            items: queue.items,
            front: queue.front + 1,
            back: queue.back
          },
          Some(front_item)
        )
      }
    }
  }
  
  // Test queue operations
  let empty_queue = create_queue()
  let (empty_queue_after_dequeue, empty_dequeued) = dequeue(empty_queue)
  assert_eq(empty_dequeued, None)
  
  let queue_with_items = empty_queue
    |> enqueue("first")
    |> enqueue("second")
    |> enqueue("third")
  
  let (queue_after_dequeue, dequeued_item) = dequeue(queue_with_items)
  assert_eq(dequeued_item, Some("first"))
  
  let (queue_after_dequeue2, dequeued_item2) = dequeue(queue_after_dequeue)
  assert_eq(dequeued_item2, Some("second"))
  
  let (queue_after_dequeue3, dequeued_item3) = dequeue(queue_after_dequeue2)
  assert_eq(dequeued_item3, Some("third"))
  
  let (final_queue, final_dequeued) = dequeue(queue_after_dequeue3)
  assert_eq(final_dequeued, None)
}

// Test 9: Performance Optimization Techniques
test "performance optimization techniques" {
  // Memoization simulation
  type MemoCache[T, U] = {
    cache: Array[(T, U)],
    hits: Int,
    misses: Int
  }
  
  let create_cache = fn() {
    {
      cache: [],
      hits: 0,
      misses: 0
    }
  }
  
  let memoize = fn(cache: MemoCache[T, U], key: T, computation: (T) -> U) {
    let mut found = None
    for (k, v) in cache.cache {
      if k == key {
        found = Some(v)
      }
    }
    
    match found {
      Some(value) => ({
        cache: cache.cache,
        hits: cache.hits + 1,
        misses: cache.misses
      }, value)
      None => {
        let computed = computation(key)
        ({
          cache: cache.cache.push((key, computed)),
          hits: cache.hits,
          misses: cache.misses + 1
        }, computed)
      }
    }
  }
  
  // Test memoization with expensive computation
  let fibonacci_with_memo = fn(n: Int, cache: MemoCache[Int, Int]) {
    if n <= 1 {
      (cache, n)
    } else {
      let (cache1, fib1) = memoize(cache, n - 1, fn(m) { fibonacci_with_memo(m, cache1).1 })
      let (cache2, fib2) = memoize(cache1, n - 2, fn(m) { fibonacci_with_memo(m, cache2).1 })
      (cache2, fib1 + fib2)
    }
  }
  
  let initial_cache = create_cache()
  let (final_cache, fib_10) = fibonacci_with_memo(10, initial_cache)
  assert_eq(fib_10, 55)
  assert_true(final_cache.hits > 0)  // Should have cache hits
  assert_true(final_cache.misses > 0)  // Should have cache misses
  
  // Batch processing
  let batch_process = fn(items: Array[T], batch_size: Int, processor: (Array[T]) -> Array[U]) {
    let mut results = []
    let mut i = 0
    
    while i < items.length() {
      let end = if i + batch_size < items.length() { i + batch_size } else { items.length() }
      let batch = items.slice(i, end - i)
      results = results + processor(batch)
      i = i + batch_size
    }
    
    results
  }
  
  // Test batch processing
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let batched_sums = batch_process(numbers, 3, fn(batch) {
    let sum = batch.reduce(fn(acc, x) { acc + x }, 0)
    [sum]
  })
  
  assert_eq(batched_sums, [6, 15, 24, 10])  // [1+2+3, 4+5+6, 7+8+9, 10]
  
  // Lazy evaluation for performance
  let lazy_filter = fn(items: Array[T], predicate: (T) -> Bool) {
    let mut filtered = []
    for item in items {
      if predicate(item) {
        filtered = filtered.push(item)
      }
    }
    filtered
  }
  
  let even_numbers = lazy_filter(numbers, fn(x) { x % 2 == 0 })
  assert_eq(even_numbers, [2, 4, 6, 8, 10])
}

// Test 10: Advanced Error Recovery
test "advanced error recovery strategies" {
  // Define error types
  enum ProcessingError {
    NetworkError(String)
    DataError(String)
    TimeoutError(Int)
    RetryExhaustedError(Int)
  }
  
  // Define result type with retry capability
  type ProcessingResult[T] = {
    success: Bool,
    data: Option[T],
    error: Option[ProcessingError],
    attempts: Int
  }
  
  let create_success_result = fn(data: T, attempts: Int) {
    {
      success: true,
      data: Some(data),
      error: None,
      attempts
    }
  }
  
  let create_error_result = fn(error: ProcessingError, attempts: Int) {
    {
      success: false,
      data: None,
      error: Some(error),
      attempts
    }
  }
  
  // Simulate unreliable operation
  let unreliable_operation = fn(attempt: Int) {
    if attempt == 1 {
      create_error_result(ProcessingError::NetworkError("Connection failed"), 1)
    } else if attempt == 2 {
      create_error_result(ProcessingError::TimeoutError(5000), 2)
    } else if attempt == 3 {
      create_success_result("Operation completed", 3)
    } else {
      create_error_result(ProcessingError::DataError("Unexpected failure"), attempt)
    }
  }
  
  // Retry with exponential backoff
  let retry_with_backoff = fn(operation: (Int) -> ProcessingResult[String], max_attempts: Int) {
    let mut attempt = 1
    let mut result = operation(attempt)
    
    while not(result.success) and attempt < max_attempts {
      attempt = attempt + 1
      let backoff = 100 * (2 ^ (attempt - 1))  // Exponential backoff
      // In real implementation, we would wait for backoff milliseconds
      result = operation(attempt)
    }
    
    if not(result.success) {
      create_error_result(ProcessingError::RetryExhaustedError(max_attempts), max_attempts)
    } else {
      result
    }
  }
  
  // Test retry mechanism
  let retry_result = retry_with_backoff(unreliable_operation, 5)
  assert_true(retry_result.success)
  assert_eq(retry_result.data, Some("Operation completed"))
  assert_eq(retry_result.attempts, 3)
  
  // Test retry exhaustion
  let always_failing = fn(attempt: Int) {
    create_error_result(ProcessingError::NetworkError("Always fails"), attempt)
  }
  
  let exhausted_result = retry_with_backoff(always_failing, 3)
  assert_false(exhausted_result.success)
  assert_eq(exhausted_result.attempts, 3)
  
  match exhausted_result.error {
    Some(ProcessingError::RetryExhaustedError(attempts)) => assert_eq(attempts, 3)
    _ => assert_true(false)
  }
  
  // Circuit breaker pattern
  type CircuitBreaker = {
    state: String,  // "closed", "open", "half-open"
    failure_count: Int,
    failure_threshold: Int,
    next_attempt: Int
  }
  
  let create_circuit_breaker = fn(threshold: Int) {
    {
      state: "closed",
      failure_count: 0,
      failure_threshold: threshold,
      next_attempt: 0
    }
  }
  
  let call_with_circuit_breaker = fn(
    operation: () -> ProcessingResult[String],
    breaker: CircuitBreaker,
    current_time: Int
  ) {
    match breaker.state {
      "open" => {
        if current_time >= breaker.next_attempt {
          // Try half-open state
          let result = operation()
          match result.success {
            true => ({
              state: "closed",
              failure_count: 0,
              failure_threshold: breaker.failure_threshold,
              next_attempt: 0
            }, result)
            false => ({
              state: "open",
              failure_count: breaker.failure_count + 1,
              failure_threshold: breaker.failure_threshold,
              next_attempt: current_time + 60000  // 1 minute timeout
            }, result)
          }
        } else {
          (breaker, create_error_result(ProcessingError::NetworkError("Circuit breaker open"), 0))
        }
      }
      "closed" => {
        let result = operation()
        match result.success {
          true => (breaker, result)
          false => {
            let new_failure_count = breaker.failure_count + 1
            if new_failure_count >= breaker.failure_threshold {
              ({
                state: "open",
                failure_count: new_failure_count,
                failure_threshold: breaker.failure_threshold,
                next_attempt: current_time + 60000  // 1 minute timeout
              }, result)
            } else {
              ({
                state: "closed",
                failure_count: new_failure_count,
                failure_threshold: breaker.failure_threshold,
                next_attempt: 0
              }, result)
            }
          }
        }
      }
      _ => (breaker, create_error_result(ProcessingError::DataError("Unknown circuit breaker state"), 0))
    }
  }
  
  // Test circuit breaker
  let initial_breaker = create_circuit_breaker(3)
  let successful_op = fn() { create_success_result("Success", 1) }
  let failing_op = fn() { create_error_result(ProcessingError::NetworkError("Service down"), 1) }
  
  let (breaker1, result1) = call_with_circuit_breaker(successful_op, initial_breaker, 1000)
  assert_true(result1.success)
  assert_eq(breaker1.state, "closed")
  assert_eq(breaker1.failure_count, 0)
  
  let (breaker2, result2) = call_with_circuit_breaker(failing_op, breaker1, 2000)
  assert_false(result2.success)
  assert_eq(breaker2.state, "closed")
  assert_eq(breaker2.failure_count, 1)
  
  let (breaker3, result3) = call_with_circuit_breaker(failing_op, breaker2, 3000)
  assert_false(result3.success)
  assert_eq(breaker3.state, "closed")
  assert_eq(breaker3.failure_count, 2)
  
  let (breaker4, result4) = call_with_circuit_breaker(failing_op, breaker3, 4000)
  assert_false(result4.success)
  assert_eq(breaker4.state, "open")  // Circuit should open now
  assert_eq(breaker4.failure_count, 3)
  
  let (breaker5, result5) = call_with_circuit_breaker(successful_op, breaker4, 5000)
  assert_false(result5.success)  // Should fail because circuit is open
  assert_eq(breaker5.state, "open")
  
  let (breaker6, result6) = call_with_circuit_breaker(successful_op, breaker5, 70000)  // After timeout
  assert_true(result6.success)  // Should succeed in half-open state
  assert_eq(breaker6.state, "closed")  // Circuit should close again
  assert_eq(breaker6.failure_count, 0)
}