// Azimuth Enhanced MoonBit Feature Tests
// This file contains advanced MoonBit test cases focusing on functional programming and system features

// Test 1: Functional Programming - Higher-Order Functions
test "higher-order functions and functional composition" {
  // Define basic functions
  let add = fn(a: Int) { fn(b: Int) { a + b } }
  let multiply = fn(a: Int) { fn(b: Int) { a * b } }
  let square = fn(x: Int) { x * x }
  let double = fn(x: Int) { x * 2 }
  
  // Function composition
  let compose = fn(f: (Int) -> Int, g: (Int) -> Int) {
    fn(x: Int) { f(g(x)) }
  }
  
  // Test function composition
  let square_then_double = compose(double, square)
  assert_eq(square_then_double(3), 18)  // (3^2) * 2 = 18
  
  let double_then_square = compose(square, double)
  assert_eq(double_then_square(3), 36)  // (3*2)^2 = 36
  
  // Currying test
  let add_five = add(5)
  assert_eq(add_five(10), 15)
  
  let multiply_by_three = multiply(3)
  assert_eq(multiply_by_three(4), 12)
  
  // Pipeline operations
  let pipeline = fn(value: Int, operations: Array[(Int) -> Int>) {
    operations.reduce(fn(acc, op) { op(acc) }, value)
  }
  
  let operations = [double, square, add(1)]
  let result = pipeline(3, operations)  // ((3 * 2)^2) + 1 = 37
  assert_eq(result, 37)
  
  // Map and filter with higher-order functions
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  let even_squares = numbers
    .filter(fn(x) { x % 2 == 0 })
    .map(square)
  
  assert_eq(even_squares, [4, 16, 36, 64, 100])
  
  // Reduce with custom function
  let factorial = fn(n: Int) {
    if n <= 1 {
      1
    } else {
      let range = []
      let mut i = 1
      while i <= n {
        range = range.push(i)
        i = i + 1
      }
      range.reduce(fn(acc, x) { acc * x }, 1)
    }
  }
  
  assert_eq(factorial(0), 1)
  assert_eq(factorial(1), 1)
  assert_eq(factorial(5), 120)
  assert_eq(factorial(6), 720)
}

// Test 2: Async Programming Patterns
test "async programming patterns and promises" {
  // Define promise-like structure
  type Promise[T] = {
    state: String,  // "pending", "fulfilled", "rejected"
    value: Option[T],
    error: Option[String],
    callbacks: Array[(T) -> Unit]
  }
  
  // Create promise
  let create_promise = fn[T]() {
    {
      state: "pending",
      value: None,
      error: None,
      callbacks: []
    }
  }
  
  // Resolve promise
  let resolve = fn[T](promise: Promise[T], value: T) {
    promise.state = "fulfilled"
    promise.value = Some(value)
    
    // Execute callbacks
    for callback in promise.callbacks {
      callback(value)
    }
  }
  
  // Reject promise
  let reject = fn[T](promise: Promise[T], error: String) {
    promise.state = "rejected"
    promise.error = Some(error)
  }
  
  // Then method
  let then = fn[T, U](promise: Promise[T], on_fulfilled: (T) -> U) {
    let new_promise = create_promise[U]()
    
    let callback = fn(value: T) {
      let result = on_fulfilled(value)
      resolve(new_promise, result)
    }
    
    match promise.state {
      "fulfilled" => {
        match promise.value {
          Some(value) => callback(value),
          None => ()
        }
      }
      "pending" => {
        promise.callbacks = promise.callbacks.push(callback)
      }
      _ => ()
    }
    
    new_promise
  }
  
  // Catch method
  let catch = fn[T](promise: Promise[T], on_rejected: (String) -> T) {
    let new_promise = create_promise[T]()
    
    match promise.state {
      "rejected" => {
        match promise.error {
          Some(error) => {
            let result = on_rejected(error)
            resolve(new_promise, result)
          }
          None => ()
        }
      }
      _ => ()
    }
    
    new_promise
  }
  
  // Test promise chain
  let promise1 = create_promise[Int]()
  let promise2 = then(promise1, fn(x) { x * 2 })
  let promise3 = then(promise2, fn(x) { x + 10 })
  
  resolve(promise1, 5)
  
  match promise3.value {
    Some(result) => assert_eq(result, 20),  // (5 * 2) + 10 = 20
    None => assert_true(false)
  }
  
  // Test async-like behavior with callbacks
  let simulate_async_operation = fn[T](operation: () -> T, callback: (T) -> Unit) {
    // Simulate async delay
    callback(operation())
  }
  
  let test_async = fn() {
    let mut result = 0
    
    simulate_async_operation(fn() { 42 }, fn(value) {
      result = value
    })
    
    result
  }
  
  assert_eq(test_async(), 42)
  
  // Test parallel async operations
  let parallel_async = fn(operations: Array[() -> Int]) {
    let mut results = []
    
    for op in operations {
      simulate_async_operation(op, fn(value) {
        results = results.push(value)
      })
    }
    
    results.sort(fn(a, b) { a - b })
  }
  
  let async_ops = [fn() { 1 }, fn() { 3 }, fn() { 2 }]
  let parallel_result = parallel_async(async_ops)
  assert_eq(parallel_result, [1, 2, 3])
}

// Test 3: Data Flow Processing
test "data flow processing and stream operations" {
  // Define stream processor
  type StreamProcessor[T] = {
    data: Array[T],
    operations: Array[(Array[T]) -> Array[T]>
  }
  
  // Create stream processor
  let create_stream = fn[T](data: Array[T]) {
    {
      data: data,
      operations: []
    }
  }
  
  // Add operation to stream
  let add_operation = fn[T](stream: StreamProcessor[T], operation: (Array[T]) -> Array[T>) {
    {
      data: stream.data,
      operations: stream.operations.push(operation)
    }
  }
  
  // Execute stream operations
  let execute = fn[T](stream: StreamProcessor[T]) {
    stream.operations.reduce(fn(acc, op) { op(acc) }, stream.data)
  }
  
  // Test stream processing
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let stream = create_stream(numbers)
  
  let processed_stream = stream
    |> add_operation(fn(arr) { arr.filter(fn(x) { x % 2 == 0 }) })
    |> add_operation(fn(arr) { arr.map(fn(x) { x * x }) })
    |> add_operation(fn(arr) { arr.sort(fn(a, b) { b - a }) })
  
  let result = execute(processed_stream)
  assert_eq(result, [100, 64, 36, 16, 4])
  
  // Test batch processing
  let batch_process = fn[T](data: Array[T], batch_size: Int, processor: (Array[T]) -> Array[T]) {
    let mut results = []
    let mut i = 0
    
    while i < data.length() {
      let end = if i + batch_size < data.length() { i + batch_size } else { data.length() }
      let batch = data.slice(i, end)
      results = results.concat(processor(batch))
      i = i + batch_size
    }
    
    results
  }
  
  let batch_data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let batch_result = batch_process(batch_data, 3, fn(batch) {
    batch.map(fn(x) { x * 2 })
  })
  
  assert_eq(batch_result, [2, 4, 6, 8, 10, 12, 14, 16, 18, 20])
  
  // Test data transformation pipeline
  let create_pipeline = fn[T]() {
    let mut stages = []
    
    {
      add_stage: fn(stage: (T) -> T) {
        stages = stages.push(stage)
      },
      
      process: fn(input: T) {
        stages.reduce(fn(acc, stage) { stage(acc) }, input)
      }
    }
  }
  
  let pipeline = create_pipeline[Int]()
  pipeline.add_stage(fn(x) { x + 1 })
  pipeline.add_stage(fn(x) { x * 2 })
  pipeline.add_stage(fn(x) { x - 3 })
  
  let pipeline_result = pipeline.process(5)  // ((5 + 1) * 2) - 3 = 9
  assert_eq(pipeline_result, 9)
}

// Test 4: Algorithm Implementation
test "sorting and searching algorithms" {
  // Bubble sort
  let bubble_sort = fn(arr: Array[Int]) {
    let mut sorted = arr
    let n = sorted.length()
    let mut i = 0
    let mut j = 0
    
    while i < n {
      j = 0
      while j < n - i - 1 {
        if sorted[j] > sorted[j + 1] {
          let temp = sorted[j]
          sorted[j] = sorted[j + 1]
          sorted[j + 1] = temp
        }
        j = j + 1
      }
      i = i + 1
    }
    
    sorted
  }
  
  // Quick sort
  let quick_sort = fn(arr: Array[Int]) {
    if arr.length() <= 1 {
      arr
    } else {
      let pivot = arr[0]
      let less = arr.slice(1, arr.length()).filter(fn(x) { x <= pivot })
      let greater = arr.slice(1, arr.length()).filter(fn(x) { x > pivot })
      
      quick_sort(less).concat([pivot]).concat(quick_sort(greater))
    }
  }
  
  // Binary search
  let binary_search = fn(arr: Array[Int], target: Int) {
    let mut left = 0
    let mut right = arr.length() - 1
    
    while left <= right {
      let mid = left + (right - left) / 2
      
      if arr[mid] == target {
        Some(mid)
      } else if arr[mid] < target {
        left = mid + 1
      } else {
        right = mid - 1
      }
    }
    
    None
  }
  
  // Test sorting algorithms
  let unsorted = [64, 34, 25, 12, 22, 11, 90]
  let bubble_sorted = bubble_sort(unsorted)
  let quick_sorted = quick_sort(unsorted)
  
  assert_eq(bubble_sorted, [11, 12, 22, 25, 34, 64, 90])
  assert_eq(quick_sorted, [11, 12, 22, 25, 34, 64, 90])
  
  // Test binary search
  let sorted_array = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
  
  let found_index = binary_search(sorted_array, 7)
  match found_index {
    Some(index) => assert_eq(index, 3),
    None => assert_true(false)
  }
  
  let not_found = binary_search(sorted_array, 8)
  match not_found {
    Some(_) => assert_true(false),
    None => assert_true(true)
  }
  
  // Test merge sort
  let merge_sort = fn(arr: Array[Int]) {
    if arr.length() <= 1 {
      arr
    } else {
      let mid = arr.length() / 2
      let left = merge_sort(arr.slice(0, mid))
      let right = merge_sort(arr.slice(mid, arr.length()))
      
      let mut merged = []
      let mut i = 0
      let mut j = 0
      
      while i < left.length() && j < right.length() {
        if left[i] <= right[j] {
          merged = merged.push(left[i])
          i = i + 1
        } else {
          merged = merged.push(right[j])
          j = j + 1
        }
      }
      
      while i < left.length() {
        merged = merged.push(left[i])
        i = i + 1
      }
      
      while j < right.length() {
        merged = merged.push(right[j])
        j = j + 1
      }
      
      merged
    }
  }
  
  let merge_sorted = merge_sort(unsorted)
  assert_eq(merge_sorted, [11, 12, 22, 25, 34, 64, 90])
}

// Test 5: Memory Management
test "memory management and resource cleanup" {
  // Define resource tracker
  type ResourceTracker = {
    allocated_resources: Array[String],
    freed_resources: Array[String]
  }
  
  // Create resource tracker
  let create_tracker = fn() {
    {
      allocated_resources: [],
      freed_resources: []
    }
  }
  
  // Allocate resource
  let allocate_resource = fn(tracker: ResourceTracker, resource_name: String) {
    tracker.allocated_resources = tracker.allocated_resources.push(resource_name)
    resource_name
  }
  
  // Free resource
  let free_resource = fn(tracker: ResourceTracker, resource_name: String) {
    tracker.freed_resources = tracker.freed_resources.push(resource_name)
  }
  
  // Test resource allocation and deallocation
  let tracker = create_tracker()
  
  let resource1 = allocate_resource(tracker, "resource1")
  let resource2 = allocate_resource(tracker, "resource2")
  let resource3 = allocate_resource(tracker, "resource3")
  
  assert_eq(tracker.allocated_resources.length(), 3)
  assert_eq(tracker.freed_resources.length(), 0)
  
  free_resource(tracker, resource2)
  free_resource(tracker, resource1)
  
  assert_eq(tracker.allocated_resources.length(), 3)
  assert_eq(tracker.freed_resources.length(), 2)
  
  // Test resource leak detection
  let detect_leaks = fn(tracker: ResourceTracker) {
    let mut leaks = []
    
    for resource in tracker.allocated_resources {
      if not(tracker.freed_resources.contains(resource)) {
        leaks = leaks.push(resource)
      }
    }
    
    leaks
  }
  
  let leaks = detect_leaks(tracker)
  assert_eq(leaks.length(), 1)
  assert_eq(leaks[0], "resource3")
  
  // Test automatic resource cleanup
  let with_resource = fn[T](tracker: ResourceTracker, resource_name: String, operation: (String) -> T) {
    let resource = allocate_resource(tracker, resource_name)
    let result = operation(resource)
    free_resource(tracker, resource)
    result
  }
  
  let cleanup_result = with_resource(tracker, "temp_resource", fn(res) {
    res.length()
  })
  
  assert_eq(cleanup_result, 13)  // "temp_resource" length
  assert_eq(tracker.allocated_resources.length(), 4)
  assert_eq(tracker.freed_resources.length(), 4)
  
  // Test memory pool
  type MemoryPool = {
    pool_size: Int,
    allocated_blocks: Int,
    free_blocks: Int
  }
  
  let create_memory_pool = fn(size: Int) {
    {
      pool_size: size,
      allocated_blocks: 0,
      free_blocks: size
    }
  }
  
  let allocate_from_pool = fn(pool: MemoryPool, size: Int) {
    if pool.free_blocks >= size {
      pool.allocated_blocks = pool.allocated_blocks + size
      pool.free_blocks = pool.free_blocks - size
      true
    } else {
      false
    }
  }
  
  let deallocate_to_pool = fn(pool: MemoryPool, size: Int) {
    if pool.allocated_blocks >= size {
      pool.allocated_blocks = pool.allocated_blocks - size
      pool.free_blocks = pool.free_blocks + size
      true
    } else {
      false
    }
  }
  
  let memory_pool = create_memory_pool(1000)
  
  assert_true(allocate_from_pool(memory_pool, 100))
  assert_eq(memory_pool.allocated_blocks, 100)
  assert_eq(memory_pool.free_blocks, 900)
  
  assert_true(allocate_from_pool(memory_pool, 500))
  assert_eq(memory_pool.allocated_blocks, 600)
  assert_eq(memory_pool.free_blocks, 400)
  
  assert_false(allocate_from_pool(memory_pool, 500))  // Not enough free blocks
  
  assert_true(deallocate_to_pool(memory_pool, 200))
  assert_eq(memory_pool.allocated_blocks, 400)
  assert_eq(memory_pool.free_blocks, 600)
}

// Test 6: Logging System
test "logging system and log management" {
  // Define log level
  enum LogLevel {
    Debug
    Info
    Warning
    Error
    Critical
  }
  
  // Define log entry
  type LogEntry = {
    timestamp: Int,
    level: LogLevel,
    message: String,
    context: Array[(String, String)]
  }
  
  // Define logger
  type Logger = {
    logs: Array[LogEntry],
    min_level: LogLevel
  }
  
  // Create logger
  let create_logger = fn(min_level: LogLevel) {
    {
      logs: [],
      min_level: min_level
    }
  }
  
  // Log level comparison
  let level_to_int = fn(level: LogLevel) {
    match level {
      LogLevel::Debug => 0,
      LogLevel::Info => 1,
      LogLevel::Warning => 2,
      LogLevel::Error => 3,
      LogLevel::Critical => 4
    }
  }
  
  // Log message
  let log = fn(logger: Logger, level: LogLevel, message: String, context: Array[(String, String)]) {
    if level_to_int(level) >= level_to_int(logger.min_level) {
      let entry = {
        timestamp: 1640995200,  // Fixed timestamp for testing
        level: level,
        message: message,
        context: context
      }
      logger.logs = logger.logs.push(entry)
    }
  }
  
  // Test logging
  let logger = create_logger(LogLevel::Info)
  
  log(logger, LogLevel::Debug, "Debug message", [])
  log(logger, LogLevel::Info, "Info message", [("module", "test")])
  log(logger, LogLevel::Warning, "Warning message", [("module", "test"), ("severity", "medium")])
  log(logger, LogLevel::Error, "Error message", [("module", "test"), ("error_code", "500")])
  
  // Debug message should not be logged (below min level)
  assert_eq(logger.logs.length(), 3)
  
  // Check log entries
  let info_log = logger.logs[0]
  assert_eq(info_log.message, "Info message")
  assert_eq(info_log.context.length(), 1)
  assert_eq(info_log.context[0], ("module", "test"))
  
  let warning_log = logger.logs[1]
  assert_eq(warning_log.message, "Warning message")
  assert_eq(warning_log.context.length(), 2)
  
  let error_log = logger.logs[2]
  assert_eq(error_log.message, "Error message")
  assert_eq(error_log.context.length(), 2)
  
  // Test log filtering
  let filter_logs = fn(logger: Logger, level: LogLevel) {
    logger.logs.filter(fn(entry) { entry.level == level })
  }
  
  let warning_logs = filter_logs(logger, LogLevel::Warning)
  assert_eq(warning_logs.length(), 1)
  assert_eq(warning_logs[0].message, "Warning message")
  
  // Test log search
  let search_logs = fn(logger: Logger, keyword: String) {
    logger.logs.filter(fn(entry) { entry.message.contains(keyword) })
  }
  
  let error_search = search_logs(logger, "Error")
  assert_eq(error_search.length(), 1)
  assert_eq(error_search[0].message, "Error message")
  
  let no_match = search_logs(logger, "Nonexistent")
  assert_eq(no_match.length(), 0)
  
  // Test log formatting
  let format_log_entry = fn(entry: LogEntry) {
    let level_str = match entry.level {
      LogLevel::Debug => "DEBUG",
      LogLevel::Info => "INFO",
      LogLevel::Warning => "WARNING",
      LogLevel::Error => "ERROR",
      LogLevel::Critical => "CRITICAL"
    }
    
    let context_str = entry.context.reduce(fn(acc, pair) {
      acc + pair.0 + "=" + pair.1 + " "
    }, "")
    
    "[" + level_str + "] " + entry.message + " " + context_str
  }
  
  let formatted_info = format_log_entry(info_log)
  assert_eq(formatted_info, "[INFO] Info message module=test ")
}

// Test 7: Configuration Management
test "configuration management and settings" {
  // Define configuration value type
  enum ConfigValue {
    StringValue(String)
    IntValue(Int)
    FloatValue(Float)
    BoolValue(Bool)
    ArrayValue(Array[String])
  }
  
  // Define configuration
  type Configuration = {
    settings: Array[(String, ConfigValue)],
    defaults: Array[(String, ConfigValue)]
  }
  
  // Create configuration
  let create_config = fn() {
    {
      settings: [],
      defaults: [
        ("timeout", ConfigValue::IntValue(30)),
        ("retries", ConfigValue::IntValue(3)),
        ("debug", ConfigValue::BoolValue(false)),
        ("hosts", ConfigValue::ArrayValue(["localhost"]))
      ]
    }
  }
  
  // Get configuration value
  let get_config = fn(config: Configuration, key: String) {
    // First check settings
    let setting_index = config.settings.index_of(fn(pair) { pair.0 == key })
    match setting_index {
      Some(index) => Some(config.settings[index].1),
      None => {
        // Then check defaults
        let default_index = config.defaults.index_of(fn(pair) { pair.0 == key })
        match default_index {
          Some(index) => Some(config.defaults[index].1),
          None => None
        }
      }
    }
  }
  
  // Set configuration value
  let set_config = fn(config: Configuration, key: String, value: ConfigValue) {
    let setting_index = config.settings.index_of(fn(pair) { pair.0 == key })
    match setting_index {
      Some(index) => {
        config.settings[index] = (key, value)
      }
      None => {
        config.settings = config.settings.push((key, value))
      }
    }
  }
  
  // Test configuration management
  let config = create_config()
  
  // Get default values
  let timeout_value = get_config(config, "timeout")
  match timeout_value {
    Some(ConfigValue::IntValue(value)) => assert_eq(value, 30),
    _ => assert_true(false)
  }
  
  let debug_value = get_config(config, "debug")
  match debug_value {
    Some(ConfigValue::BoolValue(value)) => assert_false(value),
    _ => assert_true(false)
  }
  
  // Set new values
  set_config(config, "timeout", ConfigValue::IntValue(60))
  set_config(config, "debug", ConfigValue::BoolValue(true))
  set_config(config, "new_setting", ConfigValue::StringValue("test"))
  
  // Verify updated values
  let updated_timeout = get_config(config, "timeout")
  match updated_timeout {
    Some(ConfigValue::IntValue(value)) => assert_eq(value, 60),
    _ => assert_true(false)
  }
  
  let updated_debug = get_config(config, "debug")
  match updated_debug {
    Some(ConfigValue::BoolValue(value)) => assert_true(value),
    _ => assert_true(false)
  }
  
  let new_setting = get_config(config, "new_setting")
  match new_setting {
    Some(ConfigValue::StringValue(value)) => assert_eq(value, "test"),
    _ => assert_true(false)
  }
  
  // Test configuration validation
  let validate_config = fn(config: Configuration, schema: Array[(String, ConfigValue)]) {
    let mut errors = []
    
    for (key, expected_type) in schema {
      let value = get_config(config, key)
      match value {
        Some(actual_value) => {
          // Check type compatibility
          let type_match = match (expected_type, actual_value) {
            (ConfigValue::IntValue(_), ConfigValue::IntValue(_)) => true,
            (ConfigValue::StringValue(_), ConfigValue::StringValue(_)) => true,
            (ConfigValue::FloatValue(_), ConfigValue::FloatValue(_)) => true,
            (ConfigValue::BoolValue(_), ConfigValue::BoolValue(_)) => true,
            (ConfigValue::ArrayValue(_), ConfigValue::ArrayValue(_)) => true,
            _ => false
          }
          
          if not(type_match) {
            errors = errors.push("Type mismatch for key: " + key)
          }
        }
        None => {
          errors = errors.push("Missing key: " + key)
        }
      }
    }
    
    errors
  }
  
  let schema = [
    ("timeout", ConfigValue::IntValue(0)),
    ("debug", ConfigValue::BoolValue(false)),
    ("missing_key", ConfigValue::StringValue(""))
  ]
  
  let validation_errors = validate_config(config, schema)
  assert_eq(validation_errors.length(), 1)
  assert_true(validation_errors[0].contains("Missing key"))
}

// Test 8: Caching Mechanism
test "caching mechanism and cache management" {
  // Define cache entry
  type CacheEntry[T] = {
    value: T,
    timestamp: Int,
    ttl: Int  // Time to live in seconds
  }
  
  // Define cache
  type Cache[T] = {
    entries: Array[(String, CacheEntry[T])],
    max_size: Int,
    current_time: Int
  }
  
  // Create cache
  let create_cache = fn[T](max_size: Int) {
    {
      entries: [],
      max_size: max_size,
      current_time: 0
    }
  }
  
  // Check if entry is expired
  let is_expired = fn[T](entry: CacheEntry[T], current_time: Int) {
    current_time - entry.timestamp > entry.ttl
  }
  
  // Get value from cache
  let get_cache = fn[T](cache: Cache[T], key: String) {
    let entry_index = cache.entries.index_of_fn(pair) { pair.0 == key }
    match entry_index {
      Some(index) => {
        let entry = cache.entries[index].1
        if is_expired(entry, cache.current_time) {
          // Remove expired entry
          cache.entries = cache.entries.remove_at(index)
          None
        } else {
          Some(entry.value)
        }
      }
      None => None
    }
  }
  
  // Set value in cache
  let set_cache = fn[T](cache: Cache[T], key: String, value: T, ttl: Int) {
    // Check if cache is full
    if cache.entries.length() >= cache.max_size {
      // Remove oldest entry (simple FIFO)
      cache.entries = cache.entries.slice(1, cache.entries.length())
    }
    
    let entry = {
      value: value,
      timestamp: cache.current_time,
      ttl: ttl
    }
    
    // Check if key already exists
    let entry_index = cache.entries.index_of_fn(pair) { pair.0 == key }
    match entry_index {
      Some(index) => {
        cache.entries[index] = (key, entry)
      }
      None => {
        cache.entries = cache.entries.push((key, entry))
      }
    }
  }
  
  // Test cache operations
  let cache = create_cache[String](3)
  
  // Set values
  set_cache(cache, "key1", "value1", 10)
  set_cache(cache, "key2", "value2", 20)
  set_cache(cache, "key3", "value3", 30)
  
  // Get values
  let value1 = get_cache(cache, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1"),
    None => assert_true(false)
  }
  
  let value2 = get_cache(cache, "key2")
  match value2 {
    Some(v) => assert_eq(v, "value2"),
    None => assert_true(false)
  }
  
  let missing = get_cache(cache, "missing")
  match missing {
    Some(_) => assert_true(false),
    None => assert_true(true)
  }
  
  // Test cache eviction
  set_cache(cache, "key4", "value4", 40)  // Should evict key1
  
  let evicted = get_cache(cache, "key1")
  match evicted {
    Some(_) => assert_true(false),
    None => assert_true(true)
  }
  
  let value4 = get_cache(cache, "key4")
  match value4 {
    Some(v) => assert_eq(v, "value4"),
    None => assert_true(false)
  }
  
  // Test cache expiration
  cache.current_time = 25  // Advance time
  
  let expired = get_cache(cache, "key2")  // TTL was 20, should be expired
  match expired {
    Some(_) => assert_true(false),
    None => assert_true(true)
  }
  
  let not_expired = get_cache(cache, "key3")  // TTL was 30, should not be expired
  match not_expired {
    Some(v) => assert_eq(v, "value3"),
    None => assert_true(false)
  }
  
  // Test cache statistics
  let cache_stats = fn[T](cache: Cache[T]) {
    let mut expired_count = 0
    
    for (_, entry) in cache.entries {
      if is_expired(entry, cache.current_time) {
        expired_count = expired_count + 1
      }
    }
    
    {
      total_entries: cache.entries.length(),
      expired_entries: expired_count,
      max_size: cache.max_size,
      utilization: cache.entries.length() * 100 / cache.max_size
    }
  }
  
  let stats = cache_stats(cache)
  assert_eq(stats.total_entries, 2)  // key3 and key4
  assert_eq(stats.expired_entries, 0)
  assert_eq(stats.max_size, 3)
  assert_eq(stats.utilization, 66)  // 2/3 * 100 = 66 (integer division)
}

// Test 9: Event Handling
test "event handling and event dispatching" {
  // Define event
  type Event = {
    type: String,
    data: Array[(String, String)],
    timestamp: Int
  }
  
  // Define event handler
  type EventHandler = (Event) -> Unit
  
  // Define event dispatcher
  type EventDispatcher = {
    handlers: Array[(String, EventHandler)],
    event_history: Array[Event]
  }
  
  // Create event dispatcher
  let create_dispatcher = fn() {
    {
      handlers: [],
      event_history: []
    }
  }
  
  // Register event handler
  let register_handler = fn(dispatcher: EventDispatcher, event_type: String, handler: EventHandler) {
    dispatcher.handlers = dispatcher.handlers.push((event_type, handler))
  }
  
  // Dispatch event
  let dispatch_event = fn(dispatcher: EventDispatcher, event: Event) {
    // Add to history
    dispatcher.event_history = dispatcher.event_history.push(event)
    
    // Find and call handlers
    for (handler_type, handler) in dispatcher.handlers {
      if handler_type == event.type {
        handler(event)
      }
    }
  }
  
  // Test event handling
  let dispatcher = create_dispatcher()
  let mut handled_events = []
  
  // Register handlers
  register_handler(dispatcher, "click", fn(event) {
    handled_events = handled_events.push("click: " + event.data.find_fn(pair) { pair.0 == "target" }.1)
  })
  
  register_handler(dispatcher, "click", fn(event) {
    handled_events = handled_events.push("click_count: " + event.data.find_fn(pair) { pair.0 == "count" }.1)
  })
  
  register_handler(dispatcher, "load", fn(event) {
    handled_events = handled_events.push("load: " + event.data.find_fn(pair) { pair.0 == "url" }.1)
  })
  
  // Dispatch events
  let click_event = {
    type: "click",
    data: [("target", "button"), ("count", "1")],
    timestamp: 1640995200
  }
  
  let load_event = {
    type: "load",
    data: [("url", "http://example.com")],
    timestamp: 1640995201
  }
  
  dispatch_event(dispatcher, click_event)
  dispatch_event(dispatcher, load_event)
  
  // Verify handlers were called
  assert_eq(handled_events.length(), 3)
  assert_true(handled_events.contains("click: button"))
  assert_true(handled_events.contains("click_count: 1"))
  assert_true(handled_events.contains("load: http://example.com"))
  
  // Verify event history
  assert_eq(dispatcher.event_history.length(), 2)
  assert_eq(dispatcher.event_history[0].type, "click")
  assert_eq(dispatcher.event_history[1].type, "load")
  
  // Test event filtering
  let filter_events = fn(dispatcher: EventDispatcher, event_type: String) {
    dispatcher.event_history.filter_fn(event) { event.type == event_type }
  }
  
  let click_events = filter_events(dispatcher, "click")
  assert_eq(click_events.length(), 1)
  assert_eq(click_events[0].data.find_fn(pair) { pair.0 == "target" }.1, "button")
  
  // Test event aggregation
  let aggregate_events = fn(dispatcher: EventDispatcher, event_type: String, aggregation_fn: (Array[Event]) -> String) {
    let events = filter_events(dispatcher, event_type)
    aggregation_fn(events)
  }
  
  let click_count = aggregate_events(dispatcher, "click", fn(events) {
    events.length().to_string()
  })
  
  assert_eq(click_count, "1")
  
  // Test once-only handlers
  let create_once_handler = fn(handler: EventHandler) {
    let mut called = false
    
    fn(event) {
      if not(called) {
        handler(event)
        called = true
      }
    }
  }
  
  let mut once_called = false
  let once_handler = create_once_handler(fn(event) {
    once_called = true
  })
  
  register_handler(dispatcher, "once_test", once_handler)
  
  let once_event = {
    type: "once_test",
    data: [],
    timestamp: 1640995202
  }
  
  dispatch_event(dispatcher, once_event)
  dispatch_event(dispatcher, once_event)  // Should not call handler again
  
  assert_true(once_called)
}

// Test 10: Data Validation
test "data validation and validation rules" {
  // Define validation result
  type ValidationResult = {
    is_valid: Bool,
    errors: Array[String]
  }
  
  // Define validation rule
  type ValidationRule[T] = (T) -> ValidationResult
  
  // Create successful validation result
  let success = fn() {
    {
      is_valid: true,
      errors: []
    }
  }
  
  // Create failed validation result
  let failure = fn(error: String) {
    {
      is_valid: false,
      errors: [error]
    }
  }
  
  // Combine validation results
  let combine_results = fn(results: Array[ValidationResult]) {
    let mut all_errors = []
    let mut is_valid = true
    
    for result in results {
      if not(result.is_valid) {
        is_valid = false
        all_errors = all_errors.concat(result.errors)
      }
    }
    
    {
      is_valid: is_valid,
      errors: all_errors
    }
  }
  
  // Define validation rules for strings
  let required = fn(value: String) {
    if value.length() > 0 {
      success()
    } else {
      failure("Value is required")
    }
  }
  
  let min_length = fn(min: Int) {
    fn(value: String) {
      if value.length() >= min {
        success()
      } else {
        failure("Value must be at least " + min.to_string() + " characters")
      }
    }
  }
  
  let max_length = fn(max: Int) {
    fn(value: String) {
      if value.length() <= max {
        success()
      } else {
        failure("Value must be at most " + max.to_string() + " characters")
      }
    }
  }
  
  let email_format = fn(value: String) {
    if value.contains("@") && value.contains(".") {
      success()
    } else {
      failure("Value must be a valid email address")
    }
  }
  
  // Define validation rules for numbers
  let min_value = fn(min: Int) {
    fn(value: Int) {
      if value >= min {
        success()
      } else {
        failure("Value must be at least " + min.to_string())
      }
    }
  }
  
  let max_value = fn(max: Int) {
    fn(value: Int) {
      if value <= max {
        success()
      } else {
        failure("Value must be at most " + max.to_string())
      }
    }
  }
  
  // Test string validation
  let validate_string = fn(value: String, rules: Array[ValidationRule[String]]) {
    let results = rules.map_fn(rule) { rule(value) }
    combine_results(results)
  }
  
  let name_validation = validate_string("John", [
    required,
    min_length(2),
    max_length(50)
  ])
  
  assert_true(name_validation.is_valid)
  assert_eq(name_validation.errors.length(), 0)
  
  let invalid_name = validate_string("", [
    required,
    min_length(2),
    max_length(50)
  ])
  
  assert_false(invalid_name.is_valid)
  assert_eq(invalid_name.errors.length(), 1)
  assert_true(invalid_name.errors[0].contains("required"))
  
  let email_validation = validate_string("user@example.com", [
    required,
    email_format
  ])
  
  assert_true(email_validation.is_valid)
  
  let invalid_email = validate_string("invalid-email", [
    required,
    email_format
  ])
  
  assert_false(invalid_email.is_valid)
  assert_eq(invalid_email.errors.length(), 1)
  assert_true(invalid_email.errors[0].contains("email address"))
  
  // Test number validation
  let validate_number = fn(value: Int, rules: Array[ValidationRule[Int]]) {
    let results = rules.map_fn(rule) { rule(value) }
    combine_results(results)
  }
  
  let age_validation = validate_number(25, [
    min_value(0),
    max_value(150)
  ])
  
  assert_true(age_validation.is_valid)
  
  let invalid_age = validate_number(-5, [
    min_value(0),
    max_value(150)
  ])
  
  assert_false(invalid_age.is_valid)
  assert_eq(invalid_age.errors.length(), 1)
  assert_true(invalid_age.errors[0].contains("at least 0"))
  
  // Test complex object validation
  type User = {
    name: String,
    email: String,
    age: Int
  }
  
  let validate_user = fn(user: User) {
    let name_result = validate_string(user.name, [
      required,
      min_length(2),
      max_length(50)
    ])
    
    let email_result = validate_string(user.email, [
      required,
      email_format
    ])
    
    let age_result = validate_number(user.age, [
      min_value(0),
      max_value(150)
    ])
    
    combine_results([name_result, email_result, age_result])
  }
  
  let valid_user = {
    name: "Alice",
    email: "alice@example.com",
    age: 30
  }
  
  let valid_user_result = validate_user(valid_user)
  assert_true(valid_user_result.is_valid)
  
  let invalid_user = {
    name: "",
    email: "invalid-email",
    age: -5
  }
  
  let invalid_user_result = validate_user(invalid_user)
  assert_false(invalid_user_result.is_valid)
  assert_eq(invalid_user_result.errors.length(), 3)
}