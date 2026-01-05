// Azimuth Telemetry System - New Comprehensive MoonBit Tests
// This file contains comprehensive test cases for various telemetry functionalities

// Test 1: Data Conversion and Serialization
test "data conversion and serialization operations" {
  // Test string to number conversion
  let str_int = "42"
  let str_float = "3.14159"
  let str_bool = "true"
  
  // Convert string to int
  let converted_int = str_int.to_int()
  match converted_int {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false)
  }
  
  // Convert string to float
  let converted_float = str_float.to_float()
  assert_true(converted_float > 3.14 && converted_float < 3.15)
  
  // Convert string to boolean
  let converted_bool = str_bool.to_bool()
  assert_true(converted_bool)
  
  // Test number to string conversion
  let int_val = 123
  let float_val = 45.67
  let bool_val = false
  
  let int_str = int_val.to_string()
  let float_str = float_val.to_string()
  let bool_str = bool_val.to_string()
  
  assert_eq(int_str, "123")
  assert_eq(float_str, "45.67")
  assert_eq(bool_str, "false")
  
  // Test array serialization
  let int_array = [1, 2, 3, 4, 5]
  let string_array = ["apple", "banana", "cherry"]
  
  let int_array_str = int_array.to_string()
  let string_array_str = string_array.to_string()
  
  assert_true(int_array_str.contains("1"))
  assert_true(int_array_str.contains("5"))
  assert_true(string_array_str.contains("apple"))
  assert_true(string_array_str.contains("cherry"))
  
  // Test option type serialization
  let some_value = Some("test")
  let none_value = None
  
  let some_str = some_value.to_string()
  let none_str = none_value.to_string()
  
  assert_true(some_str.contains("test"))
  assert_eq(none_str, "None")
}

// Test 2: Caching Mechanism
test "caching mechanism operations" {
  // Test simple cache implementation
  let cache = Cache::new(10) // Cache with capacity of 10 items
  
  // Test cache insertion
  Cache::put(cache, "key1", "value1")
  Cache::put(cache, "key2", "value2")
  Cache::put(cache, "key3", "value3")
  
  // Test cache retrieval
  let value1 = Cache::get(cache, "key1")
  let value2 = Cache::get(cache, "key2")
  let value3 = Cache::get(cache, "key3")
  
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  match value2 {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  match value3 {
    Some(v) => assert_eq(v, "value3")
    None => assert_true(false)
  }
  
  // Test cache miss
  let non_existent = Cache::get(cache, "non_existent_key")
  assert_eq(non_existent, None)
  
  // Test cache removal
  Cache::remove(cache, "key2")
  let removed_value = Cache::get(cache, "key2")
  assert_eq(removed_value, None)
  
  // Test cache capacity
  for i = 0; i < 15; i = i + 1 {
    Cache::put(cache, "key" + i.to_string(), "value" + i.to_string())
  }
  
  // Cache should only contain the last 10 items
  let old_value = Cache::get(cache, "key1")
  let new_value = Cache::get(cache, "key14")
  
  assert_eq(old_value, None) // Should be evicted
  match new_value {
    Some(v) => assert_eq(v, "value14")
    None => assert_true(false)
  }
  
  // Test cache clearing
  Cache::clear(cache)
  let cleared_value = Cache::get(cache, "key14")
  assert_eq(cleared_value, None)
}

// Test 3: Network Communication
test "network communication operations" {
  // Test URL parsing
  let url1 = "https://example.com:8080/path?query=value#fragment"
  let url2 = "http://localhost:3000/api/v1/resource"
  
  let parsed_url1 = URL::parse(url1)
  let parsed_url2 = URL::parse(url2)
  
  match parsed_url1 {
    Some(url) => {
      assert_eq(URL::scheme(url), "https")
      assert_eq(URL::host(url), "example.com")
      assert_eq(URL::port(url), Some(8080))
      assert_eq(URL::path(url), "/path")
      assert_eq(URL::query(url), Some("query=value"))
      assert_eq(URL::fragment(url), Some("fragment"))
    }
    None => assert_true(false)
  }
  
  match parsed_url2 {
    Some(url) => {
      assert_eq(URL::scheme(url), "http")
      assert_eq(URL::host(url), "localhost")
      assert_eq(URL::port(url), Some(3000))
      assert_eq(URL::path(url), "/api/v1/resource")
      assert_eq(URL::query(url), None)
      assert_eq(URL::fragment(url), None)
    }
    None => assert_true(false)
  }
  
  // Test HTTP header operations
  let headers = Headers::new()
  Headers::set(headers, "Content-Type", "application/json")
  Headers::set(headers, "Authorization", "Bearer token123")
  Headers::set(headers, "User-Agent", "Azimuth-Telemetry/1.0")
  
  let content_type = Headers::get(headers, "Content-Type")
  let authorization = Headers::get(headers, "Authorization")
  let user_agent = Headers::get(headers, "User-Agent")
  
  match content_type {
    Some(value) => assert_eq(value, "application/json")
    None => assert_true(false)
  }
  
  match authorization {
    Some(value) => assert_eq(value, "Bearer token123")
    None => assert_true(false)
  }
  
  match user_agent {
    Some(value) => assert_eq(value, "Azimuth-Telemetry/1.0")
    None => assert_true(false)
  }
  
  // Test HTTP status codes
  let status_codes = [
    (200, "OK"),
    (201, "Created"),
    (400, "Bad Request"),
    (401, "Unauthorized"),
    (404, "Not Found"),
    (500, "Internal Server Error")
  ]
  
  for (code, message) in status_codes {
    let status = HttpStatus::new(code)
    assert_eq(HttpStatus::code(status), code)
    assert_eq(HttpStatus::message(status), message)
    
    // Test status category
    if code >= 200 && code < 300 {
      assert_true(HttpStatus::is_success(status))
    } else if code >= 400 && code < 500 {
      assert_true(HttpStatus::is_client_error(status))
    } else if code >= 500 {
      assert_true(HttpStatus::is_server_error(status))
    }
  }
}

// Test 4: Configuration Management
test "configuration management operations" {
  // Test configuration loading
  let config = Config::new()
  
  // Test setting and getting configuration values
  Config::set_string(config, "database.host", "localhost")
  Config::set_int(config, "database.port", 5432)
  Config::set_bool(config, "database.ssl", true)
  Config::set_float(config, "database.timeout", 30.5)
  
  // Test retrieving configuration values
  let db_host = Config::get_string(config, "database.host")
  let db_port = Config::get_int(config, "database.port")
  let db_ssl = Config::get_bool(config, "database.ssl")
  let db_timeout = Config::get_float(config, "database.timeout")
  
  match db_host {
    Some(value) => assert_eq(value, "localhost")
    None => assert_true(false)
  }
  
  match db_port {
    Some(value) => assert_eq(value, 5432)
    None => assert_true(false)
  }
  
  match db_ssl {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  match db_timeout {
    Some(value) => assert_true(value > 30.0 && value < 31.0)
    None => assert_true(false)
  }
  
  // Test default values
  let non_existent_string = Config::get_string(config, "non.existent.key")
  let non_existent_int = Config::get_int(config, "non.existent.key")
  let non_existent_bool = Config::get_bool(config, "non.existent.key")
  let non_existent_float = Config::get_float(config, "non.existent.key")
  
  assert_eq(non_existent_string, None)
  assert_eq(non_existent_int, None)
  assert_eq(non_existent_bool, None)
  assert_eq(non_existent_float, None)
  
  // Test configuration with defaults
  let default_string = Config::get_string_with_default(config, "non.existent.key", "default_value")
  let default_int = Config::get_int_with_default(config, "non.existent.key", 42)
  let default_bool = Config::get_bool_with_default(config, "non.existent.key", false)
  let default_float = Config::get_float_with_default(config, "non.existent.key", 3.14)
  
  assert_eq(default_string, "default_value")
  assert_eq(default_int, 42)
  assert_false(default_bool)
  assert_true(default_float > 3.1 && default_float < 3.2)
  
  // Test nested configuration
  Config::set_string(config, "logging.level", "info")
  Config::set_string(config, "logging.format", "json")
  Config::set_string(config, "logging.output", "file")
  
  let log_level = Config::get_string(config, "logging.level")
  let log_format = Config::get_string(config, "logging.format")
  let log_output = Config::get_string(config, "logging.output")
  
  match log_level {
    Some(value) => assert_eq(value, "info")
    None => assert_true(false)
  }
  
  match log_format {
    Some(value) => assert_eq(value, "json")
    None => assert_true(false)
  }
  
  match log_output {
    Some(value) => assert_eq(value, "file")
    None => assert_true(false)
  }
  
  // Test configuration serialization
  let serialized = Config::serialize(config)
  assert_true(serialized.contains("database.host"))
  assert_true(serialized.contains("localhost"))
  assert_true(serialized.contains("logging.level"))
  assert_true(serialized.contains("info"))
  
  // Test configuration deserialization
  let new_config = Config::deserialize(serialized)
  let deserialized_host = Config::get_string(new_config, "database.host")
  
  match deserialized_host {
    Some(value) => assert_eq(value, "localhost")
    None => assert_true(false)
  }
}

// Test 5: Internationalization Support
test "internationalization support operations" {
  // Test locale creation
  let en_us = Locale::new("en-US")
  let zh_cn = Locale::new("zh-CN")
  let fr_fr = Locale::new("fr-FR")
  
  assert_eq(Locale::language(en_us), "en")
  assert_eq(Locale::country(en_us), "US")
  
  assert_eq(Locale::language(zh_cn), "zh")
  assert_eq(Locale::country(zh_cn), "CN")
  
  assert_eq(Locale::language(fr_fr), "fr")
  assert_eq(Locale::country(fr_fr), "FR")
  
  // Test message formatting
  let messages = Messages::new()
  
  // Add messages for different locales
  Messages::add(messages, en_us, "welcome", "Welcome to Azimuth Telemetry")
  Messages::add(messages, zh_cn, "welcome", "欢迎使用 Azimuth 遥测系统")
  Messages::add(messages, fr_fr, "welcome", "Bienvenue dans Azimuth Télémétrie")
  
  Messages::add(messages, en_us, "error.database", "Database connection failed")
  Messages::add(messages, zh_cn, "error.database", "数据库连接失败")
  Messages::add(messages, fr_fr, "error.database", "La connexion à la base de données a échoué")
  
  // Test message retrieval
  let welcome_en = Messages::get(messages, en_us, "welcome")
  let welcome_zh = Messages::get(messages, zh_cn, "welcome")
  let welcome_fr = Messages::get(messages, fr_fr, "welcome")
  
  match welcome_en {
    Some(msg) => assert_eq(msg, "Welcome to Azimuth Telemetry")
    None => assert_true(false)
  }
  
  match welcome_zh {
    Some(msg) => assert_eq(msg, "欢迎使用 Azimuth 遥测系统")
    None => assert_true(false)
  }
  
  match welcome_fr {
    Some(msg) => assert_eq(msg, "Bienvenue dans Azimuth Télémétrie")
    None => assert_true(false)
  }
  
  // Test parameterized messages
  Messages::add(messages, en_us, "user.greeting", "Hello, {name}! You have {count} new messages.")
  Messages::add(messages, zh_cn, "user.greeting", "你好，{name}！您有 {count} 条新消息。")
  Messages::add(messages, fr_fr, "user.greeting", "Bonjour, {name}! Vous avez {count} nouveaux messages.")
  
  let params = [
    ("name", "Alice"),
    ("count", "5")
  ]
  
  let greeting_en = Messages::format(messages, en_us, "user.greeting", params)
  let greeting_zh = Messages::format(messages, zh_cn, "user.greeting", params)
  let greeting_fr = Messages::format(messages, fr_fr, "user.greeting", params)
  
  assert_eq(greeting_en, "Hello, Alice! You have 5 new messages.")
  assert_eq(greeting_zh, "你好，Alice！您有 5 条新消息。")
  assert_eq(greeting_fr, "Bonjour, Alice! Vous avez 5 nouveaux messages.")
  
  // Test number formatting
  let number = 1234567.89
  
  let formatted_en = Locale::format_number(en_us, number)
  let formatted_zh = Locale::format_number(zh_cn, number)
  let formatted_fr = Locale::format_number(fr_fr, number)
  
  assert_true(formatted_en.contains("1,234,567.89"))
  assert_true(formatted_zh.contains("1,234,567.89"))
  assert_true(formatted_fr.contains("1 234 567,89"))
  
  // Test date formatting
  let timestamp = 1640995200000L // 2022-01-01 00:00:00 UTC
  
  let date_en = Locale::format_date(en_us, timestamp)
  let date_zh = Locale::format_date(zh_cn, timestamp)
  let date_fr = Locale::format_date(fr_fr, timestamp)
  
  assert_true(date_en.contains("2022"))
  assert_true(date_en.contains("01"))
  assert_true(date_en.contains("01"))
  
  assert_true(date_zh.contains("2022"))
  assert_true(date_zh.contains("01"))
  assert_true(date_zh.contains("01"))
  
  assert_true(date_fr.contains("2022"))
  assert_true(date_fr.contains("01"))
  assert_true(date_fr.contains("01"))
  
  // Test fallback to default locale
  let ja_jp = Locale::new("ja-JP")
  let fallback_msg = Messages::get(messages, ja_jp, "welcome")
  
  match fallback_msg {
    Some(msg) => assert_eq(msg, "Welcome to Azimuth Telemetry") // Fallback to English
    None => assert_true(false)
  }
}

// Test 6: Performance Optimization
test "performance optimization operations" {
  // Test memory pool allocation
  let pool = MemoryPool::new(100) // Pool with 100 objects
  
  // Test object allocation from pool
  let obj1 = MemoryPool::allocate(pool)
  let obj2 = MemoryPool::allocate(pool)
  let obj3 = MemoryPool::allocate(pool)
  
  assert_true(MemoryPool::is_from_pool(pool, obj1))
  assert_true(MemoryPool::is_from_pool(pool, obj2))
  assert_true(MemoryPool::is_from_pool(pool, obj3))
  
  // Test object deallocation to pool
  MemoryPool::deallocate(pool, obj1)
  MemoryPool::deallocate(pool, obj2)
  
  // Test pool statistics
  let stats = MemoryPool::stats(pool)
  assert_eq(MemoryPoolStats::allocated(stats), 3)
  assert_eq(MemoryPoolStats::deallocated(stats), 2)
  assert_eq(MemoryPoolStats::available(stats), 2)
  
  // Test batch processing
  let items = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let batch_size = 3
  
  let batches = BatchProcessor::create_batches(items, batch_size)
  assert_eq(batches.length(), 4) // 10 items with batch size 3 = 4 batches (3,3,3,1)
  
  assert_eq(batches[0].length(), 3)
  assert_eq(batches[1].length(), 3)
  assert_eq(batches[2].length(), 3)
  assert_eq(batches[3].length(), 1)
  
  // Test parallel processing simulation
  let processed_items = BatchProcessor::process_batches(batches, |batch| {
    batch.map(|item| item * 2)
  })
  
  let flattened = processed_items.flatten()
  assert_eq(flattened.length(), 10)
  
  let expected = [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
  for i = 0; i < flattened.length(); i = i + 1 {
    assert_eq(flattened[i], expected[i])
  }
  
  // Test lazy evaluation
  let lazy_value = Lazy::new(|| {
    // Simulate expensive computation
    let mut result = 0
    for i = 0; i < 1000; i = i + 1 {
      result = result + i
    }
    result
  })
  
  assert_false(Lazy::is_evaluated(lazy_value))
  
  let value1 = Lazy::get(lazy_value)
  let value2 = Lazy::get(lazy_value)
  
  assert_true(Lazy::is_evaluated(lazy_value))
  assert_eq(value1, value2) // Should be the same value
  assert_eq(value1, 499500) // Sum of 0 to 999
  
  // Test caching with TTL
  let ttl_cache = TTLCache::new(100, 60) // 100 items, 60 seconds TTL
  
  TTLCache::put(ttl_cache, "key1", "value1")
  TTLCache::put(ttl_cache, "key2", "value2")
  
  let cached_value1 = TTLCache::get(ttl_cache, "key1")
  let cached_value2 = TTLCache::get(ttl_cache, "key2")
  
  match cached_value1 {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  match cached_value2 {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  // Simulate time passage (in real implementation, this would involve actual time)
  TTLCache::advance_time(ttl_cache, 61) // Advance 61 seconds
  
  let expired_value1 = TTLCache::get(ttl_cache, "key1")
  let expired_value2 = TTLCache::get(ttl_cache, "key2")
  
  assert_eq(expired_value1, None) // Should be expired
  assert_eq(expired_value2, None) // Should be expired
}

// Test 7: Error Handling and Recovery
test "error handling and recovery operations" {
  // Test error creation and handling
  let error1 = Error::new("database.connection", "Failed to connect to database")
  let error2 = Error::with_code("network.timeout", "Request timed out", 408)
  let error3 = Error::with_cause("file.not_found", "File not found", error1)
  
  assert_eq(Error::type(error1), "database.connection")
  assert_eq(Error::message(error1), "Failed to connect to database")
  assert_eq(Error::code(error1), None)
  
  assert_eq(Error::type(error2), "network.timeout")
  assert_eq(Error::message(error2), "Request timed out")
  match Error::code(error2) {
    Some(code) => assert_eq(code, 408)
    None => assert_true(false)
  }
  
  assert_eq(Error::type(error3), "file.not_found")
  assert_eq(Error::message(error3), "File not found")
  match Error::cause(error3) {
    Some(cause) => {
      assert_eq(Error::type(cause), "database.connection")
      assert_eq(Error::message(cause), "Failed to connect to database")
    }
    None => assert_true(false)
  }
  
  // Test result type operations
  let success_result = Result::Ok("operation successful")
  let error_result = Result::Error(error1)
  
  match success_result {
    Result::Ok(value) => assert_eq(value, "operation successful")
    Result::Error(_) => assert_true(false)
  }
  
  match error_result {
    Result::Ok(_) => assert_true(false)
    Result::Error(err) => {
      assert_eq(Error::type(err), "database.connection")
      assert_eq(Error::message(err), "Failed to connect to database")
    }
  }
  
  // Test result chaining
  let chained_result = Result::map(success_result, |value| {
    value + " with additional info"
  })
  
  match chained_result {
    Result::Ok(value) => assert_eq(value, "operation successful with additional info")
    Result::Error(_) => assert_true(false)
  }
  
  let error_chained_result = Result::map(error_result, |value| {
    value + " with additional info"
  })
  
  match error_chained_result {
    Result::Ok(_) => assert_true(false)
    Result::Error(err) => {
      assert_eq(Error::type(err), "database.connection")
      assert_eq(Error::message(err), "Failed to connect to database")
    }
  }
  
  // Test retry mechanism
  let mut attempt_count = 0
  let retry_result = Retry::execute(3, || {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Result::Error(Error::new("temporary.failure", "Temporary failure"))
    } else {
      Result::Ok("success after retries")
    }
  })
  
  match retry_result {
    Result::Ok(value) => {
      assert_eq(value, "success after retries")
      assert_eq(attempt_count, 3)
    }
    Result::Error(_) => assert_true(false)
  }
  
  // Test circuit breaker
  let circuit_breaker = CircuitBreaker::new(5, 1000) // 5 failures, 1000ms timeout
  
  // Initially closed
  assert_eq(CircuitBreaker::state(circuit_breaker), Closed)
  
  // Record failures
  for i = 0; i < 5; i = i + 1 {
    CircuitBreaker::record_failure(circuit_breaker)
  }
  
  // Should be open now
  assert_eq(CircuitBreaker::state(circuit_breaker), Open)
  
  // Test execution through circuit breaker
  let circuit_result = CircuitBreaker::execute(circuit_breaker, || {
    "operation executed"
  })
  
  match circuit_result {
    Result::Ok(_) => assert_true(false) // Should fail due to open circuit
    Result::Error(err) => {
      assert_eq(Error::type(err), "circuit_breaker.open")
    }
  }
  
  // Simulate time passage
  CircuitBreaker::advance_time(circuit_breaker, 1001) // 1001ms
  
  // Should be half-open now
  assert_eq(CircuitBreaker::state(circuit_breaker), HalfOpen)
  
  // Record success
  CircuitBreaker::record_success(circuit_breaker)
  
  // Should be closed again
  assert_eq(CircuitBreaker::state(circuit_breaker), Closed)
}

// Test 8: Data Structure Operations
test "data structure operations" {
  // Test stack operations
  let stack = Stack::new()
  
  assert_true(Stack::is_empty(stack))
  assert_eq(Stack::size(stack), 0)
  
  Stack::push(stack, "first")
  Stack::push(stack, "second")
  Stack::push(stack, "third")
  
  assert_false(Stack::is_empty(stack))
  assert_eq(Stack::size(stack), 3)
  
  let top = Stack::peek(stack)
  match top {
    Some(value) => assert_eq(value, "third")
    None => assert_true(false)
  }
  
  let popped1 = Stack::pop(stack)
  let popped2 = Stack::pop(stack)
  let popped3 = Stack::pop(stack)
  
  match popped1 {
    Some(value) => assert_eq(value, "third")
    None => assert_true(false)
  }
  
  match popped2 {
    Some(value) => assert_eq(value, "second")
    None => assert_true(false)
  }
  
  match popped3 {
    Some(value) => assert_eq(value, "first")
    None => assert_true(false)
  }
  
  assert_true(Stack::is_empty(stack))
  assert_eq(Stack::size(stack), 0)
  
  let empty_pop = Stack::pop(stack)
  assert_eq(empty_pop, None)
  
  // Test queue operations
  let queue = Queue::new()
  
  assert_true(Queue::is_empty(queue))
  assert_eq(Queue::size(queue), 0)
  
  Queue::enqueue(queue, "first")
  Queue::enqueue(queue, "second")
  Queue::enqueue(queue, "third")
  
  assert_false(Queue::is_empty(queue))
  assert_eq(Queue::size(queue), 3)
  
  let front = Queue::front(queue)
  match front {
    Some(value) => assert_eq(value, "first")
    None => assert_true(false)
  }
  
  let dequeued1 = Queue::dequeue(queue)
  let dequeued2 = Queue::dequeue(queue)
  let dequeued3 = Queue::dequeue(queue)
  
  match dequeued1 {
    Some(value) => assert_eq(value, "first")
    None => assert_true(false)
  }
  
  match dequeued2 {
    Some(value) => assert_eq(value, "second")
    None => assert_true(false)
  }
  
  match dequeued3 {
    Some(value) => assert_eq(value, "third")
    None => assert_true(false)
  }
  
  assert_true(Queue::is_empty(queue))
  assert_eq(Queue::size(queue), 0)
  
  let empty_dequeue = Queue::dequeue(queue)
  assert_eq(empty_dequeue, None)
  
  // Test priority queue
  let priority_queue = PriorityQueue::new()
  
  PriorityQueue::enqueue(priority_queue, ("low", 1))
  PriorityQueue::enqueue(priority_queue, ("high", 10))
  PriorityQueue::enqueue(priority_queue, ("medium", 5))
  PriorityQueue::enqueue(priority_queue, ("urgent", 15))
  
  let priority1 = PriorityQueue::dequeue(priority_queue)
  let priority2 = PriorityQueue::dequeue(priority_queue)
  let priority3 = PriorityQueue::dequeue(priority_queue)
  let priority4 = PriorityQueue::dequeue(priority_queue)
  
  match priority1 {
    Some((value, _)) => assert_eq(value, "urgent")
    None => assert_true(false)
  }
  
  match priority2 {
    Some((value, _)) => assert_eq(value, "high")
    None => assert_true(false)
  }
  
  match priority3 {
    Some((value, _)) => assert_eq(value, "medium")
    None => assert_true(false)
  }
  
  match priority4 {
    Some((value, _)) => assert_eq(value, "low")
    None => assert_true(false)
  }
  
  // Test tree operations
  let tree = Tree::new("root")
  
  let left_child = Tree::add_child(tree, "left")
  let right_child = Tree::add_child(tree, "right")
  
  let left_grandchild = Tree::add_child(left_child, "left-grandchild")
  let right_grandchild = Tree::add_child(right_child, "right-grandchild")
  
  assert_eq(Tree::value(tree), "root")
  assert_eq(Tree::value(left_child), "left")
  assert_eq(Tree::value(right_child), "right")
  assert_eq(Tree::value(left_grandchild), "left-grandchild")
  assert_eq(Tree::value(right_grandchild), "right-grandchild")
  
  // Test tree traversal
  let preorder = Tree::preorder_traversal(tree)
  let inorder = Tree::inorder_traversal(tree)
  let postorder = Tree::postorder_traversal(tree)
  
  assert_eq(preorder, ["root", "left", "left-grandchild", "right", "right-grandchild"])
  assert_eq(inorder, ["left-grandchild", "left", "root", "right-grandchild", "right"])
  assert_eq(postorder, ["left-grandchild", "left", "right-grandchild", "right", "root"])
  
  // Test graph operations
  let graph = Graph::new()
  
  let node1 = Graph::add_node(graph, "node1")
  let node2 = Graph::add_node(graph, "node2")
  let node3 = Graph::add_node(graph, "node3")
  let node4 = Graph::add_node(graph, "node4")
  
  Graph::add_edge(graph, node1, node2)
  Graph::add_edge(graph, node1, node3)
  Graph::add_edge(graph, node2, node4)
  Graph::add_edge(graph, node3, node4)
  
  // Test graph traversal
  let bfs_result = Graph::bfs(graph, node1)
  let dfs_result = Graph::dfs(graph, node1)
  
  assert_eq(bfs_result, [node1, node2, node3, node4])
  assert_eq(dfs_result, [node1, node2, node4, node3])
  
  // Test shortest path
  let shortest_path = Graph::shortest_path(graph, node1, node4)
  match shortest_path {
    Some(path) => assert_eq(path, [node1, node2, node4])
    None => assert_true(false)
  }
}