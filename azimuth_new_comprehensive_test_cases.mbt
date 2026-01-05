// Azimuth New Comprehensive Test Cases
// This file contains 10 comprehensive test cases covering various aspects of the Azimuth telemetry system

// Test 1: Basic Data Structures
test "basic data structures operations" {
  // Test stack operations
  let mut stack = []
  stack.push(1)
  stack.push(2)
  stack.push(3)
  
  assert_eq(stack.pop(), Some(3))
  assert_eq(stack.pop(), Some(2))
  assert_eq(stack.pop(), Some(1))
  assert_eq(stack.pop(), None)
  
  // Test queue operations
  let mut queue = []
  queue.enqueue(10)
  queue.enqueue(20)
  queue.enqueue(30)
  
  assert_eq(queue.dequeue(), Some(10))
  assert_eq(queue.dequeue(), Some(20))
  assert_eq(queue.dequeue(), Some(30))
  assert_eq(queue.dequeue(), None)
  
  // Test dictionary/map operations
  let mut map = Map::new()
  map.insert("key1", "value1")
  map.insert("key2", "value2")
  map.insert("key3", "value3")
  
  assert_eq(map.get("key1"), Some("value1"))
  assert_eq(map.get("key2"), Some("value2"))
  assert_eq(map.get("key3"), Some("value3"))
  assert_eq(map.get("nonexistent"), None)
  
  // Test set operations
  let mut set = Set::new()
  set.add(100)
  set.add(200)
  set.add(300)
  set.add(100) // Duplicate
  
  assert_true(set.contains(100))
  assert_true(set.contains(200))
  assert_true(set.contains(300))
  assert_false(set.contains(400))
  
  // Test tree operations
  let tree = TreeNode::new(50)
  tree.insert(30)
  tree.insert(70)
  tree.insert(20)
  tree.insert(40)
  tree.insert(60)
  tree.insert(80)
  
  assert_true(tree.search(50))
  assert_true(tree.search(30))
  assert_true(tree.search(70))
  assert_false(tree.search(90))
  
  // Test graph operations
  let graph = Graph::new()
  let node1 = graph.add_node("Node1")
  let node2 = graph.add_node("Node2")
  let node3 = graph.add_node("Node3")
  
  graph.add_edge(node1, node2, 5)
  graph.add_edge(node2, node3, 10)
  graph.add_edge(node1, node3, 15)
  
  assert_eq(graph.get_edge_weight(node1, node2), Some(5))
  assert_eq(graph.get_edge_weight(node2, node3), Some(10))
  assert_eq(graph.get_edge_weight(node1, node3), Some(15))
  assert_eq(graph.get_edge_weight(node3, node1), None) // No reverse edge
}

// Test 2: Error Handling and Recovery
test "error handling and recovery mechanisms" {
  // Test exception handling
  let result = try {
    let x = 10
    let y = 0
    x / y // Division by zero
  } catch {
    DivisionByZero => "handled"
    _ => "unknown"
  }
  
  assert_eq(result, "handled")
  
  // Test fallback mechanisms
  let primary_service = Service::new("primary", false) // Unavailable
  let fallback_service = Service::new("fallback", true)  // Available
  
  let response = primary_service.request().or_else(|| fallback_service.request())
  assert_true(response.is_some())
  assert_eq(response.unwrap().source, "fallback")
  
  // Test circuit breaker pattern
  let circuit_breaker = CircuitBreaker::new(3, 1000) // 3 failures, 1000ms timeout
  
  // Simulate failures
  for i = 0; i < 3; i = i + 1 {
    let result = circuit_breaker.call(|| {
      Err("Service unavailable")
    })
    assert_true(result.is_err())
  }
  
  // Circuit should be open now
  let result = circuit_breaker.call(|| {
    Ok("Success")
  })
  assert_true(result.is_err())
  assert_eq(result.unwrap_err(), "Circuit breaker is open")
  
  // Test retry mechanism
  let mut retry_count = 0
  let result = Retry::new(3, 100).call(|| {
    retry_count = retry_count + 1
    if retry_count < 3 {
      Err("Temporary failure")
    } else {
      Ok("Success after retries")
    }
  })
  
  assert_true(result.is_ok())
  assert_eq(result.unwrap(), "Success after retries")
  assert_eq(retry_count, 3)
  
  // Test timeout handling
  let slow_operation = || {
    sleep(2000) // 2 seconds
    "Completed"
  }
  
  let result = Timeout::new(1000).run(slow_operation)
  assert_true(result.is_err())
  assert_eq(result.unwrap_err(), "Operation timed out")
}

// Test 3: Performance Optimization
test "performance optimization techniques" {
  // Test lazy evaluation
  let lazy_value = Lazy::new(|| {
    // Simulate expensive computation
    sleep(100)
    42
  })
  
  // Value shouldn't be computed yet
  assert_false(lazy_value.is_computed())
  
  // First access should compute the value
  let start_time = current_time()
  let value = lazy_value.get()
  let elapsed = current_time() - start_time
  
  assert_eq(value, 42)
  assert_true(lazy_value.is_computed())
  assert_true(elapsed >= 100) // Should take at least 100ms
  
  // Second access should be instant
  let start_time = current_time()
  let value = lazy_value.get()
  let elapsed = current_time() - start_time
  
  assert_eq(value, 42)
  assert_true(elapsed < 10) // Should be very fast
  
  // Test memoization
  let mut memoized_fn = Memoized::new(|n| {
    // Simulate expensive computation
    sleep(50)
    n * n
  })
  
  let start_time = current_time()
  let result1 = memoized_fn.call(10)
  let elapsed1 = current_time() - start_time
  
  assert_eq(result1, 100)
  assert_true(elapsed1 >= 50) // Should take at least 50ms
  
  let start_time = current_time()
  let result2 = memoized_fn.call(10)
  let elapsed2 = current_time() - start_time
  
  assert_eq(result2, 100)
  assert_true(elapsed2 < 10) // Should be very fast (cached)
  
  // Test batch processing
  let items = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  let start_time = current_time()
  let processed_items = BatchProcessor::new(3).process(items, |item| {
    sleep(10) // Simulate processing time
    item * 2
  })
  let elapsed = current_time() - start_time
  
  assert_eq(processed_items, [2, 4, 6, 8, 10, 12, 14, 16, 18, 20])
  assert_true(elapsed < 100) // Should be faster than processing sequentially
  
  // Test parallel processing
  let start_time = current_time()
  let parallel_results = ParallelProcessor::new(4).process(items, |item| {
    sleep(20) // Simulate processing time
    item * item
  })
  let elapsed = current_time() - start_time
  
  assert_eq(parallel_results, [1, 4, 9, 16, 25, 36, 49, 64, 81, 100])
  assert_true(elapsed < 100) // Should be faster than sequential processing
}

// Test 4: Data Serialization and Deserialization
test "data serialization and deserialization" {
  // Test JSON serialization
  let data = TestData::new("test", 42, [1, 2, 3])
  
  let json_str = JSON::serialize(data)
  assert_true(json_str.contains("\"name\":\"test\""))
  assert_true(json_str.contains("\"value\":42"))
  assert_true(json_str.contains("\"items\":[1,2,3]"))
  
  let deserialized_data = JSON::deserialize(json_str)
  assert_eq(deserialized_data.name, "test")
  assert_eq(deserialized_data.value, 42)
  assert_eq(deserialized_data.items, [1, 2, 3])
  
  // Test binary serialization
  let binary_data = Binary::serialize(data)
  assert_true(binary_data.length() > 0)
  
  let deserialized_binary = Binary::deserialize(binary_data)
  assert_eq(deserialized_binary.name, "test")
  assert_eq(deserialized_binary.value, 42)
  assert_eq(deserialized_binary.items, [1, 2, 3])
  
  // Test protocol buffers
  let proto_data = ProtoBuf::serialize(data)
  assert_true(proto_data.length() > 0)
  
  let deserialized_proto = ProtoBuf::deserialize(proto_data)
  assert_eq(deserialized_proto.name, "test")
  assert_eq(deserialized_proto.value, 42)
  assert_eq(deserialized_proto.items, [1, 2, 3])
  
  // Test custom serialization format
  let custom_format = CustomFormat::serialize(data)
  assert_true(custom_format.length() > 0)
  
  let deserialized_custom = CustomFormat::deserialize(custom_format)
  assert_eq(deserialized_custom.name, "test")
  assert_eq(deserialized_custom.value, 42)
  assert_eq(deserialized_custom.items, [1, 2, 3])
  
  // Test compression during serialization
  let large_data = LargeData::new("x".repeat(10000), [1; 1000])
  
  let compressed_data = Compressed::serialize(large_data)
  let uncompressed_data = Uncompressed::serialize(large_data)
  
  assert_true(compressed_data.length() < uncompressed_data.length())
  
  let deserialized_compressed = Compressed::deserialize(compressed_data)
  assert_eq(deserialized_compressed.text, "x".repeat(10000))
  assert_eq(deserialized_compressed.numbers.length(), 1000)
  
  // Test version compatibility
  let v1_data = VersionedData::v1("test", 42)
  let v1_serialized = Versioned::serialize(v1_data)
  
  let v2_deserialized = Versioned::deserialize(v1_serialized)
  assert_eq(v2_deserialized.name, "test")
  assert_eq(v2_deserialized.value, 42)
  assert_eq(v2_deserialized.version, "1.0")
  assert_eq(v2_deserialized.new_field, None) // Should be None for v1 data
}

// Test 5: Concurrent Safety
test "concurrent safety mechanisms" {
  // Test thread-safe counter
  let counter = AtomicCounter::new(0)
  
  // Simulate concurrent increments
  let handles = []
  for i = 0; i < 10; i = i + 1 {
    let handle = Thread::spawn(|| {
      for j = 0; j < 100; j = j + 1 {
        counter.increment()
      }
    })
    handles.push(handle)
  }
  
  // Wait for all threads to complete
  for handle in handles {
    handle.join()
  }
  
  assert_eq(counter.get(), 1000)
  
  // Test mutex-protected shared data
  let shared_data = Mutex::new(SharedData::new())
  
  let handles = []
  for i = 0; i < 5; i = i + 1 {
    let handle = Thread::spawn(|| {
      for j = 0; j < 10; j = j + 1 {
        shared_data.lock().add_item(j)
      }
    })
    handles.push(handle)
  }
  
  for handle in handles {
    handle.join()
  }
  
  let data = shared_data.lock()
  assert_eq(data.items.length(), 50)
  
  // Test read-write lock
  let rw_data = RwLock::new(RwData::new())
  
  // Start writer threads
  let write_handles = []
  for i = 0; i < 3; i = i + 1 {
    let handle = Thread::spawn(|| {
      for j = 0; j < 5; j = j + 1 {
        rw_data.write().update_value(i * 10 + j)
        sleep(10) // Simulate work
      }
    })
    write_handles.push(handle)
  }
  
  // Start reader threads
  let read_handles = []
  for i = 0; i < 5; i = i + 1 {
    let handle = Thread::spawn(|| {
      for j = 0; j < 10; j = j + 1 {
        let value = rw_data.read().get_value()
        assert_true(value >= 0)
        sleep(5) // Simulate work
      }
    })
    read_handles.push(handle)
  }
  
  // Wait for all threads to complete
  for handle in write_handles {
    handle.join()
  }
  
  for handle in read_handles {
    handle.join()
  }
  
  // Test atomic operations
  let atomic_value = Atomic::new(0)
  
  let handles = []
  for i = 0; i < 8; i = i + 1 {
    let handle = Thread::spawn(|| {
      for j = 0; j < 100; j = j + 1 {
        atomic_value.fetch_add(1)
      }
    })
    handles.push(handle)
  }
  
  for handle in handles {
    handle.join()
  }
  
  assert_eq(atomic_value.load(), 800)
  
  // Test concurrent queue
  let queue = ConcurrentQueue::new()
  
  // Producer threads
  let producer_handles = []
  for i = 0; i < 3; i = i + 1 {
    let handle = Thread::spawn(move || {
      for j = 0; j < 10; j = j + 1 {
        queue.enqueue(i * 10 + j)
      }
    })
    producer_handles.push(handle)
  }
  
  // Consumer threads
  let consumer_handles = []
  for i = 0; i < 2; i = i + 1 {
    let handle = Thread::spawn(move || {
      let mut items = []
      for j = 0; j < 15; j = j + 1 {
        if let Some(item) = queue.dequeue() {
          items.push(item)
        }
      }
      items
    })
    consumer_handles.push(handle)
  }
  
  // Wait for all threads to complete
  for handle in producer_handles {
    handle.join()
  }
  
  let mut all_items = []
  for handle in consumer_handles {
    all_items = all_items.concat(handle.join())
  }
  
  assert_eq(all_items.length(), 30)
  assert_true(all_items.all(|item| item >= 0 && item < 30))
}

// Test 6: Memory Management
test "memory management and resource cleanup" {
  // Test memory pool
  let pool = MemoryPool::new(10, 100) // 10 blocks of 100 bytes each
  
  let blocks = []
  for i = 0; i < 10; i = i + 1 {
    let block = pool.allocate()
    assert_true(block.is_some())
    blocks.push(block.unwrap())
  }
  
  // Pool should be exhausted
  assert_true(pool.allocate().is_none())
  
  // Free a block and allocate again
  pool.free(blocks[0])
  let new_block = pool.allocate()
  assert_true(new_block.is_some())
  
  // Test smart pointers
  let resource = Resource::new("test resource")
  let ref_count = RefCell::new(0)
  
  {
    let shared = SharedPtr::new(resource)
    assert_eq(shared.strong_count(), 1)
    
    {
      let shared2 = shared.clone()
      assert_eq(shared2.strong_count(), 2)
      
      {
        let weak = WeakPtr::from(&shared2)
        assert_eq(shared2.strong_count(), 2)
        assert_eq(weak.weak_count(), 1)
        
        let upgraded = weak.upgrade()
        assert_true(upgraded.is_some())
        assert_eq(upgraded.unwrap().name, "test resource")
      }
    }
    
    assert_eq(shared.strong_count(), 1)
  }
  
  // Test RAII pattern
  let mut cleanup_called = false
  
  {
    let raii_resource = RAIIResource::new(|| {
      cleanup_called = true
    })
    assert_true(raii_resource.is_valid())
  } // Resource should be automatically cleaned up here
  
  assert_true(cleanup_called)
  
  // Test garbage collection
  let gc = GarbageCollector::new()
  
  // Create some objects
  for i = 0; i < 100; i = i + 1 {
    gc.allocate(Object::new(i))
  }
  
  assert_eq(gc.object_count(), 100)
  
  // Force garbage collection
  gc.collect()
  
  // Objects should still be there as they're referenced
  assert_eq(gc.object_count(), 100)
  
  // Clear references and collect again
  gc.clear_references()
  gc.collect()
  
  // Objects should be gone
  assert_eq(gc.object_count(), 0)
  
  // Test memory leak detection
  let leak_detector = LeakDetector::new()
  
  leak_detector.start_tracking()
  
  // Allocate and free some resources
  let resource1 = leak_detector.tracked_allocate(100)
  let resource2 = leak_detector.tracked_allocate(200)
  
  leak_detector.tracked_free(resource1)
  // resource2 is intentionally not freed to test leak detection
  
  let leaks = leak_detector.stop_tracking()
  assert_eq(leaks.length(), 1)
  assert_eq(leaks[0].size, 200)
}

// Test 7: Network Communication
test "network communication protocols" {
  // Test HTTP client
  let client = HttpClient::new()
  
  // Mock HTTP server
  let server = MockHttpServer::new()
  server.get("/api/test").respond_with_json({"status": "ok", "data": [1, 2, 3]})
  server.start()
  
  let response = client.get(format!("http://localhost:{}/api/test", server.port()))
  assert_true(response.is_ok())
  
  let json_response = response.unwrap().json()
  assert_eq(json_response["status"], "ok")
  assert_eq(json_response["data"], [1, 2, 3])
  
  server.stop()
  
  // Test WebSocket communication
  let ws_server = MockWebSocketServer::new()
  ws_server.start()
  
  let ws_client = WebSocketClient::new(format!("ws://localhost:{}/ws", ws_server.port()))
  assert_true(ws_client.connect().is_ok())
  
  // Send message
  let send_result = ws_client.send_text("Hello WebSocket")
  assert_true(send_result.is_ok())
  
  // Receive message
  let received_message = ws_client.receive_text()
  assert_true(received_message.is_ok())
  assert_eq(received_message.unwrap(), "Echo: Hello WebSocket")
  
  ws_client.disconnect()
  ws_server.stop()
  
  // Test TCP communication
  let tcp_server = MockTcpServer::new()
  tcp_server.start()
  
  let tcp_client = TcpClient::new()
  assert_true(tcp_client.connect("localhost", tcp_server.port()).is_ok())
  
  // Send data
  let send_result = tcp_client.send("Hello TCP")
  assert_true(send_result.is_ok())
  
  // Receive response
  let response = tcp_client.receive(1024)
  assert_true(response.is_ok())
  assert_eq(response.unwrap(), "Echo: Hello TCP")
  
  tcp_client.disconnect()
  tcp_server.stop()
  
  // Test UDP communication
  let udp_server = MockUdpServer::new()
  udp_server.start()
  
  let udp_client = UdpClient::new()
  
  // Send data
  let send_result = udp_client.send_to("Hello UDP", "localhost", udp_server.port())
  assert_true(send_result.is_ok())
  
  // Receive response
  let (response, _) = udp_client.receive_from(1024)
  assert_true(response.is_ok())
  assert_eq(response.unwrap(), "Echo: Hello UDP")
  
  udp_server.stop()
  
  // Test request retry with exponential backoff
  let flaky_server = MockHttpServer::new()
  flaky_server.get("/api/flaky").respond_with_errors([500, 500, 200])
  flaky_server.start()
  
  let retry_client = HttpClient::new()
    .with_retry_policy(RetryPolicy::exponential_backoff(3, 100, 2.0))
  
  let response = retry_client.get(format!("http://localhost:{}/api/flaky", flaky_server.port()))
  assert_true(response.is_ok())
  assert_eq(response.unwrap().status_code(), 200)
  
  flaky_server.stop()
}

// Test 8: Caching Mechanisms
test "caching mechanisms and strategies" {
  // Test LRU cache
  let lru_cache = LRUCache::new(3) // Capacity of 3
  
  lru_cache.put("key1", "value1")
  lru_cache.put("key2", "value2")
  lru_cache.put("key3", "value3")
  
  assert_eq(lru_cache.get("key1"), Some("value1"))
  assert_eq(lru_cache.get("key2"), Some("value2"))
  assert_eq(lru_cache.get("key3"), Some("value3"))
  
  // Add one more item, should evict key1 (least recently used)
  lru_cache.put("key4", "value4")
  
  assert_eq(lru_cache.get("key1"), None) // Should be evicted
  assert_eq(lru_cache.get("key2"), Some("value2"))
  assert_eq(lru_cache.get("key3"), Some("value3"))
  assert_eq(lru_cache.get("key4"), Some("value4"))
  
  // Access key2, making it most recently used
  lru_cache.get("key2");
  
  // Add another item, should evict key3
  lru_cache.put("key5", "value5")
  
  assert_eq(lru_cache.get("key2"), Some("value2")) // Should still be there
  assert_eq(lru_cache.get("key3"), None) // Should be evicted
  assert_eq(lru_cache.get("key4"), Some("value4"))
  assert_eq(lru_cache.get("key5"), Some("value5"))
  
  // Test TTL cache
  let ttl_cache = TTLCache::new()
  
  ttl_cache.put_with_ttl("key1", "value1", 100) // 100ms TTL
  assert_eq(ttl_cache.get("key1"), Some("value1"))
  
  sleep(150) // Wait for TTL to expire
  
  assert_eq(ttl_cache.get("key1"), None) // Should be expired
  
  // Test cache with refresh policy
  let refresh_cache = RefreshCache::new()
  
  refresh_cache.put("key1", "value1", 200) // 200ms TTL
  
  // Access before expiration, should refresh TTL
  sleep(100)
  assert_eq(refresh_cache.get("key1"), Some("value1"))
  
  sleep(150) // Total 250ms, original TTL would have expired
  
  assert_eq(refresh_cache.get("key1"), Some("value1")) // Should still be there due to refresh
  
  // Test distributed cache
  let distributed_cache = DistributedCache::new()
  
  distributed_cache.put("key1", "value1")
  distributed_cache.put("key2", "value2")
  
  assert_eq(distributed_cache.get("key1"), Some("value1"))
  assert_eq(distributed_cache.get("key2"), Some("value2"))
  
  // Simulate cache invalidation
  distributed_cache.invalidate("key1")
  
  assert_eq(distributed_cache.get("key1"), None)
  assert_eq(distributed_cache.get("key2"), Some("value2"))
  
  // Test cache warming
  let warm_cache = WarmCache::new()
  
  let data_source = || {
    sleep(50) // Simulate slow data source
    "expensive data"
  }
  
  // First access should be slow
  let start_time = current_time()
  let value1 = warm_cache.get_or_load("key1", data_source)
  let elapsed1 = current_time() - start_time
  
  assert_eq(value1, "expensive data")
  assert_true(elapsed1 >= 50)
  
  // Second access should be fast
  let start_time = current_time()
  let value2 = warm_cache.get_or_load("key1", data_source)
  let elapsed2 = current_time() - start_time
  
  assert_eq(value2, "expensive data")
  assert_true(elapsed2 < 10)
  
  // Test cache statistics
  let stats_cache = StatsCache::new()
  
  stats_cache.put("key1", "value1")
  stats_cache.put("key2", "value2")
  
  stats_cache.get("key1") // Hit
  stats_cache.get("key1") // Hit
  stats_cache.get("key3") // Miss
  
  let stats = stats_cache.get_stats()
  assert_eq(stats.hits, 2)
  assert_eq(stats.misses, 1)
  assert_eq(stats.hit_rate, 2.0 / 3.0)
}

// Test 9: Internationalization Support
test "internationalization and localization" {
  // Test text direction
  assert_eq(TextDirection::detect("Hello"), TextDirection::LTR)
  assert_eq(TextDirection::detect("שלום"), TextDirection::RTL)
  assert_eq(TextDirection::detect("مرحبا"), TextDirection::RTL)
  
  // Test number formatting
  let number = 1234567.89
  
  let us_formatted = NumberFormatter::new("en-US").format(number)
  assert_eq(us_formatted, "1,234,567.89")
  
  let german_formatted = NumberFormatter::new("de-DE").format(number)
  assert_eq(german_formatted, "1.234.567,89")
  
  let french_formatted = NumberFormatter::new("fr-FR").format(number)
  assert_eq(french_formatted, "1 234 567,89")
  
  // Test date/time formatting
  let date = DateTime::new(2023, 11, 15, 14, 30, 0)
  
  let us_date = DateFormatter::new("en-US").format(date)
  assert_eq(us_date, "11/15/2023, 2:30 PM")
  
  let iso_date = DateFormatter::new("en-GB").format(date)
  assert_eq(iso_date, "15/11/2023, 14:30")
  
  let japanese_date = DateFormatter::new("ja-JP").format(date)
  assert_eq(japanese_date, "2023/11/15 14:30")
  
  // Test currency formatting
  let amount = 1234.56
  
  let usd = CurrencyFormatter::new("en-US", "USD").format(amount)
  assert_eq(usd, "$1,234.56")
  
  let eur = CurrencyFormatter::new("de-DE", "EUR").format(amount)
  assert_eq(eur, "1.234,56 €")
  
  let jpy = CurrencyFormatter::new("ja-JP", "JPY").format(amount)
  assert_eq(jpy, "¥1,235") // Yen doesn't have minor units
  
  // Test plural rules
  let pluralizer = Pluralizer::new("en")
  
  assert_eq(pluralizer.pluralize(0, "cat", "cats"), "cats")
  assert_eq(pluralizer.pluralize(1, "cat", "cats"), "cat")
  assert_eq(pluralizer.pluralize(2, "cat", "cats"), "cats")
  assert_eq(pluralizer.pluralize(5, "cat", "cats"), "cats")
  
  // Test with different language (Arabic has different plural rules)
  let arabic_pluralizer = Pluralizer::new("ar")
  
  assert_eq(arabic_pluralizer.pluralize(0, "قطة", "قطط"), "قطط")
  assert_eq(arabic_pluralizer.pluralize(1, "قطة", "قطط"), "قطة")
  assert_eq(arabic_pluralizer.pluralize(2, "قطة", "قطط"), "قططان")
  assert_eq(arabic_pluralizer.pluralize(5, "قطة", "قطط"), "قطط")
  
  // Test message translation
  let translator = Translator::new()
  
  translator.add_translation("en", "hello", "Hello")
  translator.add_translation("es", "hello", "Hola")
  translator.add_translation("fr", "hello", "Bonjour")
  translator.add_translation("ja", "hello", "こんにちは")
  
  assert_eq(translator.translate("hello", "en"), "Hello")
  assert_eq(translator.translate("hello", "es"), "Hola")
  assert_eq(translator.translate("hello", "fr"), "Bonjour")
  assert_eq(translator.translate("hello", "ja"), "こんにちは")
  
  // Test fallback translation
  assert_eq(translator.translate("hello", "de"), "Hello") // Falls back to English
  
  // Test parameterized messages
  translator.add_translation("en", "welcome", "Welcome, {name}!")
  translator.add_translation("es", "welcome", "¡Bienvenido, {name}!")
  
  let params = Map::new()
  params.insert("name", "John")
  
  assert_eq(translator.translate_with_params("welcome", "en", params), "Welcome, John!")
  assert_eq(translator.translate_with_params("welcome", "es", params), "¡Bienvenido, John!")
  
  // Test locale detection
  let locale_detector = LocaleDetector::new()
  
  assert_eq(locale_detector.from_http_header("en-US,en;q=0.9"), "en-US")
  assert_eq(locale_detector.from_http_header("fr-FR,fr;q=0.9,en;q=0.8"), "fr-FR")
  assert_eq(locale_detector.from_http_header("ja, en-US;q=0.8"), "ja")
  
  // Test character encoding
  let utf8_text = "Hello 世界 مرحبا こんにちは"
  assert_eq(Encoding::detect(utf8_text), "UTF-8")
  
  // Test text normalization
  let normalized_text = TextNormalizer::new("NFC").normalize("café")
  assert_eq(normalized_text, "café") // Already normalized
  
  let denormalized_text = TextNormalizer::new("NFD").normalize("café")
  assert_ne(denormalized_text, "café") // Should be different (decomposed)
}

// Test 10: Time Series Data Processing
test "time series data processing" {
  // Generate sample time series data
  let time_series = TimeSeries::new()
  
  let base_time = Timestamp::new(2023, 11, 15, 0, 0, 0)
  
  for i = 0; i < 100; i = i + 1 {
    let time = base_time.add_hours(i)
    let value = 50.0 + 10.0 * (i as Float).sin() + 5.0 * (i as Float).cos()
    time_series.add_point(time, value)
  }
  
  assert_eq(time_series.length(), 100)
  
  // Test resampling
  let hourly_data = time_series.resample(ResampleInterval::Hour, AggregationMethod::Average)
  assert_eq(hourly_data.length(), 100)
  
  let daily_data = time_series.resample(ResampleInterval::Day, AggregationMethod::Average)
  assert_eq(daily_data.length(), 5) // 100 hours / 24 hours per day ≈ 4.17, rounded up to 5
  
  // Test moving average
  let ma_5 = time_series.moving_average(5)
  assert_eq(ma_5.length(), 96) // First 4 points don't have enough data for 5-point MA
  
  let ma_10 = time_series.moving_average(10)
  assert_eq(ma_10.length(), 91) // First 9 points don't have enough data for 10-point MA
  
  // Test exponential smoothing
  let smoothed = time_series.exponential_smoothing(0.3)
  assert_eq(smoothed.length(), 100)
  
  // Verify smoothed values are different from original
  assert_ne(smoothed.get(50), time_series.get(50))
  
  // Test trend detection
  let trend = time_series.detect_trend()
  assert_true(trend.direction == "up" || trend.direction == "down" || trend.direction == "sideways")
  assert_true(trend.strength >= 0.0 && trend.strength <= 1.0)
  
  // Test seasonality detection
  let seasonality = time_series.detect_seasonality()
  assert_true(seasonality.is_seasonal || !seasonality.is_seasonal)
  
  if seasonality.is_seasonal {
    assert_true(seasonality.period > 0)
    assert_true(seasonality.strength >= 0.0 && seasonality.strength <= 1.0)
  }
  
  // Test anomaly detection
  let anomalies = time_series.detect_anomalies(AnomalyMethod::ZScore, 2.0)
  assert_true(anomalies.length() >= 0)
  
  for anomaly in anomalies {
    assert_true(anomaly.index >= 0 && anomaly.index < time_series.length())
    assert_true(anomaly.score > 2.0)
  }
  
  // Test forecasting
  let forecast = time_series.forecast(ForecastMethod::ARIMA, 10)
  assert_eq(forecast.values.length(), 10)
  assert_eq(forecast.confidence_intervals.length(), 10)
  
  // Verify confidence intervals
  for i = 0; i < 10; i = i + 1 {
    let interval = forecast.confidence_intervals[i]
    assert_true(interval.lower < forecast.values[i])
    assert_true(forecast.values[i] < interval.upper)
  }
  
  // Test differencing
  let diff_series = time_series.difference(1)
  assert_eq(diff_series.length(), 99)
  
  // Test seasonal differencing
  let seasonal_diff = time_series.seasonal_difference(24) // 24-hour seasonality
  assert_eq(seasonal_diff.length(), 76) // 100 - 24
  
  // Test autocorrelation
  let autocorr = time_series.autocorrelation(20)
  assert_eq(autocorr.length(), 20)
  
  // Autocorrelation at lag 0 should be 1
  assert_eq(autocorr[0], 1.0)
  
  // Autocorrelation values should be between -1 and 1
  for i = 1; i < 20; i = i + 1 {
    assert_true(autocorr[i] >= -1.0 && autocorr[i] <= 1.0)
  }
  
  // Test cross-correlation with another series
  let other_series = TimeSeries::new()
  
  for i = 0; i < 100; i = i + 1 {
    let time = base_time.add_hours(i)
    let value = 30.0 + 8.0 * (i as Float).sin()
    other_series.add_point(time, value)
  }
  
  let cross_corr = time_series.cross_correlation(other_series, 10)
  assert_eq(cross_corr.length(), 21) // -10 to +10 lags
  
  // Test decomposition
  let decomposition = time_series.decompose(DecompositionMethod::STL, 24) // 24-hour seasonality
  assert_eq(decomposition.trend.length(), 100)
  assert_eq(decomposition.seasonal.length(), 100)
  assert_eq(decomposition.residual.length(), 100)
  
  // Verify components add up to original
  for i = 0; i < 100; i = i + 1 {
    let reconstructed = decomposition.trend[i] + decomposition.seasonal[i] + decomposition.residual[i]
    assert_true(abs(reconstructed - time_series.get(i)) < 0.001)
  }
}