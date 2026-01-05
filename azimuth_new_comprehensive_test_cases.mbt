// Azimuth New Comprehensive Test Cases
// This file contains new test cases covering various MoonBit language features and telemetry system functionality

// Test 1: Async Programming Patterns and Promise Handling
test "async programming patterns and promise handling" {
  // Simulate async operations with promises
  enum Promise[T] {
    Pending
    Fulfilled(T)
    Rejected(String)
  }
  
  type AsyncTelemetryData = {
    trace_id: String,
    span_data: Array[String],
    metrics: Array[String]
  }
  
  // Create async operations
  let fetch_telemetry_data = fn(trace_id: String) {
    // Simulate async operation that would fetch telemetry data
    if trace_id.length() == 16 {
      Promise::Fulfilled({
        trace_id,
        span_data: ["database_query", "api_call", "cache_lookup"],
        metrics: ["cpu_usage:75.5", "memory_usage:60.2", "disk_io:12.3"]
      })
    } else {
      Promise::Rejected("Invalid trace ID format")
    }
  }
  
  let process_metrics = fn(data: AsyncTelemetryData) {
    let processed_metrics = []
    for metric in data.metrics {
      let parts = metric.split(":")
      if parts.length() == 2 {
        processed_metrics = processed_metrics.push({
          name: parts[0],
          value: parts[1]
        })
      }
    }
    {
      trace_id: data.trace_id,
      processed_metrics,
      span_count: data.span_data.length()
    }
  }
  
  // Promise chaining simulation
  let chain_promises = fn(trace_id: String) {
    match fetch_telemetry_data(trace_id) {
      Promise::Fulfilled(data) => Promise::Fulfilled(process_metrics(data))
      Promise::Rejected(error) => Promise::Rejected(error)
      Promise::Pending => Promise::Pending
    }
  }
  
  // Test successful async operation
  let valid_trace_id = "0af7651916cd43dd"
  let result = chain_promises(valid_trace_id)
  
  match result {
    Promise::Fulfilled(processed_data) => {
      assert_eq(processed_data.trace_id, valid_trace_id)
      assert_eq(processed_data.span_count, 3)
      assert_eq(processed_data.processed_metrics.length(), 3)
      assert_eq(processed_data.processed_metrics[0].name, "cpu_usage")
      assert_eq(processed_data.processed_metrics[0].value, "75.5")
    }
    _ => assert_true(false)
  }
  
  // Test failed async operation
  let invalid_trace_id = "invalid"
  let error_result = chain_promises(invalid_trace_id)
  
  match error_result {
    Promise::Rejected(error) => assert_eq(error, "Invalid trace ID format")
    _ => assert_true(false)
  }
  
  // Promise.all simulation
  let promise_all = fn(promises: Array[Promise[String]]) {
    let mut all_fulfilled = true
    let mut results = []
    let mut has_rejection = false
    let mut rejection_error = ""
    
    for promise in promises {
      match promise {
        Promise::Fulfilled(value) => {
          results = results.push(value)
        }
        Promise::Rejected(error) => {
          has_rejection = true
          rejection_error = error
          all_fulfilled = false
        }
        Promise::Pending => {
          all_fulfilled = false
        }
      }
    }
    
    if has_rejection {
      Promise::Rejected(rejection_error)
    } else if all_fulfilled {
      Promise::Fulfilled(results)
    } else {
      Promise::Pending
    }
  }
  
  let promises = [
    Promise::Fulfilled("trace1_data"),
    Promise::Fulfilled("trace2_data"),
    Promise::Fulfilled("trace3_data")
  ]
  
  let all_result = promise_all(promises)
  match all_result {
    Promise::Fulfilled(all_data) => {
      assert_eq(all_data.length(), 3)
      assert_eq(all_data[0], "trace1_data")
      assert_eq(all_data[1], "trace2_data")
      assert_eq(all_data[2], "trace3_data")
    }
    _ => assert_true(false)
  }
}

// Test 2: Advanced Data Structures - Trie Implementation
test "advanced data structures - trie implementation" {
  // Trie node for efficient string searching in telemetry data
  enum TrieNode {
    Node(Bool, Array[(Char, TrieNode)])  // (is_end, children)
  }
  
  let create_node = fn() {
    TrieNode::Node(false, [])
  }
  
  let insert = fn(trie: TrieNode, word: String) {
    let insert_recursive = fn(node: TrieNode, chars: Array[Char], index: Int) {
      match node {
        TrieNode::Node(is_end, children) => {
          if index >= chars.length() {
            TrieNode::Node(true, children)
          } else {
            let current_char = chars[index]
            let mut found_child = None
            let mut updated_children = []
            
            // Look for existing child
            for (char, child_node) in children {
              if char == current_char {
                found_child = Some(child_node)
              } else {
                updated_children = updated_children.push((char, child_node))
              }
            }
            
            // Update or create child
            let new_child = match found_child {
              Some(child) => insert_recursive(child, chars, index + 1)
              None => insert_recursive(create_node(), chars, index + 1)
            }
            
            updated_children = updated_children.push((current_char, new_child))
            TrieNode::Node(is_end, updated_children)
          }
        }
      }
    }
    
    insert_recursive(trie, word.to_array(), 0)
  }
  
  let search = fn(trie: TrieNode, word: String) {
    let search_recursive = fn(node: TrieNode, chars: Array[Char], index: Int) {
      match node {
        TrieNode::Node(is_end, children) => {
          if index >= chars.length() {
            is_end
          } else {
            let current_char = chars[index]
            let mut found = false
            
            for (char, child_node) in children {
              if char == current_char {
                found = true
                return search_recursive(child_node, chars, index + 1)
              }
            }
            
            false
          }
        }
      }
    }
    
    search_recursive(trie, word.to_array(), 0)
  }
  
  let starts_with = fn(trie: TrieNode, prefix: String) {
    let starts_with_recursive = fn(node: TrieNode, chars: Array[Char], index: Int) {
      match node {
        TrieNode::Node(_, children) => {
          if index >= chars.length() {
            true
          } else {
            let current_char = chars[index]
            
            for (char, child_node) in children {
              if char == current_char {
                return starts_with_recursive(child_node, chars, index + 1)
              }
            }
            
            false
          }
        }
      }
    }
    
    starts_with_recursive(trie, prefix.to_array(), 0)
  }
  
  // Build trie with telemetry operation names
  let mut trie = create_node()
  let operations = ["get", "post", "put", "delete", "patch", "options", "head", "trace"]
  
  for op in operations {
    trie = insert(trie, op)
  }
  
  // Test search
  assert_true(search(trie, "get"))
  assert_true(search(trie, "post"))
  assert_true(search(trie, "put"))
  assert_false(search(trie, "invalid"))
  assert_false(search(trie, "getpost"))
  
  // Test starts with
  assert_true(starts_with(trie, "g"))
  assert_true(starts_with(trie, "p"))
  assert_true(starts_with(trie, "pu"))
  assert_false(starts_with(trie, "x"))
  assert_false(starts_with(trie, "getpost"))
  
  // Test prefix matching for telemetry data filtering
  let telemetry_prefixes = ["span_", "metric_", "log_", "trace_"]
  
  for prefix in telemetry_prefixes {
    trie = insert(trie, prefix)
  }
  
  assert_true(starts_with(trie, "span_"))
  assert_true(starts_with(trie, "metric_"))
  assert_true(starts_with(trie, "log_"))
  assert_true(starts_with(trie, "trace_"))
  assert_false(starts_with(trie, "event_"))
}

// Test 3: Error Handling with Result Types and Recovery
test "error handling with result types and recovery" {
  // Enhanced error types for telemetry system
  enum TelemetryError {
    NetworkError(String, Int)  // (message, retry_count)
    SerializationError(String)
    ValidationError(String, Array[String])  // (field, errors)
    RateLimitError(Int, String)  // (retry_after, limit_type)
    SystemError(String, String)  // (component, error_code)
    TimeoutError(Int, String)  // (timeout_ms, operation)
  }
  
  type Result[T] = {
    success: Bool,
    data: Option[T],
    error: Option[TelemetryError>,
    metadata: Array[String]
  }
  
  // Result constructors
  let ok = fn(data: T, metadata: Array[String]) {
    {
      success: true,
      data: Some(data),
      error: None,
      metadata
    }
  }
  
  let err = fn(error: TelemetryError, metadata: Array[String]) {
    {
      success: false,
      data: None,
      error: Some(error),
      metadata
    }
  }
  
  // Error mapping and transformation
  let map_error = fn(result: Result[T], mapper: (TelemetryError) -> TelemetryError) {
    if result.success {
      result
    } else {
      match result.error {
        Some(error) => err(mapper(error), result.metadata)
        None => result
      }
    }
  }
  
  // Error chaining with context
  let chain_error = fn(result: Result[T], context: String) {
    if result.success {
      result
    } else {
      match result.error {
        Some(error) => {
          let enhanced_error = match error {
            TelemetryError::NetworkError(msg, count) => 
              TelemetryError::NetworkError(context + ": " + msg, count)
            TelemetryError::SerializationError(msg) => 
              TelemetryError::SerializationError(context + ": " + msg)
            TelemetryError::ValidationError(field, errors) => 
              TelemetryError::ValidationError(field, errors.map(fn(e) { context + ": " + e }))
            TelemetryError::RateLimitError(retry, limit_type) => 
              TelemetryError::RateLimitError(retry, context + ": " + limit_type)
            TelemetryError::SystemError(component, code) => 
              TelemetryError::SystemError(context + "." + component, code)
            TelemetryError::TimeoutError(timeout, operation) => 
              TelemetryError::TimeoutError(timeout, context + "." + operation)
          }
          err(enhanced_error, result.metadata.push("Error chained at: " + context))
        }
        None => result
      }
    }
  }
  
  // Retry mechanism with exponential backoff
  let retry_with_backoff = fn(operation: () -> Result[String], max_attempts: Int) {
    let mut attempt = 0
    let mut result = operation()
    let mut backoff_ms = 100
    
    while attempt < max_attempts and not(result.success) {
      match result.error {
        Some(TelemetryError::NetworkError(_, retry_count)) => {
          if retry_count < 3 {
            attempt = attempt + 1
            backoff_ms = backoff_ms * 2
            result = operation()
          } else {
            break
          }
        }
        Some(TelemetryError::RateLimitError(retry_after, _)) => {
          attempt = attempt + 1
          backoff_ms = retry_after
          result = operation()
        }
        Some(TelemetryError::TimeoutError(_, _)) => {
          attempt = attempt + 1
          backoff_ms = backoff_ms * 2
          result = operation()
        }
        _ => {
          break  // Don't retry other errors
        }
      }
    }
    
    result
  }
  
  // Test error creation and mapping
  let network_error = err(
    TelemetryError::NetworkError("Connection failed", 1),
    ["Initial attempt"]
  )
  
  let mapped_error = map_error(network_error, fn(error) {
    match error {
      TelemetryError::NetworkError(msg, count) => 
        TelemetryError::NetworkError("Mapped: " + msg, count + 1)
      _ => error
    }
  })
  
  match mapped_error.error {
    Some(TelemetryError::NetworkError(msg, count)) => {
      assert_eq(msg, "Mapped: Connection failed")
      assert_eq(count, 2)
    }
    _ => assert_true(false)
  }
  
  // Test error chaining
  let chained_error = chain_error(network_error, "telemetry_collector")
  match chained_error.error {
    Some(TelemetryError::NetworkError(msg, _)) => {
      assert_eq(msg, "telemetry_collector: Connection failed")
    }
    _ => assert_true(false)
  }
  
  // Test retry mechanism
  let mut attempt_count = 0
  let flaky_operation = fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      err(
        TelemetryError::NetworkError("Temporary failure", attempt_count),
        ["Attempt " + attempt_count.to_string()]
      )
    } else {
      ok("success_after_" + attempt_count.to_string() + "_attempts", ["Final attempt"])
    }
  }
  
  let retry_result = retry_with_backoff(flaky_operation, 5)
  assert_true(retry_result.success)
  assert_eq(retry_result.data, Some("success_after_3_attempts"))
}

// Test 4: Performance Optimization with Caching Strategies
test "performance optimization with caching strategies" {
  // LRU Cache implementation for telemetry data
  type LRUCache[K, V] = {
    capacity: Int,
    entries: Array[(K, V)],
    access_order: Array[K]
  }
  
  let create_lru_cache = fn(capacity: Int) {
    {
      capacity,
      entries: [],
      access_order: []
    }
  }
  
  let get = fn(cache: LRUCache[String, String], key: String) {
    let mut found_value = None
    let mut updated_entries = []
    let mut found = false
    
    // Find the value
    for (k, v) in cache.entries {
      if k == key {
        found_value = Some(v)
        found = true
      }
      updated_entries = updated_entries.push((k, v))
    }
    
    // Update access order
    let mut updated_order = []
    for k in cache.access_order {
      if k != key {
        updated_order = updated_order.push(k)
      }
    }
    
    if found {
      updated_order = updated_order.push(key)
    }
    
    {
      capacity: cache.capacity,
      entries: updated_entries,
      access_order: updated_order
    }, found_value
  }
  
  let put = fn(cache: LRUCache[String, String], key: String, value: String) {
    let mut updated_entries = []
    let mut key_exists = false
    
    // Update existing entry or add new one
    for (k, v) in cache.entries {
      if k == key {
        updated_entries = updated_entries.push((key, value))
        key_exists = true
      } else {
        updated_entries = updated_entries.push((k, v))
      }
    }
    
    if not(key_exists) {
      updated_entries = updated_entries.push((key, value))
    }
    
    // Remove least recently used if capacity exceeded
    if updated_entries.length() > cache.capacity {
      let mut new_entries = []
      let mut removed_key = ""
      
      for i in 1..updated_entries.length() {
        new_entries = new_entries.push(updated_entries[i])
      }
      
      if updated_entries.length() > 0 {
        removed_key = updated_entries[0].0
      }
      
      updated_entries = new_entries
      
      // Update access order
      let mut updated_order = []
      for k in cache.access_order {
        if k != removed_key {
          updated_order = updated_order.push(k)
        }
      }
      updated_order = updated_order.push(key)
      
      {
        capacity: cache.capacity,
        entries: updated_entries,
        access_order: updated_order
      }
    } else {
      // Update access order
      let mut updated_order = []
      for k in cache.access_order {
        if k != key {
          updated_order = updated_order.push(k)
        }
      }
      updated_order = updated_order.push(key)
      
      {
        capacity: cache.capacity,
        entries: updated_entries,
        access_order: updated_order
      }
    }
  }
  
  // Memoization with cache
  let memoize_with_lru = fn(computation: (String) -> String, cache_size: Int) {
    let cache = { mut value: create_lru_cache(cache_size) }
    
    fn(input: String) {
      let (updated_cache, cached_value) = get(cache.value, input)
      cache.value = updated_cache
      
      match cached_value {
        Some(value) => value
        None => {
          let computed = computation(input)
          cache.value = put(cache.value, input, computed)
          computed
        }
      }
    }
  }
  
  // Test LRU cache
  let cache = create_lru_cache(3)
  let cache1 = put(cache, "key1", "value1")
  let cache2 = put(cache1, "key2", "value2")
  let cache3 = put(cache2, "key3", "value3")
  
  // All keys should be present
  let (cache4, value1) = get(cache3, "key1")
  assert_eq(value1, Some("value1"))
  
  let (cache5, value2) = get(cache4, "key2")
  assert_eq(value2, Some("value2"))
  
  let (cache6, value3) = get(cache5, "key3")
  assert_eq(value3, Some("value3"))
  
  // Add fourth key, should evict least recently used
  let cache7 = put(cache6, "key4", "value4")
  let (cache8, evicted_value) = get(cache7, "key1")
  assert_eq(evicted_value, None)  // key1 should be evicted
  
  let (cache9, value4) = get(cache8, "key4")
  assert_eq(value4, Some("value4"))
  
  // Test memoization
  let expensive_operation = fn(input: String) {
    "processed_" + input + "_with_expensive_computation"
  }
  
  let memoized_operation = memoize_with_lru(expensive_operation, 5)
  
  let result1 = memoized_operation("telemetry_data")
  let result2 = memoized_operation("telemetry_data")
  let result3 = memoized_operation("different_data")
  
  assert_eq(result1, "processed_telemetry_data_with_expensive_computation")
  assert_eq(result2, result1)  // Should use cached value
  assert_eq(result3, "processed_different_data_with_expensive_computation")
}

// Test 5: Memory Management and Resource Cleanup
test "memory management and resource cleanup" {
  // Resource management with automatic cleanup
  type Resource[T] = {
    id: String,
    data: T,
    cleanup: () -> Unit,
    is_active: Bool
  }
  
  type ResourceManager = {
    resources: Array[Resource[String]],
    cleanup_log: Array[String]
  }
  
  let create_resource_manager = fn() {
    {
      resources: [],
      cleanup_log: []
    }
  }
  
  let create_resource = fn(manager: ResourceManager, id: String, data: String) {
    let resource = {
      id,
      data,
      cleanup: fn() {
        // Simulate cleanup operation
        // In real implementation, this would free resources
      },
      is_active: true
    }
    
    {
      resources: manager.resources.push(resource),
      cleanup_log: manager.cleanup_log
    }
  }
  
  let cleanup_resource = fn(manager: ResourceManager, id: String) {
    let mut updated_resources = []
    let mut cleaned_up = false
    let mut updated_log = manager.cleanup_log
    
    for resource in manager.resources {
      if resource.id == id and resource.is_active {
        resource.cleanup()
        cleaned_up = true
        updated_log = updated_log.push("Cleaned up resource: " + id)
        
        updated_resources = updated_resources.push({
          id: resource.id,
          data: resource.data,
          cleanup: resource.cleanup,
          is_active: false
        })
      } else {
        updated_resources = updated_resources.push(resource)
      }
    }
    
    {
      resources: updated_resources,
      cleanup_log: updated_log
    }
  }
  
  let cleanup_all_resources = fn(manager: ResourceManager) {
    let mut updated_resources = []
    let mut updated_log = manager.cleanup_log
    
    for resource in manager.resources {
      if resource.is_active {
        resource.cleanup()
        updated_log = updated_log.push("Cleaned up resource: " + resource.id)
        
        updated_resources = updated_resources.push({
          id: resource.id,
          data: resource.data,
          cleanup: resource.cleanup,
          is_active: false
        })
      } else {
        updated_resources = updated_resources.push(resource)
      }
    }
    
    {
      resources: updated_resources,
      cleanup_log: updated_log
    }
  }
  
  let get_active_resources_count = fn(manager: ResourceManager) {
    let mut count = 0
    for resource in manager.resources {
      if resource.is_active {
        count = count + 1
      }
    }
    count
  }
  
  // Memory pool for telemetry data
  type MemoryPool[T] = {
    pool: Array[T],
    allocated: Array[Bool],
    size: Int
  }
  
  let create_memory_pool = fn(size: Int, default_value: T) {
    let pool = []
    let allocated = []
    for i in 0..size {
      pool = pool.push(default_value)
      allocated = allocated.push(false)
    }
    
    {
      pool,
      allocated,
      size
    }
  }
  
  let allocate_from_pool = fn(pool: MemoryPool[String]) {
    let mut allocated_index = -1
    
    for i in 0..pool.size {
      if not(pool.allocated[i]) {
        allocated_index = i
        break
      }
    }
    
    if allocated_index >= 0 {
      let mut updated_allocated = []
      for i in 0..pool.size {
        if i == allocated_index {
          updated_allocated = updated_allocated.push(true)
        } else {
          updated_allocated = updated_allocated.push(pool.allocated[i])
        }
      }
      
      ({
        pool: pool.pool,
        allocated: updated_allocated,
        size: pool.size
      }, Some(allocated_index))
    } else {
      (pool, None)
    }
  }
  
  let deallocate_to_pool = fn(pool: MemoryPool[String], index: Int) {
    if index >= 0 and index < pool.size {
      let mut updated_allocated = []
      for i in 0..pool.size {
        if i == index {
          updated_allocated = updated_allocated.push(false)
        } else {
          updated_allocated = updated_allocated.push(pool.allocated[i])
        }
      }
      
      {
        pool: pool.pool,
        allocated: updated_allocated,
        size: pool.size
      }
    } else {
      pool
    }
  }
  
  // Test resource management
  let manager = create_resource_manager()
  let manager1 = create_resource(manager, "resource1", "telemetry_data_1")
  let manager2 = create_resource(manager1, "resource2", "telemetry_data_2")
  let manager3 = create_resource(manager2, "resource3", "telemetry_data_3")
  
  assert_eq(get_active_resources_count(manager3), 3)
  
  let manager4 = cleanup_resource(manager3, "resource2")
  assert_eq(get_active_resources_count(manager4), 2)
  assert_eq(manager4.cleanup_log.length(), 1)
  assert_eq(manager4.cleanup_log[0], "Cleaned up resource: resource2")
  
  let manager5 = cleanup_all_resources(manager4)
  assert_eq(get_active_resources_count(manager5), 0)
  assert_eq(manager5.cleanup_log.length(), 3)
  
  // Test memory pool
  let pool = create_memory_pool(5, "default")
  let (pool1, index1) = allocate_from_pool(pool)
  assert_eq(index1, Some(0))
  
  let (pool2, index2) = allocate_from_pool(pool1)
  assert_eq(index2, Some(1))
  
  let pool3 = deallocate_to_pool(pool2, 0)
  let (pool4, index3) = allocate_from_pool(pool3)
  assert_eq(index3, Some(0))  // Should reuse deallocated slot
}

// Test 6: Network Communication and Protocol Handling
test "network communication and protocol handling" {
  // Network packet structure for telemetry data
  type TelemetryPacket = {
    header: PacketHeader,
    payload: String,
    checksum: Int
  }
  
  type PacketHeader = {
    version: Int,
    packet_type: String,
    sequence_number: Int,
    timestamp: Int,
    source_id: String
  }
  
  // Protocol state machine
  enum ConnectionState {
    Disconnected
    Connecting
    Connected
    Authenticating
    Authenticated
    Error(String)
  }
  
  type ConnectionManager = {
    state: ConnectionState,
    packets_sent: Int,
    packets_received: Int,
    last_activity: Int
  }
  
  let create_connection_manager = fn() {
    {
      state: ConnectionState::Disconnected,
      packets_sent: 0,
      packets_received: 0,
      last_activity: 0
    }
  }
  
  let connect = fn(manager: ConnectionManager) {
    {
      state: ConnectionState::Connecting,
      packets_sent: manager.packets_sent + 1,
      packets_received: manager.packets_received,
      last_activity: 1000
    }
  }
  
  let handle_connect_response = fn(manager: ConnectionManager, success: Bool) {
    if success {
      {
        state: ConnectionState::Connected,
        packets_sent: manager.packets_sent,
        packets_received: manager.packets_received + 1,
        last_activity: 1005
      }
    } else {
      {
        state: ConnectionState::Error("Connection failed"),
        packets_sent: manager.packets_sent,
        packets_received: manager.packets_received + 1,
        last_activity: 1005
      }
    }
  }
  
  let authenticate = fn(manager: ConnectionManager, credentials: String) {
    if credentials.length() > 0 {
      {
        state: ConnectionState::Authenticating,
        packets_sent: manager.packets_sent + 1,
        packets_received: manager.packets_received,
        last_activity: 1010
      }
    } else {
      {
        state: ConnectionState::Error("Invalid credentials"),
        packets_sent: manager.packets_sent,
        packets_received: manager.packets_received,
        last_activity: 1010
      }
    }
  }
  
  let handle_auth_response = fn(manager: ConnectionManager, success: Bool) {
    if success {
      {
        state: ConnectionState::Authenticated,
        packets_sent: manager.packets_sent,
        packets_received: manager.packets_received + 1,
        last_activity: 1015
      }
    } else {
      {
        state: ConnectionState::Error("Authentication failed"),
        packets_sent: manager.packets_sent,
        packets_received: manager.packets_received + 1,
        last_activity: 1015
      }
    }
  }
  
  // Packet creation and validation
  let create_packet = fn(packet_type: String, payload: String, sequence: Int) {
    let header = {
      version: 1,
      packet_type,
      sequence_number: sequence,
      timestamp: 1000 + sequence * 10,
      source_id: "telemetry_client_001"
    }
    
    // Simple checksum calculation
    let checksum = payload.length() + sequence + header.timestamp
    
    {
      header,
      payload,
      checksum
    }
  }
  
  let validate_packet = fn(packet: TelemetryPacket) {
    let expected_checksum = packet.payload.length() + packet.header.sequence_number + packet.header.timestamp
    
    if packet.checksum == expected_checksum {
      true
    } else {
      false
    }
  }
  
  // Test connection management
  let manager = create_connection_manager()
  let manager1 = connect(manager)
  assert_eq(manager1.state, ConnectionState::Connecting)
  
  let manager2 = handle_connect_response(manager1, true)
  assert_eq(manager2.state, ConnectionState::Connected)
  
  let manager3 = authenticate(manager2, "valid_credentials")
  assert_eq(manager3.state, ConnectionState::Authenticating)
  
  let manager4 = handle_auth_response(manager3, true)
  assert_eq(manager4.state, ConnectionState::Authenticated)
  
  // Test packet creation and validation
  let packet1 = create_packet("telemetry_data", "cpu_usage:75.5,memory:60.2", 1)
  assert_true(validate_packet(packet1))
  
  let packet2 = create_packet("heartbeat", "ping", 2)
  assert_true(validate_packet(packet2))
  
  // Test invalid packet
  let invalid_packet = {
    header: packet1.header,
    payload: packet1.payload,
    checksum: 999  // Invalid checksum
  }
  assert_false(validate_packet(invalid_packet))
  
  // Test packet flow
  let send_packet = fn(manager: ConnectionManager, packet: TelemetryPacket) {
    if manager.state == ConnectionState::Authenticated and validate_packet(packet) {
      {
        state: manager.state,
        packets_sent: manager.packets_sent + 1,
        packets_received: manager.packets_received,
        last_activity: 1020
      }
    } else {
      manager
    }
  }
  
  let manager5 = send_packet(manager4, packet1)
  assert_eq(manager5.packets_sent, 4)  // connect + auth + packet1
}

// Test 7: Algorithm Implementation - Graph Traversal
test "algorithm implementation - graph traversal" {
  // Graph structure for telemetry dependency analysis
  type GraphNode = {
    id: String,
    value: String,
    dependencies: Array[String]
  }
  
  type Graph = {
    nodes: Array[GraphNode]
  }
  
  let create_node = fn(id: String, value: String, dependencies: Array[String]) {
    {
      id,
      value,
      dependencies
    }
  }
  
  let create_graph = fn(nodes: Array[GraphNode]) {
    {
      nodes
    }
  }
  
  // Depth-first search for dependency analysis
  let dfs = fn(graph: Graph, start_id: String, visited: Array[String]) {
    let mut result = []
    let mut new_visited = visited
    
    // Find the start node
    let mut start_node = None
    for node in graph.nodes {
      if node.id == start_id {
        start_node = Some(node)
      }
    }
    
    match start_node {
      Some(node) => {
        if not(new_visited.contains(start_id)) {
          new_visited = new_visited.push(start_id)
          result = result.push(node)
          
          // Visit all dependencies
          for dep_id in node.dependencies {
            let dep_results = dfs(graph, dep_id, new_visited)
            result = result + dep_results
            new_visited = new_visited + dep_results.map(fn(n) { n.id })
          }
        }
        
        result
      }
      None => []
    }
  }
  
  // Breadth-first search for shortest path analysis
  let bfs = fn(graph: Graph, start_id: String, target_id: String) {
    let queue = [(start_id, [])]  // (node_id, path)
    let mut visited = []
    
    let bfs_recursive = fn(current_queue: Array[(String, Array[String])]) {
      if current_queue.length() == 0 {
        []
      } else {
        let (current_id, path) = current_queue[0]
        let remaining_queue = current_queue.slice(1, current_queue.length())
        
        if current_id == target_id {
          path.push(current_id)
        } else if visited.contains(current_id) {
          bfs_recursive(remaining_queue)
        } else {
          visited = visited.push(current_id)
          
          // Find current node
          let mut current_node = None
          for node in graph.nodes {
            if node.id == current_id {
              current_node = Some(node)
            }
          }
          
          match current_node {
            Some(node) => {
              let mut new_queue = remaining_queue
              for dep_id in node.dependencies {
                new_queue = new_queue.push((dep_id, path.push(current_id)))
              }
              bfs_recursive(new_queue)
            }
            None => bfs_recursive(remaining_queue)
          }
        }
      }
    }
    
    bfs_recursive(queue)
  }
  
  // Topological sort for dependency ordering
  let topological_sort = fn(graph: Graph) {
    let mut in_degree = {}
    let mut sorted = []
    let mut queue = []
    
    // Calculate in-degree for each node
    for node in graph.nodes {
      in_degree = { in_degree | node.id: 0 }
    }
    
    for node in graph.nodes {
      for dep_id in node.dependencies {
        in_degree = { in_degree | dep_id: in_degree[dep_id] + 1 }
      }
    }
    
    // Find nodes with no dependencies
    for node in graph.nodes {
      if in_degree[node.id] == 0 {
        queue = queue.push(node.id)
      }
    }
    
    // Process queue
    while queue.length() > 0 {
      let current_id = queue[0]
      queue = queue.slice(1, queue.length())
      sorted = sorted.push(current_id)
      
      // Find current node and update in-degree of dependencies
      for node in graph.nodes {
        if node.id == current_id {
          for dep_id in node.dependencies {
            in_degree = { in_degree | dep_id: in_degree[dep_id] - 1 }
            if in_degree[dep_id] == 0 {
              queue = queue.push(dep_id)
            }
          }
        }
      }
    }
    
    sorted
  }
  
  // Create a telemetry dependency graph
  let nodes = [
    create_node("database", "MySQL Database", []),
    create_node("cache", "Redis Cache", ["database"]),
    create_node("api", "REST API", ["database", "cache"]),
    create_node("web", "Web Frontend", ["api"]),
    create_node("mobile", "Mobile App", ["api"]),
    create_node("analytics", "Analytics Service", ["database"])
  ]
  
  let graph = create_graph(nodes)
  
  // Test DFS
  let dfs_result = dfs(graph, "web", [])
  assert_eq(dfs_result.length(), 4)  // web -> api -> database, cache
  
  // Test BFS
  let bfs_result = bfs(graph, "web", "database")
  assert_eq(bfs_result.length(), 3)  // web -> api -> database
  
  // Test topological sort
  let topo_result = topological_sort(graph)
  assert_eq(topo_result.length(), 6)
  
  // Database should come first (no dependencies)
  assert_eq(topo_result[0], "database")
  
  // Web and mobile should come last (depend on others)
  assert_true(topo_result.contains("web"))
  assert_true(topo_result.contains("mobile"))
}

// Test 8: Data Validation and Schema Enforcement
test "data validation and schema enforcement" {
  // Schema definition for telemetry data
  enum SchemaType {
    StringType
    IntType
    FloatType
    BoolType
    ArrayType(SchemaType)
    ObjectType(Array[(String, SchemaType)])
  }
  
  type SchemaField = {
    name: String,
    schema_type: SchemaType,
    required: Bool,
    validator: Option[(String) -> Bool]
  }
  
  type Schema = {
    name: String,
    fields: Array[SchemaField]
  }
  
  type ValidationResult = {
    valid: Bool,
    errors: Array[String],
    warnings: Array[String]
  }
  
  // Schema validation
  let validate_string = fn(value: String, field: SchemaField) {
    if field.required and value.length() == 0 {
      {
        valid: false,
        errors: ["Field '" + field.name + "' is required but empty"],
        warnings: []
      }
    } else {
      match field.validator {
        Some(validator) => {
          if validator(value) {
            {
              valid: true,
              errors: [],
              warnings: []
            }
          } else {
            {
              valid: false,
              errors: ["Field '" + field.name + "' failed custom validation"],
              warnings: []
            }
          }
        }
        None => {
          {
            valid: true,
            errors: [],
            warnings: []
          }
        }
      }
    }
  }
  
  let validate_int = fn(value: Int, field: SchemaField) {
    if field.required and value == 0 {
      {
        valid: false,
        errors: ["Field '" + field.name + "' is required but zero"],
        warnings: []
      }
    } else {
      match field.validator {
        Some(validator) => {
          if validator(value.to_string()) {
            {
              valid: true,
              errors: [],
              warnings: []
            }
          } else {
            {
              valid: false,
              errors: ["Field '" + field.name + "' failed custom validation"],
              warnings: []
            }
          }
        }
        None => {
          {
            valid: true,
            errors: [],
            warnings: []
          }
        }
      }
    }
  }
  
  let validate_schema = fn(schema: Schema, data: Array[(String, String)]) {
    let mut all_valid = true
    let mut all_errors = []
    let mut all_warnings = []
    
    for field in schema.fields {
      let mut field_found = false
      let mut field_value = ""
      
      for (key, value) in data {
        if key == field.name {
          field_found = true
          field_value = value
        }
      }
      
      if not(field_found) and field.required {
        all_valid = false
        all_errors = all_errors.push("Required field '" + field.name + "' is missing")
      } else if field_found {
        let validation_result = validate_string(field_value, field)
        
        if not(validation_result.valid) {
          all_valid = false
          all_errors = all_errors + validation_result.errors
        }
        
        all_warnings = all_warnings + validation_result.warnings
      }
    }
    
    {
      valid: all_valid,
      errors: all_errors,
      warnings: all_warnings
    }
  }
  
  // Create telemetry schema
  let telemetry_schema = {
    name: "telemetry_event",
    fields: [
      {
        name: "trace_id",
        schema_type: SchemaType::StringType,
        required: true,
        validator: Some(fn(value) { value.length() == 16 })
      },
      {
        name: "span_name",
        schema_type: SchemaType::StringType,
        required: true,
        validator: Some(fn(value) { value.length() > 0 and value.length() <= 50 })
      },
      {
        name: "duration_ms",
        schema_type: SchemaType::IntType,
        required: true,
        validator: Some(fn(value) { 
          let int_val = value.to_int()
          int_val >= 0 and int_val <= 3600000
        })
      },
      {
        name: "status",
        schema_type: SchemaType::StringType,
        required: true,
        validator: Some(fn(value) { 
          value == "ok" or value == "error" or value == "timeout"
        })
      },
      {
        name: "tags",
        schema_type: SchemaType::StringType,
        required: false,
        validator: None
      }
    ]
  }
  
  // Test valid data
  let valid_data = [
    ("trace_id", "0af7651916cd43dd"),
    ("span_name", "database_query"),
    ("duration_ms", "150"),
    ("status", "ok"),
    ("tags", "env:production,service:api")
  ]
  
  let valid_result = validate_schema(telemetry_schema, valid_data)
  assert_true(valid_result.valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // Test invalid data
  let invalid_data = [
    ("trace_id", "invalid"),  // Wrong length
    ("span_name", ""),        // Empty
    ("duration_ms", "-5"),    // Negative
    ("status", "unknown")     // Invalid status
  ]
  
  let invalid_result = validate_schema(telemetry_schema, invalid_data)
  assert_false(invalid_result.valid)
  assert_eq(invalid_result.errors.length(), 4)
  
  // Test missing required field
  let missing_field_data = [
    ("trace_id", "0af7651916cd43dd"),
    ("span_name", "database_query"),
    // Missing duration_ms
    ("status", "ok")
  ]
  
  let missing_result = validate_schema(telemetry_schema, missing_field_data)
  assert_false(missing_result.valid)
  assert_eq(missing_result.errors.length(), 1)
  assert_eq(missing_result.errors[0], "Required field 'duration_ms' is missing")
}