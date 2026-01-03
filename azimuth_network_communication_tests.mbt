// Azimuth Network Communication and Telemetry Transmission Tests
// This file contains test cases for network communication and telemetry transmission

// Test 1: HTTP Client Request Handling
test "http client request handling" {
  // Define HTTP request structure
  let create_http_request = fn(method, url, headers, body) {
    { method = method, url = url, headers = headers, body = body }
  }
  
  // Define HTTP response structure
  let create_http_response = fn(status_code, headers, body) {
    { status_code = status_code, headers = headers, body = body }
  }
  
  // Test creating HTTP requests
  let get_request = create_http_request(
    "GET",
    "https://api.azimuth.com/v1/metrics",
    { "Content-Type": "application/json", "Authorization": "Bearer token123" },
    ""
  )
  
  assert_eq(get_request.method, "GET")
  assert_eq(get_request.url, "https://api.azimuth.com/v1/metrics")
  assert_eq(get_request.headers["Content-Type"], "application/json")
  assert_eq(get_request.headers["Authorization"], "Bearer token123")
  assert_eq(get_request.body, "")
  
  // Test POST request
  let post_request = create_http_request(
    "POST",
    "https://api.azimuth.com/v1/traces",
    { "Content-Type": "application/json" },
    "{\"trace_id\":\"trace123\",\"spans\":[]}"
  )
  
  assert_eq(post_request.method, "POST")
  assert_eq(post_request.url, "https://api.azimuth.com/v1/traces")
  assert_eq(post_request.headers["Content-Type"], "application/json")
  assert_eq(post_request.body, "{\"trace_id\":\"trace123\",\"spans\":[]}")
  
  // Test creating HTTP responses
  let success_response = create_http_response(
    200,
    { "Content-Type": "application/json", "Content-Length": "50" },
    "{\"status\":\"success\",\"message\":\"Data received\"}"
  )
  
  assert_eq(success_response.status_code, 200)
  assert_eq(success_response.headers["Content-Type"], "application/json")
  assert_eq(success_response.headers["Content-Length"], "50")
  assert_eq(success_response.body, "{\"status\":\"success\",\"message\":\"Data received\"}")
  
  // Test error response
  let error_response = create_http_response(
    404,
    { "Content-Type": "application/json" },
    "{\"error\":\"Not Found\",\"message\":\"Resource not found\"}"
  )
  
  assert_eq(error_response.status_code, 404)
  assert_eq(error_response.headers["Content-Type"], "application/json")
  assert_eq(error_response.body, "{\"error\":\"Not Found\",\"message\":\"Resource not found\"}")
}

// Test 2: Telemetry Data Serialization for Transmission
test "telemetry data serialization for transmission" {
  // Define telemetry data structure
  let create_telemetry_data = fn(metric_type, name, value, timestamp, tags) {
    { metric_type = metric_type, name = name, value = value, timestamp = timestamp, tags = tags }
  }
  
  // Serialize telemetry data to JSON-like string
  let serialize_telemetry = fn(data) {
    let tags_str = if data.tags.length() == 0 {
      "{}"
    } else {
      let mut tag_pairs = ""
      for (key, value) in data.tags {
        if tag_pairs.length() > 0 {
          tag_pairs = tag_pairs + ","
        }
        tag_pairs = tag_pairs + "\"" + key + "\":\"" + value + "\""
      }
      "{" + tag_pairs + "}"
    }
    
    "{\"metric_type\":\"" + data.metric_type + "\",\"name\":\"" + data.name + 
    "\",\"value\":" + data.value.to_string() + ",\"timestamp\":" + data.timestamp.to_string() + 
    ",\"tags\":" + tags_str + "}"
  }
  
  // Test serialization of counter metric
  let counter_data = create_telemetry_data(
    "counter",
    "http.requests.total",
    42,
    1640995200000,
    { "service": "api", "method": "GET", "status": "200" }
  )
  
  let counter_json = serialize_telemetry(counter_data)
  assert_eq(counter_json, "{\"metric_type\":\"counter\",\"name\":\"http.requests.total\",\"value\":42,\"timestamp\":1640995200000,\"tags\":{\"service\":\"api\",\"method\":\"GET\",\"status\":\"200\"}}")
  
  // Test serialization of gauge metric
  let gauge_data = create_telemetry_data(
    "gauge",
    "system.memory.usage",
    1024.5,
    1640995260000,
    { "service": "api", "host": "server1" }
  )
  
  let gauge_json = serialize_telemetry(gauge_data)
  assert_eq(gauge_json, "{\"metric_type\":\"gauge\",\"name\":\"system.memory.usage\",\"value\":1024.5,\"timestamp\":1640995260000,\"tags\":{\"service\":\"api\",\"host\":\"server1\"}}")
  
  // Test serialization of histogram metric
  let histogram_data = create_telemetry_data(
    "histogram",
    "http.request.duration",
    123.45,
    1640995320000,
    { "service": "api", "endpoint": "/users", "method": "POST" }
  )
  
  let histogram_json = serialize_telemetry(histogram_data)
  assert_eq(histogram_json, "{\"metric_type\":\"histogram\",\"name\":\"http.request.duration\",\"value\":123.45,\"timestamp\":1640995320000,\"tags\":{\"service\":\"api\",\"endpoint\":\"/users\",\"method\":\"POST\"}}")
  
  // Test serialization with empty tags
  let no_tags_data = create_telemetry_data(
    "counter",
    "simple.metric",
    10,
    1640995380000,
    {}
  )
  
  let no_tags_json = serialize_telemetry(no_tags_data)
  assert_eq(no_tags_json, "{\"metric_type\":\"counter\",\"name\":\"simple.metric\",\"value\":10,\"timestamp\":1640995380000,\"tags\":{}}")
}

// Test 3: Batch Telemetry Transmission
test "batch telemetry transmission" {
  // Define batch structure
  let create_telemetry_batch = fn(batch_id, timestamp, items) {
    { batch_id = batch_id, timestamp = timestamp, items = items }
  }
  
  // Serialize batch to JSON-like string
  let serialize_batch = fn(batch) {
    let items_str = if batch.items.length() == 0 {
      "[]"
    } else {
      let mut items_array = ""
      for item in batch.items {
        if items_array.length() > 0 {
          items_array = items_array + ","
        }
        items_array = items_array + item
      }
      "[" + items_array + "]"
    }
    
    "{\"batch_id\":\"" + batch.batch_id + "\",\"timestamp\":" + batch.timestamp.to_string() + 
    ",\"items\":" + items_str + "}"
  }
  
  // Create individual telemetry items
  let item1 = "{\"metric_type\":\"counter\",\"name\":\"http.requests.total\",\"value\":10,\"timestamp\":1640995200000,\"tags\":{}}"
  let item2 = "{\"metric_type\":\"gauge\",\"name\":\"system.memory.usage\",\"value\":1024.5,\"timestamp\":1640995260000,\"tags\":{}}"
  let item3 = "{\"metric_type\":\"histogram\",\"name\":\"http.request.duration\",\"value\":123.45,\"timestamp\":1640995320000,\"tags\":{}}"
  
  // Test batch creation
  let batch = create_telemetry_batch(
    "batch-123",
    1640995380000,
    [item1, item2, item3]
  )
  
  assert_eq(batch.batch_id, "batch-123")
  assert_eq(batch.timestamp, 1640995380000)
  assert_eq(batch.items.length(), 3)
  assert_eq(batch.items[0], item1)
  assert_eq(batch.items[1], item2)
  assert_eq(batch.items[2], item3)
  
  // Test batch serialization
  let batch_json = serialize_batch(batch)
  assert_eq(batch_json, "{\"batch_id\":\"batch-123\",\"timestamp\":1640995380000,\"items\":[{\"metric_type\":\"counter\",\"name\":\"http.requests.total\",\"value\":10,\"timestamp\":1640995200000,\"tags\":{}},{\"metric_type\":\"gauge\",\"name\":\"system.memory.usage\",\"value\":1024.5,\"timestamp\":1640995260000,\"tags\":{}},{\"metric_type\":\"histogram\",\"name\":\"http.request.duration\",\"value\":123.45,\"timestamp\":1640995320000,\"tags\":{}}]}")
  
  // Test empty batch
  let empty_batch = create_telemetry_batch("empty-123", 1640995380000, [])
  assert_eq(empty_batch.items.length(), 0)
  
  let empty_batch_json = serialize_batch(empty_batch)
  assert_eq(empty_batch_json, "{\"batch_id\":\"empty-123\",\"timestamp\":1640995380000,\"items\":[]}")
  
  // Test batch size limits
  let create_large_batch = fn(size) {
    let mut items = []
    for i in 0..<size {
      let item = "{\"metric_type\":\"counter\",\"name\":\"metric." + i.to_string() + "\",\"value\":" + i.to_string() + ",\"timestamp\":1640995200000,\"tags\":{}}"
      items = items.push(item)
    }
    create_telemetry_batch("large-batch", 1640995380000, items)
  }
  
  let small_batch = create_large_batch(10)
  assert_eq(small_batch.items.length(), 10)
  
  let medium_batch = create_large_batch(100)
  assert_eq(medium_batch.items.length(), 100)
  
  let large_batch = create_large_batch(1000)
  assert_eq(large_batch.items.length(), 1000)
}

// Test 4: Retry and Error Handling
test "retry and error handling" {
  // Define retry configuration
  let create_retry_config = fn(max_attempts, backoff_ms, max_backoff_ms) {
    { max_attempts = max_attempts, backoff_ms = backoff_ms, max_backoff_ms = max_backoff_ms }
  }
  
  // Define transmission result
  let create_transmission_result = fn(success, attempts, error_message) {
    { success = success, attempts = attempts, error_message = error_message }
  }
  
  // Simulate transmission with retry logic
  let transmit_with_retry = fn(data, retry_config) {
    let mut attempts = 0
    let mut success = false
    let mut error_message = ""
    
    while attempts < retry_config.max_attempts && !success {
      attempts = attempts + 1
      
      // Simulate transmission (fail first few attempts, succeed on last)
      if attempts < retry_config.max_attempts {
        error_message = "Network error: Connection timeout"
        success = false
      } else {
        success = true
        error_message = ""
      }
    }
    
    create_transmission_result(success, attempts, error_message)
  }
  
  // Test retry configuration
  let retry_config = create_retry_config(3, 1000, 10000)
  assert_eq(retry_config.max_attempts, 3)
  assert_eq(retry_config.backoff_ms, 1000)
  assert_eq(retry_config.max_backoff_ms, 10000)
  
  // Test successful transmission after retries
  let test_data = "test telemetry data"
  let result = transmit_with_retry(test_data, retry_config)
  
  assert_true(result.success)
  assert_eq(result.attempts, 3)
  assert_eq(result.error_message, "")
  
  // Test failed transmission after max retries
  let transmit_with_max_failures = fn(data, retry_config) {
    let mut attempts = 0
    let mut success = false
    let mut error_message = ""
    
    while attempts < retry_config.max_attempts && !success {
      attempts = attempts + 1
      
      // Always fail
      error_message = "Network error: Connection refused"
      success = false
    }
    
    create_transmission_result(success, attempts, error_message)
  }
  
  let failed_result = transmit_with_max_failures(test_data, retry_config)
  
  assert_false(failed_result.success)
  assert_eq(failed_result.attempts, 3)
  assert_eq(failed_result.error_message, "Network error: Connection refused")
  
  // Test exponential backoff calculation
  let calculate_backoff = fn(attempt, base_backoff_ms, max_backoff_ms) {
    let exponential_backoff = base_backoff_ms * (2 ^ (attempt - 1))
    if exponential_backoff > max_backoff_ms {
      max_backoff_ms
    } else {
      exponential_backoff
    }
  }
  
  assert_eq(calculate_backoff(1, 1000, 10000), 1000)   // 1000 * 2^0 = 1000
  assert_eq(calculate_backoff(2, 1000, 10000), 2000)   // 1000 * 2^1 = 2000
  assert_eq(calculate_backoff(3, 1000, 10000), 4000)   // 1000 * 2^2 = 4000
  assert_eq(calculate_backoff(4, 1000, 10000), 8000)   // 1000 * 2^3 = 8000
  assert_eq(calculate_backoff(5, 1000, 10000), 10000)  // 1000 * 2^4 = 16000, capped at 10000
}

// Test 5: Connection Pool Management
test "connection pool management" {
  // Define connection structure
  let create_connection = fn(id, host, port, in_use) {
    { id = id, host = host, port = port, in_use = in_use, last_used = 1640995200000 }
  }
  
  // Define connection pool
  let create_connection_pool = fn(max_connections) {
    let mut connections = []
    for i in 0..<max_connections {
      let connection = create_connection(
        "conn-" + i.to_string(),
        "api.azimuth.com",
        443,
        false
      )
      connections = connections.push(connection)
    }
    { connections = connections, max_connections = max_connections }
  }
  
  // Acquire connection from pool
  let acquire_connection = fn(pool) {
    let mut available_connection = None
    let mut updated_connections = []
    
    for connection in pool.connections {
      if !connection.in_use && available_connection == None {
        available_connection = Some({ 
          id = connection.id, 
          host = connection.host, 
          port = connection.port, 
          in_use = true, 
          last_used = 1640995300000  // Current time
        })
      } else {
        updated_connections = updated_connections.push(connection)
      }
    }
    
    match available_connection {
      None => { pool = pool, connection = None }
      Some(conn) => { 
        pool = { connections = updated_connections.push(conn), max_connections = pool.max_connections }, 
        connection = Some(conn) 
      }
    }
  }
  
  // Release connection back to pool
  let release_connection = fn(pool, connection_id) {
    let mut updated_connections = []
    
    for connection in pool.connections {
      if connection.id == connection_id {
        updated_connections = updated_connections.push({ 
          id = connection.id, 
          host = connection.host, 
          port = connection.port, 
          in_use = false, 
          last_used = 1640995300000  // Current time
        })
      } else {
        updated_connections = updated_connections.push(connection)
      }
    }
    
    { connections = updated_connections, max_connections = pool.max_connections }
  }
  
  // Get pool statistics
  let get_pool_stats = fn(pool) {
    let mut total = 0
    let mut in_use = 0
    let mut available = 0
    
    for connection in pool.connections {
      total = total + 1
      if connection.in_use {
        in_use = in_use + 1
      } else {
        available = available + 1
      }
    }
    
    { total = total, in_use = in_use, available = available }
  }
  
  // Test connection pool creation
  let pool = create_connection_pool(5)
  assert_eq(pool.connections.length(), 5)
  assert_eq(pool.max_connections, 5)
  
  let stats = get_pool_stats(pool)
  assert_eq(stats.total, 5)
  assert_eq(stats.in_use, 0)
  assert_eq(stats.available, 5)
  
  // Test connection acquisition
  let acquire_result1 = acquire_connection(pool)
  match acquire_result1.connection {
    None => assert_true(false)
    Some(conn) => {
      assert_eq(conn.host, "api.azimuth.com")
      assert_eq(conn.port, 443)
      assert_true(conn.in_use)
    }
  }
  
  let stats1 = get_pool_stats(acquire_result1.pool)
  assert_eq(stats1.total, 5)
  assert_eq(stats1.in_use, 1)
  assert_eq(stats1.available, 4)
  
  // Test multiple connection acquisitions
  let acquire_result2 = acquire_connection(acquire_result1.pool)
  let acquire_result3 = acquire_connection(acquire_result2.pool)
  let acquire_result4 = acquire_connection(acquire_result3.pool)
  let acquire_result5 = acquire_connection(acquire_result4.pool)
  
  let stats5 = get_pool_stats(acquire_result5.pool)
  assert_eq(stats5.total, 5)
  assert_eq(stats5.in_use, 5)
  assert_eq(stats5.available, 0)
  
  // Test pool exhaustion
  let acquire_result6 = acquire_connection(acquire_result5.pool)
  match acquire_result6.connection {
    None => assert_true(true)  // Should be None when pool is exhausted
    Some(_) => assert_true(false)
  }
  
  // Test connection release
  let release_result1 = release_connection(acquire_result6.pool, "conn-0")
  let stats_released = get_pool_stats(release_result1)
  assert_eq(stats_released.total, 5)
  assert_eq(stats_released.in_use, 4)
  assert_eq(stats_released.available, 1)
  
  // Test acquiring after release
  let acquire_after_release = acquire_connection(release_result1)
  match acquire_after_release.connection {
    None => assert_true(false)
    Some(conn) => assert_true(conn.in_use)
  }
  
  let final_stats = get_pool_stats(acquire_after_release.pool)
  assert_eq(final_stats.total, 5)
  assert_eq(final_stats.in_use, 5)
  assert_eq(final_stats.available, 0)
}

// Test 6: Protocol Buffer Serialization
test "protocol buffer serialization" {
  // Define simplified protocol buffer message structure
  let create_protobuf_metric = fn(name, value, timestamp, tags) {
    { 
      name = name, 
      value = value, 
      timestamp = timestamp, 
      tags = tags,
      field_numbers = {
        "name": 1,
        "value": 2,
        "timestamp": 3,
        "tags": 4
      }
    }
  }
  
  // Serialize to simplified protobuf-like binary format
  let serialize_protobuf = fn(metric) {
    let mut result = []
    
    // Field 1: name (string)
    result = result.push(0x0A)  // Field number 1, wire type 2 (length-delimited)
    result = result.push(metric.name.length())
    for i in 0 ..< metric.name.length() {
      result = result.push(metric.name.charCodeAt(i))
    }
    
    // Field 2: value (double)
    result = result.push(0x11)  // Field number 2, wire type 1 (64-bit)
    // Simplified double encoding - in real implementation would be proper binary
    result = result.push(metric.value.to_int())
    
    // Field 3: timestamp (int64)
    result = result.push(0x18)  // Field number 3, wire type 0 (varint)
    result = result.push(metric.timestamp)
    
    // Field 4: tags (repeated string)
    for (key, value) in metric.tags {
      result = result.push(0x22)  // Field number 4, wire type 2 (length-delimited)
      let tag_string = key + "=" + value
      result = result.push(tag_string.length())
      for i in 0 ..< tag_string.length() {
        result = result.push(tag_string.charCodeAt(i))
      }
    }
    
    result
  }
  
  // Deserialize from simplified protobuf-like binary format
  let deserialize_protobuf = fn(data) {
    // Simplified deserialization - in real implementation would be proper binary parsing
    let mut index = 0
    let mut name = ""
    let mut value = 0.0
    let mut timestamp = 0
    let mut tags = {}
    
    while index < data.length() {
      let field_number = data[index] >> 3
      index = index + 1
      
      match field_number {
        1 => {  // name
          let length = data[index]
          index = index + 1
          let mut name_chars = []
          for i in 0 ..< length {
            name_chars = name_chars.push(data[index + i])
          }
          index = index + length
          name = name_chars.map(fn(c) { String.fromCharCode(c) }).join("")
        }
        2 => {  // value
          value = data[index].to_float()
          index = index + 1
        }
        3 => {  // timestamp
          timestamp = data[index]
          index = index + 1
        }
        4 => {  // tags
          let length = data[index]
          index = index + 1
          let mut tag_chars = []
          for i in 0 ..< length {
            tag_chars = tag_chars.push(data[index + i])
          }
          index = index + length
          let tag_string = tag_chars.map(fn(c) { String.fromCharCode(c) }).join("")
          let parts = tag_string.split("=")
          if parts.length() == 2 {
            tags = tags.with(parts[0], parts[1])
          }
        }
        _ => {
          // Skip unknown field
          index = index + 1
        }
      }
    }
    
    { name = name, value = value, timestamp = timestamp, tags = tags }
  }
  
  // Test protobuf serialization
  let metric = create_protobuf_metric(
    "http.request.duration",
    123.45,
    1640995200000,
    { "service": "api", "method": "GET", "status": "200" }
  )
  
  let protobuf_data = serialize_protobuf(metric)
  assert_true(protobuf_data.length() > 0)
  
  // Test protobuf deserialization
  let deserialized_metric = deserialize_protobuf(protobuf_data)
  assert_eq(deserialized_metric.name, "http.request.duration")
  assert_eq(deserialized_metric.value, 123.0)  // Simplified conversion
  assert_eq(deserialized_metric.timestamp, 1640995200000)
  assert_eq(deserialized_metric.tags["service"], "api")
  assert_eq(deserialized_metric.tags["method"], "GET")
  assert_eq(deserialized_metric.tags["status"], "200")
  
  // Test with empty tags
  let metric_no_tags = create_protobuf_metric(
    "simple.metric",
    42.0,
    1640995260000,
    {}
  )
  
  let protobuf_data_no_tags = serialize_protobuf(metric_no_tags)
  let deserialized_no_tags = deserialize_protobuf(protobuf_data_no_tags)
  assert_eq(deserialized_no_tags.name, "simple.metric")
  assert_eq(deserialized_no_tags.value, 42.0)
  assert_eq(deserialized_no_tags.timestamp, 1640995260000)
  assert_eq(deserialized_no_tags.tags.length(), 0)
}

// Test 7: Compression for Network Transmission
test "compression for network transmission" {
  // Define compression algorithms
  let compress_gzip = fn(data) {
    // Simplified gzip compression simulation
    if data.length() > 10 {
      "GZIP:" + data.length().to_string() + ":" + data.substring(0, 5) + "..." + data.substring(data.length() - 5)
    } else {
      "GZIP:" + data
    }
  }
  
  let compress_deflate = fn(data) {
    // Simplified deflate compression simulation
    if data.length() > 10 {
      "DEFLATE:" + data.length().to_string() + ":" + data.substring(0, 3) + "..." + data.substring(data.length() - 3)
    } else {
      "DEFLATE:" + data
    }
  }
  
  let compress_brotli = fn(data) {
    // Simplified brotli compression simulation
    if data.length() > 10 {
      "BROTLI:" + data.length().to_string() + ":" + data.substring(0, 2) + "..." + data.substring(data.length() - 2)
    } else {
      "BROTLI:" + data
    }
  }
  
  // Define decompression functions
  let decompress_gzip = fn(compressed_data) {
    if compressed_data.starts_with("GZIP:") {
      let parts = compressed_data.split(":")
      if parts.length() == 3 {
        let original_length = parts[1].to_int()
        let preview = parts[2]
        if preview.contains("...") {
          // Reconstruct simplified data
          let preview_parts = preview.split("...")
          "reconstructed_data_of_length_" + original_length.to_string()
        } else {
          preview
        }
      } else {
        compressed_data
      }
    } else {
      compressed_data
    }
  }
  
  // Test compression with small data
  let small_data = "small"
  let compressed_small_gzip = compress_gzip(small_data)
  assert_eq(compressed_small_gzip, "GZIP:small")
  
  let compressed_small_deflate = compress_deflate(small_data)
  assert_eq(compressed_small_deflate, "DEFLATE:small")
  
  let compressed_small_brotli = compress_brotli(small_data)
  assert_eq(compressed_small_brotli, "BROTLI:small")
  
  // Test compression with large data
  let large_data = "this is a large piece of telemetry data that should be compressed for network transmission"
  let compressed_large_gzip = compress_gzip(large_data)
  assert_eq(compressed_large_gzip, "GZIP:81:this i...ission")
  
  let compressed_large_deflate = compress_deflate(large_data)
  assert_eq(compressed_large_deflate, "DEFLATE:81:thi...ion")
  
  let compressed_large_brotli = compress_brotli(large_data)
  assert_eq(compressed_large_brotli, "BROTLI:81:th...on")
  
  // Test decompression
  let decompressed_small = decompress_gzip(compressed_small_gzip)
  assert_eq(decompressed_small, "small")
  
  let decompressed_large = decompress_gzip(compressed_large_gzip)
  assert_eq(decompressed_large, "reconstructed_data_of_length_81")
  
  // Test compression ratio calculation
  let calculate_compression_ratio = fn(original, compressed) {
    (compressed.length().to_float() / original.length().to_float()) * 100.0
  }
  
  let gzip_ratio = calculate_compression_ratio(large_data, compressed_large_gzip)
  assert_true(gzip_ratio < 100.0)  // Should be smaller than original
  
  let deflate_ratio = calculate_compression_ratio(large_data, compressed_large_deflate)
  assert_true(deflate_ratio < 100.0)  // Should be smaller than original
  
  let brotli_ratio = calculate_compression_ratio(large_data, compressed_large_brotli)
  assert_true(brotli_ratio < 100.0)  // Should be smaller than original
  
  // Test compression selection based on data size
  let select_compression_method = fn(data_size) {
    if data_size < 20 {
      "none"
    } else if data_size < 100 {
      "gzip"
    } else if data_size < 1000 {
      "deflate"
    } else {
      "brotli"
    }
  }
  
  assert_eq(select_compression_method(10), "none")
  assert_eq(select_compression_method(50), "gzip")
  assert_eq(select_compression_method(500), "deflate")
  assert_eq(select_compression_method(2000), "brotli")
}

// Test 8: Telemetry Protocol Negotiation
test "telemetry protocol negotiation" {
  // Define supported protocols
  let create_protocol_support = fn() {
    {
      protocols = [
        { name = "http/json", version = "1.0", priority = 1 },
        { name = "grpc/protobuf", version = "1.0", priority = 3 },
        { name = "http/protobuf", version = "1.0", priority = 2 },
        { name = "websocket/json", version = "1.0", priority = 4 }
      ],
      default_protocol = "http/json"
    }
  }
  
  // Define client capabilities
  let create_client_capabilities = fn(protocols) {
    { supported_protocols = protocols, preferred_protocol = protocols[0] }
  }
  
  // Define server capabilities
  let create_server_capabilities = fn(protocols) {
    { supported_protocols = protocols, preferred_protocol = protocols[0] }
  }
  
  // Negotiate protocol between client and server
  let negotiate_protocol = fn(client_caps, server_caps) {
    let mut common_protocols = []
    
    for client_proto in client_caps.supported_protocols {
      for server_proto in server_caps.supported_protocols {
        if client_proto.name == server_proto.name && client_proto.version == server_proto.version {
          common_protocols = common_protocols.push({
            name = client_proto.name,
            version = client_proto.version,
            priority = client_proto.priority + server_proto.priority  // Combined priority
          })
        }
      }
    }
    
    if common_protocols.length() == 0 {
      None
    } else {
      // Sort by priority (higher is better)
      let mut best_protocol = common_protocols[0]
      for proto in common_protocols {
        if proto.priority > best_protocol.priority {
          best_protocol = proto
        }
      }
      Some(best_protocol)
    }
  }
  
  // Test protocol support
  let protocol_support = create_protocol_support()
  assert_eq(protocol_support.protocols.length(), 4)
  assert_eq(protocol_support.default_protocol, "http/json")
  
  // Test client capabilities
  let client_caps = create_client_capabilities([
    { name = "http/json", version = "1.0", priority = 1 },
    { name = "grpc/protobuf", version = "1.0", priority = 3 },
    { name = "http/protobuf", version = "1.0", priority = 2 }
  ])
  
  assert_eq(client_caps.supported_protocols.length(), 3)
  assert_eq(client_caps.preferred_protocol.name, "http/json")
  
  // Test server capabilities
  let server_caps = create_server_capabilities([
    { name = "http/json", version = "1.0", priority = 1 },
    { name = "grpc/protobuf", version = "1.0", priority = 3 },
    { name = "websocket/json", version = "1.0", priority = 4 }
  ])
  
  assert_eq(server_caps.supported_protocols.length(), 3)
  assert_eq(server_caps.preferred_protocol.name, "http/json")
  
  // Test protocol negotiation
  let negotiated_protocol = negotiate_protocol(client_caps, server_caps)
  match negotiated_protocol {
    None => assert_true(false)
    Some(proto) => {
      assert_eq(proto.name, "grpc/protobuf")
      assert_eq(proto.version, "1.0")
      assert_eq(proto.priority, 6)  // 3 (client) + 3 (server)
    }
  }
  
  // Test no common protocol
  let client_caps_no_match = create_client_capabilities([
    { name = "custom/proto", version = "1.0", priority = 1 },
    { name = "another/proto", version = "1.0", priority = 2 }
  ])
  
  let no_match_protocol = negotiate_protocol(client_caps_no_match, server_caps)
  match no_match_protocol {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // Test fallback to default protocol
  let fallback_to_default = fn(negotiated_protocol, default_protocol) {
    match negotiated_protocol {
      None => default_protocol
      Some(proto) => proto.name
    }
  }
  
  let selected_protocol1 = fallback_to_default(negotiated_protocol, protocol_support.default_protocol)
  assert_eq(selected_protocol1, "grpc/protobuf")
  
  let selected_protocol2 = fallback_to_default(no_match_protocol, protocol_support.default_protocol)
  assert_eq(selected_protocol2, "http/json")
}

// Test 9: Telemetry Endpoint Discovery
test "telemetry endpoint discovery" {
  // Define endpoint structure
  let create_endpoint = fn(url, protocol, health_check_url, priority) {
    { 
      url = url, 
      protocol = protocol, 
      health_check_url = health_check_url, 
      priority = priority,
      healthy = true,
      last_check = 1640995200000
    }
  }
  
  // Define endpoint registry
  let create_endpoint_registry = fn(endpoints) {
    { endpoints = endpoints, default_endpoint = endpoints[0] }
  }
  
  // Check endpoint health
  let check_endpoint_health = fn(endpoint) {
    // Simulate health check
    let healthy = if endpoint.url.contains("healthy") {
      true
    } else if endpoint.url.contains("unhealthy") {
      false
    } else {
      true  // Default to healthy
    }
    
    { 
      url = endpoint.url, 
      protocol = endpoint.protocol, 
      health_check_url = endpoint.health_check_url, 
      priority = endpoint.priority,
      healthy = healthy,
      last_check = 1640995300000  // Current time
    }
  }
  
  // Select best endpoint
  let select_best_endpoint = fn(registry) {
    let mut healthy_endpoints = []
    
    for endpoint in registry.endpoints {
      if endpoint.healthy {
        healthy_endpoints = healthy_endpoints.push(endpoint)
      }
    }
    
    if healthy_endpoints.length() == 0 {
      None
    } else {
      // Sort by priority (higher is better)
      let mut best_endpoint = healthy_endpoints[0]
      for endpoint in healthy_endpoints {
        if endpoint.priority > best_endpoint.priority {
          best_endpoint = endpoint
        }
      }
      Some(best_endpoint)
    }
  }
  
  // Test endpoint creation
  let endpoint1 = create_endpoint(
    "https://api-healthy.azimuth.com/telemetry",
    "grpc/protobuf",
    "https://api-healthy.azimuth.com/health",
    3
  )
  
  let endpoint2 = create_endpoint(
    "https://api-unhealthy.azimuth.com/telemetry",
    "http/json",
    "https://api-unhealthy.azimuth.com/health",
    2
  )
  
  let endpoint3 = create_endpoint(
    "https://backup.azimuth.com/telemetry",
    "http/protobuf",
    "https://backup.azimuth.com/health",
    1
  )
  
  assert_eq(endpoint1.url, "https://api-healthy.azimuth.com/telemetry")
  assert_eq(endpoint1.protocol, "grpc/protobuf")
  assert_eq(endpoint1.priority, 3)
  assert_true(endpoint1.healthy)
  
  // Test endpoint registry
  let registry = create_endpoint_registry([endpoint1, endpoint2, endpoint3])
  assert_eq(registry.endpoints.length(), 3)
  assert_eq(registry.default_endpoint.url, "https://api-healthy.azimuth.com/telemetry")
  
  // Test health check
  let health_checked_registry = {
    endpoints = [
      check_endpoint_health(registry.endpoints[0]),
      check_endpoint_health(registry.endpoints[1]),
      check_endpoint_health(registry.endpoints[2])
    ],
    default_endpoint = registry.default_endpoint
  }
  
  assert_true(health_checked_registry.endpoints[0].healthy)
  assert_false(health_checked_registry.endpoints[1].healthy)  // Contains "unhealthy"
  assert_true(health_checked_registry.endpoints[2].healthy)
  
  // Test endpoint selection
  let best_endpoint = select_best_endpoint(health_checked_registry)
  match best_endpoint {
    None => assert_true(false)
    Some(endpoint) => {
      assert_eq(endpoint.url, "https://api-healthy.azimuth.com/telemetry")
      assert_eq(endpoint.protocol, "grpc/protobuf")
      assert_eq(endpoint.priority, 3)
      assert_true(endpoint.healthy)
    }
  }
  
  // Test with all unhealthy endpoints
  let all_unhealthy_registry = {
    endpoints = [
      check_endpoint_health(create_endpoint("https://unhealthy1.azimuth.com", "http/json", "https://unhealthy1.azimuth.com/health", 3)),
      check_endpoint_health(create_endpoint("https://unhealthy2.azimuth.com", "http/json", "https://unhealthy2.azimuth.com/health", 2)),
      check_endpoint_health(create_endpoint("https://unhealthy3.azimuth.com", "http/json", "https://unhealthy3.azimuth.com/health", 1))
    ],
    default_endpoint = create_endpoint("https://unhealthy1.azimuth.com", "http/json", "https://unhealthy1.azimuth.com/health", 3)
  }
  
  let no_healthy_endpoint = select_best_endpoint(all_unhealthy_registry)
  match no_healthy_endpoint {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // Test endpoint failover
  let failover_to_next_best = fn(registry, failed_endpoint_url) {
    let mut available_endpoints = []
    
    for endpoint in registry.endpoints {
      if endpoint.url != failed_endpoint_url && endpoint.healthy {
        available_endpoints = available_endpoints.push(endpoint)
      }
    }
    
    if available_endpoints.length() == 0 {
      None
    } else {
      // Sort by priority (higher is better)
      let mut best_endpoint = available_endpoints[0]
      for endpoint in available_endpoints {
        if endpoint.priority > best_endpoint.priority {
          best_endpoint = endpoint
        }
      }
      Some(best_endpoint)
    }
  }
  
  let failover_endpoint = failover_to_next_best(health_checked_registry, "https://api-healthy.azimuth.com/telemetry")
  match failover_endpoint {
    None => assert_true(false)
    Some(endpoint) => {
      assert_eq(endpoint.url, "https://backup.azimuth.com/telemetry")
      assert_eq(endpoint.protocol, "http/protobuf")
      assert_eq(endpoint.priority, 1)
      assert_true(endpoint.healthy)
    }
  }
}

// Test 10: Telemetry Data Buffering and Flushing
test "telemetry data buffering and flushing" {
  // Define buffer configuration
  let create_buffer_config = fn(max_size, max_time_ms) {
    { max_size = max_size, max_time_ms = max_time_ms }
  }
  
  // Define buffer
  let create_buffer = fn(config) {
    { 
      items = [], 
      config = config, 
      created_at = 1640995200000,
      last_flush = 1640995200000
    }
  }
  
  // Add item to buffer
  let add_to_buffer = fn(buffer, item) {
    { items = buffer.items.push(item), config = buffer.config, created_at = buffer.created_at, last_flush = buffer.last_flush }
  }
  
  // Check if buffer should flush based on size
  let should_flush_by_size = fn(buffer) {
    buffer.items.length() >= buffer.config.max_size
  }
  
  // Check if buffer should flush based on time
  let should_flush_by_time = fn(buffer, current_time) {
    (current_time - buffer.last_flush) >= buffer.config.max_time_ms
  }
  
  // Check if buffer should flush
  let should_flush = fn(buffer, current_time) {
    should_flush_by_size(buffer) || should_flush_by_time(buffer, current_time)
  }
  
  // Flush buffer
  let flush_buffer = fn(buffer, current_time) {
    let items = buffer.items
    { 
      items = [], 
      config = buffer.config, 
      created_at = buffer.created_at,
      last_flush = current_time
    }
  }
  
  // Test buffer creation
  let buffer_config = create_buffer_config(10, 60000)  // Max 10 items or 60 seconds
  let buffer = create_buffer(buffer_config)
  
  assert_eq(buffer.config.max_size, 10)
  assert_eq(buffer.config.max_time_ms, 60000)
  assert_eq(buffer.items.length(), 0)
  
  // Test adding items to buffer
  let buffer1 = add_to_buffer(buffer, "item1")
  assert_eq(buffer1.items.length(), 1)
  assert_eq(buffer1.items[0], "item1")
  
  let buffer2 = add_to_buffer(buffer1, "item2")
  assert_eq(buffer2.items.length(), 2)
  assert_eq(buffer2.items[0], "item1")
  assert_eq(buffer2.items[1], "item2")
  
  // Test size-based flushing
  let mut buffer_to_fill = buffer2
  for i in 3 ..= 10 {
    buffer_to_fill = add_to_buffer(buffer_to_fill, "item" + i.to_string())
  }
  
  assert_eq(buffer_to_fill.items.length(), 10)
  assert_true(should_flush_by_size(buffer_to_fill))
  assert_false(should_flush_by_time(buffer_to_fill, 1640995250000))  // Only 50 seconds passed
  
  // Test time-based flushing
  let time_buffer = create_buffer(create_buffer_config(100, 30000))  // Max 100 items or 30 seconds
  let time_buffer1 = add_to_buffer(time_buffer, "item1")
  
  assert_false(should_flush_by_size(time_buffer1))  // Only 1 item, max is 100
  assert_false(should_flush_by_time(time_buffer1, 1640995250000))  // Only 50 seconds passed
  assert_true(should_flush_by_time(time_buffer1, 1640995300000))   // 100 seconds passed
  
  // Test buffer flushing
  let flushed_buffer = flush_buffer(buffer_to_fill, 1640995300000)
  assert_eq(flushed_buffer.items.length(), 0)
  assert_eq(flushed_buffer.last_flush, 1640995300000)
  
  // Test buffer with time-based flush
  let time_buffer_to_flush = add_to_buffer(time_buffer1, "item2")
  assert_eq(time_buffer_to_flush.items.length(), 2)
  
  let time_flushed_buffer = flush_buffer(time_buffer_to_flush, 1640995300000)
  assert_eq(time_flushed_buffer.items.length(), 0)
  assert_eq(time_flushed_buffer.last_flush, 1640995300000)
  
  // Test buffer statistics
  let get_buffer_stats = fn(buffer) {
    { 
      item_count = buffer.items.length(),
      max_size = buffer.config.max_size,
      max_time_ms = buffer.config.max_time_ms,
      age_ms = 1640995300000 - buffer.created_at,
      time_since_flush_ms = 1640995300000 - buffer.last_flush
    }
  }
  
  let stats = get_buffer_stats(buffer_to_fill)
  assert_eq(stats.item_count, 10)
  assert_eq(stats.max_size, 10)
  assert_eq(stats.max_time_ms, 60000)
  
  let time_stats = get_buffer_stats(time_buffer_to_flush)
  assert_eq(time_stats.item_count, 2)
  assert_eq(time_stats.max_size, 100)
  assert_eq(time_stats.max_time_ms, 30000)
}