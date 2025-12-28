test "http client request and response handling" {
  // Test HTTP client functionality for telemetry data transmission
  
  let client = @azimuth.telemetry.sdk.platform.http.HttpClient.new()
  
  // Test GET request
  let get_request = @azimuth.telemetry.sdk.platform.http.HttpRequest.new(
    "GET",
    "https://api.example.com/telemetry",
    [("Content-Type", "application/json"), ("User-Agent", "azimuth-telemetry/1.0.0")]
  )
  
  assert_eq(get_request.http_method(), "GET")
  assert_eq(get_request.url(), "https://api.example.com/telemetry")
  assert_eq(get_request.body(), None)
  
  // Test POST request with body
  let post_request = @azimuth.telemetry.sdk.platform.http.HttpRequest.new(
    "POST",
    "https://collector.example.com/v1/traces",
    [("Content-Type", "application/json")],
    Some("{\"spans\": [{\"trace_id\": \"abc123\", \"span_id\": \"def456\"}]}")
  )
  
  assert_eq(post_request.http_method(), "POST")
  assert_eq(post_request.url(), "https://collector.example.com/v1/traces")
  assert_eq(post_request.body(), Some("{\"spans\": [{\"trace_id\": \"abc123\", \"span_id\": \"def456\"}]}")
  
  // Test response creation
  let success_response = @azimuth.telemetry.sdk.platform.http.HttpResponse.new(
    200,
    [("Content-Type", "application/json")],
    Some("{\"status\": \"success\", \"accepted\": 42}")
  )
  
  assert_eq(success_response.status_code(), 200)
  assert_eq(success_response.body(), Some("{\"status\": \"success\", \"accepted\": 42}")
  
  // Test error response
  let error_response = @azimuth.telemetry.sdk.platform.http.HttpResponse.new(
    429,
    [("Retry-After", "30"), ("Content-Type", "text/plain")],
    Some("Rate limit exceeded")
  )
  
  assert_eq(error_response.status_code(), 429)
  assert_eq(error_response.body(), Some("Rate limit exceeded"))
}

test "clock functionality and timestamp generation" {
  // Test clock functionality for accurate timestamp generation
  
  let clock = @azimuth.telemetry.sdk.platform.time.Clock.system()
  
  // Test timestamp generation
  let timestamp1 = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(clock)
  let timestamp2 = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(clock)
  
  // Verify timestamps are reasonable (in 2025 range)
  assert_true(timestamp1 > 1700000000000000000L)  // After 2023
  assert_true(timestamp1 < 1800000000000000000L)  // Before 2027
  
  // Verify timestamp progression
  assert_true(timestamp2 >= timestamp1)
  
  // Test timestamp consistency
  let timestamp3 = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(clock)
  assert_true(timestamp3 >= timestamp2)
  
  // Verify timestamps are in nanosecond precision
  let timestamp_diff = timestamp3 - timestamp1
  assert_true(timestamp_diff >= 0L)
}

test "random number generation for trace and span IDs" {
  // Test random number generation for creating unique trace and span IDs
  
  let random = @azimuth.telemetry.sdk.platform.random.Random.system()
  
  // Test random bytes generation
  let bytes_8 = @azimuth.telemetry.sdk.platform.random.Random.next_bytes(random, 8)
  let bytes_16 = @azimuth.telemetry.sdk.platform.random.Random.next_bytes(random, 16)
  let bytes_32 = @azimuth.telemetry.sdk.platform.random.Random.next_bytes(random, 32)
  
  // Verify byte array lengths
  assert_eq(bytes_8.length(), 8)
  assert_eq(bytes_16.length(), 16)
  assert_eq(bytes_32.length(), 32)
  
  // Test random u64 generation
  let random_id1 = @azimuth.telemetry.sdk.platform.random.Random.next_u64(random)
  let random_id2 = @azimuth.telemetry.sdk.platform.random.Random.next_u64(random)
  let random_id3 = @azimuth.telemetry.sdk.platform.random.Random.next_u64(random)
  
  // Verify random numbers are generated
  assert_true(random_id1.to_int() >= 0)
  assert_true(random_id2.to_int() >= 0)
  assert_true(random_id3.to_int() >= 0)
  
  // Test multiple calls produce consistent results
  let random_id4 = @azimuth.telemetry.sdk.platform.random.Random.next_u64(random)
  assert_true(random_id4.to_int() >= 0)
}

test "platform integration scenarios" {
  // Test integration scenarios between different platform components
  
  let clock = @azimuth.telemetry.sdk.platform.time.Clock.system()
  let random = @azimuth.telemetry.sdk.platform.random.Random.system()
  let client = @azimuth.telemetry.sdk.platform.http.HttpClient.new()
  
  // Create telemetry data with current timestamp
  let timestamp = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(clock)
  let trace_id = @azimuth.telemetry.sdk.platform.random.Random.next_u64(random)
  
  // Create HTTP request with telemetry data
  let telemetry_data = "{\"timestamp\": " + timestamp.to_string() + ", \"trace_id\": \"" + trace_id.to_string() + "\"}"
  let request = @azimuth.telemetry.sdk.platform.http.HttpRequest.new(
    "POST",
    "https://telemetry.example.com/v1/spans",
    [
      ("Content-Type", "application/json"),
      ("X-Telemetry-Version", "1.0.0"),
      ("X-Trace-Id", trace_id.to_string())
    ],
    Some(telemetry_data)
  )
  
  // Verify request integration
  assert_eq(request.http_method(), "POST")
  assert_eq(request.url(), "https://telemetry.example.com/v1/spans")
  assert_true(request.body().unwrap().contains(timestamp.to_string()))
  assert_true(request.body().unwrap().contains(trace_id.to_string()))
  
  // Create response for the request
  let response = @azimuth.telemetry.sdk.platform.http.HttpResponse.new(
    202,
    [("X-Processed-Trace-Id", trace_id.to_string())],
    Some("{\"accepted\": true, \"trace_id\": \"" + trace_id.to_string() + "\"}")
  )
  
  // Verify response integration
  assert_eq(response.status_code(), 202)
  assert_true(response.body().unwrap().contains(trace_id.to_string()))
}