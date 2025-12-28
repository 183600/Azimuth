test "platform time abstraction" {
  // Test platform time abstraction
  let clock = @azimuth.telemetry.sdk.platform.time.Clock.system()
  
  // Test getting current time
  let time1 = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(clock)
  assert_true(time1 > 0)
  
  // Wait a bit and get time again
  // Note: In real tests, we might need to handle this differently
  // For now, just ensure the time is reasonable
  let time2 = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(clock)
  assert_true(time2 >= time1)
  
  // Test that time is in nanoseconds format
  // Current time should be roughly around 2025+ years in nanoseconds
  // 2025-01-01 would be approximately 1735689600000000000 nanoseconds since epoch
  assert_true(time1 > 1700000000000000000)  // Sometime after 2023
  assert_true(time1 < 2000000000000000000)  // Sometime before 2033
}

test "platform random abstraction" {
  // Test platform random abstraction
  let random = @azimuth.telemetry.sdk.platform.random.Random.system()
  
  // Test generating random bytes
  let buffer1 = @azimuth.telemetry.sdk.platform.random.Random.next_bytes(random, 16)
  assert_eq(buffer1.length(), 16)
  
  let buffer2 = @azimuth.telemetry.sdk.platform.random.Random.next_bytes(random, 16)
  assert_eq(buffer2.length(), 16)
  
  // Buffers should be different (very high probability)
  assert_true(buffer1 != buffer2)
  
  // Test generating random u64
  let rand1 = @azimuth.telemetry.sdk.platform.random.Random.next_u64(random)
  let rand2 = @azimuth.telemetry.sdk.platform.random.Random.next_u64(random)
  
  // Values should be different (very high probability)
  assert_true(rand1 != rand2)
  
  // Test that values are within u64 range
  assert_true(rand1 >= 0)
  assert_true(rand2 >= 0)
}

test "trace id generation using platform random" {
  // Test generating trace IDs using platform random
  let random = @azimuth.telemetry.sdk.platform.random.Random.system()
  
  // Generate a 16-byte trace ID
  let trace_id_bytes = @azimuth.telemetry.sdk.platform.random.Random.next_bytes(random, 16)
  assert_eq(trace_id_bytes.length(), 16)
  
  // Convert to hex string (this would be implemented in the actual code)
  let trace_id_hex = @azimuth.telemetry.sdk.platform.random.bytes_to_hex(trace_id_bytes)
  assert_eq(trace_id_hex.length(), 32)  // 16 bytes = 32 hex characters
  
  // Generate another trace ID and ensure they're different
  let trace_id_bytes2 = @azimuth.telemetry.sdk.platform.random.Random.next_bytes(random, 16)
  let trace_id_hex2 = @azimuth.telemetry.sdk.platform.random.bytes_to_hex(trace_id_bytes2)
  
  assert_true(trace_id_hex != trace_id_hex2)
}

test "span id generation using platform random" {
  // Test generating span IDs using platform random
  let random = @azimuth.telemetry.sdk.platform.random.Random.system()
  
  // Generate an 8-byte span ID
  let span_id_bytes = @azimuth.telemetry.sdk.platform.random.Random.next_bytes(random, 8)
  assert_eq(span_id_bytes.length(), 8)
  
  // Convert to hex string
  let span_id_hex = @azimuth.telemetry.sdk.platform.random.bytes_to_hex(span_id_bytes)
  assert_eq(span_id_hex.length(), 16)  // 8 bytes = 16 hex characters
  
  // Generate another span ID and ensure they're different
  let span_id_bytes2 = @azimuth.telemetry.sdk.platform.random.Random.next_bytes(random, 8)
  let span_id_hex2 = @azimuth.telemetry.sdk.platform.random.bytes_to_hex(span_id_bytes2)
  
  assert_true(span_id_hex != span_id_hex2)
}

test "platform http client abstraction" {
  // Test platform HTTP client abstraction
  let client = @azimuth.telemetry.sdk.platform.http.HttpClient.new()
  
  // Test creating HTTP request
  let request = @azimuth.telemetry.sdk.platform.http.HttpRequest.new(
    "GET",
    "https://httpbin.org/get",
    [],
    None
  )
  
  assert_eq(request.method(), "GET")
  assert_eq(request.url(), "https://httpbin.org/get")
  assert_true(request.headers().length() == 0)
  assert_true(request.body() == None)
  
  // Test adding headers
  let request_with_headers = @azimuth.telemetry.sdk.platform.http.HttpRequest.with_header(
    request,
    "Content-Type",
    "application/json"
  )
  
  let headers = request_with_headers.headers()
  assert_true(headers.length() > 0)
  
  // Test creating request with body
  let request_with_body = @azimuth.telemetry.sdk.platform.http.HttpRequest.new(
    "POST",
    "https://httpbin.org/post",
    [("Content-Type", "application/json")],
    Some("{\"test\": \"data\"}")
  )
  
  assert_eq(request_with_body.method(), "POST")
  assert_eq(request_with_body.url(), "https://httpbin.org/post")
  assert_true(request_with_body.body() != None)
  assert_eq(request_with_body.body().unwrap(), "{\"test\": \"data\"}")
}

test "platform http response handling" {
  // Test platform HTTP response handling
  let mock_response = @azimuth.telemetry.sdk.platform.http.HttpResponse.new(
    200,
    [("Content-Type", "application/json")],
    Some("{\"status\": \"ok\"}")
  )
  
  assert_eq(mock_response.status_code(), 200)
  assert_true(mock_response.headers().length() > 0)
  assert_true(mock_response.body() != None)
  assert_eq(mock_response.body().unwrap(), "{\"status\": \"ok\"}")
  
  // Test accessing specific headers
  let content_type = @azimuth.telemetry.sdk.platform.http.HttpResponse.get_header(mock_response, "Content-Type")
  assert_eq(content_type, Some("application/json"))
  
  let non_existent = @azimuth.telemetry.sdk.platform.http.HttpResponse.get_header(mock_response, "Non-Existent")
  assert_eq(non_existent, None)
}

test "platform error handling" {
  // Test platform error handling for time operations
  let clock = @azimuth.telemetry.sdk.platform.time.Clock.system()
  
  // This should always succeed in normal circumstances
  let time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(clock)
  assert_true(time > 0)
  
  // Test random error handling
  let random = @azimuth.telemetry.sdk.platform.random.Random.system()
  
  // These should always succeed in normal circumstances
  let bytes = @azimuth.telemetry.sdk.platform.random.Random.next_bytes(random, 8)
  assert_eq(bytes.length(), 8)
  
  let value = @azimuth.telemetry.sdk.platform.random.Random.next_u64(random)
  assert_true(value >= 0)
}

test "hex encoding and decoding" {
  // Test hex encoding and decoding utilities
  let random = @azimuth.telemetry.sdk.platform.random.Random.system()
  
  // Generate random bytes
  let bytes = @azimuth.telemetry.sdk.platform.random.Random.next_bytes(random, 10)
  
  // Encode to hex
  let hex = @azimuth.telemetry.sdk.platform.random.bytes_to_hex(bytes)
  assert_eq(hex.length(), 20)  // 10 bytes = 20 hex chars
  
  // Decode back to bytes
  let decoded_bytes = @azimuth.telemetry.sdk.platform.random.hex_to_bytes(hex)
  assert_eq(decoded_bytes, bytes)
  
  // Test with known values
  let known_bytes = [0x12, 0x34, 0x56, 0x78, 0xAB, 0xCD, 0xEF]
  let known_hex = @azimuth.telemetry.sdk.platform.random.bytes_to_hex(known_bytes)
  assert_eq(known_hex, "12345678abcdef")
  
  let decoded_known = @azimuth.telemetry.sdk.platform.random.hex_to_bytes(known_hex)
  assert_eq(decoded_known, known_bytes)
}