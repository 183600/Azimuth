// Azimuth Enhanced Fundamental Data Types Test Suite
// This file contains enhanced test cases for fundamental data types used in the telemetry system

// Test 1: Enhanced String Operations with Unicode Support
test "enhanced string operations with unicode support" {
  // Test basic string operations
  let basic_str = "azimuth"
  assert_eq(basic_str.length(), 7)
  assert_eq(basic_str.to_uppercase(), "AZIMUTH")
  assert_eq(basic_str.to_lowercase(), "azimuth")
  
  // Test unicode string operations
  let unicode_str = "遥测系统"
  assert_eq(unicode_str.length(), 4)
  assert_true(unicode_str.contains("遥测"))
  assert_true(unicode_str.contains("系统"))
  
  // Test string concatenation with unicode
  let mixed_str = basic_str + " " + unicode_str
  assert_eq(mixed_str, "azimuth 遥测系统")
  assert_eq(mixed_str.length(), 12) // 7 + 1 + 4
  
  // Test substring operations
  let long_str = "azimuth_telemetry_system"
  assert_eq(long_str.substring(0, 7), "azimuth")
  assert_eq(long_str.substring(8, 9), "telemetry")
  assert_eq(long_str.substring(18, 6), "system")
  
  // Test string splitting
  let csv_str = "service,version,environment"
  let parts = csv_str.split(",")
  assert_eq(parts.length(), 3)
  assert_eq(parts[0], "service")
  assert_eq(parts[1], "version")
  assert_eq(parts[2], "environment")
}

// Test 2: Advanced Numeric Operations
test "advanced numeric operations and precision" {
  // Test integer operations
  let int_val = 42
  assert_eq(int_val + 8, 50)
  assert_eq(int_val * 2, 84)
  assert_eq(int_val / 2, 21)
  assert_eq(int_val % 5, 2)
  
  // Test negative numbers
  let neg_int = -42
  assert_eq(neg_int + 50, 8)
  assert_eq(neg_int * -1, 42)
  assert_eq(-neg_int, 42)
  
  // Test floating point operations
  let float_val = 3.14159
  assert_eq(float_val + 1.0, 4.14159)
  assert_eq(float_val * 2.0, 6.28318)
  assert_eq(float_val / 2.0, 1.570795)
  
  // Test precision handling
  let precise_val = 0.1 + 0.2
  assert_true(precise_val > 0.29 && precise_val < 0.31)
  
  // Test large numbers
  let large_num = 1000000
  assert_eq(large_num / 1000, 1000)
  assert_eq(large_num % 1000, 0)
  
  // Test numeric conversions
  let int_to_float = int_val.to_float()
  assert_eq(int_to_float, 42.0)
  
  let float_to_int = float_val.to_int()
  assert_eq(float_to_int, 3)
}

// Test 3: Complex Boolean Logic and Truth Tables
test "complex boolean logic and truth tables" {
  // Basic boolean operations
  assert_true(true && true)
  assert_false(true && false)
  assert_false(false && false)
  
  assert_true(true || true)
  assert_true(true || false)
  assert_false(false || false)
  
  assert_false(!true)
  assert_true(!false)
  
  // Complex boolean expressions
  let a = true
  let b = false
  let c = true
  
  assert_true(a && (b || c))
  assert_false(a && b && c)
  assert_true((a || b) && (a || c))
  assert_false(!a && !b && !c)
  
  // De Morgan's laws
  assert_eq(!(a && b), !a || !b)
  assert_eq(!(a || b), !a && !b)
  
  // Boolean algebra identities
  assert_true(a && true == a)
  assert_true(a || false == a)
  assert_true(a && false == false)
  assert_true(a || true == true)
  
  // XOR operation simulation
  let xor = fn(x: Bool, y: Bool) { (x || y) && !(x && y) }
  assert_false(xor(true, true))
  assert_true(xor(true, false))
  assert_true(xor(false, true))
  assert_false(xor(false, false))
}

// Test 4: Enhanced Array Operations
test "enhanced array operations and transformations" {
  // Basic array operations
  let numbers = [1, 2, 3, 4, 5]
  assert_eq(numbers.length(), 5)
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
  
  // Array transformations
  let doubled = numbers.map(fn(x) { x * 2 })
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  let evens = numbers.filter(fn(x) { x % 2 == 0 })
  assert_eq(evens, [2, 4])
  
  let sum = numbers.reduce(fn(acc, x) { acc + x }, 0)
  assert_eq(sum, 15)
  
  // Array concatenation
  let arr1 = [1, 2, 3]
  let arr2 = [4, 5, 6]
  let concatenated = arr1 + arr2
  assert_eq(concatenated, [1, 2, 3, 4, 5, 6])
  
  // Array slicing
  let long_arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let slice = long_arr.slice(2, 5) // elements at indices 2, 3, 4
  assert_eq(slice, [3, 4, 5])
  
  // Array contains
  assert_true(numbers.contains(3))
  assert_false(numbers.contains(10))
  
  // Array find
  let found = numbers.find(fn(x) { x > 3 })
  assert_eq(found, Some(4))
  
  let not_found = numbers.find(fn(x) { x > 10 })
  assert_eq(not_found, None)
  
  // Array reverse
  let reversed = numbers.reverse()
  assert_eq(reversed, [5, 4, 3, 2, 1])
  
  // Array sorting
  let unsorted = [3, 1, 4, 1, 5, 9, 2, 6]
  let sorted = unsorted.sort()
  assert_eq(sorted, [1, 1, 2, 3, 4, 5, 6, 9])
}

// Test 5: Option Type Advanced Operations
test "option type advanced operations" {
  // Basic option operations
  let some_value = Some(42)
  let none_value = None
  
  // Option mapping
  let doubled = some_value.map(fn(x) { x * 2 })
  assert_eq(doubled, Some(84))
  
  let doubled_none = none_value.map(fn(x) { x * 2 })
  assert_eq(doubled_none, None)
  
  // Option filtering
  let filtered_some = some_value.filter(fn(x) { x > 40 })
  assert_eq(filtered_some, Some(42))
  
  let filtered_out = some_value.filter(fn(x) { x > 50 })
  assert_eq(filtered_out, None)
  
  // Option flat mapping
  let flat_mapped = some_value.flat_map(fn(x) { if x % 2 == 0 { Some(x / 2) } else { None } })
  assert_eq(flat_mapped, Some(21))
  
  let flat_mapped_none = some_value.flat_map(fn(x) { if x % 2 == 1 { Some(x / 2) } else { None } })
  assert_eq(flat_mapped_none, None)
  
  // Option default values
  assert_eq(some_value.get_or_else(0), 42)
  assert_eq(none_value.get_or_else(0), 0)
  
  // Option chaining
  let chained = Some(5)
    .map(fn(x) { x * 2 })
    .filter(fn(x) { x > 5 })
    .map(fn(x) { x + 1 })
  assert_eq(chained, Some(11))
  
  let broken_chain = Some(5)
    .map(fn(x) { x * 2 })
    .filter(fn(x) { x > 20 })
    .map(fn(x) { x + 1 })
  assert_eq(broken_chain, None)
}

// Test 6: Result Type Comprehensive Operations
test "result type comprehensive operations" {
  // Basic result operations
  let success = Ok("success")
  let failure = Err("error")
  
  // Result mapping
  let mapped_success = success.map(fn(s) { s.length() })
  assert_eq(mapped_success, Ok(7))
  
  let mapped_failure = failure.map(fn(s) { s.length() })
  assert_eq(mapped_failure, Err("error"))
  
  // Result flat mapping
  let flat_mapped_success = success.flat_map(fn(s) { if s.length() > 5 { Ok(s.length()) } else { Err("too short") } })
  assert_eq(flat_mapped_success, Ok(7))
  
  let flat_mapped_failure = failure.flat_map(fn(s) { if s.length() > 5 { Ok(s.length()) } else { Err("too short") } })
  assert_eq(flat_mapped_failure, Err("error"))
  
  // Result error mapping
  let error_mapped = failure.map_err(fn(e) { e + " (processed)" })
  assert_eq(error_mapped, Err("error (processed)"))
  
  // Result chaining
  let chained_result = Ok(5)
    .map(fn(x) { x * 2 })
    .flat_map(fn(x) { if x > 5 { Ok(x + 1) } else { Err("too small") } })
  assert_eq(chained_result, Ok(11))
  
  // Result default values
  assert_eq(success.get_or_else("default"), "success")
  assert_eq(failure.get_or_else("default"), "default")
  
  // Result to option conversion
  assert_eq(success.to_option(), Some("success"))
  assert_eq(failure.to_option(), None)
}

// Test 7: Tuple Operations and Pattern Matching
test "tuple operations and pattern matching" {
  // Basic tuple operations
  let pair = (42, "answer")
  assert_eq(pair.0, 42)
  assert_eq(pair.1, "answer")
  
  let triple = (1, 2, 3)
  assert_eq(triple.0 + triple.1 + triple.2, 6)
  
  // Tuple destructuring
  let (x, y) = pair
  assert_eq(x, 42)
  assert_eq(y, "answer")
  
  let (a, b, c) = triple
  assert_eq(a + b + c, 6)
  
  // Tuple transformation
  let transformed_pair = pair.map_first(fn(n) { n * 2 })
  assert_eq(transformed_pair, (84, "answer"))
  
  let transformed_second = pair.map_second(fn(s) { s.length() })
  assert_eq(transformed_second, (42, 6))
  
  // Tuple comparison
  let pair1 = (1, 2)
  let pair2 = (1, 2)
  let pair3 = (1, 3)
  
  assert_eq(pair1, pair2)
  assert_true(pair1 != pair3)
  
  // Nested tuples
  let nested = ((1, 2), (3, 4))
  assert_eq(nested.0.0, 1)
  assert_eq(nested.0.1, 2)
  assert_eq(nested.1.0, 3)
  assert_eq(nested.1.1, 4)
  
  // Tuple with functions
  let add_pair = fn(p: (Int, Int)) { p.0 + p.1 }
  assert_eq(add_pair((3, 4)), 7)
  
  let swap_pair = fn(p: (Int, Int)) { (p.1, p.0) }
  assert_eq(swap_pair((3, 4)), (4, 3))
}

// Test 8: Enum Advanced Pattern Matching
test "enum advanced pattern matching" {
  // Define complex enum types
  enum TelemetryValue {
    String(String)
    Int(Int)
    Float(Float)
    Bool(Bool)
    Array(Array[TelemetryValue])
    Object(Array[(String, TelemetryValue)])
  }
  
  enum StatusCode {
    Ok
    Error(String)
    Warning(String)
    Timeout(Int)
  }
  
  // Test basic enum pattern matching
  let string_val = TelemetryValue::String("test")
  let string_result = match string_val {
    TelemetryValue::String(s) => s.length()
    TelemetryValue::Int(i) => i
    TelemetryValue::Float(f) => f.to_int()
    TelemetryValue::Bool(b) => if b { 1 } else { 0 }
    TelemetryValue::Array(arr) => arr.length()
    TelemetryValue::Object(obj) => obj.length()
  }
  assert_eq(string_result, 4)
  
  // Test nested enum pattern matching
  let array_val = TelemetryValue::Array([
    TelemetryValue::String("item1"),
    TelemetryValue::Int(42),
    TelemetryValue::Bool(true)
  ])
  
  let array_result = match array_val {
    TelemetryValue::Array(arr) => {
      let mut count = 0
      for item in arr {
        match item {
          TelemetryValue::String(_) => count = count + 1
          TelemetryValue::Int(_) => count = count + 2
          TelemetryValue::Bool(_) => count = count + 3
          _ => count = count + 0
        }
      }
      count
    }
    _ => 0
  }
  assert_eq(array_result, 6) // 1 + 2 + 3
  
  // Test enum with guards
  let status_code = StatusCode::Timeout(5000)
  let status_result = match status_code {
    StatusCode::Ok => "success"
    StatusCode::Error(msg) => "error: " + msg
    StatusCode::Warning(msg) if msg.length() > 10 => "long warning: " + msg
    StatusCode::Warning(msg) => "warning: " + msg
    StatusCode::Timeout(ms) if ms > 1000 => "long timeout: " + ms.to_string() + "ms"
    StatusCode::Timeout(ms) => "short timeout: " + ms.to_string() + "ms"
  }
  assert_eq(status_result, "long timeout: 5000ms")
  
  // Test enum transformation
  let transform_value = fn(value: TelemetryValue) {
    match value {
      TelemetryValue::String(s) => TelemetryValue::Int(s.length())
      TelemetryValue::Int(i) => TelemetryValue::Float(i.to_float())
      TelemetryValue::Float(f) => TelemetryValue::Bool(f > 0.0)
      TelemetryValue::Bool(b) => TelemetryValue::String(if b { "true" } else { "false" })
      TelemetryValue::Array(arr) => TelemetryValue::Int(arr.length())
      TelemetryValue::Object(obj) => TelemetryValue::Int(obj.length())
    }
  }
  
  let transformed_string = transform_value(TelemetryValue::String("hello"))
  match transformed_string {
    TelemetryValue::Int(i) => assert_eq(i, 5)
    _ => assert_true(false)
  }
  
  let transformed_int = transform_value(TelemetryValue::Int(42))
  match transformed_int {
    TelemetryValue::Float(f) => assert_eq(f, 42.0)
    _ => assert_true(false)
  }
}

// Test 9: Record/Struct Advanced Operations
test "record and struct advanced operations" {
  // Define complex record types
  type Span = {
    name: String,
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    start_time: Int,
    end_time: Option[Int],
    status: String,
    attributes: Array[(String, String)]
  }
  
  type Metric = {
    name: String,
    value: Float,
    unit: String,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  // Create records
  let span = {
    name: "database_query",
    trace_id: "trace-123",
    span_id: "span-456",
    parent_span_id: Some("span-789"),
    start_time: 1640995200,
    end_time: Some(1640995250),
    status: "ok",
    attributes: [
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM users")
    ]
  }
  
  let metric = {
    name: "response_time",
    value: 125.5,
    unit: "ms",
    timestamp: 1640995250,
    tags: [
      ("service", "api"),
      ("endpoint", "/users")
    ]
  }
  
  // Test record field access
  assert_eq(span.name, "database_query")
  assert_eq(span.trace_id, "trace-123")
  assert_eq(span.span_id, "span-456")
  assert_eq(span.start_time, 1640995200)
  assert_eq(span.status, "ok")
  
  // Test option field access
  match span.end_time {
    Some(end_time) => assert_eq(end_time, 1640995250)
    None => assert_true(false)
  }
  
  match span.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, "span-789")
    None => assert_true(false)
  }
  
  // Test record update
  let completed_span = { span | status: "completed", end_time: Some(1640995300) }
  assert_eq(completed_span.name, "database_query")  // unchanged
  assert_eq(completed_span.status, "completed")  // updated
  assert_eq(completed_span.end_time, Some(1640995300))  // updated
  
  // Test record functions
  let span_duration = fn(s: Span) {
    match s.end_time {
      Some(end) => end - s.start_time
      None => -1  // Ongoing span
    }
  }
  
  assert_eq(span_duration(span), 50)
  assert_eq(span_duration(completed_span), 100)
  
  // Test record comparison
  let same_span = {
    name: "database_query",
    trace_id: "trace-123",
    span_id: "span-456",
    parent_span_id: Some("span-789"),
    start_time: 1640995200,
    end_time: Some(1640995250),
    status: "ok",
    attributes: [
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM users")
    ]
  }
  
  assert_eq(span.name, same_span.name)
  assert_eq(span.trace_id, same_span.trace_id)
  assert_eq(span.span_id, same_span.span_id)
  
  // Test record transformation
  let span_to_summary = fn(s: Span) {
    {
      span_name: s.name,
      trace_id: s.trace_id,
      duration: span_duration(s),
      status: s.status
    }
  }
  
  let summary = span_to_summary(span)
  assert_eq(summary.span_name, "database_query")
  assert_eq(summary.trace_id, "trace-123")
  assert_eq(summary.duration, 50)
  assert_eq(summary.status, "ok")
  
  // Test record filtering
  let filter_spans_by_status = fn(spans: Array[Span], status: String) {
    spans.filter(fn(s) { s.status == status })
  }
  
  let spans = [span, completed_span]
  let ok_spans = filter_spans_by_status(spans, "ok")
  assert_eq(ok_spans.length(), 1)
  assert_eq(ok_spans[0].name, "database_query")
  
  let completed_spans = filter_spans_by_status(spans, "completed")
  assert_eq(completed_spans.length(), 1)
  assert_eq(completed_spans[0].name, "database_query")
}

// Test 10: Advanced Type System Features
test "advanced type system features" {
  // Test generic type simulation
  let identity = fn[T](x: T) { x }
  assert_eq(identity[Int](42), 42)
  assert_eq(identity[String]("hello"), "hello")
  
  // Test higher-kinded type simulation
  let map_array = fn[T, U](arr: Array[T], f: T -> U) {
    let mut result = []
    for item in arr {
      result = result.push(f(item))
    }
    result
  }
  
  let numbers = [1, 2, 3, 4, 5]
  let strings = map_array[Int, String](numbers, fn(n) { n.to_string() })
  assert_eq(strings, ["1", "2", "3", "4", "5"])
  
  let doubled = map_array[Int, Int](numbers, fn(n) { n * 2 })
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  // Test type constraints simulation
  let comparable_less_than = fn[T](a: T, b: T) -> Bool {
    // This is a simplified simulation
    match (a, b) {
      (Int(x), Int(y)) => x < y
      (String(x), String(y)) => x < y
      (Float(x), Float(y)) => x < y
      _ => false
    }
  }
  
  assert_true(comparable_less_than[Int](1, 2))
  assert_false(comparable_less_than[Int](2, 1))
  assert_true(comparable_less_than[String]("apple", "banana"))
  assert_false(comparable_less_than[String]("banana", "apple"))
  
  // Test type class simulation
  let showable_to_string = fn[T](value: T) -> String {
    match value {
      Int(x) => x.to_string()
      String(x) => x
      Float(x) => x.to_string()
      Bool(x) => if x { "true" } else { "false" }
      _ => "unknown"
    }
  }
  
  assert_eq(showable_to_string[Int](42), "42")
  assert_eq(showable_to_string[String]("hello"), "hello")
  assert_eq(showable_to_string[Float](3.14), "3.14")
  assert_eq(showable_to_string[Bool](true), "true")
  
  // Test phantom type simulation
  type TypedValue[T] = { value: String, type_marker: T }
  
  let int_marker = { marker: "int" }
  let string_marker = { marker: "string" }
  
  let typed_int = { value: "42", type_marker: int_marker }
  let typed_string = { value: "hello", type_marker: string_marker }
  
  assert_eq(typed_int.value, "42")
  assert_eq(typed_int.type_marker.marker, "int")
  assert_eq(typed_string.value, "hello")
  assert_eq(typed_string.type_marker.marker, "string")
  
  // Test type-safe operations
  let safe_add = fn(a: TypedValue[{ marker: "int" }], b: TypedValue[{ marker: "int" }]) {
    let x = a.value.to_int()
    let y = b.value.to_int()
    { value: (x + y).to_string(), type_marker: int_marker }
  }
  
  let sum = safe_add(typed_int, { value: "10", type_marker: int_marker })
  assert_eq(sum.value, "52")
  assert_eq(sum.type_marker.marker, "int")
}