// New Comprehensive Test Cases for Azimuth Telemetry System
// This file contains additional test cases to enhance test coverage

test "numeric precision and floating point operations" {
  let float_val1 = 3.14159
  let float_val2 = 2.71828
  let sum = float_val1 + float_val2
  assert_true(sum > 5.85 && sum < 5.86)
  
  let product = float_val1 * 2.0
  assert_true(product > 6.28 && product < 6.29)
  
  let int_div = 7 / 3
  assert_eq(int_div, 2)
  
  let float_div = 7.0 / 3.0
  assert_true(float_div > 2.33 && float_div < 2.34)
}

test "string manipulation and pattern matching" {
  let text = "azimuth_telemetry_system"
  assert_eq(text.length(), 25)
  
  let prefix = text.sub(0, 7)
  assert_eq(prefix, "azimuth")
  
  let suffix = text.sub(16, 6)
  assert_eq(suffix, "system")
  
  let words = text.split("_")
  assert_eq(words.length(), 3)
  assert_eq(words[0], "azimuth")
  assert_eq(words[1], "telemetry")
  assert_eq(words[2], "system")
}

test "collection operations and transformations" {
  let numbers = [1, 2, 3, 4, 5]
  let doubled = numbers.map(fn(x) { x * 2 })
  assert_eq(doubled.length(), 5)
  assert_eq(doubled[0], 2)
  assert_eq(doubled[4], 10)
  
  let evens = numbers.filter(fn(x) { x % 2 == 0 })
  assert_eq(evens.length(), 2)
  assert_eq(evens[0], 2)
  assert_eq(evens[1], 4)
  
  let sum = numbers.reduce(fn(acc, x) { acc + x }, 0)
  assert_eq(sum, 15)
}

test "error handling and exception scenarios" {
  let safe_divide = fn(numerator: Int, denominator: Int) -> Option[Int> {
    if denominator == 0 {
      None
    } else {
      Some(numerator / denominator)
    }
  }
  
  let result1 = safe_divide(10, 2)
  match result1 {
    Some(value) => assert_eq(value, 5)
    None => assert_true(false)
  }
  
  let result2 = safe_divide(10, 0)
  match result2 {
    Some(value) => assert_true(false)
    None => assert_true(true)
  }
}

test "data structure operations and algorithms" {
  let mut stack = []
  stack.push(1)
  stack.push(2)
  stack.push(3)
  
  assert_eq(stack.length(), 3)
  assert_eq(stack.pop(), Some(3))
  assert_eq(stack.length(), 2)
  
  let mut queue = []
  queue.push_back(1)
  queue.push_back(2)
  queue.push_back(3)
  
  assert_eq(queue.length(), 3)
  assert_eq(queue.pop_front(), Some(1))
  assert_eq(queue.length(), 2)
}

test "time and duration operations" {
  let current_time = Time.now()
  assert_true(current_time > 0)
  
  let duration = Duration.from_seconds(60)
  assert_eq(duration.to_seconds(), 60)
  assert_eq(duration.to_milliseconds(), 60000)
  
  let future_time = current_time + duration
  assert_true(future_time > current_time)
}

test "type system and polymorphism" {
  let process_value = fn(value: Any) -> String {
    match value {
      x as Int => "Integer: " + x.to_string()
      x as Float => "Float: " + x.to_string()
      x as String => "String: " + x
      x as Bool => "Boolean: " + (if x { "true" } else { "false" })
      _ => "Unknown type"
    }
  }
  
  assert_eq(process_value(42), "Integer: 42")
  assert_eq(process_value(3.14), "Float: 3.14")
  assert_eq(process_value("hello"), "String: hello")
  assert_eq(process_value(true), "Boolean: true")
}

test "concurrent operations and thread safety" {
  let counter = AtomicInt.new(0)
  
  let increment = fn() {
    for i in 1..=1000 {
      counter.increment()
    }
  }
  
  let thread1 = Thread.spawn(increment)
  let thread2 = Thread.spawn(increment)
  
  thread1.join()
  thread2.join()
  
  assert_eq(counter.get(), 2000)
}

test "serialization and deserialization" {
  let data = {
    "id": 123,
    "name": "test",
    "active": true,
    "values": [1, 2, 3]
  }
  
  let json_str = JSON.stringify(data)
  assert_true(json_str.contains("\"id\":123"))
  assert_true(json_str.contains("\"name\":\"test\""))
  
  let parsed = JSON.parse(json_str)
  assert_eq(parsed["id"], 123)
  assert_eq(parsed["name"], "test")
  assert_eq(parsed["active"], true)
  assert_eq(parsed["values"].length(), 3)
}