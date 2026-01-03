// Azimuth Pattern Matching Tests
// This file contains comprehensive tests for pattern matching capabilities

// Test 1: Basic Pattern Matching on Primitives
test "basic pattern matching on primitive types" {
  // Pattern matching on integers
  let describe_number = fn(n : Int) {
    match n {
      0 => "zero",
      1 => "one",
      2 => "two",
      _ => "many"
    }
  }
  
  assert_eq(describe_number(0), "zero")
  assert_eq(describe_number(1), "one")
  assert_eq(describe_number(2), "two")
  assert_eq(describe_number(5), "many")
  
  // Pattern matching with guards
  let describe_number_with_guard = fn(n : Int) {
    match n {
      x if x < 0 => "negative",
      0 => "zero",
      x if x > 0 && x < 10 => "small positive",
      _ => "large positive"
    }
  }
  
  assert_eq(describe_number_with_guard(-5), "negative")
  assert_eq(describe_number_with_guard(0), "zero")
  assert_eq(describe_number_with_guard(5), "small positive")
  assert_eq(describe_number_with_guard(15), "large positive")
  
  // Pattern matching on booleans
  let bool_to_string = fn(b : Bool) {
    match b {
      true => "true",
      false => "false"
    }
  }
  
  assert_eq(bool_to_string(true), "true")
  assert_eq(bool_to_string(false), "false")
  
  // Pattern matching on strings
  let describe_string = fn(s : String) {
    match s {
      "" => "empty",
      "hello" => "greeting",
      x if x.length() > 10 => "long string",
      _ => "short string"
    }
  }
  
  assert_eq(describe_string(""), "empty")
  assert_eq(describe_string("hello"), "greeting")
  assert_eq(describe_string("this is a very long string"), "long string")
  assert_eq(describe_string("short"), "short string")
}

// Test 2: Pattern Matching on Option Types
test "pattern matching on option types" {
  // Basic option matching
  let option_to_string = fn(opt : Option[Int]) {
    match opt {
      Some(value) => "value: " + value.to_string(),
      None => "no value"
    }
  }
  
  assert_eq(option_to_string(Some(42)), "value: 42")
  assert_eq(option_to_string(None), "no value")
  
  // Nested option matching
  let nested_option_to_string = fn(opt : Option[Option[String]]) {
    match opt {
      Some(Some(inner)) => "nested: " + inner,
      Some(None) => "outer some, inner none",
      None => "outer none"
    }
  }
  
  assert_eq(nested_option_to_string(Some(Some("hello"))), "nested: hello")
  assert_eq(nested_option_to_string(Some(None)), "outer some, inner none")
  assert_eq(nested_option_to_string(None), "outer none")
  
  // Option matching with guards
  let option_with_guard = fn(opt : Option[Int]) {
    match opt {
      Some(x) if x > 100 => "large value: " + x.to_string(),
      Some(x) if x < 0 => "negative value: " + x.to_string(),
      Some(0) => "zero value",
      Some(x) => "normal value: " + x.to_string(),
      None => "no value"
    }
  }
  
  assert_eq(option_with_guard(Some(150)), "large value: 150")
  assert_eq(option_with_guard(Some(-5)), "negative value: -5")
  assert_eq(option_with_guard(Some(0)), "zero value")
  assert_eq(option_with_guard(Some(42)), "normal value: 42")
  assert_eq(option_with_guard(None), "no value")
}

// Test 3: Pattern Matching on Result Types
test "pattern matching on result types" {
  // Basic result matching
  let result_to_string = fn(result : Result[Int, String]) {
    match result {
      Ok(value) => "success: " + value.to_string(),
      Err(error) => "error: " + error
    }
  }
  
  assert_eq(result_to_string(Ok(42)), "success: 42")
  assert_eq(result_to_string(Err("something went wrong")), "error: something went wrong")
  
  // Result matching with guards
  let result_with_guard = fn(result : Result[Int, String]) {
    match result {
      Ok(x) if x > 100 => "large success: " + x.to_string(),
      Ok(x) if x < 0 => "negative success: " + x.to_string(),
      Ok(0) => "zero success",
      Ok(x) => "normal success: " + x.to_string(),
      Err(msg) if msg.contains("timeout") => "timeout error: " + msg,
      Err(msg) if msg.contains("network") => "network error: " + msg,
      Err(msg) => "other error: " + msg
    }
  }
  
  assert_eq(result_with_guard(Ok(150)), "large success: 150")
  assert_eq(result_with_guard(Ok(-5)), "negative success: -5")
  assert_eq(result_with_guard(Ok(0)), "zero success")
  assert_eq(result_with_guard(Ok(42)), "normal success: 42")
  assert_eq(result_with_guard(Err("connection timeout")), "timeout error: connection timeout")
  assert_eq(result_with_guard(Err("network unreachable")), "network error: network unreachable")
  assert_eq(result_with_guard(Err("unknown error")), "other error: unknown error")
  
  // Nested result matching
  let nested_result_to_string = fn(result : Result[Result[String, String], String]) {
    match result {
      Ok(Ok(inner)) => "nested success: " + inner,
      Ok(Err(inner_error)) => "nested error: " + inner_error,
      Err(outer_error) => "outer error: " + outer_error
    }
  }
  
  assert_eq(nested_result_to_string(Ok(Ok("hello"))), "nested success: hello")
  assert_eq(nested_result_to_string(Ok(Err("inner error"))), "nested error: inner error")
  assert_eq(nested_result_to_string(Err("outer error")), "outer error: outer error")
}

// Test 4: Pattern Matching on Enums
test "pattern matching on enum types" {
  // Simple enum
  enum Color {
    Red
    Green
    Blue
  }
  
  let color_to_string = fn(color : Color) {
    match color {
      Color::Red => "red",
      Color::Green => "green",
      Color::Blue => "blue"
    }
  }
  
  assert_eq(color_to_string(Color::Red), "red")
  assert_eq(color_to_string(Color::Green), "green")
  assert_eq(color_to_string(Color::Blue), "blue")
  
  // Enum with associated values
  enum Shape {
    Circle(Float)
    Rectangle(Float, Float)
    Triangle(Float, Float, Float)
  }
  
  let shape_to_string = fn(shape : Shape) {
    match shape {
      Shape::Circle(radius) => "circle with radius " + radius.to_string(),
      Shape::Rectangle(width, height) => "rectangle " + width.to_string() + "x" + height.to_string(),
      Shape::Triangle(a, b, c) => "triangle with sides " + a.to_string() + ", " + b.to_string() + ", " + c.to_string()
    }
  }
  
  assert_eq(shape_to_string(Shape::Circle(5.0)), "circle with radius 5.0")
  assert_eq(shape_to_string(Shape::Rectangle(10.0, 20.0)), "rectangle 10.0x20.0")
  assert_eq(shape_to_string(Shape::Triangle(3.0, 4.0, 5.0)), "triangle with sides 3.0, 4.0, 5.0")
  
  // Enum matching with guards
  let shape_area = fn(shape : Shape) {
    match shape {
      Shape::Circle(r) if r > 0.0 => 3.14159 * r * r,
      Shape::Rectangle(w, h) if w > 0.0 && h > 0.0 => w * h,
      Shape::Triangle(a, b, c) if a > 0.0 && b > 0.0 && c > 0.0 => {
        let s = (a + b + c) / 2.0
        (s * (s - a) * (s - b) * (s - c)).sqrt()
      }
      _ => 0.0 // Invalid dimensions
    }
  }
  
  assert_eq(shape_area(Shape::Circle(2.0)), 12.56636) // π * 2²
  assert_eq(shape_area(Shape::Rectangle(3.0, 4.0)), 12.0)
  assert_eq(shape_area(Shape::Triangle(3.0, 4.0, 5.0)), 6.0) // 3-4-5 right triangle
}

// Test 5: Pattern Matching on Tuples
test "pattern matching on tuples" {
  // Tuple destructuring
  let describe_tuple = fn(pair : (Int, String)) {
    match pair {
      (0, _) => "first is zero",
      (_, "") => "second is empty",
      (x, y) if x > 10 => "first is large: " + x.to_string() + ", second: " + y,
      (x, y) => "pair: " + x.to_string() + ", " + y
    }
  }
  
  assert_eq(describe_tuple((0, "hello")), "first is zero")
  assert_eq(describe_tuple((5, "")), "second is empty")
  assert_eq(describe_tuple((15, "world")), "first is large: 15, second: world")
  assert_eq(describe_tuple((5, "test")), "pair: 5, test")
  
  // Triple tuple matching
  let describe_triple = fn(triple : (Int, String, Bool)) {
    match triple {
      (x, y, true) => "true boolean: " + x.to_string() + ", " + y,
      (x, _, false) => "false boolean: " + x.to_string(),
      (0, _, _) => "first is zero",
      (_, y, z) => "triple: " + y + ", " + z.to_string()
    }
  }
  
  assert_eq(describe_triple((5, "hello", true)), "true boolean: 5, hello")
  assert_eq(describe_triple((10, "world", false)), "false boolean: 10")
  assert_eq(describe_triple((0, "test", true)), "first is zero")
  assert_eq(describe_triple((5, "example", false)), "false boolean: 5")
  
  // Nested tuple matching
  let nested_tuple = fn(value : ((Int, String), Bool)) {
    match value {
      ((x, y), true) => "nested true: " + x.to_string() + ", " + y,
      ((0, _), false) => "nested false with zero first",
      ((x, y), false) => "nested false: " + x.to_string() + ", " + y
    }
  }
  
  assert_eq(nested_tuple(((5, "hello"), true)), "nested true: 5, hello")
  assert_eq(nested_tuple(((0, "world"), false)), "nested false with zero first")
  assert_eq(nested_tuple(((10, "test"), false)), "nested false: 10, test")
}

// Test 6: Pattern Matching on Arrays
test "pattern matching on arrays" {
  // Array length matching
  let describe_array = fn(arr : [Int]) {
    match arr {
      [] => "empty array",
      [x] => "single element: " + x.to_string(),
      [x, y] => "two elements: " + x.to_string() + ", " + y.to_string(),
      [x, y, z] => "three elements: " + x.to_string() + ", " + y.to_string() + ", " + z.to_string(),
      arr if arr.length() > 3 => "long array with " + arr.length().to_string() + " elements"
    }
  }
  
  assert_eq(describe_array([]), "empty array")
  assert_eq(describe_array([5]), "single element: 5")
  assert_eq(describe_array([1, 2]), "two elements: 1, 2")
  assert_eq(describe_array([1, 2, 3]), "three elements: 1, 2, 3")
  assert_eq(describe_array([1, 2, 3, 4, 5]), "long array with 5 elements")
  
  // Array head-tail matching
  let sum_head_tail = fn(arr : [Int]) {
    match arr {
      [] => 0,
      [x] => x,
      [head, ..tail] => head + tail.reduce(fn(acc, val) { acc + val }, 0)
    }
  }
  
  assert_eq(sum_head_tail([]), 0)
  assert_eq(sum_head_tail([5]), 5)
  assert_eq(sum_head_tail([1, 2, 3, 4]), 10) // 1 + (2 + 3 + 4)
  
  // Array pattern with guards
  let describe_array_content = fn(arr : [Int]) {
    match arr {
      [x, ..] if x > 10 => "starts with large number: " + x.to_string(),
      [.., y] if y < 0 => "ends with negative number: " + y.to_string(),
      [x, .., y] if x == y => "starts and ends with same: " + x.to_string(),
      arr if arr.length() > 0 => "array with " + arr.length().to_string() + " elements",
      _ => "empty array"
    }
  }
  
  assert_eq(describe_array_content([15, 2, 3]), "starts with large number: 15")
  assert_eq(describe_array_content([1, 2, 3, -5]), "ends with negative number: -5")
  assert_eq(describe_array_content([7, 2, 3, 7]), "starts and ends with same: 7")
  assert_eq(describe_array_content([1, 2, 3]), "array with 3 elements")
  assert_eq(describe_array_content([]), "empty array")
}

// Test 7: Pattern Matching on Records
test "pattern matching on records" {
  // Record pattern matching
  type Person = {
    name : String
    age : Int
    email : String
  }
  
  let describe_person = fn(person : Person) {
    match person {
      { name: n, age: a, email: e } if a < 18 => "minor: " + n,
      { name: n, age: a, email: e } if a >= 65 => "senior: " + n,
      { name: n, age: a, email: e } if e.contains("@company.com") => "employee: " + n,
      { name: n, age: a, email: e } => "adult: " + n
    }
  }
  
  let minor = { name: "Alice", age: 16, email: "alice@example.com" }
  let senior = { name: "Bob", age: 70, email: "bob@example.com" }
  let employee = { name: "Charlie", age: 30, email: "charlie@company.com" }
  let adult = { name: "Diana", age: 35, email: "diana@example.com" }
  
  assert_eq(describe_person(minor), "minor: Alice")
  assert_eq(describe_person(senior), "senior: Bob")
  assert_eq(describe_person(employee), "employee: Charlie")
  assert_eq(describe_person(adult), "adult: Diana")
  
  // Partial record matching
  let extract_name = fn(person : Person) {
    match person {
      { name: n, .. } => n
    }
  }
  
  assert_eq(extract_name(minor), "Alice")
  assert_eq(extract_name(senior), "Bob")
  
  // Nested record matching
  type Address = {
    street : String
    city : String
    country : String
  }
  
  type PersonWithAddress = {
    name : String
    age : Int
    address : Address
  }
  
  let describe_location = fn(person : PersonWithAddress) {
    match person {
      { name: n, address: { city: c, country: "USA" } } => n + " lives in " + c + ", USA",
      { name: n, address: { city: c, country: country } } => n + " lives in " + c + ", " + country
    }
  }
  
  let us_person = {
    name: "Eve",
    age: 25,
    address: { street: "123 Main St", city: "New York", country: "USA" }
  }
  
  let intl_person = {
    name: "Frank",
    age: 30,
    address: { street: "456 Oak Ave", city: "London", country: "UK" }
  }
  
  assert_eq(describe_location(us_person), "Eve lives in New York, USA")
  assert_eq(describe_location(intl_person), "Frank lives in London, UK")
}

// Test 8: Complex Pattern Matching
test "complex pattern matching scenarios" {
  // Nested enum and tuple matching
  enum Status {
    Pending
    Completed
    Failed(String)
  }
  
  type Task = {
    id : Int
    status : Status
    result : Option[(Int, String)]
  }
  
  let describe_task = fn(task : Task) {
    match task {
      { id: i, status: Status::Pending, result: None } => "task " + i.to_string() + " is pending",
      { id: i, status: Status::Completed, result: Some((code, msg)) } if code == 200 => {
        "task " + i.to_string() + " completed successfully: " + msg
      }
      { id: i, status: Status::Completed, result: Some((code, msg)) } => {
        "task " + i.to_string() + " completed with code " + code.to_string() + ": " + msg
      }
      { id: i, status: Status::Failed(error), result: None } => {
        "task " + i.to_string() + " failed: " + error
      }
      { id: i, status: s, result: r } => {
        "task " + i.to_string() + " has unknown status"
      }
    }
  }
  
  let pending_task = { id: 1, status: Status::Pending, result: None }
  let success_task = { id: 2, status: Status::Completed, result: Some((200, "OK")) }
  let error_task = { id: 3, status: Status::Completed, result: Some((404, "Not Found")) }
  let failed_task = { id: 4, status: Status::Failed("Network error"), result: None }
  
  assert_eq(describe_task(pending_task), "task 1 is pending")
  assert_eq(describe_task(success_task), "task 2 completed successfully: OK")
  assert_eq(describe_task(error_task), "task 3 completed with code 404: Not Found")
  assert_eq(describe_task(failed_task), "task 4 failed: Network error")
  
  // Pattern matching with complex guards
  let complex_match = fn(value : (Option[Int], Result[String, Int])) {
    match value {
      (Some(x), Ok(s)) if x > 10 && s.length() > 5 => {
        "large number and long string: " + x.to_string() + ", " + s
      }
      (Some(x), Ok(s)) if x < 0 => "negative number with success: " + x.to_string(),
      (Some(x), Err(e)) if e > 100 => "some number with big error code: " + e.to_string(),
      (None, Ok(s)) => "no number with string: " + s,
      (Some(x), Err(e)) => "number " + x.to_string() + " with error " + e.to_string(),
      (None, Err(e)) => "no number with error " + e.to_string()
    }
  }
  
  assert_eq(complex_match((Some(15), Ok("hello world"))), "large number and long string: 15, hello world")
  assert_eq(complex_match((Some(-5), Ok("test"))), "negative number with success: -5")
  assert_eq(complex_match((Some(10), Err(150))), "some number with big error code: 150")
  assert_eq(complex_match((None, Ok("success"))), "no number with string: success")
  assert_eq(complex_match((Some(5), Err(40))), "number 5 with error 40")
  assert_eq(complex_match((None, Err(99))), "no number with error 99")
}

// Test 9: Pattern Matching in Functional Context
test "pattern matching in functional programming" {
  // Using pattern matching with map
  let option_map_with_pattern = fn(opt : Option[Int]) {
    match opt {
      Some(x) if x > 0 => Some(x * 2),
      Some(x) if x < 0 => Some(x * 3),
      Some(0) => Some(1),
      None => None
    }
  }
  
  assert_eq(option_map_with_pattern(Some(5)), Some(10))
  assert_eq(option_map_with_pattern(Some(-3)), Some(-9))
  assert_eq(option_map_with_pattern(Some(0)), Some(1))
  assert_eq(option_map_with_pattern(None), None)
  
  // Pattern matching with filter
  let filter_with_pattern = fn(arr : [(Int, String)]) {
    arr.filter(fn(pair) {
      match pair {
        (x, _) if x > 10 => true,
        (_, s) if s.length() > 5 => true,
        _ => false
      }
    })
  }
  
  let test_pairs = [(5, "short"), (15, "medium"), (8, "very long string")]
  let filtered = filter_with_pattern(test_pairs)
  assert_eq(filtered.length(), 2)
  assert_eq(filtered[0], (15, "medium"))
  assert_eq(filtered[1], (8, "very long string"))
  
  // Pattern matching with reduce
  let reduce_with_pattern = fn(arr : [Result[Int, String]]) {
    arr.reduce(fn(acc, result) {
      match (acc, result) {
        (Ok(a), Ok(b)) => Ok(a + b),
        (Ok(_), Err(e)) => Err(e),
        (Err(e), Ok(_)) => Err(e),
        (Err(e1), Err(e2)) => Err(e1 + "; " + e2)
      }
    }, Ok(0))
  }
  
  let results = [Ok(1), Ok(2), Err("error1"), Ok(3)]
  let reduced = reduce_with_pattern(results)
  match reduced {
    Ok(_) => assert_true(false),
    Err(message) => assert_eq(message, "error1")
  }
  
  let all_success = [Ok(1), Ok(2), Ok(3)]
  let success_reduced = reduce_with_pattern(all_success)
  match success_reduced {
    Ok(sum) => assert_eq(sum, 6),
    Err(_) => assert_true(false)
  }
}

// Test 10: Exhaustiveness and Irrefutable Patterns
test "exhaustiveness and irrefutable patterns" {
  // Exhaustive matching on enum
  enum Day {
    Monday
    Tuesday
    Wednesday
    Thursday
    Friday
    Saturday
    Sunday
  }
  
  let is_weekend = fn(day : Day) {
    match day {
      Day::Saturday => true,
      Day::Sunday => true,
      Day::Monday => false,
      Day::Tuesday => false,
      Day::Wednesday => false,
      Day::Thursday => false,
      Day::Friday => false
    }
  }
  
  assert_true(is_weekend(Day::Saturday))
  assert_true(is_weekend(Day::Sunday))
  assert_false(is_weekend(Day::Monday))
  assert_false(is_weekend(Day::Friday))
  
  // Irrefutable patterns (always match)
  let always_match = fn(value : (Int, String)) {
    match value {
      pair => "pair: " + pair.0.to_string() + ", " + pair.1
    }
  }
  
  assert_eq(always_match((5, "hello")), "pair: 5, hello")
  
  // Wildcard patterns
  let ignore_parts = fn(triple : (Int, String, Bool)) {
    match triple {
      (x, _, _) => "first: " + x.to_string()
    }
  }
  
  assert_eq(ignore_parts((5, "hello", true)), "first: 5")
  
  // Pattern matching with type constraints
  let process_value = fn(value : (Int, Option[String], Result[Bool, String])) {
    match value {
      (x, Some(s), Ok(true)) if x > 0 => "positive with string and true",
      (x, Some(s), Ok(false)) => "with string and false",
      (x, None, Ok(b)) => "no string with " + b.to_string(),
      (_, _, Err(e)) => "error: " + e
    }
  }
  
  assert_eq(process_value((5, Some("hello"), Ok(true))), "positive with string and true")
  assert_eq(process_value((5, Some("hello"), Ok(false))), "with string and false")
  assert_eq(process_value((5, None, Ok(true))), "no string with true")
  assert_eq(process_value((5, Some("hello"), Err("error"))), "error: error")
  
  // Nested pattern matching
  let deep_match = fn(value : Option[Result[(Int, String), String]]) {
    match value {
      Some(Ok((x, s))) if x > 0 => "nested success: " + x.to_string() + ", " + s,
      Some(Ok((x, s))) => "nested success with non-positive: " + x.to_string(),
      Some(Err(e)) => "nested error: " + e,
      None => "none"
    }
  }
  
  assert_eq(deep_match(Some(Ok((5, "hello")))), "nested success: 5, hello")
  assert_eq(deep_match(Some(Ok((-3, "world")))), "nested success with non-positive: -3")
  assert_eq(deep_match(Some(Err("error"))), "nested error: error")
  assert_eq(deep_match(None), "none")
}