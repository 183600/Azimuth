// Azimuth Type System Tests
// This file contains tests for type system features and type safety

// Test 1: Basic Type Operations
test "basic type operations and conversions" {
  // Integer types
  let int_val : Int = 42
  let int_min : Int = -2147483648
  let int_max : Int = 2147483647
  
  assert_eq(int_val, 42)
  assert_eq(int_min, -2147483648)
  assert_eq(int_max, 2147483647)
  
  // Float types
  let float_val : Float = 3.14159
  let float_scientific : Float = 1.23e-4
  let float_negative : Float = -2.71828
  
  assert_eq(float_val, 3.14159)
  assert_eq(float_scientific, 0.000123)
  assert_eq(float_negative, -2.71828)
  
  // Boolean type
  let bool_true : Bool = true
  let bool_false : Bool = false
  
  assert_true(bool_true)
  assert_false(bool_false)
  
  // String type
  let string_val : String = "Hello, MoonBit!"
  let string_empty : String = ""
  let string_unicode : String = "Unicode: αβγ"
  
  assert_eq(string_val, "Hello, MoonBit!")
  assert_eq(string_empty, "")
  assert_eq(string_unicode, "Unicode: αβγ")
  
  // Type inference
  let inferred_int = 100
  let inferred_float = 2.5
  let inferred_string = "inferred"
  let inferred_bool = true
  
  assert_eq(inferred_int, 100)
  assert_eq(inferred_float, 2.5)
  assert_eq(inferred_string, "inferred")
  assert_true(inferred_bool)
}

// Test 2: Option Type Operations
test "option type handling and operations" {
  // Some values
  let some_int : Option[Int] = Some(42)
  let some_string : Option[String] = Some("hello")
  let some_bool : Option[Bool] = Some(true)
  
  // None values
  let none_int : Option[Int] = None
  let none_string : Option[String] = None
  let none_bool : Option[Bool] = None
  
  // Pattern matching on Option
  let unwrap_or_default = fn(opt : Option[Int], default : Int) {
    match opt {
      Some(value) => value,
      None => default
    }
  }
  
  assert_eq(unwrap_or_default(some_int, 0), 42)
  assert_eq(unwrap_or_default(none_int, 0), 0)
  
  // Option map operation
  let option_map = fn(opt : Option[Int], f : (Int) -> String) {
    match opt {
      Some(value) => Some(f(value)),
      None => None
    }
  }
  
  let mapped_int = option_map(some_int, fn(x) { x.to_string() })
  match mapped_int {
    Some(value) => assert_eq(value, "42"),
    None => assert_true(false)
  }
  
  let mapped_none = option_map(none_int, fn(x) { x.to_string() })
  match mapped_none {
    Some(_) => assert_true(false),
    None => assert_true(true)
  }
  
  // Option bind operation
  let option_bind = fn(opt : Option[Int], f : (Int) -> Option[String]) {
    match opt {
      Some(value) => f(value),
      None => None
    }
  }
  
  let safe_divide = fn(a : Int, b : Int) {
    if b != 0 {
      Some((a / b).to_string())
    } else {
      None
    }
  }
  
  let bind_result = option_bind(some_int, fn(x) { safe_divide(x, 2) })
  match bind_result {
    Some(value) => assert_eq(value, "21"),
    None => assert_true(false)
  }
}

// Test 3: Result Type Operations
test "result type handling and operations" {
  // Success values
  let ok_int : Result[Int, String] = Ok(42)
  let ok_string : Result[String, Int] = Ok("success")
  
  // Error values
  let err_string : Result[Int, String] = Err("error occurred")
  let err_int : Result[String, Int] = Err(404)
  
  // Result map operation
  let result_map = fn(result : Result[Int, String], f : (Int) -> String) {
    match result {
      Ok(value) => Ok(f(value)),
      Err(error) => Err(error)
    }
  }
  
  let mapped_ok = result_map(ok_int, fn(x) { x.to_string() })
  match mapped_ok {
    Ok(value) => assert_eq(value, "42"),
    Err(_) => assert_true(false)
  }
  
  let mapped_err = result_map(err_string, fn(x) { x.to_string() })
  match mapped_err {
    Ok(_) => assert_true(false),
    Err(error) => assert_eq(error, "error occurred")
  }
  
  // Result bind operation
  let result_bind = fn(result : Result[Int, String], f : (Int) -> Result[String, String]) {
    match result {
      Ok(value) => f(value),
      Err(error) => Err(error)
    }
  }
  
  let process_value = fn(x : Int) {
    if x > 0 {
      Ok("positive: " + x.to_string())
    } else {
      Err("negative or zero")
    }
  }
  
  let bind_ok = result_bind(ok_int, process_value)
  match bind_ok {
    Ok(value) => assert_eq(value, "positive: 42"),
    Err(_) => assert_true(false)
  }
  
  let bind_err = result_bind(err_string, process_value)
  match bind_err {
    Ok(_) => assert_true(false),
    Err(error) => assert_eq(error, "error occurred")
  }
}

// Test 4: Custom Enum Types
test "custom enum types and pattern matching" {
  // Define custom enum
  enum Color {
    Red
    Green
    Blue
    RGB(Int, Int, Int)
    CMYK(Float, Float, Float, Float)
  }
  
  // Create enum values
  let red = Color::Red
  let green = Color::Green
  let blue = Color::Blue
  let custom_rgb = Color::RGB(255, 128, 0)
  let custom_cmyk = Color::CMYK(0.0, 1.0, 1.0, 0.0)
  
  // Pattern matching on enum
  let color_to_string = fn(color : Color) {
    match color {
      Color::Red => "red",
      Color::Green => "green",
      Color::Blue => "blue",
      Color::RGB(r, g, b) => "rgb(" + r.to_string() + "," + g.to_string() + "," + b.to_string() + ")",
      Color::CMYK(c, m, y, k) => "cmyk(" + c.to_string() + "," + m.to_string() + "," + y.to_string() + "," + k.to_string() + ")"
    }
  }
  
  assert_eq(color_to_string(red), "red")
  assert_eq(color_to_string(green), "green")
  assert_eq(color_to_string(blue), "blue")
  assert_eq(color_to_string(custom_rgb), "rgb(255,128,0)")
  assert_eq(color_to_string(custom_cmyk), "cmyk(0.0,1.0,1.0,0.0)")
  
  // Enum with methods
  let is_primary_color = fn(color : Color) {
    match color {
      Color::Red => true,
      Color::Green => true,
      Color::Blue => true,
      Color::RGB(_, _, _) => false,
      Color::CMYK(_, _, _, _) => false
    }
  }
  
  assert_true(is_primary_color(red))
  assert_true(is_primary_color(green))
  assert_true(is_primary_color(blue))
  assert_false(is_primary_color(custom_rgb))
  assert_false(is_primary_color(custom_cmyk))
}

// Test 5: Tuple Types
test "tuple types and operations" {
  // Pair tuple
  let pair : (Int, String) = (42, "answer")
  let string_pair : (String, String) = ("hello", "world")
  let bool_pair : (Bool, Int) = (true, 100)
  
  // Access tuple elements
  assert_eq(pair.0, 42)
  assert_eq(pair.1, "answer")
  
  assert_eq(string_pair.0, "hello")
  assert_eq(string_pair.1, "world")
  
  assert_true(bool_pair.0)
  assert_eq(bool_pair.1, 100)
  
  // Triple tuple
  let triple : (Int, String, Bool) = (1, "test", true)
  assert_eq(triple.0, 1)
  assert_eq(triple.1, "test")
  assert_true(triple.2)
  
  // Tuple operations
  let swap_pair = fn(p : (Int, String)) {
    (p.1, p.0)
  }
  
  let swapped = swap_pair(pair)
  assert_eq(swapped.0, "answer")
  assert_eq(swapped.1, 42)
  
  // Tuple destructuring in pattern matching
  let process_pair = fn(p : (Int, String)) {
    match p {
      (x, y) => x + y.length()
    }
  }
  
  assert_eq(process_pair(pair), 42 + 6) // "answer" has 6 characters
}

// Test 6: Array and List Types
test "array and list type operations" {
  // Array operations
  let int_array : [Int] = [1, 2, 3, 4, 5]
  let string_array : [String] = ["a", "b", "c"]
  let mixed_array : [(Int, String)] = [(1, "one"), (2, "two")]
  
  assert_eq(int_array.length(), 5)
  assert_eq(int_array[0], 1)
  assert_eq(int_array[4], 5)
  
  assert_eq(string_array.length(), 3)
  assert_eq(string_array[1], "b")
  
  assert_eq(mixed_array.length(), 2)
  assert_eq(mixed_array[0].0, 1)
  assert_eq(mixed_array[0].1, "one")
  
  // Array operations
  let doubled = int_array.map(fn(x) { x * 2 })
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  let evens = int_array.filter(fn(x) { x % 2 == 0 })
  assert_eq(evens, [2, 4])
  
  let sum = int_array.reduce(fn(acc, x) { acc + x }, 0)
  assert_eq(sum, 15)
  
  // Type-safe array operations
  let safe_get = fn(arr : [Int], index : Int) {
    if index >= 0 && index < arr.length() {
      Some(arr[index])
    } else {
      None
    }
  }
  
  let valid_get = safe_get(int_array, 2)
  match valid_get {
    Some(value) => assert_eq(value, 3),
    None => assert_true(false)
  }
  
  let invalid_get = safe_get(int_array, 10)
  match invalid_get {
    Some(_) => assert_true(false),
    None => assert_true(true)
  }
}

// Test 7: Record Types
test "record types and operations" {
  // Define record type
  type Person = {
    name : String
    age : Int
    email : String
    is_active : Bool
  }
  
  // Create record instances
  let person1 : Person = {
    name: "Alice",
    age: 30,
    email: "alice@example.com",
    is_active: true
  }
  
  let person2 : Person = {
    name: "Bob",
    age: 25,
    email: "bob@example.com",
    is_active: false
  }
  
  // Access record fields
  assert_eq(person1.name, "Alice")
  assert_eq(person1.age, 30)
  assert_eq(person1.email, "alice@example.com")
  assert_true(person1.is_active)
  
  assert_eq(person2.name, "Bob")
  assert_eq(person2.age, 25)
  assert_false(person2.is_active)
  
  // Record update (create new record with updated field)
  let updated_person1 = { person1 | age: 31 }
  assert_eq(updated_person1.name, "Alice")
  assert_eq(updated_person1.age, 31)
  assert_eq(updated_person1.email, "alice@example.com")
  assert_true(updated_person1.is_active)
  
  // Original record unchanged
  assert_eq(person1.age, 30)
  
  // Record functions
  let is_adult = fn(person : Person) {
    person.age >= 18
  }
  
  assert_true(is_adult(person1))
  assert_true(is_adult(person2))
  
  let create_person = fn(name : String, age : Int, email : String) {
    {
      name: name,
      age: age,
      email: email,
      is_active: true
    }
  }
  
  let person3 = create_person("Charlie", 35, "charlie@example.com")
  assert_eq(person3.name, "Charlie")
  assert_eq(person3.age, 35)
  assert_true(person3.is_active)
}

// Test 8: Generic Types
test "generic types and polymorphism" {
  // Generic function
  let identity = fn[T](x : T) {
    x
  }
  
  let int_identity = identity[Int](42)
  let string_identity = identity[String]("hello")
  let bool_identity = identity[Bool](true)
  
  assert_eq(int_identity, 42)
  assert_eq(string_identity, "hello")
  assert_true(bool_identity)
  
  // Generic container type
  type Container[T] = {
    value : T
    metadata : String
  }
  
  let int_container : Container[Int] = {
    value: 100,
    metadata: "integer container"
  }
  
  let string_container : Container[String] = {
    value: "generic",
    metadata: "string container"
  }
  
  assert_eq(int_container.value, 100)
  assert_eq(int_container.metadata, "integer container")
  
  assert_eq(string_container.value, "generic")
  assert_eq(string_container.metadata, "string container")
  
  // Generic function with Container
  let extract_value = fn[T](container : Container[T]) {
    container.value
  }
  
  let extracted_int = extract_value(int_container)
  let extracted_string = extract_value(string_container)
  
  assert_eq(extracted_int, 100)
  assert_eq(extracted_string, "generic")
  
  // Generic operations
  let swap_container = fn[T](container : Container[T]) {
    {
      value: container.value,
      metadata: "swapped: " + container.metadata
    }
  }
  
  let swapped_int = swap_container(int_container)
  assert_eq(swapped_int.value, 100)
  assert_eq(swapped_int.metadata, "swapped: integer container")
}

// Test 9: Type Constraints and Bounds
test "type constraints and bounds" {
  // Define interface-like constraints through function signatures
  let comparable_equal = fn[T](a : T, b : T) {
    // This would require T to have equality comparison
    // In MoonBit, this is handled through the type system
    a == b
  }
  
  let string_equal = comparable_equal[String]("hello", "hello")
  let string_not_equal = comparable_equal[String]("hello", "world")
  let int_equal = comparable_equal[Int](42, 42)
  let int_not_equal = comparable_equal[Int](42, 43)
  
  assert_true(string_equal)
  assert_false(string_not_equal)
  assert_true(int_equal)
  assert_false(int_not_equal)
  
  // Numeric operations constraint
  let numeric_add = fn[T](a : T, b : T) {
    // This would require T to support addition
    // Implementation depends on type system capabilities
    a + b
  }
  
  let int_sum = numeric_add[Int](10, 20)
  let float_sum = numeric_add[Float](1.5, 2.5)
  
  assert_eq(int_sum, 30)
  assert_eq(float_sum, 4.0)
  
  // Container operations constraint
  type NumericContainer[T] = {
    values : [T]
    sum : T
  }
  
  let create_int_container = fn(values : [Int]) {
    let total = values.reduce(fn(acc, x) { acc + x }, 0)
    { values: values, sum: total }
  }
  
  let int_container = create_int_container([1, 2, 3, 4, 5])
  assert_eq(int_container.values, [1, 2, 3, 4, 5])
  assert_eq(int_container.sum, 15)
}

// Test 10: Type Safety and Validation
test "type safety and validation" {
  // Type-safe operations
  let safe_divide = fn(a : Int, b : Int) {
    if b != 0 {
      Some(a / b)
    } else {
      None
    }
  }
  
  let safe_sqrt = fn(x : Float) {
    if x >= 0.0 {
      Some(x.sqrt())
    } else {
      None
    }
  }
  
  // Test safe operations
  let divide_result = safe_divide(10, 2)
  match divide_result {
    Some(result) => assert_eq(result, 5),
    None => assert_true(false)
  }
  
  let divide_by_zero = safe_divide(10, 0)
  match divide_by_zero {
    Some(_) => assert_true(false),
    None => assert_true(true)
  }
  
  let sqrt_result = safe_sqrt(16.0)
  match sqrt_result {
    Some(result) => assert_eq(result, 4.0),
    None => assert_true(false)
  }
  
  let sqrt_negative = safe_sqrt(-4.0)
  match sqrt_negative {
    Some(_) => assert_true(false),
    None => assert_true(true)
  }
  
  // Type validation functions
  let validate_email = fn(email : String) {
    if email.contains("@") && email.contains(".") {
      Ok(email)
    } else {
      Err("Invalid email format")
    }
  }
  
  let validate_age = fn(age : Int) {
    if age >= 0 && age <= 150 {
      Ok(age)
    } else {
      Err("Invalid age range")
    }
  }
  
  // Test validation
  let valid_email = validate_email("user@example.com")
  match valid_email {
    Ok(email) => assert_eq(email, "user@example.com"),
    Err(_) => assert_true(false)
  }
  
  let invalid_email = validate_email("invalid-email")
  match invalid_email {
    Ok(_) => assert_true(false),
    Err(error) => assert_eq(error, "Invalid email format")
  }
  
  let valid_age = validate_age(30)
  match valid_age {
    Ok(age) => assert_eq(age, 30),
    Err(_) => assert_true(false)
  }
  
  let invalid_age = validate_age(-5)
  match invalid_age {
    Ok(_) => assert_true(false),
    Err(error) => assert_eq(error, "Invalid age range")
  }
}