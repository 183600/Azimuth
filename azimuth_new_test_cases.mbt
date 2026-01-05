// Azimuth Telemetry System - New Test Cases
// This file contains comprehensive test cases for various telemetry features

// Test 1: Basic Data Type Conversions
test "basic data type conversions" {
  // Test integer to string conversion
  let int_val = 42
  let int_str = int_val.to_string()
  assert_eq(int_str, "42")
  
  // Test string to integer parsing
  let str_num = "123"
  let parsed_int = str_num.parse_int()
  match parsed_int {
    Some(n) => assert_eq(n, 123)
    None => assert_true(false)
  }
  
  // Test boolean to string conversion
  let bool_val = true
  let bool_str = bool_val.to_string()
  assert_eq(bool_str, "true")
}

// Test 2: Array Operations and Transformations
test "array operations and transformations" {
  // Test array filtering
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let even_numbers = numbers.filter(fn(x) { x % 2 == 0 })
  assert_eq(even_numbers.length(), 5)
  assert_eq(even_numbers[0], 2)
  assert_eq(even_numbers[4], 10)
  
  // Test array mapping
  let doubled = numbers.map(fn(x) { x * 2 })
  assert_eq(doubled.length(), 10)
  assert_eq(doubled[0], 2)
  assert_eq(doubled[9], 20)
  
  // Test array reduction
  let sum = numbers.reduce(fn(acc, x) { acc + x }, 0)
  assert_eq(sum, 55)
}

// Test 3: String Processing Operations
test "string processing operations" {
  // Test string splitting
  let sentence = "hello world moonbit testing"
  let words = sentence.split(" ")
  assert_eq(words.length(), 4)
  assert_eq(words[0], "hello")
  assert_eq(words[3], "testing")
  
  // Test string joining
  let joined = words.join("-")
  assert_eq(joined, "hello-world-moonbit-testing")
  
  // Test string contains
  assert_true(sentence.contains("moonbit"))
  assert_false(sentence.contains("python"))
  
  // Test string substring
  let substring = sentence.substring(6, 11)
  assert_eq(substring, "world")
}

// Test 4: Option Type Handling
test "option type handling" {
  // Test Some value operations
  let some_value = Some(42)
  let doubled = some_value.map(fn(x) { x * 2 })
  match doubled {
    Some(v) => assert_eq(v, 84)
    None => assert_true(false)
  }
  
  // Test None value operations
  let none_value : Int = None
  let mapped_none = none_value.map(fn(x) { x * 2 })
  match mapped_none {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test Option with default
  let with_default = some_value.with_default(0)
  assert_eq(with_default, 42)
  
  let none_with_default = none_value.with_default(100)
  assert_eq(none_with_default, 100)
}

// Test 5: Error Handling Patterns
test "error handling patterns" {
  // Test result type operations
  let success_result = Ok(42)
  let error_result = Err("Something went wrong")
  
  // Test successful result
  match success_result {
    Ok(value) => assert_eq(value, 42)
    Err(_) => assert_true(false)
  }
  
  // Test error result
  match error_result {
    Ok(_) => assert_true(false)
    Err(message) => assert_eq(message, "Something went wrong")
  }
  
  // Test result mapping
  let mapped_success = success_result.map(fn(x) { x * 2 })
  match mapped_success {
    Ok(value) => assert_eq(value, 84)
    Err(_) => assert_true(false)
  }
}

// Test 6: Hash Map Operations
test "hash map operations" {
  // Test map creation and insertion
  let mut map = Map::new()
  map.insert("key1", "value1")
  map.insert("key2", "value2")
  map.insert("key3", "value3")
  
  // Test map retrieval
  match map.get("key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  // Test map contains
  assert_true(map.contains_key("key2"))
  assert_false(map.contains_key("nonexistent"))
  
  // Test map size
  assert_eq(map.size(), 3)
  
  // Test map removal
  map.remove("key1")
  assert_false(map.contains_key("key1"))
  assert_eq(map.size(), 2)
}

// Test 7: Time and Date Operations
test "time and date operations" {
  // Test timestamp creation
  let timestamp = Time::now()
  assert_true(timestamp > 0)
  
  // Test timestamp formatting
  let formatted = Time::format(timestamp, "%Y-%m-%d %H:%M:%S")
  assert_true(formatted.length() > 0)
  
  // Test timestamp parsing
  let parsed = Time::parse(formatted, "%Y-%m-%d %H:%M:%S")
  match parsed {
    Some(t) => assert_true(t > 0)
    None => assert_true(false)
  }
  
  // Test time arithmetic
  let future = timestamp + 3600 // Add 1 hour
  assert_true(future > timestamp)
  
  let past = timestamp - 3600 // Subtract 1 hour
  assert_true(past < timestamp)
}

// Test 8: JSON Serialization and Deserialization
test "json serialization and deserialization" {
  // Test JSON object creation
  let json_obj = Json::object()
  json_obj.set("name", Json::string("Azimuth"))
  json_obj.set("version", Json::string("1.0.0"))
  json_obj.set("active", Json::boolean(true))
  
  // Test JSON serialization
  let json_string = Json::to_string(json_obj)
  assert_true(json_string.contains("Azimuth"))
  assert_true(json_string.contains("1.0.0"))
  assert_true(json_string.contains("true"))
  
  // Test JSON parsing
  let parsed_json = Json::parse(json_string)
  match parsed_json {
    Some(obj) => {
      match obj.get("name") {
        Some(Json::String(name)) => assert_eq(name, "Azimuth")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 9: Regular Expression Operations
test "regular expression operations" {
  // Test regex pattern matching
  let pattern = Regex::new("\\d{4}-\\d{2}-\\d{2}")
  let date_string = "2023-12-25"
  let invalid_date = "not-a-date"
  
  assert_true(Regex::is_match(pattern, date_string))
  assert_false(Regex::is_match(pattern, invalid_date))
  
  // Test regex find all matches
  let text = "Dates: 2023-01-01, 2023-02-14, 2023-12-25"
  let matches = Regex::find_all(pattern, text)
  assert_eq(matches.length(), 3)
  assert_eq(matches[0], "2023-01-01")
  assert_eq(matches[1], "2023-02-14")
  assert_eq(matches[2], "2023-12-25")
  
  // Test regex replacement
  let replaced = Regex::replace(pattern, text, "DATE")
  assert_eq(replaced, "Dates: DATE, DATE, DATE")
}

// Test 10: File System Operations
test "file system operations" {
  // Test file path operations
  let path = "/home/user/documents/test.txt"
  let dir_path = Path::dirname(path)
  let file_name = Path::basename(path)
  let extension = Path::extension(path)
  
  assert_eq(dir_path, "/home/user/documents")
  assert_eq(file_name, "test.txt")
  assert_eq(extension, "txt")
  
  // Test path joining
  let joined = Path::join(dir_path, "new_file.json")
  assert_eq(joined, "/home/user/documents/new_file.json")
  
  // Test path normalization
  let normalized = Path::normalize("/home/user/../admin/./documents/")
  assert_eq(normalized, "/home/admin/documents")
  
  // Test file existence check (assuming this file doesn't exist)
  assert_false(File::exists("/nonexistent/path/file.txt"))
}