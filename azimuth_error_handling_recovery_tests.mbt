// Azimuth Error Handling and Recovery Test Suite
// This file contains test cases for error handling and recovery mechanisms

// Test 1: Basic Result Type Error Handling
test "basic result type error handling" {
  let divide = fn(a: Int, b: Int) -> Result<Int, String> {
    if b == 0 {
      Err("Division by zero")
    } else {
      Ok(a / b)
    }
  }
  
  let success = divide(10, 2)
  match success {
    Ok(result) => assert_eq(result, 5)
    Err(msg) => assert_true(false)
  }
  
  let failure = divide(10, 0)
  match failure {
    Ok(result) => assert_true(false)
    Err(msg) => assert_eq(msg, "Division by zero")
  }
}

// Test 2: Error Propagation
test "error propagation" {
  let parse_int = fn(s: String) -> Result<Int, String> {
    match s {
      "0" => Ok(0)
      "1" => Ok(1)
      "2" => Ok(2)
      "3" => Ok(3)
      "4" => Ok(4)
      "5" => Ok(5)
      "6" => Ok(6)
      "7" => Ok(7)
      "8" => Ok(8)
      "9" => Ok(9)
      "10" => Ok(10)
      _ => Err("Invalid number: " + s)
    }
  }
  
  let safe_divide = fn(a_str: String, b_str: String) -> Result<Int, String> {
    match parse_int(a_str) {
      Ok(a) => {
        match parse_int(b_str) {
          Ok(b) => {
            if b == 0 {
              Err("Division by zero")
            } else {
              Ok(a / b)
            }
          }
          Err(msg) => Err(msg)
        }
      }
      Err(msg) => Err(msg)
    }
  }
  
  let result1 = safe_divide("10", "2")
  match result1 {
    Ok(value) => assert_eq(value, 5)
    Err(_) => assert_true(false)
  }
  
  let result2 = safe_divide("10", "0")
  match result2 {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Division by zero")
  }
  
  let result3 = safe_divide("ten", "2")
  match result3 {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Invalid number: ten")
  }
  
  let result4 = safe_divide("10", "two")
  match result4 {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Invalid number: two")
  }
}

// Test 3: Error Recovery with Default Values
test "error recovery with default values" {
  let safe_parse_int = fn(s: String, default: Int) -> Int {
    match s {
      "0" => 0
      "1" => 1
      "2" => 2
      "3" => 3
      "4" => 4
      "5" => 5
      "6" => 6
      "7" => 7
      "8" => 8
      "9" => 9
      "10" => 10
      _ => default
    }
  }
  
  assert_eq(safe_parse_int("5", 0), 5)
  assert_eq(safe_parse_int("invalid", 0), 0)
  assert_eq(safe_parse_int("unknown", -1), -1)
  
  let safe_index = fn(arr: Array<Int>, index: Int, default: Int) -> Int {
    if index >= 0 && index < arr.length() {
      arr[index]
    } else {
      default
    }
  }
  
  let arr = [10, 20, 30]
  assert_eq(safe_index(arr, 1, 0), 20)
  assert_eq(safe_index(arr, 5, 0), 0)
  assert_eq(safe_index(arr, -1, 0), 0)
}

// Test 4: Retry Mechanism
test "retry mechanism" {
  let mut attempt_count = 0
  let flaky_operation = fn() -> Result<String, String> {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Err("Temporary failure")
    } else {
      Ok("Success after retries")
    }
  }
  
  let retry = fn(operation: () -> Result<String, String>, max_attempts: Int) -> Result<String, String> {
    let mut attempts = 0
    let mut last_error = "Unknown error"
    
    while attempts < max_attempts {
      match operation() {
        Ok(value) => return Ok(value)
        Err(msg) => {
          last_error = msg
          attempts = attempts + 1
        }
      }
    }
    
    Err(last_error)
  }
  
  let result = retry(flaky_operation, 5)
  match result {
    Ok(value) => assert_eq(value, "Success after retries")
    Err(_) => assert_true(false)
  }
  
  assert_eq(attempt_count, 3)
  
  // Reset for next test
  attempt_count = 0
  let always_failing_operation = fn() -> Result<String, String> {
    Err("Always failing")
  }
  
  let result2 = retry(always_failing_operation, 3)
  match result2 {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Always failing")
  }
}

// Test 5: Circuit Breaker Pattern
test "circuit breaker pattern" {
  type CircuitState = { Closed, Open, HalfOpen }
  
  let mut circuit_state = CircuitState::Closed
  let mut failure_count = 0
  let mut last_failure_time = 0
  let failure_threshold = 3
  let timeout = 100  // Simulated time units
  
  let circuit_breaker = fn(operation: () -> Result<String, String>) -> Result<String, String> {
    match circuit_state {
      CircuitState::Closed => {
        match operation() {
          Ok(value) => {
            failure_count = 0
            Ok(value)
          }
          Err(msg) => {
            failure_count = failure_count + 1
            if failure_count >= failure_threshold {
              circuit_state = CircuitState::Open
              last_failure_time = 42  // Simulated current time
            }
            Err(msg)
          }
        }
      }
      CircuitState::Open => {
        // Simulate time check - in real implementation, this would check actual time
        let current_time = 50  // Simulated current time
        if current_time - last_failure_time > timeout {
          circuit_state = CircuitState::HalfOpen
          match operation() {
            Ok(value) => {
              circuit_state = CircuitState::Closed
              failure_count = 0
              Ok(value)
            }
            Err(msg) => {
              circuit_state = CircuitState::Open
              last_failure_time = current_time
              Err(msg)
            }
          }
        } else {
          Err("Circuit breaker is open")
        }
      }
      CircuitState::HalfOpen => {
        match operation() {
          Ok(value) => {
            circuit_state = CircuitState::Closed
            failure_count = 0
            Ok(value)
          }
          Err(msg) => {
            circuit_state = CircuitState::Open
            last_failure_time = 50  // Simulated current time
            Err(msg)
          }
        }
      }
    }
  }
  
  let successful_operation = fn() -> Result<String, String> {
    Ok("Operation successful")
  }
  
  let failing_operation = fn() -> Result<String, String> {
    Err("Operation failed")
  }
  
  // Initially closed, operations should work
  let result1 = circuit_breaker(successful_operation)
  match result1 {
    Ok(value) => assert_eq(value, "Operation successful")
    Err(_) => assert_true(false)
  }
  
  // After failures, circuit should open
  let _ = circuit_breaker(failing_operation)
  let _ = circuit_breaker(failing_operation)
  let result2 = circuit_breaker(failing_operation)
  match result2 {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Operation failed")
  }
  
  // Circuit should be open now
  let result3 = circuit_breaker(successful_operation)
  match result3 {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Circuit breaker is open")
  }
}

// Test 6: Timeout Handling
test "timeout handling" {
  let mut operation_time = 0
  let timed_operation = fn(duration: Int) -> Result<String, String> {
    operation_time = operation_time + 1
    if operation_time > duration {
      Err("Operation timed out")
    } else {
      Ok("Operation completed")
    }
  }
  
  let with_timeout = fn(operation: () -> Result<String, String>, timeout_limit: Int) -> Result<String, String> {
    operation_time = 0  // Reset for each operation
    let mut result = operation()
    if operation_time > timeout_limit {
      Err("Operation exceeded timeout")
    } else {
      result
    }
  }
  
  let fast_operation = fn() -> Result<String, String> {
    timed_operation(5)
  }
  
  let slow_operation = fn() -> Result<String, String> {
    timed_operation(2)
  }
  
  let result1 = with_timeout(fast_operation, 10)
  match result1 {
    Ok(value) => assert_eq(value, "Operation completed")
    Err(_) => assert_true(false)
  }
  
  let result2 = with_timeout(slow_operation, 1)
  match result2 {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Operation exceeded timeout")
  }
}

// Test 7: Fallback Mechanism
test "fallback mechanism" {
  let primary_operation = fn(input: String) -> Result<String, String> {
    match input {
      "valid" => Ok("Primary result")
      _ => Err("Primary operation failed")
    }
  }
  
  let fallback_operation = fn(input: String) -> Result<String, String> {
    match input {
      "fallback" => Ok("Fallback result")
      _ => Err("Fallback operation failed")
    }
  }
  
  let with_fallback = fn(input: String) -> Result<String, String> {
    match primary_operation(input) {
      Ok(value) => Ok(value)
      Err(_) => fallback_operation(input)
    }
  }
  
  let result1 = with_fallback("valid")
  match result1 {
    Ok(value) => assert_eq(value, "Primary result")
    Err(_) => assert_true(false)
  }
  
  let result2 = with_fallback("fallback")
  match result2 {
    Ok(value) => assert_eq(value, "Fallback result")
    Err(_) => assert_true(false)
  }
  
  let result3 = with_fallback("invalid")
  match result3 {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Fallback operation failed")
  }
}

// Test 8: Error Aggregation
test "error aggregation" {
  let validate_field = fn(field: String, value: String) -> Result<String, String> {
    match field {
      "username" => {
        if value.length() >= 3 {
          Ok("Username valid")
        } else {
          Err("Username too short")
        }
      }
      "email" => {
        if value.contains("@") {
          Ok("Email valid")
        } else {
          Err("Invalid email format")
        }
      }
      "age" => {
        let age_value = match value {
          "18" => Some(18)
          "19" => Some(19)
          "20" => Some(20)
          "21" => Some(21)
          "22" => Some(22)
          "23" => Some(23)
          "24" => Some(24)
          "25" => Some(25)
          _ => None
        }
        
        match age_value {
          Some(age) => {
            if age >= 18 {
              Ok("Age valid")
            } else {
              Err("Age too young")
            }
          }
          None => Err("Invalid age format")
        }
      }
      _ => Err("Unknown field")
    }
  }
  
  let validate_all = fn(fields: Array<(String, String)>) -> Result<String, Array<String>> {
    let mut errors = []
    
    for (field, value) in fields {
      match validate_field(field, value) {
        Ok(_) => {}
        Err(msg) => errors = errors.push(field + ": " + msg)
      }
    }
    
    if errors.length() == 0 {
      Ok("All fields valid")
    } else {
      Err(errors)
    }
  }
  
  let valid_fields = [
    ("username", "john"),
    ("email", "john@example.com"),
    ("age", "25")
  ]
  
  let result1 = validate_all(valid_fields)
  match result1 {
    Ok(value) => assert_eq(value, "All fields valid")
    Err(_) => assert_true(false)
  }
  
  let invalid_fields = [
    ("username", "jo"),
    ("email", "johnexample.com"),
    ("age", "17")
  ]
  
  let result2 = validate_all(invalid_fields)
  match result2 {
    Ok(_) => assert_true(false)
    Err(errors) => {
      assert_eq(errors.length(), 3)
      assert_true(errors.contains("username: Username too short"))
      assert_true(errors.contains("email: Invalid email format"))
      assert_true(errors.contains("age: Invalid age format"))
    }
  }
}