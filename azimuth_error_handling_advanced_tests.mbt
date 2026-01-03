// Azimuth Advanced Error Handling Tests
// This file contains comprehensive tests for advanced error handling patterns

// Test 1: Custom Error Types
test "custom error types and handling" {
  // Define custom error types
  enum ErrorType {
    NetworkError(String)
    ValidationError(String)
    DatabaseError(String)
    AuthenticationError(String)
    UnknownError
  }
  
  // Function that returns Result with custom error
  let validate_user = fn(age : Int, email : String) {
    if age < 18 {
      Err(ErrorType::ValidationError("User must be 18 or older"))
    } else if email.contains("@") == false {
      Err(ErrorType::ValidationError("Invalid email format"))
    } else {
      Ok("User validated successfully")
    }
  }
  
  // Test successful validation
  let valid_result = validate_user(25, "user@example.com")
  match valid_result {
    Ok(message) => assert_eq(message, "User validated successfully")
    Err(_) => assert_true(false)
  }
  
  // Test age validation error
  let age_error = validate_user(16, "user@example.com")
  match age_error {
    Ok(_) => assert_true(false)
    Err(ErrorType::ValidationError(msg)) => assert_eq(msg, "User must be 18 or older")
    Err(_) => assert_true(false)
  }
  
  // Test email validation error
  let email_error = validate_user(25, "invalid-email")
  match email_error {
    Ok(_) => assert_true(false)
    Err(ErrorType::ValidationError(msg)) => assert_eq(msg, "Invalid email format")
    Err(_) => assert_true(false)
  }
}

// Test 2: Error Propagation and Chaining
test "error propagation and chaining" {
  enum ProcessingError {
    InputError(String)
    TransformError(String)
    OutputError(String)
  }
  
  // Processing pipeline functions
  let validate_input = fn(input : String) {
    if input.length() == 0 {
      Err(ProcessingError::InputError("Empty input"))
    } else {
      Ok(input)
    }
  }
  
  let transform_data = fn(data : String) {
    if data.length() > 100 {
      Err(ProcessingError::TransformError("Input too long"))
    } else {
      Ok(data.to_uppercase())
    }
  }
  
  let save_output = fn(data : String) {
    if data.contains("INVALID") {
      Err(ProcessingError::OutputError("Invalid data detected"))
    } else {
      Ok("Data saved: " + data)
    }
  }
  
  // Chain operations with error propagation
  let process_data = fn(input : String) {
    match validate_input(input) {
      Ok(validated) => {
        match transform_data(validated) {
          Ok(transformed) => {
            save_output(transformed)
          }
          Err(error) => Err(error)
        }
      }
      Err(error) => Err(error)
    }
  }
  
  // Test successful processing
  let success = process_data("valid data")
  match success {
    Ok(result) => assert_eq(result, "Data saved: VALID DATA")
    Err(_) => assert_true(false)
  }
  
  // Test input error
  let input_err = process_data("")
  match input_err {
    Ok(_) => assert_true(false)
    Err(ProcessingError::InputError(msg)) => assert_eq(msg, "Empty input")
    Err(_) => assert_true(false)
  }
  
  // Test transform error
  let transform_err = process_data("this is a very long input that exceeds the maximum allowed length for processing")
  match transform_err {
    Ok(_) => assert_true(false)
    Err(ProcessingError::TransformError(msg)) => assert_eq(msg, "Input too long")
    Err(_) => assert_true(false)
  }
  
  // Test output error
  let output_err = process_data("invalid data")
  match output_err {
    Ok(_) => assert_true(false)
    Err(ProcessingError::OutputError(msg)) => assert_eq(msg, "Invalid data detected")
    Err(_) => assert_true(false)
  }
}

// Test 3: Error Recovery Strategies
test "error recovery strategies" {
  enum RetryableError {
    TemporaryError(String)
    PermanentError(String)
  }
  
  // Function with retry capability
  let retry_operation = fn(max_attempts : Int, operation : () -> Result[String, RetryableError]) {
    let mut attempts = 0
    let mut last_error = None
    
    while attempts < max_attempts {
      attempts = attempts + 1
      match operation() {
        Ok(result) => return Ok(result)
        Err(error) => {
          last_error = Some(error)
          match error {
            RetryableError::PermanentError(_) => break
            RetryableError::TemporaryError(_) => {
              // Continue retrying
              if attempts >= max_attempts {
                break
              }
            }
          }
        }
      }
    }
    
    match last_error {
      Some(error) => Err(error)
      None => Err(RetryableError::TemporaryError("Unknown error"))
    }
  }
  
  let mut attempt_count = 0
  let flaky_operation = fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Err(RetryableError::TemporaryError("Temporary failure"))
    } else {
      Ok("Success after retries")
    }
  }
  
  // Test successful retry
  attempt_count = 0
  let retry_success = retry_operation(5, flaky_operation)
  match retry_success {
    Ok(result) => assert_eq(result, "Success after retries")
    Err(_) => assert_true(false)
  }
  
  // Test permanent error
  let permanent_error_operation = fn() {
    Err(RetryableError::PermanentError("Permanent failure"))
  }
  
  let permanent_err = retry_operation(3, permanent_error_operation)
  match permanent_err {
    Ok(_) => assert_true(false)
    Err(RetryableError::PermanentError(msg)) => assert_eq(msg, "Permanent failure")
    Err(_) => assert_true(false)
  }
}

// Test 4: Fallback and Default Values
test "fallback and default value strategies" {
  enum FallbackError {
    PrimaryError(String)
    SecondaryError(String)
  }
  
  let primary_operation = fn() {
    Err(FallbackError::PrimaryError("Primary service unavailable"))
  }
  
  let secondary_operation = fn() {
    Err(FallbackError::SecondaryError("Secondary service unavailable"))
  }
  
  let default_operation = fn() {
    Ok("Default value")
  }
  
  // Fallback chain
  let execute_with_fallback = fn() {
    match primary_operation() {
      Ok(result) => Ok(result),
      Err(FallbackError::PrimaryError(_)) => {
        match secondary_operation() {
          Ok(result) => Ok(result),
          Err(FallbackError::SecondaryError(_)) => {
            default_operation()
          }
          Err(_) => Err(FallbackError::PrimaryError("Unexpected error"))
        }
      }
      Err(error) => Err(error)
    }
  }
  
  let fallback_result = execute_with_fallback()
  match fallback_result {
    Ok(result) => assert_eq(result, "Default value")
    Err(_) => assert_true(false)
  }
  
  // Test with default value function
  let with_default = fn(result : Result[String, FallbackError], default : String) {
    match result {
      Ok(value) => value
      Err(_) => default
    }
  }
  
  let error_result = Err(FallbackError::PrimaryError("Some error"))
  let with_default_result = with_default(error_result, "fallback value")
  assert_eq(with_default_result, "fallback value")
  
  let success_result = Ok("success value")
  let with_default_success = with_default(success_result, "fallback value")
  assert_eq(with_default_success, "success value")
}

// Test 5: Error Aggregation
test "error aggregation and collection" {
  enum ValidationError {
    InvalidName(String)
    InvalidAge(String)
    InvalidEmail(String)
  }
  
  let validate_name = fn(name : String) {
    if name.length() < 2 {
      Err(ValidationError::InvalidName("Name too short"))
    } else if name.length() > 50 {
      Err(ValidationError::InvalidName("Name too long"))
    } else {
      Ok(())
    }
  }
  
  let validate_age = fn(age : Int) {
    if age < 0 {
      Err(ValidationError::InvalidAge("Age cannot be negative"))
    } else if age > 150 {
      Err(ValidationError::InvalidAge("Age unrealistic"))
    } else {
      Ok(())
    }
  }
  
  let validate_email = fn(email : String) {
    if !email.contains("@") {
      Err(ValidationError::InvalidEmail("Email missing @"))
    } else if !email.contains(".") {
      Err(ValidationError::InvalidEmail("Email missing domain"))
    } else {
      Ok(())
    }
  }
  
  // Collect all validation errors
  let validate_all = fn(name : String, age : Int, email : String) {
    let mut errors = []
    
    match validate_name(name) {
      Ok(_) => ()
      Err(error) => errors = errors.push(error)
    }
    
    match validate_age(age) {
      Ok(_) => ()
      Err(error) => errors = errors.push(error)
    }
    
    match validate_email(email) {
      Ok(_) => ()
      Err(error) => errors = errors.push(error)
    }
    
    if errors.length() == 0 {
      Ok("All validations passed")
    } else {
      Err(errors)
    }
  }
  
  // Test with multiple errors
  let multiple_errors = validate_all("x", -5, "invalid")
  match multiple_errors {
    Ok(_) => assert_true(false)
    Err(errors) => assert_eq(errors.length(), 3)
  }
  
  // Test with partial errors
  let partial_errors = validate_name("x")
  match partial_errors {
    Ok(_) => assert_true(false)
    Err(ValidationError::InvalidName(_)) => assert_true(true)
  }
  
  // Test successful validation
  let success = validate_all("John Doe", 30, "john@example.com")
  match success {
    Ok(message) => assert_eq(message, "All validations passed")
    Err(_) => assert_true(false)
  }
}

// Test 6: Contextual Error Information
test "contextual error information" {
  type ErrorContext = {
    operation : String
    user_id : String
    timestamp : String
    details : String
  }
  
  type ContextualError = {
    error_type : String
    message : String
    context : ErrorContext
  }
  
  let create_error = fn(error_type : String, message : String, operation : String, user_id : String) {
    ContextualError {
      error_type: error_type,
      message: message,
      context: {
        operation: operation,
        user_id: user_id,
        timestamp: "2023-01-01T00:00:00Z",
        details: "Additional context information"
      }
    }
  }
  
  let process_with_context = fn(user_id : String, data : String) {
    if data.length() == 0 {
      Err(create_error("ValidationError", "Empty data", "process_data", user_id))
    } else if data.length() > 1000 {
      Err(create_error("SizeError", "Data too large", "process_data", user_id))
    } else {
      Ok("Processing successful")
    }
  }
  
  // Test contextual error
  let contextual_err = process_with_context("user123", "")
  match contextual_err {
    Ok(_) => assert_true(false)
    Err(error) => {
      assert_eq(error.error_type, "ValidationError")
      assert_eq(error.message, "Empty data")
      assert_eq(error.context.operation, "process_data")
      assert_eq(error.context.user_id, "user123")
    }
  }
  
  // Test success
  let success = process_with_context("user123", "valid data")
  match success {
    Ok(message) => assert_eq(message, "Processing successful")
    Err(_) => assert_true(false)
  }
}

// Test 7: Error Transformation and Mapping
test "error transformation and mapping" {
  enum DatabaseError {
    ConnectionError(String)
    QueryError(String)
  }
  
  enum BusinessError {
    ServiceUnavailable(String)
    DataInvalid(String)
  }
  
  let map_database_to_business = fn(db_error : DatabaseError) {
    match db_error {
      DatabaseError::ConnectionError(msg) => {
        BusinessError::ServiceUnavailable("Database connection failed: " + msg)
      }
      DatabaseError::QueryError(msg) => {
        BusinessError::DataInvalid("Invalid query: " + msg)
      }
    }
  }
  
  let database_operation = fn() {
    Err(DatabaseError::ConnectionError("Timeout"))
  }
  
  let business_operation = fn() {
    match database_operation() {
      Ok(result) => Ok(result),
      Err(db_error) => Err(map_database_to_business(db_error))
    }
  }
  
  let business_result = business_operation()
  match business_result {
    Ok(_) => assert_true(false)
    Err(BusinessError::ServiceUnavailable(msg)) => {
      assert_eq(msg, "Database connection failed: Timeout")
    }
    Err(_) => assert_true(false)
  }
}

// Test 8: Error Logging and Monitoring
test "error logging and monitoring patterns" {
  type ErrorLog = {
    error_id : String
    timestamp : String
    level : String
    message : String
    stack_trace : String
  }
  
  let mut error_logs = []
  
  let log_error = fn(error_type : String, message : String) {
    let error_log = {
      error_id: "ERR_" + error_logs.length().to_string(),
      timestamp: "2023-01-01T00:00:00Z",
      level: "ERROR",
      message: message,
      stack_trace: "at function_name (file:line)"
    }
    error_logs = error_logs.push(error_log)
    error_log.error_id
  }
  
  let monitored_operation = fn(input : String) {
    if input == "trigger_error" {
      let error_id = log_error("ValidationError", "Invalid input detected")
      Err("Error logged with ID: " + error_id)
    } else {
      Ok("Operation completed successfully")
    }
  }
  
  // Test error logging
  let error_result = monitored_operation("trigger_error")
  match error_result {
    Ok(_) => assert_true(false)
    Err(error_message) => {
      assert_true(error_message.contains("Error logged with ID:"))
      assert_eq(error_logs.length(), 1)
      assert_eq(error_logs[0].message, "Invalid input detected")
    }
  }
  
  // Test successful operation
  let success_result = monitored_operation("valid_input")
  match success_result {
    Ok(message) => assert_eq(message, "Operation completed successfully")
    Err(_) => assert_true(false)
  }
  
  // Verify error log count unchanged
  assert_eq(error_logs.length(), 1)
}

// Test 9: Panic Recovery and Graceful Degradation
test "panic recovery and graceful degradation" {
  let safe_divide = fn(a : Int, b : Int) {
    if b == 0 {
      Err("Division by zero")
    } else {
      Ok(a / b)
    }
  }
  
  let safe_array_access = fn(arr : [Int], index : Int) {
    if index < 0 || index >= arr.length() {
      Err("Index out of bounds")
    } else {
      Ok(arr[index])
    }
  }
  
  let graceful_degradation = fn(a : Int, b : Int, arr : [Int], index : Int) {
    match safe_divide(a, b) {
      Ok(quotient) => {
        match safe_array_access(arr, index) {
          Ok(value) => Ok(quotient + value),
          Err(_) => Ok(quotient) // Fallback to just quotient
        }
      }
      Err(_) => {
        match safe_array_access(arr, index) {
          Ok(value) => Ok(value), // Fallback to just array value
          Err(_) => Ok(0) // Ultimate fallback
        }
      }
    }
  }
  
  // Test complete success
  let complete_success = graceful_degradation(10, 2, [1, 2, 3], 1)
  match complete_success {
    Ok(result) => assert_eq(result, 6) // 10/2 + 2
    Err(_) => assert_true(false)
  }
  
  // Test partial failure (division error)
  let division_error = graceful_degradation(10, 0, [1, 2, 3], 1)
  match division_error {
    Ok(result) => assert_eq(result, 2) // Just array value
    Err(_) => assert_true(false)
  }
  
  // Test partial failure (array error)
  let array_error = graceful_degradation(10, 2, [1, 2, 3], 10)
  match array_error {
    Ok(result) => assert_eq(result, 5) // Just quotient
    Err(_) => assert_true(false)
  }
  
  // Test complete failure
  let complete_failure = graceful_degradation(10, 0, [1, 2, 3], 10)
  match complete_failure {
    Ok(result) => assert_eq(result, 0) // Ultimate fallback
    Err(_) => assert_true(false)
  }
}

// Test 10: Circuit Breaker Pattern
test "circuit breaker pattern implementation" {
  type CircuitState {
    Closed
    Open
    HalfOpen
  }
  
  type CircuitBreaker = {
    state : CircuitState
    failure_count : Int
    failure_threshold : Int
    timeout : Int
    last_failure_time : Int
  }
  
  let create_circuit_breaker = fn(threshold : Int, timeout : Int) {
    CircuitBreaker {
      state: CircuitState::Closed,
      failure_count: 0,
      failure_threshold: threshold,
      timeout: timeout,
      last_failure_time: 0
    }
  }
  
  let call_with_circuit_breaker = fn(breaker : CircuitBreaker, operation : () -> Result[String, String], current_time : Int) {
    match breaker.state {
      CircuitState::Open => {
        if current_time - breaker.last_failure_time > breaker.timeout {
          // Try half-open state
          let new_breaker = { breaker | state: CircuitState::HalfOpen }
          match operation() {
            Ok(result) => (Ok(result), { new_breaker | state: CircuitState::Closed, failure_count: 0 }),
            Err(error) => (Err(error), { new_breaker | state: CircuitState::Open, last_failure_time: current_time })
          }
        } else {
          (Err("Circuit breaker is open"), breaker)
        }
      }
      CircuitState::HalfOpen => {
        match operation() {
          Ok(result) => (Ok(result), { breaker | state: CircuitState::Closed, failure_count: 0 }),
          Err(error) => (Err(error), { breaker | state: CircuitState::Open, last_failure_time: current_time })
        }
      }
      CircuitState::Closed => {
        match operation() {
          Ok(result) => (Ok(result), { breaker | failure_count: 0 }),
          Err(error) => {
            let new_failure_count = breaker.failure_count + 1
            if new_failure_count >= breaker.failure_threshold {
              (Err(error), { breaker | state: CircuitState::Open, failure_count: new_failure_count, last_failure_time: current_time })
            } else {
              (Err(error), { breaker | failure_count: new_failure_count })
            }
          }
        }
      }
    }
  }
  
  let mut breaker = create_circuit_breaker(3, 100)
  let mut current_time = 0
  
  // Test successful operations
  let success_operation = fn() { Ok("Success") }
  let (result1, breaker1) = call_with_circuit_breaker(breaker, success_operation, current_time)
  match result1 {
    Ok(value) => assert_eq(value, "Success")
    Err(_) => assert_true(false)
  }
  
  // Test failures leading to circuit opening
  let fail_operation = fn() { Err("Service failure") }
  let (_, breaker2) = call_with_circuit_breaker(breaker1, fail_operation, current_time)
  let (_, breaker3) = call_with_circuit_breaker(breaker2, fail_operation, current_time)
  let (_, breaker4) = call_with_circuit_breaker(breaker3, fail_operation, current_time)
  
  // Circuit should now be open
  match breaker4.state {
    CircuitState::Open => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test calls to open circuit
  let (result2, _) = call_with_circuit_breaker(breaker4, success_operation, current_time)
  match result2 {
    Ok(_) => assert_true(false)
    Err(message) => assert_eq(message, "Circuit breaker is open")
  }
  
  // Test circuit recovery after timeout
  current_time = 200
  let (result3, breaker5) = call_with_circuit_breaker(breaker4, success_operation, current_time)
  match result3 {
    Ok(value) => assert_eq(value, "Success")
    Err(_) => assert_true(false)
  }
  
  // Circuit should be closed again
  match breaker5.state {
    CircuitState::Closed => assert_true(true)
    _ => assert_true(false)
  }
}