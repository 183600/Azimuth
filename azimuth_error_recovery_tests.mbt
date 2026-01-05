// Error Recovery Tests for Azimuth Telemetry System
// This file contains test cases for error recovery functionality

// Test 1: Retry Mechanism
test "retry mechanism with exponential backoff" {
  // Define retry configuration
  type RetryConfig = {
    max_attempts: Int,
    initial_delay: Int,  // in milliseconds
    max_delay: Int,     // in milliseconds
    backoff_multiplier: Float
  }
  
  // Define retry state
  type RetryState = {
    attempt: Int,
    last_attempt_time: Int,
    next_delay: Int
  }
  
  // Define operation result
  enum OperationResult[T] {
    Success(T)
    TransientError(String)
    PermanentError(String)
  }
  
  // Create retry state
  let create_retry_state = fn() {
    {
      attempt: 0,
      last_attempt_time: 0,
      next_delay: 0
    }
  }
  
  // Calculate next delay with exponential backoff
  let calculate_next_delay = fn(config: RetryConfig, state: RetryState) {
    let base_delay = config.initial_delay.to_float() * 
                    (config.backoff_multiplier ^ (state.attempt - 1).to_float())
    let delay = base_delay.to_int()
    
    if delay > config.max_delay {
      config.max_delay
    } else {
      delay
    }
  }
  
  // Check if should retry
  let should_retry = fn(config: RetryConfig, state: RetryState, result: OperationResult[String]) {
    match result {
      OperationResult::Success(_) => false
      OperationResult::PermanentError(_) => false
      OperationResult::TransientError(_) => state.attempt < config.max_attempts
    }
  }
  
  // Update retry state
  let update_retry_state = fn(config: RetryConfig, state: RetryState, current_time: Int) {
    let next_attempt = state.attempt + 1
    let next_delay = calculate_next_delay(config, { state | attempt: next_attempt })
    
    {
      attempt: next_attempt,
      last_attempt_time: current_time,
      next_delay
    }
  }
  
  // Execute operation with retry
  let execute_with_retry = fn(
    config: RetryConfig,
    operation: () -> OperationResult[String],
    current_time: Int,
    state: RetryState
  ) {
    let result = operation()
    
    if should_retry(config, state, result) {
      let updated_state = update_retry_state(config, state, current_time)
      {
        result,
        should_retry: true,
        next_attempt_time: current_time + updated_state.next_delay,
        updated_state
      }
    } else {
      {
        result,
        should_retry: false,
        next_attempt_time: 0,
        updated_state: state
      }
    }
  }
  
  // Create retry configuration
  let config = {
    max_attempts: 5,
    initial_delay: 1000,  // 1 second
    max_delay: 30000,     // 30 seconds
    backoff_multiplier: 2.0
  }
  
  // Test exponential backoff calculation
  let state1 = create_retry_state()
  let state2 = update_retry_state(config, state1, 1640995200)
  let state3 = update_retry_state(config, state2, 1640995200)
  let state4 = update_retry_state(config, state3, 1640995200)
  let state5 = update_retry_state(config, state4, 1640995200)
  
  // Verify exponential backoff
  assert_eq(state2.attempt, 1)
  assert_eq(state2.next_delay, 1000)  // 1000 * 2^0 = 1000
  
  assert_eq(state3.attempt, 2)
  assert_eq(state3.next_delay, 2000)  // 1000 * 2^1 = 2000
  
  assert_eq(state4.attempt, 3)
  assert_eq(state4.next_delay, 4000)  // 1000 * 2^2 = 4000
  
  assert_eq(state5.attempt, 4)
  assert_eq(state5.next_delay, 8000)  // 1000 * 2^3 = 8000
  
  // Test retry logic with transient error
  let mut attempt_count = 0
  let transient_operation = fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      OperationResult::TransientError("Temporary failure")
    } else {
      OperationResult::Success("Operation succeeded")
    }
  }
  
  let initial_state = create_retry_state()
  let result1 = execute_with_retry(config, transient_operation, 1640995200, initial_state)
  
  match result1.result {
    OperationResult::TransientError(_) => assert_true(true)
    _ => assert_true(false)
  }
  assert_true(result1.should_retry)
  assert_eq(result1.next_attempt_time, 1640995200 + 1000)
  
  let result2 = execute_with_retry(config, transient_operation, 1640996200, result1.updated_state)
  
  match result2.result {
    OperationResult::TransientError(_) => assert_true(true)
    _ => assert_true(false)
  }
  assert_true(result2.should_retry)
  assert_eq(result2.next_attempt_time, 1640996200 + 2000)
  
  let result3 = execute_with_retry(config, transient_operation, 1640998200, result2.updated_state)
  
  match result3.result {
    OperationResult::Success(message) => assert_eq(message, "Operation succeeded")
    _ => assert_true(false)
  }
  assert_false(result3.should_retry)
  
  // Test retry logic with permanent error
  let permanent_operation = fn() {
    OperationResult::PermanentError("Permanent failure")
  }
  
  let permanent_result = execute_with_retry(config, permanent_operation, 1640995200, create_retry_state())
  
  match permanent_result.result {
    OperationResult::PermanentError(message) => assert_eq(message, "Permanent failure")
    _ => assert_true(false)
  }
  assert_false(permanent_result.should_retry)
  
  // Test max attempts reached
  let mut max_attempts_count = 0
  let max_attempts_operation = fn() {
    max_attempts_count = max_attempts_count + 1
    OperationResult::TransientError("Always fails")
  }
  
  let mut max_state = create_retry_state()
  let mut max_result = execute_with_retry(config, max_attempts_operation, 1640995200, max_state)
  
  // Retry until max attempts reached
  while max_result.should_retry and max_result.updated_state.attempt < config.max_attempts {
    max_state = max_result.updated_state
    max_result = execute_with_retry(config, max_attempts_operation, 
                                   max_result.next_attempt_time, max_state)
  }
  
  match max_result.result {
    OperationResult::TransientError(_) => assert_true(true)
    _ => assert_true(false)
  }
  assert_false(max_result.should_retry)
  assert_eq(max_result.updated_state.attempt, config.max_attempts)
}

// Test 2: Circuit Breaker with Recovery
test "circuit breaker with automatic recovery" {
  // Define circuit breaker state
  enum CircuitState {
    Closed
    Open
    HalfOpen
  }
  
  // Define circuit breaker configuration
  type CircuitBreakerConfig = {
    failure_threshold: Int,
    success_threshold: Int,
    timeout: Int,  // in milliseconds
    monitoring_period: Int  // in milliseconds
  }
  
  // Define circuit breaker
  type CircuitBreaker = {
    name: String,
    state: CircuitState,
    failure_count: Int,
    success_count: Int,
    last_failure_time: Int,
    last_state_change: Int,
    config: CircuitBreakerConfig
  }
  
  // Create circuit breaker
  let create_circuit_breaker = fn(name: String, config: CircuitBreakerConfig) {
    {
      name,
      state: CircuitState::Closed,
      failure_count: 0,
      success_count: 0,
      last_failure_time: 0,
      last_state_change: 0,
      config
    }
  }
  
  // Record success
  let record_success = fn(breaker: CircuitBreaker, current_time: Int) {
    match breaker.state {
      CircuitState::Closed => {
        { breaker | failure_count: 0 }
      }
      CircuitState::HalfOpen => {
        let new_success_count = breaker.success_count + 1
        
        if new_success_count >= breaker.config.success_threshold {
          // Close circuit after enough successes
          {
            breaker |
            state: CircuitState::Closed,
            failure_count: 0,
            success_count: 0,
            last_state_change: current_time
          }
        } else {
          {
            breaker |
            success_count: new_success_count
          }
        }
      }
      CircuitState::Open => breaker  // Can't succeed when open
    }
  }
  
  // Record failure
  let record_failure = fn(breaker: CircuitBreaker, current_time: Int) {
    let new_failure_count = breaker.failure_count + 1
    
    match breaker.state {
      CircuitState::Closed => {
        if new_failure_count >= breaker.config.failure_threshold {
          // Open circuit after too many failures
          {
            breaker |
            state: CircuitState::Open,
            failure_count: new_failure_count,
            last_failure_time: current_time,
            last_state_change: current_time,
            success_count: 0
          }
        } else {
          {
            breaker |
            failure_count: new_failure_count
          }
        }
      }
      CircuitState::HalfOpen => {
        // Reopen circuit on failure in half-open state
        {
          breaker |
          state: CircuitState::Open,
          failure_count: new_failure_count,
          last_failure_time: current_time,
          last_state_change: current_time,
          success_count: 0
        }
      }
      CircuitState::Open => {
        {
          breaker |
          last_failure_time: current_time
        }
      }
    }
  }
  
  // Check if circuit allows requests
  let allows_request = fn(breaker: CircuitBreaker, current_time: Int) {
    match breaker.state {
      CircuitState::Closed => true
      CircuitState::Open => {
        // Check if timeout has passed for half-open transition
        current_time - breaker.last_failure_time >= breaker.config.timeout
      }
      CircuitState::HalfOpen => true
    }
  }
  
  // Try to transition to half-open
  let try_half_open = fn(breaker: CircuitBreaker, current_time: Int) {
    match breaker.state {
      CircuitState::Open => {
        if current_time - breaker.last_failure_time >= breaker.config.timeout {
          {
            breaker |
            state: CircuitState::HalfOpen,
            success_count: 0,
            last_state_change: current_time
          }
        } else {
          breaker
        }
      }
      _ => breaker
    }
  }
  
  // Create circuit breaker configuration
  let config = {
    failure_threshold: 3,
    success_threshold: 2,
    timeout: 60000,  // 60 seconds
    monitoring_period: 300000  // 5 minutes
  }
  
  // Create circuit breaker
  let breaker = create_circuit_breaker("api-service", config)
  
  // Record failures to trip the circuit
  let breaker1 = record_failure(breaker, 1640995200)
  let breaker2 = record_failure(breaker1, 1640995205)
  let breaker3 = record_failure(breaker2, 1640995210)
  
  // Should now be open
  match breaker3.state {
    CircuitState::Open => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(breaker3.failure_count, 3)
  assert_eq(breaker3.last_failure_time, 1640995210)
  
  // Should not allow requests when open
  assert_false(allows_request(breaker3, 1640995215))
  
  // Try to transition to half-open (not enough time passed)
  let still_open = try_half_open(breaker3, 1640995250)
  match still_open.state {
    CircuitState::Open => assert_true(true)
    _ => assert_true(false)
  }
  
  // Transition to half-open (enough time passed)
  let half_open = try_half_open(breaker3, 1640995270)  // 60 seconds passed
  match half_open.state {
    CircuitState::HalfOpen => assert_true(true)
    _ => assert_true(false)
  }
  
  // Should allow requests when half-open
  assert_true(allows_request(half_open, 1640995275))
  
  // Record successes in half-open to close the circuit
  let closing1 = record_success(half_open, 1640995280)
  match closing1.state {
    CircuitState::HalfOpen => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(closing1.success_count, 1)
  
  let closing2 = record_success(closing1, 1640995285)
  match closing2.state {
    CircuitState::Closed => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(closing2.failure_count, 0)
  assert_eq(closing2.success_count, 0)
  
  // Test failure in half-open reopens circuit
  let reopened = record_failure(half_open, 1640995290)
  match reopened.state {
    CircuitState::Open => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test automatic recovery after monitoring period
  let long_open = {
    reopened |
    last_state_change: 1640990000  // Long time ago
  }
  
  // After monitoring period, reset circuit breaker
  let reset_breaker = if 1640995300 - long_open.last_state_change > long_open.config.monitoring_period {
    create_circuit_breaker(long_open.name, long_open.config)
  } else {
    long_open
  }
  
  match reset_breaker.state {
    CircuitState::Closed => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(reset_breaker.failure_count, 0)
}

// Test 3: Bulkhead Pattern
test "bulkhead pattern for resource isolation" {
  // Define bulkhead configuration
  type BulkheadConfig = {
    max_concurrent: Int,
    max_queue: Int,
    timeout: Int  // in milliseconds
  }
  
  // Define task state
  enum TaskState {
    Pending
    Running
    Completed
    Failed
    Timeout
  }
  
  // Define task
  type Task = {
    id: String,
    state: TaskState,
    submit_time: Int,
    start_time: Option[Int],
    end_time: Option[Int]
  }
  
  // Define bulkhead
  type Bulkhead = {
    name: String,
    config: BulkheadConfig,
    running_tasks: Array[Task],
    queued_tasks: Array[Task],
    completed_tasks: Array[Task],
    rejected_tasks: Array[Task]
  }
  
  // Create bulkhead
  let create_bulkhead = fn(name: String, config: BulkheadConfig) {
    {
      name,
      config,
      running_tasks: [],
      queued_tasks: [],
      completed_tasks: [],
      rejected_tasks: []
    }
  }
  
  // Create task
  let create_task = fn(id: String, submit_time: Int) {
    {
      id,
      state: TaskState::Pending,
      submit_time,
      start_time: None,
      end_time: None
    }
  }
  
  // Submit task to bulkhead
  let submit_task = fn(bulkhead: Bulkhead, task: Task, current_time: Int) {
    if bulkhead.running_tasks.length() < bulkhead.config.max_concurrent {
      // Can run immediately
      let running_task = {
        task |
        state: TaskState::Running,
        start_time: Some(current_time)
      }
      
      let updated_running = bulkhead.running_tasks.push(running_task)
      
      {
        bulkhead |
        running_tasks: updated_running
      }
    } else if bulkhead.queued_tasks.length() < bulkhead.config.max_queue {
      // Queue the task
      let updated_queued = bulkhead.queued_tasks.push(task)
      
      {
        bulkhead |
        queued_tasks: updated_queued
      }
    } else {
      // Reject the task
      let rejected_task = { task | state: TaskState::Failed }
      let updated_rejected = bulkhead.rejected_tasks.push(rejected_task)
      
      {
        bulkhead |
        rejected_tasks: updated_rejected
      }
    }
  }
  
  // Complete task
  let complete_task = fn(bulkhead: Bulkhead, task_id: String, current_time: Int) {
    // Find task in running tasks
    let task_index = bulkhead.running_tasks.index_of(fn(t) { t.id == task_id })
    
    match task_index {
      Some(index) => {
        let task = bulkhead.running_tasks[index]
        let completed_task = {
          task |
          state: TaskState::Completed,
          end_time: Some(current_time)
        }
        
        // Remove from running and add to completed
        let updated_running = bulkhead.running_tasks.filter(fn(t) { t.id != task_id })
        let updated_completed = bulkhead.completed_tasks.push(completed_task)
        
        // Try to start a queued task
        let updated_bulkhead = {
          bulkhead |
          running_tasks: updated_running,
          completed_tasks: updated_completed
        }
        
        if updated_bulkhead.queued_tasks.length() > 0 {
          let next_task = updated_bulkhead.queued_tasks[0]
          let remaining_queue = updated_bulkhead.queued_tasks.slice(1, updated_bulkhead.queued_tasks.length())
          
          let running_next_task = {
            next_task |
            state: TaskState::Running,
            start_time: Some(current_time)
          }
          
          let updated_running = updated_bulkhead.running_tasks.push(running_next_task)
          
          {
            updated_bulkhead |
            running_tasks: updated_running,
            queued_tasks: remaining_queue
          }
        } else {
          updated_bulkhead
        }
      }
      None => bulkhead
    }
  }
  
  // Check for timed out tasks
  let check_timeouts = fn(bulkhead: Bulkhead, current_time: Int) {
    let mut updated_bulkhead = bulkhead
    
    // Check running tasks for timeouts
    let mut non_timeout_tasks = []
    let mut timeout_tasks = []
    
    for task in bulkhead.running_tasks {
      match task.start_time {
        Some(start_time) => {
          if current_time - start_time > bulkhead.config.timeout {
            let timeout_task = {
              task |
              state: TaskState::Timeout,
              end_time: Some(current_time)
            }
            timeout_tasks = timeout_tasks.push(timeout_task)
          } else {
            non_timeout_tasks = non_timeout_tasks.push(task)
          }
        }
        None => non_timeout_tasks = non_timeout_tasks.push(task)
      }
    }
    
    if timeout_tasks.length() > 0 {
      updated_bulkhead = {
        updated_bulkhead |
        running_tasks: non_timeout_tasks,
        completed_tasks: updated_bulkhead.completed_tasks.concat(timeout_tasks)
      }
      
      // Try to start queued tasks
      while updated_bulkhead.queued_tasks.length() > 0 and 
            updated_bulkhead.running_tasks.length() < updated_bulkhead.config.max_concurrent {
        let next_task = updated_bulkhead.queued_tasks[0]
        let remaining_queue = updated_bulkhead.queued_tasks.slice(1, updated_bulkhead.queued_tasks.length())
        
        let running_next_task = {
          next_task |
          state: TaskState::Running,
          start_time: Some(current_time)
        }
        
        let updated_running = updated_bulkhead.running_tasks.push(running_next_task)
        
        updated_bulkhead = {
          updated_bulkhead |
          running_tasks: updated_running,
          queued_tasks: remaining_queue
        }
      }
    }
    
    updated_bulkhead
  }
  
  // Create bulkhead configuration
  let config = {
    max_concurrent: 2,
    max_queue: 3,
    timeout: 5000  // 5 seconds
  }
  
  // Create bulkhead
  let bulkhead = create_bulkhead("api-bulkhead", config)
  
  // Create tasks
  let task1 = create_task("task-1", 1640995200)
  let task2 = create_task("task-2", 1640995205)
  let task3 = create_task("task-3", 1640995210)
  let task4 = create_task("task-4", 1640995215)
  let task5 = create_task("task-5", 1640995220)
  let task6 = create_task("task-6", 1640995225)  // Should be rejected
  
  // Submit tasks
  let bulkhead1 = submit_task(bulkhead, task1, 1640995200)
  assert_eq(bulkhead1.running_tasks.length(), 1)
  assert_eq(bulkhead1.queued_tasks.length(), 0)
  assert_eq(bulkhead1.rejected_tasks.length(), 0)
  
  let bulkhead2 = submit_task(bulkhead1, task2, 1640995205)
  assert_eq(bulkhead2.running_tasks.length(), 2)
  assert_eq(bulkhead2.queued_tasks.length(), 0)
  assert_eq(bulkhead2.rejected_tasks.length(), 0)
  
  let bulkhead3 = submit_task(bulkhead2, task3, 1640995210)
  assert_eq(bulkhead3.running_tasks.length(), 2)
  assert_eq(bulkhead3.queued_tasks.length(), 1)
  assert_eq(bulkhead3.rejected_tasks.length(), 0)
  
  let bulkhead4 = submit_task(bulkhead3, task4, 1640995215)
  assert_eq(bulkhead4.running_tasks.length(), 2)
  assert_eq(bulkhead4.queued_tasks.length(), 2)
  assert_eq(bulkhead4.rejected_tasks.length(), 0)
  
  let bulkhead5 = submit_task(bulkhead4, task5, 1640995220)
  assert_eq(bulkhead5.running_tasks.length(), 2)
  assert_eq(bulkhead5.queued_tasks.length(), 3)
  assert_eq(bulkhead5.rejected_tasks.length(), 0)
  
  let bulkhead6 = submit_task(bulkhead5, task6, 1640995225)
  assert_eq(bulkhead6.running_tasks.length(), 2)
  assert_eq(bulkhead6.queued_tasks.length(), 3)
  assert_eq(bulkhead6.rejected_tasks.length(), 1)
  
  // Complete task1
  let bulkhead7 = complete_task(bulkhead6, "task-1", 1640995230)
  assert_eq(bulkhead7.running_tasks.length(), 2)  // task2 and task3 (from queue)
  assert_eq(bulkhead7.queued_tasks.length(), 2)
  assert_eq(bulkhead7.completed_tasks.length(), 1)
  
  // Complete task2
  let bulkhead8 = complete_task(bulkhead7, "task-2", 1640995235)
  assert_eq(bulkhead8.running_tasks.length(), 2)  // task3 and task4 (from queue)
  assert_eq(bulkhead8.queued_tasks.length(), 1)
  assert_eq(bulkhead8.completed_tasks.length(), 2)
  
  // Test timeout detection
  let timeout_bulkhead = check_timeouts(bulkhead8, 1640995300)  // 5+ seconds after task3 started
  
  // task3 should have timed out if it started at 1640995230
  assert_eq(timeout_bulkhead.completed_tasks.length(), 3)  // task1, task2, and task3 (timeout)
  
  // Test queue processing after timeout
  assert_eq(timeout_bulkhead.running_tasks.length(), 2)  // task4 and task5 (from queue)
  assert_eq(timeout_bulkhead.queued_tasks.length(), 0)
}

// Test 4: Timeout and Deadline Management
test "timeout and deadline management" {
  // Define timeout configuration
  type TimeoutConfig = {
    default_timeout: Int,  // in milliseconds
    max_timeout: Int,      // in milliseconds
    min_timeout: Int       // in milliseconds
  }
  
  // Define operation
  type Operation = {
    id: String,
    name: String,
    timeout: Int,
    deadline: Int
  }
  
  // Define operation state
  enum OperationState {
    Pending
    Running
    Completed
    Failed
    Timeout
    Cancelled
  }
  
  // Define execution context
  type ExecutionContext = {
    operation: Operation,
    state: OperationState,
    start_time: Option[Int],
    end_time: Option[Int],
    remaining_time: Int
  }
  
  // Create operation
  let create_operation = fn(id: String, name: String, timeout: Int, deadline: Int) {
    {
      id,
      name,
      timeout,
      deadline
    }
  }
  
  // Create execution context
  let create_context = fn(operation: Operation, current_time: Int) {
    let remaining_time = deadline - current_time
    
    {
      operation,
      state: OperationState::Pending,
      start_time: None,
      end_time: None,
      remaining_time
    }
  }
  
  // Start operation
  let start_operation = fn(context: ExecutionContext, current_time: Int) {
    if context.remaining_time <= 0 {
      {
        context |
        state: OperationState::Failed,
        end_time: Some(current_time)
      }
    } else {
      let effective_timeout = if context.operation.timeout < context.remaining_time {
        context.operation.timeout
      } else {
        context.remaining_time
      }
      
      {
        context |
        state: OperationState::Running,
        start_time: Some(current_time),
        remaining_time: effective_timeout
      }
    }
  }
  
  // Complete operation
  let complete_operation = fn(context: ExecutionContext, current_time: Int, success: Bool) {
    let new_state = if success {
      OperationState::Completed
    } else {
      OperationState::Failed
    }
    
    {
      context |
      state: new_state,
      end_time: Some(current_time),
      remaining_time: 0
    }
  }
  
  // Check for timeout
  let check_timeout = fn(context: ExecutionContext, current_time: Int) {
    match context.state {
      OperationState::Running => {
        match context.start_time {
          Some(start_time) => {
            let elapsed = current_time - start_time
            
            if elapsed >= context.remaining_time {
              {
                context |
                state: OperationState::Timeout,
                end_time: Some(current_time),
                remaining_time: 0
              }
            } else {
              {
                context |
                remaining_time: context.remaining_time - elapsed
              }
            }
          }
          None => context
        }
      }
      _ => context
    }
  }
  
  // Cancel operation
  let cancel_operation = fn(context: ExecutionContext, current_time: Int) {
    {
      context |
      state: OperationState::Cancelled,
      end_time: Some(current_time),
      remaining_time: 0
    }
  }
  
  // Calculate execution time
  let execution_time = fn(context: ExecutionContext) {
    match (context.start_time, context.end_time) {
      (Some(start), Some(end)) => Some(end - start)
      _ => None
    }
  }
  
  // Create operations
  let operation1 = create_operation("op-1", "Quick Operation", 1000, 1640995300)  // 1s timeout
  let operation2 = create_operation("op-2", "Long Operation", 5000, 1640995350)  // 5s timeout
  let operation3 = create_operation("op-3", "Short Deadline", 2000, 1640995220)  // Short deadline
  
  // Create contexts
  let context1 = create_context(operation1, 1640995200)
  let context2 = create_context(operation2, 1640995200)
  let context3 = create_context(operation3, 1640995200)
  
  // Verify initial contexts
  assert_eq(context1.remaining_time, 100)  // 1640995300 - 1640995200 = 100s
  assert_eq(context2.remaining_time, 150)  // 1640995350 - 1640995200 = 150s
  assert_eq(context3.remaining_time, 20)   // 1640995220 - 1640995200 = 20s
  
  // Start operations
  let running1 = start_operation(context1, 1640995200)
  let running2 = start_operation(context2, 1640995200)
  let running3 = start_operation(context3, 1640995200)
  
  // Verify running contexts
  match running1.state {
    OperationState::Running => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(running1.remaining_time, 1000)  // Uses operation timeout (1s)
  
  match running2.state {
    OperationState::Running => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(running2.remaining_time, 5000)  // Uses operation timeout (5s)
  
  match running3.state {
    OperationState::Running => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(running3.remaining_time, 2000)  // Uses operation timeout (2s)
  
  // Complete operation1 successfully
  let completed1 = complete_operation(running1, 164099520500, true)
  match completed1.state {
    OperationState::Completed => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(execution_time(completed1).unwrap(), 500)
  
  // Check for timeout on operation2 (after 6 seconds)
  let timeout2 = check_timeout(running2, 1640995206000)
  match timeout2.state {
    OperationState::Timeout => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(execution_time(timeout2).unwrap(), 6000)
  
  // Cancel operation3
  let cancelled3 = cancel_operation(running3, 1640995210)
  match cancelled3.state {
    OperationState::Cancelled => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test deadline exceeded before start
  let late_context = create_context(operation3, 1640995230)  // After deadline
  let late_started = start_operation(late_context, 1640995230)
  
  match late_started.state {
    OperationState::Failed => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test timeout shorter than deadline
  let short_deadline_op = create_operation("op-4", "Short Timeout", 500, 1640995300)
  let short_context = create_context(short_deadline_op, 1640995200)
  let short_running = start_operation(short_context, 1640995200)
  
  assert_eq(short_running.remaining_time, 500)  // Uses timeout, not deadline
}

// Test 5: Graceful Degradation
test "graceful degradation under failure conditions" {
  // Define service level
  enum ServiceLevel {
    Full
    Degraded
    Minimal
    Unavailable
  }
  
  // Define service metrics
  type ServiceMetrics = {
    success_rate: Float,
    response_time: Int,
    error_rate: Float,
    throughput: Int
  }
  
  // Define service configuration
  type ServiceConfig = {
    name: String,
    full_level_threshold: Float,    // Success rate threshold for full service
    degraded_level_threshold: Float,  // Success rate threshold for degraded service
    minimal_level_threshold: Float,   // Success rate threshold for minimal service
    max_response_time: Int,           // Max acceptable response time
    min_throughput: Int               // Min acceptable throughput
  }
  
  // Define service state
  type ServiceState = {
    config: ServiceConfig,
    current_level: ServiceLevel,
    metrics: ServiceMetrics,
    last_level_change: Int,
    level_stable_duration: Int
  }
  
  // Create service state
  let create_service_state = fn(config: ServiceConfig, current_time: Int) {
    {
      config,
      current_level: ServiceLevel::Full,
      metrics: {
        success_rate: 100.0,
        response_time: 100,
        error_rate: 0.0,
        throughput: 1000
      },
      last_level_change: current_time,
      level_stable_duration: 0
    }
  }
  
  // Update service metrics
  let update_metrics = fn(state: ServiceState, metrics: ServiceMetrics) {
    {
      state |
      metrics
    }
  }
  
  // Determine service level based on metrics
  let determine_service_level = fn(config: ServiceConfig, metrics: ServiceMetrics) {
    if metrics.success_rate >= config.full_level_threshold and
       metrics.response_time <= config.max_response_time and
       metrics.throughput >= config.min_throughput {
      ServiceLevel::Full
    } else if metrics.success_rate >= config.degraded_level_threshold {
      ServiceLevel::Degraded
    } else if metrics.success_rate >= config.minimal_level_threshold {
      ServiceLevel::Minimal
    } else {
      ServiceLevel::Unavailable
    }
  }
  
  // Update service level
  let update_service_level = fn(state: ServiceState, current_time: Int) {
    let new_level = determine_service_level(state.config, state.metrics)
    
    if new_level != state.current_level {
      {
        state |
        current_level: new_level,
        last_level_change: current_time,
        level_stable_duration: 0
      }
    } else {
      {
        state |
        level_stable_duration: state.level_stable_duration + (current_time - state.last_level_change)
      }
    }
  }
  
  // Get service capabilities based on level
  type ServiceCapabilities = {
    read_enabled: Bool,
    write_enabled: Bool,
    search_enabled: Bool,
    analytics_enabled: Bool,
    max_concurrent_requests: Int
  }
  
  let get_capabilities = fn(level: ServiceLevel) {
    match level {
      ServiceLevel::Full => {
        {
          read_enabled: true,
          write_enabled: true,
          search_enabled: true,
          analytics_enabled: true,
          max_concurrent_requests: 1000
        }
      }
      ServiceLevel::Degraded => {
        {
          read_enabled: true,
          write_enabled: true,
          search_enabled: false,
          analytics_enabled: false,
          max_concurrent_requests: 500
        }
      }
      ServiceLevel::Minimal => {
        {
          read_enabled: true,
          write_enabled: false,
          search_enabled: false,
          analytics_enabled: false,
          max_concurrent_requests: 100
        }
      }
      ServiceLevel::Unavailable => {
        {
          read_enabled: false,
          write_enabled: false,
          search_enabled: false,
          analytics_enabled: false,
          max_concurrent_requests: 0
        }
      }
    }
  }
  
  // Create service configuration
  let config = {
    name: "api-service",
    full_level_threshold: 95.0,
    degraded_level_threshold: 80.0,
    minimal_level_threshold: 50.0,
    max_response_time: 500,
    min_throughput: 100
  }
  
  // Create service state
  let state = create_service_state(config, 1640995200)
  
  // Verify initial state
  match state.current_level {
    ServiceLevel::Full => assert_true(true)
    _ => assert_true(false)
  }
  
  let capabilities = get_capabilities(state.current_level)
  assert_true(capabilities.read_enabled)
  assert_true(capabilities.write_enabled)
  assert_true(capabilities.search_enabled)
  assert_true(capabilities.analytics_enabled)
  assert_eq(capabilities.max_concurrent_requests, 1000)
  
  // Degrade service
  let degraded_metrics = {
    success_rate: 85.0,
    response_time: 600,
    error_rate: 15.0,
    throughput: 800
  }
  
  let degraded_state = update_metrics(state, degraded_metrics)
  let updated_state1 = update_service_level(degraded_state, 1640995250)
  
  match updated_state1.current_level {
    ServiceLevel::Degraded => assert_true(true)
    _ => assert_true(false)
  }
  
  let degraded_capabilities = get_capabilities(updated_state1.current_level)
  assert_true(degraded_capabilities.read_enabled)
  assert_true(degraded_capabilities.write_enabled)
  assert_false(degraded_capabilities.search_enabled)
  assert_false(degraded_capabilities.analytics_enabled)
  assert_eq(degraded_capabilities.max_concurrent_requests, 500)
  
  // Further degrade to minimal
  let minimal_metrics = {
    success_rate: 60.0,
    response_time: 1000,
    error_rate: 40.0,
    throughput: 50
  }
  
  let minimal_state = update_metrics(updated_state1, minimal_metrics)
  let updated_state2 = update_service_level(minimal_state, 1640995300)
  
  match updated_state2.current_level {
    ServiceLevel::Minimal => assert_true(true)
    _ => assert_true(false)
  }
  
  let minimal_capabilities = get_capabilities(updated_state2.current_level)
  assert_true(minimal_capabilities.read_enabled)
  assert_false(minimal_capabilities.write_enabled)
  assert_false(minimal_capabilities.search_enabled)
  assert_false(minimal_capabilities.analytics_enabled)
  assert_eq(minimal_capabilities.max_concurrent_requests, 100)
  
  // Service unavailable
  let unavailable_metrics = {
    success_rate: 30.0,
    response_time: 2000,
    error_rate: 70.0,
    throughput: 10
  }
  
  let unavailable_state = update_metrics(updated_state2, unavailable_metrics)
  let updated_state3 = update_service_level(unavailable_state, 1640995350)
  
  match updated_state3.current_level {
    ServiceLevel::Unavailable => assert_true(true)
    _ => assert_true(false)
  }
  
  let unavailable_capabilities = get_capabilities(updated_state3.current_level)
  assert_false(unavailable_capabilities.read_enabled)
  assert_false(unavailable_capabilities.write_enabled)
  assert_false(unavailable_capabilities.search_enabled)
  assert_false(unavailable_capabilities.analytics_enabled)
  assert_eq(unavailable_capabilities.max_concurrent_requests, 0)
  
  // Recover to degraded
  let recovering_metrics = {
    success_rate: 85.0,
    response_time: 400,
    error_rate: 15.0,
    throughput: 200
  }
  
  let recovering_state = update_metrics(updated_state3, recovering_metrics)
  let updated_state4 = update_service_level(recovering_state, 1640995400)
  
  match updated_state4.current_level {
    ServiceLevel::Degraded => assert_true(true)
    _ => assert_true(false)
  }
  
  // Fully recover
  let recovered_metrics = {
    success_rate: 98.0,
    response_time: 200,
    error_rate: 2.0,
    throughput: 1200
  }
  
  let recovered_state = update_metrics(updated_state4, recovered_metrics)
  let updated_state5 = update_service_level(recovered_state, 1640995450)
  
  match updated_state5.current_level {
    ServiceLevel::Full => assert_true(true)
    _ => assert_true(false)
  }
  
  let recovered_capabilities = get_capabilities(updated_state5.current_level)
  assert_true(recovered_capabilities.read_enabled)
  assert_true(recovered_capabilities.write_enabled)
  assert_true(recovered_capabilities.search_enabled)
  assert_true(recovered_capabilities.analytics_enabled)
  assert_eq(recovered_capabilities.max_concurrent_requests, 1000)
}

// Test 6: Health Check and Self-Healing
test "health check and self-healing mechanisms" {
  // Define health status
  enum HealthStatus {
    Healthy
    Warning
    Critical
    Unhealthy
  }
  
  // Define health check
  type HealthCheck = {
    name: String,
    status: HealthStatus,
    message: String,
    last_check: Int,
    check_interval: Int,
    consecutive_failures: Int,
    max_failures: Int
  }
  
  // Define healing action
  enum HealingAction {
    Restart
    ScaleUp
    ScaleDown
    Reconfigure
    Fallback
  }
  
  // Define healing rule
  type HealingRule = {
    name: String,
    condition: HealthStatus,
    min_consecutive_failures: Int,
    action: HealingAction,
    cooldown: Int  // in milliseconds
  }
  
  // Define healing state
  type HealingState = {
    last_action: Option[HealingAction>,
    last_action_time: Int,
    action_in_progress: Bool
  }
  
  // Define service health
  type ServiceHealth = {
    service_name: String,
    checks: Array[HealthCheck],
    healing_rules: Array[HealingRule],
    healing_state: HealingState,
    overall_status: HealthStatus
  }
  
  // Create health check
  let create_health_check = fn(name: String, check_interval: Int, max_failures: Int, current_time: Int) {
    {
      name,
      status: HealthStatus::Healthy,
      message: "OK",
      last_check: current_time,
      check_interval,
      consecutive_failures: 0,
      max_failures
    }
  }
  
  // Create healing rule
  let create_healing_rule = fn(name: String, condition: HealthStatus, min_failures: Int, action: HealingAction, cooldown: Int) {
    {
      name,
      condition,
      min_consecutive_failures: min_failures,
      action,
      cooldown
    }
  }
  
  // Create service health
  let create_service_health = fn(service_name: String, checks: Array[HealthCheck], healing_rules: Array[HealingRule]) {
    {
      service_name,
      checks,
      healing_rules,
      healing_state: {
        last_action: None,
        last_action_time: 0,
        action_in_progress: false
      },
      overall_status: HealthStatus::Healthy
    }
  }
  
  // Update health check
  let update_health_check = fn(check: HealthCheck, status: HealthStatus, message: String, current_time: Int) {
    let new_consecutive_failures = match status {
      HealthStatus::Healthy => 0
      _ => check.consecutive_failures + 1
    }
    
    {
      check |
      status,
      message,
      last_check: current_time,
      consecutive_failures: new_consecutive_failures
    }
  }
  
  // Calculate overall health status
  let calculate_overall_status = fn(checks: Array[HealthCheck]) {
    if checks.length() == 0 {
      HealthStatus::Healthy
    } else {
      let has_critical = checks.any(fn(c) { match c.status { HealthStatus::Critical => true _ => false } })
      let has_unhealthy = checks.any(fn(c) { match c.status { HealthStatus::Unhealthy => true _ => false } })
      let has_warning = checks.any(fn(c) { match c.status { HealthStatus::Warning => true _ => false } })
      
      if has_critical or has_unhealthy {
        HealthStatus::Critical
      } else if has_warning {
        HealthStatus::Warning
      } else {
        HealthStatus::Healthy
      }
    }
  }
  
  // Check if healing action should be triggered
  let should_trigger_healing = fn(health: ServiceHealth, current_time: Int) {
    if health.healing_state.action_in_progress {
      None
    } else {
      // Check if in cooldown period
      let in_cooldown = match health.healing_state.last_action_time {
        0 => false
        time => {
          let applicable_rules = health.healing_rules.filter(fn(rule) {
            match health.healing_state.last_action {
              Some(action) => action == rule.action
              None => false
            }
          })
          
          if applicable_rules.length() > 0 {
            current_time - time < applicable_rules[0].cooldown
          } else {
            false
          }
        }
      }
      
      if in_cooldown {
        None
      } else {
        // Find matching healing rules
        let applicable_rules = health.healing_rules.filter(fn(rule) {
          health.checks.any(fn(check) {
            check.status == rule.condition and 
            check.consecutive_failures >= rule.min_consecutive_failures
          })
        })
        
        if applicable_rules.length() > 0 {
          Some(applicable_rules[0].action)
        } else {
          None
        }
      }
    }
  }
  
  // Apply healing action
  let apply_healing_action = fn(health: ServiceHealth, action: HealingAction, current_time: Int) {
    {
      health |
      healing_state: {
        last_action: Some(action),
        last_action_time: current_time,
        action_in_progress: true
      }
    }
  }
  
  // Complete healing action
  let complete_healing_action = fn(health: ServiceHealth, success: Bool, current_time: Int) {
    let updated_checks = if success {
      // Reset health checks on successful healing
      health.checks.map(fn(check) {
        {
          check |
          status: HealthStatus::Healthy,
          message: "Healed",
          consecutive_failures: 0
        }
      })
    } else {
      health.checks
    }
    
    {
      health |
      checks: updated_checks,
      healing_state: {
        last_action: health.healing_state.last_action,
        last_action_time: health.healing_state.last_action_time,
        action_in_progress: false
      }
    }
  }
  
  // Update service health
  let update_service_health = fn(health: ServiceHealth, current_time: Int) {
    let overall_status = calculate_overall_status(health.checks)
    
    {
      health |
      overall_status
    }
  }
  
  // Create health checks
  let database_check = create_health_check("database", 30000, 3, 1640995200)  // 30s interval, max 3 failures
  let cache_check = create_health_check("cache", 60000, 2, 1640995200)      // 60s interval, max 2 failures
  let api_check = create_health_check("api", 15000, 5, 1640995200)         // 15s interval, max 5 failures
  
  // Create healing rules
  let restart_rule = create_healing_rule("restart", HealthStatus::Unhealthy, 2, HealingAction::Restart, 300000)  // 5 min cooldown
  let scale_rule = create_healing_rule("scale", HealthStatus::Critical, 3, HealingAction::ScaleUp, 600000)     // 10 min cooldown
  
  // Create service health
  let health = create_service_health("api-service", [database_check, cache_check, api_check], [restart_rule, scale_rule])
  
  // Update health checks with failures
  let failed_db = update_health_check(health.checks[0], HealthStatus::Warning, "Slow queries", 1640995230)
  let failed_cache = update_health_check(health.checks[1], HealthStatus::Unhealthy, "Connection timeout", 1640995240)
  let failed_api = update_health_check(health.checks[2], HealthStatus::Healthy, "OK", 1640995250)
  
  let updated_checks = [failed_db, failed_cache, failed_api]
  let health1 = { health | checks: updated_checks }
  let health2 = update_service_health(health1, 1640995250)
  
  // Verify overall status
  match health2.overall_status {
    HealthStatus::Critical => assert_true(true)
    _ => assert_true(false)
  }
  
  // Check if healing should be triggered
  let healing_action = should_trigger_healing(health2, 1640995250)
  
  match healing_action {
    Some(HealingAction::Restart) => assert_true(true)
    _ => assert_true(false)
  }
  
  // Apply healing action
  let healing_health = apply_healing_action(health2, HealingAction::Restart, 1640995250)
  
  match healing_health.healing_state.last_action {
    Some(HealingAction::Restart) => assert_true(true)
    _ => assert_true(false)
  }
  assert_true(healing_health.healing_state.action_in_progress)
  
  // Complete healing action successfully
  let healed_health = complete_healing_action(healing_health, true, 1640995270)
  
  assert_false(healed_health.healing_state.action_in_progress)
  
  // Verify health checks are reset
  for check in healed_health.checks {
    match check.status {
      HealthStatus::Healthy => assert_true(true)
      _ => assert_true(false)
    }
    assert_eq(check.consecutive_failures, 0)
  }
  
  // Test cooldown period
  let failed_again_db = update_health_check(healed_health.checks[0], HealthStatus::Unhealthy, "Connection failed", 1640995300)
  let failed_again_checks = [failed_again_db, healed_health.checks[1], healed_health.checks[2]]
  let health3 = { healed_health | checks: failed_again_checks }
  let health4 = update_service_health(health3, 1640995300)
  
  // Should not trigger healing due to cooldown
  let cooldown_action = should_trigger_healing(health4, 1640995300)
  assert_eq(cooldown_action, None)
  
  // After cooldown period
  let after_cooldown_action = should_trigger_healing(health4, 1640995800)  // 5+ minutes later
  match after_cooldown_action {
    Some(HealingAction::Restart) => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 7: Rate Limiting and Throttling
test "rate limiting and throttling mechanisms" {
  // Define rate limiter type
  enum RateLimiterType {
    TokenBucket
    FixedWindow
    SlidingWindow
    LeakyBucket
  }
  
  // Define rate limiter configuration
  type RateLimiterConfig = {
    limiter_type: RateLimiterType,
    rate: Int,           // requests per second
    burst: Int,          // maximum burst size
    window_size: Int     // in milliseconds (for window-based limiters)
  }
  
  // Define token bucket state
  type TokenBucketState = {
    tokens: Float,
    last_refill: Int
  }
  
  // Define fixed window state
  type FixedWindowState = {
    window_start: Int,
    count: Int
  }
  
  // Define sliding window state
  type SlidingWindowState = {
    requests: Array[Int>  // timestamps of requests
  }
  
  // Define leaky bucket state
  type LeakyBucketState = {
    current_volume: Float,
    last_leak: Int
  }
  
  // Define rate limiter
  type RateLimiter = {
    name: String,
    config: RateLimiterConfig,
    state: RateLimiterState
  }
  
  // Define rate limiter state
  type RateLimiterState = {
    token_bucket: Option[TokenBucketState],
    fixed_window: Option[FixedWindowState],
    sliding_window: Option[SlidingWindowState],
    leaky_bucket: Option[LeakyBucketState]
  }
  
  // Create rate limiter
  let create_rate_limiter = fn(name: String, config: RateLimiterConfig, current_time: Int) {
    let initial_state = match config.limiter_type {
      RateLimiterType::TokenBucket => {
        RateLimiterState {
          token_bucket: Some({
            tokens: config.burst.to_float(),
            last_refill: current_time
          }),
          fixed_window: None,
          sliding_window: None,
          leaky_bucket: None
        }
      }
      RateLimiterType::FixedWindow => {
        RateLimiterState {
          token_bucket: None,
          fixed_window: Some({
            window_start: current_time,
            count: 0
          }),
          sliding_window: None,
          leaky_bucket: None
        }
      }
      RateLimiterType::SlidingWindow => {
        RateLimiterState {
          token_bucket: None,
          fixed_window: None,
          sliding_window: Some({
            requests: []
          }),
          leaky_bucket: None
        }
      }
      RateLimiterType::LeakyBucket => {
        RateLimiterState {
          token_bucket: None,
          fixed_window: None,
          sliding_window: None,
          leaky_bucket: Some({
            current_volume: 0.0,
            last_leak: current_time
          })
        }
      }
    }
    
    {
      name,
      config,
      state: initial_state
    }
  }
  
  // Check if request is allowed with token bucket
  let allow_token_bucket = fn(config: RateLimiterConfig, state: TokenBucketState, current_time: Int) {
    // Refill tokens
    let time_passed = current_time - state.last_refill
    let tokens_to_add = (time_passed.to_float() / 1000.0) * config.rate.to_float()
    let new_tokens = if state.tokens + tokens_to_add > config.burst.to_float() {
      config.burst.to_float()
    } else {
      state.tokens + tokens_to_add
    }
    
    if new_tokens >= 1.0 {
      // Allow request
      (true, {
        tokens: new_tokens - 1.0,
        last_refill: current_time
      })
    } else {
      // Deny request
      (false, {
        tokens: new_tokens,
        last_refill: current_time
      })
    }
  }
  
  // Check if request is allowed with fixed window
  let allow_fixed_window = fn(config: RateLimiterConfig, state: FixedWindowState, current_time: Int) {
    let window_size = config.window_size
    let max_requests = (config.rate * window_size / 1000)
    
    if current_time - state.window_start >= window_size {
      // New window
      (true, {
        window_start: current_time,
        count: 1
      })
    } else if state.count < max_requests {
      // Same window, under limit
      (true, {
        window_start: state.window_start,
        count: state.count + 1
      })
    } else {
      // Same window, over limit
      (false, state)
    }
  }
  
  // Check if request is allowed with sliding window
  let allow_sliding_window = fn(config: RateLimiterConfig, state: SlidingWindowState, current_time: Int) {
    let window_size = config.window_size
    let max_requests = (config.rate * window_size / 1000)
    
    // Remove old requests outside the window
    let valid_requests = state.requests.filter(fn(timestamp) { 
      current_time - timestamp < window_size 
    })
    
    if valid_requests.length() < max_requests {
      // Allow request
      (true, {
        requests: valid_requests.push(current_time)
      })
    } else {
      // Deny request
      (false, {
        requests: valid_requests
      })
    }
  }
  
  // Check if request is allowed with leaky bucket
  let allow_leaky_bucket = fn(config: RateLimiterConfig, state: LeakyBucketState, current_time: Int) {
    // Leak
    let time_passed = current_time - state.last_leak
    let leak_amount = (time_passed.to_float() / 1000.0) * config.rate.to_float()
    let new_volume = if state.current_volume - leak_amount < 0.0 {
      0.0
    } else {
      state.current_volume - leak_amount
    }
    
    if new_volume + 1.0 <= config.burst.to_float() {
      // Allow request
      (true, {
        current_volume: new_volume + 1.0,
        last_leak: current_time
      })
    } else {
      // Deny request
      (false, {
        current_volume: new_volume,
        last_leak: current_time
      })
    }
  }
  
  // Check if request is allowed
  let is_allowed = fn(limiter: RateLimiter, current_time: Int) {
    match limiter.config.limiter_type {
      RateLimiterType::TokenBucket => {
        match limiter.state.token_bucket {
          Some(state) => {
            let (allowed, new_state) = allow_token_bucket(limiter.config, state, current_time)
            (allowed, {
              limiter |
              state: {
                token_bucket: Some(new_state),
                fixed_window: limiter.state.fixed_window,
                sliding_window: limiter.state.sliding_window,
                leaky_bucket: limiter.state.leaky_bucket
              }
            })
          }
          None => (false, limiter)
        }
      }
      RateLimiterType::FixedWindow => {
        match limiter.state.fixed_window {
          Some(state) => {
            let (allowed, new_state) = allow_fixed_window(limiter.config, state, current_time)
            (allowed, {
              limiter |
              state: {
                token_bucket: limiter.state.token_bucket,
                fixed_window: Some(new_state),
                sliding_window: limiter.state.sliding_window,
                leaky_bucket: limiter.state.leaky_bucket
              }
            })
          }
          None => (false, limiter)
        }
      }
      RateLimiterType::SlidingWindow => {
        match limiter.state.sliding_window {
          Some(state) => {
            let (allowed, new_state) = allow_sliding_window(limiter.config, state, current_time)
            (allowed, {
              limiter |
              state: {
                token_bucket: limiter.state.token_bucket,
                fixed_window: limiter.state.fixed_window,
                sliding_window: Some(new_state),
                leaky_bucket: limiter.state.leaky_bucket
              }
            })
          }
          None => (false, limiter)
        }
      }
      RateLimiterType::LeakyBucket => {
        match limiter.state.leaky_bucket {
          Some(state) => {
            let (allowed, new_state) = allow_leaky_bucket(limiter.config, state, current_time)
            (allowed, {
              limiter |
              state: {
                token_bucket: limiter.state.token_bucket,
                fixed_window: limiter.state.fixed_window,
                sliding_window: limiter.state.sliding_window,
                leaky_bucket: Some(new_state)
              }
            })
          }
          None => (false, limiter)
        }
      }
    }
  }
  
  // Test token bucket
  let token_config = {
    limiter_type: RateLimiterType::TokenBucket,
    rate: 10,        // 10 tokens per second
    burst: 20,       // max 20 tokens
    window_size: 0    // not used
  }
  
  let token_limiter = create_rate_limiter("token-bucket", token_config, 1640995200)
  
  // Allow 20 requests immediately (burst)
  let mut token_limiter_state = token_limiter
  let mut allowed_count = 0
  
  for i in 0..25 {
    let (allowed, updated_limiter) = is_allowed(token_limiter_state, 1640995200 + i * 10)
    token_limiter_state = updated_limiter
    
    if allowed {
      allowed_count = allowed_count + 1
    }
  }
  
  assert_eq(allowed_count, 20)  // Only 20 allowed due to burst limit
  
  // Wait for refill
  let (refill_allowed, refill_limiter) = is_allowed(token_limiter_state, 1640995210)  // 10 seconds later
  
  assert_true(refill_allowed)  // Should have refilled tokens
  
  // Test fixed window
  let fixed_config = {
    limiter_type: RateLimiterType::FixedWindow,
    rate: 10,        // 10 requests per second
    burst: 0,        // not used
    window_size: 1000  // 1 second window
  }
  
  let fixed_limiter = create_rate_limiter("fixed-window", fixed_config, 1640995200)
  
  // Allow 10 requests in first window
  let mut fixed_limiter_state = fixed_limiter
  let mut fixed_allowed_count = 0
  
  for i in 0..15 {
    let (allowed, updated_limiter) = is_allowed(fixed_limiter_state, 1640995200 + i * 50)
    fixed_limiter_state = updated_limiter
    
    if allowed {
      fixed_allowed_count = fixed_allowed_count + 1
    }
  }
  
  assert_eq(fixed_allowed_count, 10)  // Only 10 allowed in window
  
  // Next window should reset
  let (next_window_allowed, next_window_limiter) = is_allowed(fixed_limiter_state, 1640995200 + 1000)
  
  assert_true(next_window_allowed)  // Should be allowed in new window
  
  // Test sliding window
  let sliding_config = {
    limiter_type: RateLimiterType::SlidingWindow,
    rate: 10,        // 10 requests per second
    burst: 0,        // not used
    window_size: 1000  // 1 second window
  }
  
  let sliding_limiter = create_rate_limiter("sliding-window", sliding_config, 1640995200)
  
  // Allow 10 requests spread over time
  let mut sliding_limiter_state = sliding_limiter
  let mut sliding_allowed_count = 0
  
  for i in 0..15 {
    let (allowed, updated_limiter) = is_allowed(sliding_limiter_state, 1640995200 + i * 100)
    sliding_limiter_state = updated_limiter
    
    if allowed {
      sliding_allowed_count = sliding_allowed_count + 1
    }
  }
  
  assert_eq(sliding_allowed_count, 10)  // Only 10 allowed in sliding window
  
  // After window slides, should allow more
  let (slide_allowed, slide_limiter) = is_allowed(sliding_limiter_state, 1640995200 + 1100)
  
  assert_true(slide_allowed)  // Should be allowed as window slid
  
  // Test leaky bucket
  let leaky_config = {
    limiter_type: RateLimiterType::LeakyBucket,
    rate: 10,        // 10 requests per second leak rate
    burst: 20,       // max 20 in bucket
    window_size: 0    // not used
  }
  
  let leaky_limiter = create_rate_limiter("leaky-bucket", leaky_config, 1640995200)
  
  // Allow 20 requests immediately (burst)
  let mut leaky_limiter_state = leaky_limiter
  let mut leaky_allowed_count = 0
  
  for i in 0..25 {
    let (allowed, updated_limiter) = is_allowed(leaky_limiter_state, 1640995200 + i * 10)
    leaky_limiter_state = updated_limiter
    
    if allowed {
      leaky_allowed_count = leaky_allowed_count + 1
    }
  }
  
  assert_eq(leaky_allowed_count, 20)  // Only 20 allowed due to burst limit
  
  // Wait for leak
  let (leak_allowed, leak_limiter) = is_allowed(leaky_limiter_state, 1640995210)  // 10 seconds later
  
  assert_true(leak_allowed)  // Should have leaked
}

// Test 8: Dead Letter Queue Handling
test "dead letter queue handling" {
  // Define message
  type Message = {
    id: String,
    payload: String,
    timestamp: Int,
    retry_count: Int,
    max_retries: Int,
    error_message: Option<String>
  }
  
  // Define queue
  type Queue = {
    name: String,
    messages: Array[Message>
  }
  
  // Define dead letter queue
  type DeadLetterQueue = {
    name: String,
    messages: Array[Message>,
    reason: String
  }
  
  // Define message handler result
  enum HandlerResult {
    Success
    TransientError(String)
    PermanentError(String)
  }
  
  // Define handler
  type Handler = {
    name: String,
    process: Message -> HandlerResult
  }
  
  // Create message
  let create_message = fn(id: String, payload: String, timestamp: Int, max_retries: Int) {
    {
      id,
      payload,
      timestamp,
      retry_count: 0,
      max_retries,
      error_message: None
    }
  }
  
  // Create queue
  let create_queue = fn(name: String) {
    { name, messages: [] }
  }
  
  // Create dead letter queue
  let create_dlq = fn(name: String, reason: String) {
    { name, messages: [], reason }
  }
  
  // Add message to queue
  let enqueue = fn(queue: Queue, message: Message) {
    let updated_messages = queue.messages.push(message)
    { queue | messages: updated_messages }
  }
  
  // Add message to dead letter queue
  let enqueue_dlq = fn(dlq: DeadLetterQueue, message: Message) {
    let updated_messages = dlq.messages.push(message)
    { dlq | messages: updated_messages }
  }
  
  // Process message
  let process_message = fn(
    queue: Queue,
    dlq: DeadLetterQueue,
    handler: Handler,
    current_time: Int
  ) {
    if queue.messages.length() == 0 {
      (queue, dlq, None)
    } else {
      let message = queue.messages[0]
      let remaining_messages = queue.messages.slice(1, queue.messages.length())
      
      let result = handler.process(message)
      
      match result {
        HandlerResult::Success => {
          // Remove message from queue
          let updated_queue = { queue | messages: remaining_messages }
          (updated_queue, dlq, Some(message.id))
        }
        HandlerResult::TransientError(error) => {
          // Retry if under max retries
          if message.retry_count < message.max_retries {
            let retry_message = {
              message |
              retry_count: message.retry_count + 1,
              error_message: Some(error)
            }
            
            // Put back in queue
            let updated_queue = { queue | messages: remaining_messages.push(retry_message) }
            (updated_queue, dlq, None)
          } else {
            // Move to dead letter queue
            let dlq_message = {
              message |
              error_message: Some("Max retries exceeded: " + error)
            }
            
            let updated_queue = { queue | messages: remaining_messages }
            let updated_dlq = enqueue_dlq(dlq, dlq_message)
            
            (updated_queue, updated_dlq, None)
          }
        }
        HandlerResult::PermanentError(error) => {
          // Move directly to dead letter queue
          let dlq_message = {
            message |
            error_message: Some(error)
          }
          
          let updated_queue = { queue | messages: remaining_messages }
          let updated_dlq = enqueue_dlq(dlq, dlq_message)
          
          (updated_queue, updated_dlq, None)
        }
      }
    }
  }
  
  // Process all messages
  let process_all = fn(
    queue: Queue,
    dlq: DeadLetterQueue,
    handler: Handler,
    current_time: Int
  ) {
    let mut current_queue = queue
    let mut current_dlq = dlq
    let mut processed_ids = []
    
    while current_queue.messages.length() > 0 {
      let (new_queue, new_dlq, processed_id) = process_message(
        current_queue,
        current_dlq,
        handler,
        current_time
      )
      
      current_queue = new_queue
      current_dlq = new_dlq
      
      match processed_id {
        Some(id) => processed_ids = processed_ids.push(id)
        None => ()
      }
    }
    
    (current_queue, current_dlq, processed_ids)
  }
  
  // Requeue message from dead letter queue
  let requeue_from_dlq = fn(
    queue: Queue,
    dlq: DeadLetterQueue,
    message_id: String,
    reset_retry_count: Bool
  ) {
    let message_index = dlq.messages.index_of(fn(m) { m.id == message_id })
    
    match message_index {
      Some(index) => {
        let message = dlq.messages[index]
        let remaining_dlq_messages = dlq.messages.filter(fn(m) { m.id != message_id })
        
        let requeued_message = if reset_retry_count {
          {
            message |
            retry_count: 0,
            error_message: None
          }
        } else {
          message
        }
        
        let updated_queue = enqueue(queue, requeued_message)
        let updated_dlq = { dlq | messages: remaining_dlq_messages }
        
        (updated_queue, updated_dlq, true)
      }
      None => (queue, dlq, false)
    }
  }
  
  // Create queues
  let main_queue = create_queue("main-queue")
  let dlq = create_dlq("main-dlq", "Processing failures")
  
  // Create messages
  let success_msg = create_message("msg-1", "success payload", 1640995200, 3)
  let transient_msg = create_message("msg-2", "transient payload", 1640995205, 3)
  let permanent_msg = create_message("msg-3", "permanent payload", 1640995210, 3)
  
  // Enqueue messages
  let queue1 = enqueue(main_queue, success_msg)
  let queue2 = enqueue(queue1, transient_msg)
  let queue3 = enqueue(queue2, permanent_msg)
  
  // Create handlers
  let success_handler = {
    name: "success-handler",
    process: fn(msg) {
      if msg.id == "msg-1" {
        HandlerResult::Success
      } else if msg.id == "msg-2" {
        if msg.retry_count < 2 {
          HandlerResult::TransientError("Temporary failure")
        } else {
          HandlerResult::Success
        }
      } else if msg.id == "msg-3" {
        HandlerResult::PermanentError("Permanent failure")
      } else {
        HandlerResult::Success
      }
    }
  }
  
  // Process messages
  let (final_queue, final_dlq, processed_ids) = process_all(queue3, dlq, success_handler, 1640995300)
  
  // Verify results
  assert_eq(processed_ids.length(), 2)  // msg-1 and msg-2 processed successfully
  assert_true(processed_ids.contains("msg-1"))
  assert_true(processed_ids.contains("msg-2"))
  
  assert_eq(final_queue.messages.length(), 0)  // All messages processed
  assert_eq(final_dlq.messages.length(), 1)  // msg-3 in DLQ
  
  match final_dlq.messages[0].error_message {
    Some(error) => assert_eq(error, "Permanent failure")
    None => assert_true(false)
  }
  
  // Test requeue from DLQ
  let (requeued_queue, requeued_dlq, requeued) = requeue_from_dlq(
    final_queue,
    final_dlq,
    "msg-3",
    true
  )
  
  assert_true(requeued)
  assert_eq(requeued_queue.messages.length(), 1)
  assert_eq(requeued_dlq.messages.length(), 0)
  
  match requeued_queue.messages[0].retry_count {
    0 => assert_true(true)
    _ => assert_true(false)
  }
  
  // Process requeued message with new handler
  let fixed_handler = {
    name: "fixed-handler",
    process: fn(msg) {
      if msg.id == "msg-3" {
        HandlerResult::Success  // Fixed the issue
      } else {
        HandlerResult::Success
      }
    }
  }
  
  let (final_queue2, final_dlq2, processed_ids2) = process_all(
    requeued_queue,
    requeued_dlq,
    fixed_handler,
    1640995400
  )
  
  assert_eq(processed_ids2.length(), 1)
  assert_eq(processed_ids2[0], "msg-3")
  assert_eq(final_queue2.messages.length(), 0)
  assert_eq(final_dlq2.messages.length(), 0)
}