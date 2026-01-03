// Azimuth Premium Exception Handling Tests
// 高级异常处理测试用例，专注于遥测系统的错误恢复和容错能力

// 测试 1: 异常类型定义和处理
test "exception types and handling" {
  // 定义遥测异常类型
  enum TelemetryException {
    NetworkException(String, Int)  // 错误消息，错误代码
    SerializationException(String)
    ValidationException(Array[String])  // 验证错误列表
    ResourceExhaustedException(String, Int)  // 资源类型，限制值
    TimeoutException(String, Int)  // 操作名称，超时时间(ms)
    CircuitBreakerOpenException(String)  // 服务名称
    RateLimitExceededException(String, Int, Int)  // 服务名，当前限制，重试时间
    ConfigurationException(String, String)  // 配置键，错误详情
  }
  
  // 创建异常处理器
  let create_exception_handler = fn() {
    let mut exception_count = 0
    let mut exceptions = []
    
    // 处理异常
    let handle_exception = fn(exception: TelemetryException) {
      exception_count = exception_count + 1
      exceptions = exceptions.push(exception)
      
      match exception {
        TelemetryException::NetworkException(msg, code) => {
          // 网络异常处理：记录错误，尝试重试
          println("网络异常: " + msg + " (代码: " + code.to_string() + ")")
          if code >= 500 {
            "server_error"
          } else if code >= 400 {
            "client_error"
          } else {
            "network_error"
          }
        }
        TelemetryException::SerializationException(msg) => {
          // 序列化异常处理：跳过无效数据
          println("序列化异常: " + msg)
          "serialization_error"
        }
        TelemetryException::ValidationException(errors) => {
          // 验证异常处理：记录所有验证错误
          println("验证异常: " + errors.join(", "))
          "validation_error"
        }
        TelemetryException::ResourceExhaustedException(resource, limit) => {
          // 资源耗尽异常处理：释放资源
          println("资源耗尽: " + resource + " (限制: " + limit.to_string() + ")")
          "resource_exhausted"
        }
        TelemetryException::TimeoutException(operation, timeout) => {
          // 超时异常处理：取消操作
          println("操作超时: " + operation + " (" + timeout.to_string() + "ms)")
          "timeout"
        }
        TelemetryException::CircuitBreakerOpenException(service) => {
          // 断路器打开异常处理：快速失败
          println("断路器打开: " + service)
          "circuit_breaker_open"
        }
        TelemetryException::RateLimitExceededException(service, limit, retry_after) => {
          // 速率限制异常处理：延迟重试
          println("速率限制: " + service + " (限制: " + limit.to_string() + ", " + retry_after.to_string() + "s后重试)")
          "rate_limit_exceeded"
        }
        TelemetryException::ConfigurationException(key, detail) => {
          // 配置异常处理：使用默认值
          println("配置错误: " + key + " - " + detail)
          "configuration_error"
        }
      }
    }
    
    // 获取异常统计
    let get_exception_stats = fn() {
      let mut stats = {
        network: 0,
        serialization: 0,
        validation: 0,
        resource_exhausted: 0,
        timeout: 0,
        circuit_breaker_open: 0,
        rate_limit_exceeded: 0,
        configuration: 0
      }
      
      for exception in exceptions {
        match exception {
          TelemetryException::NetworkException(_, _) => stats.network = stats.network + 1
          TelemetryException::SerializationException(_) => stats.serialization = stats.serialization + 1
          TelemetryException::ValidationException(_) => stats.validation = stats.validation + 1
          TelemetryException::ResourceExhaustedException(_, _) => stats.resource_exhausted = stats.resource_exhausted + 1
          TelemetryException::TimeoutException(_, _) => stats.timeout = stats.timeout + 1
          TelemetryException::CircuitBreakerOpenException(_) => stats.circuit_breaker_open = stats.circuit_breaker_open + 1
          TelemetryException::RateLimitExceededException(_, _, _) => stats.rate_limit_exceeded = stats.rate_limit_exceeded + 1
          TelemetryException::ConfigurationException(_, _) => stats.configuration = stats.configuration + 1
        }
      }
      
      stats
    }
    
    {
      handle_exception,
      get_exception_stats,
      fn() { exception_count }
    }
  }
  
  // 测试异常处理
  let handler = create_exception_handler()
  
  // 处理各种异常
  assert_eq(handler.handle_exception(TelemetryException::NetworkException("连接超时", 504)), "server_error")
  assert_eq(handler.handle_exception(TelemetryException::NetworkException("未找到", 404)), "client_error")
  assert_eq(handler.handle_exception(TelemetryException::SerializationException("无效JSON格式")), "serialization_error")
  assert_eq(handler.handle_exception(TelemetryException::ValidationException(["字段缺失", "类型错误"])), "validation_error")
  assert_eq(handler.handle_exception(TelemetryException::ResourceExhaustedException("内存", 1024)), "resource_exhausted")
  assert_eq(handler.handle_exception(TelemetryException::TimeoutException("数据库查询", 5000)), "timeout")
  assert_eq(handler.handle_exception(TelemetryException::CircuitBreakerOpenException("用户服务")), "circuit_breaker_open")
  assert_eq(handler.handle_exception(TelemetryException::RateLimitExceededException("API服务", 100, 60)), "rate_limit_exceeded")
  assert_eq(handler.handle_exception(TelemetryException::ConfigurationException("db.url", "无效的URL格式")), "configuration_error")
  
  // 验证异常统计
  let stats = handler.get_exception_stats()
  assert_eq(stats.network, 2)
  assert_eq(stats.serialization, 1)
  assert_eq(stats.validation, 1)
  assert_eq(stats.resource_exhausted, 1)
  assert_eq(stats.timeout, 1)
  assert_eq(stats.circuit_breaker_open, 1)
  assert_eq(stats.rate_limit_exceeded, 1)
  assert_eq(stats.configuration, 1)
  
  // 验证总异常数
  assert_eq(handler(), 9)
}

// 测试 2: 重试机制
test "retry mechanism" {
  // 定义重试策略
  enum RetryStrategy {
    FixedDelay(Int)  // 固定延迟(ms)
    ExponentialBackoff(Int, Float, Int)  // 初始延迟，倍数，最大延迟
    LinearBackoff(Int, Int)  // 初始延迟，增量
    NoRetry  // 不重试
  }
  
  // 定义重试结果
  type RetryResult[T] = {
    success: Bool,
    result: Option[T],
    attempts: Int,
    total_time: Int
  }
  
  // 创建重试执行器
  let create_retry_executor = fn() {
    let execute_with_retry = fn[T](
      operation: () -> Result[T, String],
      strategy: RetryStrategy,
      max_attempts: Int
    ) {
      let mut attempts = 0
      let mut total_time = 0
      let mut delay = 0
      let mut result = None
      let mut success = false
      
      while attempts < max_attempts && not success {
        attempts = attempts + 1
        
        // 执行操作
        match operation() {
          Ok(value) => {
            result = Some(value)
            success = true
          }
          Err(error) => {
            // 操作失败，计算下次重试延迟
            if attempts < max_attempts {
              match strategy {
                RetryStrategy::FixedDelay(d) => {
                  delay = d
                }
                RetryStrategy::ExponentialBackoff(initial, multiplier, max_delay) => {
                  if attempts == 1 {
                    delay = initial
                  } else {
                    delay = ((delay as Float) * multiplier).to_int().min(max_delay)
                  }
                }
                RetryStrategy::LinearBackoff(initial, increment) => {
                  delay = initial + (attempts - 1) * increment
                }
                RetryStrategy::NoRetry => {
                  break
                }
              }
              
              total_time = total_time + delay
            }
          }
        }
      }
      
      {
        success,
        result,
        attempts,
        total_time
      }
    }
    
    execute_with_retry
  }
  
  // 测试重试机制
  let executor = create_retry_executor()
  
  // 测试成功操作（不需要重试）
  let success_op = fn() { Ok("success") }
  let success_result = executor(success_op, RetryStrategy::FixedDelay(1000), 3)
  
  assert_true(success_result.success)
  assert_eq(success_result.result, Some("success"))
  assert_eq(success_result.attempts, 1)
  assert_eq(success_result.total_time, 0)
  
  // 测试固定延迟重试
  let mut attempt_count = 0
  let fail_twice_op = fn() {
    attempt_count = attempt_count + 1
    if attempt_count <= 2 {
      Err("still failing")
    } else {
      Ok("finally success")
    }
  }
  
  attempt_count = 0  // 重置计数器
  let fixed_delay_result = executor(fail_twice_op, RetryStrategy::FixedDelay(1000), 5)
  
  assert_true(fixed_delay_result.success)
  assert_eq(fixed_delay_result.result, Some("finally success"))
  assert_eq(fixed_delay_result.attempts, 3)
  assert_eq(fixed_delay_result.total_time, 2000)  // 2次重试，每次1000ms
  
  // 测试指数退避重试
  let mut exp_attempt_count = 0
  let fail_thrice_op = fn() {
    exp_attempt_count = exp_attempt_count + 1
    if exp_attempt_count <= 3 {
      Err("still failing")
    } else {
      Ok("finally success")
    }
  }
  
  exp_attempt_count = 0  // 重置计数器
  let exponential_result = executor(fail_thrice_op, RetryStrategy::ExponentialBackoff(100, 2.0, 1000), 5)
  
  assert_true(exponential_result.success)
  assert_eq(exponential_result.result, Some("finally success"))
  assert_eq(exponential_result.attempts, 4)
  assert_eq(exponential_result.total_time, 700)  // 100 + 200 + 400
  
  // 测试线性退避重试
  let mut linear_attempt_count = 0
  let always_fail_op = fn() {
    linear_attempt_count = linear_attempt_count + 1
    Err("always failing")
  }
  
  linear_attempt_count = 0  // 重置计数器
  let linear_result = executor(always_fail_op, RetryStrategy::LinearBackoff(100, 50), 4)
  
  assert_false(linear_result.success)
  assert_eq(linear_result.result, None)
  assert_eq(linear_result.attempts, 4)
  assert_eq(linear_result.total_time, 450)  // 100 + 150 + 200
  
  // 测试不重试策略
  let no_retry_result = executor(always_fail_op, RetryStrategy::NoRetry, 5)
  
  assert_false(no_retry_result.success)
  assert_eq(no_retry_result.result, None)
  assert_eq(no_retry_result.attempts, 1)
  assert_eq(no_retry_result.total_time, 0)
}

// 测试 3: 断路器模式
test "circuit breaker pattern" {
  // 定义断路器状态
  enum CircuitBreakerState {
    Closed    // 关闭状态，正常工作
    Open      // 打开状态，快速失败
    HalfOpen  // 半开状态，尝试恢复
  }
  
  // 定义断路器配置
  type CircuitBreakerConfig = {
    failure_threshold: Int,     // 失败阈值
    success_threshold: Int,     // 成功阈值（半开状态）
    timeout_ms: Int,            // 超时时间（打开状态）
    reset_timeout_ms: Int       // 重置超时（半开状态）
  }
  
  // 创建断路器
  let create_circuit_breaker = fn(config: CircuitBreakerConfig) {
    let mut state = CircuitBreakerState::Closed
    let mut failure_count = 0
    let mut success_count = 0
    let mut last_failure_time = 0
    let mut last_state_change_time = 0
    let mut total_requests = 0
    let mut total_failures = 0
    
    // 执行操作
    let execute = fn[T](operation: () -> Result[T, String], current_time: Int) {
      total_requests = total_requests + 1
      
      // 检查断路器状态
      match state {
        CircuitBreakerState::Open => {
          // 检查是否应该转为半开状态
          if current_time - last_state_change_time >= config.timeout_ms {
            state = CircuitBreakerState::HalfOpen
            last_state_change_time = current_time
            success_count = 0
          } else {
            // 仍然打开，快速失败
            return Err("Circuit breaker is open")
          }
        }
        CircuitBreakerState::HalfOpen => {
          // 检查是否应该重置超时
          if current_time - last_state_change_time >= config.reset_timeout_ms {
            state = CircuitBreakerState::Closed
            failure_count = 0
            last_state_change_time = current_time
          }
        }
        CircuitBreakerState::Closed => {
          // 正常状态，继续执行
        }
      }
      
      // 执行操作
      match operation() {
        Ok(value) => {
          // 操作成功
          match state {
            CircuitBreakerState::Closed => {
              // 关闭状态，重置失败计数
              failure_count = 0
            }
            CircuitBreakerState::HalfOpen => {
              // 半开状态，增加成功计数
              success_count = success_count + 1
              
              // 检查是否应该关闭断路器
              if success_count >= config.success_threshold {
                state = CircuitBreakerState::Closed
                failure_count = 0
                last_state_change_time = current_time
              }
            }
            CircuitBreakerState::Open => {
              // 不应该到达这里
            }
          }
          
          Ok(value)
        }
        Err(error) => {
          // 操作失败
          total_failures = total_failures + 1
          last_failure_time = current_time
          
          match state {
            CircuitBreakerState::Closed => {
              // 关闭状态，增加失败计数
              failure_count = failure_count + 1
              
              // 检查是否应该打开断路器
              if failure_count >= config.failure_threshold {
                state = CircuitBreakerState::Open
                last_state_change_time = current_time
              }
            }
            CircuitBreakerState::HalfOpen => {
              // 半开状态，立即打开断路器
              state = CircuitBreakerState::Open
              last_state_change_time = current_time
            }
            CircuitBreakerState::Open => {
              // 不应该到达这里
            }
          }
          
          Err(error)
        }
      }
    }
    
    // 获取断路器状态
    let get_state = fn() {
      {
        state,
        failure_count,
        success_count,
        last_failure_time,
        total_requests,
        total_failures,
        failure_rate: if total_requests > 0 { (total_failures as Float) / (total_requests as Float) } else { 0.0 }
      }
    }
    
    {
      execute,
      get_state
    }
  }
  
  // 测试断路器
  let config = {
    failure_threshold: 3,
    success_threshold: 2,
    timeout_ms: 5000,
    reset_timeout_ms: 3000
  }
  
  let circuit_breaker = create_circuit_breaker(config)
  let mut current_time = 1000
  
  // 初始状态应该是关闭的
  let initial_state = circuit_breaker.get_state()
  match initial_state.state {
    CircuitBreakerState::Closed => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(initial_state.failure_count, 0)
  
  // 成功操作不应该改变状态
  let success_op = fn() { Ok("success") }
  let result1 = circuit_breaker.execute(success_op, current_time)
  assert_eq(result1, Ok("success"))
  
  let state1 = circuit_breaker.get_state()
  match state1.state {
    CircuitBreakerState::Closed => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(state1.failure_count, 0)
  
  // 失败操作增加失败计数
  let fail_op = fn() { Err("operation failed") }
  
  current_time = current_time + 1000
  let result2 = circuit_breaker.execute(fail_op, current_time)
  assert_eq(result2, Err("operation failed"))
  
  let state2 = circuit_breaker.get_state()
  match state2.state {
    CircuitBreakerState::Closed => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(state2.failure_count, 1)
  
  // 继续失败直到达到阈值
  current_time = current_time + 1000
  let result3 = circuit_breaker.execute(fail_op, current_time)
  assert_eq(result3, Err("operation failed"))
  
  current_time = current_time + 1000
  let result4 = circuit_breaker.execute(fail_op, current_time)
  assert_eq(result4, Err("operation failed"))
  
  // 断路器应该打开
  let state3 = circuit_breaker.get_state()
  match state3.state {
    CircuitBreakerState::Open => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(state3.failure_count, 3)
  
  // 断路器打开时应该快速失败
  current_time = current_time + 1000
  let result5 = circuit_breaker.execute(success_op, current_time)
  assert_eq(result5, Err("Circuit breaker is open"))
  
  // 超时后转为半开状态
  current_time = current_time + 5000  // 超过timeout_ms
  let result6 = circuit_breaker.execute(success_op, current_time)
  assert_eq(result6, Ok("success"))
  
  let state4 = circuit_breaker.get_state()
  match state4.state {
    CircuitBreakerState::HalfOpen => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(state4.success_count, 1)
  
  // 继续成功直到达到成功阈值
  current_time = current_time + 1000
  let result7 = circuit_breaker.execute(success_op, current_time)
  assert_eq(result7, Ok("success"))
  
  // 断路器应该关闭
  let state5 = circuit_breaker.get_state()
  match state5.state {
    CircuitBreakerState::Closed => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(state5.failure_count, 0)
  
  // 验证总体统计
  assert_eq(state5.total_requests, 7)
  assert_eq(state5.total_failures, 3)
  assert_eq(state5.failure_rate, 3.0 / 7.0)
}

// 测试 4: 超时处理
test "timeout handling" {
  // 定义超时处理器
  type TimeoutHandler = {
    default_timeout_ms: Int,
    timeouts: Map[String, Int]  // 操作特定的超时时间
  }
  
  // 创建超时处理器
  let create_timeout_handler = fn(default_timeout_ms: Int) {
    {
      default_timeout_ms,
      timeouts: Map::empty()
    }
  }
  
  // 设置操作特定的超时
  let set_operation_timeout = fn(handler: TimeoutHandler, operation: String, timeout_ms: Int) {
    let _ = Map::insert(handler.timeouts, operation, timeout_ms)
  }
  
  // 获取操作超时时间
  let get_operation_timeout = fn(handler: TimeoutHandler, operation: String) {
    match Map::get(handler.timeouts, operation) {
      Some(timeout) => timeout
      None => handler.default_timeout_ms
    }
  }
  
  // 执行带超时的操作
  let execute_with_timeout = fn[T](
    handler: TimeoutHandler,
    operation: String,
    op: () -> T,
    start_time: Int
  ) {
    let timeout_ms = get_operation_timeout(handler, operation)
    let end_time = start_time + timeout_ms
    
    // 模拟执行操作并检查超时
    let current_time = start_time + 100  // 假设操作需要100ms
    
    if current_time > end_time {
      Err("Operation timed out after " + timeout_ms.to_string() + "ms")
    } else {
      let result = op()
      Ok((result, current_time - start_time))
    }
  }
  
  // 测试超时处理
  let handler = create_timeout_handler(5000)  // 默认5秒超时
  
  // 设置操作特定的超时
  set_operation_timeout(handler, "database.query", 2000)  // 数据库查询2秒超时
  set_operation_timeout(handler, "api.call", 10000)       // API调用10秒超时
  
  // 测试默认超时
  let default_op = fn() { "default operation result" }
  let result1 = execute_with_timeout(handler, "unknown.operation", default_op, 1000)
  match result1 {
    Ok((result, duration)) => {
      assert_eq(result, "default operation result")
      assert_eq(duration, 100)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试操作特定超时
  let db_op = fn() { "database query result" }
  let result2 = execute_with_timeout(handler, "database.query", db_op, 1000)
  match result2 {
    Ok((result, duration)) => {
      assert_eq(result, "database query result")
      assert_eq(duration, 100)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试超时情况
  let slow_op = fn() { "slow operation result" }
  let result3 = execute_with_timeout(handler, "database.query", slow_op, 3000)  // 超过2秒超时
  match result3 {
    Ok(_) => assert_true(false)
    Err(msg) => assert_true(msg.contains("timed out"))
  }
  
  // 测试长超时操作
  let api_op = fn() { "api call result" }
  let result4 = execute_with_timeout(handler, "api.call", api_op, 5000)
  match result4 {
    Ok((result, duration)) => {
      assert_eq(result, "api call result")
      assert_eq(duration, 100)
    }
    Err(_) => assert_true(false)
  }
}

// 测试 5: 异常恢复策略
test "exception recovery strategies" {
  // 定义恢复策略
  enum RecoveryStrategy {
    Retry(Int)  // 重试次数
    Fallback(() -> String)  // 备用操作
    CircuitBreaker  // 断路器
    Ignore  // 忽略错误
    LogAndContinue  // 记录错误并继续
  }
  
  // 定义操作结果
  type OperationResult[T] = {
    success: Bool,
    value: Option[T],
    fallback_used: Bool,
    error_message: Option[String]
  }
  
  // 创建恢复执行器
  let create_recovery_executor = fn() {
    let execute_with_recovery = fn[T](
      operation: () -> Result[T, String],
      strategy: RecoveryStrategy
    ) {
      match strategy {
        RecoveryStrategy::Retry(max_attempts) => {
          let mut attempts = 0
          let mut result = None
          let mut success = false
          let mut error_message = None
          
          while attempts < max_attempts && not success {
            attempts = attempts + 1
            
            match operation() {
              Ok(value) => {
                result = Some(value)
                success = true
              }
              Err(error) => {
                error_message = Some(error)
                if attempts >= max_attempts {
                  break
                }
              }
            }
          }
          
          {
            success,
            result,
            fallback_used: false,
            error_message
          }
        }
        RecoveryStrategy::Fallback(fallback_fn) => {
          match operation() {
            Ok(value) => {
              {
                success: true,
                result: Some(value),
                fallback_used: false,
                error_message: None
              }
            }
            Err(error) => {
              let fallback_value = fallback_fn()
              {
                success: true,
                result: Some(fallback_value),
                fallback_used: true,
                error_message: Some(error)
              }
            }
          }
        }
        RecoveryStrategy::CircuitBreaker => {
          // 简化的断路器实现
          let mut circuit_open = false
          let mut failure_count = 0
          
          if circuit_open {
            {
              success: false,
              result: None,
              fallback_used: false,
              error_message: Some("Circuit breaker is open")
            }
          } else {
            match operation() {
              Ok(value) => {
                failure_count = 0
                {
                  success: true,
                  result: Some(value),
                  fallback_used: false,
                  error_message: None
                }
              }
              Err(error) => {
                failure_count = failure_count + 1
                if failure_count >= 3 {
                  circuit_open = true
                }
                
                {
                  success: false,
                  result: None,
                  fallback_used: false,
                  error_message: Some(error)
                }
              }
            }
          }
        }
        RecoveryStrategy::Ignore => {
          match operation() {
            Ok(value) => {
              {
                success: true,
                result: Some(value),
                fallback_used: false,
                error_message: None
              }
            }
            Err(_) => {
              {
                success: true,
                result: None,
                fallback_used: false,
                error_message: None
              }
            }
          }
        }
        RecoveryStrategy::LogAndContinue => {
          match operation() {
            Ok(value) => {
              {
                success: true,
                result: Some(value),
                fallback_used: false,
                error_message: None
              }
            }
            Err(error) => {
              // 记录错误（模拟）
              println("操作失败，但继续执行: " + error)
              
              {
                success: true,
                result: None,
                fallback_used: false,
                error_message: Some(error)
              }
            }
          }
        }
      }
    }
    
    execute_with_recovery
  }
  
  // 测试恢复策略
  let executor = create_recovery_executor()
  
  // 测试重试策略
  let mut attempt_count = 0
  let fail_twice_op = fn() {
    attempt_count = attempt_count + 1
    if attempt_count <= 2 {
      Err("still failing")
    } else {
      Ok("finally success")
    }
  }
  
  attempt_count = 0  // 重置计数器
  let retry_result = executor(fail_twice_op, RecoveryStrategy::Retry(5))
  
  assert_true(retry_result.success)
  assert_eq(retry_result.result, Some("finally success"))
  assert_false(retry_result.fallback_used)
  assert_eq(retry_result.error_message, None)
  
  // 测试备用策略
  let fail_op = fn() { Err("operation failed") }
  let fallback_result = executor(fail_op, RecoveryStrategy::Fallback(fn() { "fallback value" }))
  
  assert_true(fallback_result.success)
  assert_eq(fallback_result.result, Some("fallback value"))
  assert_true(fallback_result.fallback_used)
  assert_eq(fallback_result.error_message, Some("operation failed"))
  
  // 测试断路器策略
  let circuit_result = executor(fail_op, RecoveryStrategy::CircuitBreaker)
  
  assert_false(circuit_result.success)
  assert_eq(circuit_result.result, None)
  assert_false(circuit_result.fallback_used)
  assert_eq(circuit_result.error_message, Some("operation failed"))
  
  // 测试忽略策略
  let ignore_result = executor(fail_op, RecoveryStrategy::Ignore)
  
  assert_true(ignore_result.success)
  assert_eq(ignore_result.result, None)
  assert_false(ignore_result.fallback_used)
  assert_eq(ignore_result.error_message, None)
  
  // 测试记录并继续策略
  let log_result = executor(fail_op, RecoveryStrategy::LogAndContinue)
  
  assert_true(log_result.success)
  assert_eq(log_result.result, None)
  assert_false(log_result.fallback_used)
  assert_eq(log_result.error_message, Some("operation failed"))
}