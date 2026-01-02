// Azimuth Fault Recovery Mechanism Test Suite
// 故障恢复机制测试用例

// 测试1: 断路器模式
test "断路器模式测试" {
  // 断路器状态
  enum CircuitBreakerState {
    Closed    // 正常状态
    Open      // 断路状态
    HalfOpen  // 半开状态
  }
  
  // 断路器配置
  struct CircuitBreakerConfig {
    failure_threshold : Int      // 失败阈值
    recovery_timeout : Int64    // 恢复超时（纳秒）
    success_threshold : Int      // 成功阈值（半开状态）
    monitoring_period : Int64   // 监控周期（纳秒）
  }
  
  fn CircuitBreakerConfig::new() -> CircuitBreakerConfig {
    CircuitBreakerConfig::{
      failure_threshold: 5,
      recovery_timeout: 60000000000L,  // 1分钟
      success_threshold: 3,
      monitoring_period: 30000000000L  // 30秒
    }
  }
  
  // 断路器
  struct CircuitBreaker {
    state : CircuitBreakerState
    failure_count : Int
    success_count : Int
    last_failure_time : Int64
    last_state_change_time : Int64
    config : CircuitBreakerConfig
  }
  
  fn CircuitBreaker::new(config : CircuitBreakerConfig) -> CircuitBreaker {
    CircuitBreaker::{
      state: CircuitBreakerState::Closed,
      failure_count: 0,
      success_count: 0,
      last_failure_time: 0L,
      last_state_change_time: 0L,
      config
    }
  }
  
  fn CircuitBreaker::call[T](breaker : CircuitBreaker, operation : () -> T) -> (CircuitBreaker, Result[T, String]) {
    let current_time = 1640995200000000000L  // 模拟当前时间
    
    match breaker.state {
      CircuitBreakerState::Open => {
        // 检查是否应该转为半开状态
        if current_time - breaker.last_failure_time >= breaker.config.recovery_timeout {
          let updated_breaker = CircuitBreaker::{
            state: CircuitBreakerState::HalfOpen,
            failure_count: 0,
            success_count: 0,
            last_failure_time: breaker.last_failure_time,
            last_state_change_time: current_time,
            config: breaker.config
          }
          execute_operation(updated_breaker, operation)
        } else {
          (breaker, Err("Circuit breaker is open"))
        }
      }
      CircuitBreakerState::Closed | CircuitBreakerState::HalfOpen => {
        execute_operation(breaker, operation)
      }
    }
  }
  
  fn execute_operation[T](breaker : CircuitBreaker, operation : () -> T) -> (CircuitBreaker, Result[T, String]) {
    try {
      let result = operation()
      let updated_breaker = handle_success(breaker)
      (updated_breaker, Ok(result))
    } catch {
      let error_message = "Operation failed"
      let updated_breaker = handle_failure(breaker)
      (updated_breaker, Err(error_message))
    }
  }
  
  fn handle_success(breaker : CircuitBreaker) -> CircuitBreaker {
    let current_time = 1640995200000000000L  // 模拟当前时间
    
    match breaker.state {
      CircuitBreakerState::Closed => {
        // 重置失败计数
        CircuitBreaker::{
          state: CircuitBreakerState::Closed,
          failure_count: 0,
          success_count: breaker.success_count + 1,
          last_failure_time: breaker.last_failure_time,
          last_state_change_time: breaker.last_state_change_time,
          config: breaker.config
        }
      }
      CircuitBreakerState::HalfOpen => {
        let new_success_count = breaker.success_count + 1
        if new_success_count >= breaker.config.success_threshold {
          // 转为关闭状态
          CircuitBreaker::{
            state: CircuitBreakerState::Closed,
            failure_count: 0,
            success_count: 0,
            last_failure_time: breaker.last_failure_time,
            last_state_change_time: current_time,
            config: breaker.config
          }
        } else {
          CircuitBreaker::{
            state: CircuitBreakerState::HalfOpen,
            failure_count: breaker.failure_count,
            success_count: new_success_count,
            last_failure_time: breaker.last_failure_time,
            last_state_change_time: breaker.last_state_change_time,
            config: breaker.config
          }
        }
      }
      CircuitBreakerState::Open => breaker  // 不应该发生
    }
  }
  
  fn handle_failure(breaker : CircuitBreaker) -> CircuitBreaker {
    let current_time = 1640995200000000000L  // 模拟当前时间
    let new_failure_count = breaker.failure_count + 1
    
    match breaker.state {
      CircuitBreakerState::Closed => {
        if new_failure_count >= breaker.config.failure_threshold {
          // 转为开启状态
          CircuitBreaker::{
            state: CircuitBreakerState::Open,
            failure_count: new_failure_count,
            success_count: 0,
            last_failure_time: current_time,
            last_state_change_time: current_time,
            config: breaker.config
          }
        } else {
          CircuitBreaker::{
            state: CircuitBreakerState::Closed,
            failure_count: new_failure_count,
            success_count: breaker.success_count,
            last_failure_time: current_time,
            last_state_change_time: breaker.last_state_change_time,
            config: breaker.config
          }
        }
      }
      CircuitBreakerState::HalfOpen => {
        // 转为开启状态
        CircuitBreaker::{
          state: CircuitBreakerState::Open,
          failure_count: new_failure_count,
          success_count: 0,
          last_failure_time: current_time,
          last_state_change_time: current_time,
          config: breaker.config
        }
      }
      CircuitBreakerState::Open => breaker  // 不应该发生
    }
  }
  
  // 测试断路器
  let config = CircuitBreakerConfig::new()
  let breaker = CircuitBreaker::new(config)
  assert_eq(breaker.state, CircuitBreakerState::Closed)
  
  // 模拟成功操作
  let (breaker1, result1) = CircuitBreaker::call(breaker, fn() { "success" });
  assert_eq(breaker1.state, CircuitBreakerState::Closed)
  assert_eq(breaker1.failure_count, 0)
  assert_eq(result1, Ok("success"))
  
  // 模拟失败操作（未达到阈值）
  let mut current_breaker = breaker1
  for i in 0..4 {
    let (new_breaker, result) = CircuitBreaker::call(current_breaker, fn() { panic("simulated failure") });
    current_breaker = new_breaker
    assert_eq(result, Err("Operation failed"))
  }
  
  assert_eq(current_breaker.state, CircuitBreakerState::Closed)
  assert_eq(current_breaker.failure_count, 4)
  
  // 第5次失败，应该触发断路器开启
  let (breaker2, result2) = CircuitBreaker::call(current_breaker, fn() { panic("simulated failure") });
  assert_eq(breaker2.state, CircuitBreakerState::Open)
  assert_eq(breaker2.failure_count, 5)
  assert_eq(result2, Err("Operation failed"))
  
  // 断路器开启时，操作应该直接失败
  let (breaker3, result3) = CircuitBreaker::call(breaker2, fn() { "should not execute" });
  assert_eq(breaker3.state, CircuitBreakerState::Open)
  assert_eq(result3, Err("Circuit breaker is open"))
  
  // 模拟超时后转为半开状态
  let breaker_after_timeout = CircuitBreaker::{
    state: CircuitBreakerState::HalfOpen,
    failure_count: 0,
    success_count: 0,
    last_failure_time: breaker2.last_failure_time,
    last_state_change_time: 1640995200000000000L + 60000000001L,  // 超过恢复超时
    config: breaker2.config
  };
  
  // 半开状态下的成功操作
  let (breaker4, result4) = CircuitBreaker::call(breaker_after_timeout, fn() { "success in half-open" });
  assert_eq(breaker4.state, CircuitBreakerState::HalfOpen)
  assert_eq(breaker4.success_count, 1)
  assert_eq(result4, Ok("success in half-open"))
  
  // 继续成功操作，达到阈值后应该转为关闭状态
  let mut current_breaker = breaker4
  for i in 0..2 {
    let (new_breaker, result) = CircuitBreaker::call(current_breaker, fn() { "success" });
    current_breaker = new_breaker
    assert_eq(result, Ok("success"))
  }
  
  assert_eq(current_breaker.state, CircuitBreakerState::Closed)
  assert_eq(current_breaker.failure_count, 0)
}

// 测试2: 重试机制
test "重试机制测试" {
  // 重试策略
  enum RetryStrategy {
    Fixed(Int)           // 固定间隔
    Exponential(Int)     // 指数退避
    Linear(Int, Int)     // 线性增长（初始间隔，增量）
    Custom(Array[Int64>)  // 自定义间隔序列
  }
  
  // 重试配置
  struct RetryConfig {
    max_attempts : Int
    strategy : RetryStrategy
    max_delay : Int64  // 最大延迟（纳秒）
    jitter : Bool      // 是否添加随机抖动
  }
  
  fn RetryConfig::new(max_attempts : Int, strategy : RetryStrategy) -> RetryConfig {
    RetryConfig::{
      max_attempts,
      strategy,
      max_delay: 30000000000L,  // 30秒
      jitter: true
    }
  }
  
  // 重试执行器
  struct RetryExecutor {
    config : RetryConfig
  }
  
  fn RetryExecutor::new(config : RetryConfig) -> RetryExecutor {
    RetryExecutor::{ config }
  }
  
  fn RetryExecutor::execute[T](executor : RetryExecutor, operation : (Int) -> Result[T, String]) -> Result[T, String] {
    for attempt in 1..executor.config.max_attempts + 1 {
      match operation(attempt) {
        Ok(result) => return Ok(result)
        Err(error) => {
          if attempt == executor.config.max_attempts {
            return Err(error)
          }
          
          // 计算延迟时间
          let delay = calculate_delay(executor.config.strategy, attempt)
          let final_delay = if delay > executor.config.max_delay {
            executor.config.max_delay
          } else {
            delay
          }
          
          // 模拟延迟（在实际实现中会等待）
          // 这里只是验证延迟计算逻辑
          assert_true(final_delay > 0)
        }
      }
    }
    
    Err("Max attempts exceeded")
  }
  
  fn calculate_delay(strategy : RetryStrategy, attempt : Int) -> Int64 {
    match strategy {
      RetryStrategy::Fixed(interval) => interval.to_long()
      RetryStrategy::Exponential(base) => (base.to_long() * (1 << (attempt - 1)))
      RetryStrategy::Linear(initial, increment) => (initial + increment * (attempt - 1)).to_long()
      RetryStrategy::Custom(delays) => {
        if attempt - 1 < delays.length() {
          delays[attempt - 1]
        } else {
          delays[delays.length() - 1]
        }
      }
    }
  }
  
  // 测试重试机制
  let fixed_config = RetryConfig::new(3, RetryStrategy::Fixed(1000000000L))  // 1秒固定间隔
  let fixed_executor = RetryExecutor::new(fixed_config);
  
  // 第一次尝试就成功
  let result1 = RetryExecutor::execute(fixed_executor, fn(attempt) { 
    if attempt == 1 { Ok("success") } else { Err("should not reach") }
  });
  assert_eq(result1, Ok("success"))
  
  // 第二次尝试成功
  let result2 = RetryExecutor::execute(fixed_executor, fn(attempt) { 
    if attempt == 2 { Ok("success") } else { Err("failed") }
  });
  assert_eq(result2, Ok("success"))
  
  // 所有尝试都失败
  let result3 = RetryExecutor::execute(fixed_executor, fn(attempt) { 
    Err("always failed")
  });
  assert_eq(result3, Err("always failed"))
  
  // 测试指数退避
  let exp_config = RetryConfig::new(4, RetryStrategy::Exponential(1000000000L))  // 1秒基础间隔
  let exp_executor = RetryExecutor::new(exp_config);
  
  let result4 = RetryExecutor::execute(exp_executor, fn(attempt) { 
    if attempt == 3 { Ok("success") } else { Err("failed") }
  });
  assert_eq(result4, Ok("success"))
  
  // 测试线性增长
  let linear_config = RetryConfig::new(3, RetryStrategy::Linear(1000000000L, 500000000L))  // 1秒初始，500ms增量
  let linear_executor = RetryExecutor::new(linear_config);
  
  let result5 = RetryExecutor::execute(linear_executor, fn(attempt) { 
    if attempt == 2 { Ok("success") } else { Err("failed") }
  });
  assert_eq(result5, Ok("success"))
  
  // 测试自定义间隔
  let custom_delays = [1000000000L, 2000000000L, 5000000000L]  // 1秒，2秒，5秒
  let custom_config = RetryConfig::new(3, RetryStrategy::Custom(custom_delays))
  let custom_executor = RetryExecutor::new(custom_config);
  
  let result6 = RetryExecutor::execute(custom_executor, fn(attempt) { 
    if attempt == 3 { Ok("success") } else { Err("failed") }
  });
  assert_eq(result6, Ok("success"))
}

// 测试3: 超时处理
test "超时处理测试" {
  // 超时策略
  enum TimeoutStrategy {
    Fixed(Int64)           // 固定超时
    Adaptive(Double)       // 自适应超时（基于历史执行时间）
    Exponential(Double)    // 指数退避超时
    PerOperation(Array[(String, Int64)])  // 每个操作的超时
  }
  
  // 超时配置
  struct TimeoutConfig {
    strategy : TimeoutStrategy
    default_timeout : Int64
    max_timeout : Int64
    min_timeout : Int64
  }
  
  fn TimeoutConfig::new(strategy : TimeoutStrategy) -> TimeoutConfig {
    TimeoutConfig::{
      strategy,
      default_timeout: 5000000000L,  // 5秒
      max_timeout: 60000000000L,     // 60秒
      min_timeout: 1000000000L       // 1秒
    }
  }
  
  // 执行历史记录
  struct ExecutionHistory {
    operation_name : String
    execution_times : Array[Int64]
    success_count : Int
    failure_count : Int
  }
  
  fn ExecutionHistory::new(operation_name : String) -> ExecutionHistory {
    ExecutionHistory::{
      operation_name,
      execution_times: [],
      success_count: 0,
      failure_count: 0
    }
  }
  
  fn ExecutionHistory::add_record(history : ExecutionHistory, execution_time : Int64, success : Bool) -> ExecutionHistory {
    let mut updated_times = history.execution_times
    updated_times = updated_times.push(execution_time)
    
    // 保持历史记录在合理范围内
    if updated_times.length() > 100 {
      updated_times = updated_times.slice(updated_times.length() - 100, updated_times.length())
    }
    
    ExecutionHistory::{
      operation_name: history.operation_name,
      execution_times: updated_times,
      success_count: history.success_count + if success { 1 } else { 0 },
      failure_count: history.failure_count + if !success { 1 } else { 0 }
    }
  }
  
  fn ExecutionHistory::get_average_time(history : ExecutionHistory) -> Int64 {
    if history.execution_times.length() == 0 {
      return 5000000000L  // 默认5秒
    }
    
    let sum = history.execution_times.reduce(fn(acc, time) { acc + time }, 0L)
    sum / history.execution_times.length().to_long()
  }
  
  fn ExecutionHistory::get_percentile(history : ExecutionHistory, percentile : Double) -> Int64 {
    if history.execution_times.length() == 0 {
      return 5000000000L  // 默认5秒
    }
    
    let sorted_times = history.execution_times.sort_by(fn(a, b) { 
      if a < b { -1 } else if a > b { 1 } else { 0 } 
    })
    
    let index = (percentile * (sorted_times.length() - 1).to_double()).to_int()
    sorted_times[index]
  }
  
  // 超时处理器
  struct TimeoutHandler {
    config : TimeoutConfig
    histories : Array[ExecutionHistory]
  }
  
  fn TimeoutHandler::new(config : TimeoutConfig) -> TimeoutHandler {
    TimeoutHandler::{
      config,
      histories: []
    }
  }
  
  fn TimeoutHandler::execute_with_timeout[T](handler : TimeoutHandler, operation_name : String, operation : () -> T) -> (TimeoutHandler, Result[T, String]) {
    let timeout = calculate_timeout(handler, operation_name)
    
    // 模拟执行操作并记录执行时间
    let start_time = 1640995200000000000L  // 模拟开始时间
    let result = try {
      Ok(operation())
    } catch {
      Err("Operation failed")
    }
    let end_time = start_time + 3000000000L  // 模拟3秒执行时间
    let execution_time = end_time - start_time
    
    // 更新历史记录
    let updated_handler = update_history(handler, operation_name, execution_time, result.is_ok());
    
    // 检查是否超时
    if execution_time > timeout {
      (updated_handler, Err("Operation timed out"))
    } else {
      (updated_handler, result)
    }
  }
  
  fn calculate_timeout(handler : TimeoutHandler, operation_name : String) -> Int64 {
    match handler.config.strategy {
      TimeoutStrategy::Fixed(timeout) => timeout
      TimeoutStrategy::Adaptive(multiplier) => {
        match handler.histories.find(fn(h) { h.operation_name == operation_name }) {
          Some(history) => {
            let avg_time = ExecutionHistory::get_average_time(history)
            let p95_time = ExecutionHistory::get_percentile(history, 0.95)
            let calculated = (avg_time.to_double() * multiplier).to_long()
            
            // 使用平均值和95%分位数的较大者
            let adaptive_timeout = if p95_time > calculated { p95_time } else { calculated }
            
            // 限制在最小和最大超时之间
            if adaptive_timeout < handler.config.min_timeout {
              handler.config.min_timeout
            } else if adaptive_timeout > handler.config.max_timeout {
              handler.config.max_timeout
            } else {
              adaptive_timeout
            }
          }
          None => handler.config.default_timeout
        }
      }
      TimeoutStrategy::Exponential(base) => {
        match handler.histories.find(fn(h) { h.operation_name == operation_name }) {
          Some(history) => {
            let failure_rate = history.failure_count.to_double() / (history.success_count + history.failure_count).to_double()
            let multiplier = 1.0 + (failure_rate * base)
            let calculated = (handler.config.default_timeout.to_double() * multiplier).to_long()
            
            if calculated > handler.config.max_timeout {
              handler.config.max_timeout
            } else {
              calculated
            }
          }
          None => handler.config.default_timeout
        }
      }
      TimeoutStrategy::PerOperation(timeouts) => {
        for (name, timeout) in timeouts {
          if name == operation_name {
            return timeout
          }
        }
        handler.config.default_timeout
      }
    }
  }
  
  fn update_history(handler : TimeoutHandler, operation_name : String, execution_time : Int64, success : Bool) -> TimeoutHandler {
    match handler.histories.find_index(fn(h) { h.operation_name == operation_name }) {
      Some(index) => {
        let history = handler.histories[index]
        let updated_history = ExecutionHistory::add_record(history, execution_time, success)
        let mut updated_histories = handler.histories
        updated_histories[index] = updated_history
        TimeoutHandler::{ config: handler.config, histories: updated_histories }
      }
      None => {
        let new_history = ExecutionHistory::add_record(ExecutionHistory::new(operation_name), execution_time, success)
        TimeoutHandler::{ config: handler.config, histories: handler.histories.push(new_history) }
      }
    }
  }
  
  // 测试超时处理
  let fixed_config = TimeoutConfig::new(TimeoutStrategy::Fixed(2000000000L))  // 2秒固定超时
  let fixed_handler = TimeoutHandler::new(fixed_config);
  
  let (handler1, result1) = TimeoutHandler::execute_with_timeout(fixed_handler, "test-op", fn() { "success" });
  assert_eq(result1, Ok("success"))
  
  // 测试自适应超时
  let adaptive_config = TimeoutConfig::new(TimeoutStrategy::Adaptive(2.0))  // 2倍平均时间
  let adaptive_handler = TimeoutHandler::new(adaptive_config);
  
  // 第一次执行
  let (handler2, result2) = TimeoutHandler::execute_with_timeout(adaptive_handler, "adaptive-op", fn() { "success" });
  assert_eq(result2, Ok("success"))
  
  // 第二次执行
  let (handler3, result3) = TimeoutHandler::execute_with_timeout(handler2, "adaptive-op", fn() { "success" });
  assert_eq(result3, Ok("success"))
  
  // 验证历史记录
  let history = handler3.histories.find(fn(h) { h.operation_name == "adaptive-op" }).unwrap();
  assert_eq(history.success_count, 2)
  assert_eq(history.execution_times.length(), 2)
  
  // 测试每个操作的超时
  let per_op_timeouts = [
    ("fast-op", 1000000000L),   // 1秒
    ("slow-op", 10000000000L)   // 10秒
  ]
  let per_op_config = TimeoutConfig::new(TimeoutStrategy::PerOperation(per_op_timeouts))
  let per_op_handler = TimeoutHandler::new(per_op_config);
  
  let (_, result4) = TimeoutHandler::execute_with_timeout(per_op_handler, "fast-op", fn() { "success" });
  assert_eq(result4, Ok("success"))
  
  let (_, result5) = TimeoutHandler::execute_with_timeout(per_op_handler, "slow-op", fn() { "success" });
  assert_eq(result5, Ok("success"))
  
  let (_, result6) = TimeoutHandler::execute_with_timeout(per_op_handler, "unknown-op", fn() { "success" });
  assert_eq(result6, Ok("success"))  // 使用默认超时
}

// 测试4: 故障检测和隔离
test "故障检测和隔离测试" {
  // 健康状态
  enum HealthStatus {
    Healthy
    Degraded
    Unhealthy
    Unknown
  }
  
  // 故障检测器
  struct HealthChecker {
    check_interval : Int64
    timeout : Int64
    consecutive_failures : Int
    consecutive_successes : Int
    failure_threshold : Int
    recovery_threshold : Int
  }
  
  fn HealthChecker::new() -> HealthChecker {
    HealthChecker::{
      check_interval: 30000000000L,  // 30秒
      timeout: 5000000000L,           // 5秒
      consecutive_failures: 0,
      consecutive_successes: 0,
      failure_threshold: 3,
      recovery_threshold: 2
    }
  }
  
  // 服务实例
  struct ServiceInstance {
    id : String
    address : String
    status : HealthStatus
    last_check_time : Int64
    health_checker : HealthChecker
  }
  
  fn ServiceInstance::new(id : String, address : String) -> ServiceInstance {
    ServiceInstance::{
      id,
      address,
      status: HealthStatus::Unknown,
      last_check_time: 0L,
      health_checker: HealthChecker::new()
    }
  }
  
  fn ServiceInstance::check_health(instance : ServiceInstance, health_check_fn : () -> Bool) -> ServiceInstance {
    let current_time = 1640995200000000000L  // 模拟当前时间
    let is_healthy = health_check_fn()
    
    let (updated_status, updated_failures, updated_successes) = if is_healthy {
      let new_successes = instance.health_checker.consecutive_successes + 1
      
      if new_successes >= instance.health_checker.recovery_threshold {
        (HealthStatus::Healthy, 0, new_successes)
      } else if instance.status == HealthStatus::Unhealthy {
        (HealthStatus::Degraded, 0, new_successes)
      } else {
        (instance.status, 0, new_successes)
      }
    } else {
      let new_failures = instance.health_checker.consecutive_failures + 1
      
      if new_failures >= instance.health_checker.failure_threshold {
        (HealthStatus::Unhealthy, new_failures, 0)
      } else if instance.status == HealthStatus::Healthy {
        (HealthStatus::Degraded, new_failures, 0)
      } else {
        (instance.status, new_failures, 0)
      }
    }
    
    let updated_checker = HealthChecker::{
      check_interval: instance.health_checker.check_interval,
      timeout: instance.health_checker.timeout,
      consecutive_failures: updated_failures,
      consecutive_successes: updated_successes,
      failure_threshold: instance.health_checker.failure_threshold,
      recovery_threshold: instance.health_checker.recovery_threshold
    }
    
    ServiceInstance::{
      id: instance.id,
      address: instance.address,
      status: updated_status,
      last_check_time: current_time,
      health_checker: updated_checker
    }
  }
  
  // 服务注册表
  struct ServiceRegistry {
    instances : Array[ServiceInstance>
    isolation_policy : IsolationPolicy
  }
  
  // 隔离策略
  struct IsolationPolicy {
    auto_isolate : Bool
    isolate_on_degraded : Bool
    recovery_check_interval : Int64
  }
  
  fn IsolationPolicy::new() -> IsolationPolicy {
    IsolationPolicy::{
      auto_isolate: true,
      isolate_on_degraded: false,
      recovery_check_interval: 60000000000L  // 1分钟
    }
  }
  
  fn ServiceRegistry::new(isolation_policy : IsolationPolicy) -> ServiceRegistry {
    ServiceRegistry::{
      instances: [],
      isolation_policy
    }
  }
  
  fn ServiceRegistry::register_instance(registry : ServiceRegistry, instance : ServiceInstance) -> ServiceRegistry {
    ServiceRegistry::{
      instances: registry.instances.push(instance),
      isolation_policy: registry.isolation_policy
    }
  }
  
  fn ServiceRegistry::get_healthy_instances(registry : ServiceRegistry) -> Array[ServiceInstance] {
    registry.instances.filter(fn(instance) { 
      instance.status == HealthStatus::Healthy || 
      (registry.isolation_policy.isolate_on_degraded == false && instance.status == HealthStatus::Degraded)
    })
  }
  
  fn ServiceRegistry::check_all_health(registry : ServiceRegistry, health_check_fn : (String) -> Bool) -> ServiceRegistry {
    let mut updated_instances = []
    
    for instance in registry.instances {
      let updated_instance = ServiceInstance::check_health(instance, fn() { health_check_fn(instance.id) });
      updated_instances = updated_instances.push(updated_instance)
    }
    
    ServiceRegistry::{
      instances: updated_instances,
      isolation_policy: registry.isolation_policy
    }
  }
  
  // 测试故障检测和隔离
  let isolation_policy = IsolationPolicy::new();
  let registry = ServiceRegistry::new(isolation_policy);
  
  // 注册服务实例
  let instance1 = ServiceInstance::new("service-1", "http://localhost:8001");
  let instance2 = ServiceInstance::new("service-2", "http://localhost:8002");
  let instance3 = ServiceInstance::new("service-3", "http://localhost:8003");
  
  let registry1 = ServiceRegistry::register_instance(registry, instance1);
  let registry2 = ServiceRegistry::register_instance(registry1, instance2);
  let registry3 = ServiceRegistry::register_instance(registry2, instance3);
  
  assert_eq(registry3.instances.length(), 3)
  
  // 模拟健康检查（全部健康）
  let registry4 = ServiceRegistry::check_all_health(registry3, fn(id) { 
    match id {
      "service-1" => true,
      "service-2" => true,
      "service-3" => true,
      _ => false
    }
  });
  
  let healthy_instances1 = ServiceRegistry::get_healthy_instances(registry4);
  assert_eq(healthy_instances1.length(), 3)
  
  // 模拟service-2故障
  let registry5 = ServiceRegistry::check_all_health(registry4, fn(id) { 
    match id {
      "service-1" => true,
      "service-2" => false,  // 故障
      "service-3" => true,
      _ => false
    }
  });
  
  let healthy_instances2 = ServiceRegistry::get_healthy_instances(registry5);
  assert_eq(healthy_instances2.length(), 2)  // service-2应该被隔离
  
  // 继续故障service-2，达到阈值
  let mut registry6 = registry5
  for i in 0..3 {
    registry6 = ServiceRegistry::check_all_health(registry6, fn(id) { 
      match id {
        "service-1" => true,
        "service-2" => false,  // 持续故障
        "service-3" => true,
        _ => false
      }
    });
  }
  
  let healthy_instances3 = ServiceRegistry::get_healthy_instances(registry6);
  assert_eq(healthy_instances3.length(), 2)  // service-2应该被标记为不健康
  
  // 验证service-2的状态
  let service2 = registry6.instances.find(fn(i) { i.id == "service-2" }).unwrap();
  assert_eq(service2.status, HealthStatus::Unhealthy)
  
  // 模拟service-2恢复
  let registry7 = ServiceRegistry::check_all_health(registry6, fn(id) { 
    match id {
      "service-1" => true,
      "service-2" => true,  // 恢复
      "service-3" => true,
      _ => false
    }
  });
  
  let service2_recovering = registry7.instances.find(fn(i) { i.id == "service-2" }).unwrap();
  assert_eq(service2_recovering.status, HealthStatus::Degraded)  // 应该先进入降级状态
  
  // 继续健康检查，达到恢复阈值
  let mut registry8 = registry7
  for i in 0..2 {
    registry8 = ServiceRegistry::check_all_health(registry8, fn(id) { 
      match id {
        "service-1" => true,
        "service-2" => true,  // 持续健康
        "service-3" => true,
        _ => false
      }
    });
  }
  
  let service2_recovered = registry8.instances.find(fn(i) { i.id == "service-2" }).unwrap();
  assert_eq(service2_recovered.status, HealthStatus::Healthy)  // 应该恢复到健康状态
  
  let healthy_instances4 = ServiceRegistry::get_healthy_instances(registry8);
  assert_eq(healthy_instances4.length(), 3)  // 所有实例都应该健康
}

// 测试5: 优雅降级和熔断
test "优雅降级和熔断测试" {
  // 降级策略
  enum FallbackStrategy {
    ReturnDefault
    ReturnCachedValue
    ReturnAlternativeService
    ReturnError
  }
  
  // 降级配置
  struct FallbackConfig {
    strategy : FallbackStrategy
    default_value : Option[String]
    alternative_service : Option[String]
    cache_ttl : Int64  // 缓存生存时间
  }
  
  fn FallbackConfig::new(strategy : FallbackStrategy) -> FallbackConfig {
    FallbackConfig::{
      strategy,
      default_value: None,
      alternative_service: None,
      cache_ttl: 30000000000L  // 30秒
    }
  }
  
  // 缓存项
  struct CacheItem {
    value : String
    timestamp : Int64
    ttl : Int64
  }
  
  fn CacheItem::new(value : String, timestamp : Int64, ttl : Int64) -> CacheItem {
    CacheItem::{ value, timestamp, ttl }
  }
  
  fn CacheItem::is_valid(item : CacheItem, current_time : Int64) -> Bool {
    current_time - item.timestamp < item.ttl
  }
  
  // 降级处理器
  struct FallbackHandler {
    config : FallbackConfig
    cache : Array[(String, CacheItem)]
  }
  
  fn FallbackHandler::new(config : FallbackConfig) -> FallbackHandler {
    FallbackHandler::{
      config,
      cache: []
    }
  }
  
  fn FallbackHandler::execute_with_fallback(handler : FallbackHandler, operation_name : String, primary_operation : () -> String) -> (FallbackHandler, String) {
    let current_time = 1640995200000000000L  // 模拟当前时间
    
    // 尝试执行主要操作
    let primary_result = try {
      Ok(primary_operation())
    } catch {
      Err("Primary operation failed")
    }
    
    match primary_result {
      Ok(result) => {
        // 成功，更新缓存
        let updated_handler = update_cache(handler, operation_name, result, current_time);
        (updated_handler, result)
      }
      Err(_) => {
        // 失败，执行降级策略
        execute_fallback_strategy(handler, operation_name, current_time)
      }
    }
  }
  
  fn execute_fallback_strategy(handler : FallbackHandler, operation_name : String, current_time : Int64) -> (FallbackHandler, String) {
    match handler.config.strategy {
      FallbackStrategy::ReturnDefault => {
        match handler.config.default_value {
          Some(value) => (handler, value)
          None => (handler, "Default value")
        }
      }
      FallbackStrategy::ReturnCachedValue => {
        match get_from_cache(handler, operation_name, current_time) {
          Some(cached_value) => (handler, cached_value)
          None => (handler, "No cached value available")
        }
      }
      FallbackStrategy::ReturnAlternativeService => {
        match handler.config.alternative_service {
          Some(service) => (handler, "Response from alternative service: " + service)
          None => (handler, "No alternative service configured")
        }
      }
      FallbackStrategy::ReturnError => {
        (handler, "Service unavailable")
      }
    }
  }
  
  fn update_cache(handler : FallbackHandler, operation_name : String, value : String, timestamp : Int64) -> FallbackHandler {
    let cache_item = CacheItem::new(value, timestamp, handler.config.cache_ttl)
    
    // 查找并更新缓存项
    let mut updated_cache = []
    let mut found = false
    
    for (name, item) in handler.cache {
      if name == operation_name {
        updated_cache = updated_cache.push((operation_name, cache_item))
        found = true
      } else {
        updated_cache = updated_cache.push((name, item))
      }
    }
    
    if !found {
      updated_cache = updated_cache.push((operation_name, cache_item))
    }
    
    FallbackHandler::{
      config: handler.config,
      cache: updated_cache
    }
  }
  
  fn get_from_cache(handler : FallbackHandler, operation_name : String, current_time : Int64) -> Option[String> {
    for (name, item) in handler.cache {
      if name == operation_name && CacheItem::is_valid(item, current_time) {
        return Some(item.value)
      }
    }
    None
  }
  
  // 测试优雅降级和熔断
  let default_config = FallbackConfig::new(FallbackStrategy::ReturnDefault);
  let default_handler = FallbackHandler::new(default_config);
  
  // 主要操作成功
  let (handler1, result1) = FallbackHandler::execute_with_fallback(default_handler, "test-op", fn() { "success" });
  assert_eq(result1, "success")
  
  // 主要操作失败，返回默认值
  let default_config_with_value = FallbackConfig::{
    strategy: FallbackStrategy::ReturnDefault,
    default_value: Some("default-response"),
    alternative_service: None,
    cache_ttl: 30000000000L
  };
  let handler_with_default = FallbackHandler::new(default_config_with_value);
  
  let (handler2, result2) = FallbackHandler::execute_with_fallback(handler_with_default, "test-op", fn() { panic("operation failed") });
  assert_eq(result2, "default-response")
  
  // 测试缓存降级
  let cache_config = FallbackConfig::new(FallbackStrategy::ReturnCachedValue);
  let cache_handler = FallbackHandler::new(cache_config);
  
  // 第一次成功执行，缓存结果
  let (cache_handler1, result3) = FallbackHandler::execute_with_fallback(cache_handler, "cache-op", fn() { "cached-result" });
  assert_eq(result3, "cached-result")
  
  // 第二次失败，返回缓存值
  let (cache_handler2, result4) = FallbackHandler::execute_with_fallback(cache_handler1, "cache-op", fn() { panic("operation failed") });
  assert_eq(result4, "cached-result")
  
  // 测试替代服务降级
  let alternative_config = FallbackConfig::{
    strategy: FallbackStrategy::ReturnAlternativeService,
    default_value: None,
    alternative_service: Some("backup-service"),
    cache_ttl: 30000000000L
  };
  let alternative_handler = FallbackHandler::new(alternative_config);
  
  let (_, result5) = FallbackHandler::execute_with_fallback(alternative_handler, "alt-op", fn() { panic("operation failed") });
  assert_eq(result5, "Response from alternative service: backup-service")
  
  // 测试返回错误降级
  let error_config = FallbackConfig::new(FallbackStrategy::ReturnError);
  let error_handler = FallbackHandler::new(error_config);
  
  let (_, result6) = FallbackHandler::execute_with_fallback(error_handler, "error-op", fn() { panic("operation failed") });
  assert_eq(result6, "Service unavailable")
}