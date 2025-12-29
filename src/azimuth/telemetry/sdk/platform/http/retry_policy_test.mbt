// HTTP客户端重试策略测试 - 用于测试HTTP请求的重试机制和策略

test "retry_policy_basic_configuration" {
  // 测试重试策略基本配置
  
  enum BackoffStrategy {
    Fixed
    Linear
    Exponential
    ExponentialWithJitter
  }
  
  struct RetryPolicy {
    max_attempts : Int
    backoff_strategy : BackoffStrategy
    base_delay_ms : Int
    max_delay_ms : Int
    multiplier : Double
    jitter_factor : Double
  }
  
  let retry_policies = [
    {
      max_attempts: 3,
      backoff_strategy: Fixed,
      base_delay_ms: 1000,
      max_delay_ms: 1000,
      multiplier: 1.0,
      jitter_factor: 0.0
    },
    {
      max_attempts: 5,
      backoff_strategy: Linear,
      base_delay_ms: 500,
      max_delay_ms: 5000,
      multiplier: 2.0,
      jitter_factor: 0.1
    },
    {
      max_attempts: 4,
      backoff_strategy: Exponential,
      base_delay_ms: 200,
      max_delay_ms: 10000,
      multiplier: 2.0,
      jitter_factor: 0.0
    },
    {
      max_attempts: 6,
      backoff_strategy: ExponentialWithJitter,
      base_delay_ms: 100,
      max_delay_ms: 30000,
      multiplier: 2.5,
      jitter_factor: 0.2
    }
  ]
  
  // 验证重试策略配置
  for policy in retry_policies {
    assert_eq(policy.max_attempts > 0, true)
    assert_eq(policy.base_delay_ms > 0, true)
    assert_eq(policy.max_delay_ms >= policy.base_delay_ms, true)
    assert_eq(policy.multiplier > 0.0, true)
    assert_eq(policy.jitter_factor >= 0.0 && policy.jitter_factor <= 1.0, true)
    
    match policy.backoff_strategy {
      Fixed => {
        assert_eq(policy.multiplier, 1.0)
        assert_eq(policy.jitter_factor, 0.0)
      }
      Linear => {
        assert_eq(policy.multiplier > 1.0, true)
      }
      Exponential => {
        assert_eq(policy.multiplier > 1.0, true)
      }
      ExponentialWithJitter => {
        assert_eq(policy.multiplier > 1.0, true)
        assert_eq(policy.jitter_factor > 0.0, true)
      }
    }
  }
}

test "delay_calculation_strategies" {
  // 测试不同退避策略的延迟计算
  
  let calculate_delay = fn(attempt : Int, strategy : BackoffStrategy, base_delay : Int, 
                           multiplier : Double, max_delay : Int, jitter : Double) : Int {
    let base_delay_double = base_delay.to_double()
    let delay = match strategy {
      Fixed => base_delay_double
      Linear => base_delay_double * (attempt.to_double() * multiplier)
      Exponential => base_delay_double * (multiplier ^ attempt.to_double())
      ExponentialWithJitter => {
        let exp_delay = base_delay_double * (multiplier ^ attempt.to_double())
        let jitter_range = exp_delay * jitter
        exp_delay + (@rand() * jitter_range * 2.0 - jitter_range)
      }
    }
    
    let capped_delay = @min(delay, max_delay.to_double())
    @max(capped_delay, 0.0).to_int()
  }
  
  // 测试固定延迟策略
  let fixed_delay = calculate_delay(1, Fixed, 1000, 1.0, 1000, 0.0)
  assert_eq(fixed_delay, 1000)
  
  let fixed_delay_attempt3 = calculate_delay(3, Fixed, 1000, 1.0, 1000, 0.0)
  assert_eq(fixed_delay_attempt3, 1000)
  
  // 测试线性退避策略
  let linear_delay1 = calculate_delay(1, Linear, 500, 2.0, 5000, 0.0)
  let linear_delay2 = calculate_delay(2, Linear, 500, 2.0, 5000, 0.0)
  let linear_delay3 = calculate_delay(3, Linear, 500, 2.0, 5000, 0.0)
  
  assert_eq(linear_delay1, 1000)   // 500 * (1 * 2.0)
  assert_eq(linear_delay2, 2000)   // 500 * (2 * 2.0)
  assert_eq(linear_delay3, 3000)   // 500 * (3 * 2.0)
  
  // 测试指数退避策略
  let exp_delay1 = calculate_delay(1, Exponential, 200, 2.0, 10000, 0.0)
  let exp_delay2 = calculate_delay(2, Exponential, 200, 2.0, 10000, 0.0)
  let exp_delay3 = calculate_delay(3, Exponential, 200, 2.0, 10000, 0.0)
  let exp_delay4 = calculate_delay(4, Exponential, 200, 2.0, 10000, 0.0)
  
  assert_eq(exp_delay1, 400)    // 200 * (2.0^1)
  assert_eq(exp_delay2, 800)    // 200 * (2.0^2)
  assert_eq(exp_delay3, 1600)   // 200 * (2.0^3)
  assert_eq(exp_delay4, 3200)   // 200 * (2.0^4)
  
  // 测试最大延迟限制
  let capped_delay = calculate_delay(10, Exponential, 200, 2.0, 5000, 0.0)
  assert_eq(capped_delay, 5000)  // 应该被限制在最大延迟
  
  // 测试带抖动的指数退避
  let jitter_delay1 = calculate_delay(2, ExponentialWithJitter, 100, 2.0, 30000, 0.2)
  let jitter_delay2 = calculate_delay(2, ExponentialWithJitter, 100, 2.0, 30000, 0.2)
  
  // 基础延迟应该是400ms，抖动范围是±80ms
  let base_exp_delay = 400.0
  let jitter_range = base_exp_delay * 0.2  // 80ms
  
  assert_eq(jitter_delay1 >= (base_exp_delay - jitter_range).to_int(), true)
  assert_eq(jitter_delay1 <= (base_exp_delay + jitter_range).to_int(), true)
  assert_eq(jitter_delay2 >= (base_exp_delay - jitter_range).to_int(), true)
  assert_eq(jitter_delay2 <= (base_exp_delay + jitter_range).to_int(), true)
}

test "retry_condition_evaluation" {
  // 测试重试条件评估
  
  enum HTTPStatus {
    Ok(200)
    Created(201)
    Accepted(202)
    BadRequest(400)
    Unauthorized(401)
    Forbidden(403)
    NotFound(404)
    TooManyRequests(429)
    InternalServerError(500)
    BadGateway(502)
    ServiceUnavailable(503)
    GatewayTimeout(504)
  }
  
  struct HTTPResponse {
    status : HTTPStatus
    headers : Array[String]
    body : String
    duration_ms : Int
  }
  
  struct RetryDecision {
    should_retry : Bool
    reason : String
    suggested_delay_ms : Int
  }
  
  // 创建响应示例
  let responses = [
    {
      status: Ok(200),
      headers: ["Content-Type: application/json"],
      body: "{\"success\": true}",
      duration_ms: 150
    },
    {
      status: TooManyRequests(429),
      headers: ["Retry-After: 60"],
      body: "{\"error\": \"rate limit exceeded\"}",
      duration_ms: 50
    },
    {
      status: InternalServerError(500),
      headers: [],
      body: "{\"error\": \"internal server error\"}",
      duration_ms: 2000
    },
    {
      status: ServiceUnavailable(503),
      headers: ["Retry-After: 30"],
      body: "{\"error\": \"service unavailable\"}",
      duration_ms: 100
    },
    {
      status: BadRequest(400),
      headers: [],
      body: "{\"error\": \"bad request\"}",
      duration_ms: 100
    }
  ]
  
  // 重试决策逻辑
  let should_retry_response = fn(response : HTTPResponse, attempt : Int, max_attempts : Int) : RetryDecision {
    match response.status {
      Ok(_) | Created(_) | Accepted(_) => {
        {
          should_retry: false,
          reason: "successful response",
          suggested_delay_ms: 0
        }
      }
      BadRequest(_) | Unauthorized(_) | Forbidden(_) | NotFound(_) => {
        {
          should_retry: false,
          reason: "client error - no point retrying",
          suggested_delay_ms: 0
        }
      }
      TooManyRequests(_) => {
        // 检查Retry-After头
        let retry_after = 60  // 默认60秒
        {
          should_retry: attempt < max_attempts,
          reason: "rate limited, will retry after specified time",
          suggested_delay_ms: retry_after * 1000
        }
      }
      InternalServerError(_) | BadGateway(_) | ServiceUnavailable(_) | GatewayTimeout(_) => {
        {
          should_retry: attempt < max_attempts,
          reason: "server error - safe to retry",
          suggested_delay_ms: 1000 * attempt  // 简单的线性退避
        }
      }
    }
  }
  
  // 测试重试决策
  let decisions = responses.map(fn(r) { should_retry_response(r, 1, 3) })
  
  assert_eq(decisions[0].should_retry, false)  // 200 OK
  assert_eq(decisions[0].reason, "successful response")
  
  assert_eq(decisions[1].should_retry, true)   // 429 Too Many Requests
  assert_eq(decisions[1].suggested_delay_ms, 60000)  // 60秒
  
  assert_eq(decisions[2].should_retry, true)   // 500 Internal Server Error
  assert_eq(decisions[2].reason, "server error - safe to retry")
  
  assert_eq(decisions[3].should_retry, true)   // 503 Service Unavailable
  assert_eq(decisions[3].suggested_delay_ms, 30000)  // 30秒
  
  assert_eq(decisions[4].should_retry, false)  // 400 Bad Request
  assert_eq(decisions[4].reason, "client error - no point retrying")
  
  // 测试最大尝试次数限制
  let response_429 = responses[1]
  let decision_attempt1 = should_retry_response(response_429, 1, 3)
  let decision_attempt3 = should_retry_response(response_429, 3, 3)
  let decision_attempt4 = should_retry_response(response_429, 4, 3)
  
  assert_eq(decision_attempt1.should_retry, true)
  assert_eq(decision_attempt3.should_retry, false)  // 达到最大尝试次数
  assert_eq(decision_attempt4.should_retry, false)  // 超过最大尝试次数
}

test "retry_execution_flow" {
  // 测试重试执行流程
  
  struct RequestAttempt {
    attempt_number : Int
    timestamp : Int64
    delay_before_ms : Int
    response_status : Int
    success : Bool
  }
  
  struct RetrySession {
    policy : RetryPolicy
    attempts : Array[RequestAttempt]
    total_duration_ms : Int
    final_success : Bool
  }
  
  // 模拟重试会话
  let simulate_retry_session = fn(policy : RetryPolicy, status_sequence : Array[Int]) : RetrySession {
    let mut attempts : Array[RequestAttempt] = []
    let mut current_timestamp = 1609459200000L  // 基础时间戳
    let mut final_success = false
    
    for i = 0; i < status_sequence.length() && i < policy.max_attempts; i = i + 1 {
      let attempt_num = i + 1
      let delay_before = if i > 0 {
        // 计算延迟
        let base_delay = policy.base_delay_ms.to_double()
        let delay = match policy.backoff_strategy {
          Exponential => base_delay * (policy.multiplier ^ i.to_double())
          Fixed => base_delay
          Linear => base_delay * (i.to_double() * policy.multiplier)
          ExponentialWithJitter => base_delay * (policy.multiplier ^ i.to_double())  // 简化，不加抖动
        }
        @min(delay, policy.max_delay_ms.to_double()).to_int()
      } else {
        0
      }
      
      current_timestamp = current_timestamp + delay_before.to_int64()
      
      let status = status_sequence[i]
      let success = status >= 200 && status < 300
      
      attempts.push({
        attempt_number: attempt_num,
        timestamp: current_timestamp,
        delay_before_ms: delay_before,
        response_status: status,
        success: success
      })
      
      if success {
        final_success = true
        break
      }
    }
    
    let total_duration = if attempts.length() > 0 {
      (attempts[attempts.length() - 1].timestamp - attempts[0].timestamp + 
       attempts[attempts.length() - 1].delay_before_ms.to_int64()).to_int()
    } else {
      0
    }
    
    {
      policy: policy,
      attempts: attempts,
      total_duration_ms: total_duration,
      final_success: final_success
    }
  }
  
  // 测试成功的重试场景
  let success_policy = {
    max_attempts: 3,
    backoff_strategy: Exponential,
    base_delay_ms: 1000,
    max_delay_ms: 10000,
    multiplier: 2.0,
    jitter_factor: 0.0
  }
  
  let success_sequence = [500, 500, 200]  // 前两次失败，第三次成功
  let success_session = simulate_retry_session(success_policy, success_sequence)
  
  assert_eq(success_session.attempts.length(), 3)
  assert_eq(success_session.final_success, true)
  assert_eq(success_session.attempts[0].delay_before_ms, 0)
  assert_eq(success_session.attempts[1].delay_before_ms, 1000)  // 1000 * 2^0
  assert_eq(success_session.attempts[2].delay_before_ms, 2000)  // 1000 * 2^1
  assert_eq(success_session.attempts[2].success, true)
  
  // 测试失败的重试场景（所有尝试都失败）
  let failure_sequence = [500, 500, 500, 500]
  let failure_session = simulate_retry_session(success_policy, failure_sequence)
  
  assert_eq(failure_session.attempts.length(), 3)  // 只尝试3次（max_attempts）
  assert_eq(failure_session.final_success, false)
  assert_eq(failure_session.attempts[2].success, false)
  
  // 测试第一次就成功的场景
  let immediate_success_sequence = [200]
  let immediate_session = simulate_retry_session(success_policy, immediate_success_sequence)
  
  assert_eq(immediate_session.attempts.length(), 1)
  assert_eq(immediate_session.final_success, true)
  assert_eq(immediate_session.attempts[0].success, true)
  assert_eq(immediate_session.total_duration_ms, 0)
}

test "retry_circuit_breaker_integration" {
  // 测试重试与熔断器的集成
  
  enum CircuitState {
    Closed
    Open
    HalfOpen
  }
  
  struct CircuitBreaker {
    state : CircuitState
    failure_threshold : Int
    recovery_timeout_ms : Int
    failure_count : Int
    last_failure_time : Int64
    success_count : Int
  }
  
  struct RetryWithCircuitBreaker {
    retry_policy : RetryPolicy
    circuit_breaker : CircuitBreaker
  }
  
  // 创建熔断器
  let create_circuit_breaker = fn(failure_threshold : Int, recovery_timeout : Int) : CircuitBreaker {
    {
      state: Closed,
      failure_threshold: failure_threshold,
      recovery_timeout_ms: recovery_timeout,
      failure_count: 0,
      last_failure_time: 0L,
      success_count: 0
    }
  }
  
  // 模拟请求执行
  let execute_with_circuit_breaker = fn(config : RetryWithCircuitBreaker, 
                                       current_time : Int64, 
                                       should_succeed : Bool) : (Bool, CircuitBreaker) {
    let mut cb = config.circuit_breaker
    
    // 检查熔断器状态
    if cb.state == Open {
      // 检查是否可以尝试恢复
      if current_time - cb.last_failure_time >= cb.recovery_timeout_ms.to_int64() {
        cb.state = HalfOpen
        cb.success_count = 0
      } else {
        // 熔断器仍然打开，拒绝请求
        return (false, cb)
      }
    }
    
    // 执行请求
    let success = should_succeed
    
    if success {
      cb.success_count = cb.success_count + 1
      
      if cb.state == HalfOpen {
        // 在半开状态下成功，关闭熔断器
        cb.state = Closed
        cb.failure_count = 0
      } else {
        // 在关闭状态下成功，重置失败计数
        cb.failure_count = @max(0, cb.failure_count - 1)
      }
    } else {
      cb.failure_count = cb.failure_count + 1
      cb.last_failure_time = current_time
      
      if cb.failure_count >= cb.failure_threshold {
        // 达到失败阈值，打开熔断器
        cb.state = Open
      } else if cb.state == HalfOpen {
        // 在半开状态下失败，重新打开熔断器
        cb.state = Open
      }
    }
    
    (success, cb)
  }
  
  // 测试熔断器行为
  let retry_policy = {
    max_attempts: 3,
    backoff_strategy: Exponential,
    base_delay_ms: 1000,
    max_delay_ms: 10000,
    multiplier: 2.0,
    jitter_factor: 0.0
  }
  
  let circuit_breaker = create_circuit_breaker(3, 30000)  // 3次失败后熔断，30秒后尝试恢复
  let config = { retry_policy: retry_policy, circuit_breaker: circuit_breaker }
  
  let mut current_cb = circuit_breaker
  let mut current_time = 1609459200000L
  
  // 前两次失败
  let (success1, cb1) = execute_with_circuit_breaker(config, current_time, false)
  let (success2, cb2) = execute_with_circuit_breaker({ config | circuit_breaker: cb1 }, current_time, false)
  
  assert_eq(success1, false)
  assert_eq(success2, false)
  assert_eq(cb1.state, Closed)
  assert_eq(cb1.failure_count, 1)
  assert_eq(cb2.state, Closed)
  assert_eq(cb2.failure_count, 2)
  
  // 第三次失败，应该触发熔断器
  let (success3, cb3) = execute_with_circuit_breaker({ config | circuit_breaker: cb2 }, current_time, false)
  
  assert_eq(success3, false)
  assert_eq(cb3.state, Open)
  assert_eq(cb3.failure_count, 3)
  
  // 熔断器打开期间，请求应该被拒绝
  let (success4, cb4) = execute_with_circuit_breaker({ config | circuit_breaker: cb3 }, current_time, true)
  
  assert_eq(success4, false)  // 即使should_succeed=true，也被熔断器拒绝
  assert_eq(cb4.state, Open)
  
  // 超过恢复超时时间后，应该允许尝试
  current_time = current_time + 35000L  // 超过30秒
  let (success5, cb5) = execute_with_circuit_breaker({ config | circuit_breaker: cb4 }, current_time, true)
  
  assert_eq(success5, true)
  assert_eq(cb5.state, Closed)  // 成功后关闭熔断器
  assert_eq(cb5.failure_count, 0)
}

test "retry_metrics_and_monitoring" {
  // 测试重试指标和监控
  
  struct RetryMetrics {
    total_requests : Int
    successful_requests : Int
    failed_requests : Int
    total_retries : Int
    average_attempts_per_request : Double
    total_delay_time_ms : Int64
    circuit_breaker_activations : Int
  }
  
  struct RequestMetrics {
    request_id : String
    attempts : Int
    total_time_ms : Int64
    final_status : String
    delays : Array[Int]
  }
  
  // 收集重试指标
  let collect_metrics = fn(requests : Array[RequestMetrics]) : RetryMetrics {
    let mut total_requests = requests.length()
    let mut successful_requests = 0
    let mut failed_requests = 0
    let mut total_retries = 0
    let mut total_delay_time = 0L
    let mut circuit_breaker_activations = 0
    
    for request in requests {
      if request.final_status == "success" {
        successful_requests = successful_requests + 1
      } else {
        failed_requests = failed_requests + 1
      }
      
      total_retries = total_retries + request.attempts - 1  // 减去第一次尝试
      
      for delay in request.delays {
        total_delay_time = total_delay_time + delay.to_int64()
      }
      
      if request.final_status == "circuit_breaker_open" {
        circuit_breaker_activations = circuit_breaker_activations + 1
      }
    }
    
    let avg_attempts = if total_requests > 0 {
      total_retries.to_double() / total_requests.to_double() + 1.0
    } else {
      0.0
    }
    
    {
      total_requests: total_requests,
      successful_requests: successful_requests,
      failed_requests: failed_requests,
      total_retries: total_retries,
      average_attempts_per_request: avg_attempts,
      total_delay_time_ms: total_delay_time,
      circuit_breaker_activations: circuit_breaker_activations
    }
  }
  
  // 创建测试请求数据
  let test_requests = [
    {
      request_id: "req-001",
      attempts: 1,
      total_time_ms: 150L,
      final_status: "success",
      delays: []
    },
    {
      request_id: "req-002",
      attempts: 3,
      total_time_ms: 3500L,
      final_status: "success",
      delays: [1000, 2000]
    },
    {
      request_id: "req-003",
      attempts: 4,
      total_time_ms: 7000L,
      final_status: "failed",
      delays: [1000, 2000, 4000]
    },
    {
      request_id: "req-004",
      attempts: 1,
      total_time_ms: 10L,
      final_status: "circuit_breaker_open",
      delays: []
    },
    {
      request_id: "req-005",
      attempts: 2,
      total_time_ms: 1500L,
      final_status: "success",
      delays: [1000]
    }
  ]
  
  let metrics = collect_metrics(test_requests)
  
  // 验证指标计算
  assert_eq(metrics.total_requests, 5)
  assert_eq(metrics.successful_requests, 3)
  assert_eq(metrics.failed_requests, 1)
  assert_eq(metrics.total_retries, 6)  // (0 + 2 + 3 + 0 + 1)
  assert_eq(metrics.average_attempts_per_request, 2.2)  // (1 + 3 + 4 + 1 + 2) / 5
  assert_eq(metrics.total_delay_time_ms, 8000L)  // (0 + 3000 + 7000 + 0 + 1000)
  assert_eq(metrics.circuit_breaker_activations, 1)
  
  // 计算成功率
  let success_rate = metrics.successful_requests.to_double() / metrics.total_requests.to_double()
  assert_eq(success_rate, 0.6)  // 60%
  
  // 计算重试率
  let retry_rate = metrics.total_retries.to_double() / metrics.total_requests.to_double()
  assert_eq(retry_rate, 1.2)  // 平均每个请求重试1.2次
  
  // 测试性能阈值
  let max_avg_attempts = 3.0
  let max_success_rate_drop = 0.5  // 成功率不应低于50%
  let max_circuit_breaker_rate = 0.2  // 熔断器激活率不应超过20%
  
  assert_eq(metrics.average_attempts_per_request <= max_avg_attempts, true)
  assert_eq(success_rate >= max_success_rate_drop, true)
  
  let circuit_breaker_rate = metrics.circuit_breaker_activations.to_double() / metrics.total_requests.to_double()
  assert_eq(circuit_breaker_rate <= max_circuit_breaker_rate, true)
}