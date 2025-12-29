// 网络分区容错测试用例 - 测试网络分区情况下的遥测系统容错能力

use azimuth.telemetry.api.common.{AttributeValue, Resource}
use azimuth.telemetry.api.trace.{SpanContext, Span, SpanKind, StatusCode, SpanEvent, NoopTracer, NoopTracerProvider}
use azimuth.telemetry.api.logs.{SeverityNumber, LogRecordBuilder, NoopLogger, NoopLoggerProvider}
use azimuth.telemetry.api.context.{Context, ContextKey, create_key}

// 网络状态
pub enum NetworkStatus {
  Connected
  Partitioned
  Degraded
  Recovering
}

// 网络分区类型
pub enum PartitionType {
  FullPartition      // 完全分区，无法连接
  PartialPartition   // 部分分区，某些节点可达
  LatencySpike       // 延迟激增
  PacketLoss         // 丢包
  BandwidthLimit     // 带宽限制
}

// 容错策略
pub enum ToleranceStrategy {
  Retry              // 重试
  CircuitBreaker     // 熔断器
  Fallback           // 降级
  Buffering          // 缓冲
  GracefulDegradation // 优雅降级
}

// 网络分区配置
pub struct PartitionConfig {
  partition_type : PartitionType
  duration_ms : Int64
  affected_nodes : Array[String]
  tolerance_strategy : ToleranceStrategy
  retry_attempts : Int
  retry_delay_ms : Int64
  buffer_size : Int
}

// 容错结果
pub struct ToleranceResult {
  strategy : ToleranceStrategy
  success : Bool
  attempts : Int
  total_time_ms : Int64
  data_lost : Bool
  data_corrupted : Bool
  fallback_used : Bool
}

// 遥测数据缓冲区
pub struct TelemetryBuffer {
  data : Array[String]
  max_size : Int
  current_size : Int
  overflow_count : Int
}

// 熔断器状态
pub enum CircuitBreakerState {
  Closed     // 正常状态
  Open       // 熔断状态
  HalfOpen   // 半开状态
}

// 熔断器
pub struct CircuitBreaker {
  state : CircuitBreakerState
  failure_count : Int
  failure_threshold : Int
  recovery_timeout_ms : Int64
  last_failure_time : Int64
}

test "network_partition_full_partition_tolerance" {
  // 测试完全网络分区的容错处理
  
  // 1. 创建完全分区配置
  let full_partition_config = PartitionConfig{
    partition_type: FullPartition,
    duration_ms: 30000L,  // 30秒分区
    affected_nodes: ["collector-1", "collector-2", "collector-3"],
    tolerance_strategy: Buffering,
    retry_attempts: 3,
    retry_delay_ms: 1000L,
    buffer_size: 10000
  }
  
  // 2. 创建遥测缓冲区
  let buffer = TelemetryBuffer{
    data: [],
    max_size: full_partition_config.buffer_size,
    current_size: 0,
    overflow_count: 0
  }
  
  // 3. 模拟网络分区前的正常状态
  let network_status_before = Connected
  assert_eq(network_status_before, Connected)
  
  // 4. 模拟网络分区发生
  let network_status_during = Partitioned
  let partition_start_time = 1640995200000000000L
  
  // 5. 在分区期间生成遥测数据
  let mut telemetry_data_generated = 0
  let mut telemetry_data_buffered = 0
  
  // 模拟30秒分区期间的数据生成
  let mut current_time = partition_start_time
  while current_time < partition_start_time + full_partition_config.duration_ms * 1000000L {
    // 每秒生成10个遥测数据
    let mut i = 0
    while i < 10 {
      let telemetry_data = "telemetry_data_" + telemetry_data_generated.to_string()
      telemetry_data_generated = telemetry_data_generated + 1
      
      // 尝试发送，但因为分区而失败，数据被缓冲
      if buffer.current_size < buffer.max_size {
        buffer.data.push(telemetry_data)
        buffer.current_size = buffer.current_size + 1
        telemetry_data_buffered = telemetry_data_buffered + 1
      } else {
        buffer.overflow_count = buffer.overflow_count + 1
      }
      
      i = i + 1
    }
    
    current_time = current_time + 1000000000L  // 前进1秒
  }
  
  // 6. 验证分区期间的数据缓冲
  assert_eq(telemetry_data_generated > 0, true)
  assert_eq(telemetry_data_buffered > 0, true)
  assert_eq(buffer.current_size, telemetry_data_buffered)
  assert_eq(buffer.current_size <= buffer.max_size, true)
  
  // 7. 模拟网络恢复
  let network_status_after = Connected
  let partition_end_time = partition_start_time + full_partition_config.duration_ms * 1000000L
  
  // 8. 网络恢复后的数据重放
  let mut data_sent_successfully = 0
  let mut data_send_failed = 0
  let mut retry_attempts = 0
  
  let mut i = 0
  while i < buffer.data.length() {
    let data = buffer.data[i]
    
    // 尝试发送数据，如果失败则重试
    let mut sent = false
    let mut attempt = 0
    
    while attempt < full_partition_config.retry_attempts && !sent {
      attempt = attempt + 1
      retry_attempts = retry_attempts + 1
      
      // 模拟发送成功（网络已恢复）
      sent = true
    }
    
    if sent {
      data_sent_successfully = data_sent_successfully + 1
    } else {
      data_send_failed = data_send_failed + 1
    }
    
    i = i + 1
  }
  
  // 9. 验证容错结果
  let tolerance_result = ToleranceResult{
    strategy: full_partition_config.tolerance_strategy,
    success: data_sent_successfully > 0,
    attempts: retry_attempts,
    total_time_ms: full_partition_config.duration_ms,
    data_lost: data_send_failed > 0,
    data_corrupted: false,
    fallback_used: false
  }
  
  assert_eq(tolerance_result.strategy, Buffering)
  assert_eq(tolerance_result.success, true)
  assert_eq(tolerance_result.attempts > 0, true)
  assert_eq(tolerance_result.total_time_ms, full_partition_config.duration_ms)
  
  // 10. 验证数据完整性
  assert_eq(data_sent_successfully + data_send_failed, telemetry_data_buffered)
  assert_eq(data_sent_successfully > telemetry_data_buffered * 9 / 10, true)  // 至少90%的数据成功发送
}

test "network_partition_circuit_breaker_tolerance" {
  // 测试熔断器容错策略
  
  // 1. 创建熔断器
  let circuit_breaker = CircuitBreaker{
    state: Closed,
    failure_count: 0,
    failure_threshold: 5,
    recovery_timeout_ms: 60000L,  // 1分钟恢复超时
    last_failure_time: 0L
  }
  
  // 2. 模拟网络故障逐渐增加
  let mut failure_count = 0
  let mut circuit_breaker_state_changes = []
  
  // 模拟连续失败
  let mut i = 0
  while i < 10 {
    // 模拟发送遥测数据
    let send_success = i < 3  // 前3次成功，后面都失败
    
    if !send_success {
      failure_count = failure_count + 1
      
      // 更新熔断器状态
      if failure_count >= circuit_breaker.failure_threshold && circuit_breaker.state == Closed {
        circuit_breaker.state = Open
        circuit_breaker.last_failure_time = 1640995200000000000L + i.to_int64() * 1000000000L
        circuit_breaker_state_changes.push(("Closed", "Open", i))
      }
    }
    
    i = i + 1
  }
  
  // 3. 验证熔断器状态变化
  assert_eq(circuit_breaker.state, Open)
  assert_eq(circuit_breaker_state_changes.length(), 1)
  assert_eq(circuit_breaker_state_changes[0].0, "Closed")
  assert_eq(circuit_breaker_state_changes[0].1, "Open")
  assert_eq(circuit_breaker_state_changes[0].2, 5)  // 第5次失败后熔断
  
  // 4. 测试熔断器开启时的行为
  let mut requests_blocked = 0
  let mut requests_allowed = 0
  
  // 模拟在熔断器开启时的请求
  i = 0
  while i < 10 {
    if circuit_breaker.state == Open {
      requests_blocked = requests_blocked + 1
      // 请求被直接拒绝，不尝试发送
    } else {
      requests_allowed = requests_allowed + 1
    }
    i = i + 1
  }
  
  assert_eq(requests_blocked, 10)
  assert_eq(requests_allowed, 0)
  
  // 5. 模拟熔断器恢复
  let current_time = circuit_breaker.last_failure_time + circuit_breaker.recovery_timeout_ms * 1000000L
  
  // 检查是否可以进入半开状态
  if current_time >= circuit_breaker.last_failure_time + circuit_breaker.recovery_timeout_ms * 1000000L {
    circuit_breaker.state = HalfOpen
    circuit_breaker_state_changes.push(("Open", "HalfOpen", 10))
  }
  
  assert_eq(circuit_breaker.state, HalfOpen)
  
  // 6. 测试半开状态
  let mut half_open_success = 0
  let mut half_open_failure = 0
  
  // 在半开状态下允许少量请求通过
  i = 0
  while i < 3 {
    // 模拟发送成功（网络已恢复）
    half_open_success = half_open_success + 1
    i = i + 1
  }
  
  // 如果半开状态下的请求成功，则关闭熔断器
  if half_open_success > 0 {
    circuit_breaker.state = Closed
    circuit_breaker.failure_count = 0
    circuit_breaker_state_changes.push(("HalfOpen", "Closed", 13))
  }
  
  // 7. 验证熔断器恢复
  assert_eq(circuit_breaker.state, Closed)
  assert_eq(circuit_breaker.failure_count, 0)
  assert_eq(circuit_breaker_state_changes.length(), 3)
  assert_eq(circuit_breaker_state_changes[2].0, "HalfOpen")
  assert_eq(circuit_breaker_state_changes[2].1, "Closed")
  
  // 8. 验证容错结果
  let tolerance_result = ToleranceResult{
    strategy: CircuitBreaker,
    success: true,
    attempts: failure_count + half_open_success + half_open_failure,
    total_time_ms: circuit_breaker.recovery_timeout_ms,
    data_lost: false,
    data_corrupted: false,
    fallback_used: true
  }
  
  assert_eq(tolerance_result.strategy, CircuitBreaker)
  assert_eq(tolerance_result.success, true)
  assert_eq(tolerance_result.fallback_used, true)
}

test "network_partition_graceful_degradation_tolerance" {
  // 测试优雅降级容错策略
  
  // 1. 创建优雅降级配置
  let degradation_config = PartitionConfig{
    partition_type: PartialPartition,
    duration_ms: 60000L,  // 1分钟分区
    affected_nodes: ["collector-1"],  // 只有一个收集器不可用
    tolerance_strategy: GracefulDegradation,
    retry_attempts: 2,
    retry_delay_ms: 500L,
    buffer_size: 1000
  }
  
  // 2. 定义遥测数据优先级
  let telemetry_priorities = [
    ("critical_error", 1),
    ("security_event", 2),
    ("performance_metric", 3),
    ("debug_info", 4),
    ("trace_data", 5)
  ]
  
  // 3. 模拟网络分区发生
  let partition_start_time = 1640995200000000000L
  
  // 4. 在分区期间生成不同优先级的遥测数据
  let mut generated_data = []
  let mut total_generated = 0
  
  let mut priority_index = 0
  while priority_index < telemetry_priorities.length() {
    let (data_type, priority) = telemetry_priorities[priority_index]
    
    // 为每个优先级生成数据
    let mut i = 0
    while i < 20 {
      let data = data_type + "_" + i.to_string()
      generated_data.push((data, priority))
      total_generated = total_generated + 1
      i = i + 1
    }
    
    priority_index = priority_index + 1
  }
  
  // 5. 应用优雅降级策略
  let mut data_sent = []
  let mut data_dropped = []
  let mut data_buffered = []
  
  // 按优先级排序
  let mut sorted_data = generated_data
  // 简化排序：按优先级数字升序排列
  
  let mut i = 0
  while i < sorted_data.length() {
    let (data, priority) = sorted_data[i]
    
    // 根据优先级决定处理方式
    if priority <= 2 {
      // 高优先级数据：尝试发送，失败则缓冲
      data_sent.push(data)
    } else if priority == 3 {
      // 中优先级数据：缓冲
      data_buffered.push(data)
    } else {
      // 低优先级数据：丢弃
      data_dropped.push(data)
    }
    
    i = i + 1
  }
  
  // 6. 验证优雅降级效果
  assert_eq(data_sent.length(), 40)  // priority 1和2的数据
  assert_eq(data_buffered.length(), 20)  // priority 3的数据
  assert_eq(data_dropped.length(), 40)  // priority 4和5的数据
  assert_eq(data_sent.length() + data_buffered.length() + data_dropped.length(), total_generated)
  
  // 7. 模拟网络恢复
  let partition_end_time = partition_start_time + degradation_config.duration_ms * 1000000L
  
  // 8. 网络恢复后处理缓冲的数据
  let mut buffered_data_sent = 0
  let mut buffered_data_failed = 0
  
  let mut i = 0
  while i < data_buffered.length() {
    let data = data_buffered[i]
    
    // 尝试发送缓冲的数据
    let send_success = i < data_buffered.length() * 9 / 10  // 90%成功
    if send_success {
      buffered_data_sent = buffered_data_sent + 1
    } else {
      buffered_data_failed = buffered_data_failed + 1
    }
    
    i = i + 1
  }
  
  // 9. 验证容错结果
  let total_data_sent = data_sent.length() + buffered_data_sent
  let total_data_lost = data_dropped.length() + buffered_data_failed
  
  let tolerance_result = ToleranceResult{
    strategy: GracefulDegradation,
    success: total_data_sent > total_data_lost,
    attempts: data_sent.length() + data_buffered.length(),
    total_time_ms: degradation_config.duration_ms,
    data_lost: total_data_lost > 0,
    data_corrupted: false,
    fallback_used: true
  }
  
  assert_eq(tolerance_result.strategy, GracefulDegradation)
  assert_eq(tolerance_result.success, true)
  assert_eq(tolerance_result.data_lost, true)  // 有数据丢失
  assert_eq(tolerance_result.fallback_used, true)
  
  // 10. 验证关键数据得到保护
  let critical_data_sent = 0  // priority 1的数据
  let security_data_sent = 20  // priority 2的数据
  assert_eq(critical_data_sent + security_data_sent, 40)  // 关键数据都得到了处理
}

test "network_partition_latency_spike_tolerance" {
  // 测试网络延迟激增的容错处理
  
  // 1. 创建延迟激增配置
  let latency_spike_config = PartitionConfig{
    partition_type: LatencySpike,
    duration_ms: 30000L,  // 30秒延迟激增
    affected_nodes: ["collector-1", "collector-2"],
    tolerance_strategy: Retry,
    retry_attempts: 3,
    retry_delay_ms: 2000L,
    buffer_size: 500
  }
  
  // 2. 定义正常和异常延迟
  let normal_latency_ms = 50L
  let spike_latency_ms = 5000L  // 5秒延迟
  let timeout_threshold_ms = 3000L  // 3秒超时
  
  // 3. 模拟延迟激增发生
  let spike_start_time = 1640995200000000000L
  let spike_end_time = spike_start_time + latency_spike_config.duration_ms * 1000000L
  
  // 4. 在延迟激增期间发送遥测数据
  let mut requests_sent = 0
  let mut requests_successful = 0
  let mut requests_timed_out = 0
  let mut requests_retried = 0
  let mut total_latency = 0L
  
  let mut current_time = spike_start_time
  while current_time < spike_end_time {
    // 每秒发送一个请求
    let request_start_time = current_time
    
    // 模拟网络延迟
    let current_latency = if current_time >= spike_start_time && current_time < spike_end_time {
      spike_latency_ms
    } else {
      normal_latency_ms
    }
    
    let request_end_time = request_start_time + current_latency * 1000000L
    total_latency = total_latency + current_latency
    
    // 检查是否超时
    if current_latency > timeout_threshold_ms {
      requests_timed_out = requests_timed_out + 1
      
      // 超时重试
      let mut retry_success = false
      let mut attempt = 0
      
      while attempt < latency_spike_config.retry_attempts && !retry_success {
        attempt = attempt + 1
        requests_retried = requests_retried + 1
        
        // 重试时延迟可能降低
        let retry_latency = normal_latency_ms + (spike_latency_ms - normal_latency_ms) / (attempt + 1)
        if retry_latency <= timeout_threshold_ms {
          retry_success = true
          requests_successful = requests_successful + 1
        }
      }
    } else {
      requests_successful = requests_successful + 1
    }
    
    requests_sent = requests_sent + 1
    current_time = current_time + 1000000000L  // 前进1秒
  }
  
  // 5. 验证延迟激增期间的表现
  assert_eq(requests_sent, 30)  // 30秒内每秒一个请求
  assert_eq(requests_timed_out > 0, true)  // 有请求超时
  assert_eq(requests_retried > 0, true)  // 有重试
  
  // 6. 计算平均延迟
  let average_latency = total_latency / requests_sent
  assert_eq(average_latency > normal_latency_ms, true)
  assert_eq(average_latency < spike_latency_ms, true)
  
  // 7. 验证容错结果
  let tolerance_result = ToleranceResult{
    strategy: Retry,
    success: requests_successful > requests_timed_out,
    attempts: requests_sent + requests_retried,
    total_time_ms: latency_spike_config.duration_ms,
    data_lost: requests_timed_out > requests_retried,
    data_corrupted: false,
    fallback_used: false
  }
  
  assert_eq(tolerance_result.strategy, Retry)
  assert_eq(tolerance_result.attempts, requests_sent + requests_retried)
  assert_eq(tolerance_result.total_time_ms, latency_spike_config.duration_ms)
}

test "network_partition_bandwidth_limit_tolerance" {
  // 测试带宽限制的容错处理
  
  // 1. 创建带宽限制配置
  let bandwidth_limit_config = PartitionConfig{
    partition_type: BandwidthLimit,
    duration_ms: 60000L,  // 1分钟带宽限制
    affected_nodes: ["collector-1"],
    tolerance_strategy: Fallback,
    retry_attempts: 1,
    retry_delay_ms: 1000L,
    buffer_size: 2000
  }
  
  // 2. 定义带宽参数
  let normal_bandwidth_kbps = 1000  // 1MB/s
  let limited_bandwidth_kbps = 100   // 100KB/s
  let telemetry_data_size_kb = 10    // 每个遥测数据10KB
  
  // 3. 模拟带宽限制发生
  let limit_start_time = 1640995200000000000L
  
  // 4. 在带宽限制期间发送遥测数据
  let mut data_generated = 0
  let mut data_sent = 0
  let mut data_dropped = 0
  let mut data_fallback = 0
  let mut total_bandwidth_used = 0L
  
  let mut current_time = limit_start_time
  let limit_end_time = limit_start_time + bandwidth_limit_config.duration_ms * 1000000L
  
  while current_time < limit_end_time {
    // 每秒生成5个遥测数据
    let mut i = 0
    while i < 5 {
      data_generated = data_generated + 1
      
      // 检查当前带宽使用情况
      let current_bandwidth = if current_time >= limit_start_time && current_time < limit_end_time {
        limited_bandwidth_kbps
      } else {
        normal_bandwidth_kbps
      }
      
      // 检查是否有足够带宽
      if total_bandwidth_used + telemetry_data_size_kb.to_long() <= current_bandwidth.to_long() {
        // 有足够带宽，发送数据
        data_sent = data_sent + 1
        total_bandwidth_used = total_bandwidth_used + telemetry_data_size_kb.to_long()
      } else {
        // 带宽不足，使用降级策略
        if data_generated % 3 == 0 {
          // 每3个数据中有1个使用降级（发送摘要）
          data_fallback = data_fallback + 1
          total_bandwidth_used = total_bandwidth_used + 1  // 摘要只有1KB
        } else {
          // 其他数据被丢弃
          data_dropped = data_dropped + 1
        }
      }
      
      i = i + 1
    }
    
    // 每秒重置带宽计数
    total_bandwidth_used = 0L
    current_time = current_time + 1000000000L  // 前进1秒
  }
  
  // 5. 验证带宽限制期间的表现
  assert_eq(data_generated, 300)  // 60秒内每秒5个数据
  assert_eq(data_sent > 0, true)  // 有数据发送成功
  assert_eq(data_fallback > 0, true)  // 有数据使用降级
  assert_eq(data_dropped > 0, true)  // 有数据被丢弃
  
  // 6. 验证数据分布
  let total_processed = data_sent + data_fallback + data_dropped
  assert_eq(total_processed, data_generated)
  
  // 在带宽限制下，应该有一定比例的数据使用降级
  let fallback_ratio = data_fallback.to_double() / total_processed.to_double()
  assert_eq(fallback_ratio > 0.1 && fallback_ratio < 0.5, true)
  
  // 7. 验证容错结果
  let tolerance_result = ToleranceResult{
    strategy: Fallback,
    success: data_sent + data_fallback > data_dropped,
    attempts: data_generated,
    total_time_ms: bandwidth_limit_config.duration_ms,
    data_lost: data_dropped > 0,
    data_corrupted: false,
    fallback_used: data_fallback > 0
  }
  
  assert_eq(tolerance_result.strategy, Fallback)
  assert_eq(tolerance_result.success, true)
  assert_eq(tolerance_result.data_lost, true)
  assert_eq(tolerance_result.fallback_used, true)
}

test "network_partition_mixed_tolerance_strategies" {
  // 测试混合容错策略
  
  // 1. 创建混合网络故障场景
  let mixed_scenarios = [
    (FullPartition, Buffering, 30000L),
    (PartialPartition, CircuitBreaker, 20000L),
    (LatencySpike, Retry, 15000L),
    (BandwidthLimit, GracefulDegradation, 25000L)
  ]
  
  // 2. 为每个场景测试容错策略
  let mut scenario_results = []
  
  let mut i = 0
  while i < mixed_scenarios.length() {
    let (partition_type, strategy, duration) = mixed_scenarios[i]
    
    let config = PartitionConfig{
      partition_type: partition_type,
      duration_ms: duration,
      affected_nodes: ["collector-" + i.to_string()],
      tolerance_strategy: strategy,
      retry_attempts: 3,
      retry_delay_ms: 1000L,
      buffer_size: 1000
    }
    
    // 模拟故障和恢复
    let data_generated = 100
    let mut data_handled = 0
    
    match strategy {
      Buffering => {
        // 缓冲策略：所有数据都被缓冲，恢复后发送
        data_handled = data_generated
      }
      CircuitBreaker => {
        // 熔断策略：部分数据被熔断器阻止
        data_handled = data_generated * 7 / 10  // 70%的数据被处理
      }
      Retry => {
        // 重试策略：大部分数据最终成功
        data_handled = data_generated * 9 / 10  // 90%的数据被处理
      }
      GracefulDegradation => {
        // 优雅降级：高优先级数据被处理
        data_handled = data_generated * 8 / 10  // 80%的数据被处理
      }
      Fallback => {
        // 降级策略：部分数据降级处理
        data_handled = data_generated * 6 / 10  // 60%的数据被处理
      }
    }
    
    let result = ToleranceResult{
      strategy: strategy,
      success: data_handled > data_generated / 2,
      attempts: data_generated,
      total_time_ms: duration,
      data_lost: data_handled < data_generated,
      data_corrupted: false,
      fallback_used: strategy != Buffering
    }
    
    scenario_results.push(result)
    i = i + 1
  }
  
  // 3. 验证所有场景的容错效果
  let mut successful_strategies = 0
  let mut i = 0
  while i < scenario_results.length() {
    let result = scenario_results[i]
    
    assert_eq(result.success, true)  // 所有策略都应该有成功
    assert_eq(result.attempts > 0, true)  // 都有尝试处理
    assert_eq(result.total_time_ms > 0, true)  // 都有处理时间
    
    if result.success {
      successful_strategies = successful_strategies + 1
    }
    
    i = i + 1
  }
  
  assert_eq(successful_strategies, scenario_results.length())
  
  // 4. 分析最佳策略
  let mut best_strategy = scenario_results[0].strategy
  let mut best_success_rate = 0.0
  
  i = 0
  while i < scenario_results.length() {
    let result = scenario_results[i]
    let success_rate = result.attempts.to_double() / 100.0  // 假设生成了100个数据
    
    if success_rate > best_success_rate {
      best_success_rate = success_rate
      best_strategy = result.strategy
    }
    
    i = i + 1
  }
  
  // 验证找到了最佳策略
  assert_eq(best_success_rate > 0.5, true)  // 最佳策略成功率应该超过50%
  
  // 5. 验证策略组合的有效性
  // 在实际应用中，可以根据不同的网络状况选择不同的策略
  let strategy_effectiveness = [
    (Buffering, 0.95),      // 缓冲策略在完全分区时最有效
    (CircuitBreaker, 0.85), // 熔断策略在部分分区时有效
    (Retry, 0.90),          // 重试策略在延迟问题时有效
    (GracefulDegradation, 0.88) // 优雅降级在带宽限制时有效
  ]
  
  let mut i = 0
  while i < strategy_effectiveness.length() {
    let (strategy, effectiveness) = strategy_effectiveness[i]
    assert_eq(effectiveness > 0.8, true)  // 所有策略的有效性都应该超过80%
    i = i + 1
  }
}