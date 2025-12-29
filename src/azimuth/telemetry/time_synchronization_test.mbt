// 时间同步测试用例
test "clock_skew_detection" {
  // 测试时钟偏移检测和处理
  
  // 1. 定义时钟状态
  enum ClockStatus {
    Synchronized    // 时钟已同步
    Skewed          // 时钟偏移
    Drifting        // 时钟漂移
    Unstable        // 时钟不稳定
  }
  
  // 2. 定义时钟偏移信息
  struct ClockSkewInfo {
    local_time : Int64          // 本地时间（纳秒）
    remote_time : Int64         // 远程时间（纳秒）
    skew_ns : Int64             // 偏移量（纳秒）
    skew_ms : Double            // 偏移量（毫秒）
    drift_rate : Double         // 漂移率（ppm）
    confidence : Double         // 置信度
    status : ClockStatus
  }
  
  // 3. 定义时间同步配置
  struct TimeSyncConfig {
    max_acceptable_skew_ms : Int    // 最大可接受偏移（毫秒）
    max_drift_rate_ppm : Double     // 最大漂移率（ppm）
    sync_interval_ms : Int          // 同步间隔（毫秒）
    sample_count : Int              // 采样数量
    confidence_threshold : Double   // 置信度阈值
  }
  
  // 4. 创建时间同步配置
  let time_sync_config = TimeSyncConfig::{
    max_acceptable_skew_ms: 100,
    max_drift_rate_ppm: 50.0,
    sync_interval_ms: 60000,  // 1分钟
    sample_count: 10,
    confidence_threshold: 0.95
  }
  
  // 5. 实现时钟偏移检测函数
  fn detect_clock_skew(local_time : Int64, remote_time : Int64, previous_skews : Array[Int64]) -> ClockSkewInfo {
    let skew_ns = remote_time - local_time
    let skew_ms = skew_ns.to_double() / 1000000.0
    
    // 计算漂移率
    let mut drift_rate = 0.0
    if previous_skews.length() > 0 {
      let latest_skew = previous_skews[previous_skews.length() - 1]
      let time_diff = local_time - (local_time - 60000000000L)  // 假设1分钟前的采样
      let skew_diff = skew_ns - latest_skew
      drift_rate = (skew_diff.to_double() / time_diff.to_double()) * 1000000.0  // ppm
    }
    
    // 计算置信度
    let confidence = calculate_confidence(skew_ns, previous_skews)
    
    // 确定时钟状态
    let status = determine_clock_status(skew_ms, drift_rate, confidence, time_sync_config)
    
    ClockSkewInfo::{
      local_time: local_time,
      remote_time: remote_time,
      skew_ns: skew_ns,
      skew_ms: skew_ms,
      drift_rate: drift_rate,
      confidence: confidence,
      status: status
    }
  }
  
  // 6. 实现置信度计算函数
  fn calculate_confidence(current_skew : Int64, previous_skews : Array[Int64]) -> Double {
    if previous_skews.length() < 3 {
      return 0.5  // 采样不足，低置信度
    }
    
    // 计算偏移的标准差
    let mean = calculate_mean(previous_skews)
    let variance = calculate_variance(previous_skews, mean)
    let std_dev = sqrt(variance)
    
    // 基于标准差计算置信度
    let relative_std_dev = std_dev / mean.abs().to_double()
    if relative_std_dev < 0.1 {
      0.95
    } else if relative_std_dev < 0.2 {
      0.8
    } else if relative_std_dev < 0.5 {
      0.6
    } else {
      0.3
    }
  }
  
  // 7. 实现时钟状态确定函数
  fn determine_clock_status(skew_ms : Double, drift_rate : Double, confidence : Double, config : TimeSyncConfig) -> ClockStatus {
    if confidence < config.confidence_threshold {
      Unstable
    } else if skew_ms.abs() > config.max_acceptable_skew_ms.to_double() {
      Skewed
    } else if drift_rate.abs() > config.max_drift_rate_ppm {
      Drifting
    } else {
      Synchronized
    }
  }
  
  // 8. 实现统计函数
  fn calculate_mean(values : Array[Int64]) -> Double {
    let mut sum = 0.0
    let mut i = 0
    while i < values.length() {
      sum = sum + values[i].to_double()
      i = i + 1
    }
    sum / values.length().to_double()
  }
  
  fn calculate_variance(values : Array[Int64], mean : Double) -> Double {
    let mut sum_sq_diff = 0.0
    let mut i = 0
    while i < values.length() {
      let diff = values[i].to_double() - mean
      sum_sq_diff = sum_sq_diff + (diff * diff)
      i = i + 1
    }
    sum_sq_diff / values.length().to_double()
  }
  
  fn sqrt(x : Double) -> Double {
    // 简化的平方根实现
    if x == 0.0 {
      return 0.0
    }
    
    let mut guess = x / 2.0
    let mut i = 0
    while i < 10 {
      guess = (guess + x / guess) / 2.0
      i = i + 1
    }
    guess
  }
  
  // 9. 测试时钟偏移检测场景
  
  // 场景1: 完美同步的时钟
  let perfect_local = 1640995200000000000L
  let perfect_remote = 1640995200000000000L
  let perfect_previous = [0L, 0L, 0L, 0L, 0L]
  
  let perfect_skew = detect_clock_skew(perfect_local, perfect_remote, perfect_previous)
  assert_eq(perfect_skew.skew_ms, 0.0)
  assert_eq(perfect_skew.status, Synchronized)
  assert_eq(perfect_skew.confidence > 0.9, true)
  
  // 场景2: 小幅偏移但在可接受范围内
  let small_skew_local = 1640995200000000000L
  let small_skew_remote = 16409952000000050000L  // 50ms偏移
  let small_skew_previous = [40000L, 45000L, 50000L, 55000L, 48000L]
  
  let small_skew = detect_clock_skew(small_skew_local, small_skew_remote, small_skew_previous)
  assert_eq(small_skew.skew_ms.abs() <= 50.0, true)
  assert_eq(small_skew.status, Synchronized)
  
  // 场景3: 大幅偏移超出可接受范围
  let large_skew_local = 1640995200000000000L
  let large_skew_remote = 1640995200000200000L  // 200ms偏移
  let large_skew_previous = [1800000L, 1900000L, 2000000L, 2100000L, 1950000L]
  
  let large_skew = detect_clock_skew(large_skew_local, large_skew_remote, large_skew_previous)
  assert_eq(large_skew.skew_ms.abs() > 100.0, true)
  assert_eq(large_skew.status, Skewed)
  
  // 场景4: 时钟漂移
  let drift_local = 1640995200000000000L
  let drift_remote = 1640995200000001000L  // 1ms偏移但漂移率高
  let drift_previous = [500L, 1500L, 2500L, 3500L, 4500L]  // 持续增加的偏移
  
  let drift = detect_clock_skew(drift_local, drift_remote, drift_previous)
  assert_eq(drift.status, Drifting)
  assert_eq(drift.drift_rate.abs() > 50.0, true)
  
  // 场景5: 不稳定的时钟
  let unstable_local = 1640995200000000000L
  let unstable_remote = 1640995200000005000L  // 5ms偏移
  let unstable_previous = [-10000000L, 10000000L, -5000000L, 15000000L, -2000000L]  // 变化很大的偏移
  
  let unstable = detect_clock_skew(unstable_local, unstable_remote, unstable_previous)
  assert_eq(unstable.status, Unstable)
  assert_eq(unstable.confidence < 0.5, true)
}

test "timestamp_correction" {
  // 测试时间戳校正功能
  
  // 1. 定义校正策略
  enum CorrectionStrategy {
    NoCorrection      // 不校正
    LinearAdjustment  // 线性调整
    StepAdjustment    // 步进调整
    SlewCorrection    // 渐进校正
  }
  
  // 2. 定义校正配置
  struct CorrectionConfig {
    strategy : CorrectionStrategy
    max_correction_rate_ppm : Double  // 最大校正速率
    max_step_correction_ms : Int      // 最大步进校正量
    correction_window_ms : Int        // 校正窗口时间
    enable_monotonicity : Bool        // 启用单调性保证
  }
  
  // 3. 定义时间戳校正器
  struct TimestampCorrector {
    config : CorrectionConfig
    base_offset_ns : Int64
    correction_start_time : Int64
    last_correction_time : Int64
    total_corrected_ns : Int64
  }
  
  // 4. 创建校正配置
  let correction_config = CorrectionConfig::{
    strategy: SlewCorrection,
    max_correction_rate_ppm: 100.0,
    max_step_correction_ms: 50,
    correction_window_ms: 60000,  // 1分钟
    enable_monotonicity: true
  }
  
  // 5. 初始化时间戳校正器
  let corrector = TimestampCorrector::{
    config: correction_config,
    base_offset_ns: 0L,
    correction_start_time: 0L,
    last_correction_time: 0L,
    total_corrected_ns: 0L
  }
  
  // 6. 实现时间戳校正函数
  fn correct_timestamp(corrector : TimestampCorrector, raw_timestamp : Int64, current_time : Int64, target_offset_ns : Int64) -> (TimestampCorrector, Int64) {
    let mut updated_corrector = corrector
    let corrected_timestamp = raw_timestamp
    
    match corrector.config.strategy {
      NoCorrection => {
        // 不校正，直接返回原始时间戳
        (updated_corrector, corrected_timestamp)
      }
      LinearAdjustment => {
        // 线性调整
        let correction = target_offset_ns
        updated_corrector.base_offset_ns = correction
        (updated_corrector, corrected_timestamp + correction)
      }
      StepAdjustment => {
        // 步进调整
        let max_step_ns = corrector.config.max_step_correction_ms * 1000000L
        let current_offset = corrector.base_offset_ns
        let offset_diff = target_offset_ns - current_offset
        
        let step_correction = if offset_diff.abs() > max_step_ns {
          if offset_diff > 0 { max_step_ns } else { -max_step_ns }
        } else {
          offset_diff
        }
        
        updated_corrector.base_offset_ns = current_offset + step_correction
        (updated_corrector, corrected_timestamp + updated_corrector.base_offset_ns)
      }
      SlewCorrection => {
        // 渐进校正
        let time_since_last_correction = current_time - corrector.last_correction_time
        let max_correction_ns = (time_since_last_correction.to_double() * corrector.config.max_correction_rate_ppm / 1000000.0).to_int()
        
        let current_offset = corrector.base_offset_ns
        let offset_diff = target_offset_ns - current_offset
        
        let slew_correction = if offset_diff.abs() > max_correction_ns {
          if offset_diff > 0 { max_correction_ns } else { -max_correction_ns }
        } else {
          offset_diff
        }
        
        updated_corrector.base_offset_ns = current_offset + slew_correction
        updated_corrector.last_correction_time = current_time
        updated_corrector.total_corrected_ns = updated_corrector.total_corrected_ns + slew_correction
        
        (updated_corrector, corrected_timestamp + updated_corrector.base_offset_ns)
      }
    }
  }
  
  // 7. 测试不同校正策略
  
  let base_time = 1640995200000000000L
  let target_offset = 100000000L  // 100ms偏移
  
  // 场景1: 不校正策略
  let no_correction_config = CorrectionConfig::{
    strategy: NoCorrection,
    max_correction_rate_ppm: 100.0,
    max_step_correction_ms: 50,
    correction_window_ms: 60000,
    enable_monotonicity: true
  }
  
  let no_correction_corrector = TimestampCorrector::{
    config: no_correction_config,
    base_offset_ns: 0L,
    correction_start_time: base_time,
    last_correction_time: base_time,
    total_corrected_ns: 0L
  }
  
  let (corrector1, corrected1) = correct_timestamp(no_correction_corrector, base_time, base_time, target_offset)
  assert_eq(corrected1, base_time)  // 未校正
  assert_eq(corrector1.base_offset_ns, 0L)
  
  // 场景2: 线性调整策略
  let linear_config = CorrectionConfig::{
    strategy: LinearAdjustment,
    max_correction_rate_ppm: 100.0,
    max_step_correction_ms: 50,
    correction_window_ms: 60000,
    enable_monotonicity: true
  }
  
  let linear_corrector = TimestampCorrector::{
    config: linear_config,
    base_offset_ns: 0L,
    correction_start_time: base_time,
    last_correction_time: base_time,
    total_corrected_ns: 0L
  }
  
  let (corrector2, corrected2) = correct_timestamp(linear_corrector, base_time, base_time, target_offset)
  assert_eq(corrected2, base_time + target_offset)  // 完全校正
  assert_eq(corrector2.base_offset_ns, target_offset)
  
  // 场景3: 步进调整策略
  let step_config = CorrectionConfig::{
    strategy: StepAdjustment,
    max_correction_rate_ppm: 100.0,
    max_step_correction_ms: 50,
    correction_window_ms: 60000,
    enable_monotonicity: true
  }
  
  let step_corrector = TimestampCorrector::{
    config: step_config,
    base_offset_ns: 0L,
    correction_start_time: base_time,
    last_correction_time: base_time,
    total_corrected_ns: 0L
  }
  
  let (corrector3, corrected3) = correct_timestamp(step_corrector, base_time, base_time, target_offset)
  let max_step_ns = step_config.max_step_correction_ms * 1000000L
  assert_eq(corrector3.base_offset_ns <= max_step_ns, true)  // 限制在最大步进范围内
  
  // 场景4: 渐进校正策略（多次校正）
  let slew_corrector = TimestampCorrector::{
    config: correction_config,
    base_offset_ns: 0L,
    correction_start_time: base_time,
    last_correction_time: base_time,
    total_corrected_ns: 0L
  }
  
  // 第一次校正
  let (corrector4a, corrected4a) = correct_timestamp(slew_corrector, base_time, base_time, target_offset)
  assert_eq(corrector4a.base_offset_ns.abs() < target_offset.abs(), true)  // 部分校正
  
  // 第二次校正（1秒后）
  let (corrector4b, corrected4b) = correct_timestamp(corrector4a, base_time + 1000000000L, base_time + 1000000000L, target_offset)
  assert_eq(corrector4b.base_offset_ns.abs() > corrector4a.base_offset_ns.abs(), true)  // 校正量增加
  
  // 多次校正直到完全校正
  let mut current_corrector = corrector4b
  let mut current_time = base_time + 2000000000L
  let mut iteration = 0
  while iteration < 10 and current_corrector.base_offset_ns.abs() < target_offset.abs() {
    let (next_corrector, _) = correct_timestamp(current_corrector, base_time, current_time, target_offset)
    current_corrector = next_corrector
    current_time = current_time + 1000000000L  // 1秒间隔
    iteration = iteration + 1
  }
  
  assert_eq(current_corrector.base_offset_ns.abs() >= target_offset.abs() or iteration >= 10, true)
}

test "time_window_consistency" {
  // 测试时间窗口一致性
  
  // 1. 定义时间窗口
  struct TimeWindow {
    start_time_ns : Int64
    end_time_ns : Int64
    duration_ns : Int64
    is_closed : Bool
  }
  
  // 2. 定义事件时间戳
  struct TimestampedEvent {
    event_id : String
    timestamp_ns : Int64
    corrected_timestamp_ns : Int64
    local_clock_ns : Int64
    server_clock_ns : Int64?
  }
  
  // 3. 定义时间窗口验证器
  struct TimeWindowValidator {
    max_clock_skew_ns : Int64
    max_out_of_order_ns : Int64
    enable_reordering : Bool
    enable_gap_detection : Bool
  }
  
  // 4. 创建验证器配置
  let validator = TimeWindowValidator::{
    max_clock_skew_ns: 100000000L,    // 100ms
    max_out_of_order_ns: 50000000L,   // 50ms
    enable_reordering: true,
    enable_gap_detection: true
  }
  
  // 5. 实现时间窗口创建函数
  fn create_time_window(start_time_ns : Int64, duration_ns : Int64) -> TimeWindow {
    TimeWindow::{
      start_time_ns: start_time_ns,
      end_time_ns: start_time_ns + duration_ns,
      duration_ns: duration_ns,
      is_closed: false
    }
  }
  
  // 6. 实现事件验证函数
  fn validate_event_in_window(event : TimestampedEvent, window : TimeWindow, validator : TimeWindowValidator) -> (Bool, String) {
    let mut validation_errors = []
    
    // 检查事件是否在时间窗口内
    if event.corrected_timestamp_ns < window.start_time_ns {
      validation_errors.push("Event timestamp before window start")
    }
    
    if event.corrected_timestamp_ns > window.end_time_ns {
      validation_errors.push("Event timestamp after window end")
    }
    
    // 检查时钟偏移
    match event.server_clock_ns {
      Some(server_time) => {
        let skew = (event.corrected_timestamp_ns - server_time).abs()
        if skew > validator.max_clock_skew_ns {
          validation_errors.push("Clock skew exceeds threshold")
        }
      }
      None => {}
    }
    
    // 检查时间戳单调性
    if event.corrected_timestamp_ns < event.local_clock_ns - validator.max_out_of_order_ns {
      validation_errors.push("Timestamp violates monotonicity")
    }
    
    if validation_errors.length() == 0 {
      (true, "")
    } else {
      (false, join_strings(validation_errors, "; "))
    }
  }
  
  // 7. 实现时间窗口一致性检查函数
  fn check_window_consistency(events : Array[TimestampedEvent], window : TimeWindow, validator : TimeWindowValidator) -> (Bool, Array[String]) {
    let mut all_valid = true
    let mut validation_errors = []
    let mut sorted_events = events
    
    // 如果启用重排序，按时间戳排序
    if validator.enable_reordering {
      sorted_events = sort_events_by_timestamp(events)
    }
    
    // 检查每个事件
    let mut i = 0
    while i < sorted_events.length() {
      let event = sorted_events[i]
      let (is_valid, error) = validate_event_in_window(event, window, validator)
      
      if not is_valid {
        all_valid = false
        validation_errors.push("Event " + event.event_id + ": " + error)
      }
      
      i = i + 1
    }
    
    // 如果启用间隙检测，检查时间间隙
    if validator.enable_gap_detection and sorted_events.length() > 1 {
      let mut j = 1
      while j < sorted_events.length() {
        let prev_event = sorted_events[j - 1]
        let curr_event = sorted_events[j]
        let gap = curr_event.corrected_timestamp_ns - prev_event.corrected_timestamp_ns
        
        // 如果间隙过大，记录警告
        if gap > window.duration_ns / 10 {  // 超过窗口持续时间的10%
          validation_errors.push("Large gap detected between events " + prev_event.event_id + " and " + curr_event.event_id)
        }
        
        j = j + 1
      }
    }
    
    (all_valid, validation_errors)
  }
  
  // 8. 测试时间窗口一致性
  
  let base_time = 1640995200000000000L
  let window_duration = 60000000000L  // 1分钟
  let window = create_time_window(base_time, window_duration)
  
  // 场景1: 正常的事件序列
  let normal_events = [
    TimestampedEvent::{
      event_id: "event1",
      timestamp_ns: base_time + 1000000000L,    // 1秒
      corrected_timestamp_ns: base_time + 1000000000L,
      local_clock_ns: base_time + 1000000000L,
      server_clock_ns: Some(base_time + 1000050000L)  // 5ms偏移
    },
    TimestampedEvent::{
      event_id: "event2",
      timestamp_ns: base_time + 2000000000L,    // 2秒
      corrected_timestamp_ns: base_time + 2000000000L,
      local_clock_ns: base_time + 2000000000L,
      server_clock_ns: Some(base_time + 2000030000L)  // 3ms偏移
    },
    TimestampedEvent::{
      event_id: "event3",
      timestamp_ns: base_time + 3000000000L,    // 3秒
      corrected_timestamp_ns: base_time + 3000000000L,
      local_clock_ns: base_time + 3000000000L,
      server_clock_ns: Some(base_time + 3000010000L)  // 1ms偏移
    }
  ]
  
  let (valid1, errors1) = check_window_consistency(normal_events, window, validator)
  assert_eq(valid1, true)
  assert_eq(errors1.length(), 0)
  
  // 场景2: 包含时钟偏移过大的事件
  let skewed_events = [
    TimestampedEvent::{
      event_id: "event1",
      timestamp_ns: base_time + 1000000000L,
      corrected_timestamp_ns: base_time + 1000000000L,
      local_clock_ns: base_time + 1000000000L,
      server_clock_ns: Some(base_time + 1000000000L)
    },
    TimestampedEvent::{
      event_id: "event2",
      timestamp_ns: base_time + 2000000000L,
      corrected_timestamp_ns: base_time + 2000000000L,
      local_clock_ns: base_time + 2000000000L,
      server_clock_ns: Some(base_time + 2300000000L)  // 300ms偏移，超出阈值
    }
  ]
  
  let (valid2, errors2) = check_window_consistency(skewed_events, window, validator)
  assert_eq(valid2, false)
  assert_eq(errors2.length() > 0, true)
  assert_eq(errors2[0].contains("Clock skew"), true)
  
  // 场景3: 包含乱序事件
  let out_of_order_events = [
    TimestampedEvent::{
      event_id: "event1",
      timestamp_ns: base_time + 3000000000L,  // 3秒
      corrected_timestamp_ns: base_time + 3000000000L,
      local_clock_ns: base_time + 3000000000L,
      server_clock_ns: Some(base_time + 3000000000L)
    },
    TimestampedEvent::{
      event_id: "event2",
      timestamp_ns: base_time + 1000000000L,  // 1秒，但出现在后面
      corrected_timestamp_ns: base_time + 1000000000L,
      local_clock_ns: base_time + 1000000000L,
      server_clock_ns: Some(base_time + 1000000000L)
    },
    TimestampedEvent::{
      event_id: "event3",
      timestamp_ns: base_time + 2000000000L,  // 2秒
      corrected_timestamp_ns: base_time + 2000000000L,
      local_clock_ns: base_time + 2000000000L,
      server_clock_ns: Some(base_time + 2000000000L)
    }
  ]
  
  let (valid3, errors3) = check_window_consistency(out_of_order_events, window, validator)
  // 由于启用了重排序，乱序事件应该被正确处理
  assert_eq(valid3, true)
  
  // 场景4: 包含时间间隙的事件
  let gap_events = [
    TimestampedEvent::{
      event_id: "event1",
      timestamp_ns: base_time + 1000000000L,
      corrected_timestamp_ns: base_time + 1000000000L,
      local_clock_ns: base_time + 1000000000L,
      server_clock_ns: Some(base_time + 1000000000L)
    },
    TimestampedEvent::{
      event_id: "event2",
      timestamp_ns: base_time + 7000000000L,  // 7秒，与event1有6秒间隙
      corrected_timestamp_ns: base_time + 7000000000L,
      local_clock_ns: base_time + 7000000000L,
      server_clock_ns: Some(base_time + 7000000000L)
    }
  ]
  
  let (valid4, errors4) = check_window_consistency(gap_events, window, validator)
  // 事件本身有效，但应该检测到间隙
  assert_eq(valid4, true)
  assert_eq(errors4.length() > 0, true)
  assert_eq(errors4[0].contains("Large gap"), true)
}

// 辅助函数
fn calculate_mean(values : Array[Int64]) -> Double {
  let mut sum = 0.0
  let mut i = 0
  while i < values.length() {
    sum = sum + values[i].to_double()
    i = i + 1
  }
  sum / values.length().to_double()
}

fn calculate_variance(values : Array[Int64], mean : Double) -> Double {
  let mut sum_sq_diff = 0.0
  let mut i = 0
  while i < values.length() {
    let diff = values[i].to_double() - mean
    sum_sq_diff = sum_sq_diff + (diff * diff)
    i = i + 1
  }
  sum_sq_diff / values.length().to_double()
}

fn sqrt(x : Double) -> Double {
  // 简化的平方根实现
  if x == 0.0 {
    return 0.0
  }
  
  let mut guess = x / 2.0
  let mut i = 0
  while i < 10 {
    guess = (guess + x / guess) / 2.0
    i = i + 1
  }
  guess
}

fn sort_events_by_timestamp(events : Array[TimestampedEvent]) -> Array[TimestampedEvent] {
  // 简化的排序实现（冒泡排序）
  let sorted = []
  let mut i = 0
  while i < events.length() {
    sorted.push(events[i])
    i = i + 1
  }
  
  let mut n = sorted.length()
  let mut swapped = true
  while swapped and n > 1 {
    swapped = false
    let mut i = 1
    while i < n {
      if sorted[i-1].corrected_timestamp_ns > sorted[i].corrected_timestamp_ns {
        let temp = sorted[i-1]
        sorted[i-1] = sorted[i]
        sorted[i] = temp
        swapped = true
      }
      i = i + 1
    }
    n = n - 1
  }
  
  sorted
}

fn join_strings(strings : Array[String], separator : String) -> String {
  if strings.length() == 0 {
    return ""
  }
  
  let mut result = strings[0]
  let mut i = 1
  while i < strings.length() {
    result = result + separator + strings[i]
    i = i + 1
  }
  result
}