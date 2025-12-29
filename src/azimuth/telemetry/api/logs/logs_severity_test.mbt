// Logs API 日志严重性级别测试用例
// 测试日志记录的严重性级别处理和过滤功能

test "logs_severity_level_hierarchy" {
  // 测试日志严重性级别的层次结构
  
  // 验证严重性级别的数值顺序
  assert_eq(Trace as Int64, 0L)
  assert_eq(Debug as Int64, 1L)
  assert_eq(Info as Int64, 2L)
  assert_eq(Warn as Int64, 3L)
  assert_eq(Error as Int64, 4L)
  assert_eq(Fatal as Int64, 5L)
  
  // 测试严重性级别比较
  fn is_more_severe(level1 : SeverityNumber, level2 : SeverityNumber) -> Bool {
    (level1 as Int64) > (level2 as Int64)
  }
  
  fn is_less_severe(level1 : SeverityNumber, level2 : SeverityNumber) -> Bool {
    (level1 as Int64) < (level2 as Int64)
  }
  
  // 验证严重性级别比较逻辑
  assert_eq(is_more_severe(Debug, Trace), true)
  assert_eq(is_more_severe(Info, Debug), true)
  assert_eq(is_more_severe(Warn, Info), true)
  assert_eq(is_more_severe(Error, Warn), true)
  assert_eq(is_more_severe(Fatal, Error), true)
  
  assert_eq(is_less_severe(Trace, Debug), true)
  assert_eq(is_less_severe(Debug, Info), true)
  assert_eq(is_less_severe(Info, Warn), true)
  assert_eq(is_less_severe(Warn, Error), true)
  assert_eq(is_less_severe(Error, Fatal), true)
  
  // 测试相同严重性级别
  assert_eq(is_more_severe(Info, Info), false)
  assert_eq(is_less_severe(Error, Error), false)
}

test "logs_severity_filtering" {
  // 测试基于严重性级别的日志过滤
  
  // 模拟日志记录
  struct LogEntry {
    severity : SeverityNumber
    message : String
    timestamp : Int64
  }
  
  // 创建不同严重性级别的日志记录
  let log_entries = [
    LogEntry::{ severity: Trace, message: "Trace message", timestamp: 1640995200000L },
    LogEntry::{ severity: Debug, message: "Debug message", timestamp: 1640995210000L },
    LogEntry::{ severity: Info, message: "Info message", timestamp: 1640995220000L },
    LogEntry::{ severity: Warn, message: "Warning message", timestamp: 1640995230000L },
    LogEntry::{ severity: Error, message: "Error message", timestamp: 1640995240000L },
    LogEntry::{ severity: Fatal, message: "Fatal message", timestamp: 1640995250000L }
  ]
  
  // 过滤函数：只保留指定级别及以上的日志
  fn filter_by_min_level(entries : Array[LogEntry], min_level : SeverityNumber) -> Array[LogEntry] {
    let filtered = []
    let mut i = 0
    while i < entries.length() {
      let entry = entries[i]
      if (entry.severity as Int64) >= (min_level as Int64) {
        filtered.push(entry)
      }
      i = i + 1
    }
    filtered
  }
  
  // 测试不同最小级别的过滤
  let warn_and_above = filter_by_min_level(log_entries, Warn)
  let error_and_above = filter_by_min_level(log_entries, Error)
  let info_and_above = filter_by_min_level(log_entries, Info)
  
  // 验证过滤结果
  assert_eq(warn_and_above.length(), 3)  // Warn, Error, Fatal
  assert_eq(error_and_above.length(), 2)  // Error, Fatal
  assert_eq(info_and_above.length(), 4)   // Info, Warn, Error, Fatal
  
  // 验证过滤结果的严重性级别
  assert_eq(warn_and_above[0].severity, Warn)
  assert_eq(warn_and_above[1].severity, Error)
  assert_eq(warn_and_above[2].severity, Fatal)
  
  assert_eq(error_and_above[0].severity, Error)
  assert_eq(error_and_above[1].severity, Fatal)
  
  assert_eq(info_and_above[0].severity, Info)
  assert_eq(info_and_above[1].severity, Warn)
  assert_eq(info_and_above[2].severity, Error)
  assert_eq(info_and_above[3].severity, Fatal)
}

test "logs_severity_text_mapping" {
  // 测试严重性级别与文本的映射
  
  // 严重性级别到文本的映射函数
  fn severity_to_text(severity : SeverityNumber) -> String {
    match severity {
      Trace => "TRACE"
      Debug => "DEBUG"
      Info => "INFO"
      Warn => "WARN"
      Error => "ERROR"
      Fatal => "FATAL"
    }
  }
  
  // 文本到严重性级别的映射函数
  fn text_to_severity(text : String) -> SeverityNumber? {
    match text {
      "TRACE" => Some(Trace)
      "DEBUG" => Some(Debug)
      "INFO" => Some(Info)
      "WARN" => Some(Warn)
      "ERROR" => Some(Error)
      "FATAL" => Some(Fatal)
      _ => None
    }
  }
  
  // 测试严重性级别到文本的映射
  assert_eq(severity_to_text(Trace), "TRACE")
  assert_eq(severity_to_text(Debug), "DEBUG")
  assert_eq(severity_to_text(Info), "INFO")
  assert_eq(severity_to_text(Warn), "WARN")
  assert_eq(severity_to_text(Error), "ERROR")
  assert_eq(severity_to_text(Fatal), "FATAL")
  
  // 测试文本到严重性级别的映射
  match text_to_severity("TRACE") {
    Some(severity) => assert_eq(severity, Trace)
    None => @test.fail("Expected Some(Trace)")
  }
  
  match text_to_severity("DEBUG") {
    Some(severity) => assert_eq(severity, Debug)
    None => @test.fail("Expected Some(Debug)")
  }
  
  match text_to_severity("INFO") {
    Some(severity) => assert_eq(severity, Info)
    None => @test.fail("Expected Some(Info)")
  }
  
  match text_to_severity("WARN") {
    Some(severity) => assert_eq(severity, Warn)
    None => @test.fail("Expected Some(Warn)")
  }
  
  match text_to_severity("ERROR") {
    Some(severity) => assert_eq(severity, Error)
    None => @test.fail("Expected Some(Error)")
  }
  
  match text_to_severity("FATAL") {
    Some(severity) => assert_eq(severity, Fatal)
    None => @test.fail("Expected Some(Fatal)")
  }
  
  // 测试无效文本
  match text_to_severity("INVALID") {
    Some(_) => @test.fail("Expected None for invalid text")
    None => assert_eq(true, true)
  }
  
  match text_to_severity("info") {
    Some(_) => @test.fail("Expected None for lowercase text")
    None => assert_eq(true, true)
  }
}

test "logs_severity_statistics" {
  // 测试日志严重性级别统计
  
  // 模拟日志记录
  struct LogEntry {
    severity : SeverityNumber
    message : String
    timestamp : Int64
  }
  
  // 创建大量不同严重性级别的日志记录
  let log_entries = [
    LogEntry::{ severity: Info, message: "Info 1", timestamp: 1640995200000L },
    LogEntry::{ severity: Warn, message: "Warning 1", timestamp: 1640995210000L },
    LogEntry::{ severity: Info, message: "Info 2", timestamp: 1640995220000L },
    LogEntry::{ severity: Error, message: "Error 1", timestamp: 1640995230000L },
    LogEntry::{ severity: Debug, message: "Debug 1", timestamp: 1640995240000L },
    LogEntry::{ severity: Info, message: "Info 3", timestamp: 1640995250000L },
    LogEntry::{ severity: Warn, message: "Warning 2", timestamp: 1640995260000L },
    LogEntry::{ severity: Error, message: "Error 2", timestamp: 1640995270000L },
    LogEntry::{ severity: Fatal, message: "Fatal 1", timestamp: 1640995280000L },
    LogEntry::{ severity: Info, message: "Info 4", timestamp: 1640995290000L }
  ]
  
  // 统计各级别日志数量
  let mut trace_count = 0L
  let mut debug_count = 0L
  let mut info_count = 0L
  let mut warn_count = 0L
  let mut error_count = 0L
  let mut fatal_count = 0L
  
  let mut i = 0
  while i < log_entries.length() {
    let entry = log_entries[i]
    match entry.severity {
      Trace => trace_count = trace_count + 1L
      Debug => debug_count = debug_count + 1L
      Info => info_count = info_count + 1L
      Warn => warn_count = warn_count + 1L
      Error => error_count = error_count + 1L
      Fatal => fatal_count = fatal_count + 1L
    }
    i = i + 1
  }
  
  // 验证统计结果
  assert_eq(trace_count, 0L)
  assert_eq(debug_count, 1L)
  assert_eq(info_count, 4L)
  assert_eq(warn_count, 2L)
  assert_eq(error_count, 2L)
  assert_eq(fatal_count, 1L)
  
  // 验证总数
  let total_count = trace_count + debug_count + info_count + warn_count + error_count + fatal_count
  assert_eq(total_count, 10L)
  assert_eq(total_count, @int.from_uint(log_entries.length()))
  
  // 计算各级别百分比
  let info_percentage = @double.from_int(info_count) / @double.from_int(total_count) * 100.0
  let warn_percentage = @double.from_int(warn_count) / @double.from_int(total_count) * 100.0
  let error_percentage = @double.from_int(error_count) / @double.from_int(total_count) * 100.0
  let fatal_percentage = @double.from_int(fatal_count) / @double.from_int(total_count) * 100.0
  
  assert_eq(info_percentage, 40.0)
  assert_eq(warn_percentage, 20.0)
  assert_eq(error_percentage, 20.0)
  assert_eq(fatal_percentage, 10.0)
  
  // 验证错误相关日志（Error + Fatal）比例
  let error_related_percentage = (error_percentage + fatal_percentage)
  assert_eq(error_related_percentage, 30.0)
}

test "logs_severity_threshold_alerting" {
  // 测试基于严重性级别的阈值告警
  
  // 模拟告警配置
  struct AlertThreshold {
    severity : SeverityNumber
    count_threshold : Int64
    time_window_ms : Int64
  }
  
  // 创建告警阈值配置
  let alert_thresholds = [
    AlertThreshold::{ severity: Error, count_threshold: 3L, time_window_ms: 60000L },
    AlertThreshold::{ severity: Fatal, count_threshold: 1L, time_window_ms: 30000L },
    AlertThreshold::{ severity: Warn, count_threshold: 5L, time_window_ms: 120000L }
  ]
  
  // 模拟日志记录
  struct LogEntry {
    severity : SeverityNumber
    message : String
    timestamp : Int64
  }
  
  // 创建测试日志记录（在时间窗口内）
  let base_time = 1640995200000L
  let log_entries = [
    LogEntry::{ severity: Error, message: "Error 1", timestamp: base_time + 1000L },
    LogEntry::{ severity: Error, message: "Error 2", timestamp: base_time + 2000L },
    LogEntry::{ severity: Error, message: "Error 3", timestamp: base_time + 3000L },
    LogEntry::{ severity: Fatal, message: "Fatal 1", timestamp: base_time + 4000L },
    LogEntry::{ severity: Warn, message: "Warning 1", timestamp: base_time + 5000L },
    LogEntry::{ severity: Warn, message: "Warning 2", timestamp: base_time + 6000L }
  ]
  
  // 检查告警函数
  fn check_alert(entries : Array[LogEntry], threshold : AlertThreshold, current_time : Int64) -> Bool {
    let mut count = 0L
    let window_start = current_time - threshold.time_window_ms
    
    let mut i = 0
    while i < entries.length() {
      let entry = entries[i]
      if entry.timestamp >= window_start && (entry.severity as Int64) >= (threshold.severity as Int64) {
        count = count + 1L
      }
      i = i + 1
    }
    
    count >= threshold.count_threshold
  }
  
  // 测试告警检查
  let current_time = base_time + 30000L
  
  // 检查Error级别告警（3个Error日志，阈值3）
  let error_alert = check_alert(log_entries, alert_thresholds[0], current_time)
  assert_eq(error_alert, true)
  
  // 检查Fatal级别告警（1个Fatal日志，阈值1）
  let fatal_alert = check_alert(log_entries, alert_thresholds[1], current_time)
  assert_eq(fatal_alert, true)
  
  // 检查Warn级别告警（2个Warn日志，阈值5）
  let warn_alert = check_alert(log_entries, alert_thresholds[2], current_time)
  assert_eq(warn_alert, false)
  
  // 测试时间窗口外的情况
  let out_of_window_time = base_time + 180000L  // 3分钟后
  let error_alert_out_of_window = check_alert(log_entries, alert_thresholds[0], out_of_window_time)
  assert_eq(error_alert_out_of_window, false)
  
  let fatal_alert_out_of_window = check_alert(log_entries, alert_thresholds[1], out_of_window_time)
  assert_eq(fatal_alert_out_of_window, false)
}

test "logs_severity_level_conversion" {
  // 测试不同严重性级别系统的转换
  
  // 模拟其他日志系统的严重性级别
  enum OtherSeverity {
    VERBOSE  // 对应Trace
    DEBUG    // 对应Debug
    INFO     // 对应Info
    WARNING  // 对应Warn
    ERROR    // 对应Error
    CRITICAL // 对应Fatal
  }
  
  // 从其他系统转换到OpenTelemetry严重性级别
  fn from_other_severity(other : OtherSeverity) -> SeverityNumber {
    match other {
      VERBOSE => Trace
      DEBUG => Debug
      INFO => Info
      WARNING => Warn
      ERROR => Error
      CRITICAL => Fatal
    }
  }
  
  // 转换到其他系统的严重性级别
  fn to_other_severity(otel : SeverityNumber) -> OtherSeverity {
    match otel {
      Trace => VERBOSE
      Debug => DEBUG
      Info => INFO
      Warn => WARNING
      Error => ERROR
      Fatal => CRITICAL
    }
  }
  
  // 测试双向转换
  let other_severities = [VERBOSE, DEBUG, INFO, WARNING, ERROR, CRITICAL]
  let otel_severities = [Trace, Debug, Info, Warn, Error, Fatal]
  
  // 测试从其他系统转换
  let mut i = 0
  while i < other_severities.length() {
    let other = other_severities[i]
    let otel = from_other_severity(other)
    assert_eq(otel as Int64, i)  // 验证转换后的级别顺序
    i = i + 1
  }
  
  // 测试转换到其他系统
  let mut j = 0
  while j < otel_severities.length() {
    let otel = otel_severities[j]
    let other = to_other_severity(otel)
    
    // 验证往返转换的一致性
    let round_trip = from_other_severity(other)
    assert_eq(round_trip as Int64, otel as Int64)
    j = j + 1
  }
  
  // 测试数值级别转换
  fn from_numeric_level(level : Int64) -> SeverityNumber {
    if level <= 0 {
      Trace
    } else if level <= 4 {
      Debug
    } else if level <= 7 {
      Info
    } else if level <= 12 {
      Warn
    } else if level <= 16 {
      Error
    } else {
      Fatal
    }
  }
  
  // 测试数值级别转换
  assert_eq(from_numeric_level(0), Trace)
  assert_eq(from_numeric_level(3), Debug)
  assert_eq(from_numeric_level(5), Info)
  assert_eq(from_numeric_level(10), Warn)
  assert_eq(from_numeric_level(15), Error)
  assert_eq(from_numeric_level(20), Fatal)
}