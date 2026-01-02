// Azimuth Complex Aggregation Algorithms Test Suite
// 复杂聚合算法测试用例

// 测试1: 分层聚合算法
test "分层聚合算法测试" {
  // 聚合层级
  enum AggregationLevel {
    Raw
    Minute
    Hour
    Day
    Week
    Month
  }
  
  // 聚合配置
  struct AggregationConfig {
    level : AggregationLevel
    window_size : Int64  // 纳秒
    max_points : Int
  }
  
  fn AggregationConfig::new(level : AggregationLevel, window_size : Int64, max_points : Int) -> AggregationConfig {
    AggregationConfig::{ level, window_size, max_points }
  }
  
  // 聚合数据点
  struct AggregationPoint {
    timestamp : Int64
    count : Int
    sum : Double
    min : Double
    max : Double
    avg : Double
    level : AggregationLevel
  }
  
  fn AggregationPoint::new(timestamp : Int64, level : AggregationLevel) -> AggregationPoint {
    AggregationPoint::{
      timestamp,
      count: 0,
      sum: 0.0,
      min: 999999999.0,  // 大数
      max: -999999999.0, // 小数
      avg: 0.0,
      level
    }
  }
  
  fn AggregationPoint::add_value(point : AggregationPoint, value : Double) -> AggregationPoint {
    let new_count = point.count + 1
    let new_sum = point.sum + value
    let new_min = if value < point.min { value } else { point.min }
    let new_max = if value > point.max { value } else { point.max }
    let new_avg = new_sum / new_count.to_double()
    
    AggregationPoint::{
      timestamp: point.timestamp,
      count: new_count,
      sum: new_sum,
      min: new_min,
      max: new_max,
      avg: new_avg,
      level: point.level
    }
  }
  
  // 分层聚合器
  struct HierarchicalAggregator {
    raw_data : Array[(Int64, Double)]
    minute_data : Array[AggregationPoint]
    hour_data : Array[AggregationPoint]
    day_data : Array[AggregationPoint]
    configs : Array[(AggregationLevel, AggregationConfig)]
  }
  
  fn HierarchicalAggregator::new() -> HierarchicalAggregator {
    let configs = [
      (AggregationLevel::Minute, AggregationConfig::new(AggregationLevel::Minute, 60000000000L, 1440)),    // 1分钟，最多1440个点（24小时）
      (AggregationLevel::Hour, AggregationConfig::new(AggregationLevel::Hour, 3600000000000L, 168)),       // 1小时，最多168个点（7天）
      (AggregationLevel::Day, AggregationConfig::new(AggregationLevel::Day, 86400000000000L, 30))         // 1天，最多30个点（1个月）
    ]
    
    HierarchicalAggregator::{
      raw_data: [],
      minute_data: [],
      hour_data: [],
      day_data: [],
      configs
    }
  }
  
  fn HierarchicalAggregator::add_data_point(aggregator : HierarchicalAggregator, timestamp : Int64, value : Double) -> HierarchicalAggregator {
    let updated_raw = aggregator.raw_data.push((timestamp, value))
    
    // 更新分钟级聚合
    let updated_minute = update_level_aggregation(updated_raw, aggregator.minute_data, AggregationLevel::Minute, 60000000000L)
    
    // 更新小时级聚合
    let updated_hour = update_level_aggregation(updated_raw, aggregator.hour_data, AggregationLevel::Hour, 3600000000000L)
    
    // 更新天级聚合
    let updated_day = update_level_aggregation(updated_raw, aggregator.day_data, AggregationLevel::Day, 86400000000000L)
    
    HierarchicalAggregator::{
      raw_data: updated_raw,
      minute_data: updated_minute,
      hour_data: updated_hour,
      day_data: updated_day,
      configs: aggregator.configs
    }
  }
  
  fn update_level_aggregation(raw_data : Array[(Int64, Double)], level_data : Array[AggregationPoint], level : AggregationLevel, window_size : Int64) -> Array[AggregationPoint] {
    if raw_data.length() == 0 {
      return level_data
    }
    
    let (min_time, max_time) = get_time_range(raw_data)
    let mut start_time = floor_to_window(min_time, window_size)
    let mut updated_data = level_data
    
    while start_time <= max_time {
      let end_time = start_time + window_size
      
      // 查找或创建聚合点
      match find_aggregation_point(updated_data, start_time) {
        Some(index) => {
          // 更新现有聚合点
          let point = updated_data[index]
          let window_values = get_values_in_window(raw_data, start_time, end_time)
          let updated_point = update_aggregation_point_with_values(point, window_values)
          updated_data[index] = updated_point
        }
        None => {
          // 创建新聚合点
          let window_values = get_values_in_window(raw_data, start_time, end_time)
          if window_values.length() > 0 {
            let new_point = create_aggregation_point_from_values(start_time, level, window_values)
            updated_data = updated_data.push(new_point)
          }
        }
      }
      
      start_time = start_time + window_size
    }
    
    // 按时间戳排序并限制点数
    let sorted_data = updated_data.sort_by(fn(a, b) { 
      if a.timestamp < b.timestamp { -1 } 
      else if a.timestamp > b.timestamp { 1 } 
      else { 0 } 
    })
    
    let max_points = match level {
      AggregationLevel::Minute => 1440
      AggregationLevel::Hour => 168
      AggregationLevel::Day => 30
      _ => 1000
    }
    
    if sorted_data.length() > max_points {
      sorted_data.slice(sorted_data.length() - max_points, sorted_data.length())
    } else {
      sorted_data
    }
  }
  
  fn get_time_range(data : Array[(Int64, Double)]) -> (Int64, Int64) {
    match data {
      [] => (0L, 0L)
      [head, ..tail] => {
        let mut min_time = head.0
        let mut max_time = head.0
        
        for (timestamp, _) in tail {
          if timestamp < min_time {
            min_time = timestamp
          }
          if timestamp > max_time {
            max_time = timestamp
          }
        }
        
        (min_time, max_time)
      }
    }
  }
  
  fn floor_to_window(timestamp : Int64, window_size : Int64) -> Int64 {
    (timestamp / window_size) * window_size
  }
  
  fn find_aggregation_point(data : Array[AggregationPoint], timestamp : Int64) -> Option[Int] {
    for i in 0..data.length() {
      if data[i].timestamp == timestamp {
        return Some(i)
      }
    }
    None
  }
  
  fn get_values_in_window(raw_data : Array[(Int64, Double)], start_time : Int64, end_time : Int64) -> Array[Double] {
    raw_data.filter(fn(pair) { 
      let timestamp = pair.0
      timestamp >= start_time && timestamp < end_time 
    }).map(fn(pair) { pair.1 })
  }
  
  fn update_aggregation_point_with_values(point : AggregationPoint, values : Array[Double]) -> AggregationPoint {
    let mut updated_point = point
    
    for value in values {
      updated_point = AggregationPoint::add_value(updated_point, value)
    }
    
    updated_point
  }
  
  fn create_aggregation_point_from_values(timestamp : Int64, level : AggregationLevel, values : Array[Double]) -> AggregationPoint {
    let mut point = AggregationPoint::new(timestamp, level)
    
    for value in values {
      point = AggregationPoint::add_value(point, value)
    }
    
    point
  }
  
  // 测试分层聚合
  let aggregator = HierarchicalAggregator::new()
  let base_time = 1640995200000000000L  // 2022-01-01 00:00:00 UTC
  
  // 添加测试数据（每分钟一个点，持续3小时）
  let mut test_aggregator = aggregator
  for i in 0..180 {  // 3小时 = 180分钟
    let timestamp = base_time + (i * 60000000000L)  // 每分钟
    let value = 10.0 + (i % 60).to_double()  // 10-69之间的值
    test_aggregator = HierarchicalAggregator::add_data_point(test_aggregator, timestamp, value)
  }
  
  // 验证原始数据
  assert_eq(test_aggregator.raw_data.length(), 180)
  
  // 验证分钟级聚合
  assert_eq(test_aggregator.minute_data.length(), 180)
  assert_eq(test_aggregator.minute_data[0].level, AggregationLevel::Minute)
  assert_eq(test_aggregator.minute_data[0].count, 1)
  assert_eq(test_aggregator.minute_data[0].avg, 10.0)
  
  // 验证小时级聚合
  assert_eq(test_aggregator.hour_data.length(), 3)  // 3小时
  assert_eq(test_aggregator.hour_data[0].level, AggregationLevel::Hour)
  assert_eq(test_aggregator.hour_data[0].count, 60)  // 每小时60分钟
  assert_true(test_aggregator.hour_data[0].avg > 30.0 && test_aggregator.hour_data[0].avg < 40.0)  // 平均值应该在合理范围内
  
  // 验证天级聚合
  assert_eq(test_aggregator.day_data.length(), 1)  // 1天
  assert_eq(test_aggregator.day_data[0].level, AggregationLevel::Day)
  assert_eq(test_aggregator.day_data[0].count, 180)  // 3小时 = 180分钟
  assert_true(test_aggregator.day_data[0].avg > 30.0 && test_aggregator.day_data[0].avg < 40.0)
}

// 测试2: 近似聚合算法
test "近似聚合算法测试" {
  // HyperLogLog 结构（简化版）
  struct HyperLogLog {
    registers : Array[Int]
    precision : Int
    alpha : Double
  }
  
  fn HyperLogLog::new(precision : Int) -> HyperLogLog {
    let m = 1 << precision  // 2^precision 个寄存器
    let alpha = match precision {
      4 => 0.673
      5 => 0.697
      6 => 0.709
      _ => 0.7213 / (1.0 + 1.079 / m.to_double())
    }
    
    HyperLogLog::{
      registers: Array::filled(m, 0),
      precision,
      alpha
    }
  }
  
  fn HyperLogLog::add(hll : HyperLogLog, item : String) -> HyperLogLog {
    // 简化的哈希函数
    let hash = simple_hash(item)
    let index = hash % (1 << hll.precision)
    let rank = count_leading_zeros(hash >> hll.precision) + 1
    
    let mut updated_registers = hll.registers
    if rank > updated_registers[index] {
      updated_registers[index] = rank
    }
    
    HyperLogLog::{
      registers: updated_registers,
      precision: hll.precision,
      alpha: hll.alpha
    }
  }
  
  fn HyperLogLog::estimate(hll : HyperLogLog) -> Double {
    let m = hll.registers.length().to_double()
    let mut sum = 0.0
    
    for register in hll.registers {
      sum = sum + (1.0 / (1.0 << register).to_double())
    }
    
    let estimate = hll.alpha * m * m / sum
    
    // 小范围修正
    if estimate <= 2.5 * m {
      let zeros = hll.registers.count(fn(r) { r == 0 }).to_double()
      if zeros > 0.0 {
        m * (m / zeros).log()
      } else {
        estimate
      }
    } else {
      estimate
    }
  }
  
  fn HyperLogLog::merge(hll1 : HyperLogLog, hll2 : HyperLogLog) -> HyperLogLog {
    if hll1.precision != hll2.precision {
      panic("Cannot merge HyperLogLog with different precision")
    }
    
    let mut merged_registers = []
    for i in 0..hll1.registers.length() {
      merged_registers = merged_registers.push(max(hll1.registers[i], hll2.registers[i]))
    }
    
    HyperLogLog::{
      registers: merged_registers,
      precision: hll1.precision,
      alpha: hll1.alpha
    }
  }
  
  // KLL 草稿结构（简化版）
  struct KLLSketch {
    k : Int
    compactors : Array[Array[Double]]
    total_count : Int
  }
  
  fn KLLSketch::new(k : Int) -> KLLSketch {
    KLLSketch::{
      k,
      compactors: [[]],
      total_count: 0
    }
  }
  
  fn KLLSketch::add(sketch : KLLSketch, value : Double) -> KLLSketch {
    let mut updated_compactors = sketch.compactors
    updated_compactors[0] = updated_compactors[0].push(value)
    
    // 压缩逻辑
    let mut level = 0
    while updated_compactors[level].length() >= sketch.k * (1 << level) {
      if level + 1 >= updated_compactors.length() {
        updated_compactors = updated_compactors.push([])
      }
      
      // 排序并压缩
      let sorted = updated_compactors[level].sort_by(fn(a, b) { 
        if a < b { -1 } else if a > b { 1 } else { 0 } 
      })
      
      let mut compacted = []
      for i in 0..sorted.length() {
        if i % 2 == 0 {
          compacted = compacted.push(sorted[i])
        }
      }
      
      updated_compactors[level] = compacted
      updated_compactors[level + 1] = updated_compactors[level + 1].concat(updated_compactors[level])
      level = level + 1
    }
    
    KLLSketch::{
      k: sketch.k,
      compactors: updated_compactors,
      total_count: sketch.total_count + 1
    }
  }
  
  fn KLLSketch::quantile(sketch : KLLSketch, q : Double) -> Double {
    if sketch.total_count == 0 {
      return 0.0
    }
    
    let rank = (q * sketch.total_count.to_double()).to_int()
    let mut all_values = []
    
    for compactor in sketch.compactors {
      all_values = all_values.concat(compactor)
    }
    
    let sorted = all_values.sort_by(fn(a, b) { 
      if a < b { -1 } else if a > b { 1 } else { 0 } 
    })
    
    if rank >= sorted.length() {
      sorted[sorted.length() - 1]
    } else {
      sorted[rank]
    }
  }
  
  // 辅助函数
  fn simple_hash(s : String) -> Int {
    let mut hash = 0
    for char in s.chars() {
      hash = ((hash << 5) - hash) + char.to_int()
    }
    if hash < 0 { hash * -1 } else { hash }
  }
  
  fn count_leading_zeros(n : Int) -> Int {
    if n == 0 {
      return 32  // 假设32位整数
    }
    
    let mut count = 0
    let mut value = n
    while (value & 0x80000000) == 0 && count < 32 {
      count = count + 1
      value = value << 1
    }
    count
  }
  
  // 测试 HyperLogLog
  let hll = HyperLogLog::new(10)  // 1024个寄存器
  let mut test_hll = hll
  
  // 添加1000个唯一元素
  for i in 0..1000 {
    test_hll = HyperLogLog::add(test_hll, "item-" + i.to_string())
  }
  
  let estimate = HyperLogLog::estimate(test_hll)
  assert_true(estimate > 900.0 && estimate < 1100.0)  // 应该在真实值的10%范围内
  
  // 添加重复元素
  for i in 0..500 {
    test_hll = HyperLogLog::add(test_hll, "item-" + i.to_string())  // 重复前500个
  }
  
  let estimate_with_duplicates = HyperLogLog::estimate(test_hll)
  assert_true(estimate_with_duplicates > 900.0 && estimate_with_duplicates < 1100.0)  // 估计值应该变化不大
  
  // 测试 HyperLogLog 合并
  let hll1 = HyperLogLog::new(10)
  let hll2 = HyperLogLog::new(10)
  
  let mut test_hll1 = hll1
  let mut test_hll2 = hll2
  
  for i in 0..500 {
    test_hll1 = HyperLogLog::add(test_hll1, "set1-" + i.to_string())
  }
  
  for i in 250..750 {
    test_hll2 = HyperLogLog::add(test_hll2, "set2-" + i.to_string())
  }
  
  let merged_hll = HyperLogLog::merge(test_hll1, test_hll2)
  let merged_estimate = HyperLogLog::estimate(merged_hll)
  
  // 两个集合的并集应该有大约750个唯一元素
  assert_true(merged_estimate > 650.0 && merged_estimate < 850.0)
  
  // 测试 KLL 草稿
  let kll = KLLSketch::new(100)
  let mut test_kll = kll
  
  // 添加正态分布数据（简化版）
  for i in 0..10000 {
    let value = 50.0 + ((i % 200).to_double() - 100.0) * 0.5  // 简化的正态分布
    test_kll = KLLSketch::add(test_kll, value)
  }
  
  // 测试分位数
  let p50 = KLLSketch::quantile(test_kll, 0.5)  // 中位数
  let p95 = KLLSketch::quantile(test_kll, 0.95)  // 95%分位数
  let p99 = KLLSketch::quantile(test_kll, 0.99)  // 99%分位数
  
  assert_true(p50 > 45.0 && p50 < 55.0)  // 中位数应该在50附近
  assert_true(p95 > 90.0)              // 95%分位数应该较高
  assert_true(p99 > p95)               // 99%分位数应该高于95%分位数
}

// 测试3: 滑动窗口聚合算法
test "滑动窗口聚合算法测试" {
  // 滑动窗口统计
  struct SlidingWindowStats {
    sum : Double
    count : Int
    min : Double
    max : Double
    variance : Double
  }
  
  fn SlidingWindowStats::new() -> SlidingWindowStats {
    SlidingWindowStats::{
      sum: 0.0,
      count: 0,
      min: 999999999.0,
      max: -999999999.0,
      variance: 0.0
    }
  }
  
  // 滑动窗口
  struct SlidingWindow {
    max_size : Int
    values : Array[Double]
    sum : Double
    sum_squares : Double  // 用于计算方差
  }
  
  fn SlidingWindow::new(max_size : Int) -> SlidingWindow {
    SlidingWindow::{
      max_size,
      values: [],
      sum: 0.0,
      sum_squares: 0.0
    }
  }
  
  fn SlidingWindow::add(window : SlidingWindow, value : Double) -> SlidingWindow {
    let mut updated_values = window.values
    let mut updated_sum = window.sum
    let mut updated_sum_squares = window.sum_squares
    
    // 如果窗口已满，移除最旧的值
    if updated_values.length() >= window.max_size {
      let old_value = updated_values[0]
      updated_values = updated_values.slice(1, updated_values.length())
      updated_sum = updated_sum - old_value
      updated_sum_squares = updated_sum_squares - (old_value * old_value)
    }
    
    // 添加新值
    updated_values = updated_values.push(value)
    updated_sum = updated_sum + value
    updated_sum_squares = updated_sum_squares + (value * value)
    
    SlidingWindow::{
      max_size: window.max_size,
      values: updated_values,
      sum: updated_sum,
      sum_squares: updated_sum_squares
    }
  }
  
  fn SlidingWindow::get_stats(window : SlidingWindow) -> SlidingWindowStats {
    match window.values {
      [] => SlidingWindowStats::new()
      [head, ..] => {
        let count = window.values.length()
        let mean = window.sum / count.to_double()
        
        let mut min = head
        let mut max = head
        
        for value in window.values {
          if value < min {
            min = value
          }
          if value > max {
            max = value
          }
        }
        
        let variance = if count > 1 {
          (window.sum_squares - (window.sum * window.sum / count.to_double())) / (count - 1).to_double()
        } else {
          0.0
        }
        
        SlidingWindowStats::{
          sum: window.sum,
          count,
          min,
          max,
          variance
        }
      }
    }
  }
  
  // 指数加权移动平均
  struct ExponentialMovingAverage {
    alpha : Double
    current_value : Option[Double]
  }
  
  fn ExponentialMovingAverage::new(alpha : Double) -> ExponentialMovingAverage {
    ExponentialMovingAverage::{ alpha, current_value: None }
  }
  
  fn ExponentialMovingAverage::update(ema : ExponentialMovingAverage, value : Double) -> ExponentialMovingAverage {
    let updated_value = match ema.current_value {
      None => value
      Some(current) => ema.alpha * value + (1.0 - ema.alpha) * current
    }
    
    ExponentialMovingAverage::{
      alpha: ema.alpha,
      current_value: Some(updated_value)
    }
  }
  
  fn ExponentialMovingAverage::get(ema : ExponentialMovingAverage) -> Option[Double] {
    ema.current_value
  }
  
  // 分位数滑动窗口
  struct QuantileSlidingWindow {
    window : SlidingWindow
    sorted_values : Array[Double>
  }
  
  fn QuantileSlidingWindow::new(max_size : Int) -> QuantileSlidingWindow {
    QuantileSlidingWindow::{
      window: SlidingWindow::new(max_size),
      sorted_values: []
    }
  }
  
  fn QuantileSlidingWindow::add(q_window : QuantileSlidingWindow, value : Double) -> QuantileSlidingWindow {
    let updated_window = SlidingWindow::add(q_window.window, value)
    
    // 重新构建排序数组
    let updated_sorted = updated_window.values.sort_by(fn(a, b) { 
      if a < b { -1 } else if a > b { 1 } else { 0 } 
    })
    
    QuantileSlidingWindow::{
      window: updated_window,
      sorted_values: updated_sorted
    }
  }
  
  fn QuantileSlidingWindow::quantile(q_window : QuantileSlidingWindow, q : Double) -> Option[Double] {
    if q_window.sorted_values.length() == 0 {
      return None
    }
    
    let index = (q * (q_window.sorted_values.length() - 1).to_double()).to_int()
    Some(q_window.sorted_values[index])
  }
  
  // 测试滑动窗口
  let window = SlidingWindow::new(10)
  let mut test_window = window
  
  // 添加10个值
  for i in 1..11 {
    test_window = SlidingWindow::add(test_window, i.to_double())
  }
  
  assert_eq(test_window.values.length(), 10)
  assert_eq(test_window.values[0], 2.0)  // 1应该被移除
  assert_eq(test_window.values[9], 10.0)
  
  let stats = SlidingWindow::get_stats(test_window)
  assert_eq(stats.count, 10)
  assert_eq(stats.sum, 65.0)  // 2+3+...+10 = 65
  assert_eq(stats.min, 2.0)
  assert_eq(stats.max, 10.0)
  
  // 再添加一个值
  test_window = SlidingWindow::add(test_window, 11.0)
  assert_eq(test_window.values.length(), 10)
  assert_eq(test_window.values[0], 3.0)  // 2应该被移除
  assert_eq(test_window.values[9], 11.0)
  
  let stats2 = SlidingWindow::get_stats(test_window)
  assert_eq(stats2.sum, 75.0)  // 3+4+...+11 = 75
  
  // 测试指数加权移动平均
  let ema = ExponentialMovingAverage::new(0.2)
  let mut test_ema = ema
  
  test_ema = ExponentialMovingAverage::update(test_ema, 10.0)
  assert_eq(ExponentialMovingAverage::get(test_ema), Some(10.0))
  
  test_ema = ExponentialMovingAverage::update(test_ema, 20.0)
  let ema_value1 = ExponentialMovingAverage::get(test_ema).unwrap()
  assert_true(ema_value1 > 10.0 && ema_value1 < 20.0)  // 应该在10和20之间
  
  test_ema = ExponentialMovingAverage::update(test_ema, 30.0)
  let ema_value2 = ExponentialMovingAverage::get(test_ema).unwrap()
  assert_true(ema_value2 > ema_value1)  // 应该继续增长
  
  // 测试分位数滑动窗口
  let q_window = QuantileSlidingWindow::new(100)
  let mut test_q_window = q_window
  
  // 添加100个值（0-99）
  for i in 0..100 {
    test_q_window = QuantileSlidingWindow::add(test_q_window, i.to_double())
  }
  
  let p50 = QuantileSlidingWindow::quantile(test_q_window, 0.5).unwrap()
  let p95 = QuantileSlidingWindow::quantile(test_q_window, 0.95).unwrap()
  let p99 = QuantileSlidingWindow::quantile(test_q_window, 0.99).unwrap()
  
  assert_eq(p50, 49.5)  // 中位数
  assert_eq(p95, 94.05)  // 95%分位数
  assert_eq(p99, 98.01)  // 99%分位数
  
  // 添加更多值，测试滑动行为
  for i in 100..200 {
    test_q_window = QuantileSlidingWindow::add(test_q_window, i.to_double())
  }
  
  let p50_after = QuantileSlidingWindow::quantile(test_q_window, 0.5).unwrap()
  let p95_after = QuantileSlidingWindow::quantile(test_q_window, 0.95).unwrap()
  
  assert_eq(p50_after, 149.5)  // 新的中位数（100-199）
  assert_eq(p95_after, 194.05)  // 新的95%分位数
}

// 测试4: 分布式聚合算法
test "分布式聚合算法测试" {
  // 分布式节点
  struct DistributedNode {
    id : String
    data : Array[Double]
    partial_aggregate : Option[PartialAggregate]
  }
  
  // 部分聚合结果
  struct PartialAggregate {
    count : Int
    sum : Double
    sum_squares : Double
    min : Double
    max : Double
    quantiles : Array[(Double, Double)]  // (quantile, value)
  }
  
  fn PartialAggregate::new() -> PartialAggregate {
    PartialAggregate::{
      count: 0,
      sum: 0.0,
      sum_squares: 0.0,
      min: 999999999.0,
      max: -999999999.0,
      quantiles: []
    }
  }
  
  fn PartialAggregate::add_value(agg : PartialAggregate, value : Double) -> PartialAggregate {
    let new_count = agg.count + 1
    let new_sum = agg.sum + value
    let new_sum_squares = agg.sum_squares + (value * value)
    let new_min = if value < agg.min { value } else { agg.min }
    let new_max = if value > agg.max { value } else { agg.max }
    
    PartialAggregate::{
      count: new_count,
      sum: new_sum,
      sum_squares: new_sum_squares,
      min: new_min,
      max: new_max,
      quantiles: agg.quantiles
    }
  }
  
  // 分布式聚合器
  struct DistributedAggregator {
    nodes : Array[DistributedNode]
    final_result : Option[PartialAggregate]
  }
  
  fn DistributedAggregator::new() -> DistributedAggregator {
    DistributedAggregator::{
      nodes: [],
      final_result: None
    }
  }
  
  fn DistributedAggregator::add_node(aggregator : DistributedAggregator, node_id : String, data : Array[Double]) -> DistributedAggregator {
    let node = DistributedNode::{
      id: node_id,
      data,
      partial_aggregate: None
    }
    
    DistributedAggregator::{
      nodes: aggregator.nodes.push(node),
      final_result: aggregator.final_result
    }
  }
  
  fn DistributedAggregator::compute_partial_aggregates(aggregator : DistributedAggregator) -> DistributedAggregator {
    let mut updated_nodes = []
    
    for node in aggregator.nodes {
      let mut partial_agg = PartialAggregate::new()
      
      for value in node.data {
        partial_agg = PartialAggregate::add_value(partial_agg, value)
      }
      
      updated_nodes = updated_nodes.push(DistributedNode::{
        id: node.id,
        data: node.data,
        partial_aggregate: Some(partial_agg)
      })
    }
    
    DistributedAggregator::{
      nodes: updated_nodes,
      final_result: aggregator.final_result
    }
  }
  
  fn DistributedAggregator::merge_aggregates(aggregator : DistributedAggregator) -> DistributedAggregator {
    let mut final_agg = PartialAggregate::new()
    
    for node in aggregator.nodes {
      match node.partial_aggregate {
        Some(partial) => {
          let combined = PartialAggregate::{
            count: final_agg.count + partial.count,
            sum: final_agg.sum + partial.sum,
            sum_squares: final_agg.sum_squares + partial.sum_squares,
            min: if final_agg.min < partial.min { final_agg.min } else { partial.min },
            max: if final_agg.max > partial.max { final_agg.max } else { partial.max },
            quantiles: []  // 简化：不合并分位数
          }
          final_agg = combined
        }
        None => ()
      }
    }
    
    DistributedAggregator::{
      nodes: aggregator.nodes,
      final_result: Some(final_agg)
    }
  }
  
  fn DistributedAggregator::get_global_stats(aggregator : DistributedAggregator) -> Option[(Double, Double, Double, Double, Double)] {
    match aggregator.final_result {
      None => None
      Some(agg) => {
        if agg.count == 0 {
          None
        } else {
          let mean = agg.sum / agg.count.to_double()
          let variance = if agg.count > 1 {
            (agg.sum_squares - (agg.sum * agg.sum / agg.count.to_double())) / (agg.count - 1).to_double()
          } else {
            0.0
          }
          
          Some((mean, variance, agg.min, agg.max, agg.count.to_double()))
        }
      }
    }
  }
  
  // 测试分布式聚合
  let aggregator = DistributedAggregator::new()
  
  // 创建3个节点的数据
  let node1_data = []
  for i in 0..100 {
    node1_data = node1_data.push((i * 0.1) + 10.0)  // 10.0 到 19.9
  }
  
  let node2_data = []
  for i in 0..150 {
    node2_data = node2_data.push((i * 0.2) + 20.0)  // 20.0 到 49.8
  }
  
  let node3_data = []
  for i in 0..80 {
    node3_data = node3_data.push((i * 0.5) + 50.0)  // 50.0 到 89.5
  }
  
  let aggregator1 = DistributedAggregator::add_node(aggregator, "node-1", node1_data)
  let aggregator2 = DistributedAggregator::add_node(aggregator1, "node-2", node2_data)
  let aggregator3 = DistributedAggregator::add_node(aggregator2, "node-3", node3_data)
  
  assert_eq(aggregator3.nodes.length(), 3)
  
  // 计算部分聚合
  let aggregator4 = DistributedAggregator::compute_partial_aggregates(aggregator3)
  
  // 验证部分聚合结果
  let node1_agg = aggregator4.nodes[0].partial_aggregate.unwrap()
  assert_eq(node1_agg.count, 100)
  assert_eq(node1_agg.min, 10.0)
  assert_eq(node1_agg.max, 19.9)
  
  let node2_agg = aggregator4.nodes[1].partial_aggregate.unwrap()
  assert_eq(node2_agg.count, 150)
  assert_eq(node2_agg.min, 20.0)
  assert_eq(node2_agg.max, 49.8)
  
  let node3_agg = aggregator4.nodes[2].partial_aggregate.unwrap()
  assert_eq(node3_agg.count, 80)
  assert_eq(node3_agg.min, 50.0)
  assert_eq(node3_agg.max, 89.5)
  
  // 合并聚合结果
  let aggregator5 = DistributedAggregator::merge_aggregates(aggregator4)
  
  // 验证全局统计
  let global_stats = DistributedAggregator::get_global_stats(aggregator5).unwrap()
  let (mean, variance, min, max, count) = global_stats
  
  assert_eq(count, 330.0)  // 100 + 150 + 80
  assert_eq(min, 10.0)
  assert_eq(max, 89.5)
  assert_true(mean > 35.0 && mean < 45.0)  // 平均值应该在合理范围内
  assert_true(variance > 0.0)  // 方差应该为正
}

// 测试5: 自适应聚合算法
test "自适应聚合算法测试" {
  // 聚合策略
  enum AggregationStrategy {
    Fixed(Int)        // 固定窗口大小
    TimeBased(Int64)  // 基于时间的窗口
    Adaptive(Double)  // 自适应，基于数据变化率
    Lossy(Double)     // 有损聚合，基于精度要求
  }
  
  // 自适应聚合器
  struct AdaptiveAggregator {
    strategy : AggregationStrategy
    data_points : Array[(Int64, Double)]
    window_size : Int
    last_variance : Double
    precision_threshold : Double
    compression_ratio : Double
  }
  
  fn AdaptiveAggregator::new(strategy : AggregationStrategy) -> AdaptiveAggregator {
    AdaptiveAggregator::{
      strategy,
      data_points: [],
      window_size: 100,
      last_variance: 0.0,
      precision_threshold: 0.01,
      compression_ratio: 0.5
    }
  }
  
  fn AdaptiveAggregator::add_point(aggregator : AdaptiveAggregator, timestamp : Int64, value : Double) -> AdaptiveAggregator {
    let updated_data = aggregator.data_points.push((timestamp, value))
    let mut updated_aggregator = AdaptiveAggregator::{
      strategy: aggregator.strategy,
      data_points: updated_data,
      window_size: aggregator.window_size,
      last_variance: aggregator.last_variance,
      precision_threshold: aggregator.precision_threshold,
      compression_ratio: aggregator.compression_ratio
    }
    
    // 根据策略调整窗口大小
    match aggregator.strategy {
      AggregationStrategy::Adaptive(change_threshold) => {
        if updated_data.length() >= 10 {
          let current_variance = calculate_variance(updated_data.slice(updated_data.length() - 10, updated_data.length()))
          let variance_change = (current_variance - aggregator.last_variance).abs() / aggregator.last_variance
          
          if variance_change > change_threshold {
            // 数据变化大，减小窗口以保持精度
            updated_aggregator.window_size = max(10, aggregator.window_size / 2)
          } else if variance_change < change_threshold / 10.0 {
            // 数据变化小，增大窗口以提高效率
            updated_aggregator.window_size = min(1000, aggregator.window_size * 2)
          }
          
          updated_aggregator.last_variance = current_variance
        }
      }
      AggregationStrategy::Lossy(precision) => {
        if updated_data.length() >= aggregator.window_size {
          // 使用有损压缩
          let compressed = lossy_compress(updated_data, precision)
          updated_aggregator.data_points = compressed
        }
      }
      _ => ()
    }
    
    updated_aggregator
  }
  
  fn calculate_variance(data : Array[(Int64, Double)]) -> Double {
    if data.length() == 0 {
      return 0.0
    }
    
    let values = data.map(fn(pair) { pair.1 })
    let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
    let mean = sum / values.length().to_double()
    
    let sum_squares = values.reduce(fn(acc, v) { acc + (v - mean) * (v - mean) }, 0.0)
    sum_squares / values.length().to_double()
  }
  
  fn lossy_compress(data : Array[(Int64, Double)], precision : Double) -> Array[(Int64, Double)] {
    if data.length() == 0 {
      return data
    }
    
    let mut compressed = []
    let mut last_value = data[0].1
    compressed = compressed.push(data[0])
    
    for i in 1..data.length() {
      let current_value = data[i].1
      let relative_change = (current_value - last_value).abs() / last_value.abs()
      
      if relative_change >= precision {
        compressed = compressed.push(data[i])
        last_value = current_value
      }
    }
    
    compressed
  }
  
  fn AdaptiveAggregator::get_aggregated_data(aggregator : AdaptiveAggregator) -> Array[(Int64, Double)] {
    match aggregator.strategy {
      AggregationStrategy::Fixed(size) => {
        if aggregator.data_points.length() <= size {
          aggregator.data_points
        } else {
          aggregator.data_points.slice(aggregator.data_points.length() - size, aggregator.data_points.length())
        }
      }
      AggregationStrategy::TimeBased(window_ms) => {
        if aggregator.data_points.length() == 0 {
          return []
        }
        
        let latest_time = aggregator.data_points[aggregator.data_points.length() - 1].0
        let window_start = latest_time - window_ms
        
        aggregator.data_points.filter(fn(pair) { pair.0 >= window_start })
      }
      _ => aggregator.data_points
    }
  }
  
  // 测试自适应聚合
  let adaptive_aggregator = AdaptiveAggregator::new(AggregationStrategy::Adaptive(0.1))
  let base_time = 1640995200000000000L
  let mut test_aggregator = adaptive_aggregator
  
  // 添加稳定数据
  for i in 0..50 {
    let timestamp = base_time + (i * 1000000000L)
    let value = 10.0 + (i % 5).to_double() * 0.1  // 小变化
    test_aggregator = AdaptiveAggregator::add_point(test_aggregator, timestamp, value)
  }
  
  // 稳定数据应该导致窗口增大
  assert_true(test_aggregator.window_size >= 100)
  
  // 添加变化数据
  for i in 50..100 {
    let timestamp = base_time + (i * 1000000000L)
    let value = 50.0 + (i * 10.0)  // 大变化
    test_aggregator = AdaptiveAggregator::add_point(test_aggregator, timestamp, value)
  }
  
  // 变化数据应该导致窗口减小
  assert_true(test_aggregator.window_size < 100)
  
  // 测试有损聚合
  let lossy_aggregator = AdaptiveAggregator::new(AggregationStrategy::Lossy(0.05))
  let mut test_lossy = lossy_aggregator
  
  // 添加变化很小的数据
  for i in 0..100 {
    let timestamp = base_time + (i * 1000000000L)
    let value = 100.0 + (i % 10).to_double() * 0.001  // 非常小的变化
    test_lossy = AdaptiveAggregator::add_point(test_lossy, timestamp, value)
  }
  
  let compressed_data = AdaptiveAggregator::get_aggregated_data(test_lossy)
  assert_true(compressed_data.length() < 100)  // 数据应该被压缩
  
  // 测试固定窗口
  let fixed_aggregator = AdaptiveAggregator::new(AggregationStrategy::Fixed(20))
  let mut test_fixed = fixed_aggregator
  
  for i in 0..50 {
    let timestamp = base_time + (i * 1000000000L)
    let value = i.to_double()
    test_fixed = AdaptiveAggregator::add_point(test_fixed, timestamp, value)
  }
  
  let fixed_data = AdaptiveAggregator::get_aggregated_data(test_fixed)
  assert_eq(fixed_data.length(), 20)  // 应该只保留最新的20个点
  assert_eq(fixed_data[0].1, 30.0)   // 第一个值应该是30
  assert_eq(fixed_data[19].1, 49.0)  // 最后一个值应该是49
  
  // 测试时间窗口
  let time_aggregator = AdaptiveAggregator::new(AggregationStrategy::TimeBased(10000000000L))  // 10秒窗口
  let mut test_time = time_aggregator
  
  for i in 0..30 {
    let timestamp = base_time + (i * 1000000000L)  // 每秒一个点
    let value = i.to_double()
    test_time = AdaptiveAggregator::add_point(test_time, timestamp, value)
  }
  
  let time_data = AdaptiveAggregator::get_aggregated_data(test_time)
  assert_eq(time_data.length(), 10)  // 应该只有10秒内的数据
  assert_eq(time_data[0].1, 20.0)   // 第一个值应该是20
  assert_eq(time_data[9].1, 29.0)   // 最后一个值应该是29
}