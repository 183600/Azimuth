// Azimuth Data Sampling Strategy Tests
// 数据采样策略测试用例 - 专注于各种数据采样算法和策略

// Test 1: 基础随机采样
test "basic random sampling" {
  let sampler = RandomSampler::new(42) // 使用固定种子确保可重现性
  
  // 创建测试数据
  let test_data = []
  for i in 0..<1000 {
    test_data = test_data.push({
      "id": i,
      "value": i * 2,
      "category": "category_" + (i % 10).to_string(),
      "timestamp": Time::now() + i
    })
  }
  
  // 测试简单随机采样
  let simple_sample = RandomSampler::simple_random(sampler, test_data, 100) // 采样100个
  assert_eq(simple_sample.length(), 100)
  
  // 验证采样数据是原始数据的子集
  for item in simple_sample {
    assert_true(test_data.contains(item))
  }
  
  // 测试无重复采样
  let unique_sample = RandomSampler::sample_without_replacement(sampler, test_data, 50)
  assert_eq(unique_sample.length(), 50)
  
  // 验证无重复
  let mut seen_ids = []
  for item in unique_sample {
    assert_false(seen_ids.contains(item.id))
    seen_ids = seen_ids.push(item.id)
  }
  
  // 测试有重复采样
  let with_replacement_sample = RandomSampler::sample_with_replacement(sampler, test_data, 50)
  assert_eq(with_replacement_sample.length(), 50)
  
  // 验证可能有重复
  let mut seen_ids_replacement = []
  let mut has_duplicates = false
  for item in with_replacement_sample {
    if seen_ids_replacement.contains(item.id) {
      has_duplicates = true
      break
    }
    seen_ids_replacement = seen_ids_replacement.push(item.id)
  }
  // 不保证一定有重复，但有可能
  
  // 测试分层随机采样
  let strata = test_data.group_by(fn(item) { item.category })
  let stratified_sample = RandomSampler::stratified_random(sampler, strata, 50) // 每层采样50个
  assert_true(stratified_sample.length() > 0)
  
  // 验证每个类别都有代表
  let sampled_categories = stratified_sample.map(fn(item) { item.category }).unique()
  assert_true(sampled_categories.length() > 1)
  
  // 测试加权随机采样
  let weighted_data = test_data.map(fn(item) { 
    { "item": item, "weight": item.value.to_float() } 
  })
  let weighted_sample = RandomSampler::weighted_random(sampler, weighted_data, 100)
  assert_eq(weighted_sample.length(), 100)
  
  // 验证权重较高的项更有可能被选中
  let high_value_items = weighted_sample.filter(fn(item) { item.item.value > 1000 })
  let high_value_ratio = high_value_items.length().to_float() / weighted_sample.length().to_float()
  let original_high_ratio = test_data.filter(fn(item) { item.value > 1000 }).length().to_float() / test_data.length().to_float()
  
  // 加权采样中高价值项的比例应该更高或相等
  assert_true(high_value_ratio >= original_high_ratio)
}

// Test 2: 系统采样
test "systematic sampling" {
  let systematic_sampler = SystematicSampler::new()
  
  // 创建有序测试数据
  let ordered_data = []
  for i in 0..<1000 {
    ordered_data = ordered_data.push({
      "id": i,
      "timestamp": Time::now() + i * 60, // 每分钟一个数据点
      "value": (i * 3) % 100
    })
  }
  
  // 测试等距系统采样
  let interval_sample = SystematicSampler::interval_sampling(systematic_sampler, ordered_data, 100)
  assert_eq(interval_sample.length(), 100)
  
  // 验证采样间隔
  let expected_interval = ordered_data.length() / 100
  for i in 1..<interval_sample.length() {
    let prev_index = ordered_data.index_of(interval_sample[i-1])
    let curr_index = ordered_data.index_of(interval_sample[i])
    assert_true(curr_index - prev_index >= expected_interval - 1)
    assert_true(curr_index - prev_index <= expected_interval + 1)
  }
  
  // 测试时间基系统采样
  let time_based_sample = SystematicSampler::time_based_sampling(systematic_sampler, ordered_data, 300) // 每5分钟采样
  assert_true(time_based_sample.length() > 0)
  
  // 验证时间间隔
  for i in 1..<time_based_sample.length() {
    let prev_time = time_based_sample[i-1].timestamp
    let curr_time = time_based_sample[i].timestamp
    assert_true(curr_time - prev_time >= 250) // 允许一些误差
    assert_true(curr_time - prev_time <= 350)
  }
  
  // 测试周期性系统采样
  let periodic_sample = SystematicSampler::periodic_sampling(systematic_sampler, ordered_data, 50)
  assert_eq(periodic_sample.length(), 50)
  
  // 验证周期性
  let expected_period = ordered_data.length() / 50
  for i in 1..<periodic_sample.length() {
    let prev_index = ordered_data.index_of(periodic_sample[i-1])
    let curr_index = ordered_data.index_of(periodic_sample[i])
    assert_true(curr_index - prev_index >= expected_period - 1)
    assert_true(curr_index - prev_index <= expected_period + 1)
  }
  
  // 测试偏移系统采样
  let offset_sample = SystematicSampler::offset_sampling(systematic_sampler, ordered_data, 100, 10) // 偏移10
  assert_eq(offset_sample.length(), 100)
  
  // 验证偏移效果
  let first_index = ordered_data.index_of(offset_sample[0])
  assert_eq(first_index, 10)
}

// Test 3: 分层采样
test "stratified sampling" {
  let stratified_sampler = StratifiedSampler::new()
  
  // 创建具有不同类别的测试数据
  let categories = ["A", "B", "C", "D", "E"]
  let test_data = []
  
  for i in 0..<1000 {
    let category = categories[i % categories.length()]
    test_data = test_data.push({
      "id": i,
      "category": category,
      "value": Random::next() % 100,
      "importance": if category == "A" { "high" } else if category == "B" { "medium" } else { "low" }
    })
  }
  
  // 按类别分组
  let strata = test_data.group_by(fn(item) { item.category })
  
  // 测试比例分层采样
  let proportional_sample = StratifiedSampler::proportional_sampling(stratified_sampler, strata, 200)
  assert_eq(proportional_sample.length(), 200)
  
  // 验证比例保持
  for category in categories {
    let original_count = strata[category].length()
    let original_ratio = original_count.to_float() / test_data.length().to_float()
    
    let sampled_count = proportional_sample.filter(fn(item) { item.category == category }).length()
    let sampled_ratio = sampled_count.to_float() / proportional_sample.length().to_float()
    
    // 允许一些采样误差
    assert_true((sampled_ratio - original_ratio).abs() < 0.05)
  }
  
  // 测试等大小分层采样
  let equal_size_sample = StratifiedSampler::equal_size_sampling(stratified_sampler, strata, 50)
  assert_eq(equal_size_sample.length(), categories.length() * 50)
  
  // 验证每层大小相等
  for category in categories {
    let category_count = equal_size_sample.filter(fn(item) { item.category == category }).length()
    assert_eq(category_count, 50)
  }
  
  // 测试最优分配分层采样
  let optimal_sample = StratifiedSampler::optimal_allocation_sampling(
    stratified_sampler, 
    strata, 
    200, 
    fn(items) { items.map(fn(item) { item.value }).standard_deviation() }
  )
  assert_eq(optimal_sample.length(), 200)
  
  // 验证方差较大的层获得更多样本
  let a_samples = optimal_sample.filter(fn(item) { item.category == "A" }).length()
  let c_samples = optimal_sample.filter(fn(item) { item.category == "C" }).length()
  
  // 由于A类别可能有更高的方差，它应该获得更多样本
  assert_true(a_samples >= c_samples)
  
  // 测试多级分层采样
  let multi_strata = test_data.group_by(fn(item) { item.importance })
  let multi_level_sample = StratifiedSampler::multi_level_sampling(stratified_sampler, multi_strata, 100)
  assert_eq(multi_level_sample.length(), 100)
  
  // 验证每个重要性级别都有代表
  let importance_levels = ["high", "medium", "low"]
  for level in importance_levels {
    let level_count = multi_level_sample.filter(fn(item) { item.importance == level }).length()
    assert_true(level_count > 0)
  }
}

// Test 4: 自适应采样
test "adaptive sampling" {
  let adaptive_sampler = AdaptiveSampler::new()
  
  // 创建具有不同特征的测试数据
  let test_data = []
  for i in 0..<1000 {
    let value = if i < 100 { 
      // 前100个数据点有异常值
      if i % 10 == 0 { 1000 + i } else { i }
    } else {
      // 正常数据
      Random::next() % 100
    }
    
    test_data = test_data.push({
      "id": i,
      "value": value,
      "timestamp": Time::now() + i,
      "is_anomaly": i < 100 && i % 10 == 0
    })
  }
  
  // 配置自适应采样器
  AdaptiveSampler::configure_anomaly_detection(adaptive_sampler, {
    "threshold": 2.0, // 2个标准差
    "window_size": 50
  })
  
  AdaptiveSampler::configure_variance_detection(adaptive_sampler, {
    "threshold": 100.0, // 方差阈值
    "window_size": 100
  })
  
  // 测试基于异常检测的自适应采样
  let anomaly_sample = AdaptiveSampler::anomaly_based_sampling(adaptive_sampler, test_data, 150)
  assert_eq(anomaly_sample.length(), 150)
  
  // 验证异常数据被优先采样
  let anomaly_in_sample = anomaly_sample.filter(fn(item) { item.is_anomaly })
  let anomaly_ratio = anomaly_in_sample.length().to_float() / anomaly_sample.length().to_float()
  let original_anomaly_ratio = test_data.filter(fn(item) { item.is_anomaly }).length().to_float() / test_data.length().to_float()
  
  assert_true(anomaly_ratio > original_anomaly_ratio * 5) // 异常数据在采样中的比例应该显著提高
  
  // 测试基于方差的自适应采样
  let variance_sample = AdaptiveSampler::variance_based_sampling(adaptive_sampler, test_data, 150)
  assert_eq(variance_sample.length(), 150)
  
  // 验证高方差区域被更多采样
  let high_variance_items = variance_sample.filter(fn(item) { item.value > 500 })
  assert_true(high_variance_items.length() > 0)
  
  // 测试基于频率的自适应采样
  let frequency_sample = AdaptiveSampler::frequency_based_sampling(adaptive_sampler, test_data, 150)
  assert_eq(frequency_sample.length(), 150)
  
  // 测试基于趋势的自适应采样
  let trend_sample = AdaptiveSampler::trend_based_sampling(adaptive_sampler, test_data, 150)
  assert_eq(trend_sample.length(), 150)
  
  // 测试多因素自适应采样
  let multi_factor_sample = AdaptiveSampler::multi_factor_sampling(adaptive_sampler, test_data, 150, {
    "anomaly_weight": 0.4,
    "variance_weight": 0.3,
    "frequency_weight": 0.2,
    "trend_weight": 0.1
  })
  assert_eq(multi_factor_sample.length(), 150)
  
  // 验证多因素采样效果
  let multi_factor_anomalies = multi_factor_sample.filter(fn(item) { item.is_anomaly })
  let multi_factor_anomaly_ratio = multi_factor_anomalies.length().to_float() / multi_factor_sample.length().to_float()
  
  assert_true(multi_factor_anomaly_ratio > original_anomaly_ratio)
  
  // 测试动态调整采样率
  let dynamic_sample = AdaptiveSampler::dynamic_rate_sampling(adaptive_sampler, test_data, {
    "initial_rate": 0.1,
    "max_rate": 0.5,
    "adjustment_factor": 1.5,
    "target_quality": 0.8
  })
  
  assert_true(dynamic_sample.length() > 0)
  assert_true(dynamic_sample.length() < test_data.length())
  
  // 获取自适应采样统计
  let sampling_stats = AdaptiveSampler::get_sampling_statistics(adaptive_sampler)
  assert_true(sampling_stats.anomaly_detection_rate > 0)
  assert_true(sampling_stats.variance_adjustment_count > 0)
  assert_true(sampling_stats.final_sampling_rate > 0)
}

// Test 5: 流数据采样
test "stream data sampling" {
  let stream_sampler = StreamSampler::new()
  
  // 配置流采样器
  StreamSampler::configure_reservoir(stream_sampler, 100) // 水库大小为100
  StreamSampler::configure_time_window(stream_sampler, 5000) // 5秒时间窗口
  StreamSampler::configure_count_window(stream_sampler, 200) // 计数窗口为200
  
  // 模拟流数据
  let stream_data = []
  for i in 0..<1000 {
    let data_point = {
      "id": i,
      "timestamp": Time::now() + i * 10, // 每10ms一个数据点
      "value": (i * 7) % 100,
      "priority": if i % 10 == 0 { "high" } else { "normal" }
    }
    stream_data = stream_data.push(data_point)
  }
  
  // 测试水库采样
  StreamSampler::start_reservoir_sampling(stream_sampler)
  for data in stream_data {
    StreamSampler::add_to_reservoir(stream_sampler, data)
  }
  
  let reservoir_sample = StreamSampler::get_reservoir_sample(stream_sampler)
  assert_eq(reservoir_sample.length(), 100)
  
  // 验证水库采样的无偏性
  let early_ids = reservoir_sample.filter(fn(item) { item.id < 200 }).length()
  let middle_ids = reservoir_sample.filter(fn(item) { item.id >= 200 && item.id < 800 }).length()
  let late_ids = reservoir_sample.filter(fn(item) { item.id >= 800 }).length()
  
  // 每个时间段应该有大致相等的样本数
  assert_true((early_ids - middle_ids).abs() <= 20)
  assert_true((middle_ids - late_ids).abs() <= 20)
  
  // 测试时间窗口采样
  StreamSampler::start_time_window_sampling(stream_sampler)
  for data in stream_data {
    StreamSampler::add_to_time_window(stream_sampler, data)
  }
  
  let time_window_sample = StreamSampler::get_time_window_sample(stream_sampler)
  assert_true(time_window_sample.length() > 0)
  
  // 验证时间窗口内的数据
  let min_time = time_window_sample.map(fn(item) { item.timestamp }).min()
  let max_time = time_window_sample.map(fn(item) { item.timestamp }).max()
  
  assert_true(max_time - min_time <= 5000) // 应该在5秒窗口内
  
  // 测试计数窗口采样
  StreamSampler::start_count_window_sampling(stream_sampler)
  for data in stream_data {
    StreamSampler::add_to_count_window(stream_sampler, data)
  }
  
  let count_window_sample = StreamSampler::get_count_window_sample(stream_sampler)
  assert_eq(count_window_sample.length(), 200)
  
  // 测试优先级采样
  StreamSampler::start_priority_sampling(stream_sampler)
  for data in stream_data {
    StreamSampler::add_to_priority_sampler(stream_sampler, data, fn(item) { 
      if item.priority == "high" { 1.0 } else { 0.1 }
    })
  }
  
  let priority_sample = StreamSampler::get_priority_sample(stream_sampler)
  assert_true(priority_sample.length() > 0)
  
  // 验证高优先级项被更多采样
  let high_priority_in_sample = priority_sample.filter(fn(item) { item.priority == "high" }).length()
  let high_priority_ratio = high_priority_in_sample.to_float() / priority_sample.length().to_float()
  let original_high_ratio = stream_data.filter(fn(item) { item.priority == "high" }).length().to_float() / stream_data.length().to_float()
  
  assert_true(high_priority_ratio > original_high_ratio)
  
  // 测试分层流采样
  StreamSampler::start_stratified_sampling(stream_sampler, fn(item) { item.priority })
  for data in stream_data {
    StreamSampler::add_to_stratified_sampler(stream_sampler, data)
  }
  
  let stratified_sample = StreamSampler::get_stratified_sample(stream_sampler)
  assert_true(stratified_sample.length() > 0)
  
  // 验证每个优先级都有代表
  let high_priority_count = stratified_sample.filter(fn(item) { item.priority == "high" }).length()
  let normal_priority_count = stratified_sample.filter(fn(item) { item.priority == "normal" }).length()
  
  assert_true(high_priority_count > 0)
  assert_true(normal_priority_count > 0)
  
  // 获取流采样统计
  let stream_stats = StreamSampler::get_statistics(stream_sampler)
  assert_true(stream_stats.total_processed == 1000)
  assert_true(stream_stats.reservoir_size == 100)
  assert_true(stream_stats.time_window_samples > 0)
  assert_true(stream_stats.priority_sampling_rate > 0)
}

// Test 6: 时间序列采样
test "time series sampling" {
  let time_series_sampler = TimeSeriesSampler::new()
  
  // 创建时间序列数据
  let start_time = Time::now()
  let time_series = []
  
  for i in 0..<1440 { // 24小时，每分钟一个数据点
    let timestamp = start_time + i * 60 // 每分钟
    let value = 50 + 10 * ((2 * 3.14159 * i / 1440).sin()) + (Random::next() % 10 - 5) // 正弦波加噪声
    let seasonal_value = 20 * ((2 * 3.14159 * i / (24 * 60)).sin()) // 日季节性
    
    time_series = time_series.push({
      "timestamp": timestamp,
      "value": value,
      "seasonal": seasonal_value,
      "trend": i * 0.01 // 轻微上升趋势
    })
  }
  
  // 测试均匀时间采样
  let uniform_sample = TimeSeriesSampler::uniform_time_sampling(time_series_sampler, time_series, 60) // 每小时一个点
  assert_eq(uniform_sample.length(), 24)
  
  // 验证时间间隔
  for i in 1..<uniform_sample.length() {
    let prev_time = uniform_sample[i-1].timestamp
    let curr_time = uniform_sample[i].timestamp
    assert_true(curr_time - prev_time >= 3000) // 至少50分钟
    assert_true(curr_time - prev_time <= 4200) // 最多70分钟
  }
  
  // 测试基于重要性的时间采样
  let importance_sample = TimeSeriesSampler::importance_based_sampling(time_series_sampler, time_series, 100, fn(point) {
    // 基于变化率的重要性
    if point.index > 0 {
      let prev_value = time_series[point.index - 1].value
      (point.value - prev_value).abs()
    } else {
      0.0
    }
  })
  
  assert_eq(importance_sample.length(), 100)
  
  // 验证变化大的点被更多采样
  let changes = []
  for i in 1..<importance_sample.length() {
    let curr_value = importance_sample[i].value
    let prev_value = importance_sample[i-1].value
    changes = changes.push((curr_value - prev_value).abs())
  }
  
  let avg_change = changes.reduce(fn(acc, change) { acc + change }, 0.0) / changes.length().to_float()
  let overall_avg_change = time_series.map(fn(point) { 
    if point.index > 0 { 
      (point.value - time_series[point.index - 1].value).abs() 
    } else { 
      0.0 
    }
  }).reduce(fn(acc, change) { acc + change }, 0.0) / (time_series.length() - 1).to_float()
  
  assert_true(avg_change > overall_avg_change)
  
  // 测试基于模式的时间采样
  let pattern_sample = TimeSeriesSampler::pattern_based_sampling(time_series_sampler, time_series, 120)
  assert_eq(pattern_sample.length(), 120)
  
  // 测试异常检测时间采样
  let anomaly_sample = TimeSeriesSampler::anomaly_based_sampling(time_series_sampler, time_series, 50, 2.0)
  assert_eq(anomaly_sample.length(), 50)
  
  // 测试多尺度时间采样
  let multi_scale_sample = TimeSeriesSampler::multi_scale_sampling(time_series_sampler, time_series, {
    "hourly": 24,
    "daily": 7,
    "weekly": 4
  })
  
  assert_eq(multi_scale_sample.hourly.length(), 24)
  assert_eq(multi_scale_sample.daily.length(), 7)
  assert_eq(multi_scale_sample.weekly.length(), 4)
  
  // 测试滑动窗口时间采样
  let sliding_sample = TimeSeriesSampler::sliding_window_sampling(time_series_sampler, time_series, 60, 240) // 4小时窗口，每60分钟采样
  assert_true(sliding_sample.length() > 0)
  
  // 验证滑动窗口
  for window in sliding_sample {
    assert_eq(window.length(), 4) // 每个窗口4个点
  }
  
  // 测试自适应时间采样
  let adaptive_sample = TimeSeriesSampler::adaptive_time_sampling(time_series_sampler, time_series, 100, {
    "min_interval": 300, // 5分钟
    "max_interval": 7200, // 2小时
    "variance_threshold": 5.0
  })
  
  assert_eq(adaptive_sample.length(), 100)
}

// Test 7: 空间数据采样
test "spatial data sampling" {
  let spatial_sampler = SpatialSampler::new()
  
  // 创建空间数据点
  let spatial_data = []
  for i in 0..<1000 {
    let lat = 39.9 + (Random::next() % 100 - 50) * 0.001 // 北京附近
    let lng = 116.4 + (Random::next() % 100 - 50) * 0.001
    let value = Random::next() % 100
    
    spatial_data = spatial_data.push({
      "id": i,
      "latitude": lat,
      "longitude": lng,
      "value": value,
      "region": "region_" + (i % 10).to_string()
    })
  }
  
  // 测试网格采样
  let grid_sample = SpatialSampler::grid_sampling(spatial_sampler, spatial_data, 10, 10)
  assert_true(grid_sample.length() <= 100) // 最多100个网格
  
  // 验证网格分布
  let grid_regions = grid_sample.group_by(fn(point) { 
    let lat_grid = ((point.latitude - 39.85) / 0.01).floor()
    let lng_grid = ((point.longitude - 116.35) / 0.01).floor()
    lat_grid.to_string() + "_" + lng_grid.to_string()
  })
  
  // 验证每个网格最多有一个点
  for (_, points) in grid_regions {
    assert_true(points.length() == 1)
  }
  
  // 测试空间分层采样
  let strata = spatial_data.group_by(fn(point) { point.region })
  let spatial_stratified_sample = SpatialSampler::stratified_sampling(spatial_sampler, strata, 50)
  assert_eq(spatial_stratified_sample.length(), strata.length() * 50)
  
  // 验证每个区域都有代表
  for (region, _) in strata {
    let region_count = spatial_stratified_sample.filter(fn(point) { point.region == region }).length()
    assert_eq(region_count, 50)
  }
  
  // 测试基于密度的采样
  let density_sample = SpatialSampler::density_based_sampling(spatial_sampler, spatial_data, 100, 0.01)
  assert_eq(density_sample.length(), 100)
  
  // 测试基于距离的采样
  let distance_sample = SpatialSampler::distance_based_sampling(spatial_sampler, spatial_data, 100, 0.005)
  assert_eq(distance_sample.length(), 100)
  
  // 验证最小距离
  for i in 0..<distance_sample.length() {
    for j in (i+1)..<distance_sample.length() {
      let p1 = distance_sample[i]
      let p2 = distance_sample[j]
      let distance = ((p1.latitude - p2.latitude).pow(2) + (p1.longitude - p2.longitude).pow(2)).sqrt()
      assert_true(distance >= 0.005)
    }
  }
  
  // 测试空间随机采样
  let spatial_random_sample = SpatialSampler::spatial_random_sampling(spatial_sampler, spatial_data, 150)
  assert_eq(spatial_random_sample.length(), 150)
  
  // 测试基于聚类的采样
  let cluster_sample = SpatialSampler::cluster_based_sampling(spatial_sampler, spatial_data, 10, 10)
  assert_eq(cluster_sample.length(), 100)
  
  // 测试空间自适应采样
  let adaptive_sample = SpatialSampler::adaptive_sampling(spatial_sampler, spatial_data, 120, {
    "density_threshold": 0.01,
    "value_variance_threshold": 10.0
  })
  
  assert_eq(adaptive_sample.length(), 120)
}

// Test 8: 多维数据采样
test "multi-dimensional data sampling" {
  let multi_sampler = MultiDimensionalSampler::new()
  
  // 创建多维数据
  let multi_data = []
  for i in 0..<1000 {
    let x = (Random::next() % 100).to_float()
    let y = (Random::next() % 100).to_float()
    let z = (Random::next() % 100).to_float()
    let category = "cat_" + (i % 5).to_string()
    
    multi_data = multi_data.push({
      "id": i,
      "features": [x, y, z],
      "category": category,
      "value": (x + y + z) / 3.0
    })
  }
  
  // 测试基于特征空间的采样
  let feature_sample = MultiDimensionalSampler::feature_space_sampling(multi_sampler, multi_data, 100, 3)
  assert_eq(feature_sample.length(), 100)
  
  // 测试基于聚类的多维采样
  let cluster_sample = MultiDimensionalSampler::cluster_sampling(multi_sampler, multi_data, 10, 10)
  assert_eq(cluster_sample.length(), 100)
  
  // 测试基于主成分分析的采样
  let pca_sample = MultiDimensionalSampler::pca_sampling(multi_sampler, multi_data, 100)
  assert_eq(pca_sample.length(), 100)
  
  // 测试基于t-SNE的采样
  let tsne_sample = MultiDimensionalSampler::tsne_sampling(multi_sampler, multi_data, 100)
  assert_eq(tsne_sample.length(), 100)
  
  // 测试基于UMAP的采样
  let umap_sample = MultiDimensionalSampler::umap_sampling(multi_sampler, multi_data, 100)
  assert_eq(umap_sample.length(), 100)
  
  // 测试多维分层采样
  let multi_strata = multi_data.group_by(fn(item) { item.category })
  let multi_stratified_sample = MultiDimensionalSampler::stratified_sampling(multi_sampler, multi_strata, 30)
  assert_eq(multi_stratified_sample.length(), multi_strata.length() * 30)
  
  // 验证每个类别都有代表
  for (category, _) in multi_strata {
    let category_count = multi_stratified_sample.filter(fn(item) { item.category == category }).length()
    assert_eq(category_count, 30)
  }
  
  // 测试基于密度的多维采样
  let density_sample = MultiDimensionalSampler::density_based_sampling(multi_sampler, multi_data, 100)
  assert_eq(density_sample.length(), 100)
  
  // 测试基于边界的多维采样
  let boundary_sample = MultiDimensionalSampler::boundary_sampling(multi_sampler, multi_data, 50)
  assert_eq(boundary_sample.length(), 50)
  
  // 测试基于异常的多维采样
  let anomaly_sample = MultiDimensionalSampler::anomaly_sampling(multi_sampler, multi_data, 50)
  assert_eq(anomaly_sample.length(), 50)
}

// Test 9: 采样策略评估和比较
test "sampling strategy evaluation and comparison" {
  let evaluator = SamplingEvaluator::new()
  
  // 创建测试数据
  let test_data = []
  for i in 0..<1000 {
    let value = if i % 100 < 5 { 
      // 5%的异常值
      1000 + Random::next() % 100
    } else {
      // 正常值
      Random::next() % 100
    }
    
    test_data = test_data.push({
      "id": i,
      "value": value,
      "category": "cat_" + (i % 10).to_string(),
      "is_anomaly": i % 100 < 5
    })
  }
  
  // 定义不同的采样策略
  let strategies = [
    {
      "name": "random",
      "sampler": fn(data, size) { RandomSampler::simple_random(RandomSampler::new(42), data, size) }
    },
    {
      "name": "systematic",
      "sampler": fn(data, size) { SystematicSampler::interval_sampling(SystematicSampler::new(), data, size) }
    },
    {
      "name": "stratified",
      "sampler": fn(data, size) { 
        let strata = data.group_by(fn(item) { item.category })
        StratifiedSampler::proportional_sampling(StratifiedSampler::new(), strata, size)
      }
    },
    {
      "name": "adaptive",
      "sampler": fn(data, size) {
        let adaptive = AdaptiveSampler::new()
        AdaptiveSampler::configure_anomaly_detection(adaptive, { "threshold": 2.0, "window_size": 50 })
        AdaptiveSampler::anomaly_based_sampling(adaptive, data, size)
      }
    }
  ]
  
  // 评估每个策略
  let evaluation_results = []
  for strategy in strategies {
    let sample = strategy.sampler(test_data, 100)
    
    // 计算评估指标
    let anomaly_representation = sample.filter(fn(item) { item.is_anomaly }).length().to_float() / sample.length().to_float()
    let original_anomaly_ratio = test_data.filter(fn(item) { item.is_anomaly }).length().to_float() / test_data.length().to_float()
    let anomaly_improvement = anomaly_representation / original_anomaly_ratio
    
    let category_coverage = sample.map(fn(item) { item.category }).unique().length()
    let original_categories = test_data.map(fn(item) { item.category }).unique().length()
    let coverage_ratio = category_coverage.to_float() / original_categories.to_float()
    
    let sample_variance = sample.map(fn(item) { item.value }).variance()
    let original_variance = test_data.map(fn(item) { item.value }).variance()
    let variance_preservation = sample_variance / original_variance
    
    let result = {
      "strategy": strategy.name,
      "anomaly_improvement": anomaly_improvement,
      "category_coverage": coverage_ratio,
      "variance_preservation": variance_preservation,
      "sample_size": sample.length()
    }
    
    evaluation_results = evaluation_results.push(result)
  }
  
  // 验证评估结果
  for result in evaluation_results {
    assert_true(result.sample_size == 100)
    assert_true(result.anomaly_improvement > 0)
    assert_true(result.category_coverage > 0)
    assert_true(result.variance_preservation > 0)
  }
  
  // 找出最佳策略
  let best_anomaly_strategy = evaluation_results.reduce(fn(best, current) { 
    if current.anomaly_improvement > best.anomaly_improvement { current } else { best }
  })
  
  let best_coverage_strategy = evaluation_results.reduce(fn(best, current) { 
    if current.category_coverage > best.category_coverage { current } else { best }
  })
  
  let best_variance_strategy = evaluation_results.reduce(fn(best, current) { 
    if current.variance_preservation > best.variance_preservation { current } else { best }
  })
  
  // 验证自适应策略在异常检测方面表现更好
  assert_true(best_anomaly_strategy.strategy == "adaptive")
  
  // 验证分层策略在类别覆盖方面表现更好
  assert_true(best_coverage_strategy.strategy == "stratified")
  
  // 生成评估报告
  let evaluation_report = SamplingEvaluator::generate_report(evaluator, evaluation_results)
  assert_true(evaluation_report.contains("Sampling Strategy Evaluation"))
  assert_true(evaluation_report.contains("Anomaly Detection"))
  assert_true(evaluation_report.contains("Category Coverage"))
  assert_true(evaluation_report.contains("Variance Preservation"))
}

// Test 10: 采样策略优化和自动化
test "sampling strategy optimization and automation" {
  let optimizer = SamplingOptimizer::new()
  
  // 创建训练数据
  let training_data = []
  for i in 0..<2000 {
    let value = if i % 100 < 10 { 
      // 10%的异常值
      1000 + Random::next() % 200
    } else {
      // 正常值
      Random::next() % 100
    }
    
    training_data = training_data.push({
      "id": i,
      "value": value,
      "category": "cat_" + (i % 15).to_string(),
      "timestamp": Time::now() + i * 60,
      "is_anomaly": i % 100 < 10,
      "importance": if i % 50 == 0 { "high" } else { "normal" }
    })
  }
  
  // 配置优化目标
  let optimization_objectives = {
    "anomaly_detection": 0.4,  // 40%权重
    "category_coverage": 0.3,  // 30%权重
    "variance_preservation": 0.2, // 20%权重
    "temporal_representation": 0.1 // 10%权重
  }
  
  // 定义可调整的采样参数
  let sampling_parameters = {
    "random": {
      "seed": [42, 123, 456],
      "replacement": [true, false]
    },
    "systematic": {
      "interval": [10, 20, 50],
      "offset": [0, 5, 10]
    },
    "stratified": {
      "allocation": ["proportional", "equal", "optimal"]
    },
    "adaptive": {
      "anomaly_threshold": [1.5, 2.0, 2.5],
      "window_size": [30, 50, 100]
    }
  }
  
  // 执行优化
  let optimization_result = SamplingOptimizer::optimize(
    optimizer, 
    training_data, 
    optimization_objectives, 
    sampling_parameters,
    100 // 采样大小
  )
  
  // 验证优化结果
  assert_true(optimization_result.best_strategy != "")
  assert_true(optimization_result.best_score > 0)
  assert_true(optimization_result.optimization_iterations > 0)
  
  // 验证最佳参数
  let best_params = optimization_result.best_parameters
  assert_true(best_params.length() > 0)
  
  // 测试优化后的采样策略
  let optimized_sampler = SamplingOptimizer::create_sampler(optimizer, optimization_result.best_strategy, best_params)
  let optimized_sample = optimized_sampler(training_data, 100)
  
  assert_eq(optimized_sample.length(), 100)
  
  // 验证优化效果
  let anomaly_ratio = optimized_sample.filter(fn(item) { item.is_anomaly }).length().to_float() / optimized_sample.length().to_float()
  let original_anomaly_ratio = training_data.filter(fn(item) { item.is_anomaly }).length().to_float() / training_data.length().to_float()
  
  assert_true(anomaly_ratio > original_anomaly_ratio)
  
  // 测试自动化采样策略选择
  let auto_strategy = SamplingOptimizer::auto_select_strategy(optimizer, training_data, {
    "data_size": 2000,
    "has_temporal_dimension": true,
    "has_categories": true,
    "has_anomalies": true,
    "sampling_goal": "anomaly_detection"
  })
  
  assert_true(auto_strategy != "")
  
  // 测试动态采样策略调整
  let dynamic_adjuster = SamplingOptimizer::create_dynamic_adjuster(optimizer, optimization_result.best_strategy)
  
  // 模拟数据流变化
  let changing_data = []
  for i in 0..<500 {
    let value = if i % 50 < 15 { 
      // 增加异常值比例到30%
      1000 + Random::next() % 200
    } else {
      Random::next() % 100
    }
    
    changing_data = changing_data.push({
      "id": i,
      "value": value,
      "is_anomaly": i % 50 < 15
    })
  }
  
  // 动态调整采样策略
  let adjusted_strategy = SamplingOptimizer::adjust_strategy(dynamic_adjuster, changing_data)
  assert_true(adjusted_strategy != "")
  
  // 测试采样策略性能监控
  let monitor = SamplingOptimizer::create_monitor(optimizer)
  SamplingOptimizer::start_monitoring(monitor)
  
  // 记录采样性能
  for i in 0..<10 {
    let sample_data = training_data.slice(i * 200, (i + 1) * 200)
    let sample = optimized_sampler(sample_data, 50)
    
    SamplingOptimizer::record_performance(monitor, {
      "strategy": optimization_result.best_strategy,
      "sample_size": sample.length(),
      "processing_time": Random::next() % 100,
      "anomaly_detection_rate": sample.filter(fn(item) { item.is_anomaly }).length().to_float() / sample.length().to_float(),
      "timestamp": Time::now()
    })
  }
  
  // 获取性能报告
  let performance_report = SamplingOptimizer::get_performance_report(monitor)
  assert_true(performance_report.contains("Sampling Performance Report"))
  assert_true(performance_report.contains("Average Processing Time"))
  assert_true(performance_report.contains("Anomaly Detection Rate"))
  
  // 测试采样策略A/B测试
  let ab_test_result = SamplingOptimizer::ab_test(
    optimizer, 
    training_data.slice(0, 1000), 
    training_data.slice(1000, 2000),
    {
      "strategy_a": optimization_result.best_strategy,
      "strategy_b": "random",
      "metrics": ["anomaly_detection", "category_coverage", "processing_time"]
    }
  )
  
  assert_true(ab_test_result.winner != "")
  assert_true(ab_test_result.confidence > 0.5)
  
  SamplingOptimizer::stop_monitoring(monitor)
}