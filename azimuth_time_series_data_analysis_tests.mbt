// Azimuth Time Series Data Analysis Tests
// 时间序列数据分析测试用例 - 专注于时间序列数据处理、分析和预测

// Test 1: 时间序列数据预处理和清洗
test "time series data preprocessing and cleaning" {
  // 创建时间序列数据处理器
  let ts_processor = TimeSeriesProcessor::new()
  
  // 配置预处理参数
  TimeSeriesProcessor::configure(ts_processor, [
    ("missing.value.strategy", StringValue("interpolation")),
    ("outlier.detection.method", StringValue("iqr")),
    ("outlier.replacement", StringValue("median")),
    ("smoothing.enabled", BoolValue(true)),
    ("smoothing.window", IntValue(5))
  ])
  
  // 生成包含问题的时间序列数据
  let base_time = Time::now() - (24 * 60 * 60 * 1000) // 24小时前
  let raw_time_series = []
  
  for i in 0..<144 { // 每10分钟一个数据点，24小时 = 144个点
    let timestamp = base_time + (i * 10 * 60 * 1000)
    
    // 生成带有噪声和异常值的数据
    let base_value = 50.0 + (Math::sin(i.to_float() * 0.1) * 20.0)
    let noise = (Math::random() - 0.5) * 10.0
    let mut value = base_value + noise
    
    // 添加缺失值
    if (i == 20 || i == 45 || i == 78 || i == 120) {
      // 这些点将作为缺失值处理
      raw_time_series = raw_time_series.push(TimeSeriesPoint::new_with_missing(timestamp))
    } 
    // 添加异常值
    else if (i == 30 || i == 60 || i == 90) {
      value = value + (if (Math::random() > 0.5) { 50.0 } else { -50.0 })
      raw_time_series = raw_time_series.push(TimeSeriesPoint::new(timestamp, value))
    }
    // 正常数据点
    else {
      raw_time_series = raw_time_series.push(TimeSeriesPoint::new(timestamp, value))
    }
  }
  
  // 验证原始数据
  assert_eq(raw_time_series.length(), 144)
  
  let mut missing_count = 0
  for point in raw_time_series {
    if (TimeSeriesPoint::is_missing(point)) {
      missing_count = missing_count + 1
    }
  }
  assert_eq(missing_count, 4)
  
  // 执行数据预处理
  let preprocessing_start = Time::now()
  let processed_series = TimeSeriesProcessor::preprocess(ts_processor, raw_time_series)
  let preprocessing_end = Time::now()
  let preprocessing_duration = preprocessing_end - preprocessing_start
  
  assert_true(preprocessing_duration < 5000) // 预处理应在5秒内完成
  assert_eq(processed_series.length(), raw_time_series.length())
  
  // 验证缺失值处理
  let mut processed_missing_count = 0
  for point in processed_series {
    if (TimeSeriesPoint::is_missing(point)) {
      processed_missing_count = processed_missing_count + 1
    }
  }
  assert_eq(processed_missing_count, 0) // 缺失值应该已被处理
  
  // 验证异常值检测和处理
  let outlier_indices = [30, 60, 90]
  for index in outlier_indices {
    if (index < processed_series.length()) {
      let original_point = raw_time_series[index]
      let processed_point = processed_series[index]
      
      if (!TimeSeriesPoint::is_missing(original_point) && !TimeSeriesPoint::is_missing(processed_point)) {
        let original_value = TimeSeriesPoint::value(original_point)
        let processed_value = TimeSeriesPoint::value(processed_point)
        
        // 异常值应该被处理（更接近正常范围）
        assert_true(Math::abs(processed_value - 50.0) < Math::abs(original_value - 50.0))
      }
    }
  }
  
  // 验证数据平滑
  let smoothed_series = TimeSeriesProcessor::apply_smoothing(ts_processor, processed_series)
  assert_eq(smoothed_series.length(), processed_series.length())
  
  // 计算平滑前后的差异
  let mut total_variance_before = 0.0
  let mut total_variance_after = 0.0
  
  for i in 1..<processed_series.length() - 1 {
    let before_value = TimeSeriesPoint::value(processed_series[i])
    let after_value = TimeSeriesPoint::value(smoothed_series[i])
    
    let prev_before = TimeSeriesPoint::value(processed_series[i-1])
    let next_before = TimeSeriesPoint::value(processed_series[i+1])
    let avg_before = (prev_before + next_before) / 2.0
    
    let prev_after = TimeSeriesPoint::value(smoothed_series[i-1])
    let next_after = TimeSeriesPoint::value(smoothed_series[i+1])
    let avg_after = (prev_after + next_after) / 2.0
    
    total_variance_before = total_variance_before + Math::pow(before_value - avg_before, 2.0)
    total_variance_after = total_variance_after + Math::pow(after_value - avg_after, 2.0)
  }
  
  // 平滑后的方差应该更小
  assert_true(total_variance_after < total_variance_before)
}

// Test 2: 时间序列模式识别和趋势分析
test "time series pattern recognition and trend analysis" {
  // 创建时间序列分析器
  let ts_analyzer = TimeSeriesAnalyzer::new()
  
  // 配置分析参数
  TimeSeriesAnalyzer::configure(ts_analyzer, [
    ("trend.method", StringValue("linear_regression")),
    ("seasonality.detection", BoolValue(true)),
    ("seasonality.periods", StringValue("24,168")), // 小时和周周期
    ("pattern.confidence.threshold", FloatValue(0.8))
  ])
  
  // 生成具有明显模式的时间序列数据
  let base_time = Time::now() - (7 * 24 * 60 * 60 * 1000) // 7天前
  let pattern_series = []
  
  for i in 0..<672 { // 每小时一个数据点，7天 = 168个点
    let timestamp = base_time + (i * 60 * 60 * 1000)
    
    // 模拟日周期（24小时）
    let daily_pattern = 10.0 * Math::sin((i % 24).to_float() * 2.0 * Math::PI / 24.0)
    
    // 模拟周周期（7天）
    let weekly_pattern = 5.0 * Math::sin((i % 168).to_float() * 2.0 * Math::PI / 168.0)
    
    // 模拟上升趋势
    let trend = 0.1 * i.to_float()
    
    // 添加噪声
    let noise = (Math::random() - 0.5) * 5.0
    
    let value = 50.0 + daily_pattern + weekly_pattern + trend + noise
    pattern_series = pattern_series.push(TimeSeriesPoint::new(timestamp, value))
  }
  
  // 执行趋势分析
  let trend_analysis = TimeSeriesAnalyzer::analyze_trend(ts_analyzer, pattern_series)
  
  // 验证趋势分析结果
  assert_true(TrendAnalysis::has_slope(trend_analysis))
  assert_true(TrendAnalysis::has_intercept(trend_analysis))
  assert_true(TrendAnalysis::has_r_squared(trend_analysis))
  
  let slope = TrendAnalysis::slope(trend_analysis)
  let r_squared = TrendAnalysis::r_squared(trend_analysis)
  
  // 应该检测到上升趋势
  assert_true(slope > 0.05) // 斜率应该为正
  assert_true(r_squared > 0.5) // R²应该大于0.5，表示有明显的趋势
  
  // 执行季节性分析
  let seasonality_analysis = TimeSeriesAnalyzer::detect_seasonality(ts_analyzer, pattern_series)
  
  // 验证季节性分析结果
  assert_true(SeasonalityAnalysis::has_seasonal_patterns(seasonality_analysis))
  
  let seasonal_periods = SeasonalityAnalysis::get_periods(seasonality_analysis)
  assert_true(seasonal_periods.length() >= 2) // 应该检测到至少2个周期
  
  // 验证检测到24小时周期
  let has_daily_period = seasonal_periods.any(fn(period) { 
    Math::abs(Period::length(period) - 24.0) < 2.0 
  })
  assert_true(has_daily_period)
  
  // 验证检测到168小时（7天）周期
  let has_weekly_period = seasonal_periods.any(fn(period) { 
    Math::abs(Period::length(period) - 168.0) < 10.0 
  })
  assert_true(has_weekly_period)
  
  // 执行模式识别
  let pattern_recognition = TimeSeriesAnalyzer::recognize_patterns(ts_analyzer, pattern_series)
  
  // 验证模式识别结果
  assert_true(PatternRecognition::has_patterns(pattern_recognition))
  
  let patterns = PatternRecognition::get_patterns(pattern_recognition)
  assert_true(patterns.length() > 0)
  
  // 验证周期性模式
  let periodic_patterns = patterns.filter(fn(pattern) { 
    Pattern::type(pattern) == Periodic 
  })
  assert_true(periodic_patterns.length() >= 2)
  
  // 验证趋势模式
  let trend_patterns = patterns.filter(fn(pattern) { 
    Pattern::type(pattern) == Trend 
  })
  assert_true(trend_patterns.length() >= 1)
  
  // 测试异常模式检测
  let anomaly_patterns = patterns.filter(fn(pattern) { 
    Pattern::type(pattern) == Anomaly 
  })
  assert_true(anomaly_patterns.length() >= 0) // 可能有也可能没有异常模式
}

// Test 3: 时间序列预测和预报
test "time series forecasting and prediction" {
  // 创建时间序列预测器
  let ts_forecaster = TimeSeriesForecaster::new()
  
  // 配置预测参数
  TimeSeriesForecaster::configure(ts_forecaster, [
    ("forecast.horizon", IntValue(48)), // 预测48小时
    ("confidence.interval", FloatValue(0.95)),
    ("models", StringValue("arima,exponential_smoothing,prophet")),
    ("ensemble.enabled", BoolValue(true))
  ])
  
  // 生成历史数据用于训练
  let base_time = Time::now() - (30 * 24 * 60 * 60 * 1000) // 30天前
  let historical_data = []
  
  for i in 0..<720 { // 每小时一个数据点，30天 = 720个点
    let timestamp = base_time + (i * 60 * 60 * 1000)
    
    // 模拟复合模式：趋势 + 季节性 + 噪声
    let trend = 0.05 * i.to_float() // 上升趋势
    let daily_seasonality = 15.0 * Math::sin((i % 24).to_float() * 2.0 * Math::PI / 24.0)
    let weekly_seasonality = 8.0 * Math::sin((i % 168).to_float() * 2.0 * Math::PI / 168.0)
    let noise = (Math::random() - 0.5) * 10.0
    
    let value = 100.0 + trend + daily_seasonality + weekly_seasonality + noise
    historical_data = historical_data.push(TimeSeriesPoint::new(timestamp, value))
  }
  
  // 拆分数据为训练集和测试集
  let train_size = (historical_data.length() * 0.8).to_int()
  let train_data = historical_data.slice(0, train_size)
  let test_data = historical_data.slice(train_size, historical_data.length() - train_size)
  
  // 训练ARIMA模型
  let arima_training_start = Time::now()
  let arima_model = TimeSeriesForecaster::train_arima(ts_forecaster, train_data, [1, 1, 1], [1, 1, 1, 0])
  let arima_training_end = Time::now()
  let arima_training_duration = arima_training_end - arima_training_start
  
  assert_true(arima_model !== None)
  assert_true(arima_training_duration < 10000) // ARIMA训练应在10秒内完成
  
  // 使用ARIMA模型进行预测
  let arima_forecast = TimeSeriesForecaster::forecast(ts_forecaster, arima_model, test_data.length())
  assert_true(arima_forecast !== None)
  
  // 计算ARIMA预测准确性
  let arima_accuracy = TimeSeriesForecaster::evaluate_forecast(ts_forecaster, arima_forecast, test_data)
  assert_true(ForecastAccuracy::has_mae(arima_accuracy))
  assert_true(ForecastAccuracy::has_rmse(arima_accuracy))
  assert_true(ForecastAccuracy::has_mape(arima_accuracy))
  
  let arima_mape = ForecastAccuracy::mape(arima_accuracy)
  assert_true(arima_mape < 20.0) // MAPE应该小于20%
  
  // 训练指数平滑模型
  let exp_smoothing_model = TimeSeriesForecaster::train_exponential_smoothing(ts_forecaster, train_data, "additive", "additive", 24)
  assert_true(exp_smoothing_model !== None)
  
  // 使用指数平滑模型进行预测
  let exp_forecast = TimeSeriesForecaster::forecast(ts_forecaster, exp_smoothing_model, test_data.length())
  assert_true(exp_forecast !== None)
  
  // 计算指数平滑预测准确性
  let exp_accuracy = TimeSeriesForecaster::evaluate_forecast(ts_forecaster, exp_forecast, test_data)
  let exp_mape = ForecastAccuracy::mape(exp_accuracy)
  assert_true(exp_mape < 25.0) // MAPE应该小于25%
  
  // 训练Prophet模型
  let prophet_model = TimeSeriesForecaster::train_prophet(ts_forecaster, train_data, [
    ("daily.seasonality", BoolValue(true)),
    ("weekly.seasonality", BoolValue(true)),
    ("yearly.seasonality", BoolValue(false)),
    ("changepoint.prior.scale", FloatValue(0.05))
  ])
  assert_true(prophet_model !== None)
  
  // 使用Prophet模型进行预测
  let prophet_forecast = TimeSeriesForecaster::forecast(ts_forecaster, prophet_model, test_data.length())
  assert_true(prophet_forecast !== None)
  
  // 计算Prophet预测准确性
  let prophet_accuracy = TimeSeriesForecaster::evaluate_forecast(ts_forecaster, prophet_forecast, test_data)
  let prophet_mape = ForecastAccuracy::mape(prophet_accuracy)
  assert_true(prophet_mape < 15.0) // Prophet应该有更好的准确性
  
  // 创建集成预测
  let ensemble_forecast = TimeSeriesForecaster::create_ensemble_forecast(
    ts_forecaster,
    [arima_forecast, exp_forecast, prophet_forecast],
    [0.3, 0.2, 0.5] // 权重
  )
  assert_true(ensemble_forecast !== None)
  
  // 评估集成预测
  let ensemble_accuracy = TimeSeriesForecaster::evaluate_forecast(ts_forecaster, ensemble_forecast, test_data)
  let ensemble_mape = ForecastAccuracy::mape(ensemble_accuracy)
  
  // 集成预测应该比单个模型更好
  assert_true(ensemble_mape <= arima_mape)
  assert_true(ensemble_mape <= exp_mape)
  assert_true(ensemble_mape <= prophet_mape)
  
  // 生成未来预测
  let future_horizon = 48 // 48小时
  let future_forecast = TimeSeriesForecaster::forecast(ts_forecaster, prophet_model, future_horizon)
  
  // 验证未来预测
  assert_true(Forecast::has_points(future_forecast))
  assert_true(Forecast::has_confidence_intervals(future_forecast))
  
  let forecast_points = Forecast::get_points(future_forecast)
  assert_eq(forecast_points.length(), future_horizon)
  
  // 验证置信区间
  for point in forecast_points {
    assert_true(ForecastPoint::has_lower_bound(point))
    assert_true(ForecastPoint::has_upper_bound(point))
    assert_true(ForecastPoint::has_prediction(point))
    
    let lower = ForecastPoint::lower_bound(point)
    let upper = ForecastPoint::upper_bound(point)
    let prediction = ForecastPoint::prediction(point)
    
    assert_true(lower <= prediction)
    assert_true(prediction <= upper)
  }
}

// Test 4: 时间序列异常检测
test "time series anomaly detection" {
  // 创建时间序列异常检测器
  let anomaly_detector = TimeSeriesAnomalyDetector::new()
  
  // 配置异常检测参数
  AnomalyDetector::configure(anomaly_detector, [
    ("detection.methods", StringValue("statistical,isolation_forest,lstm_autoencoder")),
    ("threshold.sensitivity", FloatValue(0.95)),
    ("window.size", IntValue(24)),
    ("min.anomaly.separation", IntValue(6)) // 最小异常间隔6小时
  ])
  
  // 生成包含异常的时间序列数据
  let base_time = Time::now() - (14 * 24 * 60 * 60 * 1000) // 14天前
  let anomaly_data = []
  
  for i in 0..<336 { // 每小时一个数据点，14天 = 336个点
    let timestamp = base_time + (i * 60 * 60 * 1000)
    
    // 生成正常时间序列模式
    let trend = 0.02 * i.to_float()
    let daily_pattern = 10.0 * Math::sin((i % 24).to_float() * 2.0 * Math::PI / 24.0)
    let weekly_pattern = 5.0 * Math::sin((i % 168).to_float() * 2.0 * Math::PI / 168.0)
    let noise = (Math::random() - 0.5) * 8.0
    
    let mut value = 50.0 + trend + daily_pattern + weekly_pattern + noise
    
    // 添加异常点
    if (i == 72 || i == 144 || i == 216 || i == 288) {
      // 突然跳变异常
      value = value + (if (Math::random() > 0.5) { 40.0 } else { -40.0 })
    } else if (i >= 180 && i <= 186) {
      // 持续异常（一周）
      value = value + 25.0
    }
    
    anomaly_data = anomaly_data.push(TimeSeriesPoint::new(timestamp, value))
  }
  
  // 使用统计方法检测异常
  let statistical_anomalies = AnomalyDetector::detect_statistical_anomalies(
    anomaly_detector,
    anomaly_data,
    "zscore",
    3.0 // 3倍标准差
  )
  
  // 验证统计异常检测结果
  assert_true(statistical_anomalies.length() > 0)
  
  // 应该检测到跳变异常
  let spike_anomalies = statistical_anomalies.filter(fn(anomaly) {
    Anomaly::type(anomaly) == Spike
  })
  assert_true(spike_anomalies.length() >= 4)
  
  // 使用孤立森林检测异常
  let isolation_anomalies = AnomalyDetector::detect_isolation_forest_anomalies(
    anomaly_detector,
    anomaly_data,
    0.1 // 异常分数阈值
  )
  
  // 验证孤立森林异常检测结果
  assert_true(isolation_anomalies.length() > 0)
  
  // 使用LSTM自编码器检测异常
  let lstm_anomalies = AnomalyDetector::detect_lstm_anomalies(
    anomaly_detector,
    anomaly_data,
    24, // 序列长度
    0.95 // 重构误差阈值
  )
  
  // 验证LSTM异常检测结果
  assert_true(lstm_anomalies.length() > 0)
  
  // 集成异常检测结果
  let ensemble_anomalies = AnomalyDetector::ensemble_detection(
    anomaly_detector,
    [statistical_anomalies, isolation_anomalies, lstm_anomalies],
    "voting" // 投票法
  )
  
  // 验证集成异常检测结果
  assert_true(ensemble_anomalies.length() > 0)
  
  // 分析异常检测结果
  let anomaly_analysis = AnomalyDetector::analyze_anomalies(anomaly_detector, ensemble_anomalies)
  
  // 验证异常分析结果
  assert_true(AnomalyAnalysis::has_anomaly_count(anomaly_analysis))
  assert_true(AnomalyAnalysis::has_anomaly_rate(anomaly_analysis))
  assert_true(AnomalyAnalysis::has_anomaly_patterns(anomaly_analysis))
  
  let anomaly_count = AnomalyAnalysis::anomaly_count(anomaly_analysis)
  let anomaly_rate = AnomalyAnalysis::anomaly_rate(anomaly_analysis)
  
  assert_true(anomaly_count >= 4) // 至少4个异常
  assert_true(anomaly_rate > 0.01 && anomaly_rate < 0.2) // 异常率在1%-20%之间
  
  // 测试异常解释
  let anomaly_explanations = []
  for anomaly in ensemble_anomalies.slice(0, 5) { // 只解释前5个异常
    let explanation = AnomalyDetector::explain_anomaly(anomaly_detector, anomaly, anomaly_data)
    anomaly_explanations = anomaly_explanations.push(explanation)
  }
  
  // 验证异常解释
  for explanation in anomaly_explanations {
    assert_true(AnomalyExplanation::has_reason(explanation))
    assert_true(AnomalyExplanation::has_confidence(explanation))
    assert_true(AnomalyExplanation::has_context(explanation))
  }
  
  // 测试异常修复建议
  let repair_suggestions = []
  for anomaly in ensemble_anomalies.slice(0, 3) { // 只为前3个异常提供建议
    let suggestion = AnomalyDetector::suggest_repair(anomaly_detector, anomaly, anomaly_data)
    repair_suggestions = repair_suggestions.push(suggestion)
  }
  
  // 验证修复建议
  for suggestion in repair_suggestions {
    assert_true(RepairSuggestion::has_method(suggestion))
    assert_true(RepairSuggestion::has_parameters(suggestion))
    assert_true(RepairSuggestion::has_expected_improvement(suggestion))
  }
}

// Test 5: 时间序列相关性分析
test "time series correlation analysis" {
  // 创建时间序列相关性分析器
  let correlation_analyzer = TimeSeriesCorrelationAnalyzer::new()
  
  // 配置相关性分析参数
  CorrelationAnalyzer::configure(correlation_analyzer, [
    ("correlation.methods", StringValue("pearson,spearman,kendall")),
    ("lag.analysis.enabled", BoolValue(true)),
    ("max.lag", IntValue(24)),
    ("significance.threshold", FloatValue(0.05))
  ])
  
  // 生成多个相关的时间序列
  let base_time = Time::now() - (10 * 24 * 60 * 60 * 1000) // 10天前
  let time_series_collection = []
  
  for series_idx in 0..<5 {
    let series = []
    
    for i in 0..<240 { // 每小时一个数据点，10天 = 240个点
      let timestamp = base_time + (i * 60 * 60 * 1000)
      
      let mut value = 0.0
      
      // 基础模式（所有序列共享）
      let base_pattern = 20.0 * Math::sin(i.to_float() * 0.1)
      value = value + base_pattern
      
      // 序列特定模式
      match series_idx {
        0 => {
          // 序列0：与序列1强相关
          value = value + 10.0 * Math::cos(i.to_float() * 0.15)
        }
        1 => {
          // 序列1：与序列0强相关，但有噪声
          value = value + 8.0 * Math::cos(i.to_float() * 0.15) + (Math::random() - 0.5) * 5.0
        }
        2 => {
          // 序列2：与序列3中等相关
          value = value + 15.0 * Math::sin(i.to_float() * 0.08)
        }
        3 => {
          // 序列3：与序列2中等相关，但有相位差
          value = value + 12.0 * Math::sin((i - 6).to_float() * 0.08) + (Math::random() - 0.5) * 3.0
        }
        4 => {
          // 序列4：与其他序列弱相关
          value = value + 5.0 * Math::sin(i.to_float() * 0.25) + (Math::random() - 0.5) * 15.0
        }
        _ => ()
      }
      
      series = series.push(TimeSeriesPoint::new(timestamp, value))
    }
    
    time_series_collection = time_series_collection.push(series)
  }
  
  // 计算Pearson相关系数
  let pearson_correlations = CorrelationAnalyzer::compute_pearson_correlations(
    correlation_analyzer,
    time_series_collection
  )
  
  // 验证Pearson相关系数
  assert_true(pearson_correlations.length() == 10) // 5个序列的组合数 C(5,2) = 10
  
  // 检查序列0和序列1的强相关性
  let correlation_0_1 = pearson_correlations.find(fn(correlation) {
    Correlation::series1_index(correlation) == 0 && Correlation::series2_index(correlation) == 1
  })
  
  match correlation_0_1 {
    Some(correlation) => {
      let coefficient = Correlation::coefficient(correlation)
      assert_true(Math::abs(coefficient) > 0.7) // 强相关
      assert_true(Correlation::p_value(correlation) < 0.05) // 显著相关
    }
    None => assert_true(false)
  }
  
  // 检查序列2和序列3的中等相关性
  let correlation_2_3 = pearson_correlations.find(fn(correlation) {
    Correlation::series1_index(correlation) == 2 && Correlation::series2_index(correlation) == 3
  })
  
  match correlation_2_3 {
    Some(correlation) => {
      let coefficient = Correlation::coefficient(correlation)
      assert_true(Math::abs(coefficient) > 0.4 && Math::abs(coefficient) < 0.8) // 中等相关
    }
    None => assert_true(false)
  }
  
  // 检查序列0和序列4的弱相关性
  let correlation_0_4 = pearson_correlations.find(fn(correlation) {
    Correlation::series1_index(correlation) == 0 && Correlation::series2_index(correlation) == 4
  })
  
  match correlation_0_4 {
    Some(correlation) => {
      let coefficient = Correlation::coefficient(correlation)
      assert_true(Math::abs(coefficient) < 0.4) // 弱相关
    }
    None => assert_true(false)
  }
  
  // 计算Spearman相关系数
  let spearman_correlations = CorrelationAnalyzer::compute_spearman_correlations(
    correlation_analyzer,
    time_series_collection
  )
  
  // 验证Spearman相关系数
  assert_true(spearman_correlations.length() == 10)
  
  // 执行滞后分析
  let lag_analysis = CorrelationAnalyzer::analyze_lag_correlations(
    correlation_analyzer,
    time_series_collection[0],
    time_series_collection[3],
    24 // 最大滞后24小时
  )
  
  // 验证滞后分析结果
  assert_true(LagAnalysis::has_optimal_lag(lag_analysis))
  assert_true(LagAnalysis::has_max_correlation(lag_analysis))
  assert_true(LagAnalysis::has_correlation_series(lag_analysis))
  
  let optimal_lag = LagAnalysis::optimal_lag(lag_analysis)
  let max_correlation = LagAnalysis::max_correlation(lag_analysis)
  
  // 序列3应该与序列0有6小时的滞后相关
  assert_true(Math::abs(optimal_lag - 6) <= 2) // 允许2小时的误差
  assert_true(Math::abs(max_correlation) > 0.3) // 应该有一定的相关性
  
  // 执行因果关系分析
  let causality_analysis = CorrelationAnalyzer::analyze_granger_causality(
    correlation_analyzer,
    time_series_collection[0],
    time_series_collection[1],
    5 // 5阶滞后
  )
  
  // 验证因果关系分析结果
  assert_true(CausalityAnalysis::has_f_statistic(causality_analysis))
  assert_true(CausalityAnalysis::has_p_value(causality_analysis))
  assert_true(CausalityAnalysis::has_causality_direction(causality_analysis))
  
  // 创建相关性矩阵
  let correlation_matrix = CorrelationAnalyzer::create_correlation_matrix(
    correlation_analyzer,
    pearson_correlations,
    5 // 5个序列
  )
  
  // 验证相关性矩阵
  assert_true(CorrelationMatrix::size(correlation_matrix) == 5)
  
  for i in 0..<5 {
    for j in 0..<5 {
      let correlation = CorrelationMatrix::get(correlation_matrix, i, j)
      if (i == j) {
        assert_eq(correlation, 1.0) // 对角线应该为1
      } else {
        assert_true(correlation >= -1.0 && correlation <= 1.0) // 相关系数范围
      }
    }
  }
  
  // 测试相关性网络分析
  let correlation_network = CorrelationAnalyzer::build_correlation_network(
    correlation_analyzer,
    pearson_correlations,
    0.5 // 相关性阈值
  )
  
  // 验证相关性网络
  assert_true(CorrelationNetwork::has_nodes(correlation_network))
  assert_true(CorrelationNetwork::has_edges(correlation_network))
  
  let nodes = CorrelationNetwork::get_nodes(correlation_network)
  let edges = CorrelationNetwork::get_edges(correlation_network)
  
  assert_eq(nodes.length(), 5) // 5个节点
  assert_true(edges.length() > 0) // 至少有一些边
  
  // 验证强相关的序列之间有边
  let strong_edges = edges.filter(fn(edge) {
    CorrelationEdge::weight(edge) > 0.7
  })
  assert_true(strong_edges.length() >= 1) // 至少一条强相关边
}

// Test 6: 时间序列分解和重构
test "time series decomposition and reconstruction" {
  // 创建时间序列分解器
  let ts_decomposer = TimeSeriesDecomposer::new()
  
  // 配置分解参数
  TimeSeriesDecomposer::configure(ts_decomposer, [
    ("decomposition.method", StringValue("stl")),
    ("seasonal.periods", StringValue("24,168")), // 小时和周周期
    ("robust.decomposition", BoolValue(true)),
    ("trend.degree", IntValue(1)),
    ("seasonal.degree", IntValue(1))
  ])
  
  // 生成复合时间序列数据
  let base_time = Time::now() - (14 * 24 * 60 * 60 * 1000) // 14天前
  let composite_series = []
  
  for i in 0..<336 { // 每小时一个数据点，14天 = 336个点
    let timestamp = base_time + (i * 60 * 60 * 1000)
    
    // 趋势成分：线性增长
    let trend = 0.1 * i.to_float()
    
    // 季节性成分：日周期 + 周周期
    let daily_seasonal = 20.0 * Math::sin((i % 24).to_float() * 2.0 * Math::PI / 24.0)
    let weekly_seasonal = 10.0 * Math::sin((i % 168).to_float() * 2.0 * Math::PI / 168.0)
    let seasonal = daily_seasonal + weekly_seasonal
    
    // 周期性成分：不规则周期
    let cyclic = 5.0 * Math::sin(i.to_float() * 0.03) + 3.0 * Math::cos(i.to_float() * 0.07)
    
    // 不规则成分：随机噪声
    let irregular = (Math::random() - 0.5) * 8.0
    
    // 合成值
    let value = 100.0 + trend + seasonal + cyclic + irregular
    composite_series = composite_series.push(TimeSeriesPoint::new(timestamp, value))
  }
  
  // 执行时间序列分解
  let decomposition_start = Time::now()
  let decomposition_result = TimeSeriesDecomposer::decompose(ts_decomposer, composite_series, [24, 168])
  let decomposition_end = Time::now()
  let decomposition_duration = decomposition_end - decomposition_start
  
  assert_true(decomposition_duration < 10000) // 分解应在10秒内完成
  
  // 验证分解结果
  assert_true(Decomposition::has_trend(decomposition_result))
  assert_true(Decomposition::has_seasonal(decomposition_result))
  assert_true(Decomposition::has_remainder(decomposition_result))
  
  let trend_component = Decomposition::get_trend(decomposition_result)
  let seasonal_component = Decomposition::get_seasonal(decomposition_result)
  let remainder_component = Decomposition::get_remainder(decomposition_result)
  
  assert_eq(trend_component.length(), composite_series.length())
  assert_eq(seasonal_component.length(), composite_series.length())
  assert_eq(remainder_component.length(), composite_series.length())
  
  // 验证分解的质量
  let reconstruction = TimeSeriesDecomposer::reconstruct(
    ts_decomposer,
    trend_component,
    seasonal_component,
    remainder_component
  )
  
  assert_eq(reconstruction.length(), composite_series.length())
  
  // 计算重构误差
  let mut total_squared_error = 0.0
  for i in 0..<reconstruction.length() {
    let original_value = TimeSeriesPoint::value(composite_series[i])
    let reconstructed_value = TimeSeriesPoint::value(reconstruction[i])
    let error = original_value - reconstructed_value
    total_squared_error = total_squared_error + error * error
  }
  
  let mse = total_squared_error / reconstruction.length().to_float()
  let rmse = Math::sqrt(mse)
  
  assert_true(rmse < 1.0) // 重构误差应该很小
  
  // 分析各成分的特征
  let trend_analysis = TimeSeriesDecomposer::analyze_component(ts_decomposer, trend_component, "trend")
  assert_true(ComponentAnalysis::has_slope(trend_analysis))
  assert_true(ComponentAnalysis::has_monotonicity(trend_analysis))
  
  let trend_slope = ComponentAnalysis::slope(trend_analysis)
  assert_true(trend_slope > 0.05) // 应该检测到上升趋势
  
  let seasonal_analysis = TimeSeriesDecomposer::analyze_component(ts_decomposer, seasonal_component, "seasonal")
  assert_true(ComponentAnalysis::has_periods(seasonal_analysis))
  
  let seasonal_periods = ComponentAnalysis::get_periods(seasonal_analysis)
  assert_true(seasonal_periods.length() >= 2) // 应该检测到至少2个周期
  
  // 测试自适应分解
  let adaptive_decomposition = TimeSeriesDecomposer::adaptive_decompose(
    ts_decomposer,
    composite_series,
    0.1 // 最小方差比例
  )
  
  // 验证自适应分解结果
  assert_true(AdaptiveDecomposition::has_components(adaptive_decomposition))
  assert_true(AdaptiveDecomposition::has_explained_variance(adaptive_decomposition))
  
  let explained_variance = AdaptiveDecomposition::explained_variance(adaptive_decomposition)
  assert_true(explained_variance > 0.8) // 应该解释至少80%的方差
  
  // 测试季节性调整
  let seasonally_adjusted = TimeSeriesDecomposer::remove_seasonality(
    ts_decomposer,
    composite_series,
    seasonal_component
  )
  
  // 验证季节性调整
  assert_eq(seasonally_adjusted.length(), composite_series.length())
  
  // 季节性调整后的序列应该有更少的季节性
  let original_seasonality = TimeSeriesDecomposer::measure_seasonality_strength(ts_decomposer, composite_series)
  let adjusted_seasonality = TimeSeriesDecomposer::measure_seasonality_strength(ts_decomposer, seasonally_adjusted)
  
  assert_true(adjusted_seasonality < original_seasonality)
  
  // 测试趋势去除
  let detrended = TimeSeriesDecomposer::remove_trend(
    ts_decomposer,
    composite_series,
    trend_component
  )
  
  // 验证趋势去除
  assert_eq(detrended.length(), composite_series.length())
  
  // 去趋势后的序列应该有更少的趋势
  let original_trend = TimeSeriesDecomposer::measure_trend_strength(ts_decomposer, composite_series)
  let detrended_trend = TimeSeriesDecomposer::measure_trend_strength(ts_decomposer, detrended)
  
  assert_true(detrended_trend < original_trend)
  
  // 测试异常成分分析
  let remainder_analysis = TimeSeriesDecomposer::analyze_remainder(ts_decomposer, remainder_component)
  assert_true(RemainderAnalysis::has_normality_test(remainder_analysis))
  assert_true(RemainderAnalysis::has_autocorrelation(remainder_analysis))
  
  let normality_p_value = RemainderAnalysis::normality_p_value(remainder_analysis)
  let autocorrelation = RemainderAnalysis::max_autocorrelation(remainder_analysis)
  
  // 剩余成分应该接近白噪声（低自相关）
  assert_true(Math::abs(autocorrelation) < 0.3)
}