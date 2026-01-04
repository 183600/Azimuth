// Azimuth 自适应告警阈值测试用例
// 专注于动态调整和机器学习预测的自适应告警阈值功能

// 测试1: 基于历史数据的阈值自适应
test "基于历史数据的阈值自适应测试" {
  // 创建自适应阈值管理器
  let adaptive_threshold_manager = AdaptiveThresholdManager::new()
  
  // 配置自适应策略
  AdaptiveThresholdManager::configure_adaptation_strategy(adaptive_threshold_manager, {
    algorithm: "seasonal_decomposition",
    lookback_period_days: 30,
    adaptation_frequency_hours: 6,
    min_data_points: 100,
    confidence_interval: 0.95,
    sensitivity_factor: 1.5
  })
  
  // 创建基础告警规则
  AdaptiveThresholdManager::create_alert_rule(adaptive_threshold_manager, {
    rule_id: "cpu_utilization_high",
    name: "CPU利用率过高",
    metric: "cpu_utilization",
    service: "payment.service",
    initial_threshold: {
      type: "static",
      value: 80.0,
      operator: "greater_than"
    },
    adaptation_enabled: true,
    adaptation_config: {
      seasonality: "daily",
      trend_analysis: true,
      outlier_detection: true,
      minimum_threshold: 60.0,
      maximum_threshold: 95.0
    },
    notification_channels: ["email", "slack"]
  })
  
  // 创建延迟告警规则
  AdaptiveThresholdManager::create_alert_rule(adaptive_threshold_manager, {
    rule_id: "response_time_p99_high",
    name: "P99响应时间过长",
    metric: "response_time_p99",
    service: "api.gateway",
    initial_threshold: {
      type: "static",
      value: 500.0,
      operator: "greater_than"
    },
    adaptation_enabled: true,
    adaptation_config: {
      seasonality: "weekly",
      trend_analysis: true,
      outlier_detection: true,
      minimum_threshold: 200.0,
      maximum_threshold: 2000.0
    },
    notification_channels: ["email", "slack", "pagerduty"]
  })
  
  // 生成历史数据（30天）
  let historical_data = []
  let base_time = Time::now() - (30 * 24 * 60 * 60 * 1000)  // 30天前
  
  for day in 0..=29 {
    let day_start_time = base_time + day * 24 * 60 * 60 * 1000
    
    for hour in 0..=23 {
      let hour_start_time = day_start_time + hour * 60 * 60 * 1000
      
      // 模拟日季节性模式（工作时间更高）
      let is_business_hours = hour >= 8 and hour <= 18
      let base_cpu = if is_business_hours { 60.0 } else { 30.0 }
      
      // 模拟周季节性模式（工作日更高）
      let day_of_week = day % 7
      let is_weekday = day_of_week >= 1 and day_of_week <= 5
      let weekly_factor = if is_weekday { 1.2 } else { 0.8 }
      
      // 生成CPU利用率数据
      for minute in 0..=59 {
        let timestamp = hour_start_time + minute * 60 * 1000
        
        // 添加随机波动和趋势
        let random_factor = 0.8 + (Math::random() * 0.4)  // 0.8到1.2的随机因子
        let trend_factor = 1.0 + (day * 0.002)  // 轻微上升趋势
        
        let cpu_value = base_cpu * weekly_factor * random_factor * trend_factor
        historical_data = historical_data.push({
          timestamp: timestamp,
          metric: "cpu_utilization",
          service: "payment.service",
          value: cpu_value,
          tags: [("environment", "production"), ("region", "us-west-2")]
        })
        
        // 生成响应时间数据
        let base_response_time = if is_business_hours { 300.0 } else { 150.0 }
        let response_time_value = base_response_time * weekly_factor * random_factor * trend_factor
        historical_data = historical_data.push({
          timestamp: timestamp,
          metric: "response_time_p99",
          service: "api.gateway",
          value: response_time_value,
          tags: [("environment", "production"), ("region", "us-west-2")]
        })
      }
    }
  }
  
  // 执行阈值自适应
  let adaptation_result = AdaptiveThresholdManager::adapt_thresholds(adaptive_threshold_manager, historical_data)
  
  // 验证自适应结果
  assert_true(adaptation_result.success)
  assert_true(adaptation_result.adapted_rules.length() > 0)
  
  // 检查CPU利用率规则的自适应结果
  let cpu_adaptation = adaptation_result.adapted_rules.find(fn(r) { r.rule_id == "cpu_utilization_high" })
  assert_true(cpu_adaptation != None)
  
  match cpu_adaptation {
    Some(adaptation) => {
      assert_true(adaptation.new_threshold.value > 60.0)  // 应该高于最小阈值
      assert_true(adaptation.new_threshold.value < 95.0)  // 应该低于最大阈值
      assert_true(adaptation.adaptation_confidence > 0.0)
      assert_true(adaptation.seasonality_detected)
      assert_true(adaptation.trend_detected)
      
      // 检查工作时间与非工作时间的阈值差异
      assert_true(adaptation.hourly_thresholds.length() == 24)
      
      let business_hours_avg = adaptation.hourly_thresholds
        .filter(fn(h) { h.hour >= 8 and h.hour <= 18 })
        .map(fn(h) { h.threshold })
        .reduce(fn(acc, t) { acc + t }, 0.0) / 11.0
        
      let non_business_hours_avg = adaptation.hourly_thresholds
        .filter(fn(h) { h.hour < 8 or h.hour > 18 })
        .map(fn(h) { h.threshold })
        .reduce(fn(acc, t) { acc + t }, 0.0) / 13.0
        
      assert_true(business_hours_avg > non_business_hours_avg)
    }
    None => assert_true(false)
  }
  
  // 检查响应时间规则的自适应结果
  let response_time_adaptation = adaptation_result.adapted_rules.find(fn(r) { r.rule_id == "response_time_p99_high" })
  assert_true(response_time_adaptation != None)
  
  match response_time_adaptation {
    Some(adaptation) => {
      assert_true(adaptation.new_threshold.value > 200.0)  // 应该高于最小阈值
      assert_true(adaptation.new_threshold.value < 2000.0)  // 应该低于最大阈值
      assert_true(adaptation.weekly_seasonality_detected)
      
      // 检查工作日与周末的阈值差异
      assert_true(adaptation.daily_thresholds.length() == 7)
      
      let weekday_avg = adaptation.daily_thresholds
        .filter(fn(d) { d.day_of_week >= 1 and d.day_of_week <= 5 })
        .map(fn(d) { d.threshold })
        .reduce(fn(acc, t) { acc + t }, 0.0) / 5.0
        
      let weekend_avg = adaptation.daily_thresholds
        .filter(fn(d) { d.day_of_week == 0 or d.day_of_week == 6 })
        .map(fn(d) { d.threshold })
        .reduce(fn(acc, t) { acc + t }, 0.0) / 2.0
        
      assert_true(weekday_avg > weekend_avg)
    }
    None => assert_true(false)
  }
  
  // 测试自适应阈值的告警触发
  let current_time = Time::now()
  let business_hour = 14  // 下午2点，工作时间
  let current_hourly_threshold = match cpu_adaptation {
    Some(adaptation) => adaptation.hourly_thresholds[business_hour].threshold
    None => 80.0
  }
  
  // 创建当前数据点（高于自适应阈值）
  let current_data = [
    {
      timestamp: current_time,
      metric: "cpu_utilization",
      service: "payment.service",
      value: current_hourly_threshold + 10.0,  // 高于阈值
      tags: [("environment", "production"), ("region", "us-west-2")]
    }
  ]
  
  // 检查告警触发
  let alert_evaluation = AdaptiveThresholdManager::evaluate_alerts(adaptive_threshold_manager, current_data)
  
  // 验证告警触发
  assert_true(alert_evaluation.triggered_alerts.length() > 0)
  
  let cpu_alert = alert_evaluation.triggered_alerts.find(fn(a) { a.rule_id == "cpu_utilization_high" })
  assert_true(cpu_alert != None)
  
  match cpu_alert {
    Some(alert) {
      assert_true(alert.triggered)
      assert_eq(alert.metric_value, current_hourly_threshold + 10.0)
      assert_eq(alert.adaptive_threshold, current_hourly_threshold)
      assert_true(alert.threshold_adaptation_applied)
      assert_true(alert.severity != "")
    }
    None => assert_true(false)
  }
  
  // 测试自适应性能指标
  let performance_metrics = AdaptiveThresholdManager::get_adaptation_performance(adaptive_threshold_manager)
  
  // 验证性能指标
  assert_true(performance_metrics.total_adaptations > 0)
  assert_true(performance_metrics.avg_adaptation_time_ms > 0)
  assert_true(performance_metrics.false_positive_rate < 0.1)  // 假阳性率应该小于10%
  assert_true(performance_metrics.alert_accuracy > 0.9)     // 告警准确率应该大于90%
}

// 测试2: 机器学习预测阈值
test "机器学习预测阈值测试" {
  // 创建ML预测阈值管理器
  let ml_threshold_manager = MLThresholdManager::new()
  
  // 配置ML模型
  MLThresholdManager::configure_ml_model(ml_threshold_manager, {
    model_type: "lstm_time_series",
    prediction_horizon_minutes: 60,
    training_window_days: 14,
    feature_engineering: {
      include_seasonal_features: true,
      include_trend_features: true,
      include_lag_features: true,
      include_external_factors: true
    },
    model_parameters: {
      lstm_units: 50,
      dropout_rate: 0.2,
      learning_rate: 0.001,
      epochs: 100,
      batch_size: 32
    },
    retraining_schedule: {
      frequency: "weekly",
      min_new_data_points: 1000,
      performance_threshold: 0.85
    }
  })
  
  // 创建ML增强告警规则
  MLThresholdManager::create_ml_alert_rule(ml_threshold_manager, {
    rule_id: "error_rate_prediction",
    name: "错误率预测告警",
    metric: "error_rate",
    service: "order.service",
    prediction_enabled: true,
    prediction_config: {
      prediction_threshold: 0.8,  // 80%概率超过阈值时告警
      threshold_value: 5.0,      // 5%错误率阈值
      confidence_threshold: 0.7, // 70%置信度
      lookback_window_minutes: 30
    },
    traditional_threshold: {
      type: "static",
      value: 5.0,
      operator: "greater_than"
    },
    notification_channels: ["email", "slack", "pagerduty"]
  })
  
  // 创建吞吐量预测规则
  MLThresholdManager::create_ml_alert_rule(ml_threshold_manager, {
    rule_id: "throughput_anomaly",
    name: "吞吐量异常预测",
    metric: "request_throughput",
    service: "user.service",
    prediction_enabled: true,
    prediction_config: {
      prediction_threshold: 0.75,
      anomaly_detection: true,
      deviation_threshold: 2.0,  // 2个标准差
      confidence_threshold: 0.8,
      lookback_window_minutes: 60
    },
    traditional_threshold: {
      type: "static",
      value: 100.0,
      operator: "less_than"
    },
    notification_channels: ["email", "slack"]
  })
  
  // 生成训练数据（14天）
  let training_data = []
  let base_time = Time::now() - (14 * 24 * 60 * 60 * 1000)  // 14天前
  
  for day in 0..=13 {
    let day_start_time = base_time + day * 24 * 60 * 60 * 1000
    
    for hour in 0..=23 {
      let hour_start_time = day_start_time + hour * 60 * 60 * 1000
      
      // 模拟复杂的错误率模式（包含周期性和突发事件）
      let base_error_rate = 1.0 + Math::sin(hour * Math::PI / 12) * 0.5  // 日周期性
      let weekly_pattern = if day % 7 < 5 { 1.0 } else { 0.7 }          // 周周期性
      let incident_factor = if day == 7 and hour >= 10 and hour <= 12 { 5.0 } else { 1.0 }  // 突发事件
      
      for minute in 0..=59 {
        let timestamp = hour_start_time + minute * 60 * 1000
        let noise = (Math::random() - 0.5) * 0.5  // 随机噪声
        
        let error_rate = base_error_rate * weekly_pattern * incident_factor + noise
        training_data = training_data.push({
          timestamp: timestamp,
          metric: "error_rate",
          service: "order.service",
          value: Math::max(0.0, error_rate),
          tags: [
            ("environment", "production"),
            ("region", "us-west-2"),
            ("hour_of_day", hour.to_string()),
            ("day_of_week", (day % 7).to_string())
          ]
        })
        
        // 模拟吞吐量数据
        let base_throughput = 200.0 + Math::cos(hour * Math::PI / 12) * 50.0
        let throughput_weekly_pattern = if day % 7 < 5 { 1.0 } else { 0.6 }
        let throughput_incident_factor = if day == 7 and hour >= 10 and hour <= 12 { 0.3 } else { 1.0 }
        
        let throughput_noise = (Math::random() - 0.5) * 20.0
        let throughput = base_throughput * throughput_weekly_pattern * throughput_incident_factor + throughput_noise
        
        training_data = training_data.push({
          timestamp: timestamp,
          metric: "request_throughput",
          service: "user.service",
          value: Math::max(0.0, throughput),
          tags: [
            ("environment", "production"),
            ("region", "us-west-2"),
            ("hour_of_day", hour.to_string()),
            ("day_of_week", (day % 7).to_string())
          ]
        })
      }
    }
  }
  
  // 训练ML模型
  let training_result = MLThresholdManager::train_models(ml_threshold_manager, training_data)
  
  // 验证训练结果
  assert_true(training_result.success)
  assert_true(training_result.trained_models.length() > 0)
  
  // 检查错误率模型训练结果
  let error_rate_model = training_result.trained_models.find(fn(m) { m.metric == "error_rate" })
  assert_true(error_rate_model != None)
  
  match error_rate_model {
    Some(model) {
      assert_true(model.model_id != "")
      assert_true(model.training_accuracy > 0.8)
      assert_true(model.validation_accuracy > 0.7)
      assert_true(model.training_time_ms > 0)
      assert_true(model.feature_importance.length() > 0)
    }
    None => assert_true(false)
  }
  
  // 检查吞吐量模型训练结果
  let throughput_model = training_result.trained_models.find(fn(m) { m.metric == "request_throughput" })
  assert_true(throughput_model != None)
  
  // 生成测试数据（用于预测）
  let current_time = Time::now()
  let test_data = []
  
  for i in 0..=59 {  // 最近1小时的数据
    let timestamp = current_time - (59 - i) * 60 * 1000
    
    // 模拟即将发生错误的模式（错误率逐渐上升）
    let error_rate = 1.0 + i * 0.1 + (Math::random() - 0.5) * 0.2
    test_data = test_data.push({
      timestamp: timestamp,
      metric: "error_rate",
      service: "order.service",
      value: error_rate,
      tags: [
        ("environment", "production"),
        ("region", "us-west-2"),
        ("hour_of_day", (Time::hour(current_time)).to_string()),
        ("day_of_week", (Time::day_of_week(current_time)).to_string())
      ]
    })
    
    // 模拟吞吐量下降模式
    let throughput = 200.0 - i * 2.0 + (Math::random() - 0.5) * 10.0
    test_data = test_data.push({
      timestamp: timestamp,
      metric: "request_throughput",
      service: "user.service",
      value: Math::max(0.0, throughput),
      tags: [
        ("environment", "production"),
        ("region", "us-west-2"),
        ("hour_of_day", (Time::hour(current_time)).to_string()),
        ("day_of_week", (Time::day_of_week(current_time)).to_string())
      ]
    })
  }
  
  // 执行ML预测
  let prediction_result = MLThresholdManager::generate_predictions(ml_threshold_manager, test_data)
  
  // 验证预测结果
  assert_true(prediction_result.success)
  assert_true(prediction_result.predictions.length() > 0)
  
  // 检查错误率预测
  let error_rate_prediction = prediction_result.predictions.find(fn(p) { p.metric == "error_rate" })
  assert_true(error_rate_prediction != None)
  
  match error_rate_prediction {
    Some(prediction) {
      assert_true(prediction.predicted_values.length() > 0)
      assert_true(prediction.confidence_scores.length() > 0)
      assert_true(prediction.prediction_horizon_minutes == 60)
      
      // 检查预测值是否显示上升趋势
      let first_predicted = prediction.predicted_values[0]
      let last_predicted = prediction.predicted_values[prediction.predicted_values.length() - 1]
      assert_true(last_predicted > first_predicted)
      
      // 检查预测的置信度
      let avg_confidence = prediction.confidence_scores.reduce(fn(acc, c) { acc + c }, 0.0) / prediction.confidence_scores.length()
      assert_true(avg_confidence > 0.5)
    }
    None => assert_true(false)
  }
  
  // 检查吞吐量预测
  let throughput_prediction = prediction_result.predictions.find(fn(p) { p.metric == "request_throughput" })
  assert_true(throughput_prediction != None)
  
  match throughput_prediction {
    Some(prediction) {
      assert_true(prediction.predicted_values.length() > 0)
      assert_true(prediction.anomaly_detected)
      assert_true(prediction.anomaly_score > 0.0)
    }
    None => assert_true(false)
  }
  
  // 基于预测结果评估告警
  let alert_evaluation = MLThresholdManager::evaluate_ml_alerts(ml_threshold_manager, prediction_result)
  
  // 验证预测告警
  assert_true(alert_evaluation.predictive_alerts.length() > 0)
  
  // 检查错误率预测告警
  let error_rate_alert = alert_evaluation.predictive_alerts.find(fn(a) { a.rule_id == "error_rate_prediction" })
  assert_true(error_rate_alert != None)
  
  match error_rate_alert {
    Some(alert) {
      assert_true(alert.predictive)
      assert_true(alert.prediction_confidence > 0.7)
      assert_true(alert.time_to_incident_minutes > 0)
      assert_true(alert.recommended_actions.length() > 0)
    }
    None => assert_true(false)
  }
  
  // 检查吞吐量异常告警
  let throughput_alert = alert_evaluation.predictive_alerts.find(fn(a) { a.rule_id == "throughput_anomaly" })
  assert_true(throughput_alert != None)
  
  match throughput_alert {
    Some(alert) {
      assert_true(alert.predictive)
      assert_true(alert.anomaly_score > 0.0)
      assert_true(alert.anomaly_type != "")
    }
    None => assert_true(false)
  }
  
  // 测试模型性能监控
  let model_performance = MLThresholdManager::get_model_performance(ml_threshold_manager)
  
  // 验证模型性能
  assert_true(model_performance.model_metrics.length() > 0)
  
  for metric in model_performance.model_metrics {
    assert_true(metric.model_id != "")
    assert_true(metric.metric_name != "")
    assert_true(metric.prediction_accuracy >= 0.0)
    assert_true(metric.mean_absolute_error >= 0.0)
    assert_true(metric.last_updated > 0)
  }
}

// 测试3: 多维度自适应阈值
test "多维度自适应阈值测试" {
  // 创建多维度阈值管理器
  let multi_dimensional_manager = MultiDimensionalThresholdManager::new()
  
  // 配置多维度自适应策略
  MultiDimensionalManager::configure_strategy(multi_dimensional_manager, {
    dimensions: ["service", "environment", "region", "tenant"],
    adaptation_algorithm: "hierarchical_clustering",
    min_samples_per_dimension: 50,
    correlation_threshold: 0.7,
    adaptation_frequency_hours: 12
  })
  
  // 创建多维度告警规则
  MultiDimensionalManager::create_multi_dimensional_rule(multi_dimensional_manager, {
    rule_id: "multi_dim_latency",
    name: "多维度延迟告警",
    metric: "response_time_p95",
    base_threshold: 300.0,
    dimensions: ["service", "environment", "region"],
    adaptation_config: {
      enable_dimension_specific: true,
      enable_cross_dimension_correlation: true,
      enable_hierarchical_adaptation: true,
      min_threshold: 100.0,
      max_threshold: 1000.0
    },
    notification_channels: ["email", "slack"]
  })
  
  // 创建多维度错误率规则
  MultiDimensionalManager::create_multi_dimensional_rule(multi_dimensional_manager, {
    rule_id: "multi_dim_error_rate",
    name: "多维度错误率告警",
    metric: "error_rate",
    base_threshold: 2.0,
    dimensions: ["service", "tenant", "environment"],
    adaptation_config: {
      enable_dimension_specific: true,
      enable_cross_dimension_correlation: true,
      enable_hierarchical_adaptation: true,
      min_threshold: 0.5,
      max_threshold: 10.0
    },
    notification_channels: ["email", "slack", "pagerduty"]
  })
  
  // 生成多维度测试数据
  let multi_dim_data = []
  let base_time = Time::now() - (7 * 24 * 60 * 60 * 1000)  // 7天前
  
  let services = ["payment.service", "user.service", "order.service", "inventory.service"]
  let environments = ["production", "staging", "development"]
  let regions = ["us-west-2", "us-east-1", "eu-west-1"]
  let tenants = ["tenant-a", "tenant-b", "tenant-c"]
  
  for day in 0..=6 {
    let day_start_time = base_time + day * 24 * 60 * 60 * 1000
    
    for hour in 0..=23 {
      let hour_start_time = day_start_time + hour * 60 * 60 * 1000
      
      for service in services {
        for environment in environments {
          for region in regions {
            // 为每个维度组合生成数据
            let service_factor = match service {
              "payment.service" => 1.2,
              "user.service" => 1.0,
              "order.service" => 1.1,
              "inventory.service" => 0.9,
              _ => 1.0
            }
            
            let env_factor = match environment {
              "production" => 1.3,
              "staging" => 1.0,
              "development" => 0.7,
              _ => 1.0
            }
            
            let region_factor = match region {
              "us-west-2" => 1.1,
              "us-east-1" => 1.0,
              "eu-west-1" => 0.9,
              _ => 1.0
            }
            
            let base_latency = 200.0 * service_factor * env_factor * region_factor
            let latency_noise = (Math::random() - 0.5) * 50.0
            let latency = base_latency + latency_noise
            
            multi_dim_data = multi_dim_data.push({
              timestamp: hour_start_time,
              metric: "response_time_p95",
              service: service,
              environment: environment,
              region: region,
              value: Math::max(0.0, latency),
              tags: [
                ("service", service),
                ("environment", environment),
                ("region", region)
              ]
            })
          }
        }
        
        for tenant in tenants {
          for environment in environments {
            // 为错误率生成多维度数据
            let tenant_factor = match tenant {
              "tenant-a" => 0.8,
              "tenant-b" => 1.0,
              "tenant-c" => 1.2,
              _ => 1.0
            }
            
            let env_factor = match environment {
              "production" => 0.9,
              "staging" => 1.0,
              "development" => 1.5,
              _ => 1.0
            }
            
            let base_error_rate = 1.0 * tenant_factor * env_factor
            let error_noise = (Math::random() - 0.5) * 0.5
            let error_rate = Math::max(0.0, base_error_rate + error_noise)
            
            multi_dim_data = multi_dim_data.push({
              timestamp: hour_start_time,
              metric: "error_rate",
              service: service,
              tenant: tenant,
              environment: environment,
              value: error_rate,
              tags: [
                ("service", service),
                ("tenant", tenant),
                ("environment", environment)
              ]
            })
          }
        }
      }
    }
  }
  
  // 执行多维度阈值自适应
  let multi_dim_adaptation = MultiDimensionalManager::adapt_multi_dimensional_thresholds(
    multi_dimensional_manager, 
    multi_dim_data
  )
  
  // 验证多维度自适应结果
  assert_true(multi_dim_adaptation.success)
  assert_true(multi_dim_adaptation.adapted_rules.length() > 0)
  
  // 检查延迟规则的多维度自适应
  let latency_adaptation = multi_dim_adaptation.adapted_rules.find(fn(r) { r.rule_id == "multi_dim_latency" })
  assert_true(latency_adaptation != None)
  
  match latency_adaptation {
    Some(adaptation) {
      // 检查服务特定阈值
      assert_true(adaptation.service_thresholds.length() == services.length())
      
      let payment_threshold = adaptation.service_thresholds.find(fn(s) { s.service == "payment.service" })
      assert_true(payment_threshold != None)
      
      match payment_threshold {
        Some(threshold) {
          assert_true(threshold.threshold > 200.0)  // payment.service延迟较高
          assert_true(threshold.sample_count > 0)
          assert_true(threshold.confidence_score > 0.0)
        }
        None => assert_true(false)
      }
      
      // 检查环境特定阈值
      assert_true(adaptation.environment_thresholds.length() == environments.length())
      
      let prod_threshold = adaptation.environment_thresholds.find(fn(e) { e.environment == "production" })
      assert_true(prod_threshold != None)
      
      match prod_threshold {
        Some(threshold) {
          assert_true(threshold.threshold > 200.0)  // 生产环境阈值较高
        }
        None => assert_true(false)
      }
      
      // 检查区域特定阈值
      assert_true(adaptation.region_thresholds.length() == regions.length())
      
      // 检查维度相关性
      assert_true(adaptation.correlation_matrix.length() > 0)
      
      let service_env_correlation = adaptation.correlation_matrix.find(fn(c) { 
        c.dimension1 == "service" and c.dimension2 == "environment" 
      })
      assert_true(service_env_correlation != None)
      
      match service_env_correlation {
        Some(correlation) {
          assert_true(correlation.correlation_coefficient >= -1.0 and correlation.correlation_coefficient <= 1.0)
          assert_true(correlation.significance > 0.0)
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 检查错误率规则的多维度自适应
  let error_rate_adaptation = multi_dim_adaptation.adapted_rules.find(fn(r) { r.rule_id == "multi_dim_error_rate" })
  assert_true(error_rate_adaptation != None)
  
  match error_rate_adaptation {
    Some(adaptation) {
      // 检查租户特定阈值
      assert_true(adaptation.tenant_thresholds.length() == tenants.length())
      
      let tenant_c_threshold = adaptation.tenant_thresholds.find(fn(t) { t.tenant == "tenant-c" })
      assert_true(tenant_c_threshold != None)
      
      match tenant_c_threshold {
        Some(threshold) {
          assert_true(threshold.threshold > 1.0)  // tenant-c错误率较高
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试多维度告警评估
  let current_time = Time::now()
  let current_data = [
    {
      timestamp: current_time,
      metric: "response_time_p95",
      service: "payment.service",
      environment: "production",
      region: "us-west-2",
      value: 350.0,  // 高于基础阈值
      tags: [("service", "payment.service"), ("environment", "production"), ("region", "us-west-2")]
    },
    {
      timestamp: current_time,
      metric: "error_rate",
      service: "order.service",
      tenant: "tenant-c",
      environment: "production",
      value: 3.0,  // 高于基础阈值
      tags: [("service", "order.service"), ("tenant", "tenant-c"), ("environment", "production")]
    }
  ]
  
  let multi_dim_alert_evaluation = MultiDimensionalManager::evaluate_multi_dimensional_alerts(
    multi_dimensional_manager, 
    current_data
  )
  
  // 验证多维度告警
  assert_true(multi_dim_alert_evaluation.triggered_alerts.length() > 0)
  
  // 检查延迟告警
  let latency_alert = multi_dim_alert_evaluation.triggered_alerts.find(fn(a) { a.rule_id == "multi_dim_latency" })
  assert_true(latency_alert != None)
  
  match latency_alert {
    Some(alert) {
      assert_true(alert.triggered)
      assert_true(alert.multi_dimensional)
      assert_true(alert.applied_thresholds.length() > 0)
      
      // 检查应用的阈值
      let service_threshold = alert.applied_thresholds.find(fn(t) { t.dimension == "service" })
      assert_true(service_threshold != None)
      
      let env_threshold = alert.applied_thresholds.find(fn(t) { t.dimension == "environment" })
      assert_true(env_threshold != None)
      
      let region_threshold = alert.applied_thresholds.find(fn(t) { t.dimension == "region" })
      assert_true(region_threshold != None)
    }
    None => assert_true(false)
  }
  
  // 检查错误率告警
  let error_rate_alert = multi_dim_alert_evaluation.triggered_alerts.find(fn(a) { a.rule_id == "multi_dim_error_rate" })
  assert_true(error_rate_alert != None)
  
  // 测试多维度性能分析
  let performance_analysis = MultiDimensionalManager::analyze_dimension_performance(
    multi_dimensional_manager, 
    multi_dim_data
  )
  
  // 验证性能分析
  assert_true(performance_analysis.dimension_performance.length() > 0)
  
  // 检查服务维度性能
  let service_performance = performance_analysis.dimension_performance.find(fn(p) { p.dimension == "service" })
  assert_true(service_performance != None)
  
  match service_performance {
    Some(performance) {
      assert_true(performance.dimension_values.length() == services.length())
      assert_true(performance.overall_variance > 0.0)
      
      // 检查各服务的性能指标
      for service_perf in performance.dimension_values {
        assert_true(service_perf.value != "")
        assert_true(service_perf.mean > 0.0)
        assert_true(service_perf.std_dev > 0.0)
        assert_true(service_perf.outlier_count >= 0)
      }
    }
    None => assert_true(false)
  }
  
  // 检查维度间相关性分析
  assert_true(performance_analysis.cross_dimension_correlations.length() > 0)
  
  for correlation in performance_analysis.cross_dimension_correlations {
    assert_true(correlation.dimension1 != "")
    assert_true(correlation.dimension2 != "")
    assert_true(correlation.correlation_coefficient >= -1.0 and correlation.correlation_coefficient <= 1.0)
  }
}

// 测试4: 自适应阈值异常检测
test "自适应阈值异常检测测试" {
  // 创建异常检测管理器
  let anomaly_detection_manager = AnomalyDetectionManager::new()
  
  // 配置异常检测算法
  AnomalyDetectionManager::configure_algorithms(anomaly_detection_manager, [
    {
      name: "isolation_forest",
      type: "unsupervised",
      parameters: {
        n_estimators: 100,
        contamination: 0.1,
        random_state: 42
      },
      suitable_for: ["high_dimensional", "non_linear"]
    },
    {
      name: "local_outlier_factor",
      type: "unsupervised",
      parameters: {
        n_neighbors: 20,
        contamination: 0.1,
        novelty: true
      },
      suitable_for: ["density_based", "local_anomalies"]
    },
    {
      name: "one_class_svm",
      type: "unsupervised",
      parameters: {
        kernel: "rbf",
        gamma: "scale",
        nu: 0.1
      },
      suitable_for: ["high_dimensional", "global_anomalies"]
    }
  ])
  
  // 创建自适应异常检测规则
  AnomalyDetectionManager::create_adaptive_anomaly_rule(anomaly_detection_manager, {
    rule_id: "telemetry_anomaly_detection",
    name: "遥测数据异常检测",
    metrics: ["cpu_utilization", "memory_utilization", "disk_io", "network_io"],
    adaptation_config: {
      auto_algorithm_selection: true,
      ensemble_method: "voting",
      threshold_adaptation: true,
      feedback_loop: true,
      retraining_interval_hours: 24,
      min_confidence: 0.7
    },
    notification_channels: ["email", "slack"]
  })
  
  // 生成正常和异常数据
  let training_data = []
  let test_data = []
  let base_time = Time::now() - (14 * 24 * 60 * 60 * 1000)  // 14天前
  
  // 生成正常训练数据（前10天）
  for day in 0..=9 {
    let day_start_time = base_time + day * 24 * 60 * 60 * 1000
    
    for hour in 0..=23 {
      let hour_start_time = day_start_time + hour * 60 * 60 * 1000
      
      // 正常模式：各指标之间有一定相关性
      let base_cpu = 30.0 + Math::sin(hour * Math::PI / 12) * 20.0
      let base_memory = base_cpu * 0.8 + 10.0
      let base_disk_io = base_cpu * 0.5 + 5.0
      let base_network_io = base_cpu * 1.2 + 10.0
      
      for minute in 0..=59 {
        let timestamp = hour_start_time + minute * 60 * 1000
        let noise = (Math::random() - 0.5) * 5.0
        
        training_data = training_data.push({
          timestamp: timestamp,
          metrics: {
            cpu_utilization: Math::max(0.0, Math::min(100.0, base_cpu + noise)),
            memory_utilization: Math::max(0.0, Math::min(100.0, base_memory + noise * 0.8)),
            disk_io: Math::max(0.0, base_disk_io + noise * 0.5),
            network_io: Math::max(0.0, base_network_io + noise * 1.2)
          },
          tags: [
            ("environment", "production"),
            ("region", "us-west-2"),
            ("data_type", "normal")
          ]
        })
      }
    }
  }
  
  // 生成测试数据（包含异常，后4天）
  for day in 10..=13 {
    let day_start_time = base_time + day * 24 * 60 * 60 * 1000
    
    for hour in 0..=23 {
      let hour_start_time = day_start_time + hour * 60 * 60 * 1000
      
      let base_cpu = 30.0 + Math::sin(hour * Math::PI / 12) * 20.0
      let base_memory = base_cpu * 0.8 + 10.0
      let base_disk_io = base_cpu * 0.5 + 5.0
      let base_network_io = base_cpu * 1.2 + 10.0
      
      // 在特定时间点注入异常
      let is_anomaly_hour = (day == 11 and hour >= 14 and hour <= 15) or 
                           (day == 12 and hour >= 10 and hour <= 11) or
                           (day == 13 and hour == 9)
      
      for minute in 0..=59 {
        let timestamp = hour_start_time + minute * 60 * 1000
        let noise = (Math::random() - 0.5) * 5.0
        
        let cpu = Math::max(0.0, Math::min(100.0, base_cpu + noise))
        let memory = Math::max(0.0, Math::min(100.0, base_memory + noise * 0.8))
        let disk_io = Math::max(0.0, base_disk_io + noise * 0.5)
        let network_io = Math::max(0.0, base_network_io + noise * 1.2)
        
        // 注入异常
        let (anomaly_cpu, anomaly_memory, anomaly_disk_io, anomaly_network_io) = if is_anomaly_hour {
          // 不同类型的异常
          if day == 11 {
            // CPU突增异常
            (Math::min(100.0, cpu + 40.0), memory, disk_io, network_io)
          } else if day == 12 {
            // 内存和磁盘IO异常模式
            (cpu, Math::min(100.0, memory + 30.0), disk_io + 50.0, network_io)
          } else {
            // 网络IO异常
            (cpu, memory, disk_io, network_io + 80.0)
          }
        } else {
          (cpu, memory, disk_io, network_io)
        }
        
        let data_point = {
          timestamp: timestamp,
          metrics: {
            cpu_utilization: anomaly_cpu,
            memory_utilization: anomaly_memory,
            disk_io: anomaly_disk_io,
            network_io: anomaly_network_io
          },
          tags: [
            ("environment", "production"),
            ("region", "us-west-2"),
            ("data_type", if is_anomaly_hour { "anomaly" } else { "normal" })
          ]
        }
        
        test_data = test_data.push(data_point)
      }
    }
  }
  
  // 训练异常检测模型
  let training_result = AnomalyDetectionManager::train_models(anomaly_detection_manager, training_data)
  
  // 验证训练结果
  assert_true(training_result.success)
  assert_true(training_result.trained_models.length() > 0)
  
  // 检查训练的模型
  for model in training_result.trained_models {
    assert_true(model.model_id != "")
    assert_true(model.algorithm != "")
    assert_true(model.training_score > 0.5)
    assert_true(model.feature_importance.length() > 0)
  }
  
  // 执行异常检测
  let detection_result = AnomalyDetectionManager::detect_anomalies(anomaly_detection_manager, test_data)
  
  // 验证检测结果
  assert_true(detection_result.success)
  assert_true(detection_result.anomalies.length() > 0)
  
  // 检查检测到的异常
  let detected_anomalies = detection_result.anomalies.filter(fn(a) { a.anomaly_score > 0.7 })
  assert_true(detected_anomalies.length() > 0)
  
  // 验证异常检测的准确性
  let true_anomalies = test_data.filter(fn(d) { d.tags.contains(("data_type", "anomaly")) })
  let detected_true_anomalies = detected_anomalies.filter(fn(a) { 
    test_data.any(fn(d) { d.timestamp == a.timestamp and d.tags.contains(("data_type", "anomaly")) })
  })
  
  let detection_accuracy = detected_true_anomalies.length().to_float() / true_anomalies.length().to_float()
  assert_true(detection_accuracy > 0.7)  // 检测准确率应该大于70%
  
  // 检查假阳性率
  let normal_data = test_data.filter(fn(d) { d.tags.contains(("data_type", "normal")) })
  let false_positives = detected_anomalies.filter(fn(a) { 
    test_data.any(fn(d) { d.timestamp == a.timestamp and d.tags.contains(("data_type", "normal")) })
  })
  
  let false_positive_rate = false_positives.length().to_float() / normal_data.length().to_float()
  assert_true(false_positive_rate < 0.1)  // 假阳性率应该小于10%
  
  // 测试自适应阈值调整
  let threshold_adaptation = AnomalyDetectionManager::adapt_thresholds(anomaly_detection_manager, {
    rule_id: "telemetry_anomaly_detection",
    adaptation_method: "precision_recall_optimization",
    target_precision: 0.9,
    target_recall: 0.8,
    adaptation_window_days: 7
  })
  
  // 验证阈值自适应
  assert_true(threshold_adaptation.success)
  assert_true(threshold_adaptation.new_thresholds.length() > 0)
  
  for threshold in threshold_adaptation.new_thresholds {
    assert_true(threshold.metric != "")
    assert_true(threshold.threshold_value > 0.0)
    assert_true(threshold.expected_precision > 0.0)
    assert_true(threshold.expected_recall > 0.0)
  }
  
  // 测试反馈循环
  let feedback_data = []
  for anomaly in detected_anomalies.take(10) {  // 取前10个异常作为反馈
    feedback_data = feedback_data.push({
      timestamp: anomaly.timestamp,
      actual_anomaly: true,  // 假设这些都是真实异常
      user_feedback: "true_positive",
      confidence: 0.9
    })
  }
  
  let feedback_result = AnomalyDetectionManager::incorporate_feedback(anomaly_detection_manager, feedback_data)
  
  // 验证反馈结果
  assert_true(feedback_result.success)
  assert_true(feedback_result.model_updates.length() > 0)
  
  // 检查模型更新
  for update in feedback_result.model_updates {
    assert_true(update.model_id != "")
    assert_true(update.old_performance != update.new_performance)
    assert_true(update.improvement_score > 0.0)
  }
  
  // 测试异常解释
  let anomaly_explanation = AnomalyDetectionManager::explain_anomaly(anomaly_detection_manager, {
    anomaly_timestamp: detected_anomalies[0].timestamp,
    explanation_method: "shap",
    top_features: 5
  })
  
  // 验证异常解释
  assert_true(anomaly_explanation.success)
  assert_true(anomaly_explanation.feature_contributions.length() > 0)
  
  for contribution in anomaly_explanation.feature_contributions {
    assert_true(contribution.feature != "")
    assert_true(contribution.contribution_value != 0.0)
    assert_true(contribution.importance_rank > 0)
  }
}

// 测试5: 自适应阈值性能优化
test "自适应阈值性能优化测试" {
  // 创建性能优化管理器
  let performance_optimizer = ThresholdPerformanceOptimizer::new()
  
  // 配置优化目标
  PerformanceOptimizer::set_optimization_goals(performance_optimizer, {
    primary_objectives: ["minimize_false_positives", "maximize_detection_accuracy"],
    secondary_objectives: ["minimize_alert_latency", "maximize_stability"],
    constraints: {
      max_false_positive_rate: 0.05,
      min_detection_rate: 0.9,
      max_adaptation_frequency_hours: 1,
      min_stability_period_hours: 6
    }
  })
  
  // 配置优化算法
  PerformanceOptimizer::configure_optimization_algorithms(performance_optimizer, [
    {
      name: "bayesian_optimization",
      parameters: {
        acquisition_function: "expected_improvement",
        n_initial_points: 10,
        n_iterations: 50,
        random_state: 42
      },
      suitable_for: ["expensive_objectives", "global_optimization"]
    },
    {
      name: "genetic_algorithm",
      parameters: {
        population_size: 50,
        mutation_rate: 0.1,
        crossover_rate: 0.8,
        n_generations: 20
      },
      suitable_for: ["multi_objective", "complex_search_space"]
    }
  ])
  
  // 创建待优化的阈值规则
  PerformanceOptimizer::add_rule_for_optimization(performance_optimizer, {
    rule_id: "cpu_utilization_optimized",
    name: "CPU利用率优化规则",
    metric: "cpu_utilization",
    current_threshold: 80.0,
    optimization_space: {
      min_value: 60.0,
      max_value: 95.0,
      value_type: "continuous"
    },
    adaptation_config: {
      adaptation_enabled: true,
      adaptation_sensitivity: 0.1,
      stability_weight: 0.3,
      responsiveness_weight: 0.7
    }
  })
  
  PerformanceOptimizer::add_rule_for_optimization(performance_optimizer, {
    rule_id: "memory_utilization_optimized",
    name: "内存利用率优化规则",
    metric: "memory_utilization",
    current_threshold: 85.0,
    optimization_space: {
      min_value: 70.0,
      max_value: 95.0,
      value_type: "continuous"
    },
    adaptation_config: {
      adaptation_enabled: true,
      adaptation_sensitivity: 0.15,
      stability_weight: 0.4,
      responsiveness_weight: 0.6
    }
  })
  
  // 生成历史性能数据
  let performance_history = []
  let base_time = Time::now() - (30 * 24 * 60 * 60 * 1000)  // 30天前
  
  for day in 0..=29 {
    let day_start_time = base_time + day * 24 * 60 * 60 * 1000
    
    for hour in 0..=23 {
      let hour_start_time = day_start_time + hour * 60 * 60 * 1000
      
      // 模拟不同阈值下的性能表现
      let cpu_threshold = 80.0 + (Math::random() - 0.5) * 10.0
      let memory_threshold = 85.0 + (Math::random() - 0.5) * 8.0
      
      // 模拟性能指标
      let false_positive_rate = 0.1 - (cpu_threshold - 70.0) * 0.002 + (Math::random() - 0.5) * 0.02
      let detection_rate = 0.8 + (cpu_threshold - 70.0) * 0.005 + (Math::random() - 0.5) * 0.05
      let alert_latency = 100.0 - (cpu_threshold - 70.0) * 2.0 + (Math::random() - 0.5) * 20.0
      let stability_score = 0.7 + (95.0 - cpu_threshold) * 0.01 + (Math::random() - 0.5) * 0.1
      
      performance_history = performance_history.push({
        timestamp: hour_start_time,
        rule_id: "cpu_utilization_optimized",
        threshold: cpu_threshold,
        metrics: {
          false_positive_rate: Math::max(0.0, Math::min(1.0, false_positive_rate)),
          detection_rate: Math::max(0.0, Math::min(1.0, detection_rate)),
          alert_latency: Math::max(0.0, alert_latency),
          stability_score: Math::max(0.0, Math::min(1.0, stability_score))
        },
        context: {
          hour_of_day: hour,
          day_of_week: day % 7,
          load_level: if hour >= 8 and hour <= 18 { "high" } else { "low" }
        }
      })
      
      // 内存规则性能数据
      let mem_false_positive_rate = 0.08 - (memory_threshold - 75.0) * 0.0015 + (Math::random() - 0.5) * 0.015
      let mem_detection_rate = 0.85 + (memory_threshold - 75.0) * 0.004 + (Math::random() - 0.5) * 0.04
      let mem_alert_latency = 120.0 - (memory_threshold - 75.0) * 1.5 + (Math::random() - 0.5) * 25.0
      let mem_stability_score = 0.75 + (95.0 - memory_threshold) * 0.008 + (Math::random() - 0.5) * 0.12
      
      performance_history = performance_history.push({
        timestamp: hour_start_time,
        rule_id: "memory_utilization_optimized",
        threshold: memory_threshold,
        metrics: {
          false_positive_rate: Math::max(0.0, Math::min(1.0, mem_false_positive_rate)),
          detection_rate: Math::max(0.0, Math::min(1.0, mem_detection_rate)),
          alert_latency: Math::max(0.0, mem_alert_latency),
          stability_score: Math::max(0.0, Math::min(1.0, mem_stability_score))
        },
        context: {
          hour_of_day: hour,
          day_of_week: day % 7,
          load_level: if hour >= 8 and hour <= 18 { "high" } else { "low" }
        }
      })
    }
  }
  
  // 执行性能优化
  let optimization_result = PerformanceOptimizer::optimize_thresholds(performance_optimizer, performance_history)
  
  // 验证优化结果
  assert_true(optimization_result.success)
  assert_true(optimization_result.optimized_rules.length() > 0)
  
  // 检查CPU规则优化结果
  let cpu_optimization = optimization_result.optimized_rules.find(fn(r) { r.rule_id == "cpu_utilization_optimized" })
  assert_true(cpu_optimization != None)
  
  match cpu_optimization {
    Some(optimization) {
      assert_true(optimization.old_threshold != optimization.new_threshold)
      assert_true(optimization.new_threshold >= 60.0 and optimization.new_threshold <= 95.0)
      assert_true(optimization.improvement_score > 0.0)
      assert_true(optimization.expected_performance_metrics.length() > 0)
      
      // 检查预期性能改进
      let expected_fp_rate = optimization.expected_performance_metrics.find(fn(m) { m.metric == "false_positive_rate" })
      assert_true(expected_fp_rate != None)
      
      match expected_fp_rate {
        Some(metric) {
          assert_true(metric.expected_value < 0.05)  // 应该满足约束条件
          assert_true(metric.improvement > 0.0)
        }
        None => assert_true(false)
      }
      
      let expected_detection_rate = optimization.expected_performance_metrics.find(fn(m) { m.metric == "detection_rate" })
      assert_true(expected_detection_rate != None)
      
      match expected_detection_rate {
        Some(metric) {
          assert_true(metric.expected_value > 0.9)  // 应该满足约束条件
          assert_true(metric.improvement > 0.0)
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 检查内存规则优化结果
  let memory_optimization = optimization_result.optimized_rules.find(fn(r) { r.rule_id == "memory_utilization_optimized" })
  assert_true(memory_optimization != None)
  
  // 测试多目标优化
  let multi_objective_result = PerformanceOptimizer::multi_objective_optimize(performance_optimizer, {
    rules: ["cpu_utilization_optimized", "memory_utilization_optimized"],
    objectives: ["minimize_false_positives", "maximize_detection_rate", "minimize_alert_latency"],
    optimization_method: "genetic_algorithm",
    pareto_front_size: 10
  })
  
  // 验证多目标优化结果
  assert_true(multi_objective_result.success)
  assert_true(multi_objective_result.pareto_solutions.length() > 0)
  
  // 检查帕累托前沿解
  for solution in multi_objective_result.pareto_solutions {
    assert_true(solution.rule_configurations.length() > 0)
    assert_true(solution.objective_scores.length() > 0)
    assert_true(solution.dominance_rank >= 0)
    
    // 检查约束条件
    assert_true(solution.constraint_violations.length() == 0)  // 应该没有约束违反
  }
  
  // 测试上下文感知优化
  let context_aware_result = PerformanceOptimizer::context_aware_optimize(performance_optimizer, {
    rule_id: "cpu_utilization_optimized",
    contexts: [
      { name: "business_hours", condition: "hour_of_day >= 8 and hour_of_day <= 18" },
      { name: "weekend", condition: "day_of_week == 0 or day_of_week == 6" },
      { name: "high_load", condition: "load_level == 'high'" }
    ],
    optimization_strategy: "independent"
  })
  
  // 验证上下文感知优化结果
  assert_true(context_aware_result.success)
  assert_true(context_aware_result.context_thresholds.length() > 0)
  
  // 检查各上下文的阈值
  for context_threshold in context_aware_result.context_thresholds {
    assert_true(context_threshold.context_name != "")
    assert_true(context_threshold.threshold >= 60.0 and context_threshold.threshold <= 95.0)
    assert_true(context_threshold.expected_performance.length() > 0)
  }
  
  // 测试A/B测试框架
  let ab_test_result = PerformanceOptimizer::setup_ab_test(performance_optimizer, {
    rule_id: "cpu_utilization_optimized",
    control_threshold: 80.0,
    treatment_threshold: match cpu_optimization {
      Some(optimization) => optimization.new_threshold
      None => 82.0
    },
    traffic_split: 0.1,  // 10%流量到实验组
    test_duration_days: 7,
    success_criteria: {
      min_improvement_percentage: 5.0,
      max_regression_percentage: 2.0,
      statistical_significance: 0.95
    }
  })
  
  // 验证A/B测试设置
  assert_true(ab_test_result.success)
  assert_true(ab_test_result.test_id != "")
  assert_eq(ab_test_result.control_threshold, 80.0)
  assert_true(ab_test_result.treatment_threshold != 80.0)
  assert_eq(ab_test_result.traffic_split, 0.1)
  
  // 模拟A/B测试结果
  let ab_test_results = [
    {
      timestamp: Time::now(),
      group: "control",
      false_positive_rate: 0.06,
      detection_rate: 0.88,
      alert_latency: 95.0
    },
    {
      timestamp: Time::now(),
      group: "treatment",
      false_positive_rate: 0.04,
      detection_rate: 0.92,
      alert_latency: 92.0
    }
  ]
  
  let ab_test_analysis = PerformanceOptimizer::analyze_ab_test(performance_optimizer, {
    test_id: ab_test_result.test_id,
    results: ab_test_results,
    confidence_level: 0.95
  })
  
  // 验证A/B测试分析
  assert_true(ab_test_analysis.success)
  assert_true(ab_test_analysis.statistical_significance)
  assert_true(ab_test_analysis.winner_declared)
  
  match ab_test_analysis.winner {
    Some(winner) => {
      assert_true(winner == "treatment")  // 优化后的阈值应该获胜
    }
    None => assert_true(false)
  }
  
  // 测试优化性能监控
  let optimization_monitoring = PerformanceOptimizer::setup_monitoring(performance_optimizer, {
    monitoring_interval_hours: 6,
    performance_metrics: ["false_positive_rate", "detection_rate", "alert_latency"],
    alert_thresholds: [
      { metric: "false_positive_rate", threshold: 0.1, direction: "above" },
      { metric: "detection_rate", threshold: 0.8, direction: "below" }
    ],
    auto_reoptimization: true,
    reoptimization_trigger: "performance_degradation"
  })
  
  // 验证监控设置
  assert_true(optimization_monitoring.monitoring_id != "")
  assert_eq(optimization_monitoring.monitoring_interval_hours, 6)
  assert_true(optimization_monitoring.auto_reoptimization)
  
  // 测试优化报告生成
  let optimization_report = PerformanceOptimizer::generate_optimization_report(performance_optimizer, {
    include_historical_trends: true,
    include_context_analysis: true,
    include_recommendations: true,
    report_period_days: 30
  })
  
  // 验证优化报告
  assert_true(optimization_report.report_id != "")
  assert_true(optimization_report.generated_date > 0)
  assert_true(optimization_report.overall_improvement_score > 0.0)
  assert_true(optimization_report.rule_optimizations.length() > 0)
  assert_true(optimization_report.recommendations.length() > 0)
  
  // 检查规则优化摘要
  for rule_opt in optimization_report.rule_optimizations {
    assert_true(rule_opt.rule_id != "")
    assert_true(rule_opt.old_threshold != rule_opt.new_threshold)
    assert_true(rule_opt.performance_improvements.length() > 0)
  }
}