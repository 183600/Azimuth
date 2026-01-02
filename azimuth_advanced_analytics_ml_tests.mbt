// Azimuth 高级分析功能测试
// 专注于测试机器学习集成、异常检测算法和预测性分析

// 测试1: 机器学习模型集成
test "机器学习模型集成测试" {
  // 创建ML模型管理器
  let ml_manager = @azimuth.analytics.MLModelManager.create({
    "model.repository": "local",
    "inference.engine": "onnx",
    "cache.enabled": true,
    "cache.size": 100
  })
  
  // 验证ML管理器创建成功
  assert_true(@azimuth.analytics.MLModelManager.is_valid(ml_manager))
  
  // 加载预训练的异常检测模型
  let model_load_result = ml_manager.load_model("anomaly_detection", {
    "model.path": "/models/anomaly_detection.onnx",
    "model.version": "1.0.0",
    "input.shape": [1, 10],
    "output.shape": [1, 2]  // [normal, anomaly]概率
  })
  
  // 验证模型加载成功
  assert_true(model_load_result.success)
  
  // 准备测试数据
  let normal_metrics = [
    [10.5, 20.3, 15.7, 30.2, 25.1, 18.9, 22.4, 19.8, 21.3, 17.6],
    [12.1, 18.7, 16.3, 28.9, 24.5, 20.2, 21.8, 18.5, 22.7, 19.1],
    [11.3, 19.5, 14.9, 29.7, 23.8, 19.6, 22.1, 19.2, 21.9, 18.3]
  ]
  
  let anomaly_metrics = [
    [45.7, 80.3, 65.2, 90.1, 75.8, 85.4, 70.6, 88.9, 72.3, 82.1],
    [50.2, 85.7, 70.4, 95.3, 80.1, 89.6, 75.2, 91.4, 77.8, 86.9]
  ]
  
  // 测试正常数据的预测
  for metrics in normal_metrics {
    let prediction = ml_manager.predict("anomaly_detection", metrics)
    
    // 验证预测结果
    assert_true(prediction.success)
    assert_eq(prediction.output.length(), 2)
    
    // 正常数据应该有较高的正常概率
    let normal_prob = prediction.output[0]
    let anomaly_prob = prediction.output[1]
    assert_true(normal_prob > 0.7)
    assert_true(anomaly_prob < 0.3)
  }
  
  // 测试异常数据的预测
  for metrics in anomaly_metrics {
    let prediction = ml_manager.predict("anomaly_detection", metrics)
    
    // 验证预测结果
    assert_true(prediction.success)
    assert_eq(prediction.output.length(), 2)
    
    // 异常数据应该有较高的异常概率
    let normal_prob = prediction.output[0]
    let anomaly_prob = prediction.output[1]
    assert_true(normal_prob < 0.3)
    assert_true(anomaly_prob > 0.7)
  }
}

// 测试2: 时间序列预测分析
test "时间序列预测分析测试" {
  // 创建时间序列预测器
  let ts_predictor = @azimuth.analytics.TimeSeriesPredictor.create({
    "algorithm": "ARIMA",
    "forecast.horizon": 10,
    "seasonality.period": 24,  // 24小时季节性
    "confidence.interval": 0.95
  })
  
  // 验证预测器创建成功
  assert_true(@azimuth.analytics.TimeSeriesPredictor.is_valid(ts_predictor))
  
  // 生成历史时间序列数据
  let historical_data = []
  let base_time = @azimuth.time.now() - 7 * 24 * 60 * 60 * 1000  // 7天前
  
  for i = 0; i < 168; i = i + 1 {  // 168小时（7天）
    let timestamp = base_time + i * 60 * 60 * 1000  // 每小时一个数据点
    let hourly_pattern = 50.0 + 20.0 * @azimuth.math.sin(i * 2 * 3.14159 / 24)  // 日周期模式
    let daily_pattern = 10.0 * @azimuth.math.sin(i * 2 * 3.14159 / 168)  // 周周期模式
    let noise = (@azimuth.math.random() - 0.5) * 10.0  // 随机噪声
    let value = hourly_pattern + daily_pattern + noise
    
    historical_data.push({
      "timestamp": timestamp,
      "value": value
    })
  }
  
  // 训练预测模型
  let train_result = ts_predictor.train(historical_data)
  
  // 验证训练成功
  assert_true(train_result.success)
  assert_true(train_result.model_accuracy > 0.8)  // 模型准确率应该大于80%
  
  // 生成未来预测
  let forecast = ts_predictor.forecast(10)  // 预测未来10个时间点
  
  // 验证预测结果
  assert_eq(forecast.length(), 10)
  
  for i = 0; i < forecast.length(); i = i + 1 {
    let prediction = forecast[i]
    
    // 验证预测结构
    assert_true(prediction.has_key("timestamp"))
    assert_true(prediction.has_key("value"))
    assert_true(prediction.has_key("lower_bound"))
    assert_true(prediction.has_key("upper_bound"))
    
    // 验证置信区间
    assert_true(prediction["lower_bound"] <= prediction["value"])
    assert_true(prediction["value"] <= prediction["upper_bound"])
    
    // 验证预测时间递增
    if i > 0 {
      assert_true(prediction["timestamp"] > forecast[i-1]["timestamp"])
    }
  }
}

// 测试3: 多维度关联分析
test "多维度关联分析测试" {
  // 创建关联分析器
  let correlation_analyzer = @azimuth.analytics.CorrelationAnalyzer.create({
    "method": "pearson",
    "min.correlation": 0.5,
    "significance.threshold": 0.05
  })
  
  // 验证关联分析器创建成功
  assert_true(@azimuth.analytics.CorrelationAnalyzer.is_valid(correlation_analyzer))
  
  // 生成多维度量数据
  let multi_dim_data = []
  for i = 0; i < 100; i = i + 1 {
    let cpu_usage = 30.0 + @azimuth.math.random() * 40.0  // 30-70%
    let memory_usage = cpu_usage * 0.8 + (@azimuth.math.random() - 0.5) * 10.0  // 与CPU相关
    let response_time = 50.0 + cpu_usage * 2.0 + (@azimuth.math.random() - 0.5) * 20.0  // 与CPU相关
    let error_rate = @azimuth.math.random() * 5.0  // 独立变量
    let request_rate = 100.0 + @azimuth.math.random() * 200.0  // 独立变量
    
    multi_dim_data.push({
      "timestamp": @azimuth.time.now() + i * 1000,
      "metrics": {
        "cpu.usage": cpu_usage,
        "memory.usage": memory_usage,
        "response.time": response_time,
        "error.rate": error_rate,
        "request.rate": request_rate
      }
    })
  }
  
  // 执行关联分析
  let correlation_result = correlation_analyzer.analyze(multi_dim_data)
  
  // 验证分析结果
  assert_true(correlation_result.success)
  assert_true(correlation_result.correlations.length() > 0)
  
  // 验证预期的关联关系
  let cpu_memory_corr = correlation_result.correlations.find(
    corr => corr.metric1 == "cpu.usage" && corr.metric2 == "memory.usage"
  )
  assert_true(cpu_memory_corr != null)
  assert_true(cpu_memory_corr.correlation > 0.5)  // 应该有正相关性
  
  let cpu_response_corr = correlation_result.correlations.find(
    corr => corr.metric1 == "cpu.usage" && corr.metric2 == "response.time"
  )
  assert_true(cpu_response_corr != null)
  assert_true(cpu_response_corr.correlation > 0.5)  // 应该有正相关性
  
  // 验证统计显著性
  for corr in correlation_result.correlations {
    assert_true(corr.p_value < 0.05)  // 应该统计显著
  }
}

// 测试4: 智能根因分析
test "智能根因分析测试" {
  // 创建根因分析器
  let rootcause_analyzer = @azimuth.analytics.RootCauseAnalyzer.create({
    "algorithm": "causal_inference",
    "max.depth": 5,
    "min.support": 0.1,
    "confidence.threshold": 0.7
  })
  
  // 验证根因分析器创建成功
  assert_true(@azimuth.analytics.RootCauseAnalyzer.is_valid(rootcause_analyzer))
  
  // 模拟系统故障场景数据
  let incident_data = {
    "incident": {
      "timestamp": @azimuth.time.now(),
      "type": "performance.degradation",
      "severity": "high",
      "duration": 1800000,  // 30分钟
      "affected.services": ["api-gateway", "user-service", "order-service"]
    },
    "metrics": [
      {"name": "cpu.usage", "values": [85.2, 87.5, 89.1, 91.3, 88.7], "threshold": 80.0},
      {"name": "memory.usage", "values": [92.1, 93.5, 94.2, 95.8, 93.9], "threshold": 90.0},
      {"name": "response.time", "values": [850, 920, 1100, 1350, 980], "threshold": 500},
      {"name": "error.rate", "values": [2.1, 3.5, 5.2, 7.8, 4.1], "threshold": 1.0},
      {"name": "database.connections", "values": [95, 98, 99, 100, 97], "threshold": 90}
    ],
    "events": [
      {"timestamp": @azimuth.time.now() - 600000, "type": "deployment", "service": "user-service"},
      {"timestamp": @azimuth.time.now() - 300000, "type": "config.change", "service": "api-gateway"},
      {"timestamp": @azimuth.time.now() - 120000, "type": "traffic.surge", "source": "external"}
    ],
    "logs": [
      {"timestamp": @azimuth.time.now() - 240000, "level": "ERROR", "service": "user-service", "message": "Database connection timeout"},
      {"timestamp": @azimuth.time.now() - 180000, "level": "WARN", "service": "api-gateway", "message": "Circuit breaker activated"},
      {"timestamp": @azimuth.time.now() - 60000, "level": "ERROR", "service": "order-service", "message": "Service unavailable"}
    ]
  }
  
  // 执行根因分析
  let analysis_result = rootcause_analyzer.analyze(incident_data)
  
  // 验证分析结果
  assert_true(analysis_result.success)
  assert_true(analysis_result.root_causes.length() > 0)
  
  // 验证根因排序（按置信度）
  let root_causes = analysis_result.root_causes
  for i = 0; i < root_causes.length() - 1; i = i + 1 {
    assert_true(root_causes[i].confidence >= root_causes[i+1].confidence)
  }
  
  // 验证主要根因
  let primary_root_cause = root_causes[0]
  assert_true(primary_root_cause.confidence > 0.7)
  assert_true(primary_root_cause.has_key("description"))
  assert_true(primary_root_cause.has_key("evidence"))
  assert_true(primary_root_cause.evidence.length() > 0)
  
  // 验证因果链
  assert_true(analysis_result.causal_chain.length() > 0)
  for i = 0; i < analysis_result.causal_chain.length() - 1; i = i + 1 {
    let current = analysis_result.causal_chain[i]
    let next = analysis_result.causal_chain[i+1]
    assert_true(current.timestamp <= next.timestamp)
  }
}

// 测试5: 智能告警和建议系统
test "智能告警和建议系统测试" {
  // 创建智能告警系统
  let alert_system = @azimuth.analytics.IntelligentAlertSystem.create({
    "ml.models": ["anomaly_detection", "trend_analysis"],
    "alert.suppression": true,
    "suppression.window": 300000,  // 5分钟
    "recommendation.engine": "rule_based"
  })
  
  // 验证告警系统创建成功
  assert_true(@azimuth.analytics.IntelligentAlertSystem.is_valid(alert_system))
  
  // 添加告警规则
  alert_system.add_rule("high_error_rate", {
    "condition": "error_rate > 5.0",
    "severity": "critical",
    "duration": 60000,  // 持续1分钟
    "suppression.key": "service:endpoint"
  })
  
  alert_system.add_rule("response_time_degradation", {
    "condition": "response_time_p95 > 1000",
    "severity": "warning",
    "duration": 300000,  // 持续5分钟
    "trend.analysis": true
  })
  
  // 模拟触发性数据
  let trigger_data = [
    {
      "timestamp": @azimuth.time.now() - 120000,
      "service": "user-service",
      "endpoint": "/api/users/profile",
      "error_rate": 6.5,
      "response_time_p95": 1200
    },
    {
      "timestamp": @azimuth.time.now() - 60000,
      "service": "user-service",
      "endpoint": "/api/users/profile",
      "error_rate": 7.2,
      "response_time_p95": 1350
    },
    {
      "timestamp": @azimuth.time.now(),
      "service": "user-service",
      "endpoint": "/api/users/profile",
      "error_rate": 8.1,
      "response_time_p95": 1500
    }
  ]
  
  // 处理数据并生成告警
  let alerts = []
  for data in trigger_data {
    let new_alerts = alert_system.process_data(data)
    for alert in new_alerts {
      alerts.push(alert)
    }
  }
  
  // 验证告警生成
  assert_true(alerts.length() > 0)
  
  // 验证告警内容
  let error_rate_alert = alerts.find(alert => alert.rule_id == "high_error_rate")
  assert_true(error_rate_alert != null)
  assert_eq(error_rate_alert.severity, "critical")
  assert_eq(error_rate_alert.service, "user-service")
  assert_eq(error_rate_alert.endpoint, "/api/users/profile")
  
  let response_time_alert = alerts.find(alert => alert.rule_id == "response_time_degradation")
  assert_true(response_time_alert != null)
  assert_eq(response_time_alert.severity, "warning")
  
  // 验证智能建议
  assert_true(error_rate_alert.has_key("recommendations"))
  assert_true(error_rate_alert.recommendations.length() > 0)
  
  for recommendation in error_rate_alert.recommendations {
    assert_true(recommendation.has_key("action"))
    assert_true(recommendation.has_key("priority"))
    assert_true(recommendation.has_key("description"))
  }
  
  // 测试告警抑制
  let duplicate_alerts = alert_system.process_data(trigger_data[2])
  assert_eq(duplicate_alerts.length(), 0)  // 应该被抑制
  
  // 验证抑制状态
  let suppression_status = alert_system.get_suppression_status()
  assert_true(suppression_status.length() > 0)
  
  // 测试告警恢复
  let recovery_data = {
    "timestamp": @azimuth.time.now() + 300000,  // 5分钟后
    "service": "user-service",
    "endpoint": "/api/users/profile",
    "error_rate": 1.2,
    "response_time_p95": 450
  }
  
  let recovery_alerts = alert_system.process_data(recovery_data)
  
  // 验证恢复告警
  let recovery_alert = recovery_alerts.find(alert => alert.type == "recovery")
  assert_true(recovery_alert != null)
  assert_eq(recovery_alert.resolved_rule_id, "high_error_rate")
}