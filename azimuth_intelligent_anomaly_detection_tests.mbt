// Azimuth 智能异常检测测试
// 专注于基于机器学习的遥测数据异常检测和预测

// 测试1: 基于统计模型的异常检测
test "基于统计模型的异常检测" {
  // 创建统计异常检测器
  let statistical_anomaly_detector = StatisticalAnomalyDetector::new()
  
  // 配置统计模型
  StatisticalAnomalyDetector::configure_models(statistical_anomaly_detector, [
    {
      name: "z_score",
      description: "Z-Score异常检测",
      parameters: {
        threshold: 3.0,
        window_size: 100,
        min_samples: 30
      },
      suitable_for: ["gaussian_distribution", "single_metric"]
    },
    {
      name: "iqr",
      description: "四分位距异常检测",
      parameters: {
        multiplier: 1.5,
        window_size: 200,
        min_samples: 50
      },
      suitable_for: ["skewed_distribution", "robust_to_outliers"]
    },
    {
      name: "moving_average",
      description: "移动平均异常检测",
      parameters: {
        window_size: 50,
        threshold_multiplier: 2.5,
        trend_adjustment: true
      },
      suitable_for: ["time_series", "trend_data"]
    },
    {
      name: "exponential_smoothing",
      description: "指数平滑异常检测",
      parameters: {
        alpha: 0.3,
        beta: 0.1,
        threshold_multiplier: 2.0,
        seasonal_periods: 24
      },
      suitable_for: ["seasonal_data", "trend_data"]
    }
  ])
  
  // 创建统计测试数据生成器
  let statistical_data_generator = StatisticalTestDataGenerator::new()
  
  // 生成正常数据
  let normal_data = statistical_data_generator.generate_normal_data({
    metrics: ["cpu_usage", "memory_usage", "request_rate", "response_time"],
    samples_per_metric: 1000,
    distribution_types: {
      "cpu_usage": "gaussian",
      "memory_usage": "gaussian",
      "request_rate": "poisson",
      "response_time": "log_normal"
    },
    distribution_parameters: {
      "cpu_usage": { mean: 50.0, std_dev: 10.0 },
      "memory_usage": { mean: 60.0, std_dev: 15.0 },
      "request_rate": { lambda: 100.0 },
      "response_time": { mu: 4.0, sigma: 1.0 }
    }
  })
  
  // 生成异常数据
  let anomaly_data = statistical_data_generator.generate_anomaly_data({
    metrics: ["cpu_usage", "memory_usage", "request_rate", "response_time"],
    base_normal_data: normal_data,
    anomaly_types: [
      {
        type: "spike",
        probability: 0.02,
        magnitude_multiplier: 3.0,
        duration: 1
      },
      {
        type: "level_shift",
        probability: 0.01,
        shift_magnitude: 2.0,
        duration: 10
      },
      {
        type: "trend_change",
        probability: 0.01,
        trend_slope_change: 0.5,
        duration: 20
      },
      {
        type: "variance_change",
        probability: 0.01,
        variance_multiplier: 3.0,
        duration: 15
      }
    ]
  })
  
  // 合并数据
  let test_data = normal_data + anomaly_data
  
  // 验证测试数据
  assert_eq(test_data.length(), 1000)
  assert_true(test_data[0].contains_key("cpu_usage"))
  assert_true(test_data[0].contains_key("memory_usage"))
  assert_true(test_data[0].contains_key("request_rate"))
  assert_true(test_data[0].contains_key("response_time"))
  
  // 测试Z-Score异常检测
  let z_score_result = StatisticalAnomalyDetector::detect_anomalies(
    statistical_anomaly_detector,
    "z_score",
    test_data,
    "cpu_usage"
  )
  
  // 验证Z-Score检测结果
  assert_true(z_score_result.detection_completed)
  assert_eq(z_score_result.total_samples, 1000)
  assert_true(z_score_result.anomalies_detected > 0)
  assert_true(z_score_result.anomaly_rate < 0.1) // 异常率应该小于10%
  
  // 验证异常详情
  for anomaly in z_score_result.anomaly_details {
    assert_true(anomaly.timestamp >= 0)
    assert_true(anomaly.value != 0)
    assert_true(anomaly.z_score.abs() > 3.0)
    assert_true(anomaly.severity == "low" or anomaly.severity == "medium" or anomaly.severity == "high")
  }
  
  // 测试IQR异常检测
  let iqr_result = StatisticalAnomalyDetector::detect_anomalies(
    statistical_anomaly_detector,
    "iqr",
    test_data,
    "memory_usage"
  )
  
  // 验证IQR检测结果
  assert_true(iqr_result.detection_completed)
  assert_true(iqr_result.anomalies_detected > 0)
  
  // IQR应该对异常值更鲁棒
  assert_true(iqr_result.anomaly_rate <= z_score_result.anomaly_rate * 1.2)
  
  // 测试移动平均异常检测
  let moving_average_result = StatisticalAnomalyDetector::detect_anomalies(
    statistical_anomaly_detector,
    "moving_average",
    test_data,
    "request_rate"
  )
  
  // 验证移动平均检测结果
  assert_true(moving_average_result.detection_completed)
  assert_true(moving_average_result.anomalies_detected > 0)
  
  // 移动平均应该能检测到趋势变化
  assert_true(moving_average_result.trend_anomalies > 0)
  
  // 测试指数平滑异常检测
  let exponential_smoothing_result = StatisticalAnomalyDetector::detect_anomalies(
    statistical_anomaly_detector,
    "exponential_smoothing",
    test_data,
    "response_time"
  )
  
  // 验证指数平滑检测结果
  assert_true(exponential_smoothing_result.detection_completed)
  assert_true(exponential_smoothing_result.anomalies_detected > 0)
  
  // 指数平滑应该能检测到季节性异常
  assert_true(exponential_smoothing_result.seasonal_anomalies >= 0)
  
  // 测试多指标异常检测
  let multi_metric_result = StatisticalAnomalyDetector::detect_multi_metric_anomalies(
    statistical_anomaly_detector,
    ["cpu_usage", "memory_usage", "response_time"],
    test_data,
    {
      correlation_threshold: 0.7,
      voting_strategy: "majority",
      anomaly_combination: "union"
    }
  )
  
  // 验证多指标异常检测
  assert_true(multi_metric_result.detection_completed)
  assert_true(multi_metric_result.anomalies_detected > 0)
  
  // 多指标检测应该有更高的准确性
  assert_true(multi_metric_result.accuracy_score > 0.8)
  
  // 验证指标间相关性
  assert_true(multi_metric_result.correlation_analysis.length() > 0)
  
  // 测试模型性能比较
  let model_comparison = StatisticalAnomalyDetector::compare_models(
    statistical_anomaly_detector,
    ["z_score", "iqr", "moving_average", "exponential_smoothing"],
    test_data,
    "cpu_usage"
  )
  
  // 验证模型比较
  assert_eq(model_comparison.model_results.length(), 4)
  
  // 验证各模型的性能指标
  for model_result in model_comparison.model_results {
    assert_true(model_result.precision > 0)
    assert_true(model_result.recall > 0)
    assert_true(model_result.f1_score > 0)
    assert_true(model_result.detection_time_ms < 1000) // 检测时间小于1秒
  }
  
  // 验证最佳模型
  assert_true(model_comparison.best_model.f1_score >= model_comparison.model_results[0].f1_score)
  
  // 测试模型参数优化
  let parameter_optimization = StatisticalAnomalyDetector::optimize_parameters(
    statistical_anomaly_detector,
    "z_score",
    test_data,
    "cpu_usage",
    {
      parameter_ranges: {
        threshold: [2.0, 3.0, 4.0],
        window_size: [50, 100, 200],
        min_samples: [20, 30, 50]
      },
      optimization_metric: "f1_score",
      cross_validation_folds: 5
    }
  )
  
  // 验证参数优化
  assert_true(parameter_optimization.optimization_completed)
  assert_true(parameter_optimization.best_parameters.threshold > 0)
  assert_true(parameter_optimization.best_parameters.window_size > 0)
  assert_true(parameter_optimization.best_parameters.min_samples > 0)
  
  // 优化后的参数应该有更好的性能
  assert_true(parameter_optimization.optimized_f1_score > parameter_optimization.original_f1_score)
  
  // 测试异常解释
  let anomaly_explanation = StatisticalAnomalyDetector::explain_anomalies(
    statistical_anomaly_detector,
    z_score_result.anomaly_details,
    {
      explanation_methods: ["statistical", "contextual", "historical"],
      include_visualization_data: true
    }
  )
  
  // 验证异常解释
  assert_eq(anomaly_explanation.explanations.length(), z_score_result.anomaly_details.length())
  
  for explanation in anomaly_explanation.explanations {
    assert_true(explanation.statistical_explanation.length() > 0)
    assert_true(explanation.anomaly_score > 0)
    assert_true(explanation.confidence > 0)
  }
}

// 测试2: 基于机器学习的异常检测
test "基于机器学习的异常检测" {
  // 创建机器学习异常检测器
  let ml_anomaly_detector = MLAnomalyDetector::new()
  
  // 配置机器学习模型
  MLAnomalyDetector::configure_models(ml_anomaly_detector, [
    {
      name: "isolation_forest",
      description: "孤立森林异常检测",
      parameters: {
        n_estimators: 100,
        max_samples: "auto",
        contamination: 0.1,
        random_state: 42
      },
      training_requirements: {
        min_samples: 1000,
        feature_scaling: true,
        cross_validation: true
      }
    },
    {
      name: "one_class_svm",
      description: "单类支持向量机异常检测",
      parameters: {
        kernel: "rbf",
        gamma: "scale",
        nu: 0.1,
        degree: 3
      },
      training_requirements: {
        min_samples: 500,
        feature_scaling: true,
        cross_validation: true
      }
    },
    {
      name: "local_outlier_factor",
      description: "局部异常因子异常检测",
      parameters: {
        n_neighbors: 20,
        algorithm: "auto",
        leaf_size: 30,
        contamination: 0.1
      },
      training_requirements: {
        min_samples: 500,
        feature_scaling: true,
        cross_validation: false
      }
    },
    {
      name: "autoencoder",
      description: "自编码器异常检测",
      parameters: {
        hidden_layers: [64, 32, 16, 32, 64],
        activation: "relu",
        optimizer: "adam",
        epochs: 100,
        batch_size: 32,
        learning_rate: 0.001
      },
      training_requirements: {
        min_samples: 2000,
        feature_scaling: true,
        cross_validation: true,
        early_stopping: true
      }
    }
  ])
  
  // 创建机器学习测试数据生成器
  let ml_data_generator = MLTestDataGenerator::new()
  
  // 生成训练数据
  let training_data = ml_data_generator.generate_training_data({
    features: [
      "cpu_usage", "memory_usage", "disk_io", "network_io",
      "request_rate", "response_time", "error_rate", "cache_hit_rate"
    ],
    normal_samples: 5000,
    anomaly_samples: 500,
    feature_relationships: [
      {
        source: "cpu_usage",
        target: "response_time",
        relationship: "linear",
        strength: 0.7
      },
      {
        source: "request_rate",
        target: "memory_usage",
        relationship: "linear",
        strength: 0.5
      },
      {
        source: "error_rate",
        target: "response_time",
        relationship: "exponential",
        strength: 0.8
      }
    ],
    noise_level: 0.1
  })
  
  // 生成测试数据
  let test_data = ml_data_generator.generate_test_data({
    features: [
      "cpu_usage", "memory_usage", "disk_io", "network_io",
      "request_rate", "response_time", "error_rate", "cache_hit_rate"
    ],
    normal_samples: 1000,
    anomaly_samples: 100,
    anomaly_types: ["point", "contextual", "collective"],
    difficulty_level: "medium"
  })
  
  // 验证测试数据
  assert_eq(training_data.samples.length(), 5500)
  assert_eq(training_data.features.length(), 8)
  assert_eq(test_data.samples.length(), 1100)
  
  // 训练孤立森林模型
  let isolation_forest_training = MLAnomalyDetector::train_model(
    ml_anomaly_detector,
    "isolation_forest",
    training_data
  )
  
  // 验证孤立森林训练
  assert_true(isolation_forest_training.training_successful)
  assert_true(isolation_forest_training.training_time_ms > 0)
  assert_true(isolation_forest_training.model_accuracy > 0.8)
  
  // 使用孤立森林进行异常检测
  let isolation_forest_detection = MLAnomalyDetector::detect_anomalies(
    ml_anomaly_detector,
    "isolation_forest",
    test_data
  )
  
  // 验证孤立森林检测结果
  assert_true(isolation_forest_detection.detection_completed)
  assert_eq(isolation_forest_detection.total_samples, 1100)
  assert_true(isolation_forest_detection.anomalies_detected > 0)
  assert_true(isolation_forest_detection.anomaly_rate > 0.05)
  assert_true(isolation_forest_detection.anomaly_rate < 0.3)
  
  // 训练单类SVM模型
  let one_class_svm_training = MLAnomalyDetector::train_model(
    ml_anomaly_detector,
    "one_class_svm",
    training_data
  )
  
  // 验证单类SVM训练
  assert_true(one_class_svm_training.training_successful)
  assert_true(one_class_svm_training.model_accuracy > 0.7)
  
  // 使用单类SVM进行异常检测
  let one_class_svm_detection = MLAnomalyDetector::detect_anomalies(
    ml_anomaly_detector,
    "one_class_svm",
    test_data
  )
  
  // 验证单类SVM检测结果
  assert_true(one_class_svm_detection.detection_completed)
  assert_true(one_class_svm_detection.anomalies_detected > 0)
  
  // 训练局部异常因子模型
  let lof_training = MLAnomalyDetector::train_model(
    ml_anomaly_detector,
    "local_outlier_factor",
    training_data
  )
  
  // 验证局部异常因子训练
  assert_true(lof_training.training_successful)
  
  // 使用局部异常因子进行异常检测
  let lof_detection = MLAnomalyDetector::detect_anomalies(
    ml_anomaly_detector,
    "local_outlier_factor",
    test_data
  )
  
  // 验证局部异常因子检测结果
  assert_true(lof_detection.detection_completed)
  assert_true(lof_detection.anomalies_detected > 0)
  
  // 训练自编码器模型
  let autoencoder_training = MLAnomalyDetector::train_model(
    ml_anomaly_detector,
    "autoencoder",
    training_data
  )
  
  // 验证自编码器训练
  assert_true(autoencoder_training.training_successful)
  assert_true(autoencoder_training.training_time_ms > isolation_forest_training.training_time_ms)
  assert_true(autoencoder_training.model_accuracy > 0.8)
  
  // 使用自编码器进行异常检测
  let autoencoder_detection = MLAnomalyDetector::detect_anomalies(
    ml_anomaly_detector,
    "autoencoder",
    test_data
  )
  
  // 验证自编码器检测结果
  assert_true(autoencoder_detection.detection_completed)
  assert_true(autoencoder_detection.anomalies_detected > 0)
  
  // 比较不同模型的性能
  let ml_model_comparison = MLAnomalyDetector::compare_models(
    ml_anomaly_detector,
    ["isolation_forest", "one_class_svm", "local_outlier_factor", "autoencoder"],
    test_data
  )
  
  // 验证模型比较
  assert_eq(ml_model_comparison.model_results.length(), 4)
  
  // 验证各模型的性能指标
  for model_result in ml_model_comparison.model_results {
    assert_true(model_result.precision > 0)
    assert_true(model_result.recall > 0)
    assert_true(model_result.f1_score > 0)
    assert_true(model_result.roc_auc > 0.5)
    assert_true(model_result.detection_time_ms > 0)
  }
  
  // 验证最佳模型
  assert_true(ml_model_comparison.best_model.f1_score >= 0.7)
  
  // 测试模型集成
  let ensemble_result = MLAnomalyDetector::create_ensemble(
    ml_anomaly_detector,
    ["isolation_forest", "one_class_svm", "local_outlier_factor"],
    {
      ensemble_method: "voting",
      voting_strategy: "soft",
      weight_strategy: "performance_based"
    }
  )
  
  // 验证模型集成
  assert_true(ensemble_result.ensemble_created)
  
  // 使用集成模型进行异常检测
  let ensemble_detection = MLAnomalyDetector::detect_with_ensemble(
    ml_anomaly_detector,
    ensemble_result.ensemble_id,
    test_data
  )
  
  // 验证集成检测结果
  assert_true(ensemble_detection.detection_completed)
  assert_true(ensemble_detection.anomalies_detected > 0)
  
  // 集成模型应该有更好的性能
  assert_true(ensemble_detection.f1_score > ml_model_comparison.best_model.f1_score * 0.95)
  
  // 测试特征重要性分析
  let feature_importance = MLAnomalyDetector::analyze_feature_importance(
    ml_anomaly_detector,
    "isolation_forest",
    training_data
  )
  
  // 验证特征重要性
  assert_eq(feature_importance.feature_importance_scores.length(), 8)
  
  // 验证特征重要性排序
  let sorted_importance = feature_importance.feature_importance_scores.sort_by(fn(a, b) { a.score > b.score })
  assert_true(sorted_importance[0].score >= sorted_importance[1].score)
  
  // 测试模型解释
  let model_explanation = MLAnomalyDetector::explain_model(
    ml_anomaly_detector,
    "isolation_forest",
    ensemble_detection.anomaly_details.slice(0, 10), // 解释前10个异常
    {
      explanation_methods: ["shap", "lime"],
      visualization_data: true
    }
  )
  
  // 验证模型解释
  assert_eq(model_explanation.explanations.length(), 10)
  
  for explanation in model_explanation.explanations {
    assert_true(explanation.feature_contributions.length() > 0)
    assert_true(explanation.prediction_score > 0)
    assert_true(explanation.explanation_confidence > 0)
  }
  
  // 测试模型漂移检测
  let drift_detection = MLAnomalyDetector::detect_model_drift(
    ml_anomaly_detector,
    "isolation_forest",
    training_data,
    test_data,
    {
      drift_detection_methods: ["ks_test", "psi", "kl_divergence"],
      drift_threshold: 0.05
    }
  )
  
  // 验证模型漂移检测
  assert_true(drift_detection.drift_analysis_completed)
  assert_eq(drift_detection.drift_metrics.length(), 3)
  
  for drift_metric in drift_detection.drift_metrics {
    assert_true(drift_metric.drift_score >= 0)
    assert_true(drift_metric.drift_detected == true or drift_metric.drift_detected == false)
  }
  
  // 测试模型增量学习
  let incremental_learning = MLAnomalyDetector::test_incremental_learning(
    ml_anomaly_detector,
    "isolation_forest",
    training_data,
    test_data,
    {
      learning_strategy: "periodic_retraining",
      retraining_interval: 1000,
      performance_threshold: 0.8,
      max_retraining_time: 60000 // 1分钟
    }
  )
  
  // 验证增量学习
  assert_true(incremental_learning.learning_test_completed)
  assert_true(incremental_learning.retraining_triggered or incremental_learning.retraining_not_needed)
  
  if incremental_learning.retraining_triggered {
    assert_true(incremental_learning.new_model_performance > incremental_learning.old_model_performance)
  }
}

// 测试3: 时间序列异常检测
test "时间序列异常检测" {
  // 创建时间序列异常检测器
  let time_series_anomaly_detector = TimeSeriesAnomalyDetector::new()
  
  // 配置时间序列模型
  TimeSeriesAnomalyDetector::configure_models(time_series_anomaly_detector, [
    {
      name: "arima",
      description: "自回归综合移动平均模型",
      parameters: {
        p: 1, // AR阶数
        d: 1, // 差分阶数
        q: 1, // MA阶数
        seasonal_p: 1,
        seasonal_d: 1,
        seasonal_q: 1,
        seasonal_period: 24
      },
      suitable_for: ["stationary_series", "seasonal_data"]
    },
    {
      name: "prophet",
      description: "Facebook Prophet时间序列预测模型",
      parameters: {
        growth: "linear",
        yearly_seasonality: true,
        weekly_seasonality: true,
        daily_seasonality: false,
        seasonality_mode: "additive",
        changepoint_prior_scale: 0.05
      },
      suitable_for: ["business_metrics", "trend_data"]
    },
    {
      name: "lstm",
      description: "长短期记忆网络",
      parameters: {
        sequence_length: 24,
        hidden_units: 50,
        dropout_rate: 0.2,
        epochs: 100,
        batch_size: 32,
        learning_rate: 0.001
      },
      suitable_for: ["complex_patterns", "non_linear_data"]
    },
    {
      name: "seasonal_decompose",
      description: "季节性分解模型",
      parameters: {
        model: "additive",
        period: 24,
        trend_method: "loess",
        robust: true
      },
      suitable_for: ["seasonal_data", "trend_analysis"]
    }
  ])
  
  // 创建时间序列测试数据生成器
  let time_series_data_generator = TimeSeriesDataGenerator::new()
  
  // 生成时间序列数据
  let time_series_data = time_series_data_generator.generate_time_series({
    metrics: ["cpu_usage", "memory_usage", "request_rate", "response_time"],
    time_range_days: 30,
    interval_minutes: 5,
    base_patterns: [
      {
        metric: "cpu_usage",
        pattern: "daily_seasonal",
        base_value: 50.0,
        amplitude: 20.0,
        noise_level: 5.0
      },
      {
        metric: "memory_usage",
        pattern: "weekly_trend",
        base_value: 60.0,
        trend_slope: 0.1,
        noise_level: 8.0
      },
      {
        metric: "request_rate",
        pattern: "hourly_burst",
        base_value: 100.0,
        burst_frequency: 8, // 每天8次突发
        burst_amplitude: 50.0,
        noise_level: 10.0
      },
      {
        metric: "response_time",
        pattern: "load_dependent",
        base_value: 50.0,
        load_factor: 0.2,
        noise_level: 5.0
      }
    ],
    anomaly_injections: [
      {
        type: "spike",
        probability: 0.01,
        magnitude_multiplier: 3.0,
        duration: 1
      },
      {
        type: "level_shift",
        probability: 0.005,
        shift_magnitude: 15.0,
        duration: 24
      },
      {
        type: "trend_change",
        probability: 0.003,
        trend_slope_change: 0.5,
        duration: 48
      }
    ]
  })
  
  // 验证时间序列数据
  assert_eq(time_series_data.metrics.length(), 4)
  assert_true(time_series_data.metrics[0].data_points.length() > 0)
  
  // 检查时间序列数据点
  for metric in time_series_data.metrics {
    assert_true(metric.data_points.length() > 0)
    assert_true(metric.anomalies_injected > 0)
  }
  
  // 测试ARIMA异常检测
  let arima_result = TimeSeriesAnomalyDetector::detect_anomalies(
    time_series_anomaly_detector,
    "arima",
    time_series_data.metrics[0] // cpu_usage
  )
  
  // 验证ARIMA检测结果
  assert_true(arima_result.detection_completed)
  assert_true(arima_result.anomalies_detected > 0)
  assert_true(arima_result.model_fitted)
  assert_true(arima_result.forecast_accuracy > 0.7)
  
  // 验证预测残差
  assert_true(arima_result.residual_analysis.length() > 0)
  assert_true(arima_result.residual_analysis[0].mean_abs_error > 0)
  
  // 测试Prophet异常检测
  let prophet_result = TimeSeriesAnomalyDetector::detect_anomalies(
    time_series_anomaly_detector,
    "prophet",
    time_series_data.metrics[1] // memory_usage
  )
  
  // 验证Prophet检测结果
  assert_true(prophet_result.detection_completed)
  assert_true(prophet_result.anomalies_detected > 0)
  
  // Prophet应该能检测到趋势变化
  assert_true(prophet_result.trend_anomalies > 0)
  assert_true(prophet_result.seasonal_anomalies >= 0)
  
  // 验证不确定性区间
  assert_true(prophet_result.uncertainty_intervals.length() > 0)
  
  // 测试LSTM异常检测
  let lstm_result = TimeSeriesAnomalyDetector::detect_anomalies(
    time_series_anomaly_detector,
    "lstm",
    time_series_data.metrics[2] // request_rate
  )
  
  // 验证LSTM检测结果
  assert_true(lstm_result.detection_completed)
  assert_true(lstm_result.anomalies_detected > 0)
  
  // LSTM应该能检测到复杂模式
  assert_true(lstm_result.complex_pattern_anomalies > 0)
  
  // 验证训练过程
  assert_true(lstm_result.training_history.length() > 0)
  assert_true(lstm_result.training_loss_decrease > 0)
  
  // 测试季节性分解异常检测
  let seasonal_decompose_result = TimeSeriesAnomalyDetector::detect_anomalies(
    time_series_anomaly_detector,
    "seasonal_decompose",
    time_series_data.metrics[3] // response_time
  )
  
  // 验证季节性分解检测结果
  assert_true(seasonal_decompose_result.detection_completed)
  assert_true(seasonal_decompose_result.anomalies_detected > 0)
  
  // 验证分解组件
  assert_true(seasonal_decompose_result.decomposition_components.length() == 4) // trend, seasonal, residual, observed
  
  // 测试多变量时间序列异常检测
  let multivariate_result = TimeSeriesAnomalyDetector::detect_multivariate_anomalies(
    time_series_anomaly_detector,
    ["cpu_usage", "memory_usage", "response_time"],
    time_series_data.metrics,
    {
      correlation_method: "pearson",
      lag_window: 5,
      threshold_method: "mahalanobis_distance",
      threshold_value: 3.0
    }
  )
  
  // 验证多变量时间序列异常检测
  assert_true(multivariate_result.detection_completed)
  assert_true(multivariate_result.anomalies_detected > 0)
  
  // 多变量检测应该能发现相关性异常
  assert_true(multivariate_result.correlation_anomalies > 0)
  
  // 验证相关性矩阵
  assert_true(multivariate_result.correlation_matrix.rows == 3)
  assert_true(multivariate_result.correlation_matrix.columns == 3)
  
  // 测试时间序列预测
  let forecast_result = TimeSeriesAnomalyDetector::forecast_time_series(
    time_series_anomaly_detector,
    "prophet",
    time_series_data.metrics[0], // cpu_usage
    {
      forecast_periods: 48, // 预测48个点（4小时）
      include_uncertainty: true,
      confidence_level: 0.95
    }
  )
  
  // 验证时间序列预测
  assert_true(forecast_result.forecast_completed)
  assert_eq(forecast_result.forecast_values.length(), 48)
  assert_true(forecast_result.uncertainty_intervals.length() == 48)
  
  // 验证预测质量
  assert_true(forecast_result.forecast_accuracy > 0.7)
  assert_true(forecast_result.mean_absolute_percentage_error < 0.2)
  
  // 测试异常预测
  let anomaly_forecast = TimeSeriesAnomalyDetector::forecast_anomalies(
    time_series_anomaly_detector,
    "arima",
    time_series_data.metrics[1], // memory_usage
    {
      forecast_periods: 24, // 预测24个点（2小时）
      anomaly_threshold: 2.0,
      confidence_threshold: 0.8
    }
  )
  
  // 验证异常预测
  assert_true(anomaly_forecast.forecast_completed)
  assert_true(anomaly_forecast.predicted_anomalies.length() > 0)
  
  // 验证预测置信度
  for predicted_anomaly in anomaly_forecast.predicted_anomalies {
    assert_true(predicted_anomaly.confidence > 0)
    assert_true(predicted_anomaly.anomaly_score > 0)
  }
  
  // 测试时间序列模型比较
  let ts_model_comparison = TimeSeriesAnomalyDetector::compare_models(
    time_series_anomaly_detector,
    ["arima", "prophet", "lstm", "seasonal_decompose"],
    time_series_data.metrics[0] // cpu_usage
  )
  
  // 验证时间序列模型比较
  assert_eq(ts_model_comparison.model_results.length(), 4)
  
  // 验证各模型的性能指标
  for model_result in ts_model_comparison.model_results {
    assert_true(model_result.mape > 0) // 平均绝对百分比误差
    assert_true(model_result.rmse > 0) // 均方根误差
    assert_true(model_result.mae > 0)  // 平均绝对误差
    assert_true(model_result.anomaly_detection_rate > 0)
    assert_true(model_result.training_time_ms > 0)
  }
  
  // 验证最佳模型
  assert_true(ts_model_comparison.best_model.mape < 0.3) // MAPE应该小于30%
  
  // 测试时间序列异常解释
  let ts_anomaly_explanation = TimeSeriesAnomalyDetector::explain_anomalies(
    time_series_anomaly_detector,
    prophet_result.anomaly_details.slice(0, 5), // 解释前5个异常
    {
      explanation_methods: ["decomposition", "contextual", "forecast_deviation"],
      include_visualization_data: true
    }
  )
  
  // 验证时间序列异常解释
  assert_eq(ts_anomaly_explanation.explanations.length(), 5)
  
  for explanation in ts_anomaly_explanation.explanations {
    assert_true(explanation.decomposition_explanation.length() > 0)
    assert_true(explanation.contextual_factors.length() > 0)
    assert_true(explanation.forecast_deviation > 0)
    assert_true(explanation.explanation_confidence > 0)
  }
}

// 测试4: 实时异常检测系统
test "实时异常检测系统" {
  // 创建实时异常检测系统
  let real_time_anomaly_system = RealTimeAnomalySystem::new()
  
  // 配置实时检测系统
  RealTimeAnomalySystem::configure(real_time_anomaly_system, {
    data_ingestion: {
      buffer_size: 10000,
      batch_size: 100,
      flush_interval: 1000, // 1秒
      parallel_consumers: 4
    },
    detection_pipeline: {
      stages: [
        {
          name: "preprocessing",
          type: "data_normalization",
          parameters: { method: "z_score" }
        },
        {
          name: "feature_extraction",
          type: "statistical_features",
          parameters: { window_size: 60 }
        },
        {
          name: "anomaly_detection",
          type: "ensemble",
          parameters: { models: ["isolation_forest", "local_outlier_factor"] }
        },
        {
          name: "postprocessing",
          type: "threshold_filtering",
          parameters: { min_confidence: 0.8 }
        }
      ],
      parallel_execution: true,
      checkpointing: true
    },
    alerting: {
      enabled: true,
      channels: ["webhook", "slack", "email"],
      alert_cooldown: 300, // 5分钟
      escalation_policy: "exponential_backoff"
    },
    storage: {
      raw_data_retention: 86400,      // 1天
      anomaly_data_retention: 604800, // 7天
      model_state_retention: 2592000  // 30天
    }
  })
  
  // 创建实时数据流模拟器
  let real_time_data_simulator = RealTimeDataSimulator::new()
  
  // 配置数据流模拟器
  RealTimeDataSimulator::configure(real_time_data_simulator, {
    metrics: [
      "cpu_usage", "memory_usage", "disk_io", "network_io",
      "request_rate", "response_time", "error_rate", "cache_hit_rate"
    ],
    data_rate: 100, // 每秒100个数据点
    duration: 300,  // 5分钟
    anomaly_injection: {
      probability: 0.02, // 2%异常概率
      types: ["spike", "level_shift", "pattern_change"],
      severity_distribution: { "low": 0.6, "medium": 0.3, "high": 0.1 }
    },
    seasonality: {
      enabled: true,
      periods: [60, 3600, 86400], // 1分钟、1小时、1天
      amplitudes: [0.1, 0.2, 0.3]
    },
    noise: {
      enabled: true,
      level: 0.05
    }
  })
  
  // 启动实时异常检测系统
  let system_startup = RealTimeAnomalySystem::start(real_time_anomaly_system)
  
  // 验证系统启动
  assert_true(system_startup.startup_successful)
  assert_true(system_startup.all_components_healthy)
  assert_true(system_startup.detection_pipeline_active)
  
  // 运行实时检测测试
  let real_time_test = RealTimeAnomalySystem::run_detection_test(
    real_time_anomaly_system,
    real_time_data_simulator,
    {
      test_duration: 300, // 5分钟
      performance_monitoring: true,
      alert_verification: true,
      latency_threshold_ms: 100,
      throughput_threshold: 50 // 每秒至少50个数据点
    }
  )
  
  // 验证实时检测测试
  assert_true(real_time_test.test_completed)
  assert_true(real_time_test.data_points_processed > 0)
  assert_true(real_time_test.anomalies_detected > 0)
  
  // 验证性能指标
  assert_true(real_time_test.average_latency_ms < 100)
  assert_true(real_time_test.p95_latency_ms < 200)
  assert_true(real_time_test.p99_latency_ms < 500)
  assert_true(real_time_test.throughput_per_sec > 50)
  
  // 验证异常检测质量
  assert_true(real_time_test.detection_accuracy > 0.8)
  assert_true(real_time_test.false_positive_rate < 0.1)
  assert_true(real_time_test.false_negative_rate < 0.2)
  
  // 验证告警系统
  if real_time_test.alerts_triggered > 0 {
    assert_true(real_time_test.alerts_sent > 0)
    assert_true(real_time_test.alert_delivery_rate > 0.9)
  }
  
  // 测试系统扩展性
  let scalability_test = RealTimeAnomalySystem::test_scalability(
    real_time_anomaly_system,
    {
      data_rates: [50, 100, 200, 500], // 每秒数据点数
      test_duration: 120, // 2分钟每个测试
      resource_limits: {
        max_memory_mb: 1024,
        max_cpu_percent: 80,
        max_disk_io_mb_per_sec: 100
      }
    }
  )
  
  // 验证扩展性测试
  assert_eq(scalability_test.test_results.length(), 4)
  
  for test_result in scalability_test.test_results {
    // 验证性能保持在可接受范围内
    assert_true(test_result.average_latency_ms < 1000)
    assert_true(test_result.memory_usage_mb < 1024)
    assert_true(test_result.cpu_usage_percent < 80)
    
    // 验证数据处理完整性
    assert_true(test_result.data_loss_rate < 0.01) // 数据丢失率小于1%
    assert_true(test_result.detection_accuracy > 0.7)
  }
  
  // 验证线性扩展性
  let first_test = scalability_test.test_results[0]
  let last_test = scalability_test.test_results[3]
  
  let data_rate_increase = last_test.data_rate.to_float() / first_test.data_rate.to_float()
  let latency_increase = last_test.average_latency_ms.to_float() / first_test.average_latency_ms.to_float()
  
  // 延迟增长应该小于数据率增长
  assert_true(latency_increase < data_rate_increase)
  
  // 测试系统容错性
  let fault_tolerance_test = RealTimeAnomalySystem::test_fault_tolerance(
    real_time_anomaly_system,
    {
      fault_scenarios: [
        {
          name: "model_failure",
          description: "检测模型失败",
          fault_type: "model_crash",
          duration: 30000, // 30秒
          expected_behavior: "fallback_to_backup_model"
        },
        {
          name: "data_pipeline_blockage",
          description: "数据处理管道阻塞",
          fault_type: "pipeline_blockage",
          duration: 20000, // 20秒
          expected_behavior: "buffer_data_and_resume"
        },
        {
          name: "alert_system_failure",
          description: "告警系统失败",
          fault_type: "alert_system_down",
          duration: 15000, // 15秒
          expected_behavior: "queue_alerts_and_retry"
        }
      ],
      recovery_verification: true
    }
  )
  
  // 验证容错性测试
  assert_eq(fault_tolerance_test.scenario_results.length(), 3)
  
  for scenario_result in fault_tolerance_test.scenario_results {
    assert_true(scenario_result.fault_injected)
    assert_true(scenario_result.recovery_successful)
    assert_true(scenario_result.data_integrity_maintained)
    assert_true(scenario_result.recovery_time_ms < 60000) // 恢复时间小于1分钟
  }
  
  // 测试模型更新
  let model_update_test = RealTimeAnomalySystem::test_model_updates(
    real_time_anomaly_system,
    {
      update_strategies: [
        {
          name: "periodic_retraining",
          interval: 60000, // 1分钟
          training_data_window: 3600, // 1小时数据
          performance_threshold: 0.8
        },
        {
          name: "drift_based_retraining",
          drift_threshold: 0.1,
          min_samples_since_update: 1000,
          performance_degradation_threshold: 0.1
        },
        {
          name: "incremental_learning",
          learning_rate: 0.01,
          batch_size: 100,
          update_frequency: 1000
        }
      ],
      test_duration: 300, // 5分钟
      performance_monitoring: true
    }
  )
  
  // 验证模型更新测试
  assert_eq(model_update_test.strategy_results.length(), 3)
  
  for strategy_result in model_update_test.strategy_results {
    assert_true(strategy_result.update_triggered or strategy_result.update_not_needed)
    
    if strategy_result.update_triggered {
      assert_true(strategy_result.update_successful)
      assert_true(strategy_result.new_model_performance > 0)
      assert_true(strategy_result.update_downtime_ms < 5000) // 更新停机时间小于5秒
    }
  }
  
  // 测试系统监控
  let system_monitoring = RealTimeAnomalySystem::test_system_monitoring(
    real_time_anomaly_system,
    {
      monitoring_duration: 180, // 3分钟
      metrics_collection_interval: 5000, // 5秒
      monitored_components: [
        "data_ingestion",
        "detection_pipeline",
        "alerting",
        "storage"
      ],
      health_checks: [
        "component_responsiveness",
        "data_flow_integrity",
        "model_performance",
        "resource_utilization"
      ]
    }
  )
  
  // 验证系统监控
  assert_true(system_monitoring.monitoring_active)
  assert_eq(system_monitoring.component_metrics.length(), 4)
  
  for component_metrics in system_monitoring.component_metrics {
    assert_true(component_metrics.health_score > 0.7) // 健康分数大于70%
    assert_true(component_metrics.response_time_ms < 1000)
    assert_true(component_metrics.error_rate < 0.05) // 错误率小于5%
  }
  
  // 验证系统整体健康状态
  assert_true(system_monitoring.overall_health_status == "healthy" or 
              system_monitoring.overall_health_status == "degraded")
  
  // 停止实时异常检测系统
  let system_shutdown = RealTimeAnomalySystem::stop(real_time_anomaly_system)
  
  // 验证系统关闭
  assert_true(system_shutdown.shutdown_successful)
  assert_true(system_shutdown.data_flushed)
  assert_true(system_shutdown.resources_released)
  
  // 生成实时异常检测系统报告
  let system_report = RealTimeAnomalySystem::generate_comprehensive_report(
    real_time_anomaly_system,
    real_time_test,
    scalability_test,
    fault_tolerance_test,
    model_update_test,
    system_monitoring
  )
  
  // 验证系统报告
  assert_true(system_report.executive_summary.length() > 0)
  assert_true(system_report.performance_analysis.length() > 0)
  assert_true(system_report.scalability_analysis.length() > 0)
  assert_true(system_report.fault_tolerance_analysis.length() > 0)
  assert_true(system_report.model_update_analysis.length() > 0)
  assert_true(system_report.monitoring_analysis.length() > 0)
  assert_true(system_report.recommendations.length() > 0)
  
  // 验证建议内容
  let performance_recommendations = system_report.recommendations.filter(
    fn(r) { r.category == "performance" }
  )
  assert_true(performance_recommendations.length() > 0)
  
  let reliability_recommendations = system_report.recommendations.filter(
    fn(r) { r.category == "reliability" }
  )
  assert_true(reliability_recommendations.length() > 0)
  
  let scalability_recommendations = system_report.recommendations.filter(
    fn(r) { r.category == "scalability" }
  )
  assert_true(scalability_recommendations.length() > 0)
}