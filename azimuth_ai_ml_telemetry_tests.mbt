// Azimuth AI/ML Telemetry Tests
// This file contains test cases for AI and machine learning functionality in the telemetry system

test "ml model training and validation" {
  // Test training data preparation
  let training_data = [
    (Features::new([1.0, 2.0, 3.0]), Label::new(0)),
    (Features::new([2.0, 3.0, 4.0]), Label::new(1)),
    (Features::new([3.0, 4.0, 5.0]), Label::new(1)),
    (Features::new([4.0, 5.0, 6.0]), Label::new(0)),
    (Features::new([5.0, 6.0, 7.0]), Label::new(1))
  ]
  
  // Test data split
  let (train_set, test_set) = MLData::split(training_data, 0.8)  // 80% training, 20% testing
  assert_eq(train_set.length(), 4)
  assert_eq(test_set.length(), 1)
  
  // Test model initialization
  let model = MLModel::linear_regression(
    input_size = 3,
    learning_rate = 0.01,
    epochs = 100
  )
  
  // Test model training
  let trained_model = MLModel::train(model, train_set)
  assert_true(MLModel::is_trained(trained_model))
  
  // Test model validation
  let validation_results = MLModel::validate(trained_model, test_set)
  assert_true(MLModel::accuracy(validation_results) >= 0.0)
  assert_true(MLModel::accuracy(validation_results) <= 1.0)
}

test "anomaly detection with unsupervised learning" {
  // Test normal telemetry data
  let normal_data = [
    TelemetryPoint::new(1609459200L, [10.5, 20.3, 30.7]),
    TelemetryPoint::new(1609459201L, [10.7, 20.1, 30.9]),
    TelemetryPoint::new(1609459202L, [10.3, 20.5, 30.5]),
    TelemetryPoint::new(1609459203L, [10.9, 19.9, 31.1]),
    TelemetryPoint::new(1609459204L, [10.1, 20.7, 30.3])
  ]
  
  // Test anomalous telemetry data
  let anomalous_data = [
    TelemetryPoint::new(1609459205L, [50.2, 20.3, 30.7]),  // First feature is anomalous
    TelemetryPoint::new(1609459206L, [10.5, 80.1, 30.9]),  // Second feature is anomalous
    TelemetryPoint::new(1609459207L, [10.3, 20.5, 95.2])   // Third feature is anomalous
  ]
  
  // Train anomaly detection model
  let anomaly_detector = AnomalyDetector::isolation_forest(
    contamination_rate = 0.1,
    n_estimators = 100
  )
  
  let trained_detector = AnomalyDetector::train(anomaly_detector, normal_data)
  
  // Test anomaly detection
  let normal_predictions = AnomalyDetector::predict(trained_detector, normal_data)
  let anomalous_predictions = AnomalyDetector::predict(trained_detector, anomalous_data)
  
  // Count anomalies in each dataset
  let normal_anomalies = normal_predictions.count(fn(p) => p == -1)  // -1 indicates anomaly
  let anomalous_anomalies = anomalous_predictions.count(fn(p) => p == -1)
  
  // Should find few or no anomalies in normal data
  assert_true(normal_anomalies <= 1)
  
  // Should find most or all anomalies in anomalous data
  assert_true(anomalous_anomalies >= 2)
}

test "time series forecasting with neural networks" {
  // Test time series data
  let time_series = [
    (1609459200L, 10.5),
    (1609459201L, 12.3),
    (1609459202L, 11.8),
    (1609459203L, 13.2),
    (1609459204L, 14.7),
    (1609459205L, 13.9),
    (1609459206L, 15.1),
    (1609459207L, 14.3),
    (1609459208L, 16.2),
    (1609459209L, 15.8)
  ]
  
  // Prepare sequences for training
  let sequence_length = 3
  let (sequences, targets) = TimeSeries::create_sequences(time_series, sequence_length)
  
  assert_eq(sequences.length(), 7)  // 10 - 3 = 7 sequences
  assert_eq(targets.length(), 7)
  
  // Create LSTM model for forecasting
  let lstm_model = NeuralNetwork::lstm(
    input_size = 1,
    hidden_size = 32,
    num_layers = 2,
    output_size = 1,
    learning_rate = 0.001,
    epochs = 50
  )
  
  // Train model
  let trained_lstm = NeuralNetwork::train(lstm_model, sequences, targets)
  assert_true(NeuralNetwork::is_trained(trained_lstm))
  
  // Test forecasting
  let last_sequence = sequences[sequences.length() - 1]
  let forecast = NeuralNetwork::forecast(trained_lstm, last_sequence, 3)  // Forecast 3 steps ahead
  
  assert_eq(forecast.length(), 3)
  
  // Verify forecast is reasonable (not extreme values)
  for value in forecast {
    assert_true(value > 0.0)
    assert_true(value < 100.0)
  }
}

test "reinforcement learning for adaptive telemetry" {
  // Test environment setup
  let env = TelemetryEnvironment::new(
    state_size = 4,  // CPU, memory, disk, network usage
    action_size = 3,  // Increase sampling, decrease sampling, maintain
    max_steps = 100
  )
  
  // Test agent initialization
  let agent = RLAgent::dqn(
    state_size = 4,
    action_size = 3,
    learning_rate = 0.001,
    epsilon = 1.0,  // Initial exploration rate
    epsilon_decay = 0.995,
    epsilon_min = 0.01
  )
  
  // Test initial state
  let initial_state = TelemetryEnvironment::reset(env)
  assert_eq(initial_state.length(), 4)
  
  // Test action selection
  let action = RLAgent::select_action(agent, initial_state)
  assert_true(action >= 0)
  assert_true(action < 3)
  
  // Test environment step
  let (next_state, reward, done) = TelemetryEnvironment::step(env, initial_state, action)
  assert_eq(next_state.length(), 4)
  assert_true(reward >= -1.0)
  assert_true(reward <= 1.0)
  
  // Test agent learning
  let experience = Experience::new(initial_state, action, reward, next_state, done)
  RLAgent::remember(agent, experience)
  RLAgent::learn(agent)
  
  // Test epsilon decay
  let initial_epsilon = RLAgent::epsilon(agent)
  RLAgent::decay_epsilon(agent)
  let decayed_epsilon = RLAgent::epsilon(agent)
  assert_true(decayed_epsilon < initial_epsilon)
}

test "natural language processing for log analysis" {
  // Test log messages
  let log_messages = [
    "INFO: System started successfully",
    "WARNING: High memory usage detected",
    "ERROR: Database connection failed",
    "INFO: User logged in",
    "ERROR: Authentication failed",
    "WARNING: Disk space running low",
    "INFO: Backup completed",
    "ERROR: Network timeout occurred"
  ]
  
  // Test text preprocessing
  let preprocessed_logs = NLPProcessor::preprocess(log_messages)
  assert_eq(preprocessed_logs.length(), log_messages.length())
  
  // Test feature extraction
  let features = NLPProcessor::extract_features(preprocessed_logs)
  assert_eq(features.length(), log_messages.length())
  
  // Test sentiment analysis
  let sentiments = NLPProcessor::analyze_sentiment(log_messages)
  assert_eq(sentiments.length(), log_messages.length())
  
  // Verify sentiment results
  let positive_count = sentiments.count(fn(s) => s == "positive")
  let negative_count = sentiments.count(fn(s) => s == "negative")
  let neutral_count = sentiments.count(fn(s) => s == "neutral")
  
  assert_true(positive_count >= 0)
  assert_true(negative_count >= 0)
  assert_true(neutral_count >= 0)
  assert_eq(positive_count + negative_count + neutral_count, log_messages.length())
  
  // Test log classification
  let categories = NLPProcessor::classify_logs(log_messages)
  assert_eq(categories.length(), log_messages.length())
  
  // Verify classification results
  let info_count = categories.count(fn(c) => c == "info")
  let warning_count = categories.count(fn(c) => c == "warning")
  let error_count = categories.count(fn(c) => c == "error")
  
  assert_eq(info_count, 3)  // INFO messages
  assert_eq(warning_count, 2)  // WARNING messages
  assert_eq(error_count, 3)  // ERROR messages
}

test "computer vision for visual telemetry" {
  // Test image preprocessing
  let image_data = ImageData::new(640, 480, 3)  // Width, height, channels (RGB)
  
  // Test image augmentation
  let augmented_images = ImageProcessor::augment(image_data, [
    "rotate_90",
    "flip_horizontal",
    "brightness_adjust",
    "contrast_adjust"
  ])
  
  assert_eq(augmented_images.length(), 4)
  
  // Test object detection
  let detection_model = ObjectDetector::yolo(
    input_size = [416, 416],
    confidence_threshold = 0.5,
    nms_threshold = 0.4
  )
  
  let detection_result = ObjectDetector::detect(detection_model, image_data)
  assert_true(ObjectDetector::confidence(detection_result) >= 0.0)
  assert_true(ObjectDetector::confidence(detection_result) <= 1.0)
  
  // Test image classification
  let classification_model = ImageClassifier::resnet50()
  let classification_result = ImageClassifier::classify(classification_model, image_data)
  
  assert_true(ImageClassifier::confidence(classification_result) >= 0.0)
  assert_true(ImageClassifier::confidence(classification_result) <= 1.0)
  assert_true(ImageClassifier::label(classification_result).length() > 0)
  
  // Test visual anomaly detection
  let reference_image = ImageData::new(640, 480, 3)
  let test_image = ImageData::new(640, 480, 3)
  
  let anomaly_score = VisualAnomalyDetector::detect(reference_image, test_image)
  assert_true(anomaly_score >= 0.0)
  assert_true(anomaly_score <= 1.0)
}

test "federated learning for distributed telemetry" {
  // Test federated learning setup
  let clients = [
    FederatedClient::new("client1", "region1"),
    FederatedClient::new("client2", "region2"),
    FederatedClient::new("client3", "region3")
  ]
  
  // Test global model initialization
  let global_model = FederatedModel::new(
    model_type = "neural_network",
    architecture = [10, 20, 10, 5],
    learning_rate = 0.01
  )
  
  // Test client data
  let client_data = [
    (0, [
      (Features::new([1.0, 2.0, 3.0]), Label::new(0)),
      (Features::new([2.0, 3.0, 4.0]), Label::new(1))
    ]),
    (1, [
      (Features::new([3.0, 4.0, 5.0]), Label::new(1)),
      (Features::new([4.0, 5.0, 6.0]), Label::new(0))
    ]),
    (2, [
      (Features::new([5.0, 6.0, 7.0]), Label::new(0)),
      (Features::new([6.0, 7.0, 8.0]), Label::new(1))
    ])
  ]
  
  // Test federated training rounds
  let num_rounds = 3
  let mut current_global_model = global_model
  
  for round in 0..num_rounds {
    // Client training
    let client_models = []
    for (client_idx, data) in client_data {
      let client_model = FederatedModel::clone(current_global_model)
      let trained_client = FederatedModel::train_on_client(client_model, data)
      client_models = client_models.push(trained_client)
    }
    
    // Model aggregation
    let aggregated_model = FederatedModel::aggregate(
      client_models,
      strategy = "fedavg",  // Federated averaging
      weights = [1.0, 1.0, 1.0]  // Equal weights for all clients
    )
    
    current_global_model = aggregated_model
  }
  
  // Test global model evaluation
  let test_data = [
    (Features::new([1.5, 2.5, 3.5]), Label::new(0)),
    (Features::new([2.5, 3.5, 4.5]), Label::new(1)),
    (Features::new([3.5, 4.5, 5.5]), Label::new(1)),
    (Features::new([4.5, 5.5, 6.5]), Label::new(0))
  ]
  
  let evaluation_results = FederatedModel::evaluate(current_global_model, test_data)
  assert_true(FederatedModel::accuracy(evaluation_results) >= 0.0)
  assert_true(FederatedModel::accuracy(evaluation_results) <= 1.0)
}

test "ml model explainability and interpretability" {
  // Test model initialization
  let model = XAIModel::random_forest(
    n_estimators = 100,
    max_depth = 10
  )
  
  // Test training data
  let training_data = [
    (Features::new([1.0, 2.0, 3.0]), Label::new(0)),
    (Features::new([2.0, 3.0, 4.0]), Label::new(1)),
    (Features::new([3.0, 4.0, 5.0]), Label::new(1)),
    (Features::new([4.0, 5.0, 6.0]), Label::new(0)),
    (Features::new([5.0, 6.0, 7.0]), Label::new(1))
  ]
  
  // Train model
  let trained_model = XAIModel::train(model, training_data)
  
  // Test prediction
  let test_features = Features::new([2.5, 3.5, 4.5])
  let prediction = XAIModel::predict(trained_model, test_features)
  assert_true(prediction == 0 || prediction == 1)
  
  // Test feature importance
  let feature_importance = XAIModel::feature_importance(trained_model)
  assert_eq(feature_importance.length(), 3)
  
  // Verify feature importance sums to 1.0
  let importance_sum = feature_importance.reduce(fn(acc, x) => acc + x, 0.0)
  assert_true(importance_sum - 1.0 < 0.001)
  
  // Test SHAP values for local explainability
  let shap_values = XAIModel::explain_prediction(trained_model, test_features)
  assert_eq(shap_values.length(), 3)
  
  // Test LIME for local explainability
  let lime_explanation = XAIModel::explain_with_lime(trained_model, test_features)
  assert_true(XAIModel::explanation_score(lime_explanation) >= 0.0)
  assert_true(XAIModel::explanation_score(lime_explanation) <= 1.0)
  
  // Test partial dependence plots
  let pdp_results = XAIModel::partial_dependence(trained_model, 0)  // First feature
  assert_true(XAIModel::pdp_values(pdp_results).length() > 0)
}

test "ml model deployment and monitoring" {
  // Test model deployment
  let model = MLModel::linear_regression(3, 0.01, 100)
  let trained_model = MLModel::train(model, [
    (Features::new([1.0, 2.0, 3.0]), Label::new(0)),
    (Features::new([2.0, 3.0, 4.0]), Label::new(1))
  ])
  
  // Test model serialization
  let serialized_model = ModelDeployment::serialize(trained_model)
  assert_true(serialized_model.length() > 0)
  
  // Test model deserialization
  let deserialized_model = ModelDeployment::deserialize(serialized_model)
  assert_true(MLModel::is_trained(deserialized_model))
  
  // Test model deployment
  let deployment_config = DeploymentConfig::new(
    endpoint = "/api/v1/predict",
    scaling_policy = "auto_scale",
    min_instances = 1,
    max_instances = 5
  )
  
  let deployment = ModelDeployment::deploy(deserialized_model, deployment_config)
  assert_true(ModelDeployment::is_healthy(deployment))
  
  // Test model monitoring
  let monitoring_config = MonitoringConfig::new(
    metrics = ["latency", "throughput", "error_rate", "drift"],
    alert_thresholds = [
      ("latency_ms", 100.0),
      ("error_rate", 0.05),
      ("drift_score", 0.2)
    ]
  )
  
  let monitoring = ModelMonitoring::start(deployment, monitoring_config)
  assert_true(ModelMonitoring::is_active(monitoring))
  
  // Test performance metrics collection
  let metrics = ModelMonitoring::collect_metrics(monitoring)
  assert_true(ModelMonitoring::latency_avg(metrics) >= 0.0)
  assert_true(ModelMonitoring::throughput(metrics) >= 0.0)
  assert_true(ModelMonitoring::error_rate(metrics) >= 0.0)
  assert_true(ModelMonitoring::error_rate(metrics) <= 1.0)
  
  // Test model drift detection
  let current_data = [
    Features::new([1.1, 2.1, 3.1]),
    Features::new([2.1, 3.1, 4.1]),
    Features::new([3.1, 4.1, 5.1])
  ]
  
  let reference_data = [
    Features::new([1.0, 2.0, 3.0]),
    Features::new([2.0, 3.0, 4.0]),
    Features::new([3.0, 4.0, 5.0])
  ]
  
  let drift_score = ModelMonitoring::detect_drift(current_data, reference_data)
  assert_true(drift_score >= 0.0)
  assert_true(drift_score <= 1.0)
  
  // Test model retraining trigger
  if drift_score > 0.2 {
    let retraining_result = ModelDeployment::trigger_retraining(deployment, current_data)
    assert_true(ModelDeployment::retraining_successful(retraining_result))
  }
}