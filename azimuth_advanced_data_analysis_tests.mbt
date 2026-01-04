// Azimuth Telemetry System - Advanced Data Analysis Tests
// This file contains comprehensive test cases for advanced data analysis functionality

// Test 1: Time Series Analysis
test "time series analysis" {
  // Create time series data
  let time_series_data = []
  let base_time = 1234567890L
  let base_value = 100.0
  
  // Generate data with trend and seasonality
  for i in 0..=100 {
    let trend = i.to_float() * 0.5
    let seasonal = 10.0 * Float::sin(2.0 * Float::pi() * i.to_float() / 12.0) // Monthly seasonality
    let noise = Random::float() * 5.0 - 2.5 // Random noise
    let value = base_value + trend + seasonal + noise
    
    time_series_data.push(TimeSeriesData::new(
      base_time + i * 86400L, // Daily data
      value,
      "metric_value"
    ))
  }
  
  // Create time series analyzer
  let analyzer = TimeSeriesAnalyzer::new(time_series_data)
  
  // Detect trend
  let trend_result = TimeSeriesAnalyzer::detect_trend(analyzer)
  assert_eq(trend_result.direction, TrendDirection::Increasing)
  assert_true(trend_result.slope > 0.4) // Should be close to 0.5
  assert_true(trend_result.confidence > 0.8) // High confidence
  
  // Detect seasonality
  let seasonality_result = TimeSeriesAnalyzer::detect_seasonality(analyzer)
  assert_true(seasonality_result.is_seasonal)
  assert_eq(seasonality_result.period, 12) // Monthly period
  assert_true(seasonality_result.strength > 0.5) // Moderate to strong seasonality
  
  // Forecast next 10 values
  let forecast = TimeSeriesAnalyzer::forecast(analyzer, 10)
  assert_eq(forecast.values.length(), 10)
  
  // Verify forecast is reasonable (should continue increasing trend)
  for i in 0..=9 {
    assert_true(forecast.values[i] > base_value + (100 + i).to_float() * 0.4)
  }
  
  // Verify forecast confidence intervals
  for i in 0..=9 {
    assert_true(forecast.upper_bounds[i] > forecast.values[i])
    assert_true(forecast.lower_bounds[i] < forecast.values[i])
  }
  
  // Detect anomalies in time series
  let anomalies = TimeSeriesAnalyzer::detect_anomalies(analyzer, 2.0) // 2 standard deviations
  assert_true(anomalies.length() > 0) // Should detect some anomalies due to noise
}

// Test 2: Statistical Analysis
test "statistical analysis" {
  // Create sample data
  let sample_data = [
    23.5, 25.1, 22.8, 26.3, 24.9, 21.7, 27.2, 25.8, 23.2, 24.5,
    26.1, 22.9, 25.4, 23.8, 24.2, 25.7, 23.1, 26.5, 24.7, 22.4,
    25.3, 23.9, 24.8, 26.2, 23.6, 25.0, 22.6, 24.4, 25.9, 23.3
  ]
  
  // Create statistical analyzer
  let analyzer = StatisticalAnalyzer::new(sample_data)
  
  // Calculate basic statistics
  let stats = StatisticalAnalyzer::basic_statistics(analyzer)
  
  assert_eq(stats.count, 30)
  assert_eq(stats.min, 21.7)
  assert_eq(stats.max, 27.2)
  assert_true(Float::abs(stats.mean - 24.5) < 0.1)
  assert_true(Float::abs(stats.median - 24.5) < 0.1)
  assert_true(stats.standard_deviation > 1.0)
  assert_true(stats.standard_deviation < 2.0)
  
  // Calculate percentiles
  let p25 = StatisticalAnalyzer::percentile(analyzer, 25)
  let p75 = StatisticalAnalyzer::percentile(analyzer, 75)
  let p90 = StatisticalAnalyzer::percentile(analyzer, 90)
  
  assert_true(p25 < stats.median)
  assert_true(p75 > stats.median)
  assert_true(p90 > p75)
  
  // Test normality
  let normality_test = StatisticalAnalyzer::test_normality(analyzer)
  assert_true(normality_test.p_value > 0.05) // Should not reject normality
  
  // Calculate confidence interval
  let confidence_interval = StatisticalAnalyzer::confidence_interval(analyzer, 0.95)
  assert_true(confidence_interval.lower < stats.mean)
  assert_true(confidence_interval.upper > stats.mean)
  assert_true(confidence_interval.upper - confidence_interval.lower > 0)
  
  // Compare with another sample
  let comparison_data = [
    28.5, 29.1, 27.8, 30.3, 28.9, 26.7, 31.2, 29.8, 27.2, 28.5
  ]
  
  let comparison_analyzer = StatisticalAnalyzer::new(comparison_data)
  let t_test = StatisticalAnalyzer::t_test(analyzer, comparison_analyzer)
  
  assert_true(t_test.p_value < 0.05) // Should reject null hypothesis of equal means
  assert_true(t_test.mean_difference > 3.0) // Means should differ significantly
}

// Test 3: Correlation Analysis
test "correlation analysis" {
  // Create correlated data
  let data_points = []
  for i in 0..=50 {
    let x = i.to_float()
    let y = 2.5 * x + 10.0 + (Random::float() * 10.0 - 5.0) // Linear relationship with noise
    let z = 30.0 - 1.5 * x + (Random::float() * 8.0 - 4.0) // Negative correlation with x
    
    data_points.push(DataPoint::new(x, y, z))
  }
  
  // Create correlation analyzer
  let analyzer = CorrelationAnalyzer::new(data_points)
  
  // Calculate Pearson correlation coefficients
  let xy_correlation = CorrelationAnalyzer::pearson(analyzer, "x", "y")
  let xz_correlation = CorrelationAnalyzer::pearson(analyzer, "x", "z")
  let yz_correlation = CorrelationAnalyzer::pearson(analyzer, "y", "z")
  
  assert_true(xy_correlation > 0.9) // Strong positive correlation
  assert_true(xz_correlation < -0.9) // Strong negative correlation
  assert_true(Float::abs(yz_correlation) < 0.5) // Weak correlation between y and z
  
  // Calculate Spearman rank correlation
  let xy_spearman = CorrelationAnalyzer::spearman(analyzer, "x", "y")
  let xz_spearman = CorrelationAnalyzer::spearman(analyzer, "x", "z")
  
  assert_true(xy_spearman > 0.9) // Strong positive correlation
  assert_true(xz_spearman < -0.9) // Strong negative correlation
  
  // Test significance of correlations
  let xy_significance = CorrelationAnalyzer::test_significance(analyzer, "x", "y")
  let xz_significance = CorrelationAnalyzer::test_significance(analyzer, "x", "z")
  
  assert_true(xy_significance.p_value < 0.001) // Highly significant
  assert_true(xz_significance.p_value < 0.001) // Highly significant
  
  // Create correlation matrix
  let correlation_matrix = CorrelationAnalyzer::correlation_matrix(analyzer)
  assert_eq(correlation_matrix.size, 3) // 3 variables
  
  assert_eq(correlation_matrix[("x", "y")], xy_correlation)
  assert_eq(correlation_matrix[("y", "x")], xy_correlation) // Symmetric
  assert_eq(correlation_matrix[("x", "z")], xz_correlation)
  assert_eq(correlation_matrix[("z", "x")], xz_correlation) // Symmetric
  assert_eq(correlation_matrix[("y", "z")], yz_correlation)
  assert_eq(correlation_matrix[("z", "y")], yz_correlation) // Symmetric
}

// Test 4: Cluster Analysis
test "cluster analysis" {
  // Create clustered data
  let cluster_data = []
  
  // Cluster 1: Centered around (10, 10)
  for i in 0..=20 {
    let x = 10.0 + Random::float() * 4.0 - 2.0
    let y = 10.0 + Random::float() * 4.0 - 2.0
    cluster_data.push(Point::new(x, y))
  }
  
  // Cluster 2: Centered around (20, 20)
  for i in 0..=20 {
    let x = 20.0 + Random::float() * 4.0 - 2.0
    let y = 20.0 + Random::float() * 4.0 - 2.0
    cluster_data.push(Point::new(x, y))
  }
  
  // Cluster 3: Centered around (10, 20)
  for i in 0..=20 {
    let x = 10.0 + Random::float() * 4.0 - 2.0
    let y = 20.0 + Random::float() * 4.0 - 2.0
    cluster_data.push(Point::new(x, y))
  }
  
  // Create cluster analyzer
  let analyzer = ClusterAnalyzer::new(cluster_data)
  
  // Determine optimal number of clusters using elbow method
  let optimal_k = ClusterAnalyzer::optimal_clusters(analyzer, 2, 6)
  assert_eq(optimal_k, 3) // Should detect 3 clusters
  
  // Perform K-means clustering
  let kmeans_result = ClusterAnalyzer::kmeans(analyzer, 3)
  assert_eq(kmeans_result.clusters.length(), 3)
  
  // Verify each point is assigned to a cluster
  let total_assigned = kmeans_result.clusters.fold_left(0, fn(acc, cluster) { acc + cluster.points.length() })
  assert_eq(total_assigned, cluster_data.length())
  
  // Verify cluster centers are reasonable
  let centers = kmeans_result.centers
  assert_true(centers.any(fn(center) { 
    Float::abs(center.x - 10.0) < 2.0 && Float::abs(center.y - 10.0) < 2.0 
  }))
  assert_true(centers.any(fn(center) { 
    Float::abs(center.x - 20.0) < 2.0 && Float::abs(center.y - 20.0) < 2.0 
  }))
  assert_true(centers.any(fn(center) { 
    Float::abs(center.x - 10.0) < 2.0 && Float::abs(center.y - 20.0) < 2.0 
  }))
  
  // Calculate cluster quality metrics
  let silhouette_score = ClusterAnalyzer::silhouette_score(kmeans_result)
  assert_true(silhouette_score > 0.5) // Good clustering
  
  // Test hierarchical clustering
  let hierarchical_result = ClusterAnalyzer::hierarchical(analyzer, 3)
  assert_eq(hierarchical_result.clusters.length(), 3)
  
  // Compare results
  let similarity = ClusterAnalyzer::compare_clusterings(kmeans_result, hierarchical_result)
  assert_true(similarity > 0.8) // Should be similar
}

// Test 5: Dimensionality Reduction
test "dimensionality reduction" {
  // Create high-dimensional data
  let high_dim_data = []
  for i in 0..=50 {
    // Create data with underlying 2D structure
    let x1 = i.to_float() / 10.0
    let x2 = Float::sin(x1)
    
    // Create 10 dimensions with linear combinations of x1 and x2 plus noise
    let dimensions = []
    for j in 0..=9 {
      let coeff1 = Random::float() * 2.0
      let coeff2 = Random::float() * 2.0
      let noise = Random::float() * 0.1 - 0.05
      let value = coeff1 * x1 + coeff2 * x2 + noise
      dimensions.push(value)
    }
    
    high_dim_data.push(HighDimPoint::new(dimensions))
  }
  
  // Create dimensionality reducer
  let reducer = DimensionalityReducer::new(high_dim_data)
  
  // Apply PCA
  let pca_result = DimensionalityReducer::pca(reducer, 2) // Reduce to 2 dimensions
  
  assert_eq(pca_result.reduced_data.length(), high_dim_data.length())
  assert_eq(pca_result.reduced_data[0].dimensions.length(), 2)
  
  // Verify explained variance
  assert_true(pca_result.explained_variance[0] > 0.5) // First component should explain most variance
  assert_true(pca_result.explained_variance[1] > 0.2) // Second component should explain significant variance
  assert_true(pca_result.explained_variance.sum() > 0.7) // Combined should explain most variance
  
  // Apply t-SNE
  let tsne_result = DimensionalityReducer::tsne(reducer, 2)
  
  assert_eq(tsne_result.reduced_data.length(), high_dim_data.length())
  assert_eq(tsne_result.reduced_data[0].dimensions.length(), 2)
  
  // Verify t-SNE preserves local structure
  let original_distances = DimensionalityReducer::calculate_pairwise_distances(high_dim_data)
  let reduced_distances = DimensionalityReducer::calculate_pairwise_distances(tsne_result.reduced_data)
  
  // Calculate correlation between distance matrices
  let distance_correlation = CorrelationAnalyzer::pearson_distances(original_distances, reduced_distances)
  assert_true(distance_correlation > 0.5) // Should preserve some structure
  
  // Apply UMAP
  let umap_result = DimensionalityReducer::umap(reducer, 2)
  
  assert_eq(umap_result.reduced_data.length(), high_dim_data.length())
  assert_eq(umap_result.reduced_data[0].dimensions.length(), 2)
}

// Test 6: Pattern Recognition
test "pattern recognition" {
  // Create data with patterns
  let pattern_data = []
  
  // Sinusoidal pattern
  for i in 0..=20 {
    let value = 10.0 * Float::sin(2.0 * Float::pi() * i.to_float() / 10.0)
    pattern_data.push(TimeSeriesData::new(1234567890L + i.to_long(), value, "sinusoidal"))
  }
  
  // Linear trend pattern
  for i in 0..=20 {
    let value = 5.0 + i.to_float() * 0.5
    pattern_data.push(TimeSeriesData::new(1234567911L + i.to_long(), value, "linear"))
  }
  
  // Step function pattern
  for i in 0..=20 {
    let value = if i < 10 { 15.0 } else { 25.0 }
    pattern_data.push(TimeSeriesData::new(1234567932L + i.to_long(), value, "step"))
  }
  
  // Create pattern recognizer
  let recognizer = PatternRecognizer::new()
  
  // Train recognizer with known patterns
  let training_data = [
    ("sinusoidal", pattern_data.slice(0, 21)),
    ("linear", pattern_data.slice(21, 42)),
    ("step", pattern_data.slice(42, 63))
  ]
  
  PatternRecognizer::train(recognizer, training_data)
  
  // Test pattern recognition
  let test_sinusoidal = []
  for i in 0..=15 {
    let value = 10.0 * Float::sin(2.0 * Float::pi() * i.to_float() / 10.0) + (Random::float() * 2.0 - 1.0)
    test_sinusoidal.push(TimeSeriesData::new(1234568000L + i.to_long(), value, "test"))
  }
  
  let recognized_pattern = PatternRecognizer::recognize(recognizer, test_sinusoidal)
  assert_eq(recognized_pattern.pattern_type, "sinusoidal")
  assert_true(recognized_pattern.confidence > 0.7)
  
  // Test with noisy data
  let noisy_linear = []
  for i in 0..=15 {
    let value = 5.0 + i.to_float() * 0.5 + (Random::float() * 4.0 - 2.0)
    noisy_linear.push(TimeSeriesData::new(1234568016L + i.to_long(), value, "test"))
  }
  
  let recognized_pattern2 = PatternRecognizer::recognize(recognizer, noisy_linear)
  assert_eq(recognized_pattern2.pattern_type, "linear")
  assert_true(recognized_pattern2.confidence > 0.5) // Lower confidence due to noise
  
  // Test anomaly detection based on pattern recognition
  let anomaly_data = []
  for i in 0..=15 {
    let value = if i == 10 { 50.0 } else { 10.0 * Float::sin(2.0 * Float::pi() * i.to_float() / 10.0) }
    anomaly_data.push(TimeSeriesData::new(1234568032L + i.to_long(), value, "test"))
  }
  
  let anomalies = PatternRecognizer::detect_anomalies(recognizer, anomaly_data)
  assert_true(anomalies.length() > 0)
  assert_true(anomalies.any(fn(anomaly) { anomaly.timestamp == 1234568042L })) // Should detect the anomaly
}

// Test 7: Predictive Modeling
test "predictive modeling" {
  // Create training data
  let training_data = []
  for i in 0..=100 {
    let x1 = Random::float() * 10.0
    let x2 = Random::float() * 10.0
    let y = 2.5 * x1 - 1.5 * x2 + 3.0 + (Random::float() * 2.0 - 1.0) // Linear relationship with noise
    
    training_data.push(TrainingExample::new([x1, x2], y))
  }
  
  // Create predictive model
  let model = PredictiveModel::new(ModelType::LinearRegression)
  
  // Train model
  let training_result = PredictiveModel::train(model, training_data)
  assert_true(training_result.success)
  assert_true(training_result.error < 1.0) // Low training error
  
  // Test model on validation data
  let validation_data = []
  for i in 0..=20 {
    let x1 = Random::float() * 10.0
    let x2 = Random::float() * 10.0
    let y = 2.5 * x1 - 1.5 * x2 + 3.0 // True relationship without noise
    
    validation_data.push(ValidationExample::new([x1, x2], y))
  }
  
  let validation_result = PredictiveModel::validate(model, validation_data)
  assert_true(validation_result.mean_squared_error < 2.0) // Low validation error
  assert_true(validation_result.r_squared > 0.8) // Good fit
  
  // Test individual predictions
  let test_features = [5.0, 3.0]
  let prediction = PredictiveModel::predict(model, test_features)
  let expected = 2.5 * 5.0 - 1.5 * 3.0 + 3.0 // 12.5 - 4.5 + 3.0 = 11.0
  assert_true(Float::abs(prediction - expected) < 1.0) // Should be close to expected
  
  // Test with different model type
  let polynomial_model = PredictiveModel::new(ModelType::PolynomialRegression)
  let poly_training_result = PredictiveModel::train(polynomial_model, training_data)
  assert_true(poly_training_result.success)
  
  let poly_validation_result = PredictiveModel::validate(polynomial_model, validation_data)
  assert_true(poly_validation_result.mean_squared_error < 2.0)
  
  // Compare models
  let model_comparison = PredictiveModel::compare([model, polynomial_model], validation_data)
  assert_true(model_comparison.length() == 2)
  assert_true(model_comparison[0].model_type == ModelType::LinearRegression)
  assert_true(model_comparison[1].model_type == ModelType::PolynomialRegression)
}

// Test 8: Multivariate Analysis
test "multivariate analysis" {
  // Create multivariate data
  let multivariate_data = []
  for i in 0..=100 {
    // Generate correlated variables
    let x1 = Random::float() * 10.0
    let x2 = 2.0 * x1 + Random::float() * 2.0 - 1.0 // Correlated with x1
    let x3 = 15.0 - x1 + Random::float() * 3.0 - 1.5 // Negatively correlated with x1
    let x4 = Random::float() * 8.0 + 5.0 // Independent variable
    
    multivariate_data.push(MultivariatePoint::new([x1, x2, x3, x4]))
  }
  
  // Create multivariate analyzer
  let analyzer = MultivariateAnalyzer::new(multivariate_data)
  
  // Calculate covariance matrix
  let covariance_matrix = MultivariateAnalyzer::covariance_matrix(analyzer)
  assert_eq(covariance_matrix.size, 4) // 4x4 matrix
  
  // Verify expected correlations
  assert_true(covariance_matrix[(0, 1)] > 0) // x1 and x2 positively correlated
  assert_true(covariance_matrix[(0, 2)] < 0) // x1 and x3 negatively correlated
  assert_true(Float::abs(covariance_matrix[(0, 3)]) < 1.0) // x1 and x4 weakly correlated
  
  // Perform principal component analysis
  let pca_result = MultivariateAnalyzer::pca(analyzer)
  
  assert_eq(pca_result.components.length(), 4) // 4 principal components
  assert_eq(pca_result.explained_variance.length(), 4)
  
  // Verify first component explains most variance
  assert_true(pca_result.explained_variance[0] > pca_result.explained_variance[1])
  assert_true(pca_result.explained_variance[1] > pca_result.explained_variance[2])
  assert_true(pca_result.explained_variance[2] > pca_result.explained_variance[3])
  
  // Calculate factor loadings
  let factor_loadings = MultivariateAnalyzer::factor_loadings(analyzer, 2) // 2 factors
  assert_eq(factor_loadings.size, 4) // 4 variables
  assert_eq(factor_loadings[0].length(), 2) // 2 factors per variable
  
  // Perform factor analysis
  let factor_analysis = MultivariateAnalyzer::factor_analysis(analyzer, 2)
  assert_eq(factor_analysis.loadings.length(), 4) // 4 variables
  assert_eq(factor_analysis.loadings[0].length(), 2) // 2 factors
  
  // Test discriminant analysis
  let class_labels = []
  for i in 0..=50 {
    class_labels.push("class_a")
  }
  for i in 0..=50 {
    class_labels.push("class_b")
  }
  
  let discriminant_result = MultivariateAnalyzer::discriminant_analysis(analyzer, class_labels)
  assert_true(discriminant_result.accuracy > 0.7) // Should achieve reasonable accuracy
  
  // Test canonical correlation analysis
  let variables_x = [0, 1] // First two variables
  let variables_y = [2, 3] // Last two variables
  
  let cca_result = MultivariateAnalyzer::canonical_correlation(analyzer, variables_x, variables_y)
  assert_eq(cca_result.correlations.length(), 2) // 2 canonical correlations
  assert_true(cca_result.correlations[0] >= cca_result.correlations[1]) // First should be largest
}