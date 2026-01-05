// Data Flow Pipeline Tests for Azimuth Telemetry System
// This file contains test cases for data flow pipeline functionality

test "data flow pipeline basic construction" {
  let pipeline = DataFlowPipeline::new("test_pipeline")
  
  assert_eq(DataFlowPipeline::name(pipeline), "test_pipeline")
  assert_eq(DataFlowPipeline::stage_count(pipeline), 0)
  assert_true(DataFlowPipeline::is_empty(pipeline))
}

test "data flow pipeline stage addition" {
  let pipeline = DataFlowPipeline::new("multi_stage_pipeline")
  
  // Add stages
  let stage1_id = DataFlowPipeline::add_stage(
    pipeline,
    "data_validation",
    DataValidationStage::new()
  )
  
  let stage2_id = DataFlowPipeline::add_stage(
    pipeline,
    "data_transformation",
    DataTransformationStage::new()
  )
  
  let stage3_id = DataFlowPipeline::add_stage(
    pipeline,
    "data_aggregation",
    DataAggregationStage::new()
  )
  
  assert_eq(DataFlowPipeline::stage_count(pipeline), 3)
  assert_false(DataFlowPipeline::is_empty(pipeline))
  
  // Verify stage order
  let stages = DataFlowPipeline::get_stages(pipeline)
  assert_eq(stages.length(), 3)
  assert_eq(DataFlowStage::name(stages[0]), "data_validation")
  assert_eq(DataFlowStage::name(stages[1]), "data_transformation")
  assert_eq(DataFlowStage::name(stages[2]), "data_aggregation")
}

test "data flow pipeline basic execution" {
  let pipeline = DataFlowPipeline::new("simple_pipeline")
  
  // Add a simple processing stage
  DataFlowPipeline::add_stage(
    pipeline,
    "simple_processor",
    SimpleProcessorStage::new()
  )
  
  // Create test data
  let input_data = TestDataFlowData::new("test_input", [1, 2, 3, 4, 5])
  
  // Execute pipeline
  let execution_id = DataFlowPipeline::execute(pipeline, input_data)
  
  assert_true(execution_id.length() > 0)
  
  // Wait for completion and get result
  let result = DataFlowPipeline::get_execution_result(pipeline, execution_id)
  
  match result {
    Some(execution_result) => {
      assert_eq(DataFlowExecutionResult::status(execution_result), "completed")
      assert_true(DataFlowExecutionResult::output_data(execution_result).is_some())
    }
    None => assert_true(false)
  }
}

test "data flow pipeline conditional branching" {
  let pipeline = DataFlowPipeline::new("branching_pipeline")
  
  // Add conditional stage
  let condition_stage = ConditionalStage::new(|data| {
    match TestDataFlowData::value(data) {
      IntValue(v) => v > 10,
      _ => false
    }
  })
  
  DataFlowPipeline::add_stage(pipeline, "condition_check", condition_stage)
  
  // Add true branch
  DataFlowPipeline::add_stage(
    pipeline,
    "high_value_processing",
    HighValueProcessorStage::new(),
    Some("true")
  )
  
  // Add false branch
  DataFlowPipeline::add_stage(
    pipeline,
    "low_value_processing",
    LowValueProcessorStage::new(),
    Some("false")
  )
  
  // Test with high value
  let high_value_data = TestDataFlowData::new("high_test", IntValue(15))
  let high_execution_id = DataFlowPipeline::execute(pipeline, high_value_data)
  
  let high_result = DataFlowPipeline::get_execution_result(pipeline, high_execution_id)
  match high_result {
    Some(execution_result) => {
      assert_eq(DataFlowExecutionResult::status(execution_result), "completed")
      assert_eq(DataFlowExecutionResult::execution_path(execution_result), "true")
    }
    None => assert_true(false)
  }
  
  // Test with low value
  let low_value_data = TestDataFlowData::new("low_test", IntValue(5))
  let low_execution_id = DataFlowPipeline::execute(pipeline, low_value_data)
  
  let low_result = DataFlowPipeline::get_execution_result(pipeline, low_execution_id)
  match low_result {
    Some(execution_result) => {
      assert_eq(DataFlowExecutionResult::status(execution_result), "completed")
      assert_eq(DataFlowExecutionResult::execution_path(execution_result), "false")
    }
    None => assert_true(false)
  }
}

test "data flow pipeline parallel execution" {
  let pipeline = DataFlowPipeline::new("parallel_pipeline")
  
  // Add parallel execution stage
  let parallel_stage = ParallelStage::new()
  
  // Add parallel sub-stages
  ParallelStage::add_sub_stage(parallel_stage, "sub_task_1", SubTaskStage::new(1))
  ParallelStage::add_sub_stage(parallel_stage, "sub_task_2", SubTaskStage::new(2))
  ParallelStage::add_sub_stage(parallel_stage, "sub_task_3", SubTaskStage::new(3))
  
  DataFlowPipeline::add_stage(pipeline, "parallel_processing", parallel_stage)
  
  // Execute with test data
  let input_data = TestDataFlowData::new("parallel_test", "parallel_data")
  let execution_id = DataFlowPipeline::execute(pipeline, input_data)
  
  // Get result
  let result = DataFlowPipeline::get_execution_result(pipeline, execution_id)
  match result {
    Some(execution_result) => {
      assert_eq(DataFlowExecutionResult::status(execution_result), "completed")
      assert_eq(DataFlowExecutionResult::parallel_task_count(execution_result), 3)
      assert_true(DataFlowExecutionResult::total_execution_time(execution_result) > 0)
    }
    None => assert_true(false)
  }
}

test "data flow pipeline error handling" {
  let pipeline = DataFlowPipeline::new("error_handling_pipeline")
  
  // Add stage that might fail
  let error_prone_stage = ErrorProneStage::new(0.5) // 50% failure rate
  DataFlowPipeline::add_stage(pipeline, "error_prone_stage", error_prone_stage)
  
  // Add error recovery stage
  let recovery_stage = ErrorRecoveryStage::new()
  DataFlowPipeline::add_stage(pipeline, "error_recovery", recovery_stage)
  
  // Execute multiple times to test both success and failure paths
  let mut success_count = 0
  let mut failure_count = 0
  
  for i in 0..=10 {
    let input_data = TestDataFlowData::new("test_" + i.to_string(), "test_data")
    let execution_id = DataFlowPipeline::execute(pipeline, input_data)
    
    let result = DataFlowPipeline::get_execution_result(pipeline, execution_id)
    match result {
      Some(execution_result) => {
        if DataFlowExecutionResult::status(execution_result) == "completed" {
          success_count = success_count + 1
        } else if DataFlowExecutionResult::status(execution_result) == "failed" {
          failure_count = failure_count + 1
        }
      }
      None => assert_true(false)
    }
  }
  
  // We should have both successes and failures
  assert_true(success_count > 0)
  assert_true(failure_count > 0)
  assert_eq(success_count + failure_count, 11)
}

test "data flow pipeline data transformation" {
  let pipeline = DataFlowPipeline::new("transformation_pipeline")
  
  // Add transformation stages
  let json_to_xml_stage = JsonToXmlTransformationStage::new()
  let xml_to_csv_stage = XmlToCsvTransformationStage::new()
  let csv_validation_stage = CsvValidationStage::new()
  
  DataFlowPipeline::add_stage(pipeline, "json_to_xml", json_to_xml_stage)
  DataFlowPipeline::add_stage(pipeline, "xml_to_csv", xml_to_csv_stage)
  DataFlowPipeline::add_stage(pipeline, "csv_validation", csv_validation_stage)
  
  // Create JSON input data
  let json_data = TestDataFlowData::new(
    "json_input",
    "{\"users\": [{\"name\": \"Alice\", \"age\": 30}, {\"name\": \"Bob\", \"age\": 25}]}"
  )
  
  // Execute pipeline
  let execution_id = DataFlowPipeline::execute(pipeline, json_data)
  
  // Get result
  let result = DataFlowPipeline::get_execution_result(pipeline, execution_id)
  match result {
    Some(execution_result) => {
      assert_eq(DataFlowExecutionResult::status(execution_result), "completed")
      
      let output_data = DataFlowExecutionResult::output_data(execution_result)
      match output_data {
        Some(data) => {
          // Verify the final output is valid CSV
          let csv_content = TestDataFlowData::value(data)
          match csv_content {
            StringValue(content) => {
              assert_true(content.contains("name,age"))
              assert_true(content.contains("Alice,30"))
              assert_true(content.contains("Bob,25"))
            }
            _ => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

test "data flow pipeline aggregation" {
  let pipeline = DataFlowPipeline::new("aggregation_pipeline")
  
  // Add aggregation stage
  let aggregation_stage = DataAggregationStage::new()
  DataAggregationStage::set_aggregation_function(
    aggregation_stage,
    |values| {
      let mut sum = 0
      for value in values {
        match value {
          IntValue(v) => sum = sum + v,
          _ => ()
        }
      }
      IntValue(sum / values.length())
    }
  )
  
  DataFlowPipeline::add_stage(pipeline, "data_aggregation", aggregation_stage)
  
  // Create input data with multiple values
  let input_data = TestDataFlowData::new(
    "aggregation_test",
    [10, 20, 30, 40, 50]
  )
  
  // Execute pipeline
  let execution_id = DataFlowPipeline::execute(pipeline, input_data)
  
  // Get result
  let result = DataFlowPipeline::get_execution_result(pipeline, execution_id)
  match result {
    Some(execution_result) => {
      assert_eq(DataFlowExecutionResult::status(execution_result), "completed")
      
      let output_data = DataFlowExecutionResult::output_data(execution_result)
      match output_data {
        Some(data) => {
          let aggregated_value = TestDataFlowData::value(data)
          match aggregated_value {
            IntValue(value) => assert_eq(value, 30), // Average of [10, 20, 30, 40, 50]
            _ => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

test "data flow pipeline filtering" {
  let pipeline = DataFlowPipeline::new("filtering_pipeline")
  
  // Add filtering stage
  let filtering_stage = DataFilteringStage::new()
  DataFilteringStage::set_filter_condition(
    filtering_stage,
    |data| {
      match TestDataFlowData::value(data) {
        IntValue(v) => v > 25, // Filter out values <= 25
        _ => false
      }
    }
  )
  
  DataFlowPipeline::add_stage(pipeline, "data_filtering", filtering_stage)
  
  // Create input data with mixed values
  let input_data = TestDataFlowData::new(
    "filtering_test",
    [10, 20, 30, 40, 15, 25, 35, 45]
  )
  
  // Execute pipeline
  let execution_id = DataFlowPipeline::execute(pipeline, input_data)
  
  // Get result
  let result = DataFlowPipeline::get_execution_result(pipeline, execution_id)
  match result {
    Some(execution_result) => {
      assert_eq(DataFlowExecutionResult::status(execution_result), "completed")
      
      let output_data = DataFlowExecutionResult::output_data(execution_result)
      match output_data {
        Some(data) => {
          let filtered_values = TestDataFlowData::value(data)
          match filtered_values {
            ArrayIntValue(values) => {
              assert_eq(values.length(), 4) // Only [30, 40, 35, 45] should remain
              assert_false(values.contains(10))
              assert_false(values.contains(20))
              assert_false(values.contains(15))
              assert_false(values.contains(25))
            }
            _ => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

test "data flow pipeline performance monitoring" {
  let pipeline = DataFlowPipeline::new("monitored_pipeline")
  
  // Enable performance monitoring
  DataFlowPipeline::enable_performance_monitoring(pipeline)
  
  // Add some processing stages
  for i in 0..=5 {
    let stage = ProcessingStage::new_with_delay(i * 10) // Increasing delays
    DataFlowPipeline::add_stage(pipeline, "stage_" + i.to_string(), stage)
  }
  
  // Execute pipeline
  let input_data = TestDataFlowData::new("performance_test", "test_data")
  let execution_id = DataFlowPipeline::execute(pipeline, input_data)
  
  // Get result
  let result = DataFlowPipeline::get_execution_result(pipeline, execution_id)
  match result {
    Some(execution_result) => {
      assert_eq(DataFlowExecutionResult::status(execution_result), "completed")
      
      // Check performance metrics
      let metrics = DataFlowExecutionResult::performance_metrics(execution_result)
      assert_true(metrics.length() > 0)
      
      // Verify stage execution times
      let stage_metrics = DataFlowPerformanceMetrics::stage_metrics(metrics)
      assert_eq(stage_metrics.length(), 6)
      
      // Total execution time should be sum of all stage delays
      let total_time = DataFlowPerformanceMetrics::total_execution_time(metrics)
      assert_true(total_time >= 150) // 0+10+20+30+40+50 = 150ms minimum
    }
    None => assert_true(false)
  }
}

test "data flow pipeline configuration" {
  let config = DataFlowPipelineConfig::new()
    .with_max_concurrent_executions(10)
    .with_default_timeout(30000) // 30 seconds
    .with_enable_error_recovery(true)
    .with_enable_performance_monitoring(true)
    .with_retry_count(3)
    .with_retry_delay(1000) // 1 second
  
  let pipeline = DataFlowPipeline::with_config("configured_pipeline", config)
  
  assert_eq(DataFlowPipeline::max_concurrent_executions(pipeline), 10)
  assert_eq(DataFlowPipeline::default_timeout(pipeline), 30000)
  assert_true(DataFlowPipeline::error_recovery_enabled(pipeline))
  assert_true(DataFlowPipeline::performance_monitoring_enabled(pipeline))
  assert_eq(DataFlowPipeline::retry_count(pipeline), 3)
  assert_eq(DataFlowPipeline::retry_delay(pipeline), 1000)
}