// Azimuth Serverless Computing Tests
// This file contains test cases for serverless telemetry functions and observability

// Test 1: Serverless Function Configuration and Deployment
test "serverless function configuration and deployment" {
  // Define serverless function types
  enum FunctionRuntime {
    NodeJS14
    NodeJS16
    NodeJS18
    Python38
    Python39
    Python310
    Go117
    Java11
    Java17
    Dotnet6
  }
  
  enum TriggerType {
    HTTP
    Timer
    Queue
    EventHub
    BlobStorage
    CosmosDB
    ServiceBus
    Custom
  }
  
  type ServerlessFunction = {
    name: String,
    description: String,
    runtime: FunctionRuntime,
    handler: String,
    code: String,
    environment: Array[(String, String)],
    memory_mb: Int,
    timeout_seconds: Int,
    triggers: Array[Trigger],
    bindings: Array[Binding]
  }
  
  type Trigger = {
    name: String,
    trigger_type: TriggerType,
    config: Array[(String, String)],
    direction: String  // "in", "out"
  }
  
  type Binding = {
    name: String,
    binding_type: String,
    direction: String,
    config: Array[(String, String)]
  }
  
  type DeploymentConfig = {
    provider: String,  // "aws", "azure", "gcp"
    region: String,
    resource_group: String,
    function_app: String,
    storage_account: String
  }
  
  // Create serverless function
  let create_function = fn(
    name: String,
    runtime: FunctionRuntime,
    handler: String,
    memory_mb: Int,
    timeout_seconds: Int
  ) {
    {
      name,
      description: "",
      runtime,
      handler,
      code: "",
      environment: [],
      memory_mb,
      timeout_seconds,
      triggers: [],
      bindings: []
    }
  }
  
  // Add HTTP trigger
  let add_http_trigger = fn(
    func: ServerlessFunction,
    name: String,
    route: String,
    methods: Array[String],
    auth_level: String
  ) {
    let trigger = {
      name,
      trigger_type: TriggerType::HTTP,
      config: [
        ("route", route),
        ("authLevel", auth_level),
        ("methods", methods.join(","))
      ],
      direction: "in"
    }
    
    {
      name: func.name,
      description: func.description,
      runtime: func.runtime,
      handler: func.handler,
      code: func.code,
      environment: func.environment,
      memory_mb: func.memory_mb,
      timeout_seconds: func.timeout_seconds,
      triggers: func.triggers.push(trigger),
      bindings: func.bindings
    }
  }
  
  // Add timer trigger
  let add_timer_trigger = fn(
    func: ServerlessFunction,
    name: String,
    schedule: String
  ) {
    let trigger = {
      name,
      trigger_type: TriggerType::Timer,
      config: [
        ("schedule", schedule)
      ],
      direction: "in"
    }
    
    {
      name: func.name,
      description: func.description,
      runtime: func.runtime,
      handler: func.handler,
      code: func.code,
      environment: func.environment,
      memory_mb: func.memory_mb,
      timeout_seconds: func.timeout_seconds,
      triggers: func.triggers.push(trigger),
      bindings: func.bindings
    }
  }
  
  // Add environment variable
  let add_environment_variable = fn(
    func: ServerlessFunction,
    key: String,
    value: String
  ) {
    {
      name: func.name,
      description: func.description,
      runtime: func.runtime,
      handler: func.handler,
      code: func.code,
      environment: func.environment.push((key, value)),
      memory_mb: func.memory_mb,
      timeout_seconds: func.timeout_seconds,
      triggers: func.triggers,
      bindings: func.bindings
    }
  }
  
  // Test function creation
  let telemetry_function = create_function(
    "process-telemetry",
    FunctionRuntime::NodeJS18,
    "index.handler",
    256,
    30
  )
  
  assert_eq(telemetry_function.name, "process-telemetry")
  match telemetry_function.runtime {
    FunctionRuntime::NodeJS18 => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(telemetry_function.handler, "index.handler")
  assert_eq(telemetry_function.memory_mb, 256)
  assert_eq(telemetry_function.timeout_seconds, 30)
  assert_eq(telemetry_function.triggers.length(), 0)
  
  // Add HTTP trigger
  let function_with_http = add_http_trigger(
    telemetry_function,
    "telemetryHttp",
    "telemetry",
    ["POST", "PUT"],
    "function"
  )
  
  assert_eq(function_with_http.triggers.length(), 1)
  let http_trigger = function_with_http.triggers[0]
  assert_eq(http_trigger.name, "telemetryHttp")
  match http_trigger.trigger_type {
    TriggerType::HTTP => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(http_trigger.direction, "in")
  assert_eq(http_trigger.config.length(), 3)
  assert_eq(http_trigger.config[0], ("route", "telemetry"))
  assert_eq(http_trigger.config[1], ("authLevel", "function"))
  assert_eq(http_trigger.config[2], ("methods", "POST,PUT"))
  
  // Add timer trigger
  let function_with_timer = add_timer_trigger(
    function_with_http,
    "cleanupTimer",
    "0 0 2 * * *"  // Daily at 2 AM
  )
  
  assert_eq(function_with_timer.triggers.length(), 2)
  let timer_trigger = function_with_timer.triggers[1]
  assert_eq(timer_trigger.name, "cleanupTimer")
  match timer_trigger.trigger_type {
    TriggerType::Timer => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(timer_trigger.config[0], ("schedule", "0 0 2 * * *"))
  
  // Add environment variables
  let function_with_env = add_environment_variable(
    function_with_timer,
    "TELEMETRY_CONNECTION_STRING",
    "Endpoint=sb://example.servicebus.windows.net/;SharedAccessKeyName=..."
  )
  
  let function_with_more_env = add_environment_variable(
    function_with_env,
    "LOG_LEVEL",
    "info"
  )
  
  assert_eq(function_with_more_env.environment.length(), 2)
  assert_eq(function_with_more_env.environment[0], ("TELEMETRY_CONNECTION_STRING", "Endpoint=sb://example.servicebus.windows.net/;SharedAccessKeyName=..."))
  assert_eq(function_with_more_env.environment[1], ("LOG_LEVEL", "info"))
}

// Test 2: Serverless Function Telemetry Integration
test "serverless function telemetry integration" {
  // Define telemetry integration types
  enum TelemetryProvider {
    ApplicationInsights
    OpenTelemetry
    Prometheus
    Datadog
    NewRelic
  }
  
  type TelemetryConfig = {
    provider: TelemetryProvider,
    connection_string: String,
    instrumentation_key: Option<String>,
    sampling_percentage: Float,
    custom_dimensions: Array[(String, String)]
  }
  
  type FunctionTelemetry = {
    function_name: String,
    invocation_id: String,
    start_time: Int,
    end_time: Int,
    duration: Int,
    status: String,
    input_data: String,
    output_data: String,
    exception: Option[String],
    custom_metrics: Array[(String, Float)],
    custom_properties: Array[(String, String)]
  }
  
  type TelemetryCollector = {
    config: TelemetryConfig,
    collected_data: Array[FunctionTelemetry],
    aggregated_metrics: Array[(String, Float)]
  }
  
  // Create telemetry collector
  let create_collector = fn(
    provider: TelemetryProvider,
    connection_string: String
  ) {
    let config = {
      provider,
      connection_string,
      instrumentation_key: None,
      sampling_percentage: 100.0,
      custom_dimensions: []
    }
    
    {
      config,
      collected_data: [],
      aggregated_metrics: []
    }
  }
  
  // Record function execution
  let record_execution = fn(
    collector: TelemetryCollector,
    telemetry: FunctionTelemetry
  ) {
    {
      config: collector.config,
      collected_data: collector.collected_data.push(telemetry),
      aggregated_metrics: collector.aggregated_metrics
    }
  }
  
  // Calculate aggregated metrics
  let calculate_metrics = fn(collector: TelemetryCollector) {
    let mut total_invocations = Int::to_float(collector.collected_data.length())
    let mut total_duration = 0.0
    let mut success_count = 0.0
    let mut failure_count = 0.0
    
    for telemetry in collector.collected_data {
      total_duration = total_duration + Int::to_float(telemetry.duration)
      
      if telemetry.status == "success" {
        success_count = success_count + 1.0
      } else {
        failure_count = failure_count + 1.0
      }
    }
    
    let avg_duration = if total_invocations > 0.0 {
      total_duration / total_invocations
    } else {
      0.0
    }
    
    let success_rate = if total_invocations > 0.0 {
      (success_count / total_invocations) * 100.0
    } else {
      0.0
    }
    
    [
      ("total_invocations", total_invocations),
      ("avg_duration_ms", avg_duration),
      ("success_rate_percent", success_rate),
      ("success_count", success_count),
      ("failure_count", failure_count)
    ]
  }
  
  // Update aggregated metrics
  let update_aggregated_metrics = fn(collector: TelemetryCollector) {
    let metrics = calculate_metrics(collector)
    
    {
      config: collector.config,
      collected_data: collector.collected_data,
      aggregated_metrics: metrics
    }
  }
  
  // Test telemetry collection
  let app_insights_collector = create_collector(
    TelemetryProvider::ApplicationInsights,
    "InstrumentationKey=00000000-0000-0000-0000-000000000000;IngestionEndpoint=https://example.applicationinsights.azure.com/"
  )
  
  assert_eq(app_insights_collector.config.connection_string, "InstrumentationKey=00000000-0000-0000-0000-000000000000;IngestionEndpoint=https://example.applicationinsights.azure.com/")
  match app_insights_collector.config.provider {
    TelemetryProvider::ApplicationInsights => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(app_insights_collector.config.sampling_percentage, 100.0)
  
  // Create test telemetry data
  let telemetry1 = {
    function_name: "process-telemetry",
    invocation_id: "inv-001",
    start_time: 1000,
    end_time: 1050,
    duration: 50,
    status: "success",
    input_data: "{\"metric\": \"cpu_usage\", \"value\": 75.5}",
    output_data: "{\"status\": \"processed\", \"id\": \"metric-001\"}",
    exception: None,
    custom_metrics: [("cpu_time", 25.5), ("memory_mb", 128.0)],
    custom_properties: [("region", "us-east"), ("version", "v1.2.3")]
  }
  
  let telemetry2 = {
    function_name: "process-telemetry",
    invocation_id: "inv-002",
    start_time: 1100,
    end_time: 1180,
    duration: 80,
    status: "success",
    input_data: "{\"metric\": \"memory_usage\", \"value\": 60.2}",
    output_data: "{\"status\": \"processed\", \"id\": \"metric-002\"}",
    exception: None,
    custom_metrics: [("cpu_time", 40.2), ("memory_mb", 156.0)],
    custom_properties: [("region", "us-east"), ("version", "v1.2.3")]
  }
  
  let telemetry3 = {
    function_name: "process-telemetry",
    invocation_id: "inv-003",
    start_time: 1200,
    end_time: 1250,
    duration: 50,
    status: "error",
    input_data: "{\"metric\": \"invalid_data\", \"value\": \"not_a_number\"}",
    output_data: "",
    exception: Some("Invalid metric value: not_a_number"),
    custom_metrics: [("cpu_time", 20.1), ("memory_mb", 98.0)],
    custom_properties: [("region", "us-east"), ("version", "v1.2.3")]
  }
  
  // Record executions
  let collector_with_data1 = record_execution(app_insights_collector, telemetry1)
  let collector_with_data2 = record_execution(collector_with_data1, telemetry2)
  let collector_with_data3 = record_execution(collector_with_data2, telemetry3)
  
  assert_eq(collector_with_data3.collected_data.length(), 3)
  
  // Update aggregated metrics
  let collector_with_metrics = update_aggregated_metrics(collector_with_data3)
  
  assert_eq(collector_with_metrics.aggregated_metrics.length(), 5)
  assert_eq(collector_with_metrics.aggregated_metrics[0], ("total_invocations", 3.0))
  assert_eq(collector_with_metrics.aggregated_metrics[1], ("avg_duration_ms", 60.0))  // (50 + 80 + 50) / 3
  assert_eq(collector_with_metrics.aggregated_metrics[2], ("success_rate_percent", 66.66666666666667))  // (2 / 3) * 100
  assert_eq(collector_with_metrics.aggregated_metrics[3], ("success_count", 2.0))
  assert_eq(collector_with_metrics.aggregated_metrics[4], ("failure_count", 1.0))
}

// Test 3: Serverless Function Scaling and Performance
test "serverless function scaling and performance optimization" {
  // Define scaling and performance types
  enum ScalingType {
    Consumption  // Automatic scaling
    Premium      // Pre-warmed instances
    Dedicated    // Fixed instances
  }
  
  type ScalingConfig = {
    scaling_type: ScalingType,
    min_instances: Int,
    max_instances: Int,
    burst_capacity: Int,
    pre_warmed_instances: Int
  }
  
  type PerformanceMetrics = {
    function_name: String,
    timestamp: Int,
    active_instances: Int,
    concurrent_executions: Int,
    queue_length: Int,
    avg_execution_time: Float,
    memory_usage_mb: Float,
    cpu_usage_percent: Float,
    cold_starts: Int
  }
  
  type ScalingEvent = {
    timestamp: Int,
    old_instance_count: Int,
    new_instance_count: Int,
    reason: String,
    trigger: String  // "load", "queue", "scheduled"
  }
  
  type FunctionScaling = {
    function_name: String,
    config: ScalingConfig,
    current_instances: Int,
    scaling_history: Array[ScalingEvent],
    performance_metrics: Array[PerformanceMetrics]
  }
  
  // Calculate recommended instance count
  let calculate_recommended_instances = fn(
    metrics: PerformanceMetrics,
    config: ScalingConfig
  ) {
    let recommended = match config.scaling_type {
      ScalingType::Consumption => {
        // For consumption plan, scale based on queue length and execution time
        let queue_factor = Int::to_float(metrics.queue_length) / 10.0
        let time_factor = metrics.avg_execution_time / 1000.0  // Convert to seconds
        let base_instances = 1.0
        
        let recommended_float = base_instances + queue_factor + time_factor
        Int::floor(recommended_float + 0.5)  // Round to nearest integer
      }
      ScalingType::Premium => {
        // For premium plan, maintain pre-warmed instances
        let base_instances = Int::to_float(config.pre_warmed_instances)
        let additional = if metrics.concurrent_executions > config.pre_warmed_instances {
          Int::to_float(metrics.concurrent_executions - config.pre_warmed_instances) / 2.0
        } else {
          0.0
        }
        
        Int::floor(base_instances + additional + 0.5)
      }
      ScalingType::Dedicated => {
        // For dedicated plan, use fixed instances
        config.min_instances
      }
    }
    
    // Ensure within bounds
    if recommended < config.min_instances {
      config.min_instances
    } else if recommended > config.max_instances {
      config.max_instances
    } else {
      recommended
    }
  }
  
  // Simulate scaling event
  let simulate_scaling = fn(
    scaling: FunctionScaling,
    current_metrics: PerformanceMetrics,
    current_time: Int
  ) {
    let recommended = calculate_recommended_instances(current_metrics, scaling.config)
    
    if recommended != scaling.current_instances {
      let event = {
        timestamp: current_time,
        old_instance_count: scaling.current_instances,
        new_instance_count: recommended,
        reason: if recommended > scaling.current_instances {
          "Increased instances due to high load"
        } else {
          "Decreased instances due to low load"
        },
        trigger: "load"
      }
      
      {
        function_name: scaling.function_name,
        config: scaling.config,
        current_instances: recommended,
        scaling_history: scaling.scaling_history.push(event),
        performance_metrics: scaling.performance_metrics.push(current_metrics)
      }
    } else {
      {
        function_name: scaling.function_name,
        config: scaling.config,
        current_instances: scaling.current_instances,
        scaling_history: scaling.scaling_history,
        performance_metrics: scaling.performance_metrics.push(current_metrics)
      }
    }
  }
  
  // Test scaling calculations
  let consumption_config = {
    scaling_type: ScalingType::Consumption,
    min_instances: 0,
    max_instances: 100,
    burst_capacity: 1000,
    pre_warmed_instances: 0
  }
  
  let premium_config = {
    scaling_type: ScalingType::Premium,
    min_instances: 1,
    max_instances: 20,
    burst_capacity: 500,
    pre_warmed_instances: 3
  }
  
  let dedicated_config = {
    scaling_type: ScalingType::Dedicated,
    min_instances: 5,
    max_instances: 5,
    burst_capacity: 0,
    pre_warmed_instances: 0
  }
  
  // Create test metrics
  let low_load_metrics = {
    function_name: "process-telemetry",
    timestamp: 1000,
    active_instances: 1,
    concurrent_executions: 2,
    queue_length: 5,
    avg_execution_time: 50.0,
    memory_usage_mb: 128.0,
    cpu_usage_percent: 25.0,
    cold_starts: 0
  }
  
  let high_load_metrics = {
    function_name: "process-telemetry",
    timestamp: 2000,
    active_instances: 3,
    concurrent_executions: 15,
    queue_length: 50,
    avg_execution_time: 200.0,
    memory_usage_mb: 256.0,
    cpu_usage_percent: 80.0,
    cold_starts: 1
  }
  
  // Test scaling calculations for different plans
  let consumption_recommended_low = calculate_recommended_instances(low_load_metrics, consumption_config)
  let consumption_recommended_high = calculate_recommended_instances(high_load_metrics, consumption_config)
  
  assert_eq(consumption_recommended_low, 1)  // 1 + 0.5 + 0.05 ≈ 1.55 → 2, but min is 0, so 1
  assert_eq(consumption_recommended_high, 7)  // 1 + 5.0 + 0.2 ≈ 6.2 → 6
  
  let premium_recommended_low = calculate_recommended_instances(low_load_metrics, premium_config)
  let premium_recommended_high = calculate_recommended_instances(high_load_metrics, premium_config)
  
  assert_eq(premium_recommended_low, 3)  // Pre-warmed instances
  assert_eq(premium_recommended_high, 9)  // 3 + (15 - 3) / 2 = 3 + 6 = 9
  
  let dedicated_recommended = calculate_recommended_instances(high_load_metrics, dedicated_config)
  assert_eq(dedicated_recommended, 5)  // Fixed instances
  
  // Test scaling simulation
  let initial_scaling = {
    function_name: "process-telemetry",
    config: consumption_config,
    current_instances: 1,
    scaling_history: [],
    performance_metrics: []
  }
  
  let scaling_after_low_load = simulate_scaling(initial_scaling, low_load_metrics, 1000)
  assert_eq(scaling_after_low_load.current_instances, 1)  // No change needed
  assert_eq(scaling_after_low_load.scaling_history.length(), 0)
  
  let scaling_after_high_load = simulate_scaling(scaling_after_low_load, high_load_metrics, 2000)
  assert_eq(scaling_after_high_load.current_instances, 7)  // Scale up
  assert_eq(scaling_after_high_load.scaling_history.length(), 1)
  
  let scaling_event = scaling_after_high_load.scaling_history[0]
  assert_eq(scaling_event.timestamp, 2000)
  assert_eq(scaling_event.old_instance_count, 1)
  assert_eq(scaling_event.new_instance_count, 7)
  assert_eq(scaling_event.reason, "Increased instances due to high load")
  assert_eq(scaling_event.trigger, "load")
}

// Test 4: Serverless Function Error Handling and Resilience
test "serverless function error handling and resilience patterns" {
  // Define error handling types
  enum ErrorType {
    Timeout
    MemoryLimit
    UnhandledException
    Throttling
    DependencyFailure
    ConfigurationError
  }
  
  enum RetryPolicy {
    FixedDelay(Int)     // Fixed delay in milliseconds
    ExponentialBackoff(Int, Int)  // Base delay and max delay
    LinearBackoff(Int)  // Linear increase in delay
    NoRetry
  }
  
  type ErrorInfo = {
    error_type: ErrorType,
    message: String,
    stack_trace: String,
    timestamp: Int,
    retry_count: Int
  }
  
  type RetryConfig = {
    policy: RetryPolicy,
    max_attempts: Int,
    max_total_time: Int  // Maximum total time in milliseconds
  }
  
  type CircuitBreakerState = {
    is_open: Bool,
    failure_count: Int,
    failure_threshold: Int,
    timeout: Int,
    last_failure_time: Int,
    recovery_timeout: Int
  }
  
  type ResilienceConfig = {
    retry_config: RetryConfig,
    circuit_breaker: CircuitBreakerState,
    timeout_ms: Int,
    fallback_enabled: Bool
  }
  
  type FunctionExecution = {
    function_name: String,
    invocation_id: String,
    start_time: Int,
    end_time: Int,
    status: String,  // "success", "failed", "retried", "fallback"
    errors: Array[ErrorInfo],
    retry_attempts: Int,
    circuit_breaker_triggered: Bool,
    fallback_used: Bool
  }
  
  // Calculate retry delay
  let calculate_retry_delay = fn(policy: RetryPolicy, attempt: Int) {
    match policy {
      RetryPolicy::FixedDelay(delay) => delay
      RetryPolicy::ExponentialBackoff(base, max) => {
        let delay = base * (2 ^ (attempt - 1))
        if delay > max { max } else { delay }
      }
      RetryPolicy::LinearBackoff(base) => base * attempt
      RetryPolicy::NoRetry => 0
    }
  }
  
  // Check if circuit breaker should be triggered
  let check_circuit_breaker = fn(
    state: CircuitBreakerState,
    current_time: Int,
    has_failed: Bool
  ) {
    if has_failed {
      let new_failure_count = state.failure_count + 1
      let should_open = new_failure_count >= state.failure_threshold
      
      {
        is_open: should_open,
        failure_count: new_failure_count,
        failure_threshold: state.failure_threshold,
        timeout: state.timeout,
        last_failure_time: if should_open { current_time } else { state.last_failure_time },
        recovery_timeout: state.recovery_timeout
      }
    } else if state.is_open {
      // Check if recovery timeout has passed
      let time_since_failure = current_time - state.last_failure_time
      
      if time_since_failure >= state.recovery_timeout {
        // Reset circuit breaker
        {
          is_open: false,
          failure_count: 0,
          failure_threshold: state.failure_threshold,
          timeout: state.timeout,
          last_failure_time: 0,
          recovery_timeout: state.recovery_timeout
        }
      } else {
        // Keep circuit breaker open
        state
      }
    } else {
      // Reset failure count on success
      {
        is_open: false,
        failure_count: 0,
        failure_threshold: state.failure_threshold,
        timeout: state.timeout,
        last_failure_time: state.last_failure_time,
        recovery_timeout: state.recovery_timeout
      }
    }
  }
  
  // Simulate function execution with error handling
  let simulate_execution = fn(
    function_name: String,
    resilience_config: ResilienceConfig,
    should_fail: Bool,
    error_type: ErrorType,
    current_time: Int
  ) {
    let mut execution = {
      function_name,
      invocation_id: "inv-" + current_time.to_string(),
      start_time: current_time,
      end_time: current_time + 100,  // Assume 100ms execution time
      status: "success",
      errors: [],
      retry_attempts: 0,
      circuit_breaker_triggered: false,
      fallback_used: false
    }
    
    // Check circuit breaker
    let circuit_breaker_state = resilience_config.circuit_breaker
    if circuit_breaker_state.is_open {
      execution.status = "failed"
      execution.circuit_breaker_triggered = true
      execution.fallback_used = resilience_config.fallback_enabled
      
      if resilience_config.fallback_enabled {
        execution.status = "fallback"
      }
      
      return execution
    }
    
    // Simulate execution with retries
    let mut attempt = 1
    let mut current_attempt_time = current_time
    let mut should_retry = should_fail
    
    while should_retry && attempt <= resilience_config.retry_config.max_attempts {
      let error = {
        error_type,
        message: "Simulated error of type: " + match error_type {
          ErrorType::Timeout => "Timeout"
          ErrorType::MemoryLimit => "MemoryLimit"
          ErrorType::UnhandledException => "UnhandledException"
          ErrorType::Throttling => "Throttling"
          ErrorType::DependencyFailure => "DependencyFailure"
          ErrorType::ConfigurationError => "ConfigurationError"
        },
        stack_trace: "at Function.execute (index.js:42:15)",
        timestamp: current_attempt_time,
        retry_count: attempt - 1
      }
      
      execution.errors = execution.errors.push(error)
      
      if attempt < resilience_config.retry_config.max_attempts {
        let retry_delay = calculate_retry_delay(resilience_config.retry_config.policy, attempt)
        current_attempt_time = current_attempt_time + retry_delay
        attempt = attempt + 1
        execution.retry_attempts = execution.retry_attempts + 1
        
        // Simulate if retry should fail (simplified)
        should_retry = should_fail && attempt < 3  // Assume success on 3rd attempt
      } else {
        should_retry = false
        execution.status = "failed"
      }
    }
    
    if not(should_fail) || (should_fail && execution.retry_attempts > 0 && not(should_retry)) {
      execution.status = "success"
      execution.end_time = current_attempt_time + 100
    } else if execution.status == "failed" && resilience_config.fallback_enabled {
      execution.status = "fallback"
      execution.fallback_used = true
      execution.end_time = current_attempt_time + 50  // Fallback is faster
    }
    
    execution
  }
  
  // Test retry delay calculations
  let fixed_delay = RetryPolicy::FixedDelay(1000)
  let exponential_backoff = RetryPolicy::ExponentialBackoff(100, 5000)
  let linear_backoff = RetryPolicy::LinearBackoff(500)
  
  assert_eq(calculate_retry_delay(fixed_delay, 1), 1000)
  assert_eq(calculate_retry_delay(fixed_delay, 5), 1000)
  
  assert_eq(calculate_retry_delay(exponential_backoff, 1), 100)
  assert_eq(calculate_retry_delay(exponential_backoff, 2), 200)
  assert_eq(calculate_retry_delay(exponential_backoff, 3), 400)
  assert_eq(calculate_retry_delay(exponential_backoff, 4), 800)
  assert_eq(calculate_retry_delay(exponential_backoff, 5), 1600)
  assert_eq(calculate_retry_delay(exponential_backoff, 6), 3200)
  assert_eq(calculate_retry_delay(exponential_backoff, 7), 5000)  // Capped at max
  
  assert_eq(calculate_retry_delay(linear_backoff, 1), 500)
  assert_eq(calculate_retry_delay(linear_backoff, 2), 1000)
  assert_eq(calculate_retry_delay(linear_backoff, 3), 1500)
  
  // Test circuit breaker
  let initial_circuit_state = {
    is_open: false,
    failure_count: 0,
    failure_threshold: 5,
    timeout: 30000,
    last_failure_time: 0,
    recovery_timeout: 60000
  }
  
  let circuit_after_failure1 = check_circuit_breaker(initial_circuit_state, 1000, true)
  assert_false(circuit_after_failure1.is_open)
  assert_eq(circuit_after_failure1.failure_count, 1)
  
  let circuit_after_failure5 = {
    is_open: false,
    failure_count: 4,
    failure_threshold: 5,
    timeout: 30000,
    last_failure_time: 1000,
    recovery_timeout: 60000
  }
  
  let circuit_after_failure6 = check_circuit_breaker(circuit_after_failure5, 2000, true)
  assert_true(circuit_after_failure6.is_open)
  assert_eq(circuit_after_failure6.failure_count, 5)
  assert_eq(circuit_after_failure6.last_failure_time, 2000)
  
  let circuit_after_recovery_time = check_circuit_breaker(circuit_after_failure6, 70000, false)  // After recovery timeout
  assert_false(circuit_after_recovery_time.is_open)
  assert_eq(circuit_after_recovery_time.failure_count, 0)
  
  // Test resilience simulation
  let retry_config = {
    policy: RetryPolicy::ExponentialBackoff(100, 5000),
    max_attempts: 3,
    max_total_time: 10000
  }
  
  let circuit_breaker_config = {
    is_open: false,
    failure_count: 0,
    failure_threshold: 5,
    timeout: 30000,
    last_failure_time: 0,
    recovery_timeout: 60000
  }
  
  let resilience_config = {
    retry_config,
    circuit_breaker: circuit_breaker_config,
    timeout_ms: 5000,
    fallback_enabled: true
  }
  
  // Test successful execution
  let success_execution = simulate_execution(
    "process-telemetry",
    resilience_config,
    false,
    ErrorType::Timeout,
    1000
  )
  
  assert_eq(success_execution.status, "success")
  assert_eq(success_execution.errors.length(), 0)
  assert_eq(success_execution.retry_attempts, 0)
  assert_false(success_execution.circuit_breaker_triggered)
  assert_false(success_execution.fallback_used)
  
  // Test failed execution with retries and fallback
  let failed_execution = simulate_execution(
    "process-telemetry",
    resilience_config,
    true,
    ErrorType::DependencyFailure,
    2000
  )
  
  assert_eq(failed_execution.status, "success")  // Succeeds after retries
  assert_eq(failed_execution.errors.length(), 2)  // 2 failures before success
  assert_eq(failed_execution.retry_attempts, 2)
  assert_false(failed_execution.circuit_breaker_triggered)
  assert_false(failed_execution.fallback_used)
  
  // Test execution with circuit breaker open
  let open_circuit_breaker = {
    is_open: true,
    failure_count: 5,
    failure_threshold: 5,
    timeout: 30000,
    last_failure_time: 1000,
    recovery_timeout: 60000
  }
  
  let resilience_with_open_circuit = {
    retry_config,
    circuit_breaker: open_circuit_breaker,
    timeout_ms: 5000,
    fallback_enabled: true
  }
  
  let circuit_execution = simulate_execution(
    "process-telemetry",
    resilience_with_open_circuit,
    false,
    ErrorType::Timeout,
    3000
  )
  
  assert_eq(circuit_execution.status, "fallback")
  assert_eq(circuit_execution.errors.length(), 0)
  assert_eq(circuit_execution.retry_attempts, 0)
  assert_true(circuit_execution.circuit_breaker_triggered)
  assert_true(circuit_execution.fallback_used)
}

// Test 5: Serverless Function Cost Optimization
test "serverless function cost optimization strategies" {
  // Define cost optimization types
  enum PricingModel {
    PayPerExecution
    ProvisionedConcurrency
    Dedicated
  }
  
  type CostMetrics = {
    function_name: String,
    pricing_model: PricingModel,
    invocation_count: Int,
    avg_execution_time_ms: Int,
    memory_mb: Int,
    provisioned_concurrency: Int,
    cost_per_million_executions: Float,
    cost_per_gb_second: Float,
    provisioned_concurrency_cost: Float,
    total_cost: Float
  }
  
  type OptimizationStrategy = {
    name: String,
    description: String,
    potential_savings: Float,  // percentage
    implementation_effort: String,  // "low", "medium", "high"
    trade_offs: Array[String]
  }
  
  type CostOptimization = {
    function_name: String,
    current_metrics: CostMetrics,
    recommended_strategies: Array[OptimizationStrategy],
    optimized_metrics: Option[CostMetrics>
  }
  
  // Calculate cost for pay-per-execution model
  let calculate_pay_per_execution_cost = fn(
    invocation_count: Int,
    avg_execution_time_ms: Int,
    memory_mb: Int,
    cost_per_million_executions: Float,
    cost_per_gb_second: Float
  ) {
    // Cost = (invocations / 1,000,000) * cost_per_million_executions + 
    //        (invocations * avg_execution_time_ms * memory_mb / 1,000,000,000) * cost_per_gb_second
    
    let invocation_cost = Int::to_float(invocation_count) / 1000000.0 * cost_per_million_executions
    let compute_gb_seconds = Int::to_float(invocation_count * avg_execution_time_ms * memory_mb) / 1000000000.0
    let compute_cost = compute_gb_seconds * cost_per_gb_second
    
    invocation_cost + compute_cost
  }
  
  // Calculate cost for provisioned concurrency model
  let calculate_provisioned_cost = fn(
    invocation_count: Int,
    avg_execution_time_ms: Int,
    memory_mb: Int,
    provisioned_concurrency: Int,
    cost_per_million_executions: Float,
    cost_per_gb_second: Float,
    provisioned_concurrency_cost: Float
  ) {
    let pay_per_cost = calculate_pay_per_execution_cost(
      invocation_count,
      avg_execution_time_ms,
      memory_mb,
      cost_per_million_executions,
      cost_per_gb_second
    )
    
    // Add provisioned concurrency cost (per instance per hour)
    let provisioned_cost_per_ms = provisioned_concurrency_cost / (1000 * 60 * 60)
    let monthly_provisioned_cost = Int::to_float(provisioned_concurrency) * provisioned_cost_per_ms * (30 * 24 * 60 * 60 * 1000)
    
    pay_per_cost + monthly_provisioned_cost
  }
  
  // Generate optimization strategies
  let generate_optimization_strategies = fn(metrics: CostMetrics) {
    let mut strategies = []
    
    // Memory optimization
    if metrics.memory_mb > 512 {
      strategies = strategies.push({
        name: "Memory Optimization",
        description: "Reduce memory allocation to 512MB or less",
        potential_savings: 20.0,
        implementation_effort: "medium",
        trade_offs: ["May increase execution time", "Requires testing"]
      })
    }
    
    // Execution time optimization
    if metrics.avg_execution_time_ms > 1000 {
      strategies = strategies.push({
        name: "Execution Time Optimization",
        description: "Optimize code to reduce execution time",
        potential_savings: 30.0,
        implementation_effort: "high",
        trade_offs: ["Requires code refactoring", "May increase complexity"]
      })
    }
    
    // Provisioned concurrency for high-traffic functions
    if metrics.invocation_count > 1000000 {
      strategies = strategies.push({
        name: "Provisioned Concurrency",
        description: "Enable provisioned concurrency to reduce cold starts",
        potential_savings: 15.0,
        implementation_effort: "low",
        trade_offs: ["Fixed cost regardless of usage", "Best for predictable workloads"]
      })
    }
    
    // Right-sizing for low-traffic functions
    if metrics.invocation_count < 10000 {
      strategies = strategies.push({
        name: "Memory Right-sizing",
        description: "Reduce memory allocation to minimum required",
        potential_savings: 25.0,
        implementation_effort: "low",
        trade_offs: ["May increase execution time", "Requires monitoring"]
      })
    }
    
    strategies
  }
  
  // Apply optimization strategy
  let apply_optimization = fn(
    metrics: CostMetrics,
    strategy: OptimizationStrategy
  ) {
    match strategy.name {
      "Memory Optimization" => {
        let optimized_memory = if metrics.memory_mb > 512 { 512 } else { metrics.memory_mb }
        let new_cost = calculate_pay_per_execution_cost(
          metrics.invocation_count,
          metrics.avg_execution_time_ms,
          optimized_memory,
          metrics.cost_per_million_executions,
          metrics.cost_per_gb_second
        )
        
        {
          function_name: metrics.function_name,
          pricing_model: metrics.pricing_model,
          invocation_count: metrics.invocation_count,
          avg_execution_time_ms: metrics.avg_execution_time_ms,
          memory_mb: optimized_memory,
          provisioned_concurrency: metrics.provisioned_concurrency,
          cost_per_million_executions: metrics.cost_per_million_executions,
          cost_per_gb_second: metrics.cost_per_gb_second,
          provisioned_concurrency_cost: metrics.provisioned_concurrency_cost,
          total_cost: new_cost
        }
      }
      "Execution Time Optimization" => {
        let optimized_time = if metrics.avg_execution_time_ms > 1000 {
          metrics.avg_execution_time_ms / 2
        } else {
          metrics.avg_execution_time_ms
        }
        
        let new_cost = calculate_pay_per_execution_cost(
          metrics.invocation_count,
          optimized_time,
          metrics.memory_mb,
          metrics.cost_per_million_executions,
          metrics.cost_per_gb_second
        )
        
        {
          function_name: metrics.function_name,
          pricing_model: metrics.pricing_model,
          invocation_count: metrics.invocation_count,
          avg_execution_time_ms: optimized_time,
          memory_mb: metrics.memory_mb,
          provisioned_concurrency: metrics.provisioned_concurrency,
          cost_per_million_executions: metrics.cost_per_million_executions,
          cost_per_gb_second: metrics.cost_per_gb_second,
          provisioned_concurrency_cost: metrics.provisioned_concurrency_cost,
          total_cost: new_cost
        }
      }
      _ => metrics  // Simplified for other strategies
    }
  }
  
  // Test cost calculations
  let pay_per_cost = calculate_pay_per_execution_cost(
    2000000,      // 2 million invocations
    500,          // 500ms average execution time
    1024,         // 1GB memory
    0.20,         // $0.20 per million invocations
    0.0000166667  // $0.0000166667 per GB-second
  )
  
  // Expected: (2,000,000 / 1,000,000) * $0.20 + (2,000,000 * 500 * 1024 / 1,000,000,000) * $0.0000166667
  // = 2 * $0.20 + 1,024,000,000,000 / 1,000,000,000 * $0.0000166667
  // = $0.40 + 1024 * $0.0000166667
  // = $0.40 + $0.0170667 ≈ $0.4170667
  
  assert_true(pay_per_cost > 0.40 && pay_per_cost < 0.42)
  
  let provisioned_cost = calculate_provisioned_cost(
    2000000,      // 2 million invocations
    500,          // 500ms average execution time
    1024,         // 1GB memory
    5,            // 5 provisioned instances
    0.20,         // $0.20 per million invocations
    0.0000166667, // $0.0000166667 per GB-second
    0.0000305     // $0.0000305 per GB-second for provisioned concurrency
  )
  
  assert_true(provisioned_cost > pay_per_cost)  // Provisioned should be more expensive
  
  // Test optimization strategies
  let high_memory_metrics = {
    function_name: "process-telemetry",
    pricing_model: PricingModel::PayPerExecution,
    invocation_count: 500000,
    avg_execution_time_ms: 800,
    memory_mb: 1024,
    provisioned_concurrency: 0,
    cost_per_million_executions: 0.20,
    cost_per_gb_second: 0.0000166667,
    provisioned_concurrency_cost: 0.0,
    total_cost: calculate_pay_per_execution_cost(
      500000,
      800,
      1024,
      0.20,
      0.0000166667
    )
  }
  
  let strategies = generate_optimization_strategies(high_memory_metrics)
  assert_eq(strategies.length(), 2)  // Memory optimization and execution time optimization
  
  let memory_strategy = strategies[0]
  assert_eq(memory_strategy.name, "Memory Optimization")
  assert_eq(memory_strategy.potential_savings, 20.0)
  assert_eq(memory_strategy.implementation_effort, "medium")
  
  // Test applying optimization
  let optimized_metrics = apply_optimization(high_memory_metrics, memory_strategy)
  assert_eq(optimized_metrics.memory_mb, 512)  // Reduced from 1024
  assert_true(optimized_metrics.total_cost < high_memory_metrics.total_cost)
  
  // Test cost optimization
  let cost_optimization = {
    function_name: "process-telemetry",
    current_metrics: high_memory_metrics,
    recommended_strategies: strategies,
    optimized_metrics: Some(optimized_metrics)
  }
  
  assert_eq(cost_optimization.function_name, "process-telemetry")
  assert_eq(cost_optimization.recommended_strategies.length(), 2)
  match cost_optimization.optimized_metrics {
    Some(metrics) => {
      assert_eq(metrics.memory_mb, 512)
      assert_true(metrics.total_cost < high_memory_metrics.total_cost)
    }
    None => assert_true(false)
  }
}