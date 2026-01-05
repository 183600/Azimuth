// Azimuth Edge Computing and IoT Integration Tests
// This file contains test cases for edge computing and IoT functionality in the telemetry system

test "edge device registration and discovery" {
  // Test edge device registration
  let device_info = EdgeDevice::new(
    "edge-device-001",
    "temperature-sensor",
    "v1.0.0",
    ["temperature", "humidity"]
  )
  
  // Test device properties
  assert_eq(EdgeDevice::id(device_info), "edge-device-001")
  assert_eq(EdgeDevice::type(device_info), "temperature-sensor")
  assert_eq(EdgeDevice::version(device_info), "v1.0.0")
  assert_eq(EdgeDevice::capabilities(device_info).length(), 2)
  
  // Test device registry
  let registry = EdgeRegistry::new()
  EdgeRegistry::register(registry, device_info)
  
  // Test device discovery
  let discovered = EdgeRegistry::discover_by_type(registry, "temperature-sensor")
  assert_eq(discovered.length(), 1)
  assert_eq(EdgeDevice::id(discovered[0]), "edge-device-001")
}

test "iot sensor data collection" {
  // Test sensor data creation
  let sensor_data = SensorData::new(
    "sensor-001",
    "temperature",
    23.5,
    "celsius",
    1609459200L  // Unix timestamp
  )
  
  // Test data properties
  assert_eq(SensorData::sensor_id(sensor_data), "sensor-001")
  assert_eq(SensorData::metric_type(sensor_data), "temperature")
  assert_eq(SensorData::value(sensor_data), 23.5)
  assert_eq(SensorData::unit(sensor_data), "celsius")
  assert_eq(SensorData::timestamp(sensor_data), 1609459200L)
  
  // Test data validation
  assert_true(SensorData::is_valid(sensor_data))
  
  // Test invalid data (negative temperature in Celsius might be valid but extreme)
  let invalid_data = SensorData::new("sensor-001", "temperature", -274.0, "celsius", 1609459200L)
  assert_false(SensorData::is_valid(invalid_data))  // Below absolute zero
}

test "edge computing data processing pipeline" {
  // Test data processing pipeline
  let pipeline = EdgePipeline::new()
  
  // Add processing stages
  EdgePipeline::add_stage(pipeline, "validation")
  EdgePipeline::add_stage(pipeline, "normalization")
  EdgePipeline::add_stage(pipeline, "aggregation")
  EdgePipeline::add_stage(pipeline, "filtering")
  
  // Test pipeline stages
  assert_eq(EdgePipeline::num_stages(pipeline), 4)
  
  // Process sample data through pipeline
  let raw_data = [
    SensorData::new("sensor-001", "temperature", 23.5, "celsius", 1609459200L),
    SensorData::new("sensor-002", "humidity", 65.2, "percent", 1609459201L),
    SensorData::new("sensor-001", "temperature", 24.1, "celsius", 1609459202L)
  ]
  
  let processed_data = EdgePipeline::process(pipeline, raw_data)
  
  // Verify processing results
  assert_true(processed_data.length() <= raw_data.length())  // Some data might be filtered out
  
  // Check that processed data maintains chronological order
  for i in 1..processed_data.length() {
    assert_true(
      SensorData::timestamp(processed_data[i-1]) <= SensorData::timestamp(processed_data[i])
    )
  }
}

test "edge device resource management" {
  // Test resource constraints
  let resources = EdgeResources::new(
    cpu_cores = 4,
    memory_mb = 2048,
    storage_gb = 32,
    network_bandwidth_kbps = 1000
  )
  
  // Test resource properties
  assert_eq(EdgeResources::cpu_cores(resources), 4)
  assert_eq(EdgeResources::memory_mb(resources), 2048)
  assert_eq(EdgeResources::storage_gb(resources), 32)
  assert_eq(EdgeResources::network_bandwidth_kbps(resources), 1000)
  
  // Test resource allocation
  let task_resources = EdgeResources::allocate(
    resources,
    cpu_cores = 2,
    memory_mb = 1024,
    storage_gb = 8,
    network_bandwidth_kbps = 500
  )
  
  assert_eq(EdgeResources::cpu_cores(task_resources), 2)
  assert_eq(EdgeResources::memory_mb(task_resources), 1024)
  
  // Test resource availability check
  assert_true(EdgeResources::can_allocate(
    resources,
    cpu_cores = 1,
    memory_mb = 512,
    storage_gb = 4,
    network_bandwidth_kbps = 200
  ))
  
  assert_false(EdgeResources::can_allocate(
    resources,
    cpu_cores = 8,  // More than available
    memory_mb = 1024,
    storage_gb = 4,
    network_bandwidth_kbps = 200
  ))
}

test "iot device communication protocols" {
  // Test MQTT protocol
  let mqtt_client = IoTProtocol::mqtt_client("edge-device-001", "broker.example.com", 1883)
  
  // Test client properties
  assert_eq(IoTProtocol::client_id(mqtt_client), "edge-device-001")
  assert_eq(IoTProtocol::broker_host(mqtt_client), "broker.example.com")
  assert_eq(IoTProtocol::broker_port(mqtt_client), 1883)
  
  // Test message publishing
  let message = IoTMessage::new(
    topic = "sensors/temperature",
    payload = "{\"value\": 23.5, \"unit\": \"celsius\"}",
    qos = 1,
    retain = false
  )
  
  let publish_result = IoTProtocol::publish(mqtt_client, message)
  assert_true(IoTProtocol::is_successful(publish_result))
  
  // Test message subscription
  let subscription = IoTProtocol::subscribe(mqtt_client, "sensors/+", 1)
  assert_true(IoTProtocol::is_successful(subscription))
  
  // Test CoAP protocol
  let coap_client = IoTProtocol::coap_client("edge-device-001", "coap.example.com", 5683)
  
  // Test CoAP GET request
  let get_request = CoAPRequest::new(GET, "/sensors/temperature")
  let get_response = IoTProtocol::send(coap_client, get_request)
  assert_eq(CoAPResponse::code(get_response), "2.05")  // Content
  
  // Test CoAP POST request
  let post_request = CoAPRequest::new(POST, "/sensors/data")
  let post_response = IoTProtocol::send(coap_client, post_request)
  assert_eq(CoAPResponse::code(post_response), "2.01")  // Created
}

test "edge analytics and local processing" {
  // Test local analytics engine
  let analytics = EdgeAnalytics::new()
  
  // Configure analytics rules
  EdgeAnalytics::add_rule(analytics, "temperature_threshold", [
    ("metric", "temperature"),
    ("operator", ">"),
    ("value", "30.0"),
    ("action", "alert")
  ])
  
  EdgeAnalytics::add_rule(analytics, "humidity_range", [
    ("metric", "humidity"),
    ("operator", "between"),
    ("min_value", "40.0"),
    ("max_value", "60.0"),
    ("action", "log")
  ])
  
  // Process sensor data
  let data_samples = [
    SensorData::new("sensor-001", "temperature", 32.5, "celsius", 1609459200L),
    SensorData::new("sensor-002", "humidity", 35.0, "percent", 1609459201L),
    SensorData::new("sensor-001", "temperature", 28.0, "celsius", 1609459202L),
    SensorData::new("sensor-002", "humidity", 55.0, "percent", 1609459203L)
  ]
  
  let analytics_results = EdgeAnalytics::process(analytics, data_samples)
  
  // Verify analytics results
  assert_eq(analytics_results.length(), 4)  // One result per data sample
  
  // Check for alerts
  let mut alert_count = 0
  let mut log_count = 0
  
  for result in analytics_results {
    match EdgeAnalytics::result_action(result) {
      "alert" => alert_count = alert_count + 1,
      "log" => log_count = log_count + 1,
      _ => {}
    }
  }
  
  assert_eq(alert_count, 1)  // Temperature exceeded 30.0 once
  assert_eq(log_count, 2)    // Humidity was in range twice
}

test "edge-cloud synchronization" {
  // Test edge-cloud sync manager
  let sync_manager = EdgeCloudSync::new(
    edge_id = "edge-device-001",
    cloud_endpoint = "https://cloud.example.com/api",
    sync_interval_seconds = 60
  )
  
  // Test sync configuration
  assert_eq(EdgeCloudSync::edge_id(sync_manager), "edge-device-001")
  assert_eq(EdgeCloudSync::cloud_endpoint(sync_manager), "https://cloud.example.com/api")
  assert_eq(EdgeCloudSync::sync_interval(sync_manager), 60)
  
  // Test data batching for sync
  let sensor_data = [
    SensorData::new("sensor-001", "temperature", 23.5, "celsius", 1609459200L),
    SensorData::new("sensor-002", "humidity", 65.2, "percent", 1609459201L),
    SensorData::new("sensor-001", "temperature", 24.1, "celsius", 1609459202L)
  ]
  
  let batch = EdgeCloudSync::create_batch(sync_manager, sensor_data)
  assert_eq(EdgeCloudSync::batch_size(batch), 3)
  
  // Test sync operation
  let sync_result = EdgeCloudSync::sync(sync_manager, batch)
  assert_true(EdgeCloudSync::is_successful(sync_result))
  
  // Test conflict resolution
  let local_data = SensorData::new("sensor-001", "temperature", 25.0, "celsius", 1609459203L)
  let cloud_data = SensorData::new("sensor-001", "temperature", 24.8, "celsius", 1609459203L)
  
  let resolved_data = EdgeCloudSync::resolve_conflict(sync_manager, local_data, cloud_data, "latest_wins")
  assert_eq(SensorData::value(resolved_data), 25.0)  // Local data wins
}

test "iot device security and authentication" {
  // Test device authentication
  let device_credentials = DeviceCredentials::new(
    device_id = "edge-device-001",
    access_key = "AKIAIOSFODNN7EXAMPLE",
    secret_key = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
  )
  
  // Test credential properties
  assert_eq(DeviceCredentials::device_id(device_credentials), "edge-device-001")
  assert_eq(DeviceCredentials::access_key(device_credentials), "AKIAIOSFODNN7EXAMPLE")
  assert_eq(DeviceCredentials::secret_key(device_credentials), "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY")
  
  // Test token generation
  let auth_token = DeviceAuth::generate_token(device_credentials, 3600)  // 1 hour expiry
  assert_true(DeviceAuth::is_valid(auth_token))
  
  // Test token validation
  let validation_result = DeviceAuth::validate_token(auth_token)
  assert_true(DeviceAuth::is_successful(validation_result))
  
  // Test expired token
  let expired_token = DeviceAuth::generate_token(device_credentials, -1)  // Already expired
  assert_false(DeviceAuth::is_valid(expired_token))
  
  // Test secure communication
  let secure_channel = SecureChannel::initiate(device_credentials)
  assert_true(SecureChannel::is_established(secure_channel))
  
  // Test encrypted data transmission
  let plaintext = "sensor data: temperature=23.5"
  let encrypted = SecureChannel::encrypt(secure_channel, plaintext)
  let decrypted = SecureChannel::decrypt(secure_channel, encrypted)
  
  assert_eq(decrypted, plaintext)
}

test "edge device monitoring and health checks" {
  // Test device health monitor
  let health_monitor = DeviceHealthMonitor::new("edge-device-001")
  
  // Configure health checks
  DeviceHealthMonitor::add_check(health_monitor, "cpu_usage", 80.0)  // Alert if > 80%
  DeviceHealthMonitor::add_check(health_monitor, "memory_usage", 90.0)  // Alert if > 90%
  DeviceHealthMonitor::add_check(health_monitor, "disk_usage", 85.0)  // Alert if > 85%
  DeviceHealthMonitor::add_check(health_monitor, "network_latency", 1000.0)  // Alert if > 1000ms
  
  // Simulate health metrics
  let health_metrics = [
    ("cpu_usage", 75.5),
    ("memory_usage", 82.3),
    ("disk_usage", 78.9),
    ("network_latency", 850.0)
  ]
  
  let health_status = DeviceHealthMonitor::check(health_monitor, health_metrics)
  
  // Verify health status
  assert_true(DeviceHealthMonitor::is_healthy(health_status))  // All metrics within thresholds
  
  // Simulate degraded health
  let degraded_metrics = [
    ("cpu_usage", 85.5),  // Above threshold
    ("memory_usage", 82.3),
    ("disk_usage", 78.9),
    ("network_latency", 850.0)
  ]
  
  let degraded_status = DeviceHealthMonitor::check(health_monitor, degraded_metrics)
  assert_false(DeviceHealthMonitor::is_healthy(degraded_status))
  
  // Test health alerts
  let alerts = DeviceHealthMonitor::get_alerts(degraded_status)
  assert_eq(alerts.length(), 1)  // One alert for high CPU usage
  assert_eq(DeviceHealthAlert::metric(alerts[0]), "cpu_usage")
  assert_eq(DeviceHealthAlert::value(alerts[0]), 85.5)
  assert_eq(DeviceHealthAlert::threshold(alerts[0]), 80.0)
}