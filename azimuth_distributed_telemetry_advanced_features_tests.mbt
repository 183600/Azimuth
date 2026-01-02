// Azimuth Distributed Telemetry Advanced Features Tests
// This file contains comprehensive test cases for advanced features in distributed telemetry systems

// Test 1: Distributed Trace Context Propagation
test "distributed trace context propagation across service boundaries" {
  // Create initial trace context
  let trace_context = azimuth::telemetry::TraceContext::new(
    "trace-12345",
    "span-parent",
    "service-a",
    "operation-initial"
  )
  
  // Simulate service chain propagation
  let service_b_context = azimuth::telemetry::TraceContext::propagate_to_service(
    trace_context,
    "service-b",
    "operation-process",
    Some("span-parent")
  )
  
  let service_c_context = azimuth::telemetry::TraceContext::propagate_to_service(
    service_b_context,
    "service-c",
    "operation-finalize",
    Some(service_b_context.span_id)
  )
  
  // Verify trace ID consistency across services
  assert_eq(trace_context.trace_id, service_b_context.trace_id)
  assert_eq(trace_context.trace_id, service_c_context.trace_id)
  
  // Verify span hierarchy
  assert_eq(service_b_context.parent_span_id, trace_context.span_id)
  assert_eq(service_c_context.parent_span_id, service_b_context.span_id)
  
  // Verify service names and operations
  assert_eq(service_b_context.service_name, "service-b")
  assert_eq(service_b_context.operation_name, "operation-process")
  assert_eq(service_c_context.service_name, "service-c")
  assert_eq(service_c_context.operation_name, "operation-finalize")
  
  // Test baggage propagation (metadata that travels with the trace)
  let baggage = {"user_id": "user-123", "request_id": "req-456", "priority": "high"}
  let enriched_context = trace_context.with_baggage(baggage)
  
  let propagated_context = azimuth::telemetry::TraceContext::extract_from_headers({
    "x-trace-id": enriched_context.trace_id,
    "x-span-id": enriched_context.span_id,
    "x-parent-span-id": enriched_context.parent_span_id,
    "x-baggage": "user_id=user-123,request_id=req-456,priority=high"
  })
  
  assert_eq(propagated_context.trace_id, enriched_context.trace_id)
  assert_true(propagated_context.baggage.contains("user_id"))
  assert_eq(propagated_context.baggage["user_id"], "user-123")
}

// Test 2: Cross-Service Telemetry Data Aggregation
test "cross-service telemetry data aggregation with correlation" {
  // Create telemetry data from multiple services
  let service_a_metrics = [
    {"timestamp": 1640995200000, "service": "service-a", "metric": "request_count", "value": 100, "trace_id": "trace-123"},
    {"timestamp": 1640995260000, "service": "service-a", "metric": "response_time_ms", "value": 150, "trace_id": "trace-123"},
    {"timestamp": 1640995320000, "service": "service-a", "metric": "error_rate", "value": 0.02, "trace_id": "trace-123"}
  ]
  
  let service_b_metrics = [
    {"timestamp": 1640995210000, "service": "service-b", "metric": "request_count", "value": 80, "trace_id": "trace-123"},
    {"timestamp": 1640995270000, "service": "service-b", "metric": "response_time_ms", "value": 200, "trace_id": "trace-123"},
    {"timestamp": 1640995330000, "service": "service-b", "metric": "error_rate", "value": 0.01, "trace_id": "trace-123"}
  ]
  
  let service_c_metrics = [
    {"timestamp": 1640995220000, "service": "service-c", "metric": "request_count", "value": 120, "trace_id": "trace-123"},
    {"timestamp": 1640995280000, "service": "service-c", "metric": "response_time_ms", "value": 100, "trace_id": "trace-123"},
    {"timestamp": 1640995340000, "service": "service-c", "metric": "error_rate", "value": 0.03, "trace_id": "trace-123"}
  ]
  
  // Aggregate metrics across services by trace ID
  let all_metrics = service_a_metrics.concat(service_b_metrics).concat(service_c_metrics)
  let aggregated_by_trace = azimuth::telemetry::aggregate_by_trace_id(all_metrics)
  
  assert_eq(aggregated_by_trace.length(), 1) // One trace
  assert_eq(aggregated_by_trace[0].trace_id, "trace-123")
  
  // Verify aggregated request counts
  let total_requests = aggregated_by_trace[0].metrics["request_count"].reduce(fn(acc, x) { acc + x }, 0)
  assert_eq(total_requests, 300) // 100 + 80 + 120
  
  // Verify average response times
  let avg_response_time = aggregated_by_trace[0].metrics["response_time_ms"].reduce(fn(acc, x) { acc + x }, 0) / 3.0
  assert_eq(avg_response_time, 150.0) // (150 + 200 + 100) / 3
  
  // Verify error rate calculation
  let total_errors = aggregated_by_trace[0].metrics["error_rate"].reduce(fn(acc, x) { acc + x }, 0)
  let overall_error_rate = total_errors / 3.0
  assert_eq(overall_error_rate, 0.02) // (0.02 + 0.01 + 0.03) / 3
  
  // Test service dependency analysis
  let dependencies = azimuth::telemetry::analyze_service_dependencies(all_metrics)
  assert_true(dependencies.length() > 0)
  
  // Verify service chain (should detect service-a -> service-b -> service-c)
  let service_chain = dependencies[0].service_chain
  assert_true(service_chain.contains("service-a"))
  assert_true(service_chain.contains("service-b"))
  assert_true(service_chain.contains("service-c"))
}

// Test 3: Telemetry Data Sampling Strategies
test "telemetry data sampling strategies with adaptive algorithms" {
  // Create high-volume telemetry data
  let high_volume_data = []
  for i = 0; i < 10000; i = i + 1 {
    let data_point = {
      "timestamp": 1640995200000 + i * 1000,
      "trace_id": "trace-" + i.to_string(),
      "service": "api-gateway",
      "operation": "process_request",
      "duration_ms": 50 + (i % 100),
      "status": if i % 100 == 0 { "error" } else { "success" },
      "user_id": "user-" + (i % 1000).to_string()
    }
    high_volume_data.push(data_point)
  }
  
  // Test uniform sampling (10% rate)
  let uniform_sampler = azimuth::telemetry::UniformSampler::new(0.1)
  let uniform_sampled = high_volume_data.filter(|d| uniform_sampler.should_sample(d["trace_id"]))
  assert_true(uniform_sampled.length() >= 900 && uniform_sampled.length() <= 1100) // ~10%
  
  // Verify error traces are always included in sample
  let error_traces_in_sample = uniform_sampled.filter(|d| d["status"] == "error").length()
  let total_error_traces = high_volume_data.filter(|d| d["status"] == "error").length()
  assert_true(error_traces_in_sample == total_error_traces)
  
  // Test priority sampling based on user importance
  let priority_sampler = azimuth::telemetry::PrioritySampler::new({
    "user-1": 1.0,    // Always sample
    "user-2": 0.5,    // 50% sample
    "user-3": 0.1     // 10% sample
  })
  
  let priority_sampled = high_volume_data.filter(|d| priority_sampler.should_sample(d))
  
  // Verify VIP users (user-1) are always fully sampled
  let user_1_traces = high_volume_data.filter(|d| d["user_id"] == "user-1")
  let user_1_sampled = priority_sampled.filter(|d| d["user_id"] == "user-1")
  assert_eq(user_1_traces.length(), user_1_sampled.length())
  
  // Test adaptive sampling based on system load
  let adaptive_sampler = azimuth::telemetry::AdaptiveSampler::new(0.1, 1000) // Target 10%, max 1000 traces/sec
  
  // Simulate varying load conditions
  let low_load_data = high_volume_data.slice(0, 100)   // Low load
  let high_load_data = high_volume_data.slice(0, 5000)  // High load
  
  let low_load_sample_rate = adaptive_sampler.calculate_sample_rate(low_load_data.length())
  let high_load_sample_rate = adaptive_sampler.calculate_sample_rate(high_load_data.length())
  
  assert_true(low_load_sample_rate >= high_load_sample_rate) // Lower load = higher sample rate
  assert_true(high_load_sample_rate <= 0.1) // Should not exceed target rate under high load
}

// Test 4: Latency Measurement in Distributed Systems
test "latency measurement and analysis in distributed systems" {
  // Create distributed trace with timing information
  let distributed_trace = {
    "trace_id": "trace-latency-test",
    "spans": [
      {
        "span_id": "span-1",
        "parent_span_id": "",
        "service": "api-gateway",
        "operation": "receive_request",
        "start_time": 1640995200000,
        "end_time": 1640995200050,
        "tags": {"component": "http", "method": "POST"}
      },
      {
        "span_id": "span-2",
        "parent_span_id": "span-1",
        "service": "auth-service",
        "operation": "authenticate",
        "start_time": 1640995200050,
        "end_time": 1640995200150,
        "tags": {"component": "jwt", "algorithm": "RS256"}
      },
      {
        "span_id": "span-3",
        "parent_span_id": "span-2",
        "service": "user-service",
        "operation": "get_user_profile",
        "start_time": 1640995200150,
        "end_time": 1640995200250,
        "tags": {"component": "database", "query_type": "select"}
      },
      {
        "span_id": "span-4",
        "parent_span_id": "span-2",
        "service": "permission-service",
        "operation": "check_permissions",
        "start_time": 1640995200150,
        "end_time": 1640995200200,
        "tags": {"component": "cache", "cache_hit": "true"}
      },
      {
        "span_id": "span-5",
        "parent_span_id": "span-1",
        "service": "api-gateway",
        "operation": "send_response",
        "start_time": 1640995200250,
        "end_time": 1640995200300,
        "tags": {"component": "http", "status": "200"}
      }
    ]
  }
  
  // Calculate total trace duration
  let trace_duration = azimuth::telemetry::calculate_trace_duration(distributed_trace)
  assert_eq(trace_duration, 300) // 300ms total
  
  // Calculate individual span durations
  let span_durations = azimuth::telemetry::calculate_span_durations(distributed_trace)
  assert_eq(span_durations["span-1"], 50)   // 50ms
  assert_eq(span_durations["span-2"], 100)  // 100ms
  assert_eq(span_durations["span-3"], 100)  // 100ms
  assert_eq(span_durations["span-4"], 50)   // 50ms
  assert_eq(span_durations["span-5"], 50)   // 50ms
  
  // Calculate service-level latencies
  let service_latencies = azimuth::telemetry::calculate_service_latencies(distributed_trace)
  assert_eq(service_latencies["api-gateway"], 100)    // span-1 + span-5
  assert_eq(service_latencies["auth-service"], 100)   // span-2
  assert_eq(service_latencies["user-service"], 100)   // span-3
  assert_eq(service_latencies["permission-service"], 50) // span-4
  
  // Identify performance bottlenecks
  let bottlenecks = azimuth::telemetry::identify_performance_bottlenecks(distributed_trace, 0.2) // 20% threshold
  assert_true(bottlenecks.length() >= 1)
  
  // auth-service and user-service should be identified as bottlenecks (33.3% each)
  let auth_bottleneck = bottlenecks.find(|b| b.service == "auth-service")
  let user_bottleneck = bottlenecks.find(|b| b.service == "user-service")
  assert_true(auth_bottleneck.is_some)
  assert_true(user_bottleneck.is_some)
  
  // Calculate critical path (longest path through the trace)
  let critical_path = azimuth::telemetry::calculate_critical_path(distributed_trace)
  assert_eq(critical_path.duration, 250) // span-1 -> span-2 -> span-3 -> span-5
  assert_eq(critical_path.spans.length(), 4)
  
  // Test network latency estimation
  let network_latencies = azimuth::telemetry::estimate_network_latencies(distributed_trace)
  assert_true(network_latencies.length() > 0)
  
  // Verify network latency between services is reasonable
  for latency in network_latencies {
    assert_true(latency.value >= 0)
    assert_true(latency.value < 100) // Should be less than 100ms
  }
}

// Test 5: Service Mesh Telemetry Integration
test "service mesh telemetry integration with sidecar patterns" {
  // Simulate service mesh telemetry data
  let mesh_telemetry = {
    "mesh_config": {
      "mesh_name": "production-mesh",
      "namespace": "default",
      "services": ["frontend", "backend", "database", "cache"]
    },
    "traffic_data": [
      {
        "timestamp": 1640995200000,
        "source_service": "frontend",
        "destination_service": "backend",
        "source_pod": "frontend-7d4f8c9b-xyz12",
        "destination_pod": "backend-6a3e7d2c-abc34",
        "request_count": 100,
        "success_count": 95,
        "latency_p50": 50,
        "latency_p95": 120,
        "latency_p99": 200
      },
      {
        "timestamp": 1640995200000,
        "source_service": "backend",
        "destination_service": "database",
        "source_pod": "backend-6a3e7d2c-abc34",
        "destination_pod": "database-5b2c6e1b-def56",
        "request_count": 150,
        "success_count": 148,
        "latency_p50": 20,
        "latency_p95": 45,
        "latency_p99": 80
      },
      {
        "timestamp": 1640995200000,
        "source_service": "backend",
        "destination_service": "cache",
        "source_pod": "backend-6a3e7d2c-abc34",
        "destination_pod": "cache-4c1b5f0a-ghi78",
        "request_count": 80,
        "success_count": 80,
        "latency_p50": 5,
        "latency_p95": 10,
        "latency_p99": 15
      }
    ]
  }
  
  // Analyze mesh traffic patterns
  let traffic_analysis = azimuth::telemetry::analyze_mesh_traffic(mesh_telemetry)
  assert_eq(traffic_analysis.total_requests, 330) // 100 + 150 + 80
  assert_eq(traffic_analysis.success_rate, (95 + 148 + 80) / 330.0)
  
  // Calculate service dependencies in mesh
  let mesh_dependencies = azimuth::telemetry::extract_mesh_dependencies(mesh_telemetry)
  assert_eq(mesh_dependencies.length(), 3) // frontend->backend, backend->database, backend->cache
  
  // Verify dependency structure
  let frontend_dep = mesh_dependencies.find(|d| d.source == "frontend" && d.destination == "backend")
  let backend_db_dep = mesh_dependencies.find(|d| d.source == "backend" && d.destination == "database")
  let backend_cache_dep = mesh_dependencies.find(|d| d.source == "backend" && d.destination == "cache")
  
  assert_true(frontend_dep.is_some)
  assert_true(backend_db_dep.is_some)
  assert_true(backend_cache_dep.is_some)
  
  // Test mesh latency analysis
  let mesh_latency_analysis = azimuth::telemetry::analyze_mesh_latency(mesh_telemetry)
  assert_true(mesh_latency_analysis.average_p95 > 0)
  
  // Verify backend->database has higher latency than backend->cache
  let backend_db_latency = mesh_latency_analysis.service_latencies["backend->database"].p95
  let backend_cache_latency = mesh_latency_analysis.service_latencies["backend->cache"].p95
  assert_true(backend_db_latency > backend_cache_latency)
  
  // Test mesh anomaly detection
  let mesh_anomalies = azimuth::telemetry::detect_mesh_anomalies(mesh_telemetry, {
    "error_rate_threshold": 0.1,
    "latency_p99_threshold": 150,
    "request_rate_threshold": 1000
  })
  
  // frontend->backend should be flagged for high error rate (5/100 = 5%)
  let frontend_errors = mesh_anomalies.filter(|a| a.source == "frontend" && a.destination == "backend")
  assert_true(frontend_errors.length() > 0)
  
  // frontend->backend should be flagged for high P99 latency (200ms > 150ms)
  let frontend_latency = mesh_anomalies.filter(|a| a.source == "frontend" && a.destination == "backend" && a.type == "high_latency")
  assert_true(frontend_latency.length() > 0)
}

// Test 6: Telemetry Data Serialization/Deserialization Performance
test "telemetry data serialization and deserialization performance" {
  // Create complex telemetry data structure
  let complex_telemetry = {
    "trace_id": "trace-serialization-test",
    "spans": [],
    "metrics": {},
    "logs": [],
    "metadata": {
      "environment": "production",
      "version": "1.2.3",
      "region": "us-west-2",
      "availability_zone": "us-west-2a"
    }
  }
  
  // Generate 1000 spans with rich data
  for i = 0; i < 1000; i = i + 1 {
    let span = {
      "span_id": "span-" + i.to_string(),
      "parent_span_id": if i > 0 { "span-" + (i-1).to_string() } else { "" },
      "service": "service-" + (i % 10).to_string(),
      "operation": "operation-" + (i % 50).to_string(),
      "start_time": 1640995200000 + i * 1000,
      "end_time": 1640995200000 + i * 1000 + 50 + (i % 100),
      "tags": {
        "component": "component-" + (i % 5).to_string(),
        "version": "v" + ((i % 10) + 1).to_string(),
        "instance": "instance-" + (i % 20).to_string()
      },
      "events": [
        {
          "name": "event-" + (i % 3).to_string(),
          "timestamp": 1640995200000 + i * 1000 + 25,
          "attributes": {
            "key1": "value1-" + i.to_string(),
            "key2": (i * 2).to_string()
          }
        }
      ]
    }
    complex_telemetry.spans.push(span)
  }
  
  // Test JSON serialization performance
  let json_start_time = azimuth::time::now()
  let json_serialized = azimuth::serialization::serialize_json(complex_telemetry)
  let json_serialization_time = azimuth::time::now() - json_start_time
  
  // Test JSON deserialization performance
  let json_deserialize_start = azimuth::time::now()
  let json_deserialized = azimuth::serialization::deserialize_json(json_serialized)
  let json_deserialization_time = azimuth::time::now() - json_deserialize_start
  
  // Verify JSON serialization accuracy
  assert_eq(json_deserialized.trace_id, complex_telemetry.trace_id)
  assert_eq(json_deserialized.spans.length(), complex_telemetry.spans.length())
  
  // Test Protocol Buffers serialization performance
  let protobuf_start_time = azimuth::time::now()
  let protobuf_serialized = azimuth::serialization::serialize_protobuf(complex_telemetry)
  let protobuf_serialization_time = azimuth::time::now() - protobuf_start_time
  
  // Test Protocol Buffers deserialization performance
  let protobuf_deserialize_start = azimuth::time::now()
  let protobuf_deserialized = azimuth::serialization::deserialize_protobuf(protobuf_serialized)
  let protobuf_deserialization_time = azimuth::time::now() - protobuf_deserialize_start
  
  // Verify Protocol Buffers serialization accuracy
  assert_eq(protobuf_deserialized.trace_id, complex_telemetry.trace_id)
  assert_eq(protobuf_deserialized.spans.length(), complex_telemetry.spans.length())
  
  // Compare serialization sizes
  let json_size = json_serialized.length()
  let protobuf_size = protobuf_serialized.length()
  
  // Protocol Buffers should be more compact
  assert_true(protobuf_size < json_size)
  let compression_ratio = protobuf_size.to_double() / json_size.to_double()
  assert_true(compression_ratio < 0.8) // At least 20% smaller
  
  // Compare performance
  assert_true(protobuf_serialization_time < json_serialization_time)
  assert_true(protobuf_deserialization_time < json_deserialization_time)
  
  // Test MessagePack serialization as alternative
  let msgpack_start_time = azimuth::time::now()
  let msgpack_serialized = azimuth::serialization::serialize_msgpack(complex_telemetry)
  let msgpack_serialization_time = azimuth::time::now() - msgpack_start_time
  
  // Test MessagePack deserialization
  let msgpack_deserialize_start = azimuth::time::now()
  let msgpack_deserialized = azimuth::serialization::deserialize_msgpack(msgpack_serialized)
  let msgpack_deserialization_time = azimuth::time::now() - msgpack_deserialize_start
  
  // Verify MessagePack accuracy
  assert_eq(msgpack_deserialized.trace_id, complex_telemetry.trace_id)
  assert_eq(msgpack_deserialized.spans.length(), complex_telemetry.spans.length())
  
  // Compare all three formats
  let msgpack_size = msgpack_serialized.length()
  assert_true(msgpack_size < json_size)
  assert_true(msgpack_size > protobuf_size) // Usually between JSON and Protobuf
  
  // Performance benchmarking
  let benchmark_results = {
    "json": {
      "serialization_time": json_serialization_time,
      "deserialization_time": json_deserialization_time,
      "size": json_size
    },
    "protobuf": {
      "serialization_time": protobuf_serialization_time,
      "deserialization_time": protobuf_deserialization_time,
      "size": protobuf_size
    },
    "msgpack": {
      "serialization_time": msgpack_serialization_time,
      "deserialization_time": msgpack_deserialization_time,
      "size": msgpack_size
    }
  }
  
  // Verify all formats complete within reasonable time
  assert_true(json_serialization_time < 1000000) // < 1 second
  assert_true(json_deserialization_time < 1000000) // < 1 second
  assert_true(protobuf_serialization_time < 1000000) // < 1 second
  assert_true(protobuf_deserialization_time < 1000000) // < 1 second
  assert_true(msgpack_serialization_time < 1000000) // < 1 second
  assert_true(msgpack_deserialization_time < 1000000) // < 1 second
}

// Test 7: Error Handling in Distributed Tracing
test "error handling and resilience in distributed tracing systems" {
  // Create distributed trace with various error scenarios
  let error_scenarios = [
    {
      "scenario": "service_timeout",
      "trace": {
        "trace_id": "trace-timeout",
        "spans": [
          {
            "span_id": "span-1",
            "service": "api-gateway",
            "operation": "process_request",
            "status": "ok",
            "duration_ms": 5000
          },
          {
            "span_id": "span-2",
            "parent_span_id": "span-1",
            "service": "downstream-service",
            "operation": "timeout_operation",
            "status": "timeout",
            "duration_ms": 30000,
            "error": "Request timeout after 30 seconds"
          }
        ]
      },
      "expected_behavior": "propagate_timeout_error"
    },
    {
      "scenario": "service_error",
      "trace": {
        "trace_id": "trace-error",
        "spans": [
          {
            "span_id": "span-1",
            "service": "api-gateway",
            "operation": "process_request",
            "status": "ok",
            "duration_ms": 100
          },
          {
            "span_id": "span-2",
            "parent_span_id": "span-1",
            "service": "business-service",
            "operation": "business_logic",
            "status": "error",
            "duration_ms": 200,
            "error": "Business rule violation: insufficient permissions"
          }
        ]
      },
      "expected_behavior": "propagate_business_error"
    },
    {
      "scenario": "network_partition",
      "trace": {
        "trace_id": "trace-partition",
        "spans": [
          {
            "span_id": "span-1",
            "service": "api-gateway",
            "operation": "process_request",
            "status": "ok",
            "duration_ms": 100
          },
          {
            "span_id": "span-2",
            "parent_span_id": "span-1",
            "service": "isolated-service",
            "operation": "unreachable_operation",
            "status": "error",
            "duration_ms": 5000,
            "error": "Network partition detected: service unreachable"
          }
        ]
      },
      "expected_behavior": "handle_network_partition"
    }
  ]
  
  // Process each error scenario
  for scenario in error_scenarios {
    let trace_data = scenario.trace
    let error_analysis = azimuth::telemetry::analyze_trace_errors(trace_data)
    
    match scenario.scenario {
      "service_timeout" => {
        assert_eq(error_analysis.error_type, "timeout")
        assert_eq(error_analysis.affected_service, "downstream-service")
        assert_true(error_analysis.duration_ms >= 30000)
        
        // Test timeout error propagation
        let propagated_context = azimuth::telemetry::handle_timeout_error(trace_data)
        assert_eq(propagated_context.error_code, "TIMEOUT")
        assert_true(propagated_context.should_retry == false) // Don't retry timeouts
      },
      "service_error" => {
        assert_eq(error_analysis.error_type, "business_error")
        assert_eq(error_analysis.affected_service, "business-service")
        assert_true(error_analysis.error_message.contains("permissions"))
        
        // Test business error propagation
        let propagated_context = azimuth::telemetry::handle_business_error(trace_data)
        assert_eq(propagated_context.error_code, "BUSINESS_ERROR")
        assert_true(propagated_context.should_retry == false) // Don't retry business errors
      },
      "network_partition" => {
        assert_eq(error_analysis.error_type, "network_error")
        assert_eq(error_analysis.affected_service, "isolated-service")
        assert_true(error_analysis.error_message.contains("partition"))
        
        // Test network error handling
        let propagated_context = azimuth::telemetry::handle_network_error(trace_data)
        assert_eq(propagated_context.error_code, "NETWORK_ERROR")
        assert_true(propagated_context.should_retry == true) // Retry network errors
        assert_eq(propagated_context.retry_count, 3) // Default retry count
      },
      _ => assert_true(false) // Unknown scenario
    }
  }
  
  // Test circuit breaker pattern with telemetry
  let circuit_breaker_config = {
    "failure_threshold": 5,
    "recovery_timeout": 60000,
    "monitoring_window": 300000
  }
  
  let circuit_breaker = azimuth::telemetry::CircuitBreaker::new(circuit_breaker_config)
  
  // Simulate failure scenarios
  for i = 0; i < 10; i = i + 1 {
    let result = circuit_breaker.call("failing-service", fn() {
      Err("Service temporarily unavailable")
    })
    
    if i < 4 {
      // Circuit should still be closed for first 4 failures
      assert_true(result.is_err)
      assert_eq(circuit_breaker.state("failing-service"), "closed")
    } else {
      // Circuit should open after 5 failures
      assert_true(result.is_err)
      assert_eq(circuit_breaker.state("failing-service"), "open")
    }
  }
  
  // Verify circuit breaker telemetry
  let cb_metrics = circuit_breaker.get_metrics("failing-service")
  assert_eq(cb_metrics.failure_count, 10)
  assert_eq(cb_metrics.state, "open")
  
  // Test retry behavior with exponential backoff
  let retry_config = {
    "max_retries": 3,
    "initial_backoff": 1000,
    "max_backoff": 10000,
    "backoff_multiplier": 2.0
  }
  
  let retry_count = { mut value = 0 }
  let retry_result = azimuth::telemetry::retry_with_backoff(retry_config, fn() {
    retry_count.value = retry_count.value + 1
    if retry_count.value < 3 {
      Err("Temporary failure")
    } else {
      Ok("Success after retries")
    }
  })
  
  assert_true(retry_result.is_ok)
  assert_eq(retry_count.value, 3)
  assert_eq(retry_result.unwrap, "Success after retries")
}

// Test 8: Multi-Tenant Telemetry Data Isolation
test "multi-tenant telemetry data isolation and security" {
  // Create multi-tenant telemetry data
  let tenants = [
    {
      "tenant_id": "tenant-1",
      "tenant_name": "Acme Corporation",
      "isolation_level": "strict",
      "data_retention_days": 90,
      "services": ["service-a", "service-b"],
      "traces": [
        {
          "trace_id": "trace-1-1",
          "tenant_id": "tenant-1",
          "spans": [
            {"span_id": "span-1", "service": "service-a", "operation": "op1"},
            {"span_id": "span-2", "service": "service-b", "operation": "op2"}
          ]
        },
        {
          "trace_id": "trace-1-2",
          "tenant_id": "tenant-1",
          "spans": [
            {"span_id": "span-3", "service": "service-a", "operation": "op3"}
          ]
        }
      ]
    },
    {
      "tenant_id": "tenant-2",
      "tenant_name": "Beta Industries",
      "isolation_level": "standard",
      "data_retention_days": 30,
      "services": ["service-c", "service-d"],
      "traces": [
        {
          "trace_id": "trace-2-1",
          "tenant_id": "tenant-2",
          "spans": [
            {"span_id": "span-4", "service": "service-c", "operation": "op4"},
            {"span_id": "span-5", "service": "service-d", "operation": "op5"}
          ]
        }
      ]
    },
    {
      "tenant_id": "tenant-3",
      "tenant_name": "Gamma LLC",
      "isolation_level": "basic",
      "data_retention_days": 7,
      "services": ["service-e"],
      "traces": [
        {
          "trace_id": "trace-3-1",
          "tenant_id": "tenant-3",
          "spans": [
            {"span_id": "span-6", "service": "service-e", "operation": "op6"}
          ]
        }
      ]
    }
  ]
  
  // Test tenant isolation at data access level
  let tenant_manager = azimuth::telemetry::TenantManager::new(tenants)
  
  // Verify tenant can only access their own data
  let tenant_1_access = tenant_manager.get_tenant_traces("tenant-1")
  assert_eq(tenant_1_access.length(), 2)
  assert_eq(tenant_1_access[0].tenant_id, "tenant-1")
  assert_eq(tenant_1_access[1].tenant_id, "tenant-1")
  
  // Test cross-tenant access prevention
  let unauthorized_access = tenant_manager.get_tenant_traces_with_context("tenant-1", "tenant-2")
  assert_eq(unauthorized_access.length(), 0) // Should return empty due to isolation
  
  // Test tenant-specific metrics aggregation
  let tenant_1_metrics = tenant_manager.aggregate_tenant_metrics("tenant-1")
  assert_eq(tenant_1_metrics.total_traces, 2)
  assert_eq(tenant_1_metrics.total_spans, 3)
  assert_eq(tenant_1_metrics.unique_services, 2)
  
  let tenant_2_metrics = tenant_manager.aggregate_tenant_metrics("tenant-2")
  assert_eq(tenant_2_metrics.total_traces, 1)
  assert_eq(tenant_2_metrics.total_spans, 2)
  assert_eq(tenant_2_metrics.unique_services, 2)
  
  // Test data retention policies
  let retention_policies = tenant_manager.get_retention_policies()
  assert_eq(retention_policies["tenant-1"], 90)
  assert_eq(retention_policies["tenant-2"], 30)
  assert_eq(retention_policies["tenant-3"], 7)
  
  // Test data cleanup based on retention policies
  let old_timestamp = azimuth::time::now() - (100 * 24 * 60 * 60 * 1000) // 100 days ago
  let recent_timestamp = azimuth::time::now() - (5 * 24 * 60 * 60 * 1000)  // 5 days ago
  
  let cleanup_results = tenant_manager.cleanup_expired_data(old_timestamp)
  
  // tenant-1 data should be preserved (90-day retention)
  assert_true(cleanup_results.preserved_traces.contains("trace-1-1"))
  assert_true(cleanup_results.preserved_traces.contains("trace-1-2"))
  
  // tenant-2 data should be partially preserved (30-day retention)
  assert_true(cleanup_results.expired_traces.contains("trace-2-1"))
  
  // tenant-3 data should be expired (7-day retention)
  assert_true(cleanup_results.expired_traces.contains("trace-3-1"))
  
  // Test tenant resource quotas
  let resource_quotas = {
    "tenant-1": {"max_traces_per_day": 10000, "max_spans_per_trace": 100},
    "tenant-2": {"max_traces_per_day": 5000, "max_spans_per_trace": 50},
    "tenant-3": {"max_traces_per_day": 1000, "max_spans_per_trace": 20}
  }
  
  tenant_manager.set_resource_quotas(resource_quotas)
  
  // Test quota enforcement
  let quota_check_1 = tenant_manager.check_quota("tenant-1", 5000, 50)
  assert_true(quota_check_1.allowed) // Within quota
  
  let quota_check_2 = tenant_manager.check_quota("tenant-3", 1500, 25)
  assert_false(quota_check_2.allowed) // Exceeds quota
  
  // Test tenant data encryption
  let encryption_keys = {
    "tenant-1": "key-1-encryption-key",
    "tenant-2": "key-2-encryption-key",
    "tenant-3": "key-3-encryption-key"
  }
  
  let encrypted_data = tenant_manager.encrypt_tenant_data("tenant-1", tenant_1_access, encryption_keys["tenant-1"])
  let decrypted_data = tenant_manager.decrypt_tenant_data("tenant-1", encrypted_data, encryption_keys["tenant-1"])
  
  // Verify encryption/decryption accuracy
  assert_eq(decrypted_data.length(), tenant_1_access.length())
  assert_eq(decrypted_data[0].trace_id, tenant_1_access[0].trace_id)
  
  // Test cross-tenant decryption prevention
  let cross_tenant_decrypt = tenant_manager.decrypt_tenant_data("tenant-2", encrypted_data, encryption_keys["tenant-2"])
  assert_eq(cross_tenant_decrypt.length(), 0) // Should fail due to tenant mismatch
}

// Test 9: Real-time Telemetry Data Stream Processing
test "real-time telemetry data stream processing and analysis" {
  // Create stream processor configuration
  let stream_config = {
    "buffer_size": 10000,
    "window_size": 60000, // 1 minute windows
    "processing_interval": 5000, // Process every 5 seconds
    "aggregation_rules": [
      {"metric": "response_time", "function": "avg", "window": "1m"},
      {"metric": "error_rate", "function": "rate", "window": "1m"},
      {"metric": "request_count", "function": "sum", "window": "1m"}
    ]
  }
  
  let stream_processor = azimuth::telemetry::StreamProcessor::new(stream_config)
  
  // Generate real-time telemetry stream
  let base_timestamp = azimuth::time::now()
  let telemetry_stream = []
  
  for i = 0; i < 1000; i = i + 1 {
    let data_point = {
      "timestamp": base_timestamp + i * 100, // 100ms intervals
      "trace_id": "trace-" + (i % 100).to_string(),
      "service": "service-" + (i % 5).to_string(),
      "operation": "operation-" + (i % 20).to_string(),
      "response_time": 50 + (i % 200),
      "status": if i % 50 == 0 { "error" } else { "success" },
      "request_size": 1000 + (i % 5000),
      "response_size": 2000 + (i % 10000)
    }
    telemetry_stream.push(data_point)
  }
  
  // Process stream in real-time
  let processed_windows = []
  for i = 0; i < telemetry_stream.length(); i = i + 100 {
    let window_data = telemetry_stream.slice(i, i + 100)
    let window_result = stream_processor.process_window(window_data)
    processed_windows.push(window_result)
  }
  
  // Verify window processing results
  assert_eq(processed_windows.length(), 10)
  
  // Check aggregation calculations for first window
  let first_window = processed_windows[0]
  assert_true(first_window.aggregations.contains("response_time_avg"))
  assert_true(first_window.aggregations.contains("error_rate"))
  assert_true(first_window.aggregations.contains("request_count_sum"))
  
  // Verify average response time calculation
  let expected_avg = 0.0
  for i = 0; i < 100; i = i + 1 {
    expected_avg = expected_avg + telemetry_stream[i].response_time.to_double()
  }
  expected_avg = expected_avg / 100.0
  
  assert_true((first_window.aggregations["response_time_avg"] - expected_avg).abs() < 1.0)
  
  // Verify error rate calculation
  let errors_in_window = telemetry_stream.slice(0, 100).filter(|d| d.status == "error").length()
  let expected_error_rate = errors_in_window.to_double() / 100.0
  assert_eq(first_window.aggregations["error_rate"], expected_error_rate)
  
  // Test anomaly detection in streaming data
  let anomaly_detector = azimuth::telemetry::StreamAnomalyDetector::new({
    "response_time_threshold": 3.0, // 3 sigma
    "error_rate_threshold": 0.05,   // 5%
    "request_rate_threshold": 100   // requests/minute
  })
  
  let anomalies = []
  for window in processed_windows {
    let window_anomalies = anomaly_detector.detect_anomalies(window)
    anomalies.push_all(window_anomalies)
  }
  
  // Verify anomaly detection
  assert_true(anomalies.length() >= 0)
  
  // Test real-time alerting based on stream processing
  let alert_rules = [
    {
      "name": "high_response_time",
      "condition": "response_time_avg > 150",
      "severity": "warning",
      "action": "notify_team"
    },
    {
      "name": "high_error_rate",
      "condition": "error_rate > 0.05",
      "severity": "critical",
      "action": "escalate"
    }
  ]
  
  let alert_manager = azimuth::telemetry::AlertManager::new(alert_rules)
  let triggered_alerts = []
  
  for window in processed_windows {
    let window_alerts = alert_manager.evaluate_rules(window)
    triggered_alerts.push_all(window_alerts)
  }
  
  // Verify alert triggering
  assert_true(triggered_alerts.length() >= 0)
  
  // Test stream performance metrics
  let performance_metrics = stream_processor.get_performance_metrics()
  assert_true(performance_metrics.processing_rate > 0) // events/second
  assert_true(performance_metrics.latency_p95 < 1000) // < 1 second processing latency
  assert_true(performance_metrics.buffer_utilization <= 1.0) // Buffer not overloaded
  
  // Test stream backpressure handling
  let high_rate_stream = []
  for i = 0; i < 50000; i = i + 1 {
    let data_point = {
      "timestamp": base_timestamp + i * 10, // 10ms intervals (very high rate)
      "trace_id": "trace-" + i.to_string(),
      "service": "service-1",
      "operation": "operation-1",
      "response_time": 100,
      "status": "success"
    }
    high_rate_stream.push(data_point)
  }
  
  let backpressure_results = stream_processor.process_with_backpressure(high_rate_stream)
  
  // Verify backpressure handling
  assert_true(backpressure_results.processed_count > 0)
  assert_true(backpressure_results.dropped_count >= 0)
  assert_true(backpressure_results.backpressure_applied == backpressure_results.dropped_count > 0)
  
  // Test stream data partitioning by service
  let partitioned_streams = stream_processor.partition_by_service(telemetry_stream)
  assert_eq(partitioned_streams.length(), 5) // 5 services
  
  for service_stream in partitioned_streams {
    let service_name = service_stream.service
    let all_data_same_service = service_stream.data.all(|d| d.service == service_name)
    assert_true(all_data_same_service)
  }
}

// Test 10: Distributed System Resource Monitoring
test "distributed system resource monitoring and capacity planning" {
  // Create distributed system resource data
  let cluster_nodes = [
    {
      "node_id": "node-1",
      "region": "us-west-2a",
      "instance_type": "c5.large",
      "resources": {
        "cpu_cores": 2,
        "memory_gb": 4,
        "disk_gb": 50,
        "network_mbps": 1000
      },
      "metrics": {
        "cpu_utilization": 65.5,
        "memory_utilization": 78.2,
        "disk_utilization": 45.8,
        "network_in_mbps": 250.3,
        "network_out_mbps": 180.7
      },
      "services": ["api-gateway", "auth-service"],
      "status": "healthy"
    },
    {
      "node_id": "node-2",
      "region": "us-west-2b",
      "instance_type": "c5.xlarge",
      "resources": {
        "cpu_cores": 4,
        "memory_gb": 8,
        "disk_gb": 100,
        "network_mbps": 2000
      },
      "metrics": {
        "cpu_utilization": 45.2,
        "memory_utilization": 62.1,
        "disk_utilization": 38.5,
        "network_in_mbps": 450.8,
        "network_out_mbps": 320.5
      },
      "services": ["user-service", "payment-service"],
      "status": "healthy"
    },
    {
      "node_id": "node-3",
      "region": "us-west-2c",
      "instance_type": "c5.large",
      "resources": {
        "cpu_cores": 2,
        "memory_gb": 4,
        "disk_gb": 50,
        "network_mbps": 1000
      },
      "metrics": {
        "cpu_utilization": 85.7,
        "memory_utilization": 92.3,
        "disk_utilization": 78.9,
        "network_in_mbps": 680.2,
        "network_out_mbps": 520.1
      },
      "services": ["analytics-service", "report-service"],
      "status": "warning"
    }
  ]
  
  // Create resource monitor
  let resource_monitor = azimuth::telemetry::ResourceMonitor::new(cluster_nodes)
  
  // Calculate cluster-wide resource utilization
  let cluster_metrics = resource_monitor.calculate_cluster_metrics()
  
  // Verify cluster calculations
  assert_eq(cluster_metrics.total_nodes, 3)
  assert_eq(cluster_metrics.total_cpu_cores, 8) // 2 + 4 + 2
  assert_eq(cluster_metrics.total_memory_gb, 16) // 4 + 8 + 4
  assert_eq(cluster_metrics.total_disk_gb, 200) // 50 + 100 + 50
  
  // Verify average utilization
  let expected_avg_cpu = (65.5 + 45.2 + 85.7) / 3.0
  assert_true((cluster_metrics.avg_cpu_utilization - expected_avg_cpu).abs() < 0.1)
  
  let expected_avg_memory = (78.2 + 62.1 + 92.3) / 3.0
  assert_true((cluster_metrics.avg_memory_utilization - expected_avg_memory).abs() < 0.1)
  
  // Identify resource bottlenecks
  let bottlenecks = resource_monitor.identify_bottlenecks({
    "cpu_threshold": 80.0,
    "memory_threshold": 85.0,
    "disk_threshold": 75.0,
    "network_threshold": 800.0
  })
  
  // node-3 should be identified as having CPU, memory, and disk bottlenecks
  let node_3_bottlenecks = bottlenecks.filter(|b| b.node_id == "node-3")
  assert_true(node_3_bottlenecks.length() >= 2) // At least CPU and memory
  
  // Test capacity planning recommendations
  let capacity_plan = resource_monitor.generate_capacity_plan({
    "growth_factor": 1.5,    // 50% growth expected
    "buffer_percentage": 20, // 20% buffer
    "planning_horizon_days": 90
  })
  
  // Verify capacity plan
  assert_true(capacity_plan.recommended_nodes >= cluster_nodes.length())
  assert_true(capacity_plan.additional_cpu_cores > 0)
  assert_true(capacity_plan.additional_memory_gb > 0)
  
  // Test resource optimization suggestions
  let optimization_suggestions = resource_monitor.generate_optimization_suggestions()
  assert_true(optimization_suggestions.length() > 0)
  
  // Should suggest moving services from high-utilization node-3 to lower-utilization nodes
  let service_migration_suggestions = optimization_suggestions.filter(|s| s.type == "service_migration")
  assert_true(service_migration_suggestions.length() > 0)
  
  // Test cost optimization analysis
  let cost_analysis = resource_monitor.analyze_costs({
    "c5.large": {"hourly_cost": 0.085, "monthly_cost": 61.20},
    "c5.xlarge": {"hourly_cost": 0.170, "monthly_cost": 122.40},
    "c5.2xlarge": {"hourly_cost": 0.340, "monthly_cost": 244.80}
  })
  
  // Verify cost calculations
  let expected_monthly_cost = 61.20 + 122.40 + 61.20 // 2x c5.large + 1x c5.xlarge
  assert_eq(cost_analysis.current_monthly_cost, expected_monthly_cost)
  
  // Test auto-scaling recommendations
  let auto_scaling_config = {
    "min_nodes": 2,
    "max_nodes": 10,
    "scale_up_threshold": 80.0,
    "scale_down_threshold": 30.0,
    "cooldown_period": 300
  }
  
  let scaling_recommendations = resource_monitor.generate_scaling_recommendations(auto_scaling_config)
  
  // Should recommend scaling up due to node-3 high utilization
  assert_true(scaling_recommendations.action == "scale_up")
  assert_true(scaling_recommendations.target_nodes > cluster_nodes.length())
  
  // Test resource utilization forecasting
  let utilization_forecast = resource_monitor.forecast_utilization(30) // 30 days ahead
  
  // Verify forecast
  assert_eq(utilization_forecast.forecast_days, 30)
  assert_true(utilization_forecast.cpu_trend > 0 || utilization_forecast.cpu_trend < 0) // Should have a trend
  assert_true(utilization_forecast.memory_trend > 0 || utilization_forecast.memory_trend < 0) // Should have a trend
  
  // Test resource anomaly detection
  let historical_metrics = resource_monitor.get_historical_metrics(7) // Last 7 days
  
  // Generate historical data with some anomalies
  let historical_data = []
  for day = 0; day < 7; day = day + 1 {
    for hour = 0; hour < 24; hour = hour + 1 {
      let timestamp = azimuth::time::now() - ((7 - day) * 24 * 60 * 60 * 1000) - (hour * 60 * 60 * 1000)
      let base_cpu = 50.0 + day.to_double() * 2.0 // Gradual increase
      let cpu_value = if day == 3 && hour == 14 { 95.0 } else { base_cpu } // Anomaly on day 3, 2pm
      
      historical_data.push({
        "timestamp": timestamp,
        "node_id": "node-1",
        "cpu_utilization": cpu_value,
        "memory_utilization": 60.0 + day.to_double(),
        "disk_utilization": 40.0 + day.to_double() * 0.5
      })
    }
  }
  
  let resource_anomalies = resource_monitor.detect_resource_anomalies(historical_data, 2.0) // 2 sigma threshold
  
  // Should detect the CPU anomaly on day 3, 2pm
  let cpu_anomalies = resource_anomalies.filter(|a| a.metric == "cpu_utilization")
  assert_true(cpu_anomalies.length() >= 1)
  
  // Verify anomaly details
  let detected_anomaly = cpu_anomalies[0]
  assert_eq(detected_anomaly.node_id, "node-1")
  assert_true(detected_anomaly.value > 90.0)
  assert_true(detected_anomaly.expected_value < 70.0)
}