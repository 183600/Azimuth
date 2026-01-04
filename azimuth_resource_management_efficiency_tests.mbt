// Azimuth Resource Management Efficiency Test Suite
// This file contains comprehensive test cases for resource management efficiency

// Test 1: Memory Pool Efficiency
test "memory pool efficiency" {
  // Initialize telemetry provider with memory pool configuration
  let pool_config = {
    initial_size: 100,
    max_size: 1000,
    growth_factor: 1.5,
    shrink_threshold: 0.25
  }
  
  let provider = TelemetryProvider::with_memory_pool(pool_config)
  let tracer = TelemetryProvider::get_tracer(provider, "memory_pool_tracer")
  
  // Measure initial memory usage
  let initial_memory = Memory::get_usage()
  let pool_stats_before = TelemetryProvider::get_memory_pool_stats(provider)
  
  // Create a large number of spans
  let span_count = 500
  let mut spans = []
  
  for i in 0..span_count {
    let span = Tracer::start_span(tracer, "memory_pool_span_" + i.to_string())
    Span::add_attribute(span, "iteration", i.to_string())
    spans = spans.push(span)
  }
  
  // Measure memory usage after span creation
  let peak_memory = Memory::get_usage()
  let pool_stats_after_creation = TelemetryProvider::get_memory_pool_stats(provider)
  
  // End all spans
  for span in spans {
    Span::end(span)
  }
  
  // Force export and cleanup
  TelemetryProvider::force_flush(provider)
  TelemetryProvider::cleanup(provider)
  
  // Measure memory usage after cleanup
  let final_memory = Memory::get_usage()
  let pool_stats_after_cleanup = TelemetryProvider::get_memory_pool_stats(provider)
  
  // Verify memory pool efficiency
  let memory_per_span = (peak_memory - initial_memory) / span_count
  let memory_released = peak_memory - final_memory
  let release_percentage = memory_released.to_float() / (peak_memory - initial_memory).to_float()
  
  // Memory usage should be reasonable
  assert_true(memory_per_span < 5000) // Less than 5KB per span
  
  // Memory should be released back to pool
  assert_true(release_percentage > 0.8) // At least 80% released
  
  // Verify pool statistics
  assert_true(pool_stats_after_creation.active_objects > pool_stats_before.active_objects)
  assert_true(pool_stats_after_cleanup.pool_size > pool_stats_before.pool_size)
  
  // Log efficiency metrics
  let meter = TelemetryProvider::get_meter(provider, "memory_efficiency")
  let memory_per_span_gauge = Meter::create_gauge(meter, "memory_per_span_bytes")
  let release_percentage_gauge = Meter::create_gauge(meter, "memory_release_percentage")
  let pool_efficiency_gauge = Meter::create_gauge(meter, "pool_efficiency_ratio")
  
  Gauge::set(memory_per_span_gauge, memory_per_span.to_float())
  Gauge::set(release_percentage_gauge, release_percentage * 100.0)
  
  let pool_efficiency = pool_stats_after_cleanup.pool_size.to_float() / pool_stats_after_creation.active_objects.to_float()
  Gauge::set(pool_efficiency_gauge, pool_efficiency * 100.0)
}

// Test 2: Connection Pool Efficiency
test "connection pool efficiency" {
  // Initialize telemetry provider with connection pool configuration
  let conn_pool_config = {
    min_connections: 2,
    max_connections: 10,
    connection_timeout_ms: 5000,
    idle_timeout_ms: 30000,
    max_lifetime_ms: 300000
  }
  
  let provider = TelemetryProvider::with_connection_pool(conn_pool_config)
  let tracer = TelemetryProvider::get_tracer(provider, "connection_pool_tracer")
  
  // Measure initial connection pool stats
  let pool_stats_before = TelemetryProvider::get_connection_pool_stats(provider)
  
  // Simulate concurrent operations requiring connections
  let operation_count = 50
  let mut operations = []
  
  for i in 0..operation_count {
    let span = Tracer::start_span(tracer, "operation_" + i.to_string())
    
    // Simulate connection acquisition
    let connection_acquired = TelemetryProvider::acquire_connection(provider)
    
    if connection_acquired {
      // Simulate work with connection
      Span::add_attribute(span, "connection.acquired", "true")
      Span::add_attribute(span, "connection.id", i.to_string())
      
      // Simulate work duration
      let work_duration = 100 + (i % 10) * 50 // 100-550ms
      Span::add_attribute(span, "work.duration_ms", work_duration.to_string())
      
      // Release connection
      TelemetryProvider::release_connection(provider, i.to_string())
      Span::add_attribute(span, "connection.released", "true")
    } else {
      Span::add_attribute(span, "connection.acquired", "false")
      Span::add_attribute(span, "connection.timeout", "true")
    }
    
    operations = operations.push(span)
  }
  
  // End all spans
  for operation in operations {
    Span::end(operation)
  }
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Measure final connection pool stats
  let pool_stats_after = TelemetryProvider::get_connection_pool_stats(provider)
  
  // Verify connection pool efficiency
  assert_true(pool_stats_after.total_acquisitions > 0)
  assert_true(pool_stats_after.total_releases > 0)
  
  // Most acquisitions should be successful
  let success_rate = pool_stats_after.successful_acquisitions.to_float() / pool_stats_after.total_acquisitions.to_float()
  assert_true(success_rate > 0.8) // At least 80% success rate
  
  // Average wait time should be reasonable
  assert_true(pool_stats_after.average_wait_time_ms < 1000) // Less than 1 second average wait
  
  // Verify connection reuse
  let reuse_rate = pool_stats_after.connection_reuses.to_float() / pool_stats_after.successful_acquisitions.to_float()
  assert_true(reuse_rate > 0.5) // At least 50% of connections should be reused
  
  // Log efficiency metrics
  let meter = TelemetryProvider::get_meter(provider, "connection_efficiency")
  let success_rate_gauge = Meter::create_gauge(meter, "connection_success_rate")
  let reuse_rate_gauge = Meter::create_gauge(meter, "connection_reuse_rate")
  let wait_time_gauge = Meter::create_gauge(meter, "average_wait_time_ms")
  
  Gauge::set(success_rate_gauge, success_rate * 100.0)
  Gauge::set(reuse_rate_gauge, reuse_rate * 100.0)
  Gauge::set(wait_time_gauge, pool_stats_after.average_wait_time_ms.to_float())
}

// Test 3: Buffer Pool Efficiency
test "buffer pool efficiency" {
  // Initialize telemetry provider with buffer pool configuration
  let buffer_pool_config = {
    buffer_size: 4096, // 4KB buffers
    initial_buffers: 10,
    max_buffers: 100,
    allocation_threshold: 0.8
  }
  
  let provider = TelemetryProvider::with_buffer_pool(buffer_pool_config)
  let tracer = TelemetryProvider::get_tracer(provider, "buffer_pool_tracer")
  
  // Measure initial buffer pool stats
  let pool_stats_before = TelemetryProvider::get_buffer_pool_stats(provider)
  
  // Simulate operations requiring buffers
  let operation_count = 100
  let mut operations = []
  
  for i in 0..operation_count {
    let span = Tracer::start_span(tracer, "buffer_operation_" + i.to_string())
    
    // Simulate buffer allocation
    let buffer_size = 1024 + (i % 4) * 1024 // 1KB-4KB
    let buffer_acquired = TelemetryProvider::acquire_buffer(provider, buffer_size)
    
    if buffer_acquired {
      Span::add_attribute(span, "buffer.acquired", "true")
      Span::add_attribute(span, "buffer.size", buffer_size.to_string())
      
      // Simulate buffer usage
      let usage_duration = 50 + (i % 5) * 20 // 50-130ms
      Span::add_attribute(span, "buffer.usage_duration_ms", usage_duration.to_string())
      
      // Release buffer
      TelemetryProvider::release_buffer(provider, buffer_size)
      Span::add_attribute(span, "buffer.released", "true")
    } else {
      Span::add_attribute(span, "buffer.acquired", "false")
      Span::add_attribute(span, "buffer.allocation_failed", "true")
    }
    
    operations = operations.push(span)
  }
  
  // End all spans
  for operation in operations {
    Span::end(operation)
  }
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Measure final buffer pool stats
  let pool_stats_after = TelemetryProvider::get_buffer_pool_stats(provider)
  
  // Verify buffer pool efficiency
  assert_true(pool_stats_after.total_allocations > 0)
  assert_true(pool_stats_after.total_releases > 0)
  
  // Most allocations should be successful
  let success_rate = pool_stats_after.successful_allocations.to_float() / pool_stats_after.total_allocations.to_float()
  assert_true(success_rate > 0.9) // At least 90% success rate
  
  // Verify buffer reuse
  let reuse_rate = pool_stats_after.buffer_reuses.to_float() / pool_stats_after.successful_allocations.to_float()
  assert_true(reuse_rate > 0.7) // At least 70% of buffers should be reused
  
  // Memory efficiency - buffer pool should reduce total allocations
  let memory_efficiency = pool_stats_after.buffer_reuses.to_float() / pool_stats_after.total_allocations.to_float()
  assert_true(memory_efficiency > 0.5) // At least 50% reduction in allocations
  
  // Log efficiency metrics
  let meter = TelemetryProvider::get_meter(provider, "buffer_efficiency")
  let success_rate_gauge = Meter::create_gauge(meter, "buffer_success_rate")
  let reuse_rate_gauge = Meter::create_gauge(meter, "buffer_reuse_rate")
  let memory_efficiency_gauge = Meter::create_gauge(meter, "memory_efficiency_ratio")
  
  Gauge::set(success_rate_gauge, success_rate * 100.0)
  Gauge::set(reuse_rate_gauge, reuse_rate * 100.0)
  Gauge::set(memory_efficiency_gauge, memory_efficiency * 100.0)
}

// Test 4: Thread Pool Efficiency
test "thread pool efficiency" {
  // Initialize telemetry provider with thread pool configuration
  let thread_pool_config = {
    core_threads: 2,
    max_threads: 10,
    keep_alive_time_ms: 60000,
    queue_capacity: 100
  }
  
  let provider = TelemetryProvider::with_thread_pool(thread_pool_config)
  let tracer = TelemetryProvider::get_tracer(provider, "thread_pool_tracer")
  
  // Measure initial thread pool stats
  let pool_stats_before = TelemetryProvider::get_thread_pool_stats(provider)
  
  // Simulate concurrent operations
  let task_count = 200
  let mut tasks = []
  
  for i in 0..task_count {
    let span = Tracer::start_span(tracer, "thread_task_" + i.to_string())
    
    // Submit task to thread pool
    let task_submitted = TelemetryProvider::submit_task(provider, fn() {
      // Simulate work
      let work_duration = 100 + (i % 10) * 50 // 100-550ms
      Time::sleep(work_duration)
      work_duration
    })
    
    if task_submitted {
      Span::add_attribute(span, "task.submitted", "true")
      
      // Wait for task completion
      let task_result = TelemetryProvider::get_task_result(provider)
      
      match task_result {
        Some(duration) => {
          Span::add_attribute(span, "task.completed", "true")
          Span::add_attribute(span, "task.duration_ms", duration.to_string())
        }
        None => {
          Span::add_attribute(span, "task.completed", "false")
          Span::add_attribute(span, "task.timeout", "true")
        }
      }
    } else {
      Span::add_attribute(span, "task.submitted", "false")
      Span::add_attribute(span, "task.rejected", "true")
    }
    
    tasks = tasks.push(span)
  }
  
  // End all spans
  for task in tasks {
    Span::end(task)
  }
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Measure final thread pool stats
  let pool_stats_after = TelemetryProvider::get_thread_pool_stats(provider)
  
  // Verify thread pool efficiency
  assert_true(pool_stats_after.tasks_submitted > 0)
  assert_true(pool_stats_after.tasks_completed > 0)
  
  // Most tasks should be completed
  let completion_rate = pool_stats_after.tasks_completed.to_float() / pool_stats_after.tasks_submitted.to_float()
  assert_true(completion_rate > 0.9) // At least 90% completion rate
  
  // Thread utilization should be reasonable
  let max_threads_used = pool_stats_after.max_threads_reached
  assert_true(max_threads_used <= thread_pool_config.max_threads)
  
  // Average execution time should be reasonable
  assert_true(pool_stats_after.average_execution_time_ms < 1000) // Less than 1 second average
  
  // Thread reuse efficiency
  let thread_efficiency = pool_stats_after.tasks_completed.to_float() / max_threads_used.to_float()
  assert_true(thread_efficiency > 10.0) // Each thread should handle multiple tasks
  
  // Log efficiency metrics
  let meter = TelemetryProvider::get_meter(provider, "thread_efficiency")
  let completion_rate_gauge = Meter::create_gauge(meter, "task_completion_rate")
  let thread_utilization_gauge = Meter::create_gauge(meter, "thread_utilization_ratio")
  let execution_time_gauge = Meter::create_gauge(meter, "average_execution_time_ms")
  
  Gauge::set(completion_rate_gauge, completion_rate * 100.0)
  Gauge::set(thread_utilization_gauge, (max_threads_used.to_float() / thread_pool_config.max_threads.to_float()) * 100.0)
  Gauge::set(execution_time_gauge, pool_stats_after.average_execution_time_ms.to_float())
}

// Test 5: CPU Resource Management
test "cpu resource management" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "cpu_tracer")
  
  // Measure initial CPU usage
  let initial_cpu = CPU::get_usage()
  
  // Create CPU-intensive operations
  let operation_count = 20
  let mut operations = []
  
  for i in 0..operation_count {
    let span = Tracer::start_span(tracer, "cpu_intensive_operation_" + i.to_string())
    
    // Simulate CPU-intensive work
    let work_start = CPU::get_usage()
    
    // Perform CPU-intensive calculation
    let mut result = 0
    for j in 0..100000 {
      result = result + (i * j) % 1000
    }
    
    let work_end = CPU::get_usage()
    let cpu_used = work_end - work_start
    
    Span::add_attribute(span, "cpu.time_ms", cpu_used.to_string())
    Span::add_attribute(span, "calculation.result", result.to_string())
    
    operations = operations.push(span)
  }
  
  // End all spans
  for operation in operations {
    Span::end(operation)
  }
  
  // Measure final CPU usage
  let final_cpu = CPU::get_usage()
  let total_cpu_used = final_cpu - initial_cpu
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Verify CPU resource management
  assert_true(total_cpu_used > 0)
  
  // Average CPU per operation should be reasonable
  let avg_cpu_per_operation = total_cpu_used / operation_count
  assert_true(avg_cpu_per_operation < 100) // Less than 100ms per operation
  
  // CPU usage should be proportional to work
  let spans = TelemetryProvider::get_collected_spans(provider)
  let cpu_times = spans.map(fn(s) { 
    match s.attributes.find(fn(a) { a.0 == "cpu.time_ms" }) {
      Some((_, time)) => time.to_int()
      None => 0
    }
  })
  
  let total_cpu_from_spans = cpu_times.reduce(fn(acc, time) { acc + time }, 0)
  
  // CPU time from spans should be close to measured CPU time
  let cpu_measurement_diff = (total_cpu_from_spans - total_cpu_used).abs()
  assert_true(cpu_measurement_diff < total_cpu_used / 2) // Within 50% of measured time
  
  // Log efficiency metrics
  let meter = TelemetryProvider::get_meter(provider, "cpu_efficiency")
  let cpu_per_operation_gauge = Meter::create_gauge(meter, "cpu_time_per_operation_ms")
  let total_cpu_gauge = Meter::create_gauge(meter, "total_cpu_time_ms")
  
  Gauge::set(cpu_per_operation_gauge, avg_cpu_per_operation.to_float())
  Gauge::set(total_cpu_gauge, total_cpu_used.to_float())
}

// Test 6: Disk I/O Resource Management
test "disk io resource management" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "disk_io_tracer")
  
  // Measure initial disk I/O
  let initial_disk = DiskIO::get_stats()
  
  // Create disk I/O intensive operations
  let operation_count = 10
  let file_size = 1024 * 1024 // 1MB
  let mut operations = []
  
  for i in 0..operation_count {
    let span = Tracer::start_span(tracer, "disk_io_operation_" + i.to_string())
    
    // Simulate file write
    let write_start = DiskIO::get_stats()
    
    let file_name = "test_file_" + i.to_string() + ".tmp"
    let file_content = "x".repeat(file_size)
    
    let write_success = FileSystem::write_file(file_name, file_content)
    
    let write_end = DiskIO::get_stats()
    let write_bytes = write_end.bytes_written - write_start.bytes_written
    let write_time = write_end.write_time_ms - write_start.write_time_ms
    
    if write_success {
      Span::add_attribute(span, "file.write", "success")
      Span::add_attribute(span, "file.name", file_name)
      Span::add_attribute(span, "file.size", file_size.to_string())
      Span::add_attribute(span, "disk.write_bytes", write_bytes.to_string())
      Span::add_attribute(span, "disk.write_time_ms", write_time.to_string())
      
      // Simulate file read
      let read_start = DiskIO::get_stats()
      
      let read_content = FileSystem::read_file(file_name)
      
      let read_end = DiskIO::get_stats()
      let read_bytes = read_end.bytes_read - read_start.bytes_read
      let read_time = read_end.read_time_ms - read_start.read_time_ms
      
      match read_content {
        Some(content) => {
          Span::add_attribute(span, "file.read", "success")
          Span::add_attribute(span, "disk.read_bytes", read_bytes.to_string())
          Span::add_attribute(span, "disk.read_time_ms", read_time.to_string())
          
          // Verify content
          assert_eq(content.length(), file_size)
        }
        None => {
          Span::add_attribute(span, "file.read", "failed")
        }
      }
      
      // Clean up file
      FileSystem::delete_file(file_name)
    } else {
      Span::add_attribute(span, "file.write", "failed")
    }
    
    operations = operations.push(span)
  }
  
  // End all spans
  for operation in operations {
    Span::end(operation)
  }
  
  // Measure final disk I/O
  let final_disk = DiskIO::get_stats()
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Verify disk I/O resource management
  let total_bytes_written = final_disk.bytes_written - initial_disk.bytes_written
  let total_bytes_read = final_disk.bytes_read - initial_disk.bytes_read
  let total_write_time = final_disk.write_time_ms - initial_disk.write_time_ms
  let total_read_time = final_disk.read_time_ms - initial_disk.read_time_ms
  
  // Should have written and read approximately operation_count * file_size bytes
  let expected_bytes = operation_count * file_size
  let write_efficiency = total_bytes_written.to_float() / expected_bytes.to_float()
  let read_efficiency = total_bytes_read.to_float() / expected_bytes.to_float()
  
  assert_true(write_efficiency > 0.8) // At least 80% of expected bytes written
  assert_true(read_efficiency > 0.8) // At least 80% of expected bytes read
  
  // Average I/O time should be reasonable
  let avg_write_time = total_write_time / operation_count
  let avg_read_time = total_read_time / operation_count
  
  assert_true(avg_write_time < 1000) // Less than 1 second per write
  assert_true(avg_read_time < 1000) // Less than 1 second per read
  
  // Log efficiency metrics
  let meter = TelemetryProvider::get_meter(provider, "disk_io_efficiency")
  let write_efficiency_gauge = Meter::create_gauge(meter, "write_efficiency_percentage")
  let read_efficiency_gauge = Meter::create_gauge(meter, "read_efficiency_percentage")
  let write_throughput_gauge = Meter::create_gauge(meter, "write_throughput_mb_per_sec")
  let read_throughput_gauge = Meter::create_gauge(meter, "read_throughput_mb_per_sec")
  
  Gauge::set(write_efficiency_gauge, write_efficiency * 100.0)
  Gauge::set(read_efficiency_gauge, read_efficiency * 100.0)
  
  let write_throughput = (total_bytes_written / (1024 * 1024)).to_float() / (total_write_time / 1000.0).to_float()
  let read_throughput = (total_bytes_read / (1024 * 1024)).to_float() / (total_read_time / 1000.0).to_float()
  
  Gauge::set(write_throughput_gauge, write_throughput)
  Gauge::set(read_throughput_gauge, read_throughput)
}

// Test 7: Network Resource Management
test "network resource management" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "network_tracer")
  
  // Measure initial network stats
  let initial_network = Network::get_stats()
  
  // Create network-intensive operations
  let operation_count = 15
  let request_size = 1024 // 1KB
  let mut operations = []
  
  for i in 0..operation_count {
    let span = Tracer::start_span(tracer, "network_operation_" + i.to_string())
    
    // Simulate HTTP request
    let request_start = Network::get_stats()
    
    let url = "https://httpbin.org/post"
    let request_data = "x".repeat(request_size)
    
    let response = HTTP::post(url, request_data)
    
    let request_end = Network::get_stats()
    let bytes_sent = request_end.bytes_sent - request_start.bytes_sent
    let bytes_received = request_end.bytes_received - request_start.bytes_received
    let request_time = request_end.request_time_ms - request_start.request_time_ms
    
    match response {
      Some(resp) => {
        Span::add_attribute(span, "http.request", "success")
        Span::add_attribute(span, "http.status_code", resp.status_code.to_string())
        Span::add_attribute(span, "http.url", url)
        Span::add_attribute(span, "network.bytes_sent", bytes_sent.to_string())
        Span::add_attribute(span, "network.bytes_received", bytes_received.to_string())
        Span::add_attribute(span, "network.request_time_ms", request_time.to_string())
        
        // Verify response
        assert_true(resp.status_code >= 200 && resp.status_code < 300)
      }
      None => {
        Span::add_attribute(span, "http.request", "failed")
        Span::add_attribute(span, "network.error", "true")
      }
    }
    
    operations = operations.push(span)
  }
  
  // End all spans
  for operation in operations {
    Span::end(operation)
  }
  
  // Measure final network stats
  let final_network = Network::get_stats()
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Verify network resource management
  let total_bytes_sent = final_network.bytes_sent - initial_network.bytes_sent
  let total_bytes_received = final_network.bytes_received - initial_network.bytes_sent
  let total_request_time = final_network.request_time_ms - initial_network.request_time_ms
  
  // Should have sent and received approximately operation_count * request_size bytes
  let min_expected_bytes = operation_count * request_size
  assert_true(total_bytes_sent >= min_expected_bytes)
  assert_true(total_bytes_received > 0)
  
  // Average request time should be reasonable
  let avg_request_time = total_request_time / operation_count
  assert_true(avg_request_time < 5000) // Less than 5 seconds per request
  
  // Network efficiency
  let network_efficiency = total_bytes_received.to_float() / total_bytes_sent.to_float()
  assert_true(network_efficiency > 0.1) // At least 10% of sent bytes should be received
  
  // Log efficiency metrics
  let meter = TelemetryProvider::get_meter(provider, "network_efficiency")
  let avg_request_time_gauge = Meter::create_gauge(meter, "average_request_time_ms")
  let network_efficiency_gauge = Meter::create_gauge(meter, "network_efficiency_ratio")
  let throughput_gauge = Meter::create_gauge(meter, "network_throughput_kb_per_sec")
  
  Gauge::set(avg_request_time_gauge, avg_request_time.to_float())
  Gauge::set(network_efficiency_gauge, network_efficiency * 100.0)
  
  let total_bytes = total_bytes_sent + total_bytes_received
  let throughput = (total_bytes / 1024).to_float() / (total_request_time / 1000.0).to_float()
  Gauge::set(throughput_gauge, throughput)
}

// Test 8: Resource Cleanup Efficiency
test "resource cleanup efficiency" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "cleanup_tracer")
  
  // Measure initial resource usage
  let initial_memory = Memory::get_usage()
  let initial_cpu = CPU::get_usage()
  
  // Create resource-intensive operations
  let resource_count = 100
  let mut resources = []
  
  for i in 0..resource_count {
    let span = Tracer::start_span(tracer, "resource_operation_" + i.to_string())
    
    // Allocate various resources
    let memory_size = 1024 * (i % 10 + 1) // 1KB-10KB
    let memory_allocated = Memory::allocate(memory_size)
    
    if memory_allocated {
      Span::add_attribute(span, "memory.allocated", "true")
      Span::add_attribute(span, "memory.size", memory_size.to_string())
      
      // Simulate CPU work
      let mut result = 0
      for j in 0..1000 {
        result = result + (i * j) % 100
      }
      
      Span::add_attribute(span, "cpu.work", "true")
      Span::add_attribute(span, "cpu.result", result.to_string())
      
      // Store resource reference for cleanup
      resources = resources.push((span, memory_size))
    } else {
      Span::add_attribute(span, "memory.allocated", "false")
    }
  }
  
  // Measure peak resource usage
  let peak_memory = Memory::get_usage()
  let peak_cpu = CPU::get_usage()
  
  // Cleanup resources
  let cleanup_start = Time::now()
  
  for (span, memory_size) in resources {
    Memory::deallocate(memory_size)
    Span::add_event(span, "resource.cleaned", Some([
      ("memory.size", memory_size.to_string())
    ]))
    Span::end(span)
  }
  
  let cleanup_end = Time::now()
  let cleanup_duration = cleanup_end - cleanup_start
  
  // Force cleanup
  TelemetryProvider::force_flush(provider)
  TelemetryProvider::cleanup(provider)
  
  // Measure final resource usage
  let final_memory = Memory::get_usage()
  let final_cpu = CPU::get_usage()
  
  // Verify cleanup efficiency
  let memory_used = peak_memory - initial_memory
  let memory_released = peak_memory - final_memory
  let memory_release_rate = memory_released.to_float() / memory_used.to_float()
  
  assert_true(memory_release_rate > 0.8) // At least 80% of memory should be released
  
  // Cleanup should be fast
  let cleanup_rate = resource_count.to_float() / cleanup_duration.to_seconds()
  assert_true(cleanup_rate > 100.0) // At least 100 resources per second
  
  // CPU should return to near baseline
  let cpu_used = peak_cpu - initial_cpu
  let cpu_recovered = peak_cpu - final_cpu
  let cpu_recovery_rate = cpu_recovered.to_float() / cpu_used.to_float()
  
  assert_true(cpu_recovery_rate > 0.7) // At least 70% of CPU should be recovered
  
  // Log efficiency metrics
  let meter = TelemetryProvider::get_meter(provider, "cleanup_efficiency")
  let memory_release_rate_gauge = Meter::create_gauge(meter, "memory_release_rate")
  let cpu_recovery_rate_gauge = Meter::create_gauge(meter, "cpu_recovery_rate")
  let cleanup_rate_gauge = Meter::create_gauge(meter, "cleanup_rate_resources_per_sec")
  
  Gauge::set(memory_release_rate_gauge, memory_release_rate * 100.0)
  Gauge::set(cpu_recovery_rate_gauge, cpu_recovery_rate * 100.0)
  Gauge::set(cleanup_rate_gauge, cleanup_rate)
}

// Test 9: Resource Pool Scaling Efficiency
test "resource pool scaling efficiency" {
  // Test different pool sizes
  let pool_sizes = [5, 10, 25, 50]
  let operation_count = 100
  
  for pool_size in pool_sizes {
    let pool_config = {
      min_connections: 2,
      max_connections: pool_size,
      connection_timeout_ms: 5000,
      idle_timeout_ms: 30000,
      max_lifetime_ms: 300000
    }
    
    let provider = TelemetryProvider::with_connection_pool(pool_config)
    let tracer = TelemetryProvider::get_tracer(provider, "scaling_tracer")
    
    // Measure initial pool stats
    let pool_stats_before = TelemetryProvider::get_connection_pool_stats(provider)
    
    // Simulate operations
    let scaling_start = Time::now()
    
    for i in 0..operation_count {
      let span = Tracer::start_span(tracer, "scaling_operation_" + i.to_string())
      
      // Acquire connection
      let connection_acquired = TelemetryProvider::acquire_connection(provider)
      
      if connection_acquired {
        // Simulate work
        Time::sleep(10) // 10ms work
        
        // Release connection
        TelemetryProvider::release_connection(provider, i.to_string())
      }
      
      Span::end(span)
    }
    
    let scaling_end = Time::now()
    let scaling_duration = scaling_end - scaling_start
    
    // Force export
    TelemetryProvider::force_flush(provider)
    
    // Measure final pool stats
    let pool_stats_after = TelemetryProvider::get_connection_pool_stats(provider)
    
    // Calculate scaling efficiency
    let throughput = operation_count.to_float() / scaling_duration.to_seconds()
    let connection_utilization = pool_stats_after.max_threads_reached.to_float() / pool_size.to_float()
    
    // Log efficiency metrics
    let meter = TelemetryProvider::get_meter(provider, "scaling_efficiency")
    let throughput_gauge = Meter::create_gauge(meter, "operation_throughput")
    let utilization_gauge = Meter::create_gauge(meter, "connection_utilization")
    let pool_size_gauge = Meter::create_gauge(meter, "pool_size")
    
    Gauge::set(throughput_gauge, throughput)
    Gauge::set(utilization_gauge, connection_utilization * 100.0)
    Gauge::set(pool_size_gauge, pool_size.to_float())
    
    // Verify scaling efficiency
    assert_true(throughput > 50.0) // At least 50 operations per second
    assert_true(connection_utilization <= 1.0) // Utilization should not exceed 100%
  }
}

// Test 10: Comprehensive Resource Management
test "comprehensive resource management" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "comprehensive_tracer")
  let meter = TelemetryProvider::get_meter(provider, "resource_meter")
  
  // Create resource metrics
  let memory_gauge = Meter::create_gauge(meter, "memory_usage_mb")
  let cpu_gauge = Meter::create_gauge(meter, "cpu_usage_percentage")
  let disk_io_gauge = Meter::create_gauge(meter, "disk_io_kb_per_sec")
  let network_gauge = Meter::create_gauge(meter, "network_kb_per_sec")
  
  // Measure initial resource usage
  let initial_memory = Memory::get_usage()
  let initial_cpu = CPU::get_usage()
  let initial_disk = DiskIO::get_stats()
  let initial_network = Network::get_stats()
  
  // Create comprehensive resource test
  let test_duration = 5000 // 5 seconds
  let test_start = Time::now()
  let mut operations = []
  
  while (Time::now() - test_start).to_milliseconds() < test_duration {
    let i = operations.length()
    let span = Tracer::start_span(tracer, "comprehensive_operation_" + i.to_string())
    
    // Memory operation
    let memory_size = 1024 * (i % 5 + 1) // 1KB-5KB
    let memory_allocated = Memory::allocate(memory_size)
    
    if memory_allocated {
      Span::add_attribute(span, "memory.allocated", "true")
      Span::add_attribute(span, "memory.size", memory_size.to_string())
      
      // CPU operation
      let mut result = 0
      for j in 0..100 {
        result = result + (i * j) % 100
      }
      
      Span::add_attribute(span, "cpu.work", "true")
      
      // Disk I/O operation (every 10 operations)
      if i % 10 == 0 {
        let file_name = "temp_" + i.to_string() + ".tmp"
        let file_content = "x".repeat(1024) // 1KB
        
        if FileSystem::write_file(file_name, file_content) {
          let read_content = FileSystem::read_file(file_name)
          FileSystem::delete_file(file_name)
          
          match read_content {
            Some(_) => Span::add_attribute(span, "disk.io", "success")
            None => Span::add_attribute(span, "disk.io", "failed")
          }
        } else {
          Span::add_attribute(span, "disk.io", "failed")
        }
      }
      
      // Network operation (every 20 operations)
      if i % 20 == 0 {
        let response = HTTP::get("https://httpbin.org/get")
        
        match response {
          Some(_) => Span::add_attribute(span, "network.request", "success")
          None => Span::add_attribute(span, "network.request", "failed")
        }
      }
      
      // Deallocate memory
      Memory::deallocate(memory_size)
    }
    
    operations = operations.push(span)
    
    // Update resource metrics
    let current_memory = Memory::get_usage()
    let current_cpu = CPU::get_usage()
    let current_disk = DiskIO::get_stats()
    let current_network = Network::get_stats()
    
    Gauge::set(memory_gauge, (current_memory / (1024 * 1024)).to_float())
    Gauge::set(cpu_gauge, current_cpu.to_float())
    
    let disk_io_kb = ((current_disk.bytes_read + current_disk.bytes_written) / 1024).to_float()
    let network_kb = ((current_network.bytes_sent + current_network.bytes_received) / 1024).to_float()
    
    Gauge::set(disk_io_gauge, disk_io_kb)
    Gauge::set(network_gauge, network_kb)
    
    // Small delay to prevent overwhelming the system
    Time::sleep(10)
  }
  
  // End all spans
  for operation in operations {
    Span::end(operation)
  }
  
  // Measure final resource usage
  let final_memory = Memory::get_usage()
  let final_cpu = CPU::get_usage()
  let final_disk = DiskIO::get_stats()
  let final_network = Network::get_stats()
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Calculate resource efficiency
  let memory_used = final_memory - initial_memory
  let cpu_used = final_cpu - initial_cpu
  let disk_io_bytes = (final_disk.bytes_read + final_disk.bytes_written) - (initial_disk.bytes_read + initial_disk.bytes_written)
  let network_bytes = (final_network.bytes_sent + final_network.bytes_received) - (initial_network.bytes_sent + initial_network.bytes_received)
  
  // Verify resource usage is reasonable
  let memory_per_operation = memory_used / operations.length()
  let cpu_per_operation = cpu_used / operations.length()
  
  assert_true(memory_per_operation < 10240) // Less than 10KB per operation
  assert_true(cpu_per_operation < 50) // Less than 50ms per operation
  
  // Resource efficiency metrics
  let memory_efficiency = operations.length().to_float() / (memory_used / 1024).to_float() // Operations per KB
  let cpu_efficiency = operations.length().to_float() / (cpu_used / 1000).to_float() // Operations per CPU second
  
  assert_true(memory_efficiency > 1.0) // At least 1 operation per KB
  assert_true(cpu_efficiency > 10.0) // At least 10 operations per CPU second
  
  // Log comprehensive efficiency metrics
  let comprehensive_meter = TelemetryProvider::get_meter(provider, "comprehensive_efficiency")
  let memory_efficiency_gauge = Meter::create_gauge(comprehensive_meter, "memory_efficiency_ops_per_kb")
  let cpu_efficiency_gauge = Meter::create_gauge(comprehensive_meter, "cpu_efficiency_ops_per_cpu_sec")
  let total_operations_gauge = Meter::create_gauge(comprehensive_meter, "total_operations")
  
  Gauge::set(memory_efficiency_gauge, memory_efficiency)
  Gauge::set(cpu_efficiency_gauge, cpu_efficiency)
  Gauge::set(total_operations_gauge, operations.length().to_float())
}