// Azimuth Telemetry System - Resource Lifecycle Management Tests
// This file contains comprehensive test cases for resource management and lifecycle

// Test 1: Span Lifecycle Management
test "span lifecycle management" {
  let lifecycle_manager = SpanLifecycleManager::new()
  
  // Test span creation and initialization
  let span_ctx = SpanContext::new("trace123", "span456", true, "active")
  let span = SpanLifecycleManager::create_span(lifecycle_manager, "test_operation", Internal, span_ctx)
  
  assert_eq(Span::name(span), "test_operation")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  assert_eq(Span::status(span), Unset)
  
  // Test span state transitions
  SpanLifecycleManager::set_status(lifecycle_manager, span, Ok, Some("Operation completed"))
  assert_eq(Span::status(span), Ok)
  
  // Test span event lifecycle
  let event_id = SpanLifecycleManager::add_event(lifecycle_manager, span, "operation_started", 
    Some([("component", StringValue("auth"))]))
  assert_true(event_id > 0)
  
  // Test event retrieval
  let events = SpanLifecycleManager::get_events(lifecycle_manager, span)
  assert_eq(events.length(), 1)
  assert_eq(events[0].name, "operation_started")
  
  // Test span attribute lifecycle
  SpanLifecycleManager::set_attribute(lifecycle_manager, span, "user.id", StringValue("user123"))
  SpanLifecycleManager::set_attribute(lifecycle_manager, span, "operation.type", StringValue("authentication"))
  
  let attributes = SpanLifecycleManager::get_attributes(lifecycle_manager, span)
  assert_eq(attributes.length(), 2)
  
  // Test span termination
  SpanLifecycleManager::end_span(lifecycle_manager, span)
  assert_false(Span::is_recording(span))
  
  // Test span lifecycle statistics
  let stats = SpanLifecycleManager::get_stats(lifecycle_manager)
  assert_eq(stats.total_spans_created, 1)
  assert_eq(stats.total_spans_ended, 1)
  assert_eq(stats.total_events_added, 1)
  assert_eq(stats.total_attributes_set, 2)
}

// Test 2: Resource Pool Management
test "resource pool management" {
  let resource_pool = ResourcePool::new(
    5,  // Min resources
    20, // Max resources
    30000 // Resource timeout (ms)
  )
  
  // Test resource acquisition
  let resources = []
  for i in 0..=10 {
    let resource = ResourcePool::acquire(resource_pool)
    match resource {
      Some(res) => {
        assert_true(ResourcePool::is_valid(res))
        resources.push(res)
      }
      None => assert_true(false)
    }
  }
  
  // Test pool statistics during usage
  let stats = ResourcePool::get_stats(resource_pool)
  assert_eq(stats.active_resources, 11)
  assert_eq(stats.available_resources, 0)
  assert_eq(stats.total_acquisitions, 11)
  
  // Test resource release
  for resource in resources {
    ResourcePool::release(resource_pool, resource)
  }
  
  // Verify resources are returned to pool
  let after_release_stats = ResourcePool::get_stats(resource_pool)
  assert_eq(after_release_stats.active_resources, 0)
  assert_eq(after_release_stats.available_resources, 11)
  assert_eq(after_release_stats.total_releases, 11)
  
  // Test resource cleanup
  ResourcePool::cleanup_idle_resources(resource_pool, 60000) // 1 minute idle timeout
  let cleanup_stats = ResourcePool::get_stats(resource_pool)
  assert_true(cleanup_stats.cleaned_up_resources >= 0)
}

// Test 3: Memory Lifecycle Management
test "memory lifecycle management" {
  let memory_manager = MemoryManager::new()
  
  // Test memory allocation tracking
  let initial_memory = MemoryManager::get_allocated_memory(memory_manager)
  
  // Allocate memory blocks
  let memory_blocks = []
  for i in 0..=100 {
    let block = MemoryManager::allocate(memory_manager, 1024 * (i + 1)) // 1KB to 101KB
    memory_blocks.push(block)
  }
  
  let allocated_memory = MemoryManager::get_allocated_memory(memory_manager)
  assert_true(allocated_memory > initial_memory)
  
  // Test memory block lifecycle
  for block in memory_blocks {
    assert_true(MemoryManager::is_allocated(block))
    assert_true(MemoryManager::get_size(block) > 0)
  }
  
  // Test memory deallocation
  for block in memory_blocks {
    MemoryManager::deallocate(memory_manager, block)
  }
  
  let deallocated_memory = MemoryManager::get_allocated_memory(memory_manager)
  assert_true(deallocated_memory <= allocated_memory)
  
  // Test garbage collection
  MemoryManager::force_gc(memory_manager)
  let gc_memory = MemoryManager::get_allocated_memory(memory_manager)
  assert_true(gc_memory <= deallocated_memory)
  
  // Test memory pressure handling
  MemoryManager::simulate_pressure(memory_manager, 0.8) // 80% memory usage
  let pressure_handled = MemoryManager::handle_memory_pressure(memory_manager)
  assert_true(pressure_handled)
  
  let memory_stats = MemoryManager::get_stats(memory_manager)
  assert_true(memory_stats.total_allocated > 0)
  assert_true(memory_stats.total_deallocated > 0)
  assert_true(memory_stats.gc_triggered > 0)
}

// Test 4: Connection Lifecycle Management
test "connection lifecycle management" {
  let connection_manager = ConnectionManager::new()
  
  // Test connection creation
  let connections = []
  for i in 0..=5 {
    let connection = ConnectionManager::create_connection(connection_manager, 
      "conn_" + i.to_string(), "localhost", 8080 + i)
    connections.push(connection)
  }
  
  // Test connection state management
  for connection in connections {
    assert_eq(ConnectionManager::get_state(connection_manager, connection), Created)
    
    // Activate connection
    ConnectionManager::activate(connection_manager, connection)
    assert_eq(ConnectionManager::get_state(connection_manager, connection), Active)
    
    // Test connection health
    let health = ConnectionManager::check_health(connection_manager, connection)
    assert_true(health.is_healthy)
  }
  
  // Test connection pooling
  let pool = ConnectionManager::create_pool(connection_manager, "test_pool", 3, 10)
  
  for connection in connections {
    ConnectionManager::add_to_pool(connection_manager, pool, connection)
  }
  
  let pool_stats = ConnectionManager::get_pool_stats(connection_manager, pool)
  assert_eq(pool_stats.total_connections, 6)
  assert_eq(pool_stats.active_connections, 6)
  assert_eq(pool_stats.available_connections, 6)
  
  // Test connection retrieval from pool
  let pooled_connection = ConnectionManager::get_from_pool(connection_manager, pool)
  match pooled_connection {
    Some(conn) => {
      assert_eq(ConnectionManager::get_state(connection_manager, conn), Active)
      assert_eq(ConnectionManager::get_pool_stats(connection_manager, pool).active_connections, 1)
      assert_eq(ConnectionManager::get_pool_stats(connection_manager, pool).available_connections, 5)
    }
    None => assert_true(false)
  }
  
  // Test connection lifecycle cleanup
  ConnectionManager::cleanup_stale_connections(connection_manager, pool, 300000) // 5 minutes
  let cleanup_stats = ConnectionManager::get_pool_stats(connection_manager, pool)
  assert_true(cleanup_stats.cleaned_up_connections >= 0)
}

// Test 5: Component Lifecycle Management
test "component lifecycle management" {
  let component_manager = ComponentManager::new()
  
  // Test component registration
  let telemetry_component = ComponentManager::register_component(component_manager, 
    "telemetry", TelemetryComponent::new())
  
  let metrics_component = ComponentManager::register_component(component_manager, 
    "metrics", MetricsComponent::new())
  
  let logging_component = ComponentManager::register_component(component_manager, 
    "logging", LoggingComponent::new())
  
  // Test component initialization
  ComponentManager::initialize_component(component_manager, telemetry_component)
  ComponentManager::initialize_component(component_manager, metrics_component)
  ComponentManager::initialize_component(component_manager, logging_component)
  
  assert_eq(ComponentManager::get_state(component_manager, telemetry_component), Initialized)
  assert_eq(ComponentManager::get_state(component_manager, metrics_component), Initialized)
  assert_eq(ComponentManager::get_state(component_manager, logging_component), Initialized)
  
  // Test component start
  ComponentManager::start_component(component_manager, telemetry_component)
  ComponentManager::start_component(component_manager, metrics_component)
  ComponentManager::start_component(component_manager, logging_component)
  
  assert_eq(ComponentManager::get_state(component_manager, telemetry_component), Running)
  assert_eq(ComponentManager::get_state(component_manager, metrics_component), Running)
  assert_eq(ComponentManager::get_state(component_manager, logging_component), Running)
  
  // Test component dependencies
  ComponentManager::add_dependency(component_manager, telemetry_component, metrics_component)
  ComponentManager::add_dependency(component_manager, telemetry_component, logging_component)
  
  let dependencies = ComponentManager::get_dependencies(component_manager, telemetry_component)
  assert_eq(dependencies.length(), 2)
  assert_true(dependencies.contains(metrics_component))
  assert_true(dependencies.contains(logging_component))
  
  // Test component stop
  ComponentManager::stop_component(component_manager, telemetry_component)
  assert_eq(ComponentManager::get_state(component_manager, telemetry_component), Stopped)
  
  // Test component shutdown
  ComponentManager::shutdown_component(component_manager, telemetry_component)
  ComponentManager::shutdown_component(component_manager, metrics_component)
  ComponentManager::shutdown_component(component_manager, logging_component)
  
  assert_eq(ComponentManager::get_state(component_manager, telemetry_component), Shutdown)
  assert_eq(ComponentManager::get_state(component_manager, metrics_component), Shutdown)
  assert_eq(ComponentManager::get_state(component_manager, logging_component), Shutdown)
  
  // Test component lifecycle statistics
  let lifecycle_stats = ComponentManager::get_lifecycle_stats(component_manager)
  assert_eq(lifecycle_stats.total_components, 3)
  assert_eq(lifecycle_stats.initialized_components, 3)
  assert_eq(lifecycle_stats.running_components, 0)
  assert_eq(lifecycle_stats.shutdown_components, 3)
}

// Test 6: Buffer Lifecycle Management
test "buffer lifecycle management" {
  let buffer_manager = BufferManager::new()
  
  // Test buffer creation
  let buffers = []
  for i in 0..=10 {
    let buffer = BufferManager::create_buffer(buffer_manager, 1024 * (i + 1))
    buffers.push(buffer)
  }
  
  // Test buffer state management
  for buffer in buffers {
    assert_eq(BufferManager::get_state(buffer_manager, buffer), Empty)
    
    // Write data to buffer
    let data = "test data for buffer"
    BufferManager::write(buffer_manager, buffer, data)
    assert_eq(BufferManager::get_state(buffer_manager, buffer), Data)
    
    // Read data from buffer
    let read_data = BufferManager::read(buffer_manager, buffer)
    match read_data {
      Some(content) => assert_eq(content, data)
      None => assert_true(false)
    }
  }
  
  // Test buffer pooling
  let buffer_pool = BufferManager::create_pool(buffer_manager, 1024, 50) // 1KB buffers, 50 pool size
  
  for buffer in buffers {
    BufferManager::return_to_pool(buffer_manager, buffer_pool, buffer)
  }
  
  let pool_stats = BufferManager::get_pool_stats(buffer_manager, buffer_pool)
  assert_eq(pool_stats.total_buffers, 11)
  assert_eq(pool_stats.available_buffers, 11)
  
  // Test buffer reuse
  let reused_buffers = []
  for i in 0..=5 {
    let buffer = BufferManager::get_from_pool(buffer_manager, buffer_pool)
    match buffer {
      Some(buf) => {
        assert_eq(BufferManager::get_state(buffer_manager, buf), Empty)
        reused_buffers.push(buf)
      }
      None => assert_true(false)
    }
  }
  
  // Test buffer cleanup
  for buffer in reused_buffers {
    BufferManager::cleanup(buffer_manager, buffer)
  }
  
  BufferManager::cleanup_pool(buffer_manager, buffer_pool)
  let cleanup_stats = BufferManager::get_pool_stats(buffer_manager, buffer_pool)
  assert_true(cleanup_stats.cleaned_up_buffers > 0)
}

// Test 7: Timer Lifecycle Management
test "timer lifecycle management" {
  let timer_manager = TimerManager::new()
  
  // Test timer creation
  let timers = []
  for i in 0..=5 {
    let timer = TimerManager::create_timer(timer_manager, 
      "timer_" + i.to_string(), 1000 * (i + 1)) // 1s to 6s timers
    timers.push(timer)
  }
  
  // Test timer state management
  for timer in timers {
    assert_eq(TimerManager::get_state(timer_manager, timer), Created)
    
    // Start timer
    TimerManager::start(timer_manager, timer)
    assert_eq(TimerManager::get_state(timer_manager, timer), Running)
    
    // Test timer remaining time
    let remaining = TimerManager::get_remaining_time(timer_manager, timer)
    assert_true(remaining > 0)
  }
  
  // Test timer cancellation
  TimerManager::cancel(timer_manager, timers[0])
  assert_eq(TimerManager::get_state(timer_manager, timers[0]), Cancelled)
  
  // Test timer completion simulation
  TimerManager::simulate_completion(timer_manager, timers[1])
  assert_eq(TimerManager::get_state(timer_manager, timers[1]), Completed)
  
  // Test timer reset
  TimerManager::reset(timer_manager, timers[2], 2000) // Reset to 2s
  assert_eq(TimerManager::get_state(timer_manager, timers[2]), Running)
  assert_eq(TimerManager::get_duration(timer_manager, timers[2]), 2000)
  
  // Test timer cleanup
  for timer in timers {
    TimerManager::cleanup(timer_manager, timer)
  }
  
  // Test timer lifecycle statistics
  let timer_stats = TimerManager::get_stats(timer_manager)
  assert_eq(timer_stats.total_timers_created, 6)
  assert_eq(timer_stats.total_timers_started, 5)
  assert_eq(timer_stats.total_timers_cancelled, 1)
  assert_eq(timer_stats.total_timers_completed, 1)
  assert_eq(timer_stats.total_timers_cleaned_up, 6)
}

// Test 8: File Handle Lifecycle Management
test "file handle lifecycle management" {
  let file_manager = FileManager::new()
  
  // Test file handle creation
  let file_handles = []
  for i in 0..=3 {
    let file_handle = FileManager::open_file(file_manager, 
      "test_file_" + i.to_string() + ".log", "write")
    file_handles.push(file_handle)
  }
  
  // Test file handle state management
  for handle in file_handles {
    assert_eq(FileManager::get_state(file_manager, handle), Open)
    
    // Write to file
    FileManager::write(file_manager, handle, "Test log entry\n")
    
    // Test file position
    let position = FileManager::get_position(file_manager, handle)
    assert_true(position > 0)
  }
  
  // Test file handle pooling
  let file_pool = FileManager::create_pool(file_manager, "log_files", 10)
  
  for handle in file_handles {
    FileManager::return_to_pool(file_manager, file_pool, handle)
  }
  
  let pool_stats = FileManager::get_pool_stats(file_manager, file_pool)
  assert_eq(pool_stats.total_handles, 4)
  assert_eq(pool_stats.available_handles, 4)
  
  // Test file handle reuse
  let reused_handles = []
  for i in 0..=2 {
    let handle = FileManager::get_from_pool(file_manager, file_pool, 
      "reuse_file_" + i.to_string() + ".log")
    match handle {
      Some(h) => {
        assert_eq(FileManager::get_state(file_manager, h), Open)
        reused_handles.push(h)
      }
      None => assert_true(false)
    }
  }
  
  // Test file handle flush and sync
  for handle in reused_handles {
    FileManager::flush(file_manager, handle)
    FileManager::sync(file_manager, handle)
  }
  
  // Test file handle closure
  for handle in reused_handles {
    FileManager::close(file_manager, handle)
    assert_eq(FileManager::get_state(file_manager, handle), Closed)
  }
  
  // Test file cleanup
  FileManager::cleanup_pool(file_manager, file_pool)
  let cleanup_stats = FileManager::get_pool_stats(file_manager, file_pool)
  assert_true(cleanup_stats.closed_handles > 0)
}

// Test 9: Service Lifecycle Management
test "service lifecycle management" {
  let service_manager = ServiceManager::new()
  
  // Test service registration
  let telemetry_service = ServiceManager::register_service(service_manager, 
    "telemetry_service", TelemetryService::new())
  
  let monitoring_service = ServiceManager::register_service(service_manager, 
    "monitoring_service", MonitoringService::new())
  
  // Test service configuration
  ServiceManager::configure_service(service_manager, telemetry_service, [
    ("service.name", StringValue("azimuth-telemetry")),
    ("service.version", StringValue("1.0.0")),
    ("service.port", IntValue(8080))
  ])
  
  ServiceManager::configure_service(service_manager, monitoring_service, [
    ("service.name", StringValue("azimuth-monitoring")),
    ("service.version", StringValue("1.0.0")),
    ("service.port", IntValue(8081))
  ])
  
  // Test service initialization
  ServiceManager::initialize_service(service_manager, telemetry_service)
  ServiceManager::initialize_service(service_manager, monitoring_service)
  
  assert_eq(ServiceManager::get_state(service_manager, telemetry_service), Initialized)
  assert_eq(ServiceManager::get_state(service_manager, monitoring_service), Initialized)
  
  // Test service start
  ServiceManager::start_service(service_manager, telemetry_service)
  ServiceManager::start_service(service_manager, monitoring_service)
  
  assert_eq(ServiceManager::get_state(service_manager, telemetry_service), Running)
  assert_eq(ServiceManager::get_state(service_manager, monitoring_service), Running)
  
  // Test service health monitoring
  let telemetry_health = ServiceManager::check_health(service_manager, telemetry_service)
  let monitoring_health = ServiceManager::check_health(service_manager, monitoring_service)
  
  assert_true(telemetry_health.is_healthy)
  assert_true(monitoring_health.is_healthy)
  
  // Test service restart
  ServiceManager::restart_service(service_manager, telemetry_service)
  assert_eq(ServiceManager::get_state(service_manager, telemetry_service), Running)
  
  // Test service stop
  ServiceManager::stop_service(service_manager, telemetry_service)
  ServiceManager::stop_service(service_manager, monitoring_service)
  
  assert_eq(ServiceManager::get_state(service_manager, telemetry_service), Stopped)
  assert_eq(ServiceManager::get_state(service_manager, monitoring_service), Stopped)
  
  // Test service shutdown
  ServiceManager::shutdown_service(service_manager, telemetry_service)
  ServiceManager::shutdown_service(service_manager, monitoring_service)
  
  assert_eq(ServiceManager::get_state(service_manager, telemetry_service), Shutdown)
  assert_eq(ServiceManager::get_state(service_manager, monitoring_service), Shutdown)
  
  // Test service lifecycle statistics
  let service_stats = ServiceManager::get_lifecycle_stats(service_manager)
  assert_eq(service_stats.total_services, 2)
  assert_eq(service_stats.running_services, 0)
  assert_eq(service_stats.shutdown_services, 2)
  assert_true(service_stats.total_restarts > 0)
}

// Test 10: Resource Leak Detection
test "resource leak detection" {
  let leak_detector = LeakDetector::new()
  
  // Test span leak detection
  LeakDetector::start_span_tracking(leak_detector)
  
  let leaked_spans = []
  for i in 0..=10 {
    let span_ctx = SpanContext::new("leak_test", "span_" + i.to_string(), true, "active")
    let span = Span::new("leaked_span", Internal, span_ctx)
    
    // Don't end some spans to simulate leaks
    if i % 3 != 0 {
      Span::end(span)
    } else {
      leaked_spans.push(span)
    }
  }
  
  let span_leak_report = LeakDetector::detect_span_leaks(leak_detector)
  assert_eq(span_leak_report.leaked_spans, 4) // spans 0, 3, 6, 9
  
  // Test memory leak detection
  LeakDetector::start_memory_tracking(leak_detector)
  
  let allocated_blocks = []
  for i in 0..=20 {
    let block = LeakDetector::allocate_tracked(leak_detector, 1024 * (i + 1))
    
    // Don't free some blocks to simulate leaks
    if i % 4 != 0 {
      LeakDetector::free_tracked(leak_detector, block)
    } else {
      allocated_blocks.push(block)
    }
  }
  
  let memory_leak_report = LeakDetector::detect_memory_leaks(leak_detector)
  assert_eq(memory_leak_report.leaked_blocks, 6) // blocks 0, 4, 8, 12, 16, 20
  assert_true(memory_leak_report.total_leaked_bytes > 0)
  
  // Test connection leak detection
  LeakDetector::start_connection_tracking(leak_detector)
  
  let leaked_connections = []
  for i in 0..=8 {
    let connection = LeakDetector::create_tracked_connection(leak_detector, 
      "conn_" + i.to_string())
    
    // Don't close some connections to simulate leaks
    if i % 3 != 0 {
      LeakDetector::close_tracked_connection(leak_detector, connection)
    } else {
      leaked_connections.push(connection)
    }
  }
  
  let connection_leak_report = LeakDetector::detect_connection_leaks(leak_detector)
  assert_eq(connection_leak_report.leaked_connections, 3) // connections 0, 3, 6
  
  // Test resource cleanup
  for span in leaked_spans {
    Span::end(span)
  }
  
  for block in allocated_blocks {
    LeakDetector::free_tracked(leak_detector, block)
  }
  
  for connection in leaked_connections {
    LeakDetector::close_tracked_connection(leak_detector, connection)
  }
  
  // Verify no leaks after cleanup
  let final_span_report = LeakDetector::detect_span_leaks(leak_detector)
  assert_eq(final_span_report.leaked_spans, 0)
  
  let final_memory_report = LeakDetector::detect_memory_leaks(leak_detector)
  assert_eq(final_memory_report.leaked_blocks, 0)
  
  let final_connection_report = LeakDetector::detect_connection_leaks(leak_detector)
  assert_eq(final_connection_report.leaked_connections, 0)
  
  // Test leak detection statistics
  let leak_stats = LeakDetector::get_stats(leak_detector)
  assert_true(leak_stats.total_spans_tracked > 0)
  assert_true(leak_stats.total_memory_blocks_tracked > 0)
  assert_true(leak_stats.total_connections_tracked > 0)
  assert_true(leak_stats.leaks_detected > 0)
  assert_true(leak_stats.leaks_resolved > 0)
}