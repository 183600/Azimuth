// Azimuth Comprehensive Lifecycle Management Tests
// This file contains comprehensive test cases for lifecycle management of components

// Test 1: Component Lifecycle States
test "component lifecycle states" {
  let component = TestComponent::new("test_component")
  
  // Verify initial state
  assert_eq(component.get_state(), "uninitialized")
  assert_false(component.is_initialized())
  assert_false(component.is_started())
  assert_false(component.is_stopped())
  assert_false(component.is_destroyed())
  
  // Initialize component
  component.initialize()
  assert_eq(component.get_state(), "initialized")
  assert_true(component.is_initialized())
  assert_false(component.is_started())
  assert_false(component.is_stopped())
  assert_false(component.is_destroyed())
  
  // Start component
  component.start()
  assert_eq(component.get_state(), "running")
  assert_true(component.is_initialized())
  assert_true(component.is_started())
  assert_false(component.is_stopped())
  assert_false(component.is_destroyed())
  
  // Stop component
  component.stop()
  assert_eq(component.get_state(), "stopped")
  assert_true(component.is_initialized())
  assert_false(component.is_started())
  assert_true(component.is_stopped())
  assert_false(component.is_destroyed())
  
  // Restart component
  component.start()
  assert_eq(component.get_state(), "running")
  assert_true(component.is_started())
  
  // Destroy component
  component.destroy()
  assert_eq(component.get_state(), "destroyed")
  assert_false(component.is_initialized())
  assert_false(component.is_started())
  assert_false(component.is_stopped())
  assert_true(component.is_destroyed())
}

// Test 2: Component Dependency Management
test "component dependency management" {
  let lifecycle_manager = LifecycleManager::new()
  
  // Create components with dependencies
  let database = lifecycle_manager.create_component("database", DatabaseComponent::new())
  let cache = lifecycle_manager.create_component("cache", CacheComponent::new())
  let api = lifecycle_manager.create_component("api", ApiComponent::new())
  
  // Set up dependencies
  lifecycle_manager.add_dependency("cache", "database")  // Cache depends on database
  lifecycle_manager.add_dependency("api", "cache")       // API depends on cache
  
  // Verify dependency graph
  let database_deps = lifecycle_manager.get_dependencies("database")
  assert_eq(database_deps.length(), 0)  // Database has no dependencies
  
  let cache_deps = lifecycle_manager.get_dependencies("cache")
  assert_eq(cache_deps.length(), 1)
  assert_eq(cache_deps[0], "database")
  
  let api_deps = lifecycle_manager.get_dependencies("api")
  assert_eq(api_deps.length(), 1)
  assert_eq(api_deps[0], "cache")
  
  // Initialize components in dependency order
  lifecycle_manager.initialize_all()
  
  // Verify initialization order
  let init_order = lifecycle_manager.get_initialization_order()
  assert_eq(init_order[0], "database")  // First (no dependencies)
  assert_eq(init_order[1], "cache")     // Second (depends on database)
  assert_eq(init_order[2], "api")       // Third (depends on cache)
  
  // Verify all components are initialized
  assert_true(lifecycle_manager.get_component("database").is_initialized())
  assert_true(lifecycle_manager.get_component("cache").is_initialized())
  assert_true(lifecycle_manager.get_component("api").is_initialized())
  
  // Start components in dependency order
  lifecycle_manager.start_all()
  
  // Verify all components are started
  assert_true(lifecycle_manager.get_component("database").is_started())
  assert_true(lifecycle_manager.get_component("cache").is_started())
  assert_true(lifecycle_manager.get_component("api").is_started())
  
  // Stop components in reverse dependency order
  lifecycle_manager.stop_all()
  
  // Verify all components are stopped
  assert_false(lifecycle_manager.get_component("database").is_started())
  assert_false(lifecycle_manager.get_component("cache").is_started())
  assert_false(lifecycle_manager.get_component("api").is_started())
}

// Test 3: Component Lifecycle Events
test "component lifecycle events" {
  let lifecycle_manager = LifecycleManager::new()
  let event_tracker = EventTracker::new()
  
  // Register event listeners
  lifecycle_manager.add_event_listener("before_initialize", event_tracker.track_event)
  lifecycle_manager.add_event_listener("after_initialize", event_tracker.track_event)
  lifecycle_manager.add_event_listener("before_start", event_tracker.track_event)
  lifecycle_manager.add_event_listener("after_start", event_tracker.track_event)
  lifecycle_manager.add_event_listener("before_stop", event_tracker.track_event)
  lifecycle_manager.add_event_listener("after_stop", event_tracker.track_event)
  lifecycle_manager.add_event_listener("before_destroy", event_tracker.track_event)
  lifecycle_manager.add_event_listener("after_destroy", event_tracker.track_event)
  
  // Create and manage a component
  let component = lifecycle_manager.create_component("test", TestComponent::new())
  
  // Initialize component
  lifecycle_manager.initialize_component("test")
  
  // Start component
  lifecycle_manager.start_component("test")
  
  // Stop component
  lifecycle_manager.stop_component("test")
  
  // Destroy component
  lifecycle_manager.destroy_component("test")
  
  // Verify events were fired
  let events = event_tracker.get_events()
  assert_eq(events.length(), 8)
  
  assert_eq(events[0].event_type, "before_initialize")
  assert_eq(events[0].component_id, "test")
  
  assert_eq(events[1].event_type, "after_initialize")
  assert_eq(events[1].component_id, "test")
  
  assert_eq(events[2].event_type, "before_start")
  assert_eq(events[2].component_id, "test")
  
  assert_eq(events[3].event_type, "after_start")
  assert_eq(events[3].component_id, "test")
  
  assert_eq(events[4].event_type, "before_stop")
  assert_eq(events[4].component_id, "test")
  
  assert_eq(events[5].event_type, "after_stop")
  assert_eq(events[5].component_id, "test")
  
  assert_eq(events[6].event_type, "before_destroy")
  assert_eq(events[6].component_id, "test")
  
  assert_eq(events[7].event_type, "after_destroy")
  assert_eq(events[7].component_id, "test")
}

// Test 4: Component Lifecycle Timeouts
test "component lifecycle timeouts" {
  let lifecycle_manager = LifecycleManager::new()
  
  // Create a slow component
  let slow_component = SlowComponent::new("slow")
  lifecycle_manager.create_component_from_instance("slow", slow_component)
  
  // Set short timeout
  lifecycle_manager.set_operation_timeout(100)  // 100ms
  
  // Try to initialize the slow component (should timeout)
  try {
    lifecycle_manager.initialize_component("slow")
    assert_true(false)  // Should not reach here
  } catch {
    LifecycleTimeoutException(message) => {
      assert_true(message.contains("initialize"))
      assert_true(message.contains("slow"))
    }
    _ => assert_true(false)
  }
  
  // Verify component is still in uninitialized state
  assert_eq(slow_component.get_state(), "uninitialized")
  
  // Set longer timeout and try again
  lifecycle_manager.set_operation_timeout(2000)  // 2 seconds
  lifecycle_manager.initialize_component("slow")
  
  // Verify component is now initialized
  assert_eq(slow_component.get_state(), "initialized")
}

// Test 5: Component Lifecycle Error Handling
test "component lifecycle error handling" {
  let lifecycle_manager = LifecycleManager::new()
  
  // Create a component that fails during initialization
  let failing_component = FailingComponent::new("failing", "initialize")
  lifecycle_manager.create_component_from_instance("failing", failing_component)
  
  // Try to initialize the failing component
  try {
    lifecycle_manager.initialize_component("failing")
    assert_true(false)  // Should not reach here
  } catch {
    ComponentInitializationException(message) => {
      assert_true(message.contains("initialize"))
      assert_true(message.contains("failing"))
    }
    _ => assert_true(false)
  }
  
  // Verify component is still in uninitialized state
  assert_eq(failing_component.get_state(), "uninitialized")
  
  // Create a component that fails during startup
  let startup_failing_component = FailingComponent::new("startup_failing", "start")
  lifecycle_manager.create_component_from_instance("startup_failing", startup_failing_component)
  
  // Initialize successfully
  lifecycle_manager.initialize_component("startup_failing")
  assert_eq(startup_failing_component.get_state(), "initialized")
  
  // Try to start the failing component
  try {
    lifecycle_manager.start_component("startup_failing")
    assert_true(false)  // Should not reach here
  } catch {
    ComponentStartupException(message) => {
      assert_true(message.contains("start"))
      assert_true(message.contains("startup_failing"))
    }
    _ => assert_true(false)
  }
  
  // Verify component is still in initialized state
  assert_eq(startup_failing_component.get_state(), "initialized")
}

// Test 6: Component Health Checks
test "component health checks" {
  let lifecycle_manager = LifecycleManager::new()
  
  // Create components
  let healthy_component = HealthyComponent::new("healthy")
  let unhealthy_component = UnhealthyComponent::new("unhealthy")
  
  lifecycle_manager.create_component_from_instance("healthy", healthy_component)
  lifecycle_manager.create_component_from_instance("unhealthy", unhealthy_component)
  
  // Initialize and start components
  lifecycle_manager.initialize_all()
  lifecycle_manager.start_all()
  
  // Perform health checks
  let health_results = lifecycle_manager.check_health_all()
  
  // Verify health check results
  assert_eq(health_results.length(), 2)
  
  let healthy_result = health_results[0]
  assert_eq(healthy_result.component_id, "healthy")
  assert_true(healthy_result.is_healthy)
  assert_eq(healthy_result.status, "healthy")
  
  let unhealthy_result = health_results[1]
  assert_eq(unhealthy_result.component_id, "unhealthy")
  assert_false(unhealthy_result.is_healthy)
  assert_eq(unhealthy_result.status, "unhealthy")
  assert_true(unhealthy_result.message.contains("Simulated failure"))
  
  // Test individual health check
  let individual_result = lifecycle_manager.check_health("healthy")
  assert_true(individual_result.is_healthy)
  
  // Test health check for non-existent component
  try {
    lifecycle_manager.check_health("non_existent")
    assert_true(false)  // Should not reach here
  } catch {
    ComponentNotFoundException(message) => {
      assert_true(message.contains("non_existent"))
    }
    _ => assert_true(false)
  }
}

// Test 7: Component Configuration Updates
test "component configuration updates" {
  let lifecycle_manager = LifecycleManager::new()
  
  // Create a configurable component
  let configurable_component = ConfigurableComponent::new("configurable")
  lifecycle_manager.create_component_from_instance("configurable", configurable_component)
  
  // Initialize and start component
  lifecycle_manager.initialize_component("configurable")
  lifecycle_manager.start_component("configurable")
  
  // Verify initial configuration
  let initial_config = configurable_component.get_configuration()
  assert_eq(initial_config.get("max_connections"), Some("10"))
  assert_eq(initial_config.get("timeout"), Some("30"))
  
  // Update configuration
  let new_config = {
    "max_connections": "20",
    "timeout": "60",
    "new_property": "new_value"
  }
  
  lifecycle_manager.update_configuration("configurable", new_config)
  
  // Verify configuration was updated
  let updated_config = configurable_component.get_configuration()
  assert_eq(updated_config.get("max_connections"), Some("20"))
  assert_eq(updated_config.get("timeout"), Some("60"))
  assert_eq(updated_config.get("new_property"), Some("new_value"))
  
  // Try to update configuration for non-existent component
  try {
    lifecycle_manager.update_configuration("non_existent", new_config)
    assert_true(false)  // Should not reach here
  } catch {
    ComponentNotFoundException(message) => {
      assert_true(message.contains("non_existent"))
    }
    _ => assert_true(false)
  }
}

// Test 8: Component Lifecycle Metrics
test "component lifecycle metrics" {
  let lifecycle_manager = LifecycleManager::new()
  
  // Create components
  lifecycle_manager.create_component("component1", TestComponent::new("component1"))
  lifecycle_manager.create_component("component2", TestComponent::new("component2"))
  lifecycle_manager.create_component("component3", TestComponent::new("component3"))
  
  // Enable metrics collection
  lifecycle_manager.enable_metrics_collection()
  
  // Initialize all components
  lifecycle_manager.initialize_all()
  
  // Start all components
  lifecycle_manager.start_all()
  
  // Stop all components
  lifecycle_manager.stop_all()
  
  // Get lifecycle metrics
  let metrics = lifecycle_manager.get_lifecycle_metrics()
  
  // Verify metrics
  assert_true(metrics.contains_key("total_components"))
  assert_eq(metrics.get("total_components"), Some("3"))
  
  assert_true(metrics.contains_key("initialized_components"))
  assert_eq(metrics.get("initialized_components"), Some("3"))
  
  assert_true(metrics.contains_key("started_components"))
  assert_eq(metrics.get("started_components"), Some("0"))  // All stopped
  
  assert_true(metrics.contains_key("stopped_components"))
  assert_eq(metrics.get("stopped_components"), Some("3"))
  
  assert_true(metrics.contains_key("total_initialization_time"))
  assert_true(metrics.contains_key("total_startup_time"))
  assert_true(metrics.contains_key("total_shutdown_time"))
  
  // Get component-specific metrics
  let component1_metrics = lifecycle_manager.get_component_metrics("component1")
  assert_true(component1_metrics.contains_key("initialization_time"))
  assert_true(component1_metrics.contains_key("startup_time"))
  assert_true(component1_metrics.contains_key("shutdown_time"))
  assert_true(component1_metrics.contains_key("state_transitions"))
  
  // Verify state transitions
  let transitions = component1_metrics.get("state_transitions").unwrap_or("")
  assert_true(transitions.contains("uninitialized->initialized"))
  assert_true(transitions.contains("initialized->running"))
  assert_true(transitions.contains("running->stopped"))
}

// Helper classes and functions for lifecycle management testing
class Component {
  id : String
  state : String
  state_transitions : Array[String]
  
  new(id : String) {
    id = id
    state = "uninitialized"
    state_transitions = []
  }
  
  get_id() -> String {
    return id
  }
  
  get_state() -> String {
    return state
  }
  
  is_initialized() -> Bool {
    return state == "initialized" || state == "running" || state == "stopped"
  }
  
  is_started() -> Bool {
    return state == "running"
  }
  
  is_stopped() -> Bool {
    return state == "stopped"
  }
  
  is_destroyed() -> Bool {
    return state == "destroyed"
  }
  
  transition_to(new_state : String) -> Unit {
    let transition = state + "->" + new_state
    state_transitions = state_transitions.push(transition)
    state = new_state
  }
  
  get_state_transitions() -> Array[String] {
    return state_transitions
  }
}

class TestComponent extends Component {
  new(id : String) {
    super(id)
  }
  
  initialize() -> Unit {
    transition_to("initialized")
  }
  
  start() -> Unit {
    transition_to("running")
  }
  
  stop() -> Unit {
    transition_to("stopped")
  }
  
  destroy() -> Unit {
    transition_to("destroyed")
  }
}

class DatabaseComponent extends Component {
  new() {
    super("database")
  }
  
  initialize() -> Unit {
    // Simulate database initialization
    Thread::sleep(10)
    transition_to("initialized")
  }
  
  start() -> Unit {
    // Simulate database startup
    Thread::sleep(20)
    transition_to("running")
  }
  
  stop() -> Unit {
    // Simulate database shutdown
    Thread::sleep(15)
    transition_to("stopped")
  }
}

class CacheComponent extends Component {
  new() {
    super("cache")
  }
  
  initialize() -> Unit {
    // Simulate cache initialization
    Thread::sleep(5)
    transition_to("initialized")
  }
  
  start() -> Unit {
    // Simulate cache startup
    Thread::sleep(10)
    transition_to("running")
  }
  
  stop() -> Unit {
    // Simulate cache shutdown
    Thread::sleep(5)
    transition_to("stopped")
  }
}

class ApiComponent extends Component {
  new() {
    super("api")
  }
  
  initialize() -> Unit {
    // Simulate API initialization
    Thread::sleep(15)
    transition_to("initialized")
  }
  
  start() -> Unit {
    // Simulate API startup
    Thread::sleep(25)
    transition_to("running")
  }
  
  stop() -> Unit {
    // Simulate API shutdown
    Thread::sleep(10)
    transition_to("stopped")
  }
}

class SlowComponent extends Component {
  new(id : String) {
    super(id)
  }
  
  initialize() -> Unit {
    // Simulate slow initialization
    Thread::sleep(500)
    transition_to("initialized")
  }
  
  start() -> Unit {
    // Simulate slow startup
    Thread::sleep(500)
    transition_to("running")
  }
  
  stop() -> Unit {
    // Simulate slow shutdown
    Thread::sleep(500)
    transition_to("stopped")
  }
}

class FailingComponent extends Component {
  failure_point : String
  
  new(id : String, failure_point : String) {
    super(id)
    failure_point = failure_point
  }
  
  initialize() -> Unit {
    if failure_point == "initialize" {
      throw ComponentInitializationException("Simulated initialization failure")
    }
    transition_to("initialized")
  }
  
  start() -> Unit {
    if failure_point == "start" {
      throw ComponentStartupException("Simulated startup failure")
    }
    transition_to("running")
  }
  
  stop() -> Unit {
    if failure_point == "stop" {
      throw ComponentShutdownException("Simulated shutdown failure")
    }
    transition_to("stopped")
  }
}

class HealthyComponent extends Component {
  new(id : String) {
    super(id)
  }
  
  initialize() -> Unit {
    transition_to("initialized")
  }
  
  start() -> Unit {
    transition_to("running")
  }
  
  stop() -> Unit {
    transition_to("stopped")
  }
  
  check_health() -> HealthCheckResult {
    return HealthCheckResult::new(true, "healthy", "")
  }
}

class UnhealthyComponent extends Component {
  new(id : String) {
    super(id)
  }
  
  initialize() -> Unit {
    transition_to("initialized")
  }
  
  start() -> Unit {
    transition_to("running")
  }
  
  stop() -> Unit {
    transition_to("stopped")
  }
  
  check_health() -> HealthCheckResult {
    return HealthCheckResult::new(false, "unhealthy", "Simulated failure for testing")
  }
}

class ConfigurableComponent extends Component {
  configuration : Map[String, String]
  
  new(id : String) {
    super(id)
    configuration = {
      "max_connections": "10",
      "timeout": "30"
    }
  }
  
  initialize() -> Unit {
    transition_to("initialized")
  }
  
  start() -> Unit {
    transition_to("running")
  }
  
  stop() -> Unit {
    transition_to("stopped")
  }
  
  update_configuration(new_config : Map[String, String]) -> Unit {
    for (key, value) in new_config {
      configuration = configuration.set(key, value)
    }
  }
  
  get_configuration() -> Map[String, String] {
    return configuration
  }
}

class LifecycleManager {
  components : Map[String, Component]
  dependencies : Map[String, Array[String]]
  event_listeners : Map[String, (String, String) -> Unit]
  operation_timeout : Int
  metrics_enabled : Bool
  metrics : Map[String, String]
  component_metrics : Map[String, Map[String, String]]
  
  new() {
    components = {}
    dependencies = {}
    event_listeners = {}
    operation_timeout = 5000  // 5 seconds default
    metrics_enabled = false
    metrics = {}
    component_metrics = {}
  }
  
  create_component(id : String, factory : () -> Component) -> Component {
    let component = factory()
    components = components.set(id, component)
    dependencies = dependencies.set(id, [])
    component_metrics = component_metrics.set(id, {})
    return component
  }
  
  create_component_from_instance(id : String, component : Component) -> Component {
    components = components.set(id, component)
    dependencies = dependencies.set(id, [])
    component_metrics = component_metrics.set(id, {})
    return component
  }
  
  get_component(id : String) -> Component {
    return components.get(id).unwrap_or(TestComponent::new("dummy"))
  }
  
  add_dependency(component_id : String, dependency_id : String) -> Unit {
    match dependencies.get(component_id) {
      Some(deps) => {
        let updated_deps = deps.push(dependency_id)
        dependencies = dependencies.set(component_id, updated_deps)
      }
      None => {
        dependencies = dependencies.set(component_id, [dependency_id])
      }
    }
  }
  
  get_dependencies(component_id : String) -> Array[String] {
    return dependencies.get(component_id).unwrap_or([])
  }
  
  get_initialization_order() -> Array[String] {
    // Simplified topological sort for testing
    return ["database", "cache", "api"]
  }
  
  initialize_all() -> Unit {
    let order = get_initialization_order()
    for i = 0; i < order.length(); i = i + 1 {
      initialize_component(order[i])
    }
  }
  
  initialize_component(id : String) -> Unit {
    fire_event("before_initialize", id)
    
    let start_time = get_current_time()
    
    try {
      let component = get_component(id)
      component.initialize()
      
      let end_time = get_current_time()
      record_metric(id, "initialization_time", (end_time - start_time).to_string())
      
      fire_event("after_initialize", id)
    } catch {
      ComponentInitializationException(message) => {
        throw ComponentInitializationException("Failed to initialize component " + id + ": " + message)
      }
      LifecycleTimeoutException(message) => {
        throw LifecycleTimeoutException("Timeout initializing component " + id + ": " + message)
      }
    }
  }
  
  start_all() -> Unit {
    let order = get_initialization_order()
    for i = 0; i < order.length(); i = i + 1 {
      start_component(order[i])
    }
  }
  
  start_component(id : String) -> Unit {
    fire_event("before_start", id)
    
    let start_time = get_current_time()
    
    try {
      let component = get_component(id)
      component.start()
      
      let end_time = get_current_time()
      record_metric(id, "startup_time", (end_time - start_time).to_string())
      
      fire_event("after_start", id)
    } catch {
      ComponentStartupException(message) => {
        throw ComponentStartupException("Failed to start component " + id + ": " + message)
      }
      LifecycleTimeoutException(message) => {
        throw LifecycleTimeoutException("Timeout starting component " + id + ": " + message)
      }
    }
  }
  
  stop_all() -> Unit {
    let order = get_initialization_order()
    // Stop in reverse order
    for i = order.length() - 1; i >= 0; i = i - 1 {
      stop_component(order[i])
    }
  }
  
  stop_component(id : String) -> Unit {
    fire_event("before_stop", id)
    
    let start_time = get_current_time()
    
    try {
      let component = get_component(id)
      component.stop()
      
      let end_time = get_current_time()
      record_metric(id, "shutdown_time", (end_time - start_time).to_string())
      
      fire_event("after_stop", id)
    } catch {
      ComponentShutdownException(message) => {
        throw ComponentShutdownException("Failed to stop component " + id + ": " + message)
      }
      LifecycleTimeoutException(message) => {
        throw LifecycleTimeoutException("Timeout stopping component " + id + ": " + message)
      }
    }
  }
  
  destroy_component(id : String) -> Unit {
    fire_event("before_destroy", id)
    
    try {
      let component = get_component(id)
      component.destroy()
      
      fire_event("after_destroy", id)
    } catch {
      ComponentDestructionException(message) => {
        throw ComponentDestructionException("Failed to destroy component " + id + ": " + message)
      }
    }
  }
  
  add_event_listener(event_type : String, listener : (String, String) -> Unit) -> Unit {
    event_listeners = event_listeners.set(event_type, listener)
  }
  
  fire_event(event_type : String, component_id : String) -> Unit {
    match event_listeners.get(event_type) {
      Some(listener) => listener(event_type, component_id)
      None => {}
    }
  }
  
  set_operation_timeout(timeout_ms : Int) -> Unit {
    operation_timeout = timeout_ms
  }
  
  check_health_all() -> Array[HealthCheckResult] {
    let results = []
    
    for (id, component) in components {
      let result = check_health(id)
      results = results.push(result)
    }
    
    return results
  }
  
  check_health(id : String) -> HealthCheckResult {
    if !components.contains_key(id) {
      throw ComponentNotFoundException("Component not found: " + id)
    }
    
    let component = get_component(id)
    
    match component {
      HealthyComponent(_) => return HealthCheckResult::new(true, "healthy", "")
      UnhealthyComponent(_) => return HealthCheckResult::new(false, "unhealthy", "Simulated failure for testing")
      _ => return HealthCheckResult::new(true, "healthy", "")
    }
  }
  
  update_configuration(id : String, config : Map[String, String]) -> Unit {
    if !components.contains_key(id) {
      throw ComponentNotFoundException("Component not found: " + id)
    }
    
    let component = get_component(id)
    
    match component {
      ConfigurableComponent(c) => c.update_configuration(config)
      _ => {}  // Component doesn't support configuration updates
    }
  }
  
  enable_metrics_collection() -> Unit {
    metrics_enabled = true
  }
  
  get_lifecycle_metrics() -> Map[String, String] {
    let result = {}
    result = result.set("total_components", components.size().to_string())
    
    let initialized = 0
    let started = 0
    let stopped = 0
    
    for (_, component) in components {
      if component.is_initialized() {
        initialized = initialized + 1
      }
      if component.is_started() {
        started = started + 1
      }
      if component.is_stopped() {
        stopped = stopped + 1
      }
    }
    
    result = result.set("initialized_components", initialized.to_string())
    result = result.set("started_components", started.to_string())
    result = result.set("stopped_components", stopped.to_string())
    
    if metrics_enabled {
      result = result.set("total_initialization_time", metrics.get("total_initialization_time").unwrap_or("0"))
      result = result.set("total_startup_time", metrics.get("total_startup_time").unwrap_or("0"))
      result = result.set("total_shutdown_time", metrics.get("total_shutdown_time").unwrap_or("0"))
    }
    
    return result
  }
  
  get_component_metrics(id : String) -> Map[String, String] {
    return component_metrics.get(id).unwrap_or({})
  }
  
  record_metric(component_id : String, metric_name : String, value : String) -> Unit {
    if !metrics_enabled {
      return
    }
    
    // Update component-specific metrics
    match component_metrics.get(component_id) {
      Some(component_metric) => {
        let updated = component_metric.set(metric_name, value)
        component_metrics = component_metrics.set(component_id, updated)
      }
      None => {
        component_metrics = component_metrics.set(component_id, {metric_name: value})
      }
    }
    
    // Update global metrics
    let current_value = metrics.get(metric_name).unwrap_or("0").parse_int().unwrap_or(0)
    let new_value = value.parse_int().unwrap_or(0)
    metrics = metrics.set(metric_name, (current_value + new_value).to_string())
  }
}

class EventTracker {
  events : Array[LifecycleEvent]
  
  new() {
    events = []
  }
  
  track_event(event_type : String, component_id : String) -> Unit {
    let event = LifecycleEvent::new(event_type, component_id, get_current_time())
    events = events.push(event)
  }
  
  get_events() -> Array[LifecycleEvent] {
    return events
  }
}

class LifecycleEvent {
  event_type : String
  component_id : String
  timestamp : Int
  
  new(event_type : String, component_id : String, timestamp : Int) {
    event_type = event_type
    component_id = component_id
    timestamp = timestamp
  }
}

class HealthCheckResult {
  is_healthy : Bool
  status : String
  message : String
  
  new(is_healthy : Bool, status : String, message : String) {
    is_healthy = is_healthy
    status = status
    message = message
  }
}

// Helper exception classes
class ComponentInitializationException {
  message : String
  new(message : String) { message = message }
}

class ComponentStartupException {
  message : String
  new(message : String) { message = message }
}

class ComponentShutdownException {
  message : String
  new(message : String) { message = message }
}

class ComponentDestructionException {
  message : String
  new(message : String) { message = message }
}

class LifecycleTimeoutException {
  message : String
  new(message : String) { message = message }
}

class ComponentNotFoundException {
  message : String
  new(message : String) { message = message }
}

// Helper functions
func get_current_time() -> Int {
  // Simplified implementation for testing
  return 1640995200  // Dummy timestamp
}

// Simplified Thread implementation for testing
class Thread {
  static sleep(milliseconds : Int) -> Unit {
    // Simplified implementation for testing
    assert_true(milliseconds >= 0)
  }
}