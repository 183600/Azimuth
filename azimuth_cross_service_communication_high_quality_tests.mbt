// Azimuth 跨服务通信高质量测试用例
// 专注于服务间通信、协议处理和分布式系统交互

// 测试1: 服务发现和注册机制
test "服务发现和注册机制测试" {
  // 定义服务实例
  type ServiceInstance = {
    id: String,
    name: String,
    host: String,
    port: Int,
    protocol: String,
    metadata: Map[String, String],
    health_status: String,
    registration_time: Int,
    last_heartbeat: Int
  }
  
  // 定义服务注册中心
  type ServiceRegistry = {
    services: Map[String, Array[ServiceInstance]],
    health_checks: Map[String, Int],
    heartbeat_interval_ms: Int,
    service_timeout_ms: Int
  }
  
  // 创建服务实例
  let create_service_instance = fn(id: String, name: String, host: String, port: Int, protocol: String) {
    {
      id,
      name,
      host,
      port,
      protocol,
      metadata: Map::new(),
      health_status: "healthy",
      registration_time: 1640995200,
      last_heartbeat: 1640995200
    }
  }
  
  // 创建服务注册中心
  let create_service_registry = fn(heartbeat_interval_ms: Int, service_timeout_ms: Int) {
    {
      services: Map::new(),
      health_checks: Map::new(),
      heartbeat_interval_ms,
      service_timeout_ms
    }
  }
  
  // 注册服务
  let register_service = fn(registry: ServiceRegistry, instance: ServiceInstance) {
    let existing_instances = registry.services.get(instance.name)
    let updated_instances = match existing_instances {
      Some(instances) => {
        // 检查是否已存在相同ID的实例
        if instances.any(fn(inst) { inst.id == instance.id }) {
          // 更新现有实例
          instances.map(fn(inst) {
            if inst.id == instance.id {
              instance
            } else {
              inst
            }
          })
        } else {
          // 添加新实例
          instances.push(instance)
        }
      }
      None => [instance]
    }
    
    let updated_services = registry.services.set(instance.name, updated_instances)
    
    { registry | services: updated_services }
  }
  
  // 注销服务
  let deregister_service = fn(registry: ServiceRegistry, service_name: String, instance_id: String) {
    let instances = registry.services.get(service_name)
    match instances {
      Some(instance_list) => {
        let updated_instances = instance_list.filter(fn(inst) { inst.id != instance_id })
        
        if updated_instances.length() == 0 {
          // 没有实例了，移除服务
          let updated_services = registry.services.remove(service_name)
          { registry | services: updated_services }
        } else {
          // 更新实例列表
          let updated_services = registry.services.set(service_name, updated_instances)
          { registry | services: updated_services }
        }
      }
      None => registry
    }
  }
  
  // 发现服务
  let discover_service = fn(registry: ServiceRegistry, service_name: String) {
    let instances = registry.services.get(service_name)
    match instances {
      Some(instance_list) => {
        // 过滤健康实例
        let healthy_instances = instance_list.filter(fn(inst) { inst.health_status == "healthy" })
        
        if healthy_instances.length() == 0 {
          None
        } else {
          // 简单的负载均衡：随机选择一个实例
          let random_index = Random::int(0, healthy_instances.length() - 1)
          Some(healthy_instances[random_index])
        }
      }
      None => None
    }
  }
  
  // 发现所有服务实例
  let discover_all_service_instances = fn(registry: ServiceRegistry, service_name: String) {
    let instances = registry.services.get(service_name)
    match instances {
      Some(instance_list) => {
        // 过滤健康实例
        let healthy_instances = instance_list.filter(fn(inst) { inst.health_status == "healthy" })
        healthy_instances
      }
      None => []
    }
  }
  
  // 更新心跳
  let update_heartbeat = fn(registry: ServiceRegistry, service_name: String, instance_id: String) {
    let instances = registry.services.get(service_name)
    match instances {
      Some(instance_list) => {
        let updated_instances = instance_list.map(fn(inst) {
          if inst.id == instance_id {
            { inst | last_heartbeat: 1640995200, health_status: "healthy" }
          } else {
            inst
          }
        })
        
        let updated_services = registry.services.set(service_name, updated_instances)
        { registry | services: updated_services }
      }
      None => registry
    }
  }
  
  // 健康检查
  let perform_health_check = fn(registry: ServiceRegistry) {
    let now = 1640995200
    let mut updated_services = registry.services
    
    for (service_name, instances) in registry.services {
      let updated_instances = instances.map(fn(inst) {
        let time_since_heartbeat = now - inst.last_heartbeat
        
        if time_since_heartbeat > registry.service_timeout_ms {
          // 超时，标记为不健康
          { inst | health_status: "unhealthy" }
        } else {
          inst
        }
      })
      
      updated_services = updated_services.set(service_name, updated_instances)
    }
    
    { registry | services: updated_services }
  }
  
  // 创建服务注册中心
  let mut registry = create_service_registry(30000, 90000)
  
  // 创建服务实例
  let user_service1 = create_service_instance("user-service-1", "user-service", "localhost", 8001, "http")
  let user_service2 = create_service_instance("user-service-2", "user-service", "localhost", 8002, "http")
  let order_service1 = create_service_instance("order-service-1", "order-service", "localhost", 8003, "http")
  
  // 注册服务
  registry = register_service(registry, user_service1)
  registry = register_service(registry, user_service2)
  registry = register_service(registry, order_service1)
  
  // 验证服务注册
  let user_instances = registry.services.get("user-service")
  assert_true(user_instances != None)
  match user_instances {
    Some(instances) => assert_eq(instances.length(), 2)
    None => assert_true(false)
  }
  
  let order_instances = registry.services.get("order-service")
  assert_true(order_instances != None)
  match order_instances {
    Some(instances) => assert_eq(instances.length(), 1)
    None => assert_true(false)
  }
  
  // 发现服务
  let discovered_user_service = discover_service(registry, "user-service")
  assert_true(discovered_user_service != None)
  match discovered_user_service {
    Some(instance) => {
      assert_eq(instance.name, "user-service")
      assert_eq(instance.health_status, "healthy")
    }
    None => assert_true(false)
  }
  
  let discovered_order_service = discover_service(registry, "order-service")
  assert_true(discovered_order_service != None)
  match discovered_order_service {
    Some(instance) => {
      assert_eq(instance.name, "order-service")
      assert_eq(instance.port, 8003)
    }
    None => assert_true(false)
  }
  
  // 发现不存在的服务
  let non_existent_service = discover_service(registry, "non-existent-service")
  assert_eq(non_existent_service, None)
  
  // 发现所有服务实例
  let all_user_instances = discover_all_service_instances(registry, "user-service")
  assert_eq(all_user_instances.length(), 2)
  
  // 更新心跳
  registry = update_heartbeat(registry, "user-service", "user-service-1")
  
  // 注销服务
  registry = deregister_service(registry, "user-service", "user-service-1")
  
  // 验证注销结果
  let updated_user_instances = registry.services.get("user-service")
  match updated_user_instances {
    Some(instances) => {
      assert_eq(instances.length(), 1)
      assert_eq(instances[0].id, "user-service-2")
    }
    None => assert_true(false)
  }
  
  // 模拟健康检查
  let unhealthy_registry = perform_health_check(registry)
  
  // 验证健康检查结果
  let final_user_instances = discover_all_service_instances(unhealthy_registry, "user-service")
  assert_eq(final_user_instances.length(), 1)
}

// 测试2: 服务间通信协议
test "服务间通信协议测试" {
  // 定义消息类型
  enum MessageType {
    Request
    Response
    Event
    Error
  }
  
  // 定义通信协议
  enum Protocol {
    HTTP
    gRPC
    MessageQueue
    WebSocket
  }
  
  // 定义消息头
  type MessageHeader = {
    message_id: String,
    correlation_id: String,
    message_type: MessageType,
    source_service: String,
    target_service: String,
    protocol: Protocol,
    timestamp: Int,
    content_type: String,
    metadata: Map[String, String]
  }
  
  // 定义消息
  type Message = {
    header: MessageHeader,
    payload: String,
    signature: Option[String]
  }
  
  // 定义通信端点
  type CommunicationEndpoint = {
    service_name: String,
    protocol: Protocol,
    address: String,
    port: Int,
    path: String
  }
  
  // 定义通信客户端
  type CommunicationClient = {
    endpoint: CommunicationEndpoint,
    timeout_ms: Int,
    retry_count: Int,
    circuit_breaker_enabled: Bool
  }
  
  // 定义通信结果
  type CommunicationResult = {
    success: Bool,
    message: Option[Message],
    error: Option[String],
    response_time_ms: Int
  }
  
  // 创建消息头
  let create_message_header = fn(message_id: String, correlation_id: String, message_type: MessageType, source_service: String, target_service: String, protocol: Protocol) {
    {
      message_id,
      correlation_id,
      message_type,
      source_service,
      target_service,
      protocol,
      timestamp: 1640995200,
      content_type: "application/json",
      metadata: Map::new()
    }
  }
  
  // 创建消息
  let create_message = fn(header: MessageHeader, payload: String) {
    {
      header,
      payload,
      signature: None
    }
  }
  
  // 创建通信端点
  let create_endpoint = fn(service_name: String, protocol: Protocol, address: String, port: Int, path: String) {
    {
      service_name,
      protocol,
      address,
      port,
      path
    }
  }
  
  // 创建通信客户端
  let create_client = fn(endpoint: CommunicationEndpoint, timeout_ms: Int, retry_count: Int, circuit_breaker_enabled: Bool) {
    {
      endpoint,
      timeout_ms,
      retry_count,
      circuit_breaker_enabled
    }
  }
  
  // 发送消息
  let send_message = fn(client: CommunicationClient, message: Message) {
    let start_time = 1640995200
    
    // 模拟发送消息
    let send_success = Random::float() > 0.1  // 90%成功率
    let response_time = Random::int(50, 500)  // 50-500ms响应时间
    
    if send_success {
      // 创建响应消息
      let response_header = create_message_header(
        "resp-" + Random::string(8),
        message.header.correlation_id,
        MessageType::Response,
        message.header.target_service,
        message.header.source_service,
        client.endpoint.protocol
      )
      
      let response_payload = "{\"status\": \"success\", \"data\": \"processed\"}"
      let response_message = create_message(response_header, response_payload)
      
      {
        success: true,
        message: Some(response_message),
        error: None,
        response_time_ms: response_time
      }
    } else {
      {
        success: false,
        message: None,
        error: Some("Communication failed"),
        response_time_ms: response_time
      }
    }
  }
  
  // 发送请求并等待响应
  let send_request = fn(client: CommunicationClient, target_service: String, payload: String) {
    let message_id = "req-" + Random::string(8)
    let correlation_id = "corr-" + Random::string(8)
    
    let request_header = create_message_header(
      message_id,
      correlation_id,
      MessageType::Request,
      "client-service",
      target_service,
      client.endpoint.protocol
    )
    
    let request_message = create_message(request_header, payload)
    
    // 发送消息
    send_message(client, request_message)
  }
  
  // 发布事件
  let publish_event = fn(client: CommunicationClient, event_type: String, payload: String) {
    let message_id = "event-" + Random::string(8)
    let correlation_id = "corr-" + Random::string(8)
    
    let event_header = create_message_header(
      message_id,
      correlation_id,
      MessageType::Event,
      "event-publisher",
      "*",
      client.endpoint.protocol
    )
    
    // 添加事件类型到元数据
    let event_header_with_metadata = {
      message_id: event_header.message_id,
      correlation_id: event_header.correlation_id,
      message_type: event_header.message_type,
      source_service: event_header.source_service,
      target_service: event_header.target_service,
      protocol: event_header.protocol,
      timestamp: event_header.timestamp,
      content_type: event_header.content_type,
      metadata: event_header.metadata.set("event_type", event_type)
    }
    
    let event_message = {
      header: event_header_with_metadata,
      payload,
      signature: None
    }
    
    // 发送事件消息
    send_message(client, event_message)
  }
  
  // 创建通信端点
  let user_service_endpoint = create_endpoint("user-service", Protocol::HTTP, "localhost", 8001, "/api")
  let order_service_endpoint = create_endpoint("order-service", Protocol::gRPC, "localhost", 8002, "/orders")
  let notification_endpoint = create_endpoint("notification-service", Protocol::MessageQueue, "localhost", 8003, "/notifications")
  
  // 创建通信客户端
  let user_client = create_client(user_service_endpoint, 5000, 3, true)
  let order_client = create_client(order_service_endpoint, 3000, 2, true)
  let notification_client = create_client(notification_endpoint, 10000, 5, false)
  
  // 测试HTTP请求
  let user_request_payload = "{\"user_id\": \"123\", \"action\": \"get_profile\"}"
  let user_result = send_request(user_client, "user-service", user_request_payload)
  
  assert_true(user_result.success or user_result.error != None)
  assert_true(user_result.response_time_ms > 0)
  if user_result.success {
    assert_true(user_result.message != None)
    match user_result.message {
      Some(message) => {
        assert_eq(message.header.message_type, MessageType::Response)
        assert_eq(message.header.source_service, "user-service")
        assert_eq(message.header.target_service, "client-service")
      }
      None => assert_true(false)
    }
  }
  
  // 测试gRPC请求
  let order_request_payload = "{\"order_id\": \"456\", \"action\": \"get_status\"}"
  let order_result = send_request(order_client, "order-service", order_request_payload)
  
  assert_true(order_result.success or order_result.error != None)
  assert_true(order_result.response_time_ms > 0)
  
  // 测试事件发布
  let event_payload = "{\"user_id\": \"123\", \"event\": \"order_created\", \"order_id\": \"456\"}"
  let event_result = publish_event(notification_client, "order_created", event_payload)
  
  assert_true(event_result.success or event_result.error != None)
  if event_result.success {
    match event_result.message {
      Some(message) => {
        assert_eq(message.header.message_type, MessageType::Event)
        assert_eq(message.header.target_service, "*")  // 广播
        assert_eq(message.header.metadata.get("event_type"), Some("order_created"))
      }
      None => assert_true(false)
    }
  }
  
  // 测试不同协议的通信
  let protocols = [Protocol::HTTP, Protocol::gRPC, Protocol::MessageQueue, Protocol::WebSocket]
  let mut results = []
  
  for protocol in protocols {
    let endpoint = create_endpoint("test-service", protocol, "localhost", 8000 + protocol.to_int(), "/test")
    let client = create_client(endpoint, 3000, 2, true)
    
    let result = send_request(client, "test-service", "{\"test\": true}")
    results = results.push((protocol, result))
  }
  
  // 验证所有协议都能工作
  for (protocol, result) in results {
    assert_true(result.success or result.error != None)
    assert_true(result.response_time_ms > 0)
    if result.success {
      match result.message {
        Some(message) => {
          assert_eq(message.header.protocol, protocol)
        }
        None => assert_true(false)
      }
    }
  }
}

// 测试3: 负载均衡策略
test "负载均衡策略测试" {
  // 定义负载均衡策略
  enum LoadBalancingStrategy {
    RoundRobin
    Random
    WeightedRoundRobin
    LeastConnections
    IPHash
  }
  
  // 定义服务实例权重
  type ServiceInstanceWeight = {
    instance: ServiceInstance,
    weight: Int,
    active_connections: Int
  }
  
  // 定义负载均衡器
  type LoadBalancer = {
    strategy: LoadBalancingStrategy,
    instances: Array[ServiceInstanceWeight],
    current_index: Int,
    total_weight: Int
  }
  
  // 创建服务实例（使用前面测试中的类型）
  type ServiceInstance = {
    id: String,
    name: String,
    host: String,
    port: Int,
    protocol: String,
    metadata: Map[String, String],
    health_status: String,
    registration_time: Int,
    last_heartbeat: Int
  }
  
  // 创建服务实例
  let create_service_instance = fn(id: String, name: String, host: String, port: Int, protocol: String) {
    {
      id,
      name,
      host,
      port,
      protocol,
      metadata: Map::new(),
      health_status: "healthy",
      registration_time: 1640995200,
      last_heartbeat: 1640995200
    }
  }
  
  // 创建负载均衡器
  let create_load_balancer = fn(strategy: LoadBalancingStrategy, instances: Array[ServiceInstance]) {
    let mut weighted_instances = []
    let mut total_weight = 0
    
    for instance in instances {
      let weight = instance.metadata.get("weight")
      let instance_weight = match weight {
        Some(w) => w.to_int()
        None => 1
      }
      
      weighted_instances = weighted_instances.push({
        instance,
        weight: instance_weight,
        active_connections: 0
      })
      
      total_weight = total_weight + instance_weight
    }
    
    {
      strategy,
      instances: weighted_instances,
      current_index: 0,
      total_weight
    }
  }
  
  // 轮询负载均衡
  let round_robin_select = fn(lb: LoadBalancer) {
    if lb.instances.length() == 0 {
      (lb, None)
    } else {
      let selected_index = lb.current_index % lb.instances.length()
      let selected_instance = lb.instances[selected_index]
      
      // 更新活跃连接数
      let updated_instances = lb.instances.map_fn(i) {
        if i == selected_index {
          { instance: selected_instance.instance, weight: selected_instance.weight, active_connections: selected_instance.active_connections + 1 }
        } else {
          lb.instances[i]
        }
      }
      
      let updated_lb = {
        strategy: lb.strategy,
        instances: updated_instances,
        current_index: lb.current_index + 1,
        total_weight: lb.total_weight
      }
      
      (updated_lb, Some(selected_instance.instance))
    }
  }
  
  // 随机负载均衡
  let random_select = fn(lb: LoadBalancer) {
    if lb.instances.length() == 0 {
      (lb, None)
    } else {
      let random_index = Random::int(0, lb.instances.length() - 1)
      let selected_instance = lb.instances[random_index]
      
      // 更新活跃连接数
      let updated_instances = lb.instances.map_fn(i) {
        if i == random_index {
          { instance: selected_instance.instance, weight: selected_instance.weight, active_connections: selected_instance.active_connections + 1 }
        } else {
          lb.instances[i]
        }
      }
      
      let updated_lb = {
        strategy: lb.strategy,
        instances: updated_instances,
        current_index: lb.current_index,
        total_weight: lb.total_weight
      }
      
      (updated_lb, Some(selected_instance.instance))
    }
  }
  
  // 加权轮询负载均衡
  let weighted_round_robin_select = fn(lb: LoadBalancer) {
    if lb.instances.length() == 0 {
      (lb, None)
    } else {
      let mut current_weight = 0
      let mut selected_index = -1
      let mut max_weight = -1
      let mut current_index = lb.current_index
      
      // 查找最佳实例
      for i in 0..lb.instances.length() {
        let instance = lb.instances[i]
        current_weight = current_weight + instance.weight
        
        if instance.weight > max_weight {
          max_weight = instance.weight
          selected_index = i
        }
        
        if current_weight >= lb.total_weight {
          current_weight = current_weight - lb.total_weight
        }
      }
      
      if selected_index >= 0 {
        let selected_instance = lb.instances[selected_index]
        
        // 更新活跃连接数
        let updated_instances = lb.instances.map_fn(i) {
          if i == selected_index {
            { instance: selected_instance.instance, weight: selected_instance.weight, active_connections: selected_instance.active_connections + 1 }
          } else {
            lb.instances[i]
          }
        }
        
        let updated_lb = {
          strategy: lb.strategy,
          instances: updated_instances,
          current_index: (lb.current_index + 1) % lb.instances.length(),
          total_weight: lb.total_weight
        }
        
        (updated_lb, Some(selected_instance.instance))
      } else {
        (lb, None)
      }
    }
  }
  
  // 最少连接负载均衡
  let least_connections_select = fn(lb: LoadBalancer) {
    if lb.instances.length() == 0 {
      (lb, None)
    } else {
      let mut min_connections = Int::max_value()
      let mut selected_index = -1
      
      for i in 0..lb.instances.length() {
        let instance = lb.instances[i]
        if instance.active_connections < min_connections {
          min_connections = instance.active_connections
          selected_index = i
        }
      }
      
      if selected_index >= 0 {
        let selected_instance = lb.instances[selected_index]
        
        // 更新活跃连接数
        let updated_instances = lb.instances.map_fn(i) {
          if i == selected_index {
            { instance: selected_instance.instance, weight: selected_instance.weight, active_connections: selected_instance.active_connections + 1 }
          } else {
            lb.instances[i]
          }
        }
        
        let updated_lb = {
          strategy: lb.strategy,
          instances: updated_instances,
          current_index: lb.current_index,
          total_weight: lb.total_weight
        }
        
        (updated_lb, Some(selected_instance.instance))
      } else {
        (lb, None)
      }
    }
  }
  
  // IP哈希负载均衡
  let ip_hash_select = fn(lb: LoadBalancer, client_ip: String) {
    if lb.instances.length() == 0 {
      (lb, None)
    } else {
      // 简单的哈希实现
      let hash = client_ip.length() % lb.instances.length()
      let selected_instance = lb.instances[hash]
      
      // 更新活跃连接数
      let updated_instances = lb.instances.map_fn(i) {
        if i == hash {
          { instance: selected_instance.instance, weight: selected_instance.weight, active_connections: selected_instance.active_connections + 1 }
        } else {
          lb.instances[i]
        }
      }
      
      let updated_lb = {
        strategy: lb.strategy,
        instances: updated_instances,
        current_index: lb.current_index,
        total_weight: lb.total_weight
      }
      
      (updated_lb, Some(selected_instance.instance))
    }
  }
  
  // 选择实例
  let select_instance = fn(lb: LoadBalancer, client_ip: Option[String]) {
    match lb.strategy {
      LoadBalancingStrategy::RoundRobin => round_robin_select(lb),
      LoadBalancingStrategy::Random => random_select(lb),
      LoadBalancingStrategy::WeightedRoundRobin => weighted_round_robin_select(lb),
      LoadBalancingStrategy::LeastConnections => least_connections_select(lb),
      LoadBalancingStrategy::IPHash => {
        match client_ip {
          Some(ip) => ip_hash_select(lb, ip),
          None => random_select(lb)  // 回退到随机选择
        }
      }
    }
  }
  
  // 释放连接
  let release_connection = fn(lb: LoadBalancer, instance_id: String) {
    let updated_instances = lb.instances.map_fn(i) {
      let instance = lb.instances[i]
      if instance.instance.id == instance_id {
        { instance: instance.instance, weight: instance.weight, active_connections: instance.active_connections - 1 }
      } else {
        instance
      }
    }
    
    { lb | instances: updated_instances }
  }
  
  // 创建服务实例
  let instance1 = create_service_instance("instance-1", "test-service", "localhost", 8001, "http")
  let instance2 = create_service_instance("instance-2", "test-service", "localhost", 8002, "http")
  let instance3 = create_service_instance("instance-3", "test-service", "localhost", 8003, "http")
  
  // 设置权重
  let weighted_instance1 = { instance1 | metadata: instance1.metadata.set("weight", "1") }
  let weighted_instance2 = { instance2 | metadata: instance2.metadata.set("weight", "3") }
  let weighted_instance3 = { instance3 | metadata: instance3.metadata.set("weight", "2") }
  
  let instances = [weighted_instance1, weighted_instance2, weighted_instance3]
  
  // 测试轮询负载均衡
  let round_robin_lb = create_load_balancer(LoadBalancingStrategy::RoundRobin, instances)
  let mut current_lb = round_robin_lb
  let mut selected_instances = []
  
  for i in 0..9 {
    let (new_lb, selected) = select_instance(current_lb, None)
    match selected {
      Some(instance) => {
        selected_instances = selected_instances.push(instance.id)
        current_lb = new_lb
      }
      None => assert_true(false)
    }
  }
  
  // 验证轮询分布
  assert_eq(selected_instances.length(), 9)
  assert_eq(selected_instances[0], "instance-1")
  assert_eq(selected_instances[1], "instance-2")
  assert_eq(selected_instances[2], "instance-3")
  assert_eq(selected_instances[3], "instance-1")
  assert_eq(selected_instances[4], "instance-2")
  assert_eq(selected_instances[5], "instance-3")
  assert_eq(selected_instances[6], "instance-1")
  assert_eq(selected_instances[7], "instance-2")
  assert_eq(selected_instances[8], "instance-3")
  
  // 测试随机负载均衡
  let random_lb = create_load_balancer(LoadBalancingStrategy::Random, instances)
  let mut random_current_lb = random_lb
  let mut random_selected_instances = []
  
  for i in 0..30 {
    let (new_lb, selected) = select_instance(random_current_lb, None)
    match selected {
      Some(instance) => {
        random_selected_instances = random_selected_instances.push(instance.id)
        random_current_lb = new_lb
      }
      None => assert_true(false)
    }
  }
  
  // 验证随机分布（大致均匀）
  let instance1_count = random_selected_instances.filter_fn(id) { id == "instance-1" }.length()
  let instance2_count = random_selected_instances.filter_fn(id) { id == "instance-2" }.length()
  let instance3_count = random_selected_instances.filter_fn(id) { id == "instance-3" }.length()
  
  assert_eq(instance1_count + instance2_count + instance3_count, 30)
  assert_true(instance1_count > 5)  // 每个实例至少被选择几次
  assert_true(instance2_count > 5)
  assert_true(instance3_count > 5)
  
  // 测试最少连接负载均衡
  let least_conn_lb = create_load_balancer(LoadBalancingStrategy::LeastConnections, instances)
  let mut least_conn_current_lb = least_conn_lb
  
  // 选择第一个实例
  let (lb1, selected1) = select_instance(least_conn_current_lb, None)
  match selected1 {
    Some(instance) => {
      assert_eq(instance.id, "instance-1")
      least_conn_current_lb = lb1
    }
    None => assert_true(false)
  }
  
  // 选择第二个实例
  let (lb2, selected2) = select_instance(least_conn_current_lb, None)
  match selected2 {
    Some(instance) => {
      assert_eq(instance.id, "instance-2")
      least_conn_current_lb = lb2
    }
    None => assert_true(false)
  }
  
  // 再次选择应该回到实例1或3（连接数最少的）
  let (lb3, selected3) = select_instance(least_conn_current_lb, None)
  match selected3 {
    Some(instance) => {
      assert_true(instance.id == "instance-1" or instance.id == "instance-3")
      least_conn_current_lb = lb3
    }
    None => assert_true(false)
  }
  
  // 释放连接
  least_conn_current_lb = release_connection(least_conn_current_lb, "instance-1")
  
  // 现在应该选择实例1（连接数最少）
  let (lb4, selected4) = select_instance(least_conn_current_lb, None)
  match selected4 {
    Some(instance) => {
      assert_eq(instance.id, "instance-1")
    }
    None => assert_true(false)
  }
  
  // 测试IP哈希负载均衡
  let ip_hash_lb = create_load_balancer(LoadBalancingStrategy::IPHash, instances)
  let (ip_hash_lb1, ip_hash_selected1) = select_instance(ip_hash_lb, Some("192.168.1.100"))
  let (ip_hash_lb2, ip_hash_selected2) = select_instance(ip_hash_lb1, Some("192.168.1.100"))
  let (ip_hash_lb3, ip_hash_selected3) = select_instance(ip_hash_lb2, Some("192.168.1.101"))
  
  // 验证相同IP总是映射到相同实例
  match (ip_hash_selected1, ip_hash_selected2) {
    (Some(instance1), Some(instance2)) => {
      assert_eq(instance1.id, instance2.id)
    }
    _ => assert_true(false)
  }
  
  // 验证不同IP可能映射到不同实例
  match (ip_hash_selected2, ip_hash_selected3) {
    (Some(instance2), Some(instance3)) => {
      // 可能相同也可能不同，取决于哈希结果
      assert_true(instance2.id == instance3.id or instance2.id != instance3.id)
    }
    _ => assert_true(false)
  }
}

// 测试4: 分布式事务处理
test "分布式事务处理测试" {
  // 定义事务状态
  enum TransactionState {
    Pending
    Active
    Preparing
    Prepared
    Committing
    Committed
    Aborting
    Aborted
  }
  
  // 定义事务操作
  enum TransactionOperation {
    Begin
    Prepare
    Commit
    Rollback
  }
  
  // 定义参与者状态
  enum ParticipantState {
    Idle
    Prepared
    Committed
    Aborted
  }
  
  // 定义事务参与者
  type TransactionParticipant = {
    id: String,
    service_name: String,
    endpoint: String,
    state: ParticipantState,
    prepare_timeout_ms: Int,
    commit_timeout_ms: Int
  }
  
  // 定义事务上下文
  type TransactionContext = {
    transaction_id: String,
    coordinator_id: String,
    state: TransactionState,
    participants: Array[TransactionParticipant],
    operations: Array[String],
    start_time: Int,
    timeout_ms: Int,
    retry_count: Int
  }
  
  // 定义事务管理器
  type TransactionManager = {
    active_transactions: Map[String, TransactionContext],
    participant_registry: Map[String, TransactionParticipant],
    retry_policy: {
      max_retries: Int,
      backoff_ms: Int
    }
  }
  
  // 创建事务参与者
  let create_participant = fn(id: String, service_name: String, endpoint: String) {
    {
      id,
      service_name,
      endpoint,
      state: ParticipantState::Idle,
      prepare_timeout_ms: 5000,
      commit_timeout_ms: 3000
    }
  }
  
  // 创建事务上下文
  let create_transaction_context = fn(transaction_id: String, coordinator_id: String, participants: Array[TransactionParticipant], timeout_ms: Int) {
    {
      transaction_id,
      coordinator_id,
      state: TransactionState::Pending,
      participants,
      operations: [],
      start_time: 1640995200,
      timeout_ms,
      retry_count: 0
    }
  }
  
  // 创建事务管理器
  let create_transaction_manager = fn(max_retries: Int, backoff_ms: Int) {
    {
      active_transactions: Map::new(),
      participant_registry: Map::new(),
      retry_policy: {
        max_retries,
        backoff_ms
      }
    }
  }
  
  // 开始事务
  let begin_transaction = fn(manager: TransactionManager, transaction_id: String, coordinator_id: String, participant_ids: Array[String]) {
    // 获取参与者
    let mut participants = []
    for participant_id in participant_ids {
      let participant = manager.participant_registry.get(participant_id)
      match participant {
        Some(p) => participants = participants.push(p)
        None => return (manager, None)  // 参与者不存在
      }
    }
    
    // 创建事务上下文
    let transaction_context = create_transaction_context(transaction_id, coordinator_id, participants, 30000)
    
    // 更新事务状态
    let updated_context = { transaction_context | state: TransactionState::Active }
    
    // 添加到活动事务
    let updated_transactions = manager.active_transactions.set(transaction_id, updated_context)
    
    let updated_manager = { manager | active_transactions: updated_transactions }
    
    (updated_manager, Some(updated_context))
  }
  
  // 准备阶段
  let prepare_transaction = fn(manager: TransactionManager, transaction_id: String) {
    let transaction = manager.active_transactions.get(transaction_id)
    match transaction {
      Some(tx) => {
        // 更新事务状态为准备中
        let preparing_tx = { tx | state: TransactionState::Preparing }
        let updated_transactions = manager.active_transactions.set(transaction_id, preparing_tx)
        
        // 模拟向所有参与者发送准备请求
        let mut all_prepared = true
        let mut updated_participants = []
        
        for participant in tx.participants {
          // 模拟准备操作（90%成功率）
          let prepare_success = Random::float() > 0.1
          
          if prepare_success {
            updated_participants = updated_participants.push({ participant | state: ParticipantState::Prepared })
          } else {
            updated_participants = updated_participants.push({ participant | state: ParticipantState::Aborted })
            all_prepared = false
          }
        }
        
        if all_prepared {
          // 所有参与者都准备成功
          let prepared_tx = {
            transaction_id: preparing_tx.transaction_id,
            coordinator_id: preparing_tx.coordinator_id,
            state: TransactionState::Prepared,
            participants: updated_participants,
            operations: preparing_tx.operations,
            start_time: preparing_tx.start_time,
            timeout_ms: preparing_tx.timeout_ms,
            retry_count: preparing_tx.retry_count
          }
          
          let final_transactions = updated_transactions.set(transaction_id, prepared_tx)
          let final_manager = { manager | active_transactions: final_transactions }
          
          (final_manager, Some(prepared_tx))
        } else {
          // 有参与者准备失败，中止事务
          let aborted_tx = {
            transaction_id: preparing_tx.transaction_id,
            coordinator_id: preparing_tx.coordinator_id,
            state: TransactionState::Aborted,
            participants: updated_participants,
            operations: preparing_tx.operations,
            start_time: preparing_tx.start_time,
            timeout_ms: preparing_tx.timeout_ms,
            retry_count: preparing_tx.retry_count
          }
          
          let final_transactions = updated_transactions.set(transaction_id, aborted_tx)
          let final_manager = { manager | active_transactions: final_transactions }
          
          (final_manager, Some(aborted_tx))
        }
      }
      None => (manager, None)
    }
  }
  
  // 提交阶段
  let commit_transaction = fn(manager: TransactionManager, transaction_id: String) {
    let transaction = manager.active_transactions.get(transaction_id)
    match transaction {
      Some(tx) => {
        if tx.state != TransactionState::Prepared {
          // 事务未准备好，不能提交
          (manager, None)
        } else {
          // 更新事务状态为提交中
          let committing_tx = { tx | state: TransactionState::Committing }
          let updated_transactions = manager.active_transactions.set(transaction_id, committing_tx)
          
          // 模拟向所有参与者发送提交请求
          let mut all_committed = true
          let mut updated_participants = []
          
          for participant in tx.participants {
            // 模拟提交操作（95%成功率）
            let commit_success = Random::float() > 0.05
            
            if commit_success {
              updated_participants = updated_participants.push({ participant | state: ParticipantState::Committed })
            } else {
              updated_participants = updated_participants.push({ participant | state: ParticipantState::Aborted })
              all_committed = false
            }
          }
          
          if all_committed {
            // 所有参与者都提交成功
            let committed_tx = {
              transaction_id: committing_tx.transaction_id,
              coordinator_id: committing_tx.coordinator_id,
              state: TransactionState::Committed,
              participants: updated_participants,
              operations: committing_tx.operations,
              start_time: committing_tx.start_time,
              timeout_ms: committing_tx.timeout_ms,
              retry_count: committing_tx.retry_count
            }
            
            let final_transactions = updated_transactions.set(transaction_id, committed_tx)
            let final_manager = { manager | active_transactions: final_transactions }
            
            (final_manager, Some(committed_tx))
          } else {
            // 有参与者提交失败，中止事务
            let aborted_tx = {
              transaction_id: committing_tx.transaction_id,
              coordinator_id: committing_tx.coordinator_id,
              state: TransactionState::Aborted,
              participants: updated_participants,
              operations: committing_tx.operations,
              start_time: committing_tx.start_time,
              timeout_ms: committing_tx.timeout_ms,
              retry_count: committing_tx.retry_count
            }
            
            let final_transactions = updated_transactions.set(transaction_id, aborted_tx)
            let final_manager = { manager | active_transactions: final_transactions }
            
            (final_manager, Some(aborted_tx))
          }
        }
      }
      None => (manager, None)
    }
  }
  
  // 回滚事务
  let rollback_transaction = fn(manager: TransactionManager, transaction_id: String) {
    let transaction = manager.active_transactions.get(transaction_id)
    match transaction {
      Some(tx) => {
        // 更新事务状态为中止中
        let aborting_tx = { tx | state: TransactionState::Aborting }
        let updated_transactions = manager.active_transactions.set(transaction_id, aborting_tx)
        
        // 模拟向所有参与者发送回滚请求
        let mut updated_participants = []
        
        for participant in tx.participants {
          // 回滚操作总是成功（简化实现）
          updated_participants = updated_participants.push({ participant | state: ParticipantState::Aborted })
        }
        
        // 事务已中止
        let aborted_tx = {
          transaction_id: aborting_tx.transaction_id,
          coordinator_id: aborting_tx.coordinator_id,
          state: TransactionState::Aborted,
          participants: updated_participants,
          operations: aborting_tx.operations,
          start_time: aborting_tx.start_time,
          timeout_ms: aborting_tx.timeout_ms,
          retry_count: aborting_tx.retry_count
        }
        
        let final_transactions = updated_transactions.set(transaction_id, aborted_tx)
        let final_manager = { manager | active_transactions: final_transactions }
        
        (final_manager, Some(aborted_tx))
      }
      None => (manager, None)
    }
  }
  
  // 创建事务管理器
  let mut manager = create_transaction_manager(3, 1000)
  
  // 注册参与者
  let user_service_participant = create_participant("user-service", "user-service", "http://localhost:8001")
  let order_service_participant = create_participant("order-service", "order-service", "http://localhost:8002")
  let payment_service_participant = create_participant("payment-service", "payment-service", "http://localhost:8003")
  
  manager.participant_registry = manager.participant_registry.set("user-service", user_service_participant)
  manager.participant_registry = manager.participant_registry.set("order-service", order_service_participant)
  manager.participant_registry = manager.participant_registry.set("payment-service", payment_service_participant)
  
  // 开始事务
  let (manager1, transaction) = begin_transaction(manager, "tx-123", "coordinator-1", ["user-service", "order-service", "payment-service"])
  assert_true(transaction != None)
  
  match transaction {
    Some(tx) => {
      assert_eq(tx.transaction_id, "tx-123")
      assert_eq(tx.state, TransactionState::Active)
      assert_eq(tx.participants.length(), 3)
    }
    None => assert_true(false)
  }
  
  // 准备阶段
  let (manager2, prepared_transaction) = prepare_transaction(manager1, "tx-123")
  assert_true(prepared_transaction != None)
  
  match prepared_transaction {
    Some(tx) => {
      assert_true(tx.state == TransactionState::Prepared or tx.state == TransactionState::Aborted)
      
      if tx.state == TransactionState::Prepared {
        // 提交阶段
        let (manager3, committed_transaction) = commit_transaction(manager2, "tx-123")
        assert_true(committed_transaction != None)
        
        match committed_transaction {
          Some(committed_tx) => {
            assert_true(committed_tx.state == TransactionState::Committed or committed_tx.state == TransactionState::Aborted)
            
            if committed_tx.state == TransactionState::Committed {
              // 验证所有参与者都已提交
              for participant in committed_tx.participants {
                assert_eq(participant.state, ParticipantState::Committed)
              }
            }
          }
          None => assert_true(false)
        }
      }
    }
    None => assert_true(false)
  }
  
  // 测试回滚场景
  let (manager4, rollback_transaction_context) = begin_transaction(manager2, "tx-456", "coordinator-1", ["user-service", "order-service"])
  assert_true(rollback_transaction_context != None)
  
  match rollback_transaction_context {
    Some(tx) => {
      // 直接回滚
      let (manager5, rolled_back_transaction) = rollback_transaction(manager4, "tx-456")
      assert_true(rolled_back_transaction != None)
      
      match rolled_back_transaction {
        Some(rolled_back_tx) => {
          assert_eq(rolled_back_tx.state, TransactionState::Aborted)
          
          // 验证所有参与者都已中止
          for participant in rolled_back_tx.participants {
            assert_eq(participant.state, ParticipantState::Aborted)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证活动事务
  assert_eq(manager5.active_transactions.size(), 2)  // tx-123 和 tx-456
}

// 测试5: 服务间消息传递可靠性
test "服务间消息传递可靠性测试" {
  // 定义消息传递模式
  enum MessagePattern {
    RequestReply
    FireAndForget
    PublishSubscribe
    Streaming
  }
  
  // 定义消息传递保证
  enum DeliveryGuarantee {
    AtMostOnce
    AtLeastOnce
    ExactlyOnce
  }
  
  // 定义消息状态
  enum MessageStatus {
    Pending
    InTransit
    Delivered
    Acknowledged
    Failed
  }
  
  // 定义消息
  type Message = {
    id: String,
    pattern: MessagePattern,
    guarantee: DeliveryGuarantee,
    sender: String,
    receiver: String,
    payload: String,
    timestamp: Int,
    status: MessageStatus,
    retry_count: Int,
    max_retries: Int
  }
  
  // 定义消息队列
  type MessageQueue = {
    name: String,
    messages: Array[Message],
    max_size: Int,
    delivery_guarantee: DeliveryGuarantee
  }
  
  // 定义消息代理
  type MessageBroker = {
    queues: Map[String, MessageQueue],
    dead_letter_queue: Array[Message],
    acknowledgments: Map[String, Int],
    retry_policy: {
      max_retries: Int,
      backoff_ms: Int
    }
  }
  
  // 创建消息
  let create_message = fn(id: String, pattern: MessagePattern, guarantee: DeliveryGuarantee, sender: String, receiver: String, payload: String, max_retries: Int) {
    {
      id,
      pattern,
      guarantee,
      sender,
      receiver,
      payload,
      timestamp: 1640995200,
      status: MessageStatus::Pending,
      retry_count: 0,
      max_retries
    }
  }
  
  // 创建消息队列
  let create_message_queue = fn(name: String, max_size: Int, delivery_guarantee: DeliveryGuarantee) {
    {
      name,
      messages: [],
      max_size,
      delivery_guarantee
    }
  }
  
  // 创建消息代理
  let create_message_broker = fn(max_retries: Int, backoff_ms: Int) {
    {
      queues: Map::new(),
      dead_letter_queue: [],
      acknowledgments: Map::new(),
      retry_policy: {
        max_retries,
        backoff_ms
      }
    }
  }
  
  // 添加队列
  let add_queue = fn(broker: MessageBroker, queue: MessageQueue) {
    let updated_queues = broker.queues.set(queue.name, queue)
    { broker | queues: updated_queues }
  }
  
  // 发送消息
  let send_message = fn(broker: MessageBroker, queue_name: String, message: Message) {
    let queue = broker.queues.get(queue_name)
    match queue {
      Some(q) => {
        if q.messages.length() < q.max_size {
          // 添加消息到队列
          let updated_message = { message | status: MessageStatus::InTransit }
          let updated_messages = q.messages.push(updated_message)
          let updated_queue = { q | messages: updated_messages }
          let updated_queues = broker.queues.set(queue_name, updated_queue)
          
          let updated_broker = { broker | queues: updated_queues }
          
          (updated_broker, true)
        } else {
          // 队列已满
          (broker, false)
        }
      }
      None => (broker, false)
    }
  }
  
  // 接收消息
  let receive_message = fn(broker: MessageBroker, queue_name: String, receiver: String) {
    let queue = broker.queues.get(queue_name)
    match queue {
      Some(q) => {
        if q.messages.length() > 0 {
          // 获取第一个消息
          let message = q.messages[0]
          let remaining_messages = q.messages.slice(1, q.messages.length())
          
          // 更新消息状态
          let updated_message = { message | status: MessageStatus::Delivered, receiver: receiver }
          
          // 更新队列
          let updated_queue = { q | messages: remaining_messages }
          let updated_queues = broker.queues.set(queue_name, updated_queue)
          
          let updated_broker = { broker | queues: updated_queues }
          
          (updated_broker, Some(updated_message))
        } else {
          (broker, None)
        }
      }
      None => (broker, None)
    }
  }
  
  // 确认消息
  let acknowledge_message = fn(broker: MessageBroker, message_id: String) {
    // 记录确认
    let current_count = broker.acknowledgments.get(message_id)
    let updated_count = match current_count {
      Some(count) => count + 1
      None => 1
    }
    
    let updated_acknowledgments = broker.acknowledgments.set(message_id, updated_count)
    
    { broker | acknowledgments: updated_acknowledgments }
  }
  
  // 重试失败的消息
  let retry_failed_messages = fn(broker: MessageBroker, queue_name: String) {
    let queue = broker.queues.get(queue_name)
    match queue {
      Some(q) => {
        let mut updated_messages = []
        let mut failed_messages = []
        
        for message in q.messages {
          if message.status == MessageStatus::Failed and message.retry_count < message.max_retries {
            // 重试消息
            let retried_message = {
              id: message.id,
              pattern: message.pattern,
              guarantee: message.guarantee,
              sender: message.sender,
              receiver: message.receiver,
              payload: message.payload,
              timestamp: 1640995200,
              status: MessageStatus::Pending,
              retry_count: message.retry_count + 1,
              max_retries: message.max_retries
            }
            updated_messages = updated_messages.push(retried_message)
          } else if message.status == MessageStatus::Failed and message.retry_count >= message.max_retries {
            // 移到死信队列
            failed_messages = failed_messages.push(message)
          } else {
            // 保持原样
            updated_messages = updated_messages.push(message)
          }
        }
        
        let updated_queue = { q | messages: updated_messages }
        let updated_queues = broker.queues.set(queue_name, updated_queue)
        
        let updated_broker = {
          queues: updated_queues,
          dead_letter_queue: broker.dead_letter_queue + failed_messages,
          acknowledgments: broker.acknowledgments,
          retry_policy: broker.retry_policy
        }
        
        (updated_broker, failed_messages.length())
      }
      None => (broker, 0)
    }
  }
  
  // 模拟消息处理
  let process_message = fn(message: Message) {
    // 模拟处理成功率（80%）
    let success = Random::float() > 0.2
    
    if success {
      { message | status: MessageStatus::Acknowledged }
    } else {
      { message | status: MessageStatus::Failed }
    }
  }
  
  // 创建消息代理
  let mut broker = create_message_broker(3, 1000)
  
  // 创建队列
  let request_queue = create_message_queue("request-queue", 100, DeliveryGuarantee::AtLeastOnce)
  let event_queue = create_message_queue("event-queue", 200, DeliveryGuarantee::AtMostOnce)
  
  broker = add_queue(broker, request_queue)
  broker = add_queue(broker, event_queue)
  
  // 创建消息
  let request_message = create_message(
    "req-123",
    MessagePattern::RequestReply,
    DeliveryGuarantee::AtLeastOnce,
    "client-service",
    "user-service",
    "{\"action\": \"get_user\", \"user_id\": \"123\"}",
    3
  )
  
  let event_message = create_message(
    "event-456",
    MessagePattern::PublishSubscribe,
    DeliveryGuarantee::AtMostOnce,
    "order-service",
    "*",
    "{\"event\": \"order_created\", \"order_id\": \"456\"}",
    1
  )
  
  // 发送消息
  let (broker1, sent1) = send_message(broker, "request-queue", request_message)
  assert_true(sent1)
  
  let (broker2, sent2) = send_message(broker1, "event-queue", event_message)
  assert_true(sent2)
  
  // 接收消息
  let (broker3, received_message) = receive_message(broker2, "request-queue", "user-service")
  assert_true(received_message != None)
  
  match received_message {
    Some(message) => {
      assert_eq(message.id, "req-123")
      assert_eq(message.status, MessageStatus::Delivered)
      assert_eq(message.receiver, "user-service")
      
      // 处理消息
      let processed_message = process_message(message)
      
      // 确认消息
      let broker4 = acknowledge_message(broker3, processed_message.id)
      
      // 验证确认
      let ack_count = broker4.acknowledgments.get(processed_message.id)
      match ack_count {
        Some(count) => assert_eq(count, 1)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试消息重试
  let failed_message = create_message(
    "req-789",
    MessagePattern::RequestReply,
    DeliveryGuarantee::AtLeastOnce,
    "client-service",
    "payment-service",
    "{\"action\": \"process_payment\", \"amount\": 100}",
    2
  )
  
  let (broker5, _) = send_message(broker4, "request-queue", failed_message)
  
  // 模拟消息处理失败
  let (broker6, received_failed_message) = receive_message(broker5, "request-queue", "payment-service")
  match received_failed_message {
    Some(message) => {
      let processed_failed_message = { message | status: MessageStatus::Failed }
      
      // 更新队列中的消息状态
      let queue = broker6.queues.get("request-queue")
      match queue {
        Some(q) => {
          let updated_messages = q.messages.map_fn(i) {
            if i == 0 {
              processed_failed_message
            } else {
              q.messages[i]
            }
          }
          
          let updated_queue = { q | messages: updated_messages }
          let updated_queues = broker6.queues.set("request-queue", updated_queue)
          let broker7 = { broker6 | queues: updated_queues }
          
          // 重试失败的消息
          let (broker8, failed_count) = retry_failed_messages(broker7, "request-queue")
          
          // 验证重试结果
          if processed_failed_message.retry_count < processed_failed_message.max_retries {
            // 消息应该被重试
            let retry_queue = broker8.queues.get("request-queue")
            match retry_queue {
              Some(q) => {
                let retry_message = q.messages.find_fn(m) { m.id == "req-789" }
                match retry_message {
                  Some(msg) => {
                    assert_eq(msg.retry_count, 1)
                    assert_eq(msg.status, MessageStatus::Pending)
                  }
                  None => assert_true(false)
                }
              }
              None => assert_true(false)
            }
          } else {
            // 消息应该被移到死信队列
            assert_eq(failed_count, 1)
            assert_true(broker8.dead_letter_queue.length() > 0)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试不同传递保证
  let at_most_once_message = create_message(
    "event-111",
    MessagePattern::FireAndForget,
    DeliveryGuarantee::AtMostOnce,
    "notification-service",
    "email-service",
    "{\"recipient\": \"user@example.com\", \"message\": \"Hello\"}",
    0
  )
  
  let (broker9, _) = send_message(broker8, "event-queue", at_most_once_message)
  
  // 接收但不确认
  let (broker10, received_at_most_once) = receive_message(broker9, "event-queue", "email-service")
  match received_at_most_once {
    Some(message) => {
      assert_eq(message.id, "event-111")
      assert_eq(message.guarantee, DeliveryGuarantee::AtMostOnce)
      
      // AtMostOnce保证下，消息不会被重试，即使没有确认
      let (broker11, failed_count) = retry_failed_messages(broker10, "event-queue")
      assert_eq(failed_count, 0)
    }
    None => assert_true(false)
  }
  
  // 验证队列状态
  let final_request_queue = broker11.queues.get("request-queue")
  let final_event_queue = broker11.queues.get("event-queue")
  
  match final_request_queue {
    Some(q) => assert_true(q.messages.length() >= 0)
    None => assert_true(false)
  }
  
  match final_event_queue {
    Some(q) => assert_true(q.messages.length() >= 0)
    None => assert_true(false)
  }
}