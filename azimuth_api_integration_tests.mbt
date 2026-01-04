// Azimuth API Integration Test Suite
// API集成测试套件 - 专注于REST API、GraphQL API和API网关集成

// 测试1: REST API客户端集成
test "REST API客户端集成测试" {
  // 创建REST API客户端
  let api_client = RestClient::new()
  RestClient::set_base_url(api_client, "https://api.telemetry.example.com/v1")
  RestClient::set_timeout(api_client, 10000) // 10秒超时
  RestClient::set_user_agent(api_client, "Azimuth-Telemetry/1.0")
  RestClient::add_default_header(api_client, "Accept", "application/json")
  RestClient::add_default_header(api_client, "Content-Type", "application/json")
  
  // 配置API认证
  let auth_config = ApiAuthConfig::new()
  auth_config.set_auth_type(AuthType::BearerToken)
  auth_config.set_token("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkF6aW11dGggVGVsZW1ldHJ5IiwiaWF0IjoxNjQwOTk1MjAwfQ")
  RestClient::set_auth_config(api_client, auth_config)
  
  // 测试GET请求 - 获取遥测数据
  let get_spans_request = RestRequest::new(Method::GET, "/spans")
  get_spans_request.add_query_param("trace_id", "trace-123456")
  get_spans_request.add_query_param("limit", "10")
  get_spans_request.add_query_param("include_attributes", "true")
  
  let get_start_time = Clock::now_unix_nanos(Clock::system())
  let get_response = RestClient::execute(api_client, get_spans_request)
  let get_end_time = Clock::now_unix_nanos(Clock::system())
  
  // 验证GET请求
  match get_response {
    RestResponse(status_code: 200, headers: response_headers, body: response_body) => {
      assert_true(response_headers.contains("content-type"))
      assert_true(response_body.length() > 0)
      
      // 解析JSON响应
      let spans_data = Json::parse(response_body)
      assert_true(spans_data.contains("spans"))
      assert_true(spans_data.get("spans").is_array())
      
      // 验证响应时间
      let response_time = get_end_time - get_start_time
      assert_true(response_time < 5000000000) // 小于5秒
    }
    _ => assert_true(false)
  }
  
  // 测试POST请求 - 创建新的遥测span
  let create_span_data = {
    "trace_id": "trace-789012",
    "span_id": "span-345678",
    "parent_span_id": "span-123456",
    "operation_name": "api.request",
    "service_name": "azimuth-telemetry",
    "start_time": 1640995200000,
    "end_time": 1640995200500,
    "status": "ok",
    "attributes": {
      "http.method": "POST",
      "http.url": "/api/v1/spans",
      "http.status_code": 201,
      "user.id": "12345"
    }
  }
  
  let create_span_request = RestRequest::new(Method::POST, "/spans")
  create_span_request.set_body(Json::serialize(create_span_data))
  
  let create_start_time = Clock::now_unix_nanos(Clock::system())
  let create_response = RestClient::execute(api_client, create_span_request)
  let create_end_time = Clock::now_unix_nanos(Clock::system())
  
  // 验证POST请求
  match create_response {
    RestResponse(status_code: 201, headers: response_headers, body: response_body) => {
      assert_true(response_headers.contains("location"))
      assert_true(response_body.contains("success"))
      
      // 验证创建时间
      let create_time = create_end_time - create_start_time
      assert_true(create_time < 5000000000) // 小于5秒
    }
    _ => assert_true(false)
  }
  
  // 测试PUT请求 - 更新遥测数据
  let update_span_data = {
    "status": "error",
    "attributes": {
      "error.message": "Connection timeout",
      "error.type": "network_error"
    }
  }
  
  let update_span_request = RestRequest::new(Method::PUT, "/spans/span-345678")
  update_span_request.set_body(Json::serialize(update_span_data))
  
  let update_response = RestClient::execute(api_client, update_span_request)
  
  match update_response {
    RestResponse(status_code: 200, headers: _, body: response_body) => {
      assert_true(response_body.contains("updated"))
    }
    _ => assert_true(false)
  }
  
  // 测试DELETE请求 - 删除遥测数据
  let delete_span_request = RestRequest::new(Method::DELETE, "/spans/span-345678")
  let delete_response = RestClient::execute(api_client, delete_span_request)
  
  match delete_response {
    RestResponse(status_code: 204, headers: _, body: _) => {
      assert_true(true) // 204 No Content表示成功删除
    }
    _ => assert_true(false)
  }
  
  // 测试分页请求
  let page = 1
  let page_size = 20
  let pagination_request = RestRequest::new(Method::GET, "/spans")
  pagination_request.add_query_param("page", page.to_string())
  pagination_request.add_query_param("page_size", page_size.to_string())
  
  let pagination_response = RestClient::execute(api_client, pagination_request)
  
  match pagination_response {
    RestResponse(status_code: 200, headers: response_headers, body: response_body) => {
      // 验证分页头
      assert_true(response_headers.contains("x-total-count"))
      assert_true(response_headers.contains("x-page"))
      assert_true(response_headers.contains("x-page-size"))
      
      // 解析分页数据
      let pagination_data = Json::parse(response_body)
      assert_true(pagination_data.contains("spans"))
      assert_true(pagination_data.contains("pagination"))
      
      let pagination_info = pagination_data.get("pagination")
      assert_eq(pagination_info.get("page").to_int(), page)
      assert_eq(pagination_info.get("page_size").to_int(), page_size)
    }
    _ => assert_true(false)
  }
  
  // 测试错误处理
  let error_request = RestRequest::new(Method::GET, "/spans/nonexistent-span")
  let error_response = RestClient::execute(api_client, error_request)
  
  match error_response {
    RestResponse(status_code: 404, headers: _, body: response_body) => {
      assert_true(response_body.contains("not_found") || response_body.contains("error"))
    }
    _ => assert_true(false)
  }
  
  // 验证API客户端统计
  let client_stats = RestClient::get_stats(api_client)
  assert_true(client_stats.total_requests >= 6)
  assert_true(client_stats.successful_requests >= 5)
  assert_true(client_stats.failed_requests >= 1)
  assert_true(client_stats.average_response_time > 0)
}

// 测试2: GraphQL API集成
test "GraphQL API集成测试" {
  // 创建GraphQL客户端
  let graphql_client = GraphQLClient::new()
  GraphQLClient::set_endpoint(graphql_client, "https://graphql.telemetry.example.com/v1/graphql")
  GraphQLClient::set_timeout(graphql_client, 10000) // 10秒超时
  
  // 配置认证
  let auth_headers = [
    ("Authorization", "Bearer graphql-token-123456"),
    ("X-API-Key", "graphql-api-key-789012")
  ]
  
  for (key, value) in auth_headers {
    GraphQLClient::add_header(graphql_client, key, value)
  }
  
  // 测试GraphQL查询 - 获取遥测span
  let span_query = "
    query GetSpan($spanId: ID!) {
      span(id: $spanId) {
        id
        traceId
        operationName
        serviceName
        startTime
        endTime
        status
        duration
        attributes {
          key
          value
        }
      }
    }
  "
  
  let query_variables = {
    "spanId": "span-123456"
  }
  
  let query_start_time = Clock::now_unix_nanos(Clock::system())
  let query_response = GraphQLClient::query(graphql_client, span_query, query_variables)
  let query_end_time = Clock::now_unix_nanos(Clock::system())
  
  // 验证GraphQL查询
  match query_response {
    GraphQLResponse(data: response_data, errors: None, extensions: _) => {
      assert_true(response_data.contains("span"))
      
      let span_data = response_data.get("span")
      assert_eq(span_data.get("id").to_string(), "span-123456")
      assert_true(span_data.get("operationName").to_string().length() > 0)
      assert_true(span_data.contains("attributes"))
      
      // 验证查询时间
      let query_time = query_end_time - query_start_time
      assert_true(query_time < 5000000000) // 小于5秒
    }
    GraphQLResponse(data: _, errors: Some(errors), _) => {
      // 如果有错误，应该是预期的错误（如span不存在）
      assert_true(errors.length() > 0)
    }
    _ => assert_true(false)
  }
  
  // 测试GraphQL变更 - 创建新的遥测数据
  let create_span_mutation = "
    mutation CreateSpan($input: CreateSpanInput!) {
      createSpan(input: $input) {
        id
        traceId
        operationName
        serviceName
        status
      }
    }
  "
  
  let mutation_variables = {
    "input": {
      "traceId": "trace-789012",
      "operationName": "graphql.mutation",
      "serviceName": "azimuth-telemetry",
      "startTime": 1640995200000,
      "endTime": 1640995200300,
      "status": "ok",
      "attributes": [
        {"key": "graphql.operation", "value": "CreateSpan"},
        {"key": "client.version", "value": "1.0.0"}
      ]
    }
  }
  
  let mutation_start_time = Clock::now_unix_nanos(Clock::system())
  let mutation_response = GraphQLClient::mutate(graphql_client, create_span_mutation, mutation_variables)
  let mutation_end_time = Clock::now_unix_nanos(Clock::system())
  
  // 验证GraphQL变更
  match mutation_response {
    GraphQLResponse(data: response_data, errors: None, extensions: _) => {
      assert_true(response_data.contains("createSpan"))
      
      let created_span = response_data.get("createSpan")
      assert_eq(created_span.get("traceId").to_string(), "trace-789012")
      assert_eq(created_span.get("operationName").to_string(), "graphql.mutation")
      assert_eq(created_span.get("status").to_string(), "ok")
      
      // 验证变更时间
      let mutation_time = mutation_end_time - mutation_start_time
      assert_true(mutation_time < 5000000000) // 小于5秒
    }
    _ => assert_true(false)
  }
  
  // 测试GraphQL订阅 - 实时遥测数据流
  let subscription_query = "
    subscription OnTelemetryEvents($serviceName: String!) {
      telemetryEvents(serviceName: $serviceName) {
        id
        traceId
        operationName
        status
        timestamp
      }
    }
  "
  
  let subscription_variables = {
    "serviceName": "azimuth-telemetry"
  }
  
  let subscription = GraphQLClient::subscribe(graphql_client, subscription_query, subscription_variables)
  assert_true(subscription.is_active)
  
  // 等待订阅消息
  let subscription_start = Clock::now_unix_nanos(Clock::system())
  let subscription_result = GraphQLClient::wait_for_message(subscription, 5000) // 5秒超时
  let subscription_end = Clock::now_unix_nanos(Clock::system())
  
  // 验证订阅消息
  match subscription_result {
    Some(GraphQLMessage(data: message_data, errors: None)) => {
      assert_true(message_data.contains("telemetryEvents"))
      
      let event_data = message_data.get("telemetryEvents")
      assert_eq(event_data.get("serviceName").to_string(), "azimuth-telemetry")
      assert_true(event_data.contains("timestamp"))
    }
    Some(GraphQLMessage(data: _, errors: Some(errors))) => {
      // 订阅错误也是可能的
      assert_true(errors.length() > 0)
    }
    None => {
      // 超时也是可能的，取决于服务器是否有活动
    }
  }
  
  // 关闭订阅
  GraphQLClient::close_subscription(subscription)
  
  // 测试批量查询
  let batch_query = "
    query BatchTelemetryQuery($traceIds: [ID!]!) {
      spans(traceIds: $traceIds) {
        id
        traceId
        operationName
        serviceName
        status
      }
      services {
        id
        name
        version
      }
      metrics(limit: 10) {
        name
        value
        unit
      }
    }
  "
  
  let batch_variables = {
    "traceIds": ["trace-001", "trace-002", "trace-003"]
  }
  
  let batch_start_time = Clock::now_unix_nanos(Clock::system())
  let batch_response = GraphQLClient::query(graphql_client, batch_query, batch_variables)
  let batch_end_time = Clock::now_unix_nanos(Clock::system())
  
  // 验证批量查询
  match batch_response {
    GraphQLResponse(data: response_data, errors: None, extensions: _) => {
      assert_true(response_data.contains("spans"))
      assert_true(response_data.contains("services"))
      assert_true(response_data.contains("metrics"))
      
      // 验证批量查询性能
      let batch_time = batch_end_time - batch_start_time
      assert_true(batch_time < 3000000000) // 小于3秒
    }
    _ => assert_true(false)
  }
  
  // 测试GraphQL错误处理
  let error_query = "
    query GetInvalidSpan {
      span(id: "nonexistent-id") {
        id
        name
      }
    }
  "
  
  let error_response = GraphQLClient::query(graphql_client, error_query, {})
  
  match error_response {
    GraphQLResponse(data: _, errors: Some(errors), extensions: _) => {
      assert_true(errors.length() > 0)
      
      let first_error = errors[0]
      assert_true(first_error.contains("message"))
    }
    _ => assert_true(false)
  }
  
  // 验证GraphQL客户端统计
  let graphql_stats = GraphQLClient::get_stats(graphql_client)
  assert_true(graphql_stats.total_queries >= 3)
  assert_true(graphql_stats.total_mutations >= 1)
  assert_true(graphql_stats.total_subscriptions >= 1)
  assert_true(graphql_stats.average_response_time > 0)
}

// 测试3: API网关集成
test "API网关集成测试" {
  // 创建API网关客户端
  let gateway_client = ApiGatewayClient::new()
  ApiGatewayClient::set_gateway_url(gateway_client, "https://gateway.azimuth.example.com")
  ApiGatewayClient::set_api_key(gateway_client, "gateway-api-key-123456")
  ApiGatewayClient::set_timeout(gateway_client, 15000) // 15秒超时
  
  // 注册服务
  let service_registration = {
    "service_name": "azimuth-telemetry",
    "service_version": "1.0.0",
    "service_url": "https://telemetry.internal.example.com",
    "health_check_url": "https://telemetry.internal.example.com/health",
    "load_balancing": "round_robin",
    "circuit_breaker": {
      "failure_threshold": 5,
      "recovery_timeout": 30000
    },
    "rate_limiting": {
      "requests_per_second": 100,
      "burst_size": 200
    }
  }
  
  let register_request = GatewayRequest::new(Method::POST, "/services/register")
  register_request.set_body(Json::serialize(service_registration))
  
  let register_response = ApiGatewayClient::execute(gateway_client, register_request)
  
  match register_response {
    GatewayResponse(status_code: 201, headers: _, body: response_body) => {
      assert_true(response_body.contains("registered"))
      
      let registration_data = Json::parse(response_body)
      assert_true(registration_data.contains("service_id"))
    }
    _ => assert_true(false)
  }
  
  // 测试通过网关的API请求
  let gateway_request = GatewayRequest::new(Method::GET, "/telemetry/v1/spans")
  gateway_request.add_query_param("trace_id", "trace-gateway-001")
  gateway_request.add_query_param("limit", "5")
  gateway_request.add_header("X-Request-ID", "req-gateway-001")
  
  let gateway_start_time = Clock::now_unix_nanos(Clock::system())
  let gateway_response = ApiGatewayClient::execute(gateway_client, gateway_request)
  let gateway_end_time = Clock::now_unix_nanos(Clock::system())
  
  // 验证网关响应
  match gateway_response {
    GatewayResponse(status_code: 200, headers: response_headers, body: response_body) => {
      assert_true(response_headers.contains("x-gateway-request-id"))
      assert_true(response_headers.contains("x-service-response-time"))
      assert_true(response_body.length() > 0)
      
      // 验证网关处理时间
      let gateway_time = gateway_end_time - gateway_start_time
      assert_true(gateway_time < 10000000000) // 小于10秒
      
      // 解析响应数据
      let spans_data = Json::parse(response_body)
      assert_true(spans_data.contains("spans"))
    }
    _ => assert_true(false)
  }
  
  // 测试网关负载均衡
  let load_balancer_requests = []
  for i in 1..=10 {
    let lb_request = GatewayRequest::new(Method::GET, "/telemetry/v1/health")
    lb_request.add_header("X-Test-ID", "lb-test-" + i.to_string())
    
    let lb_response = ApiGatewayClient::execute(gateway_client, lb_request)
    load_balancer_requests.push(lb_response)
  }
  
  // 验证负载均衡结果
  let mut successful_responses = 0
  let mut service_instances = []
  
  for response in load_balancer_requests {
    match response {
      GatewayResponse(status_code: 200, headers: response_headers, body: _) => {
        successful_responses = successful_responses + 1
        
        match response_headers.get("x-served-by-instance") {
          Some(instance) => service_instances.push(instance),
          None => ()
        }
      }
      _ => ()
    }
  }
  
  assert_true(successful_responses >= 8) // 至少80%成功
  
  // 验证使用了多个服务实例
  let unique_instances = service_instances.unique()
  assert_true(unique_instances.length() >= 2) // 至少使用2个实例
  
  // 测试网关限流
  let rate_limit_requests = []
  let rate_limit_start = Clock::now_unix_nanos(Clock::system())
  
  // 发送大量请求以触发限流
  for i in 1..=150 {
    let rl_request = GatewayRequest::new(Method::GET, "/telemetry/v1/metrics")
    rl_request.add_header("X-Client-ID", "rate-limit-test")
    
    let rl_response = ApiGatewayClient::execute(gateway_client, rl_request)
    rate_limit_requests.push(rl_response)
  }
  
  let rate_limit_end = Clock::now_unix_nanos(Clock::system())
  
  // 验证限流结果
  let mut success_count = 0
  let mut rate_limited_count = 0
  
  for response in rate_limit_requests {
    match response {
      GatewayResponse(status_code: 200, _, _) => success_count = success_count + 1,
      GatewayResponse(status_code: 429, headers: response_headers, _) => {
        rate_limited_count = rate_limited_count + 1
        assert_true(response_headers.contains("x-rate-limit-remaining"))
        assert_true(response_headers.contains("x-rate-limit-reset"))
      }
      _ => ()
    }
  }
  
  assert_true(success_count >= 100) // 至少100个请求成功
  assert_true(rate_limited_count >= 10) // 至少10个请求被限流
  
  // 验证限流性能
  let rate_limit_duration = rate_limit_end - rate_limit_start
  assert_true(rate_limit_duration < 30000000000) // 小于30秒
  
  // 测试网关断路器
  let circuit_breaker_requests = []
  
  // 发送请求到可能失败的服务
  for i in 1..=10 {
    let cb_request = GatewayRequest::new(Method::GET, "/unstable-service/v1/test")
    cb_request.add_header("X-Test-ID", "circuit-breaker-" + i.to_string())
    
    let cb_response = ApiGatewayClient::execute(gateway_client, cb_request)
    circuit_breaker_requests.push(cb_response)
  }
  
  // 验证断路器行为
  let mut failure_count = 0
  let mut circuit_open_count = 0
  
  for response in circuit_breaker_requests {
    match response {
      GatewayResponse(status_code: 503, headers: response_headers, _) => {
        circuit_open_count = circuit_open_count + 1
        assert_true(response_headers.contains("x-circuit-breaker"))
      }
      GatewayResponse(status_code: code, _, _) if code >= 500 => {
        failure_count = failure_count + 1
      }
      _ => ()
    }
  }
  
  // 如果有足够的失败，断路器应该打开
  if failure_count >= 5 {
    assert_true(circuit_open_count > 0)
  }
  
  // 测试网关缓存
  let cache_request = GatewayRequest::new(Method::GET, "/telemetry/v1/config")
  cache_request.add_header("X-Cache-Control", "max-age=60")
  
  // 第一次请求
  let cache_response1 = ApiGatewayClient::execute(gateway_client, cache_request)
  
  // 第二次请求（应该从缓存返回）
  let cache_response2 = ApiGatewayClient::execute(gateway_client, cache_request)
  
  match (cache_response1, cache_response2) {
    (GatewayResponse(status_code: 200, headers: headers1, body: body1),
     GatewayResponse(status_code: 200, headers: headers2, body: body2)) => {
      // 验证缓存头
      match headers2.get("x-cache") {
        Some(cache_status) => assert_true(cache_status == "HIT"),
        None => () // 缓存可能未启用
      }
      
      // 验证内容一致
      assert_eq(body1, body2)
    }
    _ => assert_true(false)
  }
  
  // 获取网关统计信息
  let gateway_stats_request = GatewayRequest::new(Method::GET, "/gateway/stats")
  let gateway_stats_response = ApiGatewayClient::execute(gateway_client, gateway_stats_request)
  
  match gateway_stats_response {
    GatewayResponse(status_code: 200, headers: _, body: response_body) => {
      let stats_data = Json::parse(response_body)
      assert_true(stats_data.contains("total_requests"))
      assert_true(stats_data.contains("active_services"))
      assert_true(stats_data.contains("rate_limit_stats"))
      assert_true(stats_data.contains("circuit_breaker_stats"))
    }
    _ => assert_true(false)
  }
  
  // 注销服务
  let unregister_request = GatewayRequest::new(Method::DELETE, "/services/azimuth-telemetry")
  let unregister_response = ApiGatewayClient::execute(gateway_client, unregister_request)
  
  match unregister_response {
    GatewayResponse(status_code: 200, headers: _, body: response_body) => {
      assert_true(response_body.contains("unregistered"))
    }
    _ => assert_true(false)
  }
}

// 测试4: API版本控制
test "API版本控制测试" {
  // 创建支持版本控制的API客户端
  let versioned_client = RestClient::new()
  RestClient::set_base_url(versioned_client, "https://api.telemetry.example.com")
  RestClient::add_default_header(versioned_client, "Accept", "application/json")
  RestClient::add_default_header(versioned_client, "API-Version", "v1")
  
  // 测试API v1
  let v1_request = RestRequest::new(Method::GET, "/spans")
  v1_request.add_header("API-Version", "v1")
  
  let v1_response = RestClient::execute(versioned_client, v1_request)
  
  match v1_response {
    RestResponse(status_code: 200, headers: response_headers, body: response_body) => {
      assert_true(response_headers.get("api-version").or_else(Some("unknown")) == "v1")
      
      let v1_data = Json::parse(response_body)
      assert_true(v1_data.contains("spans"))
      
      // 验证v1特有字段
      let spans = v1_data.get("spans")
      if spans.is_array() && spans.length() > 0 {
        let first_span = spans[0]
        assert_true(first_span.contains("trace_id"))
        assert_true(first_span.contains("span_id"))
        assert_true(first_span.contains("operation_name"))
        // v1不应该有v2特有的字段
        assert_false(first_span.contains("resource"))
      }
    }
    _ => assert_true(false)
  }
  
  // 测试API v2
  let v2_request = RestRequest::new(Method::GET, "/spans")
  v2_request.add_header("API-Version", "v2")
  
  let v2_response = RestClient::execute(versioned_client, v2_request)
  
  match v2_response {
    RestResponse(status_code: 200, headers: response_headers, body: response_body) => {
      assert_true(response_headers.get("api-version").or_else(Some("unknown")) == "v2")
      
      let v2_data = Json::parse(response_body)
      assert_true(v2_data.contains("spans"))
      
      // 验证v2特有字段
      let spans = v2_data.get("spans")
      if spans.is_array() && spans.length() > 0 {
        let first_span = spans[0]
        assert_true(first_span.contains("traceId")) // 注意：v2使用驼峰命名
        assert_true(first_span.contains("spanId"))
        assert_true(first_span.contains("operationName"))
        assert_true(first_span.contains("resource")) // v2特有字段
      }
    }
    _ => assert_true(false)
  }
  
  // 测试版本协商
  let negotiate_request = RestRequest::new(Method::GET, "/spans")
  negotiate_request.add_header("Accept", "application/json; version=2")
  negotiate_request.add_header("API-Version", "*") // 接受任何版本
  
  let negotiate_response = RestClient::execute(versioned_client, negotiate_request)
  
  match negotiate_response {
    RestResponse(status_code: 200, headers: response_headers, body: _) => {
      // 服务器应该选择最佳版本
      let negotiated_version = response_headers.get("api-version").or_else(Some("unknown"))
      assert_true(negotiated_version == "v2" || negotiated_version == "v1")
    }
    _ => assert_true(false)
  }
  
  // 测试版本兼容性检查
  let compatibility_request = RestRequest::new(Method::GET, "/api/compatibility")
  let compatibility_response = RestClient::execute(versioned_client, compatibility_request)
  
  match compatibility_response {
    RestResponse(status_code: 200, headers: _, body: response_body) => {
      let compatibility_data = Json::parse(response_body)
      assert_true(compatibility_data.contains("supported_versions"))
      assert_true(compatibility_data.contains("default_version"))
      assert_true(compatibility_data.contains("deprecated_versions"))
      
      let supported_versions = compatibility_data.get("supported_versions")
      assert_true(supported_versions.contains("v1"))
      assert_true(supported_versions.contains("v2"))
    }
    _ => assert_true(false)
  }
  
  // 测试版本弃用警告
  let deprecated_request = RestRequest::new(Method::GET, "/legacy-endpoint")
  deprecated_request.add_header("API-Version", "v1")
  
  let deprecated_response = RestClient::execute(versioned_client, deprecated_request)
  
  match deprecated_response {
    RestResponse(status_code: 200, headers: response_headers, body: _) => {
      // 弃用端点应该返回警告头
      match response_headers.get("deprecation") {
        Some(warning) => assert_true(warning.length() > 0),
        None => () // 可能没有弃用警告
      }
      
      match response_headers.get("sunset") {
        Some(sunset_date) => assert_true(sunset_date.length() > 0),
        None => () // 可能没有sunset日期
      }
    }
    _ => assert_true(false)
  }
  
  // 测试版本迁移
  let migration_request = RestRequest::new(Method::POST, "/api/migrate")
  migration_request.add_header("API-Version", "v2")
  
  let migration_data = {
    "from_version": "v1",
    "to_version": "v2",
    "data": {
      "trace_id": "trace-migration-001",
      "span_id": "span-migration-001",
      "operation_name": "migration.test",
      "start_time": 1640995200000,
      "end_time": 1640995200500,
      "status": "ok"
    }
  }
  
  migration_request.set_body(Json::serialize(migration_data))
  
  let migration_response = RestClient::execute(versioned_client, migration_request)
  
  match migration_response {
    RestResponse(status_code: 200, headers: _, body: response_body) => {
      let migration_data = Json::parse(response_body)
      assert_true(migration_data.contains("migrated_data"))
      
      let migrated_data = migration_data.get("migrated_data")
      // 验证数据已迁移到v2格式
      assert_true(migrated_data.contains("traceId")) // 驼峰命名
      assert_true(migrated_data.contains("resource")) // v2特有字段
    }
    _ => assert_true(false)
  }
  
  // 测试版本回退
  let rollback_request = RestRequest::new(Method::POST, "/api/rollback")
  rollback_request.add_header("API-Version", "v1")
  
  let rollback_data = {
    "from_version": "v2",
    "to_version": "v1",
    "data": {
      "traceId": "trace-rollback-001",
      "spanId": "span-rollback-001",
      "operationName": "rollback.test",
      "resource": {
        "service": "azimuth-telemetry",
        "version": "1.0.0"
      }
    }
  }
  
  rollback_request.set_body(Json::serialize(rollback_data))
  
  let rollback_response = RestClient::execute(versioned_client, rollback_request)
  
  match rollback_response {
    RestResponse(status_code: 200, headers: _, body: response_body) => {
      let rollback_data = Json::parse(response_body)
      assert_true(rollback_data.contains("rolled_back_data"))
      
      let rolled_back_data = rollback_data.get("rolled_back_data")
      // 验证数据已回退到v1格式
      assert_true(rolled_back_data.contains("trace_id")) // 下划线命名
      assert_false(rolled_back_data.contains("resource")) // v1没有resource字段
    }
    _ => assert_true(false)
  }
}

// 测试5: API安全和认证
test "API安全和认证测试" {
  // 创建API客户端
  let secure_client = RestClient::new()
  RestClient::set_base_url(secure_client, "https://secure-api.telemetry.example.com/v1")
  RestClient::set_timeout(secure_client, 10000)
  
  // 测试API密钥认证
  let api_key_request = RestRequest::new(Method::GET, "/auth/api-key-test")
  api_key_request.add_header("X-API-Key", "secure-api-key-123456")
  
  let api_key_response = RestClient::execute(secure_client, api_key_request)
  
  match api_key_response {
    RestResponse(status_code: 200, headers: response_headers, body: response_body) => {
      assert_true(response_body.contains("authenticated"))
      assert_true(response_headers.get("x-auth-method").or_else(Some("")) == "api_key")
    }
    _ => assert_true(false)
  }
  
  // 测试JWT令牌认证
  let jwt_token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJhenBpdXRoLXRlbGVtZXRyeSIsInJvbGVzIjpbInZpZXdlciJdLCJpYXQiOjE2NDA5OTUyMDAsImV4cCI6MTY0MDk5ODgwMH0.signature"
  
  let jwt_request = RestRequest::new(Method::GET, "/auth/jwt-test")
  jwt_request.add_header("Authorization", "Bearer " + jwt_token)
  
  let jwt_response = RestClient::execute(secure_client, jwt_request)
  
  match jwt_response {
    RestResponse(status_code: 200, headers: response_headers, body: response_body) => {
      assert_true(response_body.contains("authenticated"))
      assert_true(response_headers.get("x-auth-method").or_else(Some("")) == "jwt")
      assert_true(response_headers.contains("x-user-roles"))
    }
    _ => assert_true(false)
  }
  
  // 测试OAuth2认证
  let oauth2_request = RestRequest::new(Method::POST, "/auth/oauth2/token")
  oauth2_request.add_header("Content-Type", "application/x-www-form-urlencoded")
  
  let oauth2_data = "grant_type=client_credentials&client_id=azimuth-client&client_secret=azimuth-secret&scope=telemetry:read telemetry:write"
  oauth2_request.set_body(oauth2_data)
  
  let oauth2_response = RestClient::execute(secure_client, oauth2_request)
  
  match oauth2_response {
    RestResponse(status_code: 200, headers: _, body: response_body) => {
      let token_data = Json::parse(response_body)
      assert_true(token_data.contains("access_token"))
      assert_true(token_data.contains("token_type"))
      assert_true(token_data.contains("expires_in"))
      
      let access_token = token_data.get("access_token").to_string()
      
      // 使用获取的令牌访问受保护的资源
      let protected_request = RestRequest::new(Method::GET, "/protected/resource")
      protected_request.add_header("Authorization", "Bearer " + access_token)
      
      let protected_response = RestClient::execute(secure_client, protected_request)
      
      match protected_response {
        RestResponse(status_code: 200, headers: _, body: response_body) => {
          assert_true(response_body.contains("protected_data"))
        }
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // 测试mTLS认证
  let mtls_client = RestClient::new()
  RestClient::set_base_url(mtls_client, "https://mtls-api.telemetry.example.com/v1")
  
  // 配置客户端证书
  let cert_config = TlsConfig::new()
  cert_config.set_client_certificate("/path/to/client.crt", "/path/to/client.key")
  cert_config.set_ca_certificate("/path/to/ca.crt")
  
  RestClient::set_tls_config(mtls_client, cert_config)
  
  let mtls_request = RestRequest::new(Method::GET, "/auth/mtls-test")
  let mtls_response = RestClient::execute(mtls_client, mtls_request)
  
  match mtls_response {
    RestResponse(status_code: 200, headers: response_headers, body: response_body) => {
      assert_true(response_body.contains("authenticated"))
      assert_true(response_headers.get("x-auth-method").or_else(Some("")) == "mtls")
      assert_true(response_headers.contains("x-client-cert-cn"))
    }
    RestResponse(status_code: 401, _, _) => {
      // mTLS配置可能有问题，但这是预期的测试结果
    }
    _ => assert_true(false)
  }
  
  // 测试API权限控制
  let permission_test_requests = [
    ("/admin/users", "admin"),
    ("/telemetry/spans", "viewer"),
    ("/telemetry/config", "editor")
  ]
  
  for (endpoint, required_role) in permission_test_requests {
    let permission_request = RestRequest::new(Method::GET, endpoint)
    permission_request.add_header("Authorization", "Bearer " + jwt_token)
    
    let permission_response = RestClient::execute(secure_client, permission_request)
    
    match permission_response {
      RestResponse(status_code: 200, headers: _, body: _) => {
        // 权限足够，访问成功
      }
      RestResponse(status_code: 403, headers: _, body: response_body) => {
        // 权限不足，访问被拒绝
        assert_true(response_body.contains("forbidden") || response_body.contains("insufficient_permissions"))
      }
      _ => assert_true(false)
    }
  }
  
  // 测试API限流
  let rate_limit_headers = [
    ("X-Rate-Limit-Limit", "100"),
    ("X-Rate-Limit-Remaining", "99"),
    ("X-Rate-Limit-Reset", "1640995800")
  ]
  
  let rate_limit_request = RestRequest::new(Method::GET, "/telemetry/metrics")
  rate_limit_request.add_header("Authorization", "Bearer " + jwt_token)
  
  for (key, value) in rate_limit_headers {
    rate_limit_request.add_header(key, value)
  }
  
  let rate_limit_response = RestClient::execute(secure_client, rate_limit_request)
  
  match rate_limit_response {
    RestResponse(status_code: 200, headers: response_headers, body: _) => {
      // 验证限流头
      assert_true(response_headers.contains("x-rate-limit-remaining"))
      assert_true(response_headers.contains("x-rate-limit-reset"))
    }
    _ => assert_true(false)
  }
  
  // 测试请求签名
  let signing_client = RestClient::new()
  RestClient::set_base_url(signing_client, "https://signed-api.telemetry.example.com/v1")
  
  let signing_config = RequestSigningConfig::new()
  signing_config.set_algorithm(SigningAlgorithm::HMACSHA256)
  signing_config.set_secret_key("signing-secret-key")
  signing_config.set_header_name("X-Signature")
  
  RestClient::set_signing_config(signing_client, signing_config)
  
  let signed_request = RestRequest::new(Method::POST, "/telemetry/spans")
  signed_request.set_body("{\"test\": \"signed_request\"}")
  
  let signed_response = RestClient::execute(signing_client, signed_request)
  
  match signed_response {
    RestResponse(status_code: 200, headers: response_headers, body: _) => {
      assert_true(response_headers.contains("x-signature-verified"))
    }
    RestResponse(status_code: 401, headers: _, body: response_body) => {
      // 签名验证失败
      assert_true(response_body.contains("invalid_signature"))
    }
    _ => assert_true(false)
  }
  
  // 测试API安全头
  let security_headers_request = RestRequest::new(Method::GET, "/security/headers")
  let security_headers_response = RestClient::execute(secure_client, security_headers_request)
  
  match security_headers_response {
    RestResponse(status_code: 200, headers: response_headers, body: _) => {
      // 验证安全响应头
      assert_true(response_headers.contains("x-content-type-options"))
      assert_true(response_headers.contains("x-frame-options"))
      assert_true(response_headers.contains("x-xss-protection"))
      assert_true(response_headers.contains("strict-transport-security"))
      assert_true(response_headers.contains("content-security-policy"))
    }
    _ => assert_true(false)
  }
  
  // 测试输入验证和清理
  let xss_payload = "<script>alert('xss')</script>"
  let sql_injection_payload = "'; DROP TABLE users; --"
  
  let validation_request = RestRequest::new(Method::POST, "/telemetry/spans")
  validation_request.add_header("Content-Type", "application/json")
  
  let malicious_data = {
    "operation_name": xss_payload,
    "trace_id": sql_injection_payload,
    "service_name": "azimuth-telemetry"
  }
  
  validation_request.set_body(Json::serialize(malicious_data))
  
  let validation_response = RestClient::execute(secure_client, validation_request)
  
  match validation_response {
    RestResponse(status_code: 400, headers: _, body: response_body) => {
      // 输入验证应该拒绝恶意输入
      assert_true(response_body.contains("validation_error") || response_body.contains("invalid_input"))
    }
    RestResponse(status_code: 200, headers: _, body: response_body) => {
      // 如果请求成功，验证输入已被清理
      let safe_data = Json::parse(response_body)
      let operation_name = safe_data.get("operation_name").to_string()
      assert_false(operation_name.contains("<script>"))
    }
    _ => assert_true(false)
  }
}