// 真实世界场景综合测试 - 模拟实际应用中的遥测使用场景

use azimuth.telemetry.api.common.{AttributeValue, Attributes, Resource, InstrumentationScope}
use azimuth.telemetry.api.trace.{SpanKind, StatusCode, Span}
use azimuth.telemetry.api.metrics.{MetricType, Instrument, Measurement}
use azimuth.telemetry.api.logs.{LogRecord, Severity, Logger}
use azimuth.telemetry.api.context.{Context, Baggage, ContextKey}
use azimuth.telemetry.api.propagation.{TextMapPropagator, Propagator}

test "ecommerce_user_journey_telemetry" {
  // 测试电子商务网站用户完整购物流程的遥测
  
  // 1. 用户访问网站 - 创建根Span
  let user_id = "user_12345"
  let session_id = "session_abc123"
  let trace_id = "trace_ecomm_" + user_id
  
  let root_span = Span::{
    trace_id: trace_id,
    span_id: "span_user_entry",
    parent_span_id: None,
    name: "user_session",
    kind: SpanKind::Server,
    start_time: 1640996000L,
    end_time: None,
    status: StatusCode::Unset,
    attributes: [
      ("user.id", AttributeValue::string(user_id)),
      ("session.id", AttributeValue::string(session_id)),
      ("user.agent", AttributeValue::string("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")),
      ("ip.address", AttributeValue::string("192.168.1.100")),
      ("landing.page", AttributeValue::string("/home"))
    ],
    events: [],
    links: []
  }
  
  // 2. 用户浏览商品 - 创建子Span
  let browse_span = Span::{
    trace_id: trace_id,
    span_id: "span_browse_products",
    parent_span_id: Some("span_user_entry"),
    name: "browse_products",
    kind: SpanKind::Internal,
    start_time: 1640996050L,
    end_time: Some(1640996150L),
    status: StatusCode::Ok,
    attributes: [
      ("category", AttributeValue::string("electronics")),
      ("page.number", AttributeValue::int(1L)),
      ("products.viewed", AttributeValue::int(20L)),
      ("search.query", AttributeValue::string("smartphone")),
      ("filter.brand", AttributeValue::string("Samsung"))
    ],
    events: [
      ("product_click", 1640996100L, [
        ("product.id", AttributeValue::string("prod_samsung_123")),
        ("product.name", AttributeValue::string("Samsung Galaxy S23")),
        ("product.price", AttributeValue::float(799.99))
      ])
    ],
    links: []
  }
  
  // 3. 记录浏览指标
  let browse_metrics = [
    Measurement::{
      instrument: Instrument::counter("page_views_total", "Total page views", "count"),
      value: 1.0,
      attributes: [
        ("page.type", AttributeValue::string("product_list")),
        ("user.id", AttributeValue::string(user_id)),
        ("category", AttributeValue::string("electronics"))
      ],
      time: 1640996050L
    },
    Measurement::{
      instrument: Instrument::histogram("page_load_time_ms", "Page load time in milliseconds", "ms"),
      value: 850.0,
      attributes: [
        ("page.type", AttributeValue::string("product_list")),
        ("cache.hit", AttributeValue::bool(true))
      ],
      time: 1640996050L
    }
  ]
  
  // 4. 用户添加商品到购物车
  let cart_span = Span::{
    trace_id: trace_id,
    span_id: "span_add_to_cart",
    parent_span_id: Some("span_user_entry"),
    name: "add_to_cart",
    kind: SpanKind::Client,
    start_time: 1640996200L,
    end_time: Some(1640996220L),
    status: StatusCode::Ok,
    attributes: [
      ("product.id", AttributeValue::string("prod_samsung_123")),
      ("product.name", AttributeValue::string("Samsung Galaxy S23")),
      ("product.price", AttributeValue::float(799.99)),
      ("quantity", AttributeValue::int(1L)),
      ("cart.total_items", AttributeValue::int(1L)),
      ("cart.total_value", AttributeValue::float(799.99))
    ],
    events: [
      ("cart_updated", 1640996210L, [
        ("cart.id", AttributeValue::string("cart_789xyz")),
        ("previous.total", AttributeValue::float(0.0)),
        ("new.total", AttributeValue::float(799.99))
      ])
    ],
    links: []
  }
  
  // 5. 记录购物车操作日志
  let cart_log = LogRecord::{
    timestamp: 1640996220L,
    observed_timestamp: Some(1640996221L),
    severity: Some(Severity::Info),
    severity_text: Some("INFO"),
    body: Some("Product added to shopping cart successfully"),
    attributes: [
      ("trace_id", AttributeValue::string(trace_id)),
      ("user.id", AttributeValue::string(user_id)),
      ("product.id", AttributeValue::string("prod_samsung_123")),
      ("cart.id", AttributeValue::string("cart_789xyz")),
      ("cart.value", AttributeValue::float(799.99)),
      ("promotion.applied", AttributeValue::bool(false))
    ],
    flags: 0,
    trace_id: Some(trace_id),
    span_id: Some("span_add_to_cart"),
    trace_flags: Some(1)
  }
  
  // 6. 用户结账流程
  let checkout_span = Span::{
    trace_id: trace_id,
    span_id: "span_checkout",
    parent_span_id: Some("span_user_entry"),
    name: "checkout_process",
    kind: SpanKind::Server,
    start_time: 1640996300L,
    end_time: Some(1640996450L),
    status: StatusCode::Ok,
    attributes: [
      ("checkout.step", AttributeValue::string("payment")),
      ("payment.method", AttributeValue::string("credit_card")),
      ("payment.gateway", AttributeValue::string("stripe")),
      ("currency", AttributeValue::string("USD")),
      ("order.total", AttributeValue::float(799.99)),
      ("tax.amount", AttributeValue::float(64.0)),
      ("shipping.amount", AttributeValue::float(10.0))
    ],
    events: [
      ("payment_started", 1640996350L, [
        ("payment.id", AttributeValue::string("pay_xyz789")),
        ("amount", AttributeValue::float(799.99))
      ]),
      ("payment_completed", 1640996400L, [
        ("payment.id", AttributeValue::string("pay_xyz789")),
        ("transaction.id", AttributeValue::string("txn_abc123")),
        ("auth.code", AttributeValue::string("123456"))
      ]),
      ("order_created", 1640996450L, [
        ("order.id", AttributeValue::string("order_456def")),
        ("order.status", AttributeValue::string("confirmed"))
      ])
    ],
    links: []
  }
  
  // 7. 记录结账指标
  let checkout_metrics = [
    Measurement::{
      instrument: Instrument::counter("checkout_started_total", "Total checkouts started", "count"),
      value: 1.0,
      attributes: [
        ("user.id", AttributeValue::string(user_id)),
        ("payment.method", AttributeValue::string("credit_card"))
      ],
      time: 1640996300L
    },
    Measurement::{
      instrument: Instrument::counter("checkout_completed_total", "Total checkouts completed", "count"),
      value: 1.0,
      attributes: [
        ("user.id", AttributeValue::string(user_id)),
        ("payment.method", AttributeValue::string("credit_card")),
        ("payment.gateway", AttributeValue::string("stripe"))
      ],
      time: 1640996450L
    },
    Measurement::{
      instrument: Instrument::histogram("checkout_duration_ms", "Checkout process duration", "ms"),
      value: 150.0,
      attributes: [
        ("payment.method", AttributeValue::string("credit_card")),
        ("items.count", AttributeValue::int(1L))
      ],
      time: 1640996450L
    }
  ]
  
  // 8. 记录订单确认日志
  let order_log = LogRecord::{
    timestamp: 1640996450L,
    observed_timestamp: Some(1640996451L),
    severity: Some(Severity::Info),
    severity_text: Some("INFO"),
    body: Some("Order created and payment processed successfully"),
    attributes: [
      ("trace_id", AttributeValue::string(trace_id)),
      ("user.id", AttributeValue::string(user_id)),
      ("order.id", AttributeValue::string("order_456def")),
      ("order.total", AttributeValue::float(873.99)),
      ("payment.method", AttributeValue::string("credit_card")),
      ("shipping.address", AttributeValue::string("123 Main St, City, State 12345"))
    ],
    flags: 0,
    trace_id: Some(trace_id),
    span_id: Some("span_checkout"),
    trace_flags: Some(1)
  }
  
  // 9. 验证用户旅程的遥测数据一致性
  assert_eq(root_span.trace_id, trace_id)
  assert_eq(browse_span.trace_id, trace_id)
  assert_eq(cart_span.trace_id, trace_id)
  assert_eq(checkout_span.trace_id, trace_id)
  
  assert_eq(cart_log.trace_id, Some(trace_id))
  assert_eq(order_log.trace_id, Some(trace_id))
  
  // 验证用户ID在所有遥测数据中的一致性
  let root_user = root_span.attributes[0]
  match root_user.1 {
    StringValue(uid) => assert_eq(uid, user_id)
    _ => assert_eq(false, true)
  }
  
  let log_user = order_log.attributes[1]
  match log_user.1 {
    StringValue(uid) => assert_eq(uid, user_id)
    _ => assert_eq(false, true)
  }
  
  // 验证时间线的正确性
  assert_eq(root_span.start_time < browse_span.start_time, true)
  assert_eq(browse_span.end_time.unwrap() < cart_span.start_time, true)
  assert_eq(cart_span.end_time.unwrap() < checkout_span.start_time, true)
  assert_eq(checkout_span.end_time.unwrap() == order_log.timestamp, true)
  
  // 验证业务逻辑的正确性
  let checkout_total = checkout_span.attributes[4]
  match checkout_total.1 {
    FloatValue(total) => assert_eq(total, 799.99)
    _ => assert_eq(false, true)
  }
  
  let order_total = order_log.attributes[2]
  match order_total.1 {
    FloatValue(total) => assert_eq(total, 873.99) // 799.99 + 64.0 + 10.0
    _ => assert_eq(false, true)
  }
}

test "microservice_request_flow_telemetry" {
  // 测试微服务架构中请求流转的遥测
  
  let trace_id = "trace_microservice_abc"
  let request_id = "req_xyz123"
  
  // 1. API Gateway入口
  let gateway_span = Span::{
    trace_id: trace_id,
    span_id: "span_gateway",
    parent_span_id: None,
    name: "api_gateway_request",
    kind: SpanKind::Server,
    start_time: 1640997000L,
    end_time: Some(1640997020L),
    status: StatusCode::Ok,
    attributes: [
      ("http.method", AttributeValue::string("POST")),
      ("http.url", AttributeValue::string("/api/v1/orders")),
      ("http.scheme", AttributeValue::string("https")),
      ("http.host", AttributeValue::string("api.example.com")),
      ("user.agent", AttributeValue::string("MobileApp/1.0")),
      ("client.ip", AttributeValue::string("10.0.0.1")),
      ("request.id", AttributeValue::string(request_id))
    ],
    events: [
      ("auth_required", 1640997005L, [
        ("auth.type", AttributeValue::string("jwt")),
        ("auth.token.id", AttributeValue::string("token_456"))
      ])
    ],
    links: []
  }
  
  // 2. 认证服务调用
  let auth_span = Span::{
    trace_id: trace_id,
    span_id: "span_auth_service",
    parent_span_id: Some("span_gateway"),
    name: "authenticate_user",
    kind: SpanKind::Client,
    start_time: 1640997010L,
    end_time: Some(1640997015L),
    status: StatusCode::Ok,
    attributes: [
      ("service.name", AttributeValue::string("auth-service")),
      ("service.version", AttributeValue::string("2.3.1")),
      ("rpc.system", AttributeValue::string("grpc")),
      ("rpc.service", AttributeValue::string("AuthService")),
      ("rpc.method", AttributeValue::string("ValidateToken")),
      ("user.id", AttributeValue::string("user_789")),
      ("token.valid", AttributeValue::bool(true))
    ],
    events: [],
    links: []
  }
  
  // 3. 订单服务调用
  let order_span = Span::{
    trace_id: trace_id,
    span_id: "span_order_service",
    parent_span_id: Some("span_gateway"),
    name: "create_order",
    kind: SpanKind::Client,
    start_time: 1640997015L,
    end_time: Some(1640997018L),
    status: StatusCode::Ok,
    attributes: [
      ("service.name", AttributeValue::string("order-service")),
      ("service.version", AttributeValue::string("1.5.2")),
      ("rpc.system", AttributeValue::string("http")),
      ("http.method", AttributeValue::string("POST")),
      ("http.url", AttributeValue::string("http://order-service:8080/orders")),
      ("order.type", AttributeValue::string("purchase")),
      ("order.items_count", AttributeValue::int(3L))
    ],
    events: [],
    links: []
  }
  
  // 4. 库存服务调用
  let inventory_span = Span::{
    trace_id: trace_id,
    span_id: "span_inventory_service",
    parent_span_id: Some("span_order_service"),
    name: "check_inventory",
    kind: SpanKind::Client,
    start_time: 1640997016L,
    end_time: Some(1640997017L),
    status: StatusCode::Ok,
    attributes: [
      ("service.name", AttributeValue::string("inventory-service")),
      ("service.version", AttributeValue::string("3.1.0")),
      ("rpc.system", AttributeValue::string("grpc")),
      ("rpc.service", AttributeValue::string("InventoryService")),
      ("rpc.method", AttributeValue::string("CheckAvailability")),
      ("product.id", AttributeValue::string("prod_123")),
      ("quantity.requested", AttributeValue::int(2L)),
      ("quantity.available", AttributeValue::int(15L)),
      ("inventory.sufficient", AttributeValue::bool(true))
    ],
    events: [],
    links: []
  }
  
  // 5. 支付服务调用
  let payment_span = Span::{
    trace_id: trace_id,
    span_id: "span_payment_service",
    parent_span_id: Some("span_order_service"),
    name: "process_payment",
    kind: SpanKind::Client,
    start_time: 1640997017L,
    end_time: Some(1640997018L),
    status: StatusCode::Ok,
    attributes: [
      ("service.name", AttributeValue::string("payment-service")),
      ("service.version", AttributeValue::string("4.0.1")),
      ("rpc.system", AttributeValue::string("http")),
      ("http.method", AttributeValue::string("POST")),
      ("http.url", AttributeValue::string("http://payment-service:9090/payments")),
      ("payment.method", AttributeValue::string("credit_card")),
      ("payment.amount", AttributeValue::float(299.99)),
      ("currency", AttributeValue::string("USD")),
      ("payment.gateway", AttributeValue::string("stripe"))
    ],
    events: [
      ("payment_authorized", 1640997017L, [
        ("transaction.id", AttributeValue::string("txn_abc123")),
        ("auth.code", AttributeValue::string("987654"))
      ])
    ],
    links: []
  }
  
  // 6. 记录微服务调用指标
  let service_metrics = [
    Measurement::{
      instrument: Instrument::counter("service_requests_total", "Total service requests", "count"),
      value: 1.0,
      attributes: [
        ("service.name", AttributeValue::string("auth-service")),
        ("rpc.method", AttributeValue::string("ValidateToken")),
        ("http.status_code", AttributeValue::int(200L))
      ],
      time: 1640997015L
    },
    Measurement::{
      instrument: Instrument::counter("service_requests_total", "Total service requests", "count"),
      value: 1.0,
      attributes: [
        ("service.name", AttributeValue::string("order-service")),
        ("rpc.method", AttributeValue::string("CreateOrder")),
        ("http.status_code", AttributeValue::int(201L))
      ],
      time: 1640997018L
    },
    Measurement::{
      instrument: Instrument::histogram("service_request_duration_ms", "Service request duration", "ms"),
      value: 5.0,
      attributes: [
        ("service.name", AttributeValue::string("auth-service")),
        ("rpc.method", AttributeValue::string("ValidateToken"))
      ],
      time: 1640997015L
    },
    Measurement::{
      instrument: Instrument::histogram("service_request_duration_ms", "Service request duration", "ms"),
      value: 1.0,
      attributes: [
        ("service.name", AttributeValue::string("inventory-service")),
        ("rpc.method", AttributeValue::string("CheckAvailability"))
      ],
      time: 1640997017L
    }
  ]
  
  // 7. 记录微服务调用日志
  let microservice_log = LogRecord::{
    timestamp: 1640997020L,
    observed_timestamp: Some(1640997021L),
    severity: Some(Severity::Info),
    severity_text: Some("INFO"),
    body: Some("Microservice request flow completed successfully"),
    attributes: [
      ("trace_id", AttributeValue::string(trace_id)),
      ("request.id", AttributeValue::string(request_id)),
      ("services.involved", AttributeValue::array_string(["auth-service", "order-service", "inventory-service", "payment-service"])),
      ("total.duration_ms", AttributeValue::float(20.0)),
      ("order.created", AttributeValue::bool(true)),
      ("payment.processed", AttributeValue::bool(true))
    ],
    flags: 0,
    trace_id: Some(trace_id),
    span_id: Some("span_gateway"),
    trace_flags: Some(1)
  }
  
  // 8. 验证微服务调用链的完整性
  assert_eq(gateway_span.trace_id, trace_id)
  assert_eq(auth_span.parent_span_id, Some("span_gateway"))
  assert_eq(order_span.parent_span_id, Some("span_gateway"))
  assert_eq(inventory_span.parent_span_id, Some("span_order_service"))
  assert_eq(payment_span.parent_span_id, Some("span_order_service"))
  
  // 验证调用时间线
  assert_eq(gateway_span.start_time <= auth_span.start_time, true)
  assert_eq(auth_span.end_time.unwrap() <= order_span.start_time, true)
  assert_eq(order_span.start_time <= inventory_span.start_time, true)
  assert_eq(inventory_span.end_time.unwrap() <= payment_span.start_time, true)
  assert_eq(payment_span.end_time.unwrap() <= gateway_span.end_time.unwrap(), true)
  
  // 验证服务版本信息
  let auth_version = auth_span.attributes[1]
  match auth_version.1 {
    StringValue(version) => assert_eq(version, "2.3.1")
    _ => assert_eq(false, true)
  }
  
  // 验证请求ID的传播
  let gateway_request_id = gateway_span.attributes[6]
  match gateway_request_id.1 {
    StringValue(req_id) => assert_eq(req_id, request_id)
    _ => assert_eq(false, true)
  }
  
  // 验证库存检查的业务逻辑
  let inventory_check = inventory_span.attributes[6]
  match inventory_check.1 {
    BoolValue(sufficient) => assert_eq(sufficient, true)
    _ => assert_eq(false, true)
  }
}