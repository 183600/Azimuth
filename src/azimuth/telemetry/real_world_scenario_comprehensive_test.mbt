// 实际使用场景测试 - 模拟电商应用的完整遥测流程
// 测试真实世界场景中Azimuth Telemetry的端到端应用

test "ecommerce_user_journey_telemetry" {
  // 模拟电商用户从浏览到购买的完整旅程的遥测数据收集
  
  // 1. 初始化电商服务遥测
  let ctx = context::Context::empty()
  let resource = common::Resource::default("ecommerce-platform")
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  let tracer = tracer_provider.get_tracer("ecommerce-service", "2.1.0")
  let meter = meter_provider.get_meter("ecommerce-service", "2.1.0")
  let logger = logger_provider.get_logger("ecommerce-service", "2.1.0")
  
  // 设置用户会话上下文
  let session_key = context::create_key("session.id")
  let user_key = context::create_key("user.id")
  let device_key = context::create_key("device.type")
  
  let ctx_session = ctx
    .with_value(session_key, "sess_abc123def456")
    .with_value(user_key, "user_789012")
    .with_value(device_key, "mobile")
  
  // 2. 用户浏览商品阶段
  let (ctx_browse, browse_span) = tracer.start_span(
    ctx_session,
    "user_browse_products",
    trace::Server,
    [
      ("user.id", common::AttributeValue::string("user_789012")),
      ("session.id", common::AttributeValue::string("sess_abc123def456")),
      ("page.category", common::AttributeValue::string("electronics")),
      ("page.type", common::AttributeValue::string("product_list"))
    ],
    1640995200000000000L
  )
  
  // 记录浏览metrics
  let page_view_counter = meter.create_counter("page_views_total", "count", "Total page views")
  let search_histogram = meter.create_histogram("search_response_time_ms", "ms", "Search response time")
  
  page_view_counter.add(1L, [
    ("page.category", common::AttributeValue::string("electronics")),
    ("user.segment", common::AttributeValue::string("premium")),
    ("device.type", common::AttributeValue::string("mobile"))
  ])
  
  search_histogram.record(85.2, [
    ("search.type", common::AttributeValue::string("category_browse")),
    ("results.count", common::AttributeValue::int(48L))
  ])
  
  logger.info("User browsing electronics category", [
    ("user.id", common::AttributeValue::string("user_789012")),
    ("category", common::AttributeValue::string("electronics")),
    ("products.viewed", common::AttributeValue::int(48L)),
    ("page.load_time_ms", common::AttributeValue::float(85.2))
  ])
  
  // 3. 用户查看商品详情
  let (ctx_detail, detail_span) = tracer.start_span(
    ctx_browse,
    "view_product_detail",
    trace::Internal,
    [
      ("product.id", common::AttributeValue::string("prod_smartphone_001")),
      ("product.category", common::AttributeValue::string("electronics")),
      ("product.brand", common::AttributeValue::string("TechCorp"))
    ]
  )
  
  let product_view_counter = meter.create_counter("product_views_total", "count", "Product views")
  let inventory_gauge = meter.create_gauge("product_inventory_count", "count", "Product inventory")
  
  product_view_counter.add(1L, [
    ("product.id", common::AttributeValue::string("prod_smartphone_001")),
    ("product.category", common::AttributeValue::string("electronics"))
  ])
  
  inventory_gauge.record(25.0, [
    ("product.id", common::AttributeValue::string("prod_smartphone_001")),
    ("warehouse.region", common::AttributeValue::string("us-west-2"))
  ])
  
  logger.info("Product detail view", [
    ("product.id", common::AttributeValue::string("prod_smartphone_001")),
    ("product.price", common::AttributeValue::float(699.99)),
    ("stock.available", common::AttributeValue::bool(true)),
    ("user.favorited", common::AttributeValue::bool(false))
  ])
  
  // 4. 用户添加到购物车
  let (ctx_cart, cart_span) = tracer.start_span(
    ctx_detail,
    "add_to_cart",
    trace::Internal,
    [
      ("product.id", common::AttributeValue::string("prod_smartphone_001")),
      ("quantity", common::AttributeValue::int(1L)),
      ("cart.source", common::AttributeValue::string("product_page"))
    ]
  )
  
  let cart_counter = meter.create_counter("cart_operations_total", "count", "Cart operations")
  let cart_value_gauge = meter.create_gauge("cart_total_value", "currency", "Cart total value")
  
  cart_counter.add(1L, [
    ("operation", common::AttributeValue::string("add_item")),
    ("product.id", common::AttributeValue::string("prod_smartphone_001")),
    ("user.id", common::AttributeValue::string("user_789012"))
  ])
  
  cart_value_gauge.record(699.99, [
    ("user.id", common::AttributeValue::string("user_789012")),
    ("currency", common::AttributeValue::string("USD"))
  ])
  
  logger.info("Item added to cart", [
    ("product.id", common::AttributeValue::string("prod_smartphone_001")),
    ("quantity", common::AttributeValue::int(1L)),
    ("cart.total_items", common::AttributeValue::int(1L)),
    ("cart.total_value", common::AttributeValue::float(699.99))
  ])
  
  // 5. 用户进入结账流程
  let (ctx_checkout, checkout_span) = tracer.start_span(
    ctx_cart,
    "checkout_process",
    trace::Client,
    [
      ("checkout.type", common::AttributeValue::string("standard")),
      ("payment.method", common::AttributeValue::string("credit_card")),
      ("shipping.method", common::AttributeValue::string("express"))
    ]
  )
  
  let checkout_counter = meter.create_counter("checkout_initiated_total", "count", "Checkout initiated")
  let payment_histogram = meter.create_histogram("payment_processing_time_ms", "ms", "Payment processing time")
  
  checkout_counter.add(1L, [
    ("user.id", common::AttributeValue::string("user_789012")),
    ("checkout.flow", common::AttributeValue::string("mobile_standard"))
  ])
  
  // 6. 支付处理（模拟支付服务调用）
  let (ctx_payment, payment_span) = tracer.start_span(
    ctx_checkout,
    "payment_processing",
    trace::Client,
    [
      ("payment.provider", common::AttributeValue::string("stripe")),
      ("payment.type", common::AttributeValue::string("credit_card")),
      ("amount", common::AttributeValue::float(699.99))
    ]
  )
  
  payment_histogram.record(1250.5, [
    ("payment.provider", common::AttributeValue::string("stripe")),
    ("payment.status", common::AttributeValue::string("success"))
  ])
  
  logger.info("Payment processed successfully", [
    ("payment.id", common::AttributeValue::string("pay_xyz789")),
    ("amount", common::AttributeValue::float(699.99)),
    ("currency", common::AttributeValue::string("USD")),
    ("payment.method", common::AttributeValue::string("credit_card"))
  ])
  
  // 7. 订单创建和确认
  let (ctx_order, order_span) = tracer.start_span(
    ctx_payment,
    "create_order",
    trace::Internal,
    [
      ("order.id", common::AttributeValue::string("order_20220101_001")),
      ("order.total", common::AttributeValue::float(699.99)),
      ("order.items_count", common::AttributeValue::int(1L))
    ]
  )
  
  let order_counter = meter.create_counter("orders_created_total", "count", "Orders created")
  let revenue_counter = meter.create_counter("revenue_total", "currency", "Total revenue")
  
  order_counter.add(1L, [
    ("order.type", common::AttributeValue::string("online")),
    ("user.segment", common::AttributeValue::string("premium"))
  ])
  
  revenue_counter.add(69999, [  // 以分为单位
    ("currency", common::AttributeValue::string("USD")),
    ("product.category", common::AttributeValue::string("electronics"))
  ])
  
  logger.info("Order created successfully", [
    ("order.id", common::AttributeValue::string("order_20220101_001")),
    ("user.id", common::AttributeValue::string("user_789012")),
    ("order.total", common::AttributeValue::float(699.99)),
    ("items.count", common::AttributeValue::int(1L)),
    ("estimated.delivery", common::AttributeValue::string("2022-01-03"))
  ])
  
  // 8. 完成整个用户旅程的根span
  let completed_journey_span = trace::Span::{
    ..browse_span,
    end_time_unix_nanos: Some(1640995200450000000L),  // 450ms后完成
    status: trace::Ok,
    status_description: Some("User journey completed successfully"),
    events: [
      trace::SpanEvent::{
        name: "journey_completed",
        timestamp_unix_nanos: 1640995200450000000L,
        attributes: [
          ("total_duration_ms", common::AttributeValue::float(450.0)),
          ("pages.viewed", common::AttributeValue::int(5L)),
          ("products.viewed", common::AttributeValue::int(48L)),
          ("cart.items", common::AttributeValue::int(1L)),
          ("order.value", common::AttributeValue::float(699.99)),
          ("conversion", common::AttributeValue::bool(true))
        ]
      }
    ],
    links: [
      trace::SpanLink::{
        context: trace::SpanContext::{
          trace_id: [for i = 0; i < 16; i = i + 1].map(fn(i) { (i % 256) |> to_int64 |> Int64::to_byte }),
          span_id: [for i = 0; i < 8; i = i + 1].map(fn(i) { (i % 256) |> to_int64 |> Int64::to_byte }),
          trace_flags: 1_byte,
          trace_state: "user_id=user_789012"
        },
        attributes: [
          ("linked.operation", common::AttributeValue::string("user_recommendation")),
          ("recommendation.score", common::AttributeValue::float(0.85))
        ]
      }
    ]
  }
  
  // 验证整个用户旅程的遥测数据
  assert_eq(completed_journey_span.name, "user_browse_products")
  assert_eq(completed_journey_span.status, trace::Ok)
  assert_eq(completed_journey_span.events.length(), 1)
  assert_eq(completed_journey_span.links.length(), 1)
  
  let journey_event = completed_journey_span.events[0]
  assert_eq(journey_event.name, "journey_completed")
  
  // 验证关键业务指标
  let mut found_conversion = false
  let mut found_order_value = false
  
  let mut i = 0
  while i < journey_event.attributes.length() {
    let (key, value) = journey_event.attributes[i]
    if key == "conversion" {
      match value {
        common::BoolValue(conv) => {
          assert_eq(conv, true)
          found_conversion = true
        }
        _ => assert_eq(false, true, "Expected bool value for conversion")
      }
    }
    if key == "order.value" {
      match value {
        common::FloatValue(val) => {
          assert_eq(val, 699.99)
          found_order_value = true
        }
        _ => assert_eq(false, true, "Expected float value for order.value")
      }
    }
    i = i + 1
  }
  
  assert_eq(found_conversion, true)
  assert_eq(found_order_value, true)
}

test "ecommerce_inventory_management_telemetry" {
  // 模拟电商库存管理系统的遥测数据收集
  
  let ctx = context::Context::empty()
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  let tracer = tracer_provider.get_tracer("inventory-service", "1.5.0")
  let meter = meter_provider.get_meter("inventory-service", "1.5.0")
  let logger = logger_provider.get_logger("inventory-service", "1.5.0")
  
  // 库存同步操作
  let (ctx_sync, sync_span) = tracer.start_span(
    ctx,
    "inventory_sync",
    trace::Internal,
    [
      ("sync.type", common::AttributeValue::string("full_sync")),
      ("warehouse.count", common::AttributeValue::int(5L)),
      ("product.count", common::AttributeValue::int(10000L))
    ],
    1640995800000000000L
  )
  
  let inventory_gauge = meter.create_gauge("inventory_levels", "count", "Current inventory levels")
  let sync_counter = meter.create_counter("inventory_sync_operations", "count", "Inventory sync operations")
  
  // 模拟不同产品的库存水平
  inventory_gauge.record(25.0, [
    ("product.id", common::AttributeValue::string("prod_smartphone_001")),
    ("warehouse.id", common::AttributeValue::string("wh_seattle")),
    ("category", common::AttributeValue::string("electronics"))
  ])
  
  inventory_gauge.record(150.0, [
    ("product.id", common::AttributeValue::string("prod_laptop_002")),
    ("warehouse.id", common::AttributeValue::string("wh_seattle")),
    ("category", common::AttributeValue::string("electronics"))
  ])
  
  inventory_gauge.record(500.0, [
    ("product.id", common::AttributeValue::string("prod_headphones_003")),
    ("warehouse.id", common::AttributeValue::string("wh_la")),
    ("category", common::AttributeValue::string("electronics"))
  ])
  
  sync_counter.add(1L, [
    ("sync.type", common::AttributeValue::string("full_sync")),
    ("duration_minutes", common::AttributeValue::int(15L))
  ])
  
  logger.info("Inventory sync completed", [
    ("products.synced", common::AttributeValue::int(10000L)),
    ("warehouses.synced", common::AttributeValue::int(5L)),
    ("discrepancies.found", common::AttributeValue::int(12L)),
    ("sync.duration_minutes", common::AttributeValue::int(15L))
  ])
  
  // 库存预警检查
  let (ctx_alert, alert_span) = tracer.start_span(
    ctx_sync,
    "inventory_alert_check",
    trace::Internal,
    [
      ("alert.type", common::AttributeValue::string("low_stock")),
      ("threshold.percentage", common::AttributeValue::int(20L))
    ]
  )
  
  let low_stock_counter = meter.create_counter("low_stock_alerts", "count", "Low stock alerts")
  let out_of_stock_counter = meter.create_counter("out_of_stock_events", "count", "Out of stock events")
  
  // 模拟低库存预警
  low_stock_counter.add(3L, [
    ("warehouse.id", common::AttributeValue::string("wh_seattle")),
    ("category", common::AttributeValue::string("electronics"))
  ])
  
  out_of_stock_counter.add(1L, [
    ("product.id", common::AttributeValue::string("prod_tablet_004")),
    ("warehouse.id", common::AttributeValue::string("wh_chicago"))
  ])
  
  logger.warn("Low stock alerts generated", [
    ("products.low_stock", common::AttributeValue::int(3L)),
    ("products.out_of_stock", common::AttributeValue::int(1L)),
    ("warehouse.most_affected", common::AttributeValue::string("wh_seattle"))
  ])
  
  // 自动补货操作
  let (ctx_restock, restock_span) = tracer.start_span(
    ctx_alert,
    "automatic_restock",
    trace::Client,
    [
      ("restock.type", common::AttributeValue::string("automatic")),
      ("supplier.count", common::AttributeValue::int(2L))
    ]
  )
  
  let restock_counter = meter.create_counter("restock_orders", "count", "Restock orders created")
  let restock_value_counter = meter.create_counter("restock_value", "currency", "Restock order value")
  
  restock_counter.add(2L, [
    ("restock.type", common::AttributeValue::string("automatic")),
    ("priority", common::AttributeValue::string("high"))
  ])
  
  restock_value_counter.add(15000, [  // $150.00
    ("currency", common::AttributeValue::string("USD")),
    ("supplier", common::AttributeValue::string("tech_supplies_inc"))
  ])
  
  logger.info("Automatic restock orders created", [
    ("orders.created", common::AttributeValue::int(2L)),
    ("total.value", common::AttributeValue::float(150.00)),
    ("expected.delivery_days", common::AttributeValue::int(3L))
  ])
  
  // 完成库存管理流程
  let completed_sync_span = trace::Span::{
    ..sync_span,
    end_time_unix_nanos: Some(1640995890000000000L),  // 15分钟后完成
    status: trace::Ok,
    status_description: Some("Inventory management cycle completed"),
    events: [
      trace::SpanEvent::{
        name: "sync_summary",
        timestamp_unix_nanos: 1640995890000000000L,
        attributes: [
          ("products.processed", common::AttributeValue::int(10000L)),
          ("alerts.generated", common::AttributeValue::int(4L)),
          ("restock.orders", common::AttributeValue::int(2L)),
          ("sync.accuracy", common::AttributeValue::float(99.8))
        ]
      }
    ]
  }
  
  // 验证库存管理遥测数据
  assert_eq(completed_sync_span.name, "inventory_sync")
  assert_eq(completed_sync_span.status, trace::Ok)
  assert_eq(completed_sync_span.events.length(), 1)
  
  let sync_summary = completed_sync_span.events[0]
  assert_eq(sync_summary.name, "sync_summary")
  
  // 验证关键库存指标
  let mut found_accuracy = false
  let mut found_alerts = false
  
  let mut i = 0
  while i < sync_summary.attributes.length() {
    let (key, value) = sync_summary.attributes[i]
    if key == "sync.accuracy" {
      match value {
        common::FloatValue(accuracy) => {
          assert_eq(accuracy, 99.8)
          found_accuracy = true
        }
        _ => assert_eq(false, true, "Expected float value for sync.accuracy")
      }
    }
    if key == "alerts.generated" {
      match value {
        common::IntValue(alerts) => {
          assert_eq(alerts, 4L)
          found_alerts = true
        }
        _ => assert_eq(false, true, "Expected int value for alerts.generated")
      }
    }
    i = i + 1
  }
  
  assert_eq(found_accuracy, true)
  assert_eq(found_alerts, true)
}

test "ecommerce_recommendation_engine_telemetry" {
  // 模拟电商推荐引擎的遥测数据收集
  
  let ctx = context::Context::empty()
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  let tracer = tracer_provider.get_tracer("recommendation-service", "3.2.0")
  let meter = meter_provider.get_meter("recommendation-service", "3.2.0")
  let logger = logger_provider.get_logger("recommendation-service", "3.2.0")
  
  // 推荐生成流程
  let (ctx_recommend, recommend_span) = tracer.start_span(
    ctx,
    "generate_recommendations",
    trace::Internal,
    [
      ("user.id", common::AttributeValue::string("user_789012")),
      ("recommendation.type", common::AttributeValue::string("product_recommendations")),
      ("algorithm.version", common::AttributeValue::string("collaborative_v3"))
    ],
    1640996000000000000L
  )
  
  let recommendation_counter = meter.create_counter("recommendations_generated", "count", "Recommendations generated")
  let ml_latency_histogram = meter.create_histogram("ml_inference_latency_ms", "ms", "ML inference latency")
  
  // 模拟ML模型推理
  ml_latency_histogram.record(125.3, [
    ("model.type", common::AttributeValue::string("collaborative_filtering")),
    ("model.version", common::AttributeValue::string("v3.2.1"))
  ])
  
  recommendation_counter.add(10L, [
    ("user.segment", common::AttributeValue::string("premium")),
    ("recommendation.context", common::AttributeValue::string("product_page"))
  ])
  
  logger.info("Product recommendations generated", [
    ("user.id", common::AttributeValue::string("user_789012")),
    ("recommendations.count", common::AttributeValue::int(10L)),
    ("model.version", common::AttributeValue::string("collaborative_v3")),
    ("inference.time_ms", common::AttributeValue::float(125.3))
  ])
  
  // A/B测试跟踪
  let (ctx_abtest, abtest_span) = tracer.start_span(
    ctx_recommend,
    "ab_test_tracking",
    trace::Internal,
    [
      ("experiment.id", common::AttributeValue::string("exp_rec_algo_2022q1")),
      ("variant", common::AttributeValue::string("treatment")),
      ("user.cohort", common::AttributeValue::string("premium_mobile"))
    ]
  )
  
  let ab_test_counter = meter.create_counter("ab_test_impressions", "count", "A/B test impressions")
  let click_through_rate = meter.create_histogram("recommendation_ctr", "percentage", "Recommendation CTR")
  
  ab_test_counter.add(1L, [
    ("experiment.id", common::AttributeValue::string("exp_rec_algo_2022q1")),
    ("variant", common::AttributeValue::string("treatment"))
  ])
  
  click_through_rate.record(15.5, [
    ("recommendation.type", common::AttributeValue::string("product_recommendations")),
    ("user.segment", common::AttributeValue::string("premium"))
  ])
  
  logger.info("A/B test impression recorded", [
    ("experiment.id", common::AttributeValue::string("exp_rec_algo_2022q1")),
    ("variant", common::AttributeValue::string("treatment")),
    ("user.id", common::AttributeValue::string("user_789012"))
  ])
  
  // 推荐效果跟踪
  let (ctx_tracking, tracking_span) = tracer.start_span(
    ctx_abtest,
    "recommendation_feedback",
    trace::Internal,
    [
      ("feedback.type", common::AttributeValue::string("click")),
      ("recommendation.id", common::AttributeValue::string("rec_123456"))
    ]
  )
  
  let feedback_counter = meter.create_counter("recommendation_feedback", "count", "Recommendation feedback")
  let conversion_gauge = meter.create_gauge("recommendation_conversion_rate", "percentage", "Recommendation conversion rate")
  
  feedback_counter.add(1L, [
    ("feedback.type", common::AttributeValue::string("click")),
    ("recommendation.position", common::AttributeValue::int(2L))
  ])
  
  conversion_gauge.record(12.5, [
    ("recommendation.algorithm", common::AttributeValue::string("collaborative_v3")),
    ("time.frame", common::AttributeValue::string("24h"))
  ])
  
  logger.info("Recommendation feedback recorded", [
    ("user.id", common::AttributeValue::string("user_789012")),
    ("recommendation.id", common::AttributeValue::string("rec_123456")),
    ("feedback.type", common::AttributeValue::string("click")),
    ("position", common::AttributeValue::int(2L))
  ])
  
  // 完成推荐引擎流程
  let completed_recommend_span = trace::Span::{
    ..recommend_span,
    end_time_unix_nanos: Some(1640996000150000000L),  // 150ms后完成
    status: trace::Ok,
    status_description: Some("Recommendation generation completed"),
    events: [
      trace::SpanEvent::{
        name: "recommendation_summary",
        timestamp_unix_nanos: 1640996000150000000L,
        attributes: [
          ("recommendations.generated", common::AttributeValue::int(10L)),
          ("ml.inference.time_ms", common::AttributeValue::float(125.3)),
          ("ab.test.variant", common::AttributeValue::string("treatment")),
          ("expected.ctr", common::AttributeValue::float(15.5))
        ]
      }
    ]
  }
  
  // 验证推荐引擎遥测数据
  assert_eq(completed_recommend_span.name, "generate_recommendations")
  assert_eq(completed_recommend_span.status, trace::Ok)
  assert_eq(completed_recommend_span.events.length(), 1)
  
  let rec_summary = completed_recommend_span.events[0]
  assert_eq(rec_summary.name, "recommendation_summary")
  
  // 验证关键推荐指标
  let mut found_recommendations = false
  let mut found_ctr = false
  
  let mut i = 0
  while i < rec_summary.attributes.length() {
    let (key, value) = rec_summary.attributes[i]
    if key == "recommendations.generated" {
      match value {
        common::IntValue(rec_count) => {
          assert_eq(rec_count, 10L)
          found_recommendations = true
        }
        _ => assert_eq(false, true, "Expected int value for recommendations.generated")
      }
    }
    if key == "expected.ctr" {
      match value {
        common::FloatValue(ctr) => {
          assert_eq(ctr, 15.5)
          found_ctr = true
        }
        _ => assert_eq(false, true, "Expected float value for expected.ctr")
      }
    }
    i = i + 1
  }
  
  assert_eq(found_recommendations, true)
  assert_eq(found_ctr, true)
}